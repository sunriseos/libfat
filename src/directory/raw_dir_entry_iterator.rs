//! Low level directory entry iterator.
use crate::offset_iter::ClusterOffsetIter;
use crate::cluster::Cluster;
use crate::filesystem::FatFileSystem;

use libfs::storage::StorageDevice;
use libfs::FileSystemError;
use libfs::FileSystemResult;

use super::raw_dir_entry::FatDirEntry;

use super::FatFsType;

/// Represent a raw FAT directory entries iterator.
pub struct FatDirEntryIterator<'a, S: StorageDevice> {
    /// The cluster iterator.
    pub(crate) cluster_iter: Option<ClusterOffsetIter<'a, S>>,

    /// Filesystem reference.
    pub(crate) fs: &'a FatFileSystem<S>,

    /// The last cluster used.
    pub last_cluster: Option<Cluster>,

    /// The initial offset of the iterator in the first cluster.
    pub cluster_offset: u64,

    /// The current iteration point in the block.
    pub counter: u8,

    /// Used at the first iteration to init the counter.
    pub is_first: bool,
}

impl<'a, S: StorageDevice> FatDirEntryIterator<'a, S>
{
    /// Create a new iterator from a cluster, a block index and an offset (representing the starting point of the iterator).  
    pub fn new(
        fs: &'a FatFileSystem<S>,
        start_cluster: Cluster,
        cluster_offset: u64,
        offset: u64,
        in_root_directory: bool,
    ) -> Self {
        let cluster_iter = if in_root_directory {
            match fs.boot_record.fat_type {
                FatFsType::Fat12 | FatFsType::Fat16 => None,
                FatFsType::Fat32 => Some(ClusterOffsetIter::new(
                    fs,
                    start_cluster,
                    Some(cluster_offset),
                )),
                _ => unimplemented!(),
            }
        } else {
            Some(ClusterOffsetIter::new(
                fs,
                start_cluster,
                Some(cluster_offset),
            ))
        };

        FatDirEntryIterator {
            counter: (offset / FatDirEntry::LEN as u64) as u8,
            cluster_offset,
            is_first: true,
            fs,
            cluster_iter,
            last_cluster: None,
        }
    }
}

impl<'a, S: StorageDevice> Iterator for FatDirEntryIterator<'a, S>
{
    type Item = FileSystemResult<FatDirEntry>;
    fn next(&mut self) -> Option<FileSystemResult<FatDirEntry>> {
        let block_size = crate::MINIMAL_CLUSTER_SIZE;
        let entry_per_block_count = (block_size / FatDirEntry::LEN) as u8;
        let fs = self.fs;

        let cluster_opt = if self.counter == entry_per_block_count || self.is_first {
            if !self.is_first {
                self.counter = 0;
                self.cluster_offset += block_size as u64;
            }

            self.is_first = false;
            if let Some(cluster_iter) = &mut self.cluster_iter {
                self.cluster_offset %= u64::from(fs.boot_record.blocks_per_cluster());
                self.last_cluster = cluster_iter.next();
            } else {
                self.last_cluster = Some(Cluster(0));
            }
            self.last_cluster
        } else {
            self.last_cluster
        };

        let cluster = cluster_opt?;

        let mut raw_data = [0x0u8; FatDirEntry::LEN];

        let entry_index = (self.counter % entry_per_block_count) as usize;

        let cluster_position_opt = if self.cluster_iter.is_none() {
            let root_dir_blocks = ((u32::from(fs.boot_record.root_dir_childs_count()) * 32)
                + (u32::from(fs.boot_record.bytes_per_block()) - 1))
                / u32::from(fs.boot_record.bytes_per_block());
            let root_dir_offset = root_dir_blocks * u32::from(fs.boot_record.bytes_per_block());

            if self.cluster_offset > u64::from(root_dir_offset) {
                None
            } else {
                Some(fs.first_data_offset - u64::from(root_dir_offset) + self.cluster_offset)
            }
        } else {
            Some(cluster.to_data_bytes_offset(fs) + self.cluster_offset)
        };

        let entry_start: u64 = (entry_index * FatDirEntry::LEN) as u64;
        let read_res = fs.storage_device.read(fs.partition_start + cluster_position_opt? + entry_start, &mut raw_data).or(Err(FileSystemError::ReadFailed));

        if let Err(error) = read_res {
            return Some(Err(error));
        }

        let dir_entry = FatDirEntry::from_raw(
            &raw_data,
            cluster,
            cluster_position_opt?,
            entry_start
        );

        // The entry isn't a valid one but this doesn't mark the end of the directory
        self.counter += 1;

        Some(Ok(dir_entry))
    }
}
