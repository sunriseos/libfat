//! Low level directory entry iterator.
use super::raw_dir_entry::FatDirEntry;
use super::FatFsType;
use crate::cluster::Cluster;
use crate::filesystem::FatFileSystem;
use crate::offset_iter::ClusterOffsetIter;
use crate::utils::FileSystemIterator;
use crate::FatError;
use crate::FatFileSystemResult;
use storage_device::StorageDevice;

/// Represent a raw FAT directory entries iterator.
pub struct FatDirEntryIterator {
    /// The cluster iterator.
    pub(crate) cluster_iter: Option<ClusterOffsetIter>,

    /// The last cluster used.
    pub last_cluster: Option<Cluster>,

    /// The initial offset of the iterator in the first cluster.
    pub cluster_offset: u64,

    /// The current iteration point in the block.
    pub counter: u8,

    /// Used at the first iteration to init the counter.
    pub is_first: bool,
}

impl FatDirEntryIterator {
    /// Create a new iterator from a cluster, a block index and an offset (representing the starting point of the iterator).  
    pub fn new<S: StorageDevice>(
        fs: &FatFileSystem<S>,
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
            cluster_iter,
            last_cluster: None,
        }
    }
}

impl<S: StorageDevice> FileSystemIterator<S> for FatDirEntryIterator {
    type Item = FatFileSystemResult<FatDirEntry>;

    fn next(&mut self, filesystem: &FatFileSystem<S>) -> Option<FatFileSystemResult<FatDirEntry>> {
        let block_size = filesystem.boot_record.bytes_per_block() as usize;
        let entry_per_block_count = (block_size / FatDirEntry::LEN) as u8;

        let cluster_opt = if self.counter == entry_per_block_count || self.is_first {
            if !self.is_first {
                self.counter = 0;
                self.cluster_offset += block_size as u64;
            }

            self.is_first = false;
            if let Some(cluster_iter) = &mut self.cluster_iter {
                self.cluster_offset %=
                    u64::from(filesystem.boot_record.blocks_per_cluster()) * block_size as u64;
                self.last_cluster = cluster_iter.next(filesystem);
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
            let root_dir_blocks = ((u32::from(filesystem.boot_record.root_dir_childs_count())
                * 32)
                + (u32::from(filesystem.boot_record.bytes_per_block()) - 1))
                / u32::from(filesystem.boot_record.bytes_per_block());
            let root_dir_offset =
                root_dir_blocks * u32::from(filesystem.boot_record.bytes_per_block());

            if self.cluster_offset > u64::from(root_dir_offset) {
                None
            } else {
                Some(
                    filesystem.first_data_offset - u64::from(root_dir_offset) + self.cluster_offset,
                )
            }
        } else {
            let data_bytes_offset_res = cluster.to_data_bytes_offset(filesystem);
            if let Err(error) = data_bytes_offset_res {
                return Some(Err(error));
            }
            Some(data_bytes_offset_res.unwrap() + self.cluster_offset)
        };

        let entry_start: u64 = (entry_index * FatDirEntry::LEN) as u64;
        let read_res = filesystem
            .storage_device
            .lock()
            .read(
                filesystem.partition_start + cluster_position_opt? + entry_start,
                &mut raw_data,
            )
            .or(Err(FatError::ReadFailed));

        if let Err(error) = read_res {
            return Some(Err(error));
        }

        let dir_entry = FatDirEntry::from_raw(&raw_data, cluster, self.cluster_offset, entry_start);

        // The entry isn't a valid one but this doesn't mark the end of the directory
        self.counter += 1;

        Some(Ok(dir_entry))
    }
}
