//! Low level directory entry iterator.
use crate::block_iter::BlockIndexClusterIter;
use crate::cluster::Cluster;
use crate::filesystem::FatFileSystem;

use libfs::block::{Block, BlockDevice, BlockIndex};
use libfs::FileSystemError;
use libfs::FileSystemResult;

use super::raw_dir_entry::FatDirEntry;

use super::FatFsType;

/// Represent a raw FAT directory entries iterator.
pub struct FatDirEntryIterator<'a, T> {
    /// The cluster iterator.
    pub(crate) cluster_iter: Option<BlockIndexClusterIter<'a, T>>,

    /// Filesystem reference.
    pub(crate) fs: &'a FatFileSystem<T>,

    /// The last cluster used.
    pub last_cluster: Option<Cluster>,

    /// The first block to use in the first cluster.
    pub block_index: BlockIndex,

    /// The current iteration point in the block.
    pub counter: u8,

    /// Used at the first iteration to init the counter.
    pub is_first: bool,
}

impl<'a, T> FatDirEntryIterator<'a, T>
where
    T: BlockDevice,
{
    /// Create a new iterator from a cluster, a block index and an offset (representing the starting point of the iterator).  
    pub fn new(
        fs: &'a FatFileSystem<T>,
        start_cluster: Cluster,
        block_index: BlockIndex,
        offset: u32,
        in_root_directory: bool,
    ) -> Self {
        let cluster_iter = if in_root_directory {
            match fs.boot_record.fat_type {
                FatFsType::Fat12 | FatFsType::Fat16 => None,
                FatFsType::Fat32 => Some(BlockIndexClusterIter::new(
                    fs,
                    start_cluster,
                    Some(block_index),
                )),
                _ => unimplemented!(),
            }
        } else {
            Some(BlockIndexClusterIter::new(
                fs,
                start_cluster,
                Some(block_index),
            ))
        };

        FatDirEntryIterator {
            counter: (offset / FatDirEntry::LEN as u32) as u8,
            block_index,
            is_first: true,
            fs,
            cluster_iter,
            last_cluster: None,
        }
    }
}

impl<'a, T> Iterator for FatDirEntryIterator<'a, T>
where
    T: BlockDevice,
{
    type Item = FileSystemResult<FatDirEntry>;
    fn next(&mut self) -> Option<FileSystemResult<FatDirEntry>> {
        let entry_per_block_count = (Block::LEN / FatDirEntry::LEN) as u8;
        let fs = self.fs;

        let cluster_opt = if self.counter == entry_per_block_count || self.is_first {
            if !self.is_first {
                self.counter = 0;
                self.block_index.0 += 1;
            }

            self.is_first = false;
            if let Some(cluster_iter) = &mut self.cluster_iter {
                self.block_index.0 %= u64::from(fs.boot_record.blocks_per_cluster());
                self.last_cluster = cluster_iter.next();
            } else {
                self.last_cluster = Some(Cluster(0));
            }
            self.last_cluster
        } else {
            self.last_cluster
        };

        let cluster = cluster_opt?;

        let mut blocks = [Block::new()];

        let entry_index = (self.counter % entry_per_block_count) as usize;

        let entry_block_index = if self.cluster_iter.is_none() {
            let root_dir_blocks = ((u32::from(fs.boot_record.root_dir_childs_count()) * 32)
                + (u32::from(fs.boot_record.bytes_per_block()) - 1))
                / u32::from(fs.boot_record.bytes_per_block());

            if self.block_index.0 > u64::from(root_dir_blocks) {
                None
            } else {
                Some(BlockIndex(
                    fs.first_data_offset.0 - u64::from(root_dir_blocks) + self.block_index.0,
                ))
            }
        } else {
            Some(BlockIndex(
                cluster.to_data_block_index(fs).0 + self.block_index.0,
            ))
        };

        let read_res = fs
            .block_device
            .read(&mut blocks, fs.partition_start, entry_block_index?)
            .or(Err(FileSystemError::ReadFailed));

        if let Err(error) = read_res {
            return Some(Err(error));
        }

        let entry_start = entry_index * FatDirEntry::LEN;
        let entry_end = (entry_index + 1) * FatDirEntry::LEN;
        let dir_entry = FatDirEntry::from_raw(
            &blocks[0][entry_start..entry_end],
            cluster,
            self.block_index,
            entry_start as u32,
        );

        // The entry isn't a valid one but this doesn't mark the end of the directory
        self.counter += 1;

        Some(Ok(dir_entry))
    }
}
