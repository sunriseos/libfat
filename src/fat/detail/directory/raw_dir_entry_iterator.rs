use crate::fat::detail::block_iter::BlockIndexClusterIter;
use crate::fat::detail::cluster::Cluster;
use crate::fat::detail::filesystem::FatFileSystem;

use crate::block::{Block, BlockDevice, BlockIndex};
use crate::FileSystemError;
use crate::Result as FileSystemResult;

use super::raw_dir_entry::FatDirEntry;

pub struct FatDirEntryIterator<'a, T> {
    pub cluster_iter: BlockIndexClusterIter<'a, T>,
    pub last_cluster: Option<Cluster>,
    pub block_index: u32,
    pub counter: u8,
    pub is_first: bool,
}

impl<'a, T> FatDirEntryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn new(
        fs: &'a FatFileSystem<T>,
        start_cluster: Cluster,
        block_index: BlockIndex,
        offset: u32,
    ) -> Self {
        FatDirEntryIterator {
            counter: (offset / FatDirEntry::LEN as u32) as u8,
            block_index: block_index.0,
            is_first: true,
            cluster_iter: BlockIndexClusterIter::new(fs, start_cluster, Some(block_index)),
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
        let fs = self.cluster_iter.cluster_iter.fs;

        let cluster_opt = if self.counter == entry_per_block_count || self.is_first {
            if !self.is_first {
                self.counter = 0;
                self.block_index += 1;
            }
            self.block_index %= u32::from(fs.boot_record.blocks_per_cluster());
            self.is_first = false;
            self.last_cluster = self.cluster_iter.next();
            self.last_cluster
        } else {
            self.last_cluster
        };

        let cluster = cluster_opt?;

        let mut blocks = [Block::new()];

        let entry_index = (self.counter % entry_per_block_count) as usize;

        let read_res = fs
            .block_device
            .read(
                &mut blocks,
                fs.partition_start,
                BlockIndex(cluster.to_data_block_index(fs).0 + self.block_index),
            )
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
