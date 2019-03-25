use arrayvec::ArrayString;

use crate::attribute::Attributes;
use crate::cluster::Cluster;
use crate::filesystem::FatFileSystem;

use libfs::block::{BlockDevice, BlockIndex};
use libfs::FileSystemError;
use libfs::FileSystemResult;

use super::raw_dir_entry::FatDirEntry;

#[derive(Debug, Clone, Copy)]
pub struct DirectoryEntryRawInfo {
    pub parent_cluster: Cluster,
    pub first_entry_block_index: BlockIndex,
    pub first_entry_offset: u32,
    pub entry_count: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct DirectoryEntry {
    pub start_cluster: Cluster,
    pub raw_info: Option<DirectoryEntryRawInfo>,
    pub creation_timestamp: u64,
    pub last_access_timestamp: u64,
    pub last_modification_timestamp: u64,
    pub file_size: u32,
    pub file_name: ArrayString<[u8; Self::MAX_FILE_NAME_LEN_UNICODE]>,
    pub attribute: Attributes,
}

impl DirectoryEntryRawInfo {
    pub fn get_dir_entry<T>(&self, fs: &FatFileSystem<T>) -> FileSystemResult<FatDirEntry>
    where
        T: BlockDevice,
    {
        let mut block_iter = super::FatDirEntryIterator::new(
            fs,
            self.parent_cluster,
            self.first_entry_block_index,
            self.first_entry_offset,
        );

        let mut i = 0;
        let mut res = None;

        while i < self.entry_count {
            let result = block_iter.next();
            if let Some(result) = result {
                res = Some(result?);
            } else {
                res = None;
            }
            i += 1;
        }

        if let Some(res) = res {
            Ok(res)
        } else {
            Err(FileSystemError::NotFound)
        }
    }
}

impl DirectoryEntry {
    pub const MAX_FILE_NAME_LEN: usize = 255;

    // we actually use 256 unicode char because arrayvec doesn't define an implementation for Array<[u8; 1020]>
    pub const MAX_FILE_NAME_LEN_UNICODE: usize = 1024;
}
