use crate::FileSystemError;
use crate::Result as FileSystemResult;

use super::block::{BlockDevice, BlockIndexClusterIter};
use super::FatFileSystem;
use super::utils;

pub mod raw_dir_entry;
pub mod raw_dir_entry_iterator;
pub mod dir_entry_iterator;
pub mod dir_entry;

use dir_entry::DirectoryEntry;

use raw_dir_entry_iterator::FatDirEntryIterator;
use dir_entry_iterator::DirectoryEntryIterator;

#[derive(Copy)]
pub struct Directory<'a, T> {
    pub dir_info: DirectoryEntry,
    pub fs: &'a FatFileSystem<T>,
}

impl<'a, T> Clone for Directory<'a, T> {
    fn clone(&self) -> Self {
        Directory {
            dir_info: self.dir_info,
            fs: self.fs,
        }
    }
}

impl<'a, T> Directory<'a, T>
where
    T: BlockDevice,
{
    pub fn from_entry(fs: &'a FatFileSystem<T>, dir_info: DirectoryEntry) -> Self {
        Directory { dir_info, fs }
    }

    pub fn find_entry(self, name: &str) -> FileSystemResult<DirectoryEntry> {
        for entry in self.iter() {
            let entry = entry?;
            if entry.file_name.as_str() == name {
                return Ok(entry);
            }
        }

        Err(FileSystemError::NotFound)
    }

    pub fn open_file(self, path: &str) -> FileSystemResult<DirectoryEntry> {
        let (name, rest_opt) = utils::split_path(path);
        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        match rest_opt {
            Some(rest) => {
                if !child_entry.attribute.is_directory() {
                    Err(FileSystemError::NotFound)
                } else {
                    Directory::from_entry(fs, child_entry).open_file(rest)
                }
            }
            None => Ok(child_entry),
        }
    }

    pub fn open_dir(self, path: &str) -> FileSystemResult<Directory<'a, T>> {
        let (name, rest_opt) = utils::split_path(path);

        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        if !child_entry.attribute.is_directory() {
            return Err(FileSystemError::NotFound);
        }

        match rest_opt {
            Some(rest) => Directory::from_entry(fs, child_entry).open_dir(rest),
            None => Ok(Directory::from_entry(fs, child_entry)),
        }
    }

    fn delete_dir_entry(
        fs: &'a FatFileSystem<T>,
        dir_entry: &DirectoryEntry,
    ) -> FileSystemResult<()> {
        if let Some(raw_info) = dir_entry.raw_info {
            let mut block_iter = FatDirEntryIterator::new(
                fs,
                raw_info.parent_cluster,
                raw_info.first_entry_block_index,
                raw_info.first_entry_offset,
            );

            let mut i = 0;
            while i < raw_info.entry_count {
                if let Some(block_res) = block_iter.next() {
                    let mut res = block_res?;

                    res.set_deleted();
                    res.flush(fs)?;
                    i += 1;
                } else {
                    return Err(FileSystemError::ReadFailed);
                }
            }
        }

        Ok(())
    }

    pub fn mkdir(self, _name: &str) -> FileSystemResult<()> {
        // TODO: create_sfn, create_lfn
        // TODO: create "." ".." entries in the new directory cluster
        unimplemented!()
    }

    pub fn unlink(self, name: &str, is_dir: bool) -> FileSystemResult<()> {
        let fs = self.fs;

        let dir_entry = self.find_entry(name)?;

        if dir_entry.attribute.is_directory() != is_dir {
            if is_dir {
                return Err(FileSystemError::NotADirectory);
            }

            return Err(FileSystemError::NotAFile);
        }

        // Check for directory not being empty
        if dir_entry.attribute.is_directory()
            && Self::from_entry(fs, dir_entry).iter().nth(2).is_some()
        {
            return Err(FileSystemError::AccessDenied);
        }

        Self::delete_dir_entry(fs, &dir_entry)?;

        if dir_entry.start_cluster.0 != 0 {
            fs.free_cluster(dir_entry.start_cluster, None)?;
        }

        Ok(())
    }
}

impl<'a, T> Directory<'a, T>
where
    T: BlockDevice,
{
    pub fn fat_dir_entry_iter(self) -> FatDirEntryIterator<'a, T> {
        FatDirEntryIterator::from_directory(self)
    }

    pub fn iter(self) -> DirectoryEntryIterator<'a, T> {
        DirectoryEntryIterator::new(self)
    }
}

impl<'a, T> FatDirEntryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn from_directory(root: Directory<'a, T>) -> Self {
        let cluster = root.dir_info.start_cluster;
        let fs = &root.fs;

        FatDirEntryIterator {
            counter: 0,
            block_index: 0,
            is_first: true,
            cluster_iter: BlockIndexClusterIter::new(fs, cluster, None),
            last_cluster: None,
        }
    }
}

impl<'a, T> DirectoryEntryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn new(root: Directory<'a, T>) -> Self {
        DirectoryEntryIterator {
            directory_cluster: root.dir_info.start_cluster,
            raw_iter: FatDirEntryIterator::from_directory(root),
        }
    }
}

