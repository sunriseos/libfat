use arrayvec::ArrayString;

use crate::FileSystemError;
use crate::Result as FileSystemResult;

use super::attribute::Attributes;
use super::block::{BlockDevice, BlockIndex, BlockIndexClusterIter};
use super::cluster::Cluster;
use super::name::ShortFileName;
use super::name::ShortFileNameContext;
use super::utils;

use super::table;

use super::FatFileSystem;

pub mod dir_entry;
pub mod dir_entry_iterator;
pub mod raw_dir_entry;
pub mod raw_dir_entry_iterator;

use dir_entry::{DirectoryEntry, DirectoryEntryRawInfo};

use dir_entry_iterator::DirectoryEntryIterator;
use raw_dir_entry_iterator::FatDirEntryIterator;

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

    fn allocate_entries(
        entry: &DirectoryEntry,
        fs: &'a FatFileSystem<T>,
        count: u32,
    ) -> FileSystemResult<FatDirEntryIterator<'a, T>> {
        let mut i = 0;
        for raw_dir_entry in Directory::from_entry(fs, entry.clone()).fat_dir_entry_iter() {
            let raw_dir_entry = raw_dir_entry?;
            if raw_dir_entry.is_free() || raw_dir_entry.is_deleted() {
                i += 1;
                if i == count {
                    return Ok(FatDirEntryIterator::new(
                        fs,
                        raw_dir_entry.entry_cluster,
                        BlockIndex(raw_dir_entry.entry_index),
                        raw_dir_entry.entry_offset,
                    ));
                }
            }
        }

        // if the directory is full, try to allocate a cluster and use it
        let last_cluster = table::get_last_cluster(fs, entry.start_cluster)?;
        let new_cluster = fs.alloc_cluster(Some(last_cluster))?;

        let clear_res = fs.clean_cluster_data(new_cluster);

        if let Err(error) = clear_res {
            // If it fail here, this can be catastrophic but at least we tried our best.
            fs.free_cluster(new_cluster, Some(last_cluster))?;
            return Err(error);
        }

        Ok(FatDirEntryIterator::new(fs, new_cluster, BlockIndex(0), 0))
    }

    fn create_dir_entry(
        fs: &'a FatFileSystem<T>,
        parent_entry: &DirectoryEntry,
        attribute: Attributes,
        name: &str,
        cluster: Cluster,
    ) -> FileSystemResult<DirectoryEntry> {
        let count = 1;

        let mut free_entries_iter = Self::allocate_entries(parent_entry, fs, count)?;
        let short_file_name;

        let is_special_entry = name == "." || name == "..";
        
        if !is_special_entry {
            // TODO: create_lfn
            let mut context: ShortFileNameContext = Default::default();
            short_file_name = ShortFileName::from_unformated_str(&mut context, name);
        } else {
            short_file_name = ShortFileName::from_data(&name.as_bytes());
        }

        let mut sfn_entry = free_entries_iter.next().unwrap()?;
        sfn_entry.clear();
        sfn_entry.set_file_size(0);
        sfn_entry.set_cluster(cluster);
        sfn_entry.set_attribute(attribute);

        sfn_entry.set_short_name(&short_file_name);
        sfn_entry.flush(fs)?;

        // TODO: change this when lfn is done.
        let first_raw_dir_entry = sfn_entry;
        let file_name = ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN]>::new();

        // TODO: Move this
        Ok(DirectoryEntry {
            start_cluster: sfn_entry.get_cluster(),
            raw_info: Some(DirectoryEntryRawInfo {
                parent_cluster: first_raw_dir_entry.entry_cluster,
                first_entry_block_index: BlockIndex(first_raw_dir_entry.entry_index),
                first_entry_offset: first_raw_dir_entry.entry_offset,
                entry_count: count,
            }),
            creation_timestamp: sfn_entry.get_creation_datetime().to_unix_time(),
            last_access_timestamp: sfn_entry.get_last_access_date().to_unix_time(),
            last_modification_timestamp: sfn_entry.get_modification_datetime().to_unix_time(),
            file_size: sfn_entry.get_file_size(),
            file_name,
            attribute: sfn_entry.attribute(),
        })
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

    pub fn mkdir(&mut self, name: &str) -> FileSystemResult<()> {
        // Allocate a cluster for the directory entries
        let cluster = self.fs.alloc_cluster(None)?;

        let clear_res = self.fs.clean_cluster_data(cluster);

        if let Err(error) = clear_res {
            // If it fail here, this can be catastrophic but at least we tried our best.
            self.fs.free_cluster(cluster, None)?;
            return Err(error);
        }

        let new_entry_res = Self::create_dir_entry(
            self.fs,
            &mut self.dir_info,
            Attributes::new(Attributes::DIRECTORY),
            name,
            cluster,
        );

        // Cannot create directory?
        if let Err(err) = new_entry_res {
            self.fs.free_cluster(cluster, None)?;
            return Err(err);
        }

        let entry = new_entry_res?;

        // FIXME: check the error here and try to free the cluster if something happens?
        Self::create_dir_entry(
            self.fs,
            &entry,
            Attributes::new(Attributes::DIRECTORY),
            ".",
            entry.start_cluster,
        )?;

        let raw_info = entry.raw_info.unwrap();

        let parent_cluster = if raw_info.parent_cluster == self.fs.get_root_directory().dir_info.start_cluster {
            Cluster(0)
        } else {
            raw_info.parent_cluster
        };

        Self::create_dir_entry(
            self.fs,
            &entry,
            Attributes::new(Attributes::DIRECTORY),
            "..",
            parent_cluster,
        )?;
        Ok(())
    }

    pub fn touch(&mut self, name: &str) -> FileSystemResult<()> {
        Self::create_dir_entry(
            self.fs,
            &self.dir_info,
            Attributes::new(0),
            name,
            Cluster(0),
        )?;

        Ok(())
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
