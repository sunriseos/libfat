use arrayvec::ArrayString;

use libfs::block::{BlockDevice, BlockIndex};
use libfs::FileSystemError;
use libfs::FileSystemResult;

use super::attribute::Attributes;
use super::block_iter::BlockIndexClusterIter;
use super::cluster::Cluster;
use super::name::ShortFileName;
use super::name::ShortFileNameContext;
use super::utils;

use super::table;

use super::FatFileSystem;

pub mod dir_entry;
pub mod dir_entry_iterator;
pub(crate) mod raw_dir_entry;
pub(crate) mod raw_dir_entry_iterator;

use dir_entry::{DirectoryEntry, DirectoryEntryRawInfo};

use dir_entry_iterator::DirectoryEntryIterator;
use raw_dir_entry_iterator::FatDirEntryIterator;

#[derive(Copy)]
pub struct Directory<'a, T> {
    dir_info: DirectoryEntry,
    fs: &'a FatFileSystem<T>,
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
        if name.len() > DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE {
            return Err(FileSystemError::PathTooLong);
        }

        let mut lowercase_name: ArrayString<[u8; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]> = ArrayString::new();
        for c in name.chars() {
            lowercase_name.push(c.to_lowercase().next().unwrap());
        }

        for entry in self.iter() {
            let entry = entry?;

            let mut file_name: ArrayString<[u8; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]> = ArrayString::new();

            for c in entry.file_name.as_str().chars() {
                file_name.push(c.to_lowercase().next().unwrap());
            }

            if file_name.as_str() == lowercase_name.as_str() {
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
        for raw_dir_entry in Directory::from_entry(fs, *entry).fat_dir_entry_iter() {
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
        file_size: u32,
    ) -> FileSystemResult<DirectoryEntry> {
        let is_special_entry = name == "." || name == "..";
        let mut count: u32 = 1;

        let mut free_entries_iter = Self::allocate_entries(parent_entry, fs, count)?;

        let mut first_raw_dir_entry = None;

        let short_file_name;
        if !is_special_entry {
            let mut context: ShortFileNameContext = Default::default();
            short_file_name = ShortFileName::from_unformated_str(&mut context, name);

            let lfn_count = (name.len() as u32 + 12) / 13;
            let sfn_checksum = ShortFileName::checksum_lfn(&short_file_name.as_bytes());

            for index in 0..lfn_count {
                let target_index = lfn_count - index;
                let lfn_index = if target_index == lfn_count {
                    0x40u8 + target_index as u8
                } else {
                    target_index as u8
                };

                let mut lfn_entry = free_entries_iter.next().unwrap()?;
                if first_raw_dir_entry.is_none() {
                    first_raw_dir_entry = Some(lfn_entry);
                }

                lfn_entry.clear();
                lfn_entry.set_attribute(Attributes::new(Attributes::LFN));
                lfn_entry.set_lfn_index(lfn_index);
                lfn_entry.set_lfn_entry(&name[(target_index - 1) as usize * 13..]);
                lfn_entry.set_lfn_checksum(sfn_checksum as u8);
                lfn_entry.flush(fs)?;
            }

            count += lfn_count;
        } else {
            short_file_name = ShortFileName::from_data(&name.as_bytes());
        }

        let mut sfn_entry = free_entries_iter.next().unwrap()?;
        sfn_entry.clear();
        sfn_entry.set_file_size(file_size);
        sfn_entry.set_cluster(cluster);
        sfn_entry.set_attribute(attribute);

        sfn_entry.set_short_name(&short_file_name);
        sfn_entry.flush(fs)?;

        let file_name = ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]>::new();

        if first_raw_dir_entry.is_none() {
            first_raw_dir_entry = Some(sfn_entry);
        }

        let first_raw_dir_entry = first_raw_dir_entry.unwrap();

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
        if name.len() > DirectoryEntry::MAX_FILE_NAME_LEN {
            return Err(FileSystemError::PathTooLong);
        }

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
            &self.dir_info,
            Attributes::new(Attributes::DIRECTORY),
            name,
            cluster,
            0,
        );

        // Cannot create directory?
        if let Err(err) = new_entry_res {
            self.fs.free_cluster(cluster, None)?;
            return Err(err);
        }

        let entry = new_entry_res?;

        let res = Self::create_dir_entry(
            self.fs,
            &entry,
            Attributes::new(Attributes::DIRECTORY),
            ".",
            entry.start_cluster,
            0,
        );

        if let Err(err) = res {
            // If it fail here, this can be catastrophic but at least we tried our best.
            Self::delete_dir_entry(self.fs, &entry)?;
            self.fs.free_cluster(cluster, None)?;
            return Err(err);
        }

        let raw_info = entry.raw_info.unwrap();

        let parent_cluster =
            if raw_info.parent_cluster == self.fs.get_root_directory().dir_info.start_cluster {
                Cluster(0)
            } else {
                raw_info.parent_cluster
            };

        let res = Self::create_dir_entry(
            self.fs,
            &entry,
            Attributes::new(Attributes::DIRECTORY),
            "..",
            parent_cluster,
            0,
        );

        if let Err(err) = res {
            // If it fail here, this can be catastrophic but at least we tried our best.
            Self::delete_dir_entry(self.fs, &entry)?;
            self.fs.free_cluster(cluster, None)?;
            return Err(err);
        }

        Ok(())
    }

    pub fn touch(&mut self, name: &str) -> FileSystemResult<()> {
        if name.len() > DirectoryEntry::MAX_FILE_NAME_LEN {
            return Err(FileSystemError::PathTooLong);
        }

        Self::create_dir_entry(
            self.fs,
            &self.dir_info,
            Attributes::new(0),
            name,
            Cluster(0),
            0,
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

    // FIXME: better error managment
    pub fn rename(
        self,
        dir_entry: DirectoryEntry,
        new_name: &str,
        is_dir: bool,
    ) -> FileSystemResult<()> {
        if new_name.len() > DirectoryEntry::MAX_FILE_NAME_LEN {
            return Err(FileSystemError::PathTooLong);
        }
        let old_raw_info = dir_entry.raw_info.unwrap();

        let new_entry_count = ((new_name.len() as u32 + 12) / 13) + 1;

        // can we update in place?
        if old_raw_info.entry_count == new_entry_count
            && old_raw_info.parent_cluster == self.dir_info.start_cluster
        {
            let mut entries_iter = FatDirEntryIterator::new(
                self.fs,
                old_raw_info.parent_cluster,
                old_raw_info.first_entry_block_index,
                old_raw_info.first_entry_offset,
            );

            let mut context: ShortFileNameContext = Default::default();
            let short_file_name = ShortFileName::from_unformated_str(&mut context, new_name);

            let lfn_count = (new_name.len() as u32 + 12) / 13;
            let sfn_checksum = ShortFileName::checksum_lfn(&short_file_name.as_bytes());

            for index in 0..lfn_count {
                let target_index = lfn_count - index;
                let lfn_index = if target_index == lfn_count {
                    0x40u8 + target_index as u8
                } else {
                    target_index as u8
                };

                let mut lfn_entry = entries_iter.next().unwrap()?;

                lfn_entry.clear();
                lfn_entry.set_attribute(Attributes::new(Attributes::LFN));
                lfn_entry.set_lfn_index(lfn_index);
                lfn_entry.set_lfn_entry(&new_name[(target_index - 1) as usize * 13..]);
                lfn_entry.set_lfn_checksum(sfn_checksum as u8);
                lfn_entry.flush(self.fs)?;
            }

            let mut sfn_entry = entries_iter.next().unwrap()?;
            sfn_entry.set_short_name(&short_file_name);
            sfn_entry.flush(self.fs)?;
            return Ok(());
        }

        let new_entry = Self::create_dir_entry(
            self.fs,
            &self.dir_info,
            dir_entry.attribute,
            new_name,
            dir_entry.start_cluster,
            dir_entry.file_size,
        )?;

        if is_dir {
            let new_raw_info = new_entry.raw_info.unwrap();

            let old_parent_cluster = if old_raw_info.parent_cluster
                == self.fs.get_root_directory().dir_info.start_cluster
            {
                Cluster(0)
            } else {
                old_raw_info.parent_cluster
            };

            let new_parent_cluster = if new_raw_info.parent_cluster
                == self.fs.get_root_directory().dir_info.start_cluster
            {
                Cluster(0)
            } else {
                new_raw_info.parent_cluster
            };

            // We aren't in the same dir?
            if new_parent_cluster != old_parent_cluster {
                let mut iter =
                    FatDirEntryIterator::new(self.fs, new_entry.start_cluster, BlockIndex(0), 0);

                // FIXME: is that always the second entry?
                let mut second_entry = iter.nth(2).unwrap()?;
                second_entry.set_cluster(new_parent_cluster);
                second_entry.flush(self.fs)?;
            }
        }

        Self::delete_dir_entry(self.fs, &dir_entry)?;
        Ok(())
    }
}

impl<'a, T> Directory<'a, T>
where
    T: BlockDevice,
{
    pub(crate) fn fat_dir_entry_iter(self) -> FatDirEntryIterator<'a, T> {
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
            raw_iter: FatDirEntryIterator::from_directory(root),
        }
    }
}
