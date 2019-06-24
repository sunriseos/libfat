//! FAT directory managment.

use arrayvec::ArrayString;

use crate::FatError;
use crate::FatFileSystemResult;
use storage_device::StorageDevice;

use super::attribute::Attributes;
use super::cluster::Cluster;
use super::name::ShortFileName;
use super::name::ShortFileNameContext;
use super::offset_iter::ClusterOffsetIter;
use super::utils;

use super::table;

use super::FatFileSystem;
use super::FatFsType;

pub mod dir_entry;
pub mod dir_entry_iterator;
pub(crate) mod raw_dir_entry;
pub(crate) mod raw_dir_entry_iterator;

use dir_entry::{DirectoryEntry, DirectoryEntryRawInfo};

use dir_entry_iterator::DirectoryEntryIterator;
use raw_dir_entry_iterator::FatDirEntryIterator;

use crate::utils::FileSystemIterator;

/// Represent a Directory.
pub struct Directory<'a, S: StorageDevice> {
    /// The information about this directory.
    dir_info: DirectoryEntry,

    /// A reference to the filesystem.
    fs: &'a FatFileSystem<S>,
}

/// Represent a File.
pub struct File {
    /// The information about this file.
    pub file_info: DirectoryEntry,
}

impl<'a, S: StorageDevice> Directory<'a, S> {
    /// Helper to determine if the directory is the root directory.
    pub fn is_root_directory(&self) -> bool {
        self.dir_info.raw_info.is_none()
    }

    /// Create a directory from a filesystem reference and a directory entry.
    pub fn from_entry(fs: &'a FatFileSystem<S>, dir_info: DirectoryEntry) -> Self {
        Directory { dir_info, fs }
    }

    /// Search an entry inside the directory and if found return it.
    pub fn find_entry(&self, name: &str) -> FatFileSystemResult<DirectoryEntry> {
        if name.len() > DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE {
            return Err(FatError::PathTooLong);
        }

        let mut lowercase_name: ArrayString<[u8; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]> =
            ArrayString::new();
        for c in name.chars() {
            lowercase_name.push(c.to_lowercase().next().unwrap());
        }

        let mut iter = self.iter();
        while let Some(entry) = iter.next(self.fs) {
            let entry = entry?;

            let mut file_name: ArrayString<[u8; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]> =
                ArrayString::new();

            for c in entry.file_name.as_str().chars() {
                file_name.push(c.to_lowercase().next().unwrap());
            }

            if file_name.as_str() == lowercase_name.as_str() {
                return Ok(entry);
            }
        }

        Err(FatError::NotFound)
    }

    /// Recursively search for an entry.
    pub(crate) fn search_entry(&self, path: &str) -> FatFileSystemResult<DirectoryEntry> {
        let (name, rest_opt) = utils::split_path(path);

        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        match rest_opt {
            Some(rest) => Directory::from_entry(fs, child_entry).search_entry(rest),
            None => Ok(child_entry),
        }
    }

    /// Open a file a the given path.
    pub fn open_file(self, path: &str) -> FatFileSystemResult<File> {
        let (name, rest_opt) = utils::split_path(path);
        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        match rest_opt {
            Some(rest) => {
                if child_entry.attribute.is_directory() {
                    Err(FatError::NotAFile)
                } else {
                    Directory::from_entry(fs, child_entry).open_file(rest)
                }
            }
            None => Ok(File::from_entry(child_entry)),
        }
    }

    /// Open a directory at the given path.
    pub fn open_directory(self, path: &str) -> FatFileSystemResult<Directory<'a, S>> {
        let (name, rest_opt) = utils::split_path(path);

        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        if !child_entry.attribute.is_directory() {
            return Err(FatError::NotADirectory);
        }

        match rest_opt {
            Some(rest) => Directory::from_entry(fs, child_entry).open_directory(rest),
            None => Ok(Directory::from_entry(fs, child_entry)),
        }
    }

    /// Search space to allocate a directory entry and return a raw entry iterator to it.
    fn allocate_entries(
        entry: &DirectoryEntry,
        fs: &'a FatFileSystem<S>,
        count: u32,
    ) -> FatFileSystemResult<FatDirEntryIterator> {
        let mut i = 0;
        let directory = Directory::from_entry(fs, *entry);
        let is_root_directory = directory.is_root_directory();

        let mut fat_dir_entry_iter = directory.fat_dir_entry_iter();
        while let Some(raw_dir_entry) = fat_dir_entry_iter.next(fs) {
            let raw_dir_entry = raw_dir_entry?;
            if raw_dir_entry.is_free() || raw_dir_entry.is_deleted() {
                i += 1;
                if i == count {
                    return Ok(FatDirEntryIterator::new(
                        fs,
                        raw_dir_entry.entry_cluster,
                        raw_dir_entry.entry_cluster_offset,
                        raw_dir_entry.entry_offset,
                        is_root_directory,
                    ));
                }
            }
        }

        if is_root_directory {
            match fs.boot_record.fat_type {
                FatFsType::Fat12 | FatFsType::Fat16 => return Err(FatError::NoSpaceLeft),
                _ => {}
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

        Ok(FatDirEntryIterator::new(
            fs,
            new_cluster,
            0,
            0,
            is_root_directory,
        ))
    }

    /// Create a directory entry in a given parent directory.
    fn create_dir_entry(
        fs: &'a FatFileSystem<S>,
        parent_entry: &DirectoryEntry,
        attribute: Attributes,
        name: &str,
        cluster: Cluster,
        file_size: u32,
    ) -> FatFileSystemResult<DirectoryEntry> {
        let is_special_entry = name == "." || name == "..";
        let mut count: u32 = 1;

        let mut free_entries_iter = Self::allocate_entries(parent_entry, fs, count)?;

        let mut first_raw_dir_entry = None;

        let short_file_name;
        if !is_special_entry {
            let mut context: ShortFileNameContext = ShortFileNameContext::default();
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

                let mut lfn_entry = free_entries_iter.next(fs).unwrap()?;
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

        let mut sfn_entry = free_entries_iter.next(fs).unwrap()?;
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

        let is_in_old_root_directory = match fs.boot_record.fat_type {
            FatFsType::Fat12 | FatFsType::Fat16 => cluster.0 == 0,
            _ => false,
        };

        Ok(DirectoryEntry::from_sfn(
            sfn_entry,
            Some(DirectoryEntryRawInfo::new(
                first_raw_dir_entry.entry_cluster,
                first_raw_dir_entry.entry_cluster_offset,
                first_raw_dir_entry.entry_offset,
                count,
                is_in_old_root_directory,
            )),
            file_name,
        ))
    }

    /// Delete a directory entry in a given parent directory.
    fn delete_dir_entry(
        fs: &'a FatFileSystem<S>,
        dir_entry: &DirectoryEntry,
    ) -> FatFileSystemResult<()> {
        if let Some(raw_info) = dir_entry.raw_info {
            let mut offset_iter = FatDirEntryIterator::new(
                fs,
                raw_info.parent_cluster,
                raw_info.first_entry_cluster_offset,
                raw_info.first_entry_offset,
                raw_info.in_old_fat_root_directory,
            );

            let mut i = 0;
            while i < raw_info.entry_count {
                if let Some(block_res) = offset_iter.next(fs) {
                    let mut res = block_res?;

                    res.set_deleted();
                    res.flush(fs)?;
                    i += 1;
                } else {
                    return Err(FatError::ReadFailed);
                }
            }
        }

        Ok(())
    }

    /// Create a directory with the given name.
    pub fn create_directory(&mut self, name: &str) -> FatFileSystemResult<()> {
        if name.len() > DirectoryEntry::MAX_FILE_NAME_LEN {
            return Err(FatError::PathTooLong);
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

    /// Create a file with the given name.
    pub fn create_file(&mut self, name: &str) -> FatFileSystemResult<()> {
        if name.len() > DirectoryEntry::MAX_FILE_NAME_LEN {
            return Err(FatError::PathTooLong);
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

    /// Delete a file at the given path.
    pub fn delete_file(self, path: &str) -> FatFileSystemResult<()> {
        self.unlink(path, false)
    }

    /// Delete a directory at the given path.
    pub fn delete_directory(self, path: &str) -> FatFileSystemResult<()> {
        self.unlink(path, true)
    }

    /// Delete a directory or a file with the given name.
    fn unlink(self, name: &str, is_dir: bool) -> FatFileSystemResult<()> {
        let fs = self.fs;

        let dir_entry = self.find_entry(name)?;

        if dir_entry.attribute.is_directory() != is_dir {
            if is_dir {
                return Err(FatError::NotADirectory);
            }

            return Err(FatError::NotAFile);
        }

        // Check for directory not being empty
        if dir_entry.attribute.is_directory()
            && Self::from_entry(fs, dir_entry)
                .iter()
                .nth(self.fs, 2)
                .is_some()
        {
            return Err(FatError::AccessDenied);
        }

        Self::delete_dir_entry(fs, &dir_entry)?;

        if dir_entry.start_cluster.0 != 0 {
            fs.free_cluster(dir_entry.start_cluster, None)?;
        }

        Ok(())
    }

    /// Rename a directory or a file from a given name to another one.
    // FIXME: better error managment
    pub fn rename(
        self,
        dir_entry: DirectoryEntry,
        new_name: &str,
        is_dir: bool,
    ) -> FatFileSystemResult<()> {
        if new_name.len() > DirectoryEntry::MAX_FILE_NAME_LEN {
            return Err(FatError::PathTooLong);
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
                old_raw_info.first_entry_cluster_offset,
                old_raw_info.first_entry_offset,
                old_raw_info.in_old_fat_root_directory,
            );

            let mut context: ShortFileNameContext = ShortFileNameContext::default();
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

                let mut lfn_entry = entries_iter.next(self.fs).unwrap()?;

                lfn_entry.clear();
                lfn_entry.set_attribute(Attributes::new(Attributes::LFN));
                lfn_entry.set_lfn_index(lfn_index);
                lfn_entry.set_lfn_entry(&new_name[(target_index - 1) as usize * 13..]);
                lfn_entry.set_lfn_checksum(sfn_checksum as u8);
                lfn_entry.flush(self.fs)?;
            }

            let mut sfn_entry = entries_iter.next(self.fs).unwrap()?;
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
                let mut iter = FatDirEntryIterator::new(
                    self.fs,
                    new_entry.start_cluster,
                    0,
                    0,
                    new_raw_info.in_old_fat_root_directory,
                );

                // FIXME: is that always the second entry?
                let mut second_entry = iter.nth(self.fs, 2).unwrap()?;
                second_entry.set_cluster(new_parent_cluster);
                second_entry.flush(self.fs)?;
            }
        }

        Self::delete_dir_entry(self.fs, &dir_entry)?;
        Ok(())
    }
}

impl<'a, S: StorageDevice> Directory<'a, S> {
    /// Create a raw directory entry iterator from the directory.
    pub(crate) fn fat_dir_entry_iter(&self) -> FatDirEntryIterator {
        FatDirEntryIterator::from_directory(self)
    }
    /// Create a directory entry iterator from the directory.
    pub fn iter(&self) -> DirectoryEntryIterator {
        DirectoryEntryIterator::new(self)
    }
}

impl FatDirEntryIterator {
    /// Create a raw directory entry iterator from a directory.
    pub fn from_directory<S: StorageDevice>(root: &Directory<'_, S>) -> Self {
        let cluster = root.dir_info.start_cluster;
        let fs = &root.fs;

        let cluster_iter = if root.is_root_directory() {
            match fs.boot_record.fat_type {
                FatFsType::Fat12 | FatFsType::Fat16 => None,
                FatFsType::Fat32 => Some(ClusterOffsetIter::new(fs, cluster, None)),
                _ => unimplemented!(),
            }
        } else {
            Some(ClusterOffsetIter::new(fs, cluster, None))
        };

        FatDirEntryIterator {
            counter: 0,
            cluster_offset: 0,
            is_first: true,
            cluster_iter,
            last_cluster: None,
        }
    }
}

impl DirectoryEntryIterator {
    /// Create a directory entry iterator from a directory.
    pub fn new<S: StorageDevice>(root: &Directory<'_, S>) -> Self {
        DirectoryEntryIterator {
            raw_iter: FatDirEntryIterator::from_directory(root),
        }
    }
}

impl File {
    /// Create a file from a filesystem reference and a directory entry.
    pub fn from_entry(file_info: DirectoryEntry) -> Self {
        File { file_info }
    }

    /// Check offset range for a given fat_type.
    fn check_range(offset: u64, fs_type: FatFsType) -> FatFileSystemResult<()> {
        let max_size = match fs_type {
            FatFsType::Fat12 => 0x01FF_FFFF,
            FatFsType::Fat16 => 0x7FFF_FFFF,
            FatFsType::Fat32 => 0xFFFF_FFFF,
            _ => unimplemented!(),
        };

        if offset > max_size {
            return Err(FatError::AccessDenied);
        }

        Ok(())
    }

    /// Read at a given offset of the file into a given buffer.
    pub fn read<S: StorageDevice>(
        &mut self,
        fs: &FatFileSystem<S>,
        offset: u64,
        buf: &mut [u8],
    ) -> FatFileSystemResult<u64> {
        if Self::check_range(offset, fs.boot_record.fat_type).is_err() {
            return Ok(0);
        }

        if offset >= u64::from(self.file_info.file_size) {
            return Ok(0);
        }

        let blocks_per_cluster = u64::from(fs.boot_record.blocks_per_cluster());
        let block_size = u64::from(fs.boot_record.bytes_per_block());
        let mut cluster_offset_iterator =
            ClusterOffsetIter::new(fs, self.file_info.start_cluster, Some(offset / block_size));

        let mut read_size = 0u64;

        while read_size < buf.len() as u64 {
            let cluster_opt = cluster_offset_iterator.next(fs);
            if cluster_opt.is_none() {
                break;
            }

            let cluster = cluster_opt.unwrap();

            let mut buf_limit = block_size;

            let bytes_left = u64::from(self.file_info.file_size) - read_size - offset;

            if bytes_left == 0 {
                break;
            } else if bytes_left < buf_limit {
                buf_limit = bytes_left;
            }

            if buf_limit > buf.len() as u64 {
                buf_limit = buf.len() as u64;
            }

            let mut buf_slice = &mut buf[read_size as usize..(read_size + buf_limit) as usize];

            let cluster_offset = cluster.to_data_bytes_offset(fs);

            fs.storage_device
                .lock()
                .read(
                    fs.partition_start
                        + cluster_offset
                        + (offset % block_size)
                        + (read_size % (block_size * blocks_per_cluster)),
                    &mut buf_slice,
                )
                .or(Err(FatError::ReadFailed))?;

            read_size += buf_slice.len() as u64;
        }

        Ok(read_size)
    }

    /// Write the given buffer at a given offset of the file.
    pub fn write<S: StorageDevice>(
        &mut self,
        fs: &FatFileSystem<S>,
        offset: u64,
        buf: &[u8],
        appendable: bool,
    ) -> FatFileSystemResult<()> {
        Self::check_range(offset, fs.boot_record.fat_type)?;

        let min_size = offset + buf.len() as u64;
        if min_size > u64::from(self.file_info.file_size) {
            if appendable {
                self.set_len(fs, min_size)?;
            } else {
                return Err(FatError::AccessDenied);
            }
        }

        let blocks_per_cluster = u64::from(fs.boot_record.blocks_per_cluster());
        let block_size = u64::from(fs.boot_record.bytes_per_block());
        let mut cluster_offset_iterator =
            ClusterOffsetIter::new(fs, self.file_info.start_cluster, Some(offset / block_size));

        let mut write_size = 0u64;

        while write_size < buf.len() as u64 {
            let cluster_opt = cluster_offset_iterator.next(fs);
            if cluster_opt.is_none() {
                break;
            }

            let cluster = cluster_opt.unwrap();

            let mut buf_limit = block_size;

            if buf_limit > (&buf[write_size as usize..]).len() as u64 {
                buf_limit = (&buf[write_size as usize..]).len() as u64;
            }

            let buf_slice = &buf[write_size as usize..(write_size + buf_limit) as usize];

            let cluster_offset = cluster.to_data_bytes_offset(fs);

            fs.storage_device
                .lock()
                .write(
                    fs.partition_start
                        + cluster_offset
                        + (offset % block_size)
                        + (write_size % (block_size * blocks_per_cluster)),
                    &buf_slice,
                )
                .or(Err(FatError::WriteFailed))?;
            write_size += buf_slice.len() as u64;
        }

        Ok(())
    }

    /// Set the file length
    pub fn set_len<S: StorageDevice>(
        &mut self,
        fs: &FatFileSystem<S>,
        size: u64,
    ) -> FatFileSystemResult<()> {
        let current_len = u64::from(self.file_info.file_size);
        if size == current_len {
            return Ok(());
        } else if size > 0xFFFF_FFFF {
            return Err(FatError::NoSpaceLeft);
        }

        let raw_file_info = self.file_info.raw_info.ok_or(FatError::Custom {
            name: "Raw Info is missing ON A FILE",
        })?;
        let mut raw_dir_entry = raw_file_info.get_dir_entry(fs)?;

        let cluster_size = u64::from(
            u16::from(fs.boot_record.blocks_per_cluster()) * fs.boot_record.bytes_per_block(),
        );
        let aligned_size = utils::align_up(size, cluster_size);
        let aligned_current_len = utils::align_up(current_len, cluster_size);

        let new_size;

        if size > current_len {
            let diff_size = size - current_len;
            let mut cluster_to_add_count = (aligned_size - aligned_current_len) / cluster_size;
            let mut start_cluster =
                if self.file_info.start_cluster.0 == 0 || self.file_info.file_size == 0 {
                    None
                } else {
                    Some(table::get_last_cluster(fs, self.file_info.start_cluster)?)
                };

            let mut last_cluster = start_cluster;
            let need_update_cluster = start_cluster.is_none();

            while cluster_to_add_count != 0 {
                last_cluster = Some(fs.alloc_cluster(last_cluster)?);
                if start_cluster.is_none() {
                    start_cluster = last_cluster;
                }
                cluster_to_add_count -= 1;
            }

            new_size = self.file_info.file_size + diff_size as u32;
            if need_update_cluster {
                self.file_info.start_cluster = start_cluster.unwrap();
            }
        } else {
            let diff_size = current_len - size;
            let mut cluster_to_remove_count = (aligned_current_len - aligned_size) / cluster_size;

            while cluster_to_remove_count != 0 {
                let (last_cluster, previous_cluster) =
                    table::get_last_and_previous_cluster(fs, self.file_info.start_cluster)?;
                fs.free_cluster(last_cluster, previous_cluster)?;
                cluster_to_remove_count -= 1;
            }

            new_size = self.file_info.file_size - diff_size as u32;
        }
        // TODO: update modified date?
        // BODY: We should update the modification date here at some point.
        raw_dir_entry.set_cluster(self.file_info.start_cluster);
        raw_dir_entry.set_file_size(new_size);
        raw_dir_entry.flush(fs)?;

        self.file_info.file_size = new_size;

        Ok(())
    }
}
