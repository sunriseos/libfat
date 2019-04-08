//! High level directory entry representation.
use arrayvec::ArrayString;

use crate::attribute::Attributes;
use crate::block_iter::BlockIndexClusterIter;
use crate::cluster::Cluster;
use crate::filesystem::FatFileSystem;
use crate::table;
use crate::utils;
use crate::FatFsType;

use libfs::block::{Block, BlockDevice, BlockIndex};
use libfs::FileSystemError;
use libfs::FileSystemResult;

use super::raw_dir_entry::FatDirEntry;

#[derive(Debug, Clone, Copy)]
/// Represent the information of a child entry into in it parent entry.
pub(crate) struct DirectoryEntryRawInfo {
    /// Marker on entries present in the FAT12/FAT16 root directory.
    pub in_old_fat_root_directory: bool,

    /// The first cluster of the parent entry.
    pub parent_cluster: Cluster,

    /// The first raw entry block index of the child entry in the parent entry.
    pub first_entry_block_index: BlockIndex,

    /// The first raw entry offset of the child entry inside the block in the parent entry.
    pub first_entry_offset: u32,

    /// The count of raw entries used by the child entry.
    pub entry_count: u32,
}

impl DirectoryEntryRawInfo {
    /// Create a new directory entry raw info.
    pub fn new(
        parent_cluster: Cluster,
        first_entry_block_index: BlockIndex,
        first_entry_offset: u32,
        entry_count: u32,
        in_old_fat_root_directory: bool,
    ) -> Self {
        DirectoryEntryRawInfo {
            parent_cluster,
            first_entry_block_index,
            first_entry_offset,
            entry_count,
            in_old_fat_root_directory,
        }
    }
}

#[derive(Debug, Clone, Copy)]
/// A high level representation of a directory/file in the directory.
pub struct DirectoryEntry {
    /// The first cluster used for
    pub(crate) start_cluster: Cluster,

    /// The raw informations of the entry inside it parent.
    pub(crate) raw_info: Option<DirectoryEntryRawInfo>,

    /// The creation UNIX timestamp of the entry.
    pub creation_timestamp: u64,

    /// The last access UNIX timestamp of the entry.
    pub last_access_timestamp: u64,

    /// The last modification UNIX timestamp of the entry.
    pub last_modification_timestamp: u64,

    /// The file size of the entry.
    pub file_size: u32,

    /// The file name of the entry.
    pub file_name: ArrayString<[u8; Self::MAX_FILE_NAME_LEN_UNICODE]>,

    /// The attributes of the entry.
    pub attribute: Attributes,
}

impl DirectoryEntry {
    /// The max size of a VFAT long name.
    pub const MAX_FILE_NAME_LEN: usize = 255;

    /// The max size of a VFAT long name encoded as Unicode.
    // we actually use 256 unicode char because arrayvec doesn't define an implementation for Array<[u8; 1020]>
    pub const MAX_FILE_NAME_LEN_UNICODE: usize = 1024;

    /// Create a directory entry from SFN data.
    pub(crate) fn from_sfn(
        sfn_entry: FatDirEntry,
        raw_info: Option<DirectoryEntryRawInfo>,
        file_name: ArrayString<[u8; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]>,
    ) -> Self {
        DirectoryEntry {
            start_cluster: sfn_entry.get_cluster(),
            raw_info,
            creation_timestamp: sfn_entry.get_creation_datetime().to_unix_time(),
            last_access_timestamp: sfn_entry.get_last_access_date().to_unix_time(),
            last_modification_timestamp: sfn_entry.get_modification_datetime().to_unix_time(),
            file_size: sfn_entry.get_file_size(),
            file_name,
            attribute: sfn_entry.attribute(),
        }
    }

    /// Check offset range for a given fat_type.
    fn check_range(offset: u64, fs_type: FatFsType) -> FileSystemResult<()> {
        let max_size = match fs_type {
            FatFsType::Fat12 => 0x01FF_FFFF,
            FatFsType::Fat16 => 0x7FFF_FFFF,
            FatFsType::Fat32 => 0xFFFF_FFFF,
            _ => unimplemented!()
        };

        if offset > max_size {
            return Err(FileSystemError::AccessDenied);
        }

        Ok(())
    }

    /// Read at a given offset of the file into a given buffer.
    pub fn read<'a, T>(
        &mut self,
        fs: &'a FatFileSystem<T>,
        offset: u64,
        buf: &mut [u8],
    ) -> FileSystemResult<u64>
    where
        T: BlockDevice,
    {
        if Self::check_range(offset, fs.boot_record.fat_type).is_err() {
            return Ok(0);
        }

        if offset >= u64::from(self.file_size) {
            return Ok(0);
        }

        let device: &T = &fs.block_device;

        let mut raw_tmp_offset = offset as u64;
        let mut cluster_offset = BlockIndex(raw_tmp_offset / Block::LEN_U64);
        let mut cluster_block_iterator =
            BlockIndexClusterIter::new(fs, self.start_cluster, Some(cluster_offset));
        let blocks_per_cluster = u64::from(fs.boot_record.blocks_per_cluster());

        let mut read_size = 0u64;
        let mut blocks = [Block::new()];

        while read_size < buf.len() as u64 {
            let cluster_opt = cluster_block_iterator.next();
            if cluster_opt.is_none() {
                break;
            }

            cluster_offset = BlockIndex(raw_tmp_offset / Block::LEN_U64);

            let cluster = cluster_opt.unwrap();
            let block_start_index = cluster.to_data_block_index(fs);
            let tmp_index = cluster_offset.0 % blocks_per_cluster;
            let tmp_offset = raw_tmp_offset % Block::LEN_U64;

            device
                .read(
                    &mut blocks,
                    fs.partition_start,
                    BlockIndex(block_start_index.0 + tmp_index),
                )
                .or(Err(FileSystemError::ReadFailed))?;

            let buf_slice = &mut buf[read_size as usize..];
            let mut buf_limit = if buf_slice.len() >= Block::LEN {
                Block::LEN
            } else {
                buf_slice.len()
            };

            let bytes_left = (u64::from(self.file_size) - read_size - offset) as usize;
            if bytes_left < buf_limit {
                buf_limit = bytes_left;
            }

            for (index, buf_entry) in buf_slice.iter_mut().take(buf_limit).enumerate() {
                *buf_entry = blocks[0][tmp_offset as usize + index];
            }

            raw_tmp_offset += buf_limit as u64;
            read_size += buf_limit as u64;
        }

        Ok(read_size)
    }

    /// Write the given buffer at a given offset of the file.
    pub fn write<'a, T>(
        &mut self,
        fs: &'a FatFileSystem<T>,
        offset: u64,
        buf: &[u8],
        appendable: bool,
    ) -> FileSystemResult<()>
    where
        T: BlockDevice,
    {
        Self::check_range(offset, fs.boot_record.fat_type)?;

        let min_size = offset + buf.len() as u64;
        if min_size > u64::from(self.file_size) {
            if appendable {
                self.set_len(fs, min_size)?;
            } else {
                return Err(FileSystemError::AccessDenied);
            }
        }

        let device: &T = &fs.block_device;

        let mut raw_tmp_offset = offset as u64;
        let mut cluster_offset = BlockIndex(raw_tmp_offset / Block::LEN_U64);
        let mut cluster_block_iterator =
            BlockIndexClusterIter::new(fs, self.start_cluster, Some(cluster_offset));
        let blocks_per_cluster = u64::from(fs.boot_record.blocks_per_cluster());

        let mut write_size = 0u64;
        let mut blocks = [Block::new()];

        while write_size < buf.len() as u64 {
            let cluster = cluster_block_iterator
                .next()
                .ok_or(FileSystemError::WriteFailed)?;

            cluster_offset = BlockIndex(raw_tmp_offset / Block::LEN_U64);

            let block_start_index = cluster.to_data_block_index(fs);
            let tmp_index = cluster_offset.0 % blocks_per_cluster;
            let tmp_offset = raw_tmp_offset % Block::LEN_U64;

            device
                .read(
                    &mut blocks,
                    fs.partition_start,
                    BlockIndex(block_start_index.0 + tmp_index),
                )
                .or(Err(FileSystemError::ReadFailed))?;

            let buf_slice = &buf[write_size as usize..];
            let buf_limit = if buf_slice.len() >= Block::LEN {
                Block::LEN
            } else {
                buf_slice.len()
            };

            let block_slice = &mut blocks[0][tmp_offset as usize..];

            for (index, buf_entry) in block_slice.iter_mut().take(buf_limit).enumerate() {
                *buf_entry = buf_slice[index];
            }

            device
                .write(
                    &blocks,
                    fs.partition_start,
                    BlockIndex(block_start_index.0 + tmp_index),
                )
                .or(Err(FileSystemError::WriteFailed))?;

            raw_tmp_offset += buf_limit as u64;
            write_size += buf_limit as u64;
        }

        Ok(())
    }

    /// Set the file length
    pub fn set_len<'a, T>(&mut self, fs: &'a FatFileSystem<T>, size: u64) -> FileSystemResult<()>
    where
        T: BlockDevice,
    {
        let current_len = u64::from(self.file_size);
        if size == current_len {
            return Ok(());
        } else if size > 0xFFFF_FFFF {
            return Err(FileSystemError::NoSpaceLeft);
        }

        let raw_file_info = self.raw_info.ok_or(FileSystemError::Custom {
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
            let mut start_cluster = if self.start_cluster.0 == 0 || self.file_size == 0 {
                None
            } else {
                Some(table::get_last_cluster(fs, self.start_cluster)?)
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

            new_size = self.file_size + diff_size as u32;
            if need_update_cluster {
                self.start_cluster = start_cluster.unwrap();
            }
        } else {
            let diff_size = current_len - size;
            let mut cluster_to_remove_count = (aligned_current_len - aligned_size) / cluster_size;

            while cluster_to_remove_count != 0 {
                let (last_cluster, previous_cluster) =
                    table::get_last_and_previous_cluster(fs, self.start_cluster)?;
                fs.free_cluster(last_cluster, previous_cluster)?;
                cluster_to_remove_count -= 1;
            }

            new_size = self.file_size - diff_size as u32;
        }
        // TODO: update modified date?
        raw_dir_entry.set_cluster(self.start_cluster);
        raw_dir_entry.set_file_size(new_size);
        raw_dir_entry.flush(fs)?;

        self.file_size = new_size;

        Ok(())
    }
}

impl DirectoryEntryRawInfo {
    /// Contrust a directory entry from raw data
    pub fn get_dir_entry<T>(&self, fs: &FatFileSystem<T>) -> FileSystemResult<FatDirEntry>
    where
        T: BlockDevice,
    {
        let mut block_iter = super::FatDirEntryIterator::new(
            fs,
            self.parent_cluster,
            self.first_entry_block_index,
            self.first_entry_offset,
            self.in_old_fat_root_directory
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
