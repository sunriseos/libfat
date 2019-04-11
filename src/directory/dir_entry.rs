//! High level directory entry representation.
use arrayvec::ArrayString;

use crate::attribute::Attributes;
use crate::cluster::Cluster;
use crate::filesystem::FatFileSystem;

use crate::FatError;
use crate::FatFileSystemResult;
use storage_device::StorageDevice;

use super::raw_dir_entry::FatDirEntry;

#[derive(Debug, Clone, Copy)]
/// Represent the information of a child entry into in it parent entry.
pub(crate) struct DirectoryEntryRawInfo {
    /// Marker on entries present in the FAT12/FAT16 root directory.
    pub in_old_fat_root_directory: bool,

    /// The first cluster of the parent entry.
    pub parent_cluster: Cluster,

    /// The cluster offset of the first entry in the parent entry.
    pub first_entry_cluster_offset: u64,

    /// The first raw entry offset of the child entry in the parent entry.
    pub first_entry_offset: u64,

    /// The count of raw entries used by the child entry.
    pub entry_count: u32,
}

impl DirectoryEntryRawInfo {
    /// Create a new directory entry raw info.
    pub fn new(
        parent_cluster: Cluster,
        first_entry_cluster_offset: u64,
        first_entry_offset: u64,
        entry_count: u32,
        in_old_fat_root_directory: bool,
    ) -> Self {
        DirectoryEntryRawInfo {
            parent_cluster,
            first_entry_cluster_offset,
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
}

impl DirectoryEntryRawInfo {
    /// Contrust a directory entry from raw data
    pub fn get_dir_entry<S: StorageDevice>(
        &self,
        fs: &FatFileSystem<S>,
    ) -> FatFileSystemResult<FatDirEntry> {
        let mut offset_iter = super::FatDirEntryIterator::new(
            fs,
            self.parent_cluster,
            self.first_entry_cluster_offset,
            self.first_entry_offset,
            self.in_old_fat_root_directory,
        );

        let mut i = 0;
        let mut res = None;

        while i < self.entry_count {
            let result = offset_iter.next();
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
            Err(FatError::NotFound)
        }
    }
}
