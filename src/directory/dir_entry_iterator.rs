//! High level directory entry iterator.
use arrayvec::ArrayString;

use crate::name::LongFileName;

use libfs::storage::StorageDevice;
use libfs::FileSystemResult;

use super::dir_entry::DirectoryEntry;
use super::dir_entry::DirectoryEntryRawInfo;
use super::raw_dir_entry::FatDirEntry;
use super::raw_dir_entry_iterator::FatDirEntryIterator;

/// Represent a directory entries iterator.
pub struct DirectoryEntryIterator<'a, S: StorageDevice> {
    /// The raw directory entries (8.3/VFAT entries) iterator.
    pub(crate) raw_iter: FatDirEntryIterator<'a, S>,
}

impl<'a, S: StorageDevice> Iterator for DirectoryEntryIterator<'a, S>
{
    type Item = FileSystemResult<DirectoryEntry>;
    fn next(&mut self) -> Option<FileSystemResult<DirectoryEntry>> {
        let mut next_is_end_entry = false;
        let mut first_raw_dir_entry: Option<FatDirEntry> = None;
        let mut entry_count = 0;
        let mut lfn_index: i32 = 0;
        let mut file_name = ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]>::new();

        while let Some(entry) = self.raw_iter.next() {
            if let Err(error) = entry {
                return Some(Err(error));
            }

            let entry = entry.unwrap();
            if first_raw_dir_entry.is_none() {
                first_raw_dir_entry = Some(entry);
            }
            entry_count += 1;

            // End of directory
            if entry.is_free() {
                break;
            }

            // Deleted entry? Clear everything and continue
            if entry.is_deleted() {
                lfn_index = 0;
                file_name.clear();
                first_raw_dir_entry = None;
                entry_count = 0;

                continue;
            }

            // LFN
            if let Some(lfn_entry) = entry.long_file_name_raw() {
                let lfn = entry.as_lfn_entry();

                if (lfn.order_entry & 0x40) != 0 {
                    lfn_index = i32::from(lfn.order_entry ^ 0x40);
                }

                let mut part = ArrayString::<[_; LongFileName::MAX_LEN_UNICODE]>::new();
                // FIXME: Custom Iterator to catches those errors
                let raw_name = lfn_entry.chars().unwrap();
                for c in &raw_name {
                    if *c == '\x00' {
                        break;
                    }
                    part.push(*c);
                }

                // We do some kind of push_front by hand
                // FIXME: this is dirty
                let tmp = file_name;
                file_name.clear();
                file_name.push_str(part.as_str());
                file_name.push_str(tmp.as_str());

                let index_minus_one = lfn_index - 1;

                if lfn_index == 0 || index_minus_one <= 1 {
                    next_is_end_entry = true;
                }

                lfn_index = index_minus_one;

                continue;
            }

            if !entry.attribute().is_volume() {
                if !next_is_end_entry {
                    // discard everything that could have previously be done
                    file_name.clear();

                    let raw_name = entry.short_name().unwrap().chars();
                    for c in raw_name.iter().take(8) {
                        if *c == ' ' {
                            break;
                        }
                        file_name.push(*c);
                    }

                    // Short filename with extension
                    if raw_name[8] != ' ' {
                        file_name.push('.');
                        for c in raw_name.iter().skip(8) {
                            if *c == ' ' {
                                break;
                            }
                            file_name.push(*c);
                        }
                    }

                    // unwrap will never fail here
                    file_name =
                        ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]>::from(
                            file_name.trim_end(),
                        )
                        .unwrap();
                }
                if let Some(end_char_index) = file_name.find('\0') {
                    file_name.truncate(end_char_index);
                }

                let first_raw_dir_entry = first_raw_dir_entry.unwrap();

                // only a SFN entry
                return Some(Ok(DirectoryEntry {
                    start_cluster: entry.get_cluster(),
                    raw_info: Some(DirectoryEntryRawInfo::new(
                        first_raw_dir_entry.entry_cluster,
                        first_raw_dir_entry.entry_cluster_offset,
                        first_raw_dir_entry.entry_offset,
                        entry_count,
                        self.raw_iter.cluster_iter.is_none(),
                    )),
                    creation_timestamp: entry.get_creation_datetime().to_unix_time(),
                    last_access_timestamp: entry.get_last_access_date().to_unix_time(),
                    last_modification_timestamp: entry.get_modification_datetime().to_unix_time(),
                    file_size: entry.get_file_size(),
                    file_name,
                    attribute: entry.attribute(),
                }));
            }

            lfn_index = 0;
            next_is_end_entry = false;
            file_name.clear();
            first_raw_dir_entry = None;
            entry_count = 0;
        }

        None
    }
}
