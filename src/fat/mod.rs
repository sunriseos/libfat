pub mod detail;

use alloc::boxed::Box;
use core::iter::Iterator;
use detail::block::BlockDevice;
use detail::filesystem::FatFileSystem;

use crate::Result as FileSystemResult;
use crate::{
    DirFilterFlags, DirectoryEntry, DirectoryEntryType, DirectoryOperations, FileModeFlags,
    FileOperations, FileSystemError, FileSystemOperations,
};

struct DirectoryReader<'a, T> {
    internal_iter: detail::directory::DirectoryEntryIterator<'a, T>,
    filter_fn: &'static Fn(&detail::directory::DirectoryEntry) -> bool,
    entry_count: u64,
}

struct DirectoryFilterPredicate;
impl DirectoryFilterPredicate {
    fn all(_entry: &detail::directory::DirectoryEntry) -> bool {
        true
    }

    fn dirs(entry: &detail::directory::DirectoryEntry) -> bool {
        entry.attribute.is_directory()
    }

    fn files(entry: &detail::directory::DirectoryEntry) -> bool {
        !entry.attribute.is_directory()
    }
}

impl<B> FatFileSystem<B>
where
    B: BlockDevice,
{
    fn get_dir_from_path(&self, path: &str) -> FileSystemResult<detail::directory::Directory<B>> {
        trace!("get_dir_from_path {}", path);
        if path == "/" {
            Ok(self.get_root_directory())
        } else {
            Err(FileSystemError::Custom {
                name: "not implemented",
            })
        }
    }
}

impl<B> FileSystemOperations for FatFileSystem<B>
where
    B: BlockDevice,
{
    fn create_file(&self, name: &str, mode: FileModeFlags, size: u64) -> FileSystemResult<()> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn delete_file(&self, name: &str) -> FileSystemResult<()> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn open_file<'a>(
        &'a self,
        name: &str,
        mode: FileModeFlags,
    ) -> FileSystemResult<Box<dyn FileOperations + 'a>> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn open_directory<'a>(
        &'a self,
        name: &str,
        filter: DirFilterFlags,
    ) -> FileSystemResult<Box<dyn DirectoryOperations + 'a>> {
        let filter_fn: &'static Fn(&detail::directory::DirectoryEntry) -> bool =
            if (filter & DirFilterFlags::ALL) == DirFilterFlags::ALL {
                &DirectoryFilterPredicate::all
            } else if (filter & DirFilterFlags::DIRECTORY) == DirFilterFlags::DIRECTORY {
                &DirectoryFilterPredicate::dirs
            } else {
                &DirectoryFilterPredicate::files
            };

        let target_dir = self.get_dir_from_path(name)?;
        let target_dir_clone = target_dir.clone();

        let entry_count = target_dir.iter().filter(filter_fn).count() as u64;

        let res = Box::new(DirectoryReader {
            internal_iter: target_dir_clone.iter(),
            filter_fn,
            entry_count,
        });

        Ok(res as Box<dyn DirectoryOperations + 'a>)
    }

    fn delete_directory(&self, name: &str) -> FileSystemResult<()> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }
}

impl<'a, T> DirectoryOperations for DirectoryReader<'a, T>
where
    T: BlockDevice,
{
    fn read(&mut self, buf: &mut [DirectoryEntry]) -> FileSystemResult<u64> {
        for (index, entry) in buf.iter_mut().enumerate() {
            let mut raw_dir_entry;
            loop {
                let entry_opt = self.internal_iter.next();

                // Prematury ending
                if entry_opt.is_none() {
                    return Ok(index as u64);
                }

                raw_dir_entry = entry_opt.unwrap();
                let filter_fn = self.filter_fn;

                if filter_fn(&raw_dir_entry) {
                    break;
                }
            }

            *entry = raw_dir_entry.into_fs();
        }

        // everything was read correctly
        Ok(buf.len() as u64)
    }

    fn entry_count(&self) -> FileSystemResult<u64> {
        Ok(self.entry_count)
    }
}

impl detail::directory::DirectoryEntry {
    fn into_fs(self) -> DirectoryEntry {
        let mut path: [u8; DirectoryEntry::PATH_LEN] = [0x0; DirectoryEntry::PATH_LEN];

        let file_size = self.file_size;

        let entry_type = if self.attribute.is_directory() {
            DirectoryEntryType::Directory
        } else {
            DirectoryEntryType::File
        };

        for (index, c) in self
            .file_name
            .as_bytes()
            .iter()
            .enumerate()
            .take(DirectoryEntry::PATH_LEN)
        {
            path[index] = *c;
        }

        DirectoryEntry {
            // TODO: add real path
            path,
            entry_type,
            file_size: u64::from(file_size),
        }
    }
}
