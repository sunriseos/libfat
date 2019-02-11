pub mod detail;

use alloc::boxed::Box;
use detail::block::BlockDevice;
use detail::filesystem::FatFileSystem;

use crate::Result as FileSystemResult;
use crate::{
    DirFilterFlags, DirectoryOperations, DirectoryEntry, DirectoryEntryType, FileModeFlags, FileOperations, FileSystemError,
    FileSystemOperations,
};

struct DirectoryReader<'a, T> {
    internal: detail::directory::Directory<'a, T>,
    //internal_iter: detail::directory::DirectoryEntryIterator<'a, T>,
    entry_count: u64
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

    fn open_file<'a, T>(&'a self, name: &str, mode: FileModeFlags) -> FileSystemResult<Box<dyn FileOperations + 'a>> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn open_directory<'a, T>(&'a self, name: &str, filter: DirFilterFlags) -> FileSystemResult<Box<dyn DirectoryOperations + 'a>>
    {
        let internal_directory_res = if name == "/" {
            Ok(self.get_root_directory())
        } else {
            Err(FileSystemError::Custom {
                name: "not implemented",
            })
        };

        let internal_directory = internal_directory_res?;

        //FnMut(&Self::Item) -> bool

        let filter_fn: &Fn(&detail::directory::DirectoryEntry,) -> bool = if (filter & DirFilterFlags::ALL) == DirFilterFlags::ALL {
            &DirectoryFilterPredicate::all
        } else if (filter & DirFilterFlags::DIRECTORY) == DirFilterFlags::DIRECTORY {
            &DirectoryFilterPredicate::dirs
        } else {
            &DirectoryFilterPredicate::files
        };

        let entry_count = internal_directory.iter().filter(filter_fn).count() as u64;

        let mut res_struct = DirectoryReader {
            internal: internal_directory,
            //internal_iter: internal.iter(),
            entry_count
        };

        let res = Box::new(res_struct);
        
        Ok(res as Box<dyn DirectoryOperations + 'a>)
    }

    fn delete_directory(&self, name: &str) -> FileSystemResult<()> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }
}

impl<'a, T> DirectoryOperations for DirectoryReader<'a, T> {
    fn read(&mut self, buf: &mut [DirectoryEntry]) -> FileSystemResult<u64> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn entry_count(&self) -> FileSystemResult<u64> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }
}



impl Into<DirectoryEntry> for detail::directory::DirectoryEntry {
    fn into(self) -> DirectoryEntry {
        let mut path: [u8; DirectoryEntry::PATH_LEN] = [0x0; DirectoryEntry::PATH_LEN];

        let file_size = self.file_size;

        let entry_type = if self.attribute.is_directory() {
            DirectoryEntryType::Directory
        } else {
            DirectoryEntryType::File
        };

        for (index, c) in self.file_name.as_bytes().iter().enumerate().take(DirectoryEntry::PATH_LEN) {
            path[index] = *c;
        }

        DirectoryEntry {
            path,
            entry_type,
            file_size: u64::from(file_size)
        }
    }
}