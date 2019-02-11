pub mod detail;

use detail::block::BlockDevice;
use detail::filesystem::FatFileSystem;

use crate::Result as FileSystemResult;
use crate::{
    DirFilterFlags, DirectoryOperations, DirectoryEntry, DirectoryEntryType, FileModeFlags, FileOperations, FileSystemError,
    FileSystemOperations,
};

impl<B> FileSystemOperations for FatFileSystem<B>
where
    B: BlockDevice,
{
    fn create_file(name: &str, mode: FileModeFlags, size: u64) -> FileSystemResult<()> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn delete_file(name: &str) -> FileSystemResult<()> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn open_file<T>(name: &str, mode: FileModeFlags) -> FileSystemResult<T>
    where
        T: FileOperations,
    {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn open_directory<T>(name: &str, filter: DirFilterFlags) -> FileSystemResult<T>
    where
        T: DirectoryOperations,
    {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn delete_directory(name: &str) -> FileSystemResult<()> {
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