pub mod detail;

use detail::block::BlockDevice;
use detail::filesystem::FatFileSystem;

use crate::Result as FileSystemResult;
use crate::{
    DirFilterFlags, DirectoryOperations, FileModeFlags, FileOperations, FileSystemError,
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
