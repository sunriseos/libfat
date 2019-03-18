#![feature(alloc)]
#![feature(integer_atomics)]
#![no_std]

#[macro_use]
extern crate alloc;

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate log;

pub mod fat;

use alloc::boxed::Box;

#[derive(Debug)]
pub enum FileSystemError {
    NotFound,
    NoSpaceLeft,
    AccessDenied,
    WriteFailed,
    ReadFailed,
    NoPartitionFound,
    PartitionNotFound,
    NotAFile,
    NotADirectory,
    UnknownPartitionFormat { partition_type: u32 },
    InvalidPartition,
    Custom { name: &'static str },
}

#[derive(Debug, PartialEq)]
pub enum DirectoryEntryType {
    File,
    Directory,
}

pub struct DirectoryEntry {
    pub path: [u8; Self::PATH_LEN],
    pub entry_type: DirectoryEntryType,
    pub file_size: u64,
}

impl DirectoryEntry {
    pub const PATH_LEN: usize = 0x301;
}

bitflags! {
    pub struct FileModeFlags: u32 {
        const READABLE = 0b0000_0001;
        const WRITABLE = 0b0000_0010;
        const APPENDABLE = 0b0000_0100;
    }
}

bitflags! {
    pub struct DirFilterFlags: u32 {
        const DIRECTORY = 0b0000_0001;
        const FILE = 0b0000_0010;
        const ALL = Self::DIRECTORY.bits | Self::FILE.bits;
    }
}

#[derive(Debug)]
pub struct FileTimeStampRaw {
    pub creation_timestamp: u64,
    pub modified_timestamp: u64,
    pub accessed_timestamp: u64,
    pub is_valid: bool,
}

type Result<T> = core::result::Result<T, FileSystemError>;

pub trait FileOperations {
    fn read(&mut self, offset: u64, buf: &mut [u8]) -> Result<u64>;
    fn write(&mut self, offset: u64, buf: &[u8]) -> Result<()>;

    fn flush(&mut self) -> Result<()>;
    fn set_len(&mut self, size: u64) -> Result<()>;
    fn get_len(&mut self) -> Result<u64>;
}

pub trait DirectoryOperations {
    fn read(&mut self, buf: &mut [DirectoryEntry]) -> Result<u64>;
    fn entry_count(&self) -> Result<u64>;
}

pub trait FileSystemOperations {
    fn create_file(&self, path: &str, size: u64) -> Result<()>;
    fn create_directory(&self, path: &str) -> Result<()>;

    fn rename_file(&self, old_path: &str, new_path: &str) -> Result<()>;
    fn rename_directory(&self, old_path: &str, new_path: &str) -> Result<()>;

    fn delete_file(&self, path: &str) -> Result<()>;
    fn delete_directory(&self, path: &str) -> Result<()>;

    fn open_file<'a>(
        &'a self,
        path: &str,
        mode: FileModeFlags,
    ) -> Result<Box<dyn FileOperations + 'a>>;

    fn open_directory<'a>(
        &'a self,
        path: &str,
        filter: DirFilterFlags,
    ) -> Result<Box<dyn DirectoryOperations + 'a>>;

    fn get_file_timestamp_raw(&self, path: &str) -> Result<FileTimeStampRaw>;
}
