pub mod detail;

use alloc::boxed::Box;
use core::iter::Iterator;
use detail::block::BlockDevice;
use detail::block::Block;
use detail::block::BlockIndex;
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

struct FileInterface<'a, T> {
    fs: &'a FatFileSystem<T>,
    file_info: detail::directory::DirectoryEntry
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
        if path == "/" {
            Ok(self.get_root_directory())
        } else {
            self.get_root_directory().open_dir(path).ok_or(FileSystemError::NotFound)
        }
    }
}

impl<B> FileSystemOperations for FatFileSystem<B>
where
    B: BlockDevice,
{
    fn create_file(&self, _name: &str, _mode: FileModeFlags, _size: u64) -> FileSystemResult<()> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn delete_file(&self, _name: &str) -> FileSystemResult<()> {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn open_file<'a>(
        &'a self,
        name: &str,
        mode: FileModeFlags,
    ) -> FileSystemResult<Box<dyn FileOperations + 'a>> {
        // TODO resize files ect
        if (mode & FileModeFlags::APPENDABLE) == FileModeFlags::APPENDABLE {
            return Err(FileSystemError::Custom {
                name: "not implemented"
            });
        }

        let dir_entry = self.get_root_directory().open_file(name).ok_or(FileSystemError::NotFound)?;

        let res = Box::new(FileInterface {
            fs: self,
            file_info: dir_entry
        });

        Ok(res as Box<dyn FileOperations + 'a>)
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

impl<'a, T> FileOperations for FileInterface<'a, T>
where
    T: BlockDevice,
{
    fn read(&mut self, offset: u64, buf: &mut [u8]) -> FileSystemResult<u64>
    {
        if offset >= u64::from(self.file_info.file_size) {
            return Ok(0)
        }

        let device: &T = &self.fs.block_device;

        let mut read_size = 0u64;
        let block_start_index = self.file_info.start_cluster.to_data_block_index(self.fs);
        let mut raw_tmp_offset = offset as u32;
        let mut blocks = [Block::new()];

        while read_size < buf.len() as u64 {
            
            let tmp_index = raw_tmp_offset / Block::LEN_U32;
            let tmp_offset = raw_tmp_offset % Block::LEN_U32;

            // TODO catch error here
            device.read(&mut blocks, BlockIndex(block_start_index.0 + tmp_index)).unwrap();

            let buf_slice = &mut buf[read_size as usize..];
            let buf_limit = if buf_slice.len() >= Block::LEN {
                Block::LEN
            } else {
                buf_slice.len()
            };

            for (index, buf_entry) in buf_slice.iter_mut().take(buf_limit).enumerate() {
                *buf_entry = blocks[0][tmp_offset as usize + index];
            }

            raw_tmp_offset += buf_limit as u32;
            read_size += buf_limit as u64;
        }

        Ok(read_size)
    }

    fn write(&mut self, _offset: u64, _buf: &[u8]) -> FileSystemResult<()>
    {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn flush(&mut self) -> FileSystemResult<()>
    {
        // NOP
        Ok(())
    }

    fn set_len(&mut self, _size: u64) -> FileSystemResult<()>
    {
        Err(FileSystemError::Custom {
            name: "not implemented",
        })
    }

    fn get_len(&mut self) -> FileSystemResult<u64>
    {
        Ok(u64::from(self.file_info.file_size))
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
