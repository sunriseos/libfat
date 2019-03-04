pub mod detail;

use alloc::boxed::Box;
use core::iter::Iterator;
use detail::block::Block;
use detail::block::BlockDevice;
use detail::block::BlockIndex;
use detail::block::BlockIndexClusterIter;
use detail::filesystem::FatFileSystem;
use detail::utils::align_up;

use crate::Result as FileSystemResult;
use crate::{
    DirFilterFlags, DirectoryEntry, DirectoryEntryType, DirectoryOperations, FileModeFlags,
    FileOperations, FileSystemError, FileSystemOperations,
};

struct DirectoryReader<'a, T> {
    base_path: [u8; DirectoryEntry::PATH_LEN],
    internal_iter: detail::directory::DirectoryEntryIterator<'a, T>,
    filter_fn: &'static dyn Fn(&detail::directory::DirectoryEntry) -> bool,
    entry_count: u64,
}

struct FileInterface<'a, T> {
    fs: &'a FatFileSystem<T>,
    file_info: detail::directory::DirectoryEntry,
}

struct DirectoryFilterPredicate;
impl DirectoryFilterPredicate {
    fn all(entry: &detail::directory::DirectoryEntry) -> bool {
        let name = entry.file_name.as_str();
        name != "." && name != ".."
    }

    fn dirs(entry: &detail::directory::DirectoryEntry) -> bool {
        entry.attribute.is_directory() && Self::all(entry)
    }

    fn files(entry: &detail::directory::DirectoryEntry) -> bool {
        !entry.attribute.is_directory() && Self::all(entry)
    }
}

impl<B> FatFileSystem<B>
where
    B: BlockDevice,
{
    fn get_dir_from_path(&self, path: &str) -> FileSystemResult<detail::directory::Directory<'_, B>> {
        if path == "/" {
            Ok(self.get_root_directory())
        } else {
            self.get_root_directory()
                .open_dir(path)
                .ok_or(FileSystemError::NotFound)
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
        // TODO: separate type file operation type
        if (mode & FileModeFlags::APPENDABLE) == FileModeFlags::APPENDABLE {
            return Err(FileSystemError::Custom {
                name: "not implemented",
            });
        }

        let dir_entry = self
            .get_root_directory()
            .open_file(name)
            .ok_or(FileSystemError::NotFound)?;

        let res = Box::new(FileInterface {
            fs: self,
            file_info: dir_entry,
        });

        Ok(res as Box<dyn FileOperations + 'a>)
    }

    fn open_directory<'a>(
        &'a self,
        name: &str,
        filter: DirFilterFlags,
    ) -> FileSystemResult<Box<dyn DirectoryOperations + 'a>> {
        // reject path that are too big (shoudn't never happens but well we don't know)
        if name.len() >= DirectoryEntry::PATH_LEN {
            return Err(FileSystemError::NotFound);
        }

        let filter_fn: &'static dyn Fn(&detail::directory::DirectoryEntry) -> bool =
            if (filter & DirFilterFlags::ALL) == DirFilterFlags::ALL {
                &DirectoryFilterPredicate::all
            } else if (filter & DirFilterFlags::DIRECTORY) == DirFilterFlags::DIRECTORY {
                &DirectoryFilterPredicate::dirs
            } else {
                &DirectoryFilterPredicate::files
            };

        let target_dir = self.get_dir_from_path(name)?;
        // find a better way of doing this
        let target_dir_clone = self.get_dir_from_path(name)?;

        let entry_count = target_dir.iter().filter(filter_fn).count() as u64;

        let mut data: [u8; DirectoryEntry::PATH_LEN] = [0x0; DirectoryEntry::PATH_LEN];
        for (index, c) in name
            .as_bytes()
            .iter()
            .enumerate()
            .take(DirectoryEntry::PATH_LEN)
        {
            data[index] = *c;
        }

        // Add '/' if missing at the end
        if let Some('/') = name.chars().last() {
            // Already valid
        } else {
            data[name.as_bytes().len()] = 0x2F;
        }

        let res = Box::new(DirectoryReader {
            base_path: data,
            internal_iter: target_dir_clone.iter(),
            filter_fn,
            entry_count,
        });

        Ok(res as Box<dyn DirectoryOperations + 'a>)
    }

    fn delete_directory(&self, _name: &str) -> FileSystemResult<()> {
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

            *entry = raw_dir_entry.into_fs(&self.base_path);
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
    fn read(&mut self, offset: u64, buf: &mut [u8]) -> FileSystemResult<u64> {
        if offset >= 0xFFFF_FFFF {
            return Ok(0);
        }
        
        if offset >= u64::from(self.file_info.file_size) {
            return Ok(0);
        }

        let device: &T = &self.fs.block_device;

        let mut raw_tmp_offset = offset as u32;
        let cluster_offset = BlockIndex(raw_tmp_offset / Block::LEN_U32);
        let mut cluster_block_iterator =
            BlockIndexClusterIter::new(self.fs, self.file_info.start_cluster, Some(cluster_offset));
        let blocks_per_cluster = u32::from(self.fs.boot_record.blocks_per_cluster());

        let mut read_size = 0u64;
        let mut blocks = [Block::new()];

        raw_tmp_offset %= Block::LEN_U32;

        while read_size < buf.len() as u64 {
            let cluster_opt = cluster_block_iterator.next();
            if cluster_opt.is_none() {
                break;
            }

            let cluster = cluster_opt.unwrap();
            let block_start_index = cluster.to_data_block_index(self.fs);
            let tmp_index = cluster_offset.0 % blocks_per_cluster;
            let tmp_offset = raw_tmp_offset % Block::LEN_U32;

            device
                .read(&mut blocks, self.fs.partition_start, BlockIndex(block_start_index.0 + tmp_index))
                .or(Err(FileSystemError::ReadFailed))?;

            let buf_slice = &mut buf[read_size as usize..];
            let mut buf_limit = if buf_slice.len() >= Block::LEN {
                Block::LEN
            } else {
                buf_slice.len()
            };

            let bytes_left = (u64::from(self.file_info.file_size) - read_size) as usize;
            if bytes_left < buf_limit {
                buf_limit = bytes_left;
            }

            for (index, buf_entry) in buf_slice.iter_mut().take(buf_limit).enumerate() {
                *buf_entry = blocks[0][tmp_offset as usize + index];
            }

            raw_tmp_offset += buf_limit as u32;
            read_size += buf_limit as u64;
        }

        Ok(read_size)
    }

    fn write(&mut self, offset: u64, buf: &[u8]) -> FileSystemResult<()> {
        if offset >= 0xFFFF_FFFF {
            return Err(FileSystemError::AccessDenied);
        }

        let min_size = offset + buf.len() as u64;
        if min_size > u64::from(self.file_info.file_size) {
            self.set_len(min_size)?;
        }

        let device: &T = &self.fs.block_device;

        let mut raw_tmp_offset = offset as u32;
        let cluster_offset = BlockIndex(raw_tmp_offset / Block::LEN_U32);
        let mut cluster_block_iterator =
            BlockIndexClusterIter::new(self.fs, self.file_info.start_cluster, Some(cluster_offset));
        let blocks_per_cluster = u32::from(self.fs.boot_record.blocks_per_cluster());

        let mut write_size = 0u64;
        let mut blocks = [Block::new()];

        raw_tmp_offset %= Block::LEN_U32;

        while write_size < buf.len() as u64 {
            let cluster = cluster_block_iterator.next().ok_or(FileSystemError::WriteFailed)?;
            let block_start_index = cluster.to_data_block_index(self.fs);
            let tmp_index = cluster_offset.0 % blocks_per_cluster;
            let tmp_offset = raw_tmp_offset % Block::LEN_U32;

            device
                .read(&mut blocks, self.fs.partition_start, BlockIndex(block_start_index.0 + tmp_index))
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
                .write(&blocks, self.fs.partition_start, BlockIndex(block_start_index.0 + tmp_index))
                .or(Err(FileSystemError::WriteFailed))?;

            raw_tmp_offset += buf_limit as u32;
            write_size += buf_limit as u64;
        }

        Ok(())
    }

    fn flush(&mut self) -> FileSystemResult<()> {
        // NOP
        Ok(())
    }

    fn set_len(&mut self, size: u64) -> FileSystemResult<()> {
        let current_len = self.get_len()?;
        if size == current_len {
            return Ok(());
        } else if size > 0xFFFF_FFFF {
            return Err(FileSystemError::NoSpaceLeft);
        }

        let raw_file_info = self.file_info.raw_info.ok_or(FileSystemError::Custom {
            name: "Raw Info is missing ON A FILE",
        })?;
        let mut raw_dir_entry =
            raw_file_info
                .get_dir_entry(self.fs)
                .ok_or(FileSystemError::Custom {
                    name: "MISSING DIR ENTRY",
                })?;

        let cluster_size = u64::from(
            u16::from(self.fs.boot_record.blocks_per_cluster())
                * self.fs.boot_record.bytes_per_block(),
        );
        let aligned_size = align_up(size, cluster_size);
        let aligned_current_len = align_up(current_len, cluster_size);

        let new_size;

        if size > current_len {
            let diff_size = size - current_len;
            let mut cluster_to_add_count = (aligned_size - aligned_current_len) / cluster_size;
            let mut start_cluster =
                if self.file_info.start_cluster.0 == 0 || self.file_info.file_size == 0 {
                    None
                } else {
                    Some(self.file_info.start_cluster)
                };

            let mut last_cluster = start_cluster;
            let need_update_cluster = start_cluster.is_none();

            while cluster_to_add_count != 0 {
                last_cluster = Some(self.fs.alloc_cluster(last_cluster)?);
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
                    detail::table::get_last_and_previous_cluster(
                        self.fs,
                        self.file_info.start_cluster,
                    )?;
                self.fs.free_cluster(last_cluster, previous_cluster)?;
                cluster_to_remove_count -= 1;
            }

            new_size = self.file_info.file_size - diff_size as u32;
        }
        // TODO: update modified date?
        raw_dir_entry.set_cluster(self.file_info.start_cluster);
        raw_dir_entry.set_file_size(new_size);
        raw_dir_entry.flush(self.fs)?;

        self.file_info.file_size = new_size;

        Ok(())
    }

    fn get_len(&mut self) -> FileSystemResult<u64> {
        Ok(u64::from(self.file_info.file_size))
    }
}

impl detail::directory::DirectoryEntry {
    fn into_fs(self, base_path: &[u8; DirectoryEntry::PATH_LEN]) -> DirectoryEntry {
        let mut path: [u8; DirectoryEntry::PATH_LEN] = [0x0; DirectoryEntry::PATH_LEN];

        let file_size = self.file_size;

        let entry_type = if self.attribute.is_directory() {
            DirectoryEntryType::Directory
        } else {
            DirectoryEntryType::File
        };

        let mut base_index = 0;

        loop {
            let c = base_path[base_index];
            if c == 0x0 {
                break;
            }

            path[base_index] = c;
            base_index += 1;
        }

        for (index, c) in self
            .file_name
            .as_bytes()
            .iter()
            .enumerate()
            .take(DirectoryEntry::PATH_LEN - base_index)
        {
            path[base_index + index] = *c;
        }

        DirectoryEntry {
            path,
            entry_type,
            file_size: u64::from(file_size),
        }
    }
}
