use crate::fat::block::{Block, BlockDevice};
use crate::fat::cluster::Cluster;
use crate::fat::name::{LongFileName, ShortFileName};
use crate::fat::table::FatClusterIter;
use crate::fat::FatFileSystem;
use crate::FileSystemError;

use alloc::string::String;
use alloc::string::ToString;
use byteorder::{ByteOrder, LittleEndian};

pub struct Directory<'a, T> {
    dir_info: DirectoryEntry,
    pub fs: &'a FatFileSystem<T>,
}

impl<'a, T> Directory<'a, T> where T: BlockDevice {
    pub fn from_entry(fs: &'a FatFileSystem<T>, dir_info: DirectoryEntry) -> Self {
        Directory {
            dir_info,
            fs
        }
    }
}

#[derive(Debug)]
pub struct DirectoryEntry {
    pub start_cluster: Cluster,
    pub file_size: u32,
    pub file_name: String,
    pub attribute: Attributes,
}

pub struct DirectoryEntryIterator<'a, T: BlockDevice> {
    pub dir: &'a Directory<'a, T>,
    pub raw_iter: FatDirEntryIterator<'a, T>,
}

impl<'a, T> DirectoryEntryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn new(root: &'a Directory<'a, T>) -> Self {
        DirectoryEntryIterator {
            dir: root,
            raw_iter: FatDirEntryIterator::new(root),
        }
    }
}

impl<'a, T> Iterator for DirectoryEntryIterator<'a, T>
where
    T: BlockDevice,
{
    type Item = DirectoryEntry;
    fn next(&mut self) -> Option<DirectoryEntry> {
        let mut has_lfn = false;
        let mut file_name = String::new();

        while let Some(entry) = self.raw_iter.next() {
            if has_lfn && entry.is_long_file_name() {
                let mut part = String::new();
                // FIXME: Custom Iterator to catches those errors
                let raw_name = entry.long_file_name_raw().unwrap().chars().unwrap();
                for c in raw_name.iter() {
                    part.push(*c);
                }

                file_name.insert_str(0, part.as_str());
            } else if entry.is_long_file_name() {
                // FIXME: uncomment this when done with debugging
                /*has_lfn = true;

                // FIXME: Custom Iterator to catches those errors
                let raw_name = entry.long_file_name_raw().unwrap().chars().unwrap();
                for c in raw_name.iter() {
                    file_name.push(*c);
                }*/
            } else {
                if !has_lfn {
                    let raw_name = entry.short_name().unwrap().chars();
                    for c in raw_name.iter().take(8) {
                        file_name.push(*c);
                    }

                    // Short filename with extension
                    if raw_name[8] != ' ' {
                        file_name.push('.');
                        for c in raw_name.iter().skip(8) {
                            file_name.push(*c);
                        }
                    }
                    file_name = file_name.trim_end().to_string();
                }
                if let Some(end_char_index) = file_name.find('\0') {
                    file_name.truncate(end_char_index);
                }

                // only a SFN entry
                return Some(DirectoryEntry {
                    start_cluster: entry.get_cluster(),
                    file_size: entry.get_file_size(),
                    file_name: file_name,
                    attribute: entry.attribute(),
                });
            }
        }

        None
    }
}

pub struct FatDirEntryIterator<'a, T: BlockDevice> {
    pub dir: &'a Directory<'a, T>,
    pub cluster_iter: FatClusterIter<'a, T>,
    pub last_cluster: Option<Cluster>,
    pub counter: usize,
}

impl<'a, T> FatDirEntryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn new(root: &'a Directory<'a, T>) -> Self {
        FatDirEntryIterator {
            dir: root,
            counter: 16,
            cluster_iter: FatClusterIter::new(&root.fs, &root.dir_info.start_cluster),
            last_cluster: None,
        }
    }
}

impl<'a, T> Iterator for FatDirEntryIterator<'a, T>
where
    T: BlockDevice,
{
    type Item = FatDirEntry;
    fn next(&mut self) -> Option<FatDirEntry> {
        let cluster_opt = match self.counter {
            16 => {
                self.counter = 0;
                self.last_cluster = self.cluster_iter.next();
                self.last_cluster.clone()
            }
            _ => self.last_cluster.clone(),
        };

        let cluster = cluster_opt?;

        let mut blocks = [Block::new()];

        // FIXME: Custom Iterator to catches those errors
        self.dir
            .fs
            .block_device
            .read(&mut blocks, cluster.to_data_block_index(&self.dir.fs))
            .or(Err(FileSystemError::ReadFailed))
            .unwrap();

        let entry_start = self.counter * FatDirEntry::LEN;
        let entry_end = (self.counter + 1) * FatDirEntry::LEN;
        let dir_entry = FatDirEntry::from_raw(&blocks[0][entry_start..entry_end]);

        // The entry isn't a valid one but this doesn't mark the end of the directory

        self.counter += 1;

        if dir_entry.is_deleted() || dir_entry.is_free() {
            return self.next();
        }

        Some(dir_entry)
    }
}

#[derive(Debug)]
pub struct Attributes(u8);

impl Attributes {
    pub const READ_ONLY: u8 = 0x01;
    pub const HIDDEN: u8 = 0x02;
    pub const SYSTEM: u8 = 0x04;
    pub const VOLUME: u8 = 0x08;
    pub const DIRECTORY: u8 = 0x10;
    pub const ARCHIVE: u8 = 0x20;

    pub const LFN: u8 = Self::READ_ONLY | Self::HIDDEN | Self::SYSTEM | Self::VOLUME;

    pub fn new(value: u8) -> Attributes {
        Attributes(value)
    }

    pub fn is_read_only(&self) -> bool {
        (self.0 & Self::READ_ONLY) == Self::READ_ONLY
    }

    pub fn is_hidden(&self) -> bool {
        (self.0 & Self::HIDDEN) == Self::HIDDEN
    }

    pub fn is_system(&self) -> bool {
        (self.0 & Self::SYSTEM) == Self::SYSTEM
    }

    pub fn is_volume(&self) -> bool {
        (self.0 & Self::VOLUME) == Self::VOLUME
    }

    pub fn is_directory(&self) -> bool {
        (self.0 & Self::DIRECTORY) == Self::DIRECTORY
    }

    pub fn is_archive(&self) -> bool {
        (self.0 & Self::ARCHIVE) == Self::ARCHIVE
    }

    pub fn is_lfn(&self) -> bool {
        (self.0 & Self::LFN) == Self::LFN
    }
}

pub enum FatDirEntryType {
    ShortFileName,
    LongFileName,
}

pub struct FatDirEntry {
    data: [u8; Self::LEN],
}

impl FatDirEntry {
    pub const LEN: usize = 32;

    pub fn from_raw(data: &[u8]) -> FatDirEntry {
        let mut data_copied = [0x0u8; Self::LEN];

        if data.len() != FatDirEntry::LEN {
            panic!()
        }

        data_copied[..data.len()].clone_from_slice(&data[..]);
        FatDirEntry { data: data_copied }
    }

    pub fn is_free(&self) -> bool {
        self.data[0] == 0
    }

    pub fn is_deleted(&self) -> bool {
        self.data[0] == 0xE5
    }

    pub fn attribute(&self) -> Attributes {
        Attributes::new(self.data[11])
    }

    pub fn is_long_file_name(&self) -> bool {
        self.attribute().is_lfn()
    }

    pub fn long_file_name_raw(&self) -> Option<LongFileName> {
        if self.is_long_file_name() {
            Some(LongFileName::from_data(&self.data))
        } else {
            None
        }
    }

    pub fn is_valid(&self) -> bool {
        // TODO: do we need more?
        !self.is_free() && !self.is_deleted()
    }

    pub fn short_name(&self) -> Option<ShortFileName> {
        if !self.is_long_file_name() {
            Some(ShortFileName::from_data(&self.data[0..11]))
        } else {
            None
        }
    }

    pub fn get_cluster(&self) -> Cluster {
        let high_cluster = LittleEndian::read_u16(&self.data[20..22]) as u32;
        let low_cluster = LittleEndian::read_u16(&self.data[26..28]) as u32;

        Cluster(low_cluster | (high_cluster << 16))
    }

    pub fn get_file_size(&self) -> u32 {
        LittleEndian::read_u32(&self.data[28..32])
    }
}

impl<'a> core::fmt::Debug for FatDirEntry {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "FatDirEntry {{ ")?;
        write!(f, "{:?} ", self.attribute())?;
        if self.is_long_file_name() {
            let long_file_name_raw = self.long_file_name_raw();
            if let Some(long_file_name) = long_file_name_raw {
                write!(f, "LongFileName {{{:?}}}", long_file_name.chars().unwrap())?;
            } else {
                write!(f, "LongFileName {{ \"not a long file name?????\" }}")?;
            }
        } else {
            write!(
                f,
                "ShortFileName {{{:?}}}",
                self.short_name().unwrap().chars()
            )?;
        }
        write!(f, " }}")
    }
}

impl<'a, T> Directory<'a, T>
where
    T: BlockDevice,
{
    pub fn fat_dir_entry_iter(&'a self) -> FatDirEntryIterator<'a, T> {
        FatDirEntryIterator::new(self)
    }

    pub fn iter(&'a self) -> DirectoryEntryIterator<'a, T> {
        DirectoryEntryIterator::new(self)
    }
}
