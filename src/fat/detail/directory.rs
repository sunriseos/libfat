use super::block::{Block, BlockDevice, BlockIndex, BlockIndexClusterIter};
use super::cluster::Cluster;
use super::name::{LongFileName, ShortFileName};
use super::FatFileSystem;
use crate::FileSystemError;

use arrayvec::ArrayString;
use byteorder::{ByteOrder, LittleEndian};

#[derive(Copy)]
pub struct Directory<'a, T> {
    pub dir_info: DirectoryEntry,
    pub fs: &'a FatFileSystem<T>,
}

impl<'a, T> Clone for Directory<'a, T> {
    fn clone(&self) -> Self {
        Directory {
            dir_info: self.dir_info,
            fs: self.fs,
        }
    }
}

fn split_path(path: &str) -> (&str, Option<&str>) {
    let mut path_split = path.trim_matches('/').splitn(2, '/');

    // unwrap will never fail here
    let comp = path_split.next().unwrap();
    let rest_opt = path_split.next();

    (comp, rest_opt)
}

impl<'a, T> Directory<'a, T>
where
    T: BlockDevice,
{
    pub fn from_entry(fs: &'a FatFileSystem<T>, dir_info: DirectoryEntry) -> Self {
        Directory { dir_info, fs }
    }

    pub fn find_entry(self, name: &str) -> Option<DirectoryEntry> {
        for entry in self.iter() {
            if entry.file_name.as_str() == name {
                return Some(entry);
            }
        }

        None
    }

    pub fn open_file(self, path: &str) -> Option<DirectoryEntry> {
        let (name, rest_opt) = split_path(path);
        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        match rest_opt {
            Some(rest) => {
                if !child_entry.attribute.is_directory() {
                    None
                } else {
                    Directory::from_entry(fs, child_entry).open_file(rest)
                }
            }
            None => Some(child_entry),
        }
    }

    pub fn open_dir(self, path: &str) -> Option<Directory<'a, T>> {
        let (name, rest_opt) = split_path(path);

        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        if !child_entry.attribute.is_directory() {
            return None;
        }

        match rest_opt {
            Some(rest) => Directory::from_entry(fs, child_entry).open_dir(rest),
            None => Some(Directory::from_entry(fs, child_entry)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DirectoryEntry {
    pub start_cluster: Cluster,
    pub file_size: u32,
    pub file_name: ArrayString<[u8; Self::MAX_FILE_NAME_LEN]>,
    pub attribute: Attributes,
}

impl DirectoryEntry {
    // entry can at best have 255 chars in UTF-8
    pub const MAX_FILE_NAME_LEN: usize = 256 * 4;
}

pub struct DirectoryEntryIterator<'a, T> {
    pub raw_iter: FatDirEntryIterator<'a, T>,
}

impl<'a, T> DirectoryEntryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn new(root: Directory<'a, T>) -> Self {
        DirectoryEntryIterator {
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
        let mut next_is_end_entry = false;
        let mut lfn_index: i32 = 0;
        let mut file_name = ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN]>::new();

        while let Some(entry) = self.raw_iter.next() {
            // End of directory
            if entry.is_free() {
                break;
            }

            // Deleted entry? Clear everything and continue
            if entry.is_deleted() {
                lfn_index = 0;
                file_name.clear();

                continue;
            }

            // LFN
            if entry.is_long_file_name() {
                let first_byte = entry.get_first_byte();

                if (first_byte & 0x40) != 0 {
                    lfn_index = i32::from(first_byte ^ 0x40);
                }

                let mut part = ArrayString::<[_; LongFileName::MAX_LEN * 4]>::new();
                // FIXME: Custom Iterator to catches those errors
                let raw_name = entry.long_file_name_raw().unwrap().chars().unwrap();
                for c in raw_name.iter() {
                    part.push(*c);
                }

                // FIXME: this is dirty
                let mut tmp = ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN]>::new();
                tmp.push_str(file_name.as_str());
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
                        file_name.push(*c);
                    }

                    // Short filename with extension
                    if raw_name[8] != ' ' {
                        file_name.push('.');
                        for c in raw_name.iter().skip(8) {
                            file_name.push(*c);
                        }
                    }

                    // unwrap will never fail here
                    file_name = ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN]>::from(
                        file_name.trim_end(),
                    )
                    .unwrap();
                }
                if let Some(end_char_index) = file_name.find('\0') {
                    file_name.truncate(end_char_index);
                }

                // only a SFN entry
                return Some(DirectoryEntry {
                    start_cluster: entry.get_cluster(),
                    file_size: entry.get_file_size(),
                    file_name,
                    attribute: entry.attribute(),
                });
            }

            lfn_index = 0;
            next_is_end_entry = false;
            file_name.clear();
        }

        None
    }
}

pub struct FatDirEntryIterator<'a, T> {
    pub cluster_iter: BlockIndexClusterIter<'a, T>,
    pub last_cluster: Option<Cluster>,
    pub block_index: u32,
    pub counter: u8,
    pub is_first: bool,
}

impl<'a, T> FatDirEntryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn new(root: Directory<'a, T>) -> Self {
        let cluster = root.dir_info.start_cluster;
        let fs = &root.fs;

        FatDirEntryIterator {
            counter: 0,
            block_index: 0,
            is_first: true,
            cluster_iter: BlockIndexClusterIter::new(fs, cluster, None),
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
        let entry_per_block_count = (Block::LEN / FatDirEntry::LEN) as u8;
        let fs = self.cluster_iter.cluster_iter.fs;

        let cluster_opt = if self.counter == entry_per_block_count || self.is_first {
            self.counter = 0;
            if !self.is_first {
                self.block_index += 1;
            }
            self.block_index = self.block_index % fs.boot_record.blocks_per_cluster() as u32;
            self.is_first = false;
            self.last_cluster = self.cluster_iter.next();
            self.last_cluster
        } else {
            self.last_cluster
        };

        let cluster = cluster_opt?;

        let mut blocks = [Block::new()];

        let entry_index = (self.counter % entry_per_block_count) as usize;

        // FIXME: Custom Iterator to catches those errors
        fs.block_device
            .read(
                &mut blocks,
                BlockIndex(cluster.to_data_block_index(fs).0 + self.block_index),
            )
            .or(Err(FileSystemError::ReadFailed))
            .unwrap();

        let entry_start = entry_index * FatDirEntry::LEN;
        let entry_end = (entry_index + 1) * FatDirEntry::LEN;
        let dir_entry = FatDirEntry::from_raw(&blocks[0][entry_start..entry_end]);

        // The entry isn't a valid one but this doesn't mark the end of the directory
        self.counter += 1;

        Some(dir_entry)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Attributes(u8);

impl Attributes {
    pub const READ_ONLY: u8 = 0x01;
    pub const HIDDEN: u8 = 0x02;
    pub const SYSTEM: u8 = 0x04;
    pub const VOLUME: u8 = 0x08;
    pub const DIRECTORY: u8 = 0x10;
    pub const ARCHIVE: u8 = 0x20;
    pub const DEVICE: u8 = 0x40;

    pub const LFN: u8 = Self::READ_ONLY | Self::HIDDEN | Self::SYSTEM | Self::VOLUME;

    pub fn new(value: u8) -> Attributes {
        Attributes(value)
    }

    pub fn is_read_only(self) -> bool {
        (self.0 & Self::READ_ONLY) == Self::READ_ONLY
    }

    pub fn is_hidden(self) -> bool {
        (self.0 & Self::HIDDEN) == Self::HIDDEN
    }

    pub fn is_system(self) -> bool {
        (self.0 & Self::SYSTEM) == Self::SYSTEM
    }

    pub fn is_volume(self) -> bool {
        (self.0 & Self::VOLUME) == Self::VOLUME
    }

    pub fn is_directory(self) -> bool {
        (self.0 & Self::DIRECTORY) == Self::DIRECTORY
    }

    pub fn is_archive(self) -> bool {
        (self.0 & Self::ARCHIVE) == Self::ARCHIVE
    }

    pub fn is_lfn(self) -> bool {
        (self.0 & Self::LFN) == Self::LFN
    }

    pub fn is_device(self) -> bool {
        (self.0 & Self::DEVICE) == Self::DEVICE
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

    pub fn get_first_byte(&self) -> u8 {
        self.data[0]
    }

    pub fn is_free(&self) -> bool {
        self.get_first_byte() == 0
    }

    pub fn is_deleted(&self) -> bool {
        self.get_first_byte() == 0xE5 || self.get_first_byte() == 0x05
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
        let high_cluster = u32::from(LittleEndian::read_u16(&self.data[20..22]));
        let low_cluster = u32::from(LittleEndian::read_u16(&self.data[26..28]));

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
                if let Some(data) = long_file_name.chars() {
                    write!(f, "LongFileName {{{:?}}}", data)?;
                } else {
                    write!(f, "BROKEN LongFileName")?;
                }
            } else {
                write!(f, "LongFileName {{ \"not a long file name?????\" }}")?;
            }
        } else {
            // FIXME: SHOULDN'T UNWRAP
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
    pub fn fat_dir_entry_iter(self) -> FatDirEntryIterator<'a, T> {
        FatDirEntryIterator::new(self)
    }

    pub fn iter(self) -> DirectoryEntryIterator<'a, T> {
        DirectoryEntryIterator::new(self)
    }
}
