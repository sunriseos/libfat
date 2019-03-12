use super::block::{Block, BlockDevice, BlockIndex, BlockIndexClusterIter};
use super::cluster::Cluster;
use super::name::{LongFileName, ShortFileName};
use super::FatFileSystem;
use crate::FileSystemError;
use crate::Result as FileSystemResult;

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

    pub fn find_entry(self, name: &str) -> FileSystemResult<DirectoryEntry> {
        for entry in self.iter() {
            let entry = entry?;
            if entry.file_name.as_str() == name {
                return Ok(entry);
            }
        }

        Err(FileSystemError::NotFound)
    }

    pub fn open_file(self, path: &str) -> FileSystemResult<DirectoryEntry> {
        let (name, rest_opt) = split_path(path);
        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        match rest_opt {
            Some(rest) => {
                if !child_entry.attribute.is_directory() {
                    Err(FileSystemError::NotFound)
                } else {
                    Directory::from_entry(fs, child_entry).open_file(rest)
                }
            }
            None => Ok(child_entry),
        }
    }

    pub fn open_dir(self, path: &str) -> FileSystemResult<Directory<'a, T>> {
        let (name, rest_opt) = split_path(path);

        let fs = self.fs;

        let child_entry = self.find_entry(name)?;

        if !child_entry.attribute.is_directory() {
            return Err(FileSystemError::NotFound);
        }

        match rest_opt {
            Some(rest) => Directory::from_entry(fs, child_entry).open_dir(rest),
            None => Ok(Directory::from_entry(fs, child_entry)),
        }
    }

    fn delete_dir_entry(
        fs: &'a FatFileSystem<T>,
        dir_entry: &DirectoryEntry,
    ) -> FileSystemResult<()> {
        if let Some(raw_info) = dir_entry.raw_info {
            let mut block_iter = FatDirEntryIterator::new(
                fs,
                raw_info.parent_cluster,
                raw_info.first_entry_block_index,
                raw_info.first_entry_offset,
            );

            let mut i = 0;
            while i < raw_info.entry_count {
                if let Some(block_res) = block_iter.next() {
                    let mut res = block_res?;

                    res.set_deleted();
                    res.flush(fs)?;
                    i += 1;
                } else {
                    return Err(FileSystemError::ReadFailed);
                }
            }
        }

        Ok(())
    }

    pub fn unlink(self, name: &str) -> FileSystemResult<()> {
        let fs = self.fs;

        let dir_entry = self.find_entry(name)?;

        // Check for directory not being empty
        if dir_entry.attribute.is_directory()
            && Self::from_entry(fs, dir_entry).iter().nth(2).is_some()
        {
            return Err(FileSystemError::AccessDenied);
        }

        Self::delete_dir_entry(fs, &dir_entry)?;

        if dir_entry.start_cluster.0 != 0 {
            fs.free_cluster(dir_entry.start_cluster, None)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct DirectoryEntryRawInfo {
    pub parent_cluster: Cluster,
    pub first_entry_block_index: BlockIndex,
    pub first_entry_offset: u32,
    pub entry_count: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct DirectoryEntry {
    pub start_cluster: Cluster,
    pub(crate) raw_info: Option<DirectoryEntryRawInfo>,
    pub creation_timestamp: u64,
    pub last_access_timestamp: u64,
    pub last_modification_timestamp: u64,
    pub file_size: u32,
    pub file_name: ArrayString<[u8; Self::MAX_FILE_NAME_LEN]>,
    pub attribute: Attributes,
}

impl DirectoryEntryRawInfo {
    pub fn get_dir_entry<T>(&self, fs: &FatFileSystem<T>) -> FileSystemResult<FatDirEntry>
    where
        T: BlockDevice,
    {
        let mut block_iter = FatDirEntryIterator::new(
            fs,
            self.parent_cluster,
            self.first_entry_block_index,
            self.first_entry_offset,
        );

        let mut i = 0;
        let mut res = None;

        while i < self.entry_count {
            let result = block_iter.next();
            if let Some(result) = result {
                res = Some(result?);
            } else {
                res = None;
            }
            i += 1;
        }

        if let Some(res) = res {
            Ok(res)
        } else {
            Err(FileSystemError::NotFound)
        }
    }
}

impl DirectoryEntry {
    // entry can at best have 255 chars in UTF-8
    pub const MAX_FILE_NAME_LEN: usize = 256 * 4;
}

pub struct DirectoryEntryIterator<'a, T> {
    pub directory_cluster: Cluster,
    pub raw_iter: FatDirEntryIterator<'a, T>,
}

impl<'a, T> DirectoryEntryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn new(root: Directory<'a, T>) -> Self {
        DirectoryEntryIterator {
            directory_cluster: root.dir_info.start_cluster,
            raw_iter: FatDirEntryIterator::from_directory(root),
        }
    }
}

impl<'a, T> Iterator for DirectoryEntryIterator<'a, T>
where
    T: BlockDevice,
{
    type Item = FileSystemResult<DirectoryEntry>;
    fn next(&mut self) -> Option<FileSystemResult<DirectoryEntry>> {
        let mut next_is_end_entry = false;
        let mut first_raw_dir_entry: Option<FatDirEntry> = None;
        let mut entry_count = 0;
        let mut lfn_index: i32 = 0;
        let mut file_name = ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN]>::new();

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

                let first_raw_dir_entry = first_raw_dir_entry.unwrap();

                // only a SFN entry
                return Some(Ok(DirectoryEntry {
                    start_cluster: entry.get_cluster(),
                    raw_info: Some(DirectoryEntryRawInfo {
                        parent_cluster: first_raw_dir_entry.entry_cluster,
                        first_entry_block_index: BlockIndex(first_raw_dir_entry.entry_index),
                        first_entry_offset: first_raw_dir_entry.entry_offset,
                        entry_count,
                    }),
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
    pub fn from_directory(root: Directory<'a, T>) -> Self {
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

    pub fn new(
        fs: &'a FatFileSystem<T>,
        start_cluster: Cluster,
        block_index: BlockIndex,
        offset: u32,
    ) -> Self {
        FatDirEntryIterator {
            counter: (offset / FatDirEntry::LEN as u32) as u8,
            block_index: block_index.0,
            is_first: true,
            cluster_iter: BlockIndexClusterIter::new(fs, start_cluster, Some(block_index)),
            last_cluster: None,
        }
    }
}

impl<'a, T> Iterator for FatDirEntryIterator<'a, T>
where
    T: BlockDevice,
{
    type Item = FileSystemResult<FatDirEntry>;
    fn next(&mut self) -> Option<FileSystemResult<FatDirEntry>> {
        let entry_per_block_count = (Block::LEN / FatDirEntry::LEN) as u8;
        let fs = self.cluster_iter.cluster_iter.fs;

        let cluster_opt = if self.counter == entry_per_block_count || self.is_first {
            if !self.is_first {
                self.counter = 0;
                self.block_index += 1;
            }
            self.block_index %= u32::from(fs.boot_record.blocks_per_cluster());
            self.is_first = false;
            self.last_cluster = self.cluster_iter.next();
            self.last_cluster
        } else {
            self.last_cluster
        };

        let cluster = cluster_opt?;

        let mut blocks = [Block::new()];

        let entry_index = (self.counter % entry_per_block_count) as usize;

        let read_res = fs
            .block_device
            .read(
                &mut blocks,
                fs.partition_start,
                BlockIndex(cluster.to_data_block_index(fs).0 + self.block_index),
            )
            .or(Err(FileSystemError::ReadFailed));

        if let Err(error) = read_res {
            return Some(Err(error));
        }

        let entry_start = entry_index * FatDirEntry::LEN;
        let entry_end = (entry_index + 1) * FatDirEntry::LEN;
        let dir_entry = FatDirEntry::from_raw(
            &blocks[0][entry_start..entry_end],
            cluster,
            self.block_index,
            entry_start as u32,
        );

        // The entry isn't a valid one but this doesn't mark the end of the directory
        self.counter += 1;

        Some(Ok(dir_entry))
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

#[derive(Clone, Copy)]
pub struct FatDirEntry {
    entry_cluster: Cluster,
    entry_index: u32,
    entry_offset: u32,
    data: [u8; Self::LEN],
}

impl FatDirEntry {
    pub const LEN: usize = 32;

    pub fn from_raw(
        data: &[u8],
        entry_cluster: Cluster,
        entry_index: u32,
        entry_offset: u32,
    ) -> FatDirEntry {
        let mut data_copied = [0x0u8; Self::LEN];

        data_copied[..data.len()].clone_from_slice(&data[..]);
        FatDirEntry {
            entry_cluster,
            entry_index,
            entry_offset,
            data: data_copied,
        }
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

    pub fn set_deleted(&mut self) {
        self.data[0] = 0xE5;
    }

    pub fn flush<T>(&self, fs: &FatFileSystem<T>) -> FileSystemResult<()>
    where
        T: BlockDevice,
    {
        let mut blocks = [Block::new()];

        // FIXME: Custom Iterator to catches those errors
        fs.block_device
            .read(
                &mut blocks,
                fs.partition_start,
                BlockIndex(self.entry_cluster.to_data_block_index(fs).0 + self.entry_index),
            )
            .or(Err(FileSystemError::ReadFailed))?;

        let block = &mut blocks[0];
        let entry_start = self.entry_offset as usize;
        let entry_end = entry_start + Self::LEN;

        for (i, val) in block[entry_start..entry_end].iter_mut().enumerate() {
            *val = self.data[i];
        }

        fs.block_device
            .write(
                &blocks,
                fs.partition_start,
                BlockIndex(self.entry_cluster.to_data_block_index(fs).0 + self.entry_index),
            )
            .or(Err(FileSystemError::WriteFailed))
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

    pub fn set_cluster(&mut self, cluster: Cluster) {
        let value = cluster.0;
        let high_cluster = ((value >> 16) & 0xFFFF) as u16;
        let low_cluster = (value & 0xFFFF) as u16;

        LittleEndian::write_u16(&mut self.data[20..22], high_cluster);
        LittleEndian::write_u16(&mut self.data[26..28], low_cluster);
    }

    pub fn get_file_size(&self) -> u32 {
        LittleEndian::read_u32(&self.data[28..32])
    }

    pub fn set_file_size(&mut self, new_size: u32) {
        LittleEndian::write_u32(&mut self.data[28..32], new_size);

        // Size is 0 so cluster need to be reset
        if new_size == 0 {
            self.set_cluster(Cluster(0))
        }
    }

    pub fn get_creation_datetime(&self) -> FatDateTime {
        let raw_time = LittleEndian::read_u16(&self.data[14..16]);
        let seconds = ((raw_time & 0x1f) << 1) as u8;
        let minutes = ((raw_time >> 5) & 0x3f) as u8;
        let hour = ((raw_time >> 11) & 0x1f) as u8;

        let raw_date = LittleEndian::read_u16(&self.data[16..18]);
        let day = (raw_date & 0x1f) as u8;
        let month = ((raw_date >> 5) & 0xf) as u8;
        let year = (raw_date >> 9) & 0x7f;
        FatDateTime::new(
            1980 + year,
            month,
            day,
            hour,
            minutes,
            seconds,
            self.data[13],
        )
    }

    pub fn get_last_access_date(&self) -> FatDateTime {
        let raw_date = LittleEndian::read_u16(&self.data[18..20]);
        let day = (raw_date & 0x1f) as u8;
        let month = ((raw_date >> 5) & 0xf) as u8;
        let year = (raw_date >> 9) & 0x7f;
        FatDateTime::new(1980 + year, month, day, 0, 0, 0, 0)
    }

    pub fn get_modification_datetime(&self) -> FatDateTime {
        let raw_time = LittleEndian::read_u16(&self.data[22..24]);
        let seconds = ((raw_time & 0x1f) << 1) as u8;
        let minutes = ((raw_time >> 5) & 0x3f) as u8;
        let hour = ((raw_time >> 11) & 0x1f) as u8;

        let raw_date = LittleEndian::read_u16(&self.data[24..26]);
        let day = (raw_date & 0x1f) as u8;
        let month = ((raw_date >> 5) & 0xf) as u8;
        let year = (raw_date >> 9) & 0x7f;
        FatDateTime::new(1980 + year, month, day, hour, minutes, seconds, 0)
    }
}

#[derive(Debug)]
pub struct FatDateTime {
    year: u16,
    month: u8,
    day: u8,

    hour: u8,
    minutes: u8,
    seconds: u8,

    tenths: u8,
}

impl FatDateTime {
    const DAYS: [[u16; 12]; 4] = [
        [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335],
        [366, 397, 425, 456, 486, 517, 547, 578, 609, 639, 670, 700],
        [
            731, 762, 790, 821, 851, 882, 912, 943, 974, 1004, 1035, 1065,
        ],
        [
            1096, 1127, 1155, 1186, 1216, 1247, 1277, 1308, 1339, 1369, 1400, 1430,
        ],
    ];
    pub fn new(
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minutes: u8,
        seconds: u8,
        tenths: u8,
    ) -> Self {
        FatDateTime {
            year,
            month,
            day,
            hour,
            minutes,
            seconds,
            tenths,
        }
    }

    pub fn to_unix_time(&self) -> u64 {
        // TODO: support other ranges than 2000-2099
        if self.year > 2099 || self.year < 2000 {
            return 0;
        }

        let year = u64::from(self.year) % 100;
        let month = u64::from(self.month) - 1;
        let day = u64::from(self.day) - 1;

        let hour = u64::from(self.hour);
        let minutes = u64::from(self.minutes);
        let seconds = u64::from(self.seconds);

        946_684_800
            + (((year / 4 * (365 * 4 + 1)
                + u64::from(Self::DAYS[year as usize % 4][month as usize])
                + day)
                * 24
                + hour)
                * 60
                + minutes)
                * 60
            + seconds
    }
}

impl<'a> core::fmt::Debug for FatDirEntry {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
        FatDirEntryIterator::from_directory(self)
    }

    pub fn iter(self) -> DirectoryEntryIterator<'a, T> {
        DirectoryEntryIterator::new(self)
    }
}
