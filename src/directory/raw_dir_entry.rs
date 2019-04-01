//! Low level directory entry representation.
use byteorder::{ByteOrder, LittleEndian};
use structview::{u16_le, u32_le, View};

use crate::attribute::Attributes;
use crate::cluster::Cluster;
use crate::datetime::FatDateTime;
use crate::filesystem::FatFileSystem;
use crate::name::{LongFileName, ShortFileName};

use libfs::block::{Block, BlockDevice, BlockIndex};
use libfs::FileSystemError;
use libfs::FileSystemResult;

#[derive(Clone, Copy, View)]
#[repr(C)]
pub struct LongFileNameDirEntry {
    pub order_entry: u8,
    pub char_part_0: [u16_le; 5],
    pub attribute: u8,
    pub lfn_entry_type: u8,
    pub lfn_checksum: u8,
    pub char_part_1: [u16_le; 6],
    pub reserved: u16_le,
    pub char_part_2: [u16_le; 2],
}

#[derive(Clone, Copy, View)]
#[repr(C)]
pub struct ShortFileNameDirEntry {
    pub name: [u8; ShortFileName::MAX_LEN],
    pub attribute: u8,
    pub reserved: u8,
    pub creation_tenths: u8,
    pub creation_time: u16_le,
    pub creation_date: u16_le,
    pub last_access_date: u16_le,
    pub high_cluster: u16_le,
    pub modification_time: u16_le,
    pub modification_date: u16_le,
    pub low_cluster: u16_le,
    pub file_size: u32_le,
}

#[derive(Clone, Copy)]
pub struct FatDirEntry {
    pub entry_cluster: Cluster,
    pub entry_index: u32,
    pub entry_offset: u32,
    pub data: [u8; Self::LEN],
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
        self.get_first_byte() == 0xE5
    }

    pub fn set_deleted(&mut self) {
        self.data[0] = 0xE5;
    }

    pub fn clear(&mut self) {
        self.data = [0x0u8; Self::LEN];
    }

    pub fn flush<T>(&self, fs: &FatFileSystem<T>) -> FileSystemResult<()>
    where
        T: BlockDevice,
    {
        let mut blocks = [Block::new()];

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

    pub fn set_attribute(&mut self, attribute: Attributes) {
        self.data[11] = attribute.get_value();
    }

    pub fn is_long_file_name(&self) -> bool {
        self.attribute().is_lfn()
    }

    pub fn long_file_name_raw(&self) -> Option<LongFileName> {
        if self.is_long_file_name() {
            Some(LongFileName::from_lfn_dir_entry(self.as_lfn_entry()))
        } else {
            None
        }
    }

    pub fn short_name(&self) -> Option<ShortFileName> {
        if !self.is_long_file_name() {
            Some(ShortFileName::from_data(&self.as_sfn_entry().name))
        } else {
            None
        }
    }

    pub fn set_lfn_index(&mut self, index: u8) {
        self.data[0] = index;
    }

    pub fn set_short_name(&mut self, short_name: &ShortFileName) {
        (&mut self.data[0..11]).copy_from_slice(&short_name.as_bytes());
    }

    pub fn set_lfn_entry(&mut self, lfn: &str) {
        let lfn = LongFileName::from_utf8(lfn);

        let lfn = lfn.as_contents();

        for (i, entry) in lfn.iter().enumerate().take(5) {
            let index = 1 + i * 2;
            LittleEndian::write_u16(&mut self.data[index..index + 2], *entry);
        }
    
        for i in 0..6 {
            let index = 0xE + i * 2;
            let i = i + 5;

            LittleEndian::write_u16(&mut self.data[index..index + 2], lfn[i]);
        }

        for i in 0..2 {
            let index = 0x1C + i * 2;
            let i = i + 11;
            LittleEndian::write_u16(&mut self.data[index..index + 2], lfn[i]);
        }
    }

    pub fn as_lfn_entry(&self) -> &LongFileNameDirEntry {
        LongFileNameDirEntry::view(&self.data).unwrap()
    }

    pub fn as_sfn_entry(&self) -> &ShortFileNameDirEntry {
        ShortFileNameDirEntry::view(&self.data).unwrap()
    }

    pub fn set_lfn_checksum(&mut self, checksum: u8) {
        self.data[13] = checksum;
    }

    pub fn get_cluster(&self) -> Cluster {
        let entry = self.as_sfn_entry();
        let high_cluster = u32::from(entry.high_cluster.to_int());
        let low_cluster = u32::from(entry.low_cluster.to_int());

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
        self.as_sfn_entry().file_size.to_int()
    }

    pub fn set_file_size(&mut self, new_size: u32) {
        LittleEndian::write_u32(&mut self.data[28..32], new_size);

        // Size is 0 so cluster need to be reset
        if new_size == 0 {
            self.set_cluster(Cluster(0))
        }
    }

    pub fn get_creation_datetime(&self) -> FatDateTime {
        let entry = self.as_sfn_entry();
        let raw_time = entry.creation_time.to_int();
        let seconds = ((raw_time & 0x1f) << 1) as u8;
        let minutes = ((raw_time >> 5) & 0x3f) as u8;
        let hour = ((raw_time >> 11) & 0x1f) as u8;

        let raw_date = entry.creation_date.to_int();
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
        let entry = self.as_sfn_entry();

        let raw_date = entry.last_access_date.to_int();
        let day = (raw_date & 0x1f) as u8;
        let month = ((raw_date >> 5) & 0xf) as u8;
        let year = (raw_date >> 9) & 0x7f;
        FatDateTime::new(1980 + year, month, day, 0, 0, 0, 0)
    }

    pub fn get_modification_datetime(&self) -> FatDateTime {
        let entry = self.as_sfn_entry();

        let raw_time = entry.modification_time.to_int();
        let seconds = ((raw_time & 0x1f) << 1) as u8;
        let minutes = ((raw_time >> 5) & 0x3f) as u8;
        let hour = ((raw_time >> 11) & 0x1f) as u8;

        let raw_date = entry.modification_date.to_int();
        let day = (raw_date & 0x1f) as u8;
        let month = ((raw_date >> 5) & 0xf) as u8;
        let year = (raw_date >> 9) & 0x7f;
        FatDateTime::new(1980 + year, month, day, hour, minutes, seconds, 0)
    }
}

impl<'a> core::fmt::Debug for FatDirEntry {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "FatDirEntry {{ ")?;
        write!(f, "{:?} ", self.attribute())?;
        if self.is_long_file_name() {
            if let Some(long_file_name) = self.long_file_name_raw() {
                if let Some(data) = long_file_name.chars() {
                    write!(f, "LongFileName {{{:?}}}", data)?;
                } else {
                    write!(f, "BROKEN LongFileName")?;
                }
            } else {
                write!(f, "LongFileName {{ \"not a long file name?????\" }}")?;
            }
        } else if let Some(short_file_name) = self.short_name() {
            write!(f, "ShortFileName {{{:?}}}", short_file_name.chars())?;
        } else {
            write!(f, "ShortFileName {{ \"not a short file name?????\" }}")?;
        }
        write!(f, " }}")
    }
}
