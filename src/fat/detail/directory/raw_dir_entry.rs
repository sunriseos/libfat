use byteorder::{ByteOrder, LittleEndian};

use crate::fat::detail::attribute::Attributes;
use crate::fat::detail::block::{Block, BlockDevice, BlockIndex};
use crate::fat::detail::cluster::Cluster;
use crate::fat::detail::datetime::FatDateTime;
use crate::fat::detail::filesystem::FatFileSystem;
use crate::fat::detail::name::{LongFileName, ShortFileName};
use crate::FileSystemError;
use crate::Result as FileSystemResult;

pub enum FatDirEntryType {
    ShortFileName,
    LongFileName,
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

    pub fn set_attribute(&mut self, attribute: Attributes) {
        self.data[11] = attribute.get_value();
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

    pub fn set_short_name(&mut self, short_name: &ShortFileName) {
        (&mut self.data[0..11]).copy_from_slice(&short_name.as_bytes());
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
