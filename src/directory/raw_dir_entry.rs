//! Low level directory entry representation.
use crate::attribute::Attributes;
use crate::cluster::Cluster;
use crate::datetime::FatDateTime;
use crate::filesystem::FatFileSystem;
use crate::name::{LongFileName, ShortFileName};
use crate::FatError;
use crate::FatFileSystemResult;
use crate::FatFsType;
use core::convert::TryInto;
use storage_device::StorageDevice;

/// Represent a VFAT long name entry.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct LongFileNameDirEntry {
    /// The sequence number.
    pub order_entry: u8,

    /// Name characters. (five UCS-2 characters)
    pub char_part_0: [u16; 5],

    /// Attributes (always 0x0F).
    pub attribute: u8,

    /// Type (always 0x00 for VFAT LFN)
    pub lfn_entry_type: u8,

    /// Checksum of DOS file name.
    pub lfn_checksum: u8,

    /// Name characters. (six UCS-2 characters)
    pub char_part_1: [u16; 6],

    /// Reserved/First cluster (always 0x0000)
    pub reserved: u16,

    /// Name characters. (two UCS-2 characters)
    pub char_part_2: [u16; 2],
}

#[derive(Clone, Copy)]
/// Represent a FAT directory entry (8.3 entry/VFAT long entry)
pub struct FatDirEntry {
    /// The cluster where this entry is.
    pub entry_cluster: Cluster,

    /// The entry offset on the cluster.
    pub entry_cluster_offset: u64,

    /// The entry offset in the block.
    pub entry_offset: u64,

    /// The raw data of the entry.
    pub data: [u8; Self::LEN],
}

impl LongFileNameDirEntry {
    /// Create a new LongFileNameDirEntry from raw data.
    pub fn from_data(data: &[u8; FatDirEntry::LEN]) -> Self {
        let mut char_part_0 = [0x0; 5];
        let mut char_part_1 = [0x0; 6];
        let mut char_part_2 = [0x0; 2];

        for (i, entry) in char_part_0.iter_mut().enumerate() {
            let index = 1 + i * 2;
            let value = u16::from_le_bytes(data[index..index + 2].try_into().unwrap());

            *entry = value;
        }

        for (i, entry) in char_part_1.iter_mut().enumerate() {
            let index = 0xE + i * 2;
            let value = u16::from_le_bytes(data[index..index + 2].try_into().unwrap());

            *entry = value;
        }

        for (i, entry) in char_part_2.iter_mut().enumerate() {
            let index = 0x1C + i * 2;
            let value = u16::from_le_bytes(data[index..index + 2].try_into().unwrap());

            *entry = value;
        }

        LongFileNameDirEntry {
            order_entry: data[0],
            char_part_0,
            attribute: data[11],
            lfn_entry_type: data[12],
            lfn_checksum: data[13],
            char_part_1,
            reserved: u16::from_le_bytes(data[26..28].try_into().unwrap()),
            char_part_2,
        }
    }
}

impl FatDirEntry {
    /// The length of a FAT directory entry.
    pub const LEN: usize = 32;

    /// Create a new FAT directory entry representation from raw data.
    pub fn from_raw(
        data: &[u8],
        entry_cluster: Cluster,
        entry_cluster_offset: u64,
        entry_offset: u64,
    ) -> FatDirEntry {
        let mut data_copied = [0x0u8; Self::LEN];

        data_copied[..data.len()].clone_from_slice(&data[..]);
        FatDirEntry {
            entry_cluster,
            entry_cluster_offset,
            entry_offset,
            data: data_copied,
        }
    }

    /// Get the first byte of the raw data.
    pub fn get_first_byte(&self) -> u8 {
        self.data[0]
    }

    /// Check if the first byte is zero.
    pub fn is_free(&self) -> bool {
        self.get_first_byte() == 0
    }

    /// Check if the first byte is the delete marker.
    pub fn is_deleted(&self) -> bool {
        self.get_first_byte() == 0xE5
    }

    /// Set the deleted marker on the entry.
    pub fn set_deleted(&mut self) {
        self.data[0] = 0xE5;
    }

    /// Reinitialize the raw data buffer to a free state.
    pub fn clear(&mut self) {
        self.data = [0x0u8; Self::LEN];
    }

    /// Write the raw data buffer to disk.
    pub fn flush<S: StorageDevice>(&self, fs: &FatFileSystem<S>) -> FatFileSystemResult<()> {
        let is_in_old_root_directory = match fs.boot_record.fat_type {
            FatFsType::Fat12 | FatFsType::Fat16 => self.entry_cluster.0 == 0,
            _ => false,
        };

        let entry_offset = if is_in_old_root_directory {
            let root_dir_blocks = ((u32::from(fs.boot_record.root_dir_childs_count()) * 32)
                + (u32::from(fs.boot_record.bytes_per_block()) - 1))
                / u32::from(fs.boot_record.bytes_per_block());
            let root_dir_offset = root_dir_blocks * u32::from(fs.boot_record.bytes_per_block());

            if self.entry_cluster_offset > u64::from(root_dir_offset) {
                Err(FatError::NoSpaceLeft)
            } else {
                Ok(fs.first_data_offset - u64::from(root_dir_offset)
                    + self.entry_cluster_offset
                    + self.entry_offset)
            }
        } else {
            Ok(self.entry_cluster.to_data_bytes_offset(fs)?
                + self.entry_cluster_offset
                + self.entry_offset)
        };

        fs.storage_device
            .lock()
            .write(fs.partition_start + entry_offset?, &self.data)
            .or(Err(FatError::WriteFailed))
    }

    /// Return the entry attributes.
    pub fn attribute(&self) -> Attributes {
        Attributes::new(self.data[11])
    }

    /// Set the entry attributes.
    pub fn set_attribute(&mut self, attribute: Attributes) {
        self.data[11] = attribute.get_value();
    }

    /// Check if this entry is a VFAT long entry.
    pub fn is_long_file_name(&self) -> bool {
        self.attribute().is_lfn()
    }

    /// Read the part of the LFN from this entry or return None if not a LFN.
    pub fn long_file_name_raw(&self) -> Option<LongFileName> {
        if self.is_long_file_name() {
            Some(LongFileName::from_lfn_dir_entry(&self.as_lfn_entry()))
        } else {
            None
        }
    }

    /// Read the SFN of this entry or return None if not a SFN.
    pub fn short_name(&self) -> Option<ShortFileName> {
        if !self.is_long_file_name() {
            Some(ShortFileName::from_slice(&self.data[0..11]))
        } else {
            None
        }
    }

    /// Set the LFN order index.
    pub fn set_lfn_index(&mut self, index: u8) {
        self.data[0] = index;
    }

    /// Set the SFN in the 8.3 entry.
    pub fn set_short_name(&mut self, short_name: &ShortFileName) {
        self.data[0..11].copy_from_slice(&short_name.as_bytes());
    }

    /// Set the LFN in the VFAT long entry.
    pub fn set_lfn_entry(&mut self, lfn: &str) {
        let lfn = LongFileName::from_utf8(lfn);

        let lfn = lfn.as_contents();

        for (i, entry) in lfn.iter().enumerate().take(5) {
            let index = 1 + i * 2;
            self.data[index..index + 2].copy_from_slice(&entry.to_le_bytes());
        }

        for i in 0..6 {
            let index = 0xE + i * 2;
            let i = i + 5;

            self.data[index..index + 2].copy_from_slice(&lfn[i].to_le_bytes());
        }

        for i in 0..2 {
            let index = 0x1C + i * 2;
            let i = i + 11;
            self.data[index..index + 2].copy_from_slice(&lfn[i].to_le_bytes());
        }
    }

    /// Read the raw data as a VFAT long entry.
    pub fn as_lfn_entry(&self) -> LongFileNameDirEntry {
        LongFileNameDirEntry::from_data(&self.data)
    }

    /// Set the LFN checksum of the DOS name.
    pub fn set_lfn_checksum(&mut self, checksum: u8) {
        self.data[13] = checksum;
    }

    /// Get the child cluster used by this entry.
    pub fn get_cluster(&self) -> Cluster {
        let high_cluster = u32::from(u16::from_le_bytes(self.data[20..22].try_into().unwrap()));
        let low_cluster = u32::from(u16::from_le_bytes(self.data[26..28].try_into().unwrap()));

        Cluster(low_cluster | (high_cluster << 16))
    }

    /// Set the child cluster of this entry.
    pub fn set_cluster(&mut self, cluster: Cluster) {
        let value = cluster.0;
        let high_cluster = ((value >> 16) & 0xFFFF) as u16;
        let low_cluster = (value & 0xFFFF) as u16;

        self.data[20..22].copy_from_slice(&high_cluster.to_le_bytes());
        self.data[26..28].copy_from_slice(&low_cluster.to_le_bytes());
    }

    /// Return the file size of this entry. Always zero if a directory.
    pub fn get_file_size(&self) -> u32 {
        u32::from_le_bytes(self.data[28..32].try_into().unwrap())
    }

    /// Set the file size of the entry.
    /// NOTE: If set to 0, set_cluster is also called to reset the cluster. If any operations need to be done on the cluster, always ensure that you retrieve it first before calling this.
    pub fn set_file_size(&mut self, new_size: u32) {
        self.data[28..32].copy_from_slice(&new_size.to_le_bytes());

        // Size is 0 so cluster need to be reset
        if new_size == 0 {
            self.set_cluster(Cluster(0))
        }
    }

    /// Retrieve the creation datetime of this 8.3 entry.
    pub fn get_creation_datetime(&self) -> FatDateTime {
        let raw_time = u16::from_le_bytes(self.data[14..16].try_into().unwrap());
        let seconds = ((raw_time & 0x1f) << 1) as u8;
        let minutes = ((raw_time >> 5) & 0x3f) as u8;
        let hour = ((raw_time >> 11) & 0x1f) as u8;

        let raw_date = u16::from_le_bytes(self.data[16..18].try_into().unwrap());
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

    /// Retrieve the last access datetime of this 8.3 entry.
    pub fn get_last_access_date(&self) -> FatDateTime {
        let raw_date = u16::from_le_bytes(self.data[18..20].try_into().unwrap());
        let day = (raw_date & 0x1f) as u8;
        let month = ((raw_date >> 5) & 0xf) as u8;
        let year = (raw_date >> 9) & 0x7f;
        FatDateTime::new(1980 + year, month, day, 0, 0, 0, 0)
    }

    /// Retrieve the last modification datetime of this 8.3 entry.
    pub fn get_modification_datetime(&self) -> FatDateTime {
        let raw_time = u16::from_le_bytes(self.data[22..24].try_into().unwrap());
        let seconds = ((raw_time & 0x1f) << 1) as u8;
        let minutes = ((raw_time >> 5) & 0x3f) as u8;
        let hour = ((raw_time >> 11) & 0x1f) as u8;

        let raw_date = u16::from_le_bytes(self.data[24..26].try_into().unwrap());
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
