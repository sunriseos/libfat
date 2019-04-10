//! FATs managment.

use super::filesystem::FatFileSystem;
use super::Cluster;
use super::FatFsType;
use byteorder::{ByteOrder, LittleEndian};
use storage_device::StorageDevice;

use crate::FileSystemError;

#[derive(Debug, Copy, Clone, PartialEq)]
/// Represent a cluster chan value.
pub enum FatValue {
    /// Represent a free cluster.
    Free,

    /// Represent a used cluster.
    Data(u32),

    /// Represent a corrupted cluster (bad sectors)
    Bad,

    /// Represent the end of a cluster chain.
    EndOfChain,
}

/// Util iterator used to simplify iteration over cluster.
#[derive(Copy, Clone)]
pub struct FatClusterIter<'a, S: StorageDevice> {
    /// The filesystem it belongs to.
    pub(crate) fs: &'a FatFileSystem<S>,

    /// The last cluster returned.
    current_cluster: Option<Cluster>,

    /// The last FatValue used.
    last_fat: Option<FatValue>,
}

impl<'a, S: StorageDevice> FatClusterIter<'a, S> {
    /// Create a new Cluster iteractor starting at ``cluster``.
    pub fn new(fs: &'a FatFileSystem<S>, cluster: Cluster) -> FatClusterIter<'a, S> {
        let fat_value = FatValue::get(fs, cluster).ok();
        FatClusterIter {
            fs,
            current_cluster: Some(cluster),
            last_fat: fat_value,
        }
    }
}

impl<'a, S: StorageDevice> Iterator for FatClusterIter<'a, S> {
    type Item = Cluster;
    fn next(&mut self) -> Option<Cluster> {
        let res = self.current_cluster?;

        match self.last_fat {
            Some(FatValue::Data(data)) => {
                self.current_cluster = Some(Cluster(data));
                self.last_fat = FatValue::get(&self.fs, self.current_cluster?).ok();
            }
            _ => self.current_cluster = None,
        };

        Some(res)
    }
}

impl FatValue {
    /// Create a ``FatValue`` from a raw FAT32 value.
    fn from_fat32_value(val: u32) -> Self {
        match val {
            0 => FatValue::Free,
            0x0FFF_FFF7 => FatValue::Bad,
            0x0FFF_FFF8..=0x0FFF_FFFF => FatValue::EndOfChain,
            n => FatValue::Data(n as u32),
        }
    }

    /// Convert a ```FatValue``` to a raw FAT32 value.
    fn to_fat32_value(self) -> u32 {
        match self {
            FatValue::Free => 0,
            FatValue::Bad => 0x0FFF_FFF7,
            FatValue::EndOfChain => 0x0FFF_FFFF,
            FatValue::Data(n) => n,
        }
    }

    /// Create a ``FatValue`` from a raw FAT16 value.
    fn from_fat16_value(val: u16) -> Self {
        match val {
            0 => FatValue::Free,
            0xFFF7 => FatValue::Bad,
            0xFFF8..=0xFFFF => FatValue::EndOfChain,
            n => FatValue::Data(u32::from(n)),
        }
    }

    /// Convert a ```FatValue``` to a raw FAT16 value.
    fn to_fat16_value(self) -> u16 {
        match self {
            FatValue::Free => 0,
            FatValue::Bad => 0xFFF7,
            FatValue::EndOfChain => 0xFFFF,
            FatValue::Data(n) => n as u16,
        }
    }

    /// Create a ``FatValue`` from a raw FAT12 value.
    fn from_fat12_value(val: u16) -> Self {
        match val {
            0 => FatValue::Free,
            0xFF7 => FatValue::Bad,
            0xFF8..=0xFFF => FatValue::EndOfChain,
            n => FatValue::Data(u32::from(n)),
        }
    }

    /// Convert a ```FatValue``` to a raw FAT12 value.
    fn to_fat12_value(self) -> u16 {
        match self {
            FatValue::Free => 0,
            FatValue::Bad => 0xFF7,
            FatValue::EndOfChain => 0xFFF,
            FatValue::Data(n) => n as u16,
        }
    }

    /// Create a ```FatValue``` from a raw cluster.
    /// Used internally in get and raw_put.
    fn from_cluster<S: StorageDevice>(
        fs: &FatFileSystem<S>,
        cluster: Cluster,
        fat_index: u32,
    ) -> Result<(Self, u64), FileSystemError> {
        let fat_offset = cluster.to_fat_offset(fs.boot_record.fat_type);
        let cluster_storage_offset = cluster.to_fat_bytes_offset(fs)
            + u64::from(fat_index * fs.boot_record.fat_size())
                * u64::from(fs.boot_record.bytes_per_block());

        let cluster_offset = u64::from(fat_offset % u32::from(fs.boot_record.bytes_per_block()));
        let partition_storage_offset = fs.partition_start + cluster_storage_offset + cluster_offset;

        match fs.boot_record.fat_type {
            FatFsType::Fat32 => {
                let mut data = [0x0u8; 4];
                fs.storage_device
                    .read(partition_storage_offset, &mut data)
                    .or(Err(FileSystemError::ReadFailed))?;

                Ok((
                    Self::from_fat32_value(LittleEndian::read_u32(&data) & 0x0FFF_FFFF),
                    cluster_storage_offset,
                ))
            }
            FatFsType::Fat16 => {
                let mut data = [0x0u8; 2];
                fs.storage_device
                    .read(partition_storage_offset, &mut data)
                    .or(Err(FileSystemError::ReadFailed))?;

                Ok((
                    Self::from_fat16_value(LittleEndian::read_u16(&data)),
                    cluster_storage_offset,
                ))
            }
            FatFsType::Fat12 => {
                let mut data = [0x0u8; 2];
                fs.storage_device
                    .read(partition_storage_offset, &mut data)
                    .or(Err(FileSystemError::ReadFailed))?;

                let mut value = LittleEndian::read_u16(&data);

                value = if (cluster.0 & 1) == 1 {
                    value >> 4
                } else {
                    value & 0x0FFF
                };

                Ok((Self::from_fat12_value(value), cluster_storage_offset))
            }
            _ => unimplemented!(),
        }
    }

    /// Get the ```FatValue``` of a given cluster.
    pub fn get<S: StorageDevice>(
        fs: &FatFileSystem<S>,
        cluster: Cluster,
    ) -> Result<FatValue, FileSystemError> {
        Ok(FatValue::from_cluster(fs, cluster, 0)?.0)
    }

    /// Write the given ``FatValue``at a given ``Cluster`` in one FAT.
    fn raw_put<S: StorageDevice>(
        fs: &FatFileSystem<S>,
        cluster: Cluster,
        value: FatValue,
        fat_index: u32,
    ) -> Result<(), FileSystemError> {
        let (res, cluster_storage_offset) = FatValue::from_cluster(fs, cluster, fat_index)?;

        let fat_offset = cluster.to_fat_offset(fs.boot_record.fat_type);
        let cluster_offset = u64::from(fat_offset % u32::from(fs.boot_record.bytes_per_block()));
        let partition_storage_offset = fs.partition_start + cluster_storage_offset + cluster_offset;

        // no write needed
        if res == value {
            return Ok(());
        }

        match fs.boot_record.fat_type {
            FatFsType::Fat32 => {
                let mut data = [0x0u8; 4];
                LittleEndian::write_u32(&mut data, value.to_fat32_value() & 0x0FFF_FFFF);
                fs.storage_device
                    .write(partition_storage_offset, &data)
                    .or(Err(FileSystemError::WriteFailed))?;
            }
            FatFsType::Fat16 => {
                let mut data = [0x0u8; 2];
                LittleEndian::write_u16(&mut data, value.to_fat16_value());
                fs.storage_device
                    .write(partition_storage_offset, &data)
                    .or(Err(FileSystemError::WriteFailed))?;
            }
            FatFsType::Fat12 => {
                let mut value = value.to_fat12_value();
                value = if (cluster.0 & 1) == 1 {
                    value >> 4
                } else {
                    value & 0x0FFF
                };

                let mut data = [0x0u8; 2];
                LittleEndian::write_u16(&mut data, value);
                fs.storage_device
                    .write(partition_storage_offset, &data)
                    .or(Err(FileSystemError::WriteFailed))?;
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    /// Write the given ``FatValue``at a given ``Cluster`` in all FATs.
    pub fn put<S: StorageDevice>(
        fs: &FatFileSystem<S>,
        cluster: Cluster,
        value: FatValue,
    ) -> Result<(), FileSystemError> {
        for fat_index in 0..u32::from(fs.boot_record.fats_count()) {
            Self::raw_put(fs, cluster, value, fat_index)?;
        }
        Ok(())
    }
}

/// Get the last cluster of a cluster chain.
pub fn get_last_cluster<S: StorageDevice>(
    fs: &FatFileSystem<S>,
    cluster: Cluster,
) -> Result<Cluster, FileSystemError> {
    Ok(get_last_and_previous_cluster(fs, cluster)?.0)
}

/// Get the last cluster and prevous cluster of a cluster chain.
pub fn get_last_and_previous_cluster<S: StorageDevice>(
    fs: &FatFileSystem<S>,
    cluster: Cluster,
) -> Result<(Cluster, Option<Cluster>), FileSystemError> {
    let mut previous_cluster = None;
    let mut current_cluster = cluster;

    while let FatValue::Data(val) = FatValue::get(fs, current_cluster)? {
        previous_cluster = Some(current_cluster);
        current_cluster = Cluster(val);
    }

    Ok((current_cluster, previous_cluster))
}

/// Compute the whole cluster count of a given FileSystem.
pub fn get_free_cluster_count<S: StorageDevice>(
    fs: &FatFileSystem<S>,
) -> Result<u32, FileSystemError> {
    let mut current_cluster = Cluster(2);

    let mut res = 0;

    while current_cluster.0 < fs.boot_record.cluster_count {
        if let FatValue::Free = FatValue::get(fs, current_cluster)? {
            res += 1;
        }

        current_cluster = Cluster(current_cluster.0 + 1);
    }

    Ok(res)
}
