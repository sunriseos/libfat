//! FATs managment.

use super::filesystem::FatFileSystem;
use super::Cluster;
use super::FatFsType;
use byteorder::{ByteOrder, LittleEndian};
use libfs::block::{Block, BlockDevice, BlockIndex};

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
pub struct FatClusterIter<'a, T> {
    /// The filesystem it belongs to.
    pub(crate) fs: &'a FatFileSystem<T>,

    /// The last cluster returned.
    current_cluster: Option<Cluster>,

    /// The last FatValue used.
    last_fat: Option<FatValue>,
}

impl<'a, T> FatClusterIter<'a, T>
where
    T: BlockDevice,
{
    /// Create a new Cluster iteractor starting at ``cluster``.
    pub fn new(fs: &'a FatFileSystem<T>, cluster: Cluster) -> FatClusterIter<'a, T> {
        let fat_value = FatValue::get(fs, cluster).ok();
        FatClusterIter {
            fs,
            current_cluster: Some(cluster),
            last_fat: fat_value,
        }
    }
}

impl<'a, T> Iterator for FatClusterIter<'a, T>
where
    T: BlockDevice,
{
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
    fn from_cluster<T>(
        fs: &FatFileSystem<T>,
        blocks: &mut [Block],
        cluster: Cluster,
        fat_index: u32,
    ) -> Result<(Self, usize, BlockIndex, usize), FileSystemError>
    where
        T: BlockDevice,
    {
        let fat_offset = cluster.to_fat_offset(fs.boot_record.fat_type);
        let cluster_block_index = BlockIndex(
            cluster.to_fat_block_index(fs).0 + u64::from(fat_index * fs.boot_record.fat_size()),
        );
        let cluster_offset = (fat_offset % u32::from(fs.boot_record.bytes_per_block())) as usize;

        fs.block_device
            .read(blocks, fs.partition_start, cluster_block_index)
            .or(Err(FileSystemError::ReadFailed))?;

        let block = &blocks[0];

        match fs.boot_record.fat_type {
            FatFsType::Fat32 => Ok((
                Self::from_fat32_value(
                    LittleEndian::read_u32(&block[cluster_offset..cluster_offset + 4])
                        & 0x0FFF_FFFF,
                ),
                cluster_offset,
                cluster_block_index,
                1,
            )),
            FatFsType::Fat16 => Ok((
                Self::from_fat16_value(LittleEndian::read_u16(
                    &block[cluster_offset..cluster_offset + 2],
                )),
                cluster_offset,
                cluster_block_index,
                1,
            )),
            FatFsType::Fat12 => {
                let mut value = if cluster_offset + 2 > Block::LEN {
                    fs.block_device
                        .read(&mut blocks[1..], fs.partition_start, cluster_block_index)
                        .or(Err(FileSystemError::ReadFailed))?;
                    let mut buffer: [u8; 2] = [0x0; 2];
                    buffer[0] = blocks[0][Block::LEN - 1];
                    buffer[1] = blocks[1][0];
                    LittleEndian::read_u16(&buffer)
                } else {
                    LittleEndian::read_u16(&block[cluster_offset..cluster_offset + 2])
                };
                value = if (cluster.0 & 1) == 1 {
                    value >> 4
                } else {
                    value & 0x0FFF
                };

                Ok((
                    Self::from_fat12_value(value),
                    cluster_offset,
                    cluster_block_index,
                    2,
                ))
            }
            _ => unimplemented!(),
        }
    }

    /// Get the ```FatValue``` of a given cluster.
    pub fn get<T>(fs: &FatFileSystem<T>, cluster: Cluster) -> Result<FatValue, FileSystemError>
    where
        T: BlockDevice,
    {
        let mut blocks = [Block::new(), Block::new()];
        Ok(FatValue::from_cluster(fs, &mut blocks, cluster, 0)?.0)
    }

    /// Write the given ``FatValue``at a given ``Cluster`` in one FAT.
    fn raw_put<T>(
        fs: &FatFileSystem<T>,
        cluster: Cluster,
        value: FatValue,
        fat_index: u32,
    ) -> Result<(), FileSystemError>
    where
        T: BlockDevice,
    {
        let mut blocks = [Block::new(), Block::new()];

        let (res, cluster_offset, cluster_block_index, block_count) =
            FatValue::from_cluster(fs, &mut blocks, cluster, fat_index)?;

        // no write needed
        if res == value {
            return Ok(());
        }

        match fs.boot_record.fat_type {
            FatFsType::Fat32 => LittleEndian::write_u32(
                &mut blocks[0][cluster_offset..cluster_offset + 4],
                value.to_fat32_value() & 0x0FFF_FFFF,
            ),
            FatFsType::Fat16 => LittleEndian::write_u16(
                &mut blocks[0][cluster_offset..cluster_offset + 2],
                value.to_fat16_value(),
            ),
            FatFsType::Fat12 => {
                let mut value = value.to_fat12_value();
                value = if (cluster.0 & 1) == 1 {
                    value >> 4
                } else {
                    value & 0x0FFF
                };

                if cluster_offset + 2 > Block::LEN {
                    let mut buffer: [u8; 2] = [0x0; 2];
                    LittleEndian::write_u16(&mut buffer, value);
                    blocks[0][Block::LEN - 1] = buffer[0];
                    blocks[1][0] = buffer[1];
                } else {
                    LittleEndian::write_u16(
                        &mut blocks[0][cluster_offset..cluster_offset + 2],
                        value,
                    );
                }
            }
            _ => unimplemented!(),
        }

        fs.block_device
            .write(
                &blocks[0..block_count],
                fs.partition_start,
                cluster_block_index,
            )
            .or(Err(FileSystemError::WriteFailed))?;

        Ok(())
    }

    /// Write the given ``FatValue``at a given ``Cluster`` in all FATs.
    pub fn put<T>(
        fs: &FatFileSystem<T>,
        cluster: Cluster,
        value: FatValue,
    ) -> Result<(), FileSystemError>
    where
        T: BlockDevice,
    {
        for fat_index in 0..u32::from(fs.boot_record.fats_count()) {
            Self::raw_put(fs, cluster, value, fat_index)?;
        }
        Ok(())
    }
}

/// Get the last cluster of a cluster chain.
pub fn get_last_cluster<T>(
    fs: &FatFileSystem<T>,
    cluster: Cluster,
) -> Result<Cluster, FileSystemError>
where
    T: BlockDevice,
{
    Ok(get_last_and_previous_cluster(fs, cluster)?.0)
}

/// Get the last cluster and prevous cluster of a cluster chain.
pub fn get_last_and_previous_cluster<T>(
    fs: &FatFileSystem<T>,
    cluster: Cluster,
) -> Result<(Cluster, Option<Cluster>), FileSystemError>
where
    T: BlockDevice,
{
    let mut previous_cluster = None;
    let mut current_cluster = cluster;

    while let FatValue::Data(val) = FatValue::get(fs, current_cluster)? {
        previous_cluster = Some(current_cluster);
        current_cluster = Cluster(val);
    }

    Ok((current_cluster, previous_cluster))
}

/// Compute the whole cluster count of a given FileSystem.
pub fn get_free_cluster_count<T>(fs: &FatFileSystem<T>) -> Result<u32, FileSystemError>
where
    T: BlockDevice,
{
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
