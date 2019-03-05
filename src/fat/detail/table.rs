use super::filesystem::FatFileSystem;
use super::Cluster;
use super::{Block, BlockDevice, BlockIndex};
use byteorder::{ByteOrder, LittleEndian};

use crate::FileSystemError;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FatValue {
    Free,
    Data(u32),
    Bad,
    EndOfChain,
}

pub struct FatClusterIter<'a, T> {
    pub fs: &'a FatFileSystem<T>,
    current_cluster: Option<Cluster>,
    last_fat: Option<FatValue>,
}

impl<'a, T> FatClusterIter<'a, T>
where
    T: BlockDevice,
{
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
    pub fn from_u32(val: u32) -> FatValue {
        match val {
            0 => FatValue::Free,
            0x0FFF_FFF7 => FatValue::Bad,
            0x0FFF_FFF8..=0x0FFF_FFFF => FatValue::EndOfChain,
            n => FatValue::Data(n as u32),
        }
    }

    pub fn to_u32(self) -> u32 {
        match self {
            FatValue::Free => 0,
            FatValue::Bad => 0x0FFF_FFF7,
            FatValue::EndOfChain => 0x0FFF_FFFF,
            FatValue::Data(n) => n,
        }
    }

    pub fn from_block(block: &Block, cluster_offset: usize) -> FatValue {
        let val = LittleEndian::read_u32(&block[cluster_offset..cluster_offset + 4]) & 0x0FFF_FFFF;
        FatValue::from_u32(val)
    }

    pub fn get<T>(fs: &FatFileSystem<T>, cluster: Cluster) -> Result<FatValue, FileSystemError>
    where
        T: BlockDevice,
    {
        let mut blocks = [Block::new()];

        let fat_offset = cluster.to_fat_offset();
        let cluster_block_index = cluster.to_fat_block_index(fs);
        let cluster_offset = (fat_offset % Block::LEN_U32) as usize;

        fs.block_device
            .read(&mut blocks, fs.partition_start, cluster_block_index)
            .or(Err(FileSystemError::ReadFailed))?;

        let res = FatValue::from_block(&blocks[0], cluster_offset);

        Ok(res)
    }

    fn raw_put<T>(
        fs: &FatFileSystem<T>,
        cluster: Cluster,
        value: FatValue,
        fat_index: u32,
    ) -> Result<(), FileSystemError>
    where
        T: BlockDevice,
    {
        let mut blocks = [Block::new()];

        let fat_offset = cluster.to_fat_offset();
        let cluster_block_index =
            BlockIndex(cluster.to_fat_block_index(fs).0 + (fat_index * fs.boot_record.fat_size()));
        let cluster_offset = (fat_offset % Block::LEN_U32) as usize;

        fs.block_device
            .read(&mut blocks, fs.partition_start, cluster_block_index)
            .or(Err(FileSystemError::ReadFailed))?;

        let res = FatValue::from_block(&blocks[0], cluster_offset);

        // no write needed
        if res == value {
            return Ok(());
        }

        let value = value.to_u32() & 0x0FFF_FFFF;
        LittleEndian::write_u32(&mut blocks[0][cluster_offset..cluster_offset + 4], value);

        fs.block_device
            .write(&blocks, fs.partition_start, cluster_block_index)
            .or(Err(FileSystemError::WriteFailed))?;

        Ok(())
    }

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

pub fn get_cluster_count<T>(fs: &FatFileSystem<T>, cluster: Cluster) -> Result<u32, FileSystemError>
where
    T: BlockDevice,
{
    let mut res = 1;
    let mut current_cluster = cluster;

    while let FatValue::Data(val) = FatValue::get(fs, current_cluster)? {
        res += 1;
        current_cluster = Cluster(val);
    }

    Ok(res)
}

pub fn get_last_cluster<T>(
    fs: &FatFileSystem<T>,
    cluster: Cluster,
) -> Result<Cluster, FileSystemError>
where
    T: BlockDevice,
{
    Ok(get_last_and_previous_cluster(fs, cluster)?.0)
}

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


pub fn get_free_cluster_count<T>(
    fs: &FatFileSystem<T>
) -> Result<u32, FileSystemError>
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