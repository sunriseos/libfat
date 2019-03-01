use super::filesystem::FatFileSystem;
use super::Cluster;
use super::{Block, BlockDevice};
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
            0x0FFF_FFF8...0x0FFF_FFFF => FatValue::EndOfChain,
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
            .read(&mut blocks, cluster_block_index)
            .or(Err(FileSystemError::ReadFailed))?;

        let res = FatValue::from_block(&blocks[0], cluster_offset);

        Ok(res)
    }

    // TODO: Write to backup FAT
    pub fn put<T>(
        fs: &FatFileSystem<T>,
        cluster: Cluster,
        value: FatValue,
    ) -> Result<(), FileSystemError>
    where
        T: BlockDevice,
    {
        let mut blocks = [Block::new()];

        let fat_offset = cluster.to_fat_offset();
        let cluster_block_index = cluster.to_fat_block_index(fs);
        let cluster_offset = (fat_offset % Block::LEN_U32) as usize;

        fs.block_device
            .read(&mut blocks, cluster_block_index)
            .or(Err(FileSystemError::ReadFailed))?;

        let res = FatValue::from_block(&blocks[0], cluster_offset);

        // no write needed
        if res == value {
            return Ok(());
        }

        let value = value.to_u32() & 0x0FFF_FFFF;
        LittleEndian::write_u32(&mut blocks[0][cluster_offset..cluster_offset + 4], value);

        fs.block_device
            .write(&blocks, cluster_block_index)
            .or(Err(FileSystemError::WriteFailed))?;

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
    let mut current_cluster = cluster;

    while let FatValue::Data(val) = FatValue::get(fs, current_cluster)? {
        current_cluster = Cluster(val);
    }

    Ok(current_cluster)
}
