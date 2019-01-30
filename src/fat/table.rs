use crate::fat::FatFileSystem;
use crate::fat::Cluster;
use crate::fat::{Block, BlockIndex, BlockDevice};
use byteorder::{ByteOrder, LittleEndian};

use crate::FileSystemError;

#[derive(Debug, PartialEq)]
pub enum FatValue {
    Free,
    Data(u32),
    Bad,
    EndOfChain,
}

pub struct FatClusterIter<'a, T> {
    fs: &'a FatFileSystem<T>,
    current_cluster: Cluster,
    last_fat: FatValue,
    is_end: bool,
}

impl <'a, T> FatClusterIter<'a, T> where T: BlockDevice {
    pub fn new(fs: &'a FatFileSystem<T>, cluster: &Cluster) -> Result<FatClusterIter<'a, T>, FileSystemError> {
        let fat_value = FatValue::get(fs, &cluster)?;
        Ok(FatClusterIter {
            fs,
            current_cluster: Cluster(cluster.0),
            last_fat: fat_value,
            is_end: false
        })
    }
}

impl<'a, T> Iterator for FatClusterIter<'a, T> where T: BlockDevice {
    type Item = Cluster;
    fn next(&mut self) -> Option<Cluster> {
        if self.is_end {
            return None;
        }

        let res = self.current_cluster.0;

        match self.last_fat {
            FatValue::Data(data) => {
                self.current_cluster = Cluster(data);
            }
            _ => {
                self.is_end = true;
            }
        };
        
        Some(Cluster(res))
    }
}

impl FatValue {

    pub fn from_u32(val: u32) -> FatValue {
        match val {
            0 => FatValue::Free,
            0x0FFFFFF7 => FatValue::Bad,
            0x0FFFFFF8...0x0FFFFFFF => FatValue::EndOfChain,
            n => FatValue::Data(n as u32)
        }
    }

    pub fn from_block(block: &Block, cluster_offset: usize) -> FatValue {
        let val = LittleEndian::read_u32(&block[cluster_offset..cluster_offset+4]) & 0x0FFFFFFF;

        FatValue::from_u32(val)
    }

    pub fn get<T>(fs: &FatFileSystem<T>, cluster: &Cluster) -> Result<FatValue, FileSystemError> where T: BlockDevice {
        let mut blocks = [Block::new()];

        let fat_offset = cluster.to_fat_offset();
        let cluster_block_index = cluster.to_fat_block_index(fs);
        let cluster_offset = (fat_offset % Block::LEN_U32) as usize;

        fs.block_device.read(&mut blocks, cluster_block_index).or(Err(FileSystemError::ReadFailed))?;

        let res = FatValue::from_block(&blocks[0], cluster_offset);

        Ok(res)
    }
}


pub fn get_cluster_count<T>(fs: &FatFileSystem<T>, cluster: &Cluster) -> Result<u32, FileSystemError> where T: BlockDevice {
    /*let mut res = 0;
    let mut cluster_index = cluster.0;

    let mut blocks = [Block::new()];

    let mut fat_offset = cluster_index * 4;
    let mut cluster_block_index = BlockIndex(fs.partition_start.0 + fs.first_data_offset.0 + (fat_offset / Block::LEN_U32));
    let mut cluster_offset = (fat_offset % Block::LEN_U32) as usize;
    let mut prev_cluster_block_index;

    fs.block_device.read(&mut blocks, cluster_block_index).or(Err(FileSystemError::ReadFailed))?;

    let mut fat_value = FatValue::from_block(&blocks[0], cluster_offset);

    if fat_value == FatValue::EndOfChain || fat_value == FatValue::Free {
        return Ok(0);
    }

    while fat_value != FatValue::EndOfChain && fat_value != FatValue::Free {
        prev_cluster_block_index = cluster_block_index;

        cluster_index += 1;
        res += 1;

        fat_offset = cluster_index * 4;
        cluster_block_index = BlockIndex(fs.partition_start.0 + fs.first_data_offset.0 + (fat_offset / Block::LEN_U32));
        cluster_offset = (fat_offset % Block::LEN_U32) as usize;

        if prev_cluster_block_index.0 != cluster_block_index.0 {
            fs.block_device.read(&mut blocks, cluster_block_index).or(Err(FileSystemError::ReadFailed))?;
        }

        match fat_value {
            FatValue::Data(val) => {
                info!("0x{:x}", val);
            },
            _ => {
                panic!();
            }
        };
        fat_value = FatValue::from_block(&blocks[0], cluster_offset);
    }*/

    let mut res = 1;
    let mut current_cluster = Cluster(cluster.0);

    loop {
        let mut fat_val = FatValue::get(fs, &current_cluster)?;

        match fat_val {
            FatValue::Data(val) => {
                res += 1;

                current_cluster = Cluster(val);
            }
            _ => break
        }
    }

    Ok(res)
}
