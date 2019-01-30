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

impl FatValue {
    pub fn get<T>(fs: &FatFileSystem<T>, cluster: &Cluster) -> Result<FatValue, FileSystemError> where T: BlockDevice {
        let mut blocks = [Block::new()];

        let fat_offset = cluster.0 * 4;
        let cluster_block_index = BlockIndex(fs.partition_start.0 + fs.first_data_offset.0 + (fat_offset / Block::LEN_U32));
        let cluster_offset = (fat_offset % Block::LEN_U32) as usize;

        fs.block_device.read(&mut blocks, cluster_block_index).or(Err(FileSystemError::ReadFailed))?;

        let block = &blocks[0];

        let val = LittleEndian::read_u32(&block[cluster_offset..cluster_offset+4]) & 0x0FFFFFFF;

        let res = match val {
            0 => FatValue::Free,
            0x0FFFFFF7 => FatValue::Bad,
            0x0FFFFFF8...0x0FFFFFFF => FatValue::EndOfChain,
            n => FatValue::Data(n as u32)
        };

        info!("{:?} 0x{:x}", res, cluster.0);
        Ok(res)
    }
}


pub fn get_cluster_count<T>(fs: &FatFileSystem<T>, cluster: &Cluster) -> Result<u32, FileSystemError> where T: BlockDevice {
    let mut res = 0;
    let mut i = cluster.0;

    let mut fat_value = FatValue::get(fs, cluster)?;

    if fat_value != FatValue::EndOfChain && fat_value != FatValue::Free {
        res += 1;
    }

    while fat_value != FatValue::EndOfChain && fat_value != FatValue::Free {
        i += 1;
        res += 1;
        fat_value = FatValue::get(fs, &Cluster(i))?;
    }

    Ok(res)
}
