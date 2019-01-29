use crate::fat::FatFileSystem;
use crate::fat::Cluster;
use crate::fat::{Block, BlockIndex, BlockDevice};

use crate::FileSystemError;

pub enum FatValue {
    Free,
    Data(u32),
    Bad,
    EndOfChain,
}

impl FatValue {
    pub fn get<T>(fs: &FatFileSystem<T>, cluster: &Cluster) -> Result<u32, FileSystemError> where T: BlockDevice {
        let mut blocks = [Block::new()];

        let mut cluster_offset = BlockIndex(fs.partition_start.0 + fs.boot_record.reserved_block_count() as u32 + ((cluster.0 * 4) / Block::LEN_U32));

        fs.block_device.read(&mut blocks, cluster_offset).or(Err(FileSystemError::ReadFailed))?;

        let block = &blocks[0];
        unimplemented!()
    }
}


pub fn get_cluster_count<T>(fs: &FatFileSystem<T>, cluster: &Cluster) -> Result<u32, FileSystemError> where T: BlockDevice {
    let mut res = 0;

    let fat_value = FatValue::get(fs, cluster);


    Ok(res)
}
