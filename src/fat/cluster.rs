use crate::fat::block::{BlockDevice, BlockIndex};
use crate::fat::FatFileSystem;


pub struct Cluster(pub u32);


impl Cluster {
    pub fn to_block_index<T>(&self, fs: &FatFileSystem<T>) -> BlockIndex where T: BlockDevice {
        let first_block_of_cluster = (self.0 - 2) * u32::from(fs.boot_record.blocks_per_cluster());
        BlockIndex(fs.partition_start.0 + fs.first_data_offset.0 + first_block_of_cluster)
    }
}