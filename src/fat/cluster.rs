use crate::fat::block::{Block, BlockDevice, BlockIndex};
use crate::fat::FatFileSystem;

#[derive(Debug, Clone)]
pub struct Cluster(pub u32);

impl Cluster {
    pub fn to_data_block_index<T>(&self, fs: &FatFileSystem<T>) -> BlockIndex
    where
        T: BlockDevice,
    {
        let first_block_of_cluster = (self.0 - 2) * u32::from(fs.boot_record.blocks_per_cluster());
        BlockIndex(fs.partition_start.0 + fs.first_data_offset.0 + first_block_of_cluster)
    }

    pub fn to_fat_offset(&self) -> u32 {
        self.0 * 4
    }

    pub fn to_fat_block_index<T>(&self, fs: &FatFileSystem<T>) -> BlockIndex
    where
        T: BlockDevice,
    {
        let fat_offset = self.to_fat_offset();

        let fat_block_index = fs.partition_start.0
            + u32::from(fs.boot_record.reserved_block_count())
            + (fat_offset / Block::LEN_U32);
        BlockIndex(fat_block_index)
    }
}
