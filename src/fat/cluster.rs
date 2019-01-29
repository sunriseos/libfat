use crate::fat::block::BlockIndex;

pub struct Cluster(pub u32);


impl Cluster {
    pub fn to_block_index(&self, partition_start: BlockIndex, first_data_offset: BlockIndex, sector_per_cluster: u32) -> BlockIndex {
        BlockIndex(partition_start.0 + first_data_offset.0 + self.0 * sector_per_cluster - (2 * sector_per_cluster))
    }
}