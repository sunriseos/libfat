use crate::fat::block::BlockIndex;

pub struct Cluster(pub u32);


impl Cluster {
    pub fn to_block_index(&self, start: u32, sector_per_cluster: u32) -> BlockIndex {
        BlockIndex(start + self.0 * sector_per_cluster - (2 * sector_per_cluster))
    }
}