//! Block iterator.

use super::cluster::Cluster;
use super::filesystem::FatFileSystem;
use super::table::FatClusterIter;

use super::{BlockDevice, BlockIndex};

/// Util Iterator used to simplify iteration over block index and cluster.
#[derive(Copy, Clone)]
pub struct BlockIndexClusterIter<'a, T> {
    /// The cluster iterator.
    pub cluster_iter: FatClusterIter<'a, T>,

    /// The last cluster used.
    last_cluster: Option<Cluster>,

    /// The start block in the cluster to use.
    block_index: Option<BlockIndex>,

    /// The current iteration point in the cluster.
    counter: usize,
}

impl<'a, T> BlockIndexClusterIter<'a, T>
where
    T: BlockDevice,
{
    /// Create a new iterator from a cluster and a block index.
    pub fn new(
        fs: &'a FatFileSystem<T>,
        cluster: Cluster,
        block_index: Option<BlockIndex>,
    ) -> Self {
        let blocks_per_cluster = u64::from(fs.boot_record.blocks_per_cluster());

        let (cluster, block_index) = if let Some(block_index) = block_index {
            let cluster_offset = block_index.0 / blocks_per_cluster;
            let block_index = BlockIndex(block_index.0 % blocks_per_cluster);
            (
                Cluster(cluster.0 + cluster_offset as u32),
                Some(block_index),
            )
        } else {
            (cluster, block_index)
        };

        BlockIndexClusterIter {
            counter: blocks_per_cluster as usize,
            cluster_iter: FatClusterIter::new(fs, cluster),
            block_index,
            last_cluster: None,
        }
    }
}

impl<'a, T> Iterator for BlockIndexClusterIter<'a, T>
where
    T: BlockDevice,
{
    type Item = Cluster;
    fn next(&mut self) -> Option<Cluster> {
        let cluster_opt =
            if self.counter == self.cluster_iter.fs.boot_record.blocks_per_cluster() as usize {
                self.counter = self.block_index.or(Some(BlockIndex(0)))?.0 as usize;
                self.block_index = None;
                self.last_cluster = self.cluster_iter.next();
                self.last_cluster
            } else {
                self.last_cluster
            };

        let cluster = cluster_opt?;

        self.counter += 1;

        Some(cluster)
    }
}
