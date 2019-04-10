//! Offset iterator.

use super::cluster::Cluster;
use super::filesystem::FatFileSystem;
use super::table::FatClusterIter;

use storage_device::StorageDevice;

/// Util Iterator used to simplify iteration over cluster.
#[derive(Copy, Clone)]
pub struct ClusterOffsetIter<'a, S: StorageDevice> {
    /// The cluster iterator.
    pub cluster_iter: FatClusterIter<'a, S>,

    /// The last cluster used.
    last_cluster: Option<Cluster>,

    /// The offset in the cluster to use.
    start_cluster_offset: Option<u64>,

    /// The current iteration point in the cluster.
    counter: usize,
}

impl<'a, S: StorageDevice> ClusterOffsetIter<'a, S> {
    /// Create a new iterator from a cluster and a block index.
    pub fn new(
        fs: &'a FatFileSystem<S>,
        cluster: Cluster,
        start_cluster_offset: Option<u64>,
    ) -> Self {
        let blocks_per_cluster = u64::from(fs.boot_record.blocks_per_cluster());

        let (cluster, start_cluster_offset) =
            if let Some(start_cluster_offset) = start_cluster_offset {
                let cluster_offset = start_cluster_offset / blocks_per_cluster;
                let start_cluster_offset = start_cluster_offset % blocks_per_cluster;
                (
                    Cluster(cluster.0 + cluster_offset as u32),
                    Some(start_cluster_offset),
                )
            } else {
                (cluster, start_cluster_offset)
            };

        ClusterOffsetIter {
            counter: blocks_per_cluster as usize,
            cluster_iter: FatClusterIter::new(fs, cluster),
            start_cluster_offset,
            last_cluster: None,
        }
    }
}

impl<'a, S: StorageDevice> Iterator for ClusterOffsetIter<'a, S> {
    type Item = Cluster;
    fn next(&mut self) -> Option<Cluster> {
        let cluster_opt =
            if self.counter == self.cluster_iter.fs.boot_record.blocks_per_cluster() as usize {
                self.counter = self.start_cluster_offset.or(Some(0))? as usize;
                self.start_cluster_offset = None;
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
