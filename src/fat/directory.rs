use crate::fat::cluster::Cluster;
use crate::fat::block::BlockDevice;
use crate::fat::FatFileSystem;
use crate::fat::table;

pub struct Directory<'a, T: BlockDevice> {
    pub cluster: Cluster,
    pub fs: &'a FatFileSystem<T>,
}

impl<'a, T> Directory<'a, T> where T: BlockDevice {
    pub fn test(&self) {
        let cluster_count = table::get_cluster_count(&self.fs, &self.cluster);
        info!("Hello World\n");
    }
}