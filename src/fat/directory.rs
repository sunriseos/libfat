use crate::fat::block::BlockDevice;
use crate::fat::cluster::Cluster;
use crate::fat::table;
use crate::fat::FatFileSystem;

pub struct Directory<'a, T: BlockDevice> {
    pub cluster: Cluster,
    pub fs: &'a FatFileSystem<T>,
}

impl<'a, T> Directory<'a, T>
where
    T: BlockDevice,
{
    pub fn test(&self) {
        let clusters = table::FatClusterIter::new(&self.fs, &self.cluster);
        for cluster in clusters {
            info!("Cluster: 0x{:x}\n", cluster.0);
        }
    }
}
