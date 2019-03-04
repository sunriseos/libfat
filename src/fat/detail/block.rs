use super::cluster::Cluster;
use super::filesystem::FatFileSystem;
use super::table::FatClusterIter;

#[derive(Debug)]
pub enum Error {
    Unknown,
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Clone)]
pub struct Block {
    pub contents: [u8; Block::LEN],
}

#[derive(Debug, Copy, Clone)]
pub struct BlockIndex(pub u32);

#[derive(Debug, Copy, Clone)]
pub struct BlockCount(pub u32);

impl Block {
    pub const LEN: usize = 512;
    pub const LEN_U32: u32 = Self::LEN as u32;

    pub fn new() -> Block {
        Default::default()
    }

    pub fn as_contents(&self) -> [u8; Block::LEN] {
        self.contents
    }
}

impl Default for Block {
    fn default() -> Self {
        Block {
            contents: [0u8; Self::LEN],
        }
    }
}

impl core::ops::Deref for Block {
    type Target = [u8; Block::LEN];
    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl core::ops::DerefMut for Block {
    fn deref_mut(&mut self) -> &mut [u8; Block::LEN] {
        &mut self.contents
    }
}

impl BlockIndex {
    pub fn into_offset(self) -> u64 {
        u64::from(self.0) * (Block::LEN as u64)
    }
}

impl BlockCount {
    pub fn into_size(self) -> u64 {
        u64::from(self.0) * (Block::LEN as u64)
    }
}

pub trait BlockDevice: Sized {
    fn raw_read(&self, blocks: &mut [Block], index: BlockIndex) -> Result<()>;
    fn raw_write(&self, blocks: &[Block], index: BlockIndex) -> Result<()>;

    fn read(&self, blocks: &mut [Block], partition_start: BlockIndex, index: BlockIndex) -> Result<()> {
        self.raw_read(blocks, BlockIndex(partition_start.0 + index.0))
    }

    fn write(&self, blocks: &[Block], partition_start: BlockIndex, index: BlockIndex) -> Result<()> {
        self.raw_write(blocks, BlockIndex(partition_start.0 + index.0))
    }

    fn count(&self) -> Result<BlockCount>;
}

// Util iterator used to simplify iteration over cluster
pub struct BlockIndexClusterIter<'a, T> {
    pub cluster_iter: FatClusterIter<'a, T>,
    pub last_cluster: Option<Cluster>,
    pub block_index: Option<BlockIndex>,
    pub counter: usize,
}

impl<'a, T> BlockIndexClusterIter<'a, T>
where
    T: BlockDevice,
{
    pub fn new(
        fs: &'a FatFileSystem<T>,
        cluster: Cluster,
        block_index: Option<BlockIndex>,
    ) -> Self {
        let blocks_per_cluster = fs.boot_record.blocks_per_cluster() as usize;

        let (cluster, block_index) = if let Some(block_index) = block_index {
            let cluster_offset = block_index.0 / blocks_per_cluster as u32;
            let block_index = BlockIndex(block_index.0 % blocks_per_cluster as u32);
            (Cluster(cluster.0 + cluster_offset), Some(block_index))
        } else {
            (cluster, block_index)
        };

        BlockIndexClusterIter {
            counter: blocks_per_cluster,
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
