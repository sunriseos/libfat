#[derive(Debug)]
pub enum Error {
    Unknown
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Clone)]
pub struct Block {
    pub contents: [u8; Block::LEN]
}

#[derive(Copy, Clone)]
pub struct BlockIndex(pub u32);

#[derive(Copy, Clone)]
pub struct BlockCount(pub u32);

impl Block {
    pub const LEN: usize = 512;
    pub const LEN_U32: u32 = Self::LEN as u32;

    pub fn new() -> Block {
        Block { contents: [0u8; Self::LEN] }
    }

    pub fn as_contents(&self) -> [u8; Block::LEN] {
        self.contents
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
    pub fn into_offset(&self) -> u64 {
        u64::from(self.0) * (Block::LEN as u64)
    }
}

impl BlockCount {
    pub fn into_size(&self) -> u64 {
        u64::from(self.0) * (Block::LEN as u64)
    }
}

pub trait BlockDevice : Sized {
    fn read(&self, blocks: &mut [Block], index: BlockIndex) -> Result<()>;
    fn write(&self, blocks: &[Block], index: BlockIndex) -> Result<()>;
    fn count(&self) -> Result<BlockCount>;
}
