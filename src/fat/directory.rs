use crate::fat::block::{Block, BlockDevice};
use crate::fat::cluster::Cluster;
use crate::fat::name::ShortFileName;
use crate::fat::table;
use crate::fat::table::FatClusterIter;
use crate::fat::FatFileSystem;
use crate::FileSystemError;

pub struct Directory<'a, T: BlockDevice> {
    pub cluster: Cluster,
    pub fs: &'a FatFileSystem<T>,
}

// FIXME: We need a more hight level iterator to merge Long File Name, for now we don't support them
pub struct DirectoryIterator<'a, T: BlockDevice> {
    pub dir: &'a Directory<'a, T>,
    pub cluster_iter: FatClusterIter<'a, T>,
    pub last_cluster: Option<Cluster>,
    pub counter: usize,
}

impl<'a, T> DirectoryIterator<'a, T>
where
    T: BlockDevice,
{
    pub fn new(root: &'a Directory<'a, T>) -> Self {
        DirectoryIterator {
            dir: root,
            counter: 16,
            cluster_iter: FatClusterIter::new(&root.fs, &root.cluster),
            last_cluster: None,
        }
    }
}

impl<'a, T> Iterator for DirectoryIterator<'a, T>
where
    T: BlockDevice,
{
    type Item = FatDirEntry;
    fn next(&mut self) -> Option<FatDirEntry> {
        let cluster_opt = match self.counter {
            16 => {
                self.counter = 0;
                self.last_cluster = self.cluster_iter.next();
                self.last_cluster.clone()
            }
            _ => self.last_cluster.clone(),
        };

        let cluster = cluster_opt?;

        let mut blocks = [Block::new()];

        // FIXME: Custom Iterator to catches those errors
        self.dir
            .fs
            .block_device
            .read(&mut blocks, cluster.to_data_block_index(&self.dir.fs))
            .or(Err(FileSystemError::ReadFailed))
            .unwrap();

        let entry_start = self.counter * FatDirEntry::LEN;
        let entry_end = (self.counter + 1) * FatDirEntry::LEN;
        let dir_entry = FatDirEntry::from_raw(&blocks[0][entry_start..entry_end]);

        if dir_entry.is_end() {
            return None;
        }

        self.counter += 1;
        Some(dir_entry)
    }
}

#[derive(Debug)]
pub struct Attributes(u8);

impl Attributes {
    pub const READ_ONLY: u8 = 0x01;
    pub const HIDDEN: u8 = 0x02;
    pub const SYSTEM: u8 = 0x04;
    pub const VOLUME: u8 = 0x08;
    pub const DIRECTORY: u8 = 0x10;
    pub const ARCHIVE: u8 = 0x20;

    pub const LFN: u8 = Self::READ_ONLY | Self::HIDDEN | Self::SYSTEM | Self::VOLUME;

    pub fn new(value: u8) -> Attributes {
        Attributes(value)
    }

    pub fn is_read_only(self) -> bool {
        (self.0 & Self::READ_ONLY) == Self::READ_ONLY
    }

    pub fn is_hidden(self) -> bool {
        (self.0 & Self::HIDDEN) == Self::HIDDEN
    }

    pub fn is_system(self) -> bool {
        (self.0 & Self::SYSTEM) == Self::SYSTEM
    }

    pub fn is_volume(self) -> bool {
        (self.0 & Self::VOLUME) == Self::VOLUME
    }

    pub fn is_directory(self) -> bool {
        (self.0 & Self::DIRECTORY) == Self::DIRECTORY
    }

    pub fn is_archive(self) -> bool {
        (self.0 & Self::ARCHIVE) == Self::ARCHIVE
    }

    pub fn is_lfn(self) -> bool {
        (self.0 & Self::LFN) == Self::LFN
    }
}

pub enum FatDirEntryType {
    ShortFileName,
    LongFileName,
}

pub struct FatDirEntry {
    data: [u8; Self::LEN],
}

impl FatDirEntry {
    pub const LEN: usize = 32;

    pub fn from_raw(data: &[u8]) -> FatDirEntry {
        let mut data_copied = [0x0u8; Self::LEN];

        if data.len() != FatDirEntry::LEN {
            panic!()
        }

        for val in 0..data.len() {
            data_copied[val] = data[val];
        }
        FatDirEntry { data: data_copied }
    }

    pub fn is_end(&self) -> bool {
        self.data[0] == 0
    }

    pub fn attribute(&self) -> Attributes {
        Attributes::new(self.data[11])
    }

    pub fn is_long_file_name(&self) -> bool {
        self.attribute().is_lfn()
    }

    pub fn is_valid(&self) -> bool {
        // TODO: do we need more?
        self.is_end()
    }

    pub fn short_name(&self) -> Option<ShortFileName> {
        if !self.is_long_file_name() {
            Some(ShortFileName::from_data(&self.data[0..11]))
        } else {
            None
        }
    }
}

impl<'a> core::fmt::Debug for FatDirEntry {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "FatDirEntry {{ ")?;
        write!(f, "{:?} ", self.attribute())?;
        if self.is_long_file_name() {
            write!(f, "LongFileName {{ \"not yet implemented\" }}")?;
        } else {
            write!(
                f,
                "ShortFileName {{{:?}}}",
                self.short_name().unwrap().chars()
            )?;
        }
        write!(f, " }}")
    }
}

impl<'a, T> Directory<'a, T>
where
    T: BlockDevice,
{
    pub fn test(&self) -> Result<(), FileSystemError> {
        for dir_entry in self.iter() {
            info!("Dir Entry {:?}", dir_entry);
        }
        Ok(())
    }

    pub fn iter(&'a self) -> DirectoryIterator<'a, T> {
        DirectoryIterator::new(self)
    }
}
