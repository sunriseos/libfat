use crate::fat::block::{Block, BlockDevice};
use crate::fat::cluster::Cluster;
use crate::fat::name::ShortFileName;
use crate::fat::table;
use crate::fat::FatFileSystem;
use crate::FileSystemError;

pub struct Directory<'a, T: BlockDevice> {
    pub cluster: Cluster,
    pub fs: &'a FatFileSystem<T>,
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

pub struct FatDirEntry<'a> {
    data: &'a [u8],
}

impl<'a> FatDirEntry<'a> {
    pub const LEN: usize = 32;

    pub fn from_raw(data: &[u8]) -> FatDirEntry {
        if data.len() < FatDirEntry::LEN {
            panic!()
        }
        FatDirEntry { data }
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

impl<'a> core::fmt::Debug for FatDirEntry<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "FatDirEntry {{ ")?;
        write!(f, "{:?}", self.attribute())?;
        if self.is_long_file_name() {

        } else {
            write!(
                f,
                "ShortFile Name {{{:?}}}",
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
        let clusters = table::FatClusterIter::new(&self.fs, &self.cluster);
        for cluster in clusters {
            let mut blocks = [Block::new()];
            info!(
                "Cluster: 0x{:x}\n",
                cluster.to_data_block_index(&self.fs).into_offset()
            );

            self.fs
                .block_device
                .read(&mut blocks, cluster.to_data_block_index(&self.fs))
                .or(Err(FileSystemError::ReadFailed))?;

            for entry in 0..Block::LEN / FatDirEntry::LEN {
                let entry_start = entry * FatDirEntry::LEN;
                let entry_end = (entry + 1) * FatDirEntry::LEN;
                let dir_entry = FatDirEntry::from_raw(&blocks[0][entry_start..entry_end]);

                if dir_entry.is_end() {
                    break;
                }

                info!("Dir Entry {:?}", dir_entry);
            }
        }
        Ok(())
    }
}
