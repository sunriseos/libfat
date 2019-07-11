//! FAT Filesystem.

use arrayvec::ArrayString;
use byteorder::{ByteOrder, LittleEndian};

use super::attribute::Attributes;
use super::directory::{dir_entry::DirectoryEntry, Directory, File};
use super::offset_iter::ClusterOffsetIter;
use super::FatVolumeBootRecord;

use super::cluster::Cluster;
use super::table;
use super::table::FatValue;
use super::utils;
use super::FatError;
use super::FatFileSystemResult;
use super::FatFsType;
use spin::Mutex;
use storage_device::StorageDevice;

use core::sync::atomic::AtomicU32;
use core::sync::atomic::Ordering;

use crate::utils::FileSystemIterator;

/// Reprsent the FS Info structure of FAT32.
struct FatFileSystemInfo {
    // TODO: select Ordering wisely on operations.
    /// The last allocated cluster on the filesystem.
    last_cluster: AtomicU32,

    /// The free cluster count on the filesystem.
    free_cluster: AtomicU32,
}

impl FatFileSystemInfo {
    /// Import FS Info from a FAT32 filesystem.
    fn from_fs<S: StorageDevice>(fs: &FatFileSystem<S>) -> FatFileSystemResult<Self> {
        let mut block = [0x0u8; crate::MINIMAL_BLOCK_SIZE];

        let mut last_cluster = 0xFFFF_FFFF;
        let mut free_cluster = 0xFFFF_FFFF;

        fs.storage_device
            .lock()
            .read(
                fs.partition_start
                    + u64::from(fs.boot_record.fs_info_block())
                        * u64::from(fs.boot_record.bytes_per_block()),
                &mut block,
            )
            .or(Err(FatError::ReadFailed))?;

        // valid signature?
        if &block[0..4] == b"RRaA"
            && &block[0x1e4..0x1e8] == b"rrAa"
            && LittleEndian::read_u16(&block[0x1fe..0x200]) == 0xAA55
        {
            // check cluster sanity
            let fs_last_cluster = LittleEndian::read_u32(&block[0x1ec..0x1f0]);
            if fs_last_cluster >= 2 && fs_last_cluster < fs.boot_record.cluster_count {
                last_cluster = fs_last_cluster;
            }

            // check sanity
            let fs_free_cluster = LittleEndian::read_u32(&block[0x1e8..0x1ec]);
            if fs_free_cluster <= fs.boot_record.cluster_count {
                free_cluster = fs_free_cluster;
            }
        }

        Ok(FatFileSystemInfo {
            last_cluster: AtomicU32::new(last_cluster),
            free_cluster: AtomicU32::new(free_cluster),
        })
    }

    /// Flush the FS Info to the disk on FAT32 filesystems.
    fn flush<S: StorageDevice>(&self, fs: &FatFileSystem<S>) -> FatFileSystemResult<()> {
        if fs.boot_record.fat_type != FatFsType::Fat32 {
            return Ok(());
        }

        // We write a entire block because we want to ensure the data are correctly initialized.
        let mut block = [0x0u8; crate::MINIMAL_BLOCK_SIZE];

        LittleEndian::write_u32(&mut block[0..4], 0x4161_5252);
        LittleEndian::write_u32(&mut block[0x1e4..0x1e8], 0x6141_7272);
        LittleEndian::write_u16(&mut block[0x1fe..0x200], 0xAA55);

        LittleEndian::write_u32(
            &mut block[0x1ec..0x1f0],
            self.last_cluster.load(Ordering::SeqCst),
        );
        LittleEndian::write_u32(
            &mut block[0x1e8..0x1ec],
            self.free_cluster.load(Ordering::SeqCst),
        );

        fs.storage_device
            .lock()
            .write(
                fs.partition_start
                    + u64::from(fs.boot_record.fs_info_block())
                        * u64::from(fs.boot_record.bytes_per_block()),
                &block,
            )
            .or(Err(FatError::WriteFailed))?;

        Ok(())
    }
}

/// Represent a FAT filesystem.
#[allow(dead_code)]
pub struct FatFileSystem<S: StorageDevice> {
    /// The device device of the filesystem.
    pub(crate) storage_device: Mutex<S>,

    /// The block index of the start of the partition of this filesystem.
    pub(crate) partition_start: u64,

    /// Block index of the first block availaible for data.
    pub(crate) first_data_offset: u64,

    // TODO: check we don't go out of the partition
    /// The size of the partition.
    pub(crate) partition_size: u64,

    /// The volume information of the filesystem.
    pub(crate) boot_record: FatVolumeBootRecord,

    /// The extra infos of the filesystem.
    fat_info: FatFileSystemInfo,
}

impl<S: StorageDevice> FatFileSystem<S> {
    /// Create a new instance of FatFileSystem.
    pub(crate) fn new(
        storage_device: S,
        partition_start: u64,
        first_data_offset: u64,
        partition_size: u64,
        boot_record: FatVolumeBootRecord,
    ) -> FatFileSystemResult<FatFileSystem<S>> {
        let fs = FatFileSystem {
            storage_device: Mutex::new(storage_device),
            partition_start,
            first_data_offset,
            partition_size,
            boot_record,
            fat_info: FatFileSystemInfo {
                last_cluster: AtomicU32::new(0xFFFF_FFFF),
                free_cluster: AtomicU32::new(0xFFFF_FFFF),
            },
        };
        Ok(fs)
    }

    /// Initialize the filesystem.
    pub(crate) fn init(&mut self) -> FatFileSystemResult<()> {
        // read FAT infos
        if self.boot_record.fat_type == FatFsType::Fat32 {
            self.fat_info = FatFileSystemInfo::from_fs(self)?;
        }

        if self.fat_info.free_cluster.load(Ordering::SeqCst) == 0xFFFF_FFFF {
            self.fat_info
                .free_cluster
                .store(table::get_free_cluster_count(self)?, Ordering::SeqCst);
        }

        Ok(())
    }

    /// Get the root directory of the filesystem.
    pub(crate) fn get_root_directory(&self) -> Directory<'_, S> {
        let dir_info = DirectoryEntry {
            start_cluster: self.boot_record.root_dir_childs_cluster(),
            raw_info: None,
            file_size: 0,
            creation_timestamp: 0,
            last_access_timestamp: 0,
            last_modification_timestamp: 0,
            file_name: ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN_UNICODE]>::new(),
            attribute: Attributes::new(Attributes::DIRECTORY),
        };

        Directory::from_entry(self, dir_info)
    }

    /// Create the root directory of the filesystem during formatting.
    pub(crate) fn create_root_directory(&mut self) -> FatFileSystemResult<()> {
        let is_old_root_directory = match self.boot_record.fat_type {
            FatFsType::Fat12 | FatFsType::Fat16 => true,
            _ => false,
        };

        if !is_old_root_directory {
            // Allocate a cluster for the root directory
            let cluster = self.alloc_cluster(None)?;
            self.clean_cluster_data(cluster)?;
            self.boot_record.set_root_dir_childs_cluster(cluster);
        } else {
            // Root directory is at the start and isn't part of the FAT so we need to clean it manually
            let mut fat_dir_entry_iter = self.get_root_directory().fat_dir_entry_iter();
            while let Some(raw_dir_entry) = fat_dir_entry_iter.next(self) {
                let mut raw_dir_entry = raw_dir_entry?;
                raw_dir_entry.clear();
                raw_dir_entry.flush(self)?;
            }
        }

        Ok(())
    }

    /// Open the parent directory of a given path.
    fn open_parent_directory(&self, path: &str) -> FatFileSystemResult<Directory<'_, S>> {
        let (parent_name, _) = utils::get_parent(path);
        if parent_name == "" {
            Ok(self.get_root_directory())
        } else {
            self.get_root_directory().open_directory(parent_name)
        }
    }

    /// Search for a directory entry inside the filesystem at the given path.
    pub fn search_entry(&self, path: &str) -> FatFileSystemResult<DirectoryEntry> {
        let (_, file_name) = utils::get_parent(path);
        self.open_parent_directory(path)?.search_entry(file_name)
    }

    /// Open a directory at the given path.
    pub fn open_directory(&self, path: &str) -> FatFileSystemResult<Directory<'_, S>> {
        if path == "/" {
            Ok(self.get_root_directory())
        } else {
            self.get_root_directory().open_directory(path)
        }
    }

    /// Open a file at the given path.
    pub fn open_file(&self, path: &str) -> FatFileSystemResult<File> {
        self.get_root_directory().open_file(path)
    }

    /// Create a new directory at the given path.
    pub fn create_directory(&self, path: &str) -> FatFileSystemResult<()> {
        let (_, file_name) = utils::get_parent(path);
        let mut parent_dir = self.open_parent_directory(path)?;

        // precheck that it doesn't exist already
        if parent_dir.find_entry(file_name).is_ok() {
            return Err(FatError::FileExists);
        }

        parent_dir.create_directory(file_name)
    }

    /// Create a new file at the given path.
    pub fn create_file(&self, path: &str) -> FatFileSystemResult<()> {
        let (_, file_name) = utils::get_parent(path);
        let mut parent_dir = self.open_parent_directory(path)?;

        // precheck that it doesn't exist already
        if parent_dir.find_entry(file_name).is_ok() {
            return Err(FatError::FileExists);
        }

        parent_dir.create_file(file_name)
    }

    /// Delete a file at the given path.
    pub fn delete_file(&self, path: &str) -> FatFileSystemResult<()> {
        let (_, file_name) = utils::get_parent(path);
        self.open_parent_directory(path)?.delete_file(file_name)
    }

    /// Delete a directory at the given path.
    pub fn delete_directory(&self, path: &str) -> FatFileSystemResult<()> {
        let (_, file_name) = utils::get_parent(path);
        self.open_parent_directory(path)?
            .delete_directory(file_name)
    }

    /// Rename a file at the given path to a new path.
    pub fn rename_file(&self, old_path: &str, new_path: &str) -> FatFileSystemResult<()> {
        self.rename(old_path, new_path, false)
    }

    /// Rename a directory at the given path to a new path.
    pub fn rename_directory(&self, old_path: &str, new_path: &str) -> FatFileSystemResult<()> {
        self.rename(old_path, new_path, true)
    }

    /// Get the FAT filesystem type.
    pub fn get_type(&self) -> FatFsType {
        self.boot_record.fat_type
    }

    /// Rename a directory or a file at the given path to a new path.
    fn rename(&self, old_path: &str, new_path: &str, is_dir: bool) -> FatFileSystemResult<()> {
        let (_, file_name) = utils::get_parent(old_path);
        let parent_old_dir = self.open_parent_directory(old_path)?;

        let old_entry = parent_old_dir.find_entry(file_name)?;

        if old_entry.attribute.is_directory() != is_dir {
            if is_dir {
                return Err(FatError::NotADirectory);
            } else {
                return Err(FatError::NotAFile);
            }
        }

        let (_, file_name) = utils::get_parent(new_path);
        let parent_new_dir = self.open_parent_directory(new_path)?;

        if parent_new_dir.find_entry(file_name).is_ok() {
            return Err(FatError::FileExists);
        }

        parent_new_dir.rename(old_entry, file_name, is_dir)
    }

    /// Clean cluster chain data.
    /// Used when creating a new directory.
    pub(crate) fn clean_cluster_data(&self, cluster: Cluster) -> FatFileSystemResult<()> {
        let block = [0x0u8; crate::MINIMAL_BLOCK_SIZE];
        let mut block_index = 0;

        let mut cluster_iterator = ClusterOffsetIter::new(self, cluster, None);

        while let Some(cluster) = cluster_iterator.next(self) {
            block_index = (block_index + 1) % u32::from(self.boot_record.blocks_per_cluster());
            let write_per_block =
                self.boot_record.bytes_per_block() as usize / crate::MINIMAL_BLOCK_SIZE;

            for index in 0..write_per_block {
                self.storage_device
                    .lock()
                    .write(
                        self.partition_start
                            + cluster.to_data_bytes_offset(self)
                            + u64::from(block_index)
                                * u64::from(self.boot_record.bytes_per_block())
                            + (index * crate::MINIMAL_BLOCK_SIZE) as u64,
                        &block,
                    )
                    .or(Err(FatError::WriteFailed))?;
            }
        }

        Ok(())
    }

    /// Allocate a cluster and if specified add it to a cluster chain.
    pub(crate) fn alloc_cluster(
        &self,
        last_cluster_allocated_opt: Option<Cluster>,
    ) -> FatFileSystemResult<Cluster> {
        let mut start_cluster = Cluster(self.fat_info.last_cluster.load(Ordering::SeqCst));
        let mut resize_existing_cluster = false;

        if last_cluster_allocated_opt.is_none() {
            start_cluster = Cluster(self.fat_info.last_cluster.load(Ordering::SeqCst));
            if start_cluster.0 == 0 || start_cluster.0 >= self.boot_record.cluster_count {
                start_cluster = Cluster(1);
            }
        } else if let Some(last_cluster_allocated) = last_cluster_allocated_opt {
            start_cluster = last_cluster_allocated;

            let cluster_val = FatValue::get(self, start_cluster)?;
            if let FatValue::Data(valid_cluster) = cluster_val {
                if valid_cluster < self.boot_record.cluster_count {
                    return Ok(Cluster(valid_cluster));
                }
            }

            resize_existing_cluster = true;
        }

        if self.fat_info.free_cluster.load(Ordering::SeqCst) == 0 {
            return Err(FatError::NoSpaceLeft);
        }

        let mut number_cluster = 0;

        // Resize of exisiting cluster?
        if resize_existing_cluster {
            // test next chunk
            number_cluster = start_cluster.0 + 1;
            if number_cluster >= self.boot_record.cluster_count {
                number_cluster = 2;
            }

            let value = FatValue::get(self, Cluster(number_cluster))?;

            if value != FatValue::Free {
                let new_start = Cluster(self.fat_info.last_cluster.load(Ordering::SeqCst));
                if new_start.0 >= 2 && new_start.0 < self.boot_record.cluster_count {
                    start_cluster = new_start;
                }

                number_cluster = 0;
            }
        }

        if number_cluster == 0 {
            number_cluster = start_cluster.0;
            loop {
                number_cluster += 1;
                if number_cluster >= self.boot_record.cluster_count {
                    number_cluster = 2;
                    if number_cluster > start_cluster.0 {
                        return Err(FatError::NoSpaceLeft);
                    }
                }

                let value = FatValue::get(self, Cluster(number_cluster))?;

                if value == FatValue::Free {
                    break;
                }

                if number_cluster == start_cluster.0 {
                    return Err(FatError::NoSpaceLeft);
                }
            }
        }

        let allocated_cluster = Cluster(number_cluster);
        debug_assert!(FatValue::get(self, allocated_cluster)? == FatValue::Free);
        FatValue::put(self, allocated_cluster, FatValue::EndOfChain)?;

        // Link existing cluster with the new one availaible
        if let Some(last_cluster_allocated) = last_cluster_allocated_opt {
            debug_assert!(
                FatValue::get(self, last_cluster_allocated)? == FatValue::Free
                    || FatValue::get(self, last_cluster_allocated)? == FatValue::EndOfChain
            );
            FatValue::put(
                self,
                last_cluster_allocated,
                FatValue::Data(allocated_cluster.0),
            )?;
        }

        self.fat_info
            .last_cluster
            .store(allocated_cluster.0, Ordering::SeqCst);
        self.fat_info.free_cluster.fetch_sub(1, Ordering::SeqCst);
        self.fat_info.flush(self)?;

        Ok(allocated_cluster)
    }

    /// Free a cluster and if specified remove of a cluster chain.
    pub(crate) fn free_cluster(
        &self,
        to_remove: Cluster,
        previous_cluster: Option<Cluster>,
    ) -> FatFileSystemResult<()> {
        if let Some(previous_cluster) = previous_cluster {
            FatValue::put(self, previous_cluster, FatValue::EndOfChain)?;
        }

        let mut current_cluster = to_remove;

        loop {
            let value = FatValue::get(self, current_cluster)?;

            if value == FatValue::Free {
                break;
            }

            FatValue::put(self, current_cluster, FatValue::Free)?;

            // Invalidate last cluster if equals to the current cluster
            self.fat_info.last_cluster.compare_and_swap(
                0xFFFF_FFFF,
                current_cluster.0,
                Ordering::SeqCst,
            );

            self.fat_info.free_cluster.fetch_add(1, Ordering::SeqCst);

            match value {
                FatValue::Data(data) => {
                    current_cluster = Cluster(data);
                }
                _ => break,
            }
        }
        self.fat_info.flush(self)?;
        Ok(())
    }
}
