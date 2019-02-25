use arrayvec::ArrayString;

use super::block::{BlockCount, BlockDevice, BlockIndex};
use super::directory::{Attributes, Directory, DirectoryEntry};
use super::FatVolumeBootRecord;

use crate::Result as FileSystemResult;
use crate::FileSystemError;
use super::cluster::Cluster;
use super::table::FatValue;


pub struct FatFileSystemInfo {
    // Last allocated cluster
    last_cluster: u32,
    free_cluster: u32
}

// TODO: reduce field accesibility
pub struct FatFileSystem<T> {
    pub block_device: T,
    pub partition_start: BlockIndex,
    pub first_data_offset: BlockIndex,
    pub partition_block_count: BlockCount,
    pub boot_record: FatVolumeBootRecord,
    pub fat_info: FatFileSystemInfo,
}

impl<T> FatFileSystem<T>
where
    T: BlockDevice,
{
    pub fn new(
        block_device: T,
        partition_start: BlockIndex,
        first_data_offset: BlockIndex,
        partition_block_count: BlockCount,
        boot_record: FatVolumeBootRecord,
    ) -> FatFileSystem<T> {
        FatFileSystem {
            block_device,
            partition_start,
            first_data_offset,
            partition_block_count,
            boot_record,
            // TODO: extract fs info to get some hints
            fat_info: FatFileSystemInfo {
                last_cluster: 0xFFFF_FFFF,
                free_cluster: 0xFFFF_FFFF,
            }
        }
    }

    pub fn init(&self) {
        // TODO: check fs info struct
    }

    pub fn get_root_directory(&self) -> Directory<T> {
        let dir_info = DirectoryEntry {
            start_cluster: self.boot_record.root_dir_childs_cluster(),
            file_size: 0,
            file_name: ArrayString::<[_; DirectoryEntry::MAX_FILE_NAME_LEN]>::new(),
            attribute: Attributes::new(Attributes::DIRECTORY),
        };

        Directory::from_entry(self, dir_info)
    }

    pub fn alloc_cluster(&self, last_cluster_allocated_opt: Option<Cluster>) -> FileSystemResult<Cluster> {
        let mut start_cluster = Cluster(self.fat_info.last_cluster);
        
        let mut last_cluster_allocated = Cluster(0);
        
        if let Some(cluster) = last_cluster_allocated_opt {
            // TODO: precheck if the size is availaible
            start_cluster = last_cluster_allocated;
            last_cluster_allocated = cluster;
        } else if start_cluster.0 == 0 || start_cluster.0 >= self.boot_record.cluster_count {
                start_cluster = Cluster(1);
        }

        if self.fat_info.free_cluster == 0 {
            return Err(FileSystemError::NoSpaceLeft);
        }

        let mut number_cluster = 0;

        // Resize of exisiting cluster?
        if start_cluster == last_cluster_allocated {

            number_cluster = start_cluster.0 + 1;
            if number_cluster >= self.boot_record.cluster_count {
                number_cluster = 2;
            }
            let value = FatValue::get(self, Cluster(number_cluster))?;

            if let FatValue::Data(_) = value {
                let new_start = Cluster(self.fat_info.last_cluster);
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
                        return Err(FileSystemError::NoSpaceLeft);
                    }
                }

                let value = FatValue::get(self, Cluster(number_cluster))?;

                if value == FatValue::Free {
                    break;
                }

                if number_cluster == start_cluster.0 {
                    return Err(FileSystemError::NoSpaceLeft);
                }
            }
        }

        let allocated_cluster = Cluster(number_cluster);
        FatValue::put(self, allocated_cluster, FatValue::EndOfChain)?;
        
        // Link existing cluster with the new one availaible
        if last_cluster_allocated.0 != 0 {
            FatValue::put(self, last_cluster_allocated, FatValue::Data(allocated_cluster.0))?;
        }

        // TODO: update FS info

        Ok(allocated_cluster)
    }
}
