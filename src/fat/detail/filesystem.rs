use arrayvec::ArrayString;

use super::block::{BlockCount, BlockDevice, BlockIndex};
use super::directory::{Attributes, Directory, DirectoryEntry};
use super::FatVolumeBootRecord;

// TODO: reduce field accesibility
pub struct FatFileSystem<T> {
    pub block_device: T,
    pub partition_start: BlockIndex,
    pub first_data_offset: BlockIndex,
    pub partition_block_count: BlockCount,
    pub boot_record: FatVolumeBootRecord,
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
}
