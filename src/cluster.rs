//! FAT cluster.

use super::FatFileSystem;
use super::FatFsType;
use storage_device::StorageDevice;

#[derive(Debug, Copy, Clone, PartialEq)]
/// Represent a FAT Cluster position.
pub struct Cluster(pub u32);

impl Cluster {
    /// Compute the offset of the data from the cluster position.
    pub fn to_data_bytes_offset<S: StorageDevice>(self, fs: &FatFileSystem<S>) -> u64 {
        let first_block_of_cluster = (self.0 - 2) * u32::from(fs.boot_record.blocks_per_cluster());
        fs.first_data_offset
            + u64::from(first_block_of_cluster) * u64::from(fs.boot_record.bytes_per_block())
    }

    /// Compute the offset in the cluster map of the cluster chain.
    pub fn to_fat_offset(self, fat_type: FatFsType) -> u32 {
        match fat_type {
            FatFsType::Fat12 => self.0 + (self.0 / 2),
            FatFsType::Fat16 => self.0 * 2,
            FatFsType::Fat32 => self.0 * 4,
            _ => unimplemented!(),
        }
    }

    /// Compute the bytes offset of a cluster in the cluster map.
    pub fn to_fat_bytes_offset<S: StorageDevice>(self, fs: &FatFileSystem<S>) -> u64 {
        let fat_offset = self.to_fat_offset(fs.boot_record.fat_type);

        let fat_block_index = u32::from(fs.boot_record.reserved_block_count())
            + (fat_offset / u32::from(fs.boot_record.bytes_per_block()));
        u64::from(fat_block_index) * u64::from(fs.boot_record.bytes_per_block())
    }
}
