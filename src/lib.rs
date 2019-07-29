//! The FAT library
//! This crate currently supports FAT12/FAT16/FAT32 with a sector size >= 512 bytes.
#![no_std]

pub mod attribute;
mod cluster;
mod datetime;
pub mod directory;
pub mod filesystem;
mod name;
mod offset_iter;
mod table;
mod utils;

use byteorder::{ByteOrder, LittleEndian};
use storage_device::Block;
use storage_device::StorageDevice;

use cluster::Cluster;

use filesystem::FatFileSystem;

/// The minimal block size supported.
pub const MINIMAL_BLOCK_SIZE: usize = 512;

pub use utils::FileSystemIterator;

#[allow(unused_imports)]
#[macro_use]
extern crate log;

/// Represent a FAT filesystem error.
#[derive(Debug)]
pub enum FatError {
    /// The given resource couldn't be found.
    NotFound,

    /// There isn't enough space for a resource to be stored.
    NoSpaceLeft,

    /// The access to a given resource has been denied.
    AccessDenied,

    /// A writing operation failed on the attached storage device.
    WriteFailed,

    /// A read operation failed on the attached storage device.
    ReadFailed,

    /// The given partition cannot be found.
    PartitionNotFound,

    /// The given resource cannot be represented as a file.
    NotAFile,

    /// The given resource cannot be represented as a directory.
    NotADirectory,

    /// A resource at the given path already exist.
    FileExists,

    /// The given path is too long to be resolved.
    PathTooLong,

    /// The partition wasn't used as it's invalid.
    InvalidPartition,

    /// Represent a custom error.
    Custom {
        /// The name of the custom error.
        name: &'static str,
    },
}

/// Represent a FAT filesystem result.
pub type FatFileSystemResult<T> = core::result::Result<T, FatError>;

/// Represent FAT filesystem types.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FatFsType {
    /// FAT12 volume.
    Fat12,

    /// FAT16 volume.
    Fat16,

    /// FAT32 volume.
    Fat32,

    /// EXFAT volume.
    ExFat,
}

/// Represent the FAT Volume BootRecord.
struct FatVolumeBootRecord {
    /// The actual data of the boot record.
    data: [u8; MINIMAL_BLOCK_SIZE],

    /// The type of FAT filesystem.
    fat_type: FatFsType,

    /// The count of cluster availaible in the filesystem.
    cluster_count: u32,
}

#[allow(dead_code)]
impl FatVolumeBootRecord {
    /// Offset of the boot signature.
    const BOOTABLE_SIGNATURE: usize = 510;

    /// Offset of the FAT system identifier.
    const SYSTEM_IDENTIFIER_FAT: usize = 54;

    /// Offset of the FAT32 system identifier.
    const SYSTEM_IDENTIFIER_FAT32: usize = 82;

    /// Create a new FAT volume boot record from raw data.
    pub fn new(data: [u8; MINIMAL_BLOCK_SIZE]) -> Option<Self> {
        let mut res = Self::new_unchecked(data);

        if !res.is_valid() {
            return None;
        }

        res.initialize_cache();

        Some(res)
    }

    /// Initialize extra infos computed from the data backend.
    pub fn initialize_cache(&mut self) {
        let root_dir_blocks = ((u32::from(self.root_dir_childs_count()) * 32)
            + (u32::from(self.bytes_per_block()) - 1))
            / u32::from(self.bytes_per_block());
        let data_blocks = self.total_blocks()
            - (u32::from(self.reserved_block_count())
                + (u32::from(self.fats_count()) * self.fat_size())
                + root_dir_blocks);
        let cluster_count = data_blocks / u32::from(self.blocks_per_cluster());
        if cluster_count < 0xFF5 {
            self.fat_type = FatFsType::Fat12;
        } else if cluster_count < 0xFFF5 {
            self.fat_type = FatFsType::Fat16;
        } else {
            self.fat_type = FatFsType::Fat32;
        }
        self.cluster_count = cluster_count + 2;
    }

    /// Create a new FAT volume boot from raw data without checking for validity.
    pub(crate) fn new_unchecked(data: [u8; MINIMAL_BLOCK_SIZE]) -> Self {
        FatVolumeBootRecord {
            data,
            fat_type: FatFsType::Fat12,
            cluster_count: 0,
        }
    }

    /// Checks the validity of the boot record.
    pub fn is_valid(&self) -> bool {
        // check boot signature
        if LittleEndian::read_u16(
            &self.data[Self::BOOTABLE_SIGNATURE..Self::BOOTABLE_SIGNATURE + 2],
        ) != 0xAA55
        {
            return false;
        }

        // check jump code
        if self.data[0] != 0xE9 && self.data[0] != 0xEB && self.data[0] != 0xE8 {
            return false;
        }

        // check system identifier
        if self.data[Self::SYSTEM_IDENTIFIER_FAT..Self::SYSTEM_IDENTIFIER_FAT + 3] != *b"FAT"
            && self.data[Self::SYSTEM_IDENTIFIER_FAT32..Self::SYSTEM_IDENTIFIER_FAT32 + 5]
                != *b"FAT32"
        {
            return false;
        }

        if self.bytes_per_block() < MINIMAL_BLOCK_SIZE as u16 {
            return false;
        }

        true
    }

    /// Mark the boot record as valid
    pub(crate) fn set_valid(&mut self, fat_type: FatFsType) {
        // First boot signature
        LittleEndian::write_u16(
            &mut self.data[Self::BOOTABLE_SIGNATURE..Self::BOOTABLE_SIGNATURE + 2],
            0xAA55,
        );

        if let FatFsType::Fat32 = fat_type {
            (&mut self.data[Self::SYSTEM_IDENTIFIER_FAT32..Self::SYSTEM_IDENTIFIER_FAT32 + 5])
                .copy_from_slice(b"FAT32");
        } else {
            (&mut self.data[Self::SYSTEM_IDENTIFIER_FAT..Self::SYSTEM_IDENTIFIER_FAT + 3])
                .copy_from_slice(b"FAT");
        }

        // Set jump for signature
        (&mut self.data[0..3]).copy_from_slice(&[0xEB, 0xFE, 0x90]);
    }

    /// Set the volume label for a FAT12/FAT16 filesystem.
    pub(crate) fn set_volume_label16(&mut self, label: [u8; 11]) {
        (&mut self.data[43..54]).copy_from_slice(&label)
    }

    /// Set the volume label for a FAT32 filesystem.
    pub(crate) fn set_volume_label32(&mut self, label: [u8; 11]) {
        (&mut self.data[71..82]).copy_from_slice(&label)
    }

    /// The amount of bytes per block.
    pub fn bytes_per_block(&self) -> u16 {
        LittleEndian::read_u16(&self.data[11..13])
    }

    /// Set the amount of bytes per block.
    pub(crate) fn set_bytes_per_block(&mut self, bytes_per_block: u16) {
        LittleEndian::write_u16(&mut self.data[11..13], bytes_per_block);
    }

    /// The amount of blocks per cluster.
    pub fn blocks_per_cluster(&self) -> u8 {
        self.data[13]
    }

    /// Set the amount of blocks per cluster.
    pub fn set_blocks_per_cluster(&mut self, blocks_per_cluster: u8) {
        self.data[13] = blocks_per_cluster;
    }

    /// The count of reserved block.
    pub fn reserved_block_count(&self) -> u16 {
        LittleEndian::read_u16(&self.data[14..16])
    }

    /// Set the count of reserved block.
    pub(crate) fn set_reserved_block_count(&mut self, reserved_block_count: u16) {
        LittleEndian::write_u16(&mut self.data[14..16], reserved_block_count);
    }

    /// The number of FAT present in the filesystem.
    pub fn fats_count(&self) -> u8 {
        self.data[16]
    }

    /// Set the number of FAT present in the filesystem.
    pub fn set_fats_count(&mut self, fats_count: u8) {
        self.data[16] = fats_count;
    }

    /// The number of childs in the root directory for FAT12/FAT16 filesystem.
    pub fn root_dir_childs_count(&self) -> u16 {
        LittleEndian::read_u16(&self.data[17..19])
    }

    /// Set the number of childs in the root directory for FAT12/FAT16 filesystem.
    pub fn set_root_dir_childs_count(&mut self, root_dir_childs_count: u16) {
        LittleEndian::write_u16(&mut self.data[17..19], root_dir_childs_count);
    }

    /// The total of blocks of the filesystem. If zero, uses ``total_blocks32``.
    pub fn total_blocks16(&self) -> u16 {
        LittleEndian::read_u16(&self.data[19..21])
    }

    /// Set the total of blocks of the filesystem.
    pub(crate) fn set_total_blocks16(&mut self, total_blocks: u16) {
        LittleEndian::write_u16(&mut self.data[19..21], total_blocks);
    }

    /// Return the media type of the FAT filesystem.
    pub fn media_type(&self) -> u8 {
        self.data[21]
    }

    /// Set the media type of the FAT filesystem.
    pub(crate) fn set_media_type(&mut self, media_type: u8) {
        self.data[21] = media_type;
    }

    /// Return the size in blocks of the FAT for FAT12/FAT16 filesystems.
    pub fn fat_size16(&self) -> u16 {
        LittleEndian::read_u16(&self.data[22..24])
    }

    /// Set the size in blocks of the FAT for FAT12/FAT16 filesystems.
    pub fn set_fat_size16(&mut self, fat_size: u16) {
        LittleEndian::write_u16(&mut self.data[22..24], fat_size);
    }

    /// Physical blocks per track (INT 13h CHS geometry). Zero if unusued.
    pub fn blocks_per_track(&self) -> u16 {
        LittleEndian::read_u16(&self.data[24..26])
    }

    /// Set the number of physical blocks per track (INT 13h CHS geometry). Zero if unusued.
    pub(crate) fn set_blocks_per_track(&mut self, blocks_per_track: u16) {
        LittleEndian::write_u16(&mut self.data[24..26], blocks_per_track);
    }

    /// Number of heads (INT 13h CHS geometry). Zero if unused.
    pub fn num_heads(&self) -> u16 {
        LittleEndian::read_u16(&self.data[26..28])
    }

    /// Set the number of heads (INT 13h CHS geometry).
    pub(crate) fn set_num_heads(&mut self, num_heads: u16) {
        LittleEndian::write_u16(&mut self.data[26..28], num_heads);
    }

    /// Set the drive number on a FAT12/FAT16 fileysem.
    pub(crate) fn set_drive_number16(&mut self, drive_number: u8) {
        self.data[36] = drive_number;
    }

    /// Set the drive number on a FAT32 fileysem.
    pub(crate) fn set_drive_number32(&mut self, drive_number: u8) {
        self.data[64] = drive_number;
    }

    /// The number of hidden blocks on the FAT filesystem.
    pub fn hidden_blocks(&self) -> u32 {
        LittleEndian::read_u32(&self.data[28..32])
    }

    /// The total block count on a FAT32 filesystem.
    pub fn total_blocks32(&self) -> u32 {
        LittleEndian::read_u32(&self.data[32..36])
    }

    /// Set the total of blocks on a FAT filesystem if the sector count is greater than a u16.
    pub(crate) fn set_total_blocks32(&mut self, total_blocks: u32) {
        LittleEndian::write_u32(&mut self.data[32..36], total_blocks);
    }

    /// Return the size in blocks of the FAT for FAT32 filesystems.
    pub fn fat_size32(&self) -> u32 {
        LittleEndian::read_u32(&self.data[36..40])
    }

    /// Set the size in blocks of the FAT for FAT32 filesystems.
    pub fn set_fat_size32(&mut self, fat_size: u32) {
        LittleEndian::write_u32(&mut self.data[36..40], fat_size);
    }

    /// The block index of the FAT32's filesystem informations.
    pub fn fs_info_block(&self) -> u16 {
        LittleEndian::read_u16(&self.data[48..50])
    }

    /// Set the block index of the FAT32's filesystem informations.
    pub(crate) fn set_fs_info_block(&mut self, fs_info_block: u16) {
        LittleEndian::write_u16(&mut self.data[48..50], fs_info_block);
    }

    /// The block index of the FAT32's Boot Record Backup.
    pub fn backup_boot_record_block(&self) -> u16 {
        LittleEndian::read_u16(&self.data[50..52])
    }

    /// Set the block index of the FAT32's Boot Record Backup.
    pub(crate) fn set_backup_boot_record_block(&mut self, backup_boot_record_block: u16) {
        LittleEndian::write_u16(&mut self.data[50..52], backup_boot_record_block);
    }

    /// The root directory cluster for FAT32 filesystems.
    pub fn root_dir_childs_cluster(&self) -> Cluster {
        Cluster(LittleEndian::read_u32(&self.data[44..48]))
    }

    /// Set the root directory cluster for FAT32 filesystem.
    pub(crate) fn set_root_dir_childs_cluster(&mut self, cluster: Cluster) {
        LittleEndian::write_u32(&mut self.data[44..48], cluster.0);
    }

    /// Return the size in blocks of the FAT.
    pub fn fat_size(&self) -> u32 {
        let result = u32::from(self.fat_size16());
        if result != 0 {
            result
        } else {
            self.fat_size32()
        }
    }

    /// The total block count on a FAT filesystem.
    pub fn total_blocks(&self) -> u32 {
        let result = u32::from(self.total_blocks16());
        if result != 0 {
            result
        } else {
            self.total_blocks32()
        }
    }

    /// Flush the boot record structure inside the filesystem.
    pub(crate) fn flush<S: StorageDevice>(&self, fs: &FatFileSystem<S>) -> FatFileSystemResult<()> {
        // Write the boot record
        fs.storage_device
            .lock()
            .write(fs.partition_start, &self.data)
            .or(Err(FatError::WriteFailed))?;

        // On FAT32, we need to write the backup boot record.
        if let FatFsType::Fat32 = self.fat_type {
            fs.storage_device
                .lock()
                .write(
                    fs.partition_start
                        + u64::from(fs.boot_record.backup_boot_record_block())
                            * u64::from(fs.boot_record.bytes_per_block()),
                    &self.data,
                )
                .or(Err(FatError::WriteFailed))?;
        }

        Ok(())
    }
}

/// Get a FAT boot record from a StorageDevice.
fn get_fat_boot_record(
    storage_device: &mut dyn StorageDevice,
    partition_start: u64,
) -> FatFileSystemResult<FatVolumeBootRecord> {
    let mut block = [0x0u8; MINIMAL_BLOCK_SIZE];

    storage_device
        .read(partition_start, &mut block)
        .or(Err(FatError::ReadFailed))?;

    let boot_record = FatVolumeBootRecord::new(block);

    if boot_record.is_none() {
        return Err(FatError::InvalidPartition);
    }

    Ok(boot_record.unwrap())
}

/// Parse a FAT boot record and return a FatFileSystem instance.
fn parse_fat_boot_record<S: StorageDevice>(
    storage_device: S,
    partition_start: u64,
    partition_size: u64,
    uninitialized: bool,
) -> FatFileSystemResult<FatFileSystem<S>> {
    let mut storage_device = storage_device;
    let boot_record = get_fat_boot_record(&mut storage_device, partition_start)?;

    match boot_record.fat_type {
        FatFsType::ExFat => unimplemented!(),
        FatFsType::Fat12 | FatFsType::Fat16 | FatFsType::Fat32 => {
            // Zero on FAT32
            let root_dir_blocks = ((u32::from(boot_record.root_dir_childs_count()) * 32)
                + (u32::from(boot_record.bytes_per_block()) - 1))
                / u32::from(boot_record.bytes_per_block());

            let first_data_offset = u32::from(boot_record.reserved_block_count())
                + (u32::from(boot_record.fats_count()) * boot_record.fat_size())
                + root_dir_blocks;

            let mut file_system = FatFileSystem::new(
                storage_device,
                partition_start,
                u64::from(first_data_offset) * u64::from(boot_record.bytes_per_block()),
                partition_size,
                boot_record,
            )?;

            if !uninitialized {
                file_system.init()?;
            }

            Ok(file_system)
        }
    }
}

/// Treat the storage device directly as a filesystem.
pub fn get_raw_partition<S: StorageDevice>(
    storage_device: S,
) -> FatFileSystemResult<FatFileSystem<S>> {
    let mut storage_device = storage_device;
    let storage_len = storage_device.len().unwrap();

    parse_fat_boot_record(storage_device, 0, storage_len, false)
}

/// Format the given storage to hold a given FAT filesystem type.
pub fn format_raw_partition<S: StorageDevice>(
    mut storage_device: S,
    fat_type: FatFsType,
) -> FatFileSystemResult<()> {
    let storage_size = storage_device.len().or(Err(FatError::ReadFailed))?;

    format_partition(storage_device, fat_type, 0, storage_size)
}

/// Format the partition to hold a given FAT filesystem type.
pub fn format_partition<S: StorageDevice>(
    storage_device: S,
    fat_type: FatFsType,
    partition_start: u64,
    partition_size: u64
) -> FatFileSystemResult<()> {
    let mut storage_device = storage_device;

    // Create an empty boot record
    let mut boot_record = FatVolumeBootRecord::new_unchecked([0x0u8; MINIMAL_BLOCK_SIZE]);

    let mut blocks_per_track = 63;
    let mut heads = 255;
    let mut cluster_size = 4;

    let block_count = partition_size / Block::LEN_U64;

    if partition_size < 512 * 1024 * 1024 {
        blocks_per_track = 32;
        heads = 64;
    }

    let number_clusters = block_count / u64::from(cluster_size);

    if let FatFsType::Fat32 = fat_type {
        let size_mb = partition_size / (1024 * 1024);
        if size_mb > 32 * 1024 {
            cluster_size = 64;
        } else if size_mb > 16 * 1024 {
            cluster_size = 32;
        } else if size_mb > 8 * 1024 {
            cluster_size = 16;
        } else if size_mb > 1 * 1024 {
            cluster_size = 8;
        } else {
            cluster_size = 1;
        }
    } else if let FatFsType::Fat16 = fat_type {
        // If we have too many clusters, try to make it in range of the FAT16
        if number_clusters > 0xFFF5 {
            cluster_size = 64;
        }
    } else {
        // If we have too many clusters, try to make it in range of the FAT12
        if number_clusters > 0xFF5 {
            cluster_size = 64;
        }
    }

    let number_clusters = block_count / u64::from(cluster_size);
    if let FatFsType::Fat16 = fat_type {
        // If we have too many clusters even now, this is too big to hold this filesystem type.
        if number_clusters > 0xFFF5 {
            return Err(FatError::InvalidPartition);
        }
    } else if let FatFsType::Fat12 = fat_type {
        // If we have too many clusters even now, this is too big to hold this filesystem type.
        if number_clusters > 0xFF5 {
            return Err(FatError::InvalidPartition);
        }
    } else if let FatFsType::Fat32 = fat_type {
        // If we have too many clusters even now, this is too big to hold this filesystem type.
        if number_clusters > 0xFFFFFF5 {
            return Err(FatError::InvalidPartition);
        }
    }

    boot_record.set_media_type(0xf8);
    boot_record.set_num_heads(heads);
    boot_record.set_blocks_per_track(blocks_per_track);
    boot_record.set_blocks_per_cluster(cluster_size);
    boot_record.set_fats_count(2);
    boot_record.set_bytes_per_block(512);

    if let FatFsType::Fat32 = fat_type {
        boot_record.set_root_dir_childs_count(0);
        boot_record.set_drive_number32(0x80);
        boot_record.set_volume_label32(*b"NO NAME    ");
        boot_record.set_reserved_block_count(32);
        boot_record.set_total_blocks32(block_count as u32);

        let fat_size = (number_clusters * 4 + 8 + Block::LEN_U64 - 1) / Block::LEN_U64;
        boot_record.set_fat_size32(fat_size as u32);

        // FAT32 specific features
        boot_record.set_fs_info_block(1);
        boot_record.set_backup_boot_record_block(6);

        // Make sure to clean the fs info block as it may contains valid data
        storage_device
            .write(
                partition_start + u64::from(boot_record.fs_info_block()) * u64::from(boot_record.bytes_per_block()),
                &[0x0; 512],
            )
            .or(Err(FatError::WriteFailed))?;
    } else {
        boot_record.set_root_dir_childs_count(512);
        boot_record.set_drive_number16(0x80);
        boot_record.set_volume_label16(*b"NO NAME    ");
        boot_record.set_reserved_block_count(1);
        boot_record.set_total_blocks32(block_count as u32);

        let fat_byte_size = if number_clusters > 0xFF5 {
            number_clusters * 2 + 4
        } else {
            (number_clusters * 3 + 1) / 2 + 3
        };

        let fat_size = (fat_byte_size + Block::LEN_U64 - 1) / Block::LEN_U64;
        boot_record.set_fat_size16(fat_size as u16);
    }

    boot_record.set_valid(fat_type);

    assert!(boot_record.is_valid());

    // Init the boot_record as it should be valid now
    boot_record.initialize_cache();
    assert!(boot_record.fat_type == fat_type);

    // Write the boot record for FatFilesystem creation
    storage_device
        .write(partition_start, &boot_record.data[..])
        .or(Err(FatError::WriteFailed))?;

    // Now we open the filesystem and clean the FATs while defering initalization.
    let mut filesystem = parse_fat_boot_record(storage_device, partition_start, partition_size, true)?;
    table::FatValue::initialize(&filesystem)?;

    // Now that the FATs are clean, we can init the filesystem (and the volume information on FAT32)
    filesystem.init()?;

    filesystem.create_root_directory()?;

    // Rewrite the boot record as it might be updated
    filesystem.boot_record.flush(&filesystem)?;

    Ok(())
}

/// Treat the storage device directly as a partition and try to determine the FAT type of the partition
pub fn get_fat_type(storage_device: &mut dyn StorageDevice) -> FatFileSystemResult<FatFsType> {
    Ok(get_fat_boot_record(storage_device, 0)?.fat_type)
}

/// Parse the MBR and return an instance to a filesystem at the given partition index.
pub fn get_partition<S: StorageDevice>(
    storage_device: S,
    index: u64,
    block_size: u64,
) -> FatFileSystemResult<FatFileSystem<S>> {
    let mut block = [0x0u8; MINIMAL_BLOCK_SIZE];

    if block_size < block.len() as u64 {
        return Err(FatError::InvalidPartition);
    }

    /// The Partition Table offset.
    const PARITION_TABLE_OFFSET: usize = 446;

    /// The MBR signature offset.
    const MBR_SIGNATURE: usize = 510;

    /// The size of a partition table entry.
    const PARITION_TABLE_ENTRY_SIZE: usize = 16;

    let mut storage_device = storage_device;

    storage_device
        .read(index, &mut block)
        .or(Err(FatError::ReadFailed))?;

    if LittleEndian::read_u16(&block[MBR_SIGNATURE..MBR_SIGNATURE + 2]) != 0xAA55 {
        return Err(FatError::InvalidPartition);
    }

    let partition = if index < 4 {
        let offset = PARITION_TABLE_OFFSET + (PARITION_TABLE_ENTRY_SIZE * (index as usize));
        &block[offset..offset + PARITION_TABLE_ENTRY_SIZE]
    } else {
        return Err(FatError::PartitionNotFound);
    };

    if (partition[0] & 0x7F) != 0 {
        return Err(FatError::InvalidPartition);
    }

    let partition_start = LittleEndian::read_u32(&partition[0x8..0xC]);
    let partition_block_count = LittleEndian::read_u32(&partition[0xC..0x10]);
    let partition_type: u32 = partition[0x4].into();

    match partition_type {
        0xC => parse_fat_boot_record(
            storage_device,
            u64::from(partition_start) * block_size as u64,
            u64::from(partition_block_count) * block_size as u64,
            false,
        ),
        _ => Err(FatError::Custom {
            name: "Unknown Partition Type",
        }),
    }
}
