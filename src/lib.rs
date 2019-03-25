#![feature(alloc)]
#![no_std]

pub mod attribute;
pub(crate) mod block_iter;
pub(crate) mod cluster;
pub mod datetime;
pub mod directory;
pub mod filesystem;
pub mod name;
pub(crate) mod table;
mod utils;

use byteorder::{ByteOrder, LittleEndian};
use libfs::block::{Block, BlockCount, BlockDevice, BlockIndex};

use cluster::Cluster;

use filesystem::FatFileSystem;

use libfs::FileSystemError;

#[derive(PartialEq)]
pub enum FatFsType {
    Fat12,
    Fat16,
    Fat32,
    ExFat,
}

struct FatVolumeBootRecord {
    data: Block,
    fat_type: FatFsType,
    cluster_count: u32,
}

impl FatVolumeBootRecord {
    pub fn new(data: Block) -> FatVolumeBootRecord {
        let mut res = FatVolumeBootRecord {
            data,
            fat_type: FatFsType::Fat12,
            cluster_count: 0,
        };

        let root_dir_blocks = ((u32::from(res.root_dir_childs_count()) * 32)
            + (u32::from(res.bytes_per_block()) - 1))
            / u32::from(res.bytes_per_block());
        let data_blocks = res.total_blocks()
            - (u32::from(res.reserved_block_count())
                + (u32::from(res.fats_count()) * res.fat_size())
                + root_dir_blocks);
        let cluster_count = data_blocks / u32::from(res.blocks_per_cluster());
        if cluster_count < 4085 {
            res.fat_type = FatFsType::Fat12;
        } else if cluster_count < 65525 {
            res.fat_type = FatFsType::Fat16;
        } else {
            res.fat_type = FatFsType::Fat32;
        }
        res.cluster_count = cluster_count + 2;

        res
    }

    pub fn is_valid(&self) -> bool {
        const BOOTABLE_SIGNATURE: usize = 510;
        const SYSTEM_IDENTIFIER_FAT: usize = 36;
        const SYSTEM_IDENTIFIER_FAT32: usize = 82;

        // check boot signature
        if LittleEndian::read_u16(&self.data[BOOTABLE_SIGNATURE..BOOTABLE_SIGNATURE + 2]) != 0xAA55
        {
            return false;
        }

        // check jump code
        if self.data[0] != 0xE9 && self.data[0] != 0xEB && self.data[0] != 0xE8 {
            return false;
        }

        // check system identifier
        if self.data[SYSTEM_IDENTIFIER_FAT..SYSTEM_IDENTIFIER_FAT + 3] != [0x46, 0x41, 0x54]
            && self.data[SYSTEM_IDENTIFIER_FAT32..SYSTEM_IDENTIFIER_FAT32 + 5]
                != [0x46, 0x41, 0x54, 0x33, 0x32]
        {
            return false;
        }

        // TODO: bytes per block that are > to 512 bytes.
        if self.bytes_per_block() != Block::LEN as u16 {
            return false;
        }

        true
    }

    pub fn bytes_per_block(&self) -> u16 {
        LittleEndian::read_u16(&self.data[11..13])
    }

    pub fn blocks_per_cluster(&self) -> u8 {
        self.data[13]
    }

    pub fn reserved_block_count(&self) -> u16 {
        LittleEndian::read_u16(&self.data[14..16])
    }

    pub fn fats_count(&self) -> u8 {
        self.data[16]
    }

    pub fn root_dir_childs_count(&self) -> u16 {
        LittleEndian::read_u16(&self.data[17..19])
    }

    pub fn total_blocks16(&self) -> u16 {
        LittleEndian::read_u16(&self.data[19..21])
    }

    pub fn media_type(&self) -> u8 {
        self.data[21]
    }

    pub fn fat_size16(&self) -> u16 {
        LittleEndian::read_u16(&self.data[22..24])
    }

    pub fn blocks_per_track(&self) -> u16 {
        LittleEndian::read_u16(&self.data[24..26])
    }

    pub fn num_heads(&self) -> u16 {
        LittleEndian::read_u16(&self.data[26..28])
    }

    pub fn hidden_blocks(&self) -> u32 {
        LittleEndian::read_u32(&self.data[28..32])
    }

    pub fn total_blocks32(&self) -> u32 {
        LittleEndian::read_u32(&self.data[32..36])
    }

    pub fn fat_size32(&self) -> u32 {
        LittleEndian::read_u32(&self.data[36..40])
    }

    pub fn fs_info_block(&self) -> u16 {
        LittleEndian::read_u16(&self.data[48..50])
    }

    pub fn root_dir_childs_cluster(&self) -> Cluster {
        Cluster(LittleEndian::read_u32(&self.data[44..48]))
    }

    pub fn fat_size(&self) -> u32 {
        let result = u32::from(self.fat_size16());
        if result != 0 {
            result
        } else {
            self.fat_size32()
        }
    }

    pub fn total_blocks(&self) -> u32 {
        let result = u32::from(self.total_blocks16());
        if result != 0 {
            result
        } else {
            self.total_blocks32()
        }
    }
}

fn parse_fat_boot_record<T>(
    block_device: T,
    partition_start: BlockIndex,
    partition_block_count: BlockCount,
) -> Result<FatFileSystem<T>, FileSystemError>
where
    T: BlockDevice,
{
    let mut blocks = [Block::new()];

    block_device
        .read(&mut blocks, partition_start, BlockIndex(0))
        .or(Err(FileSystemError::ReadFailed))?;

    let block = &blocks[0];

    let boot_record: FatVolumeBootRecord = FatVolumeBootRecord::new(block.clone());

    if !boot_record.is_valid() {
        return Err(FileSystemError::InvalidPartition);
    }

    match boot_record.fat_type {
        FatFsType::Fat12 | FatFsType::Fat16 | FatFsType::ExFat => unimplemented!(),
        FatFsType::Fat32 => {
            let first_data_offset = u32::from(boot_record.reserved_block_count())
                + (u32::from(boot_record.fats_count()) * boot_record.fat_size());
            let mut file_system = FatFileSystem::new(
                block_device,
                partition_start,
                BlockIndex(first_data_offset),
                partition_block_count,
                boot_record,
            );
            file_system.init()?;
            Ok(file_system)
        }
    }
}

pub fn get_raw_partition<T>(block_device: T) -> Result<FatFileSystem<T>, FileSystemError>
where
    T: BlockDevice,
{
    parse_fat_boot_record(block_device, BlockIndex(0), BlockCount(0))
}

pub fn get_partition<T>(
    block_device: T,
    index: BlockIndex,
) -> Result<FatFileSystem<T>, FileSystemError>
where
    T: BlockDevice,
{
    let mut blocks = [Block::new()];

    const PARITION_TABLE_OFFSET: usize = 446;
    const MBR_SIGNATURE: usize = 510;
    const PARITION_TABLE_ENTRY_SIZE: usize = 16;

    block_device
        .raw_read(&mut blocks, index)
        .or(Err(FileSystemError::ReadFailed))?;

    let block = &blocks[0];

    if LittleEndian::read_u16(&block[MBR_SIGNATURE..MBR_SIGNATURE + 2]) != 0xAA55 {
        return Err(FileSystemError::InvalidPartition);
    }

    let partition = if index.0 < 4 {
        let offset = PARITION_TABLE_OFFSET + (PARITION_TABLE_ENTRY_SIZE * (index.0 as usize));
        &block[offset..offset + PARITION_TABLE_ENTRY_SIZE]
    } else {
        return Err(FileSystemError::PartitionNotFound);
    };

    if (partition[0] & 0x7F) != 0 {
        return Err(FileSystemError::InvalidPartition);
    }

    let partition_start = LittleEndian::read_u32(&partition[0x8..0xC]);
    let partition_block_count = LittleEndian::read_u32(&partition[0xC..0x10]);
    let partition_type: u32 = partition[0x4].into();

    match partition_type {
        0xC => parse_fat_boot_record(
            block_device,
            BlockIndex(partition_start),
            BlockCount(partition_block_count),
        ),
        _ => Err(FileSystemError::UnknownPartitionFormat { partition_type }),
    }
}
