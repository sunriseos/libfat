
pub mod block;

use block::{Block, BlockCount, BlockDevice, BlockIndex};

use byteorder::{ByteOrder, LittleEndian};

use crate::FileSystemError;


pub struct FatFileSystem<T> where T: BlockDevice {
    block_device: T,
    partition_start: BlockIndex,
    partition_block_count: BlockCount
}

impl<T> FatFileSystem<T> where T: BlockDevice {
    pub fn new(block_device: T, partition_start: BlockIndex, partition_block_count: BlockCount) -> FatFileSystem<T> {
        FatFileSystem { block_device, partition_start, partition_block_count }
    }
}


fn parse_fat_boot_record<T>(block_device: T, partition_start: BlockIndex, partition_block_count: BlockCount) -> Result<FatFileSystem<T>, FileSystemError> where T: BlockDevice {
    let mut blocks = [Block::new()];
    
    block_device.read(&mut blocks, partition_start).or(Err(FileSystemError::ReadFailed))?;
    
    unimplemented!()
}

pub fn get_partition<T>(block_device: T, index : BlockIndex) -> Result<FatFileSystem<T>, FileSystemError> where T: BlockDevice {
    let mut blocks = [Block::new()];

    const PARITION_TABLE_OFFSET: usize = 446;
    const MBR_SIGNATURE: usize = 510;
    const PARITION_TABLE_ENTRY_SIZE: usize = 16;

    block_device.read(&mut blocks, index).or(Err(FileSystemError::ReadFailed))?;

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
        0xC => {
            parse_fat_boot_record(block_device, BlockIndex(partition_start), BlockCount(partition_block_count))
        },
        _ => {
            Err(FileSystemError::UnknownPartitionFormat {partition_type})
        }
    }

    
}