//! FAT entry attribute.

#[derive(Debug, Clone, Copy)]
/// Represent a 8.3 entry attribute.
pub struct Attributes(u8);

impl Attributes {
    /// The filesystem will not allow a file to be opened for modification.
    // TODO: Follow this behaviour.
    pub const READ_ONLY: u8 = 0x01;

    /// Hides files or directories from normal directory views.
    pub const HIDDEN: u8 = 0x02;

    /// Indicates that the file belongs to the system and must not be physically moved (e.g., during defragmentation), because there may be references into the file using absolute addressing bypassing the file system (boot loaders, kernel images, swap files, extended attributes, etc.).
    // TODO: Follow this behaviour.
    pub const SYSTEM: u8 = 0x04;

    /// Indicates an optional directory volume label, normally only residing in a volume's root directory.
    pub const VOLUME: u8 = 0x08;

    /// Indicates that the cluster-chain associated with this entry gets interpreted as subdirectory instead of as a file. Subdirectories have a filesize entry of zero.
    pub const DIRECTORY: u8 = 0x10;

    /// Typically set by the filesystem as soon as the file is created or modified to mark the file as "dirty", and reset by backup software once the file has been backed up to indicate "pure" state.
    pub const ARCHIVE: u8 = 0x20;

    /// Define a device file. SHOULDN'T APPEARS ON DISK.
    pub const DEVICE: u8 = 0x40;

    /// Indicates a long file name entry.
    pub const LFN: u8 = Self::READ_ONLY | Self::HIDDEN | Self::SYSTEM | Self::VOLUME;

    /// Create a new Attributes from a raw u8 value.
    pub fn new(value: u8) -> Attributes {
        Attributes(value)
    }

    /// Check if the read only bit is set.
    pub fn is_read_only(self) -> bool {
        (self.0 & Self::READ_ONLY) == Self::READ_ONLY
    }

    /// Check if the hidden bit is set.
    pub fn is_hidden(self) -> bool {
        (self.0 & Self::HIDDEN) == Self::HIDDEN
    }

    /// Check if the system bit is set.
    pub fn is_system(self) -> bool {
        (self.0 & Self::SYSTEM) == Self::SYSTEM
    }

    /// Check if the volume bit is set.
    pub fn is_volume(self) -> bool {
        (self.0 & Self::VOLUME) == Self::VOLUME
    }

    /// Check if the directory bit is set.
    pub fn is_directory(self) -> bool {
        (self.0 & Self::DIRECTORY) == Self::DIRECTORY
    }

    /// Check if the archive bit is set.
    pub fn is_archive(self) -> bool {
        (self.0 & Self::ARCHIVE) == Self::ARCHIVE
    }

    /// Check if it is a long file name.
    pub fn is_lfn(self) -> bool {
        (self.0 & Self::LFN) == Self::LFN
    }

    /// Check if it is a block device.
    pub fn is_device(self) -> bool {
        (self.0 & Self::DEVICE) == Self::DEVICE
    }

    /// Get the raw attribute value.
    pub fn get_value(self) -> u8 {
        self.0
    }
}
