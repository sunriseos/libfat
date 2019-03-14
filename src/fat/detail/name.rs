use byteorder::{ByteOrder, LittleEndian};
use core::num;
use core::cmp;

pub struct ShortFileName {
    contents: [u8; ShortFileName::MAX_LEN],
}

pub struct LongFileName {
    contents: [u16; LongFileName::MAX_LEN],
}

// TODO: use that
pub enum FileNameError {
    InvalidCharacter,
    FilenameEmpty,
}

pub struct ShortFileNameGenerator;
pub struct ShortFileNameContext {
    pub checksum_inserted: bool,
    pub checksum: u16,
    pub short_name_base: [u8; ShortFileName::BASE_FILE_NAME_LEN],
    pub short_name_base_len: usize,
    pub short_name_ext: [u8; ShortFileName::EXT_LEN],
    pub short_name_ext_len: usize,
    pub last_index_value: usize,
    
}

impl ShortFileNameGenerator {
     
    fn get_index_len(index: usize) -> usize {
        let mut tmp = index;
        let mut len = 0;

        while tmp != 0 {
            tmp /= 10;
            len += 1;
        }

        if len == 0 {
            1
        } else { len }
    }

    pub fn create(context: &mut ShortFileNameContext, lfn: &str) -> Result<(), FileNameError> {
        let mut short_name_base = [0x20u8; ShortFileName::BASE_FILE_NAME_LEN];
        let mut short_name_ext = [0x20u8; ShortFileName::EXT_LEN];

        let (short_name_base_len, short_name_ext_len) = match lfn.rfind('.') {
            Some(index) => {
                let (basename_len, _basename_fits, _basename_lossy) =
                    ShortFileName::copy_format_sfn_part(&mut short_name_base, &lfn[..index]);

                let (ext_len, _ext_fits, _ext_lossy) =
                    ShortFileName::copy_format_sfn_part(&mut short_name_ext, &lfn[index + 1..]);
                (
                    basename_len,
                    ext_len
                )
            }
            None => {
                let (basename_len, _basename_fits, _basename_lossy) =
                    ShortFileName::copy_format_sfn_part(&mut short_name_base, &lfn);
                (basename_len, 0)
            }
        };

        let mut index_len = Self::get_index_len(context.last_index_value);
        let mut copy_len = 0;
        let mut checksum = 0;

        if context.checksum_inserted {
            checksum = ShortFileName::checksum(&short_name_base);
            copy_len = cmp::min(short_name_base_len, 8 - 4 - 1 - index_len);
        } else {
            copy_len = cmp::min(short_name_base_len, 8 - 1 - index_len);
        }

        if context.short_name_base_len == short_name_base_len &&
           context.short_name_base == short_name_base &&
           context.short_name_ext_len == short_name_ext_len &&
           context.short_name_ext == short_name_ext &&
           context.checksum == checksum &&
           context.last_index_value < 999 {
               context.last_index_value += 1;
               if !context.checksum_inserted && context.last_index_value > 0 {
                   context.checksum_inserted = true;
                   context.checksum = ShortFileName::checksum(&short_name_base);
               }
        } else {
            context.last_index_value = 1;
            context.checksum_inserted = false;
        }

        // recompute copy_len as checksum_inserted might have changed
        index_len = Self::get_index_len(context.last_index_value);
        if context.checksum_inserted {
            copy_len = cmp::min(short_name_base_len, 8 - 4 - 1 - index_len);
        } else {
            copy_len = cmp::min(short_name_base_len, 8 - 1 - index_len);
        }

        // TODO: build name (8.3)
        // TODO: add checksum if needed

        context.short_name_base_len = copy_len;
        context.short_name_ext_len = short_name_ext_len;
        unimplemented!()
     }
}

impl ShortFileName {
    const BASE_FILE_NAME_LEN: usize = 8;
    const EXT_LEN: usize = 3;
    const MAX_LEN: usize = ShortFileName::BASE_FILE_NAME_LEN + ShortFileName::EXT_LEN;

    pub fn from_data(data: &[u8]) -> Self {
        let mut short_name = [0x20u8; ShortFileName::MAX_LEN];

        short_name[..data.len()].clone_from_slice(&data[..]);
        ShortFileName {
            contents: short_name,
        }
    }

    pub fn from_sfn_str(_name: &str) -> Self {
        unimplemented!()
    }

    pub fn from_unformated_str(name: &str) -> Self {
        let mut short_name = [0x20u8; ShortFileName::MAX_LEN];

        // does it have an extension?
        let (_basename_len, _name_fits, _lossy_conv) = match name.rfind('.') {
            Some(index) => {
                let (basename_len, basename_fits, basename_lossy) =
                    Self::copy_format_sfn_part(&mut short_name[0..8], &name[..index]);
                let (_, ext_fits, ext_lossy) =
                    Self::copy_format_sfn_part(&mut short_name[8..11], &name[index + 1..]);
                (
                    basename_len,
                    basename_fits && ext_fits,
                    basename_lossy || ext_lossy,
                )
            }
            None => {
                let (basename_len, basename_fits, basename_lossy) =
                    Self::copy_format_sfn_part(&mut short_name[0..8], &name);
                (basename_len, basename_fits, basename_lossy)
            }
        };

        ShortFileName {
            contents: short_name,
        }
    }

    pub fn copy_format_sfn_part(dst: &mut [u8], src: &str) -> (usize, bool, bool) {
        let mut dst_pos = 0;
        let mut lossy_convertion = false;
        for c in src.chars() {
            if dst_pos == dst.len() {
                // SFN is full!
                return (dst_pos, false, lossy_convertion);
            }

            // remap chars to support 8.3 correctly
            let lfn_char = match c {
                ' ' | '.' => {
                    lossy_convertion = true;
                    continue;
                }

                // valid chars
                'A'..='Z' | 'a'..='z' | '0'..='9' => c,
                '!' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '-' | '@' | '^' | '_' | '`'
                | '{' | '}' | '~' => c,

                // disallowed remap
                _ => '_',
            };

            lossy_convertion = lossy_convertion || (lfn_char != c);

            // 8.3 only support uppercase
            let uppercase = lfn_char.to_ascii_uppercase();
            dst[dst_pos] = uppercase as u8;
            dst_pos += 1;
        }
        (dst_pos, true, lossy_convertion)
    }

    pub fn chars(&self) -> [char; ShortFileName::MAX_LEN] {
        let mut res: [char; ShortFileName::MAX_LEN] = [' '; ShortFileName::MAX_LEN];
        for (index, dst) in res.iter_mut().enumerate().take(self.contents.len()) {
            *dst = self.contents[index] as char;
        }
        res
    }

    pub fn as_bytes(&self) -> [u8; ShortFileName::MAX_LEN] {
        self.contents
    }

    // TODO: rewrite this
    pub fn checksum(short_name: &[u8]) -> u16 {
        let mut checksum = num::Wrapping(0u16);
        for b in short_name {
            checksum = (checksum << 7) + (checksum >> 1) + num::Wrapping(*b as u16);
        }
        checksum.0
    }
}

impl LongFileName {
    pub const MAX_LEN: usize = 13;

    pub fn from_data(data: &[u8]) -> Self {
        let mut long_name = [0x0; LongFileName::MAX_LEN];

        for (i, entry) in long_name.iter_mut().enumerate().take(5) {
            let index = 1 + i * 2;
            *entry = LittleEndian::read_u16(&data[index..index + 2]);
        }
        for i in 0..6 {
            let index = 0xE + i * 2;
            let i = i + 5;
            long_name[i] = LittleEndian::read_u16(&data[index..index + 2]);
        }

        for i in 0..2 {
            let index = 0x1C + i * 2;
            let i = i + 11;
            long_name[i] = LittleEndian::read_u16(&data[index..index + 2]);
        }
        LongFileName {
            contents: long_name,
        }
    }

    pub fn chars(&self) -> Option<[char; Self::MAX_LEN]> {
        let val = &self.contents;

        let mut res: [char; Self::MAX_LEN] = [' '; Self::MAX_LEN];

        for (i, c) in core::char::decode_utf16(val.iter().cloned()).enumerate() {
            if let Ok(c) = c {
                res[i] = c;
            } else {
                return None;
            }
        }

        Some(res)
    }
}
