use byteorder::{ByteOrder, LittleEndian};
use core::cmp;
use core::num;

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

#[derive(Default, Debug)]
pub struct ShortFileNameContext {
    pub checksum_inserted: bool,
    pub checksum: u16,
    pub short_name_base: [u8; ShortFileName::BASE_FILE_NAME_LEN],
    pub short_name_base_len: usize,
    pub short_name_ext: [u8; ShortFileName::EXT_LEN + 1],
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
        } else {
            len
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

    pub fn create(context: &mut ShortFileNameContext, lfn: &str) -> ShortFileName {
        let mut is_lossy = false;
        if context.short_name_base_len == 0 {
            let dot_position = lfn.rfind(".").unwrap_or(lfn.len());

            let (basename_len, _basename_fits, basename_lossy) = Self::copy_format_sfn_part(&mut context.short_name_base, &lfn[..dot_position]);
            is_lossy = is_lossy || basename_lossy;
            context.short_name_base_len = basename_len;

            context.short_name_ext_len = 0;
            if dot_position < lfn.len() {
                // '.'
                context.short_name_ext[0] = 0x2e;
                context.short_name_ext_len = 1;

                let mut copy_size = lfn.len() - dot_position;
                if copy_size > 3 {
                    copy_size = 3;
                }

                let (ext_len, _ext_fits, ext_lossy) = Self::copy_format_sfn_part(&mut context.short_name_ext[1..1 + copy_size], &lfn[dot_position + 1..]);
                context.short_name_ext_len += ext_len;

                is_lossy = is_lossy || ext_lossy;
                if ext_lossy {
                    // '~'
                    context.short_name_ext[context.short_name_ext_len - 1] = 0x7e;
                }
            }

            if context.short_name_base_len <= 2 {
                context.checksum = ShortFileName::checksum(&lfn.as_bytes());
                let mut checksum = context.checksum;

                for index in 0..4 {
                    let number = if checksum % 16 > 9 {
                        (checksum % 16) as u8 + 0x41 - 0xA
                    } else {
                        (checksum % 16) as u8 + 0x30
                    };

                    context.short_name_base[context.short_name_base_len + index] = number;
                    checksum >>= 4;
                }

                context.short_name_base_len += 4;
                context.checksum_inserted = true;
            }
        }

        context.last_index_value += 1;

        if context.last_index_value > 4 && !context.checksum_inserted {
            context.checksum = ShortFileName::checksum(&lfn.as_bytes());
            let mut checksum = context.checksum;

            for index in 2..6 {
                let number = if checksum % 16 > 9 {
                    (checksum % 16) as u8 + 0x41 - 0xA
                } else {
                    (checksum % 16) as u8 + 0x30
                };

                context.short_name_base[index] = number;
                checksum >>= 4;
            }

            context.last_index_value = 1;
            context.short_name_base_len = 6;
            context.checksum_inserted = true;
        }

        let mut index_buffer = [0x0u8; ShortFileName::BASE_FILE_NAME_LEN];

        let mut index = context.last_index_value;
        let mut index_buffer_len = ShortFileName::BASE_FILE_NAME_LEN;
        for i in 1..8 {
            if index <= 0 {
                index_buffer_len = i;
                break;
            }

            index_buffer[8 - i] = 0x30 + (index % 10) as u8;
            index /= 10;
        }

        // '~'
        index_buffer[8 - index_buffer_len] = 0x7e;

        let mut short_name = [0x20u8; ShortFileName::MAX_LEN];
        let mut short_name_len = 0;
        if context.short_name_base_len != 0 {
            (&mut short_name[0..context.short_name_base_len]).copy_from_slice(&context.short_name_base[0..context.short_name_base_len]);
            short_name_len += context.short_name_base_len;
        }

        if is_lossy || context.last_index_value > 1 {
            let slice = if short_name_len == 8 {
                (&mut short_name[short_name_len - index_buffer_len..short_name_len])
            } else {
                (&mut short_name[short_name_len..short_name_len + index_buffer_len])
            };

            slice.copy_from_slice(&index_buffer[8 - index_buffer_len..]);
        }
        //short_name_len += index_buffer_len;
        short_name_len = 8;

        if context.short_name_ext_len > 1 {
            (&mut short_name[short_name_len..short_name_len + context.short_name_ext_len - 1]).copy_from_slice(&context.short_name_ext[1..context.short_name_ext_len]);
            short_name_len = 11;
        }

        ShortFileName::from_data(&short_name)
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

    pub fn from_unformated_str(context: &mut ShortFileNameContext, name: &str) -> Self {
        //let mut short_name = [0x20u8; ShortFileName::MAX_LEN];
        ShortFileNameGenerator::create(context, name)

        // TODO: REMOVE THIS WHEN THE ABOVE IS FIXED
        //(&mut short_name[0..8]).copy_from_slice(&context.short_name_base);
        //(&mut short_name[8..11]).copy_from_slice(&context.short_name_ext);

        //Self::from_data(&short_name)
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
            checksum = (checksum << 7) + (checksum >> 1) + num::Wrapping(u16::from(*b));
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
