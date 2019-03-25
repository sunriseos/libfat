use core::num;

use super::directory::raw_dir_entry::LongFileNameDirEntry;

pub struct ShortFileName {
    contents: [u8; ShortFileName::MAX_LEN],
}

#[derive(Clone)]
pub struct LongFileName {
    contents: [u16; LongFileName::MAX_LEN],
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
    fn copy_format_sfn_part(
        dst: &mut [u8],
        src: &str,
        is_base_name: bool,
    ) -> (usize, bool, bool) {
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

                // conflict with delete marker, remap to 0x5.
                'Õ' => {
                    if dst_pos == 0 && is_base_name {
                        '\x05'
                    } else {
                        'Õ'
                    }
                }

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
            let dot_position = lfn.rfind('.').unwrap_or_else(|| lfn.len());

            let (basename_len, _basename_fits, basename_lossy) = Self::copy_format_sfn_part(
                &mut context.short_name_base,
                &lfn[..dot_position],
                true,
            );
            is_lossy = is_lossy || basename_lossy;
            context.short_name_base_len = basename_len;

            context.short_name_ext_len = 0;
            if dot_position < lfn.len() {
                context.short_name_ext[0] = b'.';
                context.short_name_ext_len = 1;

                let mut copy_size = lfn.len() - dot_position;
                if copy_size > 3 {
                    copy_size = 3;
                }

                let (ext_len, _ext_fits, ext_lossy) = Self::copy_format_sfn_part(
                    &mut context.short_name_ext[1..=copy_size],
                    &lfn[dot_position + 1..],
                    false,
                );
                context.short_name_ext_len += ext_len;

                is_lossy = is_lossy || ext_lossy;
                if ext_lossy {
                    context.short_name_ext[context.short_name_ext_len - 1] = b'~';
                }
            }

            if context.short_name_base_len <= 2 {
                context.checksum = ShortFileName::checksum(&lfn.as_bytes());
                let mut checksum = context.checksum;

                for index in 0..4 {
                    let number = if checksum % 16 > 9 {
                        (checksum % 16) as u8 + b'A' - 0xA
                    } else {
                        (checksum % 16) as u8 + b'0'
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
                    (checksum % 16) as u8 + b'A' - 0xA
                } else {
                    (checksum % 16) as u8 + b'0'
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
            if index == 0 {
                index_buffer_len = i;
                break;
            }

            index_buffer[8 - i] = b'0' + (index % 10) as u8;
            index /= 10;
        }

        index_buffer[8 - index_buffer_len] = b'~';

        let mut short_name = [0x20u8; ShortFileName::MAX_LEN];
        let mut short_name_len = 0;
        if context.short_name_base_len != 0 {
            (&mut short_name[0..context.short_name_base_len])
                .copy_from_slice(&context.short_name_base[0..context.short_name_base_len]);
            short_name_len += context.short_name_base_len;
        }

        if is_lossy || context.last_index_value > 1 {
            let slice = if short_name_len == ShortFileName::BASE_FILE_NAME_LEN {
                (&mut short_name[short_name_len - index_buffer_len..short_name_len])
            } else {
                (&mut short_name[short_name_len..short_name_len + index_buffer_len])
            };

            slice.copy_from_slice(&index_buffer[8 - index_buffer_len..]);
        }

        short_name_len = ShortFileName::BASE_FILE_NAME_LEN;

        if context.short_name_ext_len > 1 {
            (&mut short_name[short_name_len..short_name_len + context.short_name_ext_len - 1])
                .copy_from_slice(&context.short_name_ext[1..context.short_name_ext_len]);
        }

        ShortFileName::from_data(&short_name)
    }
}

impl ShortFileName {
    const BASE_FILE_NAME_LEN: usize = 8;
    const EXT_LEN: usize = 3;
    pub const MAX_LEN: usize = ShortFileName::BASE_FILE_NAME_LEN + ShortFileName::EXT_LEN;

    pub fn from_data(data: &[u8]) -> Self {
        let mut short_name = [0x20u8; ShortFileName::MAX_LEN];

        short_name[..data.len()].clone_from_slice(&data[..]);
        ShortFileName {
            contents: short_name,
        }
    }

    pub fn from_unformated_str(context: &mut ShortFileNameContext, name: &str) -> Self {
        ShortFileNameGenerator::create(context, name)
    }

    pub fn chars(&self) -> [char; ShortFileName::MAX_LEN] {
        let mut res: [char; ShortFileName::MAX_LEN] = [' '; ShortFileName::MAX_LEN];
        for (index, dst) in res.iter_mut().enumerate().take(self.contents.len()) {
            // Remap 0x5 => 0xE5 as it's not a delete marker
            if index == 0 && self.contents[index] == 0x5 {
                *dst = 'Õ';
            } else {
                *dst = self.contents[index] as char;
            }
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

    pub fn checksum_lfn(short_name: &[u8]) -> u8 {
        let mut sum = num::Wrapping(0u8);
        for b in short_name {
            sum = num::Wrapping((sum.0 & 1) << 7)
                + num::Wrapping((sum.0 & 0xfe) >> 1)
                + num::Wrapping(*b);
        }
        sum.0
    }
}

impl LongFileName {
    pub const MAX_LEN: usize = 13;
    pub const MAX_LEN_UNICODE: usize = Self::MAX_LEN * 4;

    pub fn from_lfn_dir_entry(entry: &LongFileNameDirEntry) -> Self {
        let mut long_name = [0x0; LongFileName::MAX_LEN];

        let mut index = 0;
        for c in entry.char_part_0.iter() {
            long_name[index] = c.to_int();
            index += 1;
        }

        for c in entry.char_part_1.iter() {
            long_name[index] = c.to_int();
            index += 1;
        }

        for c in entry.char_part_2.iter() {
            long_name[index] = c.to_int();
            index += 1;
        }

        LongFileName {
            contents: long_name,
        }
    }

    pub fn from_utf8(data: &str) -> Self {
        let mut long_name = [0x0u16; LongFileName::MAX_LEN];

        for (i, c) in data.chars().enumerate().take(LongFileName::MAX_LEN) {
            c.encode_utf16(&mut long_name[i..]);
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

    pub fn as_contents(&self) -> [u16; LongFileName::MAX_LEN] {
        self.contents
    }
}
