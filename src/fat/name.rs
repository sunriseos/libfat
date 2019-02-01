use core::num;

pub struct ShortFileName {
    contents: [u8; ShortFileName::MAX_LEN],
}

enum FileNameError {
    InvalidCharacter,
    FilenameEmpty,
}

impl ShortFileName {
    const BASE_FILE_NAME_LEN: usize = 8;
    const EXT_LEN: usize = 3;
    const MAX_LEN: usize = ShortFileName::BASE_FILE_NAME_LEN + ShortFileName::EXT_LEN;

    pub fn from_data(data: &[u8]) -> Self {
        let mut short_name = [0x20u8; ShortFileName::MAX_LEN];

        if data.len() != short_name.len() {
            panic!()
        }

        for val in 0..data.len() {
            short_name[val] = data[val];
        }
        ShortFileName {
            contents: short_name,
        }
    }

    pub fn from_sfn_str(name: &str) -> Self {
        unimplemented!()
    }

    pub fn from_unformated_str(name: &str) -> Self {
        let mut short_name = [0x20u8; ShortFileName::MAX_LEN];

        // does it have an extension?
        let (basename_len, name_fits, lossy_conv) = match name.rfind('.') {
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

    fn copy_format_sfn_part(dst: &mut [u8], src: &str) -> (usize, bool, bool) {
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
                'A'...'Z' | 'a'...'z' | '0'...'9' => c,
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
        for val in 0..self.contents.len() {
            res[val] = self.contents[val] as char;
        }
        res
    }

    pub fn checksum(short_name: &[u8; ShortFileName::MAX_LEN]) -> u8 {
        let mut checksum = num::Wrapping(0u8);
        for b in short_name {
            checksum = (checksum << 7) + (checksum >> 1) + num::Wrapping(*b);
        }
        checksum.0
    }
}
