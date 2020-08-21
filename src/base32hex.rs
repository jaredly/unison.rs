fn default_str_map() -> &'static str {
    "0123456789abcdefghijklmnopqrstuv"
}

// Converted from python https://github.com/graham/base32hex/blob/master/base32hex.py
pub fn encode(mut src: &[u8]) -> String {
    let mut dst = vec![];
    let str_map = default_str_map();

    while src.len() > 0 {
        let src_len = src.len();
        let mut next_byte = [0; 8];

        if src_len > 4 {
            next_byte[7] = src[4] & 0x1f;
            next_byte[6] = src[4] >> 5;
        }

        if src_len > 3 {
            next_byte[6] = next_byte[6] | (src[3] << 3) & 0x1f;
            next_byte[5] = (src[3] >> 2) & 0x1f;
            next_byte[4] = src[3] >> 7;
        }

        if src_len > 2 {
            next_byte[4] = next_byte[4] | (src[2] << 1) & 0x1f;
            next_byte[3] = (src[2] >> 4) & 0x1f;
        }

        if src_len > 1 {
            next_byte[3] = next_byte[3] | (src[1] << 4) & 0x1f;
            next_byte[2] = (src[1] >> 1) & 0x1f;
            next_byte[1] = (src[1] >> 6) & 0x1f;
        }

        if src_len > 0 {
            next_byte[1] = next_byte[1] | (src[0] << 2) & 0x1f;
            next_byte[0] = src[0] >> 3;
        }

        for nb in next_byte.iter() {
            let i = *nb as usize;
            dst.push(str_map[i..i + 1].to_owned());
        }

        if src_len > 4 {
            src = &src[5..];
        } else {
            break;
        }
    }

    dst.join("")
}

fn getHexValue(hexes: &str, value: &str) -> u8 {
    hexes.find(value).unwrap() as u8
}

pub fn decode(src: &str) -> Vec<u8> {
    let mut src = src.to_lowercase();
    let hexes = default_str_map();

    let mut end = false;
    let mut result = vec![];
    while src.len() > 0 && end == false {
        let mut dst = vec![0, 0, 0, 0, 0];
        let mut dbuf = [0; 8];

        let mut src_len = 8;

        for i in 0..8 {
            if i >= src.len() {
                src_len = i;
                end = true;
                break;
            }
            let char = &src[i..i + 1];
            if char == "=" {
                end = true;
                src_len = i;
                break;
            } else {
                dbuf[i] = getHexValue(hexes, char);
            }
        }

        if src_len >= 8 {
            dst[4] = (dbuf[6] << 5) | (dbuf[7]);
        }
        if src_len >= 7 {
            dst[3] = (dbuf[4] << 7) | (dbuf[5] << 2) | (dbuf[6] >> 3);
        }
        if src_len >= 5 {
            dst[2] = (dbuf[3] << 4) | (dbuf[4] >> 1);
        }
        if src_len >= 4 {
            dst[1] = (dbuf[1] << 6) | (dbuf[2] << 1) | (dbuf[3] >> 4);
        }
        if src_len >= 2 {
            dst[0] = (dbuf[0] << 3) | (dbuf[1] >> 2);
        }

        for i in 0..5 {
            dst[i] = dst[i] & 0xff;
        }

        if src_len == 2 {
            dst = dst[..1].into();
        } else if src_len == 4 {
            dst = dst[..2].into();
        } else if src_len == 5 {
            dst = dst[..3].into();
        } else if src_len == 7 {
            dst = dst[..4].into();
        } else if src_len == 8 {
            dst = dst[..5].into();
        }

        result.extend(dst);
        src = src[8..].into();
    }

    return result;
}
