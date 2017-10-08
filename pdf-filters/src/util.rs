fn ascii_hex_decode(ch1: u8, ch2: u8) -> u8 {
    let v1 = match ch1 {
        b'0' ... b'9' => ch1 - b'0',
        b'a' ... b'f' => ch1 - b'a' + 10,
        b'A' ... b'F' => ch1 - b'A' + 10,
        _ => { panic!("Invalid input"); },
    };

    let v2 = match ch2 {
        b'0' ... b'9' => ch2 - b'0',
        b'a' ... b'f' => ch2 - b'a' + 10,
        b'A' ... b'F' => ch2 - b'A' + 10,
        _ => { panic!("Invalid input"); },
    };

    (v1 << 4) + v2
}

/// Fixed-size stack-allocated buffer
///
/// workflow on read:
///  1. Move any allocated data to front
///  2. Request more data
///  3. Output data to reader
///  4. Set start, end correctly
pub struct Buffer
