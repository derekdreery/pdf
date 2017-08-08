//! Utility functions used elsewhere
use num;
use nom;

macro_rules! pdf_ws (
    ($i:expr, $($args:tt)*) => {{
        use $crate::util::eat_pdf_ws;
        sep!($i, eat_pdf_ws, $($args)*)
    }}
);

named!(pub eat_pdf_ws, eat_separator!(&b"\0\t\n\x0c\r "[..]));

/// Converts [u8; 3] to u8
#[inline]
pub fn arr3_to_oct(v: &[u8; 3]) -> u8 {
    (v[0] - b'0').wrapping_shl(6) + (v[1] - b'0').wrapping_shl(3) + v[2] - b'0'
}

/// Converts ascii hex value to u8
#[inline]
pub fn ascii_hex_to_u8(i: &u8) -> Option<u8> {
    match *i {
        b'0' ... b'9' => Some(i - b'0'),
        b'a' ... b'f' => Some(i - b'a' + 10),
        b'A' ... b'F' => Some(i - b'A' + 10),
        _ => None
    }
}

/// Decode patterns like #23 into b'\x23'
pub fn decode_number_sign(input: &[u8]) -> Vec<u8> {
    let mut iter = input.iter().enumerate();
    let mut out = Vec::with_capacity(input.len()); // upper bound

    while let Some((i, &ch)) = iter.next() {
        // We try to be forgiving here for old pdf documents
        if ch == b'#' {
            match (input.get(i+1).and_then(ascii_hex_to_u8),
                  input.get(i+2).and_then(ascii_hex_to_u8)) {
                (Some(first), Some(second)) => {
                    out.push((first << 4) + second);
                    iter.next();
                    iter.next();
                }
                _ => out.push(b'#')
            }
        } else {
            out.push(ch);
        }
    }
    println!("out: {:?}", String::from_utf8_lossy(&out));
    out
}

named!(pub parse_line_ending, alt!(tag!(b"\r\n") | tag!(b"\r") | tag!(b"\n")));

named!(pub parse_pos_num<u64>, flat_map!(call!(nom::digit), parse_to!(usize)));

/// Helper function to check a byte to see if pdf considers it whitespace (see spec for which char
/// codes are whitespace)
#[inline]
pub fn is_whitespace(ch: u8) -> bool {
    match ch {
        0x00 | 0x09 | 0x0A | 0x0C | 0x0D | 0x20 => true,
        _ => false
    }
}

/// Helper function to check a byte to see if pdf considers it a delimiter (see spec for which char
/// codes are delimiters)
#[inline]
pub fn is_delimiter(ch: u8) -> bool {
    match ch {
        0x28 | 0x29 | 0x3C | 0x3E | 0x5B | 0x5D | 0x7B | 0x7D | 0x2F | 0x25 => true,
        _ => false
    }
}

/// Helper function to check a byte to see if pdf considers it a normal character (not a
/// delimiter or a whitespace character)
#[inline]
pub fn is_normal_char(ch: u8) -> bool {
    ! is_whitespace(ch) && ! is_delimiter(ch)
}

#[cfg(test)]
/// Helper macro to test some input and output against a parser
macro_rules! test_helper {
    ($fn:ident => [ $( ( $input:expr, $output:expr ) ),+ ] ) => {{
        $(
            assert_eq!(super::$fn(&$input[..])
                       /**/.map_err(|_err| error_code!(ErrorKind::Custom(0)))/**/, $output);
        )+
    }};
    ($fn:ident => [ $( ( $input:expr, $output:expr, $msg:expr ) ),+ ] ) => {{
        $(
            assert_eq!(super::$fn(&$input[..])
                       /*.map_err(|_err| error_code!(ErrorKind::Custom(0)))*/, $output, $msg);
        )+
    }}
}

macro_rules! test_error {
    () => (IResult::Error(error_code!(ErrorKind::Custom(0))))
}

