//! Utility functions used elsewhere
//!
//! This is a bit of a mess of jumbled up functionality
//use num;
use nom;

macro_rules! pdf_ws (
    ($i:expr, $($args:tt)*) => {{
        use $crate::util::eat_pdf_ws;
        sep!($i, eat_pdf_ws, $($args)*)
    }}
);

named!(#[doc="Matches all tokens considered whitespace in pdf, and consumes them"],
       pub eat_pdf_ws, eat_separator!(&b"\0\t\n\x0c\r "[..]));

/// Converts [u8; 3] to u8. Used in TODO
#[inline]
pub fn arr3_to_oct(v: &[u8; 3]) -> u8 {
    (v[0] - b'0').wrapping_shl(6) + (v[1] - b'0').wrapping_shl(3) + (v[2] - b'0')
}

/// Converts u8 to [u8; 3]. Used in TODO
#[inline]
pub fn oct_to_arr3(v: u8) -> [u8; 3] {
    let v0 = ((v & 0b11000000) >> 6) + b'0';
    let v1 = ((v & 0b00111000) >> 3) + b'0';
    let v2 = (v & 0b00000111) + b'0';
    [v0, v1, v2]
}

/// Converts ascii hex value to u8. Used in TODO
#[inline]
pub fn ascii_hex_to_u8(i: &u8) -> Option<u8> {
    match *i {
        b'0' ... b'9' => Some(i - b'0'),
        b'a' ... b'f' => Some(i - b'a' + 10),
        b'A' ... b'F' => Some(i - b'A' + 10),
        _ => None
    }
}

/// Turn non-ascii charaters into "\x00" hex format. Used in TODO
pub fn encode_non_ascii(input: &[u8]) -> String {
    use std::fmt::Write;

    let mut output = String::with_capacity(input.len());
    for &ch in input {
        if ch > 31 && ch < 127 {
            output.push(ch.into());
        } else {
            write!(&mut output, "\\x{:x}", ch).unwrap(); // cannot fail
        }
    }
    output
}

/// Decode patterns like #23 into b'\x23'. Used in TODO
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
    //println!("out: {:?}", String::from_utf8_lossy(&out));
    out
}

named!(#[doc=r#"Parses a line ending in a pdf file, which is one of "\r\n", "\r", or "\n". "#],
       pub parse_line_ending, alt!(tag!(b"\r\n") | tag!(b"\r") | tag!(b"\n")));

named!(#[doc="A simple parser that takes an array of digits (`[0-9]`) and parses them into a \
       usize"],
    pub parse_pos_num<usize>, flat_map!(call!(nom::digit), parse_to!(usize)));

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

named!(#[doc = "parse a line ending in a pdf file"],
       pub pdf_eol,
       alt!(tag!(b"\r\n") | tag!(b"\r") | tag!(b"\n")));

named!(#[doc = "Line endings are different for xref records to preserve alignment \
                (\\r and \\n are preceeded by ` `)"],
       pub xref_eol,
       alt!(tag!(b" \r") | tag!(b" \n") | tag!(b"\r\n")));

/// Takes a byte array and returns an escaped string of ascii
#[cfg(test)]
pub fn u8_to_esc_ascii(i: &[u8]) -> String {
    use std::char;
    use std::ascii;

    let mut s = String::new();
    for &ch in i {
        for ech in ascii::escape_default(ch) {
            s.push(char::from_u32(ech as u32).unwrap()) // should never fail (from escape default)
        }
    }
    s
}

#[cfg(test)]
/// Helper macro to test some input and output against a parser
macro_rules! test_helper {
    ($fn:ident => [ $( ( $input:expr, $output:expr ) ),+ ] ) => {{
        $(
            assert_eq!(super::$fn(&$input[..])
                       /*.map_err(|_err| error_code!(nom::ErrorKind::Custom(0)))*/, $output,
                       r#""{}""#, ::util::u8_to_esc_ascii($input));
        )+
    }};
}

#[cfg(test)]
macro_rules! test_error {
    () => (IResult::Error(error_code!(nom::ErrorKind::Custom(0))))
}

/*
/// Useful to log a value as it is passed through
pub fn debug_log<D: ::std::fmt::Debug>(d: D) -> D {
    println!("{:?}", d);
    d
}
*/

/// Like try, but for option (yes I know it's naughty not to type errors, todo this before 1.0)
macro_rules! try_opt {
    ($e:expr) => {
        match $e {
            Some(inner) => inner,
            None => { return None; },
        }
    }
}

#[test]
fn test_oct_arr3() {
    let input = vec![
        [b'3', b'7', b'7'],
        [b'0', b'0', b'1'],
        [b'0', b'0', b'0'],
    ];
    for input in input {
        assert_eq!(oct_to_arr3(arr3_to_oct(&input)), input);
    }
    // case with overflow
    assert_eq!(oct_to_arr3(arr3_to_oct(&[b'7', b'7', b'7'])), [b'3', b'7', b'7']);
    assert_eq!(oct_to_arr3(arr3_to_oct(&[b'6', b'7', b'7'])), [b'2', b'7', b'7']);
}

/// Copy this from unstable so it works with stable rust
pub trait TryFrom<T> where Self: Sized {
    type Error;

    fn try_from(T) -> Result<Self, Self::Error>;
}

/// Copy this from unstable so it works with stable rust
pub trait TryInto<T> where T: Sized {
    type Error;

    /// Tries to perform the conversion
    fn try_into(self) -> Result<T, Self::Error>;
}

impl<S, T> TryInto<T> for S where T: TryFrom<S> {
    type Error = T::Error;

    fn try_into(self) -> Result<T, Self::Error> {
        T::try_from(self)
    }
}

/// Failable reference-to-reference conversion
pub trait TryAsRef<T> {
    type Error;

    /// Tries to get a reference
    fn try_as_ref(&self) -> Result<&T, Self::Error>;
}

/// Like dbg_dmp in nom, but limits the size of the hex dump to 10k
macro_rules! my_dbg_dmp (
  ($i: expr, $submac:ident!( $($args:tt)* ), $max_dmp:expr) => (
    {
      use nom::HexDisplay;
      let l = line!();
      match $submac!($i, $($args)*) {
        ::nom::IResult::Error(a) => {
          println!("Error({:?}) at l.{} by ' {} '\n{}", a, l, stringify!($submac!($($args)*)),
                   &$i[0..::std::cmp::min($i.len(), $max_dmp)].to_hex(8));
          ::nom::IResult::Error(a)
        },
        ::nom::IResult::Incomplete(a) => {
          println!("Incomplete({:?}) at {} by ' {} '\n{}", a, l, stringify!($submac!($($args)*)),
                   &$i[0..::std::cmp::min($i.len(), $max_dmp)].to_hex(8));
          ::nom::IResult::Incomplete(a)
        },
        a => a
      }
    }
  );

  ($i:expr, $f:ident) => (
      dbg_dmp!($i, call!($f));
  );
);
/*
/// Convert a `Vec<Option<T>>` to `Option<Vec<T>>`
pub fn lift_option<I, T>(i: I) -> Option<Vec<T>>
where I: Iterator<Item=Option<T>>
{
    let mut collector = Vec::new();

    for el in i {
        match el {
            Some(el) => { collector.push(el); },
            None => { return None; }
        }
    }

    Some(collector)
}

/// Just injects a logging message
macro_rules! warn_none {
    ($e:expr, $msg:expr) => {{
        let out = $e;
        if let None = out {
            warn!($msg)
        }
        out
    }}
}

*/
