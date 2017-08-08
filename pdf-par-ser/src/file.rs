//! file-level parsers
use std::str;
use nom;

use util;
use primitive::dictionary;

#[derive(Debug, PartialEq, Eq)]
pub struct PdfVersion {
    pub major: u8,
    pub minor: u8
}

impl Into<(u8, u8)> for PdfVersion {
    fn into(self) -> (u8, u8) {
        (self.major, self.minor)
    }
}

/// Parse the version string at the beginning of a pdf file
named!(#[doc = "Parse the version string at the beginning of a pdf file"],
       pub parse_version<PdfVersion>, do_parse!(
    tag!(b"%PDF-") >>
    // cannot panic
    major: flat_map!(call!(nom::digit), parse_to!(u8)) >>
    tag!(b".") >>
    // cannot panic
    minor: map!(nom::digit, |bytes| str::from_utf8(bytes).unwrap().parse::<u8>().unwrap()) >>
    call!(util::parse_line_ending) >>
    (PdfVersion { major, minor })
));

/// Parse the special token at the end of the file
named!(#[doc = "Parse the special token at the end of the file"], pub parse_eof, tag!(b"%%EOF"));

/// Parse the xref offset location (at end of file, used to seek to xref table)
named!(#[doc = "Parse the xref offset location"], pub parse_xref_offset<usize>, do_parse!(
    tag!(b"startxref") >>
    call!(util::parse_line_ending) >>
    num: flat_map!(call!(nom::digit), parse_to!(usize)) >>
    call!(util::parse_line_ending) >>
    (num)
));

struct Trailer {
}

named!(parse_trailer<Trailer>, pdf_ws!(do_parse!(
    tag!(b"trailer") >>
    trailer: dictionary >>
    (Trailer {})
)));

#[cfg(test)]
mod tests {
    use nom::IResult;

    #[test]
    fn parse_version() {
        let out =  super::Version { major: 1, minor: 7 };
        assert_eq!(super::parse_version(b"%PDF-1.7\r "), IResult::Done(&b" "[..], out));
    }
}

