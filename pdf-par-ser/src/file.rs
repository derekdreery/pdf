//! file-level parsers
use std::str;
use std::fmt;
use nom::{self, IResult};

use {util, Parse, ParseFrom};
use primitive::{Name};
use error::*;

/// The version of the pdf file
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct PdfVersion {
    pub major: u8,
    pub minor: u8
}

/// Parse the version string at the beginning of a pdf file
named!(#[doc = "Parse the version string at the beginning of a pdf file"],
       parse_version<PdfVersion>, do_parse!(
    tag!(b"%PDF-") >>
    major: flat_map!(call!(nom::digit), parse_to!(u8)) >>
    tag!(b".") >>
    minor: flat_map!(call!(nom::digit), parse_to!(u8)) >>
    call!(util::parse_line_ending) >>
    (PdfVersion { major, minor })
));


impl Parse for PdfVersion {
    fn parse(i: &[u8]) -> Result<(usize, PdfVersion)> {
        match parse_version(i) {
            IResult::Done(o, v) => Ok((i.len() - o.len(), v)),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
            IResult::Error(e) => bail!(Error::with_chain(e, ErrorKind::ParserError)),
        }
    }
}

/// Parse the version represented by a name in the catalog
named!(#[doc = "Parse the version represented by a name in the catalog"],
       parse_version_catalog<PdfVersion>, do_parse!(
    major: flat_map!(call!(nom::digit), parse_to!(u8)) >>
    tag!(b".") >>
    minor: flat_map!(call!(nom::digit), parse_to!(u8)) >>
    (PdfVersion { major, minor })
));

impl<'a> ParseFrom<&'a Name> for PdfVersion {
    fn parse_from(i: &'a Name) -> Result<PdfVersion> {
        match parse_version_catalog(&i[..]) {
            IResult::Done(_, v) => Ok(v),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
            IResult::Error(e) => bail!(Error::with_chain(e, ErrorKind::ParserError)),
        }
    }
}

/// A zero-sized struct representing the EOF token
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Eof;

/// Parse the special token at the end of the file
named!(#[doc = "Parse the special token at the end of the file"], parse_eof, tag!(b"%%EOF"));

impl Parse for Eof {
    fn parse(i: &[u8]) -> Result<(usize, Eof)> {
        match parse_eof(i) {
            IResult::Done(o, _) => Ok((i.len() - o.len(), Eof)),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
            IResult::Error(e) => bail!(Error::with_chain(e, ErrorKind::ParserError)),
        }
    }
}

/// The offset in a pdf file of the cross-reference table
pub struct XRefOffset(pub usize);

/// Parse the xref offset location (at end of file, used to seek to xref table)
named!(#[doc = "Parse the xref offset location"], parse_xref_offset<usize>, do_parse!(
    tag!(b"startxref") >>
    call!(util::parse_line_ending) >>
    num: flat_map!(call!(nom::digit), parse_to!(usize)) >>
    call!(util::parse_line_ending) >>
    (num)
));

impl Parse for XRefOffset {
    fn parse(i: &[u8]) -> Result<(usize, XRefOffset)> {
        match parse_xref_offset(i) {
            IResult::Done(o, offset) => Ok((i.len() - o.len(), XRefOffset(offset))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
            IResult::Error(e) => bail!(Error::with_chain(e, ErrorKind::ParserError)),
        }
    }
}

#[cfg(test)]
mod tests {
    use nom::IResult;
    use super::*;

    #[test]
    fn parse_version() {
        let out =  PdfVersion { major: 1, minor: 7 };
        assert_eq!(super::parse_version(b"%PDF-1.7\r "), IResult::Done(&b" "[..], out));
    }
}

