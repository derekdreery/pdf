//! xref
use std::collections::HashMap;

use nom::{self, IResult};

use primitive::{Ref, PdfString, Primitive, parse_dictionary};
use util::{xref_eol};
use {Parse, Downcast, Result, ErrorKind, Error};

/// A cross reference entry can either be in use, or free
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum XRefStatus {
    /// An entry in use
    Used,
    /// An unused entry
    Free,
}

/// An entry in the cross-reference table
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct XRefEntry {
    /// The location of the object
    pub loc: usize,
    /// The generation number of the record
    pub gen: u16,
    /// Whether the entry is in use or not
    pub status: XRefStatus,
}

impl Parse for XRefEntry {
    /// parse an xref entry
    fn parse(i: &[u8]) -> Result<XRefEntry> {
        match parse_xref_entry(i) {
            IResult::Done(_, out) => Ok(out),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
        }
    }
}

named!(parse_xref_entry<XRefEntry>, do_parse!(
    loc: flat_map!(take!(10), parse_to!(usize)) >>
    tag!(b" ") >>
    gen: flat_map!(take!(5), parse_to!(u16)) >>
    tag!(b" ") >>
    status: alt!(value!(XRefStatus::Used, tag!(b"n")) | value!(XRefStatus::Free, tag!(b"f"))) >>
    xref_eol >>
    (XRefEntry { loc, gen, status })
));

/// Contains information that can be used to get random access to entries in the xref table
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct XRefHeader {
    /// The id of the first entry
    pub start_obj: u64,
    /// The total number of entries in the table
    pub num_entries: u64,
    /// The length of the header in bytes
    pub byte_length: usize,
}

impl Parse for XRefHeader {
    fn parse(i: &[u8]) -> Result<XRefHeader> {
        // We need the length of the header to know the xref offset
        match parse_xref_header(i) {
            IResult::Done(iend, (start_obj, num_entries)) => Ok(XRefHeader {
                start_obj, num_entries, byte_length: i.len() - iend.len()
            }),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e)))
        }
    }
}

named!(parse_xref_header<(u64, u64)>, pdf_ws!(do_parse!(
    tag!(b"xref") >>
    start_obj: flat_map!(call!(nom::digit), parse_to!(u64)) >>
    num_entries: flat_map!(call!(nom::digit), parse_to!(u64)) >>
    ((start_obj, num_entries))
)));

/// A structure at the end of a pdf file giving information on said file
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Trailer {
    pub size: u64,
    pub prev: Option<u64>,
    pub root: Ref,
    pub encrypt: (), // todo
    pub info: Option<Ref>,
    pub id: Option<(PdfString, PdfString)>
}

/// Parses a trailer object.
// Use remove to avoid unnecessary copies
named!(parse_trailer<HashMap<Vec<u8>, Primitive> >, pdf_ws!(do_parse!(
    tag!(b"trailer") >>
    trailer_dict: parse_dictionary >>
    (trailer_dict)
)));

impl Parse for Trailer {
    fn parse(i: &[u8]) -> Result<Trailer> {
        debug!("parsing trailer dictionary");
        let mut trailer_dict = match parse_trailer(i) {
            IResult::Done(_, dict) => dict,
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e)))
        };

        debug!("trailer: Parsing size");
        let size = trailer_dict.remove(&b"Size"[..])
            .ok_or(Error::from_kind(ErrorKind::MissingDictionaryField("Size".into(), "Trailer")))?;
        let size = <i64 as Downcast<Primitive>>::downcast(size)? as u64;

        debug!("trailer: Parsing root");
        let root = trailer_dict.remove(&b"Root"[..])
            .ok_or(Error::from_kind(ErrorKind::MissingDictionaryField("Root".into(), "Trailer")))
            .and_then(Ref::downcast)?;

        debug!("trailer: Parsing id");
        let id = if let Some(id) = trailer_dict.remove(&b"ID"[..]) {
            let id = id.downcast_array_of::<PdfString>()?;
            if id.len() != 2 {
                bail!("expected two strings, fond {}", id.len());
            }
            let mut id = id.into_iter();
            let id1 = id.next().unwrap(); // cannot fail
            let id2 = id.next().unwrap(); // cannot fail

            Some((id1, id2))
        } else { None };

        debug!("trailer: parsing prev");
        let prev = if let Some(p) = trailer_dict.remove(&b"Prev"[..]) {
            Some(<i64 as Downcast<Primitive>>::downcast(p)? as u64)
        } else { None };

        let info = if let Some(info) = trailer_dict.remove(&b"Info"[..]) {
            Some(Ref::downcast(info)?)
        } else { None };

        Ok(Trailer {
            size,
            prev,
            root,
            encrypt: (), // todo
            info,
            id,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;
    use primitive::Ref;

    #[test]
    fn xref_line() {
        test_helper!(parse_xref_entry => [
            (b"0000000000 00000 n \n",
             IResult::Done(&b""[..], XRefEntry { loc: 0, gen: 0, status: XRefStatus::Used }),
             "0000000000 00000 n \\n"),
            (b"0000000000 00010 n \r",
             IResult::Done(&b""[..], XRefEntry { loc: 0, gen: 10, status: XRefStatus::Used }),
             "0000000000 00010 n \\r"),
            (b"0000000000 00000 n\r\n",
             IResult::Done(&b""[..], XRefEntry { loc: 0, gen: 0, status: XRefStatus::Used }),
             "0000000000 00000 n\\r\\n")
        ]);
    }

    #[test]
    fn xref_header() {
        test_helper!(parse_xref_header => [
            (b"xref\n1000 5\n",
             IResult::Done(&b""[..], XRefHeader {
                 start_obj: 1000,
                 num_entries: 5,
             }),
             "xref\\n1000 5\\n")
        ]);
    }

    #[test]
    fn trailer() {
        let input = br"
trailer
<< /Size 22
/Root 2 0 R
/Info 1 0 R
/ID [ < 81b14aafa313db63dbd6f981e49f94f4 >
< 81b14aafa313db63dbd6f981e49f94f4 >
]
>>
";
        let expected = Trailer {
            size: 22,
            prev: None,
            root: Ref { obj: 2, gen: 0 },
            encrypt: (), // todo
            info: Some(Ref { obj: 1, gen: 0 }),
            id: Some((
                PdfString(vec![129, 177, 74, 175, 163, 19, 219, 99, 219,
                     214, 249, 129, 228, 159, 148, 244]),
                PdfString(vec![129, 177, 74, 175, 163, 19, 219, 99, 219,
                     214, 249, 129, 228, 159, 148, 244])
             )),
        };
        //println!("{:?}", parse_trailer(&input[..]));
        assert_eq!(Trailer::parse(&input[..]).unwrap(), expected);
    }
}
