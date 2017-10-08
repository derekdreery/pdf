//! xref
use std::collections::HashMap;

use nom::{self, IResult};

use primitive::{Name, Ref, PdfString, Array, Primitive, Dictionary, Indirect, Stream, parse_dictionary};
use stream::{DecodeParams, DirectStreamParams};
use util::{xref_eol};
use {Parse, ParseFrom, Downcast, Result, ErrorKind, Error};

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
    fn parse(i: &[u8]) -> Result<(usize, XRefEntry)> {
        match parse_xref_entry(i) {
            IResult::Done(o, out) => Ok((i.len() - o.len(), out)),
            IResult::Error(e) => bail!(Error::with_chain(e, ErrorKind::ParserError)),
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
}

impl Parse for XRefHeader {
    fn parse(i: &[u8]) -> Result<(usize, XRefHeader)> {
        // We need the length of the header to know the xref offset
        match parse_xref_header(i) {
            IResult::Done(o, (start_obj, num_entries))
                => Ok((i.len() - o.len(), XRefHeader { start_obj, num_entries })),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
            IResult::Error(e) => bail!(Error::with_chain(e, ErrorKind::ParserError))
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
    pub root: Ref,
    pub encrypt: (), // todo
    pub info: Option<Ref>,
    pub id: Option<(PdfString, PdfString)>
}

/// Parses a trailer object.
// Use remove to avoid unnecessary copies
named!(parse_trailer<Result<HashMap<Vec<u8>, Primitive>>>, pdf_ws!(do_parse!(
    tag!(b"trailer") >>
    trailer_dict: parse_dictionary >>
    (trailer_dict)
)));

impl Trailer {
    /// Parse the trailer section of a cross reference table as a dictionary.
    ///
    /// There are some special tokens around the dictionary that need to be recognised.
    pub fn parse_trailer_dict(i: &[u8]) -> Result<(usize, Dictionary)> {
        match parse_trailer(i) {
            IResult::Done(o, Ok(dict)) => Ok((i.len() - o.len(), Dictionary(dict))),
            IResult::Done(_, Err(e)) => bail!(e),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n)),
            IResult::Error(e) => bail!(Error::with_chain(e, ErrorKind::ParserError))
        }
    }

    /// Consume a dictionary, merging its values with the trailer
    ///
    /// Just does size for now
    pub fn merge(&mut self, d: &mut Dictionary) -> Result<()> {
        if let Some(size) = d.remove(&b"Size"[..]) {
            self.size = <i64 as Downcast<Primitive>>::downcast(size)? as u64;
        }

        /*
        let root = trailer_dict.remove(&b"Root"[..])
            .ok_or(Error::from_kind(ErrorKind::MissingDictionaryField("Root".into(), "Trailer")))
            .and_then(Ref::downcast)?;

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


        let info = if let Some(info) = trailer_dict.remove(&b"Info"[..]) {
            Some(Ref::downcast(info)?)
        } else { None };
        */
        Ok(())
    }
}

impl ParseFrom<Dictionary> for Trailer {
    fn parse_from(mut trailer_dict: Dictionary) -> Result<Trailer> {

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

        /*
        debug!("trailer: parsing prev");
        let prev = if let Some(p) = trailer_dict.remove(&b"Prev"[..]) {
            Some(<i64 as Downcast<Primitive>>::downcast(p)? as u64)
        } else { None };

        debug!("trailer: parsing x_ref_stm");
        let x_ref_stm = if let Some(p) = trailer_dict.remove(&b"XRefStm"[..]) {
            Some(<i64 as Downcast<Primitive>>::downcast(p)? as u64)
        } else { None };
        */

        let info = if let Some(info) = trailer_dict.remove(&b"Info"[..]) {
            Some(Ref::downcast(info)?)
        } else { None };

        Ok(Trailer {
            size,
            root,
            encrypt: (), // todo
            info,
            id,
        })
    }
}

impl Parse for Trailer {
    fn parse(i: &[u8]) -> Result<(usize, Trailer)> {
        debug!("parsing trailer dictionary");
        let (len, dict) = Trailer::parse_trailer_dict(i)?;
        Ok((len, Trailer::parse_from(dict)?))
    }
}

/// Represents a subsection of the xref table
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SectionIndex {
    /// The first object number of this subsection
    pub first_obj: u64,
    /// The number of entries in this subsection
    pub num_entries: u64,
}

/// The width of the 3 columns in the xref stream
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StreamWidth {
    /// The width of the first column
    pub first: u8,
    /// The width of the second column
    pub second: u8,
    /// The width of the third column
    pub third: u8,
}

/// A set of stream params with extra entries for the contained xref table
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct XRefStreamHeader {
    /// The number one greater than the highest object number used in this section or in any
    /// section for which this is an update. It is equivalent to the Size entry in a trailer
    /// dictionary.
    pub size: u64,
    /// An array containing a pair of integers for each subsection in this section. The first
    /// integer is the first object number in the subsection; the second integer is the number
    /// of entries in the subsection
    ///
    /// The array is sorted in ascending order by object number. Subsections cannot overlap; an
    /// object number may have at most one entry in a section.
    ///
    /// Default value: [0 Size].
    pub index: Vec<SectionIndex>,
    /// The byte offset from the beginning of the file to the beginning of the previous
    /// cross-reference stream. This entry has the same function as the `Prev` entry in the
    /// trailer dictionary (Table 3.13).
    pub prev: Option<u64>,
    /// The width of the fields in this stream
    pub stream_width: StreamWidth,
    /// Any leftover params
    pub other_params: Dictionary,
}

impl ParseFrom<Dictionary> for XRefStreamHeader {
    fn parse_from(mut other_params: Dictionary) -> Result<XRefStreamHeader> {
        // Check this is an xref dict
        match other_params.remove(&b"Type"[..]) {
            Some(ty) => {
                let ty = Name::downcast(ty)?;
                if &ty != &b"XRef"[..] {
                    bail!(ErrorKind::IncorrectType(ty.into(), "XRef"));
                }
            },
            None => bail!(ErrorKind::MissingDictionaryField("Type".into(), "XRefStreamParams"))
        };

        let size = other_params.remove(&b"Size"[..]).ok_or(
            ErrorKind::MissingDictionaryField("Size".into(), "XRefStreamParams"))?;
        let size: u64 = Downcast::downcast(size)?;

        let index = match other_params.remove(&b"Index"[..]) {
            Some(i) => {
                let arr = Primitive::downcast_array_of::<u64>(i)?;
                ensure!(arr.len() % 2 == 0, "array must have even number of elements");
                let mut v = Vec::with_capacity(arr.len() / 2);
                let mut arr = arr.into_iter();
                while let Some(first_obj) = arr.next() {
                    v.push(SectionIndex {
                        first_obj,
                        // cannot fail
                        num_entries: arr.next().unwrap()
                    });
                }
                v
            },
            None => vec![SectionIndex { first_obj: 0, num_entries: size }]
        };

        let prev = if let Some(i) = other_params.remove(&b"Prev"[..]) {
            Some(<u64 as Downcast<Primitive>>::downcast(i)?)
        } else { None };

        let stream_width = match other_params.remove(&b"W"[..]) {
            Some(w) => w.downcast_array_of::<u64>()?,
            None => bail!(ErrorKind::MissingDictionaryField("Type".into(), "XRefStreamParams"))
        };
        ensure!(stream_width.len() == 3, "stream width must have 3 elements");
        // cannot fail (panic)
        let stream_width = StreamWidth {
            first: stream_width[0] as u8,
            second: stream_width[1] as u8,
            third: stream_width[2] as u8,
        };

        Ok(XRefStreamHeader {
            size,
            index,
            prev,
            stream_width,
            other_params,
        })
    }
}

/*
impl Parse for XRefStreamHeader {
    fn parse(i: &[u8]) -> Result<(usize, XRefStreamHeader)> {
        //println!("header:\n{}", String::from_utf8_lossy(&i[0.. ::std::cmp::min(i.len(), 3800)]));
        println!("parsing indirect");
        let (len, d) = Indirect::parse(i)?;
        println!("downcasting stream");
        Ok((len, d.unwrap_as::<Stream>()?))
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;
    use primitive::Ref;

    #[test]
    fn xref_line() {
        test_helper!(parse_xref_entry => [
            (b"0000000000 00000 n \n",
             IResult::Done(&b""[..], XRefEntry { loc: 0, gen: 0, status: XRefStatus::Used })),
            (b"0000000000 00010 n \r",
             IResult::Done(&b""[..], XRefEntry { loc: 0, gen: 10, status: XRefStatus::Used })),
            (b"0000000000 00000 n\r\n",
             IResult::Done(&b""[..], XRefEntry { loc: 0, gen: 0, status: XRefStatus::Used }))
        ]);
    }

    #[test]
    fn xref_header() {
        assert_eq!(XRefHeader::parse(&b"xref\n1000 5\n"[..]).ok(),
            Some((12, XRefHeader {
                start_obj: 1000,
                num_entries: 5,
            })));
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
        let expected = (input.len(), Trailer {
            size: 22,
            root: Ref { obj: 2, gen: 0 },
            encrypt: (), // todo
            info: Some(Ref { obj: 1, gen: 0 }),
            id: Some((
                PdfString(vec![129, 177, 74, 175, 163, 19, 219, 99, 219,
                     214, 249, 129, 228, 159, 148, 244]),
                PdfString(vec![129, 177, 74, 175, 163, 19, 219, 99, 219,
                     214, 249, 129, 228, 159, 148, 244])
             )),
        });
        //println!("{:?}", parse_trailer(&input[..]));
        assert_eq!(Trailer::parse(&input[..]).unwrap(), expected);
    }
}
