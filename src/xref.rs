use std::fmt;
use std::borrow::Cow;
use pdf_par_ser::xref::{XRefStatus, XRefHeader, XRefStreamHeader, XRefEntry as RawXRefEntry,
    Trailer, StreamWidth as XRefStreamWidth, SectionIndex as XRefSectionIndex };
use pdf_par_ser::primitive::{Ref, Dictionary, Primitive, Stream as RawStream};
use pdf_par_ser::{Parse, ParseFrom, Downcast, ErrorKind as ParSerKind};

use stream::Stream;
use error::{Result, ErrorKind};

/// A cross-reference table
#[derive(Debug, Clone)]
pub struct XRef<'a> {
    /// The table data
    pub tables: Vec<XRefTable<'a>>,
    /// The trailer dictionary
    pub trailer: Trailer,
}

impl<'a> XRef<'a> {

    /// Parse a cross-reference table from a byte slice (raw data)
    ///
    /// The function walks all cross-reference tables and constructs the table list and trailer
    pub fn from_raw(data: &'a [u8], offset: usize) -> Result<XRef<'a>> {

        let mut tables = Vec::new();
        let (table, mut trailer_dict) = XRefTable::from_raw(data, offset)?;
        tables.push(table);

        let mut prev: Option<u64> = Downcast::downcast(trailer_dict.remove(&b"Prev"[..]))?;
        let mut x_ref_stm: Option<u64> = Downcast::downcast(trailer_dict.remove(&b"XRefStm"[..]))?;
        let mut trailer = Trailer::parse_from(trailer_dict)?;
        //println!("{:?}", trailer);

        while prev.is_some() || x_ref_stm.is_some() {
            if let Some(x) = x_ref_stm {
                println!("Getting x_ref_stm");
                let (table, mut trailer_dict) = XRefTable::from_stream(data, x as usize)?;
                tables.push(table);
                //trailer.merge(&mut trailer_dict)?;
                x_ref_stm = Downcast::downcast(trailer_dict.remove(&b"Prev"[..]))?;

            } else if let Some(p) = prev {
                println!("Getting prev");
                // prev might be a stream or not
                if let Ok((table, mut trailer_dict)) = XRefTable::from_raw(data, p as usize) {
                    tables.push(table);
                    //trailer.merge(&mut trailer_dict)?;
                    prev = Downcast::downcast(trailer_dict.remove(&b"Prev"[..]))?;
                    x_ref_stm = Downcast::downcast(trailer_dict.remove(&b"XRefStm"[..]))?;
                } else {
                    let (table, mut trailer_dict) = XRefTable::from_stream(data, p as usize)?;
                    tables.push(table);
                    //trailer.merge(&mut trailer_dict)?;
                    x_ref_stm = Downcast::downcast(trailer_dict.remove(&b"Prev"[..]))?;
                }
            } else {
                unreachable!();
            };
        }

        Ok(XRef {
            tables,
            trailer
        })
    }

    /// Use the information in the header to fetch an entry in the xref table
    /// Table is the byte array of the xref table, not including the 2 header rows
    pub fn entry(&self, obj: u64) -> Result<XRefEntry> {
        if obj >= self.trailer.size {
            bail!(ErrorKind::XrefEntryNotPresent(obj));
        }
        for table in self.tables.iter() {
            if let Some(entry) = table.entry(obj)? {
                return Ok(entry);
            }
        }
        bail!(ErrorKind::XrefEntryNotPresent(obj));
    }

    /// Get the root catalog from this xref
    pub fn root(&self) -> Result<XRefEntry> {
        self.entry(self.trailer.root.obj)
    }

    /// Get the size of the xref table
    pub fn size(&self) -> u64 {
        self.trailer.size as u64
    }
}

/// Represents one of the cross-reference tables
///
/// We'll need some more fields to deal with streams
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum XRefTable<'a> {
    /// An old-style xref table not in an object stream
    Raw {
        /// The raw table
        data: &'a [u8],
        /// The first object number and size of the table
        index: XRefSectionIndex
    },
    /// An xref table embedded in an object stream
    Stream {
        /// The table data, after any filters have been applied
        data: Cow<'a, [u8]>,
        /// A list of the subsections' first object number and size
        subsections: Vec<XRefSectionIndex>,
        /// The width in bytes of the 3 columns. If 0, this means the column is missing and the
        /// default should be used
        widths: XRefStreamWidth,
    }
}

impl<'a> fmt::Debug for XRefTable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &XRefTable::Raw { index, .. } => f.debug_struct("XRefTable")
                .field("data", &"[<table data..>]")
                .field("index", &index)
                .finish(),
            &XRefTable::Stream { ref subsections, widths, .. } => f.debug_struct("XRefTable")
                .field("data", &"[<table data..>]")
                .field("subsections", &subsections)
                .field("widths", &widths)
                .finish()
        }
    }
}

impl<'a> XRefTable<'a> {

    /// Parses some information into the cross-reference table, and the trailer dictionary
    fn from_raw(data: &'a [u8], offset: usize) -> Result<(XRefTable<'a>, Dictionary)> {
        let (header_len, header) = match XRefHeader::parse(&data[offset..]) {
            Ok(header) => header,
            _ => bail!("could not parse xref header")
        };
        let table_offset = offset + header_len;
        let table_size = (header.num_entries * 20) as usize;
        let trailer_offset = table_offset + table_size;
        let (_, trailer_dict) = Trailer::parse_trailer_dict(&data[trailer_offset..])?;
        Ok((XRefTable::Raw {
            data: &data[table_offset..trailer_offset],
            index: XRefSectionIndex {
                first_obj: header.start_obj,
                num_entries: header.num_entries,
            }
        }, trailer_dict))
    }

    /// Parses the cross-reference table and the trailer dictionary from an xref stream
    fn from_stream(data: &'a [u8], offset: usize) -> Result<(XRefTable<'a>, Dictionary)> {
        let raw_stream = Primitive::parse(&data[offset..])?.1;
        // for error reporting
        let raw_stream_type_str = raw_stream.type_str();
        let raw_stream = match raw_stream {
            Primitive::Indirect(indirect) => indirect.deref_as::<RawStream>()?,
            Primitive::Stream(stream) => stream,
            _ => bail!(ErrorKind::ParSer(
                ParSerKind::UnexpectedToken(raw_stream_type_str.into(), "Indirect or Stream")
            ))
        };

        let Stream { data, other_params } = Stream::from_raw_direct(raw_stream, &data[offset..])?;
        let xref_header = XRefStreamHeader::parse_from(other_params)?;
        println!("{:#?}", xref_header);
        /*
        let stream_data = stream_header.stream_params.decode(
            &data[offset+header_len..],
            |r| None)?;
        */
        unimplemented!();
    }

    /// Fetches an entry of this table
    fn entry(&self, obj: u64) -> Result<Option<XRefEntry>> {
        match self {
            &XRefTable::Raw { data, index } => {
                if obj < index.first_obj || (obj - index.first_obj) > (index.num_entries as u64) {
                    return Ok(None);
                }
                let offset = ((obj - index.first_obj) * 20) as usize;
                let (_, RawXRefEntry { loc, gen, status }) = RawXRefEntry::parse(&data[offset..])?;
                Ok(Some(XRefEntry { obj, loc, gen, status }))
            },
            &XRefTable::Stream { ref data, ref subsections, widths } => unimplemented!(),
        }
    }
}

/// An entry in the cross-reference table
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct XRefEntry {
    /// The object number of the record
    pub obj: u64,
    /// The generation number of the record
    pub gen: u16,
    /// The location of the object
    pub loc: usize,
    /// Whether the entry is in use or not
    pub status: XRefStatus,
}

impl XRefEntry {
    /// Creates an equivalent indirect reference object
    fn indirect_ref(&self) -> Ref {
        Ref { obj: self.obj, gen: self.gen }
    }
}

impl Into<Ref> for XRefEntry {
    fn into(self) -> Ref {
        Ref { obj: self.obj, gen: self.gen }
    }
}

/*
/// A structure at the end of a pdf file giving information on said file
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Trailer {
    inner: RawTrailer,
}

/*
impl fmt::Debug for Trailer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}
*/

impl Trailer {
    fn new(inner: RawTrailer) -> Trailer {
        Trailer { inner }
    }

    /// Parse a trailer from a raw data stream
    pub fn from_raw(data: &[u8]) -> Result<Trailer> {
        Ok(Trailer::new(RawTrailer::parse(data)?))
    }

    /// The size of the document in bytes
    pub fn size(&self) -> u64 {
        self.inner.size
    }

    /// The byte offset from the beginning of the file to the beginning of the previous
    /// cross-reference section.
    pub fn prev(&self) -> Option<u64> {
        self.inner.prev
    }

    /// The catalog for the pdf document
    /// TODO take data arg and resolve root
    pub fn root(&self) -> Ref {
        self.inner.root
    }

    /// The info dictionary for the document
    /// TODO take data arg and resolve
    pub fn info(&self) -> Option<Ref> {
        self.inner.info
    }

    /// An optional unique pair of strings for the document (constituting a "File Identifier")
    pub fn id(&self) -> Option<(&[u8], &[u8])> {
        self.inner.id.as_ref().map(|&(ref v1, ref v2)| (&v1[..], &v2[..]))
    }
}
*/

// helper functions

