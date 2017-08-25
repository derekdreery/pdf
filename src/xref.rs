use std::fmt;
use pdf_par_ser::xref::{XRefStatus, XRefHeader, XRefEntry as RawXRefEntry, Trailer };
use pdf_par_ser::primitive::Ref;
use pdf_par_ser::Parse;

use error::{Result, ErrorKind};

/// A cross-reference table
#[derive(Clone)]
pub struct XRef<'a> {
    /// Cross-reference header information
    pub header: XRefHeader,
    /// The table data itself
    pub table: &'a [u8],
    /// The trailer dictionary
    pub trailer: Trailer,
    /// The previous xref table, extracted from the trailer, if present
    prev: Option<Box<XRef<'a>>>
}

impl<'a> fmt::Debug for XRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("XRef")
            .field("header", &self.header)
            .field("table", &"[<binary data...>]")
            .field("trailer", &self.trailer)
            .field("prev", &self.prev)
            .finish()
    }
}

impl<'a> XRef<'a> {

    /// Parse a cross-reference table from a byte slice (raw data)
    pub fn from_raw(data: &'a [u8], offset: usize) -> Result<XRef<'a>> {
        let header = match XRefHeader::parse(&data[offset..]) {
            Ok(header) => header,
            _ => bail!("could not parse xref header")
        };

        let table_size = (header.num_entries * 20) as usize;
        let total_len = offset + table_size + header.byte_length;
        let trailer = Trailer::parse(&data[total_len..])?;

        let prev = if let Some(prev) = trailer.prev {
            Some(Box::new(XRef::from_raw(data, prev as usize)?))
        } else { None };

        Ok(XRef {
            header,
            table: &data[(offset + header.byte_length) .. table_size],
            trailer,
            prev
        })
    }

    /// Use the information in the header to fetch an entry in the xref table
    /// Table is the byte array of the xref table, not including the 2 header rows
    pub fn entry(&self, obj: u64) -> Result<XRefEntry> {
        if obj < self.header.start_obj {
            bail!(ErrorKind::XrefEntryTooSmall(obj, self.header.start_obj));
        }
        if (obj - self.header.start_obj) > self.header.num_entries {
            bail!(ErrorKind::XrefEntryTooBig(obj,
                                             self.header.num_entries + self.header.start_obj));
        }
        let offset = ((obj - self.header.start_obj) * 20) as usize;
        let RawXRefEntry { loc, gen, status } = RawXRefEntry::parse(&self.table[offset..])?;
        Ok(XRefEntry { obj, loc, gen, status })
    }

    /// Get the root catalog from this xref
    pub fn root(&self) -> Result<XRefEntry> {
        self.entry(self.trailer.root.obj)
    }

    /// Get the size of this xref table
    pub fn size(&self) -> u64 {
        self.trailer.size as u64
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

