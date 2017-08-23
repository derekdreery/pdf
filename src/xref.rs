use std::fmt;
use pdf_par_ser::xref::{XRefStatus, XRefHeader, XRefEntry as RawXRefEntry, Trailer as RawTrailer};
use pdf_par_ser::primitive::Ref;
use pdf_par_ser::Parse;

use error::{Result, ErrorKind};

/// A cross-reference table
pub struct XRef<'a> {
    pub len: usize,
    pub header: XRefHeader,
    pub table: &'a [u8]
}

impl<'a> XRef<'a> {
    fn new(len: usize, header: XRefHeader, table: &'a [u8]) -> XRef<'a> {
        XRef { len, header, table }
    }

    /// Parse a cross-reference table from a byte slice (raw data)
    pub fn from_raw(data: &'a [u8]) -> Result<XRef<'a>> {
        match XRefHeader::parse(data) {
            Ok(header) => {
                let table_size = (header.num_entries * 20) as usize;
                let total_len = table_size + header.byte_length;
                Ok(XRef::new(total_len, header, &data[header.byte_length .. table_size]))
            }
            _ => bail!("could not parse xref header")
        }
    }

    /// Use the information in the header to fetch an entry in the xref table
    /// Table is the byte array of the xref table, not including the 2 header rows
    pub fn entry(&self, obj: u64) -> Result<XRefEntry> {
        if (obj - self.header.start_obj) > self.header.num_entries {
            bail!(ErrorKind::XrefEntryTooBig(obj,
                                             self.header.num_entries + self.header.start_obj));
        }
        let offset = ((obj - self.header.start_obj) * 20) as usize;
        let RawXRefEntry { loc, gen, status } = RawXRefEntry::parse(&self.table[offset..])?;
        Ok(XRefEntry { obj, loc, gen, status })
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

/// A structure at the end of a pdf file giving information on said file
#[derive(Clone, Eq, PartialEq)]
pub struct Trailer {
    inner: RawTrailer,
}

impl fmt::Debug for Trailer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}

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

// helper functions

