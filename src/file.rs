use pdf_par_ser::file::{Eof, XRefOffset, PdfVersion as RawPdfVersion};
use pdf_par_ser::Parse;
use std::fmt;
use {Result, ErrorKind};

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct PdfVersion {
    pub(crate) inner: RawPdfVersion
}

// TODO should this just derive?
impl fmt::Debug for PdfVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PdfVersion")
            .field("major", &self.inner.major)
            .field("minor", &self.inner.minor)
            .finish()
    }
}

impl Into<(u8, u8)> for PdfVersion {
    fn into(self) -> (u8, u8) {
        (self.inner.major, self.inner.minor)
    }
}

impl From<(u8, u8)> for PdfVersion {
    fn from((major, minor): (u8, u8)) -> Self {
        PdfVersion { inner: RawPdfVersion { major, minor }}
    }
}

impl fmt::Display for PdfVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.inner.major, self.inner.minor)
    }
}

/// Get the offset of the %%EOF token
pub fn get_eof_offset(buf: &[u8]) -> Result<usize> {

    let len = buf.len();
    // cannot use for loop (as we are going backwards)
    let mut i = len;
    loop {
        if i < len - 1024 {
            break;
        }
        match Eof::parse(&buf[i..]) {
            Ok((_, Eof)) => { return Ok(i); },
            _ => ()
        };
        i -= 1;
    }
    bail!(ErrorKind::NoEof);
}

pub fn get_xref_offset(buf: &[u8], eof_offset: usize) -> Result<usize> {
    use nom::IResult;

    // cannot use for loop (as we are going backwards)
    let mut i = eof_offset;
    loop {
        if i < eof_offset - 1024 {
            break;
        }
        match XRefOffset::parse(&buf[i..]) {
            Ok((_, XRefOffset(offset))) => { return Ok(offset); },
            _ => ()
        };
        i -= 1;
    }
    bail!(ErrorKind::NoXRef);
}
