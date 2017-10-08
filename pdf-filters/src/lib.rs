//! Filter types and functionality to filter streams
#[macro_use] extern crate error_chain;
mod error;

use std::io::{Read, self};
pub use error::*;

/// Wrap an io error with invalid input and given error kind
macro_rules! io_err {
    ($kind:expr) => {
        ::std::io::Error::new(::std::io::ErrorKind::InvalidData, Error::from_kind($kind))
    }
}

/// A list of the possible filters, and logic for decoding
pub enum Filter {
    ASCIIHexDecode,
    ASCII85Decode,
    LZWDecode,
    FlateDecode,
    RunLengthDecode,
    CCITTFaxDecode,
    JBIG2Decode,
    DCTDecode,
    JPXDecode,
    Crypt
}

pub struct FilterDecoder<R>
    where R: io::Read
{
    input: R,
    filter: Filter,
}

pub struct ASCIIHexDecode<R>
    where R: io::Read
{
    /// The input for this filter
    reader: R,
    /// The internal buffer for operations
    buf: [u8; 1024],
    /// The start of the currently valid slice
    start: usize,
    /// The end of the currently valid slice
    end: usize,
}

impl<R> ASCIIHexDecode<R>
    where R: io::Read
{
    pub fn new(reader: R) -> ASCIIHexDecode<R> {
        ASCIIHexDecode {
            reader,
            buf: unsafe { mem::uninitialized() },
            start: 0,
            end: 0,
        }
    }
}

impl<R> Read for ASCIIHexDecode<R>
    where R: Read
{
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        // Track where we are in the output buffer
        let mut written = 0;

        // copy any data from previous decode to beginning of buffer
        let len = self.end - self.start;
        if len > 0 {
        }
        let raw_len = self.reader.read(&mut self.buf)?;

        let mut idx = 0;
        let mut val1 = None;
        val1 = while idx < raw_len {
            idx += 1;
            match self.buf[idx - 1] {
                b'0' ... b'9' => break Ok(ch - b'0'),
                b'a' ... b'f' => break Ok(ch - b'a' + 10),
                b'A' ... b'F' => break Ok(ch - b'A' + 10),
                w if is_whitespace(w) => (),
                b'>' => break None,
                _ => { return io_err!(ErrorKind::InvalidData); },
            }
        };

        let val1 = match val1 {
            Some(ch) => ch,
            None => { return Ok( }
        };

        let ch2 = loop {
            match self.inner.next() {
                Some(ch) => match ch {
                    b'0' ... b'9' => break ch - b'0',
                    b'a' ... b'f' => break ch - b'a' + 10,
                    b'A' ... b'F' => break ch - b'A' + 10,
                    w if is_whitespace(w) => (),
                    b'>' => { break 0 },
                    _ => { return None; } // currently we don't handle errors
                },
                None => { break 0 }
            };
        };

        Some(ch1 << 4 + ch2)
    }
}

impl<I> Iterator for ASCII85Decode<I>
    where I: Iterator<Item=u8>
{
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        unimplemented!();
    }
}

impl<I> Iterator for LZWDecode<I>
    where I: Iterator<Item=u8>
{
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        unimplemented!();
    }
}

impl<I> Iterator for FlateDecode<I>
    where I: Iterator<Item=u8>
{
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        unimplemented!();
    }
}

pub struct ASCII85Decode<I>
    where I: Iterator<Item=u8>
{
    inner: I,
}

pub struct LZWDecode<I>
    where I: Iterator<Item=u8>
{
    inner: I,
}

pub struct FlateDecode<I>
    where I: Iterator<Item=u8>
{
    inner: I,
}

pub struct RunLengthDecode;
pub struct CCITTFaxDecode;
pub struct JBIG2Decode;
pub struct DCTDecode;
pub struct JPXDecode;
pub struct Crypt;

