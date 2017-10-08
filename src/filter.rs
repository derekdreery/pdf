//! Filter types and functionality to filter streams

use {Result};
use pdf_par_ser::primitive::{Name, Dictionary};
use pdf_par_ser::util::is_whitespace;

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

impl Filter {
    fn parse_from(name: Name, params: Dictionary) -> Result<Filter> {
        match &name[..] {
            b"ASCIIHexDecode" => Ok(Filter::ASCIIHexDecode),
            b"ASCII85Decode" => Ok(Filter::ASCII85Decode),
            b"LZWDecode" => Ok(Filter::LZWDecode),
            b"FlateDecode" => Ok(Filter::FlateDecode),
            b"RunLengthDecode" => Ok(Filter::RunLengthDecode),
            b"CCITTFaxDecode" => Ok(Filter::CCITTFaxDecode),
            b"JBIG2Decode" => Ok(Filter::JBIG2Decode),
            b"DCTDecode" => Ok(Filter::DCTDecode),
            b"JPXDecode" => Ok(Filter::JPXDecode),
            b"Crypt" => Ok(Filter::Crypt),
            _ => bail!("Unknown filter type: {}", String::from_utf8_lossy(&name[..])),
        }
    }

    pub fn decode<I>(&self, input: I) -> FilterDecoder<I>
        where I: Iterator<Item=u8>
    {
        unimplemented!();
    }
}

pub struct FilterDecoder<I>
    where I: Iterator<Item=u8>
{
    input: I,
    filter: Filter,
}

pub struct ASCIIHexDecode<I>
    where I: Iterator<Item=u8>
{
    inner: I,
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

impl<I> Iterator for ASCIIHexDecode<I>
    where I: Iterator<Item=u8>
{
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        let ch1 = loop {
            match self.inner.next() {
                Some(ch) => match ch {
                    b'0' ... b'9' => break ch - b'0',
                    b'a' ... b'f' => break ch - b'a' + 10,
                    b'A' ... b'F' => break ch - b'A' + 10,
                    w if is_whitespace(w) => (),
                    b'>' => { return None },
                    _ => { return None; } // currently we don't handle errors
                },
                None => { return None; }
            };
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
