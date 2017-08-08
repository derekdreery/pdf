use std::str;
use pdf_par_ser::primitive::{dictionary as parse_dictionary};
use pdf_par_ser::file::{PdfVersion, parse_eof, parse_version, parse_xref_offset};
use pdf_par_ser::IResult;

/// Get the offset of the %%EOF token
pub fn get_eof_offset(buf: &[u8]) -> Option<usize> {

    let len = buf.len();
    // cannot use for loop (as we are going backwards)
    let mut i = len;
    loop {
        if i < len - 1024 {
            break;
        }
        match parse_eof(&buf[i..]) {
            IResult::Done(_, _) => { return Some(i); },
            _ => ()
        };
        i -= 1;
    }
    None
}

pub fn get_xref_offset(buf: &[u8], eof_offset: usize) -> Option<(usize, usize)> {
    use nom::IResult;

    // cannot use for loop (as we are going backwards)
    let mut i = eof_offset;
    loop {
        if i < eof_offset - 1024 {
            break;
        }
        match parse_xref_offset(&buf[i..]) {
            IResult::Done(_, offset) => { return Some((i, offset)); },
            _ => ()
        };
        i -= 1;
    }
    None
}

