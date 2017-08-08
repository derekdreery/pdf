/// # PDF parser/generator library
///
/// A general strategy in pdf is to skip anything you can't understand and carry on at the next
/// point you know something starts at.


#[macro_use] extern crate nom;
#[macro_use] extern crate error_chain;
#[macro_use] extern crate log;
extern crate memmap;
extern crate bigdecimal;
extern crate num;
extern crate itertools;
extern crate pdf_par_ser;

mod file;

use pdf_par_ser::file::PdfVersion;

/// The main pdf struct
///
/// This wraps a slice, containing the raw data. `memmap` is recommended to avoid having to load a
/// full pdf document into memory. The parts of the document will be read as needed.
pub struct PdfReader<'a> {
    version: Option<PdfVersion>,
    inner: &'a [u8]
}

impl<'a> PdfReader<'a> {
    pub fn from_file(file: &'a [u8]) -> PdfReader {
        use pdf_par_ser::file::parse_version;
        use file::{get_eof_offset, get_xref_offset};

        let version = parse_version(file).to_result().ok();
        println!("Version: {:?}", version);
        let eof_offset = get_eof_offset(&file).unwrap();
        println!("'%%EOF' offset: {} - {:?}", eof_offset,
                 String::from_utf8_lossy(&file[eof_offset..]));
        let (startxref_offset, xref_offset) = get_xref_offset(&file, eof_offset).unwrap();
        println!("'startxref' offset: {} - {:?}", startxref_offset,
                 String::from_utf8_lossy(&file[startxref_offset..]));
        println!("'xref' offset: {}", xref_offset);

        PdfReader {
            version: version,
            inner: file
        }
    }

}
