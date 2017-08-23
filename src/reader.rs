
use pdf_par_ser::primitive::{Dictionary, Primitive};
use pdf_par_ser::file::PdfVersion as RawPdfVersion;
use pdf_par_ser::{Error as ParSerError, Downcast, Parse};

use file::{get_eof_offset, get_xref_offset};
use xref::{XRef, Trailer};
use document::RootCatalog;
use primitive::parse_object_as;
use file::PdfVersion;
use error::Result;


/// The main pdf struct
///
/// This wraps a slice, containing the raw data. `memmap` is recommended to avoid having to load a
/// full pdf document into memory. The parts of the document will be read as needed.
pub struct PdfReader<'a> {
    version: Option<RawPdfVersion>,
    data: &'a [u8],
    xref: XRef<'a>,
    trailer: Trailer,
    catalog: RootCatalog,
}

impl<'a> PdfReader<'a> {
    /// Load a pdf read from a byte array
    ///
    /// Longer term I'd like to add the ability to pass any object that implements indexing, and
    /// implement a streaming loader that blocks when some data is requested that is not yet
    /// received.
    pub fn from_bytes(data: &'a [u8]) -> Result<PdfReader> {

        let version = RawPdfVersion::parse(data).ok();
        //debug!("Version: {:?}", version);
        let eof_offset = get_eof_offset(&data)?;
        //debug!("'%%EOF' offset: {} - {:?}", eof_offset,
        //       String::from_utf8_lossy(&data[eof_offset..]));
        let xref_offset = get_xref_offset(&data, eof_offset)?;
        //debug!("{}:{}: 'xref' offset: {}", file!(), line!(), xref_offset);
        let xref = XRef::from_raw(&data[xref_offset..])?;
        let trailer = Trailer::from_raw(&data[(xref_offset + xref.len)..])?;
        //debug!("{}:{}: {:#?}", file!(), line!(), trailer);
        let root_entry = xref.entry(trailer.root().obj)?;
        let catalog = object_as_at::<Dictionary>(data, root_entry.loc)?;
        let catalog = RootCatalog::from_dict(catalog)?;
        //println!();
        //println!("{:#?}", root);

        let reader = PdfReader {
            data,
            version,
            xref,
            trailer,
            catalog,
        };

        Ok(reader)
    }

    pub fn version(&self) -> Option<PdfVersion> {
        self.version.map(|v| PdfVersion { inner: v })
    }


    /// Get an object at a given offset, returns None if object could not be found or parsed
    fn object_as_at<T>(&self, offset: usize) -> Result<Primitive>
        where T: Downcast<Primitive>
    {
        object_as_at(&self.data[..], offset)
    }
}

fn object_as_at<T>(data: &[u8], offset: usize) -> Result<T>
    where T: Downcast<Primitive>
{
    parse_object_as::<T>(&data[offset..])
}
