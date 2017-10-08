
use pdf_par_ser::primitive::{Dictionary, Primitive, Ref};
use pdf_par_ser::file::PdfVersion as RawPdfVersion;
use pdf_par_ser::document::{PageMode, PageLayout, Catalog, PageTree};
use pdf_par_ser::{Error as ParSerError, Downcast, Parse, ParseFrom};

use file::{get_eof_offset, get_xref_offset};
use document::Pages;
use xref::XRef;
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
    catalog: Catalog,
    pages: Pages<'a>,
}

impl<'a> PdfReader<'a> {
    /// Load a pdf read from a byte array
    ///
    /// Longer term I'd like to add the ability to pass any object that implements indexing, and
    /// implement a streaming loader that blocks when some data is requested that is not yet
    /// received.
    pub fn from_bytes(data: &'a [u8]) -> Result<PdfReader> {

        let version = RawPdfVersion::parse(data).ok().map(|v| v.1);
        //debug!("Version: {:?}", version);
        let eof_offset = get_eof_offset(&data)?;
        //debug!("'%%EOF' offset: {} - {:?}", eof_offset,
        //       String::from_utf8_lossy(&data[eof_offset..]));
        let xref_offset = get_xref_offset(&data, eof_offset)?;
        //debug!("{}:{}: 'xref' offset: {}", file!(), line!(), xref_offset);
        let xref = XRef::from_raw(data, xref_offset)?;
        //debug!("{}:{}: {:#?}", file!(), line!(), trailer);
        let (_, catalog) = object_as_at::<Dictionary>(data, xref.root()?.loc)?;
        let catalog = Catalog::parse_from(catalog)?;
        //println!();
        println!("{:#?}\n{:#?}", xref, catalog);
        let pages_entry = xref.entry(catalog.pages.obj)?;
        let (_, pages) = parse_object_as::<Dictionary>(&data[pages_entry.loc..])?;
        let pages = Pages::from_parts(
            PageTree::parse_from(pages)?,
            data,
        );

        let reader = PdfReader {
            data,
            version,
            xref,
            catalog,
            pages,
        };

        Ok(reader)
    }

    /// Get the version of the specification of pdf used to create this file.
    pub fn version(&self) -> Option<PdfVersion> {
        // catalog version overrides comment version
        self.catalog.version.or(self.version).map(|v| PdfVersion { inner: v })
    }

    pub fn page_count(&self) -> u64 {
        self.pages.count()
    }

    pub fn page(&self) -> Pages {
        unimplemented!();
    }

    pub fn page_labels(&self) {
        unimplemented!();
    }

    pub fn names(&self) {
        unimplemented!();
    }

    pub fn destinations(&self) {
        unimplemented!();
    }

    pub fn viewer_pref(&self) {
        unimplemented!();
    }


    /// Get the page layout for the document, if set
    pub fn page_layout(&self) -> Result<Option<PageLayout>> {
        if let Some(layout) = self.catalog.page_layout {
            match layout.resolve(|r| self.resolve(r)) {
                Ok(out) => Ok(Some(out)),
                Err(e) => bail!(e)
            }
        } else { Ok(None) }
    }


    /// Get the page mode for the document, if set
    pub fn page_mode(&self) -> Result<Option<PageMode>> {
        if let Some(mode) = self.catalog.page_mode {
            match mode.resolve(|r| self.resolve(r)) {
                Ok(out) => Ok(Some(out)),
                Err(e) => bail!(e)
            }
        } else { Ok(None) }
    }

/*
    pub outlines: Option<Ref>,
    pub threads: Option<Ref>,
    pub open_action: Option<MaybeRef<Primitive>>,
    pub additional_actions: Option<MaybeRef<Dictionary>>,
    pub uri: Option<MaybeRef<Dictionary>>,
    pub acro_form: Option<MaybeRef<Dictionary>>,
    pub metadata: Option<Ref>,
    pub structure_tree_root: Option<MaybeRef<Dictionary>>,
    pub mark_info: Option<MaybeRef<Dictionary>>,
    pub lang: Option<MaybeRef<PdfString>>,
    pub spider_info: Option<MaybeRef<Dictionary>>,
    pub output_intents: Option<MaybeRef<Array>>,
    pub piece_info: Option<MaybeRef<Dictionary>>,
    pub oc_properties: Option<MaybeRef<Dictionary>>,
    pub perms: Option<MaybeRef<Dictionary>>,
    pub legal: Option<MaybeRef<Dictionary>>,
    pub requirements: Option<MaybeRef<Array>>,
    pub collection: Option<MaybeRef<Dictionary>>,
    pub needs_rendering: Option<MaybeRef<Boolean>>,
*/

    /// Get an object at a given offset, returns None if object could not be found or parsed
    fn object_as_at<T>(&self, offset: usize) -> Result<(usize, Primitive)>
        where T: Downcast<Primitive>
    {
        object_as_at(&self.data[..], offset)
    }

    /// Function internally used to resolve positions using the xref table
    ///
    /// Uses option as we don't pass error information to the parser library (yet).
    fn resolve<'r>(&'r self, r: Ref) -> Option<&'r [u8]> {
        self.xref.entry(r.obj).map(|entry| &self.data[entry.loc..]).ok()
    }
}

fn object_as_at<T>(data: &[u8], offset: usize) -> Result<(usize, T)>
    where T: Downcast<Primitive>
{
    parse_object_as::<T>(&data[offset..])
}
