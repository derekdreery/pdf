use pdf_par_ser::document::{Catalog, PageTree};
use pdf_par_ser::primitive::{Primitive, Dictionary, Ref, Indirect};
use pdf_par_ser::file::PdfVersion;
use pdf_par_ser::ParseFrom;

use xref::XRef;
use error::{Result};

/*
#[derive(Debug, Clone)]
pub struct RootCatalog {
    inner: Catalog,
    pages: Option<PageTree>
}

impl RootCatalog {
    pub fn from_dict(d: Dictionary) -> Result<Self> {
        let inner = Catalog::parse_from(d)?;
        Ok(RootCatalog { inner, pages: None })
    }

    pub fn version(&self) -> Option<PdfVersion> {
        self.inner.version
    }

    pub fn pages<F: Fn(Ref) -> Result<Indirect>>(&self, resolver: F) -> Result<&PageTree> {
        if let &Some(ref p) = &self.pages {
            return Ok(p);
        }
        unimplemented!();
    }
}
*/

#[derive(Debug, Clone)]
pub struct Pages<'a> {
    inner: PageTree,
    data: &'a [u8],
}

impl<'a> Pages<'a> {
    /// Constructs a new Pages object from raw parts
    pub fn from_parts(inner: PageTree, data: &'a [u8]) -> Pages<'a> {
        Pages { inner, data }
    }

    /// The total number of pages in the document
    pub fn count(&self) -> u64 {
        self.inner.count as u64
    }
}
