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

mod error;
mod primitive;
mod file;
mod xref;
mod document;
mod reader;

pub use file::PdfVersion;
pub use reader::PdfReader;
pub use pdf_par_ser::document::{PageLayout, PageMode};
pub use error::*;
