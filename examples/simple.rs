extern crate memmap;
extern crate pdf;
#[macro_use] extern crate error_chain;
//extern crate pretty_env_logger;
//#[macro_use] extern crate log;

use memmap::{Mmap, Protection};
use pdf::{PdfReader, Result};


fn run() -> Result<()> {
    //pretty_env_logger::init().unwrap();
    println!("Opening a memmap to file");
    let file = Mmap::open_path("pdf_reference_1-7.pdf", Protection::Read).unwrap();
    // The unsafety comes from concurrent acces, which we will not do.
    println!("Initializing pdf reader");
    let pdf_file = PdfReader::from_bytes(unsafe { file.as_slice() })?;
    println!("Pdf version: {}", pdf_file.version().unwrap());
    Ok(())
}

quick_main!(run);
