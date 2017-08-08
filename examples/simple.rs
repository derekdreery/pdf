extern crate memmap;
extern crate pdf;

use memmap::{Mmap, Protection};
use pdf::PdfReader;


fn main() {
    println!("Opening a memmap to file");
    let file = Mmap::open_path("pdf_reference_1-7.pdf", Protection::Read).unwrap();
    // The unsafety comes from concurrent acces, which we will not do.
    println!("Initializing pdf reader");
    let pdf_file = PdfReader::from_file(unsafe { file.as_slice() });
}
