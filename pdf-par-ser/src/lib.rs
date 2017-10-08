//! # Pdf Par(ser) Ser(ializer)
//!
//! This library provides a selection of Types representing parts of a pdf document, and functions
//! that parse or serialize these types from/into a byte stream
//!
//! All the functions in this crate should run fast, and not require indirection. PDF libraries can
//! use these types and functions as building blocks on which to build their library of
//! application.
//!
//! There are 2 traits that are used for parsing: `Parse` and `TryFrom`. `Parse` is used when
//! not consuming the original input, and may require allocations, `TryFrom` consumes the input and
//! only allocates if necessary.
#[macro_use] extern crate error_chain;
#[macro_use] extern crate nom;
#[macro_use] extern crate log;
extern crate num;
extern crate bigdecimal;
extern crate itertools;

mod error;
#[macro_use] pub mod util;
pub mod primitive;
pub mod stream;
pub mod file;
pub mod xref;
pub mod document;
//pub mod linear;

use std::io;

/// Parse an object into a more structured object (defaults to parsing from a byte array)
///
/// TODO think about types, are there more flexible ways of doing this?
pub trait Parse where Self: Sized {
    /// Attempt to parse the object from the input. Return the remaining input on success
    fn parse(i: &[u8]) -> Result<(usize, Self)>;
}

/// Parse an object into a more structured object, consuming the input.
pub trait ParseFrom<Input>
    where Self: Sized
{
    /// Attempt to parse the object from the input
    fn parse_from(i: Input) -> Result<Self>;
}

/* when it's in std
impl<T, Input> TryFrom<Input, Error=Error> for T where T: ParseFrom<Input> {
    fn try_from(from: Input) -> Result<T> {
        T::parse_from(from)
    }
}
*/

/// This trait downcasts an enum of types into a specific type, and fails if the enum contains
/// another type. It has the same signature as ParseFrom, but is semantically different
pub trait Downcast<Input>
    where Self: Sized
{
    /// Casts the input down to a specific type, failing if it is a different type
    fn downcast(i: Input) -> Result<Self>;
}

impl<T, U> Downcast<Option<U>> for Option<T> where T: Downcast<U> {
    fn downcast(i: Option<U>) -> Result<Option<T>> {
        match i {
            Some(val) => Ok(Some(T::downcast(val)?)),
            None => Ok(None)
        }
    }
}

impl<T, Input> ParseFrom<Input> for T
    where T: Downcast<Input>
{
    fn parse_from(input: Input) -> Result<T> {
        T::downcast(input)
    }
}

/// Serialize some object into a less structured object
pub trait Serialize
{
    fn serialize<Output: io::Write>(self, o: Output) -> Result<()>;
}

//pub use nom::Needed;
pub use error::*;
