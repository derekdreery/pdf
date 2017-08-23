use pdf_par_ser::primitive::{Primitive, Indirect};
use pdf_par_ser::{Result as ParSerResult, Error as ParSerError, Downcast, Parse};

use {Result, Error};

pub fn parse_object_as<T>(i: &[u8]) -> Result<T>
    where T: Downcast<Primitive>
{
    // use type annotations to save my sanity
    let primitive = Primitive::parse(i)?;
    let indirect = Indirect::downcast(primitive).map_err(|e: ParSerError| Error::from(e))?;
    T::downcast(*indirect.inner).map_err(|e: ParSerError| e.into())
}
