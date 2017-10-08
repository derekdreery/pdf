use pdf_par_ser::primitive::{Primitive, Indirect};
use pdf_par_ser::{Result as ParSerResult, Error as ParSerError, Downcast, Parse};

use {Result, Error};

pub fn parse_object_as<T>(i: &[u8]) -> Result<(usize, T)>
    where T: Downcast<Primitive>
{
    // use type annotations to save my sanity
    let (len, primitive) = Primitive::parse(i)?;
    let indirect = match Indirect::downcast(primitive) {
        Ok(i) => i,
        Err(e) => bail!(e)
    };
    match T::downcast(*indirect.inner) {
        Ok(t) => Ok((len, t)),
        Err(e) => bail!(e),
    }
}
