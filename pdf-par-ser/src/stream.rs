use primitive::{Primitive, Dictionary, Name, Array, MaybeRef};
use {ParseFrom, Downcast, ErrorKind, Result};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
/// The filter or filters to use
pub enum Filter {
    /// A single filter
    Name(Name),
    /// Multiple filters
    Array(Vec<Name>),
}

impl Downcast<Primitive> for Filter {
    fn downcast(p: Primitive) -> Result<Filter> {
        match p {
            Primitive::Name(inner) => Ok(Filter::Name(inner)),
            Primitive::Array(inner) => Ok(Filter::Array(inner.downcast_of::<Name>()?)),
            _ => bail!(ErrorKind::UnexpectedToken(format!("{:?}", p), stringify!(Filter)))
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
/// Decode params for the filter or filters
pub enum DecodeParams {
    /// A single filter
    Dictionary(Dictionary),
    /// Multiple filters
    Array(Vec<Dictionary>),
}

impl Downcast<Primitive> for DecodeParams {
    fn downcast(p: Primitive) -> Result<DecodeParams> {
        match p {
            Primitive::Dictionary(inner) => Ok(DecodeParams::Dictionary(inner)),
            Primitive::Array(inner) => Ok(DecodeParams::Array(inner.downcast_of::<Dictionary>()?)),
            _ => bail!(ErrorKind::UnexpectedToken(format!("{:?}", p), stringify!(Filter)))
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StreamParams {
    /// The length of the stream
    length: MaybeRef<i64>,
    /// The filter or filters to use
    filter: Option<MaybeRef<Filter>>,
    /// Decode params for the filter or filters
    decode_params: Option<MaybeRef<DecodeParams>>,
    /// The file containing the stream data
    file: Option<()>, // todo
    /// like filter, but stream is remote file
    file_filter: Option<MaybeRef<Filter>>,
    /// like decode_params, but stream is remote file
    file_decode_params: Option<MaybeRef<DecodeParams>>,
    /// the number of bytes of the decoded stream, after filters are applied
    decompressed_length: Option<MaybeRef<i64>>,
    /// the remaining dictionary entries (if any)
    other_params: Dictionary,
}

impl ParseFrom<Dictionary> for StreamParams {
    fn parse_from(d: Dictionary) -> Result<StreamParams> {
        let mut d = d.0;
        let length = match d.remove(&b"Length"[..]) {
            Some(l) => <MaybeRef<i64> as Downcast<Primitive>>::downcast(l)?,
            None => bail!(ErrorKind::MissingDictionaryField("Length".into(), "StreamParams")),
        };

        let filter = if let Some(p) = d.remove(&b"Filter"[..]) {
            Some(<MaybeRef<Filter> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        // note the spelling of parms
        let decode_params = if let Some(p) = d.remove(&b"DecodeParms"[..]) {
            Some(<MaybeRef<DecodeParams> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        let file = None; // todo

        let file_filter = if let Some(p) = d.remove(&b"FFilter"[..]) {
            Some(<MaybeRef<Filter> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        // note the spelling of parms
        let file_decode_params = if let Some(p) = d.remove(&b"FDecodeParms"[..]) {
            Some(<MaybeRef<DecodeParams> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        let decompressed_length = if let Some(p) = d.remove(&b"DL"[..]) {
            Some(<MaybeRef<i64> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        Ok(StreamParams {
            length,
            filter,
            decode_params,
            file,
            file_filter,
            file_decode_params,
            decompressed_length,
            other_params: Dictionary(d),
        })
    }
}
