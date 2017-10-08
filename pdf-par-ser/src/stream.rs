use primitive::{Primitive, Dictionary, Name, Array, MaybeRef};
use {ParseFrom, Downcast, Error, ErrorKind, Result};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
/// The filter or filters to use
pub enum FilterField {
    /// A single filter
    Name(Name),
    /// Multiple filters
    Array(Vec<Name>),
}

impl Downcast<Primitive> for FilterField {
    fn downcast(p: Primitive) -> Result<FilterField> {
        match p {
            Primitive::Name(inner) => Ok(FilterField::Name(inner)),
            Primitive::Array(inner) => Ok(FilterField::Array(inner.downcast_of::<Name>()?)),
            _ => bail!(ErrorKind::UnexpectedToken(format!("{:?}", p), stringify!(FilterField)))
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
            _ => bail!(ErrorKind::UnexpectedToken(format!("{:?}", p), stringify!(FilterField)))
        }
    }
}

/// The standard stream parameters.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IndirectStreamParams {
    /// The length of the stream
    pub length: MaybeRef<u64>,
    /// The filter or filters to use
    pub filter: Option<MaybeRef<FilterField>>,
    /// Decode params for the filter or filters
    pub decode_params: Option<MaybeRef<DecodeParams>>,
    /// The file containing the stream data
    pub file: Option<()>, // todo
    /// like filter, but stream is remote file
    pub file_filter: Option<MaybeRef<FilterField>>,
    /// like decode_params, but stream is remote file
    pub file_decode_params: Option<MaybeRef<DecodeParams>>,
    /// the number of bytes of the decoded stream, after filters are applied
    pub decoded_length: Option<MaybeRef<u64>>,
}

// Default doesn't make sense here, but is useful for testing
#[cfg(test)]
impl Default for IndirectStreamParams {
    fn default() -> Self {
        IndirectStreamParams {
            length: MaybeRef::Direct(0),
            filter: None,
            decode_params: None,
            file: None,
            file_filter: None,
            file_decode_params: None,
            decoded_length: None,
        }
    }
}

impl ParseFrom<Dictionary> for (IndirectStreamParams, Dictionary) {
    /// Parses the stream params, and returns any unused params as a dictionary
    fn parse_from(mut d: Dictionary) -> Result<(IndirectStreamParams, Dictionary)> {
        let length = match d.remove(&b"Length"[..]) {
            Some(l) => <MaybeRef<u64> as Downcast<Primitive>>::downcast(l)?,
            None => bail!(ErrorKind::MissingDictionaryField("Length".into(), "IndirectStreamParams")),
        };

        let filter = if let Some(p) = d.remove(&b"FilterField"[..]) {
            Some(<MaybeRef<FilterField> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        // note the spelling of parms
        let decode_params = if let Some(p) = d.remove(&b"DecodeParms"[..]) {
            Some(<MaybeRef<DecodeParams> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        let file = None; // todo

        let file_filter = if let Some(p) = d.remove(&b"FFilterField"[..]) {
            Some(<MaybeRef<FilterField> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        // note the spelling of parms
        let file_decode_params = if let Some(p) = d.remove(&b"FDecodeParms"[..]) {
            Some(<MaybeRef<DecodeParams> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        let decoded_length = if let Some(p) = d.remove(&b"DL"[..]) {
            Some(<MaybeRef<u64> as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        Ok((IndirectStreamParams {
            length,
            filter,
            decode_params,
            file,
            file_filter,
            file_decode_params,
            decoded_length,
        }, d))
    }
}

/// helper function to convert None into an error, should never really be hit in practice
#[inline]
fn ref_error<T>(i: Option<T>) -> Result<T> {
    i.ok_or(Error::from(ErrorKind::ReferenceInXRefDictionary))
}

/// Macro to avoid writing the same thing out many times
macro_rules! make_direct {
    ($id:ident) => {
        match $id {
            Some(t) => Some(ref_error(t.direct())?),
            None => None
        }
    }
}

/// Same as `StreamParams`, but all fields must be direct.
///
/// This is useful for xref streams, before we have built the cross-reference table.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DirectStreamParams {
    /// The length of the stream
    pub length: u64,
    /// The filter or filters to use
    pub filter: Option<FilterField>,
    /// Decode params for the filter or filters
    pub decode_params: Option<DecodeParams>,
    /// The file containing the stream data
    pub file: Option<()>, // todo
    /// like filter, but stream is remote file
    pub file_filter: Option<FilterField>,
    /// like decode_params, but stream is remote file
    pub file_decode_params: Option<DecodeParams>,
    /// the number of bytes of the decoded stream, after filters are applied
    pub decoded_length: Option<u64>,
}

// Default doesn't make sense here, but is useful for testing
#[cfg(test)]
impl Default for DirectStreamParams {
    fn default() -> Self {
        DirectStreamParams {
            length: 0,
            filter: None,
            decode_params: None,
            file: None,
            file_filter: None,
            file_decode_params: None,
            decoded_length: None,
        }
    }
}

impl ParseFrom<Dictionary> for (DirectStreamParams, Dictionary) {
    fn parse_from(mut d: Dictionary) -> Result<(DirectStreamParams, Dictionary)> {
        let length = match d.remove(&b"Length"[..]) {
            Some(l) => <u64 as Downcast<Primitive>>::downcast(l)?,
            None => bail!(ErrorKind::MissingDictionaryField("Length".into(), "StreamParams")),
        };

        let filter = if let Some(p) = d.remove(&b"FilterField"[..]) {
            Some(<FilterField as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        // note the spelling of parms
        let decode_params = if let Some(p) = d.remove(&b"DecodeParms"[..]) {
            Some(<DecodeParams as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        let file = None; // todo

        let file_filter = if let Some(p) = d.remove(&b"FFilterField"[..]) {
            Some(<FilterField as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        // note the spelling of parms
        let file_decode_params = if let Some(p) = d.remove(&b"FDecodeParms"[..]) {
            Some(<DecodeParams as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        let decoded_length = if let Some(p) = d.remove(&b"DL"[..]) {
            Some(<u64 as Downcast<Primitive>>::downcast(p)?)
        } else { None };

        Ok((DirectStreamParams {
            length,
            filter,
            decode_params,
            file,
            file_filter,
            file_decode_params,
            decoded_length,

        }, d))
    }
}

impl ParseFrom<IndirectStreamParams> for DirectStreamParams {
    fn parse_from(i: IndirectStreamParams) -> Result<DirectStreamParams> {
        let IndirectStreamParams {
            length,
            filter,
            decode_params,
            file,
            file_filter,
            file_decode_params,
            decoded_length,
        } = i;
        Ok(DirectStreamParams {
            length: ref_error(length.direct())?,
            filter: make_direct!(filter),
            decode_params: make_direct!(decode_params),
            file: None,
            file_filter: make_direct!(file_filter),
            file_decode_params: make_direct!(file_decode_params),
            decoded_length: make_direct!(decoded_length),
        })
    }
}

impl From<DirectStreamParams> for IndirectStreamParams {
    fn from(i: DirectStreamParams) -> IndirectStreamParams {
        let DirectStreamParams {
            length,
            filter,
            decode_params,
            file,
            file_filter,
            file_decode_params,
            decoded_length,
        } = i;
        IndirectStreamParams {
            length: MaybeRef::Direct(length),
            filter: filter.map(MaybeRef::Direct),
            decode_params: decode_params.map(MaybeRef::Direct),
            file: None,
            file_filter: file_filter.map(MaybeRef::Direct),
            file_decode_params: file_decode_params.map(MaybeRef::Direct),
            decoded_length: decoded_length.map(MaybeRef::Direct),
        }

    }
}

/// Parameters for a stream
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StreamParams {
    Direct(DirectStreamParams),
    Indirect(IndirectStreamParams),
}

impl StreamParams {
    /// Get the length of the stream, if it is direct
    pub fn direct_length(&self) -> Option<u64> {
        match self {
            &StreamParams::Direct(DirectStreamParams { length, .. }) => Some(length),
            &StreamParams::Indirect(IndirectStreamParams { length, .. }) => length.direct(),
        }
    }
}

// currently this uses a simple implementation, but could be made more efficient (no clone)
impl ParseFrom<Dictionary> for (StreamParams, Dictionary) {
    fn parse_from(i: Dictionary) -> Result<(StreamParams, Dictionary)> {
        match <(DirectStreamParams, Dictionary) as ParseFrom<Dictionary>>::parse_from(i.clone()) {
            Ok((dsp, other)) => Ok((StreamParams::Direct(dsp), other)),
            Err(_) => <(IndirectStreamParams, Dictionary) as ParseFrom<Dictionary>>::parse_from(i)
                .map(|(isp, other)| (StreamParams::Indirect(isp), other))
        }
    }
}

