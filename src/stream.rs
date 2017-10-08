use std::borrow::Cow;
use std::ops::Deref;
use {Result};
use pdf_par_ser::primitive::{Stream as RawStream, Name, Dictionary, Ref, Primitive};
use pdf_par_ser::stream::{StreamParams as RawStreamParams, DirectStreamParams,
    IndirectStreamParams, FilterField, DecodeParams};

/// A stream object, containing the stream data
///
/// Lifetime is the lifetime of the underlying data. Call `into_owned`, if you want the stream to
/// outlive the data.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Stream<'a> {
    pub data: Cow<'a, [u8]>,
    pub other_params: Dictionary,
}

impl<'a> Deref for Stream<'a> {
    type Target = Cow<'a, [u8]>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'a> Stream<'a> {
    /// Take a raw stream and the raw data, and construct a stream.
    ///
    /// The resolver is an optional reference resolver to use if any params are indirect
    /// references.
    pub fn from_raw_direct(raw: RawStream, input: &'a [u8]) -> Result<Stream<'a>> {
        let RawStream {
            stream_params,
            other_params,
            data_offset
        } = raw;
        match StreamParams::parse_from_direct(stream_params)? {
            StreamParams::Inline { length, filters, decoded_length } => {
                let length = length as usize;
                let raw_data = &input[data_offset .. data_offset + length];
                Ok(Stream {
                    data: raw_data.into(),
                    other_params
                })
            }
            _ => unimplemented!()
        }
    }

    pub fn from_raw<F>(raw: RawStream, input: &'a [u8], resolver: F) -> Result<Stream<'a>>
        where F: Fn(Ref) -> &'a [u8]
    {
        unimplemented!();
    }
}

/// Parsed stream parameters, ready for processing
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StreamParams {
    /// Stream is below the params
    Inline {
        /// Encoded length
        length: u64,
        /// List of filters to be applied, front first. First param is the name, second is the
        /// decode params
        filters: Vec<(Name, Dictionary)>,
        /// Decoded length (this is a hint only if present)
        decoded_length: Option<u64>,
    },
    /// Stream is in a separate file
    File {
        /// File specification
        file: (), // todo
        /// Encoded length
        length: u64,
        /// List of filters to be applied, front first. First param is the name, second is the
        /// decode params
        filters: Vec<(Name, Dictionary)>,
        /// Decoded length (this is a hint only if present)
        decoded_length: Option<u64>,
    }
}

/// Simple boilerplate macro
macro_rules! resolve_opt {
    ($id:ident, $resolve:expr) => {
        match $id {
            Some(t) => Some(t.resolve($resolve)?),
            None => None
        }
    }
}


impl StreamParams {
    pub fn parse_from_direct(params: RawStreamParams) -> Result<StreamParams> {
        // make sure we have direct params
        let params = match params {
            RawStreamParams::Direct(d) => d,
            RawStreamParams::Indirect(i) => bail!("Called parse_from_direct with indirect params"),
        };

        // destructure
        let DirectStreamParams {
            length,
            filter,
            decode_params,
            file,
            file_filter,
            file_decode_params,
            decoded_length,
        } = params;

        if let Some(file) = file {
            let filters = match (file_filter, file_decode_params) {
                (None, None) => vec![],
                (Some(FilterField::Name(name)), Some(DecodeParams::Dictionary(dict)))
                    => vec![(name, dict)],
                (Some(FilterField::Array(names)), Some(DecodeParams::Array(dicts))) => {
                    if names.len() != dicts.len() {
                        bail!("filer and decode_params fields do not match")
                    };
                    names.into_iter().zip(dicts.into_iter()).collect()
                },
                _ => bail!("filer and decode_params fields do not match")
            };
            Ok(StreamParams::File { file, length, filters, decoded_length })
        } else {
            let filters = match (filter, decode_params) {
                (None, None) => vec![],
                (Some(FilterField::Name(name)), Some(DecodeParams::Dictionary(dict)))
                    => vec![(name, dict)],
                (Some(FilterField::Array(names)), Some(DecodeParams::Array(dicts))) => {
                    if names.len() != dicts.len() {
                        bail!("filer and decode_params fields do not match")
                    };
                    names.into_iter().zip(dicts.into_iter()).collect()
                },
                _ => bail!("filer and decode_params fields do not match")
            };
            Ok(StreamParams::Inline { length, filters, decoded_length })
        }
    }

    pub fn parse_from<'a, F>(params: RawStreamParams, resolver: F) -> Result<StreamParams>
        where F: Fn(Ref) -> Option<&'a [u8]>
    {
        let params = match params {
            RawStreamParams::Direct(d)
                => { return StreamParams::parse_from_direct(RawStreamParams::Direct(d)); },
            RawStreamParams::Indirect(i) => i
        };

        let IndirectStreamParams {
            length,
            filter,
            decode_params,
            file,
            file_filter,
            file_decode_params,
            decoded_length,
        } = params;
        let length = length.resolve(&resolver)?;
        let filter = resolve_opt!(filter, &resolver);
        let decode_params = resolve_opt!(decode_params, &resolver);
        let file_filter = resolve_opt!(file_filter, &resolver);
        let file_decode_params = resolve_opt!(file_decode_params, &resolver);
        let decoded_length = resolve_opt!(decoded_length, &resolver);
        StreamParams::parse_from_direct(RawStreamParams::Direct(DirectStreamParams {
            length,
            filter,
            decode_params,
            file,
            file_filter,
            file_decode_params,
            decoded_length,
        }))
    }
}

