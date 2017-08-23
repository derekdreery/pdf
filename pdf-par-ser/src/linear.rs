//! Types and parsers for linearization

use bigdecimal::BigDecimal;

use primitive::dictionary;

/// The hint stream location in the linearization parameter dictionary
enum HintStreamLoc {
    /// Just specify the location and length of the main hint stream
    Main(u64, u64),
    /// Specify the location and length of the main hint stream, along with the location and length
    /// of the overflow stream.
    WithOverflow(u64, u64, u64, u64),
}

/// The dictionary of linearization parameters.
///
/// As the dictionary has a forced structure we represent it as a struct, so the hashmap can be
/// discarded.
///
/// The names match the names used in the document, they are not descriptive so see the doc
/// comments for semantic meaning
struct LinearizationParameterDictionary {
    /// The linearization version number
    pub linearized: BigDecimal,
    /// The length of the entire file in bytes
    pub l: u64,
    /// Location of hint stream
    pub h: HintStreamLoc,
    /// The object number of the first page's page object
    pub o: u64,
    /// The offset of the end of the first page, relative to the beginning of the file
    pub e: u64,
    /// The number of pages in the document
    pub n: u64,
    /// The offset of the whitespace character preceeding the first entry of the main xref table
    pub t: u64,
    /// The page number of the first page. Defaults to 0.
    pub p: Option<u64>,
}

macro_rules! try_iresult {
    ($e:expr) => {{
        match $e {
            Ok(val) => val,
            Err(e) => { return IResult::Error(ErrorKind::Custom(0)); }
        }
    }}
}
/*
/// Parses the initial linear dictionary
pub fn parse_linearized(input: &[u8]) -> IResult<&[u8], LinearizationParameterDictionary> {
    let params = try_parse!(input, dictionary);

}*/
