//!
//! # Pdf Primitive
//!
//! This object is the basic building block of a pdf document. It contains semantic information
//! encoded in some way.
//!
//! Ref: spec chapter 3.1.1

use std::collections::HashMap;
use std::fmt;

use bigdecimal::BigDecimal;
use nom::{self, IResult, ErrorKind, Needed};
use itertools::Itertools;

use util::{arr3_to_oct, ascii_hex_to_u8, decode_number_sign, eat_pdf_ws, parse_pos_num,
    is_whitespace, is_normal_char};

/// A pdf primitive
#[derive(Clone, Eq, PartialEq)]
pub enum Primitive {
    /// Comments
    ///
    /// These have no semantic meaning, except for special circumstances (e.g. version)
    Comment(Vec<u8>),
    /// true or false
    Boolean(bool),
    /// Whole number
    ///
    /// pdf was designed to run on 32 bit systems and as such systems are normally limited to 32
    /// bit integers. Obviously larger numbers can be stored inside streams, and decoded by
    /// separate programs.
    Integer(i64),
    /// Possibly fractional number
    Real(BigDecimal),
    /// Byte array
    String(Vec<u8>),
    /// Byte array, same name means same primitive
    Name(Vec<u8>),
    /// List of heterogeneous primitives
    Array(Vec<Primitive>),
    /// Key-value map of heterogeneous primitives
    Dictionary(Dictionary),
    /// Arbitary data
    Stream {
        /// A stream starts with a dictionary of config information
        ///
        /// Note that `length` is an important required param that gives the length of the stream
        params: Dictionary,
        /// The offset of the start of the stream from the index of the primitive
        data_offset: usize
    },
    /// Nothing
    Null,
    /// Indirect reference to a primitive, this must be resolved using cross-reference tables
    Ref {
        /// The object ID
        obj: u64,
        /// The object generation number.
        ///
        /// This allows a document to have multiple versions of
        /// the same object, and add additional versions at the end without re-writing the
        /// whole document
        gen: u64,
    }
}

impl Primitive {
    /// Downcast this primitive into an comment
    pub fn as_comment(&self) -> Option<&Vec<u8>> {
        match self {
            &Primitive::Comment(ref c) => Some(c),
            _ => None
        }
    }
    /// Downcast this primitive into a boolean
    pub fn as_boolean(&self) -> Option<bool> {
        match self {
            &Primitive::Boolean(ref b) => Some(*b),
            _ => None
        }
    }
    /// Downcast this primitive into an int
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            &Primitive::Integer(ref i) => Some(*i),
            _ => None
        }
    }
    /// Downcast this primitive into a real
    pub fn as_real(&self) -> Option<&BigDecimal> {
        match self {
            &Primitive::Real(ref r) => Some(r),
            _ => None
        }
    }
    /// Downcast this primitive into a string
    pub fn as_string(&self) -> Option<&Vec<u8>> {
        match self {
            &Primitive::String(ref s) => Some(s),
            _ => None
        }
    }
    /// Downcast this primitive into a name
    pub fn as_name(&self) -> Option<&Vec<u8>> {
        match self {
            &Primitive::Name(ref s) => Some(s),
            _ => None
        }
    }
    /// Downcast this primitive into an array
    pub fn as_array(&self) -> Option<&Vec<Primitive>> {
        match self {
            &Primitive::Array(ref a) => Some(a),
            _ => None
        }
    }
    /// Downcast this primitive into a dictionary
    pub fn as_dictionary(&self) -> Option<&Dictionary> {
        match self {
            &Primitive::Dictionary(ref d) => Some(d),
            _ => None
        }
    }
    /// Downcast this primitive into a stream
    pub fn as_stream(&self) -> Option<(&Dictionary, usize)> {
        match self {
            &Primitive::Stream {ref params, data_offset} => Some((params, data_offset)),
            _ => None
        }
    }
    /// Whether the primitive is null
    pub fn is_null(&self) -> bool {
        match self {
            &Primitive::Null => true,
            _ => false
        }
    }
}

impl fmt::Debug for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Primitive::Comment(ref c) => write!(f, "Comment({:?})", String::from_utf8_lossy(&c)),
            &Primitive::Boolean(b) => write!(f, "Boolean({:?})", b),
            &Primitive::Integer(i) => write!(f, "Integer({:?})", i),
            &Primitive::Real(ref d) => write!(f, "Real({})", d),
            &Primitive::String(ref s) => write!(f, "String({})", String::from_utf8_lossy(&s)),
            &Primitive::Name(ref n) => write!(f, "Name({})", String::from_utf8_lossy(&n)),
            &Primitive::Array(ref objs) => write!(f, "Array({:?})", objs),
            &Primitive::Dictionary(ref map) => write!(f, "Map({:?})", map),
            &Primitive::Stream{ ref params, data_offset } =>
                f.debug_struct("")
                    .field("params", params)
                    .field("data_offset", &data_offset)
                    .finish(),
            &Primitive::Null => write!(f, "null"),
            &Primitive::Ref { obj, gen } =>
                f.debug_struct("")
                    .field("obj", &obj)
                    .field("gen", &gen)
                    .finish(),
        }
    }
}

/// Matches a pdf comment primitive
named!(#[doc = "Matches a pdf comment primitive"], pub comment<Vec<u8>>, do_parse!(
    tag!(b"%") >>
    data: take_till!(|ch| ch == b'\r' || ch == b'\n') >>
    alt!( tag!(b"\r") | tag!(b"\n") | tag!(b"\r\n")) >>
    (data.to_vec())
));


/// Matches a pdf boolean primitive
named!(#[doc = "Matches a pdf boolean primitive"], pub boolean<bool>, alt!(
    value!(true, tag!(b"true")) |
    value!(false, tag!(b"false"))
));

/// Helper to match + or -
named!(sign<bool>, alt!(
    value!(true, tag!(b"+")) |
    value!(false, tag!(b"-"))
));

/// Matches a pdf integer number primitive
named!(#[doc = "Matches a pdf integer number primitive"], pub integer<i64>, flat_map!(
    recognize!(do_parse!(
        opt!(sign) >>
        call!(nom::digit) >>
        ()
    )),
    parse_to!(i64)
));

/// Matches a pdf real number primitive
named!(#[doc = "Matches a pdf real number primitive"], pub real<BigDecimal>, map_opt!(
    recognize!(do_parse!(
        opt!(sign) >>
        opt!(nom::digit) >>
        tag!(b".") >>
        opt!(nom::digit) >>
        ()
    )),
    |num| BigDecimal::parse_bytes(num, 10)
));

/// Recursive function to match up parens
named!(string_inner, recognize!(fold_many0!(alt!(
    // we need to handle \( and \) as a special case
    recognize!(do_parse!(
        tag!(b"\\") >>
        take!(1) >>
        ()
    )) |
    delimited!(tag!(b"("), string_inner, tag!(b")")) |
    // we just recognise a string here, we don't parse it yet
    take_till1!(|ch| ch == b'(' || ch == b')' || ch == b'\\')
), (), |mut acc: (), ch| {})));

/// Helper function to convert string representation into byte array, taking care of escape
/// characters
fn string_primitive_to_vec(obj: &[u8]) -> Result<Vec<u8>, ()> {
    let mut unescaped = Vec::with_capacity(obj.len()); // todo capacity better strategy here?
    let mut obj_iter = obj.iter().peekable();

    // used for chars of form \324
    let mut literal_store = [0u8; 3];

    while let Some(&ch) = obj_iter.next() {
        if ch == b'\\' {
            let &next = obj_iter.next().ok_or(())?;
            match next {
                b'n' => unescaped.push(b'\n'),
                b'r' => unescaped.push(b'\r'),
                b't' => unescaped.push(b'\t'),
                b'b' => unescaped.push(b'\x08'),
                b'f' => unescaped.push(b'\x0C'),
                b'(' => unescaped.push(b'('),
                b')' => unescaped.push(b')'),
                b'\\' => unescaped.push(b'\\'),
                b'\n' => (), // ignore newline
                b'0' ... b'9' => {
                    literal_store = [0, 0, next];
                    for _ in 0..2 {
                        let peek = obj_iter.peek().map(|u| *u);
                        if let Some(&peek) = peek {
                            if peek < b'0' || peek > b'9' {
                                break;
                            }
                            literal_store[0] = literal_store[1];
                            literal_store[1] = literal_store[2];
                            literal_store[2] = *obj_iter.next().unwrap(); // cannot fail (Some(peek))
                        }
                    }
                    unescaped.push(arr3_to_oct(&literal_store))
                }
                _ => { return Err(()) }
            }
        } else {
            unescaped.push(ch);
        }
    }
    Ok(unescaped)
}

/// Takes a slice of hex digits (a-fA-F0-9) and decodes them
fn hex_digits_to_vec(digits: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(digits.len() / 2 + 1); // 2 chars per bytes
    let mut iter = digits.iter().filter(|ch| ! is_whitespace(**ch));

    while let Some((u, l)) = iter.next_tuple::<(_,_)>() {
        //println!("{}, {},  {}, {}", u, l, ascii_hex_to_u8(*u), ascii_hex_to_u8(*l));
        // impossible to panic
        out.push((ascii_hex_to_u8(u).unwrap() << 4) + ascii_hex_to_u8(l).unwrap());
    }
    // handle single last element
    if digits.len() % 2 != 0 {
        out.push(ascii_hex_to_u8(digits.last().unwrap()).unwrap() << 4); // impossible to panic
    }
    out
}

/// Matches a pdf string primitive
named!(#[doc = "Matches a pdf string primitive"], pub string<Vec<u8>>, alt!(map_res!(
    delimited!(tag!(b"("), string_inner, tag!(b")")),
    string_primitive_to_vec
) | map!(
    delimited!(tag!(b"<"), call!(nom::hex_digit), tag!(b">")),
    hex_digits_to_vec
)));

/// Matches a pdf name primitive
named!(#[doc = "Matches a pdf name primitive"], pub name<Vec<u8>>, do_parse!(
    tag!(b"/") >>
    out: recognize!(fold_many0!(verify!(nom::be_u8, is_normal_char), (), |_, _| {})) >>
    (decode_number_sign(out))
));

/// Matches a pdf array primitive
named!(#[doc = "Matches a pdf array primitive"], pub array<Vec<Primitive>>,
       delimited!(tag!(b"["), pdf_ws!(many0!(primitive)), tag!(b"]")));

type Dictionary = HashMap<Vec<u8>, Primitive>;

/// Matches a pdf dictionary primitive
named!(#[doc = "Matches a pdf dictionary primitive"], pub dictionary<Dictionary>,
    delimited!(tag!(b"<<"), pdf_ws!(fold_many0!(
        pdf_ws!(tuple!(name, primitive)),
        HashMap::new(),
        |mut acc: Dictionary, (name, value)| { acc.insert(name, value); acc }
    )), tag!(b">>"))
);

/// Matches either a dictionary or a stream
///
/// It's efficient to detect a dictionary, then use it for a stream if it is a stream, or just
/// return a dictionary if not
pub fn dictionary_or_stream(input: &[u8]) -> IResult<&[u8], Primitive> {
    let (input, params) = try_parse!(input, dictionary);
    // Detect whether we are in a stream
    match do_parse!(input,
                    pre_ws: eat_pdf_ws >>
                    tag!(b"stream") >>
                    post_ws: alt!(tag!(b"\n") | tag!(b"\r\n")) >>
                    (pre_ws.len() + 5 + post_ws.len())
                    ) {
        IResult::Done(i, data_offset) => IResult::Done(i, Primitive::Stream {
            params,
            data_offset
        }),
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
        IResult::Error(_) => IResult::Done(input, Primitive::Dictionary(params))
    }
}

/// Matches the null primitive
named!(#[doc = "Matches the null primitive"], pub null<()>, map!(tag!(b"null"), |_| ()));

named!(#[doc = "Matches a ref primitive"], pub primitive_ref<Primitive>, pdf_ws!(do_parse!(
    obj: call!(parse_pos_num) >>
    gen: call!(parse_pos_num) >>
    tag!(b"R") >>
    (Primitive::Ref { obj, gen })
)));

/// Matches any pdf primitive
named!(#[doc = "Matches any pdf primitive"], pub primitive<Primitive>, alt!(
    map!(comment, Primitive::Comment) |
    map!(boolean, Primitive::Boolean) |
    map!(integer, Primitive::Integer) |
    map!(real, Primitive::Real) |
    map!(string, Primitive::String) |
    map!(name, Primitive::Name) |
    map!(array, Primitive::Array) |
    dictionary_or_stream |
    map!(null, |_| Primitive::Null) |
    primitive_ref
));

/// A primitive that is wrapped with some extra data to allow indirect references to it
pub struct IndirectPrimitive {
    /// The object number of the primitive
    obj: u64,
    /// The generation number of the primitive (see Primitive::Ref)
    gen: u64,
    /// The wrapped primitive value
    primitive: Primitive,
}

named!(pub indirect_primitive<IndirectPrimitive>, pdf_ws!(do_parse!(
    obj: flat_map!(call!(nom::digit), parse_to!(u64)) >>
    gen: flat_map!(call!(nom::digit), parse_to!(u64)) >>
    primitive: primitive >>
    (IndirectPrimitive { obj, gen, primitive })
)));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comment() {
        test_helper!(comment => [
            (b"%this is a comment\n",
                IResult::Done(&b""[..], b"this is a comment".to_vec()),
                "%this is a comment\\n")
        ]);
    }

    #[test]
    fn boolean() {
        test_helper!(boolean => [
            (b"true", IResult::Done(&b""[..], true)),
            (b"false", IResult::Done(&b""[..], false)),
            (b"other", test_error!()),
            (b"tr", IResult::Incomplete(Needed::Size(4))),
            (b"f", IResult::Incomplete(Needed::Size(5)))
        ]);
    }

    #[test]
    fn integer() {
        test_helper!(integer => [
            (b" 12.3", test_error!()),
            // this behaviour means real must be tested for first
            (b"12.3", IResult::Done(&b".3"[..], 12)),
            (b"+28", IResult::Done(&b""[..], 28)),
            (b"-4123", IResult::Done(&b""[..], -4123)),
            (b"--4123", test_error!())
        ]);
    }

    #[test]
    fn real() {
        test_helper!(real => [
            // Note the difficulty with ending a stream on a real (or integer), you cannot be sure you
            // are at the end of the number
            (b"12 ", test_error!()),
            (b"12. ", IResult::Done(&b" "[..], BigDecimal::parse_bytes(b"12.", 10).unwrap()))
        ]);
    }

    #[test]
    fn string() {
        test_helper!(string => [
            (b"(simple)", IResult::Done(&b""[..], b"simple".to_vec()), "simple"),
            (b"((double))", IResult::Done(&b""[..], b"(double)".to_vec()), "(double)"),
            // Notice that we cannot detect an incomplete string, because any char is valid inside a
            // string
            (b"((double_broken)", IResult::Incomplete(Needed::Size(17)), ""),
            (b"((double_broken) ", IResult::Incomplete(Needed::Size(18)), ""),
            (b"(\\(double_broken)",
                IResult::Done(&b""[..], b"(double_broken".to_vec()),
                "(double_broken"),
            (b"(escape\\n\\t\\217\\432)",
                IResult::Done(&b""[..], b"escape\n\t\x8f\x1a".to_vec()),
                "escape\\n\\t\\217\\432"),
            (b"(Line with a \n newline)",
                IResult::Done(&b""[..], b"Line with a \n newline".to_vec()),
                "Line with a \\n newline"),
            (b"(escaped \\\nnewline)",
                IResult::Done(&b""[..], b"escaped newline".to_vec()),
                "escaped newline"),
            (b"(\\0023)", IResult::Done(&b""[..], b"\x023".to_vec()), "\\0023"),
            (b"<0000>", IResult::Done(&b""[..], b"\0\0".to_vec()), "\\0\\0"),
            (b"<A23F>", IResult::Done(&b""[..], b"\xa2\x3f".to_vec()), "\\xa2\\x3f"),
            (b"<A23F3>", IResult::Done(&b""[..], b"\xa2\x3f\x30".to_vec()), "\\xa2\\x3f\\x30")
        ]);
    }

    #[test]
    fn name() {
        test_helper!(name => [
            (b"/name", IResult::Done(&b""[..], b"name".to_vec()), "name"),
            (b"/", IResult::Done(&b""[..], b"".to_vec()), ""),
            (b"/#6E#61#6D#65", IResult::Done(&b""[..], b"name".to_vec()), "#6E#61#6D#65"),
            (b"/#6E#61#D#65", IResult::Done(&b""[..], b"na#De".to_vec()), "#6E#61#D#65")

        ]);
    }

    #[test]
    fn array() {
        test_helper!(array => [
            (b"[/name 12 true]",
             IResult::Done(&b""[..], vec![
                Primitive::Name(b"name".to_vec()),
                Primitive::Integer(12),
                Primitive::Boolean(true)
             ]),
             "[/name 12 true]"),
            (b"[/name (str]ing) 12 true]",
             IResult::Done(&b""[..], vec![
                Primitive::Name(b"name".to_vec()),
                Primitive::String(b"str]ing".to_vec()),
                Primitive::Integer(12),
                Primitive::Boolean(true)
             ]),
             "[ /name (str]ing) 12 true ]")
        ]);
    }

    #[test]
    fn dictionary() {
        // needs more tests
        let mut control = HashMap::new();
        control.insert(b"simple".to_vec(), Primitive::Name(b"dictionary".to_vec()));
        control.insert(b"of".to_vec(), Primitive::Name(b"keys_values".to_vec()));
        test_helper!(dictionary => [
            (b"<< /simple /dictionary /of /keys_values >>",
                IResult::Done(&b""[..], control.clone()),
                "<< /simple /dictionary /of /keys_values >>"),
            (b"<</simple/dictionary/of/keys_values>>",
                IResult::Done(&b""[..], control),
                "<< /simple /dictionary /of /keys_values >>")
        ]);
    }

    #[test]
    fn stream() {
        let mut test_map = HashMap::new();
        test_map.insert(b"length".to_vec(), Primitive::Integer(4));
        let mut test_map2 = HashMap::new();
        test_map2.insert(b"test".to_vec(), Primitive::Integer(4));

        test_helper!(dictionary_or_stream => [
            (b"<</test 4>> true",
                IResult::Done(&b" true"[..], Primitive::Dictionary(test_map2)),
                "<</test 4>>"),

            (b"<</length 4>> \nstream\ntest\nendstream\n",
                IResult::Done(&b"test\nendstream\n"[..], Primitive::Stream {
                    params: test_map,
                    data_offset: 8
                }),
                "<</length 4>> \nstream\ntest\nendstream\n"
            )
        ]);
    }

    #[test]
    fn null() {
        test_helper!(null => [
            (b"null", IResult::Done(&b""[..], ()), "null")
        ]);
    }

    #[test]
    fn primitive_ref() {
        test_helper!(primitive_ref => [
            (b"200 0 R", IResult::Done(&b""[..], Primitive::Ref { obj: 200, gen: 0 }), "200 0 R")
        ]);
    }

}
