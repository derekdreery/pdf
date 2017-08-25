//! A module to provide types for the basic building blocks of pdf documents (named 'primitives'),
//! and parsers and serializers to/from byte streams.
//!
//! This object is the basic building block of a pdf document. It contains semantic information
//! encoded in some way.
//!
//! Ref: spec chapter 3.1.1

use std::collections::HashMap;
use std::fmt;

use bigdecimal::BigDecimal;
use nom::{self, IResult};
use itertools::Itertools;

use stream::StreamParams;
use util::{arr3_to_oct, oct_to_arr3, ascii_hex_to_u8, decode_number_sign, eat_pdf_ws,
    parse_pos_num, is_whitespace, is_normal_char, TryAsRef};
use {Parse, ParseFrom, Downcast, Error, ErrorKind, Result};

macro_rules! newtype_impl {
    ($name:path { $wrap:path }) => {
        impl ::std::ops::Deref for $name {
            type Target = $wrap;

            fn deref(&self) -> &$wrap {
                &self.0
            }
        }

        impl ::std::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut $wrap {
                &mut self.0
            }
        }

        impl ::std::convert::From<$wrap> for $name {
            fn from(f: $wrap) -> $name {
                $name(f)
            }
        }

        impl ::std::convert::Into<$wrap> for $name {
            fn into(self) -> $wrap {
                self.0
            }
        }

        impl ::std::convert::AsRef<$wrap> for $name {
            fn as_ref(&self) -> &$wrap {
                &self.0
            }
        }
    };
}

macro_rules! newtype {
    ($name:path { $wrap:path }) => {
        #[derive(Eq, PartialEq, Clone)]
        pub struct $name(pub $wrap);
        newtype_impl!($name { $wrap });
    };
    ($(#[$attr:meta])*, $name:ident { $wrap:path }) => {
        $(#[$attr])*
        #[derive(Eq, PartialEq, Clone)]
        pub struct $name(pub $wrap);
        newtype_impl!($name { $wrap });
    };
}

newtype!(#[doc = "A pdf comment (one of the basic objects, not strictly in spec but maybe useful)"]
         #[derive(Hash)],
         Comment { Vec<u8> });
newtype!(#[doc = "A pdf boolean (one of the basic objects)"]
         #[derive(Copy, Hash, Debug)],
         Boolean { bool });

/// A pdf numeric (one of the basic objects)
///
/// It can either be an exact integer, or a decimal number with fraction. We keep the two types
/// separate here.
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum Numeric {
    /// This numeric is an exact integer
    Integer(i64),
    /// This numeric is a fractional number
    Real(BigDecimal)
}

newtype!(#[doc = "A pdf string (one of the basic objects)"]
         #[derive(Hash)], PdfString { Vec<u8> });
newtype!(#[doc = "A pdf name (one of the basic objects)"]
         #[derive(Hash)], Name { Vec<u8> });
newtype!(#[doc = "A pdf array (one of the basic objects)"]
         #[derive(Debug)], Array { Vec<Primitive> });

impl Array {
    /// Cast to a vec of a given type, failing if any of the vec are the wrong type
    pub fn downcast_of<T>(self) -> Result<Vec<T>>
        where T: Downcast<Primitive>
    {
        let mut out: Vec<T> = Vec::with_capacity(self.len());
        for el in self.0.into_iter() {
            let el: T = T::downcast(el)?;
            out.push(el);
        }
        Ok(out)
    }
}

newtype!(#[doc = "A pdf dictionary (one of the basic objects)"]
         #[derive(Debug)], Dictionary { HashMap<Vec<u8>, Primitive> });

/// A pdf stream primitive (one of the basic objects)
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Stream {
    /// A stream starts with a dictionary of config information. This object does not resolve the
    /// stream contents, it simply records the start of the data
    ///
    /// Note that `length` is an important required param that gives the length of the stream
    pub params: StreamParams,
    /// The offset of the start of the stream from the index of the primitive
    pub data_offset: usize
}

/// A pdf null object (one of the basic objects)
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Null;

impl From<()> for Null {
    fn from(_: ()) -> Null {
        Null
    }
}
impl Into<()> for Null {
    fn into(self) -> () {
        ()
    }
}

/// A pdf indirect object (one of the basic objects)
///
/// This is a wrapper around one of the other basic objects, with the object and generation number
/// for the reference. An object reference must resolve to this type of object
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Indirect {
    /// The reference information (object number and generation number)
    pub reference: Ref,
    /// The enclosed primitive
    pub inner: Box<Primitive>,
}

impl Indirect {
    /// Given a type attempt to cast the boxed primitive to that type
    pub fn unwrap_as<T>(self) -> Result<T>
        where T: ParseFrom<Primitive>
    {
        T::parse_from(*self.inner)
    }
}

/// A reference to a primitive located elsewhere.
///
/// This is not considered a primitive object in the pdf spec, but it's helpful to think of it as
/// such. It is the responsibility of the consuming library to resolve the reference.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Ref {
    /// The object ID
    pub obj: u64,
    /// The object generation number.
    ///
    /// This allows a document to have multiple versions of
    /// the same object, and add additional versions at the end without re-writing the
    /// whole document
    pub gen: u16,
}

impl Ref {
    /// When supplied with a method to resolve a reference, will downcast the result into the
    /// expected type.
    ///
    /// This is a heler for higher level libraries
    pub fn resolve<'a, F, T>(self, resolver: F) -> Result<T>
        where F: Fn(Ref) -> Option<&'a [u8]>,
              T: ParseFrom<Primitive>
    {
        let data = resolver(self).ok_or(Error::from_kind(ErrorKind::UnresolvedReference(self)))?;
        let indirect = Indirect::parse(data)?;
        // check we have the right object
        debug_assert!(indirect.reference == self);
        indirect.unwrap_as::<T>()
    }
}


/// This macro implements some helpful coercions for types similar to [u8]
macro_rules! as_bytes {
    ($name:path) => {
        impl ::std::convert::AsRef<[u8]> for $name {
            fn as_ref(&self) -> &[u8] {
                self.0.as_ref()
            }
        }

        impl ::std::cmp::PartialEq<[u8]> for $name {
            fn eq(&self, other: &[u8]) -> bool {
                &self.0[..] == other
            }
        }

        impl<'a> ::std::cmp::PartialEq<&'a [u8]> for $name {
            fn eq(&self, other: &&[u8]) -> bool {
                &&self.0[..] == other
            }
        }

        impl Into<String> for $name {
            /// Performs the conversion.
            ///
            /// Note that this is a copying operation. It is intended as a debugging aid
            fn into(self) -> String {
                String::from_utf8_lossy(&self.0[..]).into_owned()
            }
        }

        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "{}", ::std::string::String::from_utf8_lossy(&self.0))
            }
        }

        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "{:?}", ::std::string::String::from_utf8_lossy(&self.0))
            }
        }
    };
}

as_bytes!(Comment);
as_bytes!(PdfString);
as_bytes!(Name);

/// A pdf primitive, that can be any of the primitive objects defined in the spec.
///
/// Note that `into_*` are not implemented for types that are `Copy`
///
/// Sadly it's not possible to use slices to reference the raw strings, as they are encoded
#[derive(Clone, Eq, PartialEq)]
pub enum Primitive {
    /// Comments
    ///
    /// These have no semantic meaning, except for special circumstances (e.g. version)
    Comment(Comment),
    /// true or false
    Boolean(Boolean),
    /// Whole number
    ///
    /// pdf was designed to run on 32 bit systems and as such systems are normally limited to 32
    /// bit integers. Obviously larger numbers can be stored inside streams, and decoded by
    /// separate programs.
    Numeric(Numeric),
    /// Byte array
    String(PdfString),
    /// Byte array, same name means same primitive
    Name(Name),
    /// List of heterogeneous primitives
    Array(Array),
    /// Key-value map of heterogeneous primitives
    Dictionary(Dictionary),
    /// Arbitary data
    Stream(Stream),
    /// Nothing
    ///
    /// This contains the type (), for symmetry
    Null(Null),
    /// A primitive wrapped in object and gereration numbers, the target of indirect references
    Indirect(Indirect),
    /// Indirect reference to a primitive, this must be resolved using cross-reference tables
    Ref(Ref)
}

/*
macro_rules! impl_TryAsRef {
    ($var:ident, $ty:ty) => {
        impl TryAsRef<$ty> for Primitive {
            type Error = Error;

            fn try_as_ref(&self) -> Result<&$ty> {
                match self {
                    &Primitive::$var(ref val) => Ok(val),
                    _ => bail!(ErrorKind::UnexpectedToken(
                            format!("{:?}", self), stringify!($ty)))
                }
            }
        }
    }
}

impl_TryAsRef!(Comment, Comment);
impl_TryAsRef!(Numeric, Numeric);
impl_TryAsRef!(String, PdfString);
*/

macro_rules! impl_Downcast {
    ($from:tt :: $var:ident => $type:ty) => {
        impl Downcast<$from> for $type {

            fn downcast(from: $from) -> Result<$type> {
                match from {
                    $from::$var(inner) => Ok(inner),
                    _ => bail!(ErrorKind::UnexpectedToken(
                            format!("{:?}", from), stringify!($type)))

                }
            }
        }
    };

    ($from:tt :: $var:ident) => {
        impl_Downcast!(Primitive::$var => $var);
    };
}

impl_Downcast!(Primitive::Comment);
impl_Downcast!(Primitive::Boolean);
impl_Downcast!(Primitive::Numeric);
impl_Downcast!(Primitive::String => PdfString);
impl_Downcast!(Primitive::Name);
impl_Downcast!(Primitive::Array);
impl_Downcast!(Primitive::Dictionary);
impl_Downcast!(Primitive::Stream);
impl_Downcast!(Primitive::Ref);
impl_Downcast!(Primitive::Null);
impl_Downcast!(Primitive::Indirect);

impl_Downcast!(Numeric::Integer => i64);
impl_Downcast!(Numeric::Real => BigDecimal);

impl Downcast<Primitive> for i64 {
    fn downcast(from: Primitive) -> Result<i64> {
        let num = Numeric::downcast(from)?;
        Downcast::downcast(num)
    }
}

impl Downcast<Primitive> for BigDecimal {
    fn downcast(from: Primitive) -> Result<BigDecimal> {
        let num = Numeric::downcast(from)?;
        Downcast::downcast(num)
    }
}

impl Downcast<Primitive> for Primitive {
    fn downcast(from: Primitive) -> Result<Primitive> {
        Ok(from)
    }
}

impl Primitive {
    pub fn downcast_array_of<T>(self) -> Result<Vec<T>>
        where T: Downcast<Primitive>
    {
        let arr = Array::downcast(self)?;
        let mut out: Vec<T> = Vec::with_capacity(arr.len());
        for el in arr.0.into_iter() {
            let el: T = T::downcast(el)?;
            out.push(el);
        }
        Ok(out)
    }
}

/// implement `From<$type>` for `$self` where `$self` is an enum with variant `$variant`
macro_rules! impl_From {
    ($self:ident, $variant:ident, $type:ty) => {
        impl From<$type> for $self {
            fn from(t: $type) -> $self {
                $self::$variant(t)
            }
        }
    }
}

impl_From!(Numeric, Integer, i64);
impl_From!(Numeric, Real, BigDecimal);

impl_From!(Primitive, Comment, Comment);
impl_From!(Primitive, Boolean, Boolean);
impl_From!(Primitive, Numeric, Numeric);
impl_From!(Primitive, String, PdfString);
impl_From!(Primitive, Name, Name);
impl_From!(Primitive, Array, Array);
impl_From!(Primitive, Dictionary, Dictionary);
impl_From!(Primitive, Stream, Stream);
impl_From!(Primitive, Null, Null);
impl_From!(Primitive, Indirect, Indirect);
impl_From!(Primitive, Ref, Ref);

impl fmt::Debug for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Primitive::")?;
        match self {
            &Primitive::Comment(ref c) => write!(f, "Comment({:?})", String::from_utf8_lossy(&c)),
            &Primitive::Boolean(b) => write!(f, "Boolean({:?})", b),
            &Primitive::Numeric(ref n) => write!(f, "Numeric({:?})", n),
            &Primitive::String(ref s) => write!(f, "String({})", s),
            &Primitive::Name(ref n) => write!(f, "Name({})", n),
            &Primitive::Array(ref objs) => write!(f, "Array({:?})", objs),
            &Primitive::Dictionary(ref map) => {
                use std::str;

                let mut s = f.debug_struct("Dictionary");
                for (key, val) in map.iter() {
                    match str::from_utf8(key).ok() {
                        Some(k) => { s.field(k, val); },
                        None => ()
                    };
                }
                s.finish()
            }
            &Primitive::Stream(ref stream) => write!(f, "Stream({:?})", stream),
            &Primitive::Null(_) => write!(f, "Null"),
            &Primitive::Indirect(ref indirect) => write!(f, "Indirect({:?})", indirect),
            &Primitive::Ref(primitive_ref) => write!(f, "Ref({:?})", primitive_ref),
        }
    }
}

// parsers

macro_rules! impl_Parse {
    ($wrapper:ident, $parser:ident) => {
        impl Parse for $wrapper {
            fn parse(i: &[u8]) -> Result<$wrapper> {
                match $parser(i) {
                    IResult::Done(_, c) => Ok($wrapper(c)),
                    IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
                    IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
                }
            }
        }
    }
}

/// Matches a pdf comment primitive
named!(#[doc = "Matches a pdf comment primitive"], parse_comment<Vec<u8>>, do_parse!(
    tag!(b"%") >>
    data: take_till!(|ch| ch == b'\r' || ch == b'\n') >>
    alt!( tag!(b"\r") | tag!(b"\n") | tag!(b"\r\n")) >>
    (data.to_vec())
));

impl_Parse!(Comment, parse_comment);

/// Matches a pdf boolean primitive
named!(#[doc = "Matches a pdf boolean primitive"], parse_boolean<bool>, alt!(
    value!(true, tag!(b"true")) |
    value!(false, tag!(b"false"))
));

impl_Parse!(Boolean, parse_boolean);

/// Helper to match + or -
named!(sign<bool>, alt!(
    value!(true, tag!(b"+")) |
    value!(false, tag!(b"-"))
));

/// Matches a pdf integer number primitive
named!(#[doc = "Matches a pdf integer number primitive"], parse_integer<i64>, flat_map!(
    recognize!(do_parse!(
        opt!(sign) >>
        call!(nom::digit) >>
        ()
    )),
    parse_to!(i64)
));

/// Matches a pdf real number primitive
named!(#[doc = "Matches a pdf real number primitive"], parse_real<BigDecimal>, map_opt!(
    recognize!(do_parse!(
        opt!(sign) >>
        opt!(nom::digit) >>
        tag!(b".") >>
        opt!(nom::digit) >>
        ()
    )),
    |num| BigDecimal::parse_bytes(num, 10)
));

named!(#[doc = "Matches either an integer or a real number"], parse_numeric<Numeric>, alt!(
    map!(call!(parse_real), Numeric::Real) |
    map!(call!(parse_integer), Numeric::Integer)
));

impl Parse for Numeric {
    fn parse(i: &[u8]) -> Result<Numeric> {
        match parse_numeric(i) {
            IResult::Done(_, c) => Ok(c), // this is why we can't use macro
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
        }
    }
}

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
), (), |acc: (), ch| {})));

/// Helper function to convert string representation into byte array, taking care of escape
/// characters
fn string_primitive_to_vec(obj: &[u8]) -> Option<Vec<u8>> {
    // todo capacity better strategy here?
    let mut unescaped = Vec::with_capacity(obj.len());
    let mut obj_iter = obj.iter().peekable();

    // used for chars of form \324
    let mut literal_store = [0u8; 3];

    while let Some(&ch) = obj_iter.next() {
        if ch == b'\\' {
            let &next = try_opt!(obj_iter.next());
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
                _ => { return None }
            }
        } else {
            unescaped.push(ch);
        }
    }
    Some(unescaped)
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
named!(#[doc = "Matches a pdf string primitive"], parse_string<Vec<u8>>, alt!(map_opt!(
    delimited!(tag!(b"("), string_inner, tag!(b")")),
    string_primitive_to_vec
) | map!(
    pdf_ws!(delimited!(tag!(b"<"), call!(nom::hex_digit), tag!(b">"))),
    hex_digits_to_vec
)));

impl_Parse!(PdfString, parse_string);

/// Matches a pdf name primitive
named!(#[doc = "Matches a pdf name primitive"], parse_name<Vec<u8>>, do_parse!(
    tag!(b"/") >>
    out: recognize!(fold_many0!(verify!(nom::be_u8, is_normal_char), (), |_, _| {})) >>
    (decode_number_sign(out))
));

impl_Parse!(Name, parse_name);

/// Matches a pdf array primitive
named!(#[doc = "Matches a pdf array primitive"], parse_array<Result<Vec<Primitive>>>,
       pdf_ws!(delimited!(tag!(b"["), pdf_ws!(fold_many0!(
            parse_primitive,
            Ok(Vec::new()),
            |mut acc: Result<Vec<_>>, item| match item {
                Ok(item) => {
                    match &mut acc {
                        &mut Ok(ref mut acc) => {
                            acc.push(item);
                        },
                        _ => (),
                    };
                    acc
                },
                Err(e) => Err(e)
            }
        )), tag!(b"]"))));

impl Parse for Array {
    fn parse(i: &[u8]) -> Result<Array> {
        match parse_array(i) {
            IResult::Done(_, Ok(a)) => Ok(Array(a)),
            IResult::Done(_, Err(e)) => bail!(e),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
        }
    }
}

/// Matches a pdf dictionary primitive
///
/// This actually supports nested streams, although that never happens in practice
named!(#[doc = "Matches a pdf dictionary primitive"],
       pub(crate) parse_dictionary<Result<HashMap<Vec<u8>, Primitive>>>,
    pdf_ws!(delimited!(tag!(b"<<"), pdf_ws!(fold_many0!(
        pdf_ws!(tuple!(parse_name, parse_primitive)),
        Ok(HashMap::new()),
        |mut acc: Result<HashMap<Vec<u8>, Primitive>>, (name, value): (Vec<u8>, _)|
            match value {
                Ok(prim) => {
                    match &mut acc {
                        &mut Ok(ref mut inner) => {
                            inner.insert(name.into(), prim);
                        },
                        _ => ()
                    };
                    acc
                },
                Err(e) => Err(e)
            }
    )), tag!(b">>")))
);

impl Parse for Dictionary {
    fn parse(i: &[u8]) -> Result<Dictionary> {
        match parse_dictionary(i) {
            IResult::Done(_, Ok(d)) => Ok(Dictionary(d)),
            IResult::Done(_, Err(e)) => bail!(e),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
        }
    }
}


/// Matches either a dictionary or a stream
///
/// It's efficient to detect a dictionary, then use it for a stream if it is a stream, or just
/// return a dictionary if not. We have to return a result of result, to aid error checking further
/// down
fn parse_dictionary_or_stream(input: &[u8]) -> IResult<&[u8], Result<Primitive>> {
    let (input_, params) = try_parse!(input, parse_dictionary);
    debug!("Detect whether we are in a stream");
    // TODO check end of stream consistency with length
    let (input_, data_offset) = match do_parse!(input_,
        eat_pdf_ws >>
        tag!(b"stream") >>
        alt!(tag!(b"\n") | tag!(b"\r\n")) >>
        ())
    {
        IResult::Done(i, _) => (i, input.len() - i.len()),
        IResult::Incomplete(needed) => { return IResult::Incomplete(needed); },
        IResult::Error(_) => {
            return IResult::Done(input_, params.map(|p| Primitive::Dictionary(Dictionary(p))))
        }
    };

    let out = params.map(Dictionary)
        .and_then(|params| StreamParams::parse_from(params))
        .map(|sparams|
            Primitive::Stream(Stream {
                params: sparams,
                data_offset,
            })
        );

    IResult::Done(input_, out)
}

impl Parse for Stream {
    fn parse(i: &[u8]) -> Result<Stream> {
        match parse_dictionary_or_stream(i) {
            IResult::Done(_, Ok(Primitive::Stream(s))) => Ok(s),
            IResult::Done(_, Ok(_))
                => bail!(ErrorKind::UnexpectedToken("Dictionary".into(), "Stream")),
            IResult::Done(_, Err(e)) => bail!(e),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
        }
    }
}

/// Matches the null primitive
named!(#[doc = "Matches the null primitive"], parse_null<()>, map!(pdf_ws!(tag!(b"null")), |_| ()));

impl Parse for Null {
    fn parse(i: &[u8]) -> Result<Null> {
        match parse_null(i) {
            IResult::Done(_, _) => Ok(Null), // this is why we can't use macro
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
        }
    }
}

named!(#[doc = "Matches a ref primitive"], parse_ref<Ref>, pdf_ws!(do_parse!(
    obj: flat_map!(call!(nom::digit), parse_to!(u64)) >>
    gen: flat_map!(call!(nom::digit), parse_to!(u16)) >>
    tag!(b"R") >>
    (Ref { obj, gen })
)));

impl Parse for Ref {
    fn parse(i: &[u8]) -> Result<Ref> {
        match parse_ref(i) {
            IResult::Done(_, r) => Ok(r),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
        }
    }
}


named!(parse_indirect<(Ref, Result<Primitive>)>, pdf_ws!(do_parse!(
    obj: flat_map!(call!(nom::digit), parse_to!(u64)) >>
    gen: flat_map!(call!(nom::digit), parse_to!(u16)) >>
    tag!(b"obj") >>
    inner: parse_primitive >>
    tag!(b"endobj") >>
    ((Ref { obj, gen }, inner))
)));

impl Parse for Indirect {
    fn parse(i: &[u8]) -> Result<Indirect> {
        match parse_indirect(i) {
            IResult::Done(_, (reference, Ok(inner))) => Ok(Indirect {
                reference,
                inner: Box::new(inner)
            }),
            IResult::Done(_, (_, Err(e))) => Err(e),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
        }
    }
}

/// Matches any pdf primitive
///
/// The order does not match the defined order as it is significant - more specific parsers must
/// come before less specific parsers, since the grammar is ambiguous.
named!(parse_primitive<Result<Primitive>>, alt!(
    // needs to be above integer
    map!(call!(parse_indirect), |(reference, inner)|
         inner.map(|inner| Primitive::Indirect(Indirect { reference, inner: Box::new(inner) } ))) |
    map!(call!(parse_null), |_| Ok(Primitive::Null(Null))) |
    map!(call!(parse_ref), |t| Ok(Primitive::Ref(t))) |
    map!(call!(parse_boolean), |t| Ok(Primitive::Boolean(Boolean(t)))) |
    map!(call!(parse_array), |t| t.map(|arr| Primitive::Array(Array(arr)))) |
    map!(call!(parse_name), |t| Ok(Primitive::Name(Name(t)))) |
    map!(call!(parse_string), |t| Ok(Primitive::String(PdfString(t)))) |
    parse_dictionary_or_stream |
    map!(call!(parse_comment), |t| Ok(Primitive::Comment(Comment(t)))) |
    map!(call!(parse_numeric), |t| Ok(Primitive::Numeric(t)))
));

impl Parse for Primitive {
    fn parse(i: &[u8]) -> Result<Primitive> {
        match parse_primitive(i) {
            IResult::Done(_, Ok(p)) => Ok(p),
            IResult::Done(_, Err(e)) => Err(e),
            IResult::Error(e) => bail!(ErrorKind::ParserError(format!("{:?}", e))),
            IResult::Incomplete(n) => bail!(ErrorKind::ParserIncomplete(n))
        }
    }
}

// Helper types without direct parsers

/// This is a helper type, when we want to express that we must have a specific type of primitive,
/// but that it may be behind a reference
///
/// > Note: although it is allowed, don't use `T=Ref` in a MaybeRef.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MaybeRef<T> where T: ParseFrom<Primitive> {
    /// Object is behind a reference
    Ref(Ref),
    /// Object is not behind a reference
    Direct(T),
}

impl<T> MaybeRef<T> where T: ParseFrom<Primitive> {
    /// When supplied with a method to resolve a reference, will downcast the result into the
    /// expected type.
    pub fn resolve<'a, F>(self, resolver: F) -> Result<T>
        where F: Fn(Ref) -> Option<&'a [u8]>
    {
        let r = match self {
            MaybeRef::Ref(r) => r,
            MaybeRef::Direct(t) => { return Ok(t); }
        };
        let data = resolver(r).ok_or(Error::from_kind(ErrorKind::UnresolvedReference(r)))?;
        let indirect = Indirect::parse(data)?;
        // check we have the right object
        debug_assert!(indirect.reference == r);
        indirect.unwrap_as::<T>()
    }
}

impl<T> Downcast<Primitive> for MaybeRef<T> where T: Downcast<Primitive> {
    fn downcast(p: Primitive) -> Result<MaybeRef<T>> {
        match &p {
            &Primitive::Ref(r) => { return Ok(MaybeRef::Ref(r)); },
            _ => ()
        };
        Ok(MaybeRef::Direct(T::downcast(p)?))
    }
}

/*
/// A pdf object: the basic building block of a pdf file
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Object<T> {
    /// An unreferenced primitive
    Direct(T),
    /// A primitive with a ref envelope, allowing the primitive to be referenced from elsewhere
    /// in the document
    Indirect(Indirect<T>),
}

// Lift ParseFrom above Object
impl<T> ParseFrom<Object<Primitive>> for Object<T>
    where T: ParseFrom<Primitive, Error=error::Error>,
{
    type Error = error::Error;

    fn parse_from(o: Object<Primitive>) -> error::Result<Object<T>> {
        Ok(match o {
            Object::Direct(p) => Object::Direct(ParseFrom::parse_from(p)?),
            Object::Indirect(Indirect { obj, gen, inner })
                => Object::Indirect(Indirect { obj, gen, inner: ParseFrom::parse_from(inner)? })
        })
    }
}

impl<T> Object<T> {
    pub fn into_inner(self) -> T {
        match self {
            Object::Direct(inner) => inner,
            Object::Indirect(Indirect { inner, .. }) => inner
        }
    }
}

impl From<Object<Primitive>> for Primitive {
    fn from(obj: Object<Primitive>) -> Primitive {
        obj.into_inner()
    }
}

named!(#[doc="Parse a pdf object"], pub object<Object<Primitive>>, alt!(
    map!(indirect_primitive, Object::Indirect) |
    map!(primitive, Object::Direct)
));

/// Useful primitive for types that may either be behind a reference or not (but are a specific
/// Primitive)
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum MaybeRef<T> {
    NoRef(T),
    Ref(Ref)
}

impl<T> From<T> for MaybeRef<T> {
    fn from(t: T) -> MaybeRef<T> {
        MaybeRef::NoRef(t)
    }
}

impl<T> MaybeRef<T>
    where T: ParseFrom<Primitive, Error=error::Error>
{
    /// Takes a function to resolve xref, and the raw file data, and resolves the ref, if there is
    /// one.
    ///
    /// The function takes a reference, and returns the offset of the real record.
    ///
    /// The callback allows this library to avoid parsing the xref table, since this library is
    /// meant to be local only.
    fn resolve<F>(self, xref_resolve: F, data: &[u8]) -> Option<T>
        where F: Fn(Ref) -> Option<usize>
    {
        match self {
            MaybeRef::NoRef(t) => Some(t),
            MaybeRef::Ref(r) => {
                let offset = try_opt!(xref_resolve(r));
                // TODO parse from offset
                let prim = try_opt!(object(&data[offset..]).to_result().ok());
                debug_assert!(match &prim {
                    &Object::Indirect(Indirect { obj, gen, .. })
                        => obj == r.obj && gen == r.gen,
                    _ => false
                }, "Primitive should be indirect and obj/gen should match");
                Some(try_opt!(T::parse_from(prim.into()).ok()))
            }
        }
    }
}
*/
#[cfg(test)]
mod tests {
    use super::*;
    use Needed;

    #[test]
    fn comment() {
        test_helper!(parse_comment => [
            (b"%this is a comment\n",
                IResult::Done(&b""[..], b"this is a comment".to_vec()),
                "%this is a comment\\n")
        ]);
    }

    #[test]
    fn boolean() {
        test_helper!(parse_boolean => [
            (b"true", IResult::Done(&b""[..], true)),
            (b"false", IResult::Done(&b""[..], false)),
            (b"other", test_error!()),
            (b"tr", IResult::Incomplete(Needed::Size(4))),
            (b"f", IResult::Incomplete(Needed::Size(5)))
        ]);
    }

    #[test]
    fn integer() {
        test_helper!(parse_integer => [
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
        test_helper!(parse_real => [
            // Note the difficulty with ending a stream on a real (or integer), you cannot be sure you
            // are at the end of the number
            (b"12 ", test_error!()),
            (b"12. ", IResult::Done(&b" "[..], BigDecimal::parse_bytes(b"12.", 10).unwrap()))
        ]);
    }

    #[test]
    fn string() {
        test_helper!(parse_string => [
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
            (b" < A23F3 > ", IResult::Done(&b""[..], b"\xa2\x3f\x30".to_vec()), "\\xa2\\x3f\\x30")
        ]);
    }

    #[test]
    fn name() {
        test_helper!(parse_name => [
            (b"/name", IResult::Done(&b""[..], b"name".to_vec()), "name"),
            (b"/", IResult::Done(&b""[..], b"".to_vec()), ""),
            (b"/#6E#61#6D#65", IResult::Done(&b""[..], b"name".to_vec()), "#6E#61#6D#65"),
            (b"/#6E#61#D#65", IResult::Done(&b""[..], b"na#De".to_vec()), "#6E#61#D#65")

        ]);
    }

    #[test]
    fn array() {
        test_helper!(parse_array => [
            (b"[/name 12 true]",
             IResult::Done(&b""[..], vec![
                Primitive::Name(Name(b"name".to_vec().into())),
                Primitive::Numeric(Numeric::Integer(12)),
                Primitive::Boolean(Boolean(true))
             ]),
             "[/name 12 true]"),
            (b"[/name (str]ing) 12 true]",
             IResult::Done(&b""[..], vec![
                Primitive::Name(Name(b"name".to_vec().into())),
                Primitive::String(b"str]ing".to_vec().into()),
                Primitive::Numeric(Numeric::Integer(12)),
                Primitive::Boolean(Boolean(true))
             ]),
             "[ /name (str]ing) 12 true ]")
        ]);
    }

    #[test]
    fn dictionary() {
        // needs more tests
        let mut control = HashMap::new();
        control.insert(b"simple".to_vec().into(), Primitive::Name(b"dictionary".to_vec().into()));
        control.insert(b"of".to_vec().into(), Primitive::Name(b"keys_values".to_vec().into()));

        let mut control2 = HashMap::new();
        control2.insert(b"Size".to_vec(), Primitive::Numeric(Numeric::Integer(22)));
        control2.insert(b"Root".to_vec(), Primitive::Ref(Ref { obj: 2, gen: 0 }));
        control2.insert(b"Info".to_vec(), Primitive::Ref(Ref { obj: 1, gen: 0 }));
        control2.insert(b"ID".to_vec(), Primitive::Array(Array(vec![
            Primitive::String(vec![129, 177, 74, 175, 163, 19, 219, 99, 219,
                                   214, 249, 129, 228, 159, 148, 244].into()),
            Primitive::String(vec![129, 177, 74, 175, 163, 19, 219, 99, 219,
                                   214, 249, 129, 228, 159, 148, 244].into())
        ])));

        test_helper!(parse_dictionary => [
            (b"<< /simple /dictionary /of /keys_values >>",
                IResult::Done(&b""[..], control.clone()),
                "<< /simple /dictionary /of /keys_values >>"),
            (b"<</simple/dictionary/of/keys_values>>",
                IResult::Done(&b""[..], control),
                "<< /simple /dictionary /of /keys_values >>")
        ]);
        let sink = br#"
<< /Size 22
/Root 2 0 R
/Info 1 0 R
/ID [ < 81b14aafa313db63dbd6f981e49f94f4 >
< 81b14aafa313db63dbd6f981e49f94f4 >
]
>>"#;
        assert_eq!(super::parse_dictionary(sink), IResult::Done(&b""[..], control2));
    }

    #[test]
    fn stream() {
        let mut test_map = HashMap::new();
        test_map.insert(b"length".to_vec(), Primitive::Numeric(Numeric::Integer(4)));
        let mut test_map2 = HashMap::new();
        test_map2.insert(b"test".to_vec(), Primitive::Numeric(Numeric::Integer(4)));

        test_helper!(parse_dictionary_or_stream => [
            (b"<</test 4>> true",
                IResult::Done(&b"true"[..], Primitive::Dictionary(Dictionary(test_map2))),
                "<</test 4>> true"),

            (b"<</length 4>> \nstream\ntest\nendstream\n",
                IResult::Done(&b"test\nendstream\n"[..], Primitive::Stream(Stream {
                    params: Dictionary(test_map),
                    data_offset: 22,
                })),
                "<</length 4>> \nstream\ntest\nendstream\n"
            )
        ]);
    }

    #[test]
    fn null() {
        test_helper!(parse_null => [
            (b" null ", IResult::Done(&b""[..], ()), "null")
        ]);
    }

    #[test]
    fn primitive_ref() {
        test_helper!(parse_ref => [
            (b"200 0 R",
             IResult::Done(&b""[..], Ref { obj: 200, gen: 0 }),
             "200 0 R")
        ]);
    }

}
