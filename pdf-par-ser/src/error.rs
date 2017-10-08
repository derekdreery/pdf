use std::borrow::Cow;
use primitive;
pub use nom::Needed;

error_chain! {
    errors {
        // low-level parsing (lexing)
        ParserError {
            description("there was an error while parsing")
        }
        ParserIncomplete(needed: Needed) {
            description("there were not enough bytes in the input to finish parsing")
            display("there were not enough bytes in the input to finish parsing, need {:?}", needed)
        }
        ParseVersion(v: String) {
            description("invalid version")
            display(r#"the version "{}" is invalid "#, v)
        }

        // Higher level parsing
        UnexpectedToken(token: String, expected: &'static str) {
            description("unexpected token")
            display(r#"unexpected token "{}", expected a {}"#, token, expected)
        }
        MissingDictionaryField(name: Cow<'static, str>, ctx: &'static str) {
            description("missing dictionary field")
            display(r#"missing dictionary field "{}" in {}"#, name, ctx)
        }
        IncorrectType(value: String, expected: &'static str) {
            description("dictionary has incorrect type")
            display(r#"expected a {}, found a "{}""#, expected, value)
        }

        // Specific errors
        ReferenceInXRefDictionary {
            description("objects in the xref dictionary cannot be indirect references")
        }
        UnresolvedReference(ref_: primitive::Ref) {
            description("a reference could not be resolved")
            display(r#"the reference with obj number "{}" and gen number "{}" could not be found"#,
                    ref_.obj, ref_.gen)
        }
        InvalidPageLayout(page_layout: primitive::Name) {
            description("the page layout was not recognised")
            display("the page layout \"{}\" was not recognised", page_layout)
        }
        InvalidPageMode(page_mode: primitive::Name) {
            description("the page mode was not recognised")
            display("the page mode \"{}\" was not recognised", page_mode)
        }
    }
}

// this is a pain but oh well (needed for tests
#[cfg(test)]
impl PartialEq for ErrorKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&ErrorKind::ParserError(ref msg1), &ErrorKind::ParserError(ref msg2))
                => msg1 == msg2,
            (&ErrorKind::ParserIncomplete(ref needed1), &ErrorKind::ParserIncomplete(ref needed2))
                => needed1 == needed2,
            (&ErrorKind::ParseVersion(ref v1), &ErrorKind::ParseVersion(ref v2))
                => v1 == v2,
            (&ErrorKind::UnexpectedToken(ref token1, ref expected1),
             &ErrorKind::UnexpectedToken(ref token2, ref expected2))
                => token1 == token2 && expected1 == expected2,
            (&ErrorKind::MissingDictionaryField(ref name1, ref ctx1),
             &ErrorKind::MissingDictionaryField(ref name2, ref ctx2))
                => name1 == name2 && ctx1 == ctx2,
            (&ErrorKind::IncorrectType(ref value1, ref expected1),
             &ErrorKind::IncorrectType(ref value2, ref expected2))
                => value1 == value2 && expected1 == expected2,
            (&ErrorKind::ReferenceInXRefDictionary, &ErrorKind::ReferenceInXRefDictionary) => true,
            (&ErrorKind::UnresolvedReference(ref ref1),
             &ErrorKind::UnresolvedReference(ref ref2))
                => ref1 == ref2,
            (&ErrorKind::InvalidPageLayout(ref page_layout1),
             &ErrorKind::InvalidPageLayout(ref page_layout2))
                => page_layout1 == page_layout2,
            (&ErrorKind::InvalidPageMode(ref page_mode1),
             &ErrorKind::InvalidPageMode(ref page_mode2))
                => page_mode1 == page_mode2,
            _ => false,
        }
    }
}

#[cfg(test)]
impl PartialEq for Error {
    /// Currently just compares ErrorKind
    fn eq(&self, other: &Self) -> bool {
        self.kind() == other.kind()
    }
}
