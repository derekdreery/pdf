use std::borrow::Cow;
use primitive;
pub use nom::Needed;

error_chain! {
    errors {
        // low-level parsing (lexing)
        ParserError(msg: String) {
            description("there was an error while parsing")
            display("there was an error while parsing: {}", msg)
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
