use pdf_par_ser;

error_chain! {
    links {
        ParSer(pdf_par_ser::Error, pdf_par_ser::ErrorKind);
    }

    errors {
        NoEof {
            description("could not find the EOF marker in the last 1024 bytes of the file")
        }
        NoXRef {
            description("could not find the xref location in the last 1024 bytes before EOF")
        }
        XrefEntryNotPresent(requested: u64) {
            description("an xref entry was requested that isn't in the cross-reference table")
            display("the xref entry with number {} was requested, but it isn't in the \
                    cross-reference table", requested)
        }
        XrefEntryNotInUse(requested: u64) {
            description("an xref entry was requested that is marked not in use")
            display("the xref entry {} was requested, but it is not in use", requested)
        }

        // generic errors
        Utf8Decode(string: Vec<u8>) {
            description("a string could not be displayed as it is not utf8 encoded")
            display("the string {} could not be displayed as it is not utf8 encoded",
                        String::from_utf8_lossy(string))
        }
    }
}
