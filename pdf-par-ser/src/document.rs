//! Pdf document parsers (see spec for distinction between document and file)
//!
use primitive::{Dictionary, Array, Boolean, Ref, Primitive, Name, PdfString, MaybeRef};
use file::{PdfVersion};
use {ErrorKind, Result, Parse, ParseFrom, Downcast};

/// A name object specifying the page layout to be used when the document is opened:
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PageLayout {
    /// Display one page at a time
    SinglePage,
    /// Display the pages in one column
    OneColumn,
    /// Display the pages in two columns, with odd-numbered pages on the left
    TwoColumnLeft,
    /// Display the pages in two columns, with odd-numbered pages on the right
    TwoColumnRight,
    /// (PDF 1.5) Display the pages two at a time, with odd-numbered pages on the left
    TwoPageLeft,
    /// (PDF 1.5) Display the pages two at a time, with odd-numbered pages on the right
    TwoPageRight,
}

impl Default for PageLayout {
    /// The default for PageLayout is a single page layout
    fn default() -> Self {
        PageLayout::SinglePage
    }
}

impl Parse<Name> for PageLayout {
    fn parse(i: &Name) -> Result<PageLayout> {
        let r: &[u8] = i.as_ref();
        match r {
            b"SinglePage" => Ok(PageLayout::SinglePage),
            b"OneColumn" => Ok(PageLayout::OneColumn),
            b"TwoColumnLeft" => Ok(PageLayout::TwoColumnLeft),
            b"TwoColumnRight" => Ok(PageLayout::TwoColumnRight),
            b"TwoPageLeft" => Ok(PageLayout::TwoPageLeft),
            b"TwoPageRight" => Ok(PageLayout::TwoPageRight),
            _ => bail!(ErrorKind::InvalidPageLayout(i.clone()))
        }
    }
}

impl ParseFrom<Primitive> for PageLayout {
    fn parse_from(i: Primitive) -> Result<PageLayout> {
        let i = Name::downcast(i)?;
        PageLayout::parse(&i)
    }
}

/// A name object specifying how the document should be displayed when opened.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PageMode {
    /// Neither document outline nor thumbnail images visible
    UseNone,
    /// Document outline visible
    UseOutlines,
    /// Thumbnail images visible
    UseThumbs,
    /// Full-screen mode, with no menu bar, window controls, or any other window visible
    FullScreen,
    /// (PDF 1.5) Optional content group panel visible
    UseOC,
    /// (PDF 1.6) Attachments panel visible
    UseAttachments,
}

impl Default for PageMode {
    fn default() -> Self {
        PageMode::UseNone
    }
}


impl Parse<Name> for PageMode {
    fn parse(i: &Name) -> Result<PageMode> {
        let r: &[u8] = i.as_ref();
        match r {
            b"UseNone" => Ok(PageMode::UseNone),
            b"UseOutlines" => Ok(PageMode::UseOutlines),
            b"UseThumbs" => Ok(PageMode::UseThumbs),
            b"FullScreen" => Ok(PageMode::FullScreen),
            b"UseOC" => Ok(PageMode::UseOC),
            b"UseAttachments" => Ok(PageMode::UseAttachments),
            _ => bail!(ErrorKind::InvalidPageMode(i.clone()))
        }
    }
}

impl ParseFrom<Primitive> for PageMode {
    fn parse_from(i: Primitive) -> Result<PageMode> {
        let i = Name::downcast(i)?;
        PageMode::parse(&i)
    }
}

/// The document's root catalog
///
/// (Spec 3.6.1)
#[derive(Debug, Clone)]
pub struct Catalog {
    // check type is "Catalog"
    /// The version of the PDF specification to which the document conforms (for example, 1.4 )
    pub version: Option<PdfVersion>,
    /// The page tree node that is the root of the document’s page tree
    pub pages: Ref,
    /// A number tree defining the page labeling for the document.
    ///
    /// The keys in this tree are page indices; the corresponding values are page label
    /// dictionaries. Each page index denotes the first page
    /// in a labeling range to which the specified page label dictionary applies. The tree must
    /// include a value for page index 0.
    pub page_labels: Option<MaybeRef<Array>>, // todo number trees
    /// The document’s name dictionary.
    pub names: Option<MaybeRef<Dictionary>>,
    /// A dictionary of names and corresponding destinations.
    pub destinations: Option<Ref>,
    /// A viewer preferences dictionary specifying the way the document is to be displayed on
    /// the screen.
    pub viewer_pref: Option<MaybeRef<Dictionary>>,
    /// A name object specifying the page layout to be used when the document is opened.
    ///
    /// Default value: SinglePage.
    pub page_layout: Option<MaybeRef<PageLayout>>,
    /// A name object specifying how the document should be displayed when opened.
    ///
    /// Default value: UseNone
    pub page_mode: Option<MaybeRef<PageMode>>,
    /// The outline dictionary that is the root of the document’s outline hierarchy
    pub outlines: Option<Ref>,
    /// An array of thread dictionaries representing the document’s article threads
    pub threads: Option<Ref>,
    /// A value specifying a destination to be displayed or an action to be performed when the
    /// document is opened.
    pub open_action: Option<MaybeRef<Primitive>>,
    /// An additional-actions dictionary defining the actions to be taken in response to various
    /// trigger events affecting the document as a whole.
    pub additional_actions: Option<MaybeRef<Dictionary>>,
    /// A URI dictionary containing document-level information for URI (uniform resource
    /// identifier) actions.
    pub uri: Option<MaybeRef<Dictionary>>,
    /// The document’s interactive form (AcroForm) dictionary.
    pub acro_form: Option<MaybeRef<Dictionary>>,
    /// A metadata stream containing metadata for the document.
    pub metadata: Option<Ref>,
    /// The document’s structure tree root dictionary.
    pub structure_tree_root: Option<MaybeRef<Dictionary>>,
    /// A mark information dictionary containing information about the document’s usage of
    /// Tagged PDF conventions.
    pub mark_info: Option<MaybeRef<Dictionary>>,
    /// A language identifier specifying the natural language for all text in the document except
    /// where overridden by language specifications for structure elements or marked content.
    /// If this entry is absent, the language is considered unknown.
    pub lang: Option<MaybeRef<PdfString>>,
    /// A Web Capture information dictionary containing state information used by the Acrobat Web
    /// Capture (AcroSpider) plugin extension
    pub spider_info: Option<MaybeRef<Dictionary>>,
    /// An array of output intent dictionaries describing the color characteristics of output
    /// devices on which the document might be rendered.
    pub output_intents: Option<MaybeRef<Array>>,
    /// A page-piece dictionary associated with the document.
    pub piece_info: Option<MaybeRef<Dictionary>>,
    /// The document’s optional content properties dictionary.
    pub oc_properties: Option<MaybeRef<Dictionary>>,
    /// A permissions dictionary that specifies user access permissions for the document.
    pub perms: Option<MaybeRef<Dictionary>>,
    /// A dictionary containing attestations regarding the content of a PDF document, as it
    /// relates to the legality of digital signatures.
    pub legal: Option<MaybeRef<Dictionary>>,
    /// An array of requirement dictionaries representing requirements for the document.
    pub requirements: Option<MaybeRef<Array>>,
    /// A collection dictionary that a PDF consumer uses to enhance the presentation of file
    /// attachments stored in the PDF document.
    pub collection: Option<MaybeRef<Dictionary>>,
    /// A flag used to expedite the display of PDF documents containing XFA forms. It specifies
    /// whether the document must be regenerated when the document is first opened.
    pub needs_rendering: Option<MaybeRef<Boolean>>,
}

impl ParseFrom<Dictionary> for Catalog {

    fn parse_from(mut dict: Dictionary) -> Result<Catalog> {

        // Check this is a category
        match dict.remove(&b"Type"[..]) {
            Some(ty) => {
                let ty = Name::downcast(ty)?;
                if &ty != &b"Catalog"[..] {
                    bail!(ErrorKind::IncorrectType(ty.into(), "Catalog"));
                }
            },
            None => bail!(ErrorKind::MissingDictionaryField("Type".into(), "Catalog"))
        };

        // error if version cannot be parsed, continue if not present (it's optional)
        let version = match dict.remove(&b"Version"[..]) {
            Some(ent) => {
                let v_name = Name::downcast(ent)?;
                Some(PdfVersion::parse(&v_name)?)
            },
            None => None
        };

        let pages = dict.remove(&b"Pages"[..]).ok_or(
            ErrorKind::MissingDictionaryField("Pages".into(), "Catalog"))?;
        let pages = Ref::downcast(pages)?;

        let page_labels = dict.remove(&b"PageLabels"[..]);
        let page_labels = if let Some(p) = page_labels {
            match p {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Array(arr) => Some(MaybeRef::Direct(arr)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Array"))
            }
        } else { None };

        let names = dict.remove(&b"Names"[..]);
        let names = if let Some(n) = names {
            match n {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let destinations = dict.remove(&b"Destinations"[..]);
        let destinations = if let Some(d) = destinations {
            Some(Ref::downcast(d)?)
        } else { None };


        let viewer_pref = dict.remove(&b"ViewerPreference"[..]);
        let viewer_pref = if let Some(vp) = viewer_pref {
            match vp {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let page_layout = dict.remove(&b"PageLayout"[..]);
        let page_layout = if let Some(pl) = page_layout {
            match pl {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Name(n) => Some(MaybeRef::Direct(PageLayout::parse(&n)?)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Name"))
            }
        } else { None };

        let page_mode = dict.remove(&b"PageMode"[..]);
        let page_mode = if let Some(pm) = page_mode {
            match pm {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Name(n) => Some(MaybeRef::Direct(PageMode::parse(&n)?)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or PageMode"))
            }
        } else { None };

        let outlines = dict.remove(&b"Outlines"[..]);
        let outlines = if let Some(o) = outlines {
            Some(Ref::downcast(o)?)
        } else { None };

        let threads = dict.remove(&b"Threads"[..]);
        let threads = if let Some(t) = threads {
            Some(Ref::downcast(t)?)
        } else { None };

        let open_action = None; // TODO decide on type

        let additional_actions = dict.remove(&b"AA"[..]);
        let additional_actions = if let Some(aa) = additional_actions {
            match aa {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let uri = dict.remove(&b"URI"[..]);
        let uri = if let Some(uri) = uri {
            match uri {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let acro_form = dict.remove(&b"AcroForm"[..]);
        let acro_form = if let Some(af) = acro_form {
            match af {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let metadata = dict.remove(&b"Metadata"[..]);
        let metadata = if let Some(m) = metadata {
            Some(Ref::downcast(m)?)
        } else { None };

        let structure_tree_root = dict.remove(&b"StructTreeRoot"[..]);
        let structure_tree_root = if let Some(s) = structure_tree_root {
            match s {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let mark_info = dict.remove(&b"MarkInfo"[..]);
        let mark_info = if let Some(mi) = mark_info {
            match mi {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let lang = dict.remove(&b"Lang"[..]);
        let lang = if let Some(l) = lang {
            match l {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::String(s) => Some(MaybeRef::Direct(s)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or String"))
            }
        } else { None };

        let spider_info = dict.remove(&b"SpiderInfo"[..]);
        let spider_info = if let Some(si) = spider_info {
            match si {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let output_intents = dict.remove(&b"OutputIntents"[..]);
        let output_intents = if let Some(si) = output_intents {
            match si {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Array(a) => Some(MaybeRef::Direct(a)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Array"))
            }
        } else { None };

        let piece_info = dict.remove(&b"PieceInfo"[..]);
        let piece_info = if let Some(pi) = piece_info {
            match pi {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let oc_properties = dict.remove(&b"OCProperties"[..]);
        let oc_properties = if let Some(op) = oc_properties {
            match op {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let perms = dict.remove(&b"Perms"[..]);
        let perms = if let Some(p) = perms {
            match p {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let legal = dict.remove(&b"Legal"[..]);
        let legal = if let Some(op) = legal {
            match op {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let requirements = dict.remove(&b"Requirements"[..]);
        let requirements = if let Some(r) = requirements {
            match r {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Array(a) => Some(MaybeRef::Direct(a)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Array"))
            }
        } else { None };

        let collection = dict.remove(&b"Collection"[..]);
        let collection = if let Some(c) = collection {
            match c {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Dictionary(d) => Some(MaybeRef::Direct(d)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or Dictionary"))
            }
        } else { None };

        let needs_rendering = dict.remove(&b"NeedsRendering"[..]);
        let needs_rendering = if let Some(nr) = needs_rendering {
            match nr {
                Primitive::Ref(r) => Some(MaybeRef::Ref(r)),
                Primitive::Boolean(b) => Some(MaybeRef::Direct(b)),
                _ => bail!(ErrorKind::IncorrectType("<T>".into(), "Ref or bool"))
            }
        } else { None };

        Ok(Catalog {
            version,
            pages,
            page_labels,
            names,
            destinations,
            viewer_pref,
            page_layout,
            page_mode,
            outlines,
            threads,
            open_action,
            additional_actions,
            uri,
            acro_form,
            metadata,
            structure_tree_root,
            mark_info,
            lang,
            spider_info,
            output_intents,
            piece_info,
            oc_properties,
            perms,
            legal,
            requirements,
            collection,
            needs_rendering,
        })
    }
}

/*
impl Catalog {
    pub fn pages<R>(&self, data: &[u8], resolver: R) -> Result<PageTree>
        where R: Fn(Ref) -> Option<usize>
    {
        let loc = resolver(self.pages).ok_or(
            from_kind(ErrorKind::UnresolvedReference(self.pages))
        )?;
        let dict = Dictionary::try_from(
            parse_object(&data[loc..]).to_result().map_err(|_| from_kind(ErrorKind::ParserError))?
            .into_inner()
        )?;
        PageTree::try_from(dict)
    }
}
*/

/// The root of the page tree.
///
/// This allows a graph strucure to be used, to avoid having to allocate all pages in memory.
#[derive(Debug, Clone)]
pub struct PageTree {
    /// Child nodes of this page node (they may either be `PageTreeNode`s or `Page`s)
    pub kids: Vec<Ref>,
    /// The total number of pages in the pdf doc
    pub count: i64,
}

impl ParseFrom<Dictionary> for PageTree {

    /// Trys to parse a dictionary into a page tree. Consumes the dictionary
    fn parse_from(mut dict: Dictionary) -> Result<PageTree> {
        let ty = dict.remove(&b"Type"[..]).ok_or(
            ErrorKind::MissingDictionaryField("Type".into(), "PageTree"))?;
        let ty = Name::downcast(ty)?;
        if ty != &b"Pages"[..] {
            bail!(ErrorKind::IncorrectType(ty.into(), "Pages"));
        }

        if dict.remove(&b"Parent"[..]) != None {
            bail!("root of Pages tree should not have Parent");
        }

        let kids = dict.remove(&b"Kids"[..]).ok_or(
            ErrorKind::MissingDictionaryField("Kids".into(), "PageTree"))?;
        let kids = kids.downcast_array_of::<Ref>()?;

        let count = dict.remove(&b"Count"[..]).ok_or(
            ErrorKind::MissingDictionaryField("Kids".into(), "PageTree"))?;
        let count = <i64 as Downcast<Primitive>>::downcast(count)?;

        Ok(PageTree { kids, count })
    }
}

#[derive(Debug, Clone)]
pub struct PageTreeNode {
    pub parent: Ref,
    pub kids: Vec<Ref>,
    pub count: i64,
}

impl ParseFrom<Dictionary> for PageTreeNode {

    /// Parses a dictionary into a page tree node. Consumes the dictionary
    fn parse_from(mut dict: Dictionary) -> Result<PageTreeNode> {
        if dict.remove(&b"Type"[..]) != Some(Primitive::Name(Name(b"Pages".to_vec()))) {
            bail!(ErrorKind::MissingDictionaryField("Pages".into(), "PageTreeNode"));
        }

        let parent = dict.remove(&b"Parent"[..])
            .ok_or(ErrorKind::MissingDictionaryField("Parent".into(), "PageTreeNode"))?;
        let parent = Ref::downcast(parent)?;

        let kids = dict.remove(&b"Kids"[..])
            .ok_or(ErrorKind::MissingDictionaryField("Kids".into(), "PageTreeNode"))?;
        let kids = kids.downcast_array_of::<Ref>()?;

        let count = dict.remove(&b"Count"[..])
            .ok_or(ErrorKind::MissingDictionaryField("Count".into(), "PageTreeNode"))?;
        let count = <i64 as Downcast<Primitive>>::downcast(count)?;

        Ok(PageTreeNode { parent, kids, count })
    }
}


