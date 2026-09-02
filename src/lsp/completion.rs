/*****************************************************************************/
/*                                                                           */
/*                               completion.rs                               */
/*                                                                           */
/*        What would resolve here: the model, the ISA, and the fixed         */
/*                        spellings the grammar has                          */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! Completion: exactly what would resolve at this position.
//!
//! Three sources and no fourth — the unit's symbol table, the ISA table, and
//! the fixed spellings the grammar has. Nothing is scored and nothing is
//! ranked: the client sorts and filters, and the server's job is to offer the
//! set that *would resolve*, which is a question the model can answer exactly
//! (`plans/07-lsp-tooling.md` decision 9).
//!
//! **A name in a comment or a string is never offered**, and not because
//! anything filters one out: the candidates come from the symbol table, and a
//! word in a comment was never declared. That is the test.
//!
//! Which set applies is decided from the CST — the enclosing construct — and,
//! for the three one-character prefixes, from the protocol's own
//! `triggerCharacter`. That is not a shortcut around the grammar: a
//! half-typed `PAIR.` has no field-step node yet, only an `ERROR` covering
//! the `.`, and the character the user just pressed is a fact about the
//! *edit* rather than about the program — which is why the protocol carries
//! it and why reading it here is not `src/lsp/` re-reading source text for
//! meaning.
//!
//! A document being typed into is usually mid-error, so the fallback is
//! chosen to be useful rather than exact: a position the grammar cannot place
//! offers what a statement position offers, which is the largest honest set.

use crate::isa::{Mnemonic, OperandForm};
use crate::source::{FileId, Span};
use crate::symbol::SymbolKind;
use crate::{Analysis, AnalyzedUnit};

use super::context::{Context, at};

/// What a candidate is, so the protocol layer can pick an icon.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Mnemonic,
    Keyword,
    Function,
    Variable,
    Constant,
    Label,
    Field,
    /// A width annotation: `@a8`, `@a16`, `@i8`, `@i16`.
    Annotation,
}

/// What a completion request answers.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Completions {
    pub items: Vec<Candidate>,
    /// The text accepting a candidate replaces.
    ///
    /// **A candidate's label includes the punctuation that provoked it** —
    /// `.lo`, `@a8`, `#LIMIT` — because that is what the user is choosing
    /// between, and an editor showing `lo` under a list of fields is showing
    /// the wrong thing. So the character they already typed has to be
    /// *replaced*, not inserted after: without this the client appends at the
    /// cursor and produces `PAIR..lo`, `@@a8`, `##LIMIT`.
    ///
    /// Zero-width where nothing has been typed yet, which is an insertion.
    pub replace: Span,
}

/// One thing that would resolve here.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Candidate {
    /// What the editor shows and, unless `insert` says otherwise, inserts.
    pub label: String,
    pub kind: Kind,
    /// A short suffix the editor shows beside the label.
    pub detail: Option<String>,
    /// What to insert, when the spelling differs from the label — a `#`
    /// before a constant in an immediate position.
    pub insert: Option<String>,
}

impl Candidate {
    fn new(label: impl Into<String>, kind: Kind) -> Self {
        Self {
            label: label.into(),
            kind,
            detail: None,
            insert: None,
        }
    }

    fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }

    fn inserting(mut self, insert: impl Into<String>) -> Self {
        self.insert = Some(insert.into());
        self
    }
}

/// The item keywords, which are what an empty document can hold.
///
/// Written out rather than derived: a grammar rule is a shape, not a list of
/// what a user may usefully type next. `tests/lsp.rs` gates every spelling
/// here against `grammar.json`'s literals, so the list cannot name a keyword
/// the language does not have.
pub const ITEM_KEYWORDS: &[&str] = &[
    "abstract", "binary", "const", "data", "evalfunc", "extern", "far", "func", "import", "inline",
    "main", "naked", "segment", "var",
];

/// The keywords that open or close a statement inside a body.
pub const STATEMENT_KEYWORDS: &[&str] = &[
    "address", "align", "always", "break", "call", "charset", "code", "data", "else", "far",
    "goto", "never", "nocross", "repeat", "return", "return_i",
];

/// The four width annotations, without their `@`.
pub const ANNOTATIONS: &[&str] = &["a8", "a16", "i8", "i16"];

/// What may follow a `:` — the data widths a declaration or a typed view
/// takes, and the two metadata queries.
///
/// Not in decision 9's list of four contexts, and added because the legacy
/// server offered them and they are the language's own fixed spellings: a
/// user of the published extension who stops being offered `:sizeof` has lost
/// something (probe 11, `postmortem.md` §H.7.3).
pub const COLON_SUFFIXES: &[&str] = &["byte", "word", "far", "sizeof", "offsetof"];

/// The register spellings a statement may open with.
///
/// An HLA statement *starts* with one — `a = #1`, `x++`, `dpr = a` — so a
/// statement position that offered only mnemonics would be missing the
/// commonest first character in the language. `RegisterKind`'s own spellings,
/// so the list cannot fall behind a rename of one
/// (`memory/register-naming-convention`).
pub const REGISTERS: &[crate::ast::RegisterKind] = &[
    crate::ast::RegisterKind::A,
    crate::ast::RegisterKind::B,
    crate::ast::RegisterKind::X,
    crate::ast::RegisterKind::Y,
    crate::ast::RegisterKind::S,
    crate::ast::RegisterKind::P,
    crate::ast::RegisterKind::Dpr,
    crate::ast::RegisterKind::Dbr,
];

/// The characters that ask for a completion of their own.
///
/// Advertised to the client, which sends the one it saw back as
/// `CompletionContext.triggerCharacter`.
pub const TRIGGERS: &[&str] = &[".", "@", "#", ":"];

/// What would resolve at `offset` of `file`.
///
/// `trigger` is the character the client says provoked the request, or `None`
/// when the user asked for a completion without typing one.
pub fn complete(
    analysis: &Analysis,
    file: FileId,
    offset: u32,
    trigger: Option<char>,
) -> Completions {
    // A cursor sits *between* tokens, and what it is completing is the token
    // behind it: at `lda SCR|` no node covers the position past the
    // identifier's last byte, and the construct the user is inside is the
    // operand they have half of.
    let probe = match at(analysis, file, offset) {
        Some(context) if context.on_token() => offset,
        _ => offset.saturating_sub(1),
    };
    let Some(context) = at(analysis, file, probe) else {
        return Completions {
            items: Vec::new(),
            replace: Span::empty_at(file, offset),
        };
    };
    let replace = replaced_span(&context, file, offset, trigger);
    let items = match position(&context, offset, trigger) {
        Position::Annotation => ANNOTATIONS
            .iter()
            .map(|name| Candidate::new(format!("@{name}"), Kind::Annotation))
            .collect(),
        Position::Field(layout) => context
            .unit
            .model
            .symbols
            .layout(layout)
            .fields
            .iter()
            .map(|field| {
                Candidate::new(format!(".{}", field.name), Kind::Field)
                    .with_detail(format!("+{}", field.offset))
            })
            .collect(),
        Position::LocalLabel => local_labels(&context),
        Position::Immediate => immediates(&context),
        // Labelled with their `:`, as `.lo` and `@a8` are labelled with
        // theirs: `NAME:sizeof` is how the language spells it, and the
        // punctuation the user already typed is what the candidate replaces.
        Position::ColonSuffix => COLON_SUFFIXES
            .iter()
            .map(|suffix| Candidate::new(format!(":{suffix}"), Kind::Keyword))
            .collect(),
        Position::Item => ITEM_KEYWORDS
            .iter()
            .map(|keyword| Candidate::new(*keyword, Kind::Keyword))
            .collect(),
        Position::Statement => {
            let mut candidates: Vec<Candidate> = Mnemonic::ALL
                .iter()
                .map(|mnemonic| Candidate::new(mnemonic.ca65(), Kind::Mnemonic))
                .collect();
            candidates.extend(
                STATEMENT_KEYWORDS
                    .iter()
                    .map(|keyword| Candidate::new(*keyword, Kind::Keyword)),
            );
            candidates.extend(
                REGISTERS
                    .iter()
                    .map(|register| Candidate::new(register.k65(), Kind::Variable)),
            );
            candidates.extend(unit_symbols(context.unit));
            candidates.extend(local_labels(&context));
            candidates
        }
        Position::Operand => {
            let mut candidates = unit_symbols(context.unit);
            candidates.extend(local_labels(&context));
            if admits_immediate(&context) {
                candidates.extend(immediates(&context));
            }
            candidates
        }
    };
    Completions { items, replace }
}

/// The span accepting a candidate replaces.
///
/// Two cases, and neither reads source text. A *triggered* request has the
/// trigger one byte behind the cursor — all four of them are one byte — and
/// the candidate's label repeats it, so it is replaced. An untriggered one is
/// the user asking part-way through a name, and the name so far is replaced;
/// the token has to be a name-like one, because a request just after `{`
/// must not offer to overwrite the brace.
fn replaced_span(context: &Context<'_>, file: FileId, offset: u32, trigger: Option<char>) -> Span {
    if trigger.is_some() {
        return Span::new(file, offset.saturating_sub(1), offset);
    }
    let partial = context
        .unit
        .parse
        .tree
        .token_before(file, offset)
        .filter(|token| token.span.end == offset)
        .filter(|token| {
            matches!(
                token.kind.as_str(),
                "identifier" | "local_identifier" | "field_name" | "mnemonic" | "register"
            )
        });
    match partial {
        Some(token) => Span::new(file, token.span.start, offset),
        None => Span::empty_at(file, offset),
    }
}

/// Which set of candidates the position calls for.
enum Position {
    /// Just after `@`.
    Annotation,
    /// Just after `.` on a laid-out variable.
    Field(crate::symbol::LayoutId),
    /// Just after `.` where no variable precedes it — a local label.
    LocalLabel,
    /// Just after `#`.
    Immediate,
    /// Just after `:`.
    ColonSuffix,
    /// Between items, where only a declaration may stand.
    Item,
    /// Where a statement may start.
    Statement,
    /// Inside a statement, where an operand may stand.
    Operand,
}

fn position(context: &Context<'_>, offset: u32, trigger: Option<char>) -> Position {
    match trigger {
        Some('@') => return Position::Annotation,
        Some('#') => return Position::Immediate,
        Some(':') => return Position::ColonSuffix,
        Some('.') => {
            // The `.` itself is one byte behind the cursor, and what precedes
            // *it* decides whether this is a field step or a local label.
            return match layout_before(context, offset.saturating_sub(1)) {
                Some(layout) => Position::Field(layout),
                None => Position::LocalLabel,
            };
        }
        _ => {}
    }

    // A mnemonic slot is a statement position, whatever encloses it.
    if context.node_kind() == "mnemonic" {
        return Position::Statement;
    }
    // An operand slot is one the grammar names as one. Asked of the whole
    // path rather than of the innermost node, because an operand nests: the
    // `identifier` of `lda SCREEN,x` sits inside an `address` inside a
    // `native_instruction`.
    if context
        .path
        .iter()
        .any(|node| OPERAND_NODES.contains(&node.kind.as_str()))
    {
        return Position::Operand;
    }
    match context.named_node().map(|node| node.kind.as_str()) {
        // Between items: only a declaration may stand here.
        Some("source_file") | None => Position::Item,
        // The largest honest set: a body, and everything the grammar could
        // not place while the document is being typed into.
        _ => Position::Statement,
    }
}

/// The grammar's own names for the places an operand stands.
const OPERAND_NODES: &[&str] = &[
    "address",
    "immediate",
    "indirect",
    "long_indirect",
    "block_move",
    "hla_assignment",
    "hla_arithmetic",
    "hla_shift",
    "hla_step",
    "call_statement",
    "bare_call",
    "goto_statement",
    "data_value",
    "typed_entry",
    "var_entry",
    "const_entry",
];

/// The layout of the variable whose name ends at `dot`, if the `.` there is a
/// field step rather than the start of a local label.
fn layout_before(context: &Context<'_>, dot: u32) -> Option<crate::symbol::LayoutId> {
    let base = context
        .unit
        .parse
        .tree
        .token_before(context.unit.file, dot)?;
    if base.kind != "identifier" {
        return None;
    }
    // Resolved through the reference index, not by reading the text: the
    // index is what says which declaration this occurrence names.
    let id = context
        .unit
        .model
        .references
        .iter()
        .find(|(span, _)| **span == base.span)
        .map(|(_, id)| *id)?;
    match context.unit.model.symbols.kind(id) {
        SymbolKind::Var {
            layout: Some(layout),
            ..
        }
        | SymbolKind::AbstractVar { layout } => Some(layout),
        _ => None,
    }
}

/// Every unit-level name, this unit's own and the ones it imports.
fn unit_symbols(unit: &AnalyzedUnit) -> Vec<Candidate> {
    unit.model
        .symbols
        .iter()
        .filter(|(_, symbol)| symbol.is_unit_level())
        .filter(|(_, symbol)| !matches!(symbol.kind, SymbolKind::Alias { .. }))
        .map(|(_, symbol)| {
            let kind = match symbol.kind {
                SymbolKind::Const { .. } => Kind::Constant,
                SymbolKind::Function { .. } | SymbolKind::Inline => Kind::Function,
                SymbolKind::Label { .. } | SymbolKind::DataLabel { .. } => Kind::Label,
                _ => Kind::Variable,
            };
            Candidate::new(symbol.name.clone(), kind).with_detail(symbol.kind.describe())
        })
        .collect()
}

/// The enclosing function's local labels, and no other function's.
fn local_labels(context: &Context<'_>) -> Vec<Candidate> {
    let Some(owner) = context.function else {
        return Vec::new();
    };
    context
        .unit
        .model
        .symbols
        .iter()
        .filter(|(_, symbol)| symbol.scope == Some(owner))
        .filter(|(_, symbol)| matches!(symbol.kind, SymbolKind::Label { .. }))
        .map(|(_, symbol)| Candidate::new(symbol.name.clone(), Kind::Label))
        .collect()
}

/// What may follow a `#`: a value, which is a `const` or an address to load.
fn immediates(context: &Context<'_>) -> Vec<Candidate> {
    context
        .unit
        .model
        .symbols
        .iter()
        .filter(|(_, symbol)| symbol.is_unit_level())
        .filter(|(_, symbol)| {
            matches!(
                symbol.kind,
                SymbolKind::Const { .. }
                    | SymbolKind::Var { .. }
                    | SymbolKind::Label { .. }
                    | SymbolKind::DataLabel { .. }
            )
        })
        .map(|(_, symbol)| {
            let kind = match symbol.kind {
                SymbolKind::Const { .. } => Kind::Constant,
                SymbolKind::Label { .. } | SymbolKind::DataLabel { .. } => Kind::Label,
                _ => Kind::Variable,
            };
            Candidate::new(symbol.name.clone(), kind)
                .with_detail(symbol.kind.describe())
                .inserting(format!("#{}", symbol.name))
        })
        .collect()
}

/// Whether the instruction being written has an immediate form at all.
///
/// The ISA table's answer, so `sta #K` is never offered: it is not a store,
/// and offering it would trade one error for another — the same reasoning
/// `resolve.rs` uses when it decides whether to *fix* a `const` in address
/// position by inserting a `#`. A position that is not a native instruction —
/// an HLA operand, a data value, an initializer — takes a value wherever the
/// grammar allows one, so an immediate is admissible there.
fn admits_immediate(context: &Context<'_>) -> bool {
    let Some(statement) = context.statement.and_then(|id| context.statement_node(id)) else {
        return true;
    };
    let crate::ast::StatementKind::Native(native) = &statement.kind else {
        return true;
    };
    Mnemonic::parse(&native.mnemonic.text).is_some_and(|mnemonic| {
        crate::isa::spec_for_form(mnemonic, OperandForm::Immediate)
            .next()
            .is_some()
    })
}
