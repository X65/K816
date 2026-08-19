/*****************************************************************************/
/*                                                                           */
/*                               diagnostic.rs                               */
/*                                                                           */
/*               Structured diagnostics, text edits, and fixes               */
/*                                                                           */
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

//! Structured diagnostics: the internal protocol between compiler and tools.
//!
//! Diagnostics are data, never pre-rendered strings. The CLI renders them with
//! Ariadne in `main.rs`; the LSP converts them to protocol values in `lsp.rs`.
//! Both consume the same [`Diagnostic`] values, so they cannot disagree about
//! what an error means.

use crate::source::Span;

/// A stable, documented diagnostic code such as `K816-E0001`.
///
/// Codes are registered centrally here so they stay unique and stable; tests
/// and editors match on them.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DiagnosticCode(pub &'static str);

impl DiagnosticCode {
    /// Malformed source: Tree-sitter produced an `ERROR` node.
    pub const SYNTAX_ERROR: Self = Self("K816-E0001");
    /// Incomplete source: Tree-sitter inserted a `MISSING` node.
    pub const SYNTAX_MISSING: Self = Self("K816-E0002");
    /// A numeric literal does not fit its representable range.
    pub const NUMBER_RANGE: Self = Self("K816-E0003");
    /// A symbol is used but never declared.
    pub const UNDECLARED_SYMBOL: Self = Self("K816-E0101");
    /// A symbol is declared more than once.
    pub const DUPLICATE_SYMBOL: Self = Self("K816-E0102");
    /// An immediate operand does not fit the width selected by the mode.
    pub const IMMEDIATE_RANGE: Self = Self("K816-E0103");
    /// A width-dependent immediate is used where the register width is unknown.
    pub const UNKNOWN_WIDTH: Self = Self("K816-E0104");
    /// A native instruction uses a mnemonic the ISA does not know.
    pub const UNKNOWN_MNEMONIC: Self = Self("K816-E0105");
    /// An operand form is not legal for the instruction or symbol kind.
    pub const INVALID_OPERAND: Self = Self("K816-E0106");
    /// An address-encoding prefix is not legal for this instruction, e.g.
    /// `dp` on a mnemonic with no direct-page form.
    pub const INVALID_ENCODING: Self = Self("K816-E0107");
    /// An expression must fold at compile time here and does not.
    pub const NOT_CONSTANT: Self = Self("K816-E0108");
    /// A value does not fit the selected address encoding (`dp $1234`).
    pub const ADDRESS_RANGE: Self = Self("K816-E0109");
    /// A bare `const` (or const-rooted expression) in address position.
    pub const CONST_IN_ADDRESS_POSITION: Self = Self("K816-E0110");
    /// A `segment` directive is not the first entry of its data block.
    pub const SEGMENT_PLACEMENT: Self = Self("K816-E0111");
    /// A `nocross`-guarded run exceeds 256 bytes.
    pub const NOCROSS_TOO_LARGE: Self = Self("K816-E0112");
    /// The `&expr` offset marker is used outside a `,X`/`,Y` indexed mode.
    pub const OFFSET_MARKER_POSITION: Self = Self("K816-E0113");
    /// A compound expression appears in an atom-only data value list.
    pub const COMPOUND_DATA_VALUE: Self = Self("K816-E0114");
    /// A symbol is used in a role its declaration does not allow.
    pub const SYMBOL_KIND_MISMATCH: Self = Self("K816-E0115");
    /// `address` placement needs an ld65 config entry; the help carries the
    /// exact fragment to paste.
    pub const ADDRESS_NEEDS_CONFIG: Self = Self("K816-W0116");
    /// A module-level width annotation appears after a code section. A
    /// module default is a file-wide statement, so a positional reading would
    /// make the same annotation mean different things.
    pub const MODULE_WIDTH_PLACEMENT: Self = Self("K816-E0117");
    /// Two width annotations constrain the same axis differently, e.g.
    /// `func f @a8 @a16`. Repeating the *same* annotation is accepted.
    pub const CONFLICTING_WIDTH: Self = Self("K816-E0118");
    /// Reachable returns exit at different fixed widths (a divergent exit).
    pub const DIVERGENT_EXIT: Self = Self("K816-E0119");
    /// A declared exit width (`-> @a8`) disagrees with a reachable return.
    pub const EXIT_WIDTH_MISMATCH: Self = Self("K816-E0120");
    /// A call contract declares an alias on a section that is not `inline`.
    pub const ALIAS_NOT_INLINE: Self = Self("K816-E0121");
    /// A call site's register flow disagrees with the callee's declared
    /// contract — wrong registers, or a bare call with no flow clause to a
    /// contract-bearing function.
    pub const CALL_FLOW_MISMATCH: Self = Self("K816-E0122");
    /// A register live past a checked call is clobbered by the callee.
    pub const CLOBBERED_REGISTER: Self = Self("K816-W0123");
    /// K816 read this function's body, can see it may change width, and
    /// cannot say to what; callers will assume it preserves widths. An
    /// *invisible* callee gets a call-site annotation instead of this.
    pub const ASSUMED_EXIT: Self = Self("K816-W0124");
    /// A native `jsr`/`jsl` reaches a function whose declared entry contract
    /// the tracked width does not satisfy; raw native calls are not bridged.
    pub const UNBRIDGED_NATIVE_CALL: Self = Self("K816-W0125");
    /// A branch or jump leaves a scoped width block, so the block's width
    /// escapes to the target instead of being restored.
    pub const WIDTH_BLOCK_LEAK: Self = Self("K816-W0126");
    /// An assignment chain implies a register transfer the 65816 does not
    /// have. The help names the two-step route through the accumulator where
    /// one exists; K816 suggests it rather than inserting it, because an
    /// inserted transfer clobbers the accumulator.
    pub const NO_SUCH_TRANSFER: Self = Self("K816-E0127");
    /// `break` or `repeat` outside a loop. Both bind to the innermost
    /// enclosing loop, and outside one there is nothing to bind to.
    pub const LOOP_CONTROL_OUTSIDE_LOOP: Self = Self("K816-E0128");
    /// A `goto` names a label of another function. K65 labels are
    /// function-scoped or unit-level, and jumping into the middle of another
    /// function is not a control-flow form the width model can reason about.
    pub const GOTO_FOREIGN_LABEL: Self = Self("K816-E0129");
    /// A `nocross`-guarded run stopped fitting its page because an edge split
    /// grew it. The guard is evaluated after splitting, so this names the
    /// split as the cause rather than the run's written length.
    pub const NOCROSS_SPLIT_OVERFLOW: Self = Self("K816-E0130");
    /// A `naked` section declares an exit contract. A section with no return
    /// has no exit to declare.
    pub const NAKED_EXIT_CONTRACT: Self = Self("K816-E0131");
    /// An `inline` section expands into itself, directly or through a cycle,
    /// so expansion would not terminate. Recursion through a *non*-`inline`
    /// callee is fine: that one is a call, not an expansion.
    pub const INLINE_RECURSION: Self = Self("K816-E0132");
    /// A unit's `var dp` requests exceed the 256-byte direct page. Reported
    /// by the frontend rather than discovered as an ld65 segment overflow.
    pub const DIRECT_PAGE_OVERFLOW: Self = Self("K816-E0133");
    /// A subscript index does not fold to a compile-time constant. Runtime
    /// indexing is the CPU's indexed addressing modes, not a subscript.
    pub const SUBSCRIPT_NOT_CONSTANT: Self = Self("K816-E0134");
    /// A subscript index is outside the extent its declaration gives.
    pub const SUBSCRIPT_OUT_OF_RANGE: Self = Self("K816-E0135");
    /// An `abstract var` is used where storage is required. It declares a
    /// layout for metadata queries and reserves nothing, so it has no address
    /// to load, store, or transfer to.
    pub const ABSTRACT_HAS_NO_STORAGE: Self = Self("K816-E0136");
    /// A chain loads a register from memory a second time because the
    /// transfer it implies does not exist — `x = y = mem` is two loads,
    /// because the 65816 has no `Y`-to-`X` transfer. What the author wrote is
    /// what they get; the warning is that it costs a second memory access.
    pub const CHAIN_RELOADS_SOURCE: Self = Self("K816-W0137");
    /// The parser recognizes a construct the compiler cannot compile yet.
    ///
    /// The `K816-E02xx` range covers "recognized, not implemented": the
    /// grammar and AST know the form, but the owning sub-plan has not landed
    /// its semantics. Distinct from [`Self::SYNTAX_ERROR`] (malformed source)
    /// and from the K65 compatibility family below (legacy-only syntax).
    pub const NOT_YET_IMPLEMENTED: Self = Self("K816-E0201");
    /// The compiler met a CST shape it does not recognize (a K816 bug).
    pub const INTERNAL: Self = Self("K816-E0900");

    // The `K816-K65-####` family: syntax that belongs to the original K65
    // language, is reserved so K816 never reinterprets it, and is recognized
    // by the parser as a specific named node rather than a parse error. Per
    // `doc/k65-compat.md`, each such construct gets its own stable code.

    /// `image` blocks and top-level `image NAME = "file"` declarations.
    pub const K65_RESERVED_IMAGE: Self = Self("K816-K65-0001");
    /// Evaluator side-effect functions: `size`, `addbyte`, `color`, `rnd`,
    /// `print`, `error`.
    pub const K65_RESERVED_EVAL_FUNCTION: Self = Self("K816-K65-0002");

    pub fn as_str(self) -> &'static str {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Help,
    Note,
    Warning,
    Error,
}

/// A message anchored to a source span.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label {
    pub span: Span,
    pub message: String,
}

impl Label {
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

/// Whether a [`Fix`] can be applied without human review.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Applicability {
    /// Safe to apply automatically.
    MachineApplicable,
    /// Plausible but needs review.
    MaybeIncorrect,
}

/// A concrete text replacement: `span` is replaced by `replacement`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextEdit {
    pub span: Span,
    pub replacement: String,
}

impl TextEdit {
    pub fn replace(span: Span, replacement: impl Into<String>) -> Self {
        Self {
            span,
            replacement: replacement.into(),
        }
    }

    pub fn insert(span: Span, text: impl Into<String>) -> Self {
        Self::replace(Span::empty_at(span.file, span.start), text)
    }

    pub fn delete(span: Span) -> Self {
        Self::replace(span, "")
    }
}

/// A machine-applicable remediation: a titled set of edits.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fix {
    pub title: String,
    pub applicability: Applicability,
    pub edits: Vec<TextEdit>,
}

/// A reference into the K816 documentation, e.g. `doc/syntax-reference.md#func`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocsRef(pub String);

/// One complete diagnostic: code, severity, labels, notes, help, and fixes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub code: DiagnosticCode,
    pub severity: Severity,
    /// Concise problem statement, shown as the headline.
    pub title: String,
    /// The main location, with a message explaining what is wrong there.
    pub primary: Label,
    /// Related locations (declarations, previous uses, ...).
    pub secondary: Vec<Label>,
    pub notes: Vec<String>,
    /// Exact remediation advice.
    pub help: Option<String>,
    pub fixes: Vec<Fix>,
    pub docs: Option<DocsRef>,
}

impl Diagnostic {
    pub fn new(
        code: DiagnosticCode,
        severity: Severity,
        title: impl Into<String>,
        primary: Label,
    ) -> Self {
        Self {
            code,
            severity,
            title: title.into(),
            primary,
            secondary: Vec::new(),
            notes: Vec::new(),
            help: None,
            fixes: Vec::new(),
            docs: None,
        }
    }

    pub fn error(code: DiagnosticCode, title: impl Into<String>, primary: Label) -> Self {
        Self::new(code, Severity::Error, title, primary)
    }

    pub fn warning(code: DiagnosticCode, title: impl Into<String>, primary: Label) -> Self {
        Self::new(code, Severity::Warning, title, primary)
    }

    pub fn with_secondary(mut self, label: Label) -> Self {
        self.secondary.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_fix(mut self, fix: Fix) -> Self {
        self.fixes.push(fix);
        self
    }

    pub fn with_docs(mut self, docs: impl Into<String>) -> Self {
        self.docs = Some(DocsRef(docs.into()));
        self
    }

    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }
}

/// True when any diagnostic in `diagnostics` is an error.
pub fn has_errors(diagnostics: &[Diagnostic]) -> bool {
    diagnostics.iter().any(Diagnostic::is_error)
}

#[cfg(test)]
mod tests {
    use super::DiagnosticCode;

    /// Every registered code, so the uniqueness check below sees all of them.
    const REGISTERED: &[DiagnosticCode] = &[
        DiagnosticCode::SYNTAX_ERROR,
        DiagnosticCode::SYNTAX_MISSING,
        DiagnosticCode::NUMBER_RANGE,
        DiagnosticCode::UNDECLARED_SYMBOL,
        DiagnosticCode::DUPLICATE_SYMBOL,
        DiagnosticCode::IMMEDIATE_RANGE,
        DiagnosticCode::UNKNOWN_WIDTH,
        DiagnosticCode::UNKNOWN_MNEMONIC,
        DiagnosticCode::INVALID_OPERAND,
        DiagnosticCode::INVALID_ENCODING,
        DiagnosticCode::NOT_CONSTANT,
        DiagnosticCode::ADDRESS_RANGE,
        DiagnosticCode::CONST_IN_ADDRESS_POSITION,
        DiagnosticCode::SEGMENT_PLACEMENT,
        DiagnosticCode::NOCROSS_TOO_LARGE,
        DiagnosticCode::OFFSET_MARKER_POSITION,
        DiagnosticCode::COMPOUND_DATA_VALUE,
        DiagnosticCode::SYMBOL_KIND_MISMATCH,
        DiagnosticCode::ADDRESS_NEEDS_CONFIG,
        DiagnosticCode::MODULE_WIDTH_PLACEMENT,
        DiagnosticCode::CONFLICTING_WIDTH,
        DiagnosticCode::DIVERGENT_EXIT,
        DiagnosticCode::EXIT_WIDTH_MISMATCH,
        DiagnosticCode::ALIAS_NOT_INLINE,
        DiagnosticCode::CALL_FLOW_MISMATCH,
        DiagnosticCode::CLOBBERED_REGISTER,
        DiagnosticCode::ASSUMED_EXIT,
        DiagnosticCode::UNBRIDGED_NATIVE_CALL,
        DiagnosticCode::WIDTH_BLOCK_LEAK,
        DiagnosticCode::NO_SUCH_TRANSFER,
        DiagnosticCode::LOOP_CONTROL_OUTSIDE_LOOP,
        DiagnosticCode::GOTO_FOREIGN_LABEL,
        DiagnosticCode::NOCROSS_SPLIT_OVERFLOW,
        DiagnosticCode::NAKED_EXIT_CONTRACT,
        DiagnosticCode::INLINE_RECURSION,
        DiagnosticCode::DIRECT_PAGE_OVERFLOW,
        DiagnosticCode::SUBSCRIPT_NOT_CONSTANT,
        DiagnosticCode::SUBSCRIPT_OUT_OF_RANGE,
        DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
        DiagnosticCode::CHAIN_RELOADS_SOURCE,
        DiagnosticCode::NOT_YET_IMPLEMENTED,
        DiagnosticCode::INTERNAL,
        DiagnosticCode::K65_RESERVED_IMAGE,
        DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
    ];

    #[test]
    fn codes_are_unique_and_well_formed() {
        let mut seen = std::collections::HashSet::new();
        for code in REGISTERED {
            assert!(
                code.as_str().starts_with("K816-"),
                "{} lacks the K816 prefix",
                code.as_str()
            );
            assert!(
                seen.insert(code.as_str()),
                "duplicate code {}",
                code.as_str()
            );
        }
    }
}
