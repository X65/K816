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
    /// The compiler met a CST shape it does not recognize (a K816 bug).
    pub const INTERNAL: Self = Self("K816-E0900");

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
