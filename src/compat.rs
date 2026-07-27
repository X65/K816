/*****************************************************************************/
/*                                                                           */
/*                                 compat.rs                                 */
/*                                                                           */
/*        Not-yet-implemented and reserved-K65 diagnostic constructors       */
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

//! Diagnostics for syntax the parser understands but the compiler does not
//! compile: constructs whose semantics are still to be implemented, and the
//! reserved legacy-K65 namespace.
//!
//! The grammar recognizes the whole K65/K816 surface (sub-plan B), which is
//! deliberately wider than what `semantic.rs` and `lower.rs` can handle. Every
//! statement, item, and data-entry match in those modules is exhaustive and
//! non-panicking: arms with no semantics yet call [`unimplemented`] here, and
//! reserved legacy forms call [`k65_reserved`]. Each arm is replaced by real
//! behavior as its owning sub-plan lands, with the compiler pointing at any
//! that were missed.

use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::source::Span;

/// The sub-plan that owns a construct's semantics, per `plans/README.md` and
/// the Owner column of `plans/feature-manifest.md` §2.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Owner {
    /// C — the native translator.
    NativeTranslator,
    /// D — register widths and call contracts.
    WidthAndContracts,
    /// E — HLA and declaration lowering.
    HlaLowering,
    /// F — the optimizer.
    Optimizer,
    /// G — compatibility migration.
    Compatibility,
}

impl Owner {
    /// The plan letter, e.g. `"C"`.
    pub fn letter(self) -> &'static str {
        match self {
            Self::NativeTranslator => "C",
            Self::WidthAndContracts => "D",
            Self::HlaLowering => "E",
            Self::Optimizer => "F",
            Self::Compatibility => "G",
        }
    }

    /// A human-readable name for the owning work, e.g. `"native translator"`.
    pub fn title(self) -> &'static str {
        match self {
            Self::NativeTranslator => "native translator",
            Self::WidthAndContracts => "width and contract analysis",
            Self::HlaLowering => "HLA and declaration lowering",
            Self::Optimizer => "optimizer",
            Self::Compatibility => "compatibility migration",
        }
    }
}

/// `feature` is recognized by the parser but has no semantics yet; `owner`
/// names the sub-plan that will implement it.
///
/// `feature` reads as a noun phrase in the message, e.g. `"`data` blocks"`.
pub fn unimplemented(span: Span, feature: &str, owner: Owner) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::NOT_YET_IMPLEMENTED,
        format!("{feature} are recognized but not compiled yet"),
        Label::new(span, "K816 cannot compile this construct yet"),
    )
    .with_note(format!(
        "implemented by sub-plan {} ({})",
        owner.letter(),
        owner.title()
    ))
    .with_help("follow implementation status in plans/README.md")
}

/// `construct` belongs to the original K65 language, is recognized as a
/// specific named node, and is intentionally not reinterpreted by K816.
///
/// `code` must come from the `K816-K65-####` family so the compatibility
/// corpus can match on it; see `doc/k65-compat.md`.
pub fn k65_reserved(code: DiagnosticCode, span: Span, construct: &str) -> Diagnostic {
    Diagnostic::error(
        code,
        format!("K65 `{construct}` construct is not implemented yet"),
        Label::new(span, "this syntax belongs to the original K65 language"),
    )
    .with_note("the syntax is reserved for K65 compatibility")
    .with_help("follow implementation status in plans/README.md")
    .with_docs("doc/k65-compat.md")
}

#[cfg(test)]
mod tests {
    use super::{Owner, k65_reserved, unimplemented};
    use crate::diagnostic::DiagnosticCode;
    use crate::source::{FileId, SourceMap, Span};

    fn span() -> Span {
        let mut map = SourceMap::new();
        let id: FileId = map.add_file("t.k65", "data X { }");
        Span::new(id, 0, 4)
    }

    #[test]
    fn unimplemented_names_its_owner() {
        let diagnostic = unimplemented(span(), "`data` blocks", Owner::NativeTranslator);
        assert_eq!(diagnostic.code, DiagnosticCode::NOT_YET_IMPLEMENTED);
        assert_eq!(
            diagnostic.notes,
            ["implemented by sub-plan C (native translator)"]
        );
    }

    #[test]
    fn reserved_uses_the_k65_family() {
        let diagnostic = k65_reserved(DiagnosticCode::K65_RESERVED_IMAGE, span(), "image");
        assert!(diagnostic.code.as_str().starts_with("K816-K65-"));
        assert!(diagnostic.title.contains("`image`"));
    }
}
