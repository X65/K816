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
    /// G — compatibility migration.
    Compatibility,
}

// There is deliberately no `Optimizer` arm. This enum names the sub-plan that
// will *implement a construct*, and sub-plan F implements none: it rewrites
// operations the other sub-plans already lower, so no "not yet compiled"
// diagnostic could ever name it. An owner with nothing to own is a dead arm
// that invites a wrong attribution later (`postmortem.md` §C.7). If some later
// sub-plan needs one, it comes back with a call site attached.

impl Owner {
    /// The plan letter, e.g. `"C"`.
    pub fn letter(self) -> &'static str {
        match self {
            Self::NativeTranslator => "C",
            Self::WidthAndContracts => "D",
            Self::HlaLowering => "E",
            Self::Compatibility => "G",
        }
    }

    /// A human-readable name for the owning work, e.g. `"native translator"`.
    pub fn title(self) -> &'static str {
        match self {
            Self::NativeTranslator => "native translator",
            Self::WidthAndContracts => "width and contract analysis",
            Self::HlaLowering => "HLA and declaration lowering",
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

/// `bank NAME` is removed, and `segment NAME` is what replaced it.
///
/// A *removal* rather than a reservation, so the wording says so: nothing
/// later in the plan brings `bank` back, and a message reading "not
/// implemented yet" would promise otherwise. The one bank the original
/// compiled into is named `default`, and it is refused with the rest — the
/// declaration moves no byte either way, and carving a name out of the rule
/// reopens the silent acceptance the refusal was chosen over
/// (`plans/06-compatibility.md` decision 10).
pub fn k65_removed_bank(span: Span, name: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::K65_REMOVED_BANK,
        format!("K65 `bank {name}` is not a K816 declaration"),
        Label::new(span, "K816 has no bank-switching model of its own"),
    )
    .with_note(
        "the original's banks were its linker's; in K816 the memory map lives \
         in the ld65 configuration",
    )
    .with_help(format!(
        "write `segment {name}` and give `{name}` a `MEMORY`/`SEGMENTS` entry \
         in your ld65 config"
    ))
    .with_docs("doc/k65-compat.md")
}

/// One of the five HLA forms whose bytes are undocumented 6502 opcodes.
///
/// `illegal` is the 6502 mnemonic the form is a spelling of; `native` says
/// what the 65816 does with the same bytes. The help stops there on purpose:
/// K816 assembles the documented instruction sets and nothing else
/// (`doc/design.md` §1), and publishing a hand-rolled replacement sequence
/// would be inviting back the thing that boundary excludes.
pub fn k65_illegal_opcode(span: Span, form: &str, illegal: &str, native: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::K65_UNSUPPORTED_ILLEGAL_OPCODE,
        format!("K65 `{form}` assembles the illegal 6502 opcode `{illegal}`"),
        Label::new(span, "K816 does not assemble undocumented opcodes"),
    )
    .with_note(format!("on the 65816 those bytes are {native}"))
    .with_help(
        "K816 assembles the documented MOS 6502, WDC 65C02 and WDC 65C816 \
         instruction sets only",
    )
    .with_docs("doc/k65-compat.md")
}

#[cfg(test)]
mod tests {
    use super::{Owner, k65_illegal_opcode, k65_removed_bank, k65_reserved, unimplemented};
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
    fn bank_is_removed_rather_than_pending() {
        let diagnostic = k65_removed_bank(span(), "core");
        assert_eq!(diagnostic.code, DiagnosticCode::K65_REMOVED_BANK);
        // The distinction the wording carries: `bank` is not coming back, so
        // nothing here may read as a promise that it will.
        assert!(!diagnostic.title.contains("yet"));
        assert!(
            diagnostic
                .help
                .is_some_and(|help| help.contains("segment core"))
        );
    }

    #[test]
    fn an_illegal_opcode_names_what_the_bytes_do_and_offers_nothing() {
        let diagnostic = k65_illegal_opcode(span(), "a &. n", "ANC", "`PHD`");
        assert_eq!(
            diagnostic.code,
            DiagnosticCode::K65_UNSUPPORTED_ILLEGAL_OPCODE
        );
        assert!(diagnostic.notes.iter().any(|note| note.contains("PHD")));
        // No replacement sequence: the boundary is the answer, and a
        // suggested substitute would be K816 spelling an illegal opcode for
        // the author (`doc/design.md` §1). The help states the boundary and
        // proposes nothing.
        let help = diagnostic.help.expect("the boundary is stated");
        assert!(help.contains("documented"));
        assert!(!help.contains("instead") && !help.contains("write "));
    }

    #[test]
    fn reserved_uses_the_k65_family() {
        let diagnostic = k65_reserved(DiagnosticCode::K65_RESERVED_IMAGE, span(), "image");
        assert!(diagnostic.code.as_str().starts_with("K816-K65-"));
        assert!(diagnostic.title.contains("`image`"));
    }
}
