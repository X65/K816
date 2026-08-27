/*****************************************************************************/
/*                                                                           */
/*                                 compat.rs                                 */
/*                                                                           */
/*        Diagnostics for K65 constructs K816 removes or refuses             */
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

//! Diagnostics for K65 syntax K816 recognizes and does not compile.
//!
//! The grammar recognizes the whole K65/K816 surface (sub-plan B), which is
//! deliberately wider than what K816 *means*: `image` is removed, `bank` is
//! removed, three evaluator functions are refused, and five HLA forms spell
//! undocumented 6502 opcodes. Every one of them is a specific named node with
//! a diagnostic of its own, which is decision 0's second clause — a K65 form
//! K816 does not implement never comes out as a generic parse error.
//!
//! There is no longer a "not implemented yet" constructor here. It existed
//! while the sub-plans were landing and its last producer went in G6; a
//! construct that parses and has no meaning is now a compiler bug, not a
//! schedule.

use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::source::Span;

/// `image`, in every one of its spellings, is removed.
///
/// A *removal*, not a reservation: an asset pipeline is outside what the
/// compiler does (`doc/design.md` §1), no BMP decoder is cheap by any
/// reading of decision 0, and nothing later in the plan brings the construct
/// back. So the wording says so, and the help names the replacement — convert
/// the bitmap outside the build and include the result with `binary`
/// (`plans/06-compatibility.md` decision 7). No adapter tool ships with the
/// compiler.
///
/// `form` is the spelling the source used, so a source that wrote `tiles` or
/// `imgwave` reads its own word back.
pub fn k65_removed_image(span: Span, form: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::K65_RESERVED_IMAGE,
        format!("K65 `{form}` is not a K816 construct"),
        Label::new(span, "K816 has no image reader"),
    )
    .with_note(
        "reading a bitmap and scanning it into tiles is an asset pipeline, which K816 \
         does not own (`doc/design.md` §1)",
    )
    .with_help("convert the image outside the build and include the bytes with `binary`")
    .with_docs("doc/k65-compat.md")
}

/// One of the three evaluator functions K816 does not run.
///
/// A *refusal*, not a reservation: nothing later brings `rnd` back, and the
/// help names what to do instead. `print` and `error` left this family in G4
/// and `size` in G6 — all three are implemented — so what is left is
/// `addbyte`, which mutates a section; `rnd`, which is not deterministic; and
/// `color`, which is an asset pipeline's (`plans/06-compatibility.md`
/// decision 6).
pub fn k65_refused_eval_function(span: Span, name: &str) -> Diagnostic {
    let diagnostic = Diagnostic::error(
        DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
        format!("K65's evaluator function `{name}` is not in K816"),
        Label::new(span, "this call cannot be compiled"),
    );
    match name {
        // Cheap to implement and refused anyway: a seeded generator would
        // give a program bytes the original never produced twice, and an
        // unseeded one would give it different bytes every build.
        "rnd" => diagnostic
            .with_note(
                "a compiler that generates a different table on each build is a compiler \
                 whose output cannot be compared with itself",
            )
            .with_help("generate the table outside the build and include it with `binary`"),
        // Palette conversion is an asset pipeline's, like `image` — and it
        // was answering with the section-mutation note until G6 trimmed this
        // function to three names and the wrong sentence became visible.
        "color" => diagnostic
            .with_note(
                "picking a palette index for a colour is an asset pipeline's job, and K816 \
                 does not own one (`doc/design.md` §1)",
            )
            .with_help("convert the palette outside the build and include it with `binary`"),
        // Section mutation is outside what the compiler does at all.
        _ => diagnostic.with_note(
            "K816 does not let a compile-time evaluation change a section's contents \
             (`doc/design.md` §1)",
        ),
    }
    .with_docs("doc/syntax-reference.md#functions")
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
    use super::{k65_illegal_opcode, k65_removed_bank, k65_removed_image};
    use crate::diagnostic::DiagnosticCode;
    use crate::source::{FileId, SourceMap, Span};

    fn span() -> Span {
        let mut map = SourceMap::new();
        let id: FileId = map.add_file("t.k65", "data X { }");
        Span::new(id, 0, 4)
    }

    #[test]
    fn image_is_removed_rather_than_pending() {
        let diagnostic = k65_removed_image(span(), "tiles");
        assert_eq!(diagnostic.code, DiagnosticCode::K65_RESERVED_IMAGE);
        // The source's own word, so an author who wrote `tiles` reads
        // `tiles` back rather than a keyword they did not type.
        assert!(diagnostic.title.contains("`tiles`"));
        // Nothing here may read as a promise: there is no image reader
        // coming, and the help names what to do instead.
        assert!(!diagnostic.title.contains("yet"));
        assert!(
            diagnostic
                .help
                .is_some_and(|help| help.contains("`binary`"))
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
    fn every_refusal_uses_the_k65_family() {
        for diagnostic in [
            k65_removed_image(span(), "image"),
            k65_removed_bank(span(), "core"),
            k65_illegal_opcode(span(), "a &. n", "ANC", "`PHD`"),
        ] {
            assert!(
                diagnostic.code.as_str().starts_with("K816-K65-"),
                "{} is not a compatibility code",
                diagnostic.code.as_str()
            );
        }
    }
}
