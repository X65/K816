/*****************************************************************************/
/*                                                                           */
/*                              diagnostics.rs                               */
/*                                                                           */
/*        The structured diagnostic family: codes, spans, help, fixes        */
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

//! Diagnostics are the compiler's user interface, so they are tested as
//! data rather than as rendered text: the exact code, the span the caret
//! lands on, and — where a fix is inferable — the edit an editor would
//! apply.
//!
//! What a code *means* is checked in `tests/semantic.rs` next to the
//! behaviour it belongs to. This file checks the properties every
//! diagnostic in the family must have, which is the part that rots
//! silently: a code that points at the wrong span, a `Fix` whose edit does
//! not produce valid source, or a family member that quietly stops firing.

use k816::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Severity};
use k816::parser::parse_source;
use k816::semantic::analyze_file;
use k816::source::{SourceMap, Span};

/// One case: source, the code it must report, and the text the caret covers.
struct Case {
    source: &'static str,
    code: DiagnosticCode,
    /// The exact source text the primary label points at.
    caret: &'static str,
}

/// The structured family C introduces, plus the existing codes it gives real
/// users. Every one of these was unreachable, or reachable only by accident,
/// before the native translator landed.
const FAMILY: &[Case] = &[
    Case {
        source: "func main @a8 { jmp dp $10 }\n",
        code: DiagnosticCode::INVALID_ENCODING,
        caret: "dp",
    },
    Case {
        source: "func f @a8 {\n    return\n}\nvar X = f + 1\n",
        code: DiagnosticCode::NOT_CONSTANT,
        caret: "f + 1",
    },
    Case {
        source: "var SCREEN = $0400\nfunc main @a8 { lda dp SCREEN }\n",
        code: DiagnosticCode::ADDRESS_RANGE,
        caret: "SCREEN",
    },
    Case {
        source: "const K = $10\nfunc main @a8 { sta K }\n",
        code: DiagnosticCode::CONST_IN_ADDRESS_POSITION,
        caret: "K",
    },
    Case {
        source: "data bad {\n    1 2\n    segment OTHER\n    3\n}\n",
        code: DiagnosticCode::SEGMENT_PLACEMENT,
        caret: "segment OTHER",
    },
    Case {
        source: include_str!("golden/nocross-oversized.k816"),
        code: DiagnosticCode::NOCROSS_TOO_LARGE,
        caret: "nocross",
    },
    Case {
        source: "const F = 2\nfunc main @a8 { lda &F }\n",
        code: DiagnosticCode::OFFSET_MARKER_POSITION,
        caret: "&",
    },
    Case {
        source: "const A = 1\nconst B = 2\ndata d {\n    &<(A | B)\n}\n",
        code: DiagnosticCode::COMPOUND_DATA_VALUE,
        caret: "|",
    },
    Case {
        source: "func helper @a8 {\n    return\n}\nfunc main @a8 { sta helper }\n",
        code: DiagnosticCode::SYMBOL_KIND_MISMATCH,
        caret: "sta",
    },
    Case {
        source: "data pinned {\n    address $5000\n    1\n}\n",
        code: DiagnosticCode::ADDRESS_NEEDS_CONFIG,
        caret: "address $5000",
    },
    Case {
        source: "func main @a8 { inx #1 }\n",
        code: DiagnosticCode::INVALID_OPERAND,
        caret: "1",
    },
    Case {
        source: "func main @a8 { lda #$100 }\n",
        code: DiagnosticCode::IMMEDIATE_RANGE,
        caret: "$100",
    },
    Case {
        source: "func main { lda #1 }\n",
        code: DiagnosticCode::UNKNOWN_WIDTH,
        caret: "1",
    },
    Case {
        source: "func main @a8 { lda MISSING }\n",
        code: DiagnosticCode::UNDECLARED_SYMBOL,
        caret: "MISSING",
    },
    Case {
        source: "var A = $10\nvar A = $20\n",
        code: DiagnosticCode::DUPLICATE_SYMBOL,
        caret: "A",
    },
    // The width family, sub-plan D.
    Case {
        source: "func main {\n    nop\n}\n\n@a16\n",
        code: DiagnosticCode::MODULE_WIDTH_PLACEMENT,
        caret: "@a16",
    },
    Case {
        source: "func main @a8 @a16 {\n    nop\n}\n",
        code: DiagnosticCode::CONFLICTING_WIDTH,
        caret: "@a16",
    },
    Case {
        source: "func f @a8 {\n    bcc .wide\n    sep #$20\n    rts\n.wide:\n    rep #$20\n    rts\n}\n",
        code: DiagnosticCode::DIVERGENT_EXIT,
        caret: "rts",
    },
    Case {
        source: "func f @a8 -> @a16 {\n    nop\n}\n",
        code: DiagnosticCode::EXIT_WIDTH_MISMATCH,
        caret: "nop",
    },
    Case {
        source: "func f @a8 @i8 {\n    plp\n}\n",
        code: DiagnosticCode::ASSUMED_EXIT,
        caret: "plp",
    },
    Case {
        source: "func f @a8 (#factor) {\n    nop\n}\n",
        code: DiagnosticCode::ALIAS_NOT_INLINE,
        caret: "factor",
    },
    Case {
        source: "func add @a8 (a) -> a {\n    nop\n}\n\nfunc f @a8 {\n    add\n}\n",
        code: DiagnosticCode::CALL_FLOW_MISMATCH,
        caret: "add",
    },
    Case {
        source: "func callee @a8 {\n    nop\n}\n\nfunc f @a16 {\n    jsr callee\n}\n",
        code: DiagnosticCode::UNBRIDGED_NATIVE_CALL,
        caret: "jsr callee",
    },
    // The chain family: a transfer the CPU does not have, a register memory
    // cannot reach, and a store with no register to carry the value.
    Case {
        source: "func f @a8 @i8 {\n    s=y\n}\n",
        code: DiagnosticCode::NO_SUCH_INSTRUCTION,
        caret: "s",
    },
    Case {
        source: "var mem = $10\nfunc f @a8 @i8 {\n    d = mem\n}\n",
        code: DiagnosticCode::NO_SUCH_INSTRUCTION,
        caret: "d",
    },
    Case {
        source: "var mem = $10\nfunc f @a8 @i8 {\n    mem = 5\n}\n",
        code: DiagnosticCode::NO_SUCH_INSTRUCTION,
        caret: "5",
    },
    // The jump family: a `goto` reaching into another function, and the two
    // calling conventions crossed.
    Case {
        source: "func one @a8 {\n.loop:\n    nop\n}\n\nfunc two @a8 {\n    goto .loop\n}\n",
        code: DiagnosticCode::GOTO_FOREIGN_LABEL,
        caret: ".loop",
    },
    Case {
        source: "far func banked @a8 {\n    nop\n}\n\nfunc f @a8 {\n    call banked\n}\n",
        code: DiagnosticCode::CALL_CONVENTION_MISMATCH,
        caret: "banked",
    },
    Case {
        source: "func near_one @a8 {\n    nop\n}\n\nfunc f @a8 {\n    call far near_one\n}\n",
        code: DiagnosticCode::CALL_CONVENTION_MISMATCH,
        caret: "near_one",
    },
    // The section family: a `naked` section declaring an exit it cannot have,
    // and a pair of `inline`s that expand into each other.
    Case {
        source: "naked f @a8 -> @a8 a {\n    nop\n}\n",
        code: DiagnosticCode::NAKED_EXIT_CONTRACT,
        caret: "-> @a8 a",
    },
    Case {
        source: "inline one @a8 @i8 {\n    two\n}\n\ninline two @a8 @i8 {\n    one\n}\n",
        code: DiagnosticCode::INLINE_RECURSION,
        caret: "one",
    },
    // The declaration family: the storage class with no allocator, the pool
    // with a size, an index past what a count reserved, and the two ways a
    // datum can be the wrong width for the register reading it.
    Case {
        source: "var far rom\n",
        code: DiagnosticCode::FAR_NEEDS_ADDRESS,
        caret: "far",
    },
    Case {
        source: "var dp first[200]\nvar dp second[100]\n",
        code: DiagnosticCode::DIRECT_PAGE_OVERFLOW,
        caret: "var dp second[100]",
    },
    Case {
        source: "var dp late:word = $FF\n",
        code: DiagnosticCode::DIRECT_PAGE_OVERFLOW,
        caret: "$FF",
    },
    Case {
        source: "var slots:word * 4\n\nfunc f @a16 @i8 {\n    a = slots[4]\n}\n",
        code: DiagnosticCode::SUBSCRIPT_OUT_OF_RANGE,
        caret: "4",
    },
    Case {
        source: "var w:word\n\nfunc f @a8 @i8 {\n    a = w\n}\n",
        code: DiagnosticCode::DATA_WIDTH_MISMATCH,
        caret: "w",
    },
    Case {
        source: "var ptr:far\n\nfunc f @a16 @i8 {\n    a = ptr\n}\n",
        code: DiagnosticCode::DATA_WIDTH_MISMATCH,
        caret: "ptr",
    },
    Case {
        source: "var n\nvar here\n\nfunc f @a8 @i8 {\n    a = n[here]\n}\n",
        code: DiagnosticCode::SUBSCRIPT_NOT_CONSTANT,
        caret: "here",
    },
];

/// Analyzes `source` and returns the diagnostic carrying `code`.
fn diagnostic_for(source: &str, code: DiagnosticCode) -> (SourceMap, Diagnostic) {
    let mut map = SourceMap::new();
    let id = map.add_file("case.k816", source);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "`{source}` did not parse");
    let (_, diagnostics) = analyze_file(&parse.file);
    let found = diagnostics
        .into_iter()
        .find(|diagnostic| diagnostic.code == code)
        .unwrap_or_else(|| panic!("`{source}` did not report {}", code.as_str()));
    (map, found)
}

#[test]
fn every_diagnostic_points_at_the_construct_it_is_about() {
    for case in FAMILY {
        let (map, diagnostic) = diagnostic_for(case.source, case.code);
        let span = diagnostic.primary.span;
        assert_eq!(
            map.span_text(Span::new(span.file, span.start, span.end)),
            case.caret,
            "{} points at the wrong text",
            case.code.as_str()
        );
        assert!(
            !diagnostic.primary.message.is_empty(),
            "{} has an empty primary label",
            case.code.as_str()
        );
    }
}

#[test]
fn every_diagnostic_says_something_beyond_its_title() {
    // A code alone is not a diagnostic: each one carries at least a note, a
    // help line, a docs reference, or a secondary label a reader can follow.
    for case in FAMILY {
        let (_, diagnostic) = diagnostic_for(case.source, case.code);
        assert!(
            !diagnostic.notes.is_empty()
                || diagnostic.help.is_some()
                || diagnostic.docs.is_some()
                || !diagnostic.secondary.is_empty(),
            "{} explains nothing",
            case.code.as_str()
        );
    }
}

/// Every code the width model introduces is reachable from source, which is
/// what keeps a registered code from quietly becoming unused.
///
/// `W0123` is the exception, and deliberately: damage tracking is computed
/// after lowering, so it cannot be reached through `analyze_file` the way
/// every case here is. `tests/width.rs` drives it through the full pipeline.
#[test]
fn every_width_code_is_reachable() {
    for code in [
        DiagnosticCode::MODULE_WIDTH_PLACEMENT,
        DiagnosticCode::CONFLICTING_WIDTH,
        DiagnosticCode::DIVERGENT_EXIT,
        DiagnosticCode::EXIT_WIDTH_MISMATCH,
        DiagnosticCode::ASSUMED_EXIT,
        DiagnosticCode::ALIAS_NOT_INLINE,
        DiagnosticCode::CALL_FLOW_MISMATCH,
        DiagnosticCode::UNBRIDGED_NATIVE_CALL,
    ] {
        assert!(
            FAMILY.iter().any(|case| case.code == code),
            "{} has no case in the family",
            code.as_str()
        );
    }
}

/// The conditions K816 compiles *through* are exactly the ones where the
/// program is still well defined and the author has something to decide.
///
/// `address` placement emits its segment and tells the user what to add to
/// the config. An assumed exit is a case where the emitted code is correct —
/// the language rule carries the truth — and what is wrong is that the source
/// reads as if something else were true.
///
/// A width block a branch left used to be on this list. Sub-plan E4 replaced
/// that warning with a synthesized restore on the taken edge, so there is
/// nothing left to warn about (`postmortem.md` §D.6.2).
#[test]
fn only_the_compile_through_conditions_are_warnings() {
    const WARNINGS: &[DiagnosticCode] = &[
        DiagnosticCode::ADDRESS_NEEDS_CONFIG,
        DiagnosticCode::ASSUMED_EXIT,
        DiagnosticCode::UNBRIDGED_NATIVE_CALL,
    ];
    for case in FAMILY {
        let (_, diagnostic) = diagnostic_for(case.source, case.code);
        let expected = if WARNINGS.contains(&case.code) {
            Severity::Warning
        } else {
            Severity::Error
        };
        assert_eq!(diagnostic.severity, expected, "{}", case.code.as_str());
    }
}

/// Applicability is a promise about what an editor may do unattended, so
/// the two levels are held to different standards: a machine-applicable fix
/// has to leave source that *compiles*, while a fix that needs review only
/// has to leave source that parses. Both are checked by applying the edits
/// and re-analyzing, which is the only way to know a suggestion was more
/// than plausible.
#[test]
fn applying_a_fix_leaves_source_its_applicability_promises() {
    for case in FAMILY {
        let (_, diagnostic) = diagnostic_for(case.source, case.code);
        for fix in &diagnostic.fixes {
            assert!(!fix.title.is_empty(), "{}", case.code.as_str());

            // Apply the edits back to front, so earlier offsets stay valid.
            let mut fixed = case.source.to_owned();
            let mut edits = fix.edits.clone();
            edits.sort_by_key(|edit| std::cmp::Reverse(edit.span.start));
            for edit in edits {
                let range = edit.span.start as usize..edit.span.end as usize;
                fixed.replace_range(range, &edit.replacement);
            }

            let mut map = SourceMap::new();
            let id = map.add_file("fixed.k816", fixed.clone());
            let parse = parse_source(map.file(id), None);
            assert_eq!(
                parse.diagnostics,
                vec![],
                "the {} fix did not parse: {fixed}",
                case.code.as_str()
            );
            if fix.applicability != Applicability::MachineApplicable {
                continue;
            }
            let (_, diagnostics) = analyze_file(&parse.file);
            assert_eq!(
                diagnostics,
                vec![],
                "applying the machine-applicable {} fix left `{fixed}` broken",
                case.code.as_str()
            );
        }
    }
}

/// A fix is only machine-applicable when it cannot make things worse.
/// `sta #K` is not a store, so the immediate is offered for a load and
/// withheld for anything that has no immediate form.
#[test]
fn the_immediate_fix_is_offered_only_where_an_immediate_exists() {
    let (_, load) = diagnostic_for(
        "const K = $10\nfunc main @a8 { lda K }\n",
        DiagnosticCode::CONST_IN_ADDRESS_POSITION,
    );
    assert_eq!(load.fixes.len(), 1);
    assert_eq!(
        load.fixes[0].applicability,
        Applicability::MachineApplicable
    );

    let (_, store) = diagnostic_for(
        "const K = $10\nfunc main @a8 { sta K }\n",
        DiagnosticCode::CONST_IN_ADDRESS_POSITION,
    );
    assert!(store.fixes.is_empty(), "`sta #K` is not a store: {store:?}");
    assert!(
        store
            .help
            .as_deref()
            .is_some_and(|help| help.contains("`var`")),
        "the help points somewhere useful instead: {store:?}"
    );
}

#[test]
fn a_cross_unit_duplicate_names_both_definitions() {
    let mut map = SourceMap::new();
    let file = map.add_file("a.k816", "func shared @a8 {\n    nop\n}\n");
    map.add_file("b.k816", "func shared @a8 {\n    nop\n}\n");
    let result = k816::compile_to_ca65(&map, file, &k816::Options::default());

    let duplicate = result
        .diagnostics
        .iter()
        .find(|d| d.code == DiagnosticCode::DUPLICATE_SYMBOL)
        .expect("a cross-unit duplicate");
    assert_eq!(duplicate.secondary.len(), 1);
    assert_ne!(
        duplicate.primary.span.file, duplicate.secondary[0].span.file,
        "the two definitions are in different units, and both are shown"
    );
}

#[test]
fn an_illegal_operand_form_lists_what_the_instruction_accepts() {
    // "not legal here" is not actionable on its own; the modes are.
    let (_, diagnostic) = diagnostic_for(
        "func main @a8 { inx #1 }\n",
        DiagnosticCode::INVALID_OPERAND,
    );
    assert!(
        diagnostic.notes.iter().any(|note| note.contains("accepts")),
        "{diagnostic:?}"
    );
    assert_eq!(
        diagnostic.secondary.len(),
        1,
        "the mnemonic is pointed at too"
    );
}

#[test]
fn diagnostics_survive_past_the_first_error() {
    // Tools need everything wrong with a file, not the first thing.
    let mut map = SourceMap::new();
    let id = map.add_file(
        "many.k816",
        "func main @a8 {\n    lda FIRST\n    lda SECOND\n    lda THIRD\n}\n",
    );
    let parse = parse_source(map.file(id), None);
    let (_, diagnostics) = analyze_file(&parse.file);
    let undeclared: Vec<&str> = diagnostics
        .iter()
        .filter(|d| d.code == DiagnosticCode::UNDECLARED_SYMBOL)
        .map(|d| d.title.as_str())
        .collect();
    assert_eq!(
        undeclared,
        [
            "`FIRST` is not declared",
            "`SECOND` is not declared",
            "`THIRD` is not declared",
        ]
    );
}
