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
use k816::source::{Provenance, SourceMap, Span};

#[path = "common/corpus.rs"]
mod corpus_files;

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
        source: "func work @a8 { jmp dp $10 }\n",
        code: DiagnosticCode::INVALID_ENCODING,
        caret: "dp",
    },
    // A flag letter's stack shorthand: `PLP`/`PHP` in the original, refused
    // by name here so that a valid K65 line is not a parse error.
    Case {
        source: "main @a8 @i8 {\n    z??\n}\n",
        code: DiagnosticCode::K65_FLAG_STACK_FORM,
        caret: "z??",
    },
    // A `->` clause may name `c z n v` and no other flag: `i` is a mode the
    // caller establishes, not an answer the callee computes.
    Case {
        source: "func f (a) -> a, i {\n    nop\n}\n",
        code: DiagnosticCode::CONTRACT_FLAG_OUTPUT,
        caret: "i",
    },
    // `o` is the original's letter for the overflow flag in this construct
    // and `v` its letter in the branch tests; K816 spells it `v` in both.
    Case {
        source: "main @a8 @i8 {\n    o-\n}\n",
        code: DiagnosticCode::K65_OVERFLOW_FLAG_SPELLING,
        caret: "o-",
    },
    // A K816 spelling decisions 29 and 30 retired: `b` is the accumulator's
    // high half and only an `a~b` operand, so the stack cannot take it, and
    // the register `b!!` used to push is `dbr`.
    Case {
        source: "main @a8 @i8 {\n    b!!\n}\n",
        code: DiagnosticCode::RETIRED_REGISTER_SPELLING,
        caret: "!!",
    },
    Case {
        source: "func f @a8 {\n    return\n}\nvar XX = f + 1\n",
        code: DiagnosticCode::NOT_CONSTANT,
        caret: "f + 1",
    },
    Case {
        source: "var SCREEN = $0400\nfunc work @a8 { lda dp SCREEN }\n",
        code: DiagnosticCode::ADDRESS_RANGE,
        caret: "SCREEN",
    },
    Case {
        source: "const K = $10\nfunc work @a8 { sta K }\n",
        code: DiagnosticCode::CONST_IN_ADDRESS_POSITION,
        caret: "K",
    },
    Case {
        source: "var slot = $80\nfunc work @a8 { lda slot,s }\n",
        code: DiagnosticCode::STACK_RELATIVE_OFFSET,
        caret: "slot",
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
        source: "const F = 2\nfunc work @a8 { lda &F }\n",
        code: DiagnosticCode::OFFSET_MARKER_POSITION,
        caret: "&",
    },
    Case {
        source: "const AA = 1\nconst BB = 2\ndata dd {\n    &<(AA | BB)\n}\n",
        code: DiagnosticCode::COMPOUND_DATA_VALUE,
        caret: "|",
    },
    Case {
        source: "func helper @a8 {\n    return\n}\nfunc work @a8 { sta helper }\n",
        code: DiagnosticCode::SYMBOL_KIND_MISMATCH,
        caret: "sta",
    },
    Case {
        source: "data pinned {\n    address $5000\n    1\n}\n",
        code: DiagnosticCode::ADDRESS_NEEDS_CONFIG,
        caret: "address $5000",
    },
    Case {
        source: "func work @a8 { inx #1 }\n",
        code: DiagnosticCode::INVALID_OPERAND,
        caret: "1",
    },
    Case {
        source: "func work @a8 { lda #$100 }\n",
        code: DiagnosticCode::IMMEDIATE_RANGE,
        caret: "$100",
    },
    // An ordinary function, not `main`: the entry point's widths come from
    // the ABI whether it annotated them or not, so it is the one function
    // that cannot reach this.
    Case {
        source: "func f { lda #1 }\n",
        code: DiagnosticCode::UNKNOWN_WIDTH,
        caret: "1",
    },
    Case {
        source: "func work @a8 { lda MISSING }\n",
        code: DiagnosticCode::UNDECLARED_SYMBOL,
        caret: "MISSING",
    },
    Case {
        source: "var AA = $10\nvar AA = $20\n",
        code: DiagnosticCode::DUPLICATE_SYMBOL,
        caret: "AA",
    },
    // The width family, sub-plan D.
    Case {
        source: "func work {\n    nop\n}\n\n@a16\n",
        code: DiagnosticCode::MODULE_WIDTH_PLACEMENT,
        caret: "@a16",
    },
    Case {
        source: "func work @a8 @a16 {\n    nop\n}\n",
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
        source: "var mem = $10\nfunc f @a8 @i8 {\n    dpr = mem\n}\n",
        code: DiagnosticCode::NO_SUCH_INSTRUCTION,
        caret: "dpr",
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
        source: "var nn\nvar here\n\nfunc f @a8 @i8 {\n    a = nn[here]\n}\n",
        code: DiagnosticCode::SUBSCRIPT_NOT_CONSTANT,
        caret: "here",
    },
    // The layout family: a field list that packs two fields under one name
    // or none at all, a selection with nothing to select from, and the two
    // directions an `abstract var` meets storage.
    Case {
        source: "var vec[ .lo :byte .lo :word ]\n",
        code: DiagnosticCode::DUPLICATE_SYMBOL,
        caret: ".lo",
    },
    Case {
        source: "var vec[ .id :byte .str :byte[0] ]\n",
        code: DiagnosticCode::ELEMENT_COUNT_RANGE,
        caret: "0",
    },
    Case {
        source: "var vec[ .lo :byte ]\n\nfunc f @a8 @i8 {\n    a = vec.hi\n}\n",
        code: DiagnosticCode::UNDECLARED_SYMBOL,
        caret: ".hi",
    },
    // Near enough to suggest, which is the other half of the same
    // diagnostic: the one above lists the fields, this one offers an edit.
    Case {
        source: "var vec[ .state :byte ]\n\nfunc f @a8 @i8 {\n    a = vec.stat\n}\n",
        code: DiagnosticCode::UNDECLARED_SYMBOL,
        caret: ".stat",
    },
    Case {
        source: "var vec[ .a :byte ]\n\nfunc f @a8 @i8 {\n    a = vec.a.b\n}\n",
        code: DiagnosticCode::SYMBOL_KIND_MISMATCH,
        caret: ".b",
    },
    // A declaration's raw span is deliberately absent: `var buf[0]` is a K65
    // declaration the original accepts, so K816 accepts it too
    // (`tests/compat/k65/zero-element-span`). The two forms below have no K65
    // spelling and keep the rule.
    Case {
        source: "var vec[ .a :byte [0] ]\n",
        code: DiagnosticCode::ELEMENT_COUNT_RANGE,
        caret: "0",
    },
    Case {
        source: "var t:byte * 0\n",
        code: DiagnosticCode::ELEMENT_COUNT_RANGE,
        caret: "0",
    },
    Case {
        source: "var vec[ .ptr :far ]\n\nfunc f @a8 @i8 {\n    a = vec.ptr\n}\n",
        code: DiagnosticCode::DATA_WIDTH_MISMATCH,
        caret: "vec.ptr",
    },
    Case {
        source: "var counter\n\nfunc f @a8 @i8 {\n    a = counter.lo\n}\n",
        code: DiagnosticCode::SYMBOL_KIND_MISMATCH,
        caret: ".lo",
    },
    Case {
        source: "var counter\n\nfunc f @a8 @i8 {\n    a = counter:offsetof\n}\n",
        code: DiagnosticCode::SYMBOL_KIND_MISMATCH,
        caret: "offsetof",
    },
    Case {
        source: "var vec[ .lo :byte ]\n\nfunc f @a8 @i8 {\n    a = vec.lo[1]\n}\n",
        code: DiagnosticCode::SUBSCRIPT_OUT_OF_RANGE,
        caret: "1",
    },
    Case {
        source: "abstract var POINT[ .x :byte ]\n\nfunc f @a8 @i8 {\n    a = POINT.x\n}\n",
        code: DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
        caret: "POINT.x",
    },
    Case {
        source: "abstract var dp POINT[ .x :byte ]\n",
        code: DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
        caret: "dp",
    },
    Case {
        source: "abstract var POINT\n",
        code: DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
        caret: "POINT",
    },
    // A `binary` whose file is not where K816 looked. The caret is the path
    // rather than the line: the complaint is about the string.
    Case {
        source: "data blk {\n    binary \"tiles.bin\"\n}\n",
        code: DiagnosticCode::BINARY_NOT_FOUND,
        caret: "\"tiles.bin\"",
    },
    // The data-block family: a character the charset cannot emit, a value
    // too wide for its entry, and the two things a `repeat` body may not
    // contain because each copy would need its own answer.
    Case {
        source: "data dd {\n    charset \" ABC\"\n    \"AZ\"\n}\n",
        code: DiagnosticCode::CHARSET_UNMAPPED,
        caret: "\"AZ\"",
    },
    Case {
        source: "data dd {\n    byte $100\n}\n",
        code: DiagnosticCode::ADDRESS_RANGE,
        caret: "$100",
    },
    Case {
        source: "data dd {\n    repeat 2 {\n        inner: 1\n    }\n}\n",
        code: DiagnosticCode::DUPLICATE_SYMBOL,
        caret: "inner:",
    },
    Case {
        source: "data dd {\n    repeat 2 {\n        align 4\n    }\n}\n",
        code: DiagnosticCode::SEGMENT_PLACEMENT,
        caret: "align 4",
    },
    Case {
        source: "data dd {\n    for i=0..3 eval [ i - 9 ]\n}\n",
        code: DiagnosticCode::ADDRESS_RANGE,
        caret: "i - 9",
    },
    // A section read with no answer, and the one that can never have one.
    Case {
        source: "data dd {\n    [ index(below, 1) ]\n}\ndata below {\n    1\n}\n",
        code: DiagnosticCode::SECTION_NOT_READABLE,
        caret: "index(below, 1)",
    },
    Case {
        source: "data tab {\n    1 2 3\n}\ndata dd {\n    [ index(tab, 1, 0) ]\n}\n",
        code: DiagnosticCode::SECTION_NO_DIMENSIONS,
        caret: "index(tab, 1, 0)",
    },
    // The evaluator family G4 introduces.
    Case {
        source: "data dd {\n    [ print(\"ready\") ]\n}\n",
        code: DiagnosticCode::EVAL_PRINT,
        caret: "print(\"ready\")",
    },
    Case {
        source: "data dd {\n    [ error(\"no configuration\") ]\n}\n",
        code: DiagnosticCode::EVAL_ERROR,
        caret: "error(\"no configuration\")",
    },
    Case {
        source: "data dd {\n    [ SPIN = 0 while 1 { SPIN = SPIN + 1 } ]\n}\n",
        code: DiagnosticCode::EVAL_BUDGET,
        caret: "[ SPIN = 0 while 1 { SPIN = SPIN + 1 } ]",
    },
    Case {
        source: "data dd {\n    [ \"text\" ]\n}\n",
        code: DiagnosticCode::EVAL_NOT_A_NUMBER,
        caret: "[ \"text\" ]",
    },
    Case {
        source: "[ LIMIT = 1 ]\n[ LIMIT = 2 ]\n",
        code: DiagnosticCode::EVAL_REASSIGNED,
        caret: "LIMIT = 2",
    },
    Case {
        source: "data dd {\n    for i=0..3 eval [ missing ]\n}\n",
        code: DiagnosticCode::UNDECLARED_SYMBOL,
        caret: "missing",
    },
    Case {
        source: "#error \"no target system selected\"\n",
        code: DiagnosticCode::PREPROCESSOR_ERROR,
        caret: "#error \"no target system selected\"",
    },
    Case {
        source: "#warn \"the slow path is compiled in\"\n",
        code: DiagnosticCode::PREPROCESSOR_WARNING,
        caret: "#warn \"the slow path is compiled in\"",
    },
    Case {
        source: "#if 1\nvar SPEED = 1\n",
        code: DiagnosticCode::PREPROCESSOR_UNBALANCED,
        caret: "#if",
    },
    // The two branch conditions the 65816 has no single instruction for.
    // The caret is the operator and not the block it governs, because the
    // operator is what the author would change.
    Case {
        source: "func main @a8 @i8 {\n    {\n        x++\n    } >\n}\n",
        code: DiagnosticCode::INEFFICIENT_BRANCH,
        caret: ">",
    },
    // The entry point's widths are the environment's, so an annotation on
    // `main` is a claim about them rather than a request of them.
    Case {
        source: "func main @a16 {\n    nop\n}\n",
        code: DiagnosticCode::ENTRY_ABI_MISMATCH,
        caret: "@a16",
    },
    // An `inline`'s labels are renamed per use site, so neither a unit-level
    // one nor an expression mentioning one has anything to name.
    Case {
        source: "inline probe @a8 {\nspot:\n    nop\n}\n",
        code: DiagnosticCode::DUPLICATE_SYMBOL,
        caret: "spot",
    },
    Case {
        source: "inline probe @a8 {\n.spot:\n    lda #.spot\n}\n",
        code: DiagnosticCode::SYMBOL_KIND_MISMATCH,
        caret: ".spot",
    },
    // The zero-store shortcut is for a declared name, not for arithmetic
    // that happens to address one.
    Case {
        source: "var mem = $0400\nfunc f @a8 @i8 {\n    mem+1 = 0\n}\n",
        code: DiagnosticCode::NO_SUCH_INSTRUCTION,
        caret: "0",
    },
    // Sub-plan G's refusals. An `align` whose congruence class is empty, the
    // removed `bank`, and the two of the five illegal-opcode forms that
    // stand for the three shapes the grammar has.
    Case {
        source: "data dd {\n    align 3\n    1 2\n}\n",
        code: DiagnosticCode::ALIGN_UNREACHABLE,
        caret: "3",
    },
    Case {
        source: "data dd {\n    align 16 + 16\n    1 2\n}\n",
        code: DiagnosticCode::ALIGN_UNREACHABLE,
        caret: "16",
    },
    Case {
        source: "bank core\n",
        code: DiagnosticCode::K65_REMOVED_BANK,
        caret: "bank",
    },
    Case {
        source: "func f @a8 @i8 {\n    a&.#$0f\n}\n",
        code: DiagnosticCode::K65_UNSUPPORTED_ILLEGAL_OPCODE,
        caret: "&.",
    },
    // A placement directive that is not the section's first statement is the
    // same complaint a misplaced `segment` gets — and an `inline` is not a
    // section at all, so its body cannot carry one anywhere.
    Case {
        source: "func f @a8 @i8 {\n    nop\n    align 16\n}\n",
        code: DiagnosticCode::SEGMENT_PLACEMENT,
        caret: "align 16",
    },
    Case {
        source: "inline f @a8 @i8 {\n    align 16\n    nop\n}\n",
        code: DiagnosticCode::SEGMENT_PLACEMENT,
        caret: "align 16",
    },
];

/// Codes no `FAMILY` row can reach, and what covers them instead.
///
/// Two reasons a code is here rather than above: it is reported by a stage
/// `diagnostic_for` does not run — syntax diagnostics come from the parser,
/// and two of E's come from lowering — or it cannot be reached from source at
/// all, in which case the entry says why and that claim is the coverage.
const ACCOUNTED_ELSEWHERE: &[(&str, &str)] = &[
    (
        "K816-E0003",
        "tests/parser.rs: a_literal_too_big_for_32_bits_is_refused_where_it_is_written",
    ),
    (
        "K816-E0001",
        "tests/parser.rs: malformed_statement_yields_navigable_ast_after_first_error",
    ),
    (
        "K816-E0002",
        "tests/parser.rs: missing_brace_gets_a_machine_applicable_fix",
    ),
    (
        "K816-E0128",
        "tests/parser.rs: control_flow_statements_compile_or_name_their_owner",
    ),
    (
        "K816-W0123",
        "tests/compile.rs: a_call_that_damages_a_live_register_warns_at_the_call",
    ),
    (
        "K816-E0130",
        "tests/compile.rs: a_nocross_run_blames_the_split_that_grew_it",
    ),
    // Two units, so it cannot be a one-source `Case`: the code exists to say
    // that an `inline` borrowed from another unit does not compile there.
    (
        "K816-E0156",
        "tests/compile.rs: a_borrowed_body_is_diagnosed_by_the_unit_that_wrote_it",
    ),
    (
        "K816-K65-0001",
        "tests/parser.rs: a_nested_reserved_construct_reports_its_own_k65_code",
    ),
    (
        "K816-K65-0002",
        "tests/parser.rs: a_nested_reserved_construct_reports_its_own_k65_code",
    ),
    (
        "K816-E0105",
        "tests/diagnostics.rs: a_misspelled_mnemonic_is_not_an_unknown_mnemonic",
    ),
    (
        "K816-E0900",
        "unreachable by design: it claims a K816 bug, and the builder stays \
         quiet about shapes tree-sitter recovered — see \
         truncating_a_fixture_anywhere_yields_a_usable_tree",
    ),
    // Both need `--strict-k65`, which no `Case` here turns on: a `Case` is a
    // source compiled with the default options, and these two are the one
    // option that changes what the compiler reports.
    (
        "K816-E0160",
        "tests/cli.rs: strict_k65_is_opt_in_and_reaches_both_commands",
    ),
    (
        "K816-W0161",
        "tests/compat.rs: strict_mode_reports_exactly_the_rows_the_manifest_marks",
    ),
    // Reported while the *workspace* is resolved, which is a stage
    // `diagnostic_for` does not run: it analyzes one file with no `Linkage`,
    // and which units a program has is exactly what a `Linkage` knows.
    (
        "K816-E0166",
        "tests/modules.rs: an_import_that_cannot_be_read_is_e0166_at_the_string",
    ),
    // The same, and for the same reason: whether the program defines a name
    // some unit declared `extern`, and whether two units declared it
    // differently, are questions about a set of units rather than about a
    // file.
    (
        "K816-E0164",
        "tests/ffi.rs: an_extern_the_program_also_defines_is_refused",
    ),
    (
        "K816-E0165",
        "tests/ffi.rs: externs_that_disagree_are_refused_and_the_message_says_how",
    ),
    // Reachable from one file, but only through a construct `analyze_file`
    // declares nothing for: `FAMILY` compiles a snippet with no workspace,
    // and it is `declare_extern` that puts a valueless `const` in the table.
    (
        "K816-E0167",
        "tests/ffi.rs: every_compile_time_position_refuses_a_link_time_const",
    ),
    // And this one no source can reach at all: it reports a `--entry` value,
    // so there is no file for the FAMILY table to compile.
    (
        "K816-E0162",
        "tests/cli.rs: the_entry_flag_refuses_a_spelling_that_names_no_machine",
    ),
    // Reachable only under `--entry emulation`, so `FAMILY`'s
    // default-options compile cannot produce it either.
    (
        "K816-E0163",
        "tests/abi.rs: a_widening_bridge_is_refused_where_emulation_mode_is_known",
    ),
];

/// `K816-E0105` is unreachable from source, and this is why.
///
/// The grammar's `mnemonic` token matches real 65816 spellings only, so a
/// misspelling is lexed as an identifier and reports as an undeclared symbol
/// — a better diagnostic than "unknown mnemonic" would have been. The arm in
/// `resolve.rs` stays as a guard against a grammar change that would let a
/// non-mnemonic through, and this test is what would notice such a change:
/// it starts failing the day the spelling reaches the ISA table.
#[test]
fn a_misspelled_mnemonic_is_not_an_unknown_mnemonic() {
    for source in [
        "func f @a8 @i8 {\n    xyz\n}\n",
        "func f @a8 @i8 {\n    ldaa $10\n}\n",
        "func f @a8 @i8 {\n    lfa $10\n}\n",
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("typo.k816", source);
        let parse = parse_source(map.file(id), None);
        let (_, analysis) = analyze_file(&parse.file);
        // Both stages, because the refusal comes from whichever one meets it
        // first: a bare misspelling is an undeclared symbol, and one with an
        // operand does not parse as a statement at all.
        let codes: Vec<&str> = parse
            .diagnostics
            .iter()
            .chain(analysis.iter())
            .map(|diagnostic| diagnostic.code.as_str())
            .collect();
        assert!(
            !codes.contains(&"K816-E0105"),
            "`{source}` reached the unknown-mnemonic arm: {codes:?}"
        );
        assert!(
            !codes.is_empty(),
            "`{source}` has to be refused as *something*"
        );
    }
}

/// Every code `src/diagnostic.rs` registers is exercised, or says why not.
///
/// The registry is read from the source file rather than imported, for the
/// same reason `tests/doc_coverage.rs` reads its document: the list has to be
/// the one the compiler actually ships, and a copy in a test is a copy that
/// can be forgotten. Adding a code now forces the choice — write a case, name
/// the test that covers it, or state why nothing can reach it — which is the
/// gate that was missing when three codes were registered with no coverage at
/// all.
#[test]
fn every_registered_code_is_exercised_or_declared_unreachable() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/diagnostic.rs");
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));

    // `pub const NAME: Self = Self("K816-…");` — the one shape a code is
    // declared in.
    let registered: Vec<String> = source
        .lines()
        .filter_map(|line| line.trim().strip_prefix("pub const "))
        .filter_map(|line| line.split("Self(\"").nth(1))
        .filter_map(|line| line.split('"').next())
        .map(str::to_owned)
        .collect();
    assert!(
        registered.len() > 40,
        "the registry parse found only {} codes, which means the shape changed",
        registered.len()
    );

    let in_family = |code: &str| FAMILY.iter().any(|case| case.code.as_str() == code);
    let accounted = |code: &str| ACCOUNTED_ELSEWHERE.iter().any(|(named, _)| *named == code);

    let missing: Vec<&String> = registered
        .iter()
        .filter(|code| !in_family(code) && !accounted(code))
        .collect();
    assert!(
        missing.is_empty(),
        "registered codes nothing exercises: {missing:?}\n\
         add a `Case` to FAMILY, or an ACCOUNTED_ELSEWHERE entry naming the test \
         that covers it or the reason nothing can reach it"
    );

    // The reverse, so the table cannot outlive what it describes.
    for (code, why) in ACCOUNTED_ELSEWHERE {
        assert!(
            registered.iter().any(|registered| registered == code),
            "{code} is accounted for but no longer registered"
        );
        assert!(
            !in_family(code),
            "{code} is in FAMILY *and* listed as covered elsewhere ({why})"
        );
        assert!(!why.is_empty(), "{code} says nothing about its coverage");
        // A named test has to exist, or the table is describing coverage
        // that a rename quietly took away.
        if let Some((file, name)) = why.split_once(": ")
            && let Some(file) = file.strip_prefix("tests/")
        {
            let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests")
                .join(file);
            let source =
                std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
            assert!(
                source.contains(&format!("fn {name}(")),
                "{code} names `{name}` in {file}, which has no such test"
            );
        }
    }
}

/// Analyzes `source`, lowers it if analysis was clean, and returns the
/// diagnostic carrying `code`.
///
/// **Lowering is part of the walk** because some codes are only decided
/// there: what a branch condition expands to is not known until the plan is
/// chosen, and `K816-W0157` is that. Before G8 every such code went to
/// [`ACCOUNTED_ELSEWHERE`], which buys the "is it exercised" gate and gives up
/// the other four — the caret, the notes, the severity, the fixes. Running
/// the stage costs one call and buys them back.
fn diagnostic_for(source: &str, code: DiagnosticCode) -> (SourceMap, Diagnostic) {
    let mut map = SourceMap::new();
    let id = map.add_file("case.k816", source);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "`{source}` did not parse");
    let (model, diagnostics) = analyze_file(&parse.file);
    let mut diagnostics = diagnostics;
    // Only a clean analysis may be lowered: lowering a body analysis rejected
    // is what `Lowering::skipped` exists to report, and its report is a
    // different code.
    if !diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == Severity::Error)
    {
        let (_, lowered) = k816::lower::lower(model.program(&parse.file), &model);
        diagnostics.extend(lowered);
    }
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
///
/// The efficiency warning is the clearest member: `> ` and `<=` compile, and
/// what they emit is right. It is a *cost*, and the author is the one who can
/// decide whether to pay it (`plans/06-compatibility.md` decision 23).
#[test]
fn only_the_compile_through_conditions_are_warnings() {
    const WARNINGS: &[DiagnosticCode] = &[
        DiagnosticCode::ADDRESS_NEEDS_CONFIG,
        DiagnosticCode::ASSUMED_EXIT,
        DiagnosticCode::UNBRIDGED_NATIVE_CALL,
        DiagnosticCode::PREPROCESSOR_WARNING,
        DiagnosticCode::INEFFICIENT_BRANCH,
    ];
    // `print(msg)` is the one diagnostic the *source* asked for. Nothing
    // about the program is wrong, so it is neither an error nor a warning,
    // and its code says which before anyone looks it up.
    const NOTES: &[DiagnosticCode] = &[DiagnosticCode::EVAL_PRINT];
    for case in FAMILY {
        let (_, diagnostic) = diagnostic_for(case.source, case.code);
        let expected = if WARNINGS.contains(&case.code) {
            Severity::Warning
        } else if NOTES.contains(&case.code) {
            Severity::Note
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
/// A stack-relative operand is a number, and a memory location is refused.
///
/// The 65816 encodes `expr,S` as one byte counted from the stack pointer, so
/// the operand names a place in a frame rather than a location in memory.
/// That inverts the `const` rule one line above: here a `const` is exactly
/// what belongs and a `var` is the mistake.
///
/// It has to be an *error*. K816 does not choose addresses, so a `var` in the
/// offset would reach ca65 as a symbol, ld65 would resolve it, and the frame
/// would be indexed by the low byte of an address nobody picked - a wrong
/// program with nothing said. The note is where that is spelled out, so it is
/// asserted rather than left to the wording.
#[test]
fn a_memory_location_is_not_a_stack_offset() {
    for source in [
        "var slot = $80\nfunc main @a8 @i8 { lda slot,s }\n",
        "var slot = $80\nfunc main @a8 @i8 { a = (slot,s),y }\n",
    ] {
        let (_, diagnostic) = diagnostic_for(source, DiagnosticCode::STACK_RELATIVE_OFFSET);
        assert!(
            diagnostic
                .notes
                .iter()
                .any(|note| note.contains("ld65") && note.contains("slot")),
            "the note has to say why silence would be worse: {diagnostic:?}"
        );
    }
}

/// The other half of the same rule: a `const` offset is not an address error.
///
/// `K816-E0110` fires wherever a `const` stands in for a location. A stack
/// offset is the one position where the opposite holds, and the fixture the
/// legacy compiler kept for it (`syntax-native-native`) is what says so:
/// `const nameptr = $12` with `lda nameptr,s` assembles to `A3 12`.
#[test]
fn a_const_is_what_a_stack_offset_wants() {
    let mut map = SourceMap::new();
    let id = map.add_file(
        "case.k816",
        "const nameptr = $12\nfunc main @a8 @i8 { lda nameptr,s }\n",
    );
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![]);
    let (_, diagnostics) = analyze_file(&parse.file);
    assert_eq!(
        diagnostics,
        vec![],
        "a `const` is the offset this mode takes"
    );
}

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

// ---------------------------------------------------------------------------
// The reference index (sub-plan H, task H3, decision 11)
// ---------------------------------------------------------------------------

/// Every name a corpus program writes is explained.
///
/// **The inventory comes from the enumerable side.** The reference index an
/// editor reads is filled at the point a name *resolves*, and those points
/// are scattered across `resolve.rs`, `semantic.rs`, `eval.rs` and
/// `width.rs`; counting them cannot say whether one was missed. Walking every
/// name a program writes can, because `names::visit_names` is exhaustive by
/// construction — every match in it lists its variants, so a construct added
/// later cannot carry a name the walk skips
/// (`memory/inventory-from-the-enumerable-side`).
///
/// A name is explained when it is one of five things, and the five are the
/// whole list:
///
/// - a **declaration**, which is what the index points *at* rather than from;
/// - a **fixed** slot — a mnemonic, a flag letter, a segment name — whose
///   vocabulary no declaration can extend;
/// - a name with an **index entry**, which is the case this gate exists for;
/// - a name under a **diagnostic**, because a name the compiler refused has
///   no symbol to record;
/// - a **synthetic** name, which the parser invented for a `MISSING` node and
///   which covers no source text.
///
/// An evaluator name gets one more: it may resolve against the evaluator's
/// own environment, and a binding whose value was not a number never becomes
/// a symbol at all. It is explained when the unit's table does not hold the
/// name either — which is what says the evaluator, and not the compiler,
/// owns it.
#[test]
fn every_name_a_corpus_program_writes_is_explained() {
    use corpus_files::{corpus, root};
    use k816::names::{NameRole, visit_names};
    use k816::{Options, analyze_workspace};

    let mut unexplained: Vec<String> = Vec::new();
    let mut counted = 0usize;
    let mut indexed = 0usize;

    for set in corpus() {
        let mut map = SourceMap::new();
        for file in &set.files {
            let text = std::fs::read_to_string(file).expect("read a corpus source");
            let name = file
                .strip_prefix(root())
                .unwrap_or(file)
                .to_string_lossy()
                .into_owned();
            map.add_file(name, text);
        }
        let analysis = analyze_workspace(&map, &Options::default());

        for unit in &analysis.units {
            let mut names = Vec::new();
            visit_names(unit.program(), &mut |name, role| {
                names.push((name.clone(), role));
            });
            for (name, role) in names {
                counted += 1;
                if name.provenance == Provenance::Synthetic {
                    continue;
                }
                let covered = analysis.diagnostics.iter().any(|diagnostic| {
                    let at = diagnostic.primary.span;
                    at.file == name.span.file
                        && at.start <= name.span.start
                        && name.span.end <= at.end
                });
                if covered {
                    continue;
                }
                let explained = match role {
                    NameRole::Declaration | NameRole::Fixed => true,
                    NameRole::Symbol => unit.model.references.contains_key(&name.span),
                    NameRole::Field => unit.model.field_references.contains_key(&name.span),
                    NameRole::Evaluator => {
                        unit.model.references.contains_key(&name.span)
                            || unit.model.symbols.lookup(&name.text).is_none()
                    }
                };
                if explained
                    && matches!(
                        role,
                        NameRole::Symbol | NameRole::Field | NameRole::Evaluator
                    )
                {
                    indexed += 1;
                }
                if !explained {
                    unexplained.push(format!(
                        "{}: `{}` ({role:?}) at bytes {}..{}",
                        set.name, name.text, name.span.start, name.span.end
                    ));
                }
            }
        }
    }

    assert!(
        unexplained.is_empty(),
        "{} names nothing explains — each is a resolution site the reference index \
         does not record:\n  {}",
        unexplained.len(),
        unexplained.join("\n  ")
    );
    // The corpus is large enough that a walk finding nothing would be a walk
    // that broke, not a corpus with no names in it.
    assert!(counted > 3_000, "the walk found only {counted} names");
    assert!(indexed > 800, "only {indexed} names reached the index");
}

// ---------------------------------------------------------------------------
// Fixes fix (sub-plan H, task H7, decision 5)
// ---------------------------------------------------------------------------

/// Every place the compiler attaches a [`Fix`], with a source that reaches it
/// and the title it produces there.
///
/// The count is checked against a walk of `src/`, so a producer added later
/// fails this test until it is listed here with a source that reaches it —
/// which is the only way "every fix is offered, and every machine-applicable
/// one removes its diagnostic" can mean anything.
struct FixSite {
    /// Where the fix is attached, `file:line`, as a reader's pointer.
    site: &'static str,
    /// A program that reaches it.
    source: &'static str,
    /// The title it produces, exactly.
    title: &'static str,
}

const FIX_SITES: &[FixSite] = &[
    FixSite {
        site: "src/parser/diagnostics.rs — a MISSING token",
        source: "func main @a8 {\n    return\n",
        title: "insert `}`",
    },
    FixSite {
        site: "src/hla.rs — a transfer the 65816 has no instruction for",
        source: "func w @a8 @i8 {\n    dpr = x\n    return\n}\n",
        title: "transfer through `a`",
    },
    FixSite {
        site: "src/width.rs — two annotations contradicting each other",
        source: "func w @a8 @a16 @i8 {\n    return\n}\n",
        title: "drop the contradicting annotation",
    },
    FixSite {
        site: "src/resolve.rs — a `.field` step naming no field",
        source: "var PAIR[ .lo :byte .hi :byte ]\nfunc w @a8 @i8 {\n    a = PAIR.hii\n    return\n}\n",
        title: "change it to `.hi`",
    },
    FixSite {
        site: "src/resolve.rs — a `const` in address position",
        source: "const K = $10\nfunc main @a8 @i8 {\n    lda K\n    return\n}\n",
        title: "load the value with `#K`",
    },
    FixSite {
        site: "src/resolve.rs — an offset marker where none is legal",
        source: "var V = $10\nfunc w @a8 @i8 {\n    a = &V\n    return\n}\n",
        title: "remove the offset marker",
    },
    FixSite {
        site: "src/resolve.rs — an encoding prefix on a form that encodes no address",
        source: "func w @a8 @i8 {\n    bra dp there\nthere:\n    return\n}\n",
        title: "remove the encoding prefix",
    },
    FixSite {
        site: "src/resolve.rs — an encoding prefix where exactly one is legal",
        source: "func w @a8 @i8 {\n    jmp dp $10\n    return\n}\n",
        title: "use `abs` instead",
    },
    FixSite {
        site: "src/semantic.rs — a `const` with no value",
        source: "const A0\nfunc main @a8 @i8 {\n    return\n}\n",
        title: "give the constant a value",
    },
    FixSite {
        site: "src/semantic.rs — a `->` clause on a `naked` section",
        source: "naked w @a8 @i8 -> a {\n    return\n}\n",
        title: "drop the exit contract",
    },
    FixSite {
        site: "src/semantic.rs — a module default below the first item",
        source: "func w @a8 @i8 {\n    return\n}\n@a16\n",
        title: "move the default to the top of the file",
    },
];

/// Every diagnostic `source` reports, syntax and semantic together.
fn all_diagnostics(source: &str) -> Vec<Diagnostic> {
    let mut map = SourceMap::new();
    let id = map.add_file("fix.k816", source);
    let parse = parse_source(map.file(id), None);
    let (_, semantic) = analyze_file(&parse.file);
    let mut all = parse.diagnostics;
    all.extend(semantic);
    all
}

/// `source` with `fix` applied.
fn with_fix_applied(source: &str, fix: &k816::diagnostic::Fix) -> String {
    let mut edits = fix.edits.clone();
    // Back to front, so an earlier edit does not move a later one.
    edits.sort_by_key(|edit| std::cmp::Reverse(edit.span.start));
    let mut fixed = source.to_owned();
    for edit in edits {
        fixed.replace_range(
            edit.span.start as usize..edit.span.end as usize,
            &edit.replacement,
        );
    }
    fixed
}

/// Every site that attaches a fix is reached by a source here.
///
/// Counted from `src/` rather than trusted: `with_fix(` is the one way a
/// diagnostic gets one, so the number of call sites is the number of rows
/// this table must have.
#[test]
fn every_site_that_attaches_a_fix_is_reached_by_a_source() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut producers = 0;
    for file in [
        "src/hla.rs",
        "src/width.rs",
        "src/resolve.rs",
        "src/semantic.rs",
        "src/parser/diagnostics.rs",
    ] {
        let text = std::fs::read_to_string(root.join(file)).expect("read a source file");
        producers += text.matches(".with_fix(").count();
    }
    assert_eq!(
        producers,
        FIX_SITES.len(),
        "`src/` attaches {producers} fixes and this table has {} rows: a producer \
         added without a source that reaches it is a fix nothing checks",
        FIX_SITES.len()
    );

    for site in FIX_SITES {
        let titles: Vec<String> = all_diagnostics(site.source)
            .iter()
            .flat_map(|diagnostic| diagnostic.fixes.iter().map(|fix| fix.title.clone()))
            .collect();
        assert!(
            titles.iter().any(|title| title == site.title),
            "{}: the source reaches no fix titled `{}`; it produced {titles:?}",
            site.site,
            site.title
        );
    }
}

/// Applying a machine-applicable fix removes the diagnostic it was attached
/// to.
///
/// The property decision 5 adds, and the one nothing checked before: a fix
/// could have been offered for years while producing source the compiler
/// refused *differently*, and an editor's user would have seen the complaint
/// change rather than go away.
///
/// A `MaybeIncorrect` fix is applied and re-analyzed too, and what happens is
/// recorded rather than asserted — "plausible but needs review" is exactly
/// the claim that it may leave something else to say.
#[test]
fn every_machine_applicable_fix_removes_its_diagnostic() {
    let mut reviewed: Vec<String> = Vec::new();

    for site in FIX_SITES {
        let before = all_diagnostics(site.source);
        for diagnostic in &before {
            for fix in &diagnostic.fixes {
                if fix.title != site.title {
                    continue;
                }
                let fixed = with_fix_applied(site.source, fix);
                let after = all_diagnostics(&fixed);
                let survivors: Vec<(&str, &str)> = after
                    .iter()
                    .map(|diagnostic| (diagnostic.code.as_str(), diagnostic.title.as_str()))
                    .collect();

                if fix.applicability != Applicability::MachineApplicable {
                    reviewed.push(format!(
                        "{}: `{}` leaves {survivors:?}",
                        site.site, fix.title
                    ));
                    continue;
                }
                assert!(
                    !survivors.contains(&(diagnostic.code.as_str(), diagnostic.title.as_str())),
                    "{}: applying `{}` left `{}` still reported:\n{fixed}",
                    site.site,
                    fix.title,
                    diagnostic.code.as_str()
                );
            }
        }
    }

    // Every `MaybeIncorrect` fix was applied and its outcome looked at. The
    // count is asserted so a fix that stops being reachable is noticed.
    assert_eq!(
        reviewed.len(),
        FIX_SITES
            .iter()
            .filter(|site| {
                all_diagnostics(site.source).iter().any(|diagnostic| {
                    diagnostic.fixes.iter().any(|fix| {
                        fix.title == site.title
                            && fix.applicability != Applicability::MachineApplicable
                    })
                })
            })
            .count(),
        "a `MaybeIncorrect` fix went unreviewed: {reviewed:#?}"
    );
}

/// Every fix a *corpus* program produces removes its diagnostic too.
///
/// The sites above are reached by sources invented for them; this is the same
/// property over the programs the tree actually holds, which is where a fix
/// meets a construct nobody wrote a case for.
#[test]
fn every_fix_a_corpus_program_produces_is_applied_and_checked() {
    use corpus_files::{corpus, root};
    use k816::{Options, analyze_workspace};

    let mut applied = 0;
    for set in corpus() {
        for entry in &set.files {
            let source = std::fs::read_to_string(entry).expect("read a corpus source");
            let mut map = SourceMap::new();
            let file = map.add_file(
                entry
                    .strip_prefix(root())
                    .unwrap_or(entry)
                    .to_string_lossy(),
                source.clone(),
            );
            let analysis = analyze_workspace(&map, &Options::default());
            for diagnostic in &analysis.diagnostics {
                if diagnostic.primary.span.file != file {
                    continue;
                }
                for fix in &diagnostic.fixes {
                    applied += 1;
                    let fixed = with_fix_applied(&source, fix);
                    let mut after_map = SourceMap::new();
                    after_map.add_file("fixed", fixed.clone());
                    let after = analyze_workspace(&after_map, &Options::default());
                    if fix.applicability != Applicability::MachineApplicable {
                        continue;
                    }
                    assert!(
                        !after.diagnostics.iter().any(|other| {
                            other.code == diagnostic.code && other.title == diagnostic.title
                        }),
                        "{}: applying `{}` left `{}` still reported",
                        entry.display(),
                        fix.title,
                        diagnostic.code.as_str()
                    );
                }
            }
        }
    }
    assert!(
        applied > 0,
        "no corpus program produced a fix, so this gate checked nothing"
    );
}
