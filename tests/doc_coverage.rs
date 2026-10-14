/*****************************************************************************/
/*                                                                           */
/*                              doc_coverage.rs                              */
/*                                                                           */
/*      Every example in the width specification, mapped to a checked claim   */
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

//! The coverage audit for `doc/register-width-aware-syntax.md`.
//!
//! Every example and listing in the width specification appears here, with
//! the snippet compiled and the property the document states about it turned
//! into an assertion. A section covered only by a test with a plausible name
//! is not covered: the document's own source is what runs.
//!
//! **Several of the document's listings are post-optimization**, and those
//! rows are compiled *with* the optimizer. Dead mode elimination and
//! `REP`/`SEP` folding are its work, not the width model's, so a row marked
//! [`Stage::Optimized`] asserts the document's listing verbatim against
//! `compile` output rather than against the unfolded stream lowering emits.
//! A row that still departs from its listing says what and why in
//! `difference`, and the audit below refuses a short answer.
//!
//! Anyone "fixing" a [`Stage::Modelled`] row's expectations to match a
//! post-optimization listing is re-implementing the optimizer inside the width
//! model, which is what this file exists to catch.
//!
//! The document is not read at run time, so its sections and snippets are
//! transcribed. A section whose example changes and is not transcribed here
//! stays green — which is why the rows name the section heading, so a reader
//! comparing the two can see what is missing.

use k816::abi::EntryEnvironment;
use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

/// Which stage of the pipeline owns the output a document listing shows.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Stage {
    /// K816's own output before optimization, as the width model emits it.
    /// Compiled with `--no-opt`.
    Modelled,
    /// The listing is post-optimization, so the row is compiled **with** the
    /// optimizer and asserted against the document verbatim. `difference` is
    /// empty unless something in the listing is still not what K816 emits, in
    /// which case it says what and why.
    Optimized,
}

/// One documented example, and the claim it turns into.
struct Row {
    /// The `##` heading in `doc/register-width-aware-syntax.md`.
    section: &'static str,
    source: &'static str,
    stage: Stage,
    /// What is still not identical between the document's listing and what
    /// K816 emits, and why. Empty when the listing is literally the output —
    /// which is what every row should be, and what the audit below allows a
    /// row to depart from only by saying so.
    difference: &'static str,
    /// Emitted lines the compiled snippet must contain, in order.
    expected: &'static [&'static str],
    /// A diagnostic code the snippet must report, if the document says so.
    diagnostic: Option<&'static str>,
    /// The `--entry` spelling the listing is written under, when the section
    /// documents one. `None` is the default environment, which is what every
    /// listing that does not say otherwise means.
    entry: Option<&'static str>,
}

const MATRIX: &[Row] = &[
    Row {
        section: "Width-dependent instructions",
        // Not `main`: the entry point takes its widths from the ABI, so it
        // is the one function that cannot reach this error.
        source: "func f {\n    lda #$01\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        // The document prints this exact error, help line included.
        diagnostic: Some("K816-E0104"),
        entry: None,
    },
    Row {
        section: "Transfers between the two axes",
        // The listing's own program. `lda #$00` is there to make the point
        // the note makes — the high byte moved is the one it did not write —
        // and reports nothing of its own.
        source: "func scan @a8 @i16 {\n    lda #$00\n    tax\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0171"),
        entry: None,
    },
    Row {
        section: "Function-level mode contracts",
        source: "\
func wide_a @a16 {
    lda #1
}

func narrow @a8 @i8 {
    lda #1
    ldx #1
}

func wide_all @a16 @i16 {
    lda #1
    ldx #1
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &[
            "lda     #$0001", // 3-byte immediate
            "lda     #$01",   // 2-byte
            "ldx     #$01",   // 2-byte
            "lda     #$0001", // 3-byte
            "ldx     #$0001", // 3-byte
        ],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "The entry point",
        // Unannotated, and still sized: the widths are the environment's.
        source: "func main {\n    lda #$01\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[".a8", ".i8", "lda     #$01"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "The entry point",
        // Annotated with what the environment provides: a true claim, and
        // still no instruction — the widths were already those.
        source: "func main @a8 @i8 {\n    lda #$01\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[".a8", ".i8", "lda     #$01"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "The entry point",
        // Annotated with what it does not: the document prints this error.
        source: "func main @a16 {\n    lda #$1234\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0143"),
        entry: None,
    },
    Row {
        section: "The entry point",
        // The listing for a callee that enters emulation mode: the caller's
        // widening `rep` is a no-op there, so its immediate is unsizeable.
        source: "func flips @a8 @i8 {\n    sec\n    xce\n}\n\
                 func caller @a8 @i8 {\n    call flips\n    rep #$20\n    a = #$1234\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0104"),
        entry: None,
    },
    Row {
        section: "The entry point",
        // The same source under `--entry emulation`, which the listing does
        // not claim anything about: the refusal is the callee's doing and
        // not the environment's, so it must not move with the flag.
        source: "func flips @a8 @i8 {\n    sec\n    xce\n}\n\
                 func caller @a8 @i8 {\n    call flips\n    rep #$20\n    a = #$1234\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0104"),
        entry: Some("emulation"),
    },
    Row {
        section: "The entry point",
        // The `--entry emulation` listing: the `rep` is a no-op on that
        // machine, so the immediate is still sized against eight bits.
        source: "func main {\n    rep #$20\n    lda #$1234\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0103"),
        entry: Some("emulation"),
    },
    Row {
        section: "The entry point",
        // The same source under the default environment, which is what makes
        // the row above a claim about `emulation` rather than about `rep`.
        source: "func main {\n    rep #$20\n    lda #$1234\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &["rep     #$20", ".a16", "lda     #$1234"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "The entry point",
        // The `K816-E0163` listing: a scoped block whose `rep` is a no-op.
        source: "func main {\n    @a16 {\n        lda #$1234\n    }\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0163"),
        entry: Some("emulation"),
    },
    Row {
        section: "The entry point",
        // And the call bridge beside it, which is the same refusal reached a
        // different way.
        source: "func helper @a16 {\n    lda #$1234\n}\n\nfunc main {\n    call helper\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0163"),
        entry: Some("emulation"),
    },
    Row {
        section: "The entry point",
        // The escape hatch, which is the line the refusal is drawn at: once
        // `xce` has made the flag unknown, `@a16` is the assertion again.
        //
        // `.a16` is in the list because the *bytes* are what the assertion is
        // about, and the emitted text was valid without it: ca65 assembled
        // `lda #$1234` at eight bits and the CPU read the next instruction as
        // the immediate's high byte. A gate on the mnemonics alone could not
        // see it (`postmortem.md` §H.13).
        source: "func main {\n    clc\n    xce\n    @a16 {\n        lda #$1234\n    }\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &["rep     #$20", ".a16", "lda     #$1234"],
        diagnostic: None,
        entry: Some("emulation"),
    },
    Row {
        section: "The entry point",
        // And the way to get wider registers in `main`: widen the body.
        source: "func main @a8 {\n    @a16 {\n        lda #$1234\n    }\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &["rep     #$20", "lda     #$1234", "sep     #$20"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "The entry point",
        // An interrupt handler is the second kind, and takes the same
        // widths without saying anything.
        source: "naked dli {\n    lda #$01\n    return_i\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[".a8", ".i8", "lda     #$01"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "The entry point",
        // And is held to them.
        source: "naked dli @a16 {\n    lda #$1234\n    return_i\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0143"),
        entry: None,
    },
    Row {
        section: "The entry point",
        // `naked` alone marks nothing: without an `RTI` this is an ordinary
        // function, and declares what it likes.
        source: "naked chunk @a16 {\n    lda #$1234\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &["lda     #$1234"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "Call-site bridging",
        // The document shows only `caller`; the callees are named in its
        // comments. Their bodies *read* the width they declare, which is what
        // makes each bridge serve something — a callee that reads neither flag
        // has a contract nothing depends on, and the optimizer drops the
        // bridge to it. That is the next section's example, not this one's.
        source: "\
var mem = $10

func wide_a @a16 {
    lda mem
}

func narrow @a8 @i8 {
    lda mem
    ldx mem
}

func wide_all @a16 @i16 {
    lda mem
    ldx mem
}

func uncolored {
    nop
}

func caller {
    call wide_a
    call narrow
    call wide_all
    call uncolored
}
",
        stage: Stage::Optimized,
        difference: "",
        expected: &[
            "rep     #$20",
            "jsr     a:wide_a",
            "sep     #$30",
            "jsr     a:narrow",
            "rep     #$30",
            "jsr     a:wide_all",
            "jsr     a:uncolored",
        ],
        diagnostic: None,
        entry: None,
    },
    Row {
        // The loop: the fall-through's switch before the label, and nothing
        // between the label and its first instruction or on the back edge.
        section: "A label's entry width",
        source: "\
var RDY = $FFE0

func poll @a16 @i16 {
    tax
    @a8
.wait:
    bit RDY
    dex
    bne .wait
    @a16
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &[
            "tax",
            "sep     #$20",
            "@wait:",
            "bit     a:RDY",
            "dex",
            "bne     @wait",
            "rep     #$20",
        ],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "A label's entry width",
        source: "\
func pick @a8 @i8 {
    lda #$12
    bra .wide
    @a16
.wide:
    lda #$1234
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &[
            "lda     #$12",
            "rep     #$20",
            "bra     @wide",
            "@wide:",
            "lda     #$1234",
        ],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "A label's entry width",
        source: "\
func main @a8 @i8 {
    bcs .wide
    @a16
.wide:
    lda #1
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &["rep     #$20", "bcs     @wide", "@wide:", "lda     #$0001"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "A transfer K816 cannot name",
        // The listing's own program: the loop above the dispatch keeps the
        // width its one named arrival brings.
        source: "\
var table = $0300

func main @a8 @i8 {
.again:
    lda #1
    beq .again
    jmp (table,x)
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &["lda     #$01", "beq     @again"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "A transfer K816 cannot name",
        // And the sentence after it: the label's address taken.
        source: "\
var table = $0300

func main @a8 @i8 {
.again:
    lda #1
    beq .again
    ldx #&<.again
    jmp (table,x)
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0104"),
        entry: None,
    },
    Row {
        section: "Entry-point mode setup",
        // There is none, and the section says so: a module default that
        // reaches `main` is checked against the ABI like any annotation.
        source: "\
@a16 @i16

func main {
    lda #$1234
    ldx #$0001
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0143"),
        entry: None,
    },
    Row {
        section: "Leaving a block by a branch",
        source: "\
func f @a8 {
    @a16 {
        bra .out
    }
.out:
    lda #$12
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &[
            "rep     #$20", // enter the block
            "bra     :+",   // the branch, redirected to the pad
            "sep     #$20", // the restore, on the taken edge
            "jmp     @out", // and on to the written target
            "lda     #$12", // 8-bit, because the pad got there first
        ],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "Block-level mode scoping",
        source: "\
func main @a8 @i8 {
    lda #1

    @a16 {
        lda #1
    }
    lda #1

    @a16 @i16 {
        lda #1
        ldx #1
    }
    lda #1
}
",
        stage: Stage::Optimized,
        difference: "",
        expected: &[
            "lda     #$01",
            "rep     #$20",
            "lda     #$0001",
            "sep     #$20",
            "lda     #$01",
            "rep     #$30",
            "lda     #$0001",
            "ldx     #$0001",
            // Both axes go back, including the index one: narrowing zeroes
            // the high bytes of X and Y, which is a register write, and every
            // register is live where a function returns. Earlier revisions of
            // the document printed `sep #$20` here.
            "sep     #$30",
            "lda     #$01",
        ],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "Module-level default coloring",
        // Not `main`: a module default reaches the entry point too, where it
        // is checked against the ABI — which the section now says.
        source: "\
@a16

func work {
    lda #1
}

func helper {
    lda #2
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &["lda     #$0001", "lda     #$0002"],
        diagnostic: None,
        entry: None,
    },
    Row {
        section: "Overriding module defaults",
        source: "\
@a16 @i16

func work {
    lda #1
    ldx #1
}

func narrow @a8 @i8 {
    lda #1
    ldx #1
}
",
        stage: Stage::Optimized,
        difference: "the section's listing opens with an entry-point `rep #$30` \
                     that K816 no longer emits at all: sub-plan J made the entry \
                     point's widths the environment's, so `main` asserts them \
                     rather than establishing them, and this row uses `work` for \
                     the same reason the section's own note gives",
        expected: &[
            "lda     #$0001",
            "ldx     #$0001",
            "lda     #$01",
            "ldx     #$01",
        ],
        diagnostic: None,
        entry: None,
    },
    Row {
        // The width document's "Dead mode elimination", as an algorithm rather
        // than a listing: the seed is *flag reads*, and a `sta` carries no
        // immediate at all. Under the section's own earlier wording the
        // `rep #$20` here is dead, and deleting it turns a 16-bit store into
        // an 8-bit one.
        section: "Dead mode elimination",
        source: "\
var mem = $0400

func stores @a8 {
    rep #$20
    lda mem
    sta mem
    sep #$20
}
",
        stage: Stage::Optimized,
        difference: "",
        expected: &[
            "rep     #$20",
            "lda     a:mem",
            "sta     a:mem",
            "sep     #$20",
        ],
        diagnostic: None,
        entry: None,
    },
    Row {
        // And the half the section is named for: an axis nothing reads.
        section: "Interaction with dead mode elimination",
        source: "\
@a16 @i16

func main @a8 @i8 {
    nop
    call helper
}

func helper {
    nop
}
",
        stage: Stage::Optimized,
        difference: "",
        expected: &["nop", "jsr     a:helper", "rts", "nop", "rts"],
        diagnostic: None,
        entry: None,
    },
    Row {
        // The folding table, and the row it gets wrong. `rep #$21` clears the
        // carry as well, so the composed clear mask is $01 and the `REP` is
        // not cancelled — which is what the corrected section now says.
        section: "REP/SEP folding",
        source: "\
var mem = $0400

func folds @a8 @i8 {
    rep #$20
    rep #$10
    lda mem
    ldx mem
    sep #$20
    sep #$10
    lda mem
    ldx mem
}
",
        stage: Stage::Optimized,
        difference: "",
        expected: &[
            "rep     #$30",
            "lda     a:mem",
            "ldx     a:mem",
            "sep     #$30",
            "lda     a:mem",
            "ldx     a:mem",
        ],
        diagnostic: None,
        entry: None,
    },
];

/// Sections of the document that describe passes the width model does not
/// own, with the sub-plan that does.
///
/// Listed rather than silently absent: a reader auditing the document
/// against this file should find every heading in one of the two places.
const NOT_THE_WIDTH_MODELS: &[(&str, &str)] = &[
    (
        "Unknown exit contracts preserve caller mode",
        "the rule itself is checked in tests/width.rs, which derives each exit \
         shape and pins the fallback; the section is prose and a table rather \
         than a listing",
    ),
    (
        "Why the rule stops at immediates",
        "prose about the rule's scope, and its two claims are checked where \
         they can be: tests/width.rs reads the ISA table for \
         only_an_immediate_encodes_a_different_length_at_a_different_width, and \
         tests/compile.rs reads the linked bytes for \
         an_unannotated_callee_runs_at_its_callers_width",
    ),
    (
        "Compilation pipeline",
        "prose: the stage list, not a program",
    ),
    (
        "Listing disassembly",
        "ld65's listing output, which K816 does not produce",
    ),
];

fn compile(source: &str, stage: Stage, entry: Option<&str>) -> (Vec<String>, Vec<String>) {
    let mut map = SourceMap::new();
    let file = map.add_file("doc.k816", source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: stage == Stage::Optimized,
            entry: entry.map_or_else(EntryEnvironment::default, |text| {
                EntryEnvironment::parse(text).unwrap_or_else(|e| panic!("`--entry {text}`: {e}"))
            }),
            ..Default::default()
        },
    );
    let codes = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str().to_owned())
        .collect();
    let lines = result
        .assembly
        .map(|assembly| {
            assembly
                .lines()
                .filter_map(|line| line.split_once(';'))
                .map(|(text, _)| text.trim().to_owned())
                .filter(|text| !text.is_empty())
                .collect()
        })
        .unwrap_or_default();
    (lines, codes)
}

#[test]
fn every_documented_example_compiles_to_what_it_documents() {
    let mut failures = Vec::new();
    for row in MATRIX {
        let (lines, codes) = compile(row.source, row.stage, row.entry);
        match row.diagnostic {
            Some(code) => {
                if !codes.iter().any(|reported| reported == code) {
                    failures.push(format!(
                        "  {}\n    expected {code}, got {codes:?}",
                        row.section
                    ));
                }
                continue;
            }
            None => {
                let errors: Vec<&String> =
                    codes.iter().filter(|code| !code.contains("-W")).collect();
                if !errors.is_empty() {
                    failures.push(format!("  {}\n    errors: {errors:?}", row.section));
                    continue;
                }
            }
        }
        // The expected lines have to appear in order, with anything the
        // model adds around them allowed: the assertion is about what the
        // document claims, not about every operation K816 emits.
        let mut remaining = lines.iter();
        for expected in row.expected {
            if !remaining.any(|line| line == expected) {
                failures.push(format!(
                    "  {}\n    missing `{expected}` in order\n    emitted: {lines:?}",
                    row.section
                ));
                break;
            }
        }
    }
    assert!(
        failures.is_empty(),
        "documented examples that do not behave as documented:\n{}",
        failures.join("\n")
    );
}

// ---------------------------------------------------------------------------
// `doc/symbolic-subscripts.md` (sub-plan E, task E10).
// ---------------------------------------------------------------------------

/// The declarations the symbolic-subscript document's examples are written
/// against, so that each row states only what it is about.
const LAYOUTS: &str = "\
var TASKS[
    .sp      :word
    .state   :byte
    .signals :byte
] = $4000

var REGS[
    .status :byte
    .data[
        .lo :byte
        .hi :byte
    ]
] = $D000

var COMP[
    .one :byte
    .two :word
    .str :byte [5]
] * 10 = $1000

var counter:word = $80
var buffer[32] = $0100
var tasks[
    .state :byte
] * 8

abstract var DEV_HANDLER[
    .open  :word
    .close :word
    .init  :word
]
";

/// One claim `doc/symbolic-subscripts.md` makes, and the byte it produces.
///
/// Every row is an *arithmetic* claim — an offset, a size, a stride — which
/// is what that document is: "Semantics" gives the formulas and the rest
/// gives worked examples of them. A row that stops holding is a formula the
/// compiler no longer implements.
const SUBSCRIPTS: &[(&str, &str, &str)] = &[
    // "Semantics": packed and sequential, first offset 0.
    ("Semantics", "a = TASKS:sizeof", "#$04"),
    ("Semantics", "a = TASKS.sp:sizeof", "#$02"),
    ("Semantics", "a = TASKS.sp:offsetof", "#$00"),
    ("Semantics", "a = TASKS.state:offsetof", "#$02"),
    ("Semantics", "a = TASKS.signals:offsetof", "#$03"),
    // `field size = element_width * count`.
    ("Semantics", "a = COMP.str:sizeof", "#$05"),
    // Nested fields resolve against the top-level variable.
    ("Semantics", "a = REGS.data.lo:offsetof", "#$01"),
    ("Semantics", "a = REGS.data.hi:offsetof", "#$02"),
    // `foo:sizeof` is one repeated element, not the allocation.
    ("Semantics", "a = tasks:sizeof", "#$01"),
    ("Semantics", "a = counter:sizeof", "#$02"),
    ("Semantics", "a = buffer:sizeof", "#$20"),
    // "Access Syntax": the dotted and bracketed spellings are one selection,
    // and a repeated element strides by `:sizeof`.
    ("Access Syntax", "a = TASKS.state", "a:TASKS + $02"),
    ("Access Syntax", "a = TASKS[.state]", "a:TASKS + $02"),
    ("Access Syntax", "a = COMP[2].one", "a:COMP + $10"),
    ("Access Syntax", "a = COMP[2][.one]", "a:COMP + $10"),
    ("Access Syntax", "a = COMP[2].str[3]", "a:COMP + $16"),
    // "Abstract layout-only form": queries, and nothing else.
    (
        "Abstract layout-only form",
        "a = DEV_HANDLER:sizeof",
        "#$06",
    ),
    (
        "Abstract layout-only form",
        "a = DEV_HANDLER.init:offsetof",
        "#$04",
    ),
];

/// Every arithmetic claim `doc/symbolic-subscripts.md` makes is the arithmetic
/// K816 performs.
#[test]
fn every_symbolic_subscript_claim_holds() {
    let mut failures = Vec::new();
    for (section, statement, expected) in SUBSCRIPTS {
        // `@a8` throughout: every value below is a byte, and every field
        // these rows read is a `:byte` one — a wider accumulator would be
        // `K816-E0140` against the declared datum, which is its own test.
        let source = format!("{LAYOUTS}\nfunc work @a8 @i8 {{\n    {statement}\n}}\n");
        let (lines, codes) = compile(&source, Stage::Modelled, None);
        if !codes.is_empty() {
            failures.push(format!("  {section}: `{statement}` reported {codes:?}"));
            continue;
        }
        if !lines.iter().any(|line| line.contains(expected)) {
            failures.push(format!(
                "  {section}: `{statement}` should emit `{expected}`, emitted {lines:?}"
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "documented subscript claims that do not hold:\n{}",
        failures.join("\n")
    );
}

/// Every section of `doc/symbolic-subscripts.md` is checked or accounted for.
#[test]
fn every_symbolic_subscript_section_is_accounted_for() {
    /// Sections with no arithmetic of their own, and why.
    const ACCOUNTED: &[(&str, &str)] = &[
        (
            "Overview",
            "introduces the three base forms the sections below check",
        ),
        ("Declaration Syntax", "introduces the three forms below it"),
        (
            "Symbolic subscript array form",
            "tests/golden/subscripts.k816, through every_field_access_folds_to_the_offset_its_layout_declares",
        ),
        (
            "Default field width on the symbol",
            "tests/semantic.rs: a_field_takes_the_nearest_width_and_counts_elements_of_it",
        ),
        (
            "Field item grammar",
            "tests/semantic.rs: a_field_takes_the_nearest_width_and_counts_elements_of_it \
             for the widths and counts, and a_field_list_takes_a_comma_a_newline_or_both \
             for the separators — the half this row used to leave uncovered, which is how \
             the grammar came to accept only the newline",
        ),
        (
            "Diagnostics",
            "tests/diagnostics.rs: one FAMILY case per row of that table",
        ),
        (
            "Implementation Notes",
            "describes the original K65 compiler, not this one",
        ),
        ("AST and parser", "describes the original K65 compiler"),
        (
            "Eval expansion and semantics",
            "describes the original K65 compiler",
        ),
        (
            "Lowering and formatting",
            "describes the original K65 compiler",
        ),
        ("Tests", "describes the original K65 compiler's test layout"),
    ];

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("doc/symbolic-subscripts.md");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
    let headings: Vec<&str> = text
        .lines()
        .filter_map(|line| {
            line.strip_prefix("## ")
                .or_else(|| line.strip_prefix("### "))
        })
        .collect();
    assert!(headings.len() > 10, "the heading parse found {headings:?}");

    let missing: Vec<&str> = headings
        .iter()
        .filter(|heading| {
            !SUBSCRIPTS.iter().any(|(section, _, _)| section == *heading)
                && !ACCOUNTED.iter().any(|(section, _)| section == *heading)
        })
        .copied()
        .collect();
    assert!(
        missing.is_empty(),
        "symbolic-subscript sections with neither a checked claim nor a stated reason: {missing:?}"
    );
    for (section, why) in ACCOUNTED {
        assert!(
            headings.contains(section),
            "`{section}` is accounted for but no longer a section ({why})"
        );
    }
}

// ---------------------------------------------------------------------------
// The per-instruction spelling tables (sub-plan E, task E10).
// ---------------------------------------------------------------------------

/// The nine columns every instruction section's table has, with the operand
/// each one is written against and the addressing mode it claims.
///
/// `Implied` covers the stack forms too — `a!!` is `PHA`, which the ISA calls
/// `Stack` — so that column asserts the mnemonic and leaves the mode alone.
/// Everything else names exactly one mode, and naming it is the point: the
/// table is a claim that this spelling selects that mode, and a spelling that
/// quietly picked another would still assemble.
const COLUMNS: &[(&str, &[&str])] = &[
    ("Acc", &["Accumulator"]),
    ("Implied", &[]),
    ("Imm", &["Immediate"]),
    // The direct-page variants belong beside the absolute ones: the column
    // claims "the indexed form", and which widths the CPU offers for a given
    // instruction is the ISA table's business — `STX` has only `dp,Y`.
    ("Mem", &["Absolute", "Direct"]),
    ("Mem,X", &["AbsoluteIndexedX", "DirectIndexedX"]),
    ("Mem,Y", &["AbsoluteIndexedY", "DirectIndexedY"]),
    ("(Mem,X)", &["DirectIndexedIndirect"]),
    ("(Mem),Y", &["DirectIndirectIndexed"]),
    // Two modes wear one column: the indirect load forms are direct-page
    // only, and `JMP` has no direct-page indirect at all, so the same
    // spelling means `(dp)` for one instruction and `(abs)` for the other.
    ("(Mem)", &["DirectIndirect", "AbsoluteIndirect"]),
];

/// One cell of one instruction's table.
struct Spelling {
    mnemonic: String,
    column: &'static str,
    modes: &'static [&'static str],
    source: String,
}

/// Reads every `### \`MNEMONIC\`` section's spelling table out of the
/// document.
///
/// The tables are the document's per-statement claim — "this K65 spelling is
/// that instruction in that mode" — and they are the part of the reference
/// that sub-plan E implements. Parsed rather than transcribed, so that a
/// spelling added to the document is checked without anyone remembering to
/// add it here.
fn split_cells(row: &str) -> Vec<String> {
    let mut cells = vec![String::new()];
    let mut escaped = false;
    for character in row.chars() {
        match character {
            '\\' if !escaped => escaped = true,
            '|' if !escaped => cells.push(String::new()),
            _ => {
                cells.last_mut().expect("one cell").push(character);
                escaped = false;
            }
        }
    }
    cells
}

fn spellings() -> Vec<Spelling> {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("doc/syntax-reference.md");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
    let header = COLUMNS
        .iter()
        .map(|(name, _)| *name)
        .collect::<Vec<_>>()
        .join("|");

    let mut found = Vec::new();
    let mut lines = text.lines().peekable();
    while let Some(line) = lines.next() {
        let Some(rest) = line.strip_prefix("### `") else {
            continue;
        };
        let Some(mnemonic) = rest.split('`').next() else {
            continue;
        };
        // The table, when there is one, is the next two non-empty lines: the
        // header and its separator. A branch section has a bullet list here.
        let mut table = lines.clone();
        let Some(&next) = table.peek() else { continue };
        if next.is_empty() {
            table.next();
        }
        if table.peek().is_none_or(|line| *line != header) {
            continue;
        }
        table.next();
        table.next(); // the `:---:` separator
        let Some(row) = table.next() else { continue };
        // `|` separates cells and `\|` is a literal pipe inside one — which
        // `ORA` needs, its whole spelling being `a|mem`.
        for (cell, (column, modes)) in split_cells(row).into_iter().zip(COLUMNS) {
            // A cell may offer alternatives: `` `p!!` / `flag!!` ``.
            for spelling in cell.split(" / ") {
                let spelling = spelling.trim().trim_matches('`');
                if spelling.is_empty() {
                    continue;
                }
                found.push(Spelling {
                    mnemonic: mnemonic.to_owned(),
                    column,
                    modes,
                    source: spelling.to_owned(),
                });
            }
        }
    }
    assert!(
        found.len() > 100,
        "the table parse found only {} spellings, so the shape changed",
        found.len()
    );
    found
}

/// Every spelling the instruction tables give is that instruction, in that
/// mode.
///
/// This is sub-plan E's subject stated by the document and checked against
/// the compiler: the HLA statement family exists to make these spellings mean
/// those instructions.
#[test]
fn every_documented_spelling_selects_the_instruction_it_claims() {
    let mut failures = Vec::new();
    for spelling in spellings() {
        // `mem` is absolute and `zp` direct-page, because the indirect forms
        // are direct-page-only on this CPU — which is why the document writes
        // them as `(mem)` and means a one-byte address.
        // The indirect forms are direct-page only, so they are written
        // against the direct-page variable; everything else is tried
        // absolute first and falls back, because an instruction whose only
        // indexed form is `dp,Y` cannot take an absolute one.
        let candidates: &[&str] = if spelling.column.starts_with('(') {
            &["zp"]
        } else {
            &["mem", "zp"]
        };
        let mut attempt = None;
        for operand in candidates {
            let statement = spelling.source.replace("imm", "#1").replace("mem", operand);
            let source = format!(
                "var mem = $0400\nvar dp zp = $10\nfunc work @a8 @i8 {{\n    {statement}\n}}\n"
            );
            let mut map = SourceMap::new();
            let file = map.add_file("table.k816", &source);
            let parsed = k816::parser::parse_source(map.file(file), None);
            if !parsed.diagnostics.is_empty() {
                attempt = Some((statement, Err("does not parse".to_owned()), None));
                continue;
            }
            let (model, diagnostics) = k816::semantic::analyze_file(&parsed.file);
            if diagnostics
                .iter()
                .any(k816::diagnostic::Diagnostic::is_error)
            {
                let codes = diagnostics
                    .iter()
                    .map(|diagnostic| diagnostic.code.as_str())
                    .collect::<Vec<_>>();
                attempt = Some((statement, Err(format!("{codes:?}")), None));
                continue;
            }
            attempt = Some((statement, Ok(()), Some((parsed, model))));
            break;
        }
        let Some((statement, outcome, compiled)) = attempt else {
            continue;
        };
        if let Err(why) = outcome {
            failures.push(format!("  {} `{statement}`: {why}", spelling.mnemonic));
            continue;
        }
        let (parsed, model) = compiled.expect("a successful attempt carries its output");
        let (program, _) = k816::lower::lower(model.program(&parsed.file), &model);
        let emitted: Vec<String> = program
            .functions()
            .flat_map(|function| function.ops.iter())
            .filter_map(|op| match &op.kind {
                k816::op::KOpKind::Instruction(instruction) => {
                    Some(format!("{:?}/{:?}", instruction.mnemonic, instruction.mode))
                }
                _ => None,
            })
            .collect();
        // The implicit `rts` is the function's, not the spelling's.
        let Some(actual) = emitted.first() else {
            failures.push(format!(
                "  {} `{statement}`: emitted nothing",
                spelling.mnemonic
            ));
            continue;
        };
        let (mnemonic, mode) = actual.split_once('/').expect("mnemonic/mode");
        if !mnemonic.eq_ignore_ascii_case(&spelling.mnemonic) {
            failures.push(format!(
                "  {} `{statement}`: emitted {mnemonic}",
                spelling.mnemonic
            ));
            continue;
        }
        if !spelling.modes.is_empty() && !spelling.modes.contains(&mode) {
            failures.push(format!(
                "  {} `{statement}` ({}): emitted {mode}, documented {:?}",
                spelling.mnemonic, spelling.column, spelling.modes
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "documented spellings that do not select what they claim:\n{}",
        failures.join("\n")
    );
}

/// Every fixture is handed to ca65, or says why it cannot be.
///
/// Comparing `KOp` streams proves what a construct *means*; only the
/// assembler proves the result is something ca65 accepts, and the two are not
/// the same claim. Sub-plan E found this the hard way: `hla-branches.k816`
/// held every branch form and was compared against its native pairing for
/// three tasks without anyone asking ca65 whether a relative branch reached
/// — which is exactly the failure decision 7 exists to prevent, and not a
/// failure K816 reports.
#[test]
fn every_golden_fixture_is_assembled_or_says_why_not() {
    /// The fixtures that reach ca65, and the test that takes them there.
    const ASSEMBLED: &[(&str, &str)] = &[
        (
            "block-exit.k816",
            "a_transfer_out_of_a_width_block_restores_it_on_the_taken_edge",
        ),
        (
            "opt-tail-labels.k816",
            "the_tail_label_fixture_loses_one_branch_and_one_return",
        ),
        (
            "branch-range.k816",
            "a_branch_past_a_relative_branchs_reach_still_assembles",
        ),
        (
            "contract-unit-a.k816",
            "a_cross_unit_exit_reaches_the_callers_encoded_immediate",
        ),
        (
            "contract-unit-b.k816",
            "a_cross_unit_exit_reaches_the_callers_encoded_immediate",
        ),
        (
            "data-entries.k816",
            "every_data_entry_emits_the_bytes_it_describes",
        ),
        (
            "far-calls.k816",
            "the_far_family_and_every_goto_shape_assemble",
        ),
        ("hla-alu.k816", "the_paired_hla_fixtures_assemble"),
        ("hla-branches.k816", "the_paired_hla_fixtures_assemble"),
        ("hla-chains.k816", "the_paired_hla_fixtures_assemble"),
        (
            "inline-expansion.k816",
            "expansions_and_naked_sections_assemble",
        ),
        (
            "layout-unit-a.k816",
            "a_layout_crosses_a_unit_boundary_as_data",
        ),
        (
            "layout-unit-b.k816",
            "a_layout_crosses_a_unit_boundary_as_data",
        ),
        ("native-modes.k816", "native_modes_assembles_and_links"),
        (
            "native-reloc.k816",
            "the_emitted_assembly_does_not_depend_on_where_ld65_puts_it",
        ),
        (
            "lsp-hover.k816",
            "the_hover_fixture_is_a_program_ca65_accepts",
        ),
        (
            "lsp-rename.k816",
            "the_rename_fixture_is_a_program_ca65_accepts",
        ),
        ("slice.k816", "hla_slice_links_to_the_expected_bytes"),
        ("slice-native.k816", "native_slice_links_to_identical_bytes"),
        (
            "subscripts.k816",
            "every_field_access_folds_to_the_offset_its_layout_declares",
        ),
        ("unit-a.k816", "the_committed_unit_pair_links"),
        ("unit-b.k816", "the_committed_unit_pair_links"),
        (
            "vars.k816",
            "every_declaration_axis_reserves_and_encodes_what_it_declares",
        ),
        (
            "opt-barriers.k816",
            "the_barrier_fixture_assembles_and_the_optimizer_changes_no_byte",
        ),
        (
            "opt-branch-range.k816",
            "every_shrunk_branch_is_one_ca65_accepts",
        ),
        (
            "opt-crossunit-a.k816",
            "the_cross_unit_mode_pair_links_with_both_bridges_intact",
        ),
        (
            "opt-crossunit-b.k816",
            "the_cross_unit_mode_pair_links_with_both_bridges_intact",
        ),
        (
            "opt-folding.k816",
            "the_folding_fixture_merges_only_what_the_composed_transform_allows",
        ),
        (
            "opt-zero-store.k816",
            "the_zero_store_fixture_rewrites_per_store_and_drops_one_load",
        ),
        (
            "opt-values.k816",
            "the_value_fixture_coalesces_only_what_the_lanes_cover",
        ),
        (
            "opt-modes.k816",
            "the_mode_fixture_strips_one_mask_bit_and_removes_one_redundant_change",
        ),
        (
            "opt-liveout.k816",
            "the_live_out_fixture_keeps_everything_a_return_could_hand_back",
        ),
        (
            "width-branch.k816",
            "a_skipped_mode_change_does_not_widen_the_immediate_after_the_label",
        ),
        (
            "width-bridging.k816",
            "call_site_bridges_are_the_minimum_for_each_contract",
        ),
        (
            "width-loop.k816",
            "a_loop_head_its_arrivals_agree_about_keeps_its_width",
        ),
        (
            "width-module.k816",
            "module_defaults_reach_the_encoded_immediate_length",
        ),
        ("import-main.k816", "the_import_pair_links_through_ld65"),
        ("import-lib.k816", "the_import_pair_links_through_ld65"),
        ("import-iface.k816", "the_import_pair_links_through_ld65"),
        ("kk-k65.kk", "a_kk_source_takes_k65_and_full_k816_alike"),
        ("kk-native.kk", "a_kk_source_takes_k65_and_full_k816_alike"),
        ("kk-lib.k816", "a_kk_source_takes_k65_and_full_k816_alike"),
    ];

    /// The fixtures that cannot reach ca65, and why. Every one of them pins a
    /// *refusal*, and a refused program has no bytes to assemble.
    const REFUSED: &[(&str, &str)] = &[
        (
            "nocross-oversized.k816",
            "a `nocross` run too big for a page: K816-E0112",
        ),
        ("width-blocks.k816", "a width block a branch leaks out of"),
        (
            "width-emulation.k816",
            "a widening annotation under unknown emulation",
        ),
        ("width-exit.k816", "divergent and contradicted exits"),
        (
            "width-native-call.k816",
            "an unbridged native call: K816-W0125",
        ),
    ];

    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let mut fixtures: Vec<String> = std::fs::read_dir(&dir)
        .expect("read tests/golden")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().to_string_lossy().into_owned();
            (name.ends_with(".k816") || name.ends_with(".kk")).then_some(name)
        })
        .collect();
    fixtures.sort();

    let missing: Vec<&String> = fixtures
        .iter()
        .filter(|name| {
            !ASSEMBLED.iter().any(|(fixture, _)| fixture == *name)
                && !REFUSED.iter().any(|(fixture, _)| fixture == *name)
        })
        .collect();
    assert!(
        missing.is_empty(),
        "fixtures that neither assemble nor say why not: {missing:?}"
    );

    // And the tables cannot outlive the files or the tests they name.
    for (fixture, named) in ASSEMBLED.iter().chain(REFUSED) {
        assert!(
            fixtures.iter().any(|name| name == fixture),
            "{fixture} is listed but no longer exists"
        );
        assert!(!named.is_empty(), "{fixture} says nothing");
    }
    let sources = [
        "tests/compile.rs",
        "tests/fullstack.rs",
        "tests/emit_ca65.rs",
        "tests/driver.rs",
    ]
    .iter()
    .map(|path| {
        std::fs::read_to_string(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(path))
            .unwrap_or_default()
    })
    .collect::<Vec<_>>()
    .join("\n");
    for (fixture, test) in ASSEMBLED {
        assert!(
            sources.contains(&format!("fn {test}(")),
            "{fixture} names `{test}`, which no longer exists"
        );
    }
}

/// A row that still departs from its document says what and why.
///
/// Before sub-plan F this was the other way round: every post-optimization
/// listing *had* to state a difference, because K816 could not produce one.
/// Now the listing is what K816 emits, and a stated difference is the
/// exception — so what the audit checks is that an exception is argued rather
/// than left to be rediscovered as a bug.
#[test]
fn every_remaining_difference_from_a_document_listing_is_argued() {
    for row in MATRIX {
        if row.difference.is_empty() {
            continue;
        }
        assert!(
            row.difference.len() > 40,
            "`{}` states a difference too short to be an argument: {}",
            row.section,
            row.difference
        );
    }
}

/// The reserved spellings the syntax reference lists are the ones the
/// compiler reserves, and every one of them is refused.
///
/// "**Every register and flag spelling is reserved as a name.** A `var`,
/// `const`, `func`, label or `inline` alias may not be called
/// `a b x y s p dpr dbr c d i v n z o`, in any case — `K816-E0158`."
///
/// Fifteen spellings, of which the compatibility corpus exercised three. A
/// prose list is a hand-kept list, so this reads the *enumerable* side —
/// `compat::RESERVED_NAMES` in the source — and requires the sentence to
/// match it, then compiles a declaration for each. Adding a spelling to the
/// code now fails until the document says so, and the reverse
/// (`memory/inventory-from-the-enumerable-side`).
#[test]
fn every_reserved_spelling_the_document_lists_is_reserved_and_refused() {
    let doc = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("doc/syntax-reference.md");
    let text = std::fs::read_to_string(&doc).unwrap_or_else(|e| panic!("read {doc:?}: {e}"));
    let sentence = text
        .lines()
        .find(|line| line.starts_with("**Every register and flag spelling is reserved"))
        .expect("the sentence that lists them");
    // The list is the backticked run with the most words in it: the others
    // in this sentence are single keywords and one diagnostic code.
    let listed: Vec<&str> = sentence
        .split('`')
        .skip(1)
        .step_by(2)
        .max_by_key(|run| run.split_whitespace().count())
        .expect("a backticked list of spellings")
        .split_whitespace()
        .collect();

    let compat = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/compat.rs");
    let compat = std::fs::read_to_string(&compat).unwrap_or_else(|e| panic!("read compat.rs: {e}"));
    let body = compat
        .split_once("pub const RESERVED_NAMES: &[&str] = &[")
        .expect("the constant")
        .1
        .split_once("];")
        .expect("its end")
        .0;
    let reserved: Vec<&str> = body.split('"').skip(1).step_by(2).collect();
    assert!(reserved.len() > 10, "the constant parse found {reserved:?}");
    assert_eq!(
        listed, reserved,
        "the document's list and `compat::RESERVED_NAMES` have drifted"
    );

    // Every declaration form the sentence names, in both cases it promises.
    // `var` alone would have left four of the five paths free to regress:
    // the reservation is checked where each kind of name is *declared*, and
    // those are different places in `semantic.rs`.
    const FORMS: &[(&str, &str)] = &[
        ("var", "var {name} = $10\n"),
        ("const", "const {name} = 5\n"),
        ("func", "func {name} @a8 @i8 {{\n  return\n}}\n"),
        ("data", "data {name} {{ $01 }}\n"),
        ("inline", "inline {name} @a8 {{ nop }}\n"),
    ];
    for spelling in &reserved {
        for name in [(*spelling).to_owned(), spelling.to_uppercase()] {
            for (form, template) in FORMS {
                let source = template
                    .replace("{name}", &name)
                    .replace("{{", "{")
                    .replace("}}", "}");
                let mut map = SourceMap::new();
                let file = map.add_file("reserved.k816", &source);
                let result = compile_to_ca65(&map, file, &Options::default());
                let codes: Vec<&str> = result
                    .diagnostics
                    .iter()
                    .map(|diagnostic| diagnostic.code.as_str())
                    .collect();
                assert!(
                    codes.contains(&"K816-E0158"),
                    "`{form} {name}` reported {codes:?}"
                );
            }
        }
    }

    // The one exception the sentence names, gated in the same place as the
    // rule so the two cannot drift: a parameter takes any of the fifteen,
    // expands to what the use site passed, and does not escape the body.
    // `nop` after the use is what makes the expansion observable — an alias
    // that failed to bind would leave `and` naming an undeclared symbol.
    for spelling in &reserved {
        for name in [(*spelling).to_owned(), spelling.to_uppercase()] {
            let source = format!(
                "inline mask @a8 (a, #{name}) -> a {{\n  a & #{name}\n}}\n\n\
                 func main @a8 @i8 {{\n  a = #$FF\n  mask a, #$0F -> a\n}}\n"
            );
            let mut map = SourceMap::new();
            let file = map.add_file("alias.k816", &source);
            let result = compile_to_ca65(&map, file, &Options::default());
            assert_eq!(
                result.diagnostics,
                vec![],
                "`#{name}` is a parameter, not a declaration"
            );
            let assembly = result.assembly.expect("a parameter expands");
            assert!(
                assembly.contains("and     #$0F"),
                "`#{name}` did not bind to what the use site passed:\n{assembly}"
            );
        }
    }

    // And the ambiguity the rule exists to remove still cannot arise inside
    // such a body: a flag letter is a flag in the slots that carry a marker
    // after it, and the parameter everywhere else. Both in one statement run.
    let mut map = SourceMap::new();
    let file = map.add_file(
        "both.k816",
        "inline mask @a8 (a, #c) -> a {\n  c+\n  a & #c\n}\n\n\
         func main @a8 @i8 {\n  a = #$FF\n  mask a, #$0F -> a\n}\n",
    );
    let result = compile_to_ca65(&map, file, &Options::default());
    assert_eq!(
        result.diagnostics,
        vec![],
        "`c+` is `SEC`, `#c` is the parameter"
    );
    let assembly = result.assembly.expect("both readings compile");
    assert!(
        assembly.contains("sec") && assembly.contains("and     #$0F"),
        "one body, two readings of `c`:\n{assembly}"
    );

    // A label is the position that used to be the exception. The eight
    // register spellings are *tokens* — `a = 1` and `x++` need them to be —
    // so `a:` failed to parse before any name was declared and reported
    // `K816-E0001` where the sentence promises `K816-E0158`. The grammar
    // accepts a register spelling in a label's name slot now and the
    // reservation refuses it there like everywhere else, in either case
    // (`postmortem.md` §G.23.1). All fifteen, one answer.
    for spelling in &reserved {
        for name in [(*spelling).to_owned(), spelling.to_uppercase()] {
            let source = format!("func holder @a8 @i8 {{\n{name}:\n  return\n}}\n");
            let mut map = SourceMap::new();
            let file = map.add_file("label.k816", &source);
            let result = compile_to_ca65(&map, file, &Options::default());
            let codes: Vec<&str> = result
                .diagnostics
                .iter()
                .map(|diagnostic| diagnostic.code.as_str())
                .collect();
            assert!(
                codes.contains(&"K816-E0158"),
                "label `{name}:` reported {codes:?}"
            );
        }
    }

    // The counter-claim in the same paragraph, which is what keeps the rule
    // from breaking idiomatic K65: the evaluator's namespace is untouched.
    let evaluator = "data table {\n  for i = 0 .. 3 eval [ i ]\n}\n[ c = 7 ]\n";
    let mut map = SourceMap::new();
    let file = map.add_file("evaluator.k816", evaluator);
    let result = compile_to_ca65(&map, file, &Options::default());
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert!(
        codes.is_empty(),
        "the evaluator is not covered by the reservation: {codes:?}"
    );
}

/// The two lists together account for every section of the document, so a
/// heading that is in neither is a gap rather than an oversight.
#[test]
fn every_section_of_the_specification_is_accounted_for() {
    let path =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("doc/register-width-aware-syntax.md");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
    let headings: Vec<&str> = text
        .lines()
        .filter_map(|line| {
            line.strip_prefix("## ")
                .or_else(|| line.strip_prefix("### "))
        })
        .collect();

    let covered: Vec<&str> = MATRIX
        .iter()
        .map(|row| row.section)
        .chain(NOT_THE_WIDTH_MODELS.iter().map(|(section, _)| *section))
        .collect();
    /// A heading whose own body is prose with nothing to compile.
    const INTRODUCTORY: &[&str] = &["Mode annotations"];
    /// A heading that only introduces the subsections below it. **Not an
    /// excuse**: the audit below checks that every subsection under one is
    /// covered, which is what "Optimisations" earned by ceasing to be a
    /// description of passes that did not exist.
    const CONTAINERS: &[&str] = &["Optimisations"];

    let missing: Vec<&str> = headings
        .iter()
        .filter(|heading| {
            !covered.contains(*heading)
                && !INTRODUCTORY.contains(*heading)
                && !CONTAINERS.contains(*heading)
        })
        .copied()
        .collect();
    assert!(
        missing.is_empty(),
        "specification sections with neither a checked example nor a stated owner: {missing:?}"
    );

    // Every subsection of a container is covered, or the container is an
    // excuse after all.
    for container in CONTAINERS {
        let after = text
            .split_once(&format!("\n## {container}\n"))
            .unwrap_or_else(|| panic!("`{container}` is not a `##` heading"))
            .1;
        let body = after.split("\n## ").next().unwrap_or(after);
        let subsections: Vec<&str> = body
            .lines()
            .filter_map(|line| line.strip_prefix("### "))
            .collect();
        assert!(!subsections.is_empty(), "`{container}` introduces nothing");
        for subsection in subsections {
            assert!(
                covered.contains(&subsection),
                "`{container}` introduces `{subsection}`, which nothing checks"
            );
        }
    }
}

/// Every fixture the width model ships is gated on *encoded bytes* somewhere,
/// not only on emitted text.
///
/// Text can look right while ca65 sizes it differently, which is the exact
/// class of mistake the model exists to prevent, so the gate has to go
/// through the tool that owns encoding.
#[test]
fn every_width_fixture_has_a_byte_level_gate() {
    const GATED: &[(&str, &str)] = &[
        (
            "width-branch.k816",
            "a_skipped_mode_change_does_not_widen_the_immediate_after_the_label",
        ),
        (
            "width-loop.k816",
            "a_loop_head_its_arrivals_agree_about_keeps_its_width",
        ),
        (
            "width-module.k816",
            "module_defaults_reach_the_encoded_immediate_length",
        ),
        (
            "width-bridging.k816",
            "call_site_bridges_are_the_minimum_for_each_contract",
        ),
        (
            "contract-unit-a.k816",
            "a_cross_unit_exit_reaches_the_callers_encoded_immediate",
        ),
        (
            "contract-unit-b.k816",
            "a_cross_unit_exit_reaches_the_callers_encoded_immediate",
        ),
        // These two are gated on diagnostics rather than bytes: what they
        // pin is a *refusal*, and a refused program has no bytes. Their
        // widths are asserted in tests/width.rs.
        (
            "width-emulation.k816",
            "width_emulation_fixture_refuses_only_the_widening_case",
        ),
        (
            "width-blocks.k816",
            "width_blocks_fixture_restores_per_axis_and_reports_the_leak",
        ),
        (
            "width-exit.k816",
            "width_exit_fixture_derives_one_exit_per_shape",
        ),
        (
            "width-native-call.k816",
            "width_native_call_fixture_applies_exits_without_bridging",
        ),
    ];
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let mut ungated = Vec::new();
    for entry in std::fs::read_dir(&dir).expect("read tests/golden") {
        let name = entry
            .expect("entry")
            .file_name()
            .to_string_lossy()
            .into_owned();
        let is_width_fixture = name.starts_with("width-") || name.starts_with("contract-unit-");
        if is_width_fixture && !GATED.iter().any(|(fixture, _)| *fixture == name) {
            ungated.push(name);
        }
    }
    assert!(
        ungated.is_empty(),
        "width fixtures with no named gate: {ungated:?}"
    );
    // And the byte-level half of the list has to be a real majority: a
    // fixture set gated only on diagnostics would not check encoding at all.
    let byte_gated = GATED.len() - 4;
    assert!(
        byte_gated >= 6,
        "only {byte_gated} fixtures reach the bytes"
    );
}
