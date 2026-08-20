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
//! **Several of the document's listings are post-optimization.** Dead mode
//! elimination and `REP`/`SEP` folding are the optimizer's, not the width
//! model's, so K816 emits *more* mode operations than those listings show —
//! two single-bit `rep`s where the document prints one folded `rep #$30`.
//! Each such row is marked [`Stage::Optimized`] and states the difference.
//! Anyone "fixing" the expectations here to match those listings verbatim is
//! re-implementing the optimizer inside the width model.
//!
//! The document is not read at run time, so its sections and snippets are
//! transcribed. A section whose example changes and is not transcribed here
//! stays green — which is why the rows name the section heading, so a reader
//! comparing the two can see what is missing.

use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

/// Which pass owns the output a document listing shows.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Stage {
    /// K816's own output, as the width model emits it.
    Modelled,
    /// The listing is what the optimizer produces. The row asserts the
    /// unfolded, un-eliminated equivalent and `difference` names what F does
    /// to it.
    Optimized,
}

/// One documented example, and the claim it turns into.
struct Row {
    /// The `##` heading in `doc/register-width-aware-syntax.md`.
    section: &'static str,
    source: &'static str,
    stage: Stage,
    /// What sub-plan F changes about the emitted stream, for an `Optimized`
    /// row. Empty otherwise.
    difference: &'static str,
    /// Emitted lines the compiled snippet must contain, in order.
    expected: &'static [&'static str],
    /// A diagnostic code the snippet must report, if the document says so.
    diagnostic: Option<&'static str>,
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
    },
    Row {
        section: "The entry point",
        // Unannotated, and still sized: the widths are the environment's.
        source: "func main {\n    lda #$01\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[".a8", ".i8", "lda     #$01"],
        diagnostic: None,
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
    },
    Row {
        section: "The entry point",
        // Annotated with what it does not: the document prints this error.
        source: "func main @a16 {\n    lda #$1234\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &[],
        diagnostic: Some("K816-E0143"),
    },
    Row {
        section: "The entry point",
        // And the way to get wider registers in `main`: widen the body.
        source: "func main @a8 {\n    @a16 {\n        lda #$1234\n    }\n}\n",
        stage: Stage::Modelled,
        difference: "",
        expected: &["rep     #$20", "lda     #$1234", "sep     #$20"],
        diagnostic: None,
    },
    Row {
        section: "Call-site bridging",
        source: "\
func wide_a @a16 {
    nop
}

func narrow @a8 @i8 {
    nop
}

func wide_all @a16 @i16 {
    nop
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
        difference: "the listing folds `sep #$20` + `sep #$10` into `sep #$30`, \
                     and likewise for the two `rep`s; folding is F's",
        expected: &[
            "rep     #$20",
            "jsr     a:wide_a",
            "sep     #$20",
            "sep     #$10",
            "jsr     a:narrow",
            "rep     #$20",
            "rep     #$10",
            "jsr     a:wide_all",
            "jsr     a:uncolored",
        ],
        diagnostic: None,
    },
    Row {
        // The document writes the branch as `c+? goto .loop`, which is
        // sub-plan E's spelling of the native branch used here. The width
        // behaviour — a bridge anchored at the label, so arrivals by branch
        // get it too — is the same one.
        section: "Label-site bridging",
        source: "\
func main @a8 @i8 {
    bcs .loop
    @a16
.loop:
    lda #1
}
",
        stage: Stage::Modelled,
        difference: "",
        expected: &["bcs     @loop", "rep     #$20", "lda     #$0001"],
        diagnostic: None,
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
        difference: "the listing folds the block's two `rep`s into `rep #$30` and \
                     drops the restore of an axis the next block re-establishes",
        expected: &[
            "lda     #$01",
            "rep     #$20",
            "lda     #$0001",
            "sep     #$20",
            "lda     #$01",
            "rep     #$20",
            "rep     #$10",
            "lda     #$0001",
            "ldx     #$0001",
            "sep     #$20",
            "sep     #$10",
            "lda     #$01",
        ],
        diagnostic: None,
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
        difference: "the listing folds `main`'s two entry `rep`s into `rep #$30`",
        expected: &[
            "lda     #$0001",
            "ldx     #$0001",
            "lda     #$01",
            "ldx     #$01",
        ],
        diagnostic: None,
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
        "Interaction with dead mode elimination",
        "sub-plan F: the listing is what dead mode elimination produces from \
         the stream the width model emits",
    ),
    ("Dead mode elimination", "sub-plan F, phase 9 item 1"),
    ("REP/SEP folding", "sub-plan F, phase 9 item 2"),
    (
        "Compilation pipeline",
        "prose: the stage list, not a program",
    ),
    (
        "Listing disassembly",
        "ld65's listing output, which K816 does not produce",
    ),
];

fn compile(source: &str) -> (Vec<String>, Vec<String>) {
    let mut map = SourceMap::new();
    let file = map.add_file("doc.k816", source);
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
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
        let (lines, codes) = compile(row.source);
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

/// Every `Optimized` row states what the optimizer changes about it, so the
/// difference from the document is recorded rather than left to be
/// rediscovered as a bug.
#[test]
fn every_post_optimization_listing_names_its_difference() {
    for row in MATRIX {
        match row.stage {
            Stage::Optimized => assert!(
                !row.difference.is_empty(),
                "`{}` is a post-optimization listing with no difference stated",
                row.section
            ),
            Stage::Modelled => assert!(
                row.difference.is_empty(),
                "`{}` states a difference but is not marked post-optimization",
                row.section
            ),
        }
    }
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
    // Headings that only introduce the ones below them.
    const INTRODUCTORY: &[&str] = &["Mode annotations", "Optimisations"];

    let missing: Vec<&str> = headings
        .iter()
        .filter(|heading| !covered.contains(*heading) && !INTRODUCTORY.contains(*heading))
        .copied()
        .collect();
    assert!(
        missing.is_empty(),
        "specification sections with neither a checked example nor a stated owner: {missing:?}"
    );
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
