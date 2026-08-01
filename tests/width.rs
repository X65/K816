/*****************************************************************************/
/*                                                                           */
/*                                  width.rs                                 */
/*                                                                           */
/*          The width dataflow: lattice, convergence, and diagnostics        */
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

//! The width solver, examined directly rather than through what it makes the
//! compiler emit.
//!
//! Every assertion here names a state the solver reached at a specific
//! statement. The reason for testing at this level is that the failures the
//! model exists to prevent are silent: a width claimed one instruction too
//! early still assembles, still links, and produces a program whose bytes
//! are shifted from the one that was written.

use k816::ast::{AstFile, ItemKind, StatementKind, StmtId};
use k816::diagnostic::{Diagnostic, DiagnosticCode};
use k816::op::{AxisState, EmulationState, RegisterWidth};
use k816::parser::parse_source;
use k816::semantic::{SemanticModel, analyze_file};
use k816::source::SourceMap;

fn analyze(text: &str) -> (AstFile, SemanticModel, Vec<Diagnostic>) {
    let mut map = SourceMap::new();
    let id = map.add_file("width.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    let (model, diagnostics) = analyze_file(&parse.file);
    (parse.file, model, diagnostics)
}

/// The statement identities of one function's body, in source order,
/// including the ones nested in blocks.
fn statement_ids(file: &AstFile, name: &str) -> Vec<StmtId> {
    for item in &file.items {
        let ItemKind::Func(func) = &item.kind else {
            continue;
        };
        if func.name.text != name {
            continue;
        }
        let mut ids = Vec::new();
        collect(&func.body, &mut ids);
        return ids;
    }
    panic!("no function named `{name}`");
}

fn collect(block: &k816::ast::Block, out: &mut Vec<StmtId>) {
    for statement in &block.statements {
        out.push(statement.id);
        match &statement.kind {
            StatementKind::WidthBlock(width) => collect(&width.body, out),
            StatementKind::Nocross(body) => collect(body, out),
            StatementKind::Loop(loop_statement) => collect(&loop_statement.body, out),
            StatementKind::Conditional(conditional) => {
                collect(&conditional.body, out);
                if let Some(otherwise) = &conditional.otherwise {
                    collect(otherwise, out);
                }
            }
            _ => {}
        }
    }
}

/// The `(accumulator, index)` state the solver tracks before the `index`-th
/// statement of `name`.
fn before(text: &str, name: &str, index: usize) -> (AxisState, AxisState) {
    let (file, model, _) = analyze(text);
    let ids = statement_ids(&file, name);
    let symbol = model
        .symbols
        .lookup(name)
        .unwrap_or_else(|| panic!("`{name}` is not declared"));
    let analysis = model.width(symbol).expect("the function was analyzed");
    let state = analysis
        .before(ids[index])
        .expect("the statement has a tracked state");
    (state.a, state.i)
}

fn known(width: RegisterWidth) -> AxisState {
    AxisState::Known(width)
}

fn codes(diagnostics: &[Diagnostic]) -> Vec<&str> {
    diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect()
}

// ---------------------------------------------------------------------------
// The lattice and its convergence
// ---------------------------------------------------------------------------

/// A conditional branch that skips a mode change reaches the label at a
/// width no single arrival establishes. Sized against the last instruction in
/// the stream rather than against the join, the `nop` here would sit under a
/// 16-bit claim that only one of its two predecessors supports.
#[test]
fn a_merge_of_two_widths_is_unknown_not_the_last_one_seen() {
    let source = "\
func main @a8 {
    bcc .skip
    rep #$20
.skip:
    nop
}
";
    // Statements: bcc, rep, .skip, nop.
    assert_eq!(
        before(source, "main", 3),
        (AxisState::Unknown, AxisState::Unknown)
    );
}

/// The same shape with an *unconditional* branch: the mode change is not on
/// any path to the label, so the label keeps the width its one arrival has.
///
/// This is the program the emitter used to get wrong. Its linear walk applied
/// the skipped `rep` and claimed `.a16` at the label, so the following
/// `lda #$01` assembled as a three-byte `lda #$0001` and every byte after it
/// shifted. The model says 8-bit; emission has to say what the model says.
#[test]
fn a_mode_change_a_branch_skips_never_reaches_the_label() {
    let source = "\
func main @a8 {
    bra .skip
    rep #$20
.skip:
    nop
}
";
    assert_eq!(before(source, "main", 3).0, known(RegisterWidth::W8));
}

/// The same shape with every arrival agreeing keeps the width, so the
/// unknown above is a conclusion rather than a blanket pessimism.
#[test]
fn a_merge_of_one_width_keeps_it() {
    let source = "\
func main @a8 @i8 {
    bra .skip
.skip:
    nop
}
";
    assert_eq!(
        before(source, "main", 2),
        (known(RegisterWidth::W8), known(RegisterWidth::W8))
    );
}

/// A backward edge arrives at a point the forward walk has already passed,
/// so only iteration finds the merge. A solver that quietly became one pass
/// reports `Known(W8)` here.
#[test]
fn a_backward_edge_converges_at_the_loop_head() {
    let source = "\
func main @a8 {
.head:
    nop
    rep #$20
    bra .head
}
";
    // Before the first `nop`: the entry contract on the first arrival, 16-bit
    // on the back edge.
    assert_eq!(
        before(source, "main", 1),
        (AxisState::Unknown, AxisState::Unknown)
    );
}

#[test]
fn a_backward_edge_that_agrees_keeps_the_width() {
    let source = "\
func main @a8 {
.head:
    nop
    sep #$20
    bra .head
}
";
    assert_eq!(before(source, "main", 1).0, known(RegisterWidth::W8));
}

/// Nested loops close two backward edges over the same head, and the inner
/// one's width has to reach the outer one's.
#[test]
fn nested_loops_converge() {
    let source = "\
func main @a8 {
.outer:
    nop
.inner:
    nop
    rep #$20
    bra .inner
    bra .outer
}
";
    let (a, _) = before(source, "main", 3); // the second `nop`
    assert_eq!(a, AxisState::Unknown);
}

/// A block no edge reaches makes no width claim at all, which is different
/// from claiming an unknown one: `Bottom` is what tells emission to stay
/// silent rather than to say "the width became unknown here".
#[test]
fn an_unreachable_block_stays_at_bottom() {
    let source = "\
func main @a8 {
    bra .past
    nop
.past:
    nop
}
";
    // The `nop` between the unconditional branch and its target.
    assert_eq!(
        before(source, "main", 1),
        (AxisState::Bottom, AxisState::Bottom)
    );
}

// ---------------------------------------------------------------------------
// Entry state
// ---------------------------------------------------------------------------

#[test]
fn a_module_default_reaches_a_function_that_declares_nothing() {
    let source = "\
@a16
@i8

func main {
    nop
}
";
    assert_eq!(
        before(source, "main", 0),
        (known(RegisterWidth::W16), known(RegisterWidth::W8))
    );
}

#[test]
fn a_function_annotation_overrides_the_module_default_per_axis() {
    let source = "\
@a16
@i16

func main @a8 {
    nop
}
";
    // The accumulator is overridden; the index register still inherits.
    assert_eq!(
        before(source, "main", 0),
        (known(RegisterWidth::W8), known(RegisterWidth::W16))
    );
}

#[test]
fn a_partial_contract_leaves_the_other_axis_unknown() {
    let source = "func main @a8 {\n    nop\n}\n";
    assert_eq!(
        before(source, "main", 0),
        (known(RegisterWidth::W8), AxisState::Unknown)
    );
}

/// A module default is a file-wide statement, so reading it positionally
/// would make the same annotation mean different things depending on what
/// was inserted above it.
#[test]
fn a_module_default_after_a_code_section_is_rejected() {
    let (_, _, diagnostics) = analyze("func main {\n    nop\n}\n\n@a16\n");
    assert_eq!(codes(&diagnostics), ["K816-E0117"]);
    let fix = &diagnostics[0].fixes[0];
    assert_eq!(fix.edits.len(), 2, "the fix moves rather than deletes");
    assert_eq!(fix.edits[0].span.start, 0, "the default belongs at the top");
}

/// Two annotations constraining one axis differently state two incompatible
/// requirements; picking one silently would make the other a lie.
#[test]
fn contradicting_annotations_are_an_error_not_last_one_wins() {
    let (_, _, diagnostics) = analyze("func main @a8 @a16 {\n    nop\n}\n");
    assert_eq!(codes(&diagnostics), ["K816-E0118"]);
    assert_eq!(diagnostics[0].secondary.len(), 1, "the first is pointed at");
}

#[test]
fn repeating_the_same_annotation_is_accepted() {
    let (_, _, diagnostics) = analyze("func main @a8 @a8 {\n    nop\n}\n");
    assert_eq!(codes(&diagnostics), Vec::<&str>::new());
}

// ---------------------------------------------------------------------------
// Instruction transitions and emulation mode
// ---------------------------------------------------------------------------

#[test]
fn rep_and_sep_establish_their_widths() {
    let source = "\
func main {
    rep #$30
    nop
    sep #$20
    nop
}
";
    assert_eq!(
        before(source, "main", 1),
        (known(RegisterWidth::W16), known(RegisterWidth::W16))
    );
    assert_eq!(
        before(source, "main", 3),
        (known(RegisterWidth::W8), known(RegisterWidth::W16))
    );
}

/// `plp` restores a status byte K816 has not tracked, so both widths are
/// unknown afterwards — inside a fully annotated function too.
#[test]
fn plp_destroys_both_widths_even_under_a_full_contract() {
    let source = "\
func main @a8 @i8 {
    plp
    nop
}
";
    assert_eq!(
        before(source, "main", 1),
        (AxisState::Unknown, AxisState::Unknown)
    );
}

/// The M and X flags are always set in emulation mode, so a `rep` after an
/// `xce` whose result is unknown establishes nothing.
#[test]
fn a_widening_rep_is_not_believed_when_emulation_is_unknown() {
    let source = "\
func main @a8 {
    xce
    rep #$20
    nop
}
";
    assert_eq!(before(source, "main", 2).0, AxisState::Unknown);
}

/// 8-bit is the answer in both modes, so a narrowing `sep` is still believed.
/// The asymmetry is the whole point of tracking emulation at all.
#[test]
fn a_narrowing_sep_is_believed_when_emulation_is_unknown() {
    let source = "\
func main @a16 {
    xce
    sep #$20
    nop
}
";
    assert_eq!(before(source, "main", 2).0, known(RegisterWidth::W8));
}

/// An annotation is a user assertion that native mode holds — the documented
/// escape hatch for the `xce`-then-widen pattern.
#[test]
fn an_annotation_asserts_a_width_emulation_mode_would_refuse() {
    let source = "\
func main @a8 {
    xce
    @a16
    nop
}
";
    assert_eq!(before(source, "main", 2).0, known(RegisterWidth::W16));
}

#[test]
fn xce_leaves_emulation_unknown_for_the_rest_of_the_function() {
    let source = "func main @a8 {\n    xce\n    nop\n}\n";
    let (file, model, _) = analyze(source);
    let ids = statement_ids(&file, "main");
    let symbol = model.symbols.lookup("main").expect("declared");
    let analysis = model.width(symbol).expect("analyzed");
    assert_eq!(
        analysis.before(ids[0]).expect("state").e,
        EmulationState::Native,
        "every function entry is native"
    );
    assert_eq!(
        analysis.before(ids[1]).expect("state").e,
        EmulationState::Unknown
    );
}

// ---------------------------------------------------------------------------
// Scoped blocks and mode sets
// ---------------------------------------------------------------------------

#[test]
fn a_scoped_block_establishes_its_width_and_restores_it() {
    let source = "\
func main @a8 {
    @a16 {
        nop
    }
    nop
}
";
    // Statements: the block, its `nop`, the trailing `nop`.
    assert_eq!(before(source, "main", 1).0, known(RegisterWidth::W16));
    assert_eq!(before(source, "main", 2).0, known(RegisterWidth::W8));
}

#[test]
fn scoped_blocks_nest_and_each_level_restores_independently() {
    let source = "\
func main @a8 @i8 {
    @a16 {
        @i16 {
            nop
        }
        nop
    }
    nop
}
";
    assert_eq!(
        before(source, "main", 2),
        (known(RegisterWidth::W16), known(RegisterWidth::W16))
    );
    assert_eq!(
        before(source, "main", 3),
        (known(RegisterWidth::W16), known(RegisterWidth::W8))
    );
    assert_eq!(
        before(source, "main", 4),
        (known(RegisterWidth::W8), known(RegisterWidth::W8))
    );
}

/// A bare mode set has no restore — that is the whole difference from a
/// scoped block — so it holds to the end of the enclosing block.
#[test]
fn a_bare_mode_set_holds_to_the_end_of_its_block() {
    let source = "\
func main @a8 {
    @a16
    nop
    nop
}
";
    assert_eq!(before(source, "main", 1).0, known(RegisterWidth::W16));
    assert_eq!(before(source, "main", 2).0, known(RegisterWidth::W16));
}

/// A branch out of a scoped block carries the block's width to its target:
/// a restore after the branch would be dead code and one before it would
/// also run on the paths that stay in the block. Nothing is mis-sized —
/// the dataflow carries the truth — but the surprise is worth naming.
#[test]
fn a_branch_out_of_a_block_leaks_its_width_and_says_so() {
    let source = "\
func main @a8 {
    @a16 {
        bra .out
    }
.out:
    nop
}
";
    let (_, _, diagnostics) = analyze(source);
    assert!(
        codes(&diagnostics).contains(&"K816-W0126"),
        "expected the leak warning, got {:?}",
        codes(&diagnostics)
    );
    // The target is sized against the width that actually reaches it, which
    // is the block's rather than the function's.
    assert_eq!(before(source, "main", 3).0, known(RegisterWidth::W16));
}

/// Leaving through a `return` restores nothing and warns about nothing:
/// control leaves the function, so there is no width to restore to.
#[test]
fn a_return_out_of_a_block_is_silent() {
    let source = "\
func main @a8 {
    @a16 {
        return
    }
}
";
    let (_, _, diagnostics) = analyze(source);
    assert!(
        !codes(&diagnostics).contains(&"K816-W0126"),
        "a return is not a leak: {:?}",
        codes(&diagnostics)
    );
}

// ---------------------------------------------------------------------------
// What the tracked state is used for
// ---------------------------------------------------------------------------

/// The state — never the entry contract — sizes and validates an immediate.
/// Before the solver, this program was rejected by analysis and accepted by
/// emission, which is one question with two answers.
#[test]
fn an_immediate_is_sized_by_the_tracked_state_not_the_contract() {
    let (_, _, diagnostics) = analyze("func main @a8 {\n    rep #$20\n    lda #$1234\n}\n");
    assert_eq!(codes(&diagnostics), Vec::<&str>::new());

    let (_, _, diagnostics) = analyze("func main @a16 {\n    sep #$20\n    lda #$1234\n}\n");
    assert_eq!(codes(&diagnostics), ["K816-E0103"]);
}

/// An axis a `plp` destroyed is unknown even inside a function that declares
/// it, so the immediate that depends on it is refused rather than sized
/// against a contract that no longer holds.
#[test]
fn an_immediate_after_plp_is_refused_inside_an_annotated_function() {
    let (_, _, diagnostics) = analyze("func main @a8 {\n    plp\n    lda #$01\n}\n");
    assert_eq!(codes(&diagnostics), ["K816-E0104"]);
}

/// The unknown-width error names emulation mode when that is the reason, so
/// the reader is not left looking for a missing annotation.
#[test]
fn the_unknown_width_error_names_emulation_when_that_is_the_cause() {
    let (_, _, diagnostics) =
        analyze("func main @a8 {\n    xce\n    rep #$20\n    lda #$1234\n}\n");
    assert_eq!(codes(&diagnostics), ["K816-E0104"]);
    assert!(
        diagnostics[0]
            .notes
            .iter()
            .any(|note| note.contains("emulation mode")),
        "the note should explain why the `rep` was not believed: {:?}",
        diagnostics[0].notes
    );
}

// ---------------------------------------------------------------------------
// The fixtures
// ---------------------------------------------------------------------------

fn fixture(name: &str) -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/golden")
        .join(name);
    std::fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {path:?}: {error}"))
}

#[test]
fn width_loop_fixture_separates_the_two_loop_heads() {
    let source = fixture("width-loop.k816");
    let (_, _, diagnostics) = analyze(&source);
    assert_eq!(codes(&diagnostics), Vec::<&str>::new());
    // The diverging head has no width; the agreeing one keeps 8-bit. Both
    // conclusions come from the same solver run over the same shape.
    assert_eq!(before(&source, "diverging", 1).0, AxisState::Unknown);
    assert_eq!(before(&source, "agreeing", 1).0, known(RegisterWidth::W8));
}

/// The rejection the fixture's comment refers to: a width-dependent
/// immediate at a head no arrival agrees about cannot be sized.
#[test]
fn a_width_dependent_immediate_at_a_diverging_loop_head_is_rejected() {
    let (_, _, diagnostics) =
        analyze("func diverging @a8 {\n.head:\n    lda #$01\n    rep #$20\n    bra .head\n}\n");
    assert_eq!(codes(&diagnostics), ["K816-E0104"]);
}

#[test]
fn width_emulation_fixture_refuses_only_the_widening_case() {
    let source = fixture("width-emulation.k816");
    let (_, _, diagnostics) = analyze(&source);
    let width_errors: Vec<&Diagnostic> = diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.code == DiagnosticCode::UNKNOWN_WIDTH
                || diagnostic.code == DiagnosticCode::IMMEDIATE_RANGE
        })
        .collect();
    assert_eq!(
        width_errors.len(),
        1,
        "only the widening `rep` is refused: {:?}",
        codes(&diagnostics)
    );
    assert_eq!(width_errors[0].code, DiagnosticCode::UNKNOWN_WIDTH);
    // The narrowing and asserted cases both track a known width.
    assert_eq!(
        before(&source, "narrow_after_xce", 2).0,
        known(RegisterWidth::W8)
    );
    assert_eq!(
        before(&source, "asserted_after_xce", 2).0,
        known(RegisterWidth::W16)
    );
}

/// The document's block-scoping example, with the emitted mode operations
/// checked against what the *model* produces rather than what the document's
/// listing shows: that listing is post-optimization, and dead mode
/// elimination and REP/SEP folding belong to the optimizer.
#[test]
fn width_blocks_fixture_restores_per_axis_and_reports_the_leak() {
    let source = fixture("width-blocks.k816");
    let (_, _, diagnostics) = analyze(&source);
    assert_eq!(
        codes(&diagnostics),
        ["K816-W0126"],
        "the escaping branch is the only thing worth saying about this file"
    );

    // `scoped`: statements are the four `lda`s and the two blocks.
    // Before the block's `lda`, 16-bit; after the block, 8-bit again.
    assert_eq!(before(&source, "scoped", 2).0, known(RegisterWidth::W16));
    assert_eq!(before(&source, "scoped", 3).0, known(RegisterWidth::W8));

    // The target of the escaping branch is sized against the width that
    // actually reaches it, which is the block's.
    assert_eq!(before(&source, "leaking", 3).0, known(RegisterWidth::W16));
}

/// Module defaults reach a function that declares nothing, and a
/// function-level annotation overrides them one axis at a time.
#[test]
fn width_module_fixture_colors_each_function() {
    let source = fixture("width-module.k816");
    let (_, _, diagnostics) = analyze(&source);
    assert_eq!(codes(&diagnostics), Vec::<&str>::new());

    assert_eq!(
        before(&source, "inherits", 0),
        (known(RegisterWidth::W16), known(RegisterWidth::W16))
    );
    assert_eq!(
        before(&source, "narrow", 0),
        (known(RegisterWidth::W8), known(RegisterWidth::W8))
    );
    // A partial override leaves the other axis inheriting.
    assert_eq!(
        before(&source, "half", 0),
        (known(RegisterWidth::W8), known(RegisterWidth::W16))
    );
}
