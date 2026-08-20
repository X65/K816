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
use k816::op::{AxisExit, AxisState, EmulationState, RegisterWidth};
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

/// The error codes only: a body that does a `plp` also earns the
/// assumed-exit warning, which is a separate claim with its own tests.
fn error_codes(diagnostics: &[Diagnostic]) -> Vec<&str> {
    diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.is_error())
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

/// The escape hatch has to stay open for the instruction *after* it.
///
/// A 16-bit register cannot exist in emulation mode, so asserting one
/// asserts native mode too. Without that, the annotation is believed and the
/// very next `rep` is not — the hatch opens and closes again immediately,
/// which is no hatch at all.
#[test]
fn an_asserted_width_keeps_a_later_rep_believable() {
    let source = "\
func main @a8 {
    xce
    @a16
    rep #$20
    lda #$1234
}
";
    assert_eq!(error_codes(&analyze(source).2), Vec::<&str>::new());
    // Statements: xce, the mode set, the rep, the load.
    assert_eq!(before(source, "main", 2).0, known(RegisterWidth::W16));
    // The load is sized 16-bit, which is the whole point of asserting.
    assert_eq!(before(source, "main", 3).0, known(RegisterWidth::W16));
}

/// An *8-bit* assertion says nothing about emulation mode, so it does not
/// open the hatch.
///
/// 8-bit is exactly what emulation mode forces, so `@a8` is equally true in
/// both and carries no evidence. Reading it as an assertion of native mode
/// would let the `rep` below be believed on the strength of a claim that
/// never mentioned it — which is the unsoundness the emulation axis exists
/// to prevent.
#[test]
fn an_eight_bit_assertion_does_not_assert_native_mode() {
    let source = "\
func main @a16 {
    xce
    @a8
    rep #$20
    lda #$1234
}
";
    assert_eq!(error_codes(&analyze(source).2), ["K816-E0104"]);
    // The assertion itself is believed — it is 8-bit either way.
    assert_eq!(before(source, "main", 2).0, known(RegisterWidth::W8));
    // The `rep` after it is not: nothing said emulation mode was off.
    assert_eq!(before(source, "main", 3).0, AxisState::Unknown);
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

/// A branch out of a scoped block lands on a pad that restores the width
/// first, so the target runs at the width its source reads as.
///
/// A restore after the branch would be dead code and one before it would also
/// run on the paths that stay in the block, which is why the edge itself is
/// the only place it can go — and why lowering has to make one
/// (`postmortem.md` §D.6.2).
#[test]
fn a_branch_out_of_a_block_is_restored_on_the_taken_edge() {
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
    assert_eq!(
        codes(&diagnostics),
        Vec::<&str>::new(),
        "the restore is synthesized, so there is nothing to warn about"
    );
    // The width that reaches the target is the function's: the pad put it
    // back before the jump got there.
    assert_eq!(before(source, "main", 3).0, known(RegisterWidth::W8));
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
    assert_eq!(error_codes(&diagnostics), ["K816-E0104"]);
}

/// The unknown-width error names emulation mode when that is the reason, so
/// the reader is not left looking for a missing annotation.
#[test]
fn the_unknown_width_error_names_emulation_when_that_is_the_cause() {
    let (_, _, diagnostics) =
        analyze("func main @a8 {\n    xce\n    rep #$20\n    lda #$1234\n}\n");
    assert_eq!(error_codes(&diagnostics), ["K816-E0104"]);
    let diagnostics: Vec<&Diagnostic> = diagnostics.iter().filter(|d| d.is_error()).collect();
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
        2,
        "the two widenings with no evidence of native mode: {:?}",
        codes(&diagnostics)
    );
    assert!(
        width_errors
            .iter()
            .all(|diagnostic| diagnostic.code == DiagnosticCode::UNKNOWN_WIDTH)
    );
    // The assertion opens the hatch for what follows it, and a narrow one
    // does not open it at all.
    assert_eq!(
        before(&source, "asserted_then_widened", 3).0,
        known(RegisterWidth::W16)
    );
    assert_eq!(
        before(&source, "narrow_assertion_is_not_an_escape_hatch", 3).0,
        AxisState::Unknown
    );
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
        Vec::<&str>::new(),
        "every block in this file restores, the split edge included"
    );

    // `scoped`: statements are the four `lda`s and the two blocks.
    // Before the block's `lda`, 16-bit; after the block, 8-bit again.
    assert_eq!(before(&source, "scoped", 2).0, known(RegisterWidth::W16));
    assert_eq!(before(&source, "scoped", 3).0, known(RegisterWidth::W8));

    // The target of the escaping branch is sized against the width that
    // actually reaches it, which is the block's.
    assert_eq!(before(&source, "splitting", 3).0, known(RegisterWidth::W8));
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

// ---------------------------------------------------------------------------
// Exit inference
// ---------------------------------------------------------------------------

/// The exit contract of `name`, as `(accumulator, index)`.
fn exit_of(text: &str, name: &str) -> (AxisExit, AxisExit) {
    let (_, model, _) = analyze(text);
    let symbol = model
        .symbols
        .lookup(name)
        .unwrap_or_else(|| panic!("`{name}` is not declared"));
    let info = model
        .functions
        .iter()
        .find(|info| info.symbol == symbol)
        .expect("the function was analyzed");
    (info.exit.a, info.exit.i)
}

/// `Preserve` comes from an untouched axis, not from comparing the entry and
/// return states. Deriving it by comparison would make this — the most
/// ordinary shape in the language — exit `Unknown`, warn about it, and push
/// every caller into a needless bridge.
#[test]
fn a_body_that_touches_nothing_preserves_both_widths() {
    assert_eq!(
        exit_of("func f {\n    nop\n}\n", "f"),
        (AxisExit::Preserve, AxisExit::Preserve)
    );
}

#[test]
fn a_body_that_sets_one_axis_fixes_it_and_preserves_the_other() {
    assert_eq!(
        exit_of("func f @a8 {\n    rep #$20\n}\n", "f"),
        (AxisExit::Fixed(RegisterWidth::W16), AxisExit::Preserve)
    );
}

/// Two reachable returns at different fixed widths is a hard error at the
/// callee: a caller has one tracked width, so it cannot follow both paths.
/// The `Unknown ⇒ Preserve` rule never silently masks this.
#[test]
fn returns_at_two_fixed_widths_are_rejected() {
    let (_, _, diagnostics) = analyze(
        "func f @a8 {\n    bcc .wide\n    sep #$20\n    rts\n.wide:\n    rep #$20\n    rts\n}\n",
    );
    assert_eq!(codes(&diagnostics), ["K816-E0119"]);
    assert_eq!(
        diagnostics[0].secondary.len(),
        1,
        "both returns are pointed at"
    );
}

/// An exit-width clause is checked, never established: it states what the
/// body already does, so a mismatch is the body's to fix.
#[test]
fn a_declared_exit_width_a_return_contradicts_is_rejected() {
    let (_, _, diagnostics) = analyze("func f @a8 -> @a16 {\n    nop\n}\n");
    assert_eq!(codes(&diagnostics), ["K816-E0120"]);
}

#[test]
fn a_declared_exit_width_the_body_establishes_is_accepted() {
    let (_, _, diagnostics) = analyze("func f @a8 -> @a16 {\n    rep #$20\n}\n");
    assert_eq!(codes(&diagnostics), Vec::<&str>::new());
}

/// Recursion has no fixpoint to find and no sound answer better than the
/// language's fallback, so its members are pinned to it and told so.
#[test]
fn a_recursion_cycle_is_pinned_to_the_language_rule() {
    let text = "\
func ping @a8 {
    rep #$20
    call pong
}

func pong @a8 {
    rep #$20
    call ping
}
";
    assert_eq!(codes(&text_warnings(text)), ["K816-W0124", "K816-W0124"]);
    assert_eq!(
        exit_of(text, "ping"),
        (AxisExit::Preserve, AxisExit::Preserve),
        "a cycle member takes the fallback rather than a derived exit"
    );
}

/// Self-recursion is a cycle of one, and the same rule applies.
#[test]
fn a_self_call_is_a_cycle() {
    let text = "func f @a8 {\n    rep #$20\n    call f\n}\n";
    assert_eq!(codes(&text_warnings(text)), ["K816-W0124"]);
    assert_eq!(exit_of(text, "f"), (AxisExit::Preserve, AxisExit::Preserve));
}

fn text_warnings(text: &str) -> Vec<Diagnostic> {
    let (_, _, diagnostics) = analyze(text);
    diagnostics
}

/// Callees are classified before their callers, so a caller's tracker
/// advances by an exit that is already final rather than by a placeholder.
/// The order comes from condensing the call graph, not from source order:
/// `caller` is written first here and still sees `callee`'s derived exit.
#[test]
fn a_caller_written_first_still_sees_its_callees_derived_exit() {
    let text = "\
func caller @a8 {
    jsr callee
    lda #$1234
}

func callee @a8 {
    rep #$20
}
";
    let (_, _, diagnostics) = analyze(text);
    assert_eq!(
        codes(&diagnostics),
        Vec::<&str>::new(),
        "the 16-bit immediate is accepted because the callee widens"
    );
    assert_eq!(before(text, "caller", 1).0, known(RegisterWidth::W16));
}

#[test]
fn width_exit_fixture_derives_one_exit_per_shape() {
    let source = fixture("width-exit.k816");
    assert_eq!(
        exit_of(&source, "fixes_both"),
        (
            AxisExit::Fixed(RegisterWidth::W16),
            AxisExit::Fixed(RegisterWidth::W16)
        )
    );
    assert_eq!(
        exit_of(&source, "fixes_a_only"),
        (AxisExit::Fixed(RegisterWidth::W16), AxisExit::Preserve)
    );
    assert_eq!(
        exit_of(&source, "preserves"),
        (AxisExit::Preserve, AxisExit::Preserve)
    );
    assert_eq!(
        exit_of(&source, "loses_both"),
        (AxisExit::Unknown, AxisExit::Unknown)
    );

    // The caller's immediates after each call are the proof that the stored
    // analysis was rebuilt with the derived exits rather than left at the
    // placeholder every call started from.
    let (_, _, diagnostics) = analyze(&source);
    assert_eq!(
        error_codes(&diagnostics),
        Vec::<&str>::new(),
        "every 16-bit immediate in `caller` is accepted: {:?}",
        codes(&diagnostics)
    );
    assert_eq!(before(&source, "caller", 1).0, known(RegisterWidth::W16));
    assert_eq!(before(&source, "caller", 2).1, known(RegisterWidth::W16));
    // A preserving callee moves nothing, so the width before the last load is
    // still the one the previous call left.
    assert_eq!(before(&source, "caller", 6).0, known(RegisterWidth::W16));
}

// ---------------------------------------------------------------------------
// Calls
// ---------------------------------------------------------------------------

/// The bridge before a call establishes the callee's entry contract, and the
/// tracker afterwards is what the callee's exit promises.
#[test]
fn a_call_bridges_to_the_callee_and_advances_by_its_exit() {
    let text = "\
func callee @a16 {
    nop
}

func main @a8 {
    call callee
    lda #$1234
}
";
    let (_, _, diagnostics) = analyze(text);
    assert_eq!(
        codes(&diagnostics),
        Vec::<&str>::new(),
        "the 16-bit immediate is accepted: the bridge left the accumulator wide"
    );
    assert_eq!(before(text, "main", 1).0, known(RegisterWidth::W16));
}

/// A native `jsr` is the escape hatch: no bridge, and the mismatch it leaves
/// is reported rather than silently repaired.
#[test]
fn a_native_call_is_not_bridged_and_says_so() {
    let text = "\
func callee @a8 {
    nop
}

func main @a16 {
    jsr callee
    nop
}
";
    let (_, _, diagnostics) = analyze(text);
    assert_eq!(codes(&diagnostics), ["K816-W0125"]);
    assert!(
        diagnostics[0]
            .help
            .as_ref()
            .is_some_and(|help| help.contains("`call`")),
        "the fix is to use the managed form: {:?}",
        diagnostics[0].help
    );
}

/// A native call whose callee the tracked width already satisfies is silent:
/// the warning is about a mismatch, not about using `jsr`.
#[test]
fn a_native_call_that_already_satisfies_the_contract_is_silent() {
    let text = "\
func callee @a8 {
    nop
}

func main @a8 {
    jsr callee
    nop
}
";
    let (_, _, diagnostics) = analyze(text);
    assert_eq!(codes(&diagnostics), Vec::<&str>::new());
}

/// A `jmp` to a *function* leaves this one through that one, so the exit it
/// reports is that function's rather than its own.
#[test]
fn a_tail_transfer_carries_the_target_functions_exit() {
    let text = "\
func widens @a8 -> @a16 {
    rep #$20
}

func tail @a8 {
    jmp widens
}
";
    assert_eq!(
        exit_of(text, "tail"),
        (AxisExit::Fixed(RegisterWidth::W16), AxisExit::Preserve)
    );
}

#[test]
fn width_bridging_fixture_matches_the_documented_example() {
    let source = fixture("width-bridging.k816");
    let (_, _, diagnostics) = analyze(&source);
    assert_eq!(codes(&diagnostics), Vec::<&str>::new());
    // `main` is uncoloured, so each call's bridge is what establishes the
    // callee's contract; the state after each one is that contract, because
    // every callee here preserves.
    assert_eq!(
        before(&source, "main", 1),
        (known(RegisterWidth::W16), AxisState::Unknown)
    );
    assert_eq!(
        before(&source, "main", 2),
        (known(RegisterWidth::W8), known(RegisterWidth::W8))
    );
    assert_eq!(
        before(&source, "main", 3),
        (known(RegisterWidth::W16), known(RegisterWidth::W16))
    );
}

#[test]
fn width_native_call_fixture_applies_exits_without_bridging() {
    let source = fixture("width-native-call.k816");
    let (_, _, diagnostics) = analyze(&source);
    assert_eq!(
        codes(&diagnostics),
        ["K816-W0125"],
        "only the unsatisfied entry contract is worth saying"
    );
    // The callee's exit reaches the caller even though `jsr` itself changes
    // nothing.
    assert_eq!(
        before(&source, "applies_the_exit", 1).0,
        known(RegisterWidth::W16)
    );
    assert_eq!(
        exit_of(&source, "tail_transfers"),
        (AxisExit::Fixed(RegisterWidth::W16), AxisExit::Preserve)
    );
}

/// The only calls in the legacy corpus are unchecked, in uncoloured
/// functions, so the expected outcome is no bridge and no diagnostic: the
/// width model must not make legacy code louder.
///
/// The HLA statements around them are still sub-plan E's, so the assertion
/// is on the call statements' own analysis rather than on compiling the file.
#[test]
fn the_legacy_corpus_calls_produce_no_bridge_and_no_width_diagnostic() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("examples/k65/atari-xl/landscape/main.k65");
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));
    let mut map = SourceMap::new();
    let id = map.add_file("landscape.k65", source);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "the legacy source still parses");
    let (_, diagnostics) = analyze_file(&parse.file);

    let width_codes: Vec<&str> = diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .filter(|code| code.starts_with("K816-E011") || code.starts_with("K816-W012"))
        .collect();
    assert_eq!(
        width_codes,
        Vec::<&str>::new(),
        "the width model says nothing about unchecked calls in uncoloured functions"
    );

    // What it *does* say, now that sub-plan E2 lets the chains through: an
    // uncoloured function cannot size a width-dependent immediate, so every
    // `a=9` in this legacy `main` is `K816-E0104`.
    //
    // That is a documented conflict, not a bug in the chain lowering.
    // `doc/syntax-reference.md` ("Code Sections") says a `main` block
    // defaults to 8-bit register widths, and `doc/k65-compat.md` says legacy
    // code defaults to 6502-compatible 8-bit semantics throughout; the width
    // model instead treats an unannotated function as unknown, which is the
    // sound reading of a callee whose caller may have widened anything. The
    // corpus cannot compile until that is settled, so it is asserted here —
    // this test fails the day the default lands, which is when someone should
    // be looking at it.
    //
    // The count climbs as more of the file compiles: three when sub-plan E2
    // opened the chains, five once E4 opened the branch bodies around them. A
    // change in the number means more of the corpus reached the width model,
    // which is progress; a change to *none* means J landed.
    let unsizable: Vec<&str> = diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .filter(|code| *code == "K816-E0104")
        .collect();
    assert_eq!(
        unsizable, ["K816-E0104"; 5],
        "the immediates in an uncoloured legacy `main` are still unsizable"
    );
}

/// A tail jump out of a function is not an arrival at every label in it.
///
/// `goto other_function` leaves for a place K816 can name, and a named
/// function's entry is not one of this function's labels. Treating it as an
/// unknown target instead would make every label below it an unknown arrival
/// — the immediate after `.here` would lose its width, and the function's own
/// exit would stop being something a caller can rely on. That is the shape
/// `doc/syntax-reference.md`'s `naked` example is written in, so the
/// distinction has to survive.
#[test]
fn a_tail_jump_out_does_not_erase_the_widths_below_it() {
    let source = "\
func other @a8 @i8 {
    nop
}

func f @a8 @i8 {
    @a16 {
        a?0
        == goto other
    }
.here:
    lda #$12
}
";
    assert_eq!(
        compile_codes(source),
        Vec::<String>::new(),
        "the label below the tail jump keeps the width it falls through with"
    );

    // And the immediate really is sized by that width: one byte, not two.
    let mut map = SourceMap::new();
    let file = map.add_file("tail.k816", source);
    let assembly = k816::compile_to_ca65(&map, file, &k816::Options { optimize: false })
        .assembly
        .expect("compiles");
    assert!(
        assembly.contains("lda     #$12"),
        "an 8-bit immediate, not a widened one:\n{assembly}"
    );
}

// ---------------------------------------------------------------------------
// Call contracts
// ---------------------------------------------------------------------------

/// The full pipeline, because damage tracking is computed after lowering.
fn compile_codes(text: &str) -> Vec<String> {
    let mut map = SourceMap::new();
    let file = map.add_file("contract.k816", text);
    k816::compile_to_ca65(&map, file, &k816::Options { optimize: false })
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str().to_owned())
        .collect()
}

/// A register parameter is legal on every section kind; an alias is not.
/// An alias binds during macro expansion, which only an `inline` has, and a
/// `JSR` has nowhere else to put an argument.
#[test]
fn an_alias_parameter_is_inline_only() {
    let (_, _, diagnostics) = analyze("func f @a8 (#factor) {\n    nop\n}\n");
    assert_eq!(codes(&diagnostics), ["K816-E0121"]);

    let (_, _, diagnostics) = analyze("func f @a8 (a, x) {\n    nop\n}\n");
    assert_eq!(
        codes(&diagnostics),
        Vec::<&str>::new(),
        "register parameters are legal here"
    );
}

#[test]
fn a_call_site_flow_must_match_the_declaration() {
    let text = "\
func add @a8 (a, x) -> a, y {
    nop
}

func caller @a8 {
    add a, x -> a, y
}
";
    assert_eq!(codes(&analyze(text).2), Vec::<&str>::new());
}

#[test]
fn a_call_site_with_the_wrong_registers_is_rejected() {
    let text = "\
func add @a8 (a, x) -> a, y {
    nop
}

func caller @a8 {
    add a -> a
}
";
    // Both halves of the flow disagree, and both are named.
    assert_eq!(codes(&analyze(text).2), ["K816-E0122", "K816-E0122"]);
}

/// A bare call with no flow clause to a contract-bearing function is the
/// silent form of the same mistake: the callee states what it takes and the
/// call site says nothing about it.
#[test]
fn a_bare_call_to_a_contract_bearing_function_must_state_its_flow() {
    let text = "\
func add @a8 (a, x) -> a, y {
    nop
}

func caller @a8 {
    add
}
";
    let (_, _, diagnostics) = analyze(text);
    assert_eq!(codes(&diagnostics), ["K816-E0122"]);
    assert!(
        diagnostics[0]
            .help
            .as_ref()
            .is_some_and(|help| help.contains("call add")),
        "the escape hatch is offered: {:?}",
        diagnostics[0].help
    );
}

/// `call NAME` turns both checks off. That is what the keyword is for.
#[test]
fn the_call_keyword_opts_out_of_the_contract_checks() {
    let text = "\
func add @a8 (a, x) -> a, y {
    nop
}

func caller @a8 {
    call add
}
";
    assert_eq!(codes(&analyze(text).2), Vec::<&str>::new());
}

/// A register the callee writes, that the caller reads before writing it
/// again, and that the contract does not hand back, is damage.
#[test]
fn a_clobbered_register_read_after_a_checked_call_is_reported() {
    let text = "\
func clobbers @a8 @i8 (a) -> a {
    ldy #$00
}

func caller @a8 @i8 {
    ldy #$01
    clobbers a -> a
    sty $0400
}
";
    assert_eq!(compile_codes(text), ["K816-W0123"]);
}

/// A register the contract hands back is not damage: the caller asked for it.
#[test]
fn a_declared_output_register_is_not_damage() {
    let text = "\
func gives_y @a8 @i8 (a) -> a, y {
    ldy #$00
}

func caller @a8 @i8 {
    gives_y a -> a, y
    sty $0400
}
";
    assert_eq!(compile_codes(text), Vec::<String>::new());
}

/// Damage tracking is block-local: past a label, control may arrive from
/// somewhere else and the scan has nothing to say.
#[test]
fn damage_tracking_stops_at_the_end_of_the_block() {
    let text = "\
func clobbers @a8 @i8 (a) -> a {
    ldy #$00
}

func caller @a8 @i8 {
    ldy #$01
    clobbers a -> a
.elsewhere:
    sty $0400
}
";
    assert_eq!(compile_codes(text), Vec::<String>::new());
}

/// `call NAME` opts out of damage tracking too, which is the whole point of
/// the escape hatch: the legacy corpus calls without declaring anything.
#[test]
fn the_call_keyword_opts_out_of_damage_tracking() {
    let text = "\
func clobbers @a8 @i8 {
    ldy #$00
}

func caller @a8 @i8 {
    ldy #$01
    call clobbers
    sty $0400
}
";
    assert_eq!(compile_codes(text), Vec::<String>::new());
}

// ---------------------------------------------------------------------------
// Cross-unit contracts
// ---------------------------------------------------------------------------

/// A cycle that closes through another unit is still a cycle, and neither
/// unit can see it on its own.
#[test]
fn a_cross_unit_recursion_cycle_is_pinned_and_reported() {
    let mut map = SourceMap::new();
    let left = map.add_file(
        "left.k816",
        "func left @a8 {\n    rep #$20\n    call right\n}\n",
    );
    map.add_file(
        "right.k816",
        "func right @a8 {\n    rep #$20\n    call left\n}\n",
    );
    let linkage = k816::linkage::Linkage::resolve(&map);
    assert!(linkage.is_recursive("left"));
    assert!(linkage.is_recursive("right"));
    assert_eq!(
        linkage.contract("left").expect("classified").exit,
        k816::op::ExitContract::preserving(),
        "a cycle has no exit contract to compute"
    );

    let parse = parse_source(map.file(left), None);
    let (_, diagnostics) = k816::semantic::analyze_unit(&parse.file, "left.k816", &linkage);
    assert_eq!(codes(&diagnostics), ["K816-W0124"]);
}

/// The staleness gate: `wrapper` exports the exit it inherits from a callee
/// in another unit, not the `Preserve` a single-unit summary produces.
#[test]
fn contract_unit_fixtures_export_a_derived_cross_unit_exit() {
    let mut map = SourceMap::new();
    map.add_file(
        "contract-unit-a.k816",
        fixture("contract-unit-a.k816").as_str(),
    );
    map.add_file(
        "contract-unit-b.k816",
        fixture("contract-unit-b.k816").as_str(),
    );
    let linkage = k816::linkage::Linkage::resolve(&map);
    assert_eq!(
        linkage.contract("wrapper").expect("classified").exit.a,
        AxisExit::Fixed(RegisterWidth::W8),
        "the exit is derived through the foreign callee, not assumed"
    );
}

/// `RTI` restores a status byte K816 has not tracked, exactly as `PLP` does.
///
/// Unlike `PLP` it is also a *return*, so what it destroys shows up at the
/// function's exit rather than downstream: a body that returns through one
/// promises its caller nothing. It reaches the model through the effect
/// table rather than through a mnemonic match, which is what makes the four
/// unknown-transition instructions one case rather than four; this pins the
/// fourth.
#[test]
fn rti_leaves_no_width_a_caller_can_rely_on() {
    let source = "func handler @a8 @i8 {\n    rti\n}\n";
    assert_eq!(
        exit_of(source, "handler"),
        (AxisExit::Unknown, AxisExit::Unknown)
    );
    let (_, _, diagnostics) = analyze(source);
    assert_eq!(codes(&diagnostics), ["K816-W0124", "K816-W0124"]);
}
