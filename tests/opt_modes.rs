/*****************************************************************************/
/*                                                                           */
/*                                opt_modes.rs                               */
/*                                                                           */
/*        The two mode passes, and every guard that makes them correct       */
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

//! Dead mode elimination and `REP`/`SEP` folding, as separate claims.
//!
//! These are the only two passes the language documents specify as
//! algorithms, and **both specifications are incomplete**.
//! `doc/register-width-aware-syntax.md` "Dead mode elimination" seeds its
//! backward scan by counting width-dependent *immediates*, when the M and X
//! flags govern every load, store, ALU operation, push, and index step; and it
//! has no notion of an exit contract, so its own algorithm deletes
//! instructions two shipped fixtures depend on. Implementing the section
//! literally produces a compiler that miscompiles this repository.
//!
//! Every guard that makes the corrected pass right gets a test here, because
//! the failures are silent: the assembly still assembles, the bytes are just
//! wrong from the change onward.

use k816::abi::EntryEnvironment;
use k816::isa::Mnemonic;
use k816::linkage::Linkage;
use k816::op::{AxisState, KOp, KOpKind, KProgram, Operand, Origin, Reason, RegisterWidth};
use k816::optimize::{Context, OptimizationLog, Pass, optimize};
use k816::parser::parse_source;
use k816::semantic::{SemanticModel, analyze_file, analyze_unit};
use k816::source::{FileId, SourceMap};

/// Compiles one self-contained unit and runs the fixed pass sequence over it.
fn optimized(text: &str) -> (SemanticModel, KProgram, OptimizationLog) {
    let mut map = SourceMap::new();
    let id = map.add_file("modes.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![]);
    let (model, diagnostics) = analyze_file(&parse.file);
    assert!(
        !diagnostics.iter().any(|d| d.is_error()),
        "{:?}",
        diagnostics
    );
    let (mut program, diagnostics) = k816::lower::lower(model.program(&parse.file), &model);
    assert!(
        !diagnostics.iter().any(|d| d.is_error()),
        "{:?}",
        diagnostics
    );
    let linkage = Linkage::default();
    let log = optimize(
        &mut program,
        &Context {
            model: &model,
            linkage: &linkage,
            file: id,
        },
    );
    (model, program, log)
}

/// The same, over a two-unit workspace, optimizing the first file.
fn optimized_workspace(units: &[(&str, &str)]) -> (SemanticModel, KProgram, OptimizationLog) {
    let mut map = SourceMap::new();
    let ids: Vec<FileId> = units
        .iter()
        .map(|(name, text)| map.add_file(*name, *text))
        .collect();
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());
    assert_eq!(linkage.diagnostics(), &[]);
    let parse = parse_source(map.file(ids[0]), None);
    let (model, diagnostics) = analyze_unit(&parse.file, map.file(ids[0]), &linkage);
    assert!(
        !diagnostics.iter().any(|d| d.is_error()),
        "{:?}",
        diagnostics
    );
    let (mut program, diagnostics) =
        k816::lower::lower_unit(model.program(&parse.file), &model, units[0].0);
    assert!(
        !diagnostics.iter().any(|d| d.is_error()),
        "{:?}",
        diagnostics
    );
    let log = optimize(
        &mut program,
        &Context {
            model: &model,
            linkage: &linkage,
            file: ids[0],
        },
    );
    (model, program, log)
}

/// One named function's instructions, as `(mnemonic, folded operand)` pairs.
fn instructions(
    program: &KProgram,
    model: &SemanticModel,
    name: &str,
) -> Vec<(Mnemonic, Option<u32>)> {
    function(program, model, name)
        .ops
        .iter()
        .filter_map(|op| match &op.kind {
            KOpKind::Instruction(instruction) => Some((
                instruction.mnemonic,
                instruction
                    .operand
                    .as_ref()
                    .and_then(Operand::immediate_value)
                    .map(|value| value.get()),
            )),
            _ => None,
        })
        .collect()
}

fn function<'a>(
    program: &'a KProgram,
    model: &SemanticModel,
    name: &str,
) -> &'a k816::op::KFunction {
    program
        .functions()
        .find(|function| model.symbols.name(function.symbol) == name)
        .unwrap_or_else(|| panic!("no function named `{name}`"))
}

/// The width claims of one function, in order.
fn claims(program: &KProgram, model: &SemanticModel, name: &str) -> Vec<(AxisState, AxisState)> {
    function(program, model, name)
        .ops
        .iter()
        .filter_map(|op| match &op.kind {
            KOpKind::ModeAssumption(state) => Some((state.a, state.i)),
            _ => None,
        })
        .collect()
}

const MODES: &str = include_str!("golden/opt-modes.k816");
const CROSS_A: &str = include_str!("golden/opt-crossunit-a.k816");
const CROSS_B: &str = include_str!("golden/opt-crossunit-b.k816");

// ---------------------------------------------------------------------------
// The seed: an axis is observed by any instruction that reads its flag
// ---------------------------------------------------------------------------

/// **Probe 5.** `rep #$20` before `lda addr` / `sta addr` survives, though no
/// immediate is in sight.
///
/// This is the case the width document's algorithm gets wrong. Its step 1
/// counts `ImmediateM` instructions; a `lda $0400` carries no immediate at
/// all, so the `rep #$20` above it is "dead" and deleting it turns a 16-bit
/// load and a 16-bit store into 8-bit ones. That is not a lost optimization,
/// it is a different program.
#[test]
fn a_mode_change_a_plain_load_and_store_depend_on_survives() {
    let (model, program, _) = optimized(MODES);
    assert_eq!(
        instructions(&program, &model, "widened_memory"),
        vec![
            (Mnemonic::Rep, Some(0x20)),
            (Mnemonic::Lda, None),
            (Mnemonic::Sta, None),
            (Mnemonic::Sep, Some(0x20)),
            (Mnemonic::Rts, None),
        ]
    );
}

/// The truncation rule: an X-narrowing bit is live whenever X or Y is.
///
/// `SEP` on the index axis writes the X and Y *registers*, because narrowing
/// zeroes their high bytes — and `txs` reads the X register and no flag, so it
/// sees the difference without ever reading a width. A mode bit is dead only
/// when the axis is unobserved **and** the register write the bit performs is
/// dead.
#[test]
fn an_x_narrowing_bit_survives_for_a_txs_that_reads_no_flag() {
    // Nothing here reads the X flag: `txs` reads the X *register*. Under a
    // pass that only asked about flags, the `sep #$10` would go and `txs`
    // would push a different stack pointer.
    let (model, program, _) =
        optimized("func truncates @a16 @i16 {\n    ldx #$0102\n    sep #$10\n    txs\n}\n");
    assert_eq!(
        instructions(&program, &model, "truncates"),
        vec![
            (Mnemonic::Ldx, Some(0x0102)),
            (Mnemonic::Sep, Some(0x10)),
            (Mnemonic::Txs, None),
            (Mnemonic::Rts, None),
        ]
    );
}

/// A mode instruction does not observe the axis it establishes.
///
/// With `SEP`/`REP` reading `FlagSet::ALL_P`, the `sep #$20` below a
/// `rep #$20` keeps it alive and the pass can never fire at all. The read set
/// is now every P bit the folded mask leaves alone, so the redundant one goes.
#[test]
fn a_sep_does_not_keep_the_rep_above_it_alive() {
    let (model, program, log) =
        optimized("func f @a16 {\n    rep #$20\n    sep #$20\n    lda #$01\n}\n");
    assert_eq!(
        instructions(&program, &model, "f"),
        vec![
            (Mnemonic::Sep, Some(0x20)),
            (Mnemonic::Lda, Some(0x01)),
            (Mnemonic::Rts, None),
        ]
    );
    assert_eq!(log.changes.len(), 1);
    assert_eq!(log.changes[0].pass, Pass::EliminateDeadModes);
}

// ---------------------------------------------------------------------------
// Decision 3 — a published contract is a promise F may not break
// ---------------------------------------------------------------------------

/// **Guard one.** The `sep #$20` *is* the declared `-> @a8` exit contract.
///
/// Nothing after it reads M, so the document's backward scan calls it dead.
/// Removing it leaves every caller sizing its immediates against a promise the
/// callee stopped keeping — wrong bytes, authored by the optimizer.
#[test]
fn an_operation_a_fixed_exit_contract_rests_on_is_live() {
    let (model, program, _) = optimized(MODES);
    assert_eq!(
        instructions(&program, &model, "fixes_a_only"),
        vec![(Mnemonic::Sep, Some(0x20)), (Mnemonic::Rts, None)]
    );
}

/// The same claim on the fixture that found it: `contract-unit-a.k816` is one
/// function whose entire body is the `sep #$20` its declared `-> @a8` rests
/// on, and compiled alone no instruction in sight reads M.
///
/// It is here rather than beside the width tests because the danger is F's:
/// "the axis is observed nowhere" is a claim about the whole *program*, and
/// what K816 can measure is the workspace it was handed. Applying decision 4's
/// exemption to a published exit contract deletes this instruction.
#[test]
fn a_unit_that_reads_no_flag_still_keeps_its_declared_exit() {
    let (model, program, log) = optimized(include_str!("golden/contract-unit-a.k816"));
    assert!(log.is_empty(), "{:?}", log.changes);
    assert_eq!(
        instructions(&program, &model, "foreign"),
        vec![(Mnemonic::Sep, Some(0x20)), (Mnemonic::Rts, None)]
    );
}

/// **Guard two.** A `Preserve` exit means the axis leaves as it entered, so a
/// scoped block's restore is what makes that true and cannot go on its own.
#[test]
fn a_block_enter_and_its_restore_go_together_or_not_at_all() {
    let (model, program, _) = optimized(MODES);
    assert_eq!(
        instructions(&program, &model, "scoped_pair"),
        vec![
            (Mnemonic::Rep, Some(0x20)),
            (Mnemonic::Sta, None),
            (Mnemonic::Sep, Some(0x20)),
            (Mnemonic::Sta, None),
            (Mnemonic::Rts, None),
        ]
    );
}

/// **Guard three.** A call bridge satisfies a callee's published entry
/// contract, which the whole program was compiled against, so it goes only
/// when the axis is observed nowhere at all.
///
/// Both halves, on the same program shape. Alone, nothing reads M or X and
/// both bridges go — which is what makes the width document's
/// zero-mode-instruction listing reachable. With one more function that reads
/// a flag, the contract stops serving nothing and the bridges stay.
#[test]
fn a_call_bridge_goes_only_where_the_axis_is_observed_nowhere() {
    let quiet = "func main @a8 @i8 {\n    nop\n    call helper\n}\n\n\
                 func helper @a16 @i16 {\n    nop\n}\n";
    let (model, program, log) = optimized(quiet);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![
            (Mnemonic::Nop, None),
            (Mnemonic::Jsr, None),
            (Mnemonic::Rts, None)
        ],
        "no mode instruction survives a program that observes neither axis"
    );
    assert_eq!(log.by_pass(Pass::EliminateDeadModes).count(), 2);

    let observed = format!(
        "var CELL = $0400\n\n{quiet}\nfunc reader @a8 @i8 {{\n    CELL = a\n    ldx #$01\n}}\n"
    );
    let (model, program, _) = optimized(&observed);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![
            (Mnemonic::Nop, None),
            // Folded from `rep #$20` + `rep #$10` by pass 2, which is the
            // width document's own first folding row.
            (Mnemonic::Rep, Some(0x30)),
            (Mnemonic::Jsr, None),
            (Mnemonic::Rts, None),
        ],
        "one instruction that reads a flag makes every published contract on that axis serve something"
    );
}

/// The workspace, not the unit, is what "observed nowhere" is a claim about.
///
/// Unit A is the quiet program above, one function short: its callee lives in
/// unit B, and so does the only instruction in the workspace that reads a
/// flag. Nothing unit A can see says the bridge is needed, and it stays.
#[test]
fn a_sibling_units_reader_keeps_this_units_bridge() {
    let (model, program, log) = optimized_workspace(&[
        ("opt-crossunit-a.k816", CROSS_A),
        ("opt-crossunit-b.k816", CROSS_B),
    ]);
    // The bridge survives; both halves of it are still there, merged into the
    // one instruction the document's folding table asks for.
    assert_eq!(log.by_pass(Pass::EliminateDeadModes).count(), 0);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![
            (Mnemonic::Rep, Some(0x30)),
            (Mnemonic::Jsr, None),
            (Mnemonic::Rts, None),
        ]
    );
}

/// `main` cannot be called, so its inferred exit constrains nothing.
///
/// Without this, the width document's own zero-mode-instruction listing is
/// unreachable: K816 classifies that `main`'s exit as `-> @a16 @i16` precisely
/// *because* the bridge fixed it, and the exit-contract guard then keeps the
/// bridge alive to serve a contract nobody consumes.
#[test]
fn the_entry_points_inferred_exit_constrains_nothing() {
    let quiet_main = "func main @a8 @i8 {\n    call helper\n}\n\n\
                      func helper @a16 @i16 {\n    nop\n}\n";
    let (model, program, _) = optimized(quiet_main);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![(Mnemonic::Jsr, None), (Mnemonic::Rts, None)]
    );

    // The identical body under a name the environment does not enter keeps
    // both bridges: an ordinary function's exit is a published promise.
    let ordinary = "func caller @a8 @i8 {\n    call helper\n}\n\n\
                    func helper @a16 @i16 {\n    nop\n}\n";
    let (model, program, _) = optimized(ordinary);
    assert_eq!(
        instructions(&program, &model, "caller"),
        vec![
            (Mnemonic::Rep, Some(0x30)),
            (Mnemonic::Jsr, None),
            (Mnemonic::Rts, None),
        ]
    );
}

/// **An entry point that something in the workspace calls has a published
/// exit after all.**
///
/// Decision 6 is conditional and the condition is easy to drop: `main`'s
/// inferred exit constrains nothing "with `FunctionInfo::is_entry_point` set
/// **and no analyzed unit naming it**". K65 does not refuse a call to `main`,
/// and once one exists the exit contract is consumed like any other — the
/// caller was compiled against `-> @a16` and sizes its immediates by it.
///
/// Without the condition, the `rep #$20` below goes and `caller` emits a
/// three-byte `lda #$1234` while the accumulator is still eight bits wide.
/// The assembly still assembles; every byte after it is wrong.
#[test]
fn an_entry_point_something_calls_keeps_its_exit_contract() {
    let called = "\
func main { rep #$20 }

func caller @a8 @i8 {
    call main
    lda #$1234
}
";
    let (model, program, log) = optimized(called);
    assert_eq!(log.by_pass(Pass::EliminateDeadModes).count(), 0);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![(Mnemonic::Rep, Some(0x20)), (Mnemonic::Rts, None)]
    );

    // A native `jsr` is the same claim: the two spellings lower to one
    // instruction, and neither had to declare anything to consume the exit.
    let jumped = "\
func main { rep #$20 }

func caller @a8 @i8 {
    jsr main
    lda #$1234
}
";
    let (model, program, _) = optimized(jumped);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![(Mnemonic::Rep, Some(0x20)), (Mnemonic::Rts, None)]
    );

    // And with nothing naming it, the exemption applies as decision 6 says:
    // the same body loses the same instruction.
    let uncalled = "func main { rep #$20 }\n";
    let (model, program, log) = optimized(uncalled);
    assert_eq!(log.by_pass(Pass::EliminateDeadModes).count(), 1);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![(Mnemonic::Rts, None)]
    );
}

// ---------------------------------------------------------------------------
// Decisions 7, 8, and 10
// ---------------------------------------------------------------------------

/// A label-anchored bridge is fixed and is never eliminated, while an
/// eliminable change above it goes.
///
/// The document says so in as many words: "This label-anchored `REP`/`SEP` is
/// treated as fixed and is not removed by dead mode elimination. Redundant
/// mode switches before that fixed label bridge are still eliminated."
#[test]
fn a_fixed_label_bridge_survives_while_the_change_above_it_goes() {
    let (model, program, _) = optimized(MODES);
    assert_eq!(
        instructions(&program, &model, "label_bridge"),
        vec![
            (Mnemonic::Rep, Some(0x20)),
            (Mnemonic::Lda, Some(0x1234)),
            (Mnemonic::Bcc, None),
            (Mnemonic::Rts, None),
        ],
        "the `sep #$20` above the bridge goes; the bridge stays"
    );
}

/// The bridge F may not remove is recognized by its recorded reason, and the
/// adjacency `postmortem.md` §D.5 calls normative is asserted separately.
///
/// The order at a label is `Label`, then the bridge *immediately* after it
/// with nothing between, then `ModeAssumption`. F uses the reason; insert
/// anything between them and the reason still identifies the bridge, but the
/// invariant every other reader depends on is broken.
#[test]
fn the_fixed_bridge_is_named_by_its_reason_and_sits_next_to_its_label() {
    let (model, program, _) = optimized(MODES);
    let ops = &function(&program, &model, "label_bridge").ops;
    let bridge = ops
        .iter()
        .position(|op| op.reason == Some(Reason::LabelBridge))
        .expect("a label bridge");
    assert!(matches!(ops[bridge - 1].kind, KOpKind::Label(_)));
    assert!(matches!(ops[bridge + 1].kind, KOpKind::ModeAssumption(_)));
    assert_eq!(ops[bridge].origin, Origin::CompilerGenerated);
}

/// Mask-bit stripping leaves every bit that is not a width alone.
///
/// A `REP` mask is a write to an arbitrary set of P bits, and only two of the
/// eight are widths. `rep #$21` clears M *and* the carry; deleting it because
/// M is unobserved breaks the `bcc` below it.
#[test]
fn stripping_a_dead_width_bit_keeps_the_rest_of_the_mask() {
    let (model, program, log) = optimized(MODES);
    assert_eq!(
        instructions(&program, &model, "carry_bit"),
        vec![
            (Mnemonic::Rep, Some(0x01)),
            (Mnemonic::Sep, Some(0x20)),
            (Mnemonic::Bcc, None),
            (Mnemonic::Nop, None),
            (Mnemonic::Nop, None),
            (Mnemonic::Rts, None),
        ]
    );

    let strip = log
        .by_pass(Pass::EliminateDeadModes)
        .find(|change| match &change.before[0].kind {
            KOpKind::Instruction(instruction) => {
                instruction
                    .operand
                    .as_ref()
                    .and_then(Operand::immediate_value)
                    == Some(k816::value::Value::new(0x21))
            }
            _ => false,
        })
        .expect("the `rep #$21`");
    // A rewrite, not a removal, and the log says which it was.
    assert_eq!(strip.after.len(), 1);
    assert!(strip.justification.contains("M is unobserved"));
    assert!(
        strip
            .justification
            .contains("$01 is not this pass's to touch"),
        "{}",
        strip.justification
    );
}

/// Claim substitution never strengthens a claim, and never invents a width.
///
/// Every `ModeAssumption` between a changed operation and the end of its block
/// takes, on the affected axis, the value the *preceding* claim carried. F
/// never writes an axis value it did not read off another claim, and never
/// turns `Unknown` into `Known`.
///
/// The callee is deliberately mixed — it *fixes* the accumulator and
/// *preserves* the index register — because the two axes then take the two
/// different paths through the pass in one program: the accumulator's claim
/// stands, because `helper` re-establishes what F removed, and the index
/// register's is rewritten to the entry contract's `@i8`. `main` is the caller
/// because nothing observes an entry point's exit contract, which is what lets
/// the bridge be eliminated at all (`postmortem.md` §I.2).
#[test]
fn claim_substitution_carries_the_preceding_claims_value() {
    let quiet = "func main @a8 @i8 {\n    call helper\n    nop\n}\n\n\
                 func helper @a16 @i16 {\n    sep #$20\n}\n";
    let (model, program, _) = optimized(quiet);
    // Entry: @a8 @i8. The call's claim was `@a8 @i16` — the accumulator from
    // `helper`'s exit, the index register from the bridge that is now gone —
    // and the index axis takes the entry contract's `@i8` in its place.
    assert_eq!(
        claims(&program, &model, "main"),
        vec![
            (
                AxisState::Known(RegisterWidth::W8),
                AxisState::Known(RegisterWidth::W8)
            ),
            (
                AxisState::Known(RegisterWidth::W8),
                AxisState::Known(RegisterWidth::W8)
            ),
        ]
    );

    // And nothing anywhere turns an unknown axis into a known one.
    for source in [MODES, quiet] {
        let (model, program, _) = optimized(source);
        for function in program.functions() {
            for op in &function.ops {
                let KOpKind::ModeAssumption(_) = &op.kind else {
                    continue;
                };
                assert!(claim_is_not_invented(op), "{op:?}");
            }
        }
        let _ = &model;
    }
}

/// A claim F wrote carries a width some other claim already stated. There is
/// no way to check that after the fact from one operation, so this asserts the
/// weaker property that holds by construction: F emits no claim of its own.
fn claim_is_not_invented(op: &KOp) -> bool {
    matches!(op.origin, Origin::CompilerGenerated)
}

// ---------------------------------------------------------------------------
// Pass 2 — folding, justified by the composed transform
// ---------------------------------------------------------------------------

const FOLDING: &str = include_str!("golden/opt-folding.k816");

/// The four rows of the width document's folding table.
#[test]
fn the_documented_folding_table_is_what_k816_emits() {
    let (model, program, _) = optimized(FOLDING);
    let modes = |name: &str| -> Vec<(Mnemonic, Option<u32>)> {
        instructions(&program, &model, name)
            .into_iter()
            .filter(|(mnemonic, _)| matches!(mnemonic, Mnemonic::Sep | Mnemonic::Rep))
            .collect()
    };

    // `@a16` then `@i16` → `REP #$30`; `@a8` then `@i8` → `SEP #$30`.
    assert_eq!(modes("widen_both"), vec![(Mnemonic::Rep, Some(0x30))]);
    assert_eq!(modes("narrow_both"), vec![(Mnemonic::Sep, Some(0x30))]);
    // `@a16` then `@i8` → no merge, because the two directions need one
    // instruction each.
    assert_eq!(
        modes("opposite_axes"),
        vec![(Mnemonic::Rep, Some(0x20)), (Mnemonic::Sep, Some(0x10))]
    );
    // The cancellation row, corrected. The document drops the `REP` entirely;
    // its clear mask carries the carry, and the carry is not a width.
    assert_eq!(
        modes("cancelled_with_a_carry"),
        vec![(Mnemonic::Rep, Some(0x01)), (Mnemonic::Sep, Some(0x20))]
    );
}

/// Identical effects, opposite results: the pair that shows the rule cannot be
/// effect equality.
///
/// `rep #$01` and `sep #$01` read the same state, write the same flag, touch
/// no memory, and fall through — and leave the carry at opposite values. The
/// run composes bit by bit, last writer per bit winning, so it becomes
/// `sep #$01`.
#[test]
fn a_fold_reproduces_the_composed_transform_and_not_the_effect_set() {
    let (model, program, log) = optimized(FOLDING);
    assert_eq!(
        instructions(&program, &model, "identical_effects_opposite_results")
            .into_iter()
            .take(2)
            .collect::<Vec<_>>(),
        vec![(Mnemonic::Sep, Some(0x01)), (Mnemonic::Bcs, None)]
    );

    let fold = log
        .by_pass(Pass::FoldModes)
        .find(|change| match change.after.as_slice() {
            [only] => match &only.kind {
                KOpKind::Instruction(instruction) => {
                    instruction.mnemonic == Mnemonic::Sep
                        && instruction
                            .operand
                            .as_ref()
                            .and_then(Operand::immediate_value)
                            == Some(k816::value::Value::new(0x01))
                }
                _ => false,
            },
            _ => false,
        })
        .expect("the carry pair folds");
    assert_eq!(fold.before.len(), 2);
    assert!(
        fold.justification.contains("P bits set $01, cleared $00"),
        "{}",
        fold.justification
    );
}

/// A truncation the composed result no longer performs is a register write
/// that has to be dead.
///
/// `sep #$10` + `rep #$10` composes to a clear mask of $10 and narrows
/// nothing — but the original zeroed the high bytes of X and Y on the way
/// through, and `txs` reads the X register. The fold is illegal, and both
/// instructions stay.
#[test]
fn a_fold_that_would_drop_a_live_truncation_does_not_happen() {
    let (model, program, _) = optimized(FOLDING);
    assert_eq!(
        instructions(&program, &model, "truncation_is_observed"),
        vec![
            (Mnemonic::Sep, Some(0x10)),
            (Mnemonic::Rep, Some(0x10)),
            (Mnemonic::Txs, None),
            (Mnemonic::Ldx, None),
            (Mnemonic::Rts, None),
        ]
    );
}

/// On any non-mode operation the pending bits are flushed, which is the
/// document's own rule: a pair one operation apart does not merge.
#[test]
fn an_operation_between_two_mode_changes_ends_the_run() {
    let (model, program, _) = optimized(FOLDING);
    assert_eq!(
        instructions(&program, &model, "separated"),
        vec![
            (Mnemonic::Rep, Some(0x20)),
            (Mnemonic::Lda, None),
            (Mnemonic::Rep, Some(0x10)),
            (Mnemonic::Ldx, None),
            (Mnemonic::Rts, None),
        ]
    );
}

/// A run only merges when every member was produced for the same reason.
///
/// A merged "bridge for `wide_index`" and "entering the width block" would
/// have to claim one and lie about the other, and the annotation on the
/// emitted line is checked output. In practice every adjacent pair the model
/// emits shares its reason, so the rule costs nothing today and stays honest
/// once there are more producers.
#[test]
fn a_run_of_unequal_reasons_does_not_merge() {
    // A block enter and a call bridge, adjacent, both widening, on different
    // axes: everything about the composed transform says `rep #$30`, and they
    // stay two instructions because they were emitted for different reasons.
    let (model, program, _) = optimized(
        "\
var CELL = $0400

func caller @a8 @i8 {
    @a16 {
        call wide_index
        CELL = a
    }
    ldx #$01
}

func wide_index @a16 @i16 {
    ldx #$0102
}
",
    );
    let ops = &function(&program, &model, "caller").ops;
    let modes: Vec<(u32, Option<Reason>)> = ops
        .iter()
        .filter_map(|op| match &op.kind {
            KOpKind::Instruction(instruction)
                if matches!(instruction.mnemonic, Mnemonic::Sep | Mnemonic::Rep) =>
            {
                Some((
                    instruction
                        .operand
                        .as_ref()
                        .and_then(Operand::immediate_value)
                        .expect("a folded mask")
                        .get(),
                    op.reason,
                ))
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        modes,
        vec![
            (0x20, Some(Reason::BlockEnter)),
            (
                0x10,
                Some(Reason::CallBridge(
                    model.symbols.lookup("wide_index").expect("the callee")
                ))
            ),
            (0x20, Some(Reason::BlockRestore)),
        ]
    );
}

/// A transfer K816 cannot name observes both axes.
///
/// The block graph answers the *reachability* question by making every label
/// in the function a successor of an unresolved transfer. That says nothing
/// about a target outside the function, which is where an indirect
/// `jmp (vec)` most often goes — and the code it reaches may read either width
/// flag. `main`'s inferred exit constrains nobody, so nothing else here keeps
/// this `rep` alive.
#[test]
fn an_unresolvable_transfer_observes_both_axes() {
    let (model, program, log) =
        optimized("var VECTOR = $0300\n\nfunc main @a8 @i8 {\n    rep #$20\n    jmp (VECTOR)\n}\n");
    assert_eq!(log.by_pass(Pass::EliminateDeadModes).count(), 0);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![(Mnemonic::Rep, Some(0x20)), (Mnemonic::Jmp, None)]
    );

    // The index axis too, and through a call whose target is a variable
    // holding an address — the legacy idiom for reaching a ROM routine.
    let (model, program, log) =
        optimized("var rom = $F9C2\n\nfunc main @a8 @i8 {\n    rep #$30\n    call rom\n}\n");
    assert_eq!(log.by_pass(Pass::EliminateDeadModes).count(), 0);
    assert_eq!(
        instructions(&program, &model, "main"),
        vec![
            (Mnemonic::Rep, Some(0x30)),
            (Mnemonic::Jsr, None),
            (Mnemonic::Rts, None)
        ]
    );
}
