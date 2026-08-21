/*****************************************************************************/
/*                                                                           */
/*                                optimizer.rs                               */
/*                                                                           */
/*         Dead-load removal justified by explicit effects, with dumps       */
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

//! The fixed pass sequence, one test per pass per rewrite.
//!
//! Each test asserts the operation stream *before*, the stream *after*, and
//! the `Change`'s recorded justification. Phase 9's first success criterion is
//! exactly this: a test that asserts only the after-stream does not satisfy
//! it, because it cannot tell a rewrite that was justified from one that
//! happened to produce the same bytes.

use k816::effects::{ControlEffect, FlagSet, MemoryEffect, RegisterSet};
use k816::isa::Mnemonic;
use k816::linkage::Linkage;
use k816::lower::lower;
use k816::op::{KFunction, KOpKind, KProgram, Operand, Origin};
use k816::optimize::{Context, OptimizationLog, Pass, optimize};
use k816::parser::parse_source;
use k816::semantic::{SemanticModel, analyze_file};
use k816::source::{FileId, SourceMap};
use k816::value::KExpr;

const HLA_SLICE: &str = include_str!("golden/slice.k816");

/// The unit's single lowered function. A `KProgram` is an ordered stream of
/// unit-level operations and functions, so tests reach in through the same
/// accessor the compiler uses rather than indexing a bare vector.
fn first_function(program: &KProgram) -> &KFunction {
    program.functions().next().expect("one lowered function")
}

fn lowered(text: &str) -> (SourceMap, FileId, SemanticModel, KProgram) {
    lowered_as("slice.k816", text)
}

fn lowered_as(name: &str, text: &str) -> (SourceMap, FileId, SemanticModel, KProgram) {
    let mut map = SourceMap::new();
    let id = map.add_file(name, text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![]);
    let (model, diagnostics) = analyze_file(&parse.file);
    // Errors only: several fixtures earn a width or damage *warning* on
    // purpose, and the warning is usually the claim the fixture is making.
    assert_eq!(errors(&diagnostics), Vec::<String>::new());
    let (program, diagnostics) = lower(&parse.file, &model);
    assert_eq!(errors(&diagnostics), Vec::<String>::new());
    (map, id, model, program)
}

fn errors(diagnostics: &[k816::diagnostic::Diagnostic]) -> Vec<String> {
    diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.is_error())
        .map(|diagnostic| diagnostic.code.as_str().to_owned())
        .collect()
}

/// The mnemonics of one named function's stream, for a test that cares which
/// instructions survived and in what order.
fn mnemonics(program: &KProgram, model: &SemanticModel, name: &str) -> Vec<Mnemonic> {
    program
        .functions()
        .find(|function| model.symbols.name(function.symbol) == name)
        .unwrap_or_else(|| panic!("no function named `{name}`"))
        .ops
        .iter()
        .filter_map(|op| match &op.kind {
            KOpKind::Instruction(instruction) => Some(instruction.mnemonic),
            _ => None,
        })
        .collect()
}

/// Runs the fixed pass sequence over one unit analyzed on its own.
///
/// The empty [`Linkage`] is the honest description of a file compiled with no
/// `--unit` siblings: nothing else defines a name, and nothing else refers to
/// one of this unit's.
fn run(program: &mut KProgram, model: &SemanticModel, file: FileId) -> OptimizationLog {
    let linkage = Linkage::default();
    optimize(
        program,
        &Context {
            model,
            linkage: &linkage,
            file,
        },
    )
}

#[test]
fn ir_dump_shows_the_dead_load_before_and_after_optimization() {
    let (map, file, model, mut program) = lowered(HLA_SLICE);

    assert_eq!(
        program.dump(&model.symbols, &map),
        "\
func main @a8 -> a:preserve i:preserve {
  label main               ; user slice.k816:3
  mode-assume @a8 @i8      ; compiler slice.k816:3 — the function's entry contract, assumed
  lda #$00                 ; hla slice.k816:4
  lda #$01                 ; hla slice.k816:5
  sta abs SCREEN           ; hla slice.k816:6
  rts                      ; hla slice.k816:7
}
"
    );

    run(&mut program, &model, file);

    assert_eq!(
        program.dump(&model.symbols, &map),
        "\
func main @a8 -> a:preserve i:preserve {
  label main               ; user slice.k816:3
  mode-assume @a8 @i8      ; compiler slice.k816:3 — the function's entry contract, assumed
  lda #$01                 ; hla slice.k816:5
  sta abs SCREEN           ; hla slice.k816:6
  rts                      ; hla slice.k816:7
}
"
    );
}

#[test]
fn the_removal_is_justified_by_explicit_effects() {
    let (_, file, model, mut program) = lowered(HLA_SLICE);
    let log = run(&mut program, &model, file);

    assert_eq!(log.changes.len(), 1);
    let change = &log.changes[0];
    assert_eq!(change.pass, Pass::RemoveOverwritten);
    assert_eq!(change.site.function, first_function(&program).symbol);

    // What was removed: the dead `a = #0` load, and nothing replaced it.
    assert_eq!(change.before.len(), 1);
    assert!(change.after.is_empty());
    let KOpKind::Instruction(removed) = &change.before[0].kind else {
        panic!("expected an instruction");
    };
    assert_eq!(removed.mnemonic, Mnemonic::Lda);
    assert_eq!(removed.operand, Some(Operand::Immediate(KExpr::number(0))));
    assert_eq!(change.before[0].origin, Origin::HlaExpansion);

    // The justification: everything it writes (A, N, Z) is overwritten by
    // `lda #$01` before any read, and it has no observable memory or
    // control effect of its own.
    let effects = change.effects();
    assert_eq!(effects.len(), 1);
    assert!(effects[0].writes.registers.contains(RegisterSet::A));
    assert!(
        effects[0]
            .writes
            .flags
            .contains(FlagSet::N.union(FlagSet::Z))
    );
    assert_eq!(effects[0].memory, MemoryEffect::None);
    assert_eq!(effects[0].control, ControlEffect::FallThrough);
    assert_eq!(
        change.justification,
        "writes A N Z, all overwritten by `lda #$01` before any read"
    );

    let witness = change.witness.as_ref().expect("an overwriter");
    let KOpKind::Instruction(overwriter) = &witness.kind else {
        panic!("expected an instruction");
    };
    assert_eq!(overwriter.mnemonic, Mnemonic::Lda);
    assert_eq!(
        overwriter.operand,
        Some(Operand::Immediate(KExpr::number(1)))
    );
}

/// The other half of the `effects.rs` correction, asserted directly.
///
/// Narrowing `SEP`/`REP`'s read set is what makes dead mode elimination
/// possible and is *also* what would let the ordinary dead-write pass delete a
/// mode instruction: a `sep #$20` has no memory effect, falls through, and
/// writes M, and the second one below overwrites that write without reading
/// it. Nothing in that pass's candidate test rejects it, and it would do the
/// deletion with none of the guards a mode change needs — an exit contract, a
/// fixed label bridge, a live truncation, a non-width bit in the mask.
///
/// So the redundant `sep` does go, and it goes through **pass 1**, which
/// checked all of them.
#[test]
fn only_the_two_mode_passes_touch_a_mode_instruction() {
    let (_, file, model, mut program) = lowered(
        "\
func f @a16 {
    sep #$20
    sep #$20
    lda #$01
}
",
    );
    let log = run(&mut program, &model, file);
    assert_eq!(log.changes.len(), 1);
    assert_eq!(log.changes[0].pass, Pass::EliminateDeadModes);
    assert_eq!(
        mnemonics(&program, &model, "f"),
        vec![Mnemonic::Sep, Mnemonic::Lda, Mnemonic::Rts]
    );

    // And the general claim, over every shipped fixture that has mode
    // operations in it: whatever changed, a mode instruction only ever
    // changed under pass 1 or pass 2.
    for (name, source) in MODE_FIXTURES {
        let (_, file, model, mut program) = lowered_as(name, source);
        for change in run(&mut program, &model, file).changes {
            let touched_a_mode =
                change
                    .before
                    .iter()
                    .chain(&change.after)
                    .any(|op| match &op.kind {
                        KOpKind::Instruction(instruction) => {
                            matches!(instruction.mnemonic, Mnemonic::Sep | Mnemonic::Rep)
                        }
                        _ => false,
                    });
            assert!(
                !touched_a_mode
                    || matches!(change.pass, Pass::EliminateDeadModes | Pass::FoldModes),
                "`{name}`: {:?} touched a mode instruction",
                change.pass
            );
        }
    }
}

/// Every shipped fixture that contains a `REP` or a `SEP`.
const MODE_FIXTURES: &[(&str, &str)] = &[
    ("block-exit.k816", include_str!("golden/block-exit.k816")),
    (
        "data-entries.k816",
        include_str!("golden/data-entries.k816"),
    ),
    ("hla-alu.k816", include_str!("golden/hla-alu.k816")),
    (
        "native-modes.k816",
        include_str!("golden/native-modes.k816"),
    ),
    (
        "opt-barriers.k816",
        include_str!("golden/opt-barriers.k816"),
    ),
    (
        "width-blocks.k816",
        include_str!("golden/width-blocks.k816"),
    ),
    (
        "width-branch.k816",
        include_str!("golden/width-branch.k816"),
    ),
    (
        "width-bridging.k816",
        include_str!("golden/width-bridging.k816"),
    ),
    ("width-exit.k816", include_str!("golden/width-exit.k816")),
    ("width-loop.k816", include_str!("golden/width-loop.k816")),
];

#[test]
fn reads_and_barriers_keep_live_operations() {
    let (_, file, model, mut program) = lowered(HLA_SLICE);
    run(&mut program, &model, file);
    let ops = &first_function(&program).ops;

    let mnemonics: Vec<Mnemonic> = ops
        .iter()
        .filter_map(|op| match &op.kind {
            KOpKind::Instruction(instruction) => Some(instruction.mnemonic),
            _ => None,
        })
        .collect();

    // `lda #$01` survives: `sta` reads A. `rep #$10` survives: `rts` is a
    // control barrier before the X flag could be proven dead. `sta`/`rts` are
    // observable (memory write, stack + return) and are never candidates. The
    // 8-bit half of the contract is a width claim rather than an instruction,
    // so there is no `sep` here to keep or remove.
    assert_eq!(mnemonics, vec![Mnemonic::Lda, Mnemonic::Sta, Mnemonic::Rts]);
}

#[test]
fn native_and_hla_forms_optimize_identically() {
    let (_, hla_file, hla_model, mut hla) = lowered(HLA_SLICE);
    let (_, native_file, native_model, mut native) =
        lowered(include_str!("golden/slice-native.k816"));

    let hla_log = run(&mut hla, &hla_model, hla_file);
    let native_log = run(&mut native, &native_model, native_file);

    assert_eq!(hla_log.changes.len(), 1);
    assert_eq!(native_log.changes.len(), 1);
    // Identical treatment regardless of source notation…
    assert_eq!(
        hla_log.changes[0].before[0].kind,
        native_log.changes[0].before[0].kind
    );
    assert_eq!(hla_log.changes[0].pass, native_log.changes[0].pass);
    assert_eq!(
        hla_log.changes[0].justification,
        native_log.changes[0].justification
    );
    // …while the origins still tell the two notations apart.
    assert_eq!(hla_log.changes[0].before[0].origin, Origin::HlaExpansion);
    assert_eq!(native_log.changes[0].before[0].origin, Origin::User);

    let kinds = |program: &KProgram| -> Vec<KOpKind> {
        first_function(program)
            .ops
            .iter()
            .map(|op| op.kind.clone())
            .collect()
    };
    assert_eq!(kinds(&hla), kinds(&native));
}

// ---------------------------------------------------------------------------
// The block view and its liveness (sub-plan F, task F1)
// ---------------------------------------------------------------------------

const BARRIERS: &str = include_str!("golden/opt-barriers.k816");
const LIVEOUT: &str = include_str!("golden/opt-liveout.k816");

/// The strongest form the barrier claim has.
///
/// Every other fixture asserts what changed; this one asserts that nothing
/// did, and it asserts it as an **empty log** rather than as an expected
/// output. An expected output passes when a pass removes an operation and
/// another puts an equivalent one back; an empty log cannot.
#[test]
fn nothing_in_the_barrier_fixture_changes() {
    let (_, file, model, mut program) = lowered_as("opt-barriers.k816", BARRIERS);
    let before = program.clone();
    let log = run(&mut program, &model, file);
    assert!(
        log.is_empty(),
        "{:?}",
        log.changes
            .iter()
            .map(|change| (change.pass, change.justification.clone()))
            .collect::<Vec<_>>()
    );
    assert_eq!(program, before, "the stream itself is untouched");
}

/// A label is not a barrier once there is a successor relation.
///
/// The straight-line scan bailed at the first non-instruction operation and
/// called it conservatism; it was really the absence of a successor relation.
/// No path *from* `lda #$00` reads the accumulator before `lda #$01`
/// overwrites it — arriving at `spot` from elsewhere means the write never ran
/// — so the write is dead with the label there and without it.
#[test]
fn a_label_between_a_dead_write_and_its_overwriter_is_not_a_barrier() {
    let with_label = "\
var SCREEN = $0400

func main @a8 @i8 {
    lda #$00
spot:
    lda #$01
    SCREEN = a
}
";
    let (_, file, model, mut program) = lowered(with_label);
    let log = run(&mut program, &model, file);
    assert_eq!(log.changes.len(), 1);
    assert_eq!(log.changes[0].pass, Pass::RemoveOverwritten);
    assert_eq!(
        mnemonics(&program, &model, "main"),
        vec![Mnemonic::Lda, Mnemonic::Sta, Mnemonic::Rts]
    );
}

/// A call returns, so everything after the first `jsr` is still reachable.
///
/// `ControlEffect::Call` is not `FallThrough`, so a splitter written from the
/// leader rule alone ends a block at every call and gives it no successor —
/// which makes the whole rest of the function look unreachable, in a program
/// that still assembles. This asserts the edge directly, before the pass that
/// would delete what a missing one hides.
#[test]
fn a_call_in_the_middle_of_a_function_does_not_end_the_graph() {
    let source = "\
var CELL = $0400

func leaf @a8 @i8 {
    nop
}

func caller @a8 @i8 {
    lda #$01
    call leaf
    lda #$02
    CELL = a
}
";
    let (_, file, model, mut program) = lowered(source);
    run(&mut program, &model, file);
    // Nothing after the call was dropped, and `lda #$01` survived too: a call
    // is a clobber and a dataflow barrier, so A is live before it.
    assert_eq!(
        mnemonics(&program, &model, "caller"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Jsr,
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );
}

/// Nothing is dead at a function exit, and a mid-function load with the same
/// shape still goes.
#[test]
fn everything_is_live_at_every_exit() {
    let (_, file, model, mut program) = lowered_as("opt-liveout.k816", LIVEOUT);
    let log = run(&mut program, &model, file);

    // The declared output survives, and so does the *undeclared* `ldy`: a
    // caller reached through `call` may read Y, and the declaration does not
    // forbid it.
    assert_eq!(
        mnemonics(&program, &model, "produce"),
        vec![Mnemonic::Adc, Mnemonic::Ldy, Mnemonic::Rts]
    );
    // The trailing `clc` survives: no contract syntax could have declared the
    // carry an output, so the carry is live at every exit.
    assert_eq!(
        mnemonics(&program, &model, "carry"),
        vec![Mnemonic::Lda, Mnemonic::Sta, Mnemonic::Clc, Mnemonic::Rts]
    );
    // Both call spellings lower to the same `jsr` and are treated the same.
    assert_eq!(
        mnemonics(&program, &model, "caller"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Jsr,
            Mnemonic::Sta,
            Mnemonic::Sta,
            Mnemonic::Lda,
            Mnemonic::Jsr,
            Mnemonic::Sta,
            Mnemonic::Sta,
            Mnemonic::Rts,
        ]
    );

    // The one change in the whole file: the mid-function load.
    assert_eq!(log.changes.len(), 1);
    let change = &log.changes[0];
    assert_eq!(model.symbols.name(change.site.function), "middle");
    assert_eq!(
        change.justification,
        "writes A N Z, all overwritten by `lda #$08` before any read"
    );
    assert_eq!(
        mnemonics(&program, &model, "middle"),
        vec![Mnemonic::Lda, Mnemonic::Sta, Mnemonic::Rts]
    );
}

/// A branch that leaves immediately makes the write above it dead.
///
/// `width-branch.k816::skipped` loads the accumulator, branches away, and the
/// target reloads it. No path from the first load reads A before the second
/// overwrites it, so the first is dead — which the straight-line scan could
/// not see, because the branch was a barrier and the label after it was
/// another one.
#[test]
fn a_write_a_branch_leaves_behind_is_dead() {
    let (_, file, model, mut program) = lowered_as(
        "width-branch.k816",
        include_str!("golden/width-branch.k816"),
    );
    let log = run(&mut program, &model, file);
    let dead_writes: Vec<_> = log.by_pass(Pass::RemoveOverwritten).collect();
    assert_eq!(dead_writes.len(), 1);
    assert_eq!(model.symbols.name(dead_writes[0].site.function), "skipped");
    assert_eq!(
        mnemonics(&program, &model, "skipped"),
        vec![Mnemonic::Bra, Mnemonic::Rep, Mnemonic::Lda, Mnemonic::Rts],
        "the branch, the unreachable user `rep`, and the load at the label all stay"
    );
}

/// A second `optimize()` over an already-optimized program changes nothing.
///
/// Decision 18: the eight passes run once, in order, and each closes what it
/// opens. If a second run finds work, the pass that created the opportunity is
/// at fault and gets fixed there — the assertion is not a reason to add a loop
/// over the sequence.
#[test]
fn optimization_is_idempotent() {
    for (name, source) in [
        ("slice.k816", HLA_SLICE),
        ("opt-barriers.k816", BARRIERS),
        ("opt-liveout.k816", LIVEOUT),
        (
            "width-branch.k816",
            include_str!("golden/width-branch.k816"),
        ),
        (
            "width-blocks.k816",
            include_str!("golden/width-blocks.k816"),
        ),
        ("hla-alu.k816", include_str!("golden/hla-alu.k816")),
        ("subscripts.k816", include_str!("golden/subscripts.k816")),
    ] {
        let (_, file, model, mut program) = lowered_as(name, source);
        run(&mut program, &model, file);
        let second = run(&mut program, &model, file);
        assert!(second.is_empty(), "`{name}`: {:?}", second.changes);
    }
}

// ---------------------------------------------------------------------------
// Known values and known flags (sub-plan F, task F4)
// ---------------------------------------------------------------------------

const VALUES: &str = include_str!("golden/opt-values.k816");

/// A load of a value the register already holds goes; one whose flags would
/// differ stays.
#[test]
fn a_load_of_a_value_the_register_already_holds_is_coalesced() {
    let (_, file, model, mut program) = lowered_as("opt-values.k816", VALUES);
    let log = run(&mut program, &model, file);

    assert_eq!(
        mnemonics(&program, &model, "repeated_load"),
        vec![Mnemonic::Lda, Mnemonic::Sta, Mnemonic::Sta, Mnemonic::Rts]
    );
    let coalesced: Vec<_> = log.by_pass(Pass::CoalesceLoads).collect();
    assert_eq!(coalesced.len(), 2, "the load and the `ldx` that agrees");
    assert_eq!(
        coalesced[0].justification,
        "the register already holds $07, in every lane this load writes"
    );

    // A `cmp` between them rewrites N and Z from a comparison, so the second
    // load is no longer a complete no-op.
    assert_eq!(
        mnemonics(&program, &model, "compare_between"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Cmp,
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );
}

/// The value domain tracks lanes, and the tracker refuses a transfer whose
/// source does not cover what it moves.
///
/// `lda #0` at `@a8` establishes the low byte and says nothing about the
/// hidden `B` byte. `tax` at `@i16` moves all sixteen, so X is **not** known
/// to be zero and the `ldx #$0000` after it stays. With the widths agreeing,
/// the same shape coalesces.
#[test]
fn a_transfer_propagates_only_when_the_source_covers_what_it_moves() {
    let (_, file, model, mut program) = lowered_as("opt-values.k816", VALUES);
    run(&mut program, &model, file);

    // The stores became `stz`: the accumulator is known zero, which is the
    // zero-store peephole doing its own job on the way past.
    assert_eq!(
        mnemonics(&program, &model, "lanes"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Tax,
            Mnemonic::Stz,
            Mnemonic::Ldx, // stays: A was known in the low byte only
            Mnemonic::Stz,
            Mnemonic::Rts
        ]
    );
    assert_eq!(
        mnemonics(&program, &model, "lanes_agree"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Tax,
            Mnemonic::Stz,
            // the `ldx #$0000` is gone: sixteen known bits moved into sixteen
            Mnemonic::Stz,
            Mnemonic::Rts
        ]
    );
}

/// Value tracking never reads memory, so two reads of a hardware register are
/// two reads.
///
/// K65 has no `volatile` and no way to acquire one — a fixed-address `var` is
/// how the language spells a memory-mapped register, and
/// `examples/k65/atari-xl/landscape/main.k65` reads the POKEY random-number
/// register in consecutive statements.
#[test]
fn two_reads_of_a_fixed_address_var_are_both_kept() {
    let (_, file, model, mut program) = lowered_as("opt-values.k816", VALUES);
    run(&mut program, &model, file);
    assert_eq!(
        mnemonics(&program, &model, "hardware_register"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );
}

/// A flag operation whose flag already holds that value goes; one whose flag
/// was rewritten in between stays.
#[test]
fn a_flag_operation_that_would_change_nothing_is_collapsed() {
    let (_, file, model, mut program) = lowered_as("opt-values.k816", VALUES);
    let log = run(&mut program, &model, file);

    assert_eq!(
        mnemonics(&program, &model, "repeated_clear"),
        vec![
            Mnemonic::Rep, // the mask that cleared the carry, untouched
            Mnemonic::Adc,
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );
    let collapsed: Vec<_> = log.by_pass(Pass::CollapseFlags).collect();
    assert_eq!(collapsed.len(), 1);
    assert_eq!(
        collapsed[0].justification,
        "C already holds 0, so this changes nothing"
    );

    // `adc` writes the carry, so the second `clc` is establishing something
    // the first no longer guarantees.
    assert_eq!(
        mnemonics(&program, &model, "carry_is_rewritten"),
        vec![
            Mnemonic::Clc,
            Mnemonic::Adc,
            Mnemonic::Clc,
            Mnemonic::Adc,
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );
}

// ---------------------------------------------------------------------------
// Zero stores (sub-plan F, task F5)
// ---------------------------------------------------------------------------

const ZERO_STORE: &str = include_str!("golden/opt-zero-store.k816");

/// Each supported `STA` becomes `STZ`, and the leading `LDA #0` goes only
/// where A and the N and Z it wrote are dead.
#[test]
fn a_zero_chain_becomes_stz_per_store_and_drops_its_load_separately() {
    let (_, file, model, mut program) = lowered_as("opt-zero-store.k816", ZERO_STORE);
    let log = run(&mut program, &model, file);

    // Four modes `STZ` has: every store is rewritten, and the load goes.
    assert_eq!(
        mnemonics(&program, &model, "every_stz_mode"),
        vec![
            Mnemonic::Stz,
            Mnemonic::Stz,
            Mnemonic::Stz,
            Mnemonic::Stz,
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );

    // Two modes it lacks: nothing moves, because the stores still read A.
    assert_eq!(
        mnemonics(&program, &model, "modes_stz_lacks"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Sta,
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );

    // The same chain at a function's end: the stores are still rewritten and
    // the load **stays**. Everything is live at an exit, the accumulator and
    // the N and Z it wrote included, which is why the rewrite completes less
    // often here than `doc/syntax-reference.md`'s phrasing suggests.
    assert_eq!(
        mnemonics(&program, &model, "at_the_end"),
        vec![Mnemonic::Lda, Mnemonic::Stz, Mnemonic::Stz, Mnemonic::Rts]
    );

    let zero_stores: Vec<_> = log.by_pass(Pass::ZeroStores).collect();
    // Six rewrites and one removal.
    assert_eq!(zero_stores.len(), 7);
    assert_eq!(
        zero_stores[0].justification,
        "the accumulator is known zero in every lane this store writes, and `STZ` has this \
         mode: same length, same stored bytes, and it stops reading A"
    );
    let dropped = zero_stores
        .iter()
        .find(|change| change.after.is_empty())
        .expect("the load the rewrite orphaned");
    assert_eq!(
        dropped.justification,
        "nothing reads the accumulator or the N and Z it wrote, now that every store it fed \
         is an `STZ`"
    );
}

// ---------------------------------------------------------------------------
// Unreachable compiler-generated operations (sub-plan F, task F6)
// ---------------------------------------------------------------------------

/// The pass removes what the *compiler* put in an unreachable block, and
/// nothing the user wrote there.
///
/// `width-branch.k816::skipped` has a user-written `rep #$20` that no path
/// reaches, and it must survive F — the master plan's own wording restricts
/// this pass to compiler-generated operations, and `doc/design.md` §8's
/// identical-treatment rule is about how a pass *reasons*, not about who gets
/// to keep unreachable code.
#[test]
fn only_compiler_generated_operations_are_removed_as_unreachable() {
    let (_, file, model, mut program) = lowered_as(
        "width-branch.k816",
        include_str!("golden/width-branch.k816"),
    );
    let log = run(&mut program, &model, file);
    assert_eq!(log.by_pass(Pass::Unreachable).count(), 0);
    assert!(
        mnemonics(&program, &model, "skipped").contains(&Mnemonic::Rep),
        "the user's unreachable `rep` stays where it was written"
    );

    // A function that loops forever: the implicit return the compiler put
    // after it is unreachable, and it goes.
    let (_, file, model, mut program) =
        lowered("func spins @a8 @i8 {\n.head:\n    nop\n    bra .head\n}\n");
    let log = run(&mut program, &model, file);
    let unreachable: Vec<_> = log.by_pass(Pass::Unreachable).collect();
    assert_eq!(unreachable.len(), 1);
    assert_eq!(
        unreachable[0].justification,
        "no edge reaches this block, and the compiler generated the operation"
    );
    assert_eq!(
        mnemonics(&program, &model, "spins"),
        vec![Mnemonic::Nop, Mnemonic::Bra]
    );
}

/// A call in the middle of a function keeps everything after it reachable.
///
/// `ControlEffect::Call` is not `FallThrough`, so a block graph built from the
/// leader rule alone gives a `jsr` no successor and makes the whole rest of
/// the function look unreachable — which this pass would then delete, quietly,
/// in a program that still assembles.
#[test]
fn nothing_after_a_call_is_treated_as_unreachable() {
    let source = "\
var CELL = $0400

func leaf @a8 @i8 {
    nop
}

func caller @a8 @i8 {
    call leaf
    lda #$02
    CELL = a
    call leaf
    lda #$03
    CELL = a
}
";
    let (_, file, model, mut program) = lowered(source);
    let log = run(&mut program, &model, file);
    assert_eq!(log.by_pass(Pass::Unreachable).count(), 0);
    assert_eq!(
        mnemonics(&program, &model, "caller"),
        vec![
            Mnemonic::Jsr,
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Jsr,
            Mnemonic::Lda,
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );
}
