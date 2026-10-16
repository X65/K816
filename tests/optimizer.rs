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
use k816::op::{KFunction, KOpKind, KProgram, Operand, Origin, Reason};
use k816::optimize::{Change, Context, OptimizationLog, Pass, optimize};
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
    lowered_in(map, id)
}

/// The same, for a golden that `import`s: read from `tests/golden/` so the
/// paths resolve, with the closure loaded and the workspace resolved.
///
/// The fixture still describes its own program — that is what makes it
/// self-contained — but the description is a line in the source rather than
/// the absence of one, so the harness has to read it.
fn lowered_from_golden(name: &str) -> (SourceMap, FileId, SemanticModel, KProgram) {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/golden")
        .join(name);
    let mut map = SourceMap::new();
    let id = map.add_file_at(
        path.to_string_lossy(),
        &path,
        std::fs::read_to_string(&path).expect("read the golden"),
    );
    k816::load_imports(&mut map, &mut |path| std::fs::read_to_string(path));
    lowered_in(map, id)
}

fn lowered_in(map: SourceMap, id: FileId) -> (SourceMap, FileId, SemanticModel, KProgram) {
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![]);
    let (model, diagnostics) = if map.files().count() == 1 {
        analyze_file(&parse.file)
    } else {
        let linkage = k816::linkage::Linkage::resolve(&map, k816::abi::EntryEnvironment::default());
        assert_eq!(linkage.diagnostics(), &[]);
        k816::semantic::analyze_unit(&parse.file, map.file(id), &linkage)
    };
    // Errors only: several fixtures earn a width or damage *warning* on
    // purpose, and the warning is usually the claim the fixture is making.
    assert_eq!(errors(&diagnostics), Vec::<String>::new());
    let (program, diagnostics) = lower(model.program(&parse.file), &model);
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

    // And the general claim, over every fixture that compiles on its own:
    // whatever changed, a mode instruction only ever changed under pass 1 or
    // pass 2.
    for name in every_self_contained_fixture() {
        let (_, file, model, mut program) = lowered_from_golden(&name);
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

/// The same claim, once per pass.
///
/// Phase 9 requires that native and HLA-generated operations receive
/// identical treatment, and one shared case does not show it: a pass that
/// keyed on `Origin` would still agree about the slice's dead load. Each row
/// below is one program written twice — the shorthand and the native sequence
/// it expands to — and the two must optimize to the same operation stream and
/// under the same pass.
#[test]
fn every_pass_treats_the_two_notations_identically() {
    // `(what it exercises, HLA body, native body)`. The declarations and the
    // contract are shared, so only the notation differs.
    const CASES: &[(Pass, &str, &str)] = &[
        (
            Pass::RemoveOverwritten,
            "a = #$00\n    a = #$01\n    CELL = a",
            "lda #$00\n    lda #$01\n    sta CELL",
        ),
        (
            Pass::CoalesceLoads,
            "a = #$07\n    CELL = a\n    a = #$07\n    CELL = a",
            "lda #$07\n    sta CELL\n    lda #$07\n    sta CELL",
        ),
        (
            Pass::CollapseFlags,
            "rep #$01\n    c-\n    a+#$01\n    CELL = a",
            "rep #$01\n    clc\n    adc #$01\n    sta CELL",
        ),
        (
            Pass::ZeroStores,
            "CELL = a = 0\n    a = #$01\n    CELL = a",
            "lda #$00\n    sta CELL\n    lda #$01\n    sta CELL",
        ),
    ];

    for (pass, hla_body, native_body) in CASES {
        let source =
            |body: &str| format!("var CELL = $0400\n\nfunc f @a8 @i8 {{\n    {body}\n}}\n");
        let (_, hla_file, hla_model, mut hla) = lowered(&source(hla_body));
        let (_, native_file, native_model, mut native) = lowered(&source(native_body));
        let hla_log = run(&mut hla, &hla_model, hla_file);
        let native_log = run(&mut native, &native_model, native_file);

        assert!(
            hla_log.by_pass(*pass).count() > 0,
            "{pass:?} did not fire on the shorthand form"
        );
        assert_eq!(
            hla_log.by_pass(*pass).count(),
            native_log.by_pass(*pass).count(),
            "{pass:?} fired a different number of times per notation"
        );
        assert_eq!(
            mnemonics(&hla, &hla_model, "f"),
            mnemonics(&native, &native_model, "f"),
            "{pass:?} left different streams per notation"
        );
        assert_eq!(
            hla_log
                .changes
                .iter()
                .map(|change| change.justification.clone())
                .collect::<Vec<_>>(),
            native_log
                .changes
                .iter()
                .map(|change| change.justification.clone())
                .collect::<Vec<_>>(),
            "{pass:?} justified itself differently per notation"
        );
    }
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
    // The trailing `clc` survives. A clause may name the carry since sub-plan
    // G, and `carry` does not, so the seed keeps it: everything is live at
    // every exit whatever a clause says.
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
    for name in every_self_contained_fixture() {
        let (_, file, model, mut program) = lowered_from_golden(&name);
        run(&mut program, &model, file);
        let second = run(&mut program, &model, file);
        assert!(second.is_empty(), "`{name}`: {:?}", second.changes);
    }
}

/// Every `tests/golden/*.k816` that compiles on its own.
///
/// Read from the directory rather than listed, so a fixture added later is
/// covered without anyone remembering to add it — which is the difference
/// between "idempotent on the cases we thought of" and the claim decision 18
/// makes. The skipped ones each name why: a unit of a pair has no meaning
/// alone, and a fixture that pins a *refusal* has no operations to optimize.
fn every_self_contained_fixture() -> Vec<String> {
    /// `(fixture, why it cannot be compiled alone)`.
    const SKIPPED: &[(&str, &str)] = &[
        ("contract-unit-b.k816", "one unit of a cross-unit pair"),
        ("layout-unit-b.k816", "one unit of a cross-unit pair"),
        ("opt-crossunit-a.k816", "one unit of a cross-unit pair"),
        ("opt-crossunit-b.k816", "one unit of a cross-unit pair"),
        ("unit-a.k816", "one unit of a cross-unit pair"),
        ("nocross-oversized.k816", "pins a refusal: K816-E0112"),
        (
            "width-emulation.k816",
            "pins a refusal under unknown emulation",
        ),
    ];
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let mut fixtures: Vec<String> = std::fs::read_dir(&dir)
        .expect("read tests/golden")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().to_string_lossy().into_owned();
            if !(name.ends_with(".k816") || name.ends_with(".kk"))
                || SKIPPED.iter().any(|(skip, _)| *skip == name)
            {
                return None;
            }
            Some(name)
        })
        .collect();
    fixtures.sort();
    // The skip list cannot outlive the files it names.
    for (name, why) in SKIPPED {
        assert!(dir.join(name).exists(), "{name} is skipped but is gone");
        assert!(!why.is_empty(), "{name} says nothing");
    }
    assert!(fixtures.len() > 20, "the directory scan found nothing");
    fixtures
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

/// The value domain tracks lanes, and the tracker refuses a move whose
/// source does not cover what it moves.
///
/// `lda #$00` at `@a8` establishes the low byte and says nothing about the
/// hidden `B` byte. `xba` exchanges the two whatever `M` says, so what the
/// accumulator holds afterwards is a byte nothing wrote: the store after it
/// stays a `sta`, and the `lda #$00` after *that* is not a no-op. Where
/// sixteen known bits move into sixteen-bit index registers, the same shape
/// coalesces.
#[test]
fn a_transfer_propagates_only_when_the_source_covers_what_it_moves() {
    let (_, file, model, mut program) = lowered_as("opt-values.k816", VALUES);
    run(&mut program, &model, file);

    // Only the second store became `stz`. The first one is a store of the
    // hidden half, and a domain without lanes would call it a store of zero.
    assert_eq!(
        mnemonics(&program, &model, "lanes"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Xba,
            Mnemonic::Sta, // stays: A holds the half no instruction wrote
            Mnemonic::Lda, // stays: and so this load is not redundant
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

const ZERO_STORE_INDEX: &str = include_str!("golden/opt-zero-store-index.k816");

/// `STX` and `STY` of a known zero become `STZ` where the accumulator is as
/// wide as the index registers, and their `LDX #0`/`LDY #0` goes where it is
/// dead.
///
/// The width condition is the one an `STA` never needs: `STZ` stores at the
/// accumulator's width, so at `@a16 @i8` an `stx` of a known zero is one byte
/// and the `stz` it would become is two.
#[test]
fn an_index_zero_store_becomes_stz_where_the_widths_agree() {
    let (_, file, model, mut program) = lowered_as("opt-zero-store-index.k816", ZERO_STORE_INDEX);
    let log = run(&mut program, &model, file);
    use Mnemonic::{Ldx, Ldy, Rep, Rts, Stx, Sty, Stz};

    // The `ldx #0` stays because `stz zp,x` still indexes by X.
    assert_eq!(
        mnemonics(&program, &model, "both8"),
        vec![Ldx, Stz, Stz, Stz, Stz, Ldx, Ldy, Stx, Sty, Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "both16"),
        vec![Stz, Ldx, Stx, Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "a16_i8"),
        vec![Ldx, Stx, Ldx, Stx, Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "a8_i16"),
        vec![Ldy, Sty, Ldy, Sty, Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "indexed_y"),
        vec![Ldx, Stx, Ldx, Stx, Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "chains"),
        vec![Stz, Stz, Ldx, Ldy, Stx, Sty, Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "at_the_end"),
        vec![Ldx, Stz, Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "removed_switch"),
        vec![Ldx, Stx, Rep, Rts]
    );

    let zero_stores: Vec<_> = log.by_pass(Pass::ZeroStores).collect();
    // Eight rewrites — four in `both8`, one in `both16`, two in `chains`, one
    // at the end — and four removals: `both8`'s `ldy`, `both16`'s `ldx`, and
    // the two loads of `chains`.
    let (rewrites, removals): (Vec<&Change>, Vec<&Change>) = zero_stores
        .iter()
        .copied()
        .partition(|change| !change.after.is_empty());
    assert_eq!((rewrites.len(), removals.len()), (8, 4));
    assert!(rewrites.iter().any(|change| change.justification
        == "X is known zero in every lane this store writes, the accumulator is as wide as X, \
            and `STZ` has this mode: same length, same stored bytes, and it stops reading X"));
    assert!(rewrites.iter().any(|change| change.justification
        == "Y is known zero in every lane this store writes, the accumulator is as wide as Y, \
            and `STZ` has this mode: same length, same stored bytes, and it stops reading Y"));
    assert!(removals.iter().any(|change| change.justification
        == "nothing reads X or the N and Z it wrote, now that every store it fed is an `STZ`"));
    assert!(removals.iter().any(|change| change.justification
        == "nothing reads Y or the N and Z it wrote, now that every store it fed is an `STZ`"));
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

// ---------------------------------------------------------------------------
// The two legacy peephole rules (sub-plan G, task G8)
// ---------------------------------------------------------------------------

const TAIL_LABELS: &str = include_str!("golden/opt-tail-labels.k816");

/// Which functions in `opt-tail-labels.k816` a pass changed.
fn changed_by(log: &OptimizationLog, model: &SemanticModel, pass: Pass) -> Vec<String> {
    let mut names: Vec<String> = log
        .by_pass(pass)
        .map(|change| model.symbols.name(change.site.function).to_owned())
        .collect();
    names.sort();
    names
}

/// Pass 9 removes a branch to the next label, and only where falling through
/// reaches the same place.
///
/// Three cases, one file. `branch_to_next` has nothing at all between the
/// branch and its target. `branch_over_work` has an `inx`, which is the
/// difference between taking the branch and not, and it is the shape the
/// legacy peephole's `next_label_matches` refuses the same way. And
/// `native_branch_to_next` is the origin rule: the branch is a no-op there
/// too, and it stays, because the author typed it.
#[test]
fn a_branch_to_the_next_label_goes_and_a_branch_over_work_stays() {
    let (_, file, model, mut program) = lowered_as("opt-tail-labels.k816", TAIL_LABELS);
    let log = run(&mut program, &model, file);
    assert_eq!(
        changed_by(&log, &model, Pass::RemoveBranchToNextLabel),
        vec!["branch_to_next"]
    );
    assert_eq!(
        log.by_pass(Pass::RemoveBranchToNextLabel)
            .next()
            .expect("one change")
            .justification,
        "the target is the next label, which control reaches by falling through"
    );
    assert_eq!(
        mnemonics(&program, &model, "branch_to_next"),
        vec![Mnemonic::Cmp, Mnemonic::Inc, Mnemonic::Rts]
    );
    assert!(
        mnemonics(&program, &model, "branch_over_work").contains(&Mnemonic::Beq),
        "an instruction between the branch and its label keeps the branch"
    );
    assert!(
        mnemonics(&program, &model, "native_branch_to_next").contains(&Mnemonic::Bra),
        "a hand-written branch to the next label stays where it was written"
    );
    assert!(
        mnemonics(&program, &model, "frozen_tail").contains(&Mnemonic::Beq),
        "a `nocross` guard measured its run at lowering, so nothing inside it moves"
    );
}

/// A removal can expose the next one, and the pass keeps going.
///
/// `> goto done` is two branches — a guard over the branch that takes the
/// target, and the branch itself — and when `done` is the next label *both*
/// are branches to it. The second is only visible once the first is gone, so
/// a single scan would find one and stop. The whole conditional jump
/// collapses.
#[test]
fn removing_one_branch_to_the_next_label_exposes_the_next() {
    let (_, file, model, mut program) =
        lowered("func guarded @a8 @i8 {\n    a?5\n    > goto .done\n.done:\n    x++\n}\n");
    let log = run(&mut program, &model, file);
    assert_eq!(log.by_pass(Pass::RemoveBranchToNextLabel).count(), 2);
    assert_eq!(
        mnemonics(&program, &model, "guarded"),
        vec![Mnemonic::Cmp, Mnemonic::Inx, Mnemonic::Rts]
    );
}

/// Pass 10 removes a return the return above it already made, and stops at
/// anything that could let an edge in.
///
/// This is the one pass that removes a **user-written** operation, so its
/// three cases are the argument for that: `returns_twice` has nothing between
/// the two, `returns_around_a_label` has the one thing that lets control
/// arrive, and `mixed_returns` has two returns that are not the same return.
#[test]
fn a_second_return_goes_unless_something_could_reach_it() {
    let (_, file, model, mut program) = lowered_as("opt-tail-labels.k816", TAIL_LABELS);
    let log = run(&mut program, &model, file);
    assert_eq!(
        changed_by(&log, &model, Pass::CollapseDuplicateReturns),
        vec!["returns_twice"]
    );
    let change = log
        .by_pass(Pass::CollapseDuplicateReturns)
        .next()
        .expect("one change");
    assert_eq!(
        change.justification,
        "the return above it already left the function, and no label between them lets an edge in"
    );
    assert_eq!(
        change.witness.as_ref().map(|op| op.kind.clone()),
        Some(KOpKind::Instruction(k816::op::Instruction::new(
            Mnemonic::Rts,
            k816::isa::AddressingMode::Stack,
            None
        )))
    );
    assert_eq!(
        mnemonics(&program, &model, "returns_twice"),
        vec![Mnemonic::Lda, Mnemonic::Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "returns_around_a_label"),
        vec![Mnemonic::Lda, Mnemonic::Rts, Mnemonic::Rts],
        "a label between two returns is what lets an edge in"
    );
    assert_eq!(
        mnemonics(&program, &model, "mixed_returns"),
        vec![Mnemonic::Rts, Mnemonic::Rtl],
        "`rts` and `rtl` unwind different amounts of stack"
    );
    // Inside a `nocross` run both returns stay, for a reason that has nothing
    // to do with either pass's own rule: the guard's byte length was measured
    // at lowering, and removing a byte from inside the run would leave a
    // link-time assertion covering bytes that are no longer there. What goes
    // is the implicit return *after* the guard, which is outside the run and
    // is F's unreachable pass.
    assert_eq!(
        mnemonics(&program, &model, "frozen_tail"),
        vec![
            Mnemonic::Cmp,
            Mnemonic::Beq,
            Mnemonic::Inc,
            Mnemonic::Rts,
            Mnemonic::Rts
        ],
        "the frozen run keeps both returns and its branch"
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

// ---------------------------------------------------------------------------
// Branch shrinking (sub-plan F, task F7)
// ---------------------------------------------------------------------------

const BRANCH_RANGE: &str = include_str!("golden/opt-branch-range.k816");

/// A branch E emitted long, because its estimate at lowering could not prove
/// the short form reached, goes back to the short form once it does.
///
/// It comes back with the reason it had, which is why lowering leaves the
/// original reason on the guard rather than overwriting it: the annotation on
/// the emitted line still says what the branch is *for*, and only the jump
/// says why the idiom was there.
#[test]
fn a_long_branch_whose_distance_closed_goes_back_to_the_short_form() {
    let (_, file, model, mut program) = lowered_as("opt-branch-range.k816", BRANCH_RANGE);
    let log = run(&mut program, &model, file);

    let shrunk: Vec<_> = log.by_pass(Pass::ShrinkBranches).collect();
    assert_eq!(
        shrunk.len(),
        3,
        "one in the first function, two in the second"
    );
    assert_eq!(shrunk[0].before.len(), 3, "guard, jump, landing pad");
    assert_eq!(shrunk[0].after.len(), 1);
    assert_eq!(shrunk[0].after[0].reason, Some(Reason::InvertedBranch));
    assert_eq!(
        shrunk[0].justification,
        "the target is within a relative branch's reach now, by an upper bound over every \
         operation between them"
    );

    // Nothing is left of the idiom in the two functions that shrank.
    for name in ["now_fits", "second_fits_after_the_first"] {
        assert!(
            !mnemonics(&program, &model, name).contains(&Mnemonic::Jmp),
            "`{name}` still has a long-form branch"
        );
    }
}

/// The bound counts what is not an instruction.
///
/// `data_is_counted`'s body carries 128 bytes of inline data and the jump
/// around them. With the data measured as nothing, the pass would shrink a
/// branch ca65 then refuses with a range error — so the branch stays long,
/// after every other pass has taken what it can from the same body.
#[test]
fn a_branch_over_inline_data_does_not_shrink() {
    let (_, file, model, mut program) = lowered_as("opt-branch-range.k816", BRANCH_RANGE);
    run(&mut program, &model, file);
    let mnemonics = mnemonics(&program, &model, "data_is_counted");
    assert!(
        mnemonics.contains(&Mnemonic::Jmp),
        "the long form survives: {mnemonics:?}"
    );
}

/// **No `jsr`/`rts` or `jsl`/`rtl` pair is rewritten, anywhere.**
///
/// Phase 9 item 7 asks for *safe* tail calls, and the safe set K816 can
/// identify is empty: `jsr f` + `rts` → `jmp f` leaves `f` running with the
/// stack two bytes shallower and with a live slot holding the caller's own
/// return address, and no fact K816 holds rules out `f` reading it. The stack
/// is ordinary bank-zero memory, so `lda $01FE` reaches the frame without
/// naming `S`.
///
/// Asserted on a fixture rather than left implicit, so that the day someone
/// implements the rewrite without reading the reasoning, a test fails.
#[test]
fn no_call_and_return_pair_becomes_a_jump() {
    for name in every_self_contained_fixture() {
        let (_, file, model, mut program) = lowered_from_golden(&name);
        let before = tail_pairs(&program);
        run(&mut program, &model, file);
        assert_eq!(tail_pairs(&program), before, "`{name}`");
    }

    // And the fixture that carries both pairs on purpose, at the operations.
    let (_, file, model, mut program) = lowered_as("opt-barriers.k816", BARRIERS);
    run(&mut program, &model, file);
    assert_eq!(
        mnemonics(&program, &model, "tail_call"),
        vec![Mnemonic::Jsr, Mnemonic::Rts]
    );
    assert_eq!(
        mnemonics(&program, &model, "long_tail"),
        vec![Mnemonic::Jsl, Mnemonic::Rtl]
    );
}

/// Every call immediately followed by a return, as `(call, return)` pairs.
fn tail_pairs(program: &KProgram) -> Vec<(Mnemonic, Mnemonic)> {
    program
        .functions()
        .flat_map(|function| {
            function.ops.windows(2).filter_map(|pair| {
                let (KOpKind::Instruction(first), KOpKind::Instruction(second)) =
                    (&pair[0].kind, &pair[1].kind)
                else {
                    return None;
                };
                matches!(first.mnemonic, Mnemonic::Jsr | Mnemonic::Jsl)
                    .then_some((first.mnemonic, second.mnemonic))
                    .filter(|_| matches!(second.mnemonic, Mnemonic::Rts | Mnemonic::Rtl))
            })
        })
        .collect()
}

/// Every change in every log carries the streams and the effects that
/// justified it.
///
/// Phase 9's first success criterion, asserted over the whole fixture set
/// rather than only where a test happened to look: a change with an empty
/// `before`, or one whose instructions have no derivable effects, is a rewrite
/// nothing can audit.
#[test]
fn every_recorded_change_carries_its_streams_and_its_effects() {
    for name in every_self_contained_fixture() {
        let (_, file, model, mut program) = lowered_from_golden(&name);
        for change in run(&mut program, &model, file).changes {
            assert!(!change.before.is_empty(), "`{name}`: {change:?}");
            assert!(
                change.justification.len() > 20,
                "`{name}`: {}",
                change.justification
            );
            // The justification rests on effects, and the effects are derived
            // from the ISA table rather than copied into the record — so what
            // this checks is that every operation the change names *has* one.
            let effects = change.effects();
            let instructions = change
                .before
                .iter()
                .filter(|op| matches!(op.kind, KOpKind::Instruction(_)))
                .count();
            assert_eq!(effects.len(), instructions, "`{name}`: {change:?}");
            for effects in effects {
                assert_ne!(effects.control, k816::effects::ControlEffect::Unknown);
                assert_ne!(effects.memory, MemoryEffect::Unknown);
            }
        }
    }
}

/// A `nocross` guard freezes the branch inside it too.
///
/// The guard records the byte length of the run it closes, so shrinking a
/// branch inside one would leave a link-time assertion covering bytes the
/// `nocross` never claimed. Nothing in the run moves — not the dead `clc`,
/// and not the branch — and the reason is the guard rather than the distance.
#[test]
fn nothing_inside_a_nocross_run_shrinks_or_is_removed() {
    let (_, file, model, mut program) = lowered_as("opt-branch-range.k816", BRANCH_RANGE);
    let before = mnemonics(&program, &model, "frozen_run");
    run(&mut program, &model, file);
    assert_eq!(mnemonics(&program, &model, "frozen_run"), before);
    assert!(
        before.contains(&Mnemonic::Clc) && before.contains(&Mnemonic::Jmp),
        "the dead `clc` and the long form are both in it: {before:?}"
    );
}

/// `XBA` moves sixteen bits and reports on eight.
///
/// The value it establishes is full-width — it swaps the two halves of the
/// accumulator — but N and Z come from the new *low byte*, regardless of M.
/// `lda #$0001` then `xba` leaves A = $0100 with Z **set**, while a 16-bit
/// `lda #$0100` would leave Z clear, so the second load is not redundant
/// however much the accumulator agrees. Tracking the value and the flag
/// provenance as one thing loses the difference, and the program takes the
/// other branch.
#[test]
fn xba_leaves_the_tracked_flags_unknown() {
    let (_, file, model, mut program) = lowered_as("opt-values.k816", VALUES);
    run(&mut program, &model, file);
    assert_eq!(
        mnemonics(&program, &model, "xba_reports_on_the_low_byte"),
        vec![
            Mnemonic::Lda,
            Mnemonic::Xba,
            Mnemonic::Sta,
            Mnemonic::Lda, // stays: `xba` set Z from $00, this sets it from $0100
            Mnemonic::Beq,
            Mnemonic::Nop,
            Mnemonic::Nop,
            Mnemonic::Rts
        ]
    );
}

/// A memory load establishes nothing, and clears what was there.
///
/// Decision 14 has two halves. Not coalescing the memory read is the one the
/// hardware-register case makes; not carrying a stale value *across* it is the
/// other, and it is what keeps the second `lda #$07` below.
#[test]
fn a_memory_load_clears_the_tracked_value() {
    let (_, file, model, mut program) = lowered_as("opt-values.k816", VALUES);
    run(&mut program, &model, file);
    assert_eq!(
        mnemonics(&program, &model, "memory_clears_the_tracker"),
        vec![
            Mnemonic::Lda, // from memory: the first `lda #$07` was dead
            Mnemonic::Lda, // stays: nothing knows what memory held
            Mnemonic::Sta,
            Mnemonic::Rts
        ]
    );
}

/// A `SEP`/`REP` mask can write N and Z directly, and then the flags no longer
/// report on the tracked value.
///
/// `sep #$82` sets the negative and zero flags, so the load after it is not a
/// no-op however much the accumulator agrees — it puts N and Z back to what the
/// value says, and the branch below reads them. The mask is one of the few
/// operations the tracker follows *through* rather than dying at, which is what
/// makes this worth asserting.
#[test]
fn a_mask_that_writes_n_and_z_clears_the_flag_provenance() {
    let (_, file, model, mut program) = lowered_as("opt-values.k816", VALUES);
    run(&mut program, &model, file);
    assert_eq!(
        mnemonics(&program, &model, "a_mask_can_write_n_and_z"),
        vec![
            Mnemonic::Sep, // the first `lda #$07` was dead: this overwrote N and Z
            Mnemonic::Lda, // stays: the mask wrote the flags, the load reports again
            Mnemonic::Beq,
            Mnemonic::Nop,
            Mnemonic::Nop,
            Mnemonic::Rts
        ]
    );
}

/// A write the hardware reads between instructions is never dead.
///
/// No instruction in the stream reads `I`, `S` or `E`, so a liveness over the
/// stream alone calls `cli` before an idle loop dead — nothing follows — and
/// `txs` dead once a later `tcs` overwrites it. Both are wrong: an interrupt
/// is accepted only while `I` is clear, every interrupt, NMI included, pushes
/// at `S`, and `E` picks the vector and the frame. The optimizer deleted all
/// three in a kernel's start-up and system-call return before this rule.
#[test]
fn a_write_the_hardware_reads_between_instructions_is_never_dead() {
    let text = "\
func start @a16 @i16 {
    cli
    {} always
}
naked trap_exit {
    @a16 @i16
    tyx
    txs
    ldx #$10
    lda #$20
    tcs
    rti
}
func flip @a8 @i8 {
    sec
    xce
    clc
    xce
    {} always
}
";
    let (_, file, model, mut program) = lowered(text);
    let log = run(&mut program, &model, file);
    assert!(
        mnemonics(&program, &model, "start").contains(&Mnemonic::Cli),
        "`cli` before an idle loop was removed: {log:?}"
    );
    let exit = mnemonics(&program, &model, "trap_exit");
    for kept in [Mnemonic::Tyx, Mnemonic::Txs, Mnemonic::Tcs] {
        assert!(exit.contains(&kept), "{kept:?} was removed: {exit:?}");
    }
    assert_eq!(
        mnemonics(&program, &model, "flip")
            .iter()
            .filter(|mnemonic| **mnemonic == Mnemonic::Xce)
            .count(),
        2,
        "an `xce` was removed"
    );
}
