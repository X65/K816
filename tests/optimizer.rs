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

//! The slice's one optimization (master plan Phase 5): the overwritten
//! `a = #0` is removed, with IR dumps before and after and the explicit
//! effects justifying the removal.

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
    let mut map = SourceMap::new();
    let id = map.add_file("slice.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![]);
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![]);
    let (program, diagnostics) = lower(&parse.file, &model);
    assert_eq!(diagnostics, vec![]);
    (map, id, model, program)
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
/// possible and is *also* what would let this pass delete a mode instruction:
/// a `sep #$20` has no memory effect, falls through, and writes M, and the
/// second one below overwrites that write without reading it. Nothing in the
/// candidate test rejects it. Deleting it here would happen with none of the
/// guards a mode change needs — an exit contract, a fixed label bridge, a live
/// truncation, a non-width bit in the mask — which is why the exclusion landed
/// in the same change as the read-set correction rather than later.
#[test]
fn no_pass_but_the_two_mode_passes_touches_a_mode_instruction() {
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
    assert!(log.is_empty(), "{:?}", log.changes);

    let mnemonics: Vec<Mnemonic> = first_function(&program)
        .ops
        .iter()
        .filter_map(|op| match &op.kind {
            KOpKind::Instruction(instruction) => Some(instruction.mnemonic),
            _ => None,
        })
        .collect();
    assert_eq!(
        mnemonics,
        vec![Mnemonic::Sep, Mnemonic::Sep, Mnemonic::Lda, Mnemonic::Rts]
    );
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
