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
use k816::lower::lower;
use k816::op::{KFunction, KOpKind, KProgram, Operand, Origin};
use k816::optimize::optimize;
use k816::parser::parse_source;
use k816::semantic::analyze_file;
use k816::source::SourceMap;
use k816::value::KExpr;

const HLA_SLICE: &str = include_str!("golden/slice.k816");

/// The unit's single lowered function. A `KProgram` is an ordered stream of
/// unit-level operations and functions, so tests reach in through the same
/// accessor the compiler uses rather than indexing a bare vector.
fn first_function(program: &KProgram) -> &KFunction {
    program.functions().next().expect("one lowered function")
}

fn lowered(text: &str) -> (SourceMap, k816::semantic::SemanticModel, KProgram) {
    let mut map = SourceMap::new();
    let id = map.add_file("slice.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![]);
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![]);
    let (program, diagnostics) = lower(&parse.file, &model);
    assert_eq!(diagnostics, vec![]);
    (map, model, program)
}

#[test]
fn ir_dump_shows_the_dead_load_before_and_after_optimization() {
    let (map, model, mut program) = lowered(HLA_SLICE);

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

    optimize(&mut program);

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
    let (_, _, mut program) = lowered(HLA_SLICE);
    let log = optimize(&mut program);

    assert_eq!(log.removals.len(), 1);
    let removal = &log.removals[0];

    // What was removed: the dead `a = #0` load.
    let KOpKind::Instruction(removed) = &removal.removed.kind else {
        panic!("expected an instruction");
    };
    assert_eq!(removed.mnemonic, Mnemonic::Lda);
    assert_eq!(removed.operand, Some(Operand::Immediate(KExpr::number(0))));
    assert_eq!(removal.removed.origin, Origin::HlaExpansion);

    // The justification: everything it writes (A, N, Z) is overwritten by
    // `lda #$01` before any read, and it has no observable memory or
    // control effect of its own.
    assert!(removal.effects.writes.registers.contains(RegisterSet::A));
    assert!(
        removal
            .effects
            .writes
            .flags
            .contains(FlagSet::N.union(FlagSet::Z))
    );
    assert_eq!(removal.effects.memory, MemoryEffect::None);
    assert_eq!(removal.effects.control, ControlEffect::FallThrough);

    let KOpKind::Instruction(overwriter) = &removal.overwritten_by.kind else {
        panic!("expected an instruction");
    };
    assert_eq!(overwriter.mnemonic, Mnemonic::Lda);
    assert_eq!(
        overwriter.operand,
        Some(Operand::Immediate(KExpr::number(1)))
    );
}

#[test]
fn reads_and_barriers_keep_live_operations() {
    let (_, _, mut program) = lowered(HLA_SLICE);
    optimize(&mut program);
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
    let (_, _, mut hla) = lowered(HLA_SLICE);
    let (_, _, mut native) = lowered(include_str!("golden/slice-native.k816"));

    let hla_log = optimize(&mut hla);
    let native_log = optimize(&mut native);

    assert_eq!(hla_log.removals.len(), 1);
    assert_eq!(native_log.removals.len(), 1);
    // Identical treatment regardless of source notation…
    assert_eq!(
        hla_log.removals[0].removed.kind,
        native_log.removals[0].removed.kind
    );
    // …while the origins still tell the two notations apart.
    assert_eq!(hla_log.removals[0].removed.origin, Origin::HlaExpansion);
    assert_eq!(native_log.removals[0].removed.origin, Origin::User);

    let kinds = |program: &KProgram| -> Vec<KOpKind> {
        first_function(program)
            .ops
            .iter()
            .map(|op| op.kind.clone())
            .collect()
    };
    assert_eq!(kinds(&hla), kinds(&native));
}
