/*****************************************************************************/
/*                                                                           */
/*                                 lowering.rs                               */
/*                                                                           */
/*        Native/HLA KOp equivalence — the key architectural gate            */
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

//! Native and HLA slice programs must lower to identical unoptimized KOp
//! sequences (master plan Phase 5: equivalence is asserted *before*
//! optimization).

use k816::isa::Mnemonic;
use k816::lower::lower;
use k816::op::{KFunction, KOpKind, KProgram, Operand, Origin};
use k816::parser::parse_source;
use k816::semantic::analyze_file;
use k816::source::SourceMap;
use k816::value::KExpr;

/// The slice program in HLA form (`doc/design.md` "Recommended first
/// deliverable", with the fixed-address `SCREEN` declaration).
const HLA_SLICE: &str = "var SCREEN = $0400

func main @a8 @i16 {
    a = #0
    a = #1
    SCREEN = a
    return
}
";

/// The hand-written native equivalent. The `@a8 @i16` contract makes the
/// compiler emit the same explicit entry-mode setup for both forms.
const NATIVE_SLICE: &str = "var SCREEN = $0400

func main @a8 @i16 {
    lda #0
    lda #1
    sta SCREEN
    rts
}
";

/// The unit's single lowered function. A `KProgram` is an ordered stream of
/// unit-level operations and functions, so tests reach in through the same
/// accessor the compiler uses rather than indexing a bare vector.
fn first_function(program: &KProgram) -> &KFunction {
    program.functions().next().expect("one lowered function")
}

fn lower_program(text: &str) -> KProgram {
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "unexpected semantic diagnostics");
    let (program, diagnostics) = lower(&parse.file, &model);
    assert_eq!(diagnostics, vec![], "unexpected lowering diagnostics");
    program
}

#[test]
fn native_and_hla_lower_to_identical_unoptimized_kops() {
    let hla = lower_program(HLA_SLICE);
    let native = lower_program(NATIVE_SLICE);

    assert_eq!(hla.functions().count(), 1);
    assert_eq!(native.functions().count(), 1);
    assert_eq!(
        first_function(&hla).contract,
        first_function(&native).contract
    );

    // The architectural gate: identical operation streams (kinds and
    // operands). Only the recorded origins may differ between the forms.
    let hla_kinds: Vec<&KOpKind> = first_function(&hla).ops.iter().map(|op| &op.kind).collect();
    let native_kinds: Vec<&KOpKind> = first_function(&native)
        .ops
        .iter()
        .map(|op| &op.kind)
        .collect();
    assert_eq!(hla_kinds, native_kinds);
}

#[test]
fn the_unoptimized_stream_is_exactly_the_expected_slice_sequence() {
    let program = lower_program(HLA_SLICE);
    let ops = &first_function(&program).ops;

    let kinds: Vec<String> = ops
        .iter()
        .map(|op| match &op.kind {
            KOpKind::Label(symbol) => format!("label#{}", symbol.index()),
            KOpKind::Instruction(instruction) => {
                let operand = match &instruction.operand {
                    None => String::new(),
                    Some(Operand::Immediate(expr)) => {
                        format!(" #${:02X}", expr.value().expect("folded immediate").get())
                    }
                    Some(Operand::Address {
                        expr: KExpr::Symbol(symbol),
                        ..
                    }) => format!(" sym#{}", symbol.index()),
                    Some(other) => panic!("unexpected slice operand {other:?}"),
                };
                format!("{}{}", instruction.mnemonic.ca65(), operand)
            }
            other => panic!("unexpected slice operation {other:?}"),
        })
        .collect();

    assert_eq!(
        kinds,
        vec![
            "label#1",  // main
            "sep #$20", // entry @a8
            "rep #$10", // entry @i16
            "lda #$00", // the dead load — removed only by the optimizer
            "lda #$01",
            "sta sym#0", // SCREEN
            "rts",
        ]
    );
}

#[test]
fn origins_distinguish_user_hla_and_compiler_operations() {
    let hla = lower_program(HLA_SLICE);
    let native = lower_program(NATIVE_SLICE);

    let origins = |program: &KProgram| -> Vec<Origin> {
        first_function(program)
            .ops
            .iter()
            .map(|op| op.origin)
            .collect()
    };

    // Entry setup is compiler-generated in both forms; the body ops record
    // which syntax produced them.
    assert_eq!(
        origins(&hla),
        vec![
            Origin::User,              // label
            Origin::CompilerGenerated, // sep
            Origin::CompilerGenerated, // rep
            Origin::HlaExpansion,
            Origin::HlaExpansion,
            Origin::HlaExpansion,
            Origin::HlaExpansion, // return
        ]
    );
    assert_eq!(
        origins(&native),
        vec![
            Origin::User,
            Origin::CompilerGenerated,
            Origin::CompilerGenerated,
            Origin::User,
            Origin::User,
            Origin::User,
            Origin::User, // rts
        ]
    );
}

#[test]
fn every_lowered_op_retains_a_source_span() {
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", HLA_SLICE);
    let parse = parse_source(map.file(id), None);
    let (model, _) = analyze_file(&parse.file);
    let (program, _) = lower(&parse.file, &model);

    for op in &first_function(&program).ops {
        assert_eq!(op.span.file, id);
        assert!(op.span.end as usize <= HLA_SLICE.len());
    }
    // HLA statements map back to their exact source text.
    assert_eq!(
        map.span_text(first_function(&program).ops[3].span),
        "a = #0"
    );
    assert_eq!(
        map.span_text(first_function(&program).ops[5].span),
        "SCREEN = a"
    );
}

#[test]
fn functions_without_a_trailing_return_get_a_synthetic_rts() {
    let program = lower_program("func main @a8 {\n    a = #1\n}\n");
    let ops = &first_function(&program).ops;
    let last = ops.last().expect("ops exist");

    let KOpKind::Instruction(instruction) = &last.kind else {
        panic!("expected an instruction");
    };
    assert_eq!(instruction.mnemonic, Mnemonic::Rts);
    assert_eq!(last.origin, Origin::CompilerGenerated);
    assert!(last.span.is_empty(), "synthetic rts has a zero-width span");
}
