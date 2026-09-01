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

func work @a8 @i16 {
    a = #0
    a = #1
    SCREEN = a
    return
}
";

/// The hand-written native equivalent. The `@a8 @i16` contract makes the
/// compiler emit the same explicit entry-mode setup for both forms.
const NATIVE_SLICE: &str = "var SCREEN = $0400

func work @a8 @i16 {
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
    lower_unit(text).2
}

/// The same, keeping what the optimizer needs beside the stream.
fn lower_unit(
    text: &str,
) -> (
    k816::source::FileId,
    k816::semantic::SemanticModel,
    KProgram,
) {
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "unexpected semantic diagnostics");
    let (program, diagnostics) = lower(model.program(&parse.file), &model);
    assert_eq!(diagnostics, vec![], "unexpected lowering diagnostics");
    (id, model, program)
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
            KOpKind::ModeAssumption(state) => format!("assume {state:?}"),
            other => panic!("unexpected slice operation {other:?}"),
        })
        .collect();

    assert_eq!(
        kinds,
        vec![
            "label#0", // work
            // The contract is claimed, not executed: no function generates
            // width setup at entry, because its caller established the
            // widths — a call site for an ordinary function, the environment
            // for the entry point.
            "assume ModeState { a: Known(W8), i: Known(W16), e: Native }",
            "lda #$00", // the dead load — removed only by the optimizer
            "lda #$01",
            "sta sym#1", // SCREEN
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

    // The width claim is compiler-generated in both forms; the body ops
    // record which syntax produced them.
    assert_eq!(
        origins(&hla),
        vec![
            Origin::User,              // label
            Origin::CompilerGenerated, // the entry width claim
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
            Origin::CompilerGenerated, // the entry width claim
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
    let (program, _) = lower(model.program(&parse.file), &model);

    for op in &first_function(&program).ops {
        assert_eq!(op.span.file, id);
        assert!(op.span.end as usize <= HLA_SLICE.len());
    }
    // HLA statements map back to their exact source text.
    assert_eq!(
        map.span_text(first_function(&program).ops[2].span),
        "a = #0"
    );
    assert_eq!(
        map.span_text(first_function(&program).ops[4].span),
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

// ---------------------------------------------------------------------------
// Native lowering (sub-plan C, task C5).
// ---------------------------------------------------------------------------

/// The IR dump of `text`, which is what `emit-ir` prints.
fn dump(text: &str) -> String {
    let mut map = SourceMap::new();
    let id = map.add_file("native.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    let (model, diagnostics) = analyze_file(&parse.file);
    // Warnings are output, not failure: a fixture that deliberately does an
    // `xce` is worth warning about and is still exactly what is being dumped.
    let errors: Vec<&k816::diagnostic::Diagnostic> =
        diagnostics.iter().filter(|d| d.is_error()).collect();
    assert!(errors.is_empty(), "unexpected semantic errors: {errors:?}");
    let (program, diagnostics) = lower(model.program(&parse.file), &model);
    assert_eq!(diagnostics, vec![], "unexpected lowering diagnostics");
    program.dump(&model.symbols, &map)
}

#[test]
fn the_ir_dump_renders_the_whole_operand_vocabulary() {
    let text = "var ZP = $80
var SCREEN = $0400
const FIELD = 2

func work @a8 @i16 {
  .spot:
    lda ZP
    lda SCREEN,x
    lda (ZP),y
    lda [ZP],y
    lda 4,s
    lda (4,s),y
    lda &FIELD,x
    lda #&<SCREEN
    mvn $01,$02
    asl a
    bne .spot
}
";
    let dump = dump(text);
    let operations: Vec<&str> = dump
        .lines()
        .filter_map(|line| line.split_once(';'))
        .map(|(text, _)| text.trim())
        .filter(|text| !text.is_empty())
        .collect();

    assert_eq!(
        operations,
        vec![
            "label work",
            "mode-assume @a8 @i16",
            "label .spot",
            // Every reachable label carries the model's conclusion, because a
            // branch arrival cannot be read off the preceding instruction.
            "mode-assume @a8 @i16",
            // The chosen encoding is part of the dump: it is the decision a
            // reader most needs to check, and it is invisible in the source.
            "lda dp ZP",
            "lda abs SCREEN,x",
            "lda (dp ZP),y",
            "lda [dp ZP],y",
            "lda $04,s",
            "lda ($04,s),y",
            "lda dp $02,x",
            "lda #&<SCREEN",
            "mvn $01,$02",
            "asl",
            "bne .spot",
            "rts",
        ]
    );
}

/// Decision 15: a leading placement directive places the *section*, so its
/// operations precede the section's label — and an `address` puts the label
/// in a segment of its own, which has to be left again at the end of the
/// body or every later section is placed with it.
#[test]
fn a_placement_directive_wraps_the_section_it_opens() {
    let aligned = dump("func at @a8 @i8 {\n  align 16 + 4\n  a = 1\n  return\n}\n");
    let operations: Vec<&str> = aligned
        .lines()
        .filter_map(|line| line.split_once(';'))
        .map(|(text, _)| text.trim())
        .filter(|text| !text.is_empty())
        .collect();
    assert_eq!(
        operations,
        vec![
            "align $10",
            // The four bytes come before the label, so `at` names the
            // congruent address rather than the boundary.
            "res $04",
            "label at",
            "mode-assume @a8 @i8",
            "lda #$01",
            "rts",
        ]
    );

    let placed = dump("func at @a8 @i8 {\n  address $2040\n  a = 1\n  return\n}\n");
    let segments: Vec<&str> = placed
        .lines()
        .filter_map(|line| line.split_once(';'))
        .map(|(text, _)| text.trim())
        .filter(|text| text.starts_with("segment"))
        .collect();
    assert_eq!(
        segments,
        vec!["segment K816_AT_2040__AT", "segment CODE"],
        "the placed segment is entered before the label and left after the body"
    );

    // Every section kind the original's `Align` slot sits in: `func` above,
    // and `naked` and `main` here (`compiler.inc:121-123`). A data block is
    // the one place a directive may also sit *between* entries, which is
    // K816's own rule and decision 15 leaves alone.
    for section in ["naked bare @a8 @i8", "main @a8 @i8"] {
        let dumped = dump(&format!("{section} {{\n  align 8\n  a = 1\n}}\n"));
        let first = dumped
            .lines()
            .filter_map(|line| line.split_once(';'))
            .map(|(text, _)| text.trim())
            .find(|text| !text.is_empty());
        assert_eq!(first, Some("align $08"), "`{section}` opens with its align");
    }

    // The segment a placed section leaves is the one that was in force, not
    // the unit's default: a `segment` before it is lexical state, and
    // restoring `CODE` would move every section after it.
    let custom = dump(
        "segment CUSTOM\nfunc at @a8 @i8 {\n  address $2040\n  a = 1\n  return\n}\n\
         func after @a8 @i8 {\n  a = 2\n  return\n}\n",
    );
    let segments: Vec<&str> = custom
        .lines()
        .filter_map(|line| line.split_once(';'))
        .map(|(text, _)| text.trim())
        .filter(|text| text.starts_with("segment"))
        .collect();
    assert_eq!(
        segments,
        vec![
            "segment CUSTOM",
            "segment K816_AT_2040__AT",
            "segment CUSTOM"
        ]
    );
}

/// Decision 17's rule asks for the *base symbol's* address size, and which
/// symbol that is depends on scope: an `inline` alias shadows a unit-level
/// name of the same spelling, and stands for whatever the use site bound it
/// to.
///
/// Both halves are wrong bytes rather than a diagnostic, because
/// `known_address` has no answer for arithmetic, so the range check that
/// catches a bare `dp $0400` never sees the offset form.
#[test]
fn an_offset_takes_its_address_size_from_the_symbol_in_scope() {
    // The alias shadows a direct-page global of the same name and is bound
    // to an absolute variable. The store must be absolute.
    let shadowed = dump(
        "var dp target = $80\nvar abs other = $0400\n\
         inline store_to @a8 (a, target) -> a {\n  target+1 = a\n}\n\
         func place @a8 @i8 {\n  store_to a, #other -> a\n  return\n}\n",
    );
    assert!(
        shadowed.contains("sta abs (other + $01)"),
        "the alias, not the global it shadows, decides the encoding:\n{shadowed}"
    );

    // And with nothing shadowing it, the alias still has to be followed:
    // bound to a direct-page variable, the store narrows.
    let narrowed = dump(
        "var dp cursor = $10\n\
         inline store_to @a8 (a, slot) -> a {\n  slot+1 = a\n}\n\
         func place @a8 @i8 {\n  store_to a, #cursor -> a\n  return\n}\n",
    );
    assert!(
        narrowed.contains("sta dp (cursor + $01)"),
        "an alias bound to a direct-page variable narrows the offset too:\n{narrowed}"
    );
}

/// An immediate argument at a call site is an expression like any other, and
/// an evaluator bracket is one of the things it may be.
///
/// This is the failure mode a diagnostic cannot catch: an argument the
/// evaluator never ran folds to nothing, expansion has nothing to bind the
/// alias to, and the body is emitted with the alias *name* in it. K816 exits
/// zero and ca65 rejects an undefined symbol — so the assertion is on the
/// substituted operand, not on the absence of complaints.
#[test]
fn an_immediate_argument_may_be_an_evaluator_bracket() {
    let bracket = dump(
        "inline scale @a8 (a, #factor) -> a {\n  lda #factor\n}\n\
         func place @a8 @i8 {\n  scale a, #[2 + 2] -> a\n  return\n}\n",
    );
    assert!(
        bracket.contains("lda #$04"),
        "the bracket has to be evaluated before the alias can be bound:\n{bracket}"
    );
    assert!(
        !bracket.contains("#factor"),
        "an unbound alias reaches ca65 as an undefined symbol:\n{bracket}"
    );

    // The two spellings that already worked, so a future change cannot fix
    // one of the three by breaking the others.
    for argument in ["#4", "#SIZE"] {
        let program = dump(&format!(
            "const SIZE = 4\ninline scale @a8 (a, #factor) -> a {{\n  lda #factor\n}}\n\
             func place @a8 @i8 {{\n  scale a, {argument} -> a\n  return\n}}\n",
        ));
        assert!(
            program.contains("lda #$04"),
            "`{argument}` should substitute:\n{program}"
        );
    }
}

#[test]
fn a_native_only_function_lowers_without_any_hla_statement() {
    let program = lower_program(
        "var SCREEN = $0400\nfunc work @a8 @i16 {\n    lda #1\n    sta SCREEN\n    rts\n}\n",
    );
    let origins: Vec<Origin> = first_function(&program)
        .ops
        .iter()
        .map(|op| op.origin)
        .collect();
    assert_eq!(
        origins,
        vec![
            Origin::User,              // the function label
            Origin::CompilerGenerated, // the entry width claim
            Origin::User,              // lda #1
            Origin::User,              // sta SCREEN
            Origin::User,              // rts
        ],
        "no operation in a native function is an HLA expansion"
    );
}

/// **Moved by sub-plan F, task F1.** This asserted the opposite until the
/// optimizer had a block view: the straight-line scan bailed at the first
/// non-instruction operation and called it conservatism, so a `.byte` between
/// a dead write and its overwriter kept the write alive. It was really the
/// absence of a successor relation.
///
/// The claim it becomes is the true one, and it is the reason the old claim
/// was safe to drop rather than merely inconvenient: a data directive emits
/// bytes and touches no register and no flag, so it cannot observe a value.
/// A `nocross` guard *is* a barrier, and for a different reason — the guard
/// records the byte length of the run it closes, so removing an operation from
/// inside one would leave a link-time assertion covering bytes the `nocross`
/// never claimed.
#[test]
fn a_directive_is_not_a_dataflow_barrier_but_a_nocross_guard_is() {
    use k816::linkage::Linkage;
    use k816::op::{DataOp, KOp, KOpKind};
    use k816::optimize::{Context, optimize};
    use k816::value::KExpr;

    let run = |program: &mut KProgram, file, model: &k816::semantic::SemanticModel| {
        let linkage = Linkage::default();
        optimize(
            program,
            &Context {
                model,
                linkage: &linkage,
                file,
            },
        )
        .changes
        .len()
    };

    // `lda #0` is dead: `lda #1` overwrites A, N, and Z before any read.
    let (file, model, mut program) = lower_unit(HLA_SLICE);
    assert_eq!(run(&mut program, file, &model), 1);

    // A directive between them changes nothing: it emits bytes and observes
    // no CPU state, so the write is still dead.
    let (file, model, mut program) = lower_unit(HLA_SLICE);
    let function = only_function(&mut program);
    let span = function.ops[2].span;
    function.ops.insert(
        3,
        KOp::new(
            KOpKind::Data(DataOp::Byte(vec![KExpr::number(0)])),
            span,
            Origin::CompilerGenerated,
        ),
    );
    assert_eq!(
        run(&mut program, file, &model),
        1,
        "an emitted directive observes no register and no flag"
    );

    // A `nocross` guard around the pair does stop it: the guard's recorded
    // length is a claim about bytes that are still there.
    let (file, model, mut program) = lower_unit(HLA_SLICE);
    let function = only_function(&mut program);
    let span = function.ops[2].span;
    let length: u32 = function.ops[2..5]
        .iter()
        .map(k816::lower::byte_length)
        .sum();
    function.ops.insert(
        5,
        KOp::new(
            KOpKind::NocrossGuard { length },
            span,
            Origin::CompilerGenerated,
        ),
    );
    assert_eq!(
        run(&mut program, file, &model),
        0,
        "a `nocross` guard freezes the run it measures"
    );
}

/// The unit's one function, for a test that edits its stream.
fn only_function(program: &mut KProgram) -> &mut k816::op::KFunction {
    program
        .items
        .iter_mut()
        .find_map(|item| match item {
            k816::op::KProgramItem::Function(function) => Some(function),
            k816::op::KProgramItem::Op(_) | k816::op::KProgramItem::Data(_) => None,
        })
        .expect("one function")
}

// ---------------------------------------------------------------------------
// The width model's own operations (sub-plan D, task D3).
// ---------------------------------------------------------------------------

/// The lowered operations of `text`, as the IR dump renders them, with the
/// origin annotations stripped.
fn operations(text: &str) -> Vec<String> {
    dump(text)
        .lines()
        .filter_map(|line| line.split_once(';'))
        .map(|(text, _)| text.trim().to_owned())
        .filter(|text| !text.is_empty())
        .collect()
}

/// The order at a label is normative, and the optimizer depends on it.
///
/// The bridge is *immediately* after the label operation with nothing
/// between: that adjacency is how a fixed label bridge is recognized and kept
/// out of dead mode elimination. The assumption comes after the bridge,
/// because it states the width that holds once the bridge has run — emitting
/// it first would claim a width the next instruction then changes.
#[test]
fn a_label_bridge_sits_between_the_label_and_its_assumption() {
    let text = "\
func main @a8 {
    bcc .target
    @a16
.target:
    nop
}
";
    let ops = operations(text);
    let label = ops
        .iter()
        .position(|op| op == "label .target")
        .expect("the label was lowered");
    assert_eq!(
        &ops[label..label + 3],
        [
            "label .target".to_owned(),
            "rep #$20".to_owned(),
            // `@i8` and not unknown: this is the entry point, and what it
            // did not annotate the environment provides.
            "mode-assume @a16 @i8".to_owned(),
        ],
        "{ops:?}"
    );
}

/// The bridge covers exactly the axes that need it: a label all of whose
/// arrivals already agree with the annotation needs no instruction at all,
/// and not emitting one is not an optimization — the model knows the state,
/// so there is nothing to emit.
#[test]
fn a_label_whose_arrivals_already_agree_gets_no_bridge() {
    let text = "\
func work @a16 {
    bcc .target
    @a16
.target:
    nop
}
";
    let ops = operations(text);
    let label = ops
        .iter()
        .position(|op| op == "label .target")
        .expect("the label was lowered");
    assert_eq!(ops[label + 1], "mode-assume @a16 @i?", "{ops:?}");
}

/// A scoped block emits its entry bridge before the body and its restore
/// after it, per axis and only where the axis differs.
#[test]
fn a_scoped_block_brackets_its_body_with_enter_and_restore() {
    let text = "\
func main @a8 @i8 {
    @a16 {
        nop
    }
    nop
}
";
    assert_eq!(
        operations(text),
        [
            "label main",
            "mode-assume @a8 @i8",
            "rep #$20",
            "nop",
            "sep #$20",
            "nop",
            "rts",
        ]
    );
}

/// No function establishes its contract at entry — every one of them assumes
/// it, and the caller is what makes it true.
///
/// For an ordinary function the caller is a `call` site, which K816 bridges.
/// For `main` the caller is the environment, which cannot be bridged: its
/// widths are the environment's, an annotation on it is a claim about them
/// rather than a request, and one that disagrees is refused in analysis. So
/// neither kind of function generates width setup, and this is where that is
/// pinned.
#[test]
fn no_function_emits_entry_width_setup() {
    let text = "\
func work @a16 @i16 {
    nop
}

func main @a8 @i8 {
    nop
}
";
    let ops = operations(text);
    let entry: Vec<&String> = ops.iter().take_while(|op| *op != "nop").collect();
    assert_eq!(entry, ["label work", "mode-assume @a16 @i16"]);

    let main = ops
        .iter()
        .position(|op| op == "label main")
        .expect("the entry point");
    assert_eq!(
        &ops[main..main + 3],
        [
            "label main".to_owned(),
            "mode-assume @a8 @i8".to_owned(),
            "nop".to_owned(),
        ],
        "the entry point assumes the environment's widths: {ops:?}"
    );
}

/// A block no path reaches still emits its instructions — K816 does not
/// delete user code — but it makes no width claim over them.
#[test]
fn an_unreachable_block_emits_its_code_without_a_width_claim() {
    let text = "\
func main @a8 {
    bra .past
    nop
.past:
    nop
}
";
    let ops = operations(text);
    let unreachable = ops
        .iter()
        .position(|op| op == "bra .past")
        .expect("the branch");
    assert_eq!(
        ops[unreachable + 1],
        "nop",
        "the unreachable instruction is kept, with no assumption before it: {ops:?}"
    );
}

/// Where the width model refuses what an instruction's own effect implies,
/// the model wins and says so: a widening `rep` executed with the emulation
/// flag unknown establishes nothing, so the claim it would otherwise carry
/// is withdrawn.
#[test]
fn a_rep_the_model_does_not_believe_withdraws_its_claim() {
    let text = "\
func main @a8 {
    xce
    rep #$20
    nop
}
";
    let ops = operations(text);
    let rep = ops.iter().position(|op| op == "rep #$20").expect("the rep");
    assert_eq!(ops[rep + 1], "mode-assume @a? @i? emu?", "{ops:?}");
}

/// An operation records what the compiler produced it for, and both surfaces
/// read the same field.
///
/// `Origin` answers *who* wrote an operation; the reason answers *what for*,
/// and only the model that decided it can say. A `rep #$20` is a bridge for
/// one callee's contract, a scoped block's entry, or a label bridge, and the
/// instruction is identical in all three — so deriving the answer afterwards
/// from the operations around it is guesswork the moment there is more than
/// one producer of mode operations.
///
/// The callee both needs a bridge *and* moves the axis, which is what puts a
/// bridge and an exit claim in one function with different reasons. A callee
/// that merely preserved what the bridge established would get no claim at
/// all — the bridge already told the assembler, and a second claim saying the
/// same thing is noise (`postmortem.md` §I.2).
#[test]
fn a_generated_operation_says_what_it_is_for() {
    let text = "\
func narrows @a16 {
    sep #$20
}

func main @a8 @i8 {
    call narrows
    lda #$01
}
";
    let dump = dump(text);
    assert!(
        dump.contains("rep #$20                 ; compiler native.k816:6 — bridge for narrows"),
        "the bridge names the callee whose contract it satisfies: {dump}"
    );
    assert!(
        dump.contains("— narrows's exit contract"),
        "the claim after the call names where the width came from: {dump}"
    );
    assert!(
        dump.contains("— the function's entry contract, assumed"),
        "{dump}"
    );
    assert!(dump.contains("— implicit return"), "{dump}");
}

/// A callee K816 cannot see is annotated as *assumed*, in the IR dump as
/// well as in the emitted assembly.
///
/// Nothing else distinguishes it from a callee that was read and found to
/// preserve the caller's widths, which is exactly the distinction the
/// language's fallback rule turns on.
#[test]
fn an_invisible_callee_is_recorded_as_assumed_in_both_surfaces() {
    let text = "var VEC = $80\n\nfunc main @a8 {\n    jsr (VEC,x)\n    lda #$01\n}\n";
    assert!(
        dump(text).contains("— the callee's exit widths are unknown; assumed to preserve these"),
        "{}",
        dump(text)
    );

    let mut map = SourceMap::new();
    let file = map.add_file("assumed.k816", text);
    let assembly = k816::compile_to_ca65(
        &map,
        file,
        &k816::Options {
            optimize: false,
            ..Default::default()
        },
    )
    .assembly
    .expect("compiles");
    assert!(
        assembly.contains("k816: the callee's exit widths are unknown; assumed to preserve these"),
        "{assembly}"
    );
}
