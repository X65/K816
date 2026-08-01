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
            KOpKind::ModeAssumption(state) => format!("assume {state:?}"),
            other => panic!("unexpected slice operation {other:?}"),
        })
        .collect();

    assert_eq!(
        kinds,
        vec![
            "label#0",  // main
            "rep #$10", // entry @i16 — the @a8 half needs no instruction
            // The 8-bit half of the contract is claimed rather than executed.
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
    let (program, diagnostics) = lower(&parse.file, &model);
    assert_eq!(diagnostics, vec![], "unexpected lowering diagnostics");
    program.dump(&model.symbols, &map)
}

#[test]
fn the_ir_dump_renders_the_whole_operand_vocabulary() {
    let text = "var ZP = $80
var SCREEN = $0400
const FIELD = 2

func main @a8 @i16 {
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
            "label main",
            "rep #$10",
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

#[test]
fn a_native_only_function_lowers_without_any_hla_statement() {
    let program = lower_program(
        "var SCREEN = $0400\nfunc main @a8 @i16 {\n    lda #1\n    sta SCREEN\n    rts\n}\n",
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
            Origin::CompilerGenerated, // sep #$20
            Origin::CompilerGenerated, // rep #$10
            Origin::User,              // lda #1
            Origin::User,              // sta SCREEN
            Origin::User,              // rts
        ],
        "no operation in a native function is an HLA expansion"
    );
}

#[test]
fn a_directive_between_instructions_is_an_optimizer_barrier() {
    use k816::op::{DataOp, KOp, KOpKind};
    use k816::optimize::optimize;
    use k816::value::KExpr;

    // `lda #0` is dead: `lda #1` overwrites A, N, and Z before any read.
    let mut program = lower_program(HLA_SLICE);
    assert_eq!(optimize(&mut program).removals.len(), 1);

    // With a directive between them the pass must stop rather than reason
    // about what the emitted bytes do — the conservative answer for every
    // new op kind until sub-plan F says otherwise.
    let mut program = lower_program(HLA_SLICE);
    let function = program
        .items
        .iter_mut()
        .find_map(|item| match item {
            k816::op::KProgramItem::Function(function) => Some(function),
            k816::op::KProgramItem::Op(_) => None,
        })
        .expect("one function");
    let span = function.ops[3].span;
    function.ops.insert(
        4,
        KOp::new(
            KOpKind::Data(DataOp::Byte(vec![KExpr::number(0)])),
            span,
            Origin::CompilerGenerated,
        ),
    );
    assert_eq!(
        optimize(&mut program).removals.len(),
        0,
        "an emitted directive stops the scan"
    );
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
            "mode-assume @a16 @i?".to_owned(),
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
func main @a16 {
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

/// `main` establishes its contract at entry; every other function assumes it
/// and emits nothing, because its caller bridges to it.
#[test]
fn only_main_emits_entry_setup() {
    let text = "\
func main @a16 @i16 {
    nop
}

func helper @a16 @i16 {
    nop
}
";
    let ops = operations(text);
    let entry: Vec<&String> = ops.iter().take_while(|op| *op != "nop").collect();
    assert_eq!(
        entry,
        [
            "label main",
            // Only the `REP` half: the CPU is already 8-bit after reset, so a
            // `SEP` at entry would be redundant.
            "rep #$20",
            "rep #$10",
            "mode-assume @a16 @i16",
        ]
    );
    let helper = ops
        .iter()
        .position(|op| op == "label helper")
        .expect("the second function");
    assert_eq!(
        &ops[helper..helper + 3],
        [
            "label helper".to_owned(),
            "mode-assume @a16 @i16".to_owned(),
            "nop".to_owned(),
        ],
        "a non-entry function assumes its contract: {ops:?}"
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
#[test]
fn a_generated_operation_says_what_it_is_for() {
    let text = "\
func widens @a16 {
    nop
}

func main {
    call widens
    lda #$01
}
";
    let dump = dump(text);
    assert!(
        dump.contains("rep #$20                 ; compiler native.k816:6 — bridge for widens"),
        "the bridge names the callee whose contract it satisfies: {dump}"
    );
    assert!(
        dump.contains("— widens's exit contract"),
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
    let assembly = k816::compile_to_ca65(&map, file, &k816::Options { optimize: false })
        .assembly
        .expect("compiles");
    assert!(
        assembly.contains("k816: the callee's exit widths are unknown; assumed to preserve these"),
        "{assembly}"
    );
}
