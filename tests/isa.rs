/*****************************************************************************/
/*                                                                           */
/*                                   isa.rs                                  */
/*                                                                           */
/*        The 256-pair ISA table, cross-checked against the real ca65        */
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

//! The ISA table's claims, checked against the tool that owns encoding.
//!
//! `doc/w65c816s.md` §1.1 fixes the shape of the table — "24 addressing
//! modes … 92 instructions using 256 opcodes" — and the unit tests in
//! `src/isa.rs` check those counts and the keys' uniqueness. What they cannot
//! check is whether each row describes an instruction that *exists*: a table
//! of 256 plausible rows is exactly as countable as a table of 256 correct
//! ones.
//!
//! So this file emits one instruction per legal pair, in the ca65 spelling
//! the table says it has, and assembles the result with the real ca65. A pair
//! ca65 rejects is a table bug, and a pair it accepts confirms the mode, the
//! operand width, and the `#` policy together — without hand-writing 256
//! expectations.

mod common;

use std::process::Command;

use k816::isa::{
    AddressEncoding, AddressingMode, ImmediateWidthClass, InstructionSpec, Mnemonic, OperandForm,
    OperandWidth, all_specs,
};

/// The ca65 operand text for one spec, at a width ca65 will accept.
///
/// Values are chosen to fit each encoding exactly, so a spec that claims the
/// wrong operand width produces an assembly error rather than a silently
/// widened instruction.
fn operand_text(spec: &InstructionSpec) -> String {
    let literal = match spec.operand.width {
        OperandWidth::None => String::new(),
        OperandWidth::Immediate(ImmediateWidthClass::Fixed16) => "$1234".to_owned(),
        OperandWidth::Immediate(_) => "$12".to_owned(),
        OperandWidth::Address(AddressEncoding::Direct) => "$12".to_owned(),
        OperandWidth::Address(AddressEncoding::Absolute) => "$1234".to_owned(),
        OperandWidth::Address(AddressEncoding::Long) => "$123456".to_owned(),
        // A displacement is ca65's to compute, so the source names a label.
        OperandWidth::Displacement => "target".to_owned(),
        OperandWidth::BankPair => String::new(),
    };
    let prefix = if spec.operand.ca65_immediate_sigil {
        "#"
    } else {
        ""
    };
    match spec.operand.form {
        OperandForm::Nothing => String::new(),
        OperandForm::Accumulator => "a".to_owned(),
        OperandForm::Immediate | OperandForm::Address => format!("{prefix}{literal}"),
        OperandForm::AddressIndexedX => format!("{literal},x"),
        OperandForm::AddressIndexedY => format!("{literal},y"),
        OperandForm::StackRelative => format!("{literal},s"),
        OperandForm::Indirect => format!("({literal})"),
        OperandForm::IndirectIndexedX => format!("({literal},x)"),
        OperandForm::IndirectIndexedY => format!("({literal}),y"),
        OperandForm::StackRelativeIndirectIndexedY => format!("({literal},s),y"),
        OperandForm::LongIndirect => format!("[{literal}]"),
        OperandForm::LongIndirectIndexedY => format!("[{literal}],y"),
        OperandForm::BlockMove => "$12,$34".to_owned(),
    }
}

/// One assembly line, with the explicit address-size prefix K816 emits for
/// memory operands so ca65 cannot choose a different encoding from its own
/// symbol knowledge.
fn line(spec: &InstructionSpec) -> String {
    let mut operand = operand_text(spec);
    if spec.operand.form.takes_address_size_prefix()
        && let Some(encoding) = spec.encoding()
    {
        operand = format!("{}{operand}", encoding.ca65_prefix());
    }
    if operand.is_empty() {
        format!("    {}", spec.mnemonic.ca65())
    } else {
        format!("    {} {operand}", spec.mnemonic.ca65())
    }
}

#[test]
fn every_legal_pair_assembles_with_the_real_ca65() {
    let mut source = String::from(".setcpu \"65816\"\n.segment \"CODE\"\n");
    // The width-dependent immediates are sized by the assembler's tracked
    // widths, so both are pinned before anything is emitted.
    source.push_str("        .a16\n        .i16\n");
    source.push_str("target:\n");
    for spec in all_specs() {
        source.push_str(&line(spec));
        source.push('\n');
    }

    let dir = common::work_dir("isa-cross-check");
    let asm = dir.join("all-pairs.s");
    std::fs::write(&asm, &source).expect("write the probe source");

    common::run_ok(
        Command::new(common::tool("CA65", "ca65"))
            .arg("-o")
            .arg(dir.join("all-pairs.o"))
            .arg(&asm),
        &format!(
            "the ISA cross-check: ca65 rejected an instruction the table \
             claims is legal (probe: {})",
            asm.display()
        ),
    );
}

/// The width-dependent immediates change size with the tracked width, and a
/// spec that mislabels the axis assembles fine at 16 bits and wrong at 8.
#[test]
fn width_dependent_immediates_follow_their_own_axis() {
    let mut source = String::from(".setcpu \"65816\"\n.segment \"CODE\"\n");
    // An 8-bit accumulator with 16-bit index registers: an M-dependent
    // immediate now takes one byte and an X-dependent one takes two, so a
    // spec on the wrong axis produces a range error.
    source.push_str("        .a8\n        .i16\n");
    for spec in all_specs() {
        let Some(class) = spec.immediate_width() else {
            continue;
        };
        let value = match class {
            ImmediateWidthClass::MDependent => "$12",
            ImmediateWidthClass::XDependent => "$1234",
            ImmediateWidthClass::Fixed8 => "$12",
            ImmediateWidthClass::Fixed16 => "$1234",
        };
        let prefix = if spec.operand.ca65_immediate_sigil {
            "#"
        } else {
            ""
        };
        source.push_str(&format!("    {} {prefix}{value}\n", spec.mnemonic.ca65()));
    }

    let dir = common::work_dir("isa-immediate-widths");
    let asm = dir.join("immediates.s");
    std::fs::write(&asm, &source).expect("write the probe source");

    common::run_ok(
        Command::new(common::tool("CA65", "ca65"))
            .arg("-o")
            .arg(dir.join("immediates.o"))
            .arg(&asm),
        &format!(
            "the immediate-width cross-check: a width class does not match \
             ca65's (probe: {})",
            asm.display()
        ),
    );
}

/// Both long jumps belong to `JML`, because the cc65 tree's ca65 rejects
/// `jmp` with a 24-bit address outright — the assembly K816 emits is the
/// authority for what it may say, even where `doc/w65c816s.md` Table 5-5
/// files that opcode under `JMP`. The two `JML` rows are told apart by
/// source form, not by mode, which is the two-axis design doing its job.
#[test]
fn the_long_jumps_belong_to_jml() {
    use k816::isa::{spec, specs_for};

    let jmp: Vec<AddressingMode> = specs_for(Mnemonic::Jmp).map(|s| s.mode).collect();
    assert_eq!(jmp.len(), 3);
    assert!(!jmp.contains(&AddressingMode::AbsoluteLong));
    assert!(jmp.contains(&AddressingMode::AbsoluteIndirect));
    assert_eq!(
        spec(Mnemonic::Jmp, AddressingMode::AbsoluteIndirect)
            .unwrap()
            .operand
            .form,
        OperandForm::Indirect,
        "`jmp (addr)` is the 16-bit indirect jump"
    );

    let jml: Vec<AddressingMode> = specs_for(Mnemonic::Jml).map(|s| s.mode).collect();
    assert_eq!(jml.len(), 2);
    assert_eq!(
        spec(Mnemonic::Jml, AddressingMode::AbsoluteLong)
            .unwrap()
            .operand
            .form,
        OperandForm::Address,
        "`jml addr` takes a bare 24-bit address"
    );
    assert_eq!(
        spec(Mnemonic::Jml, AddressingMode::AbsoluteIndirect)
            .unwrap()
            .operand
            .form,
        OperandForm::LongIndirect,
        "`jml [addr]` is written with brackets and encodes an absolute pointer"
    );
}
