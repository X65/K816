use k816_isa65816::{RegSet, mnemonic_effects};

use crate::diag::Diagnostic;
use crate::hir::{
    AddressOperandMode, AddressSizeHint, AddressValue, IndexRegister, InstructionOp, Op, OperandOp,
    Program,
};
use crate::span::Spanned;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CpuReg {
    A,
    X,
    Y,
}

impl CpuReg {
    fn index(self) -> usize {
        match self {
            CpuReg::A => 0,
            CpuReg::X => 1,
            CpuReg::Y => 2,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FlagSet(u8);

impl FlagSet {
    const NONE: Self = Self(0);
    const N: Self = Self(1 << 0);
    const Z: Self = Self(1 << 1);
    const C: Self = Self(1 << 2);
    const V: Self = Self(1 << 3);
    const D: Self = Self(1 << 4);
    const I: Self = Self(1 << 5);
    const M_WIDTH: Self = Self(1 << 6);
    const X_WIDTH: Self = Self(1 << 7);

    fn intersects(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }
}

impl std::ops::BitOr for FlagSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

fn canonical_mnemonic(m: &str) -> String {
    m.to_ascii_lowercase()
}

/// Returns the canonical mnemonic for an unconditional return instruction.
fn return_mnemonic(m: &str) -> Option<&'static str> {
    if m.eq_ignore_ascii_case("rts") {
        Some("rts")
    } else if m.eq_ignore_ascii_case("rtl") {
        Some("rtl")
    } else if m.eq_ignore_ascii_case("rti") {
        Some("rti")
    } else {
        None
    }
}

/// Output of the peephole pass: the rewritten program plus any warnings
/// emitted by passes that drop user-visible code (currently only
/// `prune_unreachable_instructions`).
pub struct PeepholeOutput {
    pub program: Program,
    pub warnings: Vec<Diagnostic>,
}

/// Peephole optimization pass.
///
/// Runs the collected peephole rewrites over the whole program in order:
/// 1. `coalesce_repeated_immediate_loads` — drops repeated literal register
///    loads when only flag-preserving stores sit between them.
/// 2. `remove_dead_adjacent_register_writes` — drops adjacent register-only
///    writes that are immediately overwritten with fresh N/Z flags.
/// 3. `stz_rewrite` — replaces dead `LDA #0; STA mem[,X]` runs with `STZ`.
/// 4. `collapse_duplicate_flag_ops` — drops repeated adjacent flag set/clear
///    ops.
/// 5. `rewrite_tail_calls` — turns adjacent `jsr; rts` / `jsl; rtl` into
///    `jmp` / `jml`.
/// 6. `prune_unreachable_instructions` — drops plain instructions after an
///    unconditional branch/jump until the next reachable boundary. Runs
///    before `remove_branch_to_next_label` so branches separated from their
///    target only by other unreachable branches still collapse. This is
///    the only pass that emits warnings, since dropped instructions came
///    from user source and the user should know they are not encoded.
/// 7. `remove_branch_to_next_label` — removes branches/jumps to the next
///    label.
/// 8. `collapse_duplicate_returns` — drops an adjacent `rts`/`rtl`/`rti`
///    that is preceded by an identical return.
pub fn peephole_optimize(program: &Program) -> PeepholeOutput {
    let ops = coalesce_repeated_immediate_loads(&program.ops);
    let ops = remove_dead_adjacent_register_writes(&ops);
    let ops = stz_rewrite(&ops);
    let ops = collapse_duplicate_flag_ops(&ops);
    let ops = rewrite_tail_calls(&ops);
    let (ops, warnings) = prune_unreachable_instructions(&ops);
    let ops = remove_branch_to_next_label(&ops);
    let ops = collapse_duplicate_returns(&ops);
    PeepholeOutput {
        program: Program { ops },
        warnings,
    }
}

fn coalesce_repeated_immediate_loads(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut known_loads: [Option<i64>; 3] = [None, None, None];

    for op in ops {
        if is_basic_block_barrier(&op.node) {
            known_loads = [None, None, None];
            out.push(op.clone());
            continue;
        }

        match &op.node {
            Op::Instruction(inst) => {
                if let Some((reg, value)) = immediate_register_load(inst) {
                    let idx = reg.index();
                    if known_loads[idx] == Some(value) {
                        continue;
                    }
                    known_loads = [None, None, None];
                    known_loads[idx] = Some(value);
                    out.push(op.clone());
                } else if is_flag_preserving_store(inst) {
                    out.push(op.clone());
                } else {
                    known_loads = [None, None, None];
                    out.push(op.clone());
                }
            }
            _ => {
                known_loads = [None, None, None];
                out.push(op.clone());
            }
        }
    }

    out
}

fn remove_dead_adjacent_register_writes(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut i = 0;
    while i < ops.len() {
        if i + 1 < ops.len() && dead_adjacent_register_write(&ops[i].node, &ops[i + 1].node) {
            i += 1;
            continue;
        }
        out.push(ops[i].clone());
        i += 1;
    }
    out
}

fn collapse_duplicate_flag_ops(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut last_flag_op: Option<&'static str> = None;

    for op in ops {
        match &op.node {
            Op::Instruction(InstructionOp {
                mnemonic,
                operand: None,
            }) => {
                let Some(flag_op) = duplicate_flag_op_mnemonic(mnemonic) else {
                    last_flag_op = None;
                    out.push(op.clone());
                    continue;
                };
                if last_flag_op == Some(flag_op) {
                    continue;
                }
                last_flag_op = Some(flag_op);
                out.push(op.clone());
            }
            _ => {
                last_flag_op = None;
                out.push(op.clone());
            }
        }
    }

    out
}

fn rewrite_tail_calls(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut i = 0;
    while i < ops.len() {
        if i + 1 < ops.len()
            && let Some(replacement) = tail_call_replacement(&ops[i].node, &ops[i + 1].node)
        {
            let mut rewritten = ops[i].clone();
            if let Op::Instruction(inst) = &mut rewritten.node {
                inst.mnemonic = replacement.to_string();
            }
            out.push(rewritten);
            i += 2;
            continue;
        }
        out.push(ops[i].clone());
        i += 1;
    }
    out
}

fn remove_branch_to_next_label(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());

    for (idx, op) in ops.iter().enumerate() {
        if let Some(target) = branch_or_jump_direct_label(&op.node)
            && next_label_matches(ops, idx + 1, target)
        {
            continue;
        }
        out.push(op.clone());
    }

    out
}

fn prune_unreachable_instructions(ops: &[Spanned<Op>]) -> (Vec<Spanned<Op>>, Vec<Diagnostic>) {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut warnings: Vec<Diagnostic> = Vec::new();
    let mut unreachable = false;

    for (idx, op) in ops.iter().enumerate() {
        if unreachable {
            if starts_reachable_region(&op.node) {
                unreachable = false;
            } else if let Op::Instruction(inst) = &op.node {
                if !is_synthesized_function_epilogue(ops, idx, op.span, inst) {
                    warnings.push(unreachable_instruction_diagnostic(op.span, inst));
                }
                continue;
            }
        }

        out.push(op.clone());

        if is_unconditional_prune_transfer(&op.node) {
            unreachable = true;
        }
    }

    (out, warnings)
}

/// True when the op at `idx` is the compiler-emitted `rts`/`rtl` epilogue
/// — recognised by being an operandless return immediately followed by an
/// `Op::FunctionEnd` at the same span (the lowering pass at `lower.rs:2006`
/// uses `item.span` for both). User-written returns sit on a narrower span
/// and have additional ops between them and `FunctionEnd`, so they still
/// produce warnings when unreachable.
fn is_synthesized_function_epilogue(
    ops: &[Spanned<Op>],
    idx: usize,
    span: crate::span::Span,
    inst: &InstructionOp,
) -> bool {
    if inst.operand.is_some() {
        return false;
    }
    if !matches!(canonical_mnemonic(&inst.mnemonic).as_str(), "rts" | "rtl") {
        return false;
    }
    let Some(next) = ops.get(idx + 1) else {
        return false;
    };
    matches!(next.node, Op::FunctionEnd) && next.span == span
}

/// Build the warning emitted when `prune_unreachable_instructions` drops
/// an `Op::Instruction`. Two flavours:
///   * dot-prefixed mnemonic with no operand → almost certainly a label
///     declaration missing its trailing ':'; show the same advice as the
///     `EncodeError::UnknownMnemonic` enrichment in `emit_object`.
///   * everything else → generic "unreachable instruction" warning.
fn unreachable_instruction_diagnostic(span: crate::span::Span, inst: &InstructionOp) -> Diagnostic {
    let mnemonic = inst.mnemonic.as_str();
    if mnemonic.starts_with('.') && inst.operand.is_none() {
        Diagnostic::warning(
            span,
            format!(
                "unreachable instruction dropped after unconditional branch; '{mnemonic}' looks like a label declaration that is missing ':'"
            ),
        )
        .with_primary_label("label declaration".to_string())
        .with_help(format!(
            "dot-prefixed names declare local labels; add the trailing ':' (e.g. `{mnemonic}:`). Without the ':' the parser treats it as an instruction mnemonic, peephole sees it as unreachable code after the synthetic branch, and silently drops it."
        ))
        .with_note(
            "K65 local labels start with '.' and end with ':'. After constructs like `} always` the peephole pass prunes anything before the next reachable boundary, so the missing colon never reaches the encoder where the normal 'unknown mnemonic' enrichment would fire.".to_string(),
        )
    } else {
        Diagnostic::warning(
            span,
            "unreachable instruction dropped after unconditional branch".to_string(),
        )
        .with_primary_label("unreachable instruction".to_string())
        .with_help(
            "remove this instruction or move it before the preceding `bra`/`jmp`/`brl`/`jml` — the branch jumps unconditionally to a compiler-generated label, so control never falls through to here".to_string(),
        )
        .with_note(
            "Constructs like `} always`, `break`, and unconditional `goto` lower to a synthetic `bra .__k816_…` whose successor block is reachable only via the branch target. Anything sitting between the branch and the next label (or segment / function boundary) is dead and is removed before encoding.".to_string(),
        )
    }
}

fn immediate_register_load(inst: &InstructionOp) -> Option<(CpuReg, i64)> {
    let value = match inst.operand {
        Some(OperandOp::Immediate(value)) => value,
        _ => return None,
    };
    let reg = match canonical_mnemonic(&inst.mnemonic).as_str() {
        "lda" => CpuReg::A,
        "ldx" => CpuReg::X,
        "ldy" => CpuReg::Y,
        _ => return None,
    };
    Some((reg, value))
}

fn is_flag_preserving_store(inst: &InstructionOp) -> bool {
    if !matches!(inst.operand, Some(OperandOp::Address { .. })) {
        return false;
    }
    matches!(
        canonical_mnemonic(&inst.mnemonic).as_str(),
        "sta" | "stx" | "sty" | "stz"
    )
}

fn dead_adjacent_register_write(prev: &Op, next: &Op) -> bool {
    let (Op::Instruction(prev), Op::Instruction(next)) = (prev, next) else {
        return false;
    };
    let Some(prev_reg) = register_only_nz_write(prev) else {
        return false;
    };
    let Some(next_reg) = register_only_nz_overwrite(next) else {
        return false;
    };
    prev_reg == next_reg
}

fn register_only_nz_write(inst: &InstructionOp) -> Option<CpuReg> {
    if flags_written_by_instruction(inst).intersects(
        FlagSet::C | FlagSet::V | FlagSet::D | FlagSet::I | FlagSet::M_WIDTH | FlagSet::X_WIDTH,
    ) {
        return None;
    }
    if let Some((reg, _)) = immediate_register_load(inst) {
        return Some(reg);
    }
    match (canonical_mnemonic(&inst.mnemonic).as_str(), &inst.operand) {
        ("tax" | "tsx" | "tyx" | "inx" | "dex", None) => Some(CpuReg::X),
        ("tay" | "txy" | "iny" | "dey", None) => Some(CpuReg::Y),
        ("txa" | "tya" | "tsc" | "tdc", None) => Some(CpuReg::A),
        _ => None,
    }
}

fn register_only_nz_overwrite(inst: &InstructionOp) -> Option<CpuReg> {
    if flags_written_by_instruction(inst).intersects(
        FlagSet::C | FlagSet::V | FlagSet::D | FlagSet::I | FlagSet::M_WIDTH | FlagSet::X_WIDTH,
    ) {
        return None;
    }
    if let Some((reg, _)) = immediate_register_load(inst) {
        return Some(reg);
    }
    match (canonical_mnemonic(&inst.mnemonic).as_str(), &inst.operand) {
        ("tax" | "tsx" | "tyx", None) => Some(CpuReg::X),
        ("tay" | "txy", None) => Some(CpuReg::Y),
        ("txa" | "tya" | "tsc" | "tdc", None) => Some(CpuReg::A),
        _ => None,
    }
}

fn flags_written_by_instruction(inst: &InstructionOp) -> FlagSet {
    match (canonical_mnemonic(&inst.mnemonic).as_str(), &inst.operand) {
        ("clc" | "sec", None) => FlagSet::C,
        ("cld" | "sed", None) => FlagSet::D,
        ("cli" | "sei", None) => FlagSet::I,
        ("clv", None) => FlagSet::V,
        ("rep" | "sep", Some(OperandOp::Immediate(mask))) => {
            let mut flags = FlagSet::NONE;
            if mask & 0x80 != 0 {
                flags = flags | FlagSet::N;
            }
            if mask & 0x40 != 0 {
                flags = flags | FlagSet::V;
            }
            if mask & 0x20 != 0 {
                flags = flags | FlagSet::M_WIDTH;
            }
            if mask & 0x10 != 0 {
                flags = flags | FlagSet::X_WIDTH;
            }
            if mask & 0x08 != 0 {
                flags = flags | FlagSet::D;
            }
            if mask & 0x04 != 0 {
                flags = flags | FlagSet::I;
            }
            if mask & 0x02 != 0 {
                flags = flags | FlagSet::Z;
            }
            if mask & 0x01 != 0 {
                flags = flags | FlagSet::C;
            }
            flags
        }
        ("adc" | "cmp" | "cpx" | "cpy" | "sbc" | "asl" | "lsr" | "rol" | "ror", _) => {
            FlagSet::N | FlagSet::Z | FlagSet::C | FlagSet::V
        }
        (
            "and" | "bit" | "dec" | "dex" | "dey" | "eor" | "inc" | "inx" | "iny" | "lda" | "ldx"
            | "ldy" | "ora" | "pla" | "plx" | "ply" | "tax" | "tay" | "tdc" | "tsc" | "tsx" | "txa"
            | "txy" | "tya" | "tyx" | "xba",
            _,
        ) => FlagSet::N | FlagSet::Z,
        ("trb" | "tsb", _) => FlagSet::Z,
        _ => FlagSet::NONE,
    }
}

fn duplicate_flag_op_mnemonic(mnemonic: &str) -> Option<&'static str> {
    match canonical_mnemonic(mnemonic).as_str() {
        "clc" => Some("clc"),
        "sec" => Some("sec"),
        "cld" => Some("cld"),
        "sed" => Some("sed"),
        "cli" => Some("cli"),
        "sei" => Some("sei"),
        "clv" => Some("clv"),
        _ => None,
    }
}

fn tail_call_replacement(call: &Op, ret: &Op) -> Option<&'static str> {
    let (
        Op::Instruction(InstructionOp {
            mnemonic: call_mnemonic,
            operand: Some(_),
        }),
        Op::Instruction(InstructionOp {
            mnemonic: ret_mnemonic,
            operand: None,
        }),
    ) = (call, ret)
    else {
        return None;
    };
    match (
        canonical_mnemonic(call_mnemonic).as_str(),
        canonical_mnemonic(ret_mnemonic).as_str(),
    ) {
        ("jsr", "rts") => Some("jmp"),
        ("jsl", "rtl") => Some("jml"),
        _ => None,
    }
}

fn branch_or_jump_direct_label(op: &Op) -> Option<&str> {
    let Op::Instruction(inst) = op else {
        return None;
    };
    let m = canonical_mnemonic(&inst.mnemonic);
    if !is_branch_mnemonic(&m) && !matches!(m.as_str(), "jmp" | "jml") {
        return None;
    }
    let target = direct_label_operand(inst.operand.as_ref())?;
    is_synthetic_label(target).then_some(target)
}

fn direct_label_operand(operand: Option<&OperandOp>) -> Option<&str> {
    match operand {
        Some(OperandOp::Address {
            value: AddressValue::Label(label),
            mode: AddressOperandMode::Direct { index: None },
            ..
        }) => Some(label.as_str()),
        _ => None,
    }
}

fn next_label_matches(ops: &[Spanned<Op>], start: usize, target: &str) -> bool {
    for op in &ops[start..] {
        match &op.node {
            Op::Label(label) if label == target => return true,
            Op::Label(_) => continue,
            _ => return false,
        }
    }
    false
}

fn is_branch_mnemonic(mnemonic: &str) -> bool {
    matches!(
        mnemonic,
        "bcc" | "bcs" | "beq" | "bmi" | "bne" | "bpl" | "bra" | "brl" | "bvc" | "bvs"
    )
}

fn is_call_mnemonic(mnemonic: &str) -> bool {
    matches!(mnemonic, "jsr" | "jsl")
}

fn is_synthetic_label(label: &str) -> bool {
    label.contains(".__k816_")
}

fn is_unconditional_control_transfer(op: &Op) -> bool {
    let Op::Instruction(InstructionOp {
        mnemonic,
        operand: _,
    }) = op
    else {
        return false;
    };
    matches!(
        canonical_mnemonic(mnemonic).as_str(),
        "rts" | "rtl" | "rti" | "jmp" | "jml" | "bra" | "brl"
    )
}

fn is_unconditional_prune_transfer(op: &Op) -> bool {
    let Op::Instruction(inst) = op else {
        return false;
    };
    let Some(target) = direct_label_operand(inst.operand.as_ref()) else {
        return false;
    };
    if !is_synthetic_label(target) {
        return false;
    }
    matches!(
        canonical_mnemonic(&inst.mnemonic).as_str(),
        "jmp" | "jml" | "bra" | "brl"
    )
}

fn is_basic_block_barrier(op: &Op) -> bool {
    match op {
        Op::SelectSegment(_)
        | Op::FunctionStart { .. }
        | Op::FunctionEnd
        | Op::Label(_)
        | Op::EmitBytes(_)
        | Op::EmitRelocBytes { .. }
        | Op::Align { .. }
        | Op::Address(_)
        | Op::Nocross(_)
        | Op::Rep { .. }
        | Op::Sep { .. }
        | Op::DefineAbsoluteSymbol { .. }
        | Op::DefineSectionSymbol { .. }
        | Op::DefineDpFixedSymbol { .. }
        | Op::DefineDpAllocSymbol { .. }
        | Op::DefineDpAllocAlias { .. }
        | Op::SetMode(_) => true,
        Op::Instruction(inst) => {
            let m = canonical_mnemonic(&inst.mnemonic);
            is_call_mnemonic(&m) || is_branch_mnemonic(&m) || is_unconditional_control_transfer(op)
        }
    }
}

fn starts_reachable_region(op: &Op) -> bool {
    matches!(
        op,
        Op::SelectSegment(_)
            | Op::FunctionStart { .. }
            | Op::FunctionEnd
            | Op::Label(_)
            | Op::Align { .. }
            | Op::Address(_)
            | Op::Nocross(_)
            | Op::Rep { .. }
            | Op::Sep { .. }
            | Op::DefineAbsoluteSymbol { .. }
            | Op::DefineSectionSymbol { .. }
            | Op::SetMode(_)
    )
}

#[cfg(test)]
fn operands_equal(left: Option<&OperandOp>, right: Option<&OperandOp>) -> bool {
    match (left, right) {
        (None, None) => true,
        (Some(left), Some(right)) => operand_equal(left, right),
        _ => false,
    }
}

#[cfg(test)]
fn operand_equal(left: &OperandOp, right: &OperandOp) -> bool {
    match (left, right) {
        (OperandOp::Immediate(left), OperandOp::Immediate(right)) => left == right,
        (
            OperandOp::ImmediateByteRelocation {
                kind: left_kind,
                label: left_label,
            },
            OperandOp::ImmediateByteRelocation {
                kind: right_kind,
                label: right_label,
            },
        ) => left_kind == right_kind && left_label == right_label,
        (
            OperandOp::ImmediateWordRelocation {
                label: left_label,
                addend: left_addend,
                ..
            },
            OperandOp::ImmediateWordRelocation {
                label: right_label,
                addend: right_addend,
                ..
            },
        ) => left_label == right_label && left_addend == right_addend,
        (
            OperandOp::ImmediateFarRelocation {
                label: left_label,
                addend: left_addend,
                ..
            },
            OperandOp::ImmediateFarRelocation {
                label: right_label,
                addend: right_addend,
                ..
            },
        ) => left_label == right_label && left_addend == right_addend,
        (
            OperandOp::Address {
                value: left_value,
                size_hint: left_hint,
                mode: left_mode,
            },
            OperandOp::Address {
                value: right_value,
                size_hint: right_hint,
                mode: right_mode,
            },
        ) => {
            left_hint == right_hint
                && left_mode == right_mode
                && address_value_equal(left_value, right_value)
        }
        (
            OperandOp::BlockMove {
                src: left_src,
                dst: left_dst,
            },
            OperandOp::BlockMove {
                src: right_src,
                dst: right_dst,
            },
        ) => left_src == right_src && left_dst == right_dst,
        _ => false,
    }
}

#[cfg(test)]
fn address_value_equal(left: &AddressValue, right: &AddressValue) -> bool {
    match (left, right) {
        (AddressValue::Literal(left), AddressValue::Literal(right)) => left == right,
        (AddressValue::Label(left), AddressValue::Label(right)) => left == right,
        (
            AddressValue::LabelOffset {
                label: left_label,
                addend: left_addend,
            },
            AddressValue::LabelOffset {
                label: right_label,
                addend: right_addend,
            },
        ) => left_label == right_label && left_addend == right_addend,
        _ => false,
    }
}

fn collapse_duplicate_returns(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut last_return: Option<&'static str> = None;

    for op in ops {
        match &op.node {
            Op::Instruction(InstructionOp {
                mnemonic,
                operand: None,
            }) => {
                let Some(return_mnemonic) = return_mnemonic(mnemonic) else {
                    last_return = None;
                    out.push(op.clone());
                    continue;
                };
                if last_return == Some(return_mnemonic) {
                    continue;
                }
                last_return = Some(return_mnemonic);
                out.push(op.clone());
            }
            _ => {
                last_return = None;
                out.push(op.clone());
            }
        }
    }

    out
}

/// Rewrite dead `LDA #0` followed by one or more `STA mem[,X]` into `STZ`.
///
/// STZ supports direct-page, direct-page,X, absolute, and absolute,X addressing
/// modes (but not absolute-long or any indirect/Y form). This pass only
/// introduces STZ when it can also remove the corresponding accumulator load
/// and the status flags set by that load cannot be observed.
fn stz_rewrite(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut i = 0;
    while i < ops.len() {
        if !is_lda_zero(&ops[i].node) {
            out.push(ops[i].clone());
            i += 1;
            continue;
        }

        // Collect the contiguous run of rewrite-eligible `STA` ops.
        let mut run_end = i + 1;
        let mut any_rewrite = false;
        while run_end < ops.len() {
            if let Op::Instruction(inst) = &ops[run_end].node
                && inst.mnemonic.eq_ignore_ascii_case("sta")
                && stz_compatible(inst.operand.as_ref())
            {
                run_end += 1;
                any_rewrite = true;
                continue;
            }
            break;
        }

        if !any_rewrite {
            // No STA-to-STZ rewrite possible; emit the LDA unchanged.
            out.push(ops[i].clone());
            i += 1;
            continue;
        }

        // Determine whether the zero load is dead immediately after the run.
        // Dropping `lda #0` is safe only if the next instruction overwrites A
        // without reading it and refreshes the N/Z flags that `lda #0` would
        // have set. Otherwise a following branch or flag shorthand could
        // observe different processor state.
        let zero_load_dead_after = ops
            .get(run_end)
            .map(|next| a_zero_load_dead_at(&next.node))
            .unwrap_or(false);

        if zero_load_dead_after {
            // STZ is a shorter zero-store, but it no longer shows the store as
            // coming from A. Only perform that substitution when the original
            // zero load into A is dead and can be removed at the same time.
            // When A remains live, keeping the original STA sequence preserves
            // the programmer's expressed data flow in generated listings and
            // avoids turning a non-size-changing rewrite into a cosmetic one.
            for op in &ops[i + 1..run_end] {
                let mut stz_op = op.clone();
                if let Op::Instruction(inst) = &mut stz_op.node {
                    inst.mnemonic = "stz".to_string();
                }
                out.push(stz_op);
            }
        } else {
            out.extend(ops[i..run_end].iter().cloned());
        }
        i = run_end;
    }
    out
}

fn is_lda_zero(op: &Op) -> bool {
    matches!(
        op,
        Op::Instruction(InstructionOp {
            mnemonic,
            operand: Some(OperandOp::Immediate(0)),
        }) if mnemonic.eq_ignore_ascii_case("lda")
    )
}

fn stz_compatible(operand: Option<&OperandOp>) -> bool {
    let Some(OperandOp::Address {
        size_hint, mode, ..
    }) = operand
    else {
        return false;
    };
    if matches!(size_hint, AddressSizeHint::ForceAbsoluteLong) {
        return false;
    }
    matches!(
        mode,
        AddressOperandMode::Direct {
            index: None | Some(IndexRegister::X),
        }
    )
}

/// Returns `true` if `op` definitely kills both `A = 0` and the N/Z flags from
/// the removed `LDA #0`.
fn a_zero_load_dead_at(op: &Op) -> bool {
    let Op::Instruction(inst) = op else {
        return false;
    };
    let on_accumulator = inst.operand.is_none();
    let effects = mnemonic_effects(&inst.mnemonic, on_accumulator);
    let overwrites_a = effects.modifies.contains(RegSet::A) && !effects.reads.contains(RegSet::A);
    overwrites_a && refreshes_nz_after_a_overwrite(&inst.mnemonic)
}

fn refreshes_nz_after_a_overwrite(mnemonic: &str) -> bool {
    matches!(
        mnemonic.to_ascii_lowercase().as_str(),
        "lda" | "pla" | "txa" | "tya" | "tsc" | "tdc"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::AddressValue;
    use crate::span::{SourceId, Span, Spanned};

    fn op(node: Op) -> Spanned<Op> {
        Spanned::new(node, Span::new(SourceId(0), 0, 0))
    }

    fn instr(mnemonic: &str, operand: Option<OperandOp>) -> Spanned<Op> {
        op(Op::Instruction(InstructionOp {
            mnemonic: mnemonic.to_string(),
            operand,
        }))
    }

    fn sta_dp(addr: u32) -> Spanned<Op> {
        instr(
            "sta",
            Some(OperandOp::Address {
                value: AddressValue::Literal(addr),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::Direct { index: None },
            }),
        )
    }

    fn addr_dp(addr: u32) -> OperandOp {
        OperandOp::Address {
            value: AddressValue::Literal(addr),
            size_hint: AddressSizeHint::Auto,
            mode: AddressOperandMode::Direct { index: None },
        }
    }

    fn branch_target(label: &str) -> OperandOp {
        OperandOp::Address {
            value: AddressValue::Label(label.to_string()),
            size_hint: AddressSizeHint::Auto,
            mode: AddressOperandMode::Direct { index: None },
        }
    }

    fn label(name: &str) -> Spanned<Op> {
        op(Op::Label(name.to_string()))
    }

    fn sta_absolute_long(addr: u32) -> Spanned<Op> {
        instr(
            "sta",
            Some(OperandOp::Address {
                value: AddressValue::Literal(addr),
                size_hint: AddressSizeHint::ForceAbsoluteLong,
                mode: AddressOperandMode::Direct { index: None },
            }),
        )
    }

    fn mnemonics(program: &Program) -> Vec<&str> {
        program
            .ops
            .iter()
            .filter_map(|sp| match &sp.node {
                Op::Instruction(inst) => Some(inst.mnemonic.as_str()),
                _ => None,
            })
            .collect()
    }

    /// `tdc` overwrites A without reading it and refreshes N/Z, so the
    /// preceding `lda #0` is dead and gets dropped, leaving just the `stz`.
    #[test]
    fn drops_lda_zero_before_tdc_flag_refresh() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("tdc", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["stz", "tdc"]);
    }

    #[test]
    fn drops_lda_zero_before_tdc_even_with_later_branch() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("tdc", None),
                instr("bne", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["stz", "tdc", "bne"]);
    }

    /// `xba` reads *and* modifies A (it byte-swaps), so it does NOT make A
    /// dead. The leading `lda #0` must be preserved, including its flags.
    #[test]
    fn keeps_lda_zero_before_xba() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("xba", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["lda", "sta", "xba"]);
    }

    #[test]
    fn keeps_lda_zero_before_direct_flag_branch() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("beq", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["lda", "sta", "beq"]);
    }

    #[test]
    fn uppercase_lda_zero_sta_rewrites_when_a_is_dead() {
        let program = Program {
            ops: vec![
                instr("LDA", Some(OperandOp::Immediate(0))),
                instr(
                    "STA",
                    Some(OperandOp::Address {
                        value: AddressValue::Literal(0x10),
                        size_hint: AddressSizeHint::Auto,
                        mode: AddressOperandMode::Direct { index: None },
                    }),
                ),
                instr("LDA", Some(OperandOp::Immediate(1))),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["stz", "LDA"]);
    }

    #[test]
    fn incompatible_sta_preserves_original_sequence() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_absolute_long(0x12_3456),
                instr("lda", Some(OperandOp::Immediate(1))),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["lda", "sta", "lda"]);
    }

    #[test]
    fn collapses_adjacent_duplicate_returns() {
        let program = Program {
            ops: vec![
                instr("rts", None),
                instr("RTS", None),
                label("main::.far_return"),
                instr("rtl", None),
                instr("RTL", None),
                label("main::.interrupt_return"),
                instr("rti", None),
                instr("RTI", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["rts", "rtl", "rti"]);
    }

    #[test]
    fn keeps_returns_at_separate_label_entry_points() {
        let program = Program {
            ops: vec![
                instr("rts", None),
                label("main::.far_return"),
                instr("rtl", None),
                label("main::.interrupt_return"),
                instr("rti", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["rts", "rtl", "rti"]);
    }

    #[test]
    fn coalesces_repeated_immediate_loads_before_stores() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x10),
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x11),
                instr("lda", Some(OperandOp::Immediate(1))),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["lda", "sta", "sta", "lda"]);
    }

    #[test]
    fn coalesced_zero_store_run_feeds_stz_rewrite() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x11),
                instr("lda", Some(OperandOp::Immediate(1))),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["stz", "stz", "lda"]);
    }

    #[test]
    fn repeated_load_coalescing_does_not_cross_barriers() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x10),
                label("main::.barrier"),
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x11),
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x12),
                instr("jsr", Some(branch_target("helper"))),
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x13),
                instr("pha", None),
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x14),
                instr("lda", Some(addr_dp(0x20))),
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x15),
                op(Op::Sep {
                    mask: 0x20,
                    fixed: false,
                }),
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x16),
                op(Op::EmitBytes(vec![0xEA])),
                instr("lda", Some(OperandOp::Immediate(7))),
                sta_dp(0x17),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(
            mnemonics(&out),
            vec![
                "lda", "sta", "lda", "sta", "sta", "jsr", "lda", "sta", "pha", "lda", "sta", "lda",
                "lda", "sta", "lda", "sta", "lda", "sta"
            ]
        );
    }

    #[test]
    fn removes_dead_adjacent_register_writes() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(1))),
                instr("lda", Some(OperandOp::Immediate(2))),
                instr("tax", None),
                instr("ldx", Some(OperandOp::Immediate(4))),
                instr("inx", None),
                instr("ldx", Some(OperandOp::Immediate(5))),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["lda", "ldx", "ldx"]);
    }

    #[test]
    fn collapses_dead_register_writes_across_distinct_source_spans() {
        // Each ldy comes from its own source line, so the spans differ. The
        // overwrites are still safe (immediate loads do not read Y), so all
        // but the last load should be dropped.
        let mut ops: Vec<Spanned<Op>> = Vec::new();
        for (offset, value) in [(10usize, 1i64), (20, 2), (30, 3), (40, 5)] {
            ops.push(Spanned::new(
                Op::Instruction(InstructionOp {
                    mnemonic: "ldy".to_string(),
                    operand: Some(OperandOp::Immediate(value)),
                }),
                Span::new(SourceId(0), offset, offset + 1),
            ));
        }
        let program = Program { ops };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["ldy"]);
        let Op::Instruction(only) = &out.ops[0].node else {
            panic!("expected single ldy");
        };
        assert!(matches!(only.operand, Some(OperandOp::Immediate(5))));
    }

    #[test]
    fn keeps_side_effecting_dead_write_candidates() {
        let program = Program {
            ops: vec![
                instr("lda", Some(addr_dp(0x20))),
                instr("lda", Some(OperandOp::Immediate(2))),
                instr("pla", None),
                instr("lda", Some(OperandOp::Immediate(3))),
                sta_dp(0x10),
                instr("lda", Some(OperandOp::Immediate(4))),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(
            mnemonics(&out),
            vec!["lda", "lda", "pla", "lda", "sta", "lda"]
        );
    }

    #[test]
    fn collapses_adjacent_duplicate_flag_ops_only() {
        let program = Program {
            ops: vec![
                instr("clc", None),
                instr("CLC", None),
                instr("sec", None),
                instr("sec", None),
                instr("cld", None),
                instr("cld", None),
                instr("sed", None),
                instr("sed", None),
                instr("cli", None),
                instr("cli", None),
                instr("sei", None),
                instr("sei", None),
                instr("clv", None),
                instr("clv", None),
                instr("clc", None),
                instr("sec", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(
            mnemonics(&out),
            vec![
                "clc", "sec", "cld", "sed", "cli", "sei", "clv", "clc", "sec"
            ]
        );
    }

    #[test]
    fn rewrites_adjacent_tail_calls() {
        let program = Program {
            ops: vec![
                instr("jsr", Some(branch_target("near_target"))),
                instr("rts", None),
                label("main::.far_tail"),
                instr("jsl", Some(branch_target("far_target"))),
                instr("rtl", None),
                label("main::.mixed_tail"),
                instr("jsr", Some(branch_target("mixed_target"))),
                instr("rtl", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["jmp", "jml", "jsr", "rtl"]);
        let Op::Instruction(first) = &out.ops[0].node else {
            panic!("expected first op to be rewritten tail call");
        };
        assert!(operands_equal(
            first.operand.as_ref(),
            Some(&branch_target("near_target"))
        ));
    }

    #[test]
    fn removes_branch_or_jump_to_next_label_only() {
        let program = Program {
            ops: vec![
                instr("bra", Some(branch_target("main::.__k816_next_0"))),
                label("main::.__k816_next_0"),
                instr("beq", Some(branch_target("main::.__k816_also_next_1"))),
                label("main::.__k816_also_next_1"),
                instr("jmp", Some(branch_target("main::.__k816_jump_next_2"))),
                label("main::.__k816_jump_next_2"),
                instr("bne", Some(branch_target("main::.not_next"))),
                label("main::.not_next"),
                instr("nop", None),
            ],
        };
        let out = peephole_optimize(&program).program;
        assert_eq!(mnemonics(&out), vec!["bne", "nop"]);
    }

    #[test]
    fn removes_branch_to_next_label_after_pruning_intervening_branch() {
        // Mirrors the lowering of `break; } always`: an unconditional bra to
        // a synthetic break label, followed by another unconditional bra to
        // a synthetic loop label, followed by the break label itself. The
        // second bra is unreachable and must be pruned first so that the
        // first bra becomes a branch-to-next-label and is removed.
        let program = Program {
            ops: vec![
                instr("bra", Some(branch_target("main::.__k816_break_0"))),
                instr("bra", Some(branch_target("main::.__k816_loop_1"))),
                label("main::.__k816_break_0"),
                instr("nop", None),
            ],
        };
        let output = peephole_optimize(&program);
        assert_eq!(mnemonics(&output.program), vec!["nop"]);
        assert!(
            matches!(output.program.ops[0].node, Op::Label(ref l) if l == "main::.__k816_break_0")
        );
        assert_eq!(
            output.warnings.len(),
            1,
            "exactly one pruned `bra` should produce one warning"
        );
        assert!(
            output.warnings[0]
                .message
                .contains("unreachable instruction dropped after unconditional branch"),
            "expected generic warning, got: {:?}",
            output.warnings[0].message
        );
    }

    #[test]
    fn prunes_unreachable_plain_instructions_but_keeps_emitted_data() {
        let program = Program {
            ops: vec![
                instr("jmp", Some(branch_target("main::.__k816_live_0"))),
                instr("lda", Some(OperandOp::Immediate(1))),
                op(Op::EmitBytes(vec![0xAA])),
                instr("sta", Some(addr_dp(0x10))),
                label("main::.__k816_live_0"),
                instr("lda", Some(OperandOp::Immediate(2))),
            ],
        };
        let output = peephole_optimize(&program);
        assert_eq!(mnemonics(&output.program), vec!["jmp", "lda"]);
        assert!(matches!(output.program.ops[1].node, Op::EmitBytes(_)));
        assert_eq!(
            output.warnings.len(),
            2,
            "two pruned instructions (`lda #1`, `sta`) should produce two warnings"
        );
        for warning in &output.warnings {
            assert_eq!(warning.primary_label, "unreachable instruction");
        }
    }

    #[test]
    fn warns_with_label_typo_help_when_dotted_mnemonic_pruned() {
        // Reproduces the typical typo: `.exit` (no trailing ':') after `} always`
        // becomes `Op::Instruction { mnemonic: ".exit", operand: None }` and is
        // pruned. The warning must use the label-declaration flavour so users
        // see the same advice as the encoder's UnknownMnemonic enrichment.
        let program = Program {
            ops: vec![
                instr("bra", Some(branch_target("main::.__k816_loop_0"))),
                instr(".exit", None),
            ],
        };
        let output = peephole_optimize(&program);
        assert_eq!(output.warnings.len(), 1);
        let warning = &output.warnings[0];
        assert!(
            warning.message.contains("missing ':'"),
            "expected label-declaration help in message, got: {:?}",
            warning.message
        );
        assert_eq!(warning.primary_label, "label declaration");
    }
}
