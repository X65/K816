/*****************************************************************************/
/*                                                                           */
/*                                  lower.rs                                 */
/*                                                                           */
/*             Lowering native and HLA AST forms to common KOps              */
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

//! Lowering: native and HLA statements converge into the same typed [`KOp`]
//! stream. After this point no stage may care which syntax produced an
//! operation.
//!
//! The entry point `main` receives explicit entry-mode setup ops for its
//! declared contract (`sep #$20` / `rep #$10` for `@a8 @i16`), emitted for
//! both source forms so native/HLA equivalence covers them too. Whether
//! non-`main` entry contracts emit or assume is decided in sub-plan D.

use crate::ast::{AstFile, ItemKind, ReturnKind, StatementKind};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::effects::{ControlEffect, Effects, RegisterSet, WidthEffect};
use crate::hla;
use crate::isa::{AddressEncoding, AddressingMode, ImmediateWidthClass, Mnemonic, OperandWidth};
use crate::op::{
    AnchorId, AxisState, Instruction, KFunction, KOp, KOpKind, KProgram, ModeState, Operand,
    Origin, Reason, RegisterWidth, SegmentName, WidthContract,
};
use crate::resolve;
use crate::resolve::{AnonymousBlocks, Resolver};
use crate::semantic::SemanticModel;
use crate::source::Span;
use crate::symbol::SymbolId;
use crate::value::KExpr;
use crate::width::{Bridge, BridgeAnchor, P_MASK_M, P_MASK_X};

/// Lowers every analyzed function of `file` into [`KOp`] streams.
///
/// Call after error-free analysis. Constructs the analysis already rejected
/// are skipped with an internal diagnostic rather than a panic.
pub fn lower(file: &AstFile, model: &SemanticModel) -> (KProgram, Vec<Diagnostic>) {
    lower_unit(file, model, "")
}

/// Lowers `file`, naming the unit so `address` placements derive the same
/// synthetic segment names analysis reported in `W0116`.
pub fn lower_unit(
    file: &AstFile,
    model: &SemanticModel,
    unit: &str,
) -> (KProgram, Vec<Diagnostic>) {
    let mut program = KProgram::default();
    let mut diagnostics = Vec::new();
    let mut anonymous = AnonymousBlocks::default();
    let mut checked_calls: Vec<(usize, Vec<CheckedCall>)> = Vec::new();
    // Unit-level operations and functions interleave in source order,
    // because a `segment` selection is lexical state: which segment a
    // function lands in is decided by what precedes it in the file.
    for (index, item) in file.items.iter().enumerate() {
        if let ItemKind::Segment(name) = &item.kind {
            program.push_op(KOp::new(
                KOpKind::Segment(SegmentName::new(&name.text)),
                item.span,
                Origin::User,
            ));
            continue;
        }
        if let ItemKind::Data(data) = &item.kind {
            let site = resolve::data_site(&mut anonymous, unit, data, None);
            let mut resolver = Resolver::new(&model.symbols);
            for op in resolver.data_block(data, item.span, &site) {
                program.push_op(op);
            }
            continue;
        }
        let Some(info) = model.functions.iter().find(|info| info.item_index == index) else {
            continue;
        };
        let ItemKind::Func(func) = &item.kind else {
            continue;
        };

        let mut lowering = Lowering {
            model,
            function: Some(info.symbol),
            function_name: func.name.text.clone(),
            unit: unit.to_owned(),
            anonymous: &mut anonymous,
            widths: model.width(info.symbol),
            ops: Vec::new(),
            checked_calls: Vec::new(),
            diagnostics: Vec::new(),
            anchors: 0,
            loops: Vec::new(),
        };

        lowering.push(KOpKind::Label(info.symbol), func.name.span, Origin::User);
        lowering.entry(info.is_entry_point, info.contract, func.name.span);
        lowering.block(&func.body);
        lowering.implicit_return(func.body.span);
        lowering.relax_branches();

        checked_calls.push((program.functions().count(), lowering.checked_calls));
        program.push_function(KFunction {
            symbol: info.symbol,
            contract: info.contract,
            exit: info.exit,
            ops: lowering.ops,
        });
        diagnostics.append(&mut lowering.diagnostics);
    }
    diagnostics.extend(report_damaged_registers(&program, model, &checked_calls));
    (program, diagnostics)
}

/// The branch that is taken exactly when `mnemonic` is not.
///
/// Only the conditional branches have one: `BRA` is always taken, so the long
/// form of an unconditional branch is the jump alone.
fn inverse_branch(mnemonic: Mnemonic) -> Option<Mnemonic> {
    Some(match mnemonic {
        Mnemonic::Bcc => Mnemonic::Bcs,
        Mnemonic::Bcs => Mnemonic::Bcc,
        Mnemonic::Beq => Mnemonic::Bne,
        Mnemonic::Bne => Mnemonic::Beq,
        Mnemonic::Bmi => Mnemonic::Bpl,
        Mnemonic::Bpl => Mnemonic::Bmi,
        Mnemonic::Bvc => Mnemonic::Bvs,
        Mnemonic::Bvs => Mnemonic::Bvc,
        _ => return None,
    })
}

/// The bytes an operation encodes, over-estimated where the answer depends on
/// something K816 does not know yet.
///
/// Over-estimating is the safe direction for every consumer: a branch that
/// thinks the distance is longer than it is emits a longer form and still
/// assembles, and a `nocross` run that thinks it is bigger than it is reports
/// a fit problem that does not exist rather than missing one that does. An
/// under-estimate does neither (sub-plan E, decision 7).
fn byte_length(op: &KOp) -> u32 {
    let KOpKind::Instruction(instruction) = &op.kind else {
        return match &op.kind {
            KOpKind::Data(data) => data_length(data),
            // Labels, anchors, width claims, and directives emit no bytes.
            _ => 0,
        };
    };
    let Some(spec) = instruction.spec() else {
        return 0;
    };
    1 + match spec.operand.width {
        OperandWidth::None => 0,
        OperandWidth::Immediate(class) => match class {
            ImmediateWidthClass::Fixed8 => 1,
            ImmediateWidthClass::Fixed16 => 2,
            // The width may be unknown here, and the longer form is the one
            // that cannot make an estimate too small.
            ImmediateWidthClass::MDependent | ImmediateWidthClass::XDependent => 2,
        },
        OperandWidth::Address(encoding) => match encoding {
            AddressEncoding::Direct => 1,
            AddressEncoding::Absolute => 2,
            AddressEncoding::Long => 3,
        },
        OperandWidth::BankPair => 2,
        OperandWidth::Displacement => match instruction.mode {
            AddressingMode::RelativeLong => 2,
            _ => 1,
        },
    }
}

/// The bytes a data directive emits.
fn data_length(data: &crate::op::DataOp) -> u32 {
    use crate::op::DataOp;
    let count = |values: &[crate::value::KExpr]| u32::try_from(values.len()).unwrap_or(u32::MAX);
    match data {
        DataOp::Byte(values) => count(values),
        DataOp::Word(values) => 2 * count(values),
        DataOp::Far(values) => 3 * count(values),
        // A reservation whose size did not fold is not emittable, so the
        // only honest estimate is that it may be anything.
        DataOp::Reserve(bytes) => bytes.value().map_or(u32::MAX, |value| value.get()),
    }
}

/// Reports registers a checked call clobbers that its caller still reads.
///
/// Computed after lowering rather than over AST statements, so the reads and
/// writes come from the effect table uniformly: the check grows as more
/// statement forms lower, instead of needing a second effect model for HLA
/// syntax.
///
/// A *warning* rather than an error, because a callee's damage set is a
/// summary of its lowered body and is simply unavailable for a cross-unit or
/// recursive callee. Code that errors in a one-unit build and passes in a
/// two-unit build is worse than code that warns in one and is silent in the
/// other.
fn report_damaged_registers(
    program: &KProgram,
    model: &SemanticModel,
    checked_calls: &[(usize, Vec<CheckedCall>)],
) -> Vec<Diagnostic> {
    let functions: Vec<&KFunction> = program.functions().collect();
    // The damage set of each function: every register its lowered body
    // writes. A callee K816 did not lower has none, and is silent.
    let damage: std::collections::BTreeMap<SymbolId, RegisterSet> = functions
        .iter()
        .map(|function| {
            let written = function
                .ops
                .iter()
                .fold(RegisterSet::NONE, |set, op| match &op.kind {
                    KOpKind::Instruction(instruction) => {
                        set.union(instruction.effects().writes.registers)
                    }
                    _ => set,
                });
            // The stack pointer is not damage: the `JSR`/`RTS` pair moves it
            // and puts it back, so every subroutine writes it and naming that
            // would bury the registers a caller can actually lose.
            (function.symbol, written.without(RegisterSet::S))
        })
        .collect();

    let mut diagnostics = Vec::new();
    for (index, calls) in checked_calls {
        let Some(function) = functions.get(*index) else {
            continue;
        };
        for call in calls {
            let Some(damaged) = damage.get(&call.callee).copied() else {
                continue;
            };
            let declared = model
                .contracts
                .get(&call.callee)
                .map_or(RegisterSet::NONE, |flow| registers_of(&flow.outputs));
            // A register the contract hands back is not damage: the caller
            // asked for it.
            let at_risk = damaged.without(declared);
            if at_risk.is_empty() {
                continue;
            }
            if let Some(read) = first_live_read(&function.ops, call.op, at_risk) {
                diagnostics.push(clobbered_register(call.span, read, at_risk));
            }
        }
    }
    diagnostics
}

/// The span of the first operation after `call` in the same basic block that
/// reads one of `at_risk` before writing it again.
fn first_live_read(ops: &[KOp], call: usize, at_risk: RegisterSet) -> Option<Span> {
    let mut pending = at_risk;
    for op in ops.iter().skip(call + 1) {
        let KOpKind::Instruction(instruction) = &op.kind else {
            // A width claim emits no bytes and observes nothing; anything
            // else ends the block, and this scan is deliberately block-local.
            if matches!(op.kind, KOpKind::ModeAssumption(_)) {
                continue;
            }
            return None;
        };
        let effects = instruction.effects();
        if effects.reads.registers.intersects(pending) {
            return Some(op.span);
        }
        pending = pending.without(effects.writes.registers);
        if pending.is_empty() || effects.control != ControlEffect::FallThrough {
            return None;
        }
    }
    None
}

fn registers_of(registers: &[crate::ast::RegisterKind]) -> RegisterSet {
    use crate::ast::RegisterKind;
    registers
        .iter()
        .fold(RegisterSet::NONE, |set, register| match register {
            RegisterKind::A | RegisterKind::B | RegisterKind::C => set.union(RegisterSet::A),
            RegisterKind::X => set.union(RegisterSet::X),
            RegisterKind::Y => set.union(RegisterSet::Y),
            RegisterKind::S => set.union(RegisterSet::S),
            RegisterKind::D => set.union(RegisterSet::D),
            RegisterKind::P => set,
        })
}

fn clobbered_register(call: Span, read: Span, damaged: RegisterSet) -> Diagnostic {
    Diagnostic::warning(
        DiagnosticCode::CLOBBERED_REGISTER,
        format!("this call writes {damaged}, and the value is read again below"),
        Label::new(call, "the callee does not hand this register back"),
    )
    .with_secondary(Label::new(read, "read here, after the call wrote it"))
    .with_note(
        "the callee's contract names what it returns; anything else it writes is \
         damage the caller has to expect",
    )
    .with_help("save the value across the call, or declare the register as an output")
    .with_docs("doc/syntax-reference.md#code-sections")
}

struct Lowering<'a> {
    model: &'a SemanticModel,
    /// The function being lowered, which local labels are scoped to.
    function: Option<SymbolId>,
    /// Its name, which an inline data block's segment names derive from.
    function_name: String,
    unit: String,
    /// The per-site anonymous-block counters, shared with the unit walk so
    /// the names match the ones analysis reported.
    anonymous: &'a mut AnonymousBlocks,
    /// The width the model tracks at each statement. Lowering *looks up* the
    /// answer and never computes one, which is what keeps it from disagreeing
    /// with analysis about how wide an immediate is.
    widths: Option<&'a crate::width::WidthAnalysis>,
    ops: Vec<KOp>,
    /// Calls that stated a register flow, for the damage scan after lowering.
    checked_calls: Vec<CheckedCall>,
    diagnostics: Vec<Diagnostic>,
    /// How many branch targets this function has generated so far.
    anchors: u32,
    /// The loops enclosing the statement being lowered, innermost last, which
    /// is what `break` and `repeat` bind to.
    loops: Vec<LoopAnchors>,
}

/// The two targets a loop offers the statements inside it.
#[derive(Clone, Copy)]
struct LoopAnchors {
    /// Where `repeat` goes, and where a postfix condition branches back to.
    head: AnchorId,
    /// Where `break` goes: past the loop's back edge.
    exit: AnchorId,
}

/// A checked call, recorded while lowering so the block-local scan can find
/// its operation without matching by position afterwards.
struct CheckedCall {
    /// Index of the `jsr` within its function's operation stream.
    op: usize,
    callee: SymbolId,
    span: Span,
}

impl Lowering<'_> {
    /// Lowers every statement of a block, which is how a body nested in a
    /// conditional or a loop reaches the same per-statement machinery — its
    /// bridges, its width state, its origins — as a top-level one.
    fn block(&mut self, block: &crate::ast::Block) {
        for statement in &block.statements {
            self.statement(statement);
        }
    }

    /// A `nocross` run has to fit in a page, and an edge split can be what
    /// stops it fitting.
    ///
    /// The guard is evaluated *after* splitting, because that is when the run
    /// has its final size — which is the consequence decision 13 of sub-plan D
    /// named and review accepted along with the split itself
    /// (`postmortem.md` §D.6.2).
    fn check_nocross_fits(&mut self, start: usize, length: u32, span: Span) {
        if length <= 256 {
            return;
        }
        let split: u32 = self.ops[start..]
            .iter()
            .filter(|op| {
                matches!(op.reason, Some(Reason::SplitEdge | Reason::BlockRestore))
                    && op.origin != Origin::User
            })
            .map(byte_length)
            .sum();
        if split == 0 {
            // Nothing K816 added is to blame: the run was written too big,
            // which is the check sub-plan C already owns.
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::NOCROSS_TOO_LARGE,
                    format!("this `nocross` run is {length} bytes, which cannot fit a page"),
                    Label::new(span, "a page is 256 bytes"),
                )
                .with_docs("doc/syntax-reference.md#nocross-code-wrappers"),
            );
            return;
        }
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::NOCROSS_SPLIT_OVERFLOW,
                format!("this `nocross` run grew to {length} bytes and no longer fits a page"),
                Label::new(span, "a page is 256 bytes"),
            )
            .with_note(format!(
                "{split} of those bytes restore a width block a branch inside this run \
                 leaves; the restore has to go on the taken edge, so it lands here"
            ))
            .with_help(
                "move the width block or the branch out of the `nocross` run, or take the \
                 branch's target inside the block",
            )
            .with_docs("doc/register-width-aware-syntax.md#block-level-mode-scoping"),
        );
    }

    /// Gives a transfer that leaves a scoped width block somewhere to restore
    /// it: the taken edge, which lowering has to create.
    ///
    /// ```text
    ///     bcc .out          ->      bcc .pad
    ///                               jmp .past
    ///                       .pad:   sep #$20
    ///                               jmp .out
    ///                       .past:
    /// ```
    ///
    /// Every transfer the statement emitted whose target is *outside* what it
    /// emitted goes through the pad; a branch to a target the statement
    /// defines itself — the guard of a compound comparison — stays where it
    /// is, because it never leaves the block (sub-plan E, decision 9).
    fn split_taken_edges(&mut self, statement: &crate::ast::Statement, start: usize, span: Span) {
        let restores: Vec<Bridge> = self
            .widths
            .map(|analysis| {
                analysis
                    .bridges(statement.id)
                    .iter()
                    .filter(|bridge| bridge.anchor == BridgeAnchor::TakenEdge)
                    .copied()
                    .collect()
            })
            .unwrap_or_default();
        if restores.is_empty() {
            return;
        }
        // Targets the statement defines are internal to it; anything else
        // leaves.
        let internal: Vec<AnchorId> = self.ops[start..]
            .iter()
            .filter_map(|op| match op.kind {
                KOpKind::Anchor(anchor) => Some(anchor),
                _ => None,
            })
            .collect();
        let leaves = |operand: &Option<Operand>| match operand {
            Some(Operand::Anchor(anchor)) => !internal.contains(anchor),
            Some(Operand::Label(_)) => true,
            _ => false,
        };

        let mut outgoing: Vec<(usize, Option<Operand>)> = Vec::new();
        for (offset, op) in self.ops[start..].iter().enumerate() {
            let KOpKind::Instruction(instruction) = &op.kind else {
                continue;
            };
            if instruction.effects().control == ControlEffect::FallThrough {
                continue;
            }
            if leaves(&instruction.operand) {
                outgoing.push((start + offset, instruction.operand.clone()));
            }
        }
        if outgoing.is_empty() {
            return;
        }

        // One pad per distinct target: two branches of a compound comparison
        // leave through the same edge and share the restore.
        let mut pads: Vec<(Option<Operand>, AnchorId)> = Vec::new();
        for (index, target) in outgoing {
            let pad = match pads.iter().find(|(pad_target, _)| *pad_target == target) {
                Some((_, pad)) => *pad,
                None => {
                    let pad = self.anchor();
                    pads.push((target.clone(), pad));
                    pad
                }
            };
            let KOpKind::Instruction(instruction) = &mut self.ops[index].kind else {
                unreachable!("only instructions were collected");
            };
            instruction.operand = Some(Operand::Anchor(pad));
        }

        // The statement's own fall-through has to step over the pads, unless
        // the last thing it emitted leaves for good anyway.
        let past = self.anchor();
        let falls_through = self.ops.last().is_some_and(|op| match &op.kind {
            KOpKind::Instruction(instruction) => {
                instruction.effects().control != ControlEffect::Jump
            }
            _ => true,
        });
        if falls_through {
            self.jump_to(past, span, Some(Reason::SplitEdge));
        }
        for (target, pad) in pads {
            self.place(pad, span);
            for restore in &restores {
                let mnemonic = match restore.width {
                    RegisterWidth::W8 => Mnemonic::Sep,
                    RegisterWidth::W16 => Mnemonic::Rep,
                };
                self.generated_instruction(
                    mnemonic,
                    AddressingMode::Immediate,
                    Some(Operand::Immediate(KExpr::number(restore.mask))),
                    span,
                    restore.reason,
                );
            }
            self.expanded_instruction(
                Mnemonic::Jmp,
                AddressingMode::Absolute,
                target,
                span,
                Some(Reason::SplitEdge),
            );
        }
        self.place(past, span);
    }

    /// Rewrites every branch the short form cannot reach.
    ///
    /// ca65 does not relax branches — a `bcc` over 200 bytes is a range
    /// error, not a silently widened instruction — so the choice is K816's
    /// (sub-plan E, decision 7). The estimate over-counts wherever it is
    /// unsure, because an over-estimate costs a byte and always assembles
    /// while an under-estimate does not assemble at all.
    ///
    /// Each rewrite makes the function longer, which can put a second branch
    /// out of reach, so this runs to a fixed point. It terminates because a
    /// branch is only ever rewritten once: the long form contains no relative
    /// branch to the far target.
    fn relax_branches(&mut self) {
        while let Some(index) = self.first_unreachable_branch() {
            self.widen_branch(index);
        }
    }

    /// The first branch whose target the relative form cannot hold.
    fn first_unreachable_branch(&self) -> Option<usize> {
        let mut offsets = Vec::with_capacity(self.ops.len());
        let mut offset: i64 = 0;
        for op in &self.ops {
            offsets.push(offset);
            offset += i64::from(byte_length(op));
        }
        self.ops.iter().enumerate().find_map(|(index, op)| {
            let KOpKind::Instruction(instruction) = &op.kind else {
                return None;
            };
            if instruction.mode != AddressingMode::Relative {
                return None;
            }
            let Some(Operand::Anchor(anchor)) = &instruction.operand else {
                return None;
            };
            let target = self
                .ops
                .iter()
                .position(|op| op.kind == KOpKind::Anchor(*anchor))?;
            // A relative branch counts from the instruction after it.
            let from = offsets[index] + i64::from(byte_length(op));
            let distance = offsets[target] - from;
            (!(-128..=127).contains(&distance)).then_some(index)
        })
    }

    /// Replaces one branch with the inverted-branch-over-a-jump idiom.
    fn widen_branch(&mut self, index: usize) {
        let op = self.ops[index].clone();
        let KOpKind::Instruction(instruction) = &op.kind else {
            unreachable!("only an instruction is chosen for widening");
        };
        let Some(inverse) = inverse_branch(instruction.mnemonic) else {
            // An unconditional relative branch needs no guard: the jump goes
            // where it went. Nothing emits one today, so this is the shape
            // the rule takes rather than a path with a producer.
            self.ops[index] = KOp::new(
                KOpKind::Instruction(Instruction::new(
                    Mnemonic::Jmp,
                    AddressingMode::Absolute,
                    instruction.operand.clone(),
                )),
                op.span,
                op.origin,
            )
            .because(Reason::LongBranch);
            return;
        };
        let past = self.anchor();
        let target = instruction.operand.clone();
        let guard = KOp::new(
            KOpKind::Instruction(Instruction::new(
                inverse,
                AddressingMode::Relative,
                Some(Operand::Anchor(past)),
            )),
            op.span,
            op.origin,
        )
        .because(Reason::LongBranch);
        let jump = KOp::new(
            KOpKind::Instruction(Instruction::new(
                Mnemonic::Jmp,
                AddressingMode::Absolute,
                target,
            )),
            op.span,
            op.origin,
        )
        .because(Reason::LongBranch);
        let landing = KOp::new(KOpKind::Anchor(past), op.span, Origin::CompilerGenerated);
        self.ops.splice(index..=index, [guard, jump, landing]);
    }

    /// True when anything emitted since `start` branches to `anchor`.
    fn references(&self, anchor: AnchorId, start: usize) -> bool {
        self.ops[start..].iter().any(|op| {
            matches!(
                &op.kind,
                KOpKind::Instruction(Instruction {
                    operand: Some(Operand::Anchor(target)),
                    ..
                }) if *target == anchor
            )
        })
    }

    /// A fresh branch target for this function.
    fn anchor(&mut self) -> AnchorId {
        let id = AnchorId::new(self.anchors);
        self.anchors += 1;
        id
    }

    /// Marks where a generated branch lands.
    fn place(&mut self, anchor: AnchorId, span: Span) {
        self.push(KOpKind::Anchor(anchor), span, Origin::CompilerGenerated);
    }

    /// The unconditional jump a generated transfer uses.
    ///
    /// `JMP` rather than `BRA`, because `doc/syntax-reference.md` spells both
    /// documented cases that way — "`{ ... } always` // unconditional loop
    /// (JMP back to start)" and "`break` // unconditional exit (JMP forward)"
    /// — and preserving a documented expansion is worth a byte
    /// (`doc/k65-compat.md`). It also always reaches, so branch range never
    /// applies to it. Shrinking one to `BRA` is a peephole, and F's.
    fn jump_to(&mut self, anchor: AnchorId, span: Span, reason: Option<Reason>) {
        self.expanded_instruction(
            Mnemonic::Jmp,
            AddressingMode::Absolute,
            Some(Operand::Anchor(anchor)),
            span,
            reason,
        );
    }

    /// An instruction an HLA statement expands to, optionally recording what
    /// the compiler produced it for.
    fn expanded_instruction(
        &mut self,
        mnemonic: Mnemonic,
        mode: AddressingMode,
        operand: Option<Operand>,
        span: Span,
        reason: Option<Reason>,
    ) {
        let mut op = KOp::new(
            KOpKind::Instruction(Instruction::new(mnemonic, mode, operand)),
            span,
            Origin::HlaExpansion,
        );
        op.reason = reason;
        self.ops.push(op);
    }

    /// Emits the branches a condition means, all landing on `anchor`.
    fn branch_to(
        &mut self,
        condition: crate::ast::BranchCondition,
        sense: hla::BranchSense,
        anchor: AnchorId,
        span: Span,
    ) {
        // A taken branch needs no explanation: it tests what the source
        // says. An inverted one is the surprise worth annotating.
        let reason = match sense {
            hla::BranchSense::Skip => Some(Reason::InvertedBranch),
            hla::BranchSense::Take => None,
        };
        let branch = |lowering: &mut Self, mnemonic: Mnemonic, target: AnchorId| {
            lowering.expanded_instruction(
                mnemonic,
                AddressingMode::Relative,
                Some(Operand::Anchor(target)),
                span,
                reason,
            );
        };
        match hla::branch(condition.kind, sense) {
            hla::BranchPlan::Single(mnemonic) => branch(self, mnemonic, anchor),
            hla::BranchPlan::Pair(first, second) => {
                branch(self, first, anchor);
                branch(self, second, anchor);
            }
            // The guard jumps over the branch that takes the target, so it
            // needs a target of its own.
            hla::BranchPlan::Guarded { over, take } => {
                let past = self.anchor();
                branch(self, over, past);
                branch(self, take, anchor);
                self.place(past, span);
            }
        }
    }

    fn push(&mut self, kind: KOpKind, span: Span, origin: Origin) {
        self.ops.push(KOp::new(kind, span, origin));
    }

    /// Pushes a compiler-generated operation, recording what for.
    fn generated(&mut self, kind: KOpKind, span: Span, reason: Reason) {
        self.ops
            .push(KOp::new(kind, span, Origin::CompilerGenerated).because(reason));
    }

    fn generated_instruction(
        &mut self,
        mnemonic: Mnemonic,
        mode: AddressingMode,
        operand: Option<Operand>,
        span: Span,
        reason: Reason,
    ) {
        self.generated(
            KOpKind::Instruction(Instruction::new(mnemonic, mode, operand)),
            span,
            reason,
        );
    }

    fn instruction(
        &mut self,
        mnemonic: Mnemonic,
        mode: AddressingMode,
        operand: Option<Operand>,
        span: Span,
        origin: Origin,
    ) {
        self.push(
            KOpKind::Instruction(Instruction::new(mnemonic, mode, operand)),
            span,
            origin,
        );
    }

    /// The absolute address operand a near call transfers to.
    ///
    /// Every other address operand goes through the classifier, which chooses
    /// the encoding; a `jsr` has only this one, so there is nothing to choose
    /// (`postmortem.md` §D.1.6).
    fn absolute(symbol: SymbolId) -> Operand {
        Operand::Address {
            encoding: AddressEncoding::Absolute,
            expr: KExpr::Symbol(symbol),
            index: None,
        }
    }

    /// Emits the instructions an HLA statement expands to.
    ///
    /// Analysis has already expanded this statement and reported anything
    /// wrong with it, so a diagnostic here is a compiler bug rather than user
    /// error, exactly as on the native arm.
    ///
    /// The width model tracks one state per *statement*, so an expansion
    /// reconciles once, against what the whole sequence does rather than what
    /// its last instruction does. No chain instruction changes a register
    /// width today; the fold is here so that the one which eventually does is
    /// not silently dropped.
    fn expanded(
        &mut self,
        statement: &crate::ast::Statement,
        steps: Vec<crate::resolve::Classified>,
        span: Span,
    ) {
        let mut combined: Option<Effects> = None;
        for step in steps {
            let effects = step.instruction().effects();
            for _ in 0..step.repeat {
                let instruction = step.instruction();
                self.push(
                    KOpKind::Instruction(instruction),
                    span,
                    Origin::HlaExpansion,
                );
            }
            combined = Some(match combined {
                None => effects,
                Some(carried) => Effects {
                    width: match effects.width {
                        WidthEffect::Unchanged => carried.width,
                        changed => changed,
                    },
                    ..effects
                },
            });
        }
        if let Some(effects) = combined {
            self.reconcile(statement, effects);
        }
    }

    /// Establishes the function's entry widths.
    ///
    /// Only `main` emits instructions, and only the `REP` half of them: the
    /// 65816 starts in 8-bit emulation mode after reset, so a `SEP` at entry
    /// would be redundant (`doc/register-width-aware-syntax.md`, "Entry-point
    /// mode setup"). The 8-bit half of the contract is *claimed* instead, by
    /// the assumption below.
    ///
    /// Every other function assumes its contract and emits nothing: its
    /// caller bridges to it. A function called from hand-written assembly
    /// therefore trusts its caller, which is the same trust an `.a8` in a
    /// hand-written `.s` file expresses — stated in the annotation rather
    /// than defended against.
    fn entry(&mut self, is_entry_point: bool, contract: WidthContract, span: Span) {
        if is_entry_point {
            for (width, mask) in [(contract.a, P_MASK_M), (contract.i, P_MASK_X)] {
                if width != Some(RegisterWidth::W16) {
                    continue;
                }
                self.generated_instruction(
                    Mnemonic::Rep,
                    AddressingMode::Immediate,
                    Some(Operand::Immediate(KExpr::number(mask))),
                    span,
                    Reason::EntryPoint,
                );
            }
        }
        let entry = self.widths.map_or_else(ModeState::unknown, |a| a.entry);
        self.assume(entry, span, Reason::EntryContract);
    }

    /// States what the width model concluded, which is authoritative for the
    /// assembler wherever the conclusion cannot be derived from the preceding
    /// instruction.
    fn assume(&mut self, state: ModeState, span: Span, reason: Reason) {
        if state.is_bottom() {
            // No path reaches here, so there is nothing to claim. A block the
            // model never entered still emits its instructions — K816 does not
            // delete user code — but it makes no width claim over them.
            return;
        }
        self.generated(KOpKind::ModeAssumption(state), span, reason);
    }

    /// Emits the model's own `REP`/`SEP` for `statement` at one anchor.
    fn bridges(&mut self, statement: &crate::ast::Statement, anchor: BridgeAnchor) {
        let bridges: Vec<Bridge> = self
            .widths
            .map(|analysis| {
                analysis
                    .bridges(statement.id)
                    .iter()
                    .filter(|bridge| bridge.anchor == anchor)
                    .copied()
                    .collect()
            })
            .unwrap_or_default();
        for bridge in bridges {
            let mnemonic = match bridge.width {
                RegisterWidth::W8 => Mnemonic::Sep,
                RegisterWidth::W16 => Mnemonic::Rep,
            };
            self.generated_instruction(
                mnemonic,
                AddressingMode::Immediate,
                Some(Operand::Immediate(KExpr::number(bridge.mask))),
                statement.span,
                bridge.reason,
            );
        }
    }

    /// The state after `statement`, as the model concluded it.
    fn widths_after(&self, statement: &crate::ast::Statement) -> Option<ModeState> {
        self.widths
            .and_then(|analysis| analysis.after(statement.id))
    }

    /// The width state the model tracks before `statement`.
    ///
    /// A statement the analysis does not describe is a compiler bug, so it
    /// reports one instead of substituting the entry contract — which would
    /// be right most of the time, and the one time it is wrong every byte
    /// after it shifts.
    fn widths_at(&mut self, statement: &crate::ast::Statement) -> ModeState {
        match self
            .widths
            .and_then(|analysis| analysis.before(statement.id))
        {
            Some(state) => state,
            None => {
                self.diagnostics.push(Diagnostic::error(
                    DiagnosticCode::INTERNAL,
                    "internal: the width model has no state for this statement",
                    Label::new(statement.span, "no tracked width here"),
                ));
                ModeState::unknown()
            }
        }
    }

    fn statement(&mut self, statement: &crate::ast::Statement) {
        let span = statement.span;
        // Whatever the model decided to establish here runs before the
        // statement's own operations, except at a label — where it runs after
        // the label so that arrivals by branch get it too.
        self.bridges(statement, BridgeAnchor::Before);
        let start = self.ops.len();
        self.statement_kind(statement, span);
        self.split_taken_edges(statement, start, span);
    }

    fn statement_kind(&mut self, statement: &crate::ast::Statement, span: Span) {
        match &statement.kind {
            StatementKind::Native(native) => {
                // Analysis already classified this instruction and reported
                // anything wrong with it, so a second answer here would be a
                // compiler bug rather than user error.
                let mut resolver = Resolver::new(&self.model.symbols)
                    .in_function(self.function)
                    .with_widths(self.widths_at(statement));
                match resolver.classify(native) {
                    Some(classified) if !resolver.has_diagnostics() => {
                        let effects = classified.instruction().effects();
                        for _ in 0..classified.repeat {
                            let instruction = classified.instruction();
                            self.push(KOpKind::Instruction(instruction), span, Origin::User);
                        }
                        self.reconcile(statement, effects);
                    }
                    _ => self.skipped(span, "an unclassified instruction"),
                }
            }
            StatementKind::Hla(hla) => {
                let mut resolver = Resolver::new(&self.model.symbols)
                    .in_function(self.function)
                    .with_widths(self.widths_at(statement));
                let steps = hla::statement(&mut resolver, hla, span);
                match steps {
                    Some(steps) if !resolver.has_diagnostics() => {
                        self.expanded(statement, steps, span);
                    }
                    _ => self.skipped(span, "an HLA statement"),
                }
            }
            StatementKind::Return(ReturnKind::Subroutine) => {
                // `return` is the HLA spelling of `rts`.
                self.instruction(
                    Mnemonic::Rts,
                    AddressingMode::Stack,
                    None,
                    span,
                    Origin::HlaExpansion,
                );
            }
            // Analysis reports each of these as not-yet-implemented, so
            // lowering is never reached with one.
            StatementKind::Return(ReturnKind::Interrupt) => {
                self.skipped(span, "a `return_i`");
            }
            // The block's entry bridge was emitted above; its restore is
            // emitted after the body, and only for axes the block changed.
            StatementKind::WidthBlock(width) => {
                self.block(&width.body);
                self.bridges(statement, BridgeAnchor::After);
            }
            // A bare mode set is nothing but its bridge, which the anchor
            // above already emitted — unless the model moved it past a
            // following label, in which case the label emits it.
            StatementKind::ModeSet(_) => {}
            // The bridge for the callee's entry contract was emitted above;
            // what is left is the transfer itself and the claim about what
            // the callee left behind.
            // `call NAME` opts out of the contract checks entirely, damage
            // tracking included: that is what the keyword is for.
            StatementKind::Call(call) => self.call(statement, &call.target, false),
            StatementKind::BareCall(call) => {
                let checked = call.arguments.is_some() || call.outputs.is_some();
                self.call(statement, &call.target, checked);
            }
            // A conditional branches *past* its body: the instruction tests
            // the complement of what is written, which is why the sense is
            // `Skip` (sub-plan E, decision 4).
            StatementKind::Conditional(conditional) => {
                let end = self.anchor();
                let otherwise = conditional.otherwise.as_ref().map(|_| self.anchor());
                self.branch_to(
                    conditional.condition,
                    hla::BranchSense::Skip,
                    otherwise.unwrap_or(end),
                    span,
                );
                self.block(&conditional.body);
                if let (Some(alternative), Some(body)) = (otherwise, &conditional.otherwise) {
                    self.jump_to(end, span, Some(Reason::InvertedBranch));
                    self.place(alternative, span);
                    self.block(body);
                }
                self.place(end, span);
            }
            // A postfix loop is do-while: the body runs, then the condition
            // decides whether to run it again. `always` is an unconditional
            // back edge and `never` is a block with no back edge at all,
            // which exists so `break` has somewhere to go (decision 5).
            StatementKind::Loop(loop_statement) => {
                let head = self.anchor();
                let exit = self.anchor();
                let head_at = self.ops.len();
                self.place(head, span);
                self.loops.push(LoopAnchors { head, exit });
                self.block(&loop_statement.body);
                match loop_statement.condition {
                    crate::ast::LoopCondition::Branch(condition) => {
                        self.branch_to(condition, hla::BranchSense::Take, head, span);
                    }
                    crate::ast::LoopCondition::Always => {
                        self.jump_to(head, span, None);
                    }
                    crate::ast::LoopCondition::Never => {}
                }
                self.loops.pop();
                // The exit exists so `break` has somewhere to go. A loop
                // nothing breaks out of needs no landing site, and emitting
                // one would put a label in the stream that nothing names.
                if self.references(exit, head_at) {
                    self.place(exit, span);
                }
            }
            StatementKind::Break => match self.loops.last() {
                Some(loop_anchors) => self.jump_to(loop_anchors.exit, span, None),
                None => self.skipped(span, "a `break` outside a loop"),
            },
            StatementKind::Repeat => match self.loops.last() {
                Some(loop_anchors) => self.jump_to(loop_anchors.head, span, None),
                None => self.skipped(span, "a `repeat` outside a loop"),
            },
            // The conditional forms of the same two. A conditional `goto` is
            // sub-plan E5's, with the rest of the jump family.
            StatementKind::ConditionalJump(jump) => {
                let anchor = match (&jump.jump.kind, self.loops.last()) {
                    (StatementKind::Break, Some(loop_anchors)) => loop_anchors.exit,
                    (StatementKind::Repeat, Some(loop_anchors)) => loop_anchors.head,
                    (StatementKind::Break | StatementKind::Repeat, None) => {
                        return self.skipped(span, "a conditional jump outside a loop");
                    }
                    _ => return self.skipped(span, "a conditional jump"),
                };
                self.branch_to(jump.condition, hla::BranchSense::Take, anchor, span);
            }
            // `nocross` on code is advisory: the body is ordinary code, and
            // the guard is a link-time assertion about where it lands.
            StatementKind::Nocross(body) => {
                let start = self.ops.len();
                self.block(body);
                let length: u32 = self.ops[start..].iter().map(byte_length).sum();
                self.check_nocross_fits(start, length, span);
                self.push(
                    KOpKind::NocrossGuard { length },
                    span,
                    Origin::CompilerGenerated,
                );
            }
            StatementKind::Goto(_) => self.skipped(span, "a `goto` statement"),
            // An inline block emits raw bytes in the middle of executable
            // code, so control has to step over them.
            StatementKind::Data(data) => {
                let site =
                    resolve::data_site(self.anonymous, &self.unit, data, Some(&self.function_name));
                let mut resolver = Resolver::new(&self.model.symbols)
                    .in_function(self.function)
                    .with_widths(self.widths_at(statement));
                let ops = resolver.data_block(data, span, &site);
                if resolver.has_diagnostics() {
                    return self.skipped(span, "an unresolved inline data block");
                }
                self.push(KOpKind::SkipData, span, Origin::CompilerGenerated);
                self.ops.extend(ops);
                self.push(KOpKind::SkipTarget, span, Origin::CompilerGenerated);
            }
            StatementKind::Preprocessor(_) => self.skipped(span, "a preprocessor directive"),
            StatementKind::Label(label) => {
                // Analysis declared both scopes; a local label lives in the
                // enclosing function's scope, a global one in the unit's.
                let symbol = match label.scope {
                    crate::ast::LabelScope::Global => self.model.symbols.lookup(&label.name.text),
                    crate::ast::LabelScope::Local => self
                        .function
                        .and_then(|owner| self.model.symbols.lookup_local(owner, &label.name.text)),
                };
                match symbol {
                    Some(symbol) => self.push(KOpKind::Label(symbol), span, Origin::User),
                    None => return self.skipped(span, "an undeclared label"),
                }
                // The order here is normative. The bridge is *immediately*
                // after the label operation with nothing between, because that
                // adjacency is how the optimizer recognizes a fixed label
                // bridge it must not eliminate. The assumption comes after the
                // bridge because it states the width that holds once the
                // bridge has run; emitting it first would claim a width the
                // next instruction then changes.
                self.bridges(statement, BridgeAnchor::AfterLabel);
                if let Some(state) = self.widths_after(statement) {
                    self.assume(state, span, Reason::LabelMerge);
                }
            }
            StatementKind::Error => {}
        }
    }

    /// Lowers a call to `jsr`, and claims what the callee's exit promises.
    ///
    /// The claim is emitted whenever the model concluded something a reader
    /// of the instruction stream could not: after a call that changes the
    /// widths, and after one whose callee K816 cannot see, where the
    /// annotation records that the preserving answer was assumed rather than
    /// derived.
    fn call(
        &mut self,
        statement: &crate::ast::Statement,
        target: &crate::ast::Name,
        checked: bool,
    ) {
        let Some(symbol) = self.model.symbols.lookup(&target.text) else {
            return self.skipped(statement.span, "a call to an undeclared function");
        };
        let before = self.widths_at(statement);
        if checked {
            self.checked_calls.push(CheckedCall {
                op: self.ops.len(),
                callee: symbol,
                span: statement.span,
            });
        }
        // The same operand shape the classifier gives a hand-written
        // `jsr NAME`, so the managed and native forms emit identically.
        self.instruction(
            Mnemonic::Jsr,
            AddressingMode::Absolute,
            Some(Self::absolute(symbol)),
            statement.span,
            Origin::HlaExpansion,
        );
        let Some(after) = self.widths_after(statement) else {
            return;
        };
        let assumed = self
            .widths
            .is_some_and(|analysis| analysis.assumed_calls.contains(&statement.id));
        if after != before || assumed {
            let reason = if assumed {
                Reason::AssumedExit
            } else {
                Reason::CalleeExit(symbol)
            };
            self.assume(after, statement.span, reason);
        }
    }

    /// States the model's conclusion whenever it differs from what the
    /// instruction just emitted implies on its own.
    ///
    /// The assembler's tracked width has two sources — an executed
    /// instruction's own effect, and a mode assumption — and they usually
    /// agree. Where they do not, the model wins: a `rep #$20` executed with
    /// the emulation flag unknown establishes nothing, so the claim the
    /// instruction would otherwise carry has to be withdrawn rather than
    /// left standing.
    fn reconcile(&mut self, statement: &crate::ast::Statement, effects: Effects) {
        // Nothing follows an instruction that leaves for good, so there is
        // nothing for a claim there to describe.
        if matches!(
            effects.control,
            ControlEffect::Jump | ControlEffect::Return | ControlEffect::Halt
        ) {
            return;
        }
        let (Some(before), Some(after)) = (
            self.widths.and_then(|a| a.before(statement.id)),
            self.widths_after(statement),
        ) else {
            return;
        };
        if before.is_bottom() {
            return; // Unreachable code makes no claim either way.
        }
        let carried = match effects.width {
            WidthEffect::Unchanged => before,
            WidthEffect::Known { a, i } => ModeState {
                a: a.map_or(before.a, AxisState::Known),
                i: i.map_or(before.i, AxisState::Known),
                e: after.e,
            },
            WidthEffect::Unknown => ModeState {
                a: AxisState::Unknown,
                i: AxisState::Unknown,
                e: after.e,
            },
        };
        // A native call to a target K816 cannot name takes the language's
        // preserving rule, which is an assumption rather than a conclusion.
        // The claim is emitted so the emitted line can say so, even though it
        // changes nothing.
        let assumed = self
            .widths
            .is_some_and(|analysis| analysis.assumed_calls.contains(&statement.id));
        if carried != after || assumed {
            let reason = if assumed {
                Reason::AssumedExit
            } else {
                Reason::WithdrawnClaim
            };
            self.assume(after, statement.span, reason);
        }
    }

    /// K65 functions return implicitly: append `rts` unless the body already
    /// ends in a returning instruction. The synthetic op carries a zero-width
    /// origin span at the closing brace.
    fn implicit_return(&mut self, body_span: Span) {
        let ends_returning = self.ops.last().is_some_and(|op| match &op.kind {
            KOpKind::Instruction(instruction) => {
                instruction.effects().control == ControlEffect::Return
            }
            // Unit-level directives never end a function body, and a label
            // is not a return.
            KOpKind::Label(_)
            | KOpKind::Segment(_)
            | KOpKind::Data(_)
            | KOpKind::Align(_)
            | KOpKind::Export(_)
            | KOpKind::Import(_)
            | KOpKind::NocrossGuard { .. }
            | KOpKind::SkipData
            | KOpKind::SkipTarget
            | KOpKind::Anchor(_)
            // A width claim emits no bytes, so it never ends a body either.
            | KOpKind::ModeAssumption(_) => false,
        });
        if !ends_returning {
            let brace = body_span.end.saturating_sub(1).max(body_span.start);
            self.generated_instruction(
                Mnemonic::Rts,
                AddressingMode::Stack,
                None,
                Span::empty_at(body_span.file, brace),
                Reason::ImplicitReturn,
            );
        }
    }

    /// Records that lowering skipped a construct semantic analysis should
    /// already have rejected — reaching this is a compiler bug, not user
    /// error, but it must never panic.
    fn skipped(&mut self, span: Span, what: &str) {
        self.diagnostics.push(Diagnostic::error(
            DiagnosticCode::INTERNAL,
            format!("internal: lowering skipped {what} that analysis accepted"),
            Label::new(span, "this construct was not lowered"),
        ));
    }
}
