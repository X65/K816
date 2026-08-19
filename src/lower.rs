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

use crate::ast::{AstFile, HlaOperandKind, HlaStatement, ItemKind, ReturnKind, StatementKind};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::effects::{ControlEffect, Effects, RegisterSet, WidthEffect};
use crate::isa::{AddressEncoding, AddressingMode, Mnemonic};
use crate::op::{
    AxisState, Instruction, KFunction, KOp, KOpKind, KProgram, ModeState, Operand, Origin, Reason,
    RegisterWidth, SegmentName, WidthContract,
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
        };

        lowering.push(KOpKind::Label(info.symbol), func.name.span, Origin::User);
        lowering.entry(info.is_entry_point, info.contract, func.name.span);
        for statement in &func.body.statements {
            lowering.statement(statement);
        }
        lowering.implicit_return(func.body.span);

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

    /// Lowers one operand an HLA statement wrote, through the classifier the
    /// native path uses — width reconciliation included, so the width model
    /// sees the same operation whichever syntax produced it.
    ///
    /// Analysis has already classified this operand and reported anything
    /// wrong with it, so a diagnostic here is a compiler bug rather than user
    /// error, exactly as on the native arm.
    fn hla_instruction(
        &mut self,
        statement: &crate::ast::Statement,
        mnemonic: Mnemonic,
        operand: &crate::ast::HlaOperand,
        span: Span,
    ) {
        let mut resolver = Resolver::new(&self.model.symbols)
            .in_function(self.function)
            .with_widths(self.widths_at(statement));
        match resolver.classify_hla(mnemonic, operand, span) {
            Some(classified) if !resolver.has_diagnostics() => {
                let instruction = classified.instruction();
                let effects = instruction.effects();
                self.push(
                    KOpKind::Instruction(instruction),
                    span,
                    Origin::HlaExpansion,
                );
                self.reconcile(statement, effects);
            }
            _ => self.skipped(span, "an unclassified HLA operand"),
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
                        let instruction = classified.instruction();
                        let effects = instruction.effects();
                        self.push(KOpKind::Instruction(instruction), span, Origin::User);
                        self.reconcile(statement, effects);
                    }
                    _ => self.skipped(span, "an unclassified instruction"),
                }
            }
            StatementKind::Hla(HlaStatement::Assign(assign)) => {
                // Analysis accepts exactly two chain shapes so far; anything
                // else never reaches lowering. Which instruction a shape means
                // is decided here, because that is what the spelling says;
                // what its operand is is not decided here, because `lda` and
                // `sta` already asked that question (sub-plan E, decision 1).
                let (mnemonic, operand) = match (assign.targets.as_slice(), &assign.source.kind) {
                    ([_], HlaOperandKind::Immediate(_)) => (Mnemonic::Lda, &assign.source),
                    ([target], HlaOperandKind::Register(_)) => (Mnemonic::Sta, target),
                    _ => return self.skipped(span, "an unsupported assignment chain"),
                };
                self.hla_instruction(statement, mnemonic, operand, span);
            }
            StatementKind::Hla(
                HlaStatement::Exchange(_)
                | HlaStatement::Arithmetic(_)
                | HlaStatement::Shift(_)
                | HlaStatement::Step(_)
                | HlaStatement::Flag(_)
                | HlaStatement::Stack(_)
                | HlaStatement::Nop { .. }
                | HlaStatement::Break,
            ) => {
                self.skipped(span, "an HLA shorthand");
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
                for nested in &width.body.statements {
                    self.statement(nested);
                }
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
            StatementKind::Conditional(_)
            | StatementKind::ConditionalJump(_)
            | StatementKind::Loop(_)
            | StatementKind::Goto(_)
            | StatementKind::Break
            | StatementKind::Repeat
            | StatementKind::Nocross(_) => self.skipped(span, "a branch or loop statement"),
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
