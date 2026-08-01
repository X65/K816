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

use crate::ast::{
    AstFile, ExprKind, HlaOperandKind, HlaStatement, ItemKind, ReturnKind, StatementKind,
};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::effects::ControlEffect;
use crate::effects::WidthEffect;
use crate::isa::{AddressEncoding, AddressingMode, Mnemonic};
use crate::op::{
    AxisState, Instruction, KFunction, KOp, KOpKind, KProgram, ModeState, Operand, Origin,
    RegisterWidth, SegmentName, WidthContract,
};
use crate::resolve;
use crate::resolve::{AnonymousBlocks, Resolver};
use crate::semantic::SemanticModel;
use crate::source::Span;
use crate::symbol::SymbolId;
use crate::value::KExpr;
use crate::width::{Bridge, BridgeAnchor};

/// The `SEP`/`REP` mask selecting the accumulator width flag (M).
const P_MASK_M: u32 = 0x20;
/// The `SEP`/`REP` mask selecting the index register width flag (X).
const P_MASK_X: u32 = 0x10;

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
            diagnostics: Vec::new(),
        };

        lowering.push(KOpKind::Label(info.symbol), func.name.span, Origin::User);
        lowering.entry(info.is_entry_point, info.contract, func.name.span);
        for statement in &func.body.statements {
            lowering.statement(statement);
        }
        lowering.implicit_return(func.body.span);

        program.push_function(KFunction {
            symbol: info.symbol,
            contract: info.contract,
            exit: info.exit,
            ops: lowering.ops,
        });
        diagnostics.append(&mut lowering.diagnostics);
    }
    (program, diagnostics)
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
    diagnostics: Vec<Diagnostic>,
}

impl Lowering<'_> {
    fn push(&mut self, kind: KOpKind, span: Span, origin: Origin) {
        self.ops.push(KOp { kind, span, origin });
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

    /// An absolute memory reference through a declared symbol — the only
    /// address operand the slice lowers. Task C4 replaces this with the
    /// classifier that chooses the encoding.
    fn absolute(symbol: SymbolId) -> Operand {
        Operand::Address {
            encoding: AddressEncoding::Absolute,
            expr: KExpr::Symbol(symbol),
            index: None,
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
                self.instruction(
                    Mnemonic::Rep,
                    AddressingMode::Immediate,
                    Some(Operand::Immediate(KExpr::number(mask))),
                    span,
                    Origin::CompilerGenerated,
                );
            }
        }
        let entry = self.widths.map_or_else(ModeState::unknown, |a| a.entry);
        self.assume(entry, span);
    }

    /// States what the width model concluded, which is authoritative for the
    /// assembler wherever the conclusion cannot be derived from the preceding
    /// instruction.
    fn assume(&mut self, state: ModeState, span: Span) {
        if state.is_bottom() {
            // No path reaches here, so there is nothing to claim. A block the
            // model never entered still emits its instructions — K816 does not
            // delete user code — but it makes no width claim over them.
            return;
        }
        self.push(
            KOpKind::ModeAssumption(state),
            span,
            Origin::CompilerGenerated,
        );
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
            self.instruction(
                mnemonic,
                AddressingMode::Immediate,
                Some(Operand::Immediate(KExpr::number(bridge.mask))),
                statement.span,
                Origin::CompilerGenerated,
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
                        let effect = instruction.effects().width;
                        self.push(KOpKind::Instruction(instruction), span, Origin::User);
                        self.reconcile(statement, effect);
                    }
                    _ => self.skipped(span, "an unclassified instruction"),
                }
            }
            StatementKind::Hla(HlaStatement::Assign(assign)) => {
                // Analysis accepts exactly two chain shapes so far; anything
                // else never reaches lowering.
                match (assign.targets.as_slice(), &assign.source.kind) {
                    // `a = #imm` is the HLA spelling of `lda #imm`.
                    ([_], HlaOperandKind::Immediate(value)) => {
                        let ExprKind::Number(number) = value.kind else {
                            return self.skipped(span, "a malformed immediate");
                        };
                        self.instruction(
                            Mnemonic::Lda,
                            AddressingMode::Immediate,
                            Some(Operand::Immediate(KExpr::number(number))),
                            span,
                            Origin::HlaExpansion,
                        );
                    }
                    // `MEM = a` is the HLA spelling of `sta MEM`.
                    ([target], HlaOperandKind::Register(_)) => {
                        let HlaOperandKind::Memory(memory) = &target.kind else {
                            return self.skipped(span, "an unsupported assignment target");
                        };
                        let ExprKind::Symbol(name) = &memory.value.kind else {
                            return self.skipped(span, "an unsupported assignment target");
                        };
                        let Some(symbol) = self.model.symbols.lookup(&name.text) else {
                            return self.skipped(span, "an unresolved symbol");
                        };
                        self.instruction(
                            Mnemonic::Sta,
                            AddressingMode::Absolute,
                            Some(Self::absolute(symbol)),
                            span,
                            Origin::HlaExpansion,
                        );
                    }
                    _ => self.skipped(span, "an unsupported assignment chain"),
                }
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
            StatementKind::Call(_) | StatementKind::BareCall(_) => {
                self.skipped(span, "a function call");
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
                    self.assume(state, span);
                }
            }
            StatementKind::Error => {}
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
    fn reconcile(&mut self, statement: &crate::ast::Statement, effect: WidthEffect) {
        let (Some(before), Some(after)) = (
            self.widths.and_then(|a| a.before(statement.id)),
            self.widths_after(statement),
        ) else {
            return;
        };
        if before.is_bottom() {
            return; // Unreachable code makes no claim either way.
        }
        let carried = match effect {
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
        if carried != after {
            self.assume(after, statement.span);
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
            self.instruction(
                Mnemonic::Rts,
                AddressingMode::Stack,
                None,
                Span::empty_at(body_span.file, brace),
                Origin::CompilerGenerated,
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
