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
use crate::isa::{AddressEncoding, AddressingMode, Mnemonic};
use crate::op::{
    Instruction, KFunction, KOp, KOpKind, KProgram, Operand, Origin, RegisterWidth, SegmentName,
    WidthContract,
};
use crate::resolve;
use crate::resolve::{AnonymousBlocks, Resolver};
use crate::semantic::SemanticModel;
use crate::source::Span;
use crate::symbol::SymbolId;
use crate::value::KExpr;

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
            contract: info.contract,
            ops: Vec::new(),
            diagnostics: Vec::new(),
        };

        lowering.push(KOpKind::Label(info.symbol), func.name.span, Origin::User);
        if info.is_entry_point {
            lowering.entry_setup(info.contract, func.name.span);
        }
        for statement in &func.body.statements {
            lowering.statement(statement);
        }
        lowering.implicit_return(func.body.span);

        program.push_function(KFunction {
            symbol: info.symbol,
            contract: info.contract,
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
    /// Its entry contract, which sizes width-dependent immediates.
    contract: WidthContract,
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

    /// `main` has no caller, so its contract is established explicitly at
    /// entry. Each declared axis gets its own `sep`/`rep`; folding them into
    /// one instruction is the optimizer's job (sub-plan F), not lowering's.
    fn entry_setup(&mut self, contract: WidthContract, span: Span) {
        for (width, mask) in [(contract.a, P_MASK_M), (contract.i, P_MASK_X)] {
            let mnemonic = match width {
                Some(RegisterWidth::W8) => Mnemonic::Sep,
                Some(RegisterWidth::W16) => Mnemonic::Rep,
                None => continue,
            };
            self.instruction(
                mnemonic,
                AddressingMode::Immediate,
                Some(Operand::Immediate(KExpr::number(mask))),
                span,
                Origin::CompilerGenerated,
            );
        }
    }

    fn statement(&mut self, statement: &crate::ast::Statement) {
        let span = statement.span;
        match &statement.kind {
            StatementKind::Native(native) => {
                // Analysis already classified this instruction and reported
                // anything wrong with it, so a second answer here would be a
                // compiler bug rather than user error.
                let mut resolver = Resolver::new(&self.model.symbols)
                    .in_function(self.function)
                    .with_widths(self.contract);
                match resolver.classify(native) {
                    Some(classified) if !resolver.has_diagnostics() => self.push(
                        KOpKind::Instruction(classified.instruction()),
                        span,
                        Origin::User,
                    ),
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
            StatementKind::WidthBlock(_) => self.skipped(span, "a scoped width block"),
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
                    .with_widths(self.contract);
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
                    None => self.skipped(span, "an undeclared label"),
                }
            }
            StatementKind::Error => {}
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
            | KOpKind::SkipTarget => false,
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
