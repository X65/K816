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
    AstFile, ExprKind, HlaOperandKind, HlaStatement, ItemKind, NativeOperand, StatementKind,
};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::effects::ControlEffect;
use crate::isa::Mnemonic;
use crate::op::{
    Instruction, KFunction, KOp, KOpKind, KProgram, Operand, Origin, RegisterWidth, WidthContract,
};
use crate::semantic::SemanticModel;
use crate::source::Span;

/// The `SEP`/`REP` mask selecting the accumulator width flag (M).
const P_MASK_M: u32 = 0x20;
/// The `SEP`/`REP` mask selecting the index register width flag (X).
const P_MASK_X: u32 = 0x10;

/// Lowers every analyzed function of `file` into [`KOp`] streams.
///
/// Call after error-free analysis. Constructs the analysis already rejected
/// are skipped with an internal diagnostic rather than a panic.
pub fn lower(file: &AstFile, model: &SemanticModel) -> (KProgram, Vec<Diagnostic>) {
    let mut program = KProgram::default();
    let mut diagnostics = Vec::new();
    for info in &model.functions {
        let ItemKind::Func(func) = &file.items[info.item_index].kind else {
            continue;
        };

        let mut lowering = Lowering {
            model,
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

        program.functions.push(KFunction {
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
        operand: Option<Operand>,
        span: Span,
        origin: Origin,
    ) {
        self.push(
            KOpKind::Instruction(Instruction { mnemonic, operand }),
            span,
            origin,
        );
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
                Some(Operand::Immediate(mask)),
                span,
                Origin::CompilerGenerated,
            );
        }
    }

    fn statement(&mut self, statement: &crate::ast::Statement) {
        let span = statement.span;
        match &statement.kind {
            StatementKind::Native(native) => {
                let Some(mnemonic) = Mnemonic::parse(&native.mnemonic.text) else {
                    return self.skipped(span, "an unresolved mnemonic");
                };
                let operand = match &native.operand {
                    None => None,
                    Some(NativeOperand::Immediate(expr)) => match expr.kind {
                        ExprKind::Number(value) => Some(Operand::Immediate(value)),
                        // Analysis rejects every immediate it cannot reduce to
                        // a number, so nothing else can reach lowering.
                        ExprKind::Error
                        | ExprKind::Symbol(_)
                        | ExprKind::LocalSymbol(_)
                        | ExprKind::Unary(_)
                        | ExprKind::Binary(_)
                        | ExprKind::Field(_)
                        | ExprKind::Subscript(_)
                        | ExprKind::Metadata(_)
                        | ExprKind::TypedView(_)
                        | ExprKind::Eval(_) => {
                            return self.skipped(span, "an unresolved immediate");
                        }
                    },
                    // Analysis rejects every address operand that is not a
                    // plain symbol in the ISA table's absolute mode.
                    Some(NativeOperand::Address(address)) => {
                        let ExprKind::Symbol(name) = &address.value.kind else {
                            return self.skipped(span, "an unresolved address operand");
                        };
                        match self.model.symbols.lookup(&name.text) {
                            Some(symbol) => Some(Operand::Address(symbol)),
                            None => return self.skipped(span, "an unresolved symbol"),
                        }
                    }
                    Some(NativeOperand::Register(_))
                    | Some(NativeOperand::Indirect(_))
                    | Some(NativeOperand::LongIndirect(_))
                    | Some(NativeOperand::BlockMove(_)) => {
                        return self.skipped(span, "an unsupported operand form");
                    }
                };
                self.instruction(mnemonic, operand, span, Origin::User);
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
                            Some(Operand::Immediate(number)),
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
                            Some(Operand::Address(symbol)),
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
            StatementKind::Return => {
                // `return` is the HLA spelling of `rts`.
                self.instruction(Mnemonic::Rts, None, span, Origin::HlaExpansion);
            }
            // Analysis reports labels as not-yet-implemented, so lowering is
            // never reached with one.
            StatementKind::Label(_) => self.skipped(span, "a label"),
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
            KOpKind::Label(_) => false,
        });
        if !ends_returning {
            let brace = body_span.end.saturating_sub(1).max(body_span.start);
            self.instruction(
                Mnemonic::Rts,
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
