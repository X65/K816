/*****************************************************************************/
/*                                                                           */
/*                                semantic.rs                                */
/*                                                                           */
/*        Symbol, contract, operand, and width checks over the AST           */
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

//! Semantic analysis for the vertical slice: declarations, function
//! entry-mode contracts, symbol lookup, native operand legality, and
//! immediate width/range checks.
//!
//! Operates purely on the owned AST — no Tree-sitter types — and produces a
//! [`SemanticModel`] that lowering and emission consume, plus structured
//! diagnostics.

use crate::ast::{
    AstFile, Expr, ExprKind, Function, HlaStatement, ItemKind, NativeOperand, Statement,
    StatementKind, WidthAnnotationKind,
};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::isa::{AddressingMode, ImmediateWidthClass, Mnemonic, spec};
use crate::op::{RegisterWidth, WidthContract};
use crate::source::{Provenance, Span};
use crate::symbol::{SymbolId, SymbolKind, SymbolTable};

/// The result of analyzing one file: symbols plus per-function facts.
#[derive(Debug, Default)]
pub struct SemanticModel {
    pub symbols: SymbolTable,
    pub functions: Vec<FunctionInfo>,
}

/// Facts about one function needed by lowering and emission.
#[derive(Clone, Copy, Debug)]
pub struct FunctionInfo {
    pub symbol: SymbolId,
    /// Index of the corresponding [`ItemKind::Func`] in `AstFile::items`.
    pub item_index: usize,
    pub contract: WidthContract,
    /// `main` is the program entry point and receives entry-mode setup.
    pub is_entry_point: bool,
}

/// Analyzes `file`, returning the semantic model and all semantic
/// diagnostics. Runs to completion even on errors so tools see everything.
pub fn analyze_file(file: &AstFile) -> (SemanticModel, Vec<Diagnostic>) {
    let mut analysis = Analysis {
        model: SemanticModel::default(),
        diagnostics: Vec::new(),
    };
    analysis.declare_items(file);
    analysis.check_functions(file);
    (analysis.model, analysis.diagnostics)
}

struct Analysis {
    model: SemanticModel,
    diagnostics: Vec<Diagnostic>,
}

impl Analysis {
    /// Declares all top-level symbols first, so use-before-declaration inside
    /// function bodies resolves within one file.
    fn declare_items(&mut self, file: &AstFile) {
        for (item_index, item) in file.items.iter().enumerate() {
            match &item.kind {
                ItemKind::Var(var) => {
                    if var.name.provenance == Provenance::Synthetic {
                        continue; // Recovery invented the name; syntax error already reported.
                    }
                    let address = match var.address.kind {
                        ExprKind::Number(value) => value,
                        ExprKind::Error => continue,
                    };
                    self.declare(
                        &var.name.text,
                        SymbolKind::FixedAddressVar { address },
                        var.name.span,
                    );
                }
                ItemKind::Func(func) => {
                    if func.name.provenance == Provenance::Synthetic {
                        continue;
                    }
                    let Some(symbol) =
                        self.declare(&func.name.text, SymbolKind::Function, func.name.span)
                    else {
                        continue;
                    };
                    self.model.functions.push(FunctionInfo {
                        symbol,
                        item_index,
                        contract: contract_of(func),
                        is_entry_point: func.name.text == "main",
                    });
                }
                ItemKind::Error => {}
            }
        }
    }

    fn declare(&mut self, name: &str, kind: SymbolKind, span: Span) -> Option<SymbolId> {
        match self.model.symbols.declare(name, kind, span) {
            Ok(id) => Some(id),
            Err(existing) => {
                let previous = self.model.symbols.symbol(existing).declaration;
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::DUPLICATE_SYMBOL,
                        format!("`{name}` is declared more than once"),
                        Label::new(span, "redeclared here"),
                    )
                    .with_secondary(Label::new(previous, "first declared here"))
                    .with_help("rename one of the declarations"),
                );
                None
            }
        }
    }

    fn check_functions(&mut self, file: &AstFile) {
        for info in 0..self.model.functions.len() {
            let function = self.model.functions[info];
            let ItemKind::Func(func) = &file.items[function.item_index].kind else {
                continue;
            };
            for statement in &func.body.statements {
                self.check_statement(statement, function.contract);
            }
        }
    }

    fn check_statement(&mut self, statement: &Statement, contract: WidthContract) {
        match &statement.kind {
            StatementKind::Native(native) => {
                if native.mnemonic.provenance == Provenance::Synthetic {
                    return;
                }
                let Some(mnemonic) = Mnemonic::parse(&native.mnemonic.text) else {
                    self.diagnostics.push(
                        Diagnostic::error(
                            DiagnosticCode::UNKNOWN_MNEMONIC,
                            format!("unknown instruction `{}`", native.mnemonic.text),
                            Label::new(native.mnemonic.span, "not a supported 65816 mnemonic"),
                        )
                        .with_docs("doc/syntax-reference.md#standard-6502-instructions"),
                    );
                    return;
                };
                let mode = match &native.operand {
                    None => AddressingMode::Implied,
                    Some(NativeOperand::Immediate(_)) => AddressingMode::Immediate,
                    Some(NativeOperand::Address(_)) => AddressingMode::Absolute,
                };
                let Some(spec) = spec(mnemonic, mode) else {
                    self.diagnostics.push(
                        Diagnostic::error(
                            DiagnosticCode::INVALID_OPERAND,
                            format!(
                                "`{}` does not support this operand form",
                                native.mnemonic.text
                            ),
                            Label::new(statement.span, format!("{mode:?} mode is not legal here")),
                        )
                        .with_docs("doc/w65c816s.md"),
                    );
                    return;
                };
                match &native.operand {
                    Some(NativeOperand::Immediate(value)) => {
                        self.check_immediate(value, spec.immediate_width, contract, mnemonic);
                    }
                    Some(NativeOperand::Address(name)) => {
                        self.check_memory_operand(&name.text, name.span);
                    }
                    None => {}
                }
            }
            StatementKind::Hla(HlaStatement::LoadImmediate { value, .. }) => {
                // `a = #imm` sizes like the native `lda #imm`.
                self.check_immediate(
                    value,
                    Some(ImmediateWidthClass::MDependent),
                    contract,
                    Mnemonic::Lda,
                );
            }
            StatementKind::Hla(HlaStatement::Store { target, .. }) => {
                if target.provenance != Provenance::Synthetic {
                    self.check_memory_operand(&target.text, target.span);
                }
            }
            StatementKind::Return | StatementKind::Error => {}
        }
    }

    /// Validates an immediate operand against its width class and the
    /// function's entry contract (the minimal slice width model).
    fn check_immediate(
        &mut self,
        value: &Expr,
        width: Option<ImmediateWidthClass>,
        contract: WidthContract,
        mnemonic: Mnemonic,
    ) {
        let ExprKind::Number(number) = value.kind else {
            return; // Syntax diagnostics already cover malformed values.
        };
        let (limit, axis) = match width {
            Some(ImmediateWidthClass::Fixed8) => (0xFF, None),
            Some(ImmediateWidthClass::MDependent) => match contract.a {
                Some(RegisterWidth::W8) => (0xFF, None),
                Some(RegisterWidth::W16) => (0xFFFF, None),
                None => (0, Some("accumulator")),
            },
            Some(ImmediateWidthClass::XDependent) => match contract.i {
                Some(RegisterWidth::W8) => (0xFF, None),
                Some(RegisterWidth::W16) => (0xFFFF, None),
                None => (0, Some("index register")),
            },
            None => return,
        };
        if let Some(axis) = axis {
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::UNKNOWN_WIDTH,
                    format!(
                        "`{}` uses a width-dependent immediate but {axis} width is unknown",
                        mnemonic.ca65()
                    ),
                    Label::new(value.span, "cannot size this immediate"),
                )
                .with_help(if axis == "accumulator" {
                    "add @a8 or @a16 to the enclosing function"
                } else {
                    "add @i8 or @i16 to the enclosing function"
                })
                .with_docs("doc/register-width-aware-syntax.md"),
            );
            return;
        }
        if number > limit {
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::IMMEDIATE_RANGE,
                    format!("immediate ${number:X} does not fit in the selected width"),
                    Label::new(value.span, format!("maximum here is ${limit:X}")),
                )
                .with_docs("doc/register-width-aware-syntax.md"),
            );
        }
    }

    /// Validates a memory operand: the symbol must be a declared variable.
    fn check_memory_operand(&mut self, name: &str, span: Span) {
        match self.model.symbols.lookup(name) {
            None => {
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::UNDECLARED_SYMBOL,
                        format!("`{name}` is not declared"),
                        Label::new(span, "used here without a declaration"),
                    )
                    .with_help(format!("declare it first, e.g. `var {name} = $0400`"))
                    .with_docs("doc/syntax-reference.md#variable-declaration"),
                );
            }
            Some(id) => {
                let symbol = self.model.symbols.symbol(id);
                if symbol.kind == SymbolKind::Function {
                    self.diagnostics.push(
                        Diagnostic::error(
                            DiagnosticCode::INVALID_OPERAND,
                            format!("`{name}` is a function, not a variable"),
                            Label::new(span, "cannot be used as a memory operand"),
                        )
                        .with_secondary(Label::new(symbol.declaration, "declared here")),
                    );
                }
            }
        }
    }
}

/// Folds a function's width annotations into its entry contract. On repeated
/// annotations for one axis the last one wins (refined in sub-plan D).
fn contract_of(func: &Function) -> WidthContract {
    let mut contract = WidthContract::default();
    for annotation in &func.width {
        match annotation.kind {
            WidthAnnotationKind::A8 => contract.a = Some(RegisterWidth::W8),
            WidthAnnotationKind::A16 => contract.a = Some(RegisterWidth::W16),
            WidthAnnotationKind::I8 => contract.i = Some(RegisterWidth::W8),
            WidthAnnotationKind::I16 => contract.i = Some(RegisterWidth::W16),
        }
    }
    contract
}
