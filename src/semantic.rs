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
use crate::compat;
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
                    // Only the fixed-address form has semantics so far; every
                    // other axis of the declaration says who owns it.
                    if let Some(feature) = unsupported_var_feature(var) {
                        self.diagnostics.push(compat::unimplemented(
                            item.span,
                            feature,
                            compat::Owner::HlaLowering,
                        ));
                        continue;
                    }
                    let Some(address) = var
                        .address
                        .as_ref()
                        .and_then(|address| self.constant(address, "variable addresses"))
                    else {
                        continue;
                    };
                    self.declare(
                        &var.name.text,
                        SymbolKind::FixedAddressVar { address },
                        var.name.span,
                    );
                }
                ItemKind::Const(_) => self.diagnostics.push(compat::unimplemented(
                    item.span,
                    "`const` declarations",
                    compat::Owner::NativeTranslator,
                )),
                ItemKind::Segment(_) => self.diagnostics.push(compat::unimplemented(
                    item.span,
                    "`segment` selections",
                    compat::Owner::NativeTranslator,
                )),
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
                if native.repeat.is_some() {
                    self.diagnostics.push(compat::unimplemented(
                        statement.span,
                        "instruction repetition counts",
                        compat::Owner::HlaLowering,
                    ));
                    return;
                }
                // The grammar only produces mnemonics the CPU has, so a
                // mnemonic the ISA table does not carry is one the native
                // translator has yet to add, not a typo.
                let Some(mnemonic) = Mnemonic::parse(&native.mnemonic.text) else {
                    self.diagnostics.push(compat::unimplemented(
                        native.mnemonic.span,
                        &format!("`{}` instructions", native.mnemonic.text),
                        compat::Owner::NativeTranslator,
                    ));
                    return;
                };
                // Likewise for the operand shapes the frozen ISA table cannot
                // describe yet: indexing, indirection, and the encoding
                // prefixes all grow with the native translator.
                let mode = match &native.operand {
                    None => AddressingMode::Implied,
                    Some(NativeOperand::Immediate(_)) => AddressingMode::Immediate,
                    Some(NativeOperand::Address(address))
                        if address.encoding.is_none() && address.index.is_none() =>
                    {
                        AddressingMode::Absolute
                    }
                    Some(_) => {
                        self.diagnostics.push(compat::unimplemented(
                            statement.span,
                            "these operand addressing modes",
                            compat::Owner::NativeTranslator,
                        ));
                        return;
                    }
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
                    Some(NativeOperand::Address(address)) => match &address.value.kind {
                        ExprKind::Symbol(name) => self.check_memory_operand(&name.text, name.span),
                        _ => {
                            self.constant(&address.value, "address operands");
                        }
                    },
                    Some(NativeOperand::Accumulator) | Some(NativeOperand::Indirect(_)) | None => {}
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
            StatementKind::Hla(HlaStatement::Store { target, .. }) => match &target.kind {
                ExprKind::Symbol(name) if name.provenance != Provenance::Synthetic => {
                    self.check_memory_operand(&name.text, name.span);
                }
                ExprKind::Symbol(_) | ExprKind::Error => {}
                // `.LT+1 = a` and friends: the target is an address
                // expression, which needs the store lowering work.
                ExprKind::LocalSymbol(_)
                | ExprKind::Number(_)
                | ExprKind::Unary(_)
                | ExprKind::Binary(_)
                | ExprKind::Field(_)
                | ExprKind::Subscript(_)
                | ExprKind::Metadata(_)
                | ExprKind::TypedView(_)
                | ExprKind::Eval(_) => self.diagnostics.push(compat::unimplemented(
                    target.span,
                    "computed assignment targets",
                    compat::Owner::HlaLowering,
                )),
            },
            StatementKind::Label(_) => self.diagnostics.push(compat::unimplemented(
                statement.span,
                "labels",
                compat::Owner::NativeTranslator,
            )),
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
        let Some(number) = self.constant(value, "immediate operands") else {
            return;
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

    /// The value of `expr`, which must already be a literal.
    ///
    /// Constant folding — symbol references, arithmetic, and the compile-time
    /// evaluator — belongs to the native translator, so every other shape
    /// reports itself as not-yet-implemented instead of being silently
    /// ignored. `position` names where the expression appeared, in plural form
    /// ("immediate operands"). Returns `None` for malformed expressions, whose
    /// syntax diagnostic was already reported.
    fn constant(&mut self, expr: &Expr, position: &str) -> Option<u32> {
        match &expr.kind {
            ExprKind::Number(value) => Some(*value),
            ExprKind::Error => None,
            ExprKind::Symbol(_)
            | ExprKind::LocalSymbol(_)
            | ExprKind::Unary(_)
            | ExprKind::Binary(_)
            | ExprKind::Field(_)
            | ExprKind::Subscript(_)
            | ExprKind::Metadata(_)
            | ExprKind::TypedView(_)
            | ExprKind::Eval(_) => {
                self.diagnostics.push(compat::unimplemented(
                    expr.span,
                    &format!("compound expressions in {position}"),
                    compat::Owner::NativeTranslator,
                ));
                None
            }
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

/// Names the first part of `var` that has no semantics yet, or `None` when
/// the declaration is the fixed-address form the slice compiles.
fn unsupported_var_feature(var: &crate::ast::VarDeclaration) -> Option<&'static str> {
    if var.is_abstract {
        Some("`abstract var` layouts")
    } else if !var.fields.is_empty() {
        Some("symbolic subscript field lists")
    } else if var.storage.is_some() {
        Some("`dp` / `abs` / `far` storage classes")
    } else if var.width.is_some() {
        Some("typed width suffixes")
    } else if var.size.is_some() {
        Some("array declarations")
    } else if var.count.is_some() {
        Some("allocation counts")
    } else if var.address.is_none() {
        Some("linker-allocated variables")
    } else {
        None
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
