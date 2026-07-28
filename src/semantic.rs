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
    self, AstFile, DataBlock, DataEntryKind, DataValue, EvalCallName, EvalExpr, EvalExprKind, Expr,
    ExprKind, Function, FunctionKind, HlaOperandKind, HlaStatement, ItemKind, RegisterKind,
    ReservedKind, ReturnKind, Statement, StatementKind, WidthAnnotationKind,
};
use crate::compat;
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::effects::ControlEffect;
use crate::isa::{ImmediateWidthClass, Mnemonic, RegisterWidth};
use crate::op::WidthContract;
use crate::resolve::{Classified, Resolver};
use crate::source::{Provenance, Span};
use crate::symbol::{AddressSize, LabelScope, SymbolId, SymbolKind, SymbolTable};
use crate::value::KExpr;

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
        current_function: None,
    };
    analysis.declare_placed_symbols(file);
    analysis.declare_items(file);
    analysis.check_functions(file);
    (analysis.model, analysis.diagnostics)
}

struct Analysis {
    model: SemanticModel,
    diagnostics: Vec<Diagnostic>,
    /// The function whose body is being walked, for local label scope.
    current_function: Option<SymbolId>,
}

impl Analysis {
    /// First pass: declares every symbol ld65 places — functions, labels, and
    /// data-block labels.
    ///
    /// These have no value to fold, so declaring them all before anything is
    /// checked is what makes forward references work: a `goto` may name a
    /// label further down its function, and an operand may name a data block
    /// declared at the end of the file.
    ///
    /// `const` and `var` are *not* declared here. Their values fold, and a
    /// fold reads other declarations, so they stay in source order — the
    /// same rule the documentation states for evaluator constants. Declaring
    /// them early with a placeholder value would let a forward reference
    /// silently read the placeholder.
    fn declare_placed_symbols(&mut self, file: &AstFile) {
        for item in &file.items {
            match &item.kind {
                ItemKind::Func(func) => {
                    if func.name.provenance == Provenance::Synthetic
                        || unsupported_function_feature(func).is_some()
                    {
                        continue;
                    }
                    let Some(symbol) = self.declare(
                        &func.name.text,
                        SymbolKind::Function {
                            size: AddressSize::Absolute,
                        },
                        func.name.span,
                    ) else {
                        continue;
                    };
                    self.declare_labels_in(symbol, &func.body);
                }
                ItemKind::Data(data) => self.declare_data_labels(data),
                ItemKind::Var(_)
                | ItemKind::Const(_)
                | ItemKind::Segment(_)
                | ItemKind::EvalBlock(_)
                | ItemKind::Preprocessor(_)
                | ItemKind::Binary(_)
                | ItemKind::Reserved(_)
                | ItemKind::ModuleWidth(_)
                | ItemKind::Error => {}
            }
        }
    }

    /// Declares the labels of one function body. A global label is visible
    /// across the unit; a `.name` label is scoped to `owner`, so two
    /// functions may use the same one.
    fn declare_labels_in(&mut self, owner: SymbolId, block: &crate::ast::Block) {
        for statement in &block.statements {
            match &statement.kind {
                StatementKind::Label(label) => {
                    if label.name.provenance == Provenance::Synthetic {
                        continue;
                    }
                    let kind = SymbolKind::Label {
                        scope: match label.scope {
                            crate::ast::LabelScope::Global => LabelScope::Global,
                            crate::ast::LabelScope::Local => LabelScope::Local,
                        },
                        size: AddressSize::Absolute,
                    };
                    match label.scope {
                        crate::ast::LabelScope::Global => {
                            self.declare(&label.name.text, kind, label.name.span);
                        }
                        crate::ast::LabelScope::Local => {
                            self.declare_local(owner, &label.name.text, kind, label.name.span);
                        }
                    }
                }
                // Labels nest wherever a block does; the scope stays the
                // enclosing function's either way.
                StatementKind::WidthBlock(width) => self.declare_labels_in(owner, &width.body),
                StatementKind::Nocross(body) => self.declare_labels_in(owner, body),
                StatementKind::Loop(loop_statement) => {
                    self.declare_labels_in(owner, &loop_statement.body);
                }
                StatementKind::Conditional(conditional) => {
                    self.declare_labels_in(owner, &conditional.body);
                    if let Some(otherwise) = &conditional.otherwise {
                        self.declare_labels_in(owner, otherwise);
                    }
                }
                StatementKind::Data(data) => self.declare_data_labels(data),
                StatementKind::Native(_)
                | StatementKind::Hla(_)
                | StatementKind::Call(_)
                | StatementKind::BareCall(_)
                | StatementKind::ConditionalJump(_)
                | StatementKind::Goto(_)
                | StatementKind::Break
                | StatementKind::Repeat
                | StatementKind::Preprocessor(_)
                | StatementKind::Return(_)
                | StatementKind::Error => {}
            }
        }
    }

    /// Declares a data block's own name and every label inside it. Both are
    /// unit-level: a data label is exactly how one unit hands another the
    /// address of some bytes.
    fn declare_data_labels(&mut self, data: &DataBlock) {
        if let Some(name) = &data.name
            && name.provenance != Provenance::Synthetic
        {
            self.declare(
                &name.text,
                SymbolKind::DataLabel {
                    size: AddressSize::Absolute,
                },
                name.span,
            );
        }
        for entry in &data.entries {
            match &entry.kind {
                DataEntryKind::Label(name) if name.provenance != Provenance::Synthetic => {
                    self.declare(
                        &name.text,
                        SymbolKind::DataLabel {
                            size: AddressSize::Absolute,
                        },
                        name.span,
                    );
                }
                DataEntryKind::Repeat { body, .. } => self.declare_data_labels(body),
                _ => {}
            }
        }
    }

    /// Second pass: declares the symbols whose value has to be folded, and
    /// records what the rest of analysis needs about each item.
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
                    // A declaration-position prefix sets the default
                    // encoding for plain references; without one the value's
                    // own magnitude decides (`doc/syntax-reference.md`,
                    // "Address-encoding (prefixes)").
                    let size = var.storage.map_or_else(
                        || AddressSize::of_value(address),
                        |storage| address_size_of(storage.kind),
                    );
                    self.declare(
                        &var.name.text,
                        SymbolKind::Var {
                            address: Some(address),
                            size,
                        },
                        var.name.span,
                    );
                }
                ItemKind::Const(declaration) => self.declare_const_group(declaration),
                ItemKind::Data(data) => {
                    self.diagnostics.push(compat::unimplemented(
                        item.span,
                        "`data` blocks",
                        compat::Owner::NativeTranslator,
                    ));
                    self.report_reserved_in_data(data);
                }
                ItemKind::EvalBlock(expression) => {
                    self.diagnostics.push(compat::unimplemented(
                        item.span,
                        "top-level evaluator blocks",
                        compat::Owner::Compatibility,
                    ));
                    self.report_reserved_in_expr(expression);
                }
                ItemKind::Preprocessor(_) => self.diagnostics.push(compat::unimplemented(
                    item.span,
                    "preprocessor directives",
                    compat::Owner::Compatibility,
                )),
                ItemKind::Binary(_) => self.diagnostics.push(compat::unimplemented(
                    item.span,
                    "`binary` inclusions",
                    compat::Owner::Compatibility,
                )),
                // Reserved legacy syntax reports itself through the
                // `K816-K65-####` family, not as unimplemented K816 work.
                ItemKind::Reserved(reserved) => {
                    let diagnostic = reserved_diagnostic(reserved);
                    self.diagnostics.push(diagnostic);
                }
                ItemKind::ModuleWidth(_) => self.diagnostics.push(compat::unimplemented(
                    item.span,
                    "module-level default widths",
                    compat::Owner::WidthAndContracts,
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
                    // Only a plain `func` (or the legacy bare `main`) with no
                    // call contract has semantics so far.
                    if let Some(feature) = unsupported_function_feature(func) {
                        self.diagnostics.push(compat::unimplemented(
                            item.span,
                            feature,
                            compat::Owner::WidthAndContracts,
                        ));
                        continue;
                    }
                    // The first pass declared the name; a duplicate reported
                    // itself there, and this pass records what lowering and
                    // emission need about the function it kept.
                    let Some(symbol) = self.model.symbols.lookup(&func.name.text) else {
                        continue;
                    };
                    if self.model.symbols.symbol(symbol).declaration != func.name.span {
                        continue; // Another declaration won this name.
                    }
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
        let result = self.model.symbols.declare(name, kind, span);
        self.declared(name, span, result)
    }

    fn declare_local(
        &mut self,
        owner: SymbolId,
        name: &str,
        kind: SymbolKind,
        span: Span,
    ) -> Option<SymbolId> {
        let result = self.model.symbols.declare_local(owner, name, kind, span);
        self.declared(name, span, result)
    }

    /// Reports a duplicate declaration, pointing the "first declared here"
    /// label at whichever declaration actually comes first in the source.
    ///
    /// Declaration runs in two passes, so the surviving symbol is not always
    /// the earlier one — a function declared in the first pass wins a name a
    /// `var` on an earlier line asked for. Ordering by span keeps the
    /// diagnostic honest about which line the reader should look at first.
    fn declared(
        &mut self,
        name: &str,
        span: Span,
        result: Result<SymbolId, SymbolId>,
    ) -> Option<SymbolId> {
        let existing = match result {
            Ok(id) => return Some(id),
            Err(existing) => existing,
        };
        let previous = self.model.symbols.symbol(existing).declaration;
        let (first, again) = if previous.start <= span.start {
            (previous, span)
        } else {
            (span, previous)
        };
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::DUPLICATE_SYMBOL,
                format!("`{name}` is declared more than once"),
                Label::new(again, "redeclared here"),
            )
            .with_secondary(Label::new(first, "first declared here"))
            .with_help("rename one of the declarations"),
        );
        None
    }

    fn check_functions(&mut self, file: &AstFile) {
        for info in 0..self.model.functions.len() {
            let function = self.model.functions[info];
            let ItemKind::Func(func) = &file.items[function.item_index].kind else {
                continue;
            };
            self.current_function = Some(function.symbol);
            for statement in &func.body.statements {
                self.check_statement(statement, function.contract);
            }
            self.current_function = None;
        }
    }

    fn check_statement(&mut self, statement: &Statement, contract: WidthContract) {
        match &statement.kind {
            StatementKind::Native(native) => {
                // Classification is shared with lowering, so the two cannot
                // disagree about which addressing mode an operand selects.
                let mut resolver = self.resolver().with_widths(contract);
                let classified = resolver.classify(native);
                self.diagnostics.append(&mut resolver.take_diagnostics());
                if let Some(classified) = classified {
                    self.check_addressable(native, &classified);
                }
            }
            StatementKind::Hla(HlaStatement::Assign(assign)) => {
                match (assign.targets.as_slice(), &assign.source.kind) {
                    // `a = #imm` sizes like the native `lda #imm`.
                    ([target], HlaOperandKind::Immediate(value))
                        if is_accumulator(&target.kind) =>
                    {
                        self.check_immediate(
                            value,
                            Some(ImmediateWidthClass::MDependent),
                            contract,
                            Mnemonic::Lda,
                        );
                    }
                    // `MEM = a` is the store the slice compiles.
                    ([target], HlaOperandKind::Register(register))
                        if register.kind == RegisterKind::A =>
                    {
                        match plain_symbol(&target.kind) {
                            Some(name) if name.provenance != Provenance::Synthetic => {
                                self.check_memory_operand(&name.text, name.span);
                            }
                            Some(_) => {}
                            None => self.diagnostics.push(compat::unimplemented(
                                statement.span,
                                "these assignment targets",
                                compat::Owner::HlaLowering,
                            )),
                        }
                    }
                    _ => self.diagnostics.push(compat::unimplemented(
                        statement.span,
                        "these assignment chains",
                        compat::Owner::HlaLowering,
                    )),
                }
            }
            // Everything else in the HLA statement family is recognized but
            // not lowered yet.
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
                self.diagnostics.push(compat::unimplemented(
                    statement.span,
                    "these HLA shorthands",
                    compat::Owner::HlaLowering,
                ));
            }
            // Both label scopes were declared by the first pass, so a label
            // statement has nothing left to check here.
            StatementKind::Label(_) => {}
            // `return` is the slice's; `return_i` and everything else in the
            // section family belongs to later work.
            StatementKind::Return(ReturnKind::Subroutine) | StatementKind::Error => {}
            StatementKind::Return(ReturnKind::Interrupt) => {
                self.diagnostics.push(compat::unimplemented(
                    statement.span,
                    "`return_i` statements",
                    compat::Owner::HlaLowering,
                ));
            }
            StatementKind::WidthBlock(_) => self.diagnostics.push(compat::unimplemented(
                statement.span,
                "scoped width blocks",
                compat::Owner::WidthAndContracts,
            )),
            // Branches, loops, and jumps are recognized but not lowered yet.
            StatementKind::Conditional(_)
            | StatementKind::ConditionalJump(_)
            | StatementKind::Loop(_)
            | StatementKind::Goto(_)
            | StatementKind::Break
            | StatementKind::Repeat
            | StatementKind::Nocross(_) => self.diagnostics.push(compat::unimplemented(
                statement.span,
                "branch and loop statements",
                compat::Owner::HlaLowering,
            )),
            StatementKind::Data(data) => {
                self.diagnostics.push(compat::unimplemented(
                    statement.span,
                    "inline `data` blocks",
                    compat::Owner::NativeTranslator,
                ));
                self.report_reserved_in_data(data);
            }
            StatementKind::Preprocessor(_) => self.diagnostics.push(compat::unimplemented(
                statement.span,
                "preprocessor directives",
                compat::Owner::Compatibility,
            )),
            StatementKind::Call(_) | StatementKind::BareCall(_) => {
                self.diagnostics.push(compat::unimplemented(
                    statement.span,
                    "function calls",
                    compat::Owner::WidthAndContracts,
                ));
            }
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
        let Some(folded) = self.fold(value) else {
            return;
        };
        let Some(number) = folded.value().map(|value| value.get()) else {
            // A link-time immediate such as `#&&label` is legal — ca65
            // relocates it — but K816 cannot range-check an address ld65
            // has not placed, and lowering does not carry a symbolic
            // immediate until operand classification lands (task C4).
            self.diagnostics.push(compat::unimplemented(
                value.span,
                "link-time values in immediate operands",
                compat::Owner::NativeTranslator,
            ));
            return;
        };
        let Some(width) = width else {
            return;
        };
        let Some(limit) = width.max_value(contract.a, contract.i) else {
            // Only the width-dependent classes can fail to report a limit,
            // and they fail exactly when their axis is unconstrained.
            let (axis, help) = match width {
                ImmediateWidthClass::MDependent => {
                    ("accumulator", "add @a8 or @a16 to the enclosing function")
                }
                ImmediateWidthClass::XDependent => (
                    "index register",
                    "add @i8 or @i16 to the enclosing function",
                ),
                ImmediateWidthClass::Fixed8 | ImmediateWidthClass::Fixed16 => {
                    unreachable!("fixed immediate widths always report a limit")
                }
            };
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::UNKNOWN_WIDTH,
                    format!(
                        "`{}` uses a width-dependent immediate but {axis} width is unknown",
                        mnemonic.ca65()
                    ),
                    Label::new(value.span, "cannot size this immediate"),
                )
                .with_help(help)
                .with_docs("doc/register-width-aware-syntax.md"),
            );
            return;
        };
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

    /// Reports every reserved legacy construct nested inside a data block.
    ///
    /// A reserved construct keeps its own `K816-K65-####` diagnostic wherever
    /// it appears, not only when it *is* the whole item: an `image` entry and
    /// a `size(...)` call inside an expression are as reserved as a top-level
    /// `image NAME = "file"`, and reporting only the enclosing block as
    /// not-yet-implemented would hide which construct the compiler refuses.
    fn report_reserved_in_data(&mut self, data: &DataBlock) {
        for entry in &data.entries {
            match &entry.kind {
                DataEntryKind::Reserved(reserved) => {
                    let diagnostic = reserved_diagnostic(reserved);
                    self.diagnostics.push(diagnostic);
                }
                DataEntryKind::Align(expr)
                | DataEntryKind::Address(expr)
                | DataEntryKind::Nocross(Some(expr)) => self.report_reserved_in_expr(expr),
                DataEntryKind::Typed { values, .. } => {
                    for value in values {
                        if let DataValue::Expr(expr) = value {
                            self.report_reserved_in_expr(expr);
                            self.check_data_atom(expr);
                        }
                    }
                }
                DataEntryKind::Value(DataValue::Expr(expr)) => {
                    self.report_reserved_in_expr(expr);
                    self.check_data_atom(expr);
                }
                DataEntryKind::Repeat { count, body } => {
                    self.report_reserved_in_expr(count);
                    self.report_reserved_in_data(body);
                }
                DataEntryKind::ForEval { from, to, body, .. } => {
                    self.report_reserved_in_expr(from);
                    self.report_reserved_in_expr(to);
                    self.report_reserved_in_expr(body);
                }
                DataEntryKind::Code { body, .. } => {
                    for statement in &body.statements {
                        if let StatementKind::Data(nested) = &statement.kind {
                            self.report_reserved_in_data(nested);
                        }
                    }
                }
                DataEntryKind::Segment(_)
                | DataEntryKind::Nocross(None)
                | DataEntryKind::Charset(_)
                | DataEntryKind::Label(_)
                | DataEntryKind::Value(_)
                | DataEntryKind::Preprocessor(_)
                | DataEntryKind::Binary(_)
                | DataEntryKind::Error => {}
            }
        }
    }

    /// Enforces the atom-only rule for data-block value lists.
    ///
    /// Each value must be a single atom: a literal, an identifier, an
    /// address-byte operator over a symbol with at most `+`/`-` offset
    /// arithmetic, or `?` (`doc/syntax-reference.md`, "Where compound
    /// expressions are accepted"). The grammar already refuses most compound
    /// shapes; parentheses are the escape hatch it cannot close, because
    /// `&<(A | B)` is a legal *parse* of an illegal value. A compound
    /// expression belongs in a `const` that pre-folds it.
    fn check_data_atom(&mut self, expr: &Expr) {
        let Some(compound) = first_compound(expr) else {
            return;
        };
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::COMPOUND_DATA_VALUE,
                "data value lists take single atoms, not compound expressions",
                Label::new(compound, "this operator is not allowed in a value list"),
            )
            .with_note(
                "a value list separates entries by whitespace, so an operator between \
                 two of them would silently merge them into one",
            )
            .with_help("declare a `const` that folds the expression and list the `const`")
            .with_docs("doc/syntax-reference.md#where-compound-expressions-are-accepted"),
        );
    }

    /// Reports reserved evaluator calls anywhere inside `expr`.
    fn report_reserved_in_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Eval(inner) => self.report_reserved_in_eval(inner),
            ExprKind::Unary(unary) => self.report_reserved_in_expr(&unary.operand),
            ExprKind::Binary(binary) => {
                self.report_reserved_in_expr(&binary.left);
                self.report_reserved_in_expr(&binary.right);
            }
            ExprKind::Field(access) => self.report_reserved_in_expr(&access.base),
            ExprKind::Subscript(subscript) => {
                self.report_reserved_in_expr(&subscript.base);
                self.report_reserved_in_expr(&subscript.index);
            }
            ExprKind::Metadata(query) => self.report_reserved_in_expr(&query.base),
            ExprKind::TypedView(view) => self.report_reserved_in_expr(&view.base),
            ExprKind::Number(_)
            | ExprKind::Symbol(_)
            | ExprKind::LocalSymbol(_)
            | ExprKind::Error => {}
        }
    }

    /// `[ 1 + size(section) ]` is arithmetic around a reserved call and
    /// `[ max(size(a), 1) ]` nests one, so the whole evaluator tree is walked.
    fn report_reserved_in_eval(&mut self, expr: &EvalExpr) {
        match &expr.kind {
            EvalExprKind::ReservedCall(call) => {
                let diagnostic = compat::k65_reserved(
                    DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
                    call.keyword_span,
                    eval_call_text(call.name),
                );
                self.diagnostics.push(diagnostic);
                for argument in &call.arguments {
                    self.report_reserved_in_eval(argument);
                }
            }
            EvalExprKind::Call { arguments, .. } => {
                for argument in arguments {
                    self.report_reserved_in_eval(argument);
                }
            }
            EvalExprKind::Unary { operand, .. } => self.report_reserved_in_eval(operand),
            EvalExprKind::Binary { left, right, .. } => {
                self.report_reserved_in_eval(left);
                self.report_reserved_in_eval(right);
            }
            EvalExprKind::Ternary {
                condition,
                consequence,
                alternative,
            } => {
                self.report_reserved_in_eval(condition);
                self.report_reserved_in_eval(consequence);
                self.report_reserved_in_eval(alternative);
            }
            EvalExprKind::Assignment { target, value, .. } => {
                self.report_reserved_in_eval(target);
                self.report_reserved_in_eval(value);
            }
            EvalExprKind::Subscript { base, index } => {
                self.report_reserved_in_eval(base);
                self.report_reserved_in_eval(index);
            }
            EvalExprKind::Member { base, .. } => self.report_reserved_in_eval(base),
            EvalExprKind::List(items) => {
                for item in items {
                    self.report_reserved_in_eval(item);
                }
            }
            EvalExprKind::Number(_)
            | EvalExprKind::Float(_)
            | EvalExprKind::String(_)
            | EvalExprKind::Symbol(_)
            | EvalExprKind::Error => {}
        }
    }

    /// A resolver over this unit's symbols, scoped to the function being
    /// checked. Analysis owns the diagnostics it produces.
    fn resolver(&self) -> Resolver<'_> {
        Resolver::new(&self.model.symbols).in_function(self.current_function)
    }

    /// Folds `expr`, reporting whatever the fold turns up.
    fn fold(&mut self, expr: &Expr) -> Option<KExpr> {
        let mut resolver = self.resolver();
        let folded = resolver.fold(expr);
        self.diagnostics.append(&mut resolver.take_diagnostics());
        folded
    }

    /// The absolute value of `expr` in a position that requires one.
    fn constant(&mut self, expr: &Expr, position: &str) -> Option<u32> {
        let mut resolver = self.resolver();
        let value = resolver.constant(expr, position);
        self.diagnostics.append(&mut resolver.take_diagnostics());
        value
    }

    /// Declares one `const` group, applying the enum-like auto-increment.
    ///
    /// An entry without a value continues the previous one; a group that
    /// starts without one starts at zero. A *standalone* declaration with no
    /// initializer is an error rather than an implicit zero, because the
    /// shorthand only exists to number a comma-separated group
    /// (`doc/syntax-reference.md`, "Constant Declaration").
    fn declare_const_group(&mut self, declaration: &crate::ast::ConstDeclaration) {
        let standalone = declaration.entries.len() == 1;
        // `None` after an entry whose expression did not fold: the sequence
        // is broken, and guessing a successor would invent a value.
        let mut next = Some(0u32);
        for entry in &declaration.entries {
            let value = match &entry.value {
                Some(expression) => self.constant(expression, "`const` values"),
                None if standalone => {
                    self.diagnostics.push(
                        Diagnostic::error(
                            DiagnosticCode::NOT_CONSTANT,
                            format!("`const {}` has no value", entry.name.text),
                            Label::new(entry.span, "a standalone `const` needs an initializer"),
                        )
                        .with_note(
                            "the auto-increment shorthand only applies inside a \
                             comma-separated group",
                        )
                        .with_fix(Fix {
                            title: "give the constant a value".to_owned(),
                            applicability: Applicability::MaybeIncorrect,
                            edits: vec![TextEdit::replace(
                                Span::empty_at(entry.span.file, entry.span.end),
                                " = 0",
                            )],
                        })
                        .with_docs("doc/syntax-reference.md#constant-declaration"),
                    );
                    None
                }
                None => next,
            };
            next = value.map(|value| value.wrapping_add(1));
            if entry.name.provenance == Provenance::Synthetic {
                continue; // Recovery invented the name; a syntax error covers it.
            }
            if let Some(value) = value {
                self.declare(
                    &entry.name.text,
                    SymbolKind::Const { value },
                    entry.name.span,
                );
            }
        }
    }

    /// Rejects a symbol used in a role its declaration does not allow.
    ///
    /// Classification already checked the *shape* of an operand against the
    /// ISA table; this is the other axis — whether the name in it denotes
    /// something an operand may address at all. A `const` in address
    /// position is its own diagnostic, reported during classification.
    fn check_addressable(
        &mut self,
        native: &crate::ast::NativeInstruction,
        classified: &Classified,
    ) {
        let Some(operand) = &classified.operand else {
            return;
        };
        // A jump, call, or branch target names code, so a function is
        // exactly what belongs there. Only an operand the instruction reads
        // or writes as data has to name storage.
        if classified.instruction().effects().control != ControlEffect::FallThrough {
            return;
        }
        let expr = match operand {
            crate::op::Operand::Address { expr, .. }
            | crate::op::Operand::Indirect { expr, .. }
            | crate::op::Operand::StackRelative(expr) => expr,
            // An immediate, a branch target, and a block move's bank bytes
            // name values rather than storage.
            crate::op::Operand::Immediate(_)
            | crate::op::Operand::Label(_)
            | crate::op::Operand::BlockMove { .. } => return,
        };
        let mut referenced = Vec::new();
        expr.symbols(&mut referenced);
        for id in referenced {
            let symbol = self.model.symbols.symbol(id);
            if symbol.kind.is_addressable() {
                continue;
            }
            let name = symbol.name.clone();
            let declaration = symbol.declaration;
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::SYMBOL_KIND_MISMATCH,
                    format!("`{name}` is a function, not a variable"),
                    Label::new(native.mnemonic.span, "cannot be used as a memory operand"),
                )
                .with_secondary(Label::new(declaration, "declared here"))
                .with_help(format!(
                    "call it with `call {name}`, or take its address with `&&{name}`"
                )),
            );
        }
    }

    /// Validates a memory operand named by an HLA statement.
    fn check_memory_operand(&mut self, name: &str, span: Span) {
        let Some(id) = self.model.symbols.lookup(name) else {
            let mut resolver = self.resolver();
            resolver.resolve(name, span);
            self.diagnostics.append(&mut resolver.take_diagnostics());
            return;
        };
        let symbol = self.model.symbols.symbol(id);
        let declaration = symbol.declaration;
        match symbol.kind {
            SymbolKind::Var { .. } | SymbolKind::Label { .. } | SymbolKind::DataLabel { .. } => {}
            SymbolKind::Const { .. } => self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::CONST_IN_ADDRESS_POSITION,
                    format!("`{name}` is a `const`, not a memory location"),
                    Label::new(span, "a `const` names a value, not an address"),
                )
                .with_secondary(Label::new(declaration, "declared here"))
                .with_fix(Fix {
                    title: format!("load the value with `#{name}`"),
                    applicability: Applicability::MachineApplicable,
                    edits: vec![TextEdit::insert(span, "#")],
                })
                .with_docs("doc/syntax-reference.md#65816-addressing-modes-in-k65"),
            ),
            SymbolKind::Function { .. } => self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::SYMBOL_KIND_MISMATCH,
                    format!("`{name}` is a function, not a variable"),
                    Label::new(span, "cannot be used as a memory operand"),
                )
                .with_secondary(Label::new(declaration, "declared here"))
                .with_help(format!(
                    "call it with `call {name}`, or take its address with `&&{name}`"
                )),
            ),
        }
    }
}

/// The span of the first operator that makes `expr` compound rather than an
/// atom, or `None` when it is a legal data-value atom.
///
/// An address-byte operator is an atom, and so is symbol±offset arithmetic
/// underneath one — `&<ptr+1` is the documented spelling. Everything else
/// with an operator in it is compound.
fn first_compound(expr: &Expr) -> Option<Span> {
    match &expr.kind {
        ExprKind::Number(_)
        | ExprKind::Symbol(_)
        | ExprKind::LocalSymbol(_)
        | ExprKind::Eval(_)
        | ExprKind::Error => None,
        ExprKind::Unary(unary) => match unary.op {
            ast::UnaryOp::LowByte
            | ast::UnaryOp::HighByte
            | ast::UnaryOp::Address16
            | ast::UnaryOp::Address24 => first_non_offset(&unary.operand),
            ast::UnaryOp::Negate | ast::UnaryOp::BitNot => Some(unary.op_span),
        },
        ExprKind::Binary(binary) => Some(binary.op_span),
        ExprKind::Field(_)
        | ExprKind::Subscript(_)
        | ExprKind::Metadata(_)
        | ExprKind::TypedView(_) => Some(expr.span),
    }
}

/// The span of the first operator inside an address-byte operator's operand
/// that is not plain `+`/`-` offset arithmetic.
fn first_non_offset(expr: &Expr) -> Option<Span> {
    match &expr.kind {
        ExprKind::Number(_) | ExprKind::Symbol(_) | ExprKind::LocalSymbol(_) => None,
        ExprKind::Binary(binary) => match binary.op {
            ast::BinaryOp::Add | ast::BinaryOp::Sub => {
                first_non_offset(&binary.left).or_else(|| first_non_offset(&binary.right))
            }
            ast::BinaryOp::Mul
            | ast::BinaryOp::Shl
            | ast::BinaryOp::Shr
            | ast::BinaryOp::BitAnd
            | ast::BinaryOp::BitXor
            | ast::BinaryOp::BitOr => Some(binary.op_span),
        },
        ExprKind::Unary(unary) => Some(unary.op_span),
        ExprKind::Field(_)
        | ExprKind::Subscript(_)
        | ExprKind::Metadata(_)
        | ExprKind::TypedView(_)
        | ExprKind::Eval(_) => Some(expr.span),
        ExprKind::Error => None,
    }
}

/// The `K816-K65-####` diagnostic for a reserved legacy construct.
fn reserved_diagnostic(reserved: &crate::ast::ReservedConstruct) -> Diagnostic {
    let (code, construct) = match reserved.kind {
        ReservedKind::ImageBlock | ReservedKind::ImageTopLevel => {
            (DiagnosticCode::K65_RESERVED_IMAGE, "image")
        }
        ReservedKind::EvalCall(name) => (
            DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
            eval_call_text(name),
        ),
    };
    compat::k65_reserved(code, reserved.keyword_span, construct)
}

/// The source spelling of a reserved evaluator function.
fn eval_call_text(name: EvalCallName) -> &'static str {
    match name {
        EvalCallName::Size => "size",
        EvalCallName::AddByte => "addbyte",
        EvalCallName::Color => "color",
        EvalCallName::Rnd => "rnd",
        EvalCallName::Print => "print",
        EvalCallName::Error => "error",
    }
}

/// Names the first part of a code section that has no semantics yet, or
/// `None` for the plain `func` the slice compiles.
fn unsupported_function_feature(func: &Function) -> Option<&'static str> {
    if func.is_far {
        Some("`far func` sections")
    } else if func.contract.is_some() {
        Some("call contracts")
    } else {
        match func.kind {
            FunctionKind::Func | FunctionKind::Main => None,
            FunctionKind::Naked => Some("`naked` sections"),
            FunctionKind::Inline => Some("`inline` sections"),
        }
    }
}

/// True when this operand is the accumulator, the only register the slice
/// loads.
fn is_accumulator(kind: &HlaOperandKind) -> bool {
    matches!(kind, HlaOperandKind::Register(register) if register.kind == RegisterKind::A)
}

/// The plain symbol an assignment target names, if that is all it is: no
/// index, no encoding prefix, no arithmetic.
fn plain_symbol(kind: &HlaOperandKind) -> Option<&crate::ast::Name> {
    let HlaOperandKind::Memory(memory) = kind else {
        return None;
    };
    if memory.encoding.is_some() || memory.index.is_some() || memory.offset_marker.is_some() {
        return None;
    }
    match &memory.value.kind {
        ExprKind::Symbol(name) => Some(name),
        _ => None,
    }
}

/// Names the first part of `var` that has no semantics yet, or `None` when
/// the declaration is the fixed-address form the slice compiles.
fn unsupported_var_feature(var: &crate::ast::VarDeclaration) -> Option<&'static str> {
    if var.is_abstract {
        Some("`abstract var` layouts")
    } else if !var.fields.is_empty() {
        Some("symbolic subscript field lists")
    } else if var.storage.is_some() && var.address.is_none() {
        // On a pinned variable the prefix only picks the encoding plain
        // references default to, which is the address-size axis C owns. On
        // an allocated one it also asks for a storage pool — the direct-page
        // pool, or far storage — and that allocation is E's.
        Some("`dp` / `abs` / `far` storage classes on allocated variables")
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

/// The address size a `dp` / `abs` / `far` prefix forces.
fn address_size_of(kind: crate::ast::StorageClassKind) -> AddressSize {
    match kind {
        crate::ast::StorageClassKind::DirectPage => AddressSize::DirectPage,
        crate::ast::StorageClassKind::Absolute => AddressSize::Absolute,
        crate::ast::StorageClassKind::Far => AddressSize::Long,
    }
}
