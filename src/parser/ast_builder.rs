/*****************************************************************************/
/*                                                                           */
/*                               ast_builder.rs                              */
/*                                                                           */
/*                        CST to owned AST conversion                        */
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

//! CST → owned AST conversion.
//!
//! All generated node-name matching is localized here. Unexpected CST shapes
//! produce internal diagnostics and owned error nodes — never a panic — so a
//! malformed file still yields a navigable AST.

use crate::ast::{
    AddressEncoding, AddressOperand, AlignDirective, ArithmeticOp, AstFile, BareCall,
    BinaryDeclaration, BinaryExpr, BinaryInclusion, BinaryOp, Block, BlockMoveOperand,
    BranchCondition, BranchConditionKind, CallArgument, CallContract, CallStatement,
    ComparisonKind, ConditionalBlock, ConditionalJump, ConstDeclaration, ConstEntry,
    ContractOutputs, ContractParameter, DataBlock, DataEntry, DataEntryKind, DataValue,
    EvalCallName, EvalExpr, EvalExprKind, EvalFunction, Expr, ExprKind, FieldAccess,
    FieldDeclaration, FlagKind, Function, FunctionKind, GotoStatement, HlaArithmetic,
    HlaAssignment, HlaExchange, HlaFlag, HlaOperand, HlaOperandKind, HlaShift, HlaStack,
    HlaStatement, HlaStep, IllegalOpcode, IndexRegister, IndexRegisterKind, IndirectOperand, Item,
    ItemKind, LabelScope, LabelStatement, LongIndirectOperand, LoopCondition, LoopStatement,
    MetadataQuery, MetadataQueryKind, Name, NativeInstruction, NativeOperand,
    PreprocessorDirective, PreprocessorKind, Register, RegisterKind, ReservedCall,
    ReservedConstruct, ReservedKind, ReturnKind, ShiftOp, StackOp, Statement, StatementKind,
    StepOp, StmtId, StorageClass, StorageClassKind, SubscriptExpr, TestableFlag, TypeWidth,
    TypeWidthKind, TypedViewExpr, UnaryExpr, UnaryOp, VarDeclaration, WidthAnnotation,
    WidthAnnotationKind, WidthBlock,
};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::source::{Provenance, SourceFile, Span};

use super::cst::node_span;

type Node<'t> = tree_sitter::Node<'t>;

pub(super) fn build(root: Node<'_>, source: &SourceFile) -> (AstFile, Vec<Diagnostic>) {
    let mut builder = AstBuilder {
        source,
        diagnostics: Vec::new(),
        // Whether tree-sitter had to recover anywhere in this file, which is
        // what decides if an unrecognized shape is a K816 bug or debris.
        recovered: root.has_error(),
    };
    let mut file = builder.file(root);
    assign_statement_ids(&mut file);
    (file, builder.diagnostics)
}

/// Numbers every statement in `file`, in source order.
///
/// A separate pass rather than a counter inside the builder: the builder
/// finishes a nested block before the statement that contains it, so
/// construction order is not source order. One pre-order walk here gives the
/// stable key analysis and lowering both look up
/// (`plans/03-width-and-contracts.md` contract 8).
pub(crate) fn assign_statement_ids(file: &mut AstFile) {
    let mut next = 0u32;
    for item in &mut file.items {
        match &mut item.kind {
            ItemKind::Func(func) => number_block(&mut func.body, &mut next),
            ItemKind::Data(data) => number_data(data, &mut next),
            ItemKind::Var(_)
            | ItemKind::Const(_)
            | ItemKind::Segment(_)
            | ItemKind::EvalBlock(_)
            | ItemKind::EvalFunc(_)
            | ItemKind::Preprocessor(_)
            | ItemKind::Binary(_)
            | ItemKind::Reserved(_)
            | ItemKind::ModuleWidth(_)
            | ItemKind::Error => {}
        }
    }
}

fn number_block(block: &mut Block, next: &mut u32) {
    for statement in &mut block.statements {
        number_statement(statement, next);
    }
}

/// Numbers one statement and, before moving on, everything nested inside it.
///
/// The match is exhaustive so that a statement form added later cannot carry
/// nested statements the numbering silently skips — an unnumbered statement
/// is a lookup failure far from its cause.
fn number_statement(statement: &mut Statement, next: &mut u32) {
    statement.id = StmtId::new(*next);
    *next += 1;
    match &mut statement.kind {
        StatementKind::WidthBlock(width) => number_block(&mut width.body, next),
        StatementKind::Nocross(body) => number_block(body, next),
        StatementKind::Loop(loop_statement) => number_block(&mut loop_statement.body, next),
        StatementKind::Conditional(conditional) => {
            number_block(&mut conditional.body, next);
            if let Some(otherwise) = &mut conditional.otherwise {
                number_block(otherwise, next);
            }
        }
        StatementKind::ConditionalJump(jump) => number_statement(&mut jump.jump, next),
        StatementKind::Data(data) => number_data(data, next),
        StatementKind::Label(_)
        | StatementKind::Native(_)
        | StatementKind::Hla(_)
        | StatementKind::ModeSet(_)
        | StatementKind::Call(_)
        | StatementKind::BareCall(_)
        | StatementKind::Goto(_)
        | StatementKind::Break
        | StatementKind::Repeat
        | StatementKind::Preprocessor(_)
        | StatementKind::Return(_)
        | StatementKind::Align(_)
        | StatementKind::Address(_)
        | StatementKind::Reserved(_)
        | StatementKind::Error => {}
    }
}

/// A data block's `code { ... }` entries hold statements too.
fn number_data(data: &mut DataBlock, next: &mut u32) {
    for entry in &mut data.entries {
        match &mut entry.kind {
            DataEntryKind::Code { body, .. } => number_block(body, next),
            DataEntryKind::Repeat { body, .. } => number_data(body, next),
            DataEntryKind::NocrossGroup(entries) => {
                for entry in entries {
                    if let DataEntryKind::Code { body, .. } = &mut entry.kind {
                        number_block(body, next);
                    }
                }
            }
            DataEntryKind::Evaluator(_)
            | DataEntryKind::Label(_)
            | DataEntryKind::Value(_)
            | DataEntryKind::Typed { .. }
            | DataEntryKind::Align(_)
            | DataEntryKind::Address(_)
            | DataEntryKind::Segment(_)
            | DataEntryKind::Nocross
            | DataEntryKind::Charset(_)
            | DataEntryKind::ForEval { .. }
            | DataEntryKind::Preprocessor(_)
            | DataEntryKind::Binary(_)
            | DataEntryKind::Reserved(_)
            | DataEntryKind::Error => {}
        }
    }
}

struct AstBuilder<'a> {
    source: &'a SourceFile,
    diagnostics: Vec<Diagnostic>,
    /// True when the tree contains an `ERROR` or `MISSING` node anywhere.
    recovered: bool,
}

impl AstBuilder<'_> {
    fn span(&self, node: Node<'_>) -> Span {
        node_span(node, self.source)
    }

    /// `node`'s span, extended back over a preceding `->` if there is one.
    fn with_arrow(&self, node: Node<'_>) -> Span {
        let span = self.span(node);
        match node.prev_sibling() {
            Some(arrow) if arrow.kind() == "->" => {
                Span::new(span.file, node_span(arrow, self.source).start, span.end)
            }
            _ => span,
        }
    }

    fn text(&self, node: Node<'_>) -> &str {
        self.source.span_text(node_span(node, self.source))
    }

    /// Records an internal diagnostic for a CST shape this builder does not
    /// recognize. Reaching this is a grammar/builder mismatch, not user error.
    ///
    /// Which is exactly why it stays quiet once tree-sitter has recovered
    /// anywhere in the file. Recovery re-parents whatever it could not fit —
    /// a `func` whose closing brace has not been typed yet leaves its
    /// statements as children of the file root — so the shape the builder
    /// meets says nothing about the grammar, and every prefix of a real
    /// program would otherwise be reported as a compiler bug. The syntax
    /// diagnostic is the one the author needs, and it has already been
    /// collected. A builder bug in a file that parses cleanly still reports,
    /// which is the case this code was written for.
    fn unexpected(&mut self, node: Node<'_>, context: &str) {
        if self.recovered {
            return;
        }
        self.diagnostics.push(Diagnostic::error(
            DiagnosticCode::INTERNAL,
            format!("internal: unexpected `{}` node in {context}", node.kind()),
            Label::new(self.span(node), "produced by the parser here"),
        ));
    }

    fn file(&mut self, root: Node<'_>) -> AstFile {
        let mut items = Vec::new();
        let mut cursor = root.walk();
        for child in root.named_children(&mut cursor) {
            match child.kind() {
                "var_declaration" => items.extend(self.var_declaration(child)),
                "const_declaration" => items.push(self.const_declaration(child)),
                "module_width" => {
                    if let Some(width) = node_width(self, child) {
                        items.push(Item {
                            kind: ItemKind::ModuleWidth(width),
                            span: self.span(child),
                        });
                    }
                }
                "segment_declaration" => items.push(Item {
                    kind: ItemKind::Segment(self.name(child.child_by_field_name("name"), child)),
                    span: self.span(child),
                }),
                "func_declaration" => items.push(self.func_declaration(child)),
                "data_block" => items.push(Item {
                    kind: ItemKind::Data(self.data_block(child)),
                    span: self.span(child),
                }),
                "preprocessor_directive" => items.push(Item {
                    kind: ItemKind::Preprocessor(self.preprocessor_directive(child)),
                    span: self.span(child),
                }),
                "binary_declaration" => items.push(Item {
                    kind: ItemKind::Binary(BinaryDeclaration {
                        name: self.name(child.child_by_field_name("name"), child),
                        file: self.binary_inclusion(child),
                    }),
                    span: self.span(child),
                }),
                "reserved_image_toplevel" => {
                    let reserved = self.reserved(child, ReservedKind::ImageTopLevel);
                    items.push(Item {
                        kind: ItemKind::Reserved(reserved),
                        span: self.span(child),
                    });
                }
                "eval_block" => items.push(Item {
                    kind: ItemKind::EvalBlock(
                        self.expr(child.child_by_field_name("expression"), child),
                    ),
                    span: self.span(child),
                }),
                "bank_declaration" => {
                    let reserved = self.reserved_at(child, ReservedKind::Bank, child.child(0));
                    items.push(Item {
                        kind: ItemKind::Reserved(reserved),
                        span: self.span(child),
                    });
                }
                "evalfunc_declaration" => {
                    let mut parameters = Vec::new();
                    let mut cursor = child.walk();
                    for parameter in child.children_by_field_name("parameter", &mut cursor) {
                        parameters.push(self.name(Some(parameter), child));
                    }
                    let body = child.child_by_field_name("body");
                    items.push(Item {
                        kind: ItemKind::EvalFunc(EvalFunction {
                            name: self.name(child.child_by_field_name("name"), child),
                            parameters,
                            body: match body {
                                Some(body) => self.eval_bracket(body),
                                None => EvalExpr {
                                    kind: EvalExprKind::Error,
                                    span: self.span(child),
                                },
                            },
                        }),
                        span: self.span(child),
                    });
                }
                "line_comment" | "block_comment" => {}
                "ERROR" => items.push(Item {
                    kind: ItemKind::Error,
                    span: self.span(child),
                }),
                _ => {
                    self.unexpected(child, "the file root");
                    items.push(Item {
                        kind: ItemKind::Error,
                        span: self.span(child),
                    });
                }
            }
        }
        AstFile { items }
    }

    /// One `var` keyword, one [`Item`] per entry: `var a, b` is `var a` and
    /// `var b`, which is all the comma is. The first entry carries the whole
    /// declaration's span so that a single-name declaration — every one
    /// written before the group form existed — points where it always did.
    fn var_declaration(&mut self, node: Node<'_>) -> Vec<Item> {
        let is_abstract = node.child_by_field_name("abstract").is_some();
        let mut cursor = node.walk();
        let entries: Vec<Node<'_>> = node.children_by_field_name("entry", &mut cursor).collect();
        let mut items = Vec::with_capacity(entries.len());
        for (index, entry) in entries.into_iter().enumerate() {
            let span = if index == 0 {
                self.span(node)
            } else {
                self.span(entry)
            };
            items.push(self.var_entry(entry, is_abstract, span));
        }
        items
    }

    fn var_entry(&mut self, node: Node<'_>, is_abstract: bool, span: Span) -> Item {
        let name = self.name(node.child_by_field_name("name"), node);
        let storage = node
            .child_by_field_name("storage")
            .and_then(|storage| self.storage_class(storage));
        let width = node
            .child_by_field_name("type")
            .and_then(|width| self.type_width(width));
        let size = node
            .child_by_field_name("size")
            .map(|size| self.expr(Some(size), node));
        let fields = node
            .child_by_field_name("fields")
            .map(|list| self.field_list(list))
            .unwrap_or_default();
        let count = node
            .child_by_field_name("count")
            .map(|count| self.expr(Some(count), node));
        let address = node
            .child_by_field_name("address")
            .map(|address| self.expr(Some(address), node));
        // The `?` marker is read and dropped. It set a listing flag in the
        // original and moved no byte, which G0's oracle run confirmed.
        Item {
            kind: ItemKind::Var(VarDeclaration {
                name,
                is_abstract,
                storage,
                width,
                size,
                fields,
                count,
                address,
            }),
            span,
        }
    }

    fn field_list(&mut self, node: Node<'_>) -> Vec<FieldDeclaration> {
        let mut fields = Vec::new();
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            match child.kind() {
                "field_declaration" => fields.push(FieldDeclaration {
                    name: self.name(child.child_by_field_name("name"), child),
                    width: child
                        .child_by_field_name("type")
                        .and_then(|width| self.type_width(width)),
                    size: child
                        .child_by_field_name("size")
                        .map(|size| self.expr(Some(size), child)),
                    fields: child
                        .child_by_field_name("fields")
                        .map(|list| self.field_list(list))
                        .unwrap_or_default(),
                    span: self.span(child),
                }),
                "line_comment" | "block_comment" => {}
                _ => self.unexpected(child, "a field list"),
            }
        }
        fields
    }

    /// The storage prefix is lexed as an identifier so `var dp = 0` keeps
    /// declaring a variable named `dp`; anything other than the three storage
    /// classes in that slot is a user error, reported here.
    fn storage_class(&mut self, node: Node<'_>) -> Option<StorageClass> {
        let kind = match self.text(node) {
            "dp" => StorageClassKind::DirectPage,
            "abs" => StorageClassKind::Absolute,
            "far" => StorageClassKind::Far,
            other => {
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::SYNTAX_ERROR,
                        format!("`{other}` is not a storage class"),
                        Label::new(self.span(node), "expected `dp`, `abs`, or `far`"),
                    )
                    .with_help("a `var` declaration takes at most one name")
                    .with_docs("doc/syntax-reference.md#variable-declaration"),
                );
                return None;
            }
        };
        Some(StorageClass {
            kind,
            span: self.span(node),
        })
    }

    fn type_width(&mut self, node: Node<'_>) -> Option<TypeWidth> {
        let kind = match node.child_by_field_name("width").map(|w| self.text(w)) {
            Some("byte") => TypeWidthKind::Byte,
            Some("word") => TypeWidthKind::Word,
            Some("far") => TypeWidthKind::Far,
            _ => {
                self.unexpected(node, "a type suffix");
                return None;
            }
        };
        Some(TypeWidth {
            kind,
            span: self.span(node),
        })
    }

    fn const_declaration(&mut self, node: Node<'_>) -> Item {
        let mut entries = Vec::new();
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            match child.kind() {
                "const_entry" => entries.push(ConstEntry {
                    name: self.name(child.child_by_field_name("name"), child),
                    value: child
                        .child_by_field_name("value")
                        .map(|value| self.expr(Some(value), child)),
                    span: self.span(child),
                }),
                "line_comment" | "block_comment" => {}
                _ => self.unexpected(child, "a `const` declaration"),
            }
        }
        Item {
            kind: ItemKind::Const(ConstDeclaration { entries }),
            span: self.span(node),
        }
    }

    fn func_declaration(&mut self, node: Node<'_>) -> Item {
        let name = self.name(node.child_by_field_name("name"), node);
        let kind = match node.child_by_field_name("kind").map(|kind| self.text(kind)) {
            Some("func") => FunctionKind::Func,
            Some("naked") => FunctionKind::Naked,
            Some("inline") => FunctionKind::Inline,
            // No section keyword: the legacy bare `main { ... }`.
            None => FunctionKind::Main,
            Some(_) => {
                self.unexpected(node, "a code section");
                FunctionKind::Func
            }
        };
        let width = self.width_annotations(node);
        let body = match node.child_by_field_name("body") {
            Some(body) => self.block(body),
            None => {
                // Recovery produced a function without a body; keep an empty
                // block anchored at the end of the header.
                let end = self.span(node);
                Block {
                    statements: Vec::new(),
                    span: Span::empty_at(end.file, end.end),
                }
            }
        };
        Item {
            kind: ItemKind::Func(Function {
                name,
                kind,
                is_far: self.far_marker(node),
                width,
                contract: node
                    .child_by_field_name("contract")
                    .map(|contract| self.call_contract(contract)),
                body,
            }),
            span: self.span(node),
        }
    }

    /// Whether `node` carries a `far` marker. Like the storage and encoding
    /// prefixes it is lexed as an identifier, so the word is checked here.
    fn far_marker(&mut self, node: Node<'_>) -> bool {
        let Some(marker) = node.child_by_field_name("far") else {
            return false;
        };
        if self.text(marker) == "far" {
            return true;
        }
        let span = self.span(marker);
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::SYNTAX_ERROR,
                format!("`{}` is not a call-convention prefix", self.text(marker)),
                Label::new(span, "expected `far`"),
            )
            .with_docs("doc/syntax-reference.md#far-calls"),
        );
        false
    }

    fn width_annotations(&mut self, node: Node<'_>) -> Vec<WidthAnnotation> {
        let mut width = Vec::new();
        let mut cursor = node.walk();
        for annotation in node.children_by_field_name("width", &mut cursor) {
            if let Some(parsed) = self.width_annotation(annotation) {
                width.push(parsed);
            }
        }
        width
    }

    fn call_contract(&mut self, node: Node<'_>) -> CallContract {
        let inputs = node.child_by_field_name("inputs").map(|inputs| {
            let mut parameters = Vec::new();
            let mut cursor = inputs.walk();
            for child in inputs.named_children(&mut cursor) {
                match child.kind() {
                    "contract_parameter" => parameters.push(self.contract_parameter(child)),
                    "line_comment" | "block_comment" => {}
                    _ => self.unexpected(child, "a call contract"),
                }
            }
            parameters
        });
        let outputs = node.child_by_field_name("outputs").map(|outputs| {
            let mut registers = Vec::new();
            let mut cursor = outputs.walk();
            for register in outputs.children_by_field_name("register", &mut cursor) {
                registers.push(self.register(Some(register), outputs));
            }
            ContractOutputs {
                width: self.width_annotations(outputs),
                registers,
                // The `->` is part of the clause: a diagnostic that offers to
                // delete the clause has to take the arrow with it, and one
                // that points at "the declared exit" reads better including
                // the token that introduces it.
                span: self.with_arrow(outputs),
            }
        });
        CallContract {
            inputs,
            outputs,
            span: self.span(node),
        }
    }

    fn contract_parameter(&mut self, node: Node<'_>) -> ContractParameter {
        if let Some(register) = node.child_by_field_name("register") {
            return ContractParameter::Register(self.register(Some(register), node));
        }
        let name = self.name(node.child_by_field_name("alias"), node);
        // `#name` and `#name:byte` are the immediate aliases an `inline`
        // may declare; a bare name aliases a variable or address.
        if self.text(node).starts_with('#') {
            ContractParameter::ImmediateAlias {
                name,
                width: node
                    .child_by_field_name("type")
                    .and_then(|width| self.type_width(width)),
            }
        } else {
            ContractParameter::Alias(name)
        }
    }

    fn width_annotation(&mut self, node: Node<'_>) -> Option<WidthAnnotation> {
        let kind = match self.text(node) {
            "@a8" => WidthAnnotationKind::A8,
            "@a16" => WidthAnnotationKind::A16,
            "@i8" => WidthAnnotationKind::I8,
            "@i16" => WidthAnnotationKind::I16,
            _ => {
                self.unexpected(node, "a width annotation");
                return None;
            }
        };
        Some(WidthAnnotation {
            kind,
            span: self.span(node),
        })
    }

    fn block(&mut self, node: Node<'_>) -> Block {
        let mut statements = Vec::new();
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            match child.kind() {
                "label" => statements.push(self.label(child)),
                "native_instruction" => statements.push(self.native_instruction(child)),
                "block_move" => statements.push(self.block_move(child)),
                "hla_assignment" => statements.extend(self.hla_assignment(child)),
                "hla_exchange" => statements.push(self.hla_exchange(child)),
                "hla_arithmetic" => statements.push(self.hla_arithmetic(child)),
                "hla_shift" => statements.push(self.hla_shift(child)),
                "hla_step" => statements.extend(self.hla_step(child)),
                "hla_flag" => statements.push(self.hla_flag(child)),
                "hla_stack" => statements.push(self.hla_stack(child)),
                "hla_nop" => statements.push(Statement::new(
                    StatementKind::Hla(HlaStatement::Nop {
                        count: child
                            .child_by_field_name("count")
                            .map(|count| self.expr(Some(count), child)),
                    }),
                    self.span(child),
                )),
                "hla_break" => statements.push(Statement::new(
                    StatementKind::Hla(HlaStatement::Break),
                    self.span(child),
                )),
                "return_statement" => statements.push(Statement::new(
                    StatementKind::Return(if self.text(child) == "return_i" {
                        ReturnKind::Interrupt
                    } else {
                        ReturnKind::Subroutine
                    }),
                    self.span(child),
                )),
                "width_block" => {
                    let width = self.width_annotations(child);
                    let body = match child.child_by_field_name("body") {
                        Some(body) => self.block(body),
                        None => Block {
                            statements: Vec::new(),
                            span: self.span(child),
                        },
                    };
                    statements.push(Statement::new(
                        StatementKind::WidthBlock(WidthBlock { width, body }),
                        self.span(child),
                    ));
                }
                "mode_set" => {
                    let width = self.width_annotations(child);
                    statements.push(Statement::new(
                        StatementKind::ModeSet(width),
                        self.span(child),
                    ));
                }
                "call_statement" => statements.push(Statement::new(
                    StatementKind::Call(CallStatement {
                        target: self.name(child.child_by_field_name("target"), child),
                        is_far: self.far_marker(child),
                    }),
                    self.span(child),
                )),
                "conditional_block" => statements.push(self.conditional_block(child)),
                "conditional_jump" => statements.push(self.conditional_jump(child)),
                "postfix_loop" => statements.push(self.postfix_loop(child)),
                "goto_statement" => statements.push(self.goto_statement(child)),
                "break_statement" => {
                    statements.push(Statement::new(StatementKind::Break, self.span(child)))
                }
                "repeat_statement" => {
                    statements.push(Statement::new(StatementKind::Repeat, self.span(child)))
                }
                "preprocessor_directive" => statements.push(Statement::new(
                    StatementKind::Preprocessor(self.preprocessor_directive(child)),
                    self.span(child),
                )),
                "data_block" => statements.push(Statement::new(
                    StatementKind::Data(self.data_block(child)),
                    self.span(child),
                )),
                "nocross_block" => statements.push(Statement::new(
                    StatementKind::Nocross(self.body_of(child)),
                    self.span(child),
                )),
                "bare_call" => statements.push(self.bare_call(child)),
                "hla_illegal" => statements.push(self.hla_illegal(child)),
                // A code section may open with a placement directive, as
                // the original's `Align` slot allows. The grammar takes one
                // anywhere in a body so that one further down is refused
                // with a sentence rather than `expected a statement`; the
                // position rule is `semantic.rs`'s.
                "align_directive" => statements.push(Statement::new(
                    StatementKind::Align(self.align_directive(child)),
                    self.span(child),
                )),
                "address_directive" => statements.push(Statement::new(
                    StatementKind::Address(self.expr(child.child_by_field_name("address"), child)),
                    self.span(child),
                )),
                "line_comment" | "block_comment" => {}
                "ERROR" => statements.push(Statement::new(StatementKind::Error, self.span(child))),
                _ => {
                    self.unexpected(child, "a block");
                    statements.push(Statement::new(StatementKind::Error, self.span(child)));
                }
            }
        }
        Block {
            statements,
            span: self.span(node),
        }
    }

    fn native_instruction(&mut self, node: Node<'_>) -> Statement {
        let mnemonic = self.name(node.child_by_field_name("mnemonic"), node);
        let operand = node
            .child_by_field_name("operand")
            .map(|operand| self.native_operand(operand));
        let repeat = node
            .child_by_field_name("repeat")
            .map(|repeat| self.expr(Some(repeat), node));
        Statement::new(
            StatementKind::Native(NativeInstruction {
                mnemonic,
                operand,
                repeat,
            }),
            self.span(node),
        )
    }

    fn block_move(&mut self, node: Node<'_>) -> Statement {
        let mnemonic = self.name(node.child_by_field_name("mnemonic"), node);
        let operand = NativeOperand::BlockMove(BlockMoveOperand {
            source: self.expr(node.child_by_field_name("source"), node),
            destination: self.expr(node.child_by_field_name("destination"), node),
        });
        Statement::new(
            StatementKind::Native(NativeInstruction {
                mnemonic,
                operand: Some(operand),
                repeat: None,
            }),
            self.span(node),
        )
    }

    fn native_operand(&mut self, node: Node<'_>) -> NativeOperand {
        match node.kind() {
            "immediate" => NativeOperand::Immediate(self.immediate_value(node)),
            "register" => NativeOperand::Register(self.register(Some(node), node)),
            "address" => NativeOperand::Address(AddressOperand {
                encoding: self.address_encoding(node),
                offset_marker: node
                    .child_by_field_name("marker")
                    .map(|marker| self.span(marker)),
                value: self.expr(node.child_by_field_name("value"), node),
                index: self.index_register(node.child_by_field_name("index")),
            }),
            "long_indirect" => NativeOperand::LongIndirect(LongIndirectOperand {
                encoding: self.address_encoding(node),
                value: self.expr(node.child_by_field_name("value"), node),
                outer_index: self.index_register(node.child_by_field_name("outer_index")),
            }),
            "indirect" => NativeOperand::Indirect(IndirectOperand {
                encoding: self.address_encoding(node),
                value: self.expr(node.child_by_field_name("value"), node),
                inner_index: self.index_register(node.child_by_field_name("inner_index")),
                outer_index: self.index_register(node.child_by_field_name("outer_index")),
            }),
            _ => {
                self.unexpected(node, "a native operand");
                NativeOperand::Immediate(Expr {
                    kind: ExprKind::Error,
                    span: self.span(node),
                })
            }
        }
    }

    /// The operand-position `dp` / `abs` / `far` prefix. Like the declaration
    /// prefix it arrives as an identifier, so the same three names are checked
    /// here rather than in the grammar.
    fn address_encoding(&mut self, node: Node<'_>) -> Option<AddressEncoding> {
        let node = node.child_by_field_name("encoding")?;
        let kind = match self.text(node) {
            "dp" => StorageClassKind::DirectPage,
            "abs" => StorageClassKind::Absolute,
            "far" => StorageClassKind::Far,
            other => {
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::SYNTAX_ERROR,
                        format!("`{other}` is not an address-encoding prefix"),
                        Label::new(self.span(node), "expected `dp`, `abs`, or `far`"),
                    )
                    .with_help("an operand takes at most one value expression")
                    .with_docs("doc/syntax-reference.md#address-encoding-prefixes"),
                );
                return None;
            }
        };
        Some(AddressEncoding {
            kind,
            span: self.span(node),
        })
    }

    fn index_register(&mut self, node: Option<Node<'_>>) -> Option<IndexRegister> {
        let node = node?;
        let kind = match self.text(node).to_ascii_lowercase().as_str() {
            "x" => IndexRegisterKind::X,
            "y" => IndexRegisterKind::Y,
            "s" => IndexRegisterKind::Stack,
            _ => {
                self.unexpected(node, "an index register");
                return None;
            }
        };
        Some(IndexRegister {
            kind,
            span: self.span(node),
        })
    }

    /// An assignment chain. The rightmost operand is the source; everything
    /// to its left is a target, in the order the value flows through them.
    /// One or two statements: `wsync HMOVE = a` is a call and a store.
    fn hla_assignment(&mut self, node: Node<'_>) -> Vec<Statement> {
        let mut cursor = node.walk();
        let operand_nodes: Vec<Node<'_>> = node
            .children_by_field_name("operand", &mut cursor)
            .collect();
        let mut statements = Vec::new();
        let mut split = false;
        if let Some(first) = operand_nodes.first()
            && let Some(call) = self.leading_bare_call(*first)
        {
            statements.push(call);
            split = true;
        }
        // `i+ d-` followed by `s = x = $FF` is three statements. Peeled
        // before the operands are built, because what it leaves behind is a
        // different first operand.
        let peeled = match operand_nodes.first() {
            Some(first) if !split => self.leading_flag_statements(*first),
            _ => None,
        };
        let mut operands: Vec<HlaOperand> = Vec::with_capacity(operand_nodes.len());
        let mut rest = operand_nodes.as_slice();
        let flagged = peeled.is_some();
        if let Some((flags, remainder)) = peeled {
            statements.extend(flags);
            let operand = self.offset_remainder_operand(remainder);
            operands.push(operand);
            rest = &operand_nodes[1..];
        }
        let built: Vec<HlaOperand> = rest
            .iter()
            .enumerate()
            .map(|(index, operand)| self.hla_operand_after_split(*operand, split && index == 0))
            .collect();
        operands.extend(built);
        let node_span = self.span(node);
        // The store starts where the call, or the last flag statement, ended.
        let span = match operands.first() {
            Some(first) if split || flagged => {
                Span::new(node_span.file, first.span.start, node_span.end)
            }
            _ => node_span,
        };
        let source = operands.pop().unwrap_or_else(|| {
            // The grammar guarantees at least two operands; recovery may not.
            HlaOperand {
                kind: HlaOperandKind::Error,
                span: Span::empty_at(span.file, span.end),
            }
        });
        statements.push(Statement::new(
            StatementKind::Hla(HlaStatement::Assign(HlaAssignment {
                targets: operands,
                source,
            })),
            span,
        ));
        statements
    }

    /// Decision 16: `wsync HMOVE = a` is a call and a store, not one store
    /// through an operand prefixed by `wsync`.
    ///
    /// The grammar cannot make this call. `dp` and `abs` reach the prefix
    /// slot as *identifiers*, deliberately, so that they stay usable as
    /// names — `lda dp` loads the variable `dp` — and a literal token for
    /// them would win in the lexer wherever an identifier is also valid,
    /// which breaks the operand prefix it was meant to protect. So the
    /// spelling is checked here, the one place that can see it, exactly as
    /// [`Self::address_encoding`] already checks it for operands.
    ///
    /// Returns the call statement when a statement-leading target carries a
    /// prefix that is not one of the three; the caller then builds the rest
    /// of the statement with that prefix ignored.
    /// `node`'s span with its `encoding` prefix trimmed off the front, for
    /// the statement that remains once the prefix has become a call.
    fn after_prefix(&mut self, node: Node<'_>) -> Span {
        let span = self.span(node);
        match node.child_by_field_name("value") {
            Some(value) => Span::new(span.file, self.span(value).start, span.end),
            None => span,
        }
    }

    fn leading_bare_call(&mut self, node: Node<'_>) -> Option<Statement> {
        if node.kind() != "address" {
            return None;
        }
        let encoding = node.child_by_field_name("encoding")?;
        if matches!(self.text(encoding), "dp" | "abs" | "far") {
            return None;
        }
        Some(Statement::new(
            StatementKind::BareCall(BareCall {
                target: self.name(Some(encoding), node),
                arguments: None,
                outputs: None,
            }),
            self.span(encoding),
        ))
    }

    /// The flags with a set/clear shorthand, as the source spells them.
    ///
    /// `o` is the original's name for the overflow flag and `v` is K816's;
    /// `c` and `d` name registers as well, and which one a statement meant
    /// follows from its shape rather than from the spelling.
    fn flag_spelling(&self, node: Node<'_>) -> bool {
        matches!(node.kind(), "identifier" | "register")
            && matches!(
                self.text(node).to_ascii_lowercase().as_str(),
                "c" | "d" | "i" | "v" | "o"
            )
    }

    /// `i+ d-` followed by `s = x = $FF` is three statements — SEI, CLD, and
    /// a store — not one store into `i + d - s`.
    ///
    /// K65 is newline-insensitive and a flag statement ends at its sign, so
    /// a flag name followed by `+` or `-` and then anything an expression can
    /// continue with has two complete readings. The original has no such
    /// ambiguity: `Flag` there is a terminal class distinct from `Ident`
    /// (`compiler.inc:253-260`), so `o - s` can never be an expression. K816
    /// keeps the flag names usable as identifiers on purpose — a token for
    /// them wins in the lexer wherever an identifier is also valid, and
    /// `var o` stops being a declaration (`postmortem.md` §G.1.16) — so the
    /// spelling is checked here, the one place that can see it, exactly as
    /// [`Self::leading_bare_call`] checks the operand prefix.
    ///
    /// Returns the flag statements and the node the assignment's first
    /// operand should be built from instead. Fires only when *every* name it
    /// would peel is a flag spelling, so `slot+1 = a` and `payload+2 = a`
    /// stay the single stores decision 17 says they are.
    fn leading_flag_statements<'t>(
        &mut self,
        node: Node<'t>,
    ) -> Option<(Vec<Statement>, Node<'t>)> {
        if node.kind() != "address"
            || node.child_by_field_name("index").is_some()
            || node.child_by_field_name("marker").is_some()
            || node.child_by_field_name("encoding").is_some()
        {
            return None;
        }
        // The left spine, outermost first: `(i + d) - s` is `-` over `s`,
        // then `+` over `d`, and `i` is what is left at the bottom.
        let mut chain = Vec::new();
        let mut current = node.child_by_field_name("value")?;
        while current.kind() == "binary_expression" {
            let operator = current.child_by_field_name("operator")?;
            if !matches!(self.text(operator), "+" | "-") {
                return None;
            }
            chain.push((operator, current.child_by_field_name("right")?));
            current = current.child_by_field_name("left")?;
        }
        if chain.is_empty() {
            return None;
        }
        chain.reverse();
        // Every name but the last belongs to a flag statement; the last is
        // the operand the store is actually into.
        if !self.flag_spelling(current) {
            return None;
        }
        let mut names = vec![current];
        for (_, right) in &chain[..chain.len() - 1] {
            if !self.flag_spelling(*right) {
                return None;
            }
            names.push(*right);
        }
        let remainder = chain[chain.len() - 1].1;
        let statements = names
            .into_iter()
            .zip(chain.iter())
            .map(|(name, (operator, _))| self.flag_statement(name, *operator))
            .collect();
        Some((statements, remainder))
    }

    /// One peeled flag statement, from the name and the sign that follows it.
    fn flag_statement(&mut self, name: Node<'_>, operator: Node<'_>) -> Statement {
        let flag_span = self.span(name);
        let span = Span::new(flag_span.file, flag_span.start, self.span(operator).end);
        let flag = match self.text(name).to_ascii_lowercase().as_str() {
            "c" => FlagKind::Carry,
            "d" => FlagKind::Decimal,
            "i" => FlagKind::Interrupt,
            _ => FlagKind::Overflow,
        };
        Statement::new(
            StatementKind::Hla(HlaStatement::Flag(HlaFlag {
                flag,
                set: self.text(operator) == "+",
                flag_span,
            })),
            span,
        )
    }

    /// The store target left behind once the flag statements in front of it
    /// have been peeled off.
    ///
    /// It arrived inside an expression, so a register spelling reached the
    /// tree as an identifier — `s` in `i+ d- s = x = $FF` is the stack
    /// pointer, and building it as memory would report an undeclared `s`.
    fn offset_remainder_operand(&mut self, node: Node<'_>) -> HlaOperand {
        let span = self.span(node);
        if node.kind() == "identifier"
            && matches!(
                self.text(node).to_ascii_lowercase().as_str(),
                "a" | "b" | "c" | "d" | "s" | "x" | "y" | "p" | "flag"
            )
        {
            return HlaOperand {
                kind: HlaOperandKind::Register(self.register(Some(node), node)),
                span,
            };
        }
        HlaOperand {
            kind: HlaOperandKind::Memory(AddressOperand {
                encoding: None,
                offset_marker: None,
                value: self.expr(Some(node), node),
                index: None,
            }),
            span,
        }
    }

    fn hla_operand(&mut self, node: Node<'_>) -> HlaOperand {
        self.hla_operand_after_split(node, false)
    }

    /// `split` is set when [`Self::leading_bare_call`] already took this
    /// operand's `encoding` child as a statement of its own.
    fn hla_operand_after_split(&mut self, node: Node<'_>, split: bool) -> HlaOperand {
        let span = if split {
            self.after_prefix(node)
        } else {
            self.span(node)
        };
        let kind = match node.kind() {
            "register" => HlaOperandKind::Register(self.register(Some(node), node)),
            "immediate" => HlaOperandKind::Immediate(self.immediate_value(node)),
            "address" => HlaOperandKind::Memory(AddressOperand {
                encoding: if split {
                    None
                } else {
                    self.address_encoding(node)
                },
                offset_marker: node
                    .child_by_field_name("marker")
                    .map(|marker| self.span(marker)),
                value: self.expr(node.child_by_field_name("value"), node),
                index: self.index_register(node.child_by_field_name("index")),
            }),
            "indirect" => HlaOperandKind::Indirect(IndirectOperand {
                encoding: self.address_encoding(node),
                value: self.expr(node.child_by_field_name("value"), node),
                inner_index: self.index_register(node.child_by_field_name("inner_index")),
                outer_index: self.index_register(node.child_by_field_name("outer_index")),
            }),
            _ => {
                self.unexpected(node, "an assignment chain");
                HlaOperandKind::Error
            }
        };
        HlaOperand { kind, span }
    }

    /// The operator token of `node`, plus its span. `None` (with an internal
    /// diagnostic already recorded) when recovery lost it.
    fn operator<'t>(&mut self, node: Node<'t>, context: &str) -> Option<(String, Span)> {
        match node.child_by_field_name("operator") {
            Some(operator) => Some((self.text(operator).to_owned(), self.span(operator))),
            None => {
                self.unexpected(node, context);
                None
            }
        }
    }

    fn hla_arithmetic(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let Some((operator, op_span)) = self.operator(node, "an arithmetic statement") else {
            return Statement::new(StatementKind::Error, span);
        };
        let op = match operator.as_str() {
            "+" => ArithmeticOp::Add,
            "-" => ArithmeticOp::Subtract,
            "&" => ArithmeticOp::And,
            "|" => ArithmeticOp::Or,
            "^" => ArithmeticOp::Xor,
            "?" => ArithmeticOp::Compare,
            "&?" => ArithmeticOp::BitTest,
            _ => {
                self.unexpected(node, "an arithmetic statement");
                return Statement::new(StatementKind::Error, span);
            }
        };
        let operand = match node.child_by_field_name("operand") {
            Some(operand) => self.hla_operand(operand),
            None => HlaOperand {
                kind: HlaOperandKind::Error,
                span: Span::empty_at(span.file, span.end),
            },
        };
        Statement::new(
            StatementKind::Hla(HlaStatement::Arithmetic(HlaArithmetic {
                register: self.register(node.child_by_field_name("register"), node),
                op,
                operand,
                op_span,
            })),
            span,
        )
    }

    fn hla_shift(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let Some((operator, op_span)) = self.operator(node, "a shift statement") else {
            return Statement::new(StatementKind::Error, span);
        };
        let op = match operator.as_str() {
            "<<" => ShiftOp::Left,
            ">>" => ShiftOp::Right,
            "<<<" => ShiftOp::RotateLeft,
            ">>>" => ShiftOp::RotateRight,
            _ => {
                self.unexpected(node, "a shift statement");
                return Statement::new(StatementKind::Error, span);
            }
        };
        Statement::new(
            StatementKind::Hla(HlaStatement::Shift(HlaShift {
                target: self.hla_target(node, span),
                op,
                op_span,
            })),
            span,
        )
    }

    fn hla_step(&mut self, node: Node<'_>) -> Vec<Statement> {
        let mut span = self.span(node);
        let Some((operator, op_span)) = self.operator(node, "an increment statement") else {
            return vec![Statement::new(StatementKind::Error, span)];
        };
        let op = match operator.as_str() {
            "++" => StepOp::Increment,
            "--" => StepOp::Decrement,
            _ => {
                self.unexpected(node, "an increment statement");
                return vec![Statement::new(StatementKind::Error, span)];
            }
        };
        // `wsync HMOVE++`, like the assignment form, is a call and a step.
        let mut statements = Vec::new();
        let mut split = false;
        if let Some(target) = node.child_by_field_name("target")
            && let Some(call) = self.leading_bare_call(target)
        {
            span = Span::new(span.file, self.after_prefix(target).start, span.end);
            statements.push(call);
            split = true;
        }
        let target = match node.child_by_field_name("target") {
            Some(target) => self.hla_operand_after_split(target, split),
            None => HlaOperand {
                kind: HlaOperandKind::Error,
                span: Span::empty_at(span.file, span.start),
            },
        };
        statements.push(Statement::new(
            StatementKind::Hla(HlaStatement::Step(HlaStep {
                target,
                op,
                op_span,
            })),
            span,
        ));
        statements
    }

    fn hla_target(&mut self, node: Node<'_>, span: Span) -> HlaOperand {
        match node.child_by_field_name("target") {
            Some(target) => self.hla_operand(target),
            None => HlaOperand {
                kind: HlaOperandKind::Error,
                span: Span::empty_at(span.file, span.start),
            },
        }
    }

    /// `c+` and friends. The flag name shares the register token, so the four
    /// flags that have a shorthand are checked here.
    fn hla_flag(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let flag_node = node.child_by_field_name("flag");
        let Some((operator, _)) = self.operator(node, "a flag statement") else {
            return Statement::new(StatementKind::Error, span);
        };
        let name = flag_node.map(|flag| self.text(flag).to_ascii_lowercase());
        let flag = match name.as_deref() {
            Some("c") => FlagKind::Carry,
            Some("d") => FlagKind::Decimal,
            Some("i") => FlagKind::Interrupt,
            // `o` is the original's spelling of the overflow flag; `v` is
            // K816's. One flag, two names.
            Some("v") | Some("o") => FlagKind::Overflow,
            other => {
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::SYNTAX_ERROR,
                        format!(
                            "`{}` is not a flag with a set/clear shorthand",
                            other.unwrap_or_default()
                        ),
                        Label::new(span, "expected `c`, `d`, `i`, or `v`"),
                    )
                    .with_docs("doc/syntax-reference.md#sr-flags-nvmxdizc"),
                );
                return Statement::new(StatementKind::Error, span);
            }
        };
        Statement::new(
            StatementKind::Hla(HlaStatement::Flag(HlaFlag {
                flag,
                set: operator == "+",
                flag_span: flag_node.map(|flag| self.span(flag)).unwrap_or(span),
            })),
            span,
        )
    }

    fn hla_stack(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let Some((operator, op_span)) = self.operator(node, "a stack statement") else {
            return Statement::new(StatementKind::Error, span);
        };
        Statement::new(
            StatementKind::Hla(HlaStatement::Stack(HlaStack {
                register: self.register(node.child_by_field_name("register"), node),
                op: if operator == "!!" {
                    StackOp::Push
                } else {
                    StackOp::Pull
                },
                op_span,
            })),
            span,
        )
    }

    fn hla_exchange(&mut self, node: Node<'_>) -> Statement {
        Statement::new(
            StatementKind::Hla(HlaStatement::Exchange(HlaExchange {
                left: self.register(node.child_by_field_name("left"), node),
                right: self.register(node.child_by_field_name("right"), node),
            })),
            self.span(node),
        )
    }

    /// The `body` block of `node`, or an empty one anchored at its end when
    /// recovery lost it.
    fn body_of(&mut self, node: Node<'_>) -> Block {
        match node.child_by_field_name("body") {
            Some(body) if body.kind() == "block" => self.block(body),
            _ => {
                let span = self.span(node);
                Block {
                    statements: Vec::new(),
                    span: Span::empty_at(span.file, span.end),
                }
            }
        }
    }

    /// A branch condition, in either its comparison or its flag-test
    /// spelling. The grammar accepts only these, so an unknown one is an
    /// internal mismatch rather than user error.
    fn branch_condition(&mut self, node: Option<Node<'_>>, parent: Node<'_>) -> BranchCondition {
        let parent_span = self.span(parent);
        let fallback = BranchCondition {
            kind: BranchConditionKind::Comparison(ComparisonKind::NotEqual),
            span: Span::empty_at(parent_span.file, parent_span.start),
        };
        let Some(node) = node else {
            self.unexpected(parent, "a branch condition");
            return fallback;
        };
        let text = self.text(node).to_ascii_lowercase();
        let kind = match text.as_str() {
            "<" => BranchConditionKind::Comparison(ComparisonKind::Less),
            ">=" => BranchConditionKind::Comparison(ComparisonKind::GreaterEqual),
            "==" => BranchConditionKind::Comparison(ComparisonKind::Equal),
            "!=" => BranchConditionKind::Comparison(ComparisonKind::NotEqual),
            "<0" => BranchConditionKind::Comparison(ComparisonKind::Negative),
            ">=0" => BranchConditionKind::Comparison(ComparisonKind::NotNegative),
            "<<=" => BranchConditionKind::Comparison(ComparisonKind::OverflowSet),
            ">>=" => BranchConditionKind::Comparison(ComparisonKind::OverflowClear),
            ">" => BranchConditionKind::Comparison(ComparisonKind::Greater),
            "<=" => BranchConditionKind::Comparison(ComparisonKind::LessEqual),
            flag_test => {
                let mut characters = flag_test.chars();
                let flag = match characters.next() {
                    Some('c') => TestableFlag::Carry,
                    Some('n') => TestableFlag::Negative,
                    Some('z') => TestableFlag::Zero,
                    Some('v') | Some('o') => TestableFlag::Overflow,
                    _ => {
                        self.unexpected(node, "a branch condition");
                        return fallback;
                    }
                };
                BranchConditionKind::Flag {
                    flag,
                    set: characters.next() == Some('+'),
                }
            }
        };
        BranchCondition {
            kind,
            span: self.span(node),
        }
    }

    fn conditional_block(&mut self, node: Node<'_>) -> Statement {
        let otherwise = node
            .child_by_field_name("else")
            .map(|clause| self.body_of(clause));
        Statement::new(
            StatementKind::Conditional(ConditionalBlock {
                condition: self.branch_condition(node.child_by_field_name("condition"), node),
                body: self.body_of(node),
                otherwise,
            }),
            self.span(node),
        )
    }

    fn conditional_jump(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let condition = self.branch_condition(node.child_by_field_name("condition"), node);
        let jump = match node.child_by_field_name("jump") {
            Some(jump) => match jump.kind() {
                "goto_statement" => self.goto_statement(jump),
                "break_statement" => Statement::new(StatementKind::Break, self.span(jump)),
                "repeat_statement" => Statement::new(StatementKind::Repeat, self.span(jump)),
                _ => {
                    self.unexpected(jump, "a conditional jump");
                    Statement::new(StatementKind::Error, self.span(jump))
                }
            },
            None => Statement::new(StatementKind::Error, Span::empty_at(span.file, span.end)),
        };
        Statement::new(
            StatementKind::ConditionalJump(ConditionalJump {
                condition,
                jump: Box::new(jump),
            }),
            span,
        )
    }

    fn postfix_loop(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let body_node = node.child_by_field_name("body");
        let nocross = body_node.is_some_and(|body| body.kind() == "nocross_block");
        let body = match body_node {
            Some(body) if body.kind() == "nocross_block" => self.body_of(body),
            Some(body) => self.block(body),
            None => Block {
                statements: Vec::new(),
                span: Span::empty_at(span.file, span.start),
            },
        };
        let condition_node = node.child_by_field_name("condition");
        let condition = match condition_node.map(|condition| self.text(condition)) {
            Some("always") => LoopCondition::Always,
            Some("never") => LoopCondition::Never,
            _ => LoopCondition::Branch(self.branch_condition(condition_node, node)),
        };
        Statement::new(
            StatementKind::Loop(LoopStatement {
                body,
                nocross,
                condition,
            }),
            span,
        )
    }

    fn goto_statement(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let target = self.expr(node.child_by_field_name("target"), node);
        // `goto (vec)` keeps its parentheses in the source text, which is how
        // the indirect form is told from the direct one.
        let indirect = self.text(node).contains('(');
        Statement::new(
            StatementKind::Goto(GotoStatement {
                target,
                indirect,
                is_far: self.far_marker(node),
            }),
            span,
        )
    }

    fn bare_call(&mut self, node: Node<'_>) -> Statement {
        let arguments = node.child_by_field_name("arguments").map(|list| {
            let mut arguments = Vec::new();
            let mut cursor = list.walk();
            for argument in list.children_by_field_name("argument", &mut cursor) {
                arguments.push(match argument.child_by_field_name("immediate") {
                    Some(immediate) => CallArgument::Immediate(self.immediate_value(immediate)),
                    None => CallArgument::Register(
                        self.register(argument.child_by_field_name("register"), argument),
                    ),
                });
            }
            arguments
        });
        let outputs = node.child_by_field_name("outputs").map(|list| {
            let mut registers = Vec::new();
            let mut cursor = list.walk();
            for register in list.children_by_field_name("register", &mut cursor) {
                registers.push(self.register(Some(register), list));
            }
            registers
        });
        Statement::new(
            StatementKind::BareCall(BareCall {
                target: self.name(node.child_by_field_name("target"), node),
                arguments,
                outputs,
            }),
            self.span(node),
        )
    }

    fn label(&mut self, node: Node<'_>) -> Statement {
        let name_node = node.child_by_field_name("name");
        let scope = match name_node.map(|name| name.kind()) {
            Some("local_identifier") => LabelScope::Local,
            _ => LabelScope::Global,
        };
        Statement::new(
            StatementKind::Label(LabelStatement {
                name: self.name(name_node, node),
                scope,
            }),
            self.span(node),
        )
    }

    /// The numeric value of an `immediate` CST node (`#` followed by a number).
    fn immediate_value(&mut self, node: Node<'_>) -> Expr {
        self.expr(node.child_by_field_name("value"), node)
    }

    /// Register names are case-insensitive. Recovery may leave one missing,
    /// in which case the accumulator stands in at a zero-width span.
    fn register(&mut self, node: Option<Node<'_>>, parent: Node<'_>) -> Register {
        let Some(node) = node else {
            let parent_span = self.span(parent);
            return Register {
                kind: RegisterKind::A,
                span: Span::empty_at(parent_span.file, parent_span.start),
            };
        };
        let kind = match self.text(node).to_ascii_lowercase().as_str() {
            "a" => RegisterKind::A,
            "b" => RegisterKind::B,
            "c" => RegisterKind::C,
            "d" => RegisterKind::D,
            "s" => RegisterKind::S,
            "x" => RegisterKind::X,
            "y" => RegisterKind::Y,
            "p" | "flag" => RegisterKind::P,
            _ => {
                self.unexpected(node, "a register");
                RegisterKind::A
            }
        };
        Register {
            kind,
            span: self.span(node),
        }
    }

    /// An expression in the evaluator's language. Operator spellings are kept
    /// as written; nothing here is evaluated.
    fn eval_expr(&mut self, node: Option<Node<'_>>, parent: Node<'_>) -> EvalExpr {
        let Some(node) = node else {
            let parent_span = self.span(parent);
            return EvalExpr {
                kind: EvalExprKind::Error,
                span: Span::empty_at(parent_span.file, parent_span.end),
            };
        };
        let span = self.span(node);
        let operator = |builder: &mut Self| {
            node.child_by_field_name("operator")
                .map(|operator| builder.text(operator).to_owned())
                .unwrap_or_default()
        };
        let kind = match node.kind() {
            "number" => {
                let text = self.text(node);
                // Exactly the original's test: `0x` or `0b` takes the integer
                // path, everything else takes the float one
                // (`compiler_eval.cpp:168`). K816's `$` and `%` are the same
                // two radices under different spellings, so they take the
                // same path.
                let exact = text.starts_with(['$', '%'])
                    || text.starts_with("0x")
                    || text.starts_with("0X")
                    || text.starts_with("0b")
                    || text.starts_with("0B");
                match parse_number(text) {
                    Some(value) => EvalExprKind::Number { value, exact },
                    // The same complaint the K65 expression layer makes about
                    // the same literal. An `Error` node with nothing said is
                    // how `data d { [4294967296] }` used to compile to an
                    // empty block and exit zero.
                    None => {
                        self.diagnostics.push(
                            Diagnostic::error(
                                DiagnosticCode::NUMBER_RANGE,
                                format!("numeric literal `{text}` is out of range"),
                                Label::new(span, "does not fit in 32 bits"),
                            )
                            .with_help("K816 numeric literals must fit in 32 bits"),
                        );
                        EvalExprKind::Error
                    }
                }
            }
            "float" => EvalExprKind::Float(self.text(node).to_owned()),
            "char_literal" => match parse_char_literal(self.text(node)) {
                Some(value) => EvalExprKind::Number { value, exact: true },
                None => EvalExprKind::Error,
            },
            "string" => EvalExprKind::String(decode_string(self.text(node))),
            "identifier" => EvalExprKind::Symbol(self.name(Some(node), node)),
            // Only a `#if` condition reaches this: `PPIfNumber -> Number` and
            // `Number -> [ EvalCode ]`, so `#if [A && B]` is a bracket in a
            // slot the rest of the evaluator grammar has no bracket in.
            "eval_bracket" => return self.eval_bracket(node),
            "eval_parenthesized" => {
                return EvalExpr {
                    kind: self.eval_expr(node.named_child(0), node).kind,
                    span,
                };
            }
            "eval_call" => {
                let mut arguments = Vec::new();
                let mut cursor = node.walk();
                for argument in node.children_by_field_name("argument", &mut cursor) {
                    arguments.push(self.eval_expr(Some(argument), node));
                }
                EvalExprKind::Call {
                    name: self.name(node.child_by_field_name("name"), node),
                    arguments,
                }
            }
            "eval_unary" => {
                let operand = node.child_by_field_name("operand");
                EvalExprKind::Unary {
                    operator: operator(self),
                    is_suffix: operand.is_some_and(|operand| {
                        node.child_by_field_name("operator")
                            .is_some_and(|op| op.start_byte() > operand.start_byte())
                    }),
                    operand: Box::new(self.eval_expr(operand, node)),
                }
            }
            "eval_binary" => EvalExprKind::Binary {
                operator: operator(self),
                left: Box::new(self.eval_expr(node.child_by_field_name("left"), node)),
                right: Box::new(self.eval_expr(node.child_by_field_name("right"), node)),
            },
            "eval_ternary" => EvalExprKind::Ternary {
                condition: Box::new(self.eval_expr(node.child_by_field_name("condition"), node)),
                consequence: Box::new(
                    self.eval_expr(node.child_by_field_name("consequence"), node),
                ),
                alternative: Box::new(
                    self.eval_expr(node.child_by_field_name("alternative"), node),
                ),
            },
            "eval_assignment" => EvalExprKind::Assignment {
                operator: operator(self),
                target: Box::new(self.eval_expr(node.child_by_field_name("target"), node)),
                value: Box::new(self.eval_expr(node.child_by_field_name("value"), node)),
            },
            "eval_subscript" => {
                let mut cursor = node.walk();
                let index_nodes: Vec<Node<'_>> =
                    node.children_by_field_name("index", &mut cursor).collect();
                let indices = index_nodes
                    .into_iter()
                    .map(|index| self.eval_expr(Some(index), node))
                    .collect();
                EvalExprKind::Subscript {
                    base: Box::new(self.eval_expr(node.child_by_field_name("base"), node)),
                    indices,
                }
            }
            "eval_member" => EvalExprKind::Member {
                base: Box::new(self.eval_expr(node.child_by_field_name("base"), node)),
                field: self.eval_method_name(node.child_by_field_name("field"), node),
            },
            "reserved_eval_call" => {
                let mut arguments = Vec::new();
                let mut cursor = node.walk();
                for argument in node.children_by_field_name("argument", &mut cursor) {
                    arguments.push(self.eval_expr(Some(argument), node));
                }
                let name_node = node.child_by_field_name("name");
                EvalExprKind::ReservedCall(ReservedCall {
                    name: self.eval_call_name(name_node),
                    arguments,
                    keyword_span: name_node.map(|name| self.span(name)).unwrap_or(span),
                })
            }
            "eval_list" => {
                let mut items = Vec::new();
                let mut cursor = node.walk();
                for item in node.children_by_field_name("item", &mut cursor) {
                    items.push(self.eval_expr(Some(item), node));
                }
                EvalExprKind::List(items)
            }
            "eval_if" => EvalExprKind::If {
                condition: Box::new(self.eval_expr(node.child_by_field_name("condition"), node)),
                consequence: Box::new(
                    self.eval_expr(node.child_by_field_name("consequence"), node),
                ),
                alternative: node
                    .child_by_field_name("alternative")
                    .map(|alternative| Box::new(self.eval_expr(Some(alternative), node))),
            },
            "eval_while" => EvalExprKind::While {
                condition: Box::new(self.eval_expr(node.child_by_field_name("condition"), node)),
                body: Box::new(self.eval_expr(node.child_by_field_name("body"), node)),
            },
            // A block is a statement run, and so is a bracket with more than
            // one statement in it; both are built by the same helper.
            "eval_statement_block" => return self.eval_bracket(node),
            "eval_method_call" => {
                let mut arguments = Vec::new();
                let mut cursor = node.walk();
                for argument in node.children_by_field_name("argument", &mut cursor) {
                    arguments.push(self.eval_expr(Some(argument), node));
                }
                EvalExprKind::MethodCall {
                    base: Box::new(self.eval_expr(node.child_by_field_name("base"), node)),
                    method: self.eval_method_name(node.child_by_field_name("method"), node),
                    arguments,
                }
            }
            "eval_pi" => EvalExprKind::Pi,
            _ => {
                self.unexpected(node, "an evaluator expression");
                EvalExprKind::Error
            }
        };
        EvalExpr { kind, span }
    }

    /// The name in `x.f` and `x.f(a)`, without its dot.
    ///
    /// Both spellings are sugar for a call — `f(x)` and `f(x, a)` — so what
    /// the AST holds is the function's name. Everywhere else in K65 a `.name`
    /// selects a field and keeps its dot; here it never does.
    fn eval_method_name(&mut self, node: Option<Node<'_>>, parent: Node<'_>) -> Name {
        let mut name = self.name(node, parent);
        if let Some(rest) = name.text.strip_prefix('.') {
            name.text = rest.to_owned();
        }
        name
    }

    /// The interior of `[ ... ]` or `{ ... }`: a run of evaluator statements,
    /// whose value is the last one's.
    ///
    /// A single statement is not wrapped, because a one-statement run *is*
    /// that statement — and because every bracket written before G1 has that
    /// shape, so unwrapping keeps their trees identical.
    fn eval_bracket(&mut self, node: Node<'_>) -> EvalExpr {
        // A bracket the parser had to recover inside is not the program the
        // author wrote, and evaluating the tree recovery invented would
        // report names nobody typed on top of the syntax error that is the
        // real news. One complaint about one problem.
        if node.has_error() {
            return EvalExpr {
                kind: EvalExprKind::Error,
                span: self.span(node),
            };
        }
        let mut cursor = node.walk();
        let statements: Vec<Node<'_>> = node
            .named_children(&mut cursor)
            .filter(|child| !matches!(child.kind(), "line_comment" | "block_comment"))
            .collect();
        match statements.len() {
            1 => self.eval_expr(Some(statements[0]), node),
            _ => EvalExpr {
                kind: EvalExprKind::Block(
                    statements
                        .into_iter()
                        .map(|statement| self.eval_expr(Some(statement), node))
                        .collect(),
                ),
                span: self.span(node),
            },
        }
    }

    fn eval_call_name(&mut self, node: Option<Node<'_>>) -> EvalCallName {
        match node.map(|node| self.text(node)) {
            Some("addbyte") => EvalCallName::AddByte,
            Some("color") => EvalCallName::Color,
            Some("rnd") => EvalCallName::Rnd,
            Some("print") => EvalCallName::Print,
            _ => EvalCallName::Error,
        }
    }

    fn preprocessor_directive(&mut self, node: Node<'_>) -> PreprocessorDirective {
        let directive = node.child_by_field_name("directive");
        let kind = match directive.map(|directive| self.text(directive)) {
            Some("#if") => PreprocessorKind::If,
            Some("#elif") => PreprocessorKind::ElseIf,
            Some("#else") => PreprocessorKind::Else,
            Some("#endif") => PreprocessorKind::EndIf,
            Some("#error") => PreprocessorKind::Error,
            _ => PreprocessorKind::Warn,
        };
        let row = node.start_position().row;
        let condition = node.child_by_field_name("condition");
        let message = node.child_by_field_name("message");
        // A directive is confined to the line it starts on. The grammar
        // cannot say so — whitespace is an `extra` and a newline is
        // whitespace — so it is said here, over the tree the grammar built.
        // `#if` above a bare `1`, and `#if !` above a bare `0`, would
        // otherwise reach down and take it.
        let split = condition
            .and_then(|condition| self.first_row_below(condition, row))
            .or_else(|| message.filter(|message| message.start_position().row != row));
        if let Some(split) = split {
            self.split_directive(node, split);
            return PreprocessorDirective {
                kind,
                condition: None,
                message: None,
                keyword_span: directive
                    .map(|directive| self.span(directive))
                    .unwrap_or_else(|| self.span(node)),
            };
        }
        PreprocessorDirective {
            kind,
            // The condition is an evaluator expression — `#if C` is `#if [C]`
            // — which is where `!`, `&&`, `||`, and the ternary come from.
            condition: condition.map(|condition| self.eval_expr(Some(condition), node)),
            message: message.map(|message| decode_string(self.text(message))),
            keyword_span: directive
                .map(|directive| self.span(directive))
                .unwrap_or_else(|| self.span(node)),
        }
    }

    /// The first part of a condition that does not begin on `row`.
    ///
    /// The walk follows the `!` chain down to the atom it negates and stops
    /// there: what is *inside* an evaluator bracket may wrap freely, because
    /// the `]` says where it ends and nothing on a later line can be absorbed
    /// into it by accident. Everything above that bracket is a token the
    /// directive claims, and a token the directive claims has to be on the
    /// directive's line.
    fn first_row_below<'t>(&self, node: Node<'t>, row: usize) -> Option<Node<'t>> {
        let mut node = node;
        loop {
            if node.start_position().row != row {
                return Some(node);
            }
            if node.kind() != "eval_unary" {
                return None;
            }
            node = node.child_by_field_name("operand")?;
        }
    }

    fn split_directive(&mut self, node: Node<'_>, split: Node<'_>) {
        if self.recovered {
            return;
        }
        let keyword = node
            .child_by_field_name("directive")
            .map(|directive| self.span(directive))
            .unwrap_or_else(|| self.span(node));
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::SYNTAX_ERROR,
                "a preprocessor directive ends at the end of its line",
                Label::new(
                    self.span(split),
                    "this is on the next line, so it is not part of the directive",
                ),
            )
            .with_secondary(Label::new(keyword, "the directive starts here"))
            .with_help(
                "write the whole directive on one line; anything longer than a name goes in \
                 an evaluator bracket, `#if [ A && B ]`, whose contents may wrap",
            )
            .with_docs("doc/syntax-reference.md#preprocessor-directives"),
        );
    }

    /// The `keyword` field of a reserved construct, or the whole node when
    /// recovery lost it.
    fn reserved(&mut self, node: Node<'_>, kind: ReservedKind) -> ReservedConstruct {
        let keyword = node.child_by_field_name("keyword");
        self.reserved_at(node, kind, keyword)
    }

    /// A reserved construct whose keyword the grammar does not field-name.
    ///
    /// `bank` and the illegal-opcode operators were taught to the grammar by
    /// G1, which was the one regeneration this sub-plan gets; naming their
    /// keywords would need a second. The node the caller passes is the one
    /// the diagnostic underlines, and the whole construct is the fallback.
    fn reserved_at(
        &mut self,
        node: Node<'_>,
        kind: ReservedKind,
        keyword: Option<Node<'_>>,
    ) -> ReservedConstruct {
        let name = node
            .child_by_field_name("name")
            .filter(|name| name.kind() == "identifier")
            .map(|name| self.name(Some(name), node));
        ReservedConstruct {
            kind,
            keyword_span: keyword
                .map(|keyword| self.span(keyword))
                .unwrap_or_else(|| self.span(node)),
            name,
        }
    }

    /// `align N`, and the original's `align N + M`.
    ///
    /// The `+` is the grammar's own alternative rather than the expression
    /// operator, so the two operands arrive separately and nothing folds
    /// them: the offset is a congruence, not a term.
    fn align_directive(&mut self, node: Node<'_>) -> AlignDirective {
        AlignDirective {
            boundary: self.expr(node.child_by_field_name("boundary"), node),
            offset: node
                .child_by_field_name("offset")
                .map(|offset| self.expr(Some(offset), node)),
        }
    }

    /// One of the five HLA forms that spell an undocumented 6502 opcode.
    ///
    /// Which of the five is read off the `operator` field alone. Each of the
    /// five operators appears in exactly one rule of the original's grammar,
    /// so the operator names the mnemonic; the register slots are wider here
    /// only because `$.register` is one token class, and a source that fills
    /// one differently was not a K65 program either.
    fn hla_illegal(&mut self, node: Node<'_>) -> Statement {
        let operator = node.child_by_field_name("operator");
        let opcode = match operator.map(|operator| self.text(operator)) {
            Some("&.") => IllegalOpcode::Anc,
            Some("?--") => IllegalOpcode::Dcp,
            Some("-++") => IllegalOpcode::Isc,
            Some("=") => IllegalOpcode::Sax,
            Some("&=") => IllegalOpcode::Axs,
            _ => {
                self.unexpected(node, "an illegal-opcode HLA form");
                return Statement::new(StatementKind::Error, self.span(node));
            }
        };
        let reserved = self.reserved_at(node, ReservedKind::IllegalOpcode(opcode), operator);
        Statement::new(StatementKind::Reserved(reserved), self.span(node))
    }

    fn data_block(&mut self, node: Node<'_>) -> DataBlock {
        let name = node
            .child_by_field_name("name")
            .map(|name| self.name(Some(name), node));
        let entries = node
            .child_by_field_name("body")
            .map(|body| self.data_entries(body))
            .unwrap_or_default();
        DataBlock {
            name,
            nocross: node.child_by_field_name("nocross").is_some(),
            entries,
        }
    }

    /// The entries of a `data_body`, which is both a section's body and a
    /// `nocross { ... }` group's.
    fn data_entries(&mut self, body: Node<'_>) -> Vec<DataEntry> {
        let mut cursor = body.walk();
        let children: Vec<Node<'_>> = body.named_children(&mut cursor).collect();
        children
            .into_iter()
            .filter(|child| !matches!(child.kind(), "line_comment" | "block_comment"))
            .map(|child| self.data_entry(child))
            .collect()
    }

    /// The `file` field of a `binary` line, with the span of the name and the
    /// path resolved against this source's own directory.
    ///
    /// A data entry may write the file as a bare identifier, and then the
    /// name *is* the path — `loadbinary` gets the identifier's own text with
    /// no extension appended (`compiler.inc:71`), measured, unlike the
    /// `image Ident` beside it which appends `.bmp`.
    fn binary_inclusion(&mut self, node: Node<'_>) -> BinaryInclusion {
        let (path, span) = match node.child_by_field_name("file") {
            Some(file) if file.kind() == "identifier" => {
                (self.text(file).to_owned(), self.span(file))
            }
            Some(file) => (decode_string(self.text(file)), self.span(file)),
            None => (String::new(), self.span(node)),
        };
        let resolved = std::path::Path::new(self.source.name())
            .parent()
            .unwrap_or(std::path::Path::new(""))
            .join(&path);
        BinaryInclusion {
            path,
            resolved,
            span,
        }
    }

    fn data_entry(&mut self, node: Node<'_>) -> DataEntry {
        let span = self.span(node);
        let kind = match node.kind() {
            "segment_declaration" => {
                DataEntryKind::Segment(self.name(node.child_by_field_name("name"), node))
            }
            "align_directive" => DataEntryKind::Align(self.align_directive(node)),
            "address_directive" => {
                DataEntryKind::Address(self.expr(node.child_by_field_name("address"), node))
            }
            "nocross_directive" => DataEntryKind::Nocross,
            "charset_directive" => DataEntryKind::Charset(
                node.child_by_field_name("mapping")
                    .map(|mapping| decode_string(self.text(mapping)))
                    .unwrap_or_default(),
            ),
            "data_label" => DataEntryKind::Label(self.name(node.child_by_field_name("name"), node)),
            "typed_entry" => {
                let width = match node.child_by_field_name("type").map(|w| self.text(w)) {
                    Some("byte") => TypeWidthKind::Byte,
                    Some("word") => TypeWidthKind::Word,
                    Some("far") => TypeWidthKind::Far,
                    _ => {
                        self.unexpected(node, "a typed data entry");
                        TypeWidthKind::Byte
                    }
                };
                DataEntryKind::Typed {
                    width,
                    values: self.data_values(node),
                }
            }
            "data_value" => match self.data_values(node).pop() {
                Some(value) => DataEntryKind::Value(value),
                None => DataEntryKind::Error,
            },
            "code_subblock" => DataEntryKind::Code {
                nocross: node.child_by_field_name("nocross").is_some(),
                body: self.body_of(node),
            },
            "data_repeat" => DataEntryKind::Repeat {
                count: self.expr(node.child_by_field_name("count"), node),
                body: self.data_block(node),
            },
            "preprocessor_directive" => {
                DataEntryKind::Preprocessor(self.preprocessor_directive(node))
            }
            "binary_entry" => DataEntryKind::Binary(self.binary_inclusion(node)),
            "nocross_data_group" => DataEntryKind::NocrossGroup(
                node.child_by_field_name("body")
                    .map(|body| self.data_entries(body))
                    .unwrap_or_default(),
            ),
            "reserved_image_block" => {
                DataEntryKind::Reserved(self.reserved(node, ReservedKind::ImageBlock))
            }
            "evaluator_entry" => match node.child_by_field_name("body") {
                Some(body) => DataEntryKind::Evaluator(self.eval_bracket(body)),
                None => DataEntryKind::Error,
            },
            "for_eval" => DataEntryKind::ForEval {
                variable: self.name(node.child_by_field_name("variable"), node),
                from: self.expr(node.child_by_field_name("from"), node),
                to: self.expr(node.child_by_field_name("to"), node),
                body: self.expr(node.child_by_field_name("body"), node),
            },
            _ => {
                self.unexpected(node, "a data block");
                DataEntryKind::Error
            }
        };
        DataEntry { kind, span }
    }

    fn data_values(&mut self, node: Node<'_>) -> Vec<DataValue> {
        let mut values = Vec::new();
        let mut cursor = node.walk();
        for value in node.children_by_field_name("value", &mut cursor) {
            values.push(match value.kind() {
                "string" => DataValue::String(decode_string(self.text(value))),
                "undefined_byte" => DataValue::Undefined,
                _ => DataValue::Expr(self.expr(Some(value), node)),
            });
        }
        values
    }

    /// Builds a binary expression from `node`'s `left` and `right` fields.
    fn binary(&mut self, node: Node<'_>, op: BinaryOp, op_span: Span, span: Span) -> Expr {
        let left = self.expr(node.child_by_field_name("left"), node);
        let right = self.expr(node.child_by_field_name("right"), node);
        Expr {
            kind: ExprKind::Binary(BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
                op_span,
            }),
            span,
        }
    }

    fn name(&mut self, node: Option<Node<'_>>, parent: Node<'_>) -> Name {
        match node {
            Some(node) if !node.is_missing() => Name {
                text: self.text(node).to_owned(),
                span: self.span(node),
                provenance: Provenance::Source,
            },
            Some(node) => Name {
                text: String::new(),
                span: Span::empty_at(self.source.id(), node.start_byte() as u32),
                provenance: Provenance::Synthetic,
            },
            None => {
                let parent_span = self.span(parent);
                Name {
                    text: String::new(),
                    span: Span::empty_at(parent_span.file, parent_span.start),
                    provenance: Provenance::Synthetic,
                }
            }
        }
    }

    fn expr(&mut self, node: Option<Node<'_>>, parent: Node<'_>) -> Expr {
        let Some(node) = node else {
            let parent_span = self.span(parent);
            return Expr {
                kind: ExprKind::Error,
                span: Span::empty_at(parent_span.file, parent_span.end),
            };
        };
        let span = self.span(node);
        if node.is_missing() {
            return Expr {
                kind: ExprKind::Error,
                span,
            };
        }
        match node.kind() {
            "number" => {
                let text = self.text(node);
                match parse_number(text) {
                    Some(value) => Expr {
                        kind: ExprKind::Number(value),
                        span,
                    },
                    None => {
                        self.diagnostics.push(
                            Diagnostic::error(
                                DiagnosticCode::NUMBER_RANGE,
                                format!("numeric literal `{text}` is out of range"),
                                Label::new(span, "does not fit in 32 bits"),
                            )
                            .with_help("K816 numeric literals must fit in 32 bits"),
                        );
                        Expr {
                            kind: ExprKind::Error,
                            span,
                        }
                    }
                }
            }
            "identifier" => Expr {
                kind: ExprKind::Symbol(self.name(Some(node), node)),
                span,
            },
            "local_identifier" => Expr {
                kind: ExprKind::LocalSymbol(self.name(Some(node), node)),
                span,
            },
            "parenthesized_expression" => {
                // Grouping is the tree shape, so the parentheses vanish — but
                // the span keeps covering them.
                let inner = self.expr(node.named_child(0), node);
                Expr {
                    kind: inner.kind,
                    span,
                }
            }
            "unary_expression" | "address_of_expression" => {
                let Some(operator) = node.child_by_field_name("operator") else {
                    self.unexpected(node, "a unary expression");
                    return Expr {
                        kind: ExprKind::Error,
                        span,
                    };
                };
                let op = match self.text(operator) {
                    "-" => UnaryOp::Negate,
                    "~" => UnaryOp::BitNot,
                    "&<" => UnaryOp::LowByte,
                    "&>" => UnaryOp::HighByte,
                    "&&" => UnaryOp::Address16,
                    "&&&" => UnaryOp::Address24,
                    _ => {
                        self.unexpected(operator, "a unary operator");
                        return Expr {
                            kind: ExprKind::Error,
                            span,
                        };
                    }
                };
                let operand = self.expr(node.child_by_field_name("operand"), node);
                Expr {
                    kind: ExprKind::Unary(UnaryExpr {
                        op,
                        operand: Box::new(operand),
                        op_span: self.span(operator),
                    }),
                    span,
                }
            }
            "binary_expression" => {
                let Some(operator) = node.child_by_field_name("operator") else {
                    self.unexpected(node, "a binary expression");
                    return Expr {
                        kind: ExprKind::Error,
                        span,
                    };
                };
                let op = match self.text(operator) {
                    "*" => BinaryOp::Mul,
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Sub,
                    "<<" => BinaryOp::Shl,
                    ">>" => BinaryOp::Shr,
                    "&" => BinaryOp::BitAnd,
                    "^" => BinaryOp::BitXor,
                    "|" => BinaryOp::BitOr,
                    _ => {
                        self.unexpected(operator, "a binary operator");
                        return Expr {
                            kind: ExprKind::Error,
                            span,
                        };
                    }
                };
                self.binary(node, op, self.span(operator), span)
            }
            // `FOO && BAR` is bitwise AND with the 16-bit address of BAR: one
            // source token, two operations. The CST keeps what was written;
            // the AST records what it means.
            "address_and_expression" => {
                let operator = node.child_by_field_name("operator");
                let op_span = match operator {
                    Some(operator) => self.span(operator),
                    None => Span::empty_at(span.file, span.start),
                };
                let expanded = self.binary(node, BinaryOp::BitAnd, op_span, span);
                let ExprKind::Binary(BinaryExpr {
                    op, left, right, ..
                }) = expanded.kind
                else {
                    return expanded;
                };
                let address = Expr {
                    span: right.span,
                    kind: ExprKind::Unary(UnaryExpr {
                        op: UnaryOp::Address16,
                        operand: right,
                        op_span,
                    }),
                };
                Expr {
                    kind: ExprKind::Binary(BinaryExpr {
                        op,
                        left,
                        right: Box::new(address),
                        op_span,
                    }),
                    span,
                }
            }
            "field_access" => Expr {
                kind: ExprKind::Field(FieldAccess {
                    base: Box::new(self.expr(node.child_by_field_name("base"), node)),
                    field: self.name(node.child_by_field_name("field"), node),
                }),
                span,
            },
            // `foo[.field]` is the same selection as `foo.field`, so both
            // reach the AST as one shape; the CST keeps the spelling.
            "subscript_expression" => {
                let base = Box::new(self.expr(node.child_by_field_name("base"), node));
                match node.child_by_field_name("field") {
                    Some(field) => Expr {
                        kind: ExprKind::Field(FieldAccess {
                            base,
                            field: self.name(Some(field), node),
                        }),
                        span,
                    },
                    None => Expr {
                        kind: ExprKind::Subscript(SubscriptExpr {
                            base,
                            index: Box::new(self.expr(node.child_by_field_name("index"), node)),
                        }),
                        span,
                    },
                }
            }
            "metadata_query" => {
                let Some(query_node) = node.child_by_field_name("query") else {
                    self.unexpected(node, "a metadata query");
                    return Expr {
                        kind: ExprKind::Error,
                        span,
                    };
                };
                let query = match self.text(query_node) {
                    "sizeof" => MetadataQueryKind::SizeOf,
                    "offsetof" => MetadataQueryKind::OffsetOf,
                    _ => {
                        self.unexpected(query_node, "a metadata query");
                        return Expr {
                            kind: ExprKind::Error,
                            span,
                        };
                    }
                };
                Expr {
                    kind: ExprKind::Metadata(MetadataQuery {
                        base: Box::new(self.expr(node.child_by_field_name("base"), node)),
                        query,
                        query_span: self.span(query_node),
                    }),
                    span,
                }
            }
            "typed_view" => {
                let base = Box::new(self.expr(node.child_by_field_name("base"), node));
                match node
                    .child_by_field_name("type")
                    .and_then(|width| self.type_width(width))
                {
                    Some(width) => Expr {
                        kind: ExprKind::TypedView(TypedViewExpr { base, width }),
                        span,
                    },
                    None => Expr {
                        kind: ExprKind::Error,
                        span,
                    },
                }
            }
            "eval_bracket" => Expr {
                kind: ExprKind::Eval(Box::new(self.eval_bracket(node))),
                span,
            },
            "char_literal" => {
                // A character literal is a numeric atom; the grammar accepts
                // only well-formed escapes, so decoding cannot fail on a
                // tree without errors.
                let text = self.text(node);
                match parse_char_literal(text) {
                    Some(value) => Expr {
                        kind: ExprKind::Number(value),
                        span,
                    },
                    None => {
                        self.unexpected(node, "a character literal");
                        Expr {
                            kind: ExprKind::Error,
                            span,
                        }
                    }
                }
            }
            _ => {
                self.unexpected(node, "an expression");
                Expr {
                    kind: ExprKind::Error,
                    span,
                }
            }
        }
    }
}

/// The single width annotation of a `module_width` item.
fn node_width(builder: &mut AstBuilder<'_>, node: Node<'_>) -> Option<WidthAnnotation> {
    let annotation = node.child_by_field_name("width")?;
    builder.width_annotation(annotation)
}

/// Strips the quotes from a string literal and decodes its escapes.
fn decode_string(text: &str) -> String {
    let body = text
        .strip_prefix('"')
        .and_then(|text| text.strip_suffix('"'))
        .unwrap_or(text);
    let mut decoded = String::with_capacity(body.len());
    let mut characters = body.chars();
    while let Some(character) = characters.next() {
        if character != '\\' {
            decoded.push(character);
            continue;
        }
        decoded.push(match characters.next() {
            Some('a') => '\u{7}',
            Some('b') => '\u{8}',
            Some('f') => '\u{c}',
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('v') => '\u{b}',
            Some('0') => '\0',
            Some(escaped) => escaped,
            None => break,
        });
    }
    decoded
}

/// Decodes a K65 numeric literal: `$2A`, `0x2A`, `%101010`, `0b101010`, or
/// decimal `42`.
fn parse_number(text: &str) -> Option<u32> {
    if let Some(hex) = text.strip_prefix('$') {
        u32::from_str_radix(hex, 16).ok()
    } else if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
        u32::from_str_radix(hex, 16).ok()
    } else if let Some(binary) = text.strip_prefix('%') {
        u32::from_str_radix(binary, 2).ok()
    } else if let Some(binary) = text.strip_prefix("0b").or_else(|| text.strip_prefix("0B")) {
        u32::from_str_radix(binary, 2).ok()
    } else {
        text.parse().ok()
    }
}

/// Decodes a character literal, quotes and escape included: `'A'`, `'\n'`,
/// `'\x41'`. Returns the character's code point, which is its byte value for
/// the ASCII range K65 source uses.
fn parse_char_literal(text: &str) -> Option<u32> {
    let body = text.strip_prefix('\'')?.strip_suffix('\'')?;
    let mut characters = body.chars();
    let value = match characters.next()? {
        '\\' => match characters.next()? {
            'a' => 0x07,
            'b' => 0x08,
            'f' => 0x0C,
            'n' => 0x0A,
            'r' => 0x0D,
            't' => 0x09,
            'v' => 0x0B,
            '0' => 0x00,
            'x' => return u32::from_str_radix(characters.as_str(), 16).ok(),
            escaped @ ('\\' | '\'' | '"') => escaped as u32,
            _ => return None,
        },
        character => character as u32,
    };
    characters.next().is_none().then_some(value)
}
