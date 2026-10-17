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
    BranchCondition, BranchConditionKind, CallArgument, CallContract, CallStatement, ChainElement,
    ChainOperator, ComparisonKind, ConditionChain, ConditionalBlock, ConditionalJump,
    ConstDeclaration, ConstEntry, ContractOutput, ContractOutputs, ContractParameter, DataBlock,
    DataEntry, DataEntryKind, DataValue, EvalCallName, EvalExpr, EvalExprKind, EvalFunction, Expr,
    ExprKind, ExternConst, ExternDeclaration, ExternExit, ExternFunction, ExternKind, ExternVar,
    FieldAccess, FieldDeclaration, FlagKind, FlagTestSpelling, Function, FunctionKind,
    GotoStatement, HlaArithmetic, HlaAssignment, HlaExchange, HlaFlag, HlaOperand, HlaOperandKind,
    HlaShift, HlaStack, HlaStatement, HlaStep, IllegalOpcode, ImportDeclaration, IndexRegister,
    IndexRegisterKind, IndirectOperand, Item, ItemKind, LabelScope, LabelStatement,
    LongIndirectOperand, LoopCondition, LoopSpelling, LoopStatement, MetadataQuery,
    MetadataQueryKind, Name, NativeInstruction, NativeOperand, PreprocessorDirective,
    PreprocessorKind, Register, RegisterKind, ReservedCall, ReservedConstruct, ReservedKind,
    ReturnKind, ShiftOp, StackOp, StackSpelling, Statement, StatementKind, StepOp, StmtId,
    StorageClass, StorageClassKind, SubscriptExpr, TestableFlag, TypeWidth, TypeWidthKind,
    TypedViewExpr, UnaryExpr, UnaryOp, VarDeclaration, WidthAnnotation, WidthAnnotationKind,
    WidthBlock,
};
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
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
            | ItemKind::Import(_)
            | ItemKind::Extern(_)
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
        StatementKind::Loop(loop_statement) => {
            number_block(&mut loop_statement.body, next);
            if let LoopCondition::Branch(chain) = &mut loop_statement.condition {
                for slot in chain.slots_mut() {
                    number_block(slot, next);
                }
            }
        }
        StatementKind::Conditional(conditional) => {
            for slot in conditional.condition.slots_mut() {
                number_block(slot, next);
            }
            number_block(&mut conditional.body, next);
            if let Some(otherwise) = &mut conditional.otherwise {
                number_block(otherwise, next);
            }
        }
        StatementKind::ConditionalJump(jump) => {
            for slot in jump.condition.slots_mut() {
                number_block(slot, next);
            }
            number_statement(&mut jump.jump, next);
        }
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
        | StatementKind::Continue
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
                "import_declaration" => items.push(Item {
                    kind: ItemKind::Import(ImportDeclaration {
                        file: self.binary_inclusion(child),
                    }),
                    span: self.span(child),
                }),
                "extern_declaration" => {
                    if let Some(declaration) = self.extern_declaration(child) {
                        items.push(Item {
                            kind: ItemKind::Extern(declaration),
                            span: self.span(child),
                        });
                    }
                }
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
                    width: child
                        .child_by_field_name("type")
                        .and_then(|width| self.type_width(width)),
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

    /// `extern func` / `extern var` / `extern const`.
    ///
    /// Returns `None` only where the declaration has no name to hang a
    /// symbol on, which recovery can produce; the parse diagnostic has
    /// already been reported by then and a second complaint would be noise.
    fn extern_declaration(&mut self, node: Node<'_>) -> Option<ExternDeclaration> {
        let item = node.child_by_field_name("item")?;
        let kind = match item.kind() {
            "extern_func" => {
                ExternKind::Func(ExternFunction {
                    name: self.name(item.child_by_field_name("name"), item),
                    // `far_marker` takes the node that *carries* the field
                    // and checks the word itself, because the marker is an
                    // aliased identifier rather than a keyword.
                    is_far: self.far_marker(item),
                    width: self.width_annotations(item),
                    exit: item.child_by_field_name("exit").map(|exit| ExternExit {
                        width: self.width_annotations(exit),
                        span: self.span(exit),
                    }),
                })
            }
            "extern_var" => ExternKind::Var(ExternVar {
                name: self.name(item.child_by_field_name("name"), item),
                storage: item
                    .child_by_field_name("storage")
                    .and_then(|storage| self.storage_class(storage)),
                width: item
                    .child_by_field_name("type")
                    .and_then(|width| self.type_width(width)),
            }),
            "extern_const" => ExternKind::Const(ExternConst {
                name: self.name(item.child_by_field_name("name"), item),
                width: item
                    .child_by_field_name("type")
                    .and_then(|width| self.type_width(width)),
            }),
            _ => {
                self.unexpected(item, "an `extern` declaration");
                return None;
            }
        };
        Some(ExternDeclaration {
            kind,
            keyword_span: self.span(node.child(0)?),
            span: self.span(node),
        })
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
            let list = self.contract_outputs(outputs);
            ContractOutputs {
                width: self.width_annotations(outputs),
                outputs: list,
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
                "push_statement" => statements.extend(self.push_statement(child)),
                "pull_statement" => statements.extend(self.pull_statement(child)),
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
                "if_statement" => statements.extend(self.if_statement(child)),
                "conditional_block" => statements.push(self.conditional_block(child)),
                "conditional_jump" => statements.push(self.conditional_jump(child)),
                "postfix_loop" => statements.push(self.postfix_loop(child)),
                "loop_statement" => statements.push(self.loop_statement(child)),
                "goto_statement" => statements.push(self.goto_statement(child)),
                "break_statement" => {
                    statements.push(Statement::new(StatementKind::Break, self.span(child)))
                }
                "repeat_statement" => {
                    statements.push(Statement::new(StatementKind::Repeat, self.span(child)))
                }
                "continue_statement" => {
                    statements.push(Statement::new(StatementKind::Continue, self.span(child)))
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
                "hla_stack_flag" => statements.push(self.hla_stack_flag(child)),
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
    /// `o` is here so that `o-` is *recognised* and refused with `v-` named
    /// rather than falling through to arithmetic: the original spells the
    /// set/clear shorthand `o` and the branch tests `v`, and K816 spells both
    /// `v` (`plans/06-compatibility.md` decision 32). Every letter here is a
    /// flag and nothing else — `c` and `d` stopped naming registers with
    /// decisions 29 and 30 — so a statement's shape no longer has to settle
    /// which was meant.
    fn flag_spelling(&self, node: Node<'_>) -> bool {
        matches!(node.kind(), "identifier" | "register")
            && matches!(
                self.text(node).to_ascii_lowercase().as_str(),
                "c" | "d" | "i" | "v" | "o"
            )
    }

    /// The reserved node for `o`/`O` where `v` is meant.
    ///
    /// A node rather than a diagnostic, because that is the shape decision 0
    /// gives every retired spelling: the parser recognises the construct and
    /// `semantic.rs` says what is wrong with it, so the refusal arrives with
    /// the rest and a parse stays clean.
    fn overflow_spelling(&self, span: Span) -> ReservedConstruct {
        ReservedConstruct {
            kind: ReservedKind::OverflowFlagSpelling,
            keyword_span: span,
            name: None,
        }
    }

    /// Whether `text` is the retired `o` spelling of the overflow flag.
    fn is_overflow_spelling(text: &str) -> bool {
        text.eq_ignore_ascii_case("o")
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
        let text = self.text(name).to_ascii_lowercase();
        if Self::is_overflow_spelling(&text) {
            return Statement::new(StatementKind::Reserved(self.overflow_spelling(span)), span);
        }
        let flag = match text.as_str() {
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
            Some("v") => FlagKind::Overflow,
            // Recognised so it is refused by name rather than reaching the
            // "not a flag" arm below, which would say the wrong thing: `o`
            // *is* a flag, and the complaint is about which letter spells it.
            Some("o") => {
                return Statement::new(StatementKind::Reserved(self.overflow_spelling(span)), span);
            }
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
        // `c??` and `c!!` are the K65 flag shorthand, not a register move:
        // the 65816 has no instruction that pushes the carry, and the
        // original meant `PLP`/`PHP` by them like every other letter. They
        // arrive here rather than in `hla_stack_flag` because `c` is in the
        // register token, and the register token's own rule is that the
        // builder decides which meaning a statement asked for.
        let register = node.child_by_field_name("register");
        if register.is_some_and(|register| self.text(register).eq_ignore_ascii_case("c")) {
            return Statement::new(StatementKind::Reserved(self.stack_flag(node)), span);
        }
        Statement::new(
            StatementKind::Hla(HlaStatement::Stack(HlaStack {
                register: self.register(node.child_by_field_name("register"), node),
                op: if operator == "!!" {
                    StackOp::Push
                } else {
                    StackOp::Pull
                },
                op_span,
                spelling: StackSpelling::Shorthand,
            })),
            span,
        )
    }

    /// `push a, $0012, x` — one statement per item, in the order written,
    /// as an assignment chain is one per step. A register is pushed by its
    /// own instruction, and anything else is a constant for `PEA`.
    fn push_statement(&mut self, node: Node<'_>) -> Vec<Statement> {
        let mut statements = Vec::new();
        let mut cursor = node.walk();
        for item in node.children_by_field_name("item", &mut cursor) {
            let span = self.span(item);
            let kind = match item.kind() {
                "register" => HlaStatement::Stack(HlaStack {
                    register: self.register(Some(item), node),
                    op: StackOp::Push,
                    op_span: span,
                    spelling: StackSpelling::Word,
                }),
                "immediate" => HlaStatement::PushConstant {
                    value: self.expr(item.child_by_field_name("value"), item),
                    marked: true,
                },
                _ => HlaStatement::PushConstant {
                    value: self.expr(Some(item), node),
                    marked: false,
                },
            };
            statements.push(Statement::new(StatementKind::Hla(kind), span));
        }
        statements
    }

    /// `pull x, a` — one statement per register, in the order written.
    fn pull_statement(&mut self, node: Node<'_>) -> Vec<Statement> {
        let mut statements = Vec::new();
        let mut cursor = node.walk();
        for item in node.children_by_field_name("register", &mut cursor) {
            let span = self.span(item);
            let stack = HlaStack {
                register: self.register(Some(item), node),
                op: StackOp::Pull,
                op_span: span,
                spelling: StackSpelling::Word,
            };
            statements.push(Statement::new(
                StatementKind::Hla(HlaStatement::Stack(stack)),
                span,
            ));
        }
        statements
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
        if node.kind() == "flag_condition" {
            return self.flag_condition(node).unwrap_or(fallback);
        }
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
                // `!c?` is K816's spelling of a clear flag, `c?` of a set one,
                // and `c-?` / `c+?` K65's of the same two.
                let (negated, flag_test) = match flag_test.strip_prefix('!') {
                    Some(rest) => (true, rest),
                    None => (false, flag_test),
                };
                let mut characters = flag_test.chars();
                let flag = match characters.next() {
                    Some('c') => TestableFlag::Carry,
                    Some('n') => TestableFlag::Negative,
                    Some('z') => TestableFlag::Zero,
                    Some('v') => TestableFlag::Overflow,
                    // `o-?` and `o+?` were K816's own extension on top of
                    // decision 14: the original spells the branch tests `v`
                    // and has no `o` form at all (`compiler.inc:318-319`).
                    // Reported here rather than as a reserved node because a
                    // condition is an operand and there is nowhere to hang a
                    // statement — but it is the same refusal, from the same
                    // place, so the two spellings cannot drift apart.
                    Some('o') | Some('O') => {
                        self.diagnostics
                            .push(crate::compat::k65_overflow_flag_spelling(self.span(node)));
                        return fallback;
                    }
                    _ => {
                        self.unexpected(node, "a branch condition");
                        return fallback;
                    }
                };
                match characters.next() {
                    Some('?') => BranchConditionKind::Flag {
                        flag,
                        set: !negated,
                        spelling: FlagTestSpelling::Bare,
                    },
                    sign => BranchConditionKind::Flag {
                        flag,
                        set: sign == Some('+'),
                        spelling: FlagTestSpelling::Signed,
                    },
                }
            }
        };
        BranchCondition {
            kind,
            span: self.span(node),
        }
    }

    /// `z`, `!c` after `if` or `while`: a flag test by its letter
    /// (`plans/17-flag-tests.md`). The grammar takes any identifier there, so
    /// that a name which is not a testable flag is refused with a sentence
    /// rather than a parse error.
    fn flag_condition(&mut self, node: Node<'_>) -> Option<BranchCondition> {
        let name = node.child_by_field_name("flag")?;
        let letter = self.text(name).to_owned();
        let flag = match letter.to_ascii_lowercase().as_str() {
            "c" => TestableFlag::Carry,
            "z" => TestableFlag::Zero,
            "n" => TestableFlag::Negative,
            "v" => TestableFlag::Overflow,
            // The same refusal `o?` gets, from the same place — and here the
            // fix is the one letter, which `o?` has no single edit for.
            "o" => {
                let span = self.span(name);
                self.diagnostics
                    .push(
                        crate::compat::k65_overflow_flag_spelling(span).with_fix(Fix {
                            title: "write `v`".to_owned(),
                            applicability: Applicability::MachineApplicable,
                            edits: vec![TextEdit::replace(span, "v")],
                        }),
                    );
                return None;
            }
            _ => {
                let span = self.span(name);
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::FLAG_TEST_LETTER,
                        format!("`{letter}` is not a flag a branch can test"),
                        Label::new(span, "after `if` or `while`, a name is a flag test"),
                    )
                    .with_note(
                        "a branch tests the carry `c`, zero `z`, negative `n` or overflow `v`; \
                         `d` and `i` are flags no branch reads",
                    )
                    .with_help(
                        "write one of `c`, `z`, `n` or `v` (`!` before it for a clear flag), or a \
                         relation such as `==` after a compare",
                    )
                    .with_docs("doc/syntax-reference.md#conditions"),
                );
                return None;
            }
        };
        Some(BranchCondition {
            kind: BranchConditionKind::Flag {
                flag,
                set: node.child_by_field_name("negated").is_none(),
                spelling: FlagTestSpelling::Letter,
            },
            span: self.span(node),
        })
    }

    /// The `condition_chain` of an `if` or a `while`, and the span of its
    /// first slot — which is hoisted as a statement of its own, pushed onto
    /// `before`, as Q built the compare in an `if`
    /// (`plans/18-conditions.md` decision 1). A later element's slot stays
    /// in the element. A mix of `|` and `&` is refused.
    fn condition_chain(
        &mut self,
        parent: Node<'_>,
        before: &mut Vec<Statement>,
    ) -> (Option<Span>, ConditionChain) {
        let Some(chain) = parent.child_by_field_name("condition") else {
            self.unexpected(parent, "a condition");
            let parent_span = self.span(parent);
            let fallback = BranchCondition {
                kind: BranchConditionKind::Comparison(ComparisonKind::NotEqual),
                span: Span::empty_at(parent_span.file, parent_span.start),
            };
            return (None, ConditionChain::single(fallback));
        };
        let mut cursor = chain.walk();
        let mut operator: Option<(ChainOperator, Span)> = None;
        for node in chain.children_by_field_name("operator", &mut cursor) {
            let written = match self.text(node) {
                "|" => ChainOperator::Any,
                _ => ChainOperator::All,
            };
            match operator {
                None => operator = Some((written, self.span(node))),
                Some((first, first_span)) if first != written => {
                    let span = self.span(node);
                    self.diagnostics.push(
                        Diagnostic::error(
                            DiagnosticCode::MIXED_CHAIN,
                            "a chain joins its conditions with one operator",
                            Label::new(span, "this is the other operator"),
                        )
                        .with_secondary(Label::new(first_span, "the chain's operator"))
                        .with_note(
                            "`|` branches when any condition holds and `&` when all do; a chain \
                             of both has no precedence to read it by",
                        )
                        .with_help("nest an `if` for the inner group")
                        .with_docs("doc/syntax-reference.md#conditions"),
                    );
                }
                Some(_) => {}
            }
        }
        let mut cursor = chain.walk();
        let mut elements = Vec::new();
        let mut first_slot = None;
        for (index, node) in chain
            .children_by_field_name("element", &mut cursor)
            .enumerate()
        {
            let slot_node = node.child_by_field_name("slot");
            let slot_statements = slot_node.map(|slot| match slot.kind() {
                "hla_step" => self.hla_step(slot),
                _ => vec![self.hla_arithmetic(slot)],
            });
            let slot = match (index, slot_statements) {
                (0, Some(statements)) => {
                    first_slot = slot_node.map(|slot| self.span(slot));
                    before.extend(statements);
                    None
                }
                (_, Some(statements)) => {
                    let span = self.span(slot_node.expect("a slot has a node"));
                    Some(Block { statements, span })
                }
                (_, None) => None,
            };
            let condition_node = node.child_by_field_name("condition");
            let (condition, assumed) = match condition_node {
                Some(condition) => (self.branch_condition(Some(condition), node), false),
                // A step alone tests `!=`, as `DEX`/`BNE` does.
                None => {
                    let end = self.span(node).end;
                    (
                        BranchCondition {
                            kind: BranchConditionKind::Comparison(ComparisonKind::NotEqual),
                            span: Span::empty_at(self.span(node).file, end),
                        },
                        true,
                    )
                }
            };
            let element_span = match (index, slot_node, condition_node) {
                // The first element's span starts at its condition: its
                // slot is a statement of its own.
                (0, _, Some(condition)) => self.span(condition),
                _ => self.span(node),
            };
            elements.push(ChainElement {
                slot,
                condition,
                assumed,
                span: element_span,
            });
        }
        let whole = self.span(chain);
        let start = elements
            .first()
            .map_or(whole.start, |element| element.span.start);
        (
            first_slot,
            ConditionChain {
                elements,
                operator: operator.map(|(operator, _)| operator),
                span: Span::new(whole.file, start, whole.end),
            },
        )
    }

    fn conditional_block(&mut self, node: Node<'_>) -> Statement {
        let (otherwise, chained) = self.else_clause(node);
        Statement::new(
            StatementKind::Conditional(ConditionalBlock {
                condition: ConditionChain::single(
                    self.branch_condition(node.child_by_field_name("condition"), node),
                ),
                body: self.body_of(node),
                otherwise,
                keyword: None,
                compare: None,
                chained,
            }),
            self.span(node),
        )
    }

    /// The `else` of a conditional, and whether it was written `else if`.
    ///
    /// A chain is the `if` inside a block of its own, spanning the `if`:
    /// `else if == { … }` is `else { if == { … } }`, which is what every
    /// consumer of the AST already handles (`plans/16-if-statement.md`
    /// decision 4).
    fn else_clause(&mut self, node: Node<'_>) -> (Option<Block>, bool) {
        let Some(clause) = node.child_by_field_name("else") else {
            return (None, false);
        };
        match clause.child_by_field_name("chain") {
            Some(chain) => (
                Some(Block {
                    statements: self.if_statement(chain),
                    span: self.span(chain),
                }),
                true,
            ),
            None => (Some(self.body_of(clause)), false),
        }
    }

    /// `if COND { … }`, `if a?10 COND { … }`, `if COND goto …`: the
    /// statements K65 writes for the same thing, a compare (when one was
    /// written in the `if`) and then the bare conditional or jump.
    fn if_statement(&mut self, node: Node<'_>) -> Vec<Statement> {
        let mut statements = Vec::new();
        let keyword = node
            .child(0)
            .filter(|child| child.kind() == "if")
            .map(|child| self.span(child));
        let (compare, condition) = self.condition_chain(node, &mut statements);
        let whole = self.span(node);
        // From the first condition on: the compare before it has a statement
        // of its own, and two statements must not claim the same text.
        let span = Span::new(whole.file, condition.span.start, whole.end);
        if node.child_by_field_name("jump").is_some() {
            let jump = self.jump(node, span);
            statements.push(Statement::new(
                StatementKind::ConditionalJump(ConditionalJump {
                    condition,
                    jump: Box::new(jump),
                    keyword,
                    compare,
                }),
                span,
            ));
        } else {
            let (otherwise, chained) = self.else_clause(node);
            statements.push(Statement::new(
                StatementKind::Conditional(ConditionalBlock {
                    condition,
                    body: self.body_of(node),
                    otherwise,
                    keyword,
                    compare,
                    chained,
                }),
                span,
            ));
        }
        statements
    }

    fn conditional_jump(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let condition = self.branch_condition(node.child_by_field_name("condition"), node);
        let jump = self.jump(node, span);
        Statement::new(
            StatementKind::ConditionalJump(ConditionalJump {
                condition: ConditionChain::single(condition),
                jump: Box::new(jump),
                keyword: None,
                compare: None,
            }),
            span,
        )
    }

    /// The `goto`, `break` or `repeat` a condition guards.
    ///
    /// A jump whose keyword the parser invented is no jump: building it would
    /// add "`repeat` is not inside a loop" to the syntax error that already
    /// says what is missing.
    fn jump(&mut self, node: Node<'_>, span: Span) -> Statement {
        match node.child_by_field_name("jump") {
            Some(jump) if jump.child(0).is_some_and(super::diagnostics::invented_jump) => {
                Statement::new(StatementKind::Error, self.span(jump))
            }
            Some(jump) => match jump.kind() {
                "goto_statement" => self.goto_statement(jump),
                "break_statement" => Statement::new(StatementKind::Break, self.span(jump)),
                "repeat_statement" => Statement::new(StatementKind::Repeat, self.span(jump)),
                "continue_statement" => Statement::new(StatementKind::Continue, self.span(jump)),
                _ => {
                    self.unexpected(jump, "a conditional jump");
                    Statement::new(StatementKind::Error, self.span(jump))
                }
            },
            None => Statement::new(StatementKind::Error, Span::empty_at(span.file, span.end)),
        }
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
            _ => LoopCondition::Branch(ConditionChain::single(
                self.branch_condition(condition_node, node),
            )),
        };
        Statement::new(
            StatementKind::Loop(LoopStatement {
                body,
                nocross,
                condition,
                spelling: LoopSpelling::Postfix,
                test: None,
            }),
            span,
        )
    }

    /// `loop { ... } always`, `loop { ... } never`, `loop { ... } while !=`,
    /// with `nocross` in front of any of them: the same loop as the postfix
    /// form, with the keyword saying where it begins.
    fn loop_statement(&mut self, node: Node<'_>) -> Statement {
        let span = self.span(node);
        let mut body = self.body_of(node);
        let condition_node = node.child_by_field_name("condition");
        // `while x?10 <`, `while x--`: the first element's compare or step
        // runs as the body's last statement, which is where K65 writes it
        // (`plans/16-if-statement.md` decision 9) — so a `repeat`, which goes
        // to the top, skips it, and `nocross` keeps it inside the page it
        // checks.
        let mut test = None;
        let condition = match condition_node {
            Some(condition) if condition.kind() == "while_condition" => {
                let (first_slot, chain) = self.condition_chain(condition, &mut body.statements);
                test = first_slot;
                LoopCondition::Branch(chain)
            }
            Some(condition) if self.text(condition) == "always" => LoopCondition::Always,
            Some(condition) if self.text(condition) == "never" => LoopCondition::Never,
            _ => LoopCondition::Branch(ConditionChain::single(self.branch_condition(None, node))),
        };
        Statement::new(
            StatementKind::Loop(LoopStatement {
                body,
                nocross: node.child_by_field_name("nocross").is_some(),
                condition,
                spelling: LoopSpelling::Keyword,
                test,
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
        let outputs = node
            .child_by_field_name("outputs")
            .map(|list| self.contract_outputs(list));
        Statement::new(
            StatementKind::BareCall(BareCall {
                target: self.name(node.child_by_field_name("target"), node),
                arguments,
                outputs,
            }),
            self.span(node),
        )
    }

    /// The entries of a `->` list, **in written order**.
    ///
    /// Walked child by child rather than through `children_by_field_name`,
    /// which yields one field at a time and would sort every register ahead
    /// of every flag. The order is the whole point: a call site restates the
    /// callee's clause and the two are compared as written.
    fn contract_outputs(&mut self, list: Node<'_>) -> Vec<ContractOutput> {
        let mut outputs = Vec::new();
        for index in 0..list.child_count() as u32 {
            let Some(child) = list.child(index) else {
                continue;
            };
            match list.field_name_for_child(index) {
                Some("register") => {
                    outputs.push(ContractOutput::Register(self.register(Some(child), list)))
                }
                Some("flag") => outputs.push(ContractOutput::Flag(self.name(Some(child), list))),
                _ => {}
            }
        }
        outputs
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
            "dpr" => RegisterKind::Dpr,
            "dbr" => RegisterKind::Dbr,
            "s" => RegisterKind::S,
            "x" => RegisterKind::X,
            "y" => RegisterKind::Y,
            "p" => RegisterKind::P,
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

    /// A flag letter's stack shorthand — `z??`, `n!!`, `v??`, `i!!`, and the
    /// `c` forms the grammar routes through `hla_stack`.
    ///
    /// Recognized so that a valid K65 line is refused by name rather than
    /// reaching `expected a statement`, which is what decision 0 forbids.
    fn hla_stack_flag(&mut self, node: Node<'_>) -> Statement {
        Statement::new(
            StatementKind::Reserved(self.stack_flag(node)),
            self.span(node),
        )
    }

    /// The reserved construct a flag letter's stack shorthand becomes.
    ///
    /// The caret covers the whole form, `z??` rather than `z`: the letter on
    /// its own is not the mistake, and neither is the operator — what has no
    /// meaning here is the pair. The letter is carried as the `name` so the
    /// message reads the source's own word back, and it is the first
    /// character of the form whichever rule matched, because `hla_stack_flag`
    /// is one token and the `hla_stack` node a `c??` arrives on is exactly
    /// the two.
    fn stack_flag(&mut self, node: Node<'_>) -> ReservedConstruct {
        let span = self.span(node);
        let text = self
            .text(node)
            .chars()
            .next()
            .map(String::from)
            .unwrap_or_default();
        ReservedConstruct {
            kind: ReservedKind::StackFlag,
            keyword_span: span,
            name: Some(crate::ast::Name {
                text,
                span,
                provenance: Provenance::Source,
            }),
        }
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
                "string" => DataValue::String {
                    text: decode_string(self.text(value)),
                    span: self.span(value),
                },
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
