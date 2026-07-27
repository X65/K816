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
    AstFile, BinaryExpr, BinaryOp, Block, Expr, ExprKind, Function, HlaStatement, Item, ItemKind,
    Name, NativeInstruction, NativeOperand, Register, RegisterKind, Statement, StatementKind,
    UnaryExpr, UnaryOp, VarDeclaration, WidthAnnotation, WidthAnnotationKind,
};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::source::{Provenance, SourceFile, Span};

use super::cst::node_span;

type Node<'t> = tree_sitter::Node<'t>;

pub(super) fn build(root: Node<'_>, source: &SourceFile) -> (AstFile, Vec<Diagnostic>) {
    let mut builder = AstBuilder {
        source,
        diagnostics: Vec::new(),
    };
    let file = builder.file(root);
    (file, builder.diagnostics)
}

struct AstBuilder<'a> {
    source: &'a SourceFile,
    diagnostics: Vec<Diagnostic>,
}

impl AstBuilder<'_> {
    fn span(&self, node: Node<'_>) -> Span {
        node_span(node, self.source)
    }

    fn text(&self, node: Node<'_>) -> &str {
        self.source.span_text(node_span(node, self.source))
    }

    /// Records an internal diagnostic for a CST shape this builder does not
    /// recognize. Reaching this is a grammar/builder mismatch, not user error.
    fn unexpected(&mut self, node: Node<'_>, context: &str) {
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
                "var_declaration" => items.push(self.var_declaration(child)),
                "func_declaration" => items.push(self.func_declaration(child)),
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

    fn var_declaration(&mut self, node: Node<'_>) -> Item {
        let name = self.name(node.child_by_field_name("name"), node);
        let address = self.expr(node.child_by_field_name("address"), node);
        Item {
            kind: ItemKind::Var(VarDeclaration { name, address }),
            span: self.span(node),
        }
    }

    fn func_declaration(&mut self, node: Node<'_>) -> Item {
        let name = self.name(node.child_by_field_name("name"), node);
        let mut width = Vec::new();
        let mut cursor = node.walk();
        for annotation in node.children_by_field_name("width", &mut cursor) {
            if let Some(parsed) = self.width_annotation(annotation) {
                width.push(parsed);
            }
        }
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
            kind: ItemKind::Func(Function { name, width, body }),
            span: self.span(node),
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
                "native_instruction" => statements.push(self.native_instruction(child)),
                "hla_load_immediate" => statements.push(self.hla_load_immediate(child)),
                "hla_store" => statements.push(self.hla_store(child)),
                "return_statement" => statements.push(Statement {
                    kind: StatementKind::Return,
                    span: self.span(child),
                }),
                "line_comment" | "block_comment" => {}
                "ERROR" => statements.push(Statement {
                    kind: StatementKind::Error,
                    span: self.span(child),
                }),
                _ => {
                    self.unexpected(child, "a block");
                    statements.push(Statement {
                        kind: StatementKind::Error,
                        span: self.span(child),
                    });
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
            .map(|operand| match operand.kind() {
                "immediate" => NativeOperand::Immediate(self.immediate_value(operand)),
                "address" => NativeOperand::Address(self.name(operand.child(0), operand)),
                _ => {
                    self.unexpected(operand, "a native operand");
                    NativeOperand::Immediate(Expr {
                        kind: ExprKind::Error,
                        span: self.span(operand),
                    })
                }
            });
        Statement {
            kind: StatementKind::Native(NativeInstruction { mnemonic, operand }),
            span: self.span(node),
        }
    }

    fn hla_load_immediate(&mut self, node: Node<'_>) -> Statement {
        let register = self.register(node.child_by_field_name("register"), node);
        let value = match node.child_by_field_name("value") {
            Some(immediate) => self.immediate_value(immediate),
            None => self.expr(None, node),
        };
        Statement {
            kind: StatementKind::Hla(HlaStatement::LoadImmediate { register, value }),
            span: self.span(node),
        }
    }

    fn hla_store(&mut self, node: Node<'_>) -> Statement {
        let target = self.name(node.child_by_field_name("target"), node);
        let register = self.register(node.child_by_field_name("source"), node);
        Statement {
            kind: StatementKind::Hla(HlaStatement::Store { target, register }),
            span: self.span(node),
        }
    }

    /// The numeric value of an `immediate` CST node (`#` followed by a number).
    fn immediate_value(&mut self, node: Node<'_>) -> Expr {
        self.expr(node.child_by_field_name("value"), node)
    }

    fn register(&mut self, node: Option<Node<'_>>, parent: Node<'_>) -> Register {
        // The slice grammar only knows the accumulator; recovery may leave the
        // register missing, in which case `a` is still the only possibility.
        let span = match node {
            Some(node) => self.span(node),
            None => {
                let parent_span = self.span(parent);
                Span::empty_at(parent_span.file, parent_span.start)
            }
        };
        Register {
            kind: RegisterKind::A,
            span,
        }
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
            "eval_bracket" => {
                let inner = self.expr(node.child_by_field_name("expression"), node);
                Expr {
                    kind: ExprKind::Eval(Box::new(inner)),
                    span,
                }
            }
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
