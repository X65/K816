/*****************************************************************************/
/*                                                                           */
/*                                   ast.rs                                  */
/*                                                                           */
/*              Owned syntax AST for native and HLA K816 source              */
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

//! Owned syntax AST.
//!
//! Built from the Tree-sitter CST by `parser::ast_builder`; contains no
//! Tree-sitter types. Every node keeps its exact byte [`Span`]. Native and
//! HLA statements stay distinct until lowering. Malformed constructs become
//! owned `Error` nodes so later stages keep working past the first syntax
//! error; missing constructs are marked [`Provenance::Synthetic`] while
//! retaining a zero-width origin span.

use crate::source::{Provenance, Span};

/// One parsed source file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AstFile {
    pub items: Vec<Item>,
}

/// A top-level item.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ItemKind {
    Var(VarDeclaration),
    Func(Function),
    /// Malformed top-level construct; the span covers the unparsable region.
    Error,
}

/// An identifier with its source text and span.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Name {
    pub text: String,
    pub span: Span,
    /// [`Provenance::Synthetic`] when the parser had to invent this name for
    /// a `MISSING` node; the span is then the zero-width origin.
    pub provenance: Provenance,
}

/// `var NAME = $addr` — fixed-address declaration (the only slice form).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarDeclaration {
    pub name: Name,
    pub address: Expr,
}

/// `func NAME @a8 @i16 { ... }`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub width: Vec<WidthAnnotation>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WidthAnnotation {
    pub kind: WidthAnnotationKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WidthAnnotationKind {
    A8,
    A16,
    I8,
    I16,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind {
    Native(NativeInstruction),
    Hla(HlaStatement),
    Return,
    /// Malformed statement; the span covers the unparsable region.
    Error,
}

/// A native instruction as written, e.g. `lda #1`. The mnemonic stays source
/// text here; it is validated against the typed ISA during lowering.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeInstruction {
    pub mnemonic: Name,
    pub operand: Option<NativeOperand>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NativeOperand {
    /// `#expr`
    Immediate(Expr),
    /// A symbol used as an address operand, e.g. `sta SCREEN`.
    Address(Name),
}

/// HLA statements stay distinct from native syntax until lowering.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HlaStatement {
    /// `a = #imm`
    LoadImmediate { register: Register, value: Expr },
    /// `MEM = a`
    Store { target: Name, register: Register },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Register {
    pub kind: RegisterKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterKind {
    A,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    /// A numeric literal, already decoded from its source form.
    Number(u32),
    /// Malformed or missing expression.
    Error,
}
