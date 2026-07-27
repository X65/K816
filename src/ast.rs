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
    Const(ConstDeclaration),
    /// `segment NAME` — selects the output segment for what follows.
    Segment(Name),
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

/// A `var` declaration in any of its documented shapes:
/// `var NAME = $addr`, `var NAME`, `var dp NAME`, `var NAME:word`,
/// `var NAME[32]`, `var NAME:byte * 32`.
///
/// The three axes are independent: [`StorageClass`] chooses where the
/// variable lives and how plain references encode, [`TypeWidth`] is the
/// element's data width, and `size`/`count` are the two different ways of
/// repeating it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarDeclaration {
    pub name: Name,
    /// `abstract var` — a layout with no storage, for metadata queries only.
    pub is_abstract: bool,
    /// `dp` / `abs` / `far` before the name.
    pub storage: Option<StorageClass>,
    /// `:byte` / `:word` / `:far` after the name.
    pub width: Option<TypeWidth>,
    /// `[N]` — a raw span of N elements, without repeat-element metadata.
    pub size: Option<Expr>,
    /// `[ .x :word .y :word ]` — a symbolic subscript field list. Mutually
    /// exclusive with `size`: a bracket opens one or the other.
    pub fields: Vec<FieldDeclaration>,
    /// `* count` — the allocation count, which does create repeat elements.
    pub count: Option<Expr>,
    /// `= expr`. Absent for linker-allocated variables.
    pub address: Option<Expr>,
}

/// One `.name` entry of a symbolic subscript field list. Fields nest, and a
/// field without its own width takes the declaration's default.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldDeclaration {
    /// Keeps its leading `.`, matching the source text its span covers.
    pub name: Name,
    pub width: Option<TypeWidth>,
    /// `[count]` — repeated elements of this field.
    pub size: Option<Expr>,
    /// A nested field list.
    pub fields: Vec<FieldDeclaration>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StorageClass {
    pub kind: StorageClassKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageClassKind {
    /// `dp` — a slot in the 256-byte direct-page pool.
    DirectPage,
    /// `abs` — 16-bit absolute, the default.
    Absolute,
    /// `far` — 24-bit, and never auto-allocated.
    Far,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeWidth {
    pub kind: TypeWidthKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeWidthKind {
    Byte,
    Word,
    Far,
}

/// `const A = 0, B, C` — one comma-separated group. An entry without a value
/// continues the previous one (enum-like auto-increment).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstDeclaration {
    pub entries: Vec<ConstEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstEntry {
    pub name: Name,
    pub value: Option<Expr>,
    pub span: Span,
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
    /// `name:` or `.name:`
    Label(LabelStatement),
    Native(NativeInstruction),
    Hla(HlaStatement),
    Return,
    /// Malformed statement; the span covers the unparsable region.
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LabelStatement {
    /// The label's name. A local label keeps its leading `.`, matching the
    /// source text its span covers.
    pub name: Name,
    pub scope: LabelScope,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LabelScope {
    /// `name:` — visible across the compilation unit.
    Global,
    /// `.name:` — scoped to the enclosing function.
    Local,
}

/// A native instruction as written, e.g. `lda #1`. The mnemonic stays source
/// text here; it is validated against the typed ISA during lowering.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeInstruction {
    pub mnemonic: Name,
    pub operand: Option<NativeOperand>,
    /// `<mnemonic> * N` — repeat an implied-mode instruction N times.
    pub repeat: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NativeOperand {
    /// `#expr`
    Immediate(Expr),
    /// `A` — the accumulator itself, as in `asl a`.
    Accumulator,
    /// `[dp|abs|far] expr [,X|,Y]`. K65 does not spell zero page and absolute
    /// differently: which encoding an address gets is decided from its value
    /// and the optional prefix.
    Address(AddressOperand),
    /// `([dp|abs|far] expr [,X]) [,Y]`, including the 65816 `(expr,S),Y`.
    Indirect(IndirectOperand),
    /// `[expr]` / `[expr],Y` — indirect long. Only a raw mnemonic can take
    /// this shape; inside an HLA assignment the brackets are the evaluator's.
    LongIndirect(LongIndirectOperand),
    /// `mvn src,dst` / `mvp src,dst` — the two-operand block moves.
    BlockMove(BlockMoveOperand),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AddressOperand {
    pub encoding: Option<AddressEncoding>,
    /// `&expr` — the indexed-offset marker, which lets a constant serve as a
    /// small field offset when the struct base is in X or Y. Legal only in
    /// the indexed modes; the span is the `&` itself.
    pub offset_marker: Option<Span>,
    pub value: Expr,
    pub index: Option<IndexRegister>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LongIndirectOperand {
    pub encoding: Option<AddressEncoding>,
    pub value: Expr,
    /// `[expr],Y`
    pub outer_index: Option<IndexRegister>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockMoveOperand {
    pub source: Expr,
    pub destination: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndirectOperand {
    pub encoding: Option<AddressEncoding>,
    pub value: Expr,
    /// `(expr,X)` — indexing applied before the pointer is read.
    pub inner_index: Option<IndexRegister>,
    /// `(expr),Y` — indexing applied to the pointer that was read.
    pub outer_index: Option<IndexRegister>,
}

/// An operand-position `dp` / `abs` / `far` prefix, which forces the
/// address-encoding width independently of the value's data width.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AddressEncoding {
    pub kind: StorageClassKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IndexRegister {
    pub kind: IndexRegisterKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexRegisterKind {
    X,
    Y,
    /// `S` — stack-relative addressing, which shares the operand slot with
    /// the two index registers but is a different mode.
    Stack,
}

/// HLA statements stay distinct from native syntax until lowering.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HlaStatement {
    /// `a = #imm`
    LoadImmediate { register: Register, value: Expr },
    /// `MEM = a`, and the label+offset form `.LT+1 = a`. The target is an
    /// expression because self-modifying-code idioms address into an
    /// instruction rather than at a symbol.
    Store { target: Expr, register: Register },
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
    /// A numeric or character literal, already decoded from its source form.
    Number(u32),
    /// A reference to a named symbol: a `const`, `var`, or global label.
    Symbol(Name),
    /// A reference to a function-scoped local label, `.name`. The name keeps
    /// its leading `.`.
    LocalSymbol(Name),
    /// `-x`, `~x`, `&<x`, `&>x`, `&&x`, `&&&x`.
    Unary(UnaryExpr),
    /// `x * y`, `x | y`, ...
    Binary(BinaryExpr),
    /// `foo.field`, and the equivalent spelling `foo[.field]` — both are the
    /// same selection, so the AST keeps one shape for them.
    Field(FieldAccess),
    /// `COMP[2]` — a repeat element at a compile-time constant index.
    Subscript(SubscriptExpr),
    /// `NAME:sizeof`, `NAME.field:offsetof`.
    Metadata(MetadataQuery),
    /// `x:byte` at a use site — a data-width view of one expression.
    TypedView(TypedViewExpr),
    /// `[ ... ]` — a compile-time evaluator bracket. Its interior is a K65
    /// expression until the evaluator gets its own grammar and AST.
    Eval(Box<Expr>),
    /// Malformed or missing expression.
    Error,
}

/// Parentheses do not appear here: grouping is the tree shape. The grouped
/// expression's span still covers the parentheses, so diagnostics can point
/// at what the user wrote.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub operand: Box<Expr>,
    /// The operator's own span, for diagnostics about the operator itself.
    pub op_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    /// `-x`
    Negate,
    /// `~x`
    BitNot,
    /// `&<x` — low byte of an address.
    LowByte,
    /// `&>x` — high byte of an address.
    HighByte,
    /// `&&x` — 16-bit address.
    Address16,
    /// `&&&x` — 24-bit address.
    Address24,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op_span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldAccess {
    pub base: Box<Expr>,
    /// Keeps its leading `.`, matching the source text its span covers.
    pub field: Name,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubscriptExpr {
    pub base: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MetadataQuery {
    pub base: Box<Expr>,
    pub query: MetadataQueryKind,
    pub query_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MetadataQueryKind {
    /// `:sizeof` — the element size, never `size * count`.
    SizeOf,
    /// `:offsetof` — a field's offset within its top-level variable.
    OffsetOf,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedViewExpr {
    pub base: Box<Expr>,
    pub width: TypeWidth,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Mul,
    Add,
    Sub,
    Shl,
    Shr,
    BitAnd,
    BitXor,
    BitOr,
}
