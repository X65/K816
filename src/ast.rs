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
    /// A file-top `@a8` — the module's default width for that axis.
    ModuleWidth(WidthAnnotation),
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

/// A code section: `func`, `naked`, `inline`, or the legacy bare `main`,
/// optionally `far`, with width annotations and a call contract.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub kind: FunctionKind,
    /// `far func` — called with JSL and returning with RTL.
    pub is_far: bool,
    pub width: Vec<WidthAnnotation>,
    pub contract: Option<CallContract>,
    pub body: Block,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FunctionKind {
    /// `func` — auto-`RTS` at the end.
    Func,
    /// `naked` — no auto-`RTS`, no exit contract.
    Naked,
    /// `inline` — expanded at the call site.
    Inline,
    /// `main { ... }` written without a section keyword.
    Main,
}

/// `(a, x) -> @a8 a, y`. Absent entirely for the legacy unchecked form;
/// present with no inputs (`()`) for a checked zero-input call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallContract {
    /// `Some` when the input list was written, even if it was empty.
    pub inputs: Option<Vec<ContractParameter>>,
    /// `Some` when a `->` clause was written.
    pub outputs: Option<ContractOutputs>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ContractParameter {
    Register(Register),
    /// `#name` / `#name:byte` — an `inline` immediate alias.
    ImmediateAlias {
        name: Name,
        width: Option<TypeWidth>,
    },
    /// A bare identifier alias for a variable or address.
    Alias(Name),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ContractOutputs {
    /// Exit-width annotations, which are check-only.
    pub width: Vec<WidthAnnotation>,
    pub registers: Vec<Register>,
    pub span: Span,
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
    /// `@a16 { ... }` — a width scoped to a block, restored after it.
    WidthBlock(WidthBlock),
    /// `call NAME`, `call far NAME`, `far NAME`.
    Call(CallStatement),
    /// `inc_x` on its own — a call or an inline expansion.
    BareCall(Name),
    Return(ReturnKind),
    /// Malformed statement; the span covers the unparsable region.
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WidthBlock {
    pub width: Vec<WidthAnnotation>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallStatement {
    pub target: Name,
    /// `call far NAME` or `far NAME` — a 24-bit call.
    pub is_far: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReturnKind {
    /// `return` — RTS (or RTL in a `far func`).
    Subroutine,
    /// `return_i` — RTI.
    Interrupt,
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
    /// A register named as the operand, as in `asl a`.
    Register(Register),
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
    /// An assignment chain: `a = #imm`, `MEM = a`, `x = a = dst0`,
    /// `UART_DATA = a = text,x`, `mem = 0`.
    Assign(HlaAssignment),
    /// `a~b` / `b~a` — the XBA exchange.
    Exchange(HlaExchange),
    /// `a+mem`, `a-#1`, `a&mask`, and the compares `a?imm` and `a&?mem`.
    Arithmetic(HlaArithmetic),
    /// `a<<`, `mem,x>>>`
    Shift(HlaShift),
    /// `x++`, `mem,x--`
    Step(HlaStep),
    /// `c+`, `d-`, `i+`, `v-`
    Flag(HlaFlag),
    /// `a!!`, `d??`, `p!!`
    Stack(HlaStack),
    /// `*` or `* N` — one or N `NOP`s.
    Nop { count: Option<Expr> },
    /// `%` — the one-character `brk #0` breakpoint marker.
    Break,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HlaArithmetic {
    pub register: Register,
    pub op: ArithmeticOp,
    pub operand: HlaOperand,
    pub op_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArithmeticOp {
    /// `+` — ADC
    Add,
    /// `-` — SBC
    Subtract,
    /// `&` — AND
    And,
    /// `|` — ORA
    Or,
    /// `^` — EOR
    Xor,
    /// `?` — CMP / CPX / CPY
    Compare,
    /// `&?` — BIT
    BitTest,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HlaShift {
    pub target: HlaOperand,
    pub op: ShiftOp,
    pub op_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ShiftOp {
    /// `<<` — ASL
    Left,
    /// `>>` — LSR
    Right,
    /// `<<<` — ROL
    RotateLeft,
    /// `>>>` — ROR
    RotateRight,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HlaStep {
    pub target: HlaOperand,
    pub op: StepOp,
    pub op_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StepOp {
    /// `++` — INC / INX / INY
    Increment,
    /// `--` — DEC / DEX / DEY
    Decrement,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HlaFlag {
    pub flag: FlagKind,
    /// `true` for `+` (set), `false` for `-` (clear).
    pub set: bool,
    pub flag_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FlagKind {
    Carry,
    Decimal,
    Interrupt,
    Overflow,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HlaStack {
    pub register: Register,
    pub op: StackOp,
    pub op_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StackOp {
    /// `!!` — push
    Push,
    /// `??` — pull
    Pull,
}

/// One assignment chain. The value flows from `source` leftwards through
/// every target, so `dst0 = dst1 = a = src` has targets `[dst0, dst1, a]`
/// and source `src`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HlaAssignment {
    pub targets: Vec<HlaOperand>,
    pub source: HlaOperand,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HlaExchange {
    pub left: Register,
    pub right: Register,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HlaOperand {
    pub kind: HlaOperandKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HlaOperandKind {
    Register(Register),
    /// `#expr`
    Immediate(Expr),
    /// A memory reference, in the same shapes a raw mnemonic accepts.
    Memory(AddressOperand),
    Indirect(IndirectOperand),
    /// Malformed operand; the span covers the unparsable region.
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Register {
    pub kind: RegisterKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterKind {
    /// The 8-bit accumulator.
    A,
    /// The high half of the 16-bit accumulator.
    B,
    /// The full 16-bit accumulator, available to transfers only.
    C,
    /// The direct page register.
    D,
    /// The stack pointer.
    S,
    X,
    Y,
    /// The processor status register, spelled `p` or `flag`.
    P,
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
