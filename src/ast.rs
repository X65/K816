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
    /// `data NAME { ... }` at the top level.
    Data(DataBlock),
    /// A top-level `[ NAME = expr, ... ]` evaluator block.
    EvalBlock(Expr),
    /// `#if` / `#elif` / `#else` / `#endif` / `#error` / `#warn`.
    Preprocessor(PreprocessorDirective),
    /// `binary NAME = "file"`.
    Binary(BinaryDeclaration),
    /// Recognized legacy syntax K816 reserves and never reinterprets.
    Reserved(ReservedConstruct),
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
    /// This statement's identity within its file.
    pub id: StmtId,
}

impl Statement {
    /// A statement whose identity has not been assigned yet.
    ///
    /// The builder constructs statements as it recognizes them, which is not
    /// source order once blocks nest, so `assign_statement_ids` numbers them
    /// afterwards in one pre-order walk.
    pub fn new(kind: StatementKind, span: Span) -> Self {
        Self {
            kind,
            span,
            id: StmtId::UNASSIGNED,
        }
    }
}

/// Identifies one statement within its file.
///
/// Analysis and lowering walk the AST independently, so a per-statement fact
/// — the width state before a statement, most of all — needs a key both
/// stages can name. Re-deriving one by position is what lets the two stages
/// silently disagree, which is the failure this identity exists to prevent.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StmtId(u32);

impl StmtId {
    /// The value a freshly built statement carries until the numbering pass
    /// reaches it. Deliberately not a valid key: a lookup that finds it is a
    /// compiler bug, not a statement whose state happens to be missing.
    pub const UNASSIGNED: Self = Self(u32::MAX);

    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> u32 {
        self.0
    }

    pub fn is_assigned(self) -> bool {
        self != Self::UNASSIGNED
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind {
    /// `name:` or `.name:`
    Label(LabelStatement),
    Native(NativeInstruction),
    Hla(HlaStatement),
    /// `@a16 { ... }` — a width scoped to a block, restored after it.
    WidthBlock(WidthBlock),
    /// A bare `@a16` between statements — sets the width for the rest of the
    /// enclosing block, with no restore. That is the whole difference from a
    /// [`StatementKind::WidthBlock`].
    ModeSet(Vec<WidthAnnotation>),
    /// `call NAME`, `call far NAME`, `far NAME`.
    Call(CallStatement),
    /// `inc_x` on its own — a call or an inline expansion — and the
    /// contract-bearing form `add a, x -> a, y`.
    BareCall(BareCall),
    /// `>={ ... }` / `c-?{ ... }`, with an optional `else`.
    Conditional(ConditionalBlock),
    /// `< goto label`, `c-? break`, `!= repeat`.
    ConditionalJump(ConditionalJump),
    /// `{ ... } !=`, `{ ... } always`, `{ ... } never`.
    Loop(LoopStatement),
    /// `goto label`, `goto (vec)`, `goto far label`.
    Goto(GotoStatement),
    /// `break` on its own.
    Break,
    /// `repeat` on its own.
    Repeat,
    /// `nocross { ... }` — advisory page-fit request on code.
    Nocross(Block),
    /// `data { ... }` inside a function body, emitting bytes inline.
    Data(DataBlock),
    /// A preprocessor directive inside a code section.
    Preprocessor(PreprocessorDirective),
    Return(ReturnKind),
    /// Malformed statement; the span covers the unparsable region.
    Error,
}

/// A branch condition, in either of its two spellings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BranchCondition {
    pub kind: BranchConditionKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BranchConditionKind {
    /// `<`, `>=`, `==`, `!=`, `<0`, `>=0`, `<=`, `>`, `<<=`, `>>=`
    Comparison(ComparisonKind),
    /// `c+?`, `n-?`, `z+?`, `v-?`
    Flag { flag: TestableFlag, set: bool },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComparisonKind {
    /// `<` — BCC
    Less,
    /// `>=` — BCS
    GreaterEqual,
    /// `==` — BEQ
    Equal,
    /// `!=` — BNE
    NotEqual,
    /// `<0` — BMI
    Negative,
    /// `>=0` — BPL
    NotNegative,
    /// `<<=` — BVS
    OverflowSet,
    /// `>>=` — BVC
    OverflowClear,
    /// `>` — the inefficient forward form.
    Greater,
    /// `<=` — the inefficient backward form.
    LessEqual,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TestableFlag {
    Carry,
    Negative,
    Zero,
    Overflow,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionalBlock {
    pub condition: BranchCondition,
    pub body: Block,
    /// The `else` block, if one was written.
    pub otherwise: Option<Block>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionalJump {
    pub condition: BranchCondition,
    pub jump: Box<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LoopStatement {
    pub body: Block,
    /// `nocross { ... } !=` — the wrapper form.
    pub nocross: bool,
    pub condition: LoopCondition,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopCondition {
    /// `{ ... } !=` — branch back while the condition holds.
    Branch(BranchCondition),
    /// `{ ... } always` — an unconditional loop.
    Always,
    /// `{ ... } never` — a one-shot block.
    Never,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GotoStatement {
    /// The jump target. `goto (vec)` holds the vector's address expression.
    pub target: Expr,
    /// `goto (vec)` — jump through the address stored at the target.
    pub indirect: bool,
    /// `goto far label` — JML rather than JMP.
    pub is_far: bool,
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

/// A call written as just the callee's name, optionally stating the register
/// flow: `add a, x -> a, y`. Omitting the clause keeps the legacy unchecked
/// behavior, which is why both halves are optional and distinguishable from
/// empty.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BareCall {
    pub target: Name,
    /// `Some` when an argument list was written.
    pub arguments: Option<Vec<CallArgument>>,
    /// `Some` when a `->` clause was written.
    pub outputs: Option<Vec<Register>>,
}

/// What a call site can hand over. Registers and immediates are the whole
/// list because nothing else survives a `JSR`: K816 has no parameter-passing
/// mechanism, and the identifier aliases an `inline` may declare are bound
/// during macro expansion rather than passed at a call. See
/// `doc/syntax-reference.md` "Why aliases are inline-only".
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallArgument {
    Register(Register),
    /// `#16`
    Immediate(Expr),
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

impl RegisterKind {
    /// The register's K65 spelling, for diagnostics that quote a chain.
    pub fn k65(self) -> &'static str {
        match self {
            Self::A => "a",
            Self::B => "b",
            Self::C => "c",
            Self::D => "d",
            Self::S => "s",
            Self::X => "x",
            Self::Y => "y",
            Self::P => "p",
        }
    }
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
    /// `[ ... ]` — a compile-time evaluator bracket. Its interior is the
    /// evaluator's own language, not this one.
    Eval(Box<EvalExpr>),
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

/// An expression in the compile-time evaluator's language.
///
/// Grammar only: nothing here is evaluated, and the operator spellings are
/// kept as written rather than classified, because which of them are
/// deterministic enough to keep is the compatibility work's decision. Floats
/// keep their source text for the same reason — and because an AST that
/// derives `Eq` cannot hold one.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EvalExpr {
    pub kind: EvalExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalExprKind {
    Number(u32),
    /// A floating-point literal, as written.
    Float(String),
    /// A string literal, quotes stripped and escapes decoded.
    String(String),
    Symbol(Name),
    Call {
        name: Name,
        arguments: Vec<EvalExpr>,
    },
    Unary {
        operator: String,
        operand: Box<EvalExpr>,
        /// `true` for the suffix `++` / `--`.
        is_suffix: bool,
    },
    Binary {
        operator: String,
        left: Box<EvalExpr>,
        right: Box<EvalExpr>,
    },
    Ternary {
        condition: Box<EvalExpr>,
        consequence: Box<EvalExpr>,
        alternative: Box<EvalExpr>,
    },
    Assignment {
        operator: String,
        target: Box<EvalExpr>,
        value: Box<EvalExpr>,
    },
    Subscript {
        base: Box<EvalExpr>,
        index: Box<EvalExpr>,
    },
    Member {
        base: Box<EvalExpr>,
        field: Name,
    },
    /// `a, b, c` — the expression list, whose value is the last item.
    List(Vec<EvalExpr>),
    /// A call to one of the reserved evaluator functions.
    ReservedCall(ReservedCall),
    Error,
}

/// `data NAME { ... }` or `data { ... }`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataBlock {
    pub name: Option<Name>,
    /// `nocross data { ... }` — the wrapper form of the page-fit request.
    pub nocross: bool,
    pub entries: Vec<DataEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataEntry {
    pub kind: DataEntryKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataEntryKind {
    /// `segment NAME`, legal only as the first entry.
    Segment(Name),
    /// `align N`
    Align(Expr),
    /// `address $addr`
    Address(Expr),
    /// `nocross` — guards the run of statically-sizeable entries that
    /// follows it, and takes no operand. See `grammar.js` for why the older
    /// `nocross EXPR` form is gone.
    Nocross,
    /// `charset "..."`
    Charset(String),
    /// `hello:`
    Label(Name),
    /// `byte 1 2 3`, `word main 0`, `far handler`.
    Typed {
        width: TypeWidthKind,
        values: Vec<DataValue>,
    },
    /// A bare value, which defaults to one byte.
    Value(DataValue),
    /// `code { ... }`, optionally `nocross`.
    Code {
        nocross: bool,
        body: Block,
    },
    /// `repeat N { ... }`
    Repeat {
        count: Expr,
        body: DataBlock,
    },
    /// `for x=A..B eval [ ... ]`
    ForEval {
        variable: Name,
        from: Expr,
        to: Expr,
        body: Expr,
    },
    /// A preprocessor directive inside a data block.
    Preprocessor(PreprocessorDirective),
    /// `binary "file"`
    Binary(String),
    /// An `image` entry — recognized, reserved, never reinterpreted.
    Reserved(ReservedConstruct),
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreprocessorDirective {
    pub kind: PreprocessorKind,
    /// The `#if` / `#elif` condition, when the directive takes one.
    pub condition: Option<Expr>,
    /// The `#error` / `#warn` message, when the directive takes one.
    pub message: Option<String>,
    pub keyword_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PreprocessorKind {
    If,
    ElseIf,
    Else,
    EndIf,
    Error,
    Warn,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryDeclaration {
    pub name: Name,
    pub file: String,
}

/// A construct that belongs to the original K65 language, is recognized as
/// its own named node so it never becomes a parse error, and is never
/// reinterpreted as K816 syntax. The compiler reports it with a
/// `K816-K65-####` diagnostic; see `doc/k65-compat.md`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReservedConstruct {
    pub kind: ReservedKind,
    /// The keyword itself, so the diagnostic can point at it precisely.
    pub keyword_span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReservedKind {
    /// `image ...` inside a data block.
    ImageBlock,
    /// `image NAME = "file"` at the top level.
    ImageTopLevel,
    /// A reserved evaluator function used as a whole construct.
    EvalCall(EvalCallName),
}

/// The evaluator functions that mutate sections, print, or randomize, which
/// deterministic evaluation cannot keep.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EvalCallName {
    Size,
    AddByte,
    Color,
    Rnd,
    Print,
    Error,
}

/// A reserved evaluator call *inside* an expression: `[ 1 + size(section) ]`
/// is arithmetic around one, and `[ max(size(a), 1) ]` nests them. Arguments
/// stay ordinary parsed expressions, which may themselves contain more.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReservedCall {
    pub name: EvalCallName,
    pub arguments: Vec<EvalExpr>,
    pub keyword_span: Span,
}

/// One value in a data entry. Value lists are atom-only, so this is an
/// expression that the grammar has already restricted.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataValue {
    /// A literal, symbol, address-byte operator, or evaluator bracket.
    Expr(Expr),
    /// A string, whose bytes go through the active `charset`.
    String(String),
    /// `?` — a reserved slot of the entry's width.
    Undefined,
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
