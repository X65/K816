use crate::span::{Span, Spanned};

/// Original number literal format, preserved for round-trip formatting.
/// Non-decimal variants carry the original digit count for width preservation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NumFmt {
    #[default]
    Dec, // plain decimal: 42
    Char,        // character literal: 'A', '\n'
    Zero(u8),    // zero-only decimal with explicit width: 00, 0000
    Percent(u8), // %01001010 (digit count = 8)
    Bin(u8),     // 0b01001010 (digit count = 8)
    Dollar(u8),  // $FFE0 (digit count = 4)
    Hex(u8),     // 0xFFE0 (digit count = 4)
}

/// Data width for typed variables and typed views.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataWidth {
    Byte,
    Word,
    Far,
}

/// CPU register width (8 or 16 bits).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegWidth {
    W8,
    W16,
}

/// Mode contract for function headers: optional A and Index widths.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ModeContract {
    pub a_width: Option<RegWidth>,
    pub i_width: Option<RegWidth>,
}

#[derive(Debug, Clone)]
pub struct Comment {
    pub text: String,
    pub span: Span,
}

#[derive(Debug, Clone, Default)]
pub struct File {
    pub mode_default: ModeContract,
    pub items: Vec<Spanned<Item>>,
    pub comments: Vec<Comment>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Segment(SegmentDecl),
    Const(ConstDecl),
    ConstGroup(Vec<ConstDecl>),
    EvaluatorBlock(EvaluatorBlock),
    Var(VarDecl),
    DataBlock(DataBlock),
    NamedDataBlock(NamedDataBlock),
    CodeBlock(CodeBlock),
    Statement(Stmt),
}

#[derive(Debug, Clone)]
pub struct SegmentDecl {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: String,
    pub initializer: Expr,
    pub initializer_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct EvaluatorBlock {
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub data_width: Option<DataWidth>,
    pub array_len: Option<Expr>,
    pub symbolic_subscript_fields: Option<Vec<SymbolicSubscriptFieldDecl>>,
    pub initializer: Option<Expr>,
    pub initializer_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct SymbolicSubscriptFieldDecl {
    pub name: String,
    pub data_width: Option<DataWidth>,
    pub count: Option<Expr>,
    pub count_span: Option<Span>,
    pub nested_fields: Option<Vec<SymbolicSubscriptFieldDecl>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CodeBlock {
    pub name: String,
    pub name_span: Option<Span>,
    pub is_far: bool,
    pub is_naked: bool,
    pub is_inline: bool,
    pub mode_contract: ModeContract,
    pub body: Vec<Spanned<Stmt>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Segment(SegmentDecl),
    Label(LabelDecl),
    Var(VarDecl),
    DataBlock(DataBlock),
    Address(u32),
    Align {
        boundary: u16,
        offset: u16,
    },
    Nocross(u16),
    Instruction(Instruction),
    Call(CallStmt),
    Hla(HlaStmt),
    ModeSet {
        a_width: Option<RegWidth>,
        i_width: Option<RegWidth>,
    },
    ModeScopedBlock {
        a_width: Option<RegWidth>,
        i_width: Option<RegWidth>,
        body: Vec<Spanned<Stmt>>,
    },
    SwapAB,
    Empty,
}

#[derive(Debug, Clone)]
pub struct LabelDecl {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub mnemonic: String,
    pub operand: Option<Operand>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexRegister {
    X,
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandAddrMode {
    Direct,
    Indirect,
    IndexedIndirectX,
    IndirectIndexedY,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Immediate {
        expr: Expr,
        explicit_hash: bool,
    },
    Value {
        expr: Expr,
        force_far: bool,
        index: Option<IndexRegister>,
        addr_mode: OperandAddrMode,
    },
    /// Bare HLA expression where addressing mode is deferred to lowering.
    /// The semantic model determines whether this is immediate (consts/numbers)
    /// or absolute (vars/functions/labels).
    Auto {
        expr: Expr,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HlaOperandExpr {
    pub expr: Expr,
    pub index: Option<IndexRegister>,
    pub addr_mode: OperandAddrMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaCpuRegister {
    A,
    B,
    C,
    D,
    S,
    X,
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaAluOp {
    Add,
    Sub,
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaIncDecOp {
    Inc,
    Dec,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HlaIncDecTarget {
    Register(IndexRegister),
    Address(HlaOperandExpr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaShiftOp {
    Asl,
    Lsr,
    Rol,
    Ror,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HlaShiftTarget {
    Accumulator,
    Address(HlaOperandExpr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaFlag {
    Carry,
    Decimal,
    Interrupt,
    Overflow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaStackTarget {
    A,
    P,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaRegister {
    A,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaCompareOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaBranchForm {
    FlagQuestion,
    FlagPlain,
    Symbolic,
}

#[derive(Debug, Clone)]
pub struct HlaCondition {
    pub lhs: HlaRegister,
    pub op: HlaCompareOp,
    pub rhs: Option<Expr>,
    /// Span of the `ConditionSeed` statement (e.g. `a?0`) when the condition
    /// was combined from a seed + close-op pair during normalization. Used by
    /// the lowerer to attach the CMP instruction to the correct source line.
    pub seed_span: Option<crate::span::Span>,
}

#[derive(Debug, Clone)]
pub enum HlaRhs {
    Immediate(Expr),
    Value {
        expr: Expr,
        index: Option<IndexRegister>,
        addr_mode: OperandAddrMode,
    },
}

#[derive(Debug, Clone)]
pub enum HlaStmt {
    RegisterAssign {
        register: HlaCpuRegister,
        rhs: HlaOperandExpr,
    },
    RegisterStore {
        dest: HlaOperandExpr,
        src: HlaCpuRegister,
    },
    RegisterTransfer {
        dest: HlaCpuRegister,
        src: HlaCpuRegister,
    },
    AssignmentChain {
        idents: Vec<String>,
        tail_expr: Option<HlaOperandExpr>,
    },
    AccumulatorAlu {
        op: HlaAluOp,
        rhs: HlaOperandExpr,
    },
    AccumulatorBitTest {
        rhs: HlaOperandExpr,
    },
    IndexCompare {
        register: IndexRegister,
        rhs: HlaOperandExpr,
    },
    IncDec {
        op: HlaIncDecOp,
        target: HlaIncDecTarget,
    },
    ShiftRotate {
        op: HlaShiftOp,
        target: HlaShiftTarget,
    },
    FlagSet {
        flag: HlaFlag,
        set: bool,
    },
    StackOp {
        target: HlaStackTarget,
        push: bool,
    },
    Goto {
        target: Expr,
        indirect: bool,
        far: bool,
    },
    BranchGoto {
        mnemonic: String,
        target: Expr,
        form: HlaBranchForm,
    },
    Return {
        interrupt: bool,
    },
    XAssignImmediate {
        rhs: Expr,
    },
    XIncrement,
    StoreFromA {
        dests: Vec<String>,
        rhs: HlaRhs,
        load_start: Option<usize>,
        store_end: Option<usize>,
    },
    WaitLoopWhileNFlagClear {
        symbol: String,
    },
    ConditionSeed {
        lhs: HlaRegister,
        rhs: HlaOperandExpr,
    },
    DoOpen,
    DoCloseNFlagClear,
    DoCloseNFlagSet,
    DoCloseWithOp {
        op: HlaCompareOp,
    },
    DoClose {
        condition: HlaCondition,
    },
    DoCloseAlways,
    DoCloseNever,
    DoCloseBranch {
        mnemonic: String,
    },
    LoopBreak {
        mnemonic: String,
    },
    LoopRepeat {
        mnemonic: String,
    },
    NeverBlock {
        body: Vec<Spanned<Stmt>>,
    },
    RepeatNop(usize),
    PrefixConditional {
        skip_mnemonic: String,
        form: HlaBranchForm,
        body: Vec<Spanned<Stmt>>,
        else_body: Option<Vec<Spanned<Stmt>>>,
    },
}

#[derive(Debug, Clone)]
pub struct CallStmt {
    pub target: String,
    pub is_far: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Number(i64, NumFmt),
    Ident(String),
    IdentSpanned {
        name: String,
        start: usize,
        end: usize,
    },
    EvalText(String),
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Binary {
        op: ExprBinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: ExprUnaryOp,
        expr: Box<Expr>,
    },
    TypedView {
        expr: Box<Expr>,
        width: DataWidth,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprBinaryOp {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprUnaryOp {
    LowByte,
    HighByte,
    WordLittleEndian,
    FarLittleEndian,
    EvalBracketed,
}

#[derive(Debug, Clone)]
pub struct NamedDataBlock {
    pub name: String,
    pub name_span: Span,
    pub entries: Vec<Spanned<NamedDataEntry>>,
}

#[derive(Debug, Clone)]
pub struct NamedDataForEvalRange {
    pub iterator: String,
    pub start: Expr,
    pub end: Expr,
    pub eval: String,
}

#[derive(Debug, Clone)]
pub enum NamedDataEntry {
    Segment(SegmentDecl),
    Label(String),
    Address(u32),
    Align(u16),
    Nocross(u16),
    Bytes(Vec<Expr>),
    Words(Vec<Expr>),
    Fars(Vec<Expr>),
    ForEvalRange(NamedDataForEvalRange),
    String(String),
    Repeat {
        count: u16,
        body: Vec<Spanned<NamedDataEntry>>,
    },
    Code(Vec<Spanned<Stmt>>),
    Evaluator(String),
    Charset(String),
}

#[derive(Debug, Clone)]
pub struct DataBlock {
    pub commands: Vec<Spanned<DataCommand>>,
}

#[derive(Debug, Clone)]
pub enum DataCommand {
    Align(u16),
    Address(u32),
    Nocross(u16),
    Bytes(Vec<i64>),
    Convert { kind: String, args: Vec<DataArg> },
    Ignored,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataArg {
    Int(i64),
    Str(String),
}
