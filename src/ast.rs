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
    /// `evalfunc NAME(a, b) [ ... ]` — a user-defined evaluator function.
    EvalFunc(EvalFunction),
    /// `#if` / `#elif` / `#else` / `#endif` / `#error` / `#warn`.
    Preprocessor(PreprocessorDirective),
    /// `binary NAME = "file"`.
    Binary(BinaryDeclaration),
    /// Recognized legacy syntax K816 reserves and never reinterprets.
    Reserved(ReservedConstruct),
    /// A file-top `@a8` — the module's default width for that axis.
    ModuleWidth(WidthAnnotation),
    /// `import "lib.k816"` — names a unit this one may see.
    Import(ImportDeclaration),
    /// `extern func` / `extern var` / `extern const` — a symbol another
    /// object provides.
    Extern(ExternDeclaration),
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
    /// `const E:byte = -1`: the size the constant declares, if it declares
    /// one (`plans/15-constant-sizes.md` decision 6).
    pub width: Option<TypeWidth>,
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
    /// The registers and flags handed back, **in written order**, because a
    /// call site restates the clause and the two are compared as written.
    pub outputs: Vec<ContractOutput>,
    pub span: Span,
}

/// One entry of a `->` clause: a register, or a flag.
///
/// `(a) -> a, c` hands back the accumulator and the carry. A flag output is
/// the same kind of promise a register output is — it is what draws the line
/// between a *result* and damage `K816-W0123` reports
/// (`plans/06-compatibility.md` decision 19).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ContractOutput {
    Register(Register),
    /// One of `c z n v`. Which flags may be named is contract analysis, not
    /// grammar, so the name is carried as written and checked there.
    Flag(Name),
}

impl ContractOutput {
    pub fn span(&self) -> Span {
        match self {
            Self::Register(register) => register.span,
            Self::Flag(name) => name.span,
        }
    }
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
    /// `continue` — jump to the loop's test, where its `while` compare or
    /// step runs; in an `always` loop the back edge, in a `never` loop the
    /// end (`plans/18-conditions.md` decision 4).
    Continue,
    /// `nocross { ... }` — advisory page-fit request on code.
    Nocross(Block),
    /// `unroll 4 { ... }` — the block written out N times.
    Unroll(Unroll),
    /// `data { ... }` inside a function body, emitting bytes inline.
    Data(DataBlock),
    /// `align N` / `align N + M` at the head of a code section.
    Align(AlignDirective),
    /// `address $addr` at the head of a code section.
    Address(Expr),
    /// An HLA form that spells an undocumented 6502 opcode — recognized,
    /// reserved, never reinterpreted.
    Reserved(ReservedConstruct),
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
    /// `c+?`, `n-?`, `z+?`, `v-?` — or sub-plan O's `c?`, `!n?`, or K816's
    /// letter after `if` / `while`, `c`, `!n`
    Flag {
        flag: TestableFlag,
        set: bool,
        spelling: FlagTestSpelling,
    },
}

/// How a flag test was written. All three mean the same branch; a K816
/// source may use only the last (`plans/14-readable-hla.md` decisions 4 and
/// 11, `plans/17-flag-tests.md` decision 2).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FlagTestSpelling {
    /// K65's `c+?` / `c-?`.
    Signed,
    /// Sub-plan O's `c?` / `!c?`.
    Bare,
    /// `c` / `!c` after `if` or `while`, which say it is a test.
    Letter,
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

/// `>= { … }`, or K816's `if >= { … }`.
///
/// **A compare written in the `if` is not here.** `if a?10 < { … }` is built
/// as the statement `a?10` followed by this conditional, the shape K65's
/// `a?10` / `< { … }` already has, so everything after the parser — the
/// width model, lowering, the optimizer — sees one program however it was
/// spelled (`plans/16-if-statement.md` decision 5). The statement's span
/// therefore starts at the condition, never at the `if`, so that the
/// compare's span and this one do not overlap.
/// `c | z`, `!c & !z`, `a?#10 >= & a?#20 <`: one or more conditions joined
/// by one operator, each with a slot of its own (`plans/18-conditions.md`
/// decision 1). A single condition is a chain of one element with no
/// operator — the shape every condition has had since sub-plan Q, so a pass
/// that reads `elements[0]` of a one-element chain reads what it always did.
///
/// **The first element's slot is not here.** A compare or step written
/// before the first condition is the statement before the conditional, as
/// Q built it (`ConditionalBlock::compare`); a later element's slot runs
/// only when the branches before it fell through, so it stays inside the
/// element, as a block of one statement every walker descends into
/// ([`ConditionChain::slots`]).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionChain {
    pub elements: Vec<ChainElement>,
    /// `None` for a chain of one element.
    pub operator: Option<ChainOperator>,
    pub span: Span,
}

impl ConditionChain {
    /// A chain of the one condition.
    pub fn single(condition: BranchCondition) -> Self {
        let span = condition.span;
        Self {
            elements: vec![ChainElement {
                slot: None,
                condition,
                assumed: false,
                span,
            }],
            operator: None,
            span,
        }
    }

    /// The first condition — the only one, for every chain written before
    /// sub-plan S, and the one a hover or a fix on "the condition" means.
    pub fn first(&self) -> &BranchCondition {
        &self.elements[0].condition
    }

    /// The slot blocks of the elements after the first, which hold the
    /// statements a walker over nested blocks has to reach.
    pub fn slots(&self) -> impl Iterator<Item = &Block> {
        self.elements
            .iter()
            .filter_map(|element| element.slot.as_ref())
    }

    pub fn slots_mut(&mut self) -> impl Iterator<Item = &mut Block> {
        self.elements
            .iter_mut()
            .filter_map(|element| element.slot.as_mut())
    }
}

/// One condition of a chain, and the compare or step written in front of it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ChainElement {
    /// The element's compare or step — a block of one statement (two, when a
    /// step's target began with a bare call). `None` for the first element,
    /// whose slot is the statement before the conditional.
    pub slot: Option<Block>,
    pub condition: BranchCondition,
    /// The condition was not written: a step alone tests `!=`
    /// (`plans/18-conditions.md` decision 3), and a `when` value `==`
    /// (decision 2). Its span is empty, at the step's or the value's end.
    pub assumed: bool,
    pub span: Span,
}

/// How a chain's conditions combine.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChainOperator {
    /// `|` — any condition holds.
    Any,
    /// `&` — all conditions hold.
    All,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionalBlock {
    pub condition: ConditionChain,
    pub body: Block,
    /// The `else` block, if one was written.
    pub otherwise: Option<Block>,
    /// The `if` this conditional was written with, or `None` for K65's bare
    /// `COND { … }`. Which one a source may use is its dialect's question
    /// (`src/spelling.rs`); what it means is the same.
    pub keyword: Option<Span>,
    /// The span of a compare written in the `if` — the statement before
    /// this one, which is where it was built.
    pub compare: Option<Span>,
    /// The `else` was written `else if …`: `otherwise` is a block K816
    /// built around that `if`, and has no braces in the source. A chain is
    /// the nested `else { if … }` in every other respect.
    pub chained: bool,
    /// The `when` arm this conditional was built from, or `None` for one
    /// written as such.
    pub arm: Option<WhenArm>,
}

/// `unroll 4 { … }`: the block written out N times, with no counter, jump or
/// register of its own (`plans/18-conditions.md` decision 9).
///
/// As parsed, `body` is the block once. The preprocessor folds the count and
/// replaces `body` with that many copies of it, so every stage after walks
/// the statements a program that wrote them N times would have — an
/// `inline` used inside is expanded once per copy, and a width the first
/// copy leaves is the one the second starts at.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Unroll {
    /// The `unroll` keyword.
    pub keyword: Span,
    pub count: Expr,
    pub body: Block,
    /// How many copies `body` holds: one as parsed, the count once the
    /// preprocessor has expanded it.
    pub copies: u32,
}

/// An arm of `when a { 4 { … } 10, 20 { … } else { … } }`, which the
/// builder desugars into the `if a?4 == { … } else if a?10 == | a?20 == { … }
/// else { … }` it stands for (`plans/18-conditions.md` decision 2): the
/// conditional is that `if`, and this records what was written.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WhenArm {
    /// The `when` keyword.
    pub keyword: Span,
    /// The register the `when` compares.
    pub register: Register,
    /// The whole `when`, from its keyword to its closing brace.
    pub span: Span,
    /// Every value of every arm, in source order, on the first arm's
    /// conditional; empty on the others. One list is what the duplicate
    /// check and the hover read.
    pub values: Vec<HlaOperand>,
    /// How many arms have values: one conditional each.
    pub arms: usize,
}

/// `< goto done`, `< break`, `!= repeat`, or K816's `if < goto done`.
/// A compare written in the `if` is a statement before it, as for
/// [`ConditionalBlock`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionalJump {
    pub condition: ConditionChain,
    pub jump: Box<Statement>,
    /// The `if` this jump was written with, or `None` for K65's bare form.
    pub keyword: Option<Span>,
    /// The span of a compare written in the `if`, as for
    /// [`ConditionalBlock::compare`].
    pub compare: Option<Span>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LoopStatement {
    pub body: Block,
    /// `nocross { ... } !=` — the wrapper form.
    pub nocross: bool,
    pub condition: LoopCondition,
    pub spelling: LoopSpelling,
    /// `while x?10 <`, `while x--`: the span of the compare or step written
    /// in the test's first element. The statement itself is the last of
    /// `body`, which is where it runs and what K65 writes
    /// (`plans/16-if-statement.md` decision 9).
    pub test: Option<Span>,
}

/// How a loop was written. Both mean the same loop; a K816 source may use
/// only the second (`plans/14-readable-hla.md` decisions 5 and 11).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopSpelling {
    /// K65's `{ ... } !=`, `{ ... } always`, `{ ... } never`.
    Postfix,
    /// K816's `loop { ... } while !=`, `loop { ... } always`,
    /// `loop { ... } never`.
    Keyword,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LoopCondition {
    /// `{ ... } !=` — branch back while the condition holds.
    Branch(ConditionChain),
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
    /// `Some` when a `->` clause was written. A call site restates what the
    /// callee declares, flags included, so this is the declaration's shape.
    pub outputs: Option<Vec<ContractOutput>>,
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
    /// `a!!`, `dpr??`, `p!!` — or one register of `push a, x` / `pull x, a`.
    Stack(HlaStack),
    /// One constant of `push a, $0012, x`: `PEA`, two bytes whatever `M`
    /// says. `marked` when it was written with `#`, which is what lets a
    /// variable's address be pushed: a bare name is the place, as in
    /// `a = mem`, and `push` takes no place.
    PushConstant { value: Expr, marked: bool },
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

impl ArithmeticOp {
    /// The operator as written, for diagnostics that quote a statement.
    pub fn k65(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::And => "&",
            Self::Or => "|",
            Self::Xor => "^",
            Self::Compare => "?",
            Self::BitTest => "&?",
        }
    }
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
    pub spelling: StackSpelling,
}

/// How a push or pull was written. Both mean the same instruction; a K816
/// source may use only the second (`plans/14-readable-hla.md` decisions 9 and
/// 11).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StackSpelling {
    /// K65's `a!!` / `a??`.
    Shorthand,
    /// K816's `push a` / `pull a`, one register of a list.
    Word,
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

/// A register a source line can name.
///
/// One rule decides the spellings: a single letter is a flag, and a register
/// that is not `a x y s` is spelled by its datasheet abbreviation
/// (`plans/06-compatibility.md` decision 27). `a x y s` are the four the
/// original names; `c` and `d` are the carry and decimal *flags* there, so the
/// registers that once took those letters are `dpr` and `dbr`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterKind {
    /// The accumulator, at whatever width `M` selects.
    ///
    /// One name for one register: the width-independent transfers (`TCD`,
    /// `TDC`, `TCS`, `TSC`) move all sixteen bits and are spelled with this
    /// too, because a second name for the same register bought nothing but
    /// the question of which one to write.
    A,
    /// The high half of the accumulator, and *only* an `XBA` operand.
    ///
    /// Not a register in the effect model — `XBA` exchanges one register's
    /// halves, so it reads and writes `A`. It is therefore not a transfer
    /// operand, not a contract output, and not a stack target; the register
    /// `b!!` used to push is [`Self::Dbr`] (decision 30).
    B,
    /// The direct page register, spelled `dpr`.
    ///
    /// Not `dp`: that is the storage-class prefix in `var dp counter`, and a
    /// token would stop it being a declaration.
    Dpr,
    /// The data bank register, spelled `dbr`.
    Dbr,
    /// The stack pointer.
    S,
    X,
    Y,
    /// The processor status register, spelled `p` or `flag`.
    P,
}

impl RegisterKind {
    /// The register's source spelling, for diagnostics that quote a chain.
    pub fn k65(self) -> &'static str {
        match self {
            Self::A => "a",
            Self::B => "b",
            Self::Dpr => "dpr",
            Self::Dbr => "dbr",
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
    /// An integer literal, already decoded, with the one thing about its
    /// spelling the evaluator's arithmetic can see.
    ///
    /// The original parses a bracket's literals two different ways: one
    /// beginning `0x` or `0b` goes through `ParseInt` and is exact, and every
    /// other one goes through `ParseFloat` and is a **`float`**
    /// (`compiler_eval.cpp:168-172`). So `16777217` and `16777216` are the
    /// same number there and `0x1000001` and `0x1000000` are not — a
    /// distinction no decoded value can carry, which is why the flag is here
    /// rather than derived later.
    Number {
        value: u32,
        /// `false` for a decimal literal, which narrows to `f32` as the
        /// original's does. K816's `$`/`%` spellings are exact, like the
        /// `0x`/`0b` they stand for.
        exact: bool,
    },
    /// `pi` — the original's one built-in constant, and a keyword rather
    /// than a name, so nothing can shadow it.
    Pi,
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
    /// `sec[i]` and `sec[row, col]` — a byte of the section `base` names.
    /// One index or two; the grammar accepts no more.
    Subscript {
        base: Box<EvalExpr>,
        indices: Vec<EvalExpr>,
    },
    /// `x.f` — sugar for `f(x)`, which is all it has ever been
    /// (`compiler.inc:448`). Kept as its own node so a diagnostic can point
    /// at the spelling the author wrote.
    Member {
        base: Box<EvalExpr>,
        field: Name,
    },
    /// `x.f(a)` — sugar for `f(x, a)` (`compiler.inc:449`).
    MethodCall {
        base: Box<EvalExpr>,
        method: Name,
        arguments: Vec<EvalExpr>,
    },
    /// `a, b, c` — the expression list, whose value is the last item.
    List(Vec<EvalExpr>),
    /// A run of statements: the interior of `[ ... ]` with more than one, and
    /// every `{ ... }`. Its value is the last statement's, because the
    /// original's `EvalCode -> EvalCode EvalInstr` pops the previous one
    /// (`compiler.inc:360`).
    Block(Vec<EvalExpr>),
    /// `if C S` and `if C S else S`. Without an `else`, an untaken `if`
    /// evaluates to zero — the original pushes `#0` before the test and pops
    /// it only on the taken path (`compiler.inc:365-366`).
    If {
        condition: Box<EvalExpr>,
        consequence: Box<EvalExpr>,
        alternative: Option<Box<EvalExpr>>,
    },
    /// `while C S`. Its value is the last iteration's, or zero if the body
    /// never ran, for the same reason (`compiler.inc:367`).
    While {
        condition: Box<EvalExpr>,
        body: Box<EvalExpr>,
    },
    /// A call to one of the reserved evaluator functions.
    ReservedCall(ReservedCall),
    Error,
}

/// `evalfunc NAME(a, b) [ ... ]` or `evalfunc NAME(a, b) { ... }`.
///
/// The original allocates the parameters as static slots once, at definition
/// time, rather than as a frame — which is why a function that calls itself
/// there reaches evaluator function zero instead of recursing
/// (`plans/postmortem.md` §G.2.3). K816 gives each call its own frame, so
/// recursion works and is bounded by the step budget.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EvalFunction {
    pub name: Name,
    pub parameters: Vec<Name>,
    pub body: EvalExpr,
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

/// The entries a run emits, with every `nocross { … }` group spliced in where
/// it stands, and where each group began.
///
/// A group is a brace around ordinary entries: it moves no byte and it is not
/// itself one, so *every* walk that asks "what does this section emit, in what
/// order" has to see through it. That is more walks than it sounds — the
/// resolver's, the one that numbers `code { }` subblocks, the width solver's,
/// and the one that declares labels — and each of them keys something by
/// position, so a walk that stopped at the brace would key it by a different
/// position than the walk beside it. One function, so they cannot disagree.
///
/// Each `groups` entry is `(index in the flattened list where the group's own
/// entries begin, the group entry itself)`, which is what a caller needs to
/// put the group's page-fit guard after the right one. Groups nest, and an
/// inner group is listed after the outer one it sits in.
pub fn flatten_data_entries<'a>(
    entries: &'a [DataEntry],
    flat: &mut Vec<&'a DataEntry>,
    groups: &mut Vec<(usize, &'a DataEntry)>,
) {
    for entry in entries {
        match &entry.kind {
            DataEntryKind::NocrossGroup(inner) => {
                groups.push((flat.len(), entry));
                flatten_data_entries(inner, flat, groups);
            }
            _ => flat.push(entry),
        }
    }
}

/// [`flatten_data_entries`] for a caller that does not need the groups.
pub fn flat_data_entries(entries: &[DataEntry]) -> Vec<&DataEntry> {
    let mut flat = Vec::with_capacity(entries.len());
    flatten_data_entries(entries, &mut flat, &mut Vec::new());
    flat
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataEntryKind {
    /// `segment NAME`, legal only as the first entry.
    Segment(Name),
    /// `align N`, or `align N + M`.
    Align(AlignDirective),
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
    /// `nocross { ... }` around a group of entries — the same page-fit
    /// request a bare [`DataEntryKind::Nocross`] makes, bounded by the braces
    /// instead of by the end of the statically-sizeable run.
    NocrossGroup(Vec<DataEntry>),
    /// `evaluator [ ... ]` — a per-byte transform over the bytes the rest of
    /// the section *generates*, with `x` bound to each byte. Installed where
    /// it is written and replaced by a later one; literal entries are not
    /// touched (`plans/06-compatibility.md` decision 5).
    Evaluator(EvalExpr),
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
    /// `binary "file"` — the file's bytes, in place.
    Binary(BinaryInclusion),
    /// An `image` entry — recognized, reserved, never reinterpreted.
    Reserved(ReservedConstruct),
    Error,
}

/// `align N`, and the original's `align N + M`.
///
/// The offset is not the expression operator's `+`: `align 16 + 4` means
/// "at an address congruent to 4 modulo 16", not `align 20`. K816 lowers the
/// pair to `.align N` followed by `.res M`, and the section's label follows
/// both so that it lands in the congruence class the source asked for
/// (`plans/06-compatibility.md` decision 15).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AlignDirective {
    pub boundary: Expr,
    pub offset: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreprocessorDirective {
    pub kind: PreprocessorKind,
    /// The `#if` / `#elif` condition, when the directive takes one.
    ///
    /// An evaluator expression rather than a K65 one: the original evaluates
    /// a condition through the same interpreter a bracket goes through
    /// (`compiler.inc:23-25`, `PPIfNumber`), which is where `!`, `&&`, `||`
    /// and the ternary come from, and it reads the same namespace — an
    /// evaluator variable and a `const` alike (`plans/06-compatibility.md`
    /// decision 9).
    pub condition: Option<EvalExpr>,
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
    pub file: BinaryInclusion,
}

/// The file a `binary` names, and the span of the string that named it.
///
/// The span is the diagnostic's, not the entry's: a missing file is a
/// complaint about the path, and pointing at the whole line would make the
/// author look for it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryInclusion {
    /// The path exactly as the source wrote it. It is what `.incbin` gets:
    /// ca65 resolves it against its own search list, and rewriting it here
    /// would break a build that passes `--bin-include-dir`
    /// (`plans/06-compatibility.md` decision 8).
    pub path: String,
    /// The same path joined to the directory of the source that wrote it,
    /// which is where K816 checks the file exists. Recorded by the parser
    /// because that is the only stage that knows which file the line came
    /// from; a source with no directory of its own resolves against the
    /// working directory, which is what joining onto an empty prefix does.
    pub resolved: std::path::PathBuf,
    pub span: Span,
}

/// `import "lib.k816"` — the unit set, stated in the source.
///
/// The payload is `binary`'s, because the question is the same one: which
/// file does this string name, relative to the source that wrote it. What
/// differs is who answers it — ca65 resolves a `binary` path against its own
/// search list, so `path` is handed on verbatim, while an `import` is
/// resolved by K816 itself and only `resolved` is ever opened.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportDeclaration {
    pub file: BinaryInclusion,
}

/// `extern func _puts @a8 -> @a16` — a symbol no K816 unit defines.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternDeclaration {
    pub kind: ExternKind,
    /// The `extern` keyword, so a diagnostic about the declaration as a
    /// whole has something shorter than the line to point at.
    pub keyword_span: Span,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExternKind {
    Func(ExternFunction),
    Var(ExternVar),
    /// `extern const NAME` — a value ld65 resolves. It has no expression,
    /// which is the whole difference from a `const`.
    Const(ExternConst),
}

/// A value another object provides, and the size it is imported at, if the
/// declaration states one (`extern const ROWS:byte` is `.importzp ROWS`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternConst {
    pub name: Name,
    pub width: Option<TypeWidth>,
}

/// A code section another object provides.
///
/// The width annotations are the *declared* contract, and unlike a
/// `Function`'s they are never checked against a body, because there is none
/// here to check (`plans/10-modules-and-ffi.md` decision 5). `exit` is a
/// width vector rather than a `CallContract`: an extern may not name output
/// registers or flags, since those exist to be verified.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternFunction {
    pub name: Name,
    /// `extern far func` — reached by JSL, returning with RTL.
    pub is_far: bool,
    pub width: Vec<WidthAnnotation>,
    pub exit: Option<ExternExit>,
}

/// The `-> @a8 @i16` half of an extern function's declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternExit {
    pub width: Vec<WidthAnnotation>,
    pub span: Span,
}

/// The name an `extern` declares, whichever form it took.
pub fn extern_name(kind: &ExternKind) -> &Name {
    match kind {
        ExternKind::Func(function) => &function.name,
        ExternKind::Var(variable) => &variable.name,
        ExternKind::Const(constant) => &constant.name,
    }
}

/// Storage another object provides. No address, no span, no count, no
/// initializer: K816 emits nothing for it, so there is nothing to describe
/// but the name and how wide its address is.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternVar {
    pub name: Name,
    pub storage: Option<StorageClass>,
    pub width: Option<TypeWidth>,
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
    /// The name the construct declares, when it declares one — `bank core`,
    /// `image sheet = "…"`. A migration diagnostic that can quote the name
    /// hands back a line the author can paste.
    pub name: Option<Name>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReservedKind {
    /// `image ...` inside a data block.
    ImageBlock,
    /// `image NAME = "file"` at the top level.
    ImageTopLevel,
    /// A reserved evaluator function used as a whole construct.
    EvalCall(EvalCallName),
    /// `bank NAME` — removed, with `segment NAME` named as the replacement.
    Bank,
    /// One of the five HLA forms that spell an undocumented 6502 opcode.
    IllegalOpcode(IllegalOpcode),
    /// `z??`, `c!!`, `n??`, `v!!`, `i??` — a flag letter's stack shorthand.
    ///
    /// In the original the letter is decoration and every one of them is
    /// plain `PLP`/`PHP`. K816 does not give them that meaning: `p??`/`p!!`
    /// say it without pretending to move one flag. All seven letters are here
    /// since `plans/06-compatibility.md` decisions 29 and 30 moved the
    /// data-bank and direct-page registers off `b` and `d`.
    StackFlag,
    /// `o`/`O` where the overflow flag is meant — `o-`, `o+`, `o-?`, `o+?`.
    ///
    /// The original spells the set/clear shorthand `o` and the branch tests
    /// `v`, one flag under two letters; K816 spells it `v` in both and names
    /// that here rather than accepting either everywhere (decision 32).
    OverflowFlagSpelling,
}

/// The five K65 HLA forms whose bytes are illegal 6502 opcodes.
///
/// Their encodings are real, documented instructions on the 65816 — a
/// different instruction each — so there is nothing to be compatible *with*,
/// and `doc/design.md` §1 puts them outside the instruction sets K816
/// assembles. Named individually so the diagnostic can say which byte column
/// the source landed in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IllegalOpcode {
    /// `a &. imm` — ANC.
    Anc,
    /// `mem = a & x` — SAX.
    Sax,
    /// `x &= a - imm` — AXS.
    Axs,
    /// `a ?-- mem` — DCP.
    Dcp,
    /// `a -++ mem` — ISC.
    Isc,
}

/// The evaluator functions that mutate sections, print, or randomize, which
/// deterministic evaluation cannot keep.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EvalCallName {
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
    ///
    /// It carries its own span because a typed entry holds several values and
    /// a diagnostic about *this* string may not point at all of them —
    /// `byte "A" "B\n"` has one escape in it and one thing to say about it.
    String { text: String, span: Span },
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

/// Visits every statement of `block` in source order, entering nested blocks
/// before moving on — the same pre-order walk that numbers them.
///
/// The match is exhaustive for the same reason
/// [`assign_statement_ids`](crate::parser) is: a statement form added later
/// that carries nested statements would otherwise be silently invisible to
/// every consumer of this walk, and "the editor knows nothing about the
/// inside of this construct" is a bug that looks like a missing feature.
pub fn visit_statements(block: &Block, visit: &mut impl FnMut(&Statement)) {
    for statement in &block.statements {
        visit_statement(statement, visit);
    }
}

fn visit_statement(statement: &Statement, visit: &mut impl FnMut(&Statement)) {
    visit(statement);
    match &statement.kind {
        StatementKind::WidthBlock(width) => visit_statements(&width.body, visit),
        StatementKind::Nocross(body) => visit_statements(body, visit),
        StatementKind::Unroll(unroll) => visit_statements(&unroll.body, visit),
        StatementKind::Loop(loop_statement) => {
            visit_statements(&loop_statement.body, visit);
            if let LoopCondition::Branch(chain) = &loop_statement.condition {
                for slot in chain.slots() {
                    visit_statements(slot, visit);
                }
            }
        }
        StatementKind::Conditional(conditional) => {
            for slot in conditional.condition.slots() {
                visit_statements(slot, visit);
            }
            visit_statements(&conditional.body, visit);
            if let Some(otherwise) = &conditional.otherwise {
                visit_statements(otherwise, visit);
            }
        }
        StatementKind::ConditionalJump(jump) => {
            for slot in jump.condition.slots() {
                visit_statements(slot, visit);
            }
            visit_statement(&jump.jump, visit);
        }
        StatementKind::Data(data) => visit_data_statements(data, visit),
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

/// Visits every statement inside a data block's `code { … }` entries.
pub fn visit_data_statements(data: &DataBlock, visit: &mut impl FnMut(&Statement)) {
    for entry in &data.entries {
        match &entry.kind {
            DataEntryKind::Code { body, .. } => visit_statements(body, visit),
            DataEntryKind::Repeat { body, .. } => visit_data_statements(body, visit),
            DataEntryKind::NocrossGroup(entries) => {
                for entry in entries {
                    if let DataEntryKind::Code { body, .. } = &entry.kind {
                        visit_statements(body, visit);
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
