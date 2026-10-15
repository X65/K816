/*****************************************************************************/
/*                                                                           */
/*                                   op.rs                                   */
/*                                                                           */
/*        The common lowered operation model shared by native and HLA        */
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

//! `KOp`: the common, assembly-shaped operation both native and HLA syntax
//! lower into.
//!
//! After lowering, the optimizer and emitter must not care which syntax
//! produced an operation — only its [`Origin`] records that, for source
//! annotations. `doc/design.md` §7 is the authority for the shape.
//!
//! An [`Instruction`] stores its [`AddressingMode`] explicitly. Deriving the
//! mode from the operand shape only works while direct, absolute, and long
//! are distinguishable by shape, which stops being true the moment the ISA
//! table carries all 24 hardware modes: `lda $10`, `lda a:$10`, and
//! `lda f:$10` are one source shape and three modes. Classification chooses
//! the mode; `effects()` keys on `(mnemonic, mode)`.

use crate::effects::{Effects, instruction_effects};
use crate::isa::{AddressEncoding, AddressingMode, Mnemonic};
use crate::source::Span;
use crate::symbol::{AddressSize, SymbolId};
use crate::value::{KExpr, Value};

pub use crate::isa::RegisterWidth;

/// Where a lowered operation came from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    /// Written by the user as a native instruction.
    User,
    /// Expanded from an HLA statement.
    HlaExpansion,
    /// Synthesized by the compiler (entry-mode setup, implicit `rts`).
    CompilerGenerated,
}

/// One lowered operation with its source position and origin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KOp {
    pub kind: KOpKind,
    pub span: Span,
    pub origin: Origin,
    /// Why the compiler produced this operation, where [`Origin`] alone does
    /// not say enough for a reader to check it.
    ///
    /// `Origin::CompilerGenerated` answers *who* wrote an operation; this
    /// answers *what for*, and only the model that decided it can say. A
    /// `rep #$20` is a bridge for one callee's contract, or a scoped block's
    /// entry, or a label bridge every arrival needs, and the instruction is
    /// identical in all three. Deriving the answer at emission from the
    /// operations around it worked while there was one producer of mode
    /// operations; carrying it is what keeps that true once there are
    /// several.
    pub reason: Option<Reason>,
}

impl KOp {
    pub fn new(kind: KOpKind, span: Span, origin: Origin) -> Self {
        Self {
            kind,
            span,
            origin,
            reason: None,
        }
    }

    /// The same operation, recording what the compiler produced it for.
    pub fn because(mut self, reason: Reason) -> Self {
        self.reason = Some(reason);
        self
    }
}

/// What a compiler-generated operation is for.
///
/// Every variant names a decision some model made, so the annotation on an
/// emitted line and the line in an IR dump both state the decision rather
/// than restating the instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Reason {
    /// `main` has no caller, so its contract is established at entry.
    EntryPoint,
    /// Direct-page bytes reserved so the allocator steps over a *pinned*
    /// slot. A pinned declaration is an equate and reserves nothing, so
    /// without these the linker would hand an allocated variable an address
    /// the source had already claimed.
    PinnedSlot,
    /// The function's declared entry contract, assumed rather than executed:
    /// its caller is what bridges to it.
    EntryContract,
    /// A bare `@a16` statement.
    ModeSet,
    /// Entering a scoped width block.
    BlockEnter,
    /// Restoring the width a scoped block changed.
    BlockRestore,
    /// Switching a branch to the entry width a width statement in front of
    /// its target label states: the label is entered like a function, and
    /// the caller is what bridges.
    LabelEntry(SymbolId),
    /// The width every path to a label agrees on.
    LabelMerge,
    /// Satisfying a callee's entry contract before transferring to it.
    CallBridge(SymbolId),
    /// What a callee's exit contract promises about the caller's widths.
    CalleeExit(SymbolId),
    /// A callee K816 cannot see. The language requires such a function to
    /// leave the caller's widths alone, so the answer is assumed rather than
    /// derived — which is worth saying, because nothing else distinguishes
    /// it from a callee that was read and found to preserve them.
    AssumedExit,
    /// The width model refused what the instruction's own effect implies —
    /// a widening `REP` executed with the emulation flag unknown establishes
    /// nothing — so the claim the instruction would carry is withdrawn.
    WithdrawnClaim,
    /// A function returns implicitly.
    ImplicitReturn,
    /// The branch a conditional lowers to, which tests the *opposite* of what
    /// the source condition says: `>={ … }` branches away on carry clear, so
    /// the emitted `bcc` and the written `>=` disagree by construction.
    InvertedBranch,
    /// A landing pad on the taken edge of a transfer that leaves a scoped
    /// width block, so the block's width is restored before control reaches
    /// the target. The restore itself is a [`Self::BlockRestore`]; this names
    /// the control flow K816 grew around it.
    SplitEdge,
    /// The target is too far for a relative branch, so the short form was
    /// replaced by the inverted-branch-over-a-jump idiom. ca65 does not relax
    /// branches, so the choice is K816's and worth naming where it made one.
    LongBranch,
    /// Expanded from an `inline` section's body. The operation's span points
    /// into the `inline`, so the reason is what says which call site put it
    /// here.
    InlineExpansion(SymbolId),
}

impl Reason {
    /// True for the five reasons that name a `REP`/`SEP` **K816 authored
    /// itself** to establish a width.
    ///
    /// The discriminator the assembler's width tracking needs. A `rep #$20`
    /// executed with the emulation flag unknown establishes *nothing* — the
    /// CPU ignores it in emulation mode — so a user-written one leaves the
    /// tracked width unknown. A bridge is different: `width::record_bridges`
    /// authors one only where the model concluded the width holds, and it
    /// refuses to author a widening one where emulation is *known* to defeat
    /// it (`K816-E0163`). What is left is the documented `xce`-then-widen
    /// escape hatch, where an `@a16` annotation is the author's assertion
    /// that the machine is native — and that assertion is what every
    /// immediate below it was sized against, so the assembler is told it
    /// (`postmortem.md` §H.13).
    pub fn is_width_bridge(self) -> bool {
        matches!(
            self,
            Self::ModeSet
                | Self::BlockEnter
                | Self::BlockRestore
                | Self::LabelEntry(_)
                | Self::CallBridge(_)
        )
    }

    /// The annotation text, given a way to name a symbol.
    pub fn describe(self, name: impl Fn(SymbolId) -> String) -> String {
        match self {
            Self::EntryPoint => "entry-point width setup".to_owned(),
            Self::PinnedSlot => "stepping over a pinned direct-page slot".to_owned(),
            Self::EntryContract => "the function's entry contract, assumed".to_owned(),
            Self::ModeSet => "the width set here".to_owned(),
            Self::BlockEnter => "entering the width block".to_owned(),
            Self::BlockRestore => "restoring the width the block changed".to_owned(),
            Self::LabelEntry(label) => format!("bridge for {}", name(label)),
            Self::LabelMerge => "the width every path to this label agrees on".to_owned(),
            Self::CallBridge(callee) => format!("bridge for {}", name(callee)),
            Self::CalleeExit(callee) => format!("{}'s exit contract", name(callee)),
            Self::AssumedExit => {
                "the callee's exit widths are unknown; assumed to preserve these".to_owned()
            }
            Self::WithdrawnClaim => {
                "the width above is not established, so no claim is carried".to_owned()
            }
            Self::ImplicitReturn => "implicit return".to_owned(),
            Self::InvertedBranch => "the source condition, inverted to skip the body".to_owned(),
            Self::LongBranch => "the target is out of a relative branch's reach".to_owned(),
            Self::SplitEdge => {
                "the landing pad this transfer restores the block's width on".to_owned()
            }
            Self::InlineExpansion(section) => format!("expanded from inline {}", name(section)),
        }
    }
}

/// The operation itself.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KOpKind {
    Instruction(Instruction),
    Label(SymbolId),
    /// The assembler-visible width claim of `doc/design.md` §7: "from here,
    /// A is 8-bit and X is 16-bit".
    ///
    /// It emits no bytes. It is what the width model concluded, as opposed to
    /// what an instruction's own [`crate::effects::WidthEffect`] establishes,
    /// and it is the authoritative of the two — the model emits one wherever
    /// its conclusion cannot be derived from the preceding instruction: at a
    /// function's entry, after a call, and at every reachable label.
    ///
    /// The standing invariant for the optimizer: a `ModeAssumption` states
    /// what the width model concluded, so a pass that removes or narrows mode
    /// operations must keep the assumptions true or delete them with the ops.
    ModeAssumption(ModeState),
    /// `segment NAME` — selects the output segment for what follows.
    ///
    /// Carries the name rather than a [`SymbolId`] as sketched in
    /// `doc/design.md` §7: segment names live in ld65's namespace, not in the
    /// unit's symbol table, and interning them there would make a segment
    /// named like a function a duplicate-symbol error.
    ///
    /// `addressing` is the ca65 *segment type*, and it is `None` for every
    /// segment a K65 source names. A segment K816 synthesizes may need it:
    /// the address size of a label is the address size of the segment it is
    /// defined in, so a direct-page reservation has to be in a segment that
    /// says `zeropage` or its `.exportzp` is a ca65 error rather than a
    /// one-byte operand.
    Segment {
        name: SegmentName,
        addressing: Option<AddressSize>,
    },
    /// Emitted bytes: `.byte` / `.word` / `.faraddr` / `.res`.
    Data(DataOp),
    /// `align N` — `.align` in ca65.
    Align(KExpr),
    /// This unit defines the symbol and other units may use it.
    Export(SymbolId),
    /// Another unit in the workspace defines the symbol.
    Import(SymbolId),
    /// A jump over inline data, and the anchor it lands on.
    ///
    /// An anonymous data block inside a function body emits raw bytes in the
    /// middle of executable code, so control has to step over them
    /// (`doc/syntax-reference.md`, "Data Blocks"). The target is written as a
    /// ca65 *unnamed* label rather than a synthesized one: a named label
    /// here would either pollute the unit's namespace or split the
    /// cheap-local scope the enclosing function's `.name` labels live in,
    /// which is backend decision 4's caveat.
    SkipData,
    /// The landing site of the [`KOpKind::SkipData`] above it.
    SkipTarget,
    /// The site a generated branch or jump names. Emits no bytes.
    Anchor(AnchorId),
    /// `nocross`: a link-time check that the `length` bytes just emitted do
    /// not straddle a 256-byte page.
    ///
    /// K816 checks the run's *size* at compile time — a run larger than a
    /// page can never fit — but only ld65 knows where it lands, so the
    /// placement half is an assertion ld65 evaluates. The legacy linker's
    /// automatic pre-padding is deliberately not recreated: K816 does not
    /// have a linker (sub-plan C, backend decision 6).
    NocrossGuard {
        length: u32,
    },
}

/// An output segment name, as ld65 sees it.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SegmentName(String);

impl SegmentName {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for SegmentName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// One emitted data directive. The entry widths beyond raw bytes belong to
/// sub-plan E's typed entries; the shape is here so C's emission machinery
/// and E's lowering meet in the same place.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataOp {
    /// `.byte` — one byte per value.
    Byte(Vec<KExpr>),
    /// `.word` — two bytes per value.
    Word(Vec<KExpr>),
    /// `.faraddr` — three bytes per value.
    Far(Vec<KExpr>),
    /// `.res N` — N reserved bytes, the `?` placeholder's expansion.
    Reserve(KExpr),
    /// `.incbin "file"` — a `binary` inclusion, which ca65 reads at assembly
    /// time.
    ///
    /// `path` is what the source wrote and what ca65 gets. `resolved` is
    /// where K816 found the file, against the including source's directory,
    /// so a later stage that needs the bytes — a section read — does not
    /// have to re-derive it. `length` is what K816 measured there, so every
    /// stage that sizes an operation stream still has an answer
    /// (`plans/06-compatibility.md` decision 8).
    IncBin {
        path: String,
        resolved: std::path::PathBuf,
        length: u32,
    },
}

/// A strongly typed instruction: validated mnemonic, chosen hardware mode,
/// and typed operand.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub mode: AddressingMode,
    pub operand: Option<Operand>,
}

/// Which index register an operand applies.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexRegister {
    X,
    Y,
}

/// A typed operand. Every value-carrying variant holds a [`KExpr`], because
/// an operand that mentions a link-time symbol must reach emission unsolved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    /// `#expr`. Whether ca65 prints the `#` is the ISA table's to say:
    /// `pea`, `brk`, and `cop` take a source `#` and emit without one.
    Immediate(KExpr),
    /// A memory reference with a chosen address encoding.
    Address {
        encoding: AddressEncoding,
        expr: KExpr,
        index: Option<IndexRegister>,
    },
    /// `(expr)`, `(expr,X)`, `(expr),Y`, `[expr]`, `[expr],Y`, `(expr,S),Y`.
    Indirect {
        encoding: AddressEncoding,
        expr: KExpr,
        /// `[expr]` rather than `(expr)`.
        long: bool,
        /// Indexing applied before the pointer is read, or the `S` of
        /// `(expr,S),Y` when `stack_relative` is set.
        inner: Option<IndexRegister>,
        /// Indexing applied to the pointer that was read.
        outer: Option<IndexRegister>,
        /// `(expr,S),Y` — the base is stack-relative.
        stack_relative: bool,
    },
    /// `expr,S`.
    StackRelative(KExpr),
    /// `src,dst` — the block moves' bank byte pair.
    BlockMove { source: KExpr, destination: KExpr },
    /// A branch, jump, or `PER` target.
    Label(KExpr),
    /// A branch or jump to a target the compiler generated: the end of a
    /// conditional's body, a loop's head, the landing pad of a split edge.
    ///
    /// It is not a `Label(KExpr)` because there is no symbol to name — a
    /// generated target has no source name to take, and inventing one would
    /// either pollute the unit's namespace or land in the cheap-local scope
    /// the function's own `.name` labels live in (sub-plan C, backend
    /// decision 4). Emission spells it as a ca65 unnamed label, which is what
    /// `SkipData` already does for the jump around inline data.
    Anchor(AnchorId),
}

/// A branch target the compiler generated, numbered within its function.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AnchorId(u32);

impl AnchorId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

impl Operand {
    /// The immediate's folded value, when this operand is an already-absolute
    /// immediate. `SEP`/`REP` narrow their flag writes with it.
    pub fn immediate_value(&self) -> Option<Value> {
        match self {
            Self::Immediate(expr) => expr.value(),
            Self::Address { .. }
            | Self::Indirect { .. }
            | Self::StackRelative(_)
            | Self::BlockMove { .. }
            | Self::Label(_)
            | Self::Anchor(_) => None,
        }
    }
}

impl Instruction {
    pub fn new(mnemonic: Mnemonic, mode: AddressingMode, operand: Option<Operand>) -> Self {
        Self {
            mnemonic,
            mode,
            operand,
        }
    }

    /// An instruction with no operand at all.
    pub fn implied(mnemonic: Mnemonic, mode: AddressingMode) -> Self {
        Self::new(mnemonic, mode, None)
    }

    /// The ISA row this instruction matched.
    pub fn spec(&self) -> Option<&'static crate::isa::InstructionSpec> {
        crate::isa::spec(self.mnemonic, self.mode)
    }

    /// The conservative CPU effects of this instruction, derived from the
    /// ISA table (never stored on the op itself).
    pub fn effects(&self) -> Effects {
        let immediate = self
            .operand
            .as_ref()
            .and_then(Operand::immediate_value)
            .map(Value::get);
        instruction_effects(self.mnemonic, self.mode, immediate)
    }
}

/// A function's entry-mode contract: the widths its body assumes on entry.
/// `None` on an axis means the contract leaves that width unspecified.
///
/// A contract is a *requirement* a caller must satisfy. The width the
/// compiler believes holds at a given operation is a [`ModeState`], and the
/// two are deliberately separate types even though their width fields
/// resemble each other: conflating them is how an entry contract silently
/// substitutes itself for a state the compiler does not actually know.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WidthContract {
    pub a: Option<RegisterWidth>,
    pub i: Option<RegisterWidth>,
}

/// One element of the per-axis width lattice: `Bottom ⊑ Known(w) ⊑ Unknown`.
///
/// Three values, not two. A dataflow solver that starts every label at
/// `Unknown` converges immediately to unknown-everywhere and is useless,
/// while one that starts at `Known` is unsound. `Bottom` is the identity of
/// the join and the only correct initial value for a block no edge has been
/// processed for; a block still at `Bottom` at the fixpoint is unreachable.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum AxisState {
    /// No path reaches here yet.
    #[default]
    Bottom,
    Known(RegisterWidth),
    /// Some path leaves this width unknowable. Sticky: only an annotation, an
    /// entry contract, a permitted `REP`/`SEP`, or a callee's `Fixed` exit
    /// clears it, and nothing may substitute a plausible default for it.
    Unknown,
}

impl AxisState {
    /// The lattice join. `Bottom` is the identity; two equal `Known` join to
    /// themselves; everything else joins to `Unknown`.
    pub fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Bottom, state) | (state, Self::Bottom) => state,
            (Self::Known(left), Self::Known(right)) if left == right => Self::Known(left),
            _ => Self::Unknown,
        }
    }

    /// The width this axis is known to have, or `None` when it is unknown or
    /// unreached. Callers must handle `None` explicitly rather than default.
    pub fn width(self) -> Option<RegisterWidth> {
        match self {
            Self::Known(width) => Some(width),
            Self::Bottom | Self::Unknown => None,
        }
    }

    /// True when no path has reached this axis yet.
    pub fn is_bottom(self) -> bool {
        self == Self::Bottom
    }
}

/// What the compiler knows about the CPU's emulation flag.
///
/// It is tracked because `REP` cannot widen a register in emulation mode:
/// `doc/w65c816s.md` says "the M and X flags are always equal to one in
/// Emulation mode", so a `rep #$20` there establishes nothing. A walk that
/// records 16-bit would accept an immediate the CPU executes as 8-bit.
///
/// Three states, and the third is the one the language cannot write. There is
/// no syntax to declare or exchange emulation mode, so nothing propagates
/// across a call and nothing annotates a function with it; what a body starts
/// in comes from the **build's entry environment** instead
/// (`--entry emulation`, `abi::EntryEnvironment`). `XCE` exchanges the carry
/// with the emulation bit, and K816 tracks neither, so it lands on `Unknown`
/// from wherever it started.
///
/// [`Self::Emulation`] is a *stronger* answer than `Unknown`, not a weaker
/// one: it says the machine is in emulation mode, so both register widths are
/// pinned at eight and a `rep` can be given an exact answer rather than
/// refused (`width.rs::believe`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum EmulationState {
    /// The emulation bit is clear: `REP`/`SEP` mean what they say.
    #[default]
    Native,
    /// The emulation bit is set: M and X are forced to one, so both registers
    /// are eight bits wide and stay that way until an `XCE`.
    Emulation,
    /// The bit was exchanged and K816 does not know which way.
    Unknown,
}

impl EmulationState {
    /// The join: an answer survives a merge only where both sides give it.
    ///
    /// `Native` is no longer an identity — `Native ⊔ Emulation` is
    /// `Unknown` — but it never needed to be. The identity of the width
    /// lattice is [`ModeState::bottom`], and [`ModeState::join`] returns the
    /// other side outright when one is bottom, so an edge no path has reached
    /// contributes nothing here either.
    pub fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Native, Self::Native) => Self::Native,
            (Self::Emulation, Self::Emulation) => Self::Emulation,
            _ => Self::Unknown,
        }
    }
}

/// The width fact the compiler believes at one point in a function body.
///
/// Its default is the join identity — both axes `Bottom`, emulation
/// `Native` — which is what a block no edge has reached yet starts at.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ModeState {
    pub a: AxisState,
    pub i: AxisState,
    pub e: EmulationState,
}

impl ModeState {
    /// The join identity: no path has reached here.
    pub fn bottom() -> Self {
        Self::default()
    }

    /// Both axes unknown — the state of a function entered with no contract.
    pub fn unknown() -> Self {
        Self {
            a: AxisState::Unknown,
            i: AxisState::Unknown,
            e: EmulationState::Native,
        }
    }

    /// The entry state a contract establishes: a declared axis is known, an
    /// undeclared one is unknown. Never `Bottom` — the entry block is reached
    /// by definition.
    ///
    /// The emulation state is a parameter because a contract cannot carry
    /// one — the language has no syntax for emulation mode — so it comes from
    /// the build's entry environment instead of from the annotation
    /// ([`crate::abi::EntryEnvironment::body_state`]). It used to be
    /// hardcoded `Native` here, which was the same claim made in a place
    /// nothing could tell it from the join identity.
    pub fn from_contract(contract: WidthContract, e: EmulationState) -> Self {
        Self {
            a: contract.a.map_or(AxisState::Unknown, AxisState::Known),
            i: contract.i.map_or(AxisState::Unknown, AxisState::Known),
            e,
        }
    }

    /// The per-axis join, used at every merge point.
    pub fn join(self, other: Self) -> Self {
        // A `Bottom` side contributes nothing at all, emulation included:
        // joining an unreached edge must not pull a reached one to unknown.
        if self.is_bottom() {
            return other;
        }
        if other.is_bottom() {
            return self;
        }
        Self {
            a: self.a.join(other.a),
            i: self.i.join(other.i),
            e: self.e.join(other.e),
        }
    }

    /// True when no path reaches this point.
    pub fn is_bottom(self) -> bool {
        self.a.is_bottom() && self.i.is_bottom()
    }
}

/// What one axis does to the caller's width across a call.
///
/// Per axis rather than per function: partial contracts already exist per
/// axis, and a body that does `sep #$20` and never touches an index register
/// is exactly `Fixed(W8)` on A and `Preserve` on X. Collapsing the two into
/// one classification would force `Unknown` and pessimize every caller.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum AxisExit {
    /// Every reachable return leaves this axis at the same width.
    Fixed(RegisterWidth),
    /// The axis is untouched on every path, so it exits as it entered.
    Preserve,
    /// Not classifiable. `doc/register-width-aware-syntax.md` requires
    /// callers to treat this as [`Self::Preserve`]; the callee is warned
    /// wherever K816 can see that the assumption may be false.
    #[default]
    Unknown,
}

impl AxisExit {
    /// The width a caller may believe after the call, given the width it
    /// believed before it. `Unknown` preserves, per the language rule.
    pub fn apply(self, before: AxisState) -> AxisState {
        match self {
            Self::Fixed(width) => AxisState::Known(width),
            Self::Preserve | Self::Unknown => before,
        }
    }
}

/// What a call does to the caller's belief about the emulation flag.
///
/// The flag is not a width, so it does not fit [`AxisExit`]: there is no
/// "sixteen bits" to fix it at, and the fallback a caller must take when
/// nothing classified the callee is not `Preserve`. It is a separate axis for
/// the same reason `E` is a separate field of [`ModeState`] — it decides what
/// the width axes *mean*.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum EmulationExit {
    /// No reachable return executed an `XCE`, so the flag comes back as the
    /// caller had it, whatever that was.
    Preserve,
    /// Every reachable return leaves the flag in the same known state.
    Fixed(EmulationState),
    /// The returns disagree, or one of them could not say.
    Unknown,
    /// Nothing classified this callee: it is outside the workspace, or in a
    /// call cycle.
    ///
    /// **`Native` survives this and `Emulation` does not**, which is the
    /// asymmetry `abi::EntryEnvironment::callee_emulation` argues at the
    /// other end of a call. Every program in the tree rests on the assumption
    /// that a callee returns onto the machine it was called from, and
    /// invalidating `Native` would make every `rep` after every call
    /// unsizeable; `Emulation` is newer and inherits no such debt, and
    /// keeping it would size an immediate at one byte for a register an
    /// unseen callee had just widened (`postmortem.md` §G.20.10).
    #[default]
    Assumed,
}

impl EmulationExit {
    /// The flag a caller may believe after the call, given what it believed
    /// before it.
    pub fn apply(self, before: EmulationState) -> EmulationState {
        match self {
            Self::Preserve => before,
            Self::Fixed(state) => state,
            Self::Unknown => EmulationState::Unknown,
            Self::Assumed => match before {
                EmulationState::Native => EmulationState::Native,
                EmulationState::Emulation | EmulationState::Unknown => EmulationState::Unknown,
            },
        }
    }
}

/// A function's outbound width contract, per axis.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ExitContract {
    pub a: AxisExit,
    pub i: AxisExit,
    /// What the call does to the caller's emulation flag.
    pub e: EmulationExit,
}

impl ExitContract {
    /// The language's fallback for a callee K816 cannot classify: leave the
    /// caller's tracked widths alone.
    pub fn preserving() -> Self {
        Self {
            a: AxisExit::Preserve,
            i: AxisExit::Preserve,
            // Not `Preserve`: this is the contract of a callee nobody
            // classified, and such a callee may execute an `XCE`. The widths
            // are grandfathered by the language rule; the emulation flag is
            // grandfathered only where it is `Native`.
            e: EmulationExit::Assumed,
        }
    }

    /// The state a caller tracks after this call.
    pub fn apply(self, before: ModeState) -> ModeState {
        ModeState {
            a: self.a.apply(before.a),
            i: self.i.apply(before.i),
            e: self.e.apply(before.e),
        }
    }
}

/// One lowered function: its symbol, entry contract, inferred exit contract,
/// and operation stream.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KFunction {
    pub symbol: SymbolId,
    pub contract: WidthContract,
    /// What callers may believe about their widths after calling it.
    pub exit: ExitContract,
    pub ops: Vec<KOp>,
}

/// One entry of a unit's ordered item stream.
///
/// Top-level labels, segment selections, data, alignment, and linkage
/// directives interleave with functions in source order, and separate
/// unordered collections would lose the lexical segment state that decides
/// where each of them lands. Emission walks this stream in order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KProgramItem {
    Function(KFunction),
    /// A unit-level operation: a segment selection, a label, data, alignment,
    /// or a linkage directive.
    Op(KOp),
    /// One top-level data block's operations, kept together as a run.
    ///
    /// A data block is bytes, and bytes need no state — until a `code { }`
    /// subblock puts instructions among them, and an instruction is sized by
    /// the width state tracked across the operations around it. That state
    /// exists over a run and not over a single operation, which is why a
    /// block arrives whole rather than one op at a time.
    Data(Vec<KOp>),
}

/// A complete lowered compilation unit.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct KProgram {
    pub items: Vec<KProgramItem>,
}

impl KProgram {
    pub fn push_function(&mut self, function: KFunction) {
        self.items.push(KProgramItem::Function(function));
    }

    /// Every operation in the program, in emission order.
    pub fn ops(&self) -> impl Iterator<Item = &KOp> {
        self.items.iter().flat_map(|item| match item {
            KProgramItem::Function(function) => function.ops.as_slice().iter(),
            KProgramItem::Op(op) => std::slice::from_ref(op).iter(),
            KProgramItem::Data(ops) => ops.as_slice().iter(),
        })
    }

    /// Adds one top-level data block's operations, as a run.
    ///
    /// A run rather than one operation at a time because a `code { }`
    /// subblock puts *instructions* among the bytes, and an instruction is
    /// emitted with the width state tracked across the ops around it. A
    /// single op has no such state; a run does.
    pub fn push_data(&mut self, ops: Vec<KOp>) {
        self.items.push(KProgramItem::Data(ops));
    }

    pub fn push_op(&mut self, op: KOp) {
        self.items.push(KProgramItem::Op(op));
    }

    /// The unit's functions, in source order.
    pub fn functions(&self) -> impl Iterator<Item = &KFunction> {
        self.items.iter().filter_map(|item| match item {
            KProgramItem::Function(function) => Some(function),
            KProgramItem::Op(_) | KProgramItem::Data(_) => None,
        })
    }

    /// The unit's functions, in source order, for passes that rewrite them.
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut KFunction> {
        self.items.iter_mut().filter_map(|item| match item {
            KProgramItem::Function(function) => Some(function),
            KProgramItem::Op(_) | KProgramItem::Data(_) => None,
        })
    }

    /// Renders the operation streams as a stable, human-readable IR dump —
    /// the `emit-ir` output that makes optimizer behavior inspectable.
    pub fn dump(
        &self,
        symbols: &crate::symbol::SymbolTable,
        map: &crate::source::SourceMap,
    ) -> String {
        let mut out = String::new();
        for (index, item) in self.items.iter().enumerate() {
            if index > 0 {
                out.push('\n');
            }
            match item {
                KProgramItem::Op(op) => {
                    let text = dump_op(op, symbols);
                    out.push_str(&format!(
                        "{:<26} ; {}\n",
                        text,
                        origin_note(op, symbols, map)
                    ));
                }
                KProgramItem::Data(ops) => {
                    for op in ops {
                        let text = dump_op(op, symbols);
                        out.push_str(&format!(
                            "{:<26} ; {}\n",
                            text,
                            origin_note(op, symbols, map)
                        ));
                    }
                }
                KProgramItem::Function(function) => {
                    out.push_str(&format!("func {}", symbols.name(function.symbol)));
                    for (width, names) in [
                        (function.contract.a, ["@a8", "@a16"]),
                        (function.contract.i, ["@i8", "@i16"]),
                    ] {
                        match width {
                            Some(RegisterWidth::W8) => out.push_str(&format!(" {}", names[0])),
                            Some(RegisterWidth::W16) => out.push_str(&format!(" {}", names[1])),
                            None => {}
                        }
                    }
                    out.push_str(&format!(" -> {}", dump_exit(function.exit)));
                    out.push_str(" {\n");
                    for op in &function.ops {
                        let text = dump_op(op, symbols);
                        out.push_str(&format!(
                            "  {:<24} ; {}\n",
                            text,
                            origin_note(op, symbols, map)
                        ));
                    }
                    out.push_str("}\n");
                }
            }
        }
        out
    }
}

/// The IR text of one operation, without its origin annotation.
///
/// Public because the optimizer's change log names operations in the same
/// spelling `emit-ir` prints them in: a reader comparing a dump against an
/// `--explain` line should not have to translate between two renderings.
pub fn dump_op(op: &KOp, symbols: &crate::symbol::SymbolTable) -> String {
    match &op.kind {
        KOpKind::Label(symbol) => format!("label {}", symbols.name(*symbol)),
        KOpKind::Segment { name, .. } => format!("segment {name}"),
        KOpKind::Align(expr) => format!("align {}", dump_expr(expr, symbols)),
        KOpKind::Export(symbol) => format!("export {}", symbols.name(*symbol)),
        KOpKind::NocrossGuard { length } => format!("nocross {length}"),
        KOpKind::SkipData => "skip-data".to_owned(),
        KOpKind::SkipTarget => "skip-target".to_owned(),
        KOpKind::Anchor(anchor) => format!("anchor#{}", anchor.index()),
        KOpKind::Import(symbol) => format!("import {}", symbols.name(*symbol)),
        KOpKind::Data(data) => dump_data(data, symbols),
        KOpKind::ModeAssumption(state) => dump_mode_state(*state),
        KOpKind::Instruction(instruction) => {
            let operand = match &instruction.operand {
                None => String::new(),
                Some(operand) => format!(" {}", dump_operand(operand, symbols)),
            };
            format!("{}{}", instruction.mnemonic.ca65(), operand)
        }
    }
}

/// The IR text of a function's inferred exit contract.
///
/// Rendered on every header rather than only where it is interesting: what a
/// caller may believe after a call is the fact call-site bridging turns into
/// instructions, so a reader checking the bridges needs to see it stated.
fn dump_exit(exit: ExitContract) -> String {
    fn axis(exit: AxisExit, names: [&'static str; 4]) -> &'static str {
        match exit {
            AxisExit::Fixed(RegisterWidth::W8) => names[0],
            AxisExit::Fixed(RegisterWidth::W16) => names[1],
            AxisExit::Preserve => names[2],
            AxisExit::Unknown => names[3],
        }
    }
    format!(
        "{} {}",
        axis(exit.a, ["@a8", "@a16", "a:preserve", "a:?"]),
        axis(exit.i, ["@i8", "@i16", "i:preserve", "i:?"]),
    )
}

/// The IR text of a mode assumption: `mode-assume @a8 @i16`.
///
/// An unknown axis renders as `@a?`, because "the model does not know" and
/// "the model did not say" are different claims and only the first of them
/// forbids a width-dependent immediate. An axis still at `Bottom` renders as
/// `@a!`: the model never emits an assumption in an unreachable block, so
/// seeing one in a dump is a compiler bug worth being able to name.
fn dump_mode_state(state: ModeState) -> String {
    fn axis(state: AxisState, names: [&'static str; 4]) -> &'static str {
        match state {
            AxisState::Known(RegisterWidth::W8) => names[0],
            AxisState::Known(RegisterWidth::W16) => names[1],
            AxisState::Unknown => names[2],
            AxisState::Bottom => names[3],
        }
    }
    let emulation = match state.e {
        EmulationState::Native => "",
        EmulationState::Emulation => " emu",
        EmulationState::Unknown => " emu?",
    };
    format!(
        "mode-assume {} {}{emulation}",
        axis(state.a, ["@a8", "@a16", "@a?", "@a!"]),
        axis(state.i, ["@i8", "@i16", "@i?", "@i!"]),
    )
}

fn dump_data(data: &DataOp, symbols: &crate::symbol::SymbolTable) -> String {
    let (keyword, values) = match data {
        DataOp::Byte(values) => ("byte", values),
        DataOp::Word(values) => ("word", values),
        DataOp::Far(values) => ("far", values),
        DataOp::Reserve(count) => return format!("res {}", dump_expr(count, symbols)),
        DataOp::IncBin { path, length, .. } => {
            return format!("incbin {path:?} ({length} bytes)");
        }
    };
    let rendered: Vec<String> = values
        .iter()
        .map(|value| dump_expr(value, symbols))
        .collect();
    format!("{keyword} {}", rendered.join(" "))
}

fn dump_operand(operand: &Operand, symbols: &crate::symbol::SymbolTable) -> String {
    match operand {
        Operand::Immediate(expr) => format!("#{}", dump_expr(expr, symbols)),
        Operand::Label(expr) => dump_expr(expr, symbols),
        Operand::Anchor(anchor) => format!("anchor#{}", anchor.index()),
        Operand::StackRelative(expr) => format!("{},s", dump_expr(expr, symbols)),
        Operand::BlockMove {
            source,
            destination,
        } => format!(
            "{},{}",
            dump_expr(source, symbols),
            dump_expr(destination, symbols)
        ),
        Operand::Address {
            encoding,
            expr,
            index,
        } => format!(
            "{} {}{}",
            encoding.k65_keyword(),
            dump_expr(expr, symbols),
            dump_index(*index)
        ),
        Operand::Indirect {
            encoding,
            expr,
            long,
            inner,
            outer,
            stack_relative,
        } => {
            let (open, close) = if *long { ("[", "]") } else { ("(", ")") };
            // A stack-relative pointer is one byte by definition, so naming
            // its encoding would be noise; every other indirection is a
            // classification decision worth showing.
            let (inner_text, encoding_text) = if *stack_relative {
                (",s".to_owned(), String::new())
            } else {
                (dump_index(*inner), format!("{} ", encoding.k65_keyword()))
            };
            format!(
                "{open}{encoding_text}{}{inner_text}{close}{}",
                dump_expr(expr, symbols),
                dump_index(*outer)
            )
        }
    }
}

fn dump_index(index: Option<IndexRegister>) -> String {
    match index {
        Some(IndexRegister::X) => ",x".to_owned(),
        Some(IndexRegister::Y) => ",y".to_owned(),
        None => String::new(),
    }
}

/// A stable, fully parenthesized rendering of a lowered expression.
///
/// The IR dump is read alongside K65 source, so it keeps the K65 operator
/// spellings rather than the ca65 ones the emitter prints. Grouping is
/// explicit for the same reason it is in emitted assembly: a reader should
/// never have to know a precedence table to check the tree.
fn dump_expr(expr: &KExpr, symbols: &crate::symbol::SymbolTable) -> String {
    match expr {
        KExpr::Number(value) => value.to_string(),
        KExpr::Symbol(symbol) => symbols.name(*symbol).to_owned(),
        KExpr::Unary { op, operand } => {
            let inner = dump_expr(operand, symbols);
            match op {
                crate::value::UnaryOp::Negate => format!("-{inner}"),
                crate::value::UnaryOp::BitNot => format!("~{inner}"),
                crate::value::UnaryOp::LowByte => format!("&<{inner}"),
                crate::value::UnaryOp::HighByte => format!("&>{inner}"),
                // K65 has no spelling for the bank byte; K816 synthesizes it.
                crate::value::UnaryOp::BankByte => format!("bank({inner})"),
                crate::value::UnaryOp::Word => format!("&&{inner}"),
                crate::value::UnaryOp::Long => format!("&&&{inner}"),
                // No K65 spelling: the use's width is what narrows it.
                crate::value::UnaryOp::Narrow { to, from } => {
                    format!("narrow{}({inner}, from {})", to * 8, from * 8)
                }
            }
        }
        KExpr::Binary { op, left, right } => format!(
            "({} {} {})",
            dump_expr(left, symbols),
            k65_operator(*op),
            dump_expr(right, symbols)
        ),
    }
}

/// The K65 spelling of a lowered binary operator.
fn k65_operator(op: crate::value::BinaryOp) -> &'static str {
    use crate::value::BinaryOp;
    match op {
        BinaryOp::Mul => "*",
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitXor => "^",
        BinaryOp::BitOr => "|",
    }
}

/// `origin file:line` annotation for one dumped op, with the reason the
/// compiler produced it when there is one.
///
/// The reason is the same text the emitted assembly carries, from the same
/// field, so the dump and the output cannot disagree about why an operation
/// exists.
fn origin_note(
    op: &KOp,
    symbols: &crate::symbol::SymbolTable,
    map: &crate::source::SourceMap,
) -> String {
    let source = map.file(op.span.file);
    let line = source.line_col(op.span.start).line + 1;
    let origin = match op.origin {
        Origin::User => "user",
        Origin::HlaExpansion => "hla",
        Origin::CompilerGenerated => "compiler",
    };
    match op.reason {
        Some(reason) => format!(
            "{origin} {}:{line} — {}",
            source.name(),
            reason.describe(|symbol| symbols.name(symbol).to_owned())
        ),
        None => format!("{origin} {}:{line}", source.name()),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        AxisExit, AxisState, EmulationExit, EmulationState, ExitContract, ModeState, RegisterWidth,
    };

    #[test]
    fn bottom_is_the_join_identity() {
        for state in [
            AxisState::Bottom,
            AxisState::Known(RegisterWidth::W8),
            AxisState::Known(RegisterWidth::W16),
            AxisState::Unknown,
        ] {
            assert_eq!(state.join(AxisState::Bottom), state);
            assert_eq!(AxisState::Bottom.join(state), state);
        }
    }

    #[test]
    fn equal_widths_join_to_themselves_and_differing_ones_to_unknown() {
        let eight = AxisState::Known(RegisterWidth::W8);
        let sixteen = AxisState::Known(RegisterWidth::W16);
        assert_eq!(eight.join(eight), eight);
        assert_eq!(eight.join(sixteen), AxisState::Unknown);
        // Unknown is sticky: nothing joins its way back out.
        assert_eq!(eight.join(AxisState::Unknown), AxisState::Unknown);
    }

    #[test]
    fn an_unreached_edge_contributes_nothing_to_a_merge() {
        // Joining a block no path has reached must not pull a reached one to
        // unknown — that is the whole reason `Bottom` exists.
        let reached = ModeState::from_contract(
            super::WidthContract {
                a: Some(RegisterWidth::W8),
                i: None,
            },
            EmulationState::Native,
        );
        assert_eq!(reached.join(ModeState::bottom()), reached);
        assert_eq!(ModeState::bottom().join(reached), reached);
    }

    #[test]
    fn an_emulation_answer_survives_a_merge_only_where_both_sides_give_it() {
        use EmulationState::{Emulation, Native, Unknown};
        assert_eq!(Native.join(Native), Native);
        assert_eq!(Emulation.join(Emulation), Emulation);
        assert_eq!(Native.join(Unknown), Unknown);
        assert_eq!(Emulation.join(Unknown), Unknown);
        // The pair that costs `Native` its identity, and the reason
        // `ModeState::join` short-circuits on `Bottom` rather than relying on
        // one.
        assert_eq!(Native.join(Emulation), Unknown);
        assert_eq!(Emulation.join(Native), Unknown);
    }

    /// An unreached edge still contributes nothing, emulation included.
    #[test]
    fn bottom_is_the_identity_that_native_stopped_being() {
        let emulating = ModeState {
            a: AxisState::Known(RegisterWidth::W8),
            i: AxisState::Known(RegisterWidth::W8),
            e: EmulationState::Emulation,
        };
        assert_eq!(emulating.join(ModeState::bottom()), emulating);
        assert_eq!(ModeState::bottom().join(emulating), emulating);
    }

    #[test]
    fn an_unknown_exit_preserves_the_callers_state() {
        // The documented `Unknown ⇒ Preserve` rule, at the type level.
        let before = ModeState::from_contract(
            super::WidthContract {
                a: Some(RegisterWidth::W16),
                i: Some(RegisterWidth::W8),
            },
            EmulationState::Native,
        );
        assert_eq!(ExitContract::default().apply(before), before);
        assert_eq!(ExitContract::preserving().apply(before), before);
        let fixes_a = ExitContract {
            a: AxisExit::Fixed(RegisterWidth::W8),
            i: AxisExit::Preserve,
            e: EmulationExit::Preserve,
        };
        let after = fixes_a.apply(before);
        assert_eq!(after.a, AxisState::Known(RegisterWidth::W8));
        assert_eq!(after.i, before.i);
        assert_eq!(after.e, before.e);
    }

    /// The emulation axis, including the asymmetry an unclassified callee
    /// carries.
    #[test]
    fn an_exit_contract_says_what_a_call_does_to_the_emulation_flag() {
        let after = |exit: EmulationExit, before: EmulationState| exit.apply(before);
        // Nothing exchanged the flag, so the caller keeps whatever it had.
        for state in [
            EmulationState::Native,
            EmulationState::Emulation,
            EmulationState::Unknown,
        ] {
            assert_eq!(after(EmulationExit::Preserve, state), state);
        }
        // A classified exchange is an answer, not a doubt.
        assert_eq!(
            after(
                EmulationExit::Fixed(EmulationState::Native),
                EmulationState::Emulation
            ),
            EmulationState::Native
        );
        assert_eq!(
            after(EmulationExit::Unknown, EmulationState::Native),
            EmulationState::Unknown
        );
        // Unclassified: `Native` is grandfathered, `Emulation` is not.
        assert_eq!(
            after(EmulationExit::Assumed, EmulationState::Native),
            EmulationState::Native
        );
        assert_eq!(
            after(EmulationExit::Assumed, EmulationState::Emulation),
            EmulationState::Unknown
        );
        // And `preserving()` — the contract of a callee nobody classified —
        // is the assumed one, not the preserving one.
        assert_eq!(ExitContract::preserving().e, EmulationExit::Assumed);
    }

    #[test]
    fn a_mode_assumption_dumps_every_axis_state() {
        assert_eq!(
            super::dump_mode_state(ModeState {
                a: AxisState::Known(RegisterWidth::W8),
                i: AxisState::Known(RegisterWidth::W16),
                e: EmulationState::Native,
            }),
            "mode-assume @a8 @i16"
        );
        assert_eq!(
            super::dump_mode_state(ModeState {
                a: AxisState::Unknown,
                i: AxisState::Bottom,
                e: EmulationState::Unknown,
            }),
            "mode-assume @a? @i! emu?"
        );
    }
}
