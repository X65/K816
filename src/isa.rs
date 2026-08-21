/*****************************************************************************/
/*                                                                           */
/*                                   isa.rs                                  */
/*                                                                           */
/*             Typed 65816 instruction set: mnemonics and modes              */
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

//! The typed ISA: one authoritative description of every supported
//! instruction and addressing mode.
//!
//! K816 never encodes opcodes — ca65 owns encoding — so an
//! [`InstructionSpec`] describes legality, operand shape, operand width, and
//! ca65 spelling only. CPU effects are derived from this table in
//! `effects.rs`, keyed by the same (mnemonic, mode) pair, so they cannot
//! silently go stale. `doc/w65c816s.md` is the hardware authority behind
//! every entry.
//!
//! # Hardware mode and source shape are different axes
//!
//! [`AddressingMode`] carries the CPU's hardware mode — the 24 modes of
//! `doc/w65c816s.md` §3.5 — and nothing else. It deliberately cannot
//! classify the *source* spelling: WDC's [`AddressingMode::Stack`] groups the
//! operandless pushes, `PEA #word`, `PEI (dp)`, `PER label`, and the
//! `BRK`/`COP` signature byte, which share no operand syntax at all.
//! [`OperandForm`] is the second axis: it records the AST operand shape a
//! spec accepts. `(mnemonic, mode)` stays the unique hardware key; operand
//! classification (sub-plan C, task C4) selects among a mnemonic's specs by
//! matching `OperandForm` and then choosing an address encoding.
//!
//! The table carries all 92 mnemonics and exactly 256 legal
//! (mnemonic, mode) pairs — `doc/w65c816s.md` §1.1 states "24 addressing
//! modes … 92 instructions using 256 opcodes", which makes both a countable
//! completeness gate rather than a judgement call.

/// Declares the mnemonic set once: the variant, its ca65 spelling, and the
/// source spelling that resolves to it.
///
/// Matches over [`Mnemonic`] elsewhere stay wildcard-free, so adding a
/// mnemonic here without describing its effects still fails to compile.
macro_rules! mnemonics {
    ($($variant:ident => $spelling:literal),+ $(,)?) => {
        /// A validated 65816 mnemonic.
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub enum Mnemonic {
            $($variant),+
        }

        impl Mnemonic {
            /// Every mnemonic, for table-driven tests.
            pub const ALL: &'static [Self] = &[$(Self::$variant),+];

            /// Resolves a source spelling (case-insensitive).
            pub fn parse(text: &str) -> Option<Self> {
                let lower = text.to_ascii_lowercase();
                match lower.as_str() {
                    $($spelling => Some(Self::$variant),)+
                    _ => None,
                }
            }

            /// The ca65 spelling of this mnemonic.
            pub fn ca65(self) -> &'static str {
                match self {
                    $(Self::$variant => $spelling),+
                }
            }
        }
    };
}

mnemonics! {
    Adc => "adc", And => "and", Asl => "asl",
    Bcc => "bcc", Bcs => "bcs", Beq => "beq", Bit => "bit", Bmi => "bmi",
    Bne => "bne", Bpl => "bpl", Bra => "bra", Brk => "brk", Brl => "brl",
    Bvc => "bvc", Bvs => "bvs",
    Clc => "clc", Cld => "cld", Cli => "cli", Clv => "clv", Cmp => "cmp",
    Cop => "cop", Cpx => "cpx", Cpy => "cpy",
    Dec => "dec", Dex => "dex", Dey => "dey",
    Eor => "eor",
    Inc => "inc", Inx => "inx", Iny => "iny",
    Jml => "jml", Jmp => "jmp", Jsl => "jsl", Jsr => "jsr",
    Lda => "lda", Ldx => "ldx", Ldy => "ldy", Lsr => "lsr",
    Mvn => "mvn", Mvp => "mvp",
    Nop => "nop",
    Ora => "ora",
    Pea => "pea", Pei => "pei", Per => "per",
    Pha => "pha", Phb => "phb", Phd => "phd", Phk => "phk", Php => "php",
    Phx => "phx", Phy => "phy",
    Pla => "pla", Plb => "plb", Pld => "pld", Plp => "plp", Plx => "plx",
    Ply => "ply",
    Rep => "rep", Rol => "rol", Ror => "ror", Rti => "rti", Rtl => "rtl",
    Rts => "rts",
    Sbc => "sbc", Sec => "sec", Sed => "sed", Sei => "sei", Sep => "sep",
    Sta => "sta", Stp => "stp", Stx => "stx", Sty => "sty", Stz => "stz",
    Tax => "tax", Tay => "tay", Tcd => "tcd", Tcs => "tcs", Tdc => "tdc",
    Trb => "trb", Tsb => "tsb", Tsc => "tsc", Tsx => "tsx", Txa => "txa",
    Txs => "txs", Txy => "txy", Tya => "tya", Tyx => "tyx",
    Wai => "wai", Wdm => "wdm",
    Xba => "xba", Xce => "xce",
}

/// A hardware addressing mode: the 24 modes of `doc/w65c816s.md` §3.5, in the
/// document's own order, each named after the symbol of its Table 5-6 row.
///
/// This is the CPU's axis only. See the module documentation for why it
/// cannot double as a classification of the source operand spelling.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AddressingMode {
    /// `a` — absolute.
    Absolute,
    /// `(a,x)` — absolute indexed indirect.
    AbsoluteIndexedIndirect,
    /// `a,x` — absolute indexed with X.
    AbsoluteIndexedX,
    /// `a,y` — absolute indexed with Y.
    AbsoluteIndexedY,
    /// `(a)` — absolute indirect.
    AbsoluteIndirect,
    /// `al,x` — absolute long indexed with X.
    AbsoluteLongIndexedX,
    /// `al` — absolute long.
    AbsoluteLong,
    /// `A` — accumulator.
    Accumulator,
    /// `xyc` — block move.
    BlockMove,
    /// `(d,x)` — direct indexed indirect.
    DirectIndexedIndirect,
    /// `d,x` — direct indexed with X.
    DirectIndexedX,
    /// `d,y` — direct indexed with Y.
    DirectIndexedY,
    /// `(d),y` — direct indirect indexed.
    DirectIndirectIndexed,
    /// `[d],y` — direct indirect long indexed.
    DirectIndirectLongIndexed,
    /// `[d]` — direct indirect long.
    DirectIndirectLong,
    /// `(d)` — direct indirect.
    DirectIndirect,
    /// `d` — direct.
    Direct,
    /// `#` — immediate.
    Immediate,
    /// `i` — implied.
    Implied,
    /// `rl` — program counter relative long.
    RelativeLong,
    /// `r` — program counter relative.
    Relative,
    /// `s` — stack.
    Stack,
    /// `d,s` — stack relative.
    StackRelative,
    /// `(d,s),y` — stack relative indirect indexed.
    StackRelativeIndirectIndexed,
}

impl AddressingMode {
    /// Every hardware mode, for table-driven tests.
    pub const ALL: [Self; 24] = [
        Self::Absolute,
        Self::AbsoluteIndexedIndirect,
        Self::AbsoluteIndexedX,
        Self::AbsoluteIndexedY,
        Self::AbsoluteIndirect,
        Self::AbsoluteLongIndexedX,
        Self::AbsoluteLong,
        Self::Accumulator,
        Self::BlockMove,
        Self::DirectIndexedIndirect,
        Self::DirectIndexedX,
        Self::DirectIndexedY,
        Self::DirectIndirectIndexed,
        Self::DirectIndirectLongIndexed,
        Self::DirectIndirectLong,
        Self::DirectIndirect,
        Self::Direct,
        Self::Immediate,
        Self::Implied,
        Self::RelativeLong,
        Self::Relative,
        Self::Stack,
        Self::StackRelative,
        Self::StackRelativeIndirectIndexed,
    ];

    /// The Table 5-6 symbol for this mode, e.g. `"(d,s),y"`.
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Absolute => "a",
            Self::AbsoluteIndexedIndirect => "(a,x)",
            Self::AbsoluteIndexedX => "a,x",
            Self::AbsoluteIndexedY => "a,y",
            Self::AbsoluteIndirect => "(a)",
            Self::AbsoluteLongIndexedX => "al,x",
            Self::AbsoluteLong => "al",
            Self::Accumulator => "A",
            Self::BlockMove => "xyc",
            Self::DirectIndexedIndirect => "(d,x)",
            Self::DirectIndexedX => "d,x",
            Self::DirectIndexedY => "d,y",
            Self::DirectIndirectIndexed => "(d),y",
            Self::DirectIndirectLongIndexed => "[d],y",
            Self::DirectIndirectLong => "[d]",
            Self::DirectIndirect => "(d)",
            Self::Direct => "d",
            Self::Immediate => "#",
            Self::Implied => "i",
            Self::RelativeLong => "rl",
            Self::Relative => "r",
            Self::Stack => "s",
            Self::StackRelative => "d,s",
            Self::StackRelativeIndirectIndexed => "(d,s),y",
        }
    }

    /// The address encoding this mode's operand field uses, or `None` when
    /// the mode encodes no address (implied, accumulator, immediate, stack,
    /// relative, and block move).
    pub fn encoding(self) -> Option<AddressEncoding> {
        match self {
            Self::Direct
            | Self::DirectIndexedX
            | Self::DirectIndexedY
            | Self::DirectIndexedIndirect
            | Self::DirectIndirect
            | Self::DirectIndirectIndexed
            | Self::DirectIndirectLong
            | Self::DirectIndirectLongIndexed => Some(AddressEncoding::Direct),
            Self::Absolute
            | Self::AbsoluteIndexedX
            | Self::AbsoluteIndexedY
            | Self::AbsoluteIndexedIndirect
            | Self::AbsoluteIndirect => Some(AddressEncoding::Absolute),
            Self::AbsoluteLong | Self::AbsoluteLongIndexedX => Some(AddressEncoding::Long),
            Self::Accumulator
            | Self::BlockMove
            | Self::Immediate
            | Self::Implied
            | Self::Relative
            | Self::RelativeLong
            | Self::Stack
            | Self::StackRelative
            | Self::StackRelativeIndirectIndexed => None,
        }
    }
}

/// The width of the address an operand encodes, and the ca65 address-size
/// prefix that pins it (`doc/ca65.sgml`, "Address sizes").
///
/// K816 chooses and validates the encoding; every emitted memory operand
/// carries the prefix explicitly so ca65 cannot pick a different one from its
/// own symbol knowledge (sub-plan C, backend decision 3).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AddressEncoding {
    /// Direct page: one operand byte, `z:` in ca65.
    Direct,
    /// Absolute: two operand bytes, `a:` in ca65.
    Absolute,
    /// Absolute long: three operand bytes, `f:` in ca65.
    Long,
}

impl AddressEncoding {
    /// The ca65 address-size prefix, including its colon.
    pub fn ca65_prefix(self) -> &'static str {
        match self {
            Self::Direct => "z:",
            Self::Absolute => "a:",
            Self::Long => "f:",
        }
    }

    /// The K65 operand prefix keyword that forces this encoding.
    pub fn k65_keyword(self) -> &'static str {
        match self {
            Self::Direct => "dp",
            Self::Absolute => "abs",
            Self::Long => "far",
        }
    }

    /// The largest value this encoding can hold.
    pub fn max_value(self) -> u32 {
        match self {
            Self::Direct => 0xFF,
            Self::Absolute => 0xFFFF,
            Self::Long => 0xFF_FFFF,
        }
    }
}

/// Width of one CPU register pair per the 65816 M/X flags.
///
/// Lives here rather than in `op.rs` because it describes the CPU, and both
/// the ISA table and the effect model need it without depending on the
/// lowered operation model. `op::RegisterWidth` re-exports it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RegisterWidth {
    W8,
    W16,
}

/// How wide an immediate operand is encoded.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ImmediateWidthClass {
    /// Always one byte (`SEP`/`REP` masks, the `BRK`/`COP` signature byte).
    Fixed8,
    /// Always two bytes (`PEA`).
    Fixed16,
    /// One byte with an 8-bit accumulator, two bytes with a 16-bit one.
    MDependent,
    /// One byte with 8-bit index registers, two bytes with 16-bit ones.
    XDependent,
}

impl ImmediateWidthClass {
    /// The largest value this class admits at `width`, or `None` when the
    /// class is width-dependent and the width is unknown.
    pub fn max_value(self, a: Option<RegisterWidth>, i: Option<RegisterWidth>) -> Option<u32> {
        let limit = |width: Option<RegisterWidth>| match width {
            Some(RegisterWidth::W8) => Some(0xFF),
            Some(RegisterWidth::W16) => Some(0xFFFF),
            None => None,
        };
        match self {
            Self::Fixed8 => Some(0xFF),
            Self::Fixed16 => Some(0xFFFF),
            Self::MDependent => limit(a),
            Self::XDependent => limit(i),
        }
    }
}

/// The shape of the operand as *written in K816 source*.
///
/// One form may map to several hardware modes: `OperandForm::Address` covers
/// direct, absolute, and absolute long, which K65 does not spell differently
/// (`doc/syntax-reference.md`, "Address-encoding (prefixes)"). Classification
/// therefore matches the form first and then selects the encoding from the
/// operand prefix and the value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OperandForm {
    /// No operand at all: `rts`, `pha`, `nop`.
    Nothing,
    /// The accumulator, written explicitly (`asl a`) or left out (`asl`).
    Accumulator,
    /// `#expr`.
    Immediate,
    /// A bare address expression: `lda addr`, and the branch targets
    /// `bcc label` / `brl label` / `per label`.
    Address,
    /// `expr,X`.
    AddressIndexedX,
    /// `expr,Y`.
    AddressIndexedY,
    /// `expr,S`.
    StackRelative,
    /// `(expr)`.
    Indirect,
    /// `(expr,X)`.
    IndirectIndexedX,
    /// `(expr),Y`.
    IndirectIndexedY,
    /// `(expr,S),Y`.
    StackRelativeIndirectIndexedY,
    /// `[expr]`.
    LongIndirect,
    /// `[expr],Y`.
    LongIndirectIndexedY,
    /// `src,dst` — the two-operand block moves.
    BlockMove,
}

impl OperandForm {
    /// True when the emitted operand carries an explicit ca65 address-size
    /// prefix (`z:` / `a:` / `f:`).
    ///
    /// Only these three forms are ambiguous: one K65 spelling covers direct,
    /// absolute, and long, so without a prefix ca65 would pick the encoding
    /// from its own symbol knowledge rather than the one K816 validated
    /// (sub-plan C, backend decision 3). Every other form fixes its operand
    /// size by its shape — `(d,x)` is always direct-page, `(d,s),y` always
    /// stack-relative — and ca65 rejects a prefix there outright, which the
    /// cross-check in `tests/isa.rs` confirms against the real assembler.
    pub fn takes_address_size_prefix(self) -> bool {
        matches!(
            self,
            Self::Address | Self::AddressIndexedX | Self::AddressIndexedY
        )
    }
}

/// The encoded width of a spec's operand field, which fixes the range K816
/// validates the folded value against.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperandWidth {
    /// No operand bytes are encoded.
    None,
    /// A constant operand, sized by its [`ImmediateWidthClass`].
    Immediate(ImmediateWidthClass),
    /// An address of the given encoding width.
    Address(AddressEncoding),
    /// Two bank bytes, source then destination.
    BankPair,
    /// A displacement ca65 computes from the target address. K816 never
    /// computes a branch offset, because it does not own placement
    /// (sub-plan C, backend decision 7).
    Displacement,
}

/// What a spec accepts in operand position, and how ca65 spells it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OperandConstraint {
    /// The source operand shape this spec accepts.
    pub form: OperandForm,
    /// The encoded operand width, and therefore the legal value range.
    pub width: OperandWidth,
    /// Whether ca65 prints a `#` before the operand.
    ///
    /// Not derivable from the source form: K65 writes `pea #addr`,
    /// `brk #sig`, and `cop #sig` with a `#`, while ca65 spells all three
    /// without one, because on the hardware they are Stack-mode instructions
    /// rather than Immediate-mode ones.
    pub ca65_immediate_sigil: bool,
}

impl OperandConstraint {
    /// A form ca65 prints with a `#`.
    pub const fn immediate(form: OperandForm, width: ImmediateWidthClass) -> Self {
        Self {
            form,
            width: OperandWidth::Immediate(width),
            ca65_immediate_sigil: true,
        }
    }

    /// A form ca65 prints without a `#`.
    pub const fn plain(form: OperandForm, width: OperandWidth) -> Self {
        Self {
            form,
            width,
            ca65_immediate_sigil: false,
        }
    }
}

/// One legal (mnemonic, addressing mode) combination.
#[derive(Debug, PartialEq, Eq)]
pub struct InstructionSpec {
    pub mnemonic: Mnemonic,
    pub mode: AddressingMode,
    pub operand: OperandConstraint,
}

impl InstructionSpec {
    /// The immediate width class of this spec's operand, when it encodes a
    /// constant rather than an address, a displacement, or nothing.
    pub fn immediate_width(&self) -> Option<ImmediateWidthClass> {
        match self.operand.width {
            OperandWidth::Immediate(class) => Some(class),
            OperandWidth::None
            | OperandWidth::Address(_)
            | OperandWidth::BankPair
            | OperandWidth::Displacement => None,
        }
    }

    /// The bytes this instruction encodes: the opcode plus its operand.
    ///
    /// **Over-estimated where the answer depends on something the table does
    /// not know.** A width-dependent immediate counts as its longest form,
    /// because over-estimating is the safe direction for every consumer: a
    /// branch that thinks the distance is longer than it is emits a longer
    /// form and still assembles, and a `nocross` run that thinks it is bigger
    /// than it is reports a fit problem that does not exist rather than
    /// missing one that does. An under-estimate does neither.
    ///
    /// The answer lives here, beside [`OperandWidth`], rather than in each
    /// caller: lowering needs it to choose a branch form and to measure a
    /// `nocross` run, and the optimizer needs it to decide whether a branch
    /// E emitted long now reaches. Two implementations would put the same
    /// branch in two places.
    pub fn encoded_length(&self) -> u32 {
        1 + match self.operand.width {
            OperandWidth::None => 0,
            OperandWidth::Immediate(class) => match class {
                ImmediateWidthClass::Fixed8 => 1,
                ImmediateWidthClass::Fixed16 => 2,
                ImmediateWidthClass::MDependent | ImmediateWidthClass::XDependent => 2,
            },
            OperandWidth::Address(encoding) => match encoding {
                AddressEncoding::Direct => 1,
                AddressEncoding::Absolute => 2,
                AddressEncoding::Long => 3,
            },
            OperandWidth::BankPair => 2,
            OperandWidth::Displacement => match self.mode {
                AddressingMode::RelativeLong => 2,
                _ => 1,
            },
        }
    }

    /// The address encoding this spec's operand uses, if it encodes one.
    pub fn encoding(&self) -> Option<AddressEncoding> {
        match self.operand.width {
            OperandWidth::Address(encoding) => Some(encoding),
            OperandWidth::None
            | OperandWidth::Immediate(_)
            | OperandWidth::BankPair
            | OperandWidth::Displacement => None,
        }
    }
}

/// The memory-addressing modes every accumulator operation supports —
/// `ADC`, `AND`, `CMP`, `EOR`, `LDA`, `ORA`, `SBC`, and `STA` — in
/// `doc/w65c816s.md` Table 5-5 column order. The seven that also take an
/// immediate get it separately; `STA` does not.
const ACCUMULATOR_MODES: &[AddressingMode] = &[
    AddressingMode::DirectIndexedIndirect,
    AddressingMode::StackRelative,
    AddressingMode::Direct,
    AddressingMode::DirectIndirectLong,
    AddressingMode::Absolute,
    AddressingMode::AbsoluteLong,
    AddressingMode::DirectIndirectIndexed,
    AddressingMode::DirectIndirect,
    AddressingMode::StackRelativeIndirectIndexed,
    AddressingMode::DirectIndexedX,
    AddressingMode::DirectIndirectLongIndexed,
    AddressingMode::AbsoluteIndexedY,
    AddressingMode::AbsoluteIndexedX,
    AddressingMode::AbsoluteLongIndexedX,
];

/// The read-modify-write shape shared by the shifts, rotates, `INC`, and
/// `DEC`: the accumulator plus four memory modes.
const SHIFT_MODES: &[AddressingMode] = &[
    AddressingMode::Accumulator,
    AddressingMode::Absolute,
    AddressingMode::AbsoluteIndexedX,
    AddressingMode::Direct,
    AddressingMode::DirectIndexedX,
];

/// Every entry whose operand constraint follows from its mode alone, which
/// is all of them except the immediates and WDC's Stack group.
const ADDRESSED: &[(Mnemonic, &[AddressingMode])] = &[
    (Mnemonic::Adc, ACCUMULATOR_MODES),
    (Mnemonic::And, ACCUMULATOR_MODES),
    (Mnemonic::Cmp, ACCUMULATOR_MODES),
    (Mnemonic::Eor, ACCUMULATOR_MODES),
    (Mnemonic::Lda, ACCUMULATOR_MODES),
    (Mnemonic::Ora, ACCUMULATOR_MODES),
    (Mnemonic::Sbc, ACCUMULATOR_MODES),
    (Mnemonic::Sta, ACCUMULATOR_MODES),
    (Mnemonic::Asl, SHIFT_MODES),
    (Mnemonic::Lsr, SHIFT_MODES),
    (Mnemonic::Rol, SHIFT_MODES),
    (Mnemonic::Ror, SHIFT_MODES),
    (Mnemonic::Dec, SHIFT_MODES),
    (Mnemonic::Inc, SHIFT_MODES),
    (
        Mnemonic::Bit,
        &[
            AddressingMode::Absolute,
            AddressingMode::AbsoluteIndexedX,
            AddressingMode::Direct,
            AddressingMode::DirectIndexedX,
        ],
    ),
    (
        Mnemonic::Cpx,
        &[AddressingMode::Absolute, AddressingMode::Direct],
    ),
    (
        Mnemonic::Cpy,
        &[AddressingMode::Absolute, AddressingMode::Direct],
    ),
    (
        Mnemonic::Ldx,
        &[
            AddressingMode::Absolute,
            AddressingMode::AbsoluteIndexedY,
            AddressingMode::Direct,
            AddressingMode::DirectIndexedY,
        ],
    ),
    (
        Mnemonic::Ldy,
        &[
            AddressingMode::Absolute,
            AddressingMode::AbsoluteIndexedX,
            AddressingMode::Direct,
            AddressingMode::DirectIndexedX,
        ],
    ),
    (
        Mnemonic::Stx,
        &[
            AddressingMode::Absolute,
            AddressingMode::Direct,
            AddressingMode::DirectIndexedY,
        ],
    ),
    (
        Mnemonic::Sty,
        &[
            AddressingMode::Absolute,
            AddressingMode::Direct,
            AddressingMode::DirectIndexedX,
        ],
    ),
    (
        Mnemonic::Stz,
        &[
            AddressingMode::Absolute,
            AddressingMode::AbsoluteIndexedX,
            AddressingMode::Direct,
            AddressingMode::DirectIndexedX,
        ],
    ),
    (
        Mnemonic::Trb,
        &[AddressingMode::Absolute, AddressingMode::Direct],
    ),
    (
        Mnemonic::Tsb,
        &[AddressingMode::Absolute, AddressingMode::Direct],
    ),
    // The two long jumps are `JML`'s: the cc65 tree's ca65 spells the
    // 24-bit jump `jml al` and rejects `jmp al` outright, so `jml` is the
    // canonical mnemonic for it even though `doc/w65c816s.md` Table 5-5
    // lists that opcode under `JMP`. `jmp [a]`, which some assemblers also
    // accept, is a source alias resolved during classification — one opcode
    // keeps exactly one canonical spec. The two `JML` rows are told apart by
    // their source form: `jml al` is a bare address, `jml [a]` a bracketed
    // pointer, which is precisely why form is a second axis.
    (
        Mnemonic::Jmp,
        &[
            AddressingMode::Absolute,
            AddressingMode::AbsoluteIndirect,
            AddressingMode::AbsoluteIndexedIndirect,
        ],
    ),
    (Mnemonic::Jml, &[AddressingMode::AbsoluteLong]),
    (
        Mnemonic::Jsr,
        &[
            AddressingMode::Absolute,
            AddressingMode::AbsoluteIndexedIndirect,
        ],
    ),
    (Mnemonic::Jsl, &[AddressingMode::AbsoluteLong]),
    (Mnemonic::Mvn, &[AddressingMode::BlockMove]),
    (Mnemonic::Mvp, &[AddressingMode::BlockMove]),
    (Mnemonic::Bcc, &[AddressingMode::Relative]),
    (Mnemonic::Bcs, &[AddressingMode::Relative]),
    (Mnemonic::Beq, &[AddressingMode::Relative]),
    (Mnemonic::Bmi, &[AddressingMode::Relative]),
    (Mnemonic::Bne, &[AddressingMode::Relative]),
    (Mnemonic::Bpl, &[AddressingMode::Relative]),
    (Mnemonic::Bra, &[AddressingMode::Relative]),
    (Mnemonic::Bvc, &[AddressingMode::Relative]),
    (Mnemonic::Bvs, &[AddressingMode::Relative]),
    (Mnemonic::Brl, &[AddressingMode::RelativeLong]),
    (Mnemonic::Clc, &[AddressingMode::Implied]),
    (Mnemonic::Cld, &[AddressingMode::Implied]),
    (Mnemonic::Cli, &[AddressingMode::Implied]),
    (Mnemonic::Clv, &[AddressingMode::Implied]),
    (Mnemonic::Sec, &[AddressingMode::Implied]),
    (Mnemonic::Sed, &[AddressingMode::Implied]),
    (Mnemonic::Sei, &[AddressingMode::Implied]),
    (Mnemonic::Dex, &[AddressingMode::Implied]),
    (Mnemonic::Dey, &[AddressingMode::Implied]),
    (Mnemonic::Inx, &[AddressingMode::Implied]),
    (Mnemonic::Iny, &[AddressingMode::Implied]),
    (Mnemonic::Nop, &[AddressingMode::Implied]),
    (Mnemonic::Stp, &[AddressingMode::Implied]),
    (Mnemonic::Wai, &[AddressingMode::Implied]),
    (Mnemonic::Tax, &[AddressingMode::Implied]),
    (Mnemonic::Tay, &[AddressingMode::Implied]),
    (Mnemonic::Tcd, &[AddressingMode::Implied]),
    (Mnemonic::Tcs, &[AddressingMode::Implied]),
    (Mnemonic::Tdc, &[AddressingMode::Implied]),
    (Mnemonic::Tsc, &[AddressingMode::Implied]),
    (Mnemonic::Tsx, &[AddressingMode::Implied]),
    (Mnemonic::Txa, &[AddressingMode::Implied]),
    (Mnemonic::Txs, &[AddressingMode::Implied]),
    (Mnemonic::Txy, &[AddressingMode::Implied]),
    (Mnemonic::Tya, &[AddressingMode::Implied]),
    (Mnemonic::Tyx, &[AddressingMode::Implied]),
    (Mnemonic::Xba, &[AddressingMode::Implied]),
    (Mnemonic::Xce, &[AddressingMode::Implied]),
];

/// The instructions WDC groups under Stack mode that take no operand at all.
const OPERANDLESS_STACK: &[Mnemonic] = &[
    Mnemonic::Pha,
    Mnemonic::Phb,
    Mnemonic::Phd,
    Mnemonic::Phk,
    Mnemonic::Php,
    Mnemonic::Phx,
    Mnemonic::Phy,
    Mnemonic::Pla,
    Mnemonic::Plb,
    Mnemonic::Pld,
    Mnemonic::Plp,
    Mnemonic::Plx,
    Mnemonic::Ply,
    Mnemonic::Rti,
    Mnemonic::Rtl,
    Mnemonic::Rts,
];

/// The entries a mode cannot describe on its own.
///
/// Immediates need a width class, and WDC's Stack mode deliberately groups
/// unrelated source spellings: `PEA #word` pushes a constant, `PEI (dp)`
/// pushes the word at a direct-page pointer, `PER label` pushes a computed
/// address, and `BRK`/`COP` carry a signature byte. All five are written
/// with the shape below in K65 and, except `PEI` and `PER`, spelled without
/// a `#` by ca65 — which is why the sigil is recorded rather than derived.
const SPECIAL: &[(Mnemonic, AddressingMode, OperandConstraint)] = &[
    (
        Mnemonic::Adc,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::MDependent),
    ),
    (
        Mnemonic::And,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::MDependent),
    ),
    (
        Mnemonic::Bit,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::MDependent),
    ),
    (
        Mnemonic::Cmp,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::MDependent),
    ),
    (
        Mnemonic::Eor,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::MDependent),
    ),
    (
        Mnemonic::Lda,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::MDependent),
    ),
    (
        Mnemonic::Ora,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::MDependent),
    ),
    (
        Mnemonic::Sbc,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::MDependent),
    ),
    (
        Mnemonic::Cpx,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::XDependent),
    ),
    (
        Mnemonic::Cpy,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::XDependent),
    ),
    (
        Mnemonic::Ldx,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::XDependent),
    ),
    (
        Mnemonic::Ldy,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::XDependent),
    ),
    (
        Mnemonic::Rep,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::Fixed8),
    ),
    (
        Mnemonic::Sep,
        AddressingMode::Immediate,
        OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::Fixed8),
    ),
    // `wdm #imm` is written with a `#` and spelled `wdm $nn` by ca65.
    (
        Mnemonic::Wdm,
        AddressingMode::Immediate,
        OperandConstraint {
            form: OperandForm::Immediate,
            width: OperandWidth::Immediate(ImmediateWidthClass::Fixed8),
            ca65_immediate_sigil: false,
        },
    ),
    (
        Mnemonic::Brk,
        AddressingMode::Stack,
        OperandConstraint {
            form: OperandForm::Immediate,
            width: OperandWidth::Immediate(ImmediateWidthClass::Fixed8),
            ca65_immediate_sigil: false,
        },
    ),
    (
        Mnemonic::Cop,
        AddressingMode::Stack,
        OperandConstraint {
            form: OperandForm::Immediate,
            width: OperandWidth::Immediate(ImmediateWidthClass::Fixed8),
            ca65_immediate_sigil: false,
        },
    ),
    (
        Mnemonic::Pea,
        AddressingMode::Stack,
        OperandConstraint {
            form: OperandForm::Immediate,
            width: OperandWidth::Immediate(ImmediateWidthClass::Fixed16),
            ca65_immediate_sigil: false,
        },
    ),
    (
        Mnemonic::Pei,
        AddressingMode::Stack,
        OperandConstraint::plain(
            OperandForm::Indirect,
            OperandWidth::Address(AddressEncoding::Direct),
        ),
    ),
    (
        Mnemonic::Per,
        AddressingMode::Stack,
        OperandConstraint::plain(OperandForm::Address, OperandWidth::Displacement),
    ),
    // Absolute indirect long: two operand bytes, written `jml [expr]`.
    (
        Mnemonic::Jml,
        AddressingMode::AbsoluteIndirect,
        OperandConstraint::plain(
            OperandForm::LongIndirect,
            OperandWidth::Address(AddressEncoding::Absolute),
        ),
    ),
];

/// The operand constraint a mode implies on its own.
///
/// Every mode except `Immediate` and `Stack` fixes both the source shape and
/// the encoded width; those two are listed in [`SPECIAL`] instead.
fn derived_constraint(mode: AddressingMode) -> Option<OperandConstraint> {
    use AddressEncoding::{Absolute, Direct, Long};
    use OperandForm as F;
    use OperandWidth::Address;
    let plain = |form, width| Some(OperandConstraint::plain(form, width));
    match mode {
        AddressingMode::Direct => plain(F::Address, Address(Direct)),
        AddressingMode::DirectIndexedX => plain(F::AddressIndexedX, Address(Direct)),
        AddressingMode::DirectIndexedY => plain(F::AddressIndexedY, Address(Direct)),
        AddressingMode::DirectIndexedIndirect => plain(F::IndirectIndexedX, Address(Direct)),
        AddressingMode::DirectIndirect => plain(F::Indirect, Address(Direct)),
        AddressingMode::DirectIndirectIndexed => plain(F::IndirectIndexedY, Address(Direct)),
        AddressingMode::DirectIndirectLong => plain(F::LongIndirect, Address(Direct)),
        AddressingMode::DirectIndirectLongIndexed => {
            plain(F::LongIndirectIndexedY, Address(Direct))
        }
        AddressingMode::StackRelative => plain(F::StackRelative, Address(Direct)),
        AddressingMode::StackRelativeIndirectIndexed => {
            plain(F::StackRelativeIndirectIndexedY, Address(Direct))
        }
        AddressingMode::Absolute => plain(F::Address, Address(Absolute)),
        AddressingMode::AbsoluteIndexedX => plain(F::AddressIndexedX, Address(Absolute)),
        AddressingMode::AbsoluteIndexedY => plain(F::AddressIndexedY, Address(Absolute)),
        AddressingMode::AbsoluteIndirect => plain(F::Indirect, Address(Absolute)),
        AddressingMode::AbsoluteIndexedIndirect => plain(F::IndirectIndexedX, Address(Absolute)),
        AddressingMode::AbsoluteLong => plain(F::Address, Address(Long)),
        AddressingMode::AbsoluteLongIndexedX => plain(F::AddressIndexedX, Address(Long)),
        AddressingMode::Accumulator => plain(F::Accumulator, OperandWidth::None),
        AddressingMode::Implied => plain(F::Nothing, OperandWidth::None),
        // K816 never computes a branch displacement: it does not own
        // placement, so an out-of-range branch is ca65's error to report.
        AddressingMode::Relative | AddressingMode::RelativeLong => {
            plain(F::Address, OperandWidth::Displacement)
        }
        AddressingMode::BlockMove => plain(F::BlockMove, OperandWidth::BankPair),
        AddressingMode::Immediate | AddressingMode::Stack => None,
    }
}

/// Every legal (mnemonic, mode) pair, built once from the tables above.
static SPECS: std::sync::LazyLock<Vec<InstructionSpec>> = std::sync::LazyLock::new(|| {
    let mut specs = Vec::with_capacity(256);
    for (mnemonic, modes) in ADDRESSED {
        for &mode in *modes {
            let operand = derived_constraint(mode)
                .expect("an addressed mode always implies its operand constraint");
            specs.push(InstructionSpec {
                mnemonic: *mnemonic,
                mode,
                operand,
            });
        }
    }
    for mnemonic in OPERANDLESS_STACK {
        specs.push(InstructionSpec {
            mnemonic: *mnemonic,
            mode: AddressingMode::Stack,
            operand: OperandConstraint::plain(OperandForm::Nothing, OperandWidth::None),
        });
    }
    for (mnemonic, mode, operand) in SPECIAL {
        specs.push(InstructionSpec {
            mnemonic: *mnemonic,
            mode: *mode,
            operand: *operand,
        });
    }
    specs.sort_by_key(|spec| (spec.mnemonic, spec.mode));
    specs
});
/// Looks up the specification for a (mnemonic, mode) pair; `None` means the
/// combination is illegal on the 65816 or not yet supported.
pub fn spec(mnemonic: Mnemonic, mode: AddressingMode) -> Option<&'static InstructionSpec> {
    // The table is sorted by its own key, so lookup is a binary search.
    SPECS
        .binary_search_by_key(&(mnemonic, mode), |spec| (spec.mnemonic, spec.mode))
        .ok()
        .map(|index| &SPECS[index])
}

/// Every spec of one mnemonic, in table order — the candidate set operand
/// classification narrows by matching [`OperandForm`].
pub fn specs_for(mnemonic: Mnemonic) -> impl Iterator<Item = &'static InstructionSpec> {
    SPECS.iter().filter(move |spec| spec.mnemonic == mnemonic)
}

/// The single spec of `mnemonic` whose source shape is `form`, if there is
/// exactly one. Several address modes share a form — direct, absolute, and
/// long are one K65 spelling — so the address family is resolved by encoding
/// rather than by form alone.
pub fn spec_for_form(
    mnemonic: Mnemonic,
    form: OperandForm,
) -> impl Iterator<Item = &'static InstructionSpec> {
    specs_for(mnemonic).filter(move |spec| spec.operand.form == form)
}

/// All supported instruction specifications, for table-driven tests.
pub fn all_specs() -> &'static [InstructionSpec] {
    &SPECS
}

/// The hardware mode of `mnemonic` written without an operand, or `None`
/// when the mnemonic always takes one.
///
/// WDC spreads the operandless instructions across `i` (implied), `s`
/// (stack), and `A` (accumulator), so the mode cannot be read off the absent
/// operand; it has to be looked up.
pub fn operandless_mode(mnemonic: Mnemonic) -> Option<AddressingMode> {
    specs_for(mnemonic)
        .find(|spec| {
            matches!(
                spec.operand.form,
                OperandForm::Nothing | OperandForm::Accumulator
            )
        })
        .map(|spec| spec.mode)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `doc/w65c816s.md` §1.1: "24 addressing modes … 92 instructions using
    /// 256 opcodes". Each number is a countable completeness gate.
    #[test]
    fn the_table_carries_every_opcode_exactly_once() {
        assert_eq!(all_specs().len(), 256, "one spec per opcode");
        let mut seen = std::collections::HashSet::new();
        for spec in all_specs() {
            assert!(
                seen.insert((spec.mnemonic, spec.mode)),
                "duplicate key {:?} {:?}",
                spec.mnemonic,
                spec.mode
            );
        }
        assert_eq!(Mnemonic::ALL.len(), 92);
        assert_eq!(AddressingMode::ALL.len(), 24);
    }

    #[test]
    fn every_mnemonic_and_every_mode_is_used() {
        for mnemonic in Mnemonic::ALL {
            assert!(
                specs_for(*mnemonic).next().is_some(),
                "{mnemonic:?} has no legal mode"
            );
        }
        for mode in AddressingMode::ALL {
            assert!(
                all_specs().iter().any(|spec| spec.mode == mode),
                "{mode:?} is used by no instruction"
            );
        }
    }

    #[test]
    fn every_spec_is_reachable_through_lookup() {
        for expected in all_specs() {
            assert_eq!(spec(expected.mnemonic, expected.mode), Some(expected));
        }
    }

    /// Every legal instruction has an encoded length, and it is the opcode
    /// plus the widest form its operand can take.
    ///
    /// Over-estimating is the safe direction for both consumers: lowering
    /// chooses a longer branch form and still assembles, and the optimizer
    /// refuses to shrink a branch it cannot show reaches. An under-estimate
    /// does neither, which is a `Range error` from ca65 at best.
    #[test]
    fn every_instruction_encodes_between_one_and_four_bytes() {
        for spec in all_specs() {
            let length = spec.encoded_length();
            assert!(
                (1..=4).contains(&length),
                "{:?} {:?} encodes {length} bytes",
                spec.mnemonic,
                spec.mode
            );
        }
        // The widest immediate wins where the width is not known here.
        let lda = spec(Mnemonic::Lda, AddressingMode::Immediate).expect("lda #imm");
        assert_eq!(lda.encoded_length(), 3);
        // And a fixed one is its own size.
        let sep = spec(Mnemonic::Sep, AddressingMode::Immediate).expect("sep #imm");
        assert_eq!(sep.encoded_length(), 2);
        // A long address is three bytes of operand.
        let long = spec(Mnemonic::Lda, AddressingMode::AbsoluteLong).expect("lda long");
        assert_eq!(long.encoded_length(), 4);
        // A relative branch is one byte of displacement; the long one is two.
        assert_eq!(
            spec(Mnemonic::Bcc, AddressingMode::Relative)
                .expect("bcc")
                .encoded_length(),
            2
        );
        assert_eq!(
            spec(Mnemonic::Brl, AddressingMode::RelativeLong)
                .expect("brl")
                .encoded_length(),
            3
        );
    }

    #[test]
    fn immediate_width_is_present_exactly_for_constant_operands() {
        for spec in all_specs() {
            assert_eq!(
                spec.immediate_width().is_some(),
                spec.operand.form == OperandForm::Immediate,
                "{:?} {:?}",
                spec.mnemonic,
                spec.mode
            );
        }
    }

    #[test]
    fn mode_symbols_are_unique_across_the_twenty_four_modes() {
        let mut seen = std::collections::HashSet::new();
        for mode in AddressingMode::ALL {
            assert!(seen.insert(mode.symbol()), "duplicate symbol {mode:?}");
        }
        assert_eq!(seen.len(), 24);
    }

    #[test]
    fn address_modes_report_the_encoding_they_use() {
        assert_eq!(
            AddressingMode::Direct.encoding(),
            Some(AddressEncoding::Direct)
        );
        assert_eq!(
            AddressingMode::AbsoluteIndexedY.encoding(),
            Some(AddressEncoding::Absolute)
        );
        assert_eq!(
            AddressingMode::AbsoluteLongIndexedX.encoding(),
            Some(AddressEncoding::Long)
        );
        assert_eq!(AddressingMode::Stack.encoding(), None);
        assert_eq!(AddressingMode::Relative.encoding(), None);
    }

    #[test]
    fn immediate_width_limits_follow_the_tracked_widths() {
        use ImmediateWidthClass::*;
        assert_eq!(Fixed8.max_value(None, None), Some(0xFF));
        assert_eq!(Fixed16.max_value(None, None), Some(0xFFFF));
        assert_eq!(MDependent.max_value(None, None), None);
        assert_eq!(
            MDependent.max_value(Some(RegisterWidth::W16), None),
            Some(0xFFFF)
        );
        assert_eq!(
            XDependent.max_value(Some(RegisterWidth::W16), Some(RegisterWidth::W8)),
            Some(0xFF)
        );
    }

    /// WDC's Stack mode groups source spellings that share no syntax, which
    /// is the whole reason [`OperandForm`] is a second axis. These five are
    /// the awkward ones, and two of them are also why the ca65 `#` is
    /// recorded rather than derived from the form.
    #[test]
    fn the_stack_group_keeps_its_five_different_source_shapes() {
        let form = |mnemonic| spec(mnemonic, AddressingMode::Stack).map(|s| s.operand.form);
        assert_eq!(form(Mnemonic::Pha), Some(OperandForm::Nothing));
        assert_eq!(form(Mnemonic::Rts), Some(OperandForm::Nothing));
        assert_eq!(form(Mnemonic::Pea), Some(OperandForm::Immediate));
        assert_eq!(form(Mnemonic::Brk), Some(OperandForm::Immediate));
        assert_eq!(form(Mnemonic::Cop), Some(OperandForm::Immediate));
        assert_eq!(form(Mnemonic::Pei), Some(OperandForm::Indirect));
        assert_eq!(form(Mnemonic::Per), Some(OperandForm::Address));

        // K65 writes `pea #addr`, `brk #sig`, and `cop #sig` with a `#`;
        // ca65 spells all three without one.
        for mnemonic in [Mnemonic::Pea, Mnemonic::Brk, Mnemonic::Cop] {
            let spec = spec(mnemonic, AddressingMode::Stack).unwrap();
            assert!(!spec.operand.ca65_immediate_sigil, "{mnemonic:?}");
        }
        // A real immediate keeps it.
        assert!(
            spec(Mnemonic::Lda, AddressingMode::Immediate)
                .unwrap()
                .operand
                .ca65_immediate_sigil
        );
    }

    #[test]
    fn pea_is_a_fixed_sixteen_bit_immediate_not_an_m_dependent_one() {
        assert_eq!(
            spec(Mnemonic::Pea, AddressingMode::Stack)
                .unwrap()
                .immediate_width(),
            Some(ImmediateWidthClass::Fixed16)
        );
    }

    /// `PER` and the branches take a bare address in K65 and a displacement
    /// ca65 computes — K816 never works one out, because it does not own
    /// placement.
    #[test]
    fn relative_targets_are_displacements_ca65_computes() {
        for (mnemonic, mode) in [
            (Mnemonic::Bcc, AddressingMode::Relative),
            (Mnemonic::Brl, AddressingMode::RelativeLong),
            (Mnemonic::Per, AddressingMode::Stack),
        ] {
            let spec = spec(mnemonic, mode).unwrap();
            assert_eq!(spec.operand.form, OperandForm::Address, "{mnemonic:?}");
            assert_eq!(
                spec.operand.width,
                OperandWidth::Displacement,
                "{mnemonic:?}"
            );
        }
    }

    /// One source shape covers direct, absolute, and long, which is why
    /// classification narrows by form and *then* picks an encoding.
    #[test]
    fn one_source_form_can_cover_three_encodings() {
        let encodings: Vec<_> = spec_for_form(Mnemonic::Lda, OperandForm::Address)
            .map(|spec| spec.encoding())
            .collect();
        assert_eq!(
            encodings,
            vec![
                Some(AddressEncoding::Absolute),
                Some(AddressEncoding::Long),
                Some(AddressEncoding::Direct),
            ]
        );
    }

    #[test]
    fn illegal_combinations_are_rejected() {
        // No direct-page jump exists, and ca65 rejects `jmp z:$12` too.
        assert_eq!(spec(Mnemonic::Jmp, AddressingMode::Direct), None);
        assert_eq!(spec(Mnemonic::Sta, AddressingMode::Immediate), None);
        assert_eq!(spec(Mnemonic::Rts, AddressingMode::Absolute), None);
        assert_eq!(spec(Mnemonic::Sep, AddressingMode::Absolute), None);
        assert_eq!(spec(Mnemonic::Inx, AddressingMode::Accumulator), None);
        assert_eq!(spec(Mnemonic::Ldx, AddressingMode::DirectIndexedX), None);
        assert_eq!(spec(Mnemonic::Ldy, AddressingMode::DirectIndexedY), None);
    }

    #[test]
    fn mnemonics_parse_case_insensitively_and_round_trip() {
        for mnemonic in Mnemonic::ALL {
            let spelling = mnemonic.ca65();
            assert_eq!(Mnemonic::parse(spelling), Some(*mnemonic));
            assert_eq!(
                Mnemonic::parse(&spelling.to_ascii_uppercase()),
                Some(*mnemonic)
            );
        }
        assert_eq!(Mnemonic::parse("lax"), None);
        assert_eq!(Mnemonic::parse(""), None);
    }
}
