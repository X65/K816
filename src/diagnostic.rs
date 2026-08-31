/*****************************************************************************/
/*                                                                           */
/*                               diagnostic.rs                               */
/*                                                                           */
/*               Structured diagnostics, text edits, and fixes               */
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

//! Structured diagnostics: the internal protocol between compiler and tools.
//!
//! Diagnostics are data, never pre-rendered strings. The CLI renders them with
//! Ariadne in `main.rs`; the LSP converts them to protocol values in `lsp.rs`.
//! Both consume the same [`Diagnostic`] values, so they cannot disagree about
//! what an error means.

use crate::source::Span;

/// A stable, documented diagnostic code such as `K816-E0001`.
///
/// Codes are registered centrally here so they stay unique and stable; tests
/// and editors match on them.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DiagnosticCode(pub &'static str);

impl DiagnosticCode {
    /// Malformed source: Tree-sitter produced an `ERROR` node.
    pub const SYNTAX_ERROR: Self = Self("K816-E0001");
    /// Incomplete source: Tree-sitter inserted a `MISSING` node.
    pub const SYNTAX_MISSING: Self = Self("K816-E0002");
    /// A numeric literal does not fit its representable range.
    pub const NUMBER_RANGE: Self = Self("K816-E0003");
    /// A symbol is used but never declared.
    pub const UNDECLARED_SYMBOL: Self = Self("K816-E0101");
    /// A symbol is declared more than once.
    pub const DUPLICATE_SYMBOL: Self = Self("K816-E0102");
    /// An immediate operand does not fit the width selected by the mode.
    pub const IMMEDIATE_RANGE: Self = Self("K816-E0103");
    /// A width-dependent immediate is used where the register width is unknown.
    pub const UNKNOWN_WIDTH: Self = Self("K816-E0104");
    /// A native instruction uses a mnemonic the ISA does not know.
    pub const UNKNOWN_MNEMONIC: Self = Self("K816-E0105");
    /// An operand form is not legal for the instruction or symbol kind.
    pub const INVALID_OPERAND: Self = Self("K816-E0106");
    /// An address-encoding prefix is not legal for this instruction, e.g.
    /// `dp` on a mnemonic with no direct-page form.
    pub const INVALID_ENCODING: Self = Self("K816-E0107");
    /// An expression must fold at compile time here and does not.
    pub const NOT_CONSTANT: Self = Self("K816-E0108");
    /// A value does not fit the selected address encoding (`dp $1234`).
    pub const ADDRESS_RANGE: Self = Self("K816-E0109");
    /// A bare `const` (or const-rooted expression) in address position.
    pub const CONST_IN_ADDRESS_POSITION: Self = Self("K816-E0110");
    /// A placement directive sits where it cannot take effect: a `segment`
    /// that is not the first entry of its data block, an `align` or
    /// `address` that is not the first statement of its code section, or any
    /// of `segment` / `align` / `address` / `nocross` inside a `repeat` body.
    ///
    /// One code because it is one complaint from where the author sits —
    /// placement is a property of where the block's bytes land, and none of
    /// the positions has an answer to give: bytes are already placed before
    /// the first two, and a repetition would ask the third for one answer
    /// per copy.
    pub const SEGMENT_PLACEMENT: Self = Self("K816-E0111");
    /// A `nocross`-guarded run exceeds 256 bytes.
    pub const NOCROSS_TOO_LARGE: Self = Self("K816-E0112");
    /// The `&expr` offset marker is used outside a `,X`/`,Y` indexed mode.
    pub const OFFSET_MARKER_POSITION: Self = Self("K816-E0113");
    /// A compound expression appears in an atom-only data value list.
    pub const COMPOUND_DATA_VALUE: Self = Self("K816-E0114");
    /// A symbol is used in a role its declaration does not allow.
    pub const SYMBOL_KIND_MISMATCH: Self = Self("K816-E0115");
    /// `address` placement needs an ld65 config entry; the help carries the
    /// exact fragment to paste.
    pub const ADDRESS_NEEDS_CONFIG: Self = Self("K816-W0116");
    /// A module-level width annotation appears after a code section. A
    /// module default is a file-wide statement, so a positional reading would
    /// make the same annotation mean different things.
    pub const MODULE_WIDTH_PLACEMENT: Self = Self("K816-E0117");
    /// Two width annotations constrain the same axis differently, e.g.
    /// `func f @a8 @a16`. Repeating the *same* annotation is accepted.
    pub const CONFLICTING_WIDTH: Self = Self("K816-E0118");
    /// Reachable returns exit at different fixed widths (a divergent exit).
    pub const DIVERGENT_EXIT: Self = Self("K816-E0119");
    /// A declared exit width (`-> @a8`) disagrees with a reachable return.
    pub const EXIT_WIDTH_MISMATCH: Self = Self("K816-E0120");
    /// A call contract declares an alias on a section that is not `inline`.
    pub const ALIAS_NOT_INLINE: Self = Self("K816-E0121");
    /// A call site's register flow disagrees with the callee's declared
    /// contract — wrong registers, or a bare call with no flow clause to a
    /// contract-bearing function.
    pub const CALL_FLOW_MISMATCH: Self = Self("K816-E0122");
    /// A register live past a checked call is clobbered by the callee.
    pub const CLOBBERED_REGISTER: Self = Self("K816-W0123");
    /// K816 read this function's body, can see it may change width, and
    /// cannot say to what; callers will assume it preserves widths. An
    /// *invisible* callee gets a call-site annotation instead of this.
    pub const ASSUMED_EXIT: Self = Self("K816-W0124");
    /// A native `jsr`/`jsl` reaches a function whose declared entry contract
    /// the tracked width does not satisfy; raw native calls are not bridged.
    pub const UNBRIDGED_NATIVE_CALL: Self = Self("K816-W0125");
    /// An HLA statement the 65816 has no instruction for: a register transfer
    /// it does not have, a register memory cannot be loaded into or stored
    /// from, a store that names no register to carry the value, or a
    /// shorthand a register does not support — `x+1`, `v+`, `s!!`.
    ///
    /// One code for the family because it is one complaint from where the
    /// author sits: the spelling is legal K65 and the CPU cannot do it. The
    /// help names the route where one exists — the two-step transfer, or the
    /// register that does have the instruction. K816 suggests it rather than
    /// inserting it, because an inserted transfer clobbers a register the
    /// author is entitled to still be using.
    pub const NO_SUCH_INSTRUCTION: Self = Self("K816-E0127");
    /// `break` or `repeat` outside a loop. Both bind to the innermost
    /// enclosing loop, and outside one there is nothing to bind to.
    pub const LOOP_CONTROL_OUTSIDE_LOOP: Self = Self("K816-E0128");
    /// A `goto` names a label of another function. K65 labels are
    /// function-scoped or unit-level, and jumping into the middle of another
    /// function is not a control-flow form the width model can reason about.
    pub const GOTO_FOREIGN_LABEL: Self = Self("K816-E0129");
    /// A `nocross`-guarded run stopped fitting its page because an edge split
    /// grew it. The guard is evaluated after splitting, so this names the
    /// split as the cause rather than the run's written length.
    pub const NOCROSS_SPLIT_OVERFLOW: Self = Self("K816-E0130");
    /// A `naked` section declares an exit contract. A section with no return
    /// has no exit to declare.
    pub const NAKED_EXIT_CONTRACT: Self = Self("K816-E0131");
    /// An `inline` section expands into itself, directly or through a cycle,
    /// so expansion would not terminate. Recursion through a *non*-`inline`
    /// callee is fine: that one is a call, not an expansion.
    pub const INLINE_RECURSION: Self = Self("K816-E0132");
    /// A unit's `var dp` requests exceed the 256-byte direct page. Reported
    /// by the frontend rather than discovered as an ld65 segment overflow.
    pub const DIRECT_PAGE_OVERFLOW: Self = Self("K816-E0133");
    /// A subscript index does not fold to a compile-time constant. Runtime
    /// indexing is the CPU's indexed addressing modes, not a subscript.
    pub const SUBSCRIPT_NOT_CONSTANT: Self = Self("K816-E0134");
    /// A subscript index is outside the extent its declaration gives.
    pub const SUBSCRIPT_OUT_OF_RANGE: Self = Self("K816-E0135");
    /// An `abstract var` meets storage, in either direction: it is used where
    /// an address is required, or its own declaration asks for storage it
    /// cannot have — a `dp`/`abs`/`far` prefix, an initializer, an allocation
    /// count, a raw span, or no field list at all.
    ///
    /// One code for both because it is one complaint from where the author
    /// sits: this declares a layout, and a layout has no bytes. It reserves
    /// nothing, emits nothing, and has no address to load, store, or transfer
    /// to (`doc/syntax-reference.md`, "Abstract symbolic layouts").
    pub const ABSTRACT_HAS_NO_STORAGE: Self = Self("K816-E0136");
    /// A call site and its callee disagree about the calling convention: a
    /// near `call` to a `far func`, or a `call far` to a near one.
    ///
    /// The legacy linker reported this; K816 reports it in the frontend,
    /// where the callee's declaration is still in hand
    /// (`plans/feature-manifest.md` §3). It is an error rather than a silent
    /// correction because the mismatch is a stack-depth bug: a `jsr` pushes
    /// two bytes and the callee's `rtl` pops three.
    ///
    /// Code `0137` is skipped — it was registered for an unreachable case
    /// during E0 and burned when E2 retired it.
    pub const CALL_CONVENTION_MISMATCH: Self = Self("K816-E0138");
    /// A `far` variable has no explicit address. The 24-bit storage class is
    /// the one with no auto-allocator: nothing in the frontend or in ld65
    /// knows which of the 256 banks an unaddressed `far` symbol belongs to,
    /// so the declaration has to say (`doc/syntax-reference.md`, "Far
    /// storage").
    pub const FAR_NEEDS_ADDRESS: Self = Self("K816-E0139");
    /// A datum's declared width does not fit the register the access moves it
    /// through: a `:word` variable read with an 8-bit accumulator, a `:byte`
    /// one read with a 16-bit one, or a `:far` one read at all — no register
    /// holds three bytes.
    ///
    /// This is the *data-width* axis of a declaration, not the address-size
    /// axis: `var dp p:far` is a legal 24-bit datum in the direct page, and
    /// what this code refuses is loading it into a register
    /// (`doc/syntax-reference.md`, "Two orthogonal axes").
    pub const DATA_WIDTH_MISMATCH: Self = Self("K816-E0140");
    /// An element count is not a positive number: `var t * 0` and the field
    /// count `.str[0]` describe storage that has no elements, and every
    /// access to one is out of range the moment it is written
    /// (`doc/symbolic-subscripts.md`, "Diagnostics").
    ///
    /// A declaration's *raw span* is deliberately not in that list. `var
    /// buf[0]` is a K65 declaration and the original accepts it, reserving
    /// nothing, so K816 does too — the two forms above have no K65 spelling
    /// to be compatible with (`postmortem.md` §G.3.2).
    pub const ELEMENT_COUNT_RANGE: Self = Self("K816-E0141");
    /// A string character has no slot in the `charset` in force. A charset is
    /// an index table, so a character it does not list has no byte to emit —
    /// and guessing one (the character's own value, say) would put a byte in
    /// the output that the declared mapping says nothing about.
    pub const CHARSET_UNMAPPED: Self = Self("K816-E0142");
    /// The entry point declares a width the environment does not provide.
    ///
    /// Every other function's contract is a requirement on its *callers*,
    /// which K816 satisfies by bridging at each call site. `main`'s caller is
    /// the environment, and an environment cannot be bridged — so an
    /// annotation on `main` is a claim *about* the entry ABI rather than a
    /// request of it, and one the ABI does not satisfy is false. Generating a
    /// `REP` to make it true would be K816 deciding an ABI it does not own
    /// (`plans/09-entry-abi.md`).
    pub const ENTRY_ABI_MISMATCH: Self = Self("K816-E0143");
    /// An `align` asks for a placement no address can satisfy: a boundary
    /// that is not a power of two in `1..=65536`, or an offset that is not
    /// less than the boundary.
    ///
    /// One code, because it is one complaint — the congruence class the
    /// directive names is empty. Both halves are the original K65
    /// compiler's own checks (`setalign`, `compiler_src.cpp:877`), adopted
    /// so that `compatible` is a claim rather than a hope
    /// (`plans/06-compatibility.md` decision 15).
    pub const ALIGN_UNREACHABLE: Self = Self("K816-E0144");
    /// `print(msg)` in a live evaluation, carrying the evaluated message.
    ///
    /// A note rather than a warning: the original writes the text to the
    /// build log and nothing about the program is wrong, so nothing should
    /// look wrong (`plans/06-compatibility.md` decision 6). `N` is a third
    /// letter beside `E` and `W`, and it is here because the severity is part
    /// of what a code means: a reader who sees `K816-N0145` in a build log
    /// should not have to look it up to know the build was fine.
    pub const EVAL_PRINT: Self = Self("K816-N0145");
    /// `error(msg)` in a live evaluation, carrying the evaluated message.
    pub const EVAL_ERROR: Self = Self("K816-E0146");
    /// An evaluation visited more nodes than [`crate::eval::STEP_BUDGET`]
    /// allows — a `while` that does not terminate, or `evalfunc` recursion
    /// with no base case.
    pub const EVAL_BUDGET: Self = Self("K816-E0147");
    /// An evaluator value that is not a number where one is needed: a string
    /// or a data section at an integer boundary, or as an operand of
    /// arithmetic that has no meaning for it.
    pub const EVAL_NOT_A_NUMBER: Self = Self("K816-E0148");
    /// An evaluator name assigned in one bracket and again in another, or
    /// assigned when the compiler already owns the name.
    ///
    /// `doc/syntax-reference.md` "Compile-time Evaluator" makes an evaluator
    /// name write-once outside the block that introduced it. The original
    /// does not: `[N = 1] [N = 2]` compiles there and `N` is 2
    /// (`tests/compat/k65/eval-reassignment/`). The refusal is therefore a
    /// deliberate K816 rule, not a compatibility one.
    pub const EVAL_REASSIGNED: Self = Self("K816-E0149");
    /// `#error "message"` in a live preprocessor region, carrying the message.
    pub const PREPROCESSOR_ERROR: Self = Self("K816-E0150");
    /// `#warn "message"` in a live preprocessor region, carrying the message.
    pub const PREPROCESSOR_WARNING: Self = Self("K816-W0151");
    /// A preprocessor directive that does not pair with another one: an
    /// `#if` the enclosing block ends without closing, an `#endif`, `#else`
    /// or `#elif` with no `#if` above it, or a second `#else` in one chain.
    ///
    /// One code for all four shapes because they are one mistake seen from
    /// four sides, and each report carries both spans — the directive that
    /// cannot pair and the one it should have paired with, or the brace that
    /// ended the region first (`plans/06-compatibility.md` decision 9).
    ///
    /// The original has no diagnostic here at all: its grammar pairs the
    /// directives as a production, so three of these four shapes are a bare
    /// "syntax error" and the fourth — a region crossing a block boundary —
    /// aborts the compiler (`tests/compat/k816/preprocessor-unbalanced/`).
    pub const PREPROCESSOR_UNBALANCED: Self = Self("K816-E0152");
    /// A `binary` inclusion naming a file that is not there.
    ///
    /// ca65 resolves an `.incbin` path against its own search list and
    /// reports a missing file without a span; K816 checks at compile time,
    /// relative to the directory of the source that wrote the line, so the
    /// author gets the line rather than an assembler message
    /// (`plans/06-compatibility.md` decision 8). The original aborts here
    /// with no diagnostic at all.
    pub const BINARY_NOT_FOUND: Self = Self("K816-E0153");
    /// An evaluator read of a section's bytes — `index(sec, i)`, `sec[i]`,
    /// `sec.index(i)`, `size(sec)` — that K816 cannot answer.
    ///
    /// Four shapes, one code, because they are one question refused for four
    /// reasons: the value is not a section at all; it names a *code* section,
    /// whose bytes width analysis, the optimizer and ca65 decide long after
    /// an evaluation runs; it names a section written *below* the read; or
    /// the section holds a byte only the linker knows. The original answers
    /// the second and aborts on the other three
    /// (`plans/06-compatibility.md` decision 5).
    pub const SECTION_NOT_READABLE: Self = Self("K816-E0154");
    /// The two-dimensional `index(sec, x, y)` on a section with no width and
    /// height.
    ///
    /// The original sets those from a top-level `image` and nothing else, so
    /// with `image` removed (decision 7) no K816 section has them. The
    /// wording is the original's own complaint, so a ported source reads the
    /// same sentence it read there.
    ///
    /// It is what a two-dimensional read of a *section* reports. A read whose
    /// base is not a section at all is [`Self::SECTION_NOT_READABLE`] first,
    /// which is the order `Eval_Index_2D` uses too — `ToSection` before the
    /// dimension check — and the more useful of the two, since naming a
    /// number as a section without dimensions helps nobody.
    pub const SECTION_NO_DIMENSIONS: Self = Self("K816-E0155");
    /// An `inline` borrowed from another unit does not compile there, so it
    /// cannot be expanded here either.
    ///
    /// The body's own complaints are the defining unit's to make — it is in
    /// this workspace and it makes them — and repeating them once per unit
    /// that expands the section would multiply one mistake by the call graph,
    /// which is sub-plan E's decision 12 against the unit count. What is left
    /// here is one line saying where to look, and it is an *error* because
    /// without it this unit would emit an expansion of a body nothing
    /// checked.
    pub const BORROWED_INLINE_UNUSABLE: Self = Self("K816-E0156");
    /// A branch condition the 65816 has no single instruction for: unsigned
    /// `>` is "carry set *and* not equal", unsigned `<=` is "carry clear *or*
    /// equal", and each costs two branch instructions wherever it is written.
    ///
    /// K816's warning, not the original's: `>` and `<=` are absent from
    /// `compiler.inc`'s `BranchOp` entirely (`plans/06-compatibility.md`
    /// decision 23), so the operators and the complaint about them are both
    /// extensions. The legacy K816 warned per *spelling* — `} >` costs three
    /// branches, `} <=` costs two — and both halves of that are wrong here:
    /// K816's `>` expansion is two branches, and the two conditions negate
    /// into each other, so which of them a site pays for depends on the form
    /// and not on the token. The help names the pair the site actually emits.
    pub const INEFFICIENT_BRANCH: Self = Self("K816-W0157");
    /// A K816 register spelling that decisions 29 and 30 retired.
    ///
    /// `c` and `d` in a register position, `b` outside `~`, and the
    /// `b!!`/`b??` stack forms. Each names its replacement — the accumulator
    /// is `a` at both widths, and the two registers that had taken the flag
    /// letters are `dpr` and `dbr`. Registered because a spelling that
    /// vanishes into a generic parse error is what `plans/06-compatibility.md`
    /// decision 0 exists to prevent, and decision 33 applies that rule to
    /// K816's own retired spellings and not only to the original's.
    pub const RETIRED_REGISTER_SPELLING: Self = Self("K816-E0158");
    /// A `->` clause naming a flag outside `c z n v`.
    ///
    /// Those four are the flags a callee can *compute* and a branch can test.
    /// `i` and `d` are real P-register bits, and are refused because they are
    /// modes the caller establishes before the call: declaring one as an
    /// output would be a promise nothing could check against the body.
    pub const CONTRACT_FLAG_OUTPUT: Self = Self("K816-E0159");
    /// `--strict-k65` met a construct the original K65 grammar does not have.
    ///
    /// One per occurrence, naming the construct and — where one exists — what
    /// a K65 source writes instead. The set of constructs is not this file's
    /// to decide: it is the `K816-extension` rows of
    /// `doc/k65-compat-manifest.md` whose `Strict` column reads `rejects`,
    /// mirrored by `compat::MANIFEST` and checked against the file by
    /// `tests/compat.rs` (`plans/06-compatibility.md` decision 20).
    pub const STRICT_K65_EXTENSION: Self = Self("K816-E0160");
    /// `--strict-k65` met a construct that compiles in both languages and
    /// means something different here.
    ///
    /// A warning rather than an error, because the source *is* K65 and the
    /// original would build it — just not into these bytes. Only the
    /// `intentionally-changed` rows whose construct still compiles reach it;
    /// where K816 already refuses the construct, decision 0 says the flag has
    /// nothing to add and the row's `Strict` column reads `—`.
    pub const STRICT_K65_CHANGED: Self = Self("K816-W0161");

    // `K816-E0201` — "recognized but not compiled yet" — was retired in G6,
    // with the last construct that produced it. The range `K816-E02xx` stays
    // reserved for the same meaning: nothing else may take the number, and a
    // construct the grammar knows and the compiler does not is now a bug
    // rather than a schedule.
    /// The compiler met a CST shape it does not recognize (a K816 bug).
    pub const INTERNAL: Self = Self("K816-E0900");

    // The `K816-K65-####` family: syntax that belongs to the original K65
    // language, is reserved so K816 never reinterprets it, and is recognized
    // by the parser as a specific named node rather than a parse error. Per
    // `doc/k65-compat.md`, each such construct gets its own stable code.

    /// `image` blocks and top-level `image NAME = "file"` declarations.
    pub const K65_RESERVED_IMAGE: Self = Self("K816-K65-0001");
    /// Evaluator side-effect functions: `size`, `addbyte`, `color`, `rnd`,
    /// `print`, `error`.
    pub const K65_RESERVED_EVAL_FUNCTION: Self = Self("K816-K65-0002");
    /// `bank NAME`. Removed rather than reserved: the bank-switching scheme
    /// the original's linker implied is the ld65 configuration's, and
    /// `segment NAME` is the spelling K816 has for it. An alias would
    /// silently accept a source whose memory map K816 does not reproduce
    /// (`plans/06-compatibility.md` decision 10).
    pub const K65_REMOVED_BANK: Self = Self("K816-K65-0003");
    /// The five HLA forms that spell undocumented 6502 opcodes: `a &. imm`,
    /// `mem = a & x`, `x &= a - imm`, `a ?-- mem`, `a -++ mem`.
    ///
    /// Not "not implemented yet": K816 assembles the legal MOS 6502, WDC
    /// 65C02 and WDC 65C816 instruction sets and nothing else
    /// (`doc/design.md` §1), and every one of these encodings is a
    /// *different*, documented 65816 instruction — two of them `STP` and
    /// `XCE`. The help says which, and stops there.
    pub const K65_UNSUPPORTED_ILLEGAL_OPCODE: Self = Self("K816-K65-0004");
    /// A flag letter's stack shorthand — `z??`, `c!!`, `n??`, `v!!`, `i??`.
    ///
    /// In the original the letter is decoration: every one of the seven
    /// letters is plain `PLP`/`PHP`, all fourteen spellings meaning the same
    /// two instructions. K816 does not carry that, and now refuses all seven
    /// uniformly: `b` and `d` used to be excluded because they named the
    /// data-bank and direct-page registers here, and those registers are
    /// `dbr` and `dpr` since `plans/06-compatibility.md` decisions 29 and 30.
    pub const K65_FLAG_STACK_FORM: Self = Self("K816-K65-0005");
    /// `o` or `O` where the overflow flag is meant — `o-`, `o+`, `o-?`, `o+?`.
    ///
    /// The original spells the *set/clear* shorthand with `o` (`o-` is
    /// `"CL"+"v"`, `compiler.inc:161-162, 259-260`) and the *branch tests*
    /// with `v` (`v-?` is `BVC`, `:318-319`) — one flag under two letters,
    /// split by construct. K816 accepted both letters everywhere, which is
    /// what let the two intermix; it now spells the flag `v` throughout and
    /// names that here (decision 32).
    pub const K65_OVERFLOW_FLAG_SPELLING: Self = Self("K816-K65-0006");

    pub fn as_str(self) -> &'static str {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Help,
    Note,
    Warning,
    Error,
}

/// A message anchored to a source span.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label {
    pub span: Span,
    pub message: String,
}

impl Label {
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

/// Whether a [`Fix`] can be applied without human review.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Applicability {
    /// Safe to apply automatically.
    MachineApplicable,
    /// Plausible but needs review.
    MaybeIncorrect,
}

/// A concrete text replacement: `span` is replaced by `replacement`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextEdit {
    pub span: Span,
    pub replacement: String,
}

impl TextEdit {
    pub fn replace(span: Span, replacement: impl Into<String>) -> Self {
        Self {
            span,
            replacement: replacement.into(),
        }
    }

    pub fn insert(span: Span, text: impl Into<String>) -> Self {
        Self::replace(Span::empty_at(span.file, span.start), text)
    }

    pub fn delete(span: Span) -> Self {
        Self::replace(span, "")
    }
}

/// A machine-applicable remediation: a titled set of edits.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fix {
    pub title: String,
    pub applicability: Applicability,
    pub edits: Vec<TextEdit>,
}

/// A reference into the K816 documentation, e.g. `doc/syntax-reference.md#func`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocsRef(pub String);

/// One complete diagnostic: code, severity, labels, notes, help, and fixes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub code: DiagnosticCode,
    pub severity: Severity,
    /// Concise problem statement, shown as the headline.
    pub title: String,
    /// The main location, with a message explaining what is wrong there.
    pub primary: Label,
    /// Related locations (declarations, previous uses, ...).
    pub secondary: Vec<Label>,
    pub notes: Vec<String>,
    /// Exact remediation advice.
    pub help: Option<String>,
    pub fixes: Vec<Fix>,
    pub docs: Option<DocsRef>,
}

impl Diagnostic {
    pub fn new(
        code: DiagnosticCode,
        severity: Severity,
        title: impl Into<String>,
        primary: Label,
    ) -> Self {
        Self {
            code,
            severity,
            title: title.into(),
            primary,
            secondary: Vec::new(),
            notes: Vec::new(),
            help: None,
            fixes: Vec::new(),
            docs: None,
        }
    }

    pub fn error(code: DiagnosticCode, title: impl Into<String>, primary: Label) -> Self {
        Self::new(code, Severity::Error, title, primary)
    }

    pub fn warning(code: DiagnosticCode, title: impl Into<String>, primary: Label) -> Self {
        Self::new(code, Severity::Warning, title, primary)
    }

    /// A note the compiler was *asked* to emit, rather than one it decided to.
    ///
    /// `print(msg)` is the only producer: nothing about the program is wrong,
    /// so nothing should look wrong, but the message still has to reach the
    /// user with a span and a code so a fixture can name it.
    pub fn note(code: DiagnosticCode, title: impl Into<String>, primary: Label) -> Self {
        Self::new(code, Severity::Note, title, primary)
    }

    pub fn with_secondary(mut self, label: Label) -> Self {
        self.secondary.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_fix(mut self, fix: Fix) -> Self {
        self.fixes.push(fix);
        self
    }

    pub fn with_docs(mut self, docs: impl Into<String>) -> Self {
        self.docs = Some(DocsRef(docs.into()));
        self
    }

    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }
}

/// True when any diagnostic in `diagnostics` is an error.
pub fn has_errors(diagnostics: &[Diagnostic]) -> bool {
    diagnostics.iter().any(Diagnostic::is_error)
}

#[cfg(test)]
mod tests {
    use super::DiagnosticCode;

    /// Every registered code, so the uniqueness check below sees all of them.
    const REGISTERED: &[DiagnosticCode] = &[
        DiagnosticCode::SYNTAX_ERROR,
        DiagnosticCode::SYNTAX_MISSING,
        DiagnosticCode::NUMBER_RANGE,
        DiagnosticCode::UNDECLARED_SYMBOL,
        DiagnosticCode::DUPLICATE_SYMBOL,
        DiagnosticCode::IMMEDIATE_RANGE,
        DiagnosticCode::UNKNOWN_WIDTH,
        DiagnosticCode::UNKNOWN_MNEMONIC,
        DiagnosticCode::INVALID_OPERAND,
        DiagnosticCode::INVALID_ENCODING,
        DiagnosticCode::NOT_CONSTANT,
        DiagnosticCode::ADDRESS_RANGE,
        DiagnosticCode::CONST_IN_ADDRESS_POSITION,
        DiagnosticCode::SEGMENT_PLACEMENT,
        DiagnosticCode::NOCROSS_TOO_LARGE,
        DiagnosticCode::OFFSET_MARKER_POSITION,
        DiagnosticCode::COMPOUND_DATA_VALUE,
        DiagnosticCode::SYMBOL_KIND_MISMATCH,
        DiagnosticCode::ADDRESS_NEEDS_CONFIG,
        DiagnosticCode::MODULE_WIDTH_PLACEMENT,
        DiagnosticCode::CONFLICTING_WIDTH,
        DiagnosticCode::DIVERGENT_EXIT,
        DiagnosticCode::EXIT_WIDTH_MISMATCH,
        DiagnosticCode::ALIAS_NOT_INLINE,
        DiagnosticCode::CALL_FLOW_MISMATCH,
        DiagnosticCode::CLOBBERED_REGISTER,
        DiagnosticCode::ASSUMED_EXIT,
        DiagnosticCode::UNBRIDGED_NATIVE_CALL,
        DiagnosticCode::NO_SUCH_INSTRUCTION,
        DiagnosticCode::LOOP_CONTROL_OUTSIDE_LOOP,
        DiagnosticCode::GOTO_FOREIGN_LABEL,
        DiagnosticCode::NOCROSS_SPLIT_OVERFLOW,
        DiagnosticCode::NAKED_EXIT_CONTRACT,
        DiagnosticCode::INLINE_RECURSION,
        DiagnosticCode::DIRECT_PAGE_OVERFLOW,
        DiagnosticCode::SUBSCRIPT_NOT_CONSTANT,
        DiagnosticCode::SUBSCRIPT_OUT_OF_RANGE,
        DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
        DiagnosticCode::CALL_CONVENTION_MISMATCH,
        DiagnosticCode::FAR_NEEDS_ADDRESS,
        DiagnosticCode::DATA_WIDTH_MISMATCH,
        DiagnosticCode::ELEMENT_COUNT_RANGE,
        DiagnosticCode::CHARSET_UNMAPPED,
        DiagnosticCode::ENTRY_ABI_MISMATCH,
        DiagnosticCode::INTERNAL,
        DiagnosticCode::ALIGN_UNREACHABLE,
        DiagnosticCode::EVAL_PRINT,
        DiagnosticCode::EVAL_ERROR,
        DiagnosticCode::EVAL_BUDGET,
        DiagnosticCode::EVAL_NOT_A_NUMBER,
        DiagnosticCode::EVAL_REASSIGNED,
        DiagnosticCode::PREPROCESSOR_ERROR,
        DiagnosticCode::PREPROCESSOR_WARNING,
        DiagnosticCode::PREPROCESSOR_UNBALANCED,
        DiagnosticCode::BINARY_NOT_FOUND,
        DiagnosticCode::SECTION_NOT_READABLE,
        DiagnosticCode::SECTION_NO_DIMENSIONS,
        DiagnosticCode::BORROWED_INLINE_UNUSABLE,
        DiagnosticCode::INEFFICIENT_BRANCH,
        DiagnosticCode::RETIRED_REGISTER_SPELLING,
        DiagnosticCode::CONTRACT_FLAG_OUTPUT,
        DiagnosticCode::STRICT_K65_EXTENSION,
        DiagnosticCode::STRICT_K65_CHANGED,
        DiagnosticCode::K65_RESERVED_IMAGE,
        DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
        DiagnosticCode::K65_REMOVED_BANK,
        DiagnosticCode::K65_UNSUPPORTED_ILLEGAL_OPCODE,
        DiagnosticCode::K65_FLAG_STACK_FORM,
        DiagnosticCode::K65_OVERFLOW_FLAG_SPELLING,
    ];

    /// Every declared code is in [`REGISTERED`].
    ///
    /// Without this the uniqueness check above only sees the codes someone
    /// remembered to list, which is how three of them — `E0156`, `W0157`, and
    /// `K65-0005` — went unchecked for four tasks. Reading the source is the
    /// only way to ask the question: a `const` nothing references is invisible
    /// at run time.
    #[test]
    fn every_declared_code_is_registered() {
        let listed: std::collections::HashSet<&str> =
            REGISTERED.iter().map(|code| code.as_str()).collect();
        for line in include_str!("diagnostic.rs").lines() {
            let Some(rest) = line.trim().strip_prefix("pub const ") else {
                continue;
            };
            let Some((name, value)) = rest.split_once(": Self = Self(\"") else {
                continue;
            };
            let Some((code, _)) = value.split_once('"') else {
                continue;
            };
            assert!(
                listed.contains(code),
                "{name} ({code}) is declared but missing from REGISTERED"
            );
        }
    }

    #[test]
    fn codes_are_unique_and_well_formed() {
        let mut seen = std::collections::HashSet::new();
        for code in REGISTERED {
            assert!(
                code.as_str().starts_with("K816-"),
                "{} lacks the K816 prefix",
                code.as_str()
            );
            assert!(
                seen.insert(code.as_str()),
                "duplicate code {}",
                code.as_str()
            );
        }
    }
}
