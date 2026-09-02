/*****************************************************************************/
/*                                                                           */
/*                                 compat.rs                                 */
/*                                                                           */
/*        Diagnostics for K65 constructs K816 removes or refuses             */
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

//! Diagnostics for K65 syntax K816 recognizes and does not compile.
//!
//! The grammar recognizes the whole K65/K816 surface (sub-plan B), which is
//! deliberately wider than what K816 *means*: `image` is removed, `bank` is
//! removed, three evaluator functions are refused, and five HLA forms spell
//! undocumented 6502 opcodes. Every one of them is a specific named node with
//! a diagnostic of its own, which is decision 0's second clause — a K65 form
//! K816 does not implement never comes out as a generic parse error.
//!
//! There is no longer a "not implemented yet" constructor here. It existed
//! while the sub-plans were landing and its last producer went in G6; a
//! construct that parses and has no meaning is now a compiler bug, not a
//! schedule.

use crate::ast::{
    AstFile, BinaryOp, Block, BranchCondition, BranchConditionKind, CallArgument, ComparisonKind,
    ContractOutput, DataBlock, DataEntry, DataEntryKind, DataValue, EvalExpr, EvalExprKind, Expr,
    ExprKind, FieldDeclaration, Function, HlaOperand, HlaOperandKind, HlaStatement, Item, ItemKind,
    LoopCondition, NativeOperand, Register, RegisterKind, Statement, StatementKind,
    StorageClassKind, UnaryOp, VarDeclaration,
};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::source::Span;

/// `image`, in every one of its spellings, is removed.
///
/// A *removal*, not a reservation: an asset pipeline is outside what the
/// compiler does (`doc/design.md` §1), no BMP decoder is cheap by any
/// reading of decision 0, and nothing later in the plan brings the construct
/// back. So the wording says so, and the help names the replacement — convert
/// the bitmap outside the build and include the result with `binary`
/// (`plans/06-compatibility.md` decision 7). No adapter tool ships with the
/// compiler.
///
/// `form` is the spelling the source used, so a source that wrote `tiles` or
/// `imgwave` reads its own word back.
pub fn k65_removed_image(span: Span, form: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::K65_RESERVED_IMAGE,
        format!("K65 `{form}` is not a K816 construct"),
        Label::new(span, "K816 has no image reader"),
    )
    .with_note(
        "reading a bitmap and scanning it into tiles is an asset pipeline, which K816 \
         does not own (`doc/design.md` §1)",
    )
    .with_help("convert the image outside the build and include the bytes with `binary`")
    .with_docs("doc/k65-compat.md")
}

/// One of the three evaluator functions K816 does not run.
///
/// A *refusal*, not a reservation: nothing later brings `rnd` back, and the
/// help names what to do instead. `print` and `error` left this family in G4
/// and `size` in G6 — all three are implemented — so what is left is
/// `addbyte`, which mutates a section; `rnd`, which is not deterministic; and
/// `color`, which is an asset pipeline's (`plans/06-compatibility.md`
/// decision 6).
pub fn k65_refused_eval_function(span: Span, name: &str) -> Diagnostic {
    let diagnostic = Diagnostic::error(
        DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
        format!("K65's evaluator function `{name}` is not in K816"),
        Label::new(span, "this call cannot be compiled"),
    );
    match name {
        // Cheap to implement and refused anyway: a seeded generator would
        // give a program bytes the original never produced twice, and an
        // unseeded one would give it different bytes every build.
        "rnd" => diagnostic
            .with_note(
                "a compiler that generates a different table on each build is a compiler \
                 whose output cannot be compared with itself",
            )
            .with_help("generate the table outside the build and include it with `binary`"),
        // Palette conversion is an asset pipeline's, like `image` — and it
        // was answering with the section-mutation note until G6 trimmed this
        // function to three names and the wrong sentence became visible.
        "color" => diagnostic
            .with_note(
                "picking a palette index for a colour is an asset pipeline's job, and K816 \
                 does not own one (`doc/design.md` §1)",
            )
            .with_help("convert the palette outside the build and include it with `binary`"),
        // Section mutation is outside what the compiler does at all.
        _ => diagnostic.with_note(
            "K816 does not let a compile-time evaluation change a section's contents \
             (`doc/design.md` §1)",
        ),
    }
    .with_docs("doc/syntax-reference.md#functions")
}

/// A flag letter's stack shorthand: `z??`, `c!!`, `n??`, `v!!`, `i??`.
///
/// A *refusal*, and the help is the whole point of having one. In the
/// original the letter is decoration — `compiler.inc:167-180` gives all seven
/// letters the same two actions, `PLP` and `PHP` — so an author who wrote
/// `c??` was pushing the entire status register and very likely believed
/// otherwise. `p??`/`p!!` is that instruction said plainly, and it is what
/// the help names.
///
/// `b` and `d` are not refused: they name the data-bank and direct-page
/// registers on the 65816, which the 6502 did not have, so `b!!` is `PHB`
/// here and `PHP` there. That is a K65 spelling with a K816 meaning, and it
/// is written down as one (`doc/k65-compat-manifest.md`, `instr-stack-flags`)
/// rather than left for someone to find in the bytes.
pub fn k65_flag_stack_form(span: Span, letter: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::K65_FLAG_STACK_FORM,
        format!("K65's `{letter}??` and `{letter}!!` push and pull *every* flag"),
        Label::new(span, "this letter names no register the stack can move"),
    )
    .with_note(
        "in the original the letter is decoration: all seven of `z c n v b i d` assemble          to plain `PLP`/`PHP`, so nothing about this spelling moves one flag on its own",
    )
    .with_help("write `p??` / `p!!`, which is the same two instructions and says so")
    .with_docs("doc/k65-compat.md")
}

/// `o`/`O` where the overflow flag is meant.
///
/// A *refusal* of a spelling the original accepts, which decision 0 normally
/// forbids — so the note carries the reason. The original is not consistent
/// about this flag: `Instr -> Flag -` spells it `o` (`compiler.inc:161-162`,
/// `259-260`) while `BranchOp -> v-?` spells it `v` (`:318-319`), so a source
/// clearing the flag and branching on it writes two different letters for one
/// bit. K816 accepted both letters in both constructs, which did not resolve
/// the split so much as let it into a single line. One letter, everywhere.
pub fn k65_overflow_flag_spelling(span: Span) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::K65_OVERFLOW_FLAG_SPELLING,
        "K816 spells the overflow flag `v`, not `o`",
        Label::new(span, "write `v` here"),
    )
    .with_note(
        "the original spells the set/clear shorthand `o-` and the branch tests `v-?`, \
         which is one flag under two letters; K816 uses `v` for both",
    )
    .with_help("`v-` is `CLV`, and `v-?` / `v+?` are the branch tests")
    .with_docs("doc/k65-compat.md")
}

/// A K816 register spelling decisions 29 and 30 retired.
///
/// K816's own spelling rather than the original's, and it gets the same
/// courtesy for the same reason: decision 0 refuses generic parse errors for
/// retired syntax because the author needs to be told what to write instead,
/// and that need does not depend on which dialect the spelling came from
/// (decision 33). Without this `c = s` reads as a store into an undeclared
/// name and the help offers `var c = $0400` — advice that compiles and does
/// something else entirely.
pub fn retired_register_spelling(span: Span, spelling: &str) -> Option<Diagnostic> {
    let (was, now) = match spelling.to_ascii_lowercase().as_str() {
        "c" => (
            "the 16-bit accumulator",
            "the accumulator is `a` at both widths, and `c` is the carry flag",
        ),
        "d" => (
            "the direct page register",
            "the direct page register is `dpr`, and `d` is the decimal flag",
        ),
        _ => return None,
    };
    Some(
        Diagnostic::error(
            DiagnosticCode::RETIRED_REGISTER_SPELLING,
            format!("`{spelling}` no longer names {was}"),
            Label::new(span, "this is a flag letter now"),
        )
        .with_note(
            "a single letter is a flag, and a register that is not `a x y s` is spelled \
             by its datasheet abbreviation — `dpr`, `dbr`",
        )
        .with_help(now)
        .with_docs("doc/syntax-reference.md#registers"),
    )
}

/// The names a declaration may not take: every register and flag spelling.
///
/// K816's reserved vocabulary, which is a superset of the original's for one
/// reason — K816 has flags the 6502 cannot set or clear (`z`, `n`) and
/// registers the 6502 does not have (`dpr`, `dbr`). The original reserves its
/// own the same way: `Const -> Ident` excludes the register and flag terminal
/// classes, which is why `const A` is a syntax error there.
///
/// **The compile-time evaluator is not covered**, and deliberately: the
/// original's `AllIdent -> Ident | A | X | Y | S | Flag` lets these letters be
/// names again in an evaluator block and as a `for` variable, `for i = 0..3`
/// is the idiomatic K65 loop, and eleven fixtures use it. The evaluator keeps
/// its own environment, so the two namespaces never meet.
pub const RESERVED_NAMES: &[&str] = &[
    // Registers.
    "a", "b", "x", "y", "s", "p", "dpr", "dbr", //
    // Flags, `o` included: it is not a K816 spelling any more (decision 32),
    // and letting a *variable* take the letter would be the confusion that
    // decision over, one level down.
    "c", "d", "i", "v", "n", "z", "o",
];

/// Why `name` cannot be declared, when it cannot — the refusal's own
/// sentence, without a span around it.
///
/// Split out from [`reserved_name`] because a *rename* has to make the same
/// refusal and has no span to anchor a diagnostic to: a new name an editor
/// accepts and the next build rejects is worse than one the editor declined,
/// and the two agreeing has to be a fact rather than a habit.
pub fn reserved_name_refusal(name: &str) -> Option<String> {
    let lowered = name.to_ascii_lowercase();
    if !RESERVED_NAMES.contains(&lowered.as_str()) {
        return None;
    }
    let what = if matches!(
        lowered.as_str(),
        "a" | "b" | "x" | "y" | "s" | "p" | "dpr" | "dbr"
    ) {
        "a register"
    } else {
        "a flag"
    };
    Some(format!(
        "`{name}` names {what}, so a declaration cannot take it"
    ))
}

/// A declaration asking for a register or flag spelling.
///
/// Reserved in the symbol table rather than in the lexer, which is the whole
/// trick: a token for these letters wins wherever an identifier is also
/// valid, and that is how `i+ d-` came to parse as the arithmetic `i + d`
/// (`postmortem.md` §G.1.16). The grammar keeps them as identifiers and this
/// refuses the ones that would shadow a register or a flag.
pub fn reserved_name(span: Span, name: &str) -> Option<Diagnostic> {
    let refusal = reserved_name_refusal(name)?;
    Some(
        Diagnostic::error(
            DiagnosticCode::RETIRED_REGISTER_SPELLING,
            refusal,
            Label::new(span, "this name is reserved"),
        )
        .with_note(
            "every register and flag spelling is reserved, whatever the case: a name that \
             is also a register would mean two things in the same statement",
        )
        .with_help(
            "pick another name; the compile-time evaluator is unaffected, so \
                    `for i = 0..3` and `[ c = 7 ]` still work",
        )
        .with_docs("doc/syntax-reference.md#registers"),
    )
}

/// `bank NAME` is removed, and `segment NAME` is what replaced it.
///
/// A *removal* rather than a reservation, so the wording says so: nothing
/// later in the plan brings `bank` back, and a message reading "not
/// implemented yet" would promise otherwise. The one bank the original
/// compiled into is named `default`, and it is refused with the rest — the
/// declaration moves no byte either way, and carving a name out of the rule
/// reopens the silent acceptance the refusal was chosen over
/// (`plans/06-compatibility.md` decision 10).
pub fn k65_removed_bank(span: Span, name: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::K65_REMOVED_BANK,
        format!("K65 `bank {name}` is not a K816 declaration"),
        Label::new(span, "K816 has no bank-switching model of its own"),
    )
    .with_note(
        "the original's banks were its linker's; in K816 the memory map lives \
         in the ld65 configuration",
    )
    .with_help(format!(
        "write `segment {name}` and give `{name}` a `MEMORY`/`SEGMENTS` entry \
         in your ld65 config"
    ))
    .with_docs("doc/k65-compat.md")
}

/// One of the five HLA forms whose bytes are undocumented 6502 opcodes.
///
/// `illegal` is the 6502 mnemonic the form is a spelling of; `native` says
/// what the 65816 does with the same bytes. The help stops there on purpose:
/// K816 assembles the documented instruction sets and nothing else
/// (`doc/design.md` §1), and publishing a hand-rolled replacement sequence
/// would be inviting back the thing that boundary excludes.
pub fn k65_illegal_opcode(span: Span, form: &str, illegal: &str, native: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::K65_UNSUPPORTED_ILLEGAL_OPCODE,
        format!("K65 `{form}` assembles the illegal 6502 opcode `{illegal}`"),
        Label::new(span, "K816 does not assemble undocumented opcodes"),
    )
    .with_note(format!("on the 65816 those bytes are {native}"))
    .with_help(
        "K816 assembles the documented MOS 6502, WDC 65C02 and WDC 65C816 \
         instruction sets only",
    )
    .with_docs("doc/k65-compat.md")
}

// ---------------------------------------------------------------------------
// The manifest, mirrored
// ---------------------------------------------------------------------------

/// One row's status in `doc/k65-compat-manifest.md`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Classification {
    /// Implemented with the original's semantics.
    Compatible,
    /// K816 does something else on purpose.
    IntentionallyChanged,
    /// The original did something its own documentation contradicts.
    LegacyDefective,
    /// Same behaviour, different bytes, because ca65/ld65 own encoding and
    /// placement.
    BackendDifferent,
    /// Not in the original grammar.
    K816Extension,
}

/// What `--strict-k65` does where a row's construct appears.
///
/// Not derivable from [`Classification`], which is the whole reason the
/// manifest carries it as a column: fifteen of the eighteen
/// `intentionally-changed` grammar rows are constructs K816 already refuses,
/// where decision 0 says the flag has nothing to add, and three more name a
/// corner no node marks.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Strict {
    /// An error: a K816 extension with no K65 spelling.
    Rejects,
    /// A warning: the construct compiles in both and means something else
    /// here.
    Warns,
    /// Nothing to report. The row's `Reason` says which of the three it is.
    Quiet,
}

/// One row of `doc/k65-compat-manifest.md`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ManifestRow {
    pub id: &'static str,
    pub status: Classification,
    pub strict: Strict,
}

const fn row(id: &'static str, status: Classification, strict: Strict) -> ManifestRow {
    ManifestRow { id, status, strict }
}

use Classification::{
    BackendDifferent, Compatible, IntentionallyChanged, K816Extension, LegacyDefective,
};
use Strict::{Quiet, Rejects, Warns};

/// Every row of `doc/k65-compat-manifest.md`, in the file's order.
///
/// The mirror decision 20 asks for, and `tests/compat.rs` reads the file and
/// checks the two agree row for row — in both directions, so a row added to
/// one and not the other is a failure rather than a note. That is the only
/// thing keeping the table below from drifting into a second opinion about
/// what the compiler does: the file is the document, this is what the flag
/// reads, and nothing else may decide either.
pub const MANIFEST: &[ManifestRow] = &[
    // The original grammar, rule by rule.
    row("syntax-toplist", Compatible, Quiet),
    row("topelem-empty", Compatible, Quiet),
    row("topelem-preprocessor", Compatible, Quiet),
    row("topelem-pperror", Compatible, Quiet),
    row("ppif-directives", Compatible, Quiet),
    row("ppifnumber-condition", IntentionallyChanged, Quiet),
    row("topelem-eval-block", IntentionallyChanged, Quiet),
    row("topelem-var", Compatible, Quiet),
    row("var-declaration", BackendDifferent, Quiet),
    row("var-verbose-marker", Compatible, Quiet),
    row("topelem-const", IntentionallyChanged, Quiet),
    row("topelem-bank", IntentionallyChanged, Quiet),
    row("topelem-image", IntentionallyChanged, Quiet),
    row("topelem-binary", LegacyDefective, Quiet),
    row("topelem-data", Compatible, Quiet),
    row("data-arg", Compatible, Quiet),
    row("data-address-word", Compatible, Quiet),
    row("data-image-tiles", IntentionallyChanged, Quiet),
    row("data-imgwave", IntentionallyChanged, Quiet),
    row("data-binary", BackendDifferent, Quiet),
    row("data-nocross-block", Compatible, Quiet),
    row("data-undef", Compatible, Quiet),
    row("data-repeat", Compatible, Quiet),
    row("data-code-block", Compatible, Quiet),
    row("data-nocross-flag", Compatible, Quiet),
    row("data-charset", Compatible, Quiet),
    row("data-evaluator-entry", IntentionallyChanged, Warns),
    row("data-string", Compatible, Quiet),
    row("data-for-eval", Compatible, Quiet),
    row("data-label", Compatible, Quiet),
    row("data-preprocessor", Compatible, Quiet),
    row("topelem-inline", Compatible, Quiet),
    row("topelem-macro", Compatible, Quiet),
    row("topelem-func", Compatible, Quiet),
    row("topelem-naked", Compatible, Quiet),
    row("topelem-main", Compatible, Quiet),
    row("secstart", Compatible, Quiet),
    row("align-directive", BackendDifferent, Quiet),
    row("sequence", Compatible, Quiet),
    row("instr-preprocessor", Compatible, Quiet),
    row("instr-semicolon", Compatible, Quiet),
    row("instr-nop-wait", IntentionallyChanged, Warns),
    row("instr-call", Compatible, Quiet),
    row("instr-far-call", Compatible, Quiet),
    row("instr-far-goto", Compatible, Quiet),
    row("instr-arith", Compatible, Quiet),
    row("instr-postfix", Compatible, Quiet),
    row("instr-flag-set", LegacyDefective, Quiet),
    row("instr-flag-clear", Compatible, Quiet),
    row("instr-stack-a", Compatible, Quiet),
    row("instr-stack-flags", IntentionallyChanged, Quiet),
    row("instr-mov", Compatible, Quiet),
    row("instr-sax", IntentionallyChanged, Quiet),
    row("instr-axs", IntentionallyChanged, Quiet),
    row("instr-return", Compatible, Quiet),
    row("instr-branch-block", Compatible, Quiet),
    row("instr-goto", Compatible, Quiet),
    row("instr-call-label", Compatible, Quiet),
    row("instr-label", Compatible, Quiet),
    row("instr-data-block", Compatible, Quiet),
    row("instr-break", Compatible, Quiet),
    row("instr-repeat", Compatible, Quiet),
    row("putlabel", Compatible, Quiet),
    row("allident", Compatible, Quiet),
    row("register-tokens", Compatible, Quiet),
    row("flag-tokens", IntentionallyChanged, Quiet),
    row("arg-forms", Compatible, Quiet),
    row("vararg-label-offset", Compatible, Quiet),
    row("arithop", Compatible, Quiet),
    row("arithop-anc", IntentionallyChanged, Quiet),
    row("arithop-dcp", IntentionallyChanged, Quiet),
    row("arithop-isc", IntentionallyChanged, Quiet),
    row("postfixop", Compatible, Quiet),
    row("branchop", Compatible, Quiet),
    row("number", Compatible, Quiet),
    row("number-eval-bracket", IntentionallyChanged, Quiet),
    row("ident", Compatible, Quiet),
    row("evalcode", Compatible, Quiet),
    row("evalinstr-statements", Compatible, Quiet),
    row("evexpr-operators", LegacyDefective, Quiet),
    row("evelem-values", Compatible, Quiet),
    row("evelem-incdec", LegacyDefective, Quiet),
    row("evelem-index", IntentionallyChanged, Quiet),
    row("evelem-call", Compatible, Quiet),
    row("evelem-method", Compatible, Quiet),
    row("topelem-evalfunc", Compatible, Quiet),
    row("terminal-numbers", LegacyDefective, Quiet),
    row("terminal-ident", Compatible, Quiet),
    row("terminal-string", Compatible, Quiet),
    row("terminal-comment", Compatible, Quiet),
    // K816 constructs that are not in the original grammar.
    row("k816-width-annotations", K816Extension, Rejects),
    row("k816-far-function", K816Extension, Rejects),
    row("k816-segment", K816Extension, Rejects),
    row("k816-call-contracts", K816Extension, Rejects),
    row("k816-register-names", K816Extension, Rejects),
    row("k816-accumulator-one-name", K816Extension, Rejects),
    row("k816-retired-register-spellings", K816Extension, Quiet),
    row("k816-unlisted-output", K816Extension, Quiet),
    row("k816-zero-store-stz", K816Extension, Quiet),
    row("k816-const-auto-increment", K816Extension, Rejects),
    row("k816-symbolic-subscript", K816Extension, Rejects),
    row("k816-field-access", K816Extension, Rejects),
    row("k816-metadata-query", K816Extension, Rejects),
    row("k816-typed-view", K816Extension, Rejects),
    row("k816-abstract-var", K816Extension, Rejects),
    row("k816-zero-store-target", K816Extension, Quiet),
    row("k816-eval-extensions", K816Extension, Rejects),
    row("k816-eval-diagnostics", K816Extension, Quiet),
    row("k816-postfix-gt", K816Extension, Rejects),
    row("k816-postfix-le", K816Extension, Rejects),
    row("k816-hex-binary-char-literals", K816Extension, Rejects),
    row("k816-unreachable-warning", IntentionallyChanged, Quiet),
    row("k816-tail-call", IntentionallyChanged, Quiet),
    row("k816-optimizer-dead-writes", BackendDifferent, Quiet),
    row("k816-entry-abi", K816Extension, Quiet),
    row("k816-strict-k65", K816Extension, Quiet),
    row("pp-dead-conditions", IntentionallyChanged, Quiet),
    row("newline-subscript", Compatible, Quiet),
    row("k65-corpus-whole-program", IntentionallyChanged, Quiet),
    row("optimizer-dead-register-write", BackendDifferent, Quiet),
    row("optimizer-dead-flag-write", BackendDifferent, Quiet),
    row("k65-far-call-in-inline", LegacyDefective, Quiet),
    row("k65-local-labels-across-far", Compatible, Quiet),
    row("k816-direct-page-pool", K816Extension, Rejects),
    row("k816-nocross-shape", BackendDifferent, Quiet),
    row("k816-cross-unit-contract", K816Extension, Quiet),
    row("k816-far-in-inline", K816Extension, Quiet),
    row("k816-inline-across-units", K816Extension, Quiet),
    row("k816-native-mnemonics", K816Extension, Rejects),
    row("k816-typed-data-entries", K816Extension, Rejects),
    row("k816-status-register-stack", K816Extension, Rejects),
    row("k816-operand-expressions", K816Extension, Rejects),
    row("k816-immediate-marker", K816Extension, Rejects),
    row("k816-address-encoding", K816Extension, Rejects),
    row("k816-stack-relative", K816Extension, Rejects),
    row("k816-indexed-offset", K816Extension, Rejects),
    row("k816-import", K816Extension, Rejects),
    row("k816-extern", K816Extension, Rejects),
    row("k65-global-namespace", IntentionallyChanged, Quiet),
    row("optimizer-branch-to-next-label", BackendDifferent, Quiet),
    row("optimizer-duplicate-return", BackendDifferent, Quiet),
];

/// The row `id` names, or `None` when the manifest has no such row.
pub fn manifest_row(id: &str) -> Option<&'static ManifestRow> {
    MANIFEST.iter().find(|row| row.id == id)
}

// ---------------------------------------------------------------------------
// `--strict-k65`
// ---------------------------------------------------------------------------

/// What a reported row is called in a message, and what a K65 source writes
/// instead where anything does.
///
/// Separate from [`MANIFEST`] because only the rows that report need it, and a
/// `what` on every row would be a second description of the language beside
/// the manifest's own. `strict_rows_are_all_reportable` asserts the two lists
/// name the same set.
struct Reported {
    id: &'static str,
    /// A noun phrase: "the construct" the message names.
    what: &'static str,
    /// The K65 spelling, where one exists. `None` says so out loud rather
    /// than leaving the author to guess that nothing was offered.
    instead: Option<&'static str>,
}

const REPORTED: &[Reported] = &[
    Reported {
        id: "k816-width-annotations",
        what: "a register-width annotation",
        instead: None,
    },
    Reported {
        id: "k816-far-function",
        what: "long addressing",
        instead: Some(
            "K65's `far` is a bank-switching stub its linker generates, not an instruction",
        ),
    },
    Reported {
        id: "k816-segment",
        what: "`segment`",
        instead: Some("K65 writes `bank NAME`, which K816 removed the other way (`K816-K65-0003`)"),
    },
    Reported {
        id: "k816-call-contracts",
        what: "a call contract",
        instead: Some("K65 calls are unchecked; write the call without the `(…)` or `->` clause"),
    },
    Reported {
        id: "k816-register-names",
        what: "a register the 6502 does not have",
        instead: None,
    },
    Reported {
        id: "k816-accumulator-one-name",
        what: "`b`, the accumulator's high half",
        instead: None,
    },
    Reported {
        id: "k816-const-auto-increment",
        what: "a `const` group entry with no value",
        instead: Some("K65 dies when a valueless constant is read; give every entry a value"),
    },
    Reported {
        id: "k816-symbolic-subscript",
        what: "a symbolic subscript",
        instead: Some("K65's declaration bracket holds a size in bytes and nothing else"),
    },
    Reported {
        id: "k816-field-access",
        what: "an access into a layout",
        instead: Some("K65's operand grammar is `Ident`, `Ident ± Ident` and `Ident ± Number`"),
    },
    Reported {
        id: "k816-metadata-query",
        what: "a compile-time metadata query",
        instead: None,
    },
    Reported {
        id: "k816-typed-view",
        what: "a data-width annotation",
        instead: Some("every K65 datum is a byte, and a word is two entries"),
    },
    Reported {
        id: "k816-abstract-var",
        what: "`abstract var`",
        instead: None,
    },
    Reported {
        id: "k816-eval-extensions",
        what: "an evaluator operator or function K816 added",
        instead: None,
    },
    Reported {
        id: "k816-postfix-gt",
        what: "the `>` branch condition",
        instead: Some("K65's `BranchOp` has `>=`, `<`, `==`, `!=`, `<0`, `>=0`, `>>=` and `<<=`"),
    },
    Reported {
        id: "k816-postfix-le",
        what: "the `<=` branch condition",
        instead: Some("K65's `BranchOp` has `>=`, `<`, `==`, `!=`, `<0`, `>=0`, `>>=` and `<<=`"),
    },
    Reported {
        id: "k816-hex-binary-char-literals",
        what: "a `$`, `%` or character literal",
        instead: Some(
            "K65 writes `0x` for hex and `0b` for binary, and has no character literal and no string escape",
        ),
    },
    Reported {
        id: "k816-direct-page-pool",
        what: "the `dp` storage class",
        instead: Some("K65 has no direct-page pool; pin the address with `= $addr`"),
    },
    Reported {
        id: "k816-native-mnemonics",
        what: "a raw 65xx mnemonic",
        instead: Some(
            "K65 has no raw-mnemonic production at all; write the HLA form the instruction spells",
        ),
    },
    Reported {
        id: "k816-typed-data-entries",
        what: "a typed data entry",
        instead: Some("K65's data entries are bytes, and a 16-bit address is `&& NAME`"),
    },
    Reported {
        id: "k816-status-register-stack",
        what: "`p??` / `p!!`",
        instead: Some(
            "K65's own `Flag ??` / `Flag !!` rules are commented out; its seven letter forms are `PLP`/`PHP` under another name, and K816 refuses those",
        ),
    },
    Reported {
        id: "k816-operand-expressions",
        what: "an operand expression",
        instead: Some(
            "K65's operand arithmetic is `NAME + NAME` and `NAME - number` and nothing else; fold the rest in a `[ … ]` bracket",
        ),
    },
    Reported {
        id: "k816-immediate-marker",
        what: "the `#` immediate marker",
        instead: Some(
            "K65 has no `#` at all, and needs none: a literal and a `const` are immediates in both compilers, so drop the marker — before a *name* it means the address as a value, which K65 spells one byte at a time with `&<` / `&>`",
        ),
    },
    Reported {
        id: "k816-address-encoding",
        what: "an address-encoding prefix",
        instead: Some(
            "K65 decides zero page against absolute from the value; there is no prefix to write",
        ),
    },
    Reported {
        id: "k816-stack-relative",
        what: "stack-relative indexing",
        instead: Some(
            "K65 indexes with `,X` and `,Y` and nothing else; the 6502 has no stack-relative mode",
        ),
    },
    Reported {
        id: "k816-indexed-offset",
        what: "the `&` indexed-offset marker",
        instead: Some("K65 indexes a name — `NAME,x` — and has no marker for a constant offset"),
    },
    Reported {
        id: "instr-nop-wait",
        what: "the cycle wait",
        instead: Some(
            "the original counts *cycles* and spends an odd one through `NOP $80`, which is `TSB dp` on the 65816; K816 emits N `NOP`s",
        ),
    },
    Reported {
        id: "data-evaluator-entry",
        what: "an assignment inside a section transform",
        instead: Some(
            "the original's assignment outlives the transform and K816's does not; assign in a block of its own",
        ),
    },
    Reported {
        id: "k816-import",
        what: "`import`",
        instead: Some(
            "K65 has no import syntax: its linker resolved every name across a project's \
             sources, and K816 compiles one unit and the units it names",
        ),
    },
    Reported {
        id: "k816-extern",
        what: "an `extern` declaration",
        instead: Some(
            "K65 has no way to name a symbol another object provides, because it had no \
             other language to meet",
        ),
    },
];

/// Every construct of one unit that a manifest row classifies, as
/// `(row id, span)` in source order.
///
/// The match over the AST decision 20 asks for, exposed rather than kept
/// inside [`strict_k65`] so the suite can ask what the walk *finds* without
/// going through the wording of a diagnostic. `tests/compat.rs` runs it over
/// the whole corpus and checks that the set of rows it produces is the set
/// the manifest's `Strict` column marks — which is the only thing that keeps
/// a row from being marked in the file and found by nothing.
///
/// The walk is over the program *after* the preprocessor, which is what the
/// build compiles: a construct inside a region this unit did not take is not
/// in this build and the flag has nothing to say about it. Nodes whose span
/// belongs to another file are borrowed `inline` bodies and are skipped —
/// the unit that wrote a construct is the unit that has to change it, which
/// is the rule `K816-E0156` already states.
pub fn classify(file: &AstFile, source: &crate::source::SourceFile) -> Vec<(&'static str, Span)> {
    let mut walk = StrictWalk {
        text: source.text(),
        unit: source.id(),
        evalfunc: None,
        found: Vec::new(),
    };
    walk.file(file);
    walk.found.sort_by_key(|(_, span)| (span.start, span.end));
    walk.found
}

/// Everything `--strict-k65` has to say about one unit.
pub fn strict_k65(file: &AstFile, source: &crate::source::SourceFile) -> Vec<Diagnostic> {
    classify(file, source)
        .into_iter()
        .filter_map(|(id, span)| report(id, span))
        .collect()
}

/// One diagnostic for one occurrence, or `None` where the row says nothing.
fn report(id: &'static str, span: Span) -> Option<Diagnostic> {
    let row = manifest_row(id)?;
    let reported = REPORTED.iter().find(|entry| entry.id == id)?;
    let mut diagnostic = match row.strict {
        Strict::Rejects => Diagnostic::error(
            DiagnosticCode::STRICT_K65_EXTENSION,
            format!("{} is not in the original K65 language", reported.what),
            Label::new(span, "`--strict-k65` rejects this"),
        ),
        Strict::Warns => Diagnostic::warning(
            DiagnosticCode::STRICT_K65_CHANGED,
            format!("{} means something else in the original K65", reported.what),
            Label::new(span, "this compiles in both and produces different bytes"),
        ),
        Strict::Quiet => return None,
    }
    .with_note(format!(
        "`doc/k65-compat-manifest.md` classifies this as `{id}`"
    ));
    if let Some(instead) = reported.instead {
        diagnostic = diagnostic.with_help(instead);
    }
    Some(diagnostic.with_docs("doc/k65-compat.md"))
}

/// The tree walk decision 20 asks for: one classification per node.
///
/// It answers about the *source*, not about what the source means, which is
/// why it runs on the AST and not on the semantic model — a `dpr` is a K816
/// spelling whether or not the instruction it names ever assembles. Nodes
/// whose span belongs to another file are borrowed `inline` bodies and are
/// skipped: the unit that wrote a construct is the unit that has to change it.
struct StrictWalk<'a> {
    text: &'a str,
    unit: crate::source::FileId,
    /// The `evalfunc` whose body is being walked, for the self-call.
    evalfunc: Option<String>,
    found: Vec<(&'static str, Span)>,
}

impl StrictWalk<'_> {
    fn mark(&mut self, id: &'static str, span: Span) {
        if span.file != self.unit {
            return;
        }
        self.found.push((id, span));
    }

    /// The source text a span covers, for the rows that turn on a *spelling*
    /// the AST decodes away — `$FF` and `255` are one `Number(255)`.
    fn spelling(&self, span: Span) -> &str {
        self.text
            .get(span.start as usize..span.end as usize)
            .unwrap_or_default()
    }

    /// A literal written in one of K816's own spellings.
    ///
    /// `$` and `%` and a character literal are the three the original's
    /// terminal alphabet has not (`compiler.inc:478-497`). The check is on the
    /// text because the decoded value cannot carry it.
    fn literal(&mut self, span: Span) {
        if span.file != self.unit {
            return;
        }
        let text = self.spelling(span).trim_start();
        if text.starts_with('$') || text.starts_with('%') || text.starts_with('\'') {
            self.mark("k816-hex-binary-char-literals", span);
        }
    }

    /// A string written with an escape in it, which the original's
    /// `"[^"]*"` terminal cannot express.
    fn string_literal(&mut self, span: Span) {
        if span.file != self.unit {
            return;
        }
        if self.spelling(span).contains('\\') {
            self.mark("k816-hex-binary-char-literals", span);
        }
    }

    fn file(&mut self, file: &AstFile) {
        for item in &file.items {
            self.item(item);
        }
    }

    fn item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Var(var) => self.var(var, item.span),
            ItemKind::Const(declaration) => {
                // A valueless entry *inside a group* is K816's enum-like
                // auto-increment. A standalone one is the original's
                // `Const -> Ident`, which K816 refuses outright
                // (`topelem-const`) — a different row, and not this one.
                let group = declaration.entries.len() > 1;
                for entry in &declaration.entries {
                    if group && entry.value.is_none() {
                        self.mark("k816-const-auto-increment", entry.span);
                    }
                    if let Some(value) = &entry.value {
                        self.expr(value);
                    }
                }
            }
            ItemKind::Segment(name) => self.mark("k816-segment", name.span),
            ItemKind::Data(block) => self.data_block(block),
            ItemKind::EvalBlock(expr) => self.expr(expr),
            // A self-call is one of `k816-eval-extensions`' five, and it is
            // the only one that needs to know where it stands: the *name* is
            // ordinary, and what makes it an extension is that it is the name
            // of the function being defined.
            ItemKind::EvalFunc(function) => {
                let outer = self.evalfunc.replace(function.name.text.clone());
                self.eval(&function.body);
                self.evalfunc = outer;
            }
            ItemKind::Preprocessor(directive) => {
                if let Some(condition) = &directive.condition {
                    self.eval(condition);
                }
            }
            ItemKind::Binary(_) | ItemKind::Reserved(_) | ItemKind::Error => {}
            ItemKind::ModuleWidth(width) => self.mark("k816-width-annotations", width.span),
            ItemKind::Import(_) => self.mark("k816-import", item.span),
            // One mark per declaration, at the `extern` keyword. The widths
            // inside are not separately `k816-width-annotations`: an extern
            // has no K65 spelling with or without them, so a second
            // diagnostic on the same line would say nothing the first did
            // not.
            ItemKind::Extern(declaration) => self.mark("k816-extern", declaration.keyword_span),
            ItemKind::Func(function) => self.function(function),
        }
    }

    fn var(&mut self, var: &VarDeclaration, span: Span) {
        if var.is_abstract {
            self.mark("k816-abstract-var", span);
        }
        if let Some(storage) = &var.storage {
            match storage.kind {
                // The pool is its own decision and its own row (G7).
                StorageClassKind::DirectPage => {
                    self.mark("k816-direct-page-pool", storage.span);
                }
                // `abs` and `far` are the same axis one step down: K65's
                // `Var -> Ident [BracketNumber] [= Number]` has no storage
                // class at all, and no 24-bit address to give `far`.
                StorageClassKind::Absolute | StorageClassKind::Far => {
                    self.mark("k816-address-encoding", storage.span);
                }
            }
        }
        if let Some(width) = &var.width {
            self.mark("k816-typed-view", width.span);
        }
        if !var.fields.is_empty() {
            self.mark("k816-symbolic-subscript", span);
            for field in &var.fields {
                self.field_declaration(field);
            }
        }
        if let Some(count) = &var.count {
            self.mark("k816-symbolic-subscript", count.span);
            self.expr(count);
        }
        if let Some(size) = &var.size {
            self.expr(size);
        }
        if let Some(address) = &var.address {
            self.expr(address);
        }
    }

    fn field_declaration(&mut self, field: &FieldDeclaration) {
        if let Some(width) = &field.width {
            self.mark("k816-typed-view", width.span);
        }
        if let Some(size) = &field.size {
            self.expr(size);
        }
        for nested in &field.fields {
            self.field_declaration(nested);
        }
    }

    fn function(&mut self, function: &Function) {
        if function.is_far {
            self.mark("k816-far-function", function.name.span);
        }
        for width in &function.width {
            self.mark("k816-width-annotations", width.span);
        }
        if let Some(contract) = &function.contract {
            self.mark("k816-call-contracts", contract.span);
            if let Some(outputs) = &contract.outputs {
                for width in &outputs.width {
                    self.mark("k816-width-annotations", width.span);
                }
            }
        }
        self.block(&function.body);
    }

    fn block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Label(_)
            | StatementKind::Break
            | StatementKind::Repeat
            | StatementKind::Reserved(_)
            | StatementKind::Return(_)
            | StatementKind::Error => {}
            // Every raw mnemonic, not only the long ones: `compiler.inc` has no
            // mnemonic production at all, so `lda $10` is as much K816's as
            // `jsl` is. The three long forms keep their own row on top of this
            // one, because what makes them an extension is the *addressing*
            // and a K65 author reading the message needs that said.
            StatementKind::Native(instruction) => {
                self.mark("k816-native-mnemonics", instruction.mnemonic.span);
                if matches!(
                    instruction.mnemonic.text.to_ascii_lowercase().as_str(),
                    "jsl" | "jml" | "rtl"
                ) {
                    self.mark("k816-far-function", instruction.mnemonic.span);
                }
                if let Some(operand) = &instruction.operand {
                    self.native_operand(operand);
                }
                if let Some(repeat) = &instruction.repeat {
                    self.expr(repeat);
                }
            }
            StatementKind::Hla(hla) => self.hla(hla, statement.span),
            StatementKind::WidthBlock(block) => {
                for width in &block.width {
                    self.mark("k816-width-annotations", width.span);
                }
                self.block(&block.body);
            }
            StatementKind::ModeSet(widths) => {
                for width in widths {
                    self.mark("k816-width-annotations", width.span);
                }
            }
            // `far NAME` and `call NAME` are both K65's (`instr-far-call`,
            // `instr-call-label`). What K816 changed is what `far` *means* —
            // a `JSL` rather than a linker-generated stub — and that is
            // `k816-far-function`'s subject at the declaration, not here.
            StatementKind::Call(_) => {}
            StatementKind::BareCall(call) => {
                if call.arguments.is_some() || call.outputs.is_some() {
                    self.mark("k816-call-contracts", statement.span);
                }
                for argument in call.arguments.iter().flatten() {
                    match argument {
                        CallArgument::Register(register) => self.register(register),
                        CallArgument::Immediate(expr) => self.expr(expr),
                    }
                }
                for output in call.outputs.iter().flatten() {
                    if let ContractOutput::Register(register) = output {
                        self.register(register);
                    }
                }
            }
            StatementKind::Conditional(conditional) => {
                self.condition(&conditional.condition);
                self.block(&conditional.body);
                if let Some(otherwise) = &conditional.otherwise {
                    self.block(otherwise);
                }
            }
            StatementKind::ConditionalJump(jump) => {
                self.condition(&jump.condition);
                self.statement(&jump.jump);
            }
            StatementKind::Loop(loop_statement) => {
                if let LoopCondition::Branch(condition) = &loop_statement.condition {
                    self.condition(condition);
                }
                self.block(&loop_statement.body);
            }
            // `far goto NAME` is the original's own word order and its own
            // construct (`instr-far-goto`), landed in G7.
            StatementKind::Goto(goto) => self.expr(&goto.target),
            StatementKind::Nocross(block) => self.block(block),
            StatementKind::Data(block) => self.data_block(block),
            StatementKind::Align(align) => {
                self.expr(&align.boundary);
                if let Some(offset) = &align.offset {
                    self.expr(offset);
                }
            }
            StatementKind::Address(expr) => self.expr(expr),
            StatementKind::Preprocessor(directive) => {
                if let Some(condition) = &directive.condition {
                    self.eval(condition);
                }
            }
        }
    }

    /// `>` and `<=` are absent from the original's `BranchOp` entirely, in
    /// every position — a prefix block, a postfix loop, a conditional jump —
    /// which is why the check is on the condition and not on the statement.
    fn condition(&mut self, condition: &BranchCondition) {
        match condition.kind {
            BranchConditionKind::Comparison(ComparisonKind::Greater) => {
                self.mark("k816-postfix-gt", condition.span);
            }
            BranchConditionKind::Comparison(ComparisonKind::LessEqual) => {
                self.mark("k816-postfix-le", condition.span);
            }
            _ => {}
        }
    }

    fn hla(&mut self, hla: &HlaStatement, span: Span) {
        match hla {
            HlaStatement::Assign(assign) => {
                for target in &assign.targets {
                    self.hla_operand(target);
                }
                self.hla_operand(&assign.source);
            }
            HlaStatement::Exchange(exchange) => {
                self.register(&exchange.left);
                self.register(&exchange.right);
            }
            HlaStatement::Arithmetic(arithmetic) => {
                self.register(&arithmetic.register);
                self.hla_operand(&arithmetic.operand);
            }
            HlaStatement::Shift(shift) => self.hla_operand(&shift.target),
            HlaStatement::Step(step) => self.hla_operand(&step.target),
            HlaStatement::Flag(_) => {}
            // `a??`/`a!!` is K65's (`instr-stack-a`). `p??`/`p!!` is not: the
            // original's two `Flag ??`/`Flag !!` rules are commented out, and
            // the seven letter forms that replaced them are `PLP`/`PHP` under
            // a misleading name, which K816 refuses (`instr-stack-flags`). So
            // the spelling K816 offers instead is one the original never had.
            HlaStatement::Stack(stack) => {
                if stack.register.kind == RegisterKind::P {
                    self.mark("k816-status-register-stack", stack.register.span);
                }
                self.register(&stack.register);
            }
            // `*`, `* N` and `%` all reach `opnop`, which counts *cycles*.
            // Decision 11: the one place a K65 spelling keeps a K816 meaning,
            // and the one place the flag has something to warn about that the
            // compiler is not already refusing.
            HlaStatement::Nop { count } => {
                self.mark("instr-nop-wait", span);
                if let Some(count) = count {
                    self.expr(count);
                }
            }
            HlaStatement::Break => self.mark("instr-nop-wait", span),
        }
    }

    fn hla_operand(&mut self, operand: &HlaOperand) {
        match &operand.kind {
            HlaOperandKind::Register(register) => self.register(register),
            // `#` marks an immediate here and nowhere in the original, which
            // has none: `Arg -> Number` *is* the immediate (`:269`), so `a = 1`
            // loads one there and an address here.
            HlaOperandKind::Immediate(expr) => {
                self.mark("k816-immediate-marker", operand.span);
                self.expr(expr);
            }
            HlaOperandKind::Memory(address) => {
                self.encoding(address.encoding.as_ref());
                self.index(address.index.as_ref());
                // `&expr,x` — a constant serving as a small field offset while
                // the base sits in X or Y. `Arg -> VarArg , X` takes a
                // `VarArg`, which is a name with at most an offset, and has no
                // marker to write.
                if let Some(marker) = address.offset_marker {
                    self.mark("k816-indexed-offset", marker);
                }
                self.expr(&address.value);
            }
            HlaOperandKind::Indirect(indirect) => {
                self.encoding(indirect.encoding.as_ref());
                self.index(indirect.inner_index.as_ref());
                self.index(indirect.outer_index.as_ref());
                self.expr(&indirect.value);
            }
            HlaOperandKind::Error => {}
        }
    }

    /// `,s` — stack-relative indexing.
    ///
    /// `Arg -> VarArg , X | VarArg , Y | ( VarArg , X ) | ( VarArg ) , Y` is
    /// the whole of the original's indexing (`:272-276`), and the 6502 has no
    /// stack-relative mode for a fifth to name.
    fn index(&mut self, index: Option<&crate::ast::IndexRegister>) {
        if let Some(index) = index
            && index.kind == crate::ast::IndexRegisterKind::Stack
        {
            self.mark("k816-stack-relative", index.span);
        }
    }

    /// A `dp` / `abs` / `far` prefix in operand position.
    ///
    /// K65 decides zero page against absolute from the value — its own grammar
    /// comment says the two are not spelled differently — and it has no 24-bit
    /// address at all, so every one of the three is K816's.
    fn encoding(&mut self, encoding: Option<&crate::ast::AddressEncoding>) {
        if let Some(encoding) = encoding {
            self.mark("k816-address-encoding", encoding.span);
        }
    }

    /// A raw mnemonic's operand, which reports nothing of its own.
    ///
    /// `k816-native-mnemonics` is one row for the whole native layer, operand
    /// grammar included: the original has no mnemonic production, so it has no
    /// operand grammar for one either, and `#` and `dp` there are part of the
    /// form already reported rather than two more findings. What *is* walked
    /// is what an operand can nest — a literal's spelling, and an evaluator
    /// bracket, both of which are rows in their own right wherever they stand.
    fn native_operand(&mut self, operand: &NativeOperand) {
        match operand {
            NativeOperand::Immediate(expr) => self.expr(expr),
            NativeOperand::Register(register) => self.register(register),
            NativeOperand::Address(address) => self.expr(&address.value),
            NativeOperand::Indirect(indirect) => self.expr(&indirect.value),
            NativeOperand::LongIndirect(indirect) => self.expr(&indirect.value),
            NativeOperand::BlockMove(_) => {}
        }
    }

    fn register(&mut self, register: &Register) {
        match register.kind {
            RegisterKind::Dpr | RegisterKind::Dbr => {
                self.mark("k816-register-names", register.span);
            }
            RegisterKind::B => self.mark("k816-accumulator-one-name", register.span),
            _ => {}
        }
    }

    fn data_block(&mut self, block: &DataBlock) {
        for entry in &block.entries {
            self.data_entry(entry);
        }
    }

    fn data_entry(&mut self, entry: &DataEntry) {
        match &entry.kind {
            DataEntryKind::Segment(name) => self.mark("k816-segment", name.span),
            DataEntryKind::Align(align) => {
                self.expr(&align.boundary);
                if let Some(offset) = &align.offset {
                    self.expr(offset);
                }
            }
            DataEntryKind::Address(expr) => self.expr(expr),
            DataEntryKind::Nocross | DataEntryKind::Label(_) | DataEntryKind::Error => {}
            DataEntryKind::Charset(_) => self.string_literal(entry.span),
            // `byte 1 2`, `word main`, `far handler`. K65's data list has one
            // untyped entry (`Data -> Arg`, a byte) and one width keyword-free
            // way to spell an address (`Data -> && VarArg`); the keywords are
            // K816's.
            DataEntryKind::Typed { values, .. } => {
                self.mark("k816-typed-data-entries", entry.span);
                for value in values {
                    self.data_value(value);
                }
            }
            DataEntryKind::Value(value) => self.data_value(value),
            DataEntryKind::Code { body, .. } => self.block(body),
            DataEntryKind::NocrossGroup(entries) => {
                for entry in entries {
                    self.data_entry(entry);
                }
            }
            // The transform itself is K65's. What differs is that an
            // assignment inside it does not outlive the byte it ran for, so
            // only a body that assigns has anything to warn about — a
            // transform that merely computes agrees with the original.
            DataEntryKind::Evaluator(body) => {
                if eval_assigns(body) {
                    self.mark("data-evaluator-entry", entry.span);
                }
                self.eval(body);
            }
            DataEntryKind::Repeat { count, body } => {
                self.expr(count);
                self.data_block(body);
            }
            DataEntryKind::ForEval { from, to, body, .. } => {
                self.expr(from);
                self.expr(to);
                self.expr(body);
            }
            DataEntryKind::Preprocessor(directive) => {
                if let Some(condition) = &directive.condition {
                    self.eval(condition);
                }
            }
            DataEntryKind::Binary(_) | DataEntryKind::Reserved(_) => {}
        }
    }

    fn data_value(&mut self, value: &DataValue) {
        match value {
            DataValue::Expr(expr) => self.expr(expr),
            // The string's own span, not the entry's: `byte "A" "B\n"` has one
            // escape in it, and reporting the entry once per value in it said
            // the same thing twice.
            DataValue::String { span, .. } => self.string_literal(*span),
            DataValue::Undefined => {}
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Number(_) => self.literal(expr.span),
            ExprKind::Symbol(_) | ExprKind::LocalSymbol(_) | ExprKind::Error => {}
            // `&<` and `&>` are K65's (`arg-forms`) and so is `&&`
            // (`data-address-word`) — K816 widened only *where* the last may
            // appear, which is the same free superset `#error` at three levels
            // is. `&&&` is the 24-bit form and the 6502 has no such address;
            // `-x` and `~x` are in the original's *evaluator* and nowhere in
            // its operand grammar.
            ExprKind::Unary(unary) => {
                if matches!(
                    unary.op,
                    UnaryOp::Negate | UnaryOp::BitNot | UnaryOp::Address24
                ) {
                    self.mark("k816-operand-expressions", unary.op_span);
                }
                self.expr(&unary.operand);
            }
            // `VarArg -> Ident ± Ident | Ident ± Number` is the whole of K65's
            // operand arithmetic (`compiler.inc:279-282`), and it is narrower
            // than "one operator between two leaves": the **left** side is an
            // `Ident`, always. `base + 1` is K65's and `1 + base` is not,
            // which is not symmetry anyone would have guessed and is why the
            // rule is read off the production rather than paraphrased.
            ExprKind::Binary(binary) => {
                let name = |expr: &Expr| {
                    matches!(expr.kind, ExprKind::Symbol(_) | ExprKind::LocalSymbol(_))
                };
                let leaf = |expr: &Expr| name(expr) || matches!(expr.kind, ExprKind::Number(_));
                if !matches!(binary.op, BinaryOp::Add | BinaryOp::Sub)
                    || !name(&binary.left)
                    || !leaf(&binary.right)
                {
                    self.mark("k816-operand-expressions", binary.op_span);
                }
                self.expr(&binary.left);
                self.expr(&binary.right);
            }
            ExprKind::Field(access) => {
                self.mark("k816-field-access", expr.span);
                self.expr(&access.base);
            }
            ExprKind::Subscript(subscript) => {
                self.mark("k816-field-access", expr.span);
                self.expr(&subscript.base);
                self.expr(&subscript.index);
            }
            ExprKind::Metadata(query) => {
                self.mark("k816-metadata-query", expr.span);
                self.expr(&query.base);
            }
            ExprKind::TypedView(view) => {
                self.mark("k816-typed-view", expr.span);
                self.expr(&view.base);
            }
            ExprKind::Eval(eval) => self.eval(eval),
        }
    }

    /// The evaluator's own language.
    ///
    /// All five of `k816-eval-extensions` are found here, and only two of them
    /// are spellings. `abs` is absent from the original's opcode table and
    /// `||` is not a token there. The other three are the original
    /// *malfunctioning* at syntax it accepts: the prefix step operators abort
    /// it, a **self-call** reaches evaluator function zero — `fact(4)` is `01`
    /// in the golden and `$18` here — and `0b1010` reads as **zero** inside a
    /// bracket, because the evaluator's `ParseInt` handles `0x` and not `0b`
    /// while the same literal outside a bracket is ten in both.
    ///
    /// *Mutual* recursion is not among them: the original refuses a forward
    /// reference between two `evalfunc`s outright ("Unknown evaluator function
    /// 'odd'"), and so does K816 (`K816-E0101`). Measured, so the walk looks
    /// for a self-call and not for a cycle.
    fn eval(&mut self, expr: &EvalExpr) {
        match &expr.kind {
            EvalExprKind::Number { .. } => {
                self.literal(expr.span);
                // `0b` is a K65 spelling that means one thing outside a
                // bracket and another inside one. `%` is K816's own and is
                // `k816-hex-binary-char-literals` wherever it stands.
                if self.spelling(expr.span).trim_start().starts_with("0b") {
                    self.mark("k816-eval-extensions", expr.span);
                }
            }
            EvalExprKind::String(_) => self.string_literal(expr.span),
            EvalExprKind::Pi | EvalExprKind::Float(_) | EvalExprKind::Symbol(_) => {}
            EvalExprKind::Call { name, arguments } => {
                if name.text == "abs" || self.evalfunc.as_deref() == Some(name.text.as_str()) {
                    self.mark("k816-eval-extensions", name.span);
                }
                for argument in arguments {
                    self.eval(argument);
                }
            }
            EvalExprKind::Unary {
                operator,
                operand,
                is_suffix,
            } => {
                if !is_suffix && (operator == "++" || operator == "--") {
                    self.mark("k816-eval-extensions", expr.span);
                }
                self.eval(operand);
            }
            EvalExprKind::Binary {
                operator,
                left,
                right,
            } => {
                if operator == "||" {
                    self.mark("k816-eval-extensions", expr.span);
                }
                self.eval(left);
                self.eval(right);
            }
            EvalExprKind::Ternary {
                condition,
                consequence,
                alternative,
            } => {
                self.eval(condition);
                self.eval(consequence);
                self.eval(alternative);
            }
            EvalExprKind::Assignment { target, value, .. } => {
                self.eval(target);
                self.eval(value);
            }
            EvalExprKind::Subscript { base, indices } => {
                self.eval(base);
                for index in indices {
                    self.eval(index);
                }
            }
            EvalExprKind::Member { base, .. } => self.eval(base),
            EvalExprKind::MethodCall {
                base, arguments, ..
            } => {
                self.eval(base);
                for argument in arguments {
                    self.eval(argument);
                }
            }
            EvalExprKind::List(items) => {
                for item in items {
                    self.eval(item);
                }
            }
            EvalExprKind::ReservedCall(_) => {}
            EvalExprKind::Block(items) => {
                for item in items {
                    self.eval(item);
                }
            }
            EvalExprKind::If {
                condition,
                consequence,
                alternative,
            } => {
                self.eval(condition);
                self.eval(consequence);
                if let Some(alternative) = alternative {
                    self.eval(alternative);
                }
            }
            EvalExprKind::While { condition, body } => {
                self.eval(condition);
                self.eval(body);
            }
            EvalExprKind::Error => {}
        }
    }
}

/// Whether an evaluator body assigns anything, at any depth.
///
/// An increment counts: `[ x + n++ ]` writes `n` as surely as `n = n + 1`
/// does, and it is the write that does not outlive the byte.
fn eval_assigns(expr: &EvalExpr) -> bool {
    match &expr.kind {
        EvalExprKind::Assignment { .. } => true,
        EvalExprKind::Unary {
            operator, operand, ..
        } => matches!(operator.as_str(), "++" | "--") || eval_assigns(operand),
        EvalExprKind::Number { .. }
        | EvalExprKind::Pi
        | EvalExprKind::Float(_)
        | EvalExprKind::String(_)
        | EvalExprKind::Symbol(_)
        | EvalExprKind::ReservedCall(_)
        | EvalExprKind::Error => false,
        EvalExprKind::Call { arguments, .. } => arguments.iter().any(eval_assigns),
        EvalExprKind::Binary { left, right, .. } => eval_assigns(left) || eval_assigns(right),
        EvalExprKind::Ternary {
            condition,
            consequence,
            alternative,
        } => eval_assigns(condition) || eval_assigns(consequence) || eval_assigns(alternative),
        EvalExprKind::Subscript { base, indices } => {
            eval_assigns(base) || indices.iter().any(eval_assigns)
        }
        EvalExprKind::Member { base, .. } => eval_assigns(base),
        EvalExprKind::MethodCall {
            base, arguments, ..
        } => eval_assigns(base) || arguments.iter().any(eval_assigns),
        EvalExprKind::List(items) | EvalExprKind::Block(items) => items.iter().any(eval_assigns),
        EvalExprKind::If {
            condition,
            consequence,
            alternative,
        } => {
            eval_assigns(condition)
                || eval_assigns(consequence)
                || alternative.as_deref().is_some_and(eval_assigns)
        }
        EvalExprKind::While { condition, body } => eval_assigns(condition) || eval_assigns(body),
    }
}

#[cfg(test)]
mod tests {
    use super::{k65_illegal_opcode, k65_removed_bank, k65_removed_image};
    use crate::diagnostic::DiagnosticCode;
    use crate::source::{FileId, SourceMap, Span};

    fn span() -> Span {
        let mut map = SourceMap::new();
        let id: FileId = map.add_file("t.k65", "data X { }");
        Span::new(id, 0, 4)
    }

    #[test]
    fn image_is_removed_rather_than_pending() {
        let diagnostic = k65_removed_image(span(), "tiles");
        assert_eq!(diagnostic.code, DiagnosticCode::K65_RESERVED_IMAGE);
        // The source's own word, so an author who wrote `tiles` reads
        // `tiles` back rather than a keyword they did not type.
        assert!(diagnostic.title.contains("`tiles`"));
        // Nothing here may read as a promise: there is no image reader
        // coming, and the help names what to do instead.
        assert!(!diagnostic.title.contains("yet"));
        assert!(
            diagnostic
                .help
                .is_some_and(|help| help.contains("`binary`"))
        );
    }

    #[test]
    fn bank_is_removed_rather_than_pending() {
        let diagnostic = k65_removed_bank(span(), "core");
        assert_eq!(diagnostic.code, DiagnosticCode::K65_REMOVED_BANK);
        // The distinction the wording carries: `bank` is not coming back, so
        // nothing here may read as a promise that it will.
        assert!(!diagnostic.title.contains("yet"));
        assert!(
            diagnostic
                .help
                .is_some_and(|help| help.contains("segment core"))
        );
    }

    #[test]
    fn an_illegal_opcode_names_what_the_bytes_do_and_offers_nothing() {
        let diagnostic = k65_illegal_opcode(span(), "a &. n", "ANC", "`PHD`");
        assert_eq!(
            diagnostic.code,
            DiagnosticCode::K65_UNSUPPORTED_ILLEGAL_OPCODE
        );
        assert!(diagnostic.notes.iter().any(|note| note.contains("PHD")));
        // No replacement sequence: the boundary is the answer, and a
        // suggested substitute would be K816 spelling an illegal opcode for
        // the author (`doc/design.md` §1). The help states the boundary and
        // proposes nothing.
        let help = diagnostic.help.expect("the boundary is stated");
        assert!(help.contains("documented"));
        assert!(!help.contains("instead") && !help.contains("write "));
    }

    /// Every row the manifest marks has wording, and nothing else does.
    ///
    /// [`MANIFEST`] decides *whether* a row reports and [`REPORTED`] decides
    /// *what it says*, and the two are separate because only nineteen rows
    /// need the second. Two lists means they can disagree, and the way they
    /// disagree is silent: `report` returns `None` when a row has no wording,
    /// so a marked row with no entry here would simply never be reported —
    /// the exact failure `strict_mode_reports_exactly_the_rows_the_manifest_marks`
    /// takes a whole corpus to notice.
    #[test]
    fn every_reported_row_has_wording_and_every_wording_a_row() {
        use super::{MANIFEST, REPORTED, Strict};

        for row in MANIFEST {
            let has_wording = REPORTED.iter().any(|entry| entry.id == row.id);
            assert_eq!(
                row.strict != Strict::Quiet,
                has_wording,
                "`{}` is marked {:?} and {} wording",
                row.id,
                row.strict,
                if has_wording { "has" } else { "has no" }
            );
        }
        for entry in REPORTED {
            assert!(
                MANIFEST.iter().any(|row| row.id == entry.id),
                "`{}` has wording and is not a manifest row",
                entry.id
            );
        }
    }

    #[test]
    fn every_refusal_uses_the_k65_family() {
        for diagnostic in [
            k65_removed_image(span(), "image"),
            k65_removed_bank(span(), "core"),
            k65_illegal_opcode(span(), "a &. n", "ANC", "`PHD`"),
        ] {
            assert!(
                diagnostic.code.as_str().starts_with("K816-K65-"),
                "{} is not a compatibility code",
                diagnostic.code.as_str()
            );
        }
    }
}
