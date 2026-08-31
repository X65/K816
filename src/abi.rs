/*****************************************************************************/
/*                                                                           */
/*                                  abi.rs                                   */
/*                                                                           */
/*         The state the machine is in when the entry point runs             */
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

//! The entry ABI: what state the CPU is in when `main` gets control.
//!
//! This is one fact, and it exists as a *value* because it used to exist as
//! two comments. `lower.rs` said "the 65816 starts in 8-bit emulation mode
//! after reset, so a `SEP` at entry would be redundant"; `op.rs` said "every
//! function entry is `Native`". Both were documented, both were locally
//! reasoned, and neither could detect the other — which is
//! `plans/postmortem.md` §E.2.1, and the rule it produced: **a claim about
//! the target machine belongs in one module, citable and testable, not in the
//! comment of whichever pass needed it first.** This is that module.
//!
//! # What is frozen here, and what is not
//!
//! The *default* is frozen: [`EntryEnvironment::STARTUP_STUB`], taken from
//! the repository rather than from the hardware.
//! `examples/fullstack/startup.asm` does `sei; clc; xce; jsr main`, so `main`
//! is called by a stub that has already left emulation mode and has widened
//! neither axis. The entry ABI is a contract between the startup code and the
//! compiler, so the ABI is what the startup code does — not what the CPU does
//! at reset.
//!
//! The *value* is no longer frozen: [`crate::Options::entry`] carries it and
//! `--entry` states it, so a build that is not entered by that stub can say
//! so (`plans/06-compatibility.md` decision 21). What is still absent is
//! target *names* — no `--target snes`, no configuration file — because a
//! name is a mapping onto these axes and the configuration it has to agree
//! with does not exist yet (sub-plan I). The axes landed; the names did not.
//!
//! # Which sections the ABI answers for
//!
//! Two, and for one reason: **their caller is not a call site**. `main` is
//! entered by the startup stub and an interrupt handler by the hardware, so
//! neither has a caller K816 can bridge to. Every *other* function's contract
//! is a requirement on its callers, satisfied by a `REP`/`SEP` emitted at
//! each call site — which is why an ordinary function may declare whatever
//! widths it needs and these two may not.
//!
//! # What an annotation on one of them means
//!
//! It is an **assertion**, not a request: a claim *about* the ABI. A true one
//! is allowed and costs nothing, and a false one is
//! [`crate::diagnostic::DiagnosticCode::ENTRY_ABI_MISMATCH`]. No width setup
//! is generated for either kind — emitting a `REP` to make the claim true
//! would be K816 choosing an ABI that belongs to the startup code and to
//! whatever the interrupt interrupted.

use crate::diagnostic::DiagnosticCode;
use crate::isa::RegisterWidth;
use crate::op::{AxisState, EmulationState, ModeState, WidthContract};
use crate::width::Axis;

/// Why a `--entry` spelling was refused.
///
/// Not a [`crate::diagnostic::Diagnostic`]: that anchors a message to a
/// span in a file, and a command line has neither. It carries the code
/// anyway — the pairing of code and wording belongs in the library, beside
/// the rule being enforced, rather than in the CLI that prints it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EntrySpelling {
    message: String,
}

impl EntrySpelling {
    /// The code every `--entry` refusal carries.
    pub const CODE: DiagnosticCode = DiagnosticCode::ENTRY_SPELLING;

    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    /// What went wrong, without the code.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl std::fmt::Display for EntrySpelling {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

/// The machine state an entry point is handed.
///
/// Three axes, because they are the three the width model tracks. Banks,
/// memory layout, and the direct-page register are not here: an ABI fact
/// nothing can check is a comment with a struct around it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EntryEnvironment {
    /// Whether the CPU has left emulation mode.
    pub e: EmulationState,
    /// The accumulator width the entry point starts with.
    pub a: RegisterWidth,
    /// The index-register width the entry point starts with.
    pub i: RegisterWidth,
}

impl Default for EntryEnvironment {
    /// [`Self::STARTUP_STUB`]: what a build gets when it states nothing.
    ///
    /// A default rather than a hardcode, which is the whole of the difference
    /// J was after: the compiler no longer *decides* what `main` is entered
    /// with, it is told, and this is what it is told when the command line is
    /// silent.
    fn default() -> Self {
        Self::STARTUP_STUB
    }
}

impl EntryEnvironment {
    /// The environment `examples/fullstack/startup.asm` establishes: native
    /// mode, 8-bit accumulator, 8-bit index registers.
    ///
    /// Emulation would have been the safer *guess* — assuming native where
    /// the machine is in emulation emits a two-byte immediate the CPU reads
    /// as one, while the reverse only refuses working code — and that
    /// asymmetry is worth remembering when a second environment appears. But
    /// nothing is being guessed while the stub is in the tree.
    pub const STARTUP_STUB: Self = Self {
        e: EmulationState::Native,
        a: RegisterWidth::W8,
        i: RegisterWidth::W8,
    };

    /// Reads a `--entry` value: `native,a8,i8`, and every prefix and subset of
    /// it (`plans/06-compatibility.md` decision 21).
    ///
    /// Three axes, each optional, written in one order — mode, then
    /// accumulator, then index. An axis left out keeps the default's value,
    /// so `--entry a16` states one thing and inherits the rest, and the order
    /// is fixed so that a reader of a build script does not have to work out
    /// which spelling of the same environment they are looking at.
    ///
    /// `emulation` is not just a third value of the first axis: the machine
    /// it describes forces M and X to one (`doc/w65c816s.md`), so both
    /// registers are eight bits wide and there is nothing to choose. Writing
    /// `emulation,a16` therefore describes no machine at all, and it is
    /// refused rather than silently corrected — a build that believes it
    /// asked for sixteen bits and got eight would find out in the bytes.
    pub fn parse(text: &str) -> Result<Self, EntrySpelling> {
        /// Which axis a field belongs to, which is also its position.
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        enum Slot {
            Mode,
            Accumulator,
            Index,
        }

        impl Slot {
            fn name(self) -> &'static str {
                match self {
                    Self::Mode => "mode",
                    Self::Accumulator => "accumulator",
                    Self::Index => "index-register",
                }
            }
        }

        // Written but empty: a shell that dropped the value, most likely, and
        // not a request for the default — which is what saying nothing at all
        // already means.
        if text.trim().is_empty() {
            return Err(EntrySpelling::new(
                "no axis named; write `native,a8,i8` or any part of it, or leave the flag \
                 off for the default",
            ));
        }

        let mut e: Option<EmulationState> = None;
        let mut a: Option<(RegisterWidth, &str)> = None;
        let mut i: Option<(RegisterWidth, &str)> = None;
        let mut filled: Option<Slot> = None;

        for field in text.split(',') {
            let field = field.trim();
            let slot = match field {
                "native" => {
                    e = Some(EmulationState::Native);
                    Slot::Mode
                }
                "emulation" => {
                    e = Some(EmulationState::Emulation);
                    Slot::Mode
                }
                "a8" => {
                    a = Some((RegisterWidth::W8, field));
                    Slot::Accumulator
                }
                "a16" => {
                    a = Some((RegisterWidth::W16, field));
                    Slot::Accumulator
                }
                "i8" => {
                    i = Some((RegisterWidth::W8, field));
                    Slot::Index
                }
                "i16" => {
                    i = Some((RegisterWidth::W16, field));
                    Slot::Index
                }
                "" => {
                    return Err(EntrySpelling::new(
                        "an axis is missing between the commas; write `native,a8,i8` or any \
                         part of it",
                    ));
                }
                other => {
                    return Err(EntrySpelling::new(format!(
                        "unknown axis `{other}` (`native` or `emulation`, `a8` or `a16`, `i8` \
                         or `i16`)"
                    )));
                }
            };
            match filled {
                Some(previous) if previous == slot => {
                    return Err(EntrySpelling::new(format!(
                        "`{field}` states the {} axis twice",
                        slot.name()
                    )));
                }
                Some(previous) if previous > slot => {
                    return Err(EntrySpelling::new(format!(
                        "`{field}` comes after the {} axis; the order is `native`/`emulation`, \
                         `a8`/`a16`, `i8`/`i16`",
                        previous.name()
                    )));
                }
                _ => filled = Some(slot),
            }
        }

        // `emulation` is the one axis that decides another, so a claim on the
        // widths is checked against it rather than overwritten by it.
        if e == Some(EmulationState::Emulation) {
            for (stated, register) in [(a, "accumulator"), (i, "index register")] {
                if let Some((RegisterWidth::W16, spelling)) = stated {
                    return Err(EntrySpelling::new(format!(
                        "`emulation` forces an 8-bit {register}, so `{spelling}` contradicts \
                         it; write `emulation` or `emulation,{}`",
                        spelling.replace("16", "8")
                    )));
                }
            }
        }

        let default = Self::default();
        Ok(Self {
            e: e.unwrap_or(default.e),
            // Emulation pins both axes at eight whatever the default is, so
            // that a later change to the default cannot make `--entry
            // emulation` describe a machine that does not exist.
            a: match e {
                Some(EmulationState::Emulation) => RegisterWidth::W8,
                _ => a.map_or(default.a, |(width, _)| width),
            },
            i: match e {
                Some(EmulationState::Emulation) => RegisterWidth::W8,
                _ => i.map_or(default.i, |(width, _)| width),
            },
        })
    }

    /// This environment as a tracked width state.
    pub fn state(self) -> ModeState {
        ModeState {
            a: AxisState::Known(self.a),
            i: AxisState::Known(self.i),
            e: self.e,
        }
    }

    /// The width state an entry point starts in, given what it annotated.
    ///
    /// A *matching* annotation and the environment are the same answer, so
    /// which one is read does not matter; a mismatched one has already been
    /// refused, and the annotation is read anyway so that the body is checked
    /// against the widths its author believed it had. Reporting the ABI
    /// mismatch and then a second complaint per immediate would be two
    /// diagnostics for one mistake.
    pub fn entry_state(self, contract: WidthContract) -> ModeState {
        ModeState {
            a: AxisState::Known(contract.a.unwrap_or(self.a)),
            i: AxisState::Known(contract.i.unwrap_or(self.i)),
            e: self.e,
        }
    }

    /// The axes `contract` declares that this environment does not provide.
    ///
    /// An annotation that restates what the environment provides is not a
    /// mismatch — it is a true claim, and repeating a true claim is allowed.
    /// An axis the annotation left unstated is not one either: it asks
    /// nothing, so there is nothing to disagree with.
    pub fn mismatches(self, contract: WidthContract) -> Vec<Axis> {
        let mut mismatched = Vec::new();
        if matches!(contract.a, Some(width) if width != self.a) {
            mismatched.push(Axis::Accumulator);
        }
        if matches!(contract.i, Some(width) if width != self.i) {
            mismatched.push(Axis::Index);
        }
        mismatched
    }

    /// The width this environment provides on `axis`.
    pub fn width(self, axis: Axis) -> RegisterWidth {
        match axis {
            Axis::Accumulator => self.a,
            Axis::Index => self.i,
        }
    }

    /// The width state a function body starts in.
    ///
    /// The single answer to "what holds when control arrives here", for both
    /// of the passes that ask: the width solver, which walks forward from it,
    /// and lowering, which claims it to the assembler. An ordinary function's
    /// answer is its contract — a caller that does not satisfy it is bridged
    /// at the call site — and a section the ABI answers for takes the
    /// environment's, since what enters it cannot be bridged.
    pub fn body_state(self, entry: Option<EntryKind>, contract: WidthContract) -> ModeState {
        match entry {
            Some(_) => self.entry_state(contract),
            None => ModeState::from_contract(contract, self.callee_emulation()),
        }
    }

    /// Nothing known but the emulation state, for a body K816 cannot see the
    /// caller of at all — a `code { }` block inside a data section.
    pub fn unknown_state(self) -> ModeState {
        ModeState {
            a: AxisState::Unknown,
            i: AxisState::Unknown,
            e: self.callee_emulation(),
        }
    }

    /// The emulation state a body that is *called* starts in.
    ///
    /// No contract can carry emulation mode — the language has no syntax for
    /// it — so nothing propagates it across a call and a callee's only ground
    /// for believing anything is a whole-program guarantee. `Native` is the
    /// one this compiler has always made, and it is **unchecked**: a caller
    /// that runs `xce` before the call leaves a native-entry program in
    /// emulation mode, and the callee goes on believing its `rep` widens
    /// (`plans/09-entry-abi.md` probe 2). It is kept because every program in
    /// the tree rests on it, and narrowing it would make an ordinary `rep`
    /// unsizeable everywhere.
    ///
    /// It is not *extended* to the new environment. Carrying `Emulation` into
    /// a callee would have it believe a `rep` establishes eight bits, and a
    /// caller that had left emulation mode would then get a one-byte
    /// immediate for a sixteen-bit load — a wrong byte, in the direction
    /// `plans/09-entry-abi.md` decision 3 singles out as the dangerous one.
    /// `Unknown` refuses to size the immediate instead, which costs working
    /// code and never writes a wrong one. A program that really does leave
    /// emulation mode says so with the annotation that is already the
    /// documented escape hatch for `xce`-then-widen.
    ///
    /// The entry point itself is unaffected: nothing runs before it, so there
    /// is no caller to have exchanged anything, and it starts in exactly what
    /// the environment states.
    fn callee_emulation(self) -> EmulationState {
        match self.e {
            EmulationState::Native => EmulationState::Native,
            EmulationState::Emulation | EmulationState::Unknown => EmulationState::Unknown,
        }
    }
}

/// A section whose caller is not a call site, and so whose widths are the
/// environment's rather than its contract's.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EntryKind {
    /// `main`, entered by the startup code.
    Main,
    /// A section that returns through `RTI`, entered by the hardware.
    ///
    /// The same environment answers for it, and for a reason that is worth
    /// keeping separate from "they happen to match": an interrupt is taken at
    /// whatever widths the interrupted code was running at, and the program's
    /// widths are the environment's until something widens them. A scoped
    /// `@a16 { … }` block that an interrupt lands inside is the case that
    /// would make this wrong, and K816 cannot see it — which is exactly why
    /// the ABI is a stated contract rather than an inference.
    Interrupt,
}

impl EntryKind {
    /// How to name this section in a diagnostic.
    pub fn describe(self) -> &'static str {
        match self {
            Self::Main => "the entry point",
            Self::Interrupt => "an interrupt handler",
        }
    }

    /// Who enters it, which is the reason it cannot be bridged.
    pub fn caller(self) -> &'static str {
        match self {
            Self::Main => "`main`'s caller is the environment",
            Self::Interrupt => "an interrupt handler is entered by the hardware",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn contract(a: Option<RegisterWidth>, i: Option<RegisterWidth>) -> WidthContract {
        WidthContract { a, i }
    }

    #[test]
    fn the_environment_answers_the_axes_an_entry_point_left_unstated() {
        let environment = EntryEnvironment::default();
        assert_eq!(environment, EntryEnvironment::STARTUP_STUB);

        // The case that makes a legacy `main` compile at all: no annotation,
        // and the widths are the environment's rather than unknown.
        let unannotated = environment.body_state(Some(EntryKind::Main), contract(None, None));
        assert_eq!(unannotated, environment.state());
        assert_eq!(unannotated.a, AxisState::Known(RegisterWidth::W8));
        assert_eq!(unannotated.i, AxisState::Known(RegisterWidth::W8));

        // A mismatched annotation is refused before this is asked, and the
        // state it produces is what the author believed — so that the body's
        // own diagnostics are about the body rather than about the ABI.
        let refused = environment.body_state(
            Some(EntryKind::Interrupt),
            contract(Some(RegisterWidth::W16), None),
        );
        assert_eq!(refused.a, AxisState::Known(RegisterWidth::W16));
        assert_eq!(refused.i, AxisState::Known(RegisterWidth::W8));
    }

    #[test]
    fn an_ordinary_function_is_answered_by_its_contract_and_not_by_the_abi() {
        // Its caller can be bridged, so an unstated axis is unknown and stays
        // that way — taking the entry ABI here would be inventing a width no
        // caller promised.
        let environment = EntryEnvironment::default();
        let unannotated = environment.body_state(None, contract(None, None));
        assert_eq!(unannotated.a, AxisState::Unknown);
        assert_eq!(unannotated.i, AxisState::Unknown);

        let annotated = environment.body_state(None, contract(Some(RegisterWidth::W16), None));
        assert_eq!(annotated.a, AxisState::Known(RegisterWidth::W16));
        assert_eq!(annotated.i, AxisState::Unknown);
    }

    #[test]
    fn only_a_contradicting_annotation_is_a_mismatch() {
        let environment = EntryEnvironment::default();
        // Saying nothing asks nothing.
        assert_eq!(environment.mismatches(contract(None, None)), []);
        // Restating what the environment provides is a true claim.
        assert_eq!(
            environment.mismatches(contract(Some(RegisterWidth::W8), Some(RegisterWidth::W8))),
            []
        );
        // And exactly the axes that contradict it are named.
        assert_eq!(
            environment.mismatches(contract(Some(RegisterWidth::W16), None)),
            [Axis::Accumulator]
        );
        assert_eq!(
            environment.mismatches(contract(Some(RegisterWidth::W8), Some(RegisterWidth::W16))),
            [Axis::Index]
        );
        assert_eq!(
            environment.mismatches(contract(Some(RegisterWidth::W16), Some(RegisterWidth::W16))),
            [Axis::Accumulator, Axis::Index]
        );
    }

    /// Every `--entry` spelling, read back as the environment it names.
    ///
    /// The subsets matter as much as the whole: an axis left out has to mean
    /// *the default*, because the flag's job is to change what a build states
    /// and not to force it to state everything.
    #[test]
    fn every_entry_spelling_names_the_environment_it_reads_like() {
        let default = EntryEnvironment::default();
        for (text, expected) in [
            ("native,a8,i8", default),
            ("native", default),
            ("a8", default),
            ("i8", default),
            (
                "a16",
                EntryEnvironment {
                    a: RegisterWidth::W16,
                    ..default
                },
            ),
            (
                "native,a16,i16",
                EntryEnvironment {
                    a: RegisterWidth::W16,
                    i: RegisterWidth::W16,
                    ..default
                },
            ),
            (
                "i16",
                EntryEnvironment {
                    i: RegisterWidth::W16,
                    ..default
                },
            ),
            (
                "emulation",
                EntryEnvironment {
                    e: EmulationState::Emulation,
                    a: RegisterWidth::W8,
                    i: RegisterWidth::W8,
                },
            ),
            (
                "emulation,a8,i8",
                EntryEnvironment {
                    e: EmulationState::Emulation,
                    a: RegisterWidth::W8,
                    i: RegisterWidth::W8,
                },
            ),
        ] {
            assert_eq!(
                EntryEnvironment::parse(text),
                Ok(expected),
                "`--entry {text}`"
            );
        }

        // Whitespace around an axis is allowed, so a build script may write
        // the list the way a person reads it.
        assert_eq!(EntryEnvironment::parse(" native , a8 , i8 "), Ok(default));
    }

    /// Every refusal, and the part of the spelling it names.
    ///
    /// The wording is asserted rather than merely the failure, because the
    /// whole reason this is a refusal and not a silent correction is that the
    /// author has to be told *which* half of what they wrote is impossible.
    #[test]
    fn a_spelling_that_names_no_machine_is_refused_and_says_which_half() {
        for (text, expected) in [
            ("emulation,a16", "forces an 8-bit accumulator"),
            ("emulation,a8,i16", "forces an 8-bit index register"),
            ("a8,native", "comes after the accumulator axis"),
            ("i8,a8", "comes after the index-register axis"),
            ("a8,a16", "states the accumulator axis twice"),
            ("native,native", "states the mode axis twice"),
            ("native,,a8", "an axis is missing between the commas"),
            ("A8", "unknown axis `A8`"),
            ("native a8", "unknown axis `native a8`"),
        ] {
            let refusal = EntryEnvironment::parse(text).expect_err(text);
            assert!(
                refusal.message().contains(expected),
                "`--entry {text}` said {refusal:?}, expected {expected:?}"
            );
        }
    }

    /// `emulation` pins the widths whatever the default happens to be.
    ///
    /// The default is native `a8,i8` today, so "emulation forces eight" and
    /// "the default is eight" give the same answer and a version that only
    /// defaulted would pass. This is the assertion that separates them.
    #[test]
    fn emulation_forces_eight_rather_than_inheriting_it() {
        let environment = EntryEnvironment::parse("emulation").expect("a legal spelling");
        assert_eq!(environment.a, RegisterWidth::W8);
        assert_eq!(environment.i, RegisterWidth::W8);
        assert_eq!(environment.e, EmulationState::Emulation);
        // And it is the state a body starts in, not merely a parsed value.
        assert_eq!(
            environment.body_state(Some(EntryKind::Main), contract(None, None)),
            environment.state()
        );
    }

    /// A callee does not inherit an emulation guarantee, and does inherit a
    /// native one.
    ///
    /// The asymmetry is deliberate and is the only place it is written as an
    /// assertion: `Native` is the assumption every existing program rests on,
    /// and extending it to `Emulation` would size an immediate against a
    /// guarantee no call site made.
    #[test]
    fn only_a_native_guarantee_reaches_a_callee() {
        let native = EntryEnvironment::default();
        let emulation = EntryEnvironment::parse("emulation").expect("a legal spelling");
        assert_eq!(
            native.body_state(None, contract(None, None)).e,
            EmulationState::Native
        );
        assert_eq!(
            emulation.body_state(None, contract(None, None)).e,
            EmulationState::Unknown
        );
        // The entry point keeps what the environment states, in both.
        assert_eq!(
            emulation
                .body_state(Some(EntryKind::Main), contract(None, None))
                .e,
            EmulationState::Emulation
        );
    }

    /// The seam `postmortem.md` §E.2.1 says was missing: the state lowering
    /// claims and the state the solver walks from are one value.
    ///
    /// Under the assertion model there is nothing left to reconcile — an
    /// entry point starts at the environment's widths and no instruction is
    /// generated to change them — so the claim is simply that a *legal*
    /// entry contract states the environment and nothing else.
    #[test]
    fn a_legal_entry_contract_is_the_environment_itself() {
        let environment = EntryEnvironment::default();
        for annotation in [
            contract(None, None),
            contract(Some(RegisterWidth::W8), None),
            contract(None, Some(RegisterWidth::W8)),
            contract(Some(RegisterWidth::W8), Some(RegisterWidth::W8)),
        ] {
            assert_eq!(environment.mismatches(annotation), [], "for {annotation:?}");
            for kind in [EntryKind::Main, EntryKind::Interrupt] {
                assert_eq!(
                    environment.body_state(Some(kind), annotation),
                    environment.state(),
                    "{kind:?} for {annotation:?}"
                );
            }
        }
    }
}
