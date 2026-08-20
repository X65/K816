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
//! The *value* is frozen: [`EntryEnvironment::STARTUP_STUB`], taken from the
//! repository rather than from the hardware. `examples/fullstack/startup.asm`
//! does `sei; clc; xce; jsr main`, so `main` is called by a stub that has
//! already left emulation mode and has widened neither axis. The entry ABI is
//! a contract between the startup code and the compiler, so the ABI is what
//! the startup code does — not what the CPU does at reset.
//!
//! What is *not* here is what makes the value a **choice**: no `--entry`, no
//! target names, no configuration file. Sub-plan J (`plans/09-entry-abi.md`)
//! owns those, and until it lands [`environment`] answers with the one
//! environment K816 knows. The seam is deliberately this narrow: committing
//! to a way of *supplying* an ABI before the configuration it has to agree
//! with exists is how two ways of saying one thing end up disagreeing.
//!
//! # What an annotation on `main` means
//!
//! It is an **assertion**, not a request. Every other function's contract is
//! a requirement on its callers, which K816 satisfies by bridging at each
//! call site; `main`'s caller is the environment, and an environment cannot
//! be bridged. So an annotation on `main` is a claim *about* the ABI: a true
//! one is allowed and costs nothing, and a false one is
//! [`crate::diagnostic::DiagnosticCode::ENTRY_ABI_MISMATCH`]. No width setup
//! is generated for an entry point at all — emitting a `REP` to make the
//! claim true would be K816 choosing an ABI that belongs to the startup
//! code.

use crate::isa::RegisterWidth;
use crate::op::{AxisState, EmulationState, ModeState, WidthContract};
use crate::width::Axis;

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
}

/// The environment this build targets.
///
/// Hardcoded until sub-plan J gives the build a say. Every pass that needs
/// the entry state calls this, so J replaces one function body rather than
/// hunting for the passes that each decided for themselves.
pub fn environment() -> EntryEnvironment {
    EntryEnvironment::STARTUP_STUB
}

/// The width state a function body starts in.
///
/// The single answer to "what holds when control arrives here", for both of
/// the passes that ask: the width solver, which walks forward from it, and
/// lowering, which claims it to the assembler. An ordinary function's answer
/// is its contract — a caller that does not satisfy it is bridged at the call
/// site — and the entry point's is the environment's, since its caller is the
/// environment and cannot be bridged.
pub fn entry_state(is_entry_point: bool, contract: WidthContract) -> ModeState {
    if is_entry_point {
        environment().entry_state(contract)
    } else {
        ModeState::from_contract(contract)
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
        let environment = environment();
        assert_eq!(environment, EntryEnvironment::STARTUP_STUB);

        // The case that makes a legacy `main` compile at all: no annotation,
        // and the widths are the environment's rather than unknown.
        let unannotated = entry_state(true, contract(None, None));
        assert_eq!(unannotated, environment.state());
        assert_eq!(unannotated.a, AxisState::Known(RegisterWidth::W8));
        assert_eq!(unannotated.i, AxisState::Known(RegisterWidth::W8));

        // A mismatched annotation is refused before this is asked, and the
        // state it produces is what the author believed — so that the body's
        // own diagnostics are about the body rather than about the ABI.
        let refused = entry_state(true, contract(Some(RegisterWidth::W16), None));
        assert_eq!(refused.a, AxisState::Known(RegisterWidth::W16));
        assert_eq!(refused.i, AxisState::Known(RegisterWidth::W8));
    }

    #[test]
    fn an_ordinary_function_is_answered_by_its_contract_and_not_by_the_abi() {
        // Its caller can be bridged, so an unstated axis is unknown and stays
        // that way — taking the entry ABI here would be inventing a width no
        // caller promised.
        let unannotated = entry_state(false, contract(None, None));
        assert_eq!(unannotated.a, AxisState::Unknown);
        assert_eq!(unannotated.i, AxisState::Unknown);

        let annotated = entry_state(false, contract(Some(RegisterWidth::W16), None));
        assert_eq!(annotated.a, AxisState::Known(RegisterWidth::W16));
        assert_eq!(annotated.i, AxisState::Unknown);
    }

    #[test]
    fn only_a_contradicting_annotation_is_a_mismatch() {
        let environment = environment();
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

    /// The seam `postmortem.md` §E.2.1 says was missing: the state lowering
    /// claims and the state the solver walks from are one value.
    ///
    /// Under the assertion model there is nothing left to reconcile — an
    /// entry point starts at the environment's widths and no instruction is
    /// generated to change them — so the claim is simply that a *legal*
    /// entry contract states the environment and nothing else.
    #[test]
    fn a_legal_entry_contract_is_the_environment_itself() {
        let environment = environment();
        for annotation in [
            contract(None, None),
            contract(Some(RegisterWidth::W8), None),
            contract(None, Some(RegisterWidth::W8)),
            contract(Some(RegisterWidth::W8), Some(RegisterWidth::W8)),
        ] {
            assert_eq!(environment.mismatches(annotation), [], "for {annotation:?}");
            assert_eq!(
                entry_state(true, annotation),
                environment.state(),
                "for {annotation:?}"
            );
        }
    }
}
