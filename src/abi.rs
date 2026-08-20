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
//! What is *not* here is everything that makes the value a choice: no
//! `--entry`, no target names, no configuration file, and no assertion model.
//! Sub-plan J (`plans/09-entry-abi.md`) owns those, and until it lands
//! [`environment`] answers with the one environment K816 knows. The seam is
//! deliberately this narrow: committing to a way of *supplying* an ABI before
//! the configuration it has to agree with exists is how two ways of saying
//! one thing end up disagreeing.
//!
//! # What an annotation on `main` means today
//!
//! Today it is a *request*: `func main @a16` still gets the entry setup that
//! widens the accumulator, and the environment supplies only the axes the
//! annotation left unstated. Under J it becomes an *assertion* — a mismatch
//! is a diagnostic, and the setup goes, because `main`'s caller is the
//! environment and an environment cannot be bridged. Changing that is J's
//! job, and the one thing this module makes sure of is that when J changes
//! it, there is a single place to change.

use crate::isa::RegisterWidth;
use crate::op::{AxisState, EmulationState, ModeState, WidthContract};

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
    /// An annotation wins where there is one, because today it is a request
    /// K816 satisfies with entry setup; the environment answers the axes that
    /// said nothing, which is what lets an unannotated `main` size an
    /// immediate at all. Under J the precedence goes the other way and the
    /// difference becomes a diagnostic — see the module documentation.
    pub fn entry_state(self, contract: WidthContract) -> ModeState {
        ModeState {
            a: AxisState::Known(contract.a.unwrap_or(self.a)),
            i: AxisState::Known(contract.i.unwrap_or(self.i)),
            e: self.e,
        }
    }

    /// The setup an entry point needs to reach `contract` from here, per
    /// axis: the width to establish, or `None` where the environment already
    /// provides it.
    ///
    /// Lowering emits from this rather than from "a `SEP` at entry would be
    /// redundant", so that the instruction it emits and the state the model
    /// assumes are derived from one value.
    pub fn setup(self, contract: WidthContract) -> [Option<RegisterWidth>; 2] {
        [
            entry_setup(self.a, contract.a),
            entry_setup(self.i, contract.i),
        ]
    }
}

/// What one axis has to be set to, or `None` when it is already right.
fn entry_setup(provided: RegisterWidth, requested: Option<RegisterWidth>) -> Option<RegisterWidth> {
    match requested {
        Some(requested) if requested != provided => Some(requested),
        _ => None,
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

        // An annotation is a request today, so it wins over the environment,
        // and the axis beside it still comes from the environment.
        let widened = entry_state(true, contract(Some(RegisterWidth::W16), None));
        assert_eq!(widened.a, AxisState::Known(RegisterWidth::W16));
        assert_eq!(widened.i, AxisState::Known(RegisterWidth::W8));
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
    fn setup_is_what_the_environment_does_not_already_provide() {
        let environment = environment();
        // Nothing to do for the widths the environment hands over.
        assert_eq!(environment.setup(contract(None, None)), [None, None]);
        assert_eq!(
            environment.setup(contract(Some(RegisterWidth::W8), Some(RegisterWidth::W8))),
            [None, None],
            "an annotation that restates the ABI asks for no instructions"
        );
        // And exactly the axes that differ.
        assert_eq!(
            environment.setup(contract(Some(RegisterWidth::W16), None)),
            [Some(RegisterWidth::W16), None]
        );
    }

    /// The seam `postmortem.md` §E.2.1 says was missing: the state lowering
    /// claims and the state the solver walks from are one value, so they
    /// cannot be made to disagree by editing one of them.
    #[test]
    fn what_lowering_claims_and_what_the_solver_assumes_is_one_value() {
        for annotation in [
            contract(None, None),
            contract(Some(RegisterWidth::W16), None),
            contract(Some(RegisterWidth::W8), Some(RegisterWidth::W16)),
        ] {
            let environment = environment();
            let claimed = entry_state(true, annotation);
            // Whatever setup lowering emits, applying it to the environment
            // has to land on the state the model assumes.
            let [a, i] = environment.setup(annotation);
            let reached = ModeState {
                a: AxisState::Known(a.unwrap_or(environment.a)),
                i: AxisState::Known(i.unwrap_or(environment.i)),
                e: environment.e,
            };
            assert_eq!(reached, claimed, "for {annotation:?}");
        }
    }
}
