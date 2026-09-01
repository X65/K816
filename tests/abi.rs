/*****************************************************************************/
/*                                                                           */
/*                                  abi.rs                                   */
/*                                                                           */
/*         The entry ABI: an annotation on `main` is an assertion            */
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

//! The entry ABI from the outside: which sections it answers for, and what
//! an annotation on one of them does.
//!
//! Two kinds, and one reason: **their caller is not a call site**. `main` is
//! entered by the startup code; a section that returns through `RTI` is
//! entered by the hardware. Every *other* function's contract is a
//! requirement on its callers, which K816 satisfies by bridging at each call
//! site — so an ordinary function may declare whatever widths it needs, and
//! these two may not.
//!
//! An annotation on one of them is a claim *about* the ABI rather than a
//! request of it. A true claim is allowed and costs nothing; a false one is
//! `K816-E0143`, because emitting a `REP` to make it true would be K816
//! choosing an ABI that belongs to the startup code and to whatever the
//! interrupt interrupted.
//!
//! The *default* environment is `examples/fullstack/startup.asm`'s — native
//! mode, `@a8`, `@i8` — and most of this file checks against it, because that
//! is what a build that says nothing gets. The value itself is
//! [`Options::entry`], which `--entry` states, so a build entered by
//! something other than that stub can say so and be checked against what it
//! said.

use k816::abi::EntryEnvironment;
use k816::diagnostic::DiagnosticCode;
use k816::isa::RegisterWidth;
use k816::source::{SourceMap, Span};
use k816::{Options, compile_to_ca65};

/// Compiles `source` for the default environment.
fn compile(source: &str) -> (Vec<k816::diagnostic::Diagnostic>, String) {
    compile_in(EntryEnvironment::default(), source)
}

/// Compiles `source` for `environment` and returns its diagnostics and its
/// assembly.
fn compile_in(
    environment: EntryEnvironment,
    source: &str,
) -> (Vec<k816::diagnostic::Diagnostic>, String) {
    let mut map = SourceMap::new();
    let file = map.add_file("entry.k816", source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            entry: environment,
            ..Default::default()
        },
    );
    (result.diagnostics, result.assembly.unwrap_or_default())
}

/// The error codes `source` reports for the default environment.
fn errors(source: &str) -> Vec<String> {
    errors_in(EntryEnvironment::default(), source)
}

/// The error codes `source` reports for `environment`.
fn errors_in(environment: EntryEnvironment, source: &str) -> Vec<String> {
    compile_in(environment, source)
        .0
        .iter()
        .filter(|diagnostic| diagnostic.is_error())
        .map(|diagnostic| diagnostic.code.as_str().to_owned())
        .collect()
}

/// The width setup K816 generated for the entry point, if any.
///
/// Found by its own annotation rather than by position: an operation
/// lowering generates for an entry contract carries `Reason::EntryPoint`,
/// which emission prints as "entry-point width setup". There should never be
/// one — that is what half of this file is about — so the helper exists to
/// say so rather than to describe one.
fn entry_setup(assembly: &str) -> Vec<String> {
    assembly
        .lines()
        .filter(|line| line.contains("entry-point width setup"))
        .filter_map(|line| line.split(';').next())
        .map(|line| line.trim().to_owned())
        .collect()
}

/// The annotations that contradict the environment `src/abi.rs` freezes.
///
/// Native mode, `@a8`, `@i8` — so every widening annotation on `main` is a
/// claim the environment does not satisfy.
const MISMATCHED: &[(&str, &str, usize)] = &[
    (
        "the accumulator, widened past what the environment provides",
        "func main @a16 {\n    nop\n}\n",
        1,
    ),
    (
        "the index registers, likewise",
        "func main @i16 {\n    nop\n}\n",
        1,
    ),
    (
        "one axis restating the ABI and the other contradicting it",
        "func main @a8 @i16 {\n    nop\n}\n",
        1,
    ),
    // Two false claims, two diagnostics: each annotation is separately
    // fixable, so each gets its own.
    (
        "both axes at once",
        "func main @a16 @i16 {\n    nop\n}\n",
        2,
    ),
];

#[test]
fn a_main_annotation_the_environment_does_not_satisfy_is_refused() {
    for (what, source, count) in MISMATCHED {
        assert_eq!(
            errors(source),
            vec!["K816-E0143".to_owned(); *count],
            "{what}: expected exactly the ABI mismatch"
        );
    }
}

/// The refusal names the axis, and only the axis that disagrees.
///
/// `@a8 @i16` is half right, and saying so is the difference between a
/// diagnostic an author can act on and one that says "your annotations are
/// wrong".
#[test]
fn the_refusal_names_the_axis_that_disagrees() {
    let (diagnostics, _) = compile("func main @a8 @i16 {\n    nop\n}\n");
    let refusal = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == DiagnosticCode::ENTRY_ABI_MISMATCH)
        .expect("the mismatch is reported");
    assert!(
        refusal.title.contains("index-register"),
        "the index axis is the one that disagrees: {}",
        refusal.title
    );
    assert!(
        refusal.primary.message.contains("@i8"),
        "and the label says what the environment does provide: {}",
        refusal.primary.message
    );

    // Both axes wrong is two diagnostics, not one that mentions two things:
    // each annotation is separately fixable.
    let (diagnostics, _) = compile("func main @a16 @i16 {\n    nop\n}\n");
    assert_eq!(
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.code == DiagnosticCode::ENTRY_ABI_MISMATCH)
            .count(),
        2
    );
}

/// The refusal points at the annotation, which is what the author can change.
#[test]
fn the_refusal_points_at_the_annotation_and_not_at_the_body() {
    let source = "func main @a16 {\n    nop\n}\n";
    let mut map = SourceMap::new();
    let file = map.add_file("entry.k816", source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let refusal = result
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == DiagnosticCode::ENTRY_ABI_MISMATCH)
        .expect("the mismatch is reported");
    let span = refusal.primary.span;
    assert_eq!(
        map.span_text(Span::new(span.file, span.start, span.end)),
        "@a16"
    );
}

/// A width `main` inherited from a module default is refused too, and says
/// where it came from.
///
/// A module default is file-wide and reaches `main` like any other function,
/// so it can make a claim about the ABI just as an annotation can. There is
/// no annotation on `main` to point at, so the caret goes to the name and a
/// note names the source.
#[test]
fn a_module_default_that_contradicts_the_abi_is_refused_where_it_lands() {
    let source = "@a16\nfunc main {\n    nop\n}\n";
    assert_eq!(errors(source), ["K816-E0143"]);

    let (diagnostics, _) = compile(source);
    let refusal = &diagnostics[0];
    assert!(
        refusal
            .notes
            .iter()
            .any(|note| note.contains("module-level default")),
        "the note names where the width came from: {refusal:?}"
    );
}

/// An annotation that restates the ABI is legal, and asks for nothing.
///
/// This is the shape the check must not break: `@a8 @i8` is exactly what the
/// environment provides, so there is nothing to refuse. Confusing "annotated"
/// with "disagreeing" would make every honest `main` an error.
#[test]
fn an_annotation_that_restates_the_abi_is_legal_and_emits_nothing() {
    for source in [
        "func main {\n    a = #$01\n    x = #$02\n}\n",
        "func main @a8 {\n    a = #$01\n    x = #$02\n}\n",
        "func main @i8 {\n    a = #$01\n    x = #$02\n}\n",
        "func main @a8 @i8 {\n    a = #$01\n    x = #$02\n}\n",
    ] {
        let (diagnostics, assembly) = compile(source);
        assert_eq!(
            diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.is_error())
                .count(),
            0,
            "`{source}` states the ABI truthfully:\n{assembly}"
        );
        assert_eq!(
            entry_setup(&assembly),
            Vec::<String>::new(),
            "and there is nothing to set up:\n{assembly}"
        );
    }
}

/// No entry point ever gets width setup, whatever it declared.
///
/// The other half of the assertion model, and the one with teeth: a compiler
/// that reports the mismatch and still emits the `REP` has kept the old
/// behaviour and added a complaint about it. Nothing can reach lowering with
/// a mismatched annotation — analysis refuses it first — so the claim is that
/// the generator is gone, not merely unreachable.
#[test]
fn an_entry_point_never_gets_generated_width_setup() {
    for source in [
        "func main {\n    nop\n}\n",
        "func main @a8 @i8 {\n    nop\n}\n",
        // A body that widens for itself is the documented way to get 16-bit
        // registers in `main`, and it is a scoped block rather than an entry
        // contract: the widening belongs to the body, not to the ABI.
        "func main @a8 {\n    @a16 {\n        a = #$0102\n    }\n}\n",
    ] {
        let (diagnostics, assembly) = compile(source);
        assert_eq!(
            diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.is_error())
                .count(),
            0,
            "`{source}`:\n{assembly}"
        );
        assert_eq!(
            entry_setup(&assembly),
            Vec::<String>::new(),
            "`{source}`:\n{assembly}"
        );
    }
}

/// Only `main` is checked against the ABI.
///
/// Every other function is reached by a call K816 can bridge, so a widening
/// annotation on one is an ordinary requirement rather than a claim about the
/// environment — and refusing those would make the ABI a default for every
/// function nothing else describes.
#[test]
fn an_ordinary_function_may_declare_whatever_widths_it_needs() {
    let (diagnostics, assembly) =
        compile("func wide @a16 @i16 {\n    a = #$0102\n}\n\nfunc main {\n    call wide\n}\n");
    assert_eq!(
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.is_error())
            .count(),
        0,
        "{assembly}"
    );
    // And the call site is where the widths get established, which is the
    // whole reason an ordinary contract may say anything it likes.
    assert!(
        assembly.contains("rep     #$20") && assembly.contains("rep     #$10"),
        "the bridge is at the call site:\n{assembly}"
    );
}

/// A section that returns through `RTI` takes the ABI's widths too.
///
/// It is entered by the hardware, so — exactly like `main` — there is no call
/// site to bridge from, and its widths are the environment's whether it
/// annotated them or not. Without this an interrupt handler could not size a
/// single immediate: nothing K816 can see establishes a width on the way in.
#[test]
fn an_interrupt_handler_takes_its_widths_from_the_environment() {
    for source in [
        // `return_i` is the K65 spelling.
        "naked handler {\n    a = #1\n    x = #2\n    return_i\n}\n",
        // The native one is the same instruction and the same conclusion.
        "naked handler {\n    a = #1\n    rti\n}\n",
        // A handler that returns from inside a conditional is still entered
        // by the hardware.
        "naked handler {\n    a?0\n    == { return_i }\n    return_i\n}\n",
        // And so is one whose return sits under a width block.
        "naked handler {\n    a = #1\n    @a16 {\n        a = #$0102\n    }\n    return_i\n}\n",
    ] {
        let (assembly, codes) = (compile(source).1, errors(source));
        assert_eq!(codes, Vec::<String>::new(), "`{source}`:\n{assembly}");
        assert_eq!(
            entry_setup(&assembly),
            Vec::<String>::new(),
            "and nothing is generated to establish them:\n{assembly}"
        );
    }

    // The widths really are the ABI's: an 8-bit immediate encodes in one
    // byte, and a two-byte value does not fit.
    let (_, assembly) = compile("naked handler {\n    a = #1\n    return_i\n}\n");
    assert!(assembly.contains("lda     #$01"), "{assembly}");
    let codes = errors("naked handler {\n    a = #$100\n    return_i\n}\n");
    assert_eq!(codes, ["K816-E0103"]);
}

/// And it is held to them, the same way `main` is.
#[test]
fn a_handler_annotation_the_environment_does_not_satisfy_is_refused() {
    let codes = errors("naked handler @a16 {\n    nop\n    return_i\n}\n");
    assert_eq!(codes, ["K816-E0143"]);

    let (diagnostics, _) = compile("naked handler @a16 {\n    nop\n    return_i\n}\n");
    let refusal = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == DiagnosticCode::ENTRY_ABI_MISMATCH)
        .expect("the mismatch is reported");
    // The secondary label says *why* this section is held to the ABI, which
    // is the part a reader of a `naked` section would not guess.
    assert!(
        refusal
            .secondary
            .iter()
            .any(|label| label.message.contains("an interrupt handler")),
        "the refusal says which kind of entry this is: {refusal:?}"
    );
    assert!(
        refusal
            .notes
            .iter()
            .any(|note| note.contains("entered by the hardware")),
        "and why it cannot be bridged: {refusal:?}"
    );

    // Restating the ABI is a true claim here too.
    let codes = errors("naked handler @a8 @i8 {\n    nop\n    return_i\n}\n");
    assert_eq!(codes, Vec::<String>::new());
}

/// Returning through `RTI` is what makes a section a handler — not `naked`,
/// and not its name.
///
/// `naked` says "no automatic `RTS`", which an interrupt handler needs and so
/// do other things; the *instruction* is the signal, because `RTI` exists to
/// pop the status an interrupt pushed. A `func` that returns through one is
/// treated the same way, and a `naked` section that does not is not treated
/// specially at all.
#[test]
fn the_rti_is_what_marks_a_handler_and_not_the_section_kind() {
    // `naked` without an `RTI`: an ordinary contract, freely declared.
    let codes = errors("naked chunk @a16 {\n    a = #$0102\n}\n");
    assert_eq!(codes, Vec::<String>::new());

    // A `func` with one: held to the ABI, because something entered it that
    // pushed a status byte.
    let codes = errors("func odd @a16 {\n    nop\n    rti\n}\n");
    assert_eq!(codes, ["K816-E0143"]);
}

/// The example in the tree obeys the ABI it runs in.
///
/// `examples/fullstack/main.k816` used to be `func main @a8 @i16` against a
/// stub that widens nothing. It is checked here as well as by the fullstack
/// image test, because that test would go on passing if the example were
/// changed to something that merely *links*.
#[test]
fn the_fullstack_example_states_the_environment_it_runs_in() {
    let source = include_str!("../examples/fullstack/main.k816");
    assert_eq!(errors(source), Vec::<String>::new());
    assert!(
        source.contains("func main @a8 {"),
        "the entry contract restates the ABI rather than asking to change it"
    );
}

/// The environment is a value the build states, and stating a different one
/// moves the check with it.
///
/// This is the whole of what J was after and could not do
/// (`plans/09-entry-abi.md` decision 1): `abi::environment()` used to be a
/// function returning a constant, so every one of the assertions above was
/// really an assertion about that constant. With `Options::entry` carrying
/// it, the *same* source is a true claim under one environment and `E0143`
/// under another, which is the only way to tell a check that reads the
/// environment from a check that repeats it.
#[test]
fn the_check_follows_the_environment_the_build_states() {
    let wide = EntryEnvironment::new(
        k816::op::EmulationState::Native,
        RegisterWidth::W16,
        RegisterWidth::W8,
    )
    .expect("a machine that exists");
    let source = "func main @a8 {\n    a = #$01\n    return\n}\n";

    // True under the startup stub's environment, false under one that hands
    // `main` a sixteen-bit accumulator.
    assert_eq!(errors(source), Vec::<String>::new());
    assert_eq!(errors_in(wide, source), ["K816-E0143"]);

    // And the reverse, so that neither answer is the one the compiler always
    // gives.
    let wide_source = "func main @a16 {\n    a = #$0102\n    return\n}\n";
    assert_eq!(errors(wide_source), ["K816-E0143"]);
    assert_eq!(errors_in(wide, wide_source), Vec::<String>::new());
}

/// An *unannotated* body takes its widths from the stated environment too,
/// not just the annotated one.
///
/// The distinction matters because the mismatch check reads the environment
/// through `mismatches` and the width walk reads it through `body_state`;
/// a build that threaded the value into one and not the other would pass the
/// test above and still size every immediate against the old constant.
#[test]
fn an_unannotated_entry_point_sizes_its_immediates_against_the_environment() {
    let wide = EntryEnvironment::new(
        k816::op::EmulationState::Native,
        RegisterWidth::W16,
        RegisterWidth::W8,
    )
    .expect("a machine that exists");
    let source = "func main {\n    a = #$0102\n    return\n}\n";

    // Eight-bit environment: the immediate does not fit the accumulator.
    assert_eq!(errors(source), ["K816-E0103"]);

    // Sixteen-bit environment: the same immediate is exactly right, and the
    // assembly claims the width the environment provided rather than one
    // K816 established.
    let (diagnostics, assembly) = compile_in(wide, source);
    assert!(
        !diagnostics
            .iter()
            .any(k816::diagnostic::Diagnostic::is_error),
        "{diagnostics:?}"
    );
    assert!(assembly.contains(".a16"), "{assembly}");
    assert_eq!(entry_setup(&assembly), Vec::<String>::new());
}

/// An emulation entry pins both axes at eight, and says so through the
/// ordinary mismatch check rather than through a rule of its own.
///
/// `--entry emulation` is not a third value of a mode axis that then leaves
/// the widths free: `doc/w65c816s.md` has M and X forced to one in emulation
/// mode, so the machine has no other widths to offer. A `@a16` on `main` is
/// therefore false in exactly the way `@a16` against a native 8-bit
/// environment is false, and it is the same `E0143`.
#[test]
fn an_emulation_entry_leaves_no_width_to_declare() {
    let emulation = EntryEnvironment::emulation();

    // Restating what emulation mode forces is a true claim, like any other.
    let restated = "func main @a8 @i8 {\n    a = #$01\n    return\n}\n";
    assert_eq!(errors_in(emulation, restated), Vec::<String>::new());

    // Claiming otherwise is the ordinary mismatch, on each axis.
    let wide = "func main @a16 @i16 {\n    a = #$0102\n    return\n}\n";
    assert_eq!(
        errors_in(emulation, wide),
        ["K816-E0143", "K816-E0143"],
        "one refusal per axis the environment does not provide"
    );
}

/// A `rep` establishes nothing in emulation mode, and the model says what it
/// *does* establish rather than giving up.
///
/// This is the one dataflow rule `EmulationState::Emulation` adds. Under
/// `Unknown` — after an `xce` — the width becomes unknown and the immediate
/// is unsizeable; under `Emulation` the width is known to still be eight, so
/// the immediate is refused for the reason that is actually true of the
/// machine, and an eight-bit one goes on compiling.
#[test]
fn a_rep_in_emulation_mode_leaves_the_width_where_it_was() {
    let emulation = EntryEnvironment::emulation();
    let source = "func main {\n    rep #$20\n    a = #$0102\n    return\n}\n";

    // Native: the `rep` widens and the immediate fits.
    assert_eq!(errors(source), Vec::<String>::new());

    // Emulation: the width is still known, and it is eight — so the
    // complaint is about the immediate's range, not about an unknown width.
    assert_eq!(errors_in(emulation, source), ["K816-E0103"]);

    // And the same body with an eight-bit immediate compiles, which is what
    // separates "the width is known to be eight" from "the width is unknown".
    let narrow = "func main {\n    rep #$20\n    a = #$01\n    return\n}\n";
    assert_eq!(errors_in(emulation, narrow), Vec::<String>::new());
}

/// An ordinary function under an emulation entry starts with emulation mode
/// *unknown*, not with the environment's answer.
///
/// Nothing propagates emulation mode across a call — no contract can state
/// it — so a callee's only ground is a whole-program guarantee. `Native` is
/// the one this compiler has always assumed and every program in the tree
/// rests on it; carrying `Emulation` in the same way would have a callee
/// believe a `rep` establishes eight bits, and a caller that had left
/// emulation mode would get a one-byte immediate for a sixteen-bit load.
/// Refusing to size it costs working code; sizing it wrong writes a wrong
/// byte.
#[test]
fn a_callee_under_an_emulation_entry_does_not_inherit_the_guarantee() {
    let emulation = EntryEnvironment::emulation();
    let source = "func helper {\n    rep #$20\n    a = #$0102\n    return\n}\n\
                  func main {\n    call helper\n    return\n}\n";

    // Native entry: the callee inherits it and the `rep` widens, exactly as
    // it did before `--entry` existed.
    assert_eq!(errors(source), Vec::<String>::new());

    // Emulation entry: unknown, so the immediate is refused rather than
    // sized against a guarantee no call site made.
    assert_eq!(errors_in(emulation, source), ["K816-E0104"]);

    // The entry point itself is not affected — nothing runs before it, so
    // there is no caller to have exchanged anything.
    let inline_body = "func main {\n    rep #$20\n    a = #$01\n    return\n}\n";
    assert_eq!(errors_in(emulation, inline_body), Vec::<String>::new());
}

/// In emulation mode nothing but an `xce` can change a register width — not
/// even the instructions the effect table gives up on.
///
/// `RTI` and `PLP` restore a status byte K816 cannot read, so under `Native`
/// they make both widths unknown and every immediate below them unsizeable.
/// Under `Emulation` there is nothing to be unsure about: M and X are forced
/// to one, so whatever byte comes back, both registers are still eight bits.
/// This is the second half of the same rule `believe` is the first half of,
/// and without it `--entry emulation` would report an unknown width at every
/// `plp` in a program whose widths are pinned by the machine.
#[test]
fn a_restored_status_byte_cannot_widen_a_register_in_emulation_mode() {
    let emulation = EntryEnvironment::emulation();
    let source = "func main {\n    php\n    plp\n    a = #$01\n    return\n}\n";

    // Native: the status byte could have said anything, so the width below
    // it is unknown and the immediate cannot be sized.
    assert_eq!(errors(source), ["K816-E0104"]);

    // Emulation: still eight bits, and the immediate compiles.
    assert_eq!(errors_in(emulation, source), Vec::<String>::new());

    // `xce` is the exception, and it is the exception in *both*: it decides
    // the emulation flag from the carry, so nothing below it is known.
    let exchanged = "func main {\n    xce\n    a = #$01\n    return\n}\n";
    assert_eq!(errors_in(emulation, exchanged), ["K816-E0104"]);
    assert_eq!(errors(exchanged), ["K816-E0104"]);
}

/// What ca65 is told and what the model believes are one answer, in emulation
/// mode too.
///
/// This is the half a source-level test cannot see. The width model reads an
/// instruction's effect through the emulation state; emission used to read it
/// straight off the effect table, so a `rep #$20` under `--entry emulation`
/// made the model say "still 8-bit" and made the assembly say `.a16`. ca65
/// sizes immediates from the directive, so the next `lda #` would have been
/// assembled two bytes wide for a register the CPU is running at one — and
/// the byte after it read as the operand's high half.
///
/// Both are checked, because the claim is that they *agree*: the same source
/// keeps its `.a16` under the default environment.
#[test]
fn emission_reads_a_width_effect_through_the_same_emulation_state_the_model_does() {
    let emulation = EntryEnvironment::emulation();
    let source = "func main {\n    rep #$20\n    a = #$01\n    return\n}\n";

    let (_, assembly) = compile_in(emulation, source);
    assert!(
        !assembly.contains(".a16"),
        "the `rep` establishes nothing on this machine: {assembly}"
    );
    assert!(assembly.contains("lda     #$01"), "{assembly}");

    // Under the default the `rep` really does widen, and the directive says
    // so — which is what makes the assertion above about `emulation`.
    let (_, assembly) = compile_in(EntryEnvironment::default(), source);
    assert!(assembly.contains(".a16"), "{assembly}");

    // `PLP` restores a status byte, and in emulation mode no status byte can
    // widen a register — so the assembly says nothing about an unknown width
    // where the default's assembly has to.
    let restoring = "func main {\n    php\n    plp\n    a = #$01\n    return\n}\n";
    let (_, assembly) = compile_in(emulation, restoring);
    assert!(
        !assembly.contains("width is unknown from here"),
        "{assembly}"
    );
}

// ---------------------------------------------------------------------------
// Sixteen bits where emulation mode is known (K816-E0163)
// ---------------------------------------------------------------------------

/// Every route to a widening `REP` K816 authors itself is refused where the
/// machine is known to be in emulation mode.
///
/// The instruction assembles and does nothing there — M and X are forced to
/// one — so the bridge succeeds on paper and the code below it is assembled
/// sixteen bits wide while the CPU runs it at eight, reading the next opcode
/// as operand data. There are five ways to reach a bridge and they are
/// checked in one place, so this asserts the ones a source can spell.
#[test]
fn a_widening_bridge_is_refused_where_emulation_mode_is_known() {
    let emulation = EntryEnvironment::emulation();

    // A scoped block: the `rep` is generated to enter it.
    let scoped = "func main {\n    @a16 {\n        a = #$1234\n    }\n    return\n}\n";
    assert_eq!(errors_in(emulation, scoped), ["K816-E0163"]);
    assert_eq!(errors(scoped), Vec::<String>::new());

    // A managed call to a wide callee: the `rep` is the call-site bridge.
    // The callee itself is legal — it may be called from native code — so it
    // is the *call* that is refused, and only under this environment.
    let bridged = "func helper @a16 {\n    a = #$1234\n    return\n}\n\
                   func main {\n    call helper\n    return\n}\n";
    assert_eq!(errors_in(emulation, bridged), ["K816-E0163"]);
    assert_eq!(errors(bridged), Vec::<String>::new());

    // The index axis reaches it too, and both axes at once are two refusals.
    let both = "func main {\n    @a16 @i16 {\n        nop\n    }\n    return\n}\n";
    assert_eq!(errors_in(emulation, both), ["K816-E0163", "K816-E0163"]);

    // Narrowing is never refused: eight bits is what the machine already is.
    let narrow = "func main {\n    @a8 {\n        a = #$01\n    }\n    return\n}\n";
    assert_eq!(errors_in(emulation, narrow), Vec::<String>::new());
}

/// After an `xce` the refusal lifts, because the flag is no longer known.
///
/// This is the line between `K816-E0163` and the escape hatch: `@a16` where
/// emulation is *unknown* is the documented assertion for exactly the
/// `xce`-then-widen pattern, and it must keep working. Only a *known*
/// emulation state contradicts it.
#[test]
fn a_sixteen_bit_assertion_is_still_the_escape_hatch_once_the_flag_is_unknown() {
    let emulation = EntryEnvironment::emulation();
    let source = "func main {\n    clc\n    xce\n    @a16 {\n        a = #$1234\n    }\n    \
                  return\n}\n";
    assert_eq!(errors_in(emulation, source), Vec::<String>::new());

    // And an ordinary function under the same build, which starts with the
    // flag unknown for the same reason.
    let callee = "func wide {\n    @a16 {\n        a = #$1234\n    }\n    return\n}\n\
                  func main {\n    call wide\n    return\n}\n";
    assert_eq!(errors_in(emulation, callee), Vec::<String>::new());
}

/// A known emulation state is not upgraded by an annotation that contradicts
/// it.
///
/// `@a16` used to set the tracked emulation flag to `Native` whenever it
/// appeared — sound where the flag is unknown, since the author knows
/// something the compiler does not, and wrong where the *build* has stated
/// the machine. Without this the refusal above would fire once and then every
/// `rep` below it would be believed, which is the miscompile arriving one
/// statement later.
#[test]
fn an_annotation_does_not_overrule_what_the_build_stated_about_the_machine() {
    let emulation = EntryEnvironment::emulation();
    let source = "func main {\n    @a16 {\n        nop\n    }\n    rep #$20\n    \
                  a = #$1234\n    return\n}\n";
    let codes = errors_in(emulation, source);
    assert!(
        codes.contains(&"K816-E0163".to_owned()),
        "the block is refused: {codes:?}"
    );
    assert!(
        codes.contains(&"K816-E0103".to_owned()),
        "and the `rep` below it still establishes nothing: {codes:?}"
    );
}

/// Emulation mode does not survive a call, because no contract can carry it.
///
/// A callee may run `clc; xce`, satisfy its declared *width* exit contract,
/// and return onto a machine that has left emulation mode. The caller cannot
/// see that — a contract describes widths only — so believing `Emulation`
/// afterwards would size an immediate at one byte for a register the callee
/// had just widened, and the CPU would eat the following opcode.
///
/// `Native` deliberately does survive: every program in the tree rests on
/// that assumption and it is argued in `abi::callee_emulation`. The point
/// here is that the *new* state does not inherit it.
#[test]
fn emulation_mode_does_not_survive_a_callee_that_exchanges_the_flag() {
    let emulation = EntryEnvironment::emulation();
    // `leave` really does change the machine, and its width contract is
    // still satisfiable, so nothing else refuses this.
    let source = "func leave -> @a8 @i8 {\n    clc\n    xce\n    sep #$30\n    return\n}\n\
                  func main {\n    call leave\n    rep #$20\n    a = #$01\n    return\n}\n";
    let codes = errors_in(emulation, source);
    assert!(
        codes.contains(&"K816-E0104".to_owned()),
        "the `rep` after the call establishes nothing, so the immediate is \
         unsizeable rather than sized at one byte: {codes:?}"
    );

    // Before the call the state is still known, so the same `rep` is a
    // no-op with an exact answer and the eight-bit immediate compiles.
    let before = "func main {\n    rep #$20\n    a = #$01\n    return\n}\n";
    assert_eq!(errors_in(emulation, before), Vec::<String>::new());
}

/// And it survives a callee that leaves the flag alone.
///
/// G11 invalidated `Emulation` at *every* call, because a contract described
/// widths and nothing else, so any callee might have run an `xce`. The exit
/// contract has an emulation axis now, so a callee that does not touch the
/// flag classifies as `EmulationExit::Preserve` and the caller keeps what it
/// knew — which is the difference between a rule and a blanket.
#[test]
fn emulation_mode_survives_a_callee_that_leaves_the_flag_alone() {
    let emulation = EntryEnvironment::emulation();
    let source = "func quiet {\n    nop\n    return\n}\n\
                  func main {\n    call quiet\n    rep #$20\n    a = #$01\n    return\n}\n";
    assert_eq!(
        errors_in(emulation, source),
        Vec::<String>::new(),
        "the `rep` is still a hardware no-op with an exact answer, so the \
         eight-bit immediate is sizeable"
    );
}

/// The other half of the same hole: a callee that leaves *native* mode.
///
/// This is `plans/09-entry-abi.md`'s probe 2, and until the exit contract
/// could describe the flag it was a miscompile rather than a diagnostic.
/// `flips` runs `sec; xce`, so the machine is emulating when it returns; the
/// caller's `rep #$20` is then a no-op, and the accumulator the `lda`
/// believes is sixteen bits wide is eight. K816 emitted `.a16` and a
/// three-byte immediate for it.
///
/// The width axes cannot catch this on their own: `xce` makes them
/// `Unknown`, and `AxisExit::Unknown` is defined to *preserve* the caller's
/// widths — the language rule that keeps an unclassifiable callee usable. It
/// is the emulation axis that has to say something, and it says `Unknown`.
#[test]
fn a_callee_that_leaves_native_mode_is_not_believed_afterwards() {
    let source = "func flips @a8 @i8 {\n    sec\n    xce\n}\n\
                  func caller @a8 @i8 {\n    call flips\n    rep #$20\n    a = #$1234\n}\n";
    let codes = errors(source);
    assert!(
        codes.contains(&"K816-E0104".to_owned()),
        "the widening `rep` after `flips` establishes nothing: {codes:?}"
    );
}

/// A callee nobody classified keeps the older assumption, in one direction.
///
/// A recursive function has no exit contract to compute — the classification
/// would be its own input — so it takes `ExitContract::preserving()`, whose
/// emulation axis is `Assumed`. `Native` survives that and `Emulation` does
/// not: every program in the tree rests on a callee returning onto the
/// machine it was called from, and invalidating `Native` would make every
/// `rep` after every call unsizeable, while `Emulation` is newer and
/// inherits no such debt.
#[test]
fn an_unclassified_callee_keeps_native_and_drops_emulation() {
    let recursive = "func loop @a8 @i8 {\n    < goto .done\n    call loop\n.done:\n    return\n}\n\
                     func caller @a8 @i8 {\n    call loop\n    rep #$20\n    a = #$1234\n}\n";
    assert_eq!(
        errors(recursive),
        Vec::<String>::new(),
        "under a native entry the caller may still believe its own `rep`"
    );

    let emulating = "func loop {\n    < goto .done\n    call loop\n.done:\n    return\n}\n\
                     func main {\n    call loop\n    rep #$20\n    a = #$01\n    return\n}\n";
    let codes = errors_in(EntryEnvironment::emulation(), emulating);
    assert!(
        codes.contains(&"K816-E0104".to_owned()),
        "and under an emulation entry it may not: {codes:?}"
    );
}

/// An environment that describes no machine cannot be built, by any route.
///
/// `--entry` refused the spelling, and a library caller assembling the struct
/// went straight past that check — so the fields are private and `new` is the
/// one door. Everything downstream trusts the value, which is only safe if
/// there is no way to hand it a false one.
#[test]
fn an_environment_that_describes_no_machine_cannot_be_constructed() {
    use k816::op::EmulationState;

    for (e, a, i) in [
        (
            EmulationState::Emulation,
            RegisterWidth::W16,
            RegisterWidth::W8,
        ),
        (
            EmulationState::Emulation,
            RegisterWidth::W8,
            RegisterWidth::W16,
        ),
        (
            EmulationState::Emulation,
            RegisterWidth::W16,
            RegisterWidth::W16,
        ),
        // "I do not know" is not a statement about the machine, and an entry
        // environment is nothing but a statement about the machine.
        (
            EmulationState::Unknown,
            RegisterWidth::W8,
            RegisterWidth::W8,
        ),
    ] {
        assert!(
            EntryEnvironment::new(e, a, i).is_err(),
            "{e:?} {a:?} {i:?} was accepted"
        );
    }

    // The four that do describe a machine are built without complaint, and
    // the emulation one has the widths the machine forces.
    for (a, i) in [
        (RegisterWidth::W8, RegisterWidth::W8),
        (RegisterWidth::W8, RegisterWidth::W16),
        (RegisterWidth::W16, RegisterWidth::W8),
        (RegisterWidth::W16, RegisterWidth::W16),
    ] {
        assert!(EntryEnvironment::new(EmulationState::Native, a, i).is_ok());
    }
    let emulation = EntryEnvironment::emulation();
    assert_eq!(emulation.e(), EmulationState::Emulation);
    assert_eq!(emulation.a(), RegisterWidth::W8);
    assert_eq!(emulation.i(), RegisterWidth::W8);
}
