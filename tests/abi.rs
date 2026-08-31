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
    let wide = EntryEnvironment {
        a: RegisterWidth::W16,
        ..EntryEnvironment::default()
    };
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
    let wide = EntryEnvironment {
        a: RegisterWidth::W16,
        ..EntryEnvironment::default()
    };
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
