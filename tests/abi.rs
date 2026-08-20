/*****************************************************************************/
/*                                                                           */
/*                                  abi.rs                                   */
/*                                                                           */
/*        The entry ABI: what holds today, and what sub-plan J owes          */
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

//! The entry ABI from the outside: what an annotation on `main` does.
//!
//! Two halves, and they are deliberately in one file so that neither can be
//! changed without seeing the other.
//!
//! The **passing** tests pin what K816 does today: an annotation on `main` is
//! a *request*, satisfied with entry setup that widens the axis before the
//! body runs, and the environment answers whichever axes the annotation left
//! unstated.
//!
//! The **ignored** tests pin what sub-plan J is for. Under the assertion
//! model an annotation on `main` is a claim *about* the environment rather
//! than a request of it: `main`'s caller is the environment, an environment
//! cannot be bridged, so an annotation the environment does not satisfy is a
//! diagnostic and there is no setup to emit. They fail today, on purpose —
//! run them with `cargo test --test abi -- --ignored` to see the gap, and
//! delete the `#[ignore]` when J lands.
//!
//! They deliberately do **not** name a diagnostic code. `postmortem.md`
//! §E.1.14's rule is that a plan reserves nothing and the constant in
//! `diagnostic.rs` is the allocation; J's number has already moved four times
//! (`E0138` → `E0139` → `E0141` → `E0142` → `E0143`) while E tasks took each
//! one in turn. What these assert is the *shape* — refused, pointed at the
//! annotation, and no entry setup — which is what J actually owes and what no
//! renumbering can invalidate.

use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

/// Compiles `source` and returns its error codes and its assembly.
fn compile(source: &str) -> (Vec<String>, String) {
    let mut map = SourceMap::new();
    let file = map.add_file("entry.k816", source);
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    let errors = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.is_error())
        .map(|diagnostic| diagnostic.code.as_str().to_owned())
        .collect();
    (errors, result.assembly.unwrap_or_default())
}

/// The width setup K816 generated for the entry point, if any.
///
/// Found by its own annotation rather than by position: the operations
/// lowering generates for an entry contract carry `Reason::EntryPoint`, which
/// emission prints as "entry-point width setup". Scanning between `main:` and
/// the body would also pick up the debug directives and the mode claims,
/// neither of which is what this is asking about.
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
/// claim the environment does not satisfy. The last is not a hypothetical:
/// `examples/fullstack/main.k816` is written this way.
const MISMATCHED: &[(&str, &str)] = &[
    (
        "the accumulator, widened past what the environment provides",
        "func main @a16 {\n    a = #$0102\n}\n",
    ),
    (
        "the index registers, likewise",
        "func main @i16 {\n    x = #$0102\n}\n",
    ),
    (
        "one axis restating the ABI and the other contradicting it",
        "func main @a8 @i16 {\n    a = #$01\n    x = #$0102\n}\n",
    ),
];

/// An annotation that restates the ABI is not a mismatch, and asks for
/// nothing.
///
/// This is the one shape that has to behave identically before and after J:
/// `@a8 @i8` is exactly what the environment provides, so there is nothing to
/// refuse and nothing to set up. If J ever makes *this* an error it has
/// confused "annotated" with "disagreeing".
#[test]
fn an_annotation_that_restates_the_abi_is_legal_and_emits_nothing() {
    let (errors, assembly) = compile("func main @a8 @i8 {\n    a = #$01\n    x = #$02\n}\n");
    assert_eq!(errors, Vec::<String>::new(), "{assembly}");
    assert_eq!(
        entry_setup(&assembly),
        Vec::<String>::new(),
        "the environment already provides both axes:\n{assembly}"
    );
}

/// Today: an annotation on `main` is a request, and K816 generates the setup
/// that satisfies it.
///
/// Pinned so that the change J makes is visible as a change *here*, not as a
/// silent difference in emitted bytes. The paired expectation is
/// `a_main_annotation_the_environment_does_not_satisfy_is_refused` below,
/// which is ignored until J lands.
#[test]
fn today_a_mismatched_annotation_is_satisfied_with_entry_setup() {
    for (what, source) in MISMATCHED {
        let (errors, assembly) = compile(source);
        assert_eq!(errors, Vec::<String>::new(), "{what}:\n{assembly}");
        assert!(
            !entry_setup(&assembly).is_empty(),
            "{what}: expected entry setup that widens the axis:\n{assembly}"
        );
    }

    // The axis the annotation did not mention still comes from the ABI, which
    // is what makes `@a16` alone emit one instruction rather than two.
    let (_, assembly) = compile("func main @a16 {\n    a = #$0102\n}\n");
    assert_eq!(entry_setup(&assembly), ["rep     #$20"], "{assembly}");
}

/// **Fails until sub-plan J.** An annotation the environment does not satisfy
/// is a diagnostic.
///
/// `main`'s caller is the environment. Every other function's contract is a
/// requirement on its callers that K816 bridges at each call site; the
/// environment cannot be bridged, so the requirement stays and the bridge has
/// to go. A `func main @a16` against an 8-bit environment is therefore a
/// claim that is simply false, and generating a `REP` to make it true is
/// K816 deciding an ABI it does not own.
#[test]
#[ignore = "sub-plan J: an annotation on `main` is still a request, not an assertion"]
fn a_main_annotation_the_environment_does_not_satisfy_is_refused() {
    for (what, source) in MISMATCHED {
        let (errors, assembly) = compile(source);
        assert!(
            !errors.is_empty(),
            "{what}: expected a diagnostic, got a clean compile:\n{assembly}"
        );
    }
}

/// **Fails until sub-plan J.** A refused annotation generates no setup.
///
/// The two halves are separate tests because they can fail separately, and
/// the second is the one with teeth: a compiler that reports the mismatch and
/// still emits the `REP` has kept the behaviour and added a complaint about
/// it.
#[test]
#[ignore = "sub-plan J: the entry setup goes when the annotation becomes an assertion"]
fn a_refused_annotation_leaves_no_entry_setup_behind() {
    for (what, source) in MISMATCHED {
        let (_, assembly) = compile(source);
        assert_eq!(
            entry_setup(&assembly),
            Vec::<String>::new(),
            "{what}: the setup is still here:\n{assembly}"
        );
    }
}

/// **Fails until sub-plan J.** The diagnostic points at the annotation.
///
/// Where it points is what makes it actionable: the annotation is the thing
/// the author can change, and the environment is not. Sub-plan J's plan also
/// wants the *help* to name the state the environment does provide, which
/// this does not assert — a message that has not been written yet is not a
/// contract, and over-specifying one is how a test becomes something to edit
/// rather than something to satisfy.
#[test]
#[ignore = "sub-plan J: there is no diagnostic to point anywhere yet"]
fn the_refusal_points_at_the_annotation_and_not_at_the_body() {
    let source = "func main @a16 {\n    a = #$0102\n}\n";
    let mut map = SourceMap::new();
    let file = map.add_file("entry.k816", source);
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    let refusal = result
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.is_error())
        .expect("a diagnostic");
    let span = refusal.primary.span;
    assert_eq!(
        map.span_text(k816::source::Span::new(span.file, span.start, span.end)),
        "@a16",
        "the annotation is what the author can change"
    );
}

/// **Fails until sub-plan J.** The example in the tree is one of these.
///
/// `examples/fullstack/main.k816` is `func main @a8 @i16`, and the fullstack
/// test links and runs it — so this is not a hypothetical shape, it is the
/// one program in the repository that J breaks. When J lands, that file moves
/// its widening into the body (`@i16 { … }`, or an explicit `rep #$10`)
/// rather than into the stub, which would make 16-bit index registers
/// everyone's ABI in order to satisfy one program.
#[test]
#[ignore = "sub-plan J: the fullstack example still compiles because a request is still a request"]
fn the_fullstack_example_contradicts_the_environment_it_runs_in() {
    let (errors, assembly) = compile(include_str!("../examples/fullstack/main.k816"));
    assert!(
        !errors.is_empty(),
        "`@i16` against an 8-bit environment is a claim nothing satisfies:\n{assembly}"
    );
}
