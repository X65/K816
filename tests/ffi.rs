/*****************************************************************************/
/*                                                                           */
/*                                  ffi.rs                                   */
/*                                                                           */
/*         Naming a symbol that some other language's object provides        */
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

//! Sub-plan K's foreign-symbol declarations: `extern`.
//!
//! The whole construct rests on a promise nothing checks. Every other
//! contract in this compiler is *derived* — `classify_exit` walks a body's
//! reachable returns and reports what it finds — and an `extern` has no body,
//! which is exactly what it is for. So the tests here divide in two: the ones
//! that check the declaration is *carried* faithfully into the bytes, and the
//! ones that check the compiler refuses a program whose declarations
//! contradict each other or the program itself. There is no third kind, and
//! there cannot be: whether the foreign object agrees is a question only
//! running the program answers.

use k816::abi::EntryEnvironment;
use k816::linkage::Linkage;
use k816::source::{FileId, SourceMap};

/// Compiles `text` alone and returns the assembly, requiring no errors.
fn assembly(text: &str) -> String {
    let mut map = SourceMap::new();
    let file = map.add_file("ffi.k816", text);
    let result = k816::compile_to_ca65(&map, file, &k816::Options::default());
    let errors: Vec<&str> = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.is_error())
        .map(|diagnostic| diagnostic.code.0)
        .collect();
    assert_eq!(errors, Vec::<&str>::new(), "{:?}", result.diagnostics);
    result.assembly.expect("compiles")
}

/// The diagnostic codes compiling `file` out of `map` produces.
fn codes_for(map: &SourceMap, file: FileId) -> Vec<&'static str> {
    k816::compile_to_ca65(map, file, &k816::Options::default())
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.0)
        .collect()
}

/// The instruction lines of `assembly`, mnemonic and operand only.
fn instructions(assembly: &str) -> Vec<String> {
    assembly
        .lines()
        .filter(|line| line.starts_with("        ") && !line.trim_start().starts_with('.'))
        .map(|line| {
            line.split(';')
                .next()
                .unwrap_or(line)
                .split_whitespace()
                .collect::<Vec<_>>()
                .join(" ")
        })
        .filter(|line| !line.is_empty())
        .collect()
}

// ---------------------------------------------------------------------------
// What the declaration carries
// ---------------------------------------------------------------------------

/// Every `extern` shape emits the `.import` spelling ca65 needs, and
/// reserves nothing.
///
/// The address size is the whole of what crosses a module boundary in this
/// toolchain: ca65 validates addressing modes against an imported symbol's,
/// so a `dp` variable imported as absolute is a wrong *instruction* here
/// rather than a wrong name. And an extern reserves no storage and emits no
/// equate — what it is worth is ld65's answer and nobody else's.
#[test]
fn every_extern_shape_imports_with_its_address_size_and_reserves_nothing() {
    let text = "extern func _puts\n\
                extern far func rom_entry\n\
                extern var _count\n\
                extern var dp ptr\n\
                extern var far rom\n\
                extern var abs port : word\n\
                \n\
                func main @a8 @i8 {\n\
                \x20   _count = a\n\
                \x20   a = ptr\n\
                \x20   call _puts\n\
                \x20   call far rom_entry\n\
                \x20   a = far rom\n\
                \x20   rts\n\
                }\n";
    let assembly = assembly(text);
    let imports: Vec<&str> = assembly
        .lines()
        .filter(|line| line.starts_with(".import"))
        .map(|line| line.split("  ").next().unwrap_or(line).trim())
        .collect();
    assert_eq!(
        imports,
        vec![
            ".import _puts",
            ".import rom_entry : far",
            ".import _count",
            ".importzp ptr",
            ".import rom : far",
        ],
    );

    // `port` is declared and never used, so nothing is imported for it: an
    // interface unit costs the objects that read it nothing. Its `: word`
    // is what would have been checked against the register width had
    // anything read it, which `rom` — declared `far` in its *address* and
    // unstated in its datum — deliberately does not repeat.
    assert!(
        !assembly.contains(" port"),
        "an extern nothing reads needs no line:\n{assembly}"
    );
    // And nothing is reserved or equated for any of them.
    assert!(!assembly.contains(".res"), "{assembly}");
    assert!(!assembly.contains(":="), "{assembly}");

    // The comment says which it is. An extern's line is a promise, and
    // "defined by" would send a reader looking for a body that is not there.
    assert!(
        assembly.contains("; k816: declared extern by ffi.k816:1"),
        "{assembly}"
    );
}

/// A declared exit reaches the caller's encoded immediate.
///
/// The claim the whole construct is for: a callee K816 has never read can
/// *state* what it leaves, and the caller sizes its next immediate against
/// the statement. Three bytes rather than two, decided by a line in a source
/// file and nothing else.
#[test]
fn a_declared_exit_reaches_the_callers_encoded_immediate() {
    let widened = assembly(
        "extern func setup @a8 @i8 -> @a16\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   call setup\n\
         \x20   lda #$0002\n\
         \x20   sep #$20\n\
         \x20   rts\n\
         }\n",
    );
    assert!(
        instructions(&widened).contains(&"lda #$0002".to_owned()),
        "the declared 16-bit exit sized the immediate:\n{widened}"
    );

    // Without the clause the caller keeps believing its own width, and the
    // identical source is a two-byte load of the same number. That is the
    // whole difference the declaration makes, and it is a difference in the
    // *bytes* rather than in a diagnostic — which is why an extern with a
    // wrong clause is a bug nothing here can catch.
    let narrow = assembly(
        "extern func setup\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   call setup\n\
         \x20   lda #$0002\n\
         \x20   rts\n\
         }\n",
    );
    assert!(
        instructions(&narrow).contains(&"lda #$02".to_owned()),
        "a clauseless extern preserves, so the accumulator is still 8-bit:\n{narrow}"
    );
}

/// A declared entry is bridged to, and only where the caller is not there
/// already.
#[test]
fn a_declared_entry_is_bridged_before_the_call() {
    let bridged = assembly(
        "extern func wants_wide @a16\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   call wants_wide\n\
         \x20   rts\n\
         }\n",
    );
    assert_eq!(
        instructions(&bridged),
        vec!["rep #$20", "jsr a:wants_wide", "rts"],
        "the caller is narrowed and the bridge widens it:\n{bridged}"
    );

    // Not `main`, whose widths are the environment's and cannot be
    // declared away (`K816-E0143`).
    let already = assembly(
        "extern func wants_wide @a16\n\
         \n\
         func caller @a16 @i8 {\n\
         \x20   call wants_wide\n\
         \x20   rts\n\
         }\n",
    );
    assert_eq!(
        instructions(&already),
        vec!["jsr a:wants_wide", "rts"],
        "nothing to bridge:\n{already}"
    );
}

/// A clauseless extern preserves all three axes, and says so by emitting
/// nothing.
///
/// Stronger than the fallback an *invisible* callee gets, which assumes the
/// emulation flag may have moved. cc65 generates 6502 code — no `REP`, no
/// `SEP`, no `XCE` — so it cannot move m, x or e, and the declaration that
/// says nothing is the declaration that says that.
#[test]
fn a_clauseless_extern_preserves_all_three_axes() {
    let text = "extern func quiet\n\
                \n\
                func main @a8 @i8 {\n\
                \x20   call quiet\n\
                \x20   lda #$01\n\
                \x20   rts\n\
                }\n";
    let assembly = assembly(text);
    assert_eq!(
        instructions(&assembly),
        vec!["jsr a:quiet", "lda #$01", "rts"],
        "nothing is re-established around the call:\n{assembly}"
    );
    // And no claim, no comment, and above all no "assumed": an extern is a
    // contract, and the compiler is not guessing.
    assert!(
        !assembly.contains("assumed to preserve"),
        "a declared callee is not an assumption:\n{assembly}"
    );
}

/// The declared kind decides the call, so a mismatched spelling is refused.
///
/// `K816-E0138` is E5's check and it needed nothing new: a foreign section's
/// kind is what the declaration says, and the same code compares it to the
/// call site.
#[test]
fn a_convention_mismatch_on_an_extern_is_refused_both_ways() {
    for (declaration, call) in [
        ("extern func near_one\n", "call far near_one"),
        ("extern far func far_one\n", "call far_one"),
    ] {
        let name = if call.contains("near") {
            "near_one"
        } else {
            "far_one"
        };
        let mut map = SourceMap::new();
        let file = map.add_file(
            "convention.k816",
            format!("{declaration}\nfunc main @a8 @i8 {{\n    {call}\n}}\n"),
        );
        assert_eq!(
            codes_for(&map, file),
            vec!["K816-E0138"],
            "`{call}` against `{name}`"
        );
    }
}

// ---------------------------------------------------------------------------
// What the declaration is checked against
// ---------------------------------------------------------------------------

/// An `extern` and a definition of one name, in one unit and across two.
#[test]
fn an_extern_the_program_also_defines_is_refused() {
    // In one unit, where the symbol table's own duplicate rule sees it.
    let mut map = SourceMap::new();
    let file = map.add_file(
        "same.k816",
        "extern func helper\n\nfunc helper @a8 @i8 {\n    nop\n}\n",
    );
    assert_eq!(codes_for(&map, file), vec!["K816-E0102"]);

    // Across two, where only the workspace can see it.
    let mut map = SourceMap::new();
    let entry = map.add_file(
        "user.k816",
        "import \"body.k816\"\n\
         extern func helper\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   call helper\n\
         }\n",
    );
    map.add_file("body.k816", "func helper @a8 @i8 {\n    nop\n}\n");
    let reported = Linkage::resolve(&map, EntryEnvironment::default());
    let found: Vec<&'static str> = reported
        .diagnostics()
        .iter()
        .map(|diagnostic| diagnostic.code.0)
        .collect();
    assert_eq!(found, vec!["K816-E0164"]);
    // The body stays canonical, so an editor sends a reader to the truth.
    assert_eq!(
        reported.definition("helper").map(|found| found.origin),
        Some(k816::linkage::Origin::Body)
    );
    assert!(codes_for(&map, entry).contains(&"K816-E0164"));
}

/// A collision anywhere in the loaded closure is found, even where the two
/// units cannot see each other.
///
/// Visibility decides what a unit may *name*; it does not decide what ld65
/// will be handed. Two objects, one with a body and one with an import of
/// the same name, link to whichever the linker finds — which is the
/// ambiguity this refuses.
#[test]
fn a_collision_out_of_sight_is_still_a_collision() {
    let mut map = SourceMap::new();
    let entry = map.add_file(
        "main.k816",
        "import \"iface.k816\"\n\
         import \"body.k816\"\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   nop\n\
         }\n",
    );
    // Neither of these imports the other, so neither can see the conflict.
    map.add_file("iface.k816", "extern func helper\n");
    map.add_file("body.k816", "func helper @a8 @i8 {\n    nop\n}\n");
    assert!(codes_for(&map, entry).contains(&"K816-E0164"));
}

/// Two identical `extern`s are one import; two that differ are an error, and
/// the message names the axis.
#[test]
fn externs_that_disagree_are_refused_and_the_message_says_how() {
    // Identical, in two units that both declare it: ordinary. This is how an
    // interface unit imported by ten units works.
    let mut map = SourceMap::new();
    let entry = map.add_file(
        "a.k816",
        "extern var dp ptr\n\nfunc main @a8 @i8 {\n    a = ptr\n}\n",
    );
    map.add_file(
        "b.k816",
        "extern var dp ptr\n\nfunc other @a8 @i8 {\n    a = ptr\n}\n",
    );
    assert_eq!(codes_for(&map, entry), Vec::<&str>::new());

    // And every axis they can differ on.
    let cases: &[(&str, &str, &str)] = &[
        (
            "extern var dp ptr\n",
            "extern var abs ptr\n",
            "address size",
        ),
        (
            "extern var abs ptr : byte\n",
            "extern var abs ptr : word\n",
            "declared width",
        ),
        (
            "extern func f\n",
            "extern far func f\n",
            "calling convention",
        ),
        ("extern func f\n", "extern var f\n", "kind of thing"),
        (
            "extern func f -> @a8\n",
            "extern func f -> @a16\n",
            "width contract",
        ),
        (
            "extern func f @a8\n",
            "extern func f @a16\n",
            "width contract",
        ),
    ];
    for (left, right, axis) in cases {
        let mut map = SourceMap::new();
        map.add_file("left.k816", *left);
        map.add_file("right.k816", *right);
        let linkage = Linkage::resolve(&map, EntryEnvironment::default());
        let reported: Vec<&k816::diagnostic::Diagnostic> = linkage.diagnostics().iter().collect();
        assert_eq!(
            reported.iter().map(|d| d.code.0).collect::<Vec<_>>(),
            vec!["K816-E0165"],
            "{left:?} against {right:?}"
        );
        assert!(
            reported[0].notes.iter().any(|note| note.contains(axis)),
            "the note names the axis `{axis}`: {:?}",
            reported[0].notes
        );
    }
}

/// A call to a clauseless extern does not lose a *known* emulation state.
///
/// The one axis where the default is stronger than the fallback an invisible
/// callee gets, and the only place the difference is visible. Under
/// `--entry emulation` the machine is known to be in emulation mode, where a
/// sixteen-bit annotation names a width that does not exist and is refused;
/// a callee whose exit merely *assumed* the flag would drop the caller to
/// "unknown", where the same annotation is the documented escape hatch and
/// compiles. So `Preserve` here is the difference between a refusal and a
/// program that assembles into nonsense.
///
/// It is true of the foreign code that exists: cc65 generates 6502, which
/// has no `XCE`. A callee that does run one says so.
#[test]
fn an_extern_call_does_not_lose_a_known_emulation_state() {
    let text = "extern func quiet\n\
                \n\
                func main {\n\
                \x20   call quiet\n\
                \x20   @a16 {\n\
                \x20       nop\n\
                \x20   }\n\
                }\n";
    let mut map = SourceMap::new();
    let file = map.add_file("emulation.k816", text);
    let options = k816::Options {
        entry: EntryEnvironment::parse("emulation").expect("an emulation environment"),
        ..k816::Options::default()
    };
    let codes: Vec<&'static str> = k816::compile_to_ca65(&map, file, &options)
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.0)
        .collect();
    assert_eq!(
        codes,
        vec!["K816-E0163"],
        "the emulation flag survived the call, so the widening is still refused"
    );

    // A callee K816 *cannot* see is the contrast: the fallback assumes the
    // flag may have moved, the state becomes unknown, and the same
    // annotation is allowed through as the escape hatch it is.
    let mut map = SourceMap::new();
    let file = map.add_file(
        "invisible.k816",
        "func main {\n    jsr (VEC,x)\n    @a16 {\n        nop\n    }\n}\n\nvar VEC = $0300\n",
    );
    let codes: Vec<&'static str> = k816::compile_to_ca65(&map, file, &options)
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.0)
        .collect();
    assert!(
        !codes.contains(&"K816-E0163"),
        "an unclassified callee loses the flag, which is what an extern does not: {codes:?}"
    );
}
