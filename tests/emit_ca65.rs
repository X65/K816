/*****************************************************************************/
/*                                                                           */
/*                                emit_ca65.rs                               */
/*                                                                           */
/*              Golden tests for the emitted ca65 assembly text              */
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

//! Exact ca65 output for the slice (master plan Phase 5): readable assembly
//! with a source or debug annotation on every emitted operation.
//! Golden changes are reviewed individually, never bulk-blessed.

use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

mod common;

/// The lowered, unoptimized program of one source — what `emitted_bytes`
/// measures.
fn lowered(name: &str, text: &str) -> k816::op::KProgram {
    lowered_allowing(name, text, false)
}

/// [`lowered`], for a source that warns on purpose.
///
/// A section that runs an `xce` earns `K816-W0124` — it is the whole point of
/// the escape-hatch cases — so the strict helper cannot express them. Errors
/// are still refused: a program that does not compile has no bytes to count.
fn lowered_allowing(name: &str, text: &str, warnings: bool) -> k816::op::KProgram {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let linkage = k816::linkage::Linkage::resolve(&map, k816::abi::EntryEnvironment::default());
    let source = map.file(file);
    let parse = k816::parser::parse_source(source, None);
    let (model, diagnostics) = k816::semantic::analyze_unit(&parse.file, source, &linkage);
    assert_diagnostics(&diagnostics, warnings);
    let (program, lower_diagnostics) =
        k816::lower::lower_unit(model.program(&parse.file), &model, source.name());
    assert_diagnostics(&lower_diagnostics, warnings);
    program
}

fn assert_diagnostics(diagnostics: &[k816::diagnostic::Diagnostic], warnings: bool) {
    if warnings {
        assert!(
            !k816::diagnostic::has_errors(diagnostics),
            "unexpected errors: {diagnostics:?}"
        );
    } else {
        assert_eq!(diagnostics, [], "unexpected diagnostics");
    }
}

fn bytes(name: &str, text: &str) -> u32 {
    k816::emit_ca65::emitted_bytes(&lowered(name, text))
}

fn bytes_allowing_warnings(name: &str, text: &str) -> u32 {
    k816::emit_ca65::emitted_bytes(&lowered_allowing(name, text, true))
}

fn emit(name: &str, text: &str) -> String {
    emit_allowing(name, text, false)
}

/// [`emit`], for a source that warns on purpose.
fn emit_allowing(name: &str, text: &str, warnings: bool) -> String {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_diagnostics(&result.diagnostics, warnings);
    result.assembly.expect("compilation succeeds")
}

#[test]
fn hla_slice_emits_the_golden_assembly() {
    let assembly = emit("slice.k816", include_str!("golden/slice.k816"));
    assert_eq!(assembly, include_str!("golden/slice-unopt.s"));
}

#[test]
fn every_instruction_carries_debug_and_source_annotations() {
    let assembly = emit("slice.k816", include_str!("golden/slice.k816"));

    let instruction_lines: Vec<&str> = assembly
        .lines()
        .filter(|line| {
            let trimmed = line.trim_start();
            ["sep", "rep", "lda", "sta", "rts"]
                .iter()
                .any(|mnemonic| trimmed.starts_with(mnemonic))
        })
        .collect();
    // Four: the slice's own instructions and nothing else. An entry point
    // gets no width setup — the environment provides its widths, and an
    // annotation on `main` is a claim about them rather than a request.
    assert_eq!(instruction_lines.len(), 4);
    for line in &instruction_lines {
        assert!(
            line.contains("; slice.k816:") || line.contains("; k816:"),
            "instruction lacks an annotation: {line}"
        );
    }

    // One `.dbg line` directive precedes every instruction — four, now that
    // an entry point generates no width setup of its own.
    let dbg_lines = assembly
        .lines()
        .filter(|line| line.trim_start().starts_with(".dbg    line"))
        .count();
    assert_eq!(dbg_lines, 4);

    // The debug file table names the K816 source.
    assert!(assembly.contains(".dbg    file, \"slice.k816\", 0, 0"));
}

#[test]
fn functions_are_exported_for_cross_module_references() {
    let assembly = emit("slice.k816", include_str!("golden/slice.k816"));
    assert!(assembly.contains("\n.export main "), "{assembly}");
}

#[test]
fn fixed_address_vars_become_equates_not_resolved_addresses() {
    let assembly = emit("slice.k816", include_str!("golden/slice.k816"));
    assert!(assembly.contains("SCREEN          := $0400"));
    // The store references the symbol, never a numeric address: ca65/ld65
    // own resolution.
    assert!(assembly.contains("sta     a:SCREEN"));
    assert!(!assembly.contains("sta     $0400"));
}

/// The entry contract's directives sit between the function label and the
/// first instruction that depends on a width.
///
/// Both axes are claimed even though the source annotates one: an entry
/// point's widths are the environment's, and the assembler has to be told
/// both or it sizes the first immediate by whatever it last assumed.
#[test]
fn width_directives_track_the_entry_contract() {
    let assembly = emit("slice.k816", include_str!("golden/slice.k816"));
    let main_at = assembly.find("main:").expect("main label");
    let a8_at = assembly.find("        .a8 ").expect(".a8 directive");
    let i8_at = assembly.find("        .i8 ").expect(".i8 directive");
    let first_load = assembly.find("        lda ").expect("the first load");
    assert!(main_at < a8_at && a8_at < first_load, "{assembly}");
    assert!(main_at < i8_at && i8_at < first_load, "{assembly}");
}

// ---------------------------------------------------------------------------
// Label emission (sub-plan C, task C2 with backend decision 4).
// ---------------------------------------------------------------------------

#[test]
fn local_labels_become_ca65_cheap_locals() {
    let assembly = emit(
        "labels.k816",
        "var SCREEN = $0400\nfunc work @a8 @i16 {\n  .again:\n    lda #1\n    sta .again\n}\n",
    );
    assert!(assembly.contains("\n@again:"), "{assembly}");
    assert!(
        assembly.contains("sta     a:@again"),
        "references use the cheap-local spelling too: {assembly}"
    );
}

#[test]
fn a_global_label_inside_a_function_qualifies_its_local_labels() {
    // ca65 restarts the cheap-local scope at every non-cheap label, so a
    // global label written inside a function body would strand every `@name`
    // declared before it. The qualified spelling is what keeps a reference
    // across that boundary resolvable instead of failing inside ca65.
    let assembly = emit(
        "split.k816",
        "func work @a8 @i16 {\n  .again:\n    lda #1\n  done:\n    sta .again\n}\n",
    );
    assert!(assembly.contains("\nwork_again:"), "{assembly}");
    assert!(assembly.contains("\ndone:"), "{assembly}");
    assert!(assembly.contains("sta     a:work_again"), "{assembly}");
    let code_only: String = assembly
        .lines()
        .map(|line| line.split(';').next().unwrap_or_default())
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        !code_only.contains('@'),
        "no cheap local may survive the split: {assembly}"
    );
}

#[test]
fn two_functions_reusing_a_local_label_name_stay_distinct() {
    let assembly = emit(
        "twice.k816",
        "func one @a8 {\n  .loop:\n    lda #1\n}\nfunc two @a8 {\n  .loop:\n    lda #2\n}\n",
    );
    assert_eq!(
        assembly.matches("\n@loop:").count(),
        2,
        "each function opens its own cheap-local scope: {assembly}"
    );
}

// ---------------------------------------------------------------------------
// Operands, expressions, and relocations (sub-plan C, task C6).
// ---------------------------------------------------------------------------

/// The instruction lines of `text`, without their annotations.
fn instructions(name: &str, text: &str) -> Vec<String> {
    emit_unoptimized(name, text)
        .lines()
        .filter_map(|line| line.split_once(';'))
        .map(|(code, _)| code.trim().to_owned())
        // Directives, labels, and equates are their own tests' business.
        .filter(|code| {
            !code.is_empty()
                && !code.starts_with('.')
                && !code.contains(":=")
                && !code.ends_with(':')
        })
        .collect()
}

/// Compiles without the optimizer, tolerating warnings.
///
/// Several fixtures here deliberately do things that are worth warning about
/// — a `plp` before a return, a `jsr` back into the function itself — because
/// what is being examined is the *emitted text* around them. The warnings
/// themselves have their own tests; an error still fails the run.
fn emit_unoptimized(name: &str, text: &str) -> String {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let errors: Vec<&k816::diagnostic::Diagnostic> = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.is_error())
        .collect();
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    result.assembly.expect("compilation succeeds")
}

#[test]
fn every_addressing_mode_emits_its_ca65_syntax() {
    let text = "var ZP = $80
var SCREEN = $0400
const FIELD = 2

func work @a8 @i16 {
    lda ZP
    lda SCREEN
    lda far ZP
    lda SCREEN,x
    lda (ZP,x)
    lda (ZP),y
    lda [ZP]
    lda [ZP],y
    lda 4,s
    lda (4,s),y
    lda &FIELD,x
    lda #1
    asl a
    inx
    pha
    pea #$1234
    pei (ZP)
    per work
    brk #$42
    cop #$7F
    mvn $01,$02
    jml [SCREEN]
    jmp (SCREEN)
    jsr work
    bne work
}
";
    assert_eq!(
        instructions("modes.k816", text),
        vec![
            // An ordinary function gets no entry setup, and neither would
            // already 8-bit after reset.
            "lda     z:ZP",
            "lda     a:SCREEN",
            "lda     f:ZP",
            "lda     a:SCREEN,x",
            "lda     (ZP,x)",
            "lda     (ZP),y",
            "lda     [ZP]",
            "lda     [ZP],y",
            "lda     $04,s",
            "lda     ($04,s),y",
            "lda     z:$02,x",
            "lda     #$01",
            // The accumulator modes are spelled without the register.
            "asl",
            "inx",
            "pha",
            // K65 writes these three with a `#`; ca65 spells them without.
            "pea     $1234",
            "pei     (ZP)",
            "per     work",
            "brk     $42",
            "cop     $7F",
            // The banks keep their `#`: without it ca65 reads each operand
            // as an address and encodes its bank byte, so `mvn $01,$02`
            // would move between banks 0 and 0 (`tests/ca65_guide.rs` is
            // what found this).
            "mvn     #$01,#$02",
            "jml     [SCREEN]",
            "jmp     (SCREEN)",
            "jsr     a:work",
            "bne     work",
            "rts",
        ]
    );
}

#[test]
fn expressions_use_ca65s_named_operators_and_explicit_grouping() {
    let text = "func work @a16 @i16 {
  spot:
    lda #spot + 1 & $FF
    lda #spot * 2
    lda #spot << 1 | 3
    lda #~spot
    lda #-spot
    lda #&<spot
    lda #&>spot
    lda #&&spot
}
";
    assert_eq!(
        instructions("expr.k816", text),
        vec![
            // `&`, `^`, `<<`, and `>>` sit at ca65's precedence 2 and `|` at
            // 3, so only explicit grouping keeps the K65 meaning.
            "lda     #((spot + $01) .bitand $FF)",
            "lda     #(spot * $02)",
            "lda     #((spot .shl $01) .bitor $03)",
            "lda     #(~ spot)",
            "lda     #(0 - spot)",
            "lda     #.lobyte(spot)",
            "lda     #.hibyte(spot)",
            "lda     #.loword(spot)",
            "rts",
        ]
    );
}

/// `lda a:(spot + 2)` is not the absolute load it looks like — ca65 reads
/// the parenthesis as an indirection and rejects the line. The outermost
/// pair is dropped in operand position for exactly that reason; every
/// nested level keeps its own.
#[test]
fn an_address_operand_drops_only_its_outermost_parentheses() {
    let text = "func work @a16 @i16 {\n  spot:\n    lda spot + 2 & $FF\n    lda spot + 2\n}\n";
    let lines = instructions("outer.k816", text);
    assert!(
        lines.contains(&"lda     a:spot + $02".to_owned()),
        "{lines:?}"
    );
    assert!(
        lines.contains(&"lda     a:(spot + $02) .bitand $FF".to_owned()),
        "{lines:?}"
    );
}

/// A width transition K816 cannot resolve must produce no `.a…`/`.i…` claim
/// at all: a wrong one changes how ca65 sizes every immediate that follows.
#[test]
fn an_unknown_width_transition_emits_no_width_claim() {
    let assembly = emit_unoptimized(
        "unknown.k816",
        "func work @a8 @i16 {\n    plp\n    sep #$20\n}\n",
    );
    let claims: Vec<&str> = assembly
        .lines()
        .map(|line| line.split(';').next().unwrap_or_default().trim())
        .filter(|line| line.starts_with(".a") || line.starts_with(".i"))
        .collect();
    // The contract's two claims, then `sep #$20` re-establishing the
    // accumulator width after `plp` made it unknown. Nothing claims a width
    // between the `plp` and the `sep`, and the index width is never
    // re-claimed: the `plp` dropped it and only an annotation could bring it
    // back.
    assert_eq!(claims, vec![".a8", ".i16", ".a8"], "{assembly}");
}

// ---------------------------------------------------------------------------
// Data blocks (sub-plan C, task C8).
// ---------------------------------------------------------------------------

/// The emitted lines of `text` without their annotations or debug entries.
fn lines(name: &str, text: &str) -> Vec<String> {
    emit_unoptimized(name, text)
        .lines()
        .map(|line| {
            line.split_once(';')
                .map_or(line, |(code, _)| code)
                .trim_end()
        })
        .filter(|line| !line.is_empty() && !line.trim_start().starts_with(".dbg"))
        .map(str::to_owned)
        .collect()
}

/// A named block's label marks its *first emitted byte*, so a leading
/// `align` has to precede it — otherwise the alignment would move the bytes
/// out from under the label that names them.
#[test]
fn placement_directives_precede_the_block_label() {
    let emitted = lines("place.k816", "data table {\n    align 16\n    1 2\n}\n");
    let align = emitted.iter().position(|l| l.contains(".align")).unwrap();
    let label = emitted.iter().position(|l| l == "table:").unwrap();
    assert!(align < label, "{emitted:?}");
}

#[test]
fn raw_values_and_the_placeholder_emit_bytes() {
    let emitted = lines(
        "values.k816",
        "var ptr = $2345\ndata bytes {\n    $7F 1\n    hello:\n    &<ptr &>ptr\n    ? ?\n}\n",
    );
    assert!(emitted.contains(&"bytes:".to_owned()), "{emitted:?}");
    assert!(emitted.contains(&"hello:".to_owned()), "{emitted:?}");
    for expected in [
        "        .byte    $7F",
        "        .byte    $01",
        "        .byte    .lobyte(ptr)",
        "        .byte    .hibyte(ptr)",
        // `?` is a source-level affordance: on the wire it is a zero.
        "        .byte    $00",
    ] {
        assert!(
            emitted.contains(&expected.to_owned()),
            "{expected}: {emitted:?}"
        );
    }
}

/// K816 checks the run's size; ld65 checks where it lands, because only
/// placement knows (sub-plan C, backend decision 6). The legacy linker's
/// automatic pre-padding is deliberately not recreated.
#[test]
fn nocross_emits_a_link_time_page_assertion() {
    let emitted = lines("nocross.k816", "nocross data guarded {\n    1 2 3 4\n}\n");
    assert_eq!(
        emitted.last().map(String::as_str),
        Some(
            "        .assert  .hibyte(* - 1) = .hibyte(* - 4), lderror, \
             \"nocross run crosses a page boundary\""
        ),
        "the guard follows the run it guards: {emitted:?}"
    );
    // `lderror`, not `error`: where the run lands is decided after ca65 has
    // finished, so the check is ld65's and the action says so. Both actions
    // reach ld65 — the guard was never inert — but `error` also asks the
    // assembler to try, and a source it cannot fold collects a "Cannot
    // evaluate this source code assertion" warning about someone else's
    // check. `k816/nocross-page-assert` links a run that ends on the last
    // byte of a page, and one byte more fails ld65.
}

/// ca65 cannot pin a relocatable block to an address, so the request becomes
/// a segment the ld65 config gives a fixed `start` — and the warning quotes
/// the fragment to paste, because K816 does not write linker configs.
#[test]
fn address_placement_becomes_a_named_segment_and_says_so() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "place.k816",
        "data pinned {\n    address $5000\n    1 2\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );

    assert_eq!(result.diagnostics.len(), 1, "{:?}", result.diagnostics);
    let warning = &result.diagnostics[0];
    assert_eq!(
        warning.code,
        k816::diagnostic::DiagnosticCode::ADDRESS_NEEDS_CONFIG
    );
    assert_eq!(warning.severity, k816::diagnostic::Severity::Warning);
    assert!(
        warning.notes[0].contains("K816_AT_5000__PINNED: load = MAIN, type = ro, start = $5000"),
        "the note is a config fragment to paste: {warning:?}"
    );

    let assembly = result.assembly.expect("a warning does not stop emission");
    assert!(
        assembly.contains(".segment \"K816_AT_5000__PINNED\""),
        "{assembly}"
    );
}

/// The config entry is hand-maintained, so a name that renumbers when an
/// unrelated block is inserted earlier would be a real failure. A named
/// block is identified by its name alone; only the anonymous forms carry a
/// position-derived counter, and the warning says so.
#[test]
fn a_named_blocks_address_segment_does_not_renumber() {
    let name_of = |text: &str| {
        let mut map = SourceMap::new();
        let file = map.add_file("stable.k816", text);
        let result = compile_to_ca65(
            &map,
            file,
            &Options {
                optimize: false,
                ..Default::default()
            },
        );
        result
            .assembly
            .expect("compilation succeeds")
            .lines()
            .find(|line| line.contains("K816_AT_"))
            // The comment names the source line, which is exactly what an
            // inserted block does move.
            .map(|line| line.split(';').next().unwrap_or_default().trim().to_owned())
            .expect("a synthetic segment")
    };

    let alone = name_of("data pinned {\n    address $5000\n    1\n}\n");
    let preceded = name_of("data other {\n    1\n}\ndata pinned {\n    address $5000\n    1\n}\n");
    assert_eq!(
        alone, preceded,
        "inserting a block ahead must not rename it"
    );

    // Two blocks asking for one address get their own segments: the two
    // intents — overlay them, or concatenate them — are the config's to
    // choose between, and an accidental collision must stay visible.
    let mut map = SourceMap::new();
    let file = map.add_file(
        "two.k816",
        "data aa {\n    address $5000\n    1\n}\ndata bb {\n    address $5000\n    2\n}\n",
    );
    let both = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let assembly = both.assembly.expect("compilation succeeds");
    assert!(assembly.contains("K816_AT_5000__A"), "{assembly}");
    assert!(assembly.contains("K816_AT_5000__B"), "{assembly}");
}

/// An anonymous block inside a function emits raw bytes in the middle of
/// executable code, so control steps over them. The landing site is a ca65
/// *unnamed* label: a named one would either pollute the unit's namespace
/// or split the cheap-local scope the function's own labels live in.
#[test]
fn an_inline_data_block_is_jumped_over() {
    let emitted = lines(
        "inline.k816",
        "func work @a8 @i16 {\n  .again:\n    data { $EA }\n    lda #1\n    bne .again\n}\n",
    );
    let jump = emitted
        .iter()
        .position(|l| l.contains("jmp     :+"))
        .unwrap();
    let byte = emitted
        .iter()
        .position(|l| l.contains(".byte    $EA"))
        .unwrap();
    let target = emitted.iter().position(|l| l == ":").unwrap();
    assert!(jump < byte && byte < target, "{emitted:?}");
    // The function's own local label survives as a cheap local.
    assert!(emitted.contains(&"@again:".to_owned()), "{emitted:?}");
    assert!(
        emitted.iter().any(|l| l.contains("bne     @again")),
        "{emitted:?}"
    );
}

/// Phase 6's criterion is that *every* emitted operation carries a source or
/// debug annotation — "instruction, label, directive, and data". Checking
/// only instructions is what let labels and linkage directives go bare, so
/// this checks the whole file: anything that is not the fixed preamble, a
/// `.dbg line` directive, or a blank line has to explain where it came from.
#[test]
fn every_emitted_line_says_where_it_came_from() {
    // Everything C emits, in one unit: equates, derived linkage, segments,
    // labels of both scopes, data, placement directives, the nocross guard,
    // the inline-data jump, and instructions.
    let text = "var ZP = $80
var far BANKED = $030000

segment FAST

data table {
    align 4
    inner:
    1 2
}

nocross data guarded {
    3 4
}

data pinned {
    address $5000
    5
}

func work @a8 @i16 {
  .spot:
    data { $EA }
    lda ZP
    lda BANKED
    bne .spot
  global:
    sta ZP
}
";
    let mut map = SourceMap::new();
    let file = map.add_file("annotated.k816", text);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let assembly = result
        .assembly
        .expect("a W0116 warning does not stop emission");

    // The fixed preamble is the file header, not an operation.
    const PREAMBLE: &[&str] = &[
        "; Generated by k816",
        ".setcpu",
        ".debuginfo",
        ".dbg    file,",
    ];
    let bare: Vec<&str> = assembly
        .lines()
        .filter(|line| !line.trim().is_empty())
        .filter(|line| {
            !PREAMBLE
                .iter()
                .any(|prefix| line.trim_start().starts_with(prefix))
        })
        // A `.dbg line` directive *is* the debug annotation of the operation
        // beneath it, so it does not need one of its own.
        .filter(|line| !line.trim_start().starts_with(".dbg    line"))
        .filter(|line| !line.contains(';'))
        .collect();
    assert!(
        bare.is_empty(),
        "these emitted operations carry no annotation:\n{}\n\nfull output:\n{assembly}",
        bare.join("\n")
    );
}

#[test]
fn labels_and_linkage_directives_quote_their_own_source() {
    let assembly = emit_unoptimized(
        "labelled.k816",
        "func main @a8 {\n  spot:\n    bne spot\n}\n",
    );
    // A label quotes the line it was written on…
    assert!(assembly.contains("spot:"), "{assembly}");
    let label_line = assembly
        .lines()
        .find(|line| line.trim_start().starts_with("spot:"))
        .expect("the label");
    assert!(
        label_line.contains("; labelled.k816:2: spot:"),
        "a label quotes its source: {label_line}"
    );

    // …and a derived export says why it exists, since nothing in the source
    // asked for it.
    let export_line = assembly
        .lines()
        .find(|line| line.starts_with(".export"))
        .expect("the derived export");
    assert!(
        export_line.contains("; k816: a function defined here, visible to other units"),
        "{export_line}"
    );
}

// ---------------------------------------------------------------------------
// The width model's claims (sub-plan D, task D4).
// ---------------------------------------------------------------------------

/// The emitter has exactly two sources for its tracked width, and they are
/// both needed: an ordinary user-written `rep` still updates ca65's
/// assumption, so the load below assembles three bytes wide.
#[test]
fn an_ordinary_rep_still_updates_the_assemblers_assumption() {
    let assembly = emit_unoptimized(
        "rep.k816",
        "func main @a8 {\n    rep #$20\n    lda #$1234\n}\n",
    );
    let rep_at = assembly.find("rep     #$20").expect("the rep");
    let a16_at = assembly.find("        .a16").expect(".a16");
    let lda_at = assembly.find("lda     #$1234").expect("the load");
    assert!(rep_at < a16_at && a16_at < lda_at, "{assembly}");
}

/// A branch that skips a mode change must not leave a width claim behind it.
/// The model says the label is 8-bit; the emitter says what the model says,
/// rather than what its own linear walk over the operations would imply.
#[test]
fn a_skipped_mode_change_does_not_claim_a_width_at_the_label() {
    let assembly = emit_unoptimized(
        "branch.k816",
        "func main @a8 {\n    bra .after\n    rep #$20\n.after:\n    lda #$01\n}\n",
    );
    let label_at = assembly.find("@after:").expect("the label");
    let after = &assembly[label_at..];
    let a8_at = after.find("        .a8").expect("the label's claim");
    let lda_at = after.find("lda     #$01").expect("the load");
    assert!(a8_at < lda_at, "the claim precedes the load: {assembly}");
    assert!(
        !after[..lda_at].contains(".a16"),
        "the skipped width must not reach the label: {assembly}"
    );
}

/// An unknown axis emits *no* directive, because ca65 has no way to un-assume
/// one, and says so instead.
///
/// That silence is sound only because K816 refuses width-dependent immediates
/// while the state is unknown, so the two are asserted together here: a
/// change that removes the refusal has to fail on this side as well.
#[test]
fn an_unknown_axis_emits_an_explanation_rather_than_a_directive() {
    let source = "func main @a8 @i8 {\n    plp\n    nop\n}\n";
    let assembly = emit_unoptimized("unknown.k816", source);
    let plp_at = assembly.find("plp").expect("the plp");
    let after = &assembly[plp_at..];
    assert!(
        !after.contains(".a8") && !after.contains(".a16"),
        "no directive can un-assume a width: {assembly}"
    );
    assert!(
        after.contains("accumulator width is unknown from here"),
        "the silence explains itself: {assembly}"
    );

    // The other half of the pair: the immediate that would need that width is
    // refused, which is what makes emitting nothing sound.
    let mut map = SourceMap::new();
    let file = map.add_file(
        "unknown.k816",
        "func main @a8 {\n    plp\n    lda #$01\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let errors: Vec<&str> = result
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.is_error())
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(errors, ["K816-E0104"]);
}

/// Where emulation mode is why a width became unknown, the emitted
/// explanation says so — the reader is otherwise left looking for a missing
/// annotation.
#[test]
fn the_explanation_names_emulation_mode_when_that_is_the_cause() {
    let assembly = emit_unoptimized(
        "emu.k816",
        "func main @a8 @i8 {\n    xce\n    rep #$20\n    nop\n}\n",
    );
    assert!(
        assembly.contains("because emulation mode is unknown here"),
        "{assembly}"
    );
}

/// `emitted_bytes` sizes a width-dependent immediate at the width ca65 is
/// told to assemble it at, and nothing else at all.
///
/// The claim is worth a test of its own because the obvious implementation —
/// `lower::byte_length` over every operation — is *wrong* by exactly one byte
/// per eight-bit immediate, and wrong in the direction that makes a saving
/// look bigger than it is. `nocross` needs that over-estimate and a size
/// report must not have it.
#[test]
fn the_byte_count_sizes_an_immediate_at_the_width_it_will_be_assembled_at() {
    // `lda #$01` (2) + the implicit `rts` (1).
    assert_eq!(bytes("a8.k816", "func w @a8 @i8 {\n    lda #$01\n}\n"), 3);
    // The same instruction at sixteen bits is one byte longer.
    assert_eq!(
        bytes("a16.k816", "func w @a16 @i8 {\n    lda #$0001\n}\n"),
        4
    );
    // An operand that depends on no width is unaffected either way.
    assert_eq!(bytes("dp.k816", "func w @a8 @i8 {\n    lda $12\n}\n"), 3);
    // And the width an instruction *establishes* is tracked: `lda` (2),
    // `rep #$20` (2), `lda` (3), `rts` (1).
    assert_eq!(
        bytes(
            "mid.k816",
            "func w @a8 @i8 {\n    lda #$01\n    rep #$20\n    lda #$0002\n}\n"
        ),
        8
    );
}

/// A bridge K816 authored establishes its width for the assembler, even where
/// the emulation flag is unknown.
///
/// A `rep #$20` the *source* wrote there establishes nothing — the CPU
/// ignores it in emulation mode — and the emitter says so. A **bridge** is
/// different: `width::record_bridges` authors one only where the model
/// concluded the width holds, and refuses a widening one outright where
/// emulation is known to defeat it (`K816-E0163`). What is left is the
/// documented `xce`-then-widen escape hatch, where the `@a16` annotation is
/// the author's assertion that the machine is native.
///
/// **That assertion is what the immediates below it were sized against.**
/// Withdrawing it here left ca65 assembling `lda #$12` with one operand byte
/// after a `rep` that had widened the register, so the CPU read the *next*
/// instruction as the immediate's high byte (`postmortem.md` §H.13).
#[test]
fn a_bridge_establishes_its_width_even_where_emulation_is_unknown() {
    // `opaque` runs an `xce`, so the flag is unknown after the call and the
    // `@a16` block is the escape hatch.
    let assembly = emit_allowing(
        "hatch.k816",
        "naked opaque @a8 @i8 {\n    xce\n    return\n}\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   call opaque\n\
         \x20   @a16 {\n\
         \x20       lda #$12\n\
         \x20   }\n\
         \x20   nop\n\
         \x20   return\n\
         }\n",
        true,
    );

    let entering = assembly
        .find("entering the width block")
        .expect("the block is bridged");
    let after = &assembly[entering..];
    assert!(
        after.starts_with(|_: char| true) && after.contains(".a16"),
        "the bridge did not claim its width:\n{assembly}"
    );
    // The claim comes before the immediate, which is what sizes it.
    let claim = assembly.find(".a16").expect("the claim");
    let load = assembly.find("lda").expect("the load");
    assert!(claim < load, "the claim is after the load:\n{assembly}");
    assert!(
        !after[..load - entering].contains("accumulator width is unknown"),
        "the bridge's width was withdrawn:\n{assembly}"
    );

    // A `rep` the *source* wrote in the same place still establishes nothing:
    // it carries no assertion, so the tracked width is dropped.
    let written = emit_allowing(
        "written.k816",
        "func main @a8 @i8 {\n    xce\n    rep #$20\n    nop\n    return\n}\n",
        true,
    );
    assert!(
        written.contains("because emulation mode is unknown here"),
        "{written}"
    );
    assert!(!written.contains(".a16"), "{written}");
}

/// The model's byte count is what ca65 assembles, across every width
/// transition the language has.
///
/// The two sizings — `emitted_bytes`, which decides branch ranges and
/// `nocross` runs, and ca65's, which decides the object file — read the same
/// `.a8`-family directives. Where they part company the bytes are wrong and
/// nothing else notices: the emitted *text* is valid either way, and the
/// program simply executes something else. This is the gate that would have
/// caught §H.13.
#[test]
fn the_model_and_ca65_size_every_width_transition_alike() {
    // (name, source, the bytes the model counts)
    let cases: &[(&str, String, u32)] = &[
        // A widening annotation: `lda #$0002` is three bytes under `@a16`.
        (
            "block",
            "func w @a8 @i8 {\n    @a16 {\n        lda #$0002\n    }\n    nop\n}\n".to_owned(),
            // rep(2) + lda(3) + sep(2) + nop(1) + rts(1)
            9,
        ),
        // The same block after a callee that runs an `xce`, which is the
        // escape hatch: the flag is unknown and the widths are the same.
        (
            "hatch",
            "naked opaque @a8 @i8 {\n    xce\n    return\n}\n\
             \n\
             func w @a8 @i8 {\n\
             \x20   call opaque\n\
             \x20   @a16 {\n\
             \x20       lda #$0002\n\
             \x20   }\n\
             \x20   nop\n\
             }\n"
            .to_owned(),
            // opaque: xce(1) + rts(1); w: jsr(3) + rep(2) + lda(3) + sep(2)
            // + nop(1) + rts(1)
            14,
        ),
        // A narrowing bridge, which every emulation state agrees about.
        (
            "narrow",
            "func w @a16 @i8 {\n    @a8 {\n        lda #$02\n    }\n    nop\n}\n".to_owned(),
            // sep(2) + lda(2) + rep(2) + nop(1) + rts(1)
            8,
        ),
        // A callee whose exit puts back exactly what the bridge to it took
        // away. The model's state before the call and after it are the same
        // sixteen bits, and between them the bridge narrowed the accumulator
        // and told the assembler so. The claim after the call is therefore
        // load-bearing — without it ca65 keeps the bridge's eight bits and
        // sizes the immediate below as two bytes while the model counts
        // three. Nothing caught that until every corpus program was
        // assembled through the driver (`postmortem.md` §I.2).
        (
            "restored",
            "func fixes_a @a8 {\n    rep #$20\n}\n\n\
             func w @a16 @i8 {\n\
             \x20   call fixes_a\n\
             \x20   lda #$0002\n\
             }\n"
            .to_owned(),
            // fixes_a: rep(2) + rts(1); w: sep(2) + jsr(3) + lda(3) + rts(1)
            12,
        ),
    ];

    for (name, source, expected) in cases {
        let file = format!("{name}.k816");
        assert_eq!(
            bytes_allowing_warnings(&file, source),
            *expected,
            "{name}: the model's byte count moved"
        );

        // And ca65 agrees, which is the half a unit test cannot assert.
        let dir = common::work_dir(&format!("size-{name}"));
        let asm = dir.join("out.s");
        let obj = dir.join("out.o");
        let bin = dir.join("out.bin");
        std::fs::write(&asm, emit_allowing(&file, source, true)).expect("write assembly");
        common::run_ok(
            std::process::Command::new(common::tool("CA65", "ca65"))
                .arg(&asm)
                .args(["-o".as_ref(), obj.as_os_str()]),
            "ca65",
        );
        let config =
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden/slice.cfg");
        common::run_ok(
            std::process::Command::new(common::tool("LD65", "ld65"))
                .args(["-C".as_ref(), config.as_os_str()])
                .args(["-o".as_ref(), bin.as_os_str()])
                .arg(&obj),
            "ld65",
        );
        let linked = std::fs::read(&bin).expect("read the linked binary");
        assert_eq!(
            linked.len() as u32,
            *expected,
            "{name}: ca65 assembled {} bytes where the model counted {expected}",
            linked.len()
        );
    }
}

/// Every unit-level definition leaves the object file, carrying its address
/// size, and the four kinds that name nothing do not.
///
/// Sub-plan K removed the condition that used to gate a `const` and a
/// fixed-address `var` on another unit being seen to reference them. It could
/// not survive per-file compilation: under `import`, the unit being compiled
/// is the only one the exporter is looking at, so "somebody asks" is not a
/// question it can answer. What replaces it is what cc65 itself does — export
/// every non-`static` definition, with no marker in the source either way.
///
/// The address size is the point of the assertion rather than a detail. ca65
/// validates addressing modes against an imported symbol's size, so a `dp`
/// variable exported as absolute is a wrong instruction in the importing unit
/// rather than a wrong name.
#[test]
fn every_unit_level_definition_is_exported_with_its_address_size() {
    let source = "const ROWS = 25\n\
                  const WIDE = $12345\n\
                  var PORT = $D020\n\
                  var dp slot = $80\n\
                  var far rom = $018000\n\
                  var allocated:word\n\
                  \n\
                  data table { 1 2 3 }\n\
                  \n\
                  inline twice @a8 @i8 {\n\
                  \x20   nop\n\
                  }\n\
                  \n\
                  func main @a8 @i8 {\n\
                  global_label:\n\
                  \x20   .local_label:\n\
                  \x20   call twice\n\
                  \x20   return\n\
                  }\n";
    let assembly = emit("exports.k816", source);
    let exports: Vec<&str> = assembly
        .lines()
        .filter(|line| line.starts_with(".export"))
        .map(|line| line.split("  ").next().unwrap_or(line).trim())
        .collect();

    // Symbol-table order: the sections and data labels are declared before
    // the values, because `declare_placed_symbols` runs first.
    assert_eq!(
        exports,
        vec![
            ".export table",
            ".export main",
            ".export global_label",
            // A `const` has no address, so it crosses as the value it is.
            // The size is stated because ca65 would otherwise take it from
            // the value and export a small one as zeropage, which every
            // absolute import of it then disagrees with.
            ".export ROWS : abs = $0019",
            ".export WIDE : far = $12345",
            // A place, with the size a reference to it must encode.
            ".export PORT",
            ".exportzp slot",
            ".export rom : far",
            ".export allocated",
        ],
    );

    // And the four kinds that name no place and no value stay in. An
    // `inline` has no address at all — its body is expanded at each use site
    // — and a local label is scoped to one section.
    for absent in ["twice", "local_label"] {
        assert!(
            !assembly.contains(&format!(".export {absent}")),
            "`{absent}` has nothing to hand another unit:\n{assembly}"
        );
    }

    // ca65 accepts all of it, which is the half a string comparison cannot
    // assert: a name it reads as a register or an address-size prefix is an
    // assembly error rather than a wrong byte.
    let dir = common::work_dir("exports-everything");
    let asm = dir.join("out.s");
    std::fs::write(&asm, &assembly).expect("write assembly");
    common::run_ok(
        std::process::Command::new(common::tool("CA65", "ca65"))
            .arg(&asm)
            .args(["-o".as_ref(), dir.join("out.o").as_os_str()]),
        "ca65",
    );
}

/// A `const` this unit exports links against another module's `.import`
/// without ld65 warning about the address size.
///
/// The two halves have to agree and only one of them is K816's. ca65 derives
/// an `=`-defined symbol's address size from its *value*, so a bare
/// `.export ROWS = 25` exports a zeropage symbol; a hand-written module that
/// writes the ordinary `.import ROWS` then draws "Address size mismatch for
/// `ROWS`" from ld65 — a warning, so the link succeeds and nobody looks. The
/// explicit `: abs` is what prevents it, and this is the test that would
/// notice if it went away.
#[test]
fn an_exported_const_imports_without_an_address_size_warning() {
    let dir = common::work_dir("const-export-size");
    let assembly = emit(
        "defs.k816",
        "const ROWS = 25\n\nfunc main @a8 @i8 {\n    return\n}\n",
    );
    assert!(
        assembly.contains(".export ROWS : abs = $0019"),
        "the size is stated:\n{assembly}"
    );

    let k816_asm = dir.join("defs.s");
    std::fs::write(&k816_asm, &assembly).expect("write the K816 assembly");
    // A hand-written consumer, spelling the import the ordinary way.
    let hand_asm = dir.join("user.s");
    std::fs::write(
        &hand_asm,
        // `#<` because an absolute-sized symbol does not fit an eight-bit
        // immediate, which is cc65's own idiom for a link-time value used as
        // a byte (`libsrc/common/_heap.s` writes `lda #<__STACKSIZE__`).
        ".setcpu \"65816\"\n.import ROWS\n.export user\nuser:\n        lda #<ROWS\n        rts\n",
    )
    .expect("write the hand-written module");

    for (source, object) in [(&k816_asm, "defs.o"), (&hand_asm, "user.o")] {
        common::run_ok(
            std::process::Command::new(common::tool("CA65", "ca65"))
                .arg(source)
                .args(["-o".as_ref(), dir.join(object).as_os_str()]),
            "ca65",
        );
    }
    let config = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden/slice.cfg");
    let output = std::process::Command::new(common::tool("LD65", "ld65"))
        .args(["-C".as_ref(), config.as_os_str()])
        .args(["-o".as_ref(), dir.join("out.bin").as_os_str()])
        .arg(dir.join("defs.o"))
        .arg(dir.join("user.o"))
        .output()
        .expect("run ld65");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "ld65 failed: {stderr}");
    assert!(
        !stderr.contains("Address size mismatch"),
        "ld65 disagreed about `ROWS`: {stderr}"
    );
}

/// The importing golden pair assembles and links, and the contract crossed.
///
/// Two units that name each other rather than being named together: nothing
/// outside the sources says they belong to one program. The bytes are the
/// claim — `lda #$0002` is three bytes only if `widen`'s declared exit
/// reached the caller across the boundary, and `lda cursor` is two only if
/// the sibling's `.importzp` did.
#[test]
fn the_import_pair_links_through_ld65() {
    let dir = common::work_dir("import-pair");
    let golden = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden");

    let mut map = k816::source::SourceMap::new();
    let entry_path = golden.join("import-main.k816");
    map.add_file_at(
        entry_path.to_string_lossy(),
        &entry_path,
        std::fs::read_to_string(&entry_path).expect("read the entry"),
    );
    // Nothing names the sibling: the loader reads the source and finds it.
    // Both units are *reached*, because the pair imports each other — a
    // cycle is legal, and the loader answers what the edges point at rather
    // than what it happened to open.
    let reached = k816::load_imports(&mut map, &mut |path| std::fs::read_to_string(path));
    assert_eq!(
        reached.len(),
        2,
        "each unit is reached by the other's import"
    );
    let sibling = *reached
        .iter()
        .find(|file| map.file(**file).name().ends_with("import-lib.k816"))
        .expect("the sibling was loaded");
    // An interface unit is reached the same way, and adds nothing to the
    // object: an extern nothing references needs no line.
    let iface_path = golden.join("import-iface.k816");
    let iface = map.add_file_at(
        iface_path.to_string_lossy(),
        &iface_path,
        std::fs::read_to_string(&iface_path).expect("read the interface unit"),
    );

    let mut objects = Vec::new();
    for (index, file) in [map.locate(&entry_path).expect("the entry"), sibling, iface]
        .into_iter()
        .enumerate()
    {
        let result = compile_to_ca65(&map, file, &Options::default());
        assert_diagnostics(&result.diagnostics, false);
        let assembly = result.assembly.expect("compiles");
        if file == iface {
            assert!(
                !assembly.contains(".import") && !assembly.contains("        lda"),
                "an interface unit emits no code and imports nothing:\n{assembly}"
            );
        }
        let asm = dir.join(format!("unit{index}.s"));
        let obj = dir.join(format!("unit{index}.o"));
        std::fs::write(&asm, &assembly).expect("write assembly");
        common::run_ok(
            std::process::Command::new(common::tool("CA65", "ca65"))
                .arg(&asm)
                .args(["-o".as_ref(), obj.as_os_str()]),
            "ca65",
        );
        objects.push(obj);
    }

    let config = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/driver.cfg");
    let bin = dir.join("out.bin");
    let mut link = std::process::Command::new(common::tool("LD65", "ld65"));
    link.args(["-C".as_ref(), config.as_os_str()])
        .args(["-o".as_ref(), bin.as_os_str()]);
    for object in &objects {
        link.arg(object);
    }
    common::run_ok(&mut link, "ld65");
    assert!(
        std::fs::metadata(&bin).expect("the linked image").len() > 0,
        "the pair links"
    );
}
