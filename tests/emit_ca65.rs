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

fn emit(name: &str, text: &str) -> String {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "unexpected diagnostics");
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
    assert_eq!(instruction_lines.len(), 6);
    for line in &instruction_lines {
        assert!(
            line.contains("; slice.k816:") || line.contains("; k816:"),
            "instruction lacks an annotation: {line}"
        );
    }

    // One `.dbg line` directive precedes every instruction.
    let dbg_lines = assembly
        .lines()
        .filter(|line| line.trim_start().starts_with(".dbg    line"))
        .count();
    assert_eq!(dbg_lines, 6);

    // The debug file table names the K816 source.
    assert!(assembly.contains(".dbg    file, \"slice.k816\", 0, 0"));
}

#[test]
fn functions_are_exported_for_cross_module_references() {
    let assembly = emit("slice.k816", include_str!("golden/slice.k816"));
    assert!(assembly.contains("\n.export main\n"));
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

#[test]
fn width_directives_track_the_entry_contract() {
    let assembly = emit("slice.k816", include_str!("golden/slice.k816"));
    let main_at = assembly.find("main:").expect("main label");
    let a8_at = assembly.find("        .a8\n").expect(".a8 directive");
    let i16_at = assembly.find("        .i16\n").expect(".i16 directive");
    let first_dbg = assembly[main_at..].find(".dbg    line").unwrap() + main_at;
    assert!(main_at < a8_at && a8_at < first_dbg);
    assert!(main_at < i16_at && i16_at < first_dbg);
}

// ---------------------------------------------------------------------------
// Label emission (sub-plan C, task C2 with backend decision 4).
// ---------------------------------------------------------------------------

#[test]
fn local_labels_become_ca65_cheap_locals() {
    let assembly = emit(
        "labels.k816",
        "var SCREEN = $0400\nfunc main @a8 @i16 {\n  .again:\n    lda #1\n    sta .again\n}\n",
    );
    assert!(assembly.contains("\n@again:\n"), "{assembly}");
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
        "func main @a8 @i16 {\n  .again:\n    lda #1\n  done:\n    sta .again\n}\n",
    );
    assert!(assembly.contains("\nmain_again:\n"), "{assembly}");
    assert!(assembly.contains("\ndone:\n"), "{assembly}");
    assert!(assembly.contains("sta     a:main_again"), "{assembly}");
    assert!(
        !assembly.contains('@'),
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
        assembly.matches("\n@loop:\n").count(),
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
        // Directives and equates are their own tests' business.
        .filter(|code| !code.is_empty() && !code.starts_with('.') && !code.contains(":="))
        .collect()
}

fn emit_unoptimized(name: &str, text: &str) -> String {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "unexpected diagnostics");
    result.assembly.expect("compilation succeeds")
}

#[test]
fn every_addressing_mode_emits_its_ca65_syntax() {
    let text = "var ZP = $80
var SCREEN = $0400
const FIELD = 2

func main @a8 @i16 {
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
    per main
    brk #$42
    cop #$7F
    mvn $01,$02
    jml [SCREEN]
    jmp (SCREEN)
    jsr main
    bne main
}
";
    assert_eq!(
        instructions("modes.k816", text),
        vec![
            "sep     #$20",
            "rep     #$10",
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
            "per     main",
            "brk     $42",
            "cop     $7F",
            "mvn     $01,$02",
            "jml     [SCREEN]",
            "jmp     (SCREEN)",
            "jsr     a:main",
            "bne     main",
            "rts",
        ]
    );
}

#[test]
fn expressions_use_ca65s_named_operators_and_explicit_grouping() {
    let text = "func main @a16 @i16 {
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
            "rep     #$20",
            "rep     #$10",
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
    let text = "func main @a16 @i16 {\n  spot:\n    lda spot + 2 & $FF\n    lda spot + 2\n}\n";
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
        "func main @a8 @i16 {\n    plp\n    sep #$20\n}\n",
    );
    let claims: Vec<&str> = assembly
        .lines()
        .map(str::trim)
        .filter(|line| line.starts_with(".a") || line.starts_with(".i"))
        .collect();
    // The entry contract's two claims, then `sep #$20` re-establishing the
    // accumulator width after `plp` made it unknown. Nothing claims a width
    // between the `plp` and the `sep`.
    assert_eq!(claims, vec![".a8", ".i16", ".a8"], "{assembly}");
}
