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
    assert!(assembly.contains("sta     SCREEN"));
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
