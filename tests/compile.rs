/*****************************************************************************/
/*                                                                           */
/*                                 compile.rs                                */
/*                                                                           */
/*        End-to-end ca65/ld65 verification: bytes and source ownership      */
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

//! Assembles and links the slice through the real ca65/ld65 (master plan
//! Phase 5): the linked program has the expected bytes and the debug
//! database records K816 source ownership.
//!
//! The tools are located in the cc65 tree's `bin/` (`../../bin` from this
//! crate), overridable via the `CA65`/`LD65` environment variables.

mod common;

use std::path::PathBuf;
use std::process::Command;

use common::{run_ok, tool, work_dir};
use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

/// Compiles `text`, assembles, links, and returns (binary bytes, debug file).
fn compile_and_link(test: &str, name: &str, text: &str, options: &Options) -> (Vec<u8>, String) {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, options);
    assert_eq!(result.diagnostics, vec![], "unexpected diagnostics");
    let assembly = result.assembly.expect("compilation succeeds");

    let dir = work_dir(test);
    let asm_path = dir.join("out.s");
    let obj_path = dir.join("out.o");
    let bin_path = dir.join("out.bin");
    let dbg_path = dir.join("out.dbg");
    std::fs::write(&asm_path, assembly).expect("write assembly");

    run_ok(
        Command::new(tool("CA65", "ca65"))
            .arg(&asm_path)
            .args(["-o".as_ref(), obj_path.as_os_str()]),
        "ca65 (build it with `make -C ../.. ca65` first)",
    );

    let config = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden/slice.cfg");
    run_ok(
        Command::new(tool("LD65", "ld65"))
            .args(["-C".as_ref(), config.as_os_str()])
            .args(["-o".as_ref(), bin_path.as_os_str()])
            .args(["--dbgfile".as_ref(), dbg_path.as_os_str()])
            .arg(&obj_path),
        "ld65 (build it with `make -C ../.. ld65` first)",
    );

    (
        std::fs::read(&bin_path).expect("read linked binary"),
        std::fs::read_to_string(&dbg_path).expect("read debug file"),
    )
}

/// The unoptimized slice: entry setup, both loads, store, return.
///
/// Only the `REP` half of `main @a8 @i16` is emitted. The CPU is already
/// 8-bit after reset, so a `SEP` at entry would be redundant
/// (`doc/register-width-aware-syntax.md`, "Entry-point mode setup"); the
/// 8-bit half is claimed by a mode assumption instead, which is why the
/// `lda #$00` below is still the two-byte form.
const UNOPTIMIZED_BYTES: &[u8] = &[
    0xC2, 0x10, // rep #$10  — entry @i16, the only half worth emitting
    0xA9, 0x00, // lda #$00  — the dead load (removed only by the optimizer)
    0xA9, 0x01, // lda #$01
    0x8D, 0x00, 0x04, // sta $0400
    0x60, // rts
];

#[test]
fn hla_slice_links_to_the_expected_bytes() {
    let (bytes, _) = compile_and_link(
        "hla-unopt",
        "slice.k816",
        include_str!("golden/slice.k816"),
        &Options { optimize: false },
    );
    assert_eq!(bytes, UNOPTIMIZED_BYTES);
}

#[test]
fn native_slice_links_to_identical_bytes() {
    let (bytes, _) = compile_and_link(
        "native-unopt",
        "slice-native.k816",
        include_str!("golden/slice-native.k816"),
        &Options { optimize: false },
    );
    assert_eq!(bytes, UNOPTIMIZED_BYTES);
}

/// The optimized slice: the dead `lda #$00` is gone, nothing else changes.
const OPTIMIZED_BYTES: &[u8] = &[
    0xC2, 0x10, // rep #$10  — entry @i16
    0xA9, 0x01, // lda #$01
    0x8D, 0x00, 0x04, // sta $0400
    0x60, // rts
];

#[test]
fn optimized_slice_links_without_the_dead_load() {
    let (hla, _) = compile_and_link(
        "hla-opt",
        "slice.k816",
        include_str!("golden/slice.k816"),
        &Options { optimize: true },
    );
    assert_eq!(hla, OPTIMIZED_BYTES);

    let (native, _) = compile_and_link(
        "native-opt",
        "slice-native.k816",
        include_str!("golden/slice-native.k816"),
        &Options { optimize: true },
    );
    assert_eq!(native, OPTIMIZED_BYTES);
}

#[test]
fn debug_database_records_k816_source_ownership() {
    let (_, dbg) = compile_and_link(
        "hla-dbg",
        "slice.k816",
        include_str!("golden/slice.k816"),
        &Options { optimize: false },
    );

    // The K816 source is in the file table...
    assert!(
        dbg.lines()
            .any(|line| line.starts_with("file") && line.contains("name=\"slice.k816\"")),
        "missing K816 file entry in:\n{dbg}"
    );
    let file_id = dbg
        .lines()
        .find(|line| line.starts_with("file") && line.contains("name=\"slice.k816\""))
        .and_then(|line| {
            line.split(',')
                .find_map(|field| field.strip_prefix("file\tid="))
        })
        .expect("file id")
        .to_owned();

    // ...and every slice source line 3..=7 owns generated code.
    for line_number in 3..=7 {
        assert!(
            dbg.lines().any(|line| {
                line.starts_with("line")
                    && line.contains(&format!("file={file_id}"))
                    && line.contains(&format!("line={line_number}"))
                    && line.contains("type=1")
            }),
            "line {line_number} has no ownership record in:\n{dbg}"
        );
    }
}

// ---------------------------------------------------------------------------
// ca65 expression emission (sub-plan C, backend decision 1).
// ---------------------------------------------------------------------------

/// The failure this guards against produces *wrong numbers in a working
/// program*, not an error message, so it is checked on the linked bytes.
///
/// ca65's precedence table is not K65's: `&`, `^`, `<<`, and `>>` sit at the
/// same level as `*` there, and `|` at the same level as `+`. So
/// `spot + 1 & $FF` means `(spot + 1) & $FF` in K65 and `spot + (1 & $FF)`
/// in ca65 — the same text, two different addresses. Emitting the K65 tree
/// with explicit grouping is what keeps the two agreeing.
#[test]
fn a_mixed_precedence_expression_links_to_the_k65_value() {
    // `spot` is a label, so ld65 places it and evaluates the expression
    // after placement: nothing here is folded by K816.
    let (bytes, _) = compile_and_link(
        "precedence",
        "precedence.k816",
        "func main @a16 @i16 {\n  spot:\n    lda #spot + 1 & $FF\n}\n",
        &Options { optimize: false },
    );

    // `main` is the entry point, so its two width-setup instructions come
    // first; `spot` is therefore at $8004, the config placing CODE at $8000.
    assert_eq!(
        &bytes[..4],
        &[0xC2, 0x20, 0xC2, 0x10],
        "entry width setup: {bytes:02X?}"
    );
    assert_eq!(bytes[4], 0xA9, "lda immediate: {bytes:02X?}");
    assert_eq!(
        u16::from_le_bytes([bytes[5], bytes[6]]),
        0x0005,
        "K65 groups this as `(spot + 1) & $FF` = ($8004 + 1) & $FF = 5; \
         ca65's own precedence would have given `spot + (1 & $FF)` = $8005"
    );
    assert_eq!(bytes[7], 0x60, "implicit rts");
}

/// ca65 stores an unresolved expression in the object file and ld65
/// evaluates it after placement, so K816 does not reject `label * 2` merely
/// because a label is involved (sub-plan C, backend decision 10).
#[test]
fn a_link_time_multiplication_survives_to_ld65() {
    let (bytes, _) = compile_and_link(
        "relocatable-multiply",
        "reloc.k816",
        "func main @a16 @i16 {\n  spot:\n    lda #&<spot * 2\n}\n",
        &Options { optimize: false },
    );
    assert_eq!(bytes[4], 0xA9, "{bytes:02X?}");
    // `spot` lands at $8004 after the entry setup, so the low byte is $04
    // and the product is 8. The point is that the multiplication reached
    // ld65 at all rather than being rejected for naming a label.
    assert_eq!(u16::from_le_bytes([bytes[5], bytes[6]]), 0x0008);
}

// ---------------------------------------------------------------------------
// Derived linkage across units (sub-plan C, task C7).
// ---------------------------------------------------------------------------

/// Compiles `main`, assembles both units, links them, and returns the bytes.
///
/// K65 has no import syntax, so the two units are handed to the compiler
/// together and it works out what crosses between them.
fn link_two_units(test: &str, main: (&str, &str), other: (&str, &str)) -> Vec<u8> {
    let mut map = SourceMap::new();
    let main_file = map.add_file(main.0, main.1);
    let other_file = map.add_file(other.0, other.1);

    let dir = work_dir(test);
    let mut objects = Vec::new();
    for (name, file) in [(main.0, main_file), (other.0, other_file)] {
        let result = compile_to_ca65(&map, file, &Options { optimize: false });
        assert_eq!(
            result.diagnostics,
            vec![],
            "unexpected diagnostics in {name}"
        );
        let assembly = result.assembly.expect("compilation succeeds");
        let asm_path = dir.join(format!("{name}.s"));
        let obj_path = dir.join(format!("{name}.o"));
        std::fs::write(&asm_path, assembly).expect("write assembly");
        run_ok(
            Command::new(tool("CA65", "ca65"))
                .arg(&asm_path)
                .args(["-o".as_ref(), obj_path.as_os_str()]),
            "ca65",
        );
        objects.push(obj_path);
    }

    let config = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden/slice.cfg");
    let bin_path = dir.join("two-units.bin");
    let mut link = Command::new(tool("LD65", "ld65"));
    link.args(["-C".as_ref(), config.as_os_str()])
        .args(["-o".as_ref(), bin_path.as_os_str()]);
    for object in &objects {
        link.arg(object);
    }
    run_ok(&mut link, "ld65");
    std::fs::read(&bin_path).expect("read linked binary")
}

/// The import and export qualifiers are not decoration: ca65 validates an
/// addressing mode against the imported symbol's address size, so a
/// direct-page reference across a unit boundary assembles as one byte only
/// if the import says `zeropage`.
#[test]
fn derived_linkage_carries_each_symbols_address_size() {
    let bytes = link_two_units(
        "two-units",
        (
            "a.k816",
            "func main @a8 @i16 {\n    jsr helper\n    lda CURSOR\n    lda BANKED\n}\n",
        ),
        (
            "b.k816",
            "var dp CURSOR = $10\nvar far BANKED = $030000\nfunc helper @a8 {\n    nop\n}\n",
        ),
    );

    assert_eq!(
        bytes,
        vec![
            0xC2, 0x10, // rep #$10  — entry @i16; @a8 needs no instruction
            0x20, 0x0C, 0x80, // jsr helper — absolute, an ordinary import
            0xA5, 0x10, // lda $10    — one byte, from `.importzp`
            0xAF, 0x00, 0x00, 0x03, // lda $030000 — three, from `: far`
            0x60, // rts
            0xEA, 0x60, // helper: nop, rts
        ],
        "{bytes:02X?}"
    );
}

#[test]
fn two_units_defining_one_name_is_a_frontend_error() {
    let mut map = SourceMap::new();
    let file = map.add_file("a.k816", "func shared @a8 {\n    nop\n}\n");
    map.add_file("b.k816", "func shared @a8 {\n    nop\n}\n");

    let result = compile_to_ca65(&map, file, &Options::default());
    let duplicates: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.code == k816::diagnostic::DiagnosticCode::DUPLICATE_SYMBOL)
        .collect();
    assert_eq!(duplicates.len(), 1, "{:?}", result.diagnostics);
    assert_eq!(
        duplicates[0].secondary.len(),
        1,
        "the other definition is pointed at too"
    );
    assert!(result.assembly.is_none(), "nothing is emitted");
}

// ---------------------------------------------------------------------------
// The end-to-end gates (sub-plan C, task C9).
// ---------------------------------------------------------------------------

/// Compiles `path` and returns the emitted assembly.
fn compile_fixture(name: &str, text: &str) -> String {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    let errors: Vec<_> = result.diagnostics.iter().filter(|d| d.is_error()).collect();
    assert!(errors.is_empty(), "{name}: {errors:?}");
    result.assembly.expect("compilation succeeds")
}

/// Assembles `assembly` and links it with `config`, returning the bytes.
fn assemble_and_link(test: &str, assembly: &str, config: &str) -> Vec<u8> {
    let dir = work_dir(test);
    let asm_path = dir.join("out.s");
    let obj_path = dir.join("out.o");
    let bin_path = dir.join(format!("{config}.bin"));
    std::fs::write(&asm_path, assembly).expect("write assembly");
    run_ok(
        Command::new(tool("CA65", "ca65"))
            .arg(&asm_path)
            .args(["-o".as_ref(), obj_path.as_os_str()]),
        "ca65",
    );
    let config = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/golden")
        .join(format!("{config}.cfg"));
    run_ok(
        Command::new(tool("LD65", "ld65"))
            .args(["-C".as_ref(), config.as_os_str()])
            .args(["-o".as_ref(), bin_path.as_os_str()])
            .arg(&obj_path),
        "ld65",
    );
    std::fs::read(&bin_path).expect("read linked binary")
}

/// The operand-vocabulary gate: every addressing mode K816 claims, written
/// in K65 rather than synthesized from the table, assembles and links.
#[test]
fn native_modes_assembles_and_links() {
    let assembly = compile_fixture(
        "native-modes.k816",
        include_str!("golden/native-modes.k816"),
    );
    let bytes = assemble_and_link("native-modes", &assembly, "slice");
    assert!(!bytes.is_empty(), "the fixture links to something");
}

/// **K816 never resolves an address owned by ld65**, proven rather than
/// asserted: the same object file, linked against two configs that place it
/// differently, gives two different binaries — and the assembly K816 emitted
/// is byte-identical either way, because it never contained an address at
/// all.
#[test]
fn the_emitted_assembly_does_not_depend_on_where_ld65_puts_it() {
    let assembly = compile_fixture(
        "native-reloc.k816",
        include_str!("golden/native-reloc.k816"),
    );
    // Compiling is deterministic and config-blind: nothing K816 does can
    // depend on a linker config it never reads.
    let again = compile_fixture(
        "native-reloc.k816",
        include_str!("golden/native-reloc.k816"),
    );
    assert_eq!(assembly, again);

    let low = assemble_and_link("reloc-low", &assembly, "reloc-low");
    let high = assemble_and_link("reloc-high", &assembly, "reloc-high");
    assert_eq!(low.len(), high.len(), "the same program, placed twice");
    assert_ne!(
        low, high,
        "if the two binaries matched, no address in them came from ld65"
    );
}

/// The two-unit pair as committed fixtures, rather than as strings in a
/// test: they are what a reader looks at to see what derived linkage means.
#[test]
fn the_committed_unit_pair_links() {
    let bytes = link_two_units(
        "unit-pair",
        ("unit-a.k816", include_str!("golden/unit-a.k816")),
        ("unit-b.k816", include_str!("golden/unit-b.k816")),
    );
    assert_eq!(
        bytes,
        vec![
            0xC2, 0x10, // rep #$10 — entry @i16; @a8 needs no instruction
            0x20, 0x0E, 0x80, // jsr helper
            0xA5, 0x10, // lda $10       — one byte, from `.importzp`
            0xAF, 0x00, 0x00, 0x03, // lda $030000 — three, from `: far`
            0x85, 0x10, // sta $10
            0x60, // rts
            0xEA, 0x60, // helper: nop, rts
        ],
        "{bytes:02X?}"
    );
}

/// The legacy-source gate. 213 lines of real `var NAME = 0xADDR;`
/// declarations, semicolons and all, have to analyze with *zero*
/// diagnostics — and every symbol has to get the address size a reference
/// to it would encode.
#[test]
fn the_legacy_hardware_definitions_analyze_clean() {
    use k816::symbol::{AddressSize, SymbolKind};

    let text = include_str!("../examples/k65/atari-xl/include/defs.k65");
    let mut map = SourceMap::new();
    let file = map.add_file("defs.k65", text);
    let result = compile_to_ca65(&map, file, &Options::default());
    assert_eq!(result.diagnostics, vec![], "defs.k65 is not clean");

    let parse = k816::parser::parse_source(map.file(file), None);
    let (model, _) = k816::semantic::analyze_file(&parse.file);
    let mut variables = 0;
    for (_, symbol) in model.symbols.iter() {
        let SymbolKind::Var { address, size, .. } = symbol.kind else {
            continue;
        };
        variables += 1;
        let address = address.expect("every declaration here is pinned");
        // They are all `$Dxxx` hardware registers and `$02xx` OS vectors,
        // so every one of them is absolute — none auto-shrinks to page zero
        // and none needs a bank byte.
        assert_eq!(
            size,
            AddressSize::Absolute,
            "{} at ${address:04X}",
            symbol.name
        );
    }
    assert!(variables > 100, "only {variables} declarations were read");
}

/// The data half of the `dli-example`: `nocross`, raw bytes, and
/// address-byte operators over both an external label and the block's own.
/// Its `naked` and `main` bodies stay sub-plan E's, so the block is
/// compiled on its own.
#[test]
fn the_legacy_display_list_data_block_compiles() {
    let source = include_str!("../examples/k65/atari-xl/dli-example/main.k65");
    let block = source
        .split_once("data displist")
        .map(|(_, rest)| rest.split_once("\n}").expect("the block closes").0)
        .expect("the display list is still there");
    let text = format!("var screen = 0x8000\n\ndata displist{block}\n}}\n");

    let assembly = compile_fixture("displist.k65", &text);
    // The block's own label, an address-byte operator over an external
    // symbol, and one over the block itself.
    assert!(assembly.contains("displist:"), "{assembly}");
    assert!(assembly.contains(".byte    .lobyte(screen)"), "{assembly}");
    assert!(
        assembly.contains(".byte    .hibyte(displist)"),
        "{assembly}"
    );

    // The list is 17 bytes and opens with `nocross`, so every one of them is
    // in the guarded run — including the first `0x70`, which the grammar
    // used to swallow as a run length.
    assert_eq!(
        assembly.matches(".byte").count(),
        17,
        "no byte of the display list may go missing: {assembly}"
    );
    assert!(
        assembly.contains(".hibyte(* - 1) = .hibyte(* - 17)"),
        "the guard covers the whole list: {assembly}"
    );

    // …and it assembles and links, guard included.
    assemble_and_link("displist", &assembly, "slice");
}

// ---------------------------------------------------------------------------
// Immediate sizes agree with tracked modes (sub-plan D, phase 7 criterion).
// ---------------------------------------------------------------------------

/// Assembles `text` and returns the linked bytes, without asserting a layout.
///
/// The gate below is on *encoded lengths*, so it has to go through the tool
/// that owns encoding. Comparing emitted text would pass on output that ca65
/// then sizes differently, which is exactly the class of mistake the width
/// model exists to prevent.
fn linked(test: &str, name: &str, text: &str) -> Vec<u8> {
    let (bytes, _) = compile_and_link(test, name, text, &Options { optimize: false });
    bytes
}

/// The regression the emitter used to have: a branch that skips a mode change
/// left the assembler believing the skipped width, so the immediate after the
/// label was assembled three bytes wide and everything after it shifted.
#[test]
fn a_skipped_mode_change_does_not_widen_the_immediate_after_the_label() {
    let bytes = linked(
        "width-branch",
        "width-branch.k816",
        include_str!("golden/width-branch.k816"),
    );

    // `skipped`: lda #$01 (2), bra (2), rep #$20 (2), lda #$01 (2), rts (1).
    assert_eq!(
        &bytes[..9],
        &[0xA9, 0x01, 0x80, 0x02, 0xC2, 0x20, 0xA9, 0x01, 0x60],
        "the load after the label is the two-byte form: {bytes:02X?}"
    );

    // `taken`: the same mode change *before* the branch is on every path, so
    // the same source line assembles three bytes wide.
    assert_eq!(
        &bytes[9..19],
        &[0xA9, 0x01, 0xC2, 0x20, 0x80, 0x00, 0xA9, 0x01, 0x00, 0x60],
        "the load after the label is the three-byte form: {bytes:02X?}"
    );

    // `resolved`: the annotation at the label bridges both arrivals to 16-bit,
    // so the load is three bytes and the bridge is adjacent to the label.
    assert_eq!(
        &bytes[19..],
        &[0x90, 0x02, 0xC2, 0x20, 0xC2, 0x20, 0xA9, 0x02, 0x01, 0x60],
        "{bytes:02X?}"
    );
}

/// A plain `rep` before an immediate is believed: the tracked state, not the
/// entry contract, decides the size.
#[test]
fn a_rep_before_an_immediate_widens_it() {
    let bytes = linked(
        "width-rep",
        "rep.k816",
        "func main @a8 {\n    rep #$20\n    lda #$1234\n}\n",
    );
    assert_eq!(
        bytes,
        vec![0xC2, 0x20, 0xA9, 0x34, 0x12, 0x60],
        "{bytes:02X?}"
    );
}

/// Module defaults reach the encoded length: the same `#1` is three bytes in
/// the inheriting function and two in the one that overrides.
#[test]
fn module_defaults_reach_the_encoded_immediate_length() {
    let bytes = linked(
        "width-module",
        "width-module.k816",
        include_str!("golden/width-module.k816"),
    );
    assert_eq!(
        bytes,
        vec![
            // inherits: lda #$0001, ldx #$0001, rts
            0xA9, 0x01, 0x00, 0xA2, 0x01, 0x00, 0x60, // narrow: lda #$01, ldx #$01, rts
            0xA9, 0x01, 0xA2, 0x01, 0x60,
            // half: lda #$01 (overridden), ldx #$0001 (inherited), rts
            0xA9, 0x01, 0xA2, 0x01, 0x00, 0x60,
        ],
        "{bytes:02X?}"
    );
}

/// A loop head every arrival agrees about keeps its width all the way to the
/// encoded byte.
#[test]
fn a_loop_head_its_arrivals_agree_about_keeps_its_width() {
    let bytes = linked(
        "width-loop",
        "width-loop.k816",
        include_str!("golden/width-loop.k816"),
    );
    assert_eq!(
        bytes,
        vec![
            // diverging: nop, rep #$20, bra .head
            0xEA, 0xC2, 0x20, 0x80, 0xFB, 0x60,
            // agreeing: lda #$01 — two bytes, from the 8-bit head
            0xA9, 0x01, 0xE2, 0x20, 0x80, 0xFA, 0x60,
        ],
        "{bytes:02X?}"
    );
}

/// A scoped block's width reaches the encoded length inside it and is gone
/// again after it.
#[test]
fn a_scoped_block_changes_the_encoded_length_only_inside_itself() {
    let bytes = linked(
        "width-block-bytes",
        "block.k816",
        "func main @a8 {\n    lda #$01\n    @a16 {\n        lda #$01\n    }\n    lda #$01\n}\n",
    );
    assert_eq!(
        bytes,
        vec![
            0xA9, 0x01, // lda #$01 — 8-bit
            0xC2, 0x20, // rep #$20 — enter the block
            0xA9, 0x01, 0x00, // lda #$0001 — 16-bit inside it
            0xE2, 0x20, // sep #$20 — restore
            0xA9, 0x01, // lda #$01 — 8-bit again
            0x60,
        ],
        "{bytes:02X?}"
    );
}

/// The documented call-site bridging example, checked as bytes.
///
/// Each bridge is the minimum for its callee's contract: one instruction per
/// axis that differs, none for an axis the callee leaves unspecified, and
/// none at all for the uncoloured callee. The masks are unfolded — merging
/// `rep #$20` and `rep #$10` into `rep #$30` is the optimizer's, so the
/// document's listing shows fewer instructions than this.
#[test]
fn call_site_bridges_are_the_minimum_for_each_contract() {
    let bytes = linked(
        "width-bridging",
        "width-bridging.k816",
        include_str!("golden/width-bridging.k816"),
    );
    // The four callees come first: `nop`, `rts` each.
    let main_at = 4 * 2;
    assert_eq!(
        &bytes[main_at..main_at + 20],
        &[
            0xC2, 0x20, // rep #$20 — bridge for wide_a @a16
            0x20, 0x00, 0x80, // jsr wide_a
            0xE2, 0x20, // sep #$20 — bridge for narrow @a8 @i8
            0xE2, 0x10, // sep #$10
            0x20, 0x02, 0x80, // jsr narrow
            0xC2, 0x20, // rep #$20 — bridge for wide_all @a16 @i16
            0xC2, 0x10, // rep #$10
            0x20, 0x04, 0x80, // jsr wide_all
            0x20, // jsr uncolored — no bridge at all
        ],
        "{bytes:02X?}"
    );
}

/// A summary taken before cross-unit resolution is not merely imprecise, it
/// is unsound.
///
/// `wrapper @a16` calls a `foreign` that leaves the accumulator 8-bit.
/// Analyzed alone, `foreign` does not resolve, its call falls back to the
/// language's preserving rule, and `wrapper` looks like it exits 16-bit —
/// so a caller sizes its next immediate one byte too wide and every byte
/// after it shifts. The workspace phase classifies exits over the whole
/// call graph, so what a unit exports and what it emits are one answer.
#[test]
fn a_cross_unit_exit_reaches_the_callers_encoded_immediate() {
    let bytes = link_two_units(
        "cross-unit-exit",
        (
            "contract-unit-b.k816",
            include_str!("golden/contract-unit-b.k816"),
        ),
        (
            "contract-unit-a.k816",
            include_str!("golden/contract-unit-a.k816"),
        ),
    );
    assert_eq!(
        bytes,
        vec![
            // wrapper: jsr foreign, rts
            0x20, 0x0A, 0x80, 0x60,
            // third: jsr wrapper, lda #$01 — two bytes, from `foreign`'s exit
            0x20, 0x00, 0x80, 0xA9, 0x01, 0x60, // foreign: sep #$20, rts
            0xE2, 0x20, 0x60,
        ],
        "{bytes:02X?}"
    );
}

/// The exported contract and the emitted code come from the same run.
///
/// The summary a unit hands the workspace is the one its own code was
/// generated against; if the two could differ, a caller would bridge from
/// one and size its immediates from the other.
#[test]
fn an_exported_contract_matches_the_code_the_unit_emits() {
    use k816::linkage::Linkage;
    use k816::op::{AxisExit, RegisterWidth};

    let mut map = SourceMap::new();
    map.add_file(
        "contract-unit-a.k816",
        include_str!("golden/contract-unit-a.k816"),
    );
    let b = map.add_file(
        "contract-unit-b.k816",
        include_str!("golden/contract-unit-b.k816"),
    );
    let linkage = Linkage::resolve(&map);

    // `wrapper` exports the exit it inherits from `foreign`, not the
    // `Preserve` a single-unit summary would produce.
    let contract = linkage
        .contract("wrapper")
        .expect("wrapper has a classified contract");
    assert_eq!(contract.exit.a, AxisExit::Fixed(RegisterWidth::W8));
    assert_eq!(
        linkage.contract("foreign").expect("foreign").exit.a,
        AxisExit::Fixed(RegisterWidth::W8)
    );

    // And the code the unit emits says the same thing: the load after the
    // call to `wrapper` sits under an 8-bit claim.
    let result = compile_to_ca65(&map, b, &Options { optimize: false });
    let assembly = result.assembly.expect("compiles");
    let call = assembly.find("jsr     a:wrapper").expect("the call");
    let load = assembly.find("lda     #$01").expect("the load");
    let claim = assembly[call..load].find("        .a8").is_some();
    assert!(
        claim,
        "the callee's exit is claimed before the load: {assembly}"
    );
}

/// A branch whose target the relative form cannot reach still assembles.
///
/// This is the fixture whose whole purpose is that ca65 accepts it: the
/// assembler reports a range error rather than widening a branch itself, so a
/// compiler that emits the short form and hopes produces a program that does
/// not build (sub-plan E, decision 7).
#[test]
fn a_branch_past_a_relative_branchs_reach_still_assembles() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "branch-range.k816",
        include_str!("golden/branch-range.k816"),
    );
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "the fixture must compile");
    let assembly = result.assembly.expect("compilation succeeds");

    // The long form is the inverted branch over a jump, so the emitted
    // conditional carries the branch the source did *not* write.
    assert!(
        assembly.contains("bcs") && assembly.contains("jmp"),
        "expected the inverted-branch-over-a-jump idiom:\n{assembly}"
    );
    assemble_and_link("branch_range", &assembly, "slice");
}

/// A transfer out of a scoped width block restores it on the taken edge, and
/// the result assembles and links.
#[test]
fn a_transfer_out_of_a_width_block_restores_it_on_the_taken_edge() {
    let mut map = SourceMap::new();
    let file = map.add_file("block-exit.k816", include_str!("golden/block-exit.k816"));
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "the fixture must compile");
    let assembly = result.assembly.expect("compilation succeeds");

    // Three exits, each with its own restore: the native branch, the
    // generated one, and the nested pair — which restores twice.
    let restores = assembly
        .matches("restoring the width the block changed")
        .count();
    assert_eq!(restores, 4, "one per axis left behind:\n{assembly}");
    assemble_and_link("block_exit", &assembly, "slice");
}

/// The jump family and the far family, through the assembler.
///
/// `jsl`/`rtl` pair up, `jml` carries a bank byte, and every `goto` shape
/// reaches its target — which is only really established by ca65 accepting
/// the operand sizes and ld65 resolving the far addresses.
#[test]
fn the_far_family_and_every_goto_shape_assemble() {
    let mut map = SourceMap::new();
    let file = map.add_file("far-calls.k816", include_str!("golden/far-calls.k816"));
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "the fixture must compile");
    let assembly = result.assembly.expect("compilation succeeds");

    // A far section is entered long and left long, both returns included.
    assert_eq!(
        assembly.matches("jsl     f:banked").count(),
        2,
        "both spellings of the far call:\n{assembly}"
    );
    assert_eq!(
        assembly.matches("        rtl").count(),
        2,
        "the written return and the implicit one:\n{assembly}"
    );
    // A near call to a near section is untouched by any of it.
    assert!(
        assembly.contains("jsr     a:calls_far"),
        "a near call stays near:\n{assembly}"
    );
    // The three `goto` shapes, plus the one that gets its length from the
    // target rather than from a keyword.
    for expected in [
        "jmp     a:@direct", // goto .direct
        "jmp     (vec)",     // goto (vec)
        "jml     f:banked",  // goto far banked, and the keyword-less tail jump
        "bne     @loop",     // != goto .loop
        "bcc     @done",     // < goto .done
    ] {
        assert!(
            assembly.contains(expected),
            "expected `{expected}`:\n{assembly}"
        );
    }
    // `goto banked` names a `far func`, so it is long without the keyword.
    assert_eq!(
        assembly.matches("jml     f:banked").count(),
        3,
        "`goto far`, the tail jump, and the conditional one:\n{assembly}"
    );
    // A conditional `goto` past the short form's reach widens like any other
    // branch K816 chose.
    assert!(
        assembly.contains("bcs     :+") && assembly.contains("jmp     a:@out"),
        "the distant conditional goto widened:\n{assembly}"
    );
    assemble_and_link("far_calls", &assembly, "slice");
}

/// A `far func` a caller reaches with `.export`/`.import` keeps its address
/// size across the unit boundary, and the mismatched convention is refused.
#[test]
fn the_near_far_convention_is_checked_across_units() {
    use k816::diagnostic::DiagnosticCode;

    let mut map = SourceMap::new();
    map.add_file("far-lib.k816", "far func lib_init @a8 @i8 {\n    nop\n}\n");
    let user = map.add_file(
        "far-user.k816",
        "func main @a8 @i8 {\n    call far lib_init\n}\n",
    );
    let result = compile_to_ca65(&map, user, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "the matching pair compiles");
    let assembly = result.assembly.expect("compilation succeeds");
    assert!(
        assembly.contains(".import lib_init: far"),
        "the callee's address size crosses the boundary:\n{assembly}"
    );
    assert!(
        assembly.contains("jsl     f:lib_init"),
        "a foreign far callee is still reached long:\n{assembly}"
    );

    // The same callee, called near: the frontend refuses it, which is the
    // check the legacy linker used to make.
    let mut map = SourceMap::new();
    map.add_file("far-lib.k816", "far func lib_init @a8 @i8 {\n    nop\n}\n");
    let user = map.add_file(
        "far-user.k816",
        "func main @a8 @i8 {\n    call lib_init\n}\n",
    );
    let result = compile_to_ca65(&map, user, &Options { optimize: false });
    let codes: Vec<_> = result.diagnostics.iter().map(|d| d.code).collect();
    assert_eq!(codes, vec![DiagnosticCode::CALL_CONVENTION_MISMATCH]);
}

/// Every declaration axis, through the assembler and the linker.
///
/// The claims worth making about a `var` are what it *reserves* and what a
/// reference to it *encodes*, and the two are independent: a pinned variable
/// becomes an equate and reserves nothing, an allocated one becomes a
/// reservation ld65 places, and neither of those facts follows from the
/// datum width. The link is the real gate — a direct-page reservation only
/// works if ca65 gave its label a direct-page address size, which it only
/// does if the segment said so.
#[test]
fn every_declaration_axis_reserves_and_encodes_what_it_declares() {
    let mut map = SourceMap::new();
    let file = map.add_file("vars.k816", include_str!("golden/vars.k816"));
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "the fixture must compile");
    let assembly = result.assembly.expect("compilation succeeds");

    // A pinned variable is an equate, whatever its storage class.
    for equate in [
        "mmio            := $2100",
        "meter           := $0010",
        "rom             := $C08000",
        "port            := $0080",
        "pointer         := $00F0",
    ] {
        assert!(
            assembly.contains(equate),
            "expected `{equate}`:\n{assembly}"
        );
    }
    // …and reserves nothing: an equate names an address someone else owns.
    assert!(
        !assembly.contains("mmio:"),
        "a pinned variable is not a label:\n{assembly}"
    );

    // An allocated one reserves exactly what its axes multiply out to.
    for (name, bytes) in [
        ("counter", "$01"), // one untyped datum
        ("w", "$02"),       // one word
        ("buffer", "$20"),  // a raw span of 32 bytes
        ("grid", "$08"),    // a raw span of 4 words
        ("slots", "$08"),   // 4 word elements
        ("pairs", "$0C"),   // 3 elements of 2 words
        ("late", "$01"),
        ("fast", "$01"),
        ("cursor", "$02"),
    ] {
        let reservation = format!("{name}:");
        let index = assembly
            .find(&reservation)
            .unwrap_or_else(|| panic!("`{name}` is allocated:\n{assembly}"));
        assert!(
            assembly[index..].contains(&format!(".res     {bytes}")),
            "`{name}` reserves {bytes}:\n{assembly}"
        );
    }

    // The direct page is a pool, not bytes in a segment: its reservations
    // are in a zeropage-typed segment of their own, and the `segment BSS`
    // two of them were written under did not move them.
    let zeropage = assembly
        .find(".segment \"ZEROPAGE\": zeropage")
        .expect("the pool has a segment of its own");
    for pooled in ["fast:", "cursor:"] {
        assert!(
            assembly[zeropage..].contains(pooled),
            "`{pooled}` belongs to the pool:\n{assembly}"
        );
    }
    assert!(
        assembly[..zeropage].contains("late:"),
        "an absolute allocation stays in the segment it was written in:\n{assembly}"
    );

    // The storage class decides the encoding, and it is not the data width.
    for expected in [
        "lda     a:mmio",    // absolute by magnitude
        "lda     z:meter",   // direct page by prefix
        "lda     f:rom",     // long by prefix
        "lda     a:port",    // the prefix outranks the page-zero value
        "lda     z:fast",    // an allocated pool slot, still one byte
        "lda     [pointer]", // 24 bits reach no register, so: a pointer
        "lda     f:w",       // a use-site view: `far` encoding, byte datum
    ] {
        assert!(
            assembly.contains(expected),
            "expected `{expected}`:\n{assembly}"
        );
    }

    // A subscript is address arithmetic, and the stride is the element —
    // which the raw span is part of and the count is not.
    for expected in [
        "lda     a:slots  ",     // slots[0] is slots, with no arithmetic after it
        "lda     a:slots + $06", // slots[3], striding one word
        "lda     a:grid + $02",  // a raw span has no elements: bytes
        "lda     a:pairs + $08", // 2 x (2 words)
        "lda     a:buffer + $08",
    ] {
        assert!(
            assembly.contains(expected),
            "expected `{}`:\n{assembly}",
            expected.trim_end()
        );
    }

    assemble_and_link("vars", &assembly, "vars");
}

/// What a field list declares, and what an access to one folds to.
///
/// The gate is the *numbers*: a layout is offsets, an access is a constant
/// added to a base, and a metadata query is that constant on its own. If
/// packing and access ever disagreed about one field, they would disagree
/// here first.
#[test]
fn every_field_access_folds_to_the_offset_its_layout_declares() {
    let mut map = SourceMap::new();
    let file = map.add_file("subscripts.k816", include_str!("golden/subscripts.k816"));
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "the fixture must compile");
    let assembly = result.assembly.expect("compilation succeeds");

    // A layout does not change what a declaration *is*: a pinned base is
    // still an equate, and an allocated one still reserves what its fields
    // and its count multiply out to.
    for equate in ["TASKS           := $4000", "COMP            := $1000"] {
        assert!(
            assembly.contains(equate),
            "expected `{equate}`:\n{assembly}"
        );
    }
    let sprites = assembly.find("sprites:").expect("an allocated layout");
    assert!(
        assembly[sprites..].contains(".res     $14"),
        "4 elements of (2 + 2 + 1):\n{assembly}"
    );
    let cursor = assembly.find("cursor:").expect("a pooled layout");
    assert!(
        assembly[cursor..].contains(".res     $05"),
        "the pool takes the layout's size, `:far` field included:\n{assembly}"
    );
    // An `abstract var` reserves nothing and names nothing: no label, no
    // equate, and no export. It appears in the emitted source only where the
    // line it came from is echoed as a comment.
    assert!(
        !assembly
            .lines()
            .any(|line| line.trim_start().starts_with("DEV_HANDLER")
                || line.contains(".export DEV_HANDLER")),
        "a layout with no storage emits nothing:\n{assembly}"
    );

    // `:sizeof` is one element and `:offsetof` is a field's distance from
    // the base of its variable — both integers by the time ca65 sees them.
    for (expected, source) in [
        ("lda     #$04", "a = TASKS:sizeof"),
        ("lda     #$03", "a = TASKS.signals:offsetof"),
        ("lda     #$02", "a = REGS.data.hi:offsetof"),
        ("lda     #$04", "a = baz.len:offsetof"),
        ("lda     #$06", "a = DEV_HANDLER:sizeof"),
        ("lda     #$04", "a = DEV_HANDLER.init:offsetof"),
    ] {
        let site = assembly
            .find(source)
            .unwrap_or_else(|| panic!("`{source}` is in the fixture:\n{assembly}"));
        let line = assembly[..site].rfind('\n').expect("a line start");
        assert!(
            assembly[line..site].contains(expected),
            "`{source}` folds to `{expected}`:\n{assembly}"
        );
    }

    // An access is the base plus a constant: the field's offset, the
    // element's stride, and the field element's own width, added up.
    for expected in [
        "sta     a:TASKS + $02",   // .state, after a word
        "sta     a:REGS  ",        // .status is first, so there is nothing to add
        "sta     a:baz + $04",     // two default-width fields precede `.len`
        "sta     z:cursor + $01",  // a pooled base keeps its one-byte encoding
        "lda     a:COMP + $10",    // COMP[2]: two 8-byte elements
        "lda     a:COMP + $16",    // COMP[2].str[3]: 16 + 3 + 3
        "lda     a:COMP + $19",    // COMP[3].two: 24 + 1
        "lda     a:COMP + $0F",    // COMP[1].str[4]: 8 + 3 + 4
        "ldx     a:sprites + $13", // an allocated base, and ld65 adds the rest
        "lda     a:REGS + $01",    // a nested field selects the sub-layout's base
    ] {
        assert!(
            assembly.contains(expected),
            "expected `{}`:\n{assembly}",
            expected.trim_end()
        );
    }

    // The instruction a line of the fixture compiled to. The echoed source
    // and its line number differ by construction, so neither belongs in a
    // comparison between two spellings.
    let instruction = |source: &str| {
        let site = assembly
            .find(source)
            .unwrap_or_else(|| panic!("`{source}` is in the fixture:\n{assembly}"));
        let start = assembly[..site].rfind('\n').expect("a line start");
        let comment = assembly[start..site].find(';').expect("an echoed line");
        assembly[start..start + comment].trim().to_owned()
    };

    // The two spellings of one selection produce one address, at both the
    // field step and the field-element step.
    assert_eq!(
        instruction("a = COMP[2].one"),
        instruction("a = COMP[2][.one]"),
        "`COMP[2].one` and `COMP[2][.one]` are one selection"
    );
    assert_eq!(
        instruction("a = COMP[2].str[3]"),
        instruction("a = COMP[2][.str][3]"),
        "the bracketed spelling indexes the same field element"
    );

    // `:sizeof` is the size of a *field* when a field is what was selected,
    // and the document's plain-variable answers are the ones worth pinning
    // because they are the ones a reader would guess wrong.
    for (source, expected) in [
        ("a = COMP.str:sizeof", "#$05"),  // five bytes, not one
        ("a = REGS.data:sizeof", "#$02"), // the nested layout's own size
        ("a = counter:sizeof", "#$02"),
        ("a = buffer:sizeof", "#$20"), // the whole raw span
        ("a = tasks:sizeof", "#$01"),  // one element, not the allocation
    ] {
        assert!(
            instruction(source).ends_with(expected),
            "`{source}` folds to `{expected}`, got `{}`",
            instruction(source)
        );
    }

    // `&&NAME[i] == &&NAME + i * NAME:sizeof`, which is the identity that
    // makes `:sizeof` mean one element. Checked as arithmetic rather than as
    // a string, so the fixture's own numbers cannot drift apart.
    let hex = |text: &str, after: &str| -> u32 {
        let tail = text.split(after).nth(1).unwrap_or_else(|| {
            panic!("expected `{after}` in `{text}`");
        });
        let digits: String = tail.chars().take_while(|c| c.is_ascii_hexdigit()).collect();
        u32::from_str_radix(&digits, 16).expect("a hex number")
    };
    let element = hex(&instruction("a = COMP:sizeof"), "#$");
    assert_eq!(element, 8, "the fixture's element size");
    for index in 1..=3u32 {
        let taken = instruction(&format!("a = &&COMP[{index}]"));
        assert_eq!(
            hex(&taken, "+ $"),
            index * element,
            "&&COMP[{index}] is &&COMP + {index} * COMP:sizeof, got `{taken}`"
        );
    }

    // A `:far` field reaches no register, so it is reached as a pointer —
    // and the long-indirect form is deliberately outside the datum-width
    // check, since it names the pointer rather than the datum.
    assert!(
        assembly.contains("lda     [(cursor + $02)]"),
        "a far field goes through a pointer:\n{assembly}"
    );

    assemble_and_link("subscripts", &assembly, "vars");
}

/// A layout stops at the unit boundary, and the two halves of that.
///
/// A `LayoutId` indexes the arena of the unit that computed it, so a layout
/// cannot be handed to another unit — and both consequences are worth
/// pinning, because each is a behaviour a reader would otherwise guess at.
/// An `abstract var` is not exportable, so two units may declare the same
/// one; an imported `var` arrives without its fields, so selecting one is
/// refused here rather than answered from the wrong arena.
#[test]
fn a_layout_does_not_cross_a_unit_boundary() {
    use k816::diagnostic::DiagnosticCode;

    // Both units declare `HANDLER`, and that is not a duplicate definition:
    // a layout has no place and no value, so there is nothing for ld65 to
    // resolve and nothing for two definitions to disagree about.
    let bytes = link_two_units(
        "cross-unit-layout",
        (
            "layout-unit-b.k816",
            include_str!("golden/layout-unit-b.k816"),
        ),
        (
            "layout-unit-a.k816",
            include_str!("golden/layout-unit-a.k816"),
        ),
    );
    assert!(!bytes.is_empty(), "the two units link");

    // Each unit answers the query from its own copy: 4 bytes of layout, and
    // `.close` two bytes in.
    let mut map = SourceMap::new();
    let b = map.add_file(
        "layout-unit-b.k816",
        include_str!("golden/layout-unit-b.k816"),
    );
    map.add_file(
        "layout-unit-a.k816",
        include_str!("golden/layout-unit-a.k816"),
    );
    let result = compile_to_ca65(&map, b, &Options { optimize: false });
    assert_eq!(result.diagnostics, vec![], "both units compile");
    let assembly = result.assembly.expect("compilation succeeds");
    assert!(
        assembly.contains("lda     #$04") && assembly.contains("lda     #$02"),
        "the queries answer from this unit's own layout:\n{assembly}"
    );
    // The variable crosses as a name and an address size, which is what the
    // import carries and all it carries.
    assert!(
        assembly.contains(".import TABLE") && assembly.contains("lda     a:TABLE"),
        "the imported name is still an ordinary address:\n{assembly}"
    );

    // And its fields do not cross. The refusal names the boundary rather
    // than claiming the declaration has no fields, because it has — over
    // there.
    let mut map = SourceMap::new();
    let user = map.add_file("user.k816", "func main @a8 @i8 {\n    a = TABLE.hi\n}\n");
    map.add_file(
        "layout-unit-a.k816",
        include_str!("golden/layout-unit-a.k816"),
    );
    let result = compile_to_ca65(&map, user, &Options { optimize: false });
    let refusal = result
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == DiagnosticCode::SYMBOL_KIND_MISMATCH)
        .expect("a foreign field selection is refused");
    assert!(
        refusal
            .notes
            .iter()
            .any(|note| note.contains("another unit")),
        "the refusal names the boundary: {refusal:?}"
    );
}

/// A `nocross` run that no longer fits blames whatever made it not fit.
///
/// Two ways to overflow a page and they are different complaints: the run was
/// written too big, which is the author's arithmetic, or K816 grew it by
/// putting a width restore on a branch's taken edge, which is not. Both come
/// from one site, so the interesting claim is that it picks the right one —
/// and neither had a test on the *code* path, only on data.
#[test]
fn a_nocross_run_blames_the_split_that_grew_it() {
    use k816::diagnostic::DiagnosticCode;

    let compile = |source: &str| {
        let mut map = SourceMap::new();
        let file = map.add_file("nocross.k816", source);
        compile_to_ca65(&map, file, &Options { optimize: false }).diagnostics
    };

    // 250 bytes of `nop`, then a width block a branch leaves. Written, the
    // run fits; lowered, the restore on the taken edge pushes it over.
    let grown = compile(
        "func f @a8 @i8 {\n\
         \x20   nocross {\n\
         \x20       nop * 250\n\
         \x20       @a16 {\n\
         \x20           bra .out\n\
         \x20       }\n\
         \x20   }\n\
         .out:\n\
         \x20   nop\n\
         }\n",
    );
    let split = grown
        .iter()
        .find(|diagnostic| diagnostic.code == DiagnosticCode::NOCROSS_SPLIT_OVERFLOW)
        .unwrap_or_else(|| panic!("expected the split to be blamed: {grown:?}"));
    assert!(
        split.notes.iter().any(|note| note.contains("restore")),
        "the note names the bytes K816 added: {split:?}"
    );
    assert!(
        split.help.is_some(),
        "and offers the two ways out: {split:?}"
    );

    // The same site, the other arm: nothing K816 added is to blame, so the
    // run is simply too big and the older code says so.
    let written = compile("func f @a8 @i8 {\n    nocross {\n        nop * 260\n    }\n}\n");
    let codes: Vec<&str> = written
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert!(
        codes.contains(&"K816-E0112") && !codes.contains(&"K816-E0130"),
        "a run written too big is not the split's fault: {codes:?}"
    );
}

/// A call that damages a register the caller is still using warns at the call.
///
/// The callee's contract names what it hands back; anything else it writes is
/// damage, and the warning is only worth making where the caller *reads* the
/// register afterwards — so the test pins both halves, the warning and the
/// silence when the contract covers it.
#[test]
fn a_call_that_damages_a_live_register_warns_at_the_call() {
    use k816::diagnostic::DiagnosticCode;

    let compile = |source: &str| {
        let mut map = SourceMap::new();
        let file = map.add_file("clobber.k816", source);
        compile_to_ca65(&map, file, &Options { optimize: false }).diagnostics
    };

    // `callee` returns A and quietly steps X; the caller reads X after it.
    let damaged = compile(
        "var mem = $10\n\n\
         func callee @a8 @i8 (a) -> a {\n    x++\n}\n\n\
         func caller @a8 @i8 {\n\
         \x20   x = mem\n\
         \x20   a = #1\n\
         \x20   callee a -> a\n\
         \x20   mem = x\n\
         }\n",
    );
    let warning = damaged
        .iter()
        .find(|diagnostic| diagnostic.code == DiagnosticCode::CLOBBERED_REGISTER)
        .unwrap_or_else(|| panic!("expected the clobber warning: {damaged:?}"));
    assert!(!warning.is_error(), "damage is a warning, not a refusal");
    assert_eq!(
        warning.secondary.len(),
        1,
        "it points at the read as well as the call: {warning:?}"
    );

    // Declared as an output, the same write is what the caller asked for.
    let declared = compile(
        "var mem = $10\n\n\
         func callee @a8 @i8 (a) -> a, x {\n    x++\n}\n\n\
         func caller @a8 @i8 {\n\
         \x20   x = mem\n\
         \x20   a = #1\n\
         \x20   callee a -> a, x\n\
         \x20   mem = x\n\
         }\n",
    );
    assert!(
        !declared
            .iter()
            .any(|diagnostic| diagnostic.code == DiagnosticCode::CLOBBERED_REGISTER),
        "a register the contract hands back is not damage: {declared:?}"
    );
}

/// The section kinds that are not subroutines, through the assembler.
///
/// `inline` is the one construct that changes the *shape* of the operation
/// stream rather than adding to it, so the claims worth making are about what
/// is no longer there: no `jsr`, no `rts`, and no label two expansions could
/// both define.
#[test]
fn expansions_and_naked_sections_assemble() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "inline-expansion.k816",
        include_str!("golden/inline-expansion.k816"),
    );
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(
        warnings,
        ["K816-W0124", "K816-W0124"],
        "only the `rti` exit, which no caller can rely on"
    );
    let assembly = result.assembly.expect("compilation succeeds");

    // An `inline` has no address: nothing defines it, nothing exports it, and
    // no use site transfers to it.
    for absent in ["inc_y:", "scale:", "store_to:", "settle:", ".export inc_y"] {
        assert!(
            !assembly.contains(absent),
            "an `inline` is not a place, so `{absent}` must not appear:\n{assembly}"
        );
    }
    assert!(
        !assembly.contains("jsr     a:inc_y"),
        "an `inline` is expanded, not called:\n{assembly}"
    );

    // Expanded twice each, in the body that used them.
    assert_eq!(
        assembly.matches("iny ").count(),
        2,
        "`inc_y` used twice is two `iny`s:\n{assembly}"
    );
    // The alias takes the encoding its bound symbol earns, not one the
    // declaration had to guess.
    assert!(assembly.contains("sta     a:mem"), "{assembly}");
    assert!(
        assembly.contains("sta     z:cursor"),
        "a direct-page argument narrows the store:\n{assembly}"
    );
    // The immediate alias is sized by the section's own contract.
    assert!(assembly.contains("lda     #$0010"), "{assembly}");
    assert!(assembly.contains("lda     #$1234"), "{assembly}");

    // `naked` appends nothing. `tail` ends in its jump and `handler` in its
    // `rti`, so the only `rts` in the file is the two `func`s'.
    assert_eq!(
        assembly.matches("        rts").count(),
        2,
        "only the two `func` sections return:\n{assembly}"
    );
    assert!(assembly.contains("        rti"), "{assembly}");
    assemble_and_link("inline_expansion", &assembly, "slice");
}

/// An argument that does not fit is reported at the use site that passed it.
///
/// The body is correct on its own terms — it is the *argument* that makes the
/// complaint true — so the caret belongs where the argument was written
/// (sub-plan E, decision 12).
#[test]
fn an_argument_that_does_not_fit_is_reported_where_it_was_passed() {
    use k816::diagnostic::DiagnosticCode;

    let mut map = SourceMap::new();
    let source = "inline narrow @a8 (a, #value) -> a {\n    lda #value\n}\n\n\
                  func f @a8 @i8 {\n    narrow a, #300 -> a\n}\n";
    let file = map.add_file("narrow.k816", source);
    let result = compile_to_ca65(&map, file, &Options { optimize: false });
    let reported: Vec<_> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code)
        .collect();
    assert_eq!(reported, vec![DiagnosticCode::IMMEDIATE_RANGE]);

    let diagnostic = &result.diagnostics[0];
    let caret = map.file(file).span_text(diagnostic.primary.span);
    assert_eq!(caret, "narrow a, #300 -> a", "the use site, not the body");
    let body = map.file(file).span_text(diagnostic.secondary[0].span);
    assert_eq!(body, "value", "and the parameter it was bound to");
}
