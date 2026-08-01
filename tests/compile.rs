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
        let SymbolKind::Var { address, size } = symbol.kind else {
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
