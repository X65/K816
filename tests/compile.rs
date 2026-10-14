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
use k816::abi::EntryEnvironment;
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

/// [`compile_and_link`] for a fixture that legitimately warns.
///
/// The stricter helper asserts *no* diagnostics, which is right for a fixture
/// whose subject is the bytes. A fixture whose subject is something else may
/// still have a warning in it on purpose — and "no errors" is the property
/// that says it is a program.
fn link_allowing_warnings(test: &str, name: &str, text: &str) -> Vec<u8> {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, &Options::default());
    assert!(
        !k816::diagnostic::has_errors(&result.diagnostics),
        "unexpected errors: {:?}",
        result.diagnostics
    );
    let assembly = result.assembly.expect("compilation succeeds");

    let dir = work_dir(test);
    let asm_path = dir.join("out.s");
    let obj_path = dir.join("out.o");
    let bin_path = dir.join("out.bin");
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
            .arg(&obj_path),
        "ld65 (build it with `make -C ../.. ld65` first)",
    );
    std::fs::read(&bin_path).expect("read linked binary")
}

/// Sub-plan H's hover fixture is a program, not a sample of syntax.
///
/// `tests/lsp.rs` reads it as the one source for the hover table, and a hover
/// is about what the model *understood* — so a fixture that stopped
/// assembling would be a table asserting about a program the compiler was
/// refusing. Its two `K816-W0124` warnings are its subject: they are the
/// section whose exit the model cannot classify, which is the case decision 7
/// names.
#[test]
fn the_hover_fixture_is_a_program_ca65_accepts() {
    let bytes = link_allowing_warnings(
        "hover-fixture",
        "lsp-hover.k816",
        include_str!("golden/lsp-hover.k816"),
    );
    assert!(!bytes.is_empty(), "the fixture linked to no bytes at all");
}

/// The `xce`-then-widen escape hatch assembles to the bytes the model
/// counted.
///
/// `doc/register-width-aware-syntax.md` documents `@a16` after an `xce` as an
/// *assertion*: the flag is unknown, and the author is saying the machine is
/// native. K816 emits the `rep` and used to withhold the `.a16` that goes
/// with it, so ca65 assembled `lda #$12` with **one** operand byte after a
/// `REP` that had widened the register — and the CPU read the following `nop`
/// as the immediate's high byte, executing an instruction nobody wrote.
///
/// The bytes are the whole point of this test: the emitted *text* was valid
/// either way, and the program simply did something else
/// (`postmortem.md` §H.13).
#[test]
fn the_escape_hatch_assembles_to_the_width_it_asserts() {
    const SOURCE: &str = "naked opaque @a8 @i8 {\n\
        \x20   xce\n\
        \x20   return\n\
        }\n\
        \n\
        func main @a8 @i8 {\n\
        \x20   call opaque\n\
        \x20   @a16 {\n\
        \x20       lda #$12\n\
        \x20   }\n\
        \x20   nop\n\
        \x20   return\n\
        }\n";

    let bytes = link_allowing_warnings("escape-hatch", "hatch.k816", SOURCE);
    assert_eq!(
        bytes,
        &[
            0xFB, // opaque: xce
            0x60, // opaque: rts
            0x20, 0x00, 0x80, // main: jsr opaque
            0xC2, 0x20, // rep #$20 — the block's entry bridge
            0xA9, 0x12, 0x00, // lda #$0012 — *two* operand bytes
            0xEA, // nop, which the one-byte form used to swallow
            0x60, // rts
        ],
        "the escape hatch assembled to the wrong widths"
    );
}

/// Sub-plan H's rename fixture is a program too, for the same reason.
#[test]
fn the_rename_fixture_is_a_program_ca65_accepts() {
    let (bytes, _) = compile_and_link(
        "rename-fixture",
        "lsp-rename.k816",
        include_str!("golden/lsp-rename.k816"),
        &Options::default(),
    );
    assert!(!bytes.is_empty(), "the fixture linked to no bytes at all");
}

/// The unoptimized slice: both loads, the store, and the return.
///
/// No entry setup at all. `main @a8` restates what the environment provides,
/// and an annotation on `main` is a claim about the entry ABI rather than a
/// request K816 satisfies with a `REP` — so there is nothing to emit
/// (`doc/register-width-aware-syntax.md`, "The entry point"). The widths are
/// claimed to the assembler by a mode assumption instead, which is why the
/// `lda #$00` below is the one-byte form.
const UNOPTIMIZED_BYTES: &[u8] = &[
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
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(bytes, UNOPTIMIZED_BYTES);
}

#[test]
fn native_slice_links_to_identical_bytes() {
    let (bytes, _) = compile_and_link(
        "native-unopt",
        "slice-native.k816",
        include_str!("golden/slice-native.k816"),
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(bytes, UNOPTIMIZED_BYTES);
}

/// The optimized slice: the dead `lda #$00` is gone, nothing else changes.
const OPTIMIZED_BYTES: &[u8] = &[
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
        &Options {
            optimize: true,
            ..Default::default()
        },
    );
    assert_eq!(hla, OPTIMIZED_BYTES);

    let (native, _) = compile_and_link(
        "native-opt",
        "slice-native.k816",
        include_str!("golden/slice-native.k816"),
        &Options {
            optimize: true,
            ..Default::default()
        },
    );
    assert_eq!(native, OPTIMIZED_BYTES);
}

#[test]
fn debug_database_records_k816_source_ownership() {
    let (_, dbg) = compile_and_link(
        "hla-dbg",
        "slice.k816",
        include_str!("golden/slice.k816"),
        &Options {
            optimize: false,
            ..Default::default()
        },
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

    // ...and every slice source line that *produces* code owns some: the
    // four statements, lines 4..=7. The declaration on line 3 owns none any
    // more, because an entry point generates no width setup — the widths are
    // the environment's, and claiming them to the assembler emits no bytes.
    for line_number in 4..=7 {
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
        "func precedence @a16 @i16 {\n  spot:\n    lda #spot + 1 & $FF\n}\n",
        &Options {
            optimize: false,
            ..Default::default()
        },
    );

    // An ordinary function, so nothing precedes its body: `spot` is at
    // $8000, the config placing CODE there. (An entry point would look the
    // same — no width setup is generated for one either — but this is not
    // one, because it declares widths the environment does not provide.)
    assert_eq!(bytes[0], 0xA9, "lda immediate: {bytes:02X?}");
    assert_eq!(
        u16::from_le_bytes([bytes[1], bytes[2]]),
        0x0001,
        "K65 groups this as `(spot + 1) & $FF` = ($8004 + 1) & $FF = 5; \
         ca65's own precedence would have given `spot + (1 & $FF)` = $8001"
    );
    assert_eq!(bytes[3], 0x60, "implicit rts");
}

/// ca65 stores an unresolved expression in the object file and ld65
/// evaluates it after placement, so K816 does not reject `label * 2` merely
/// because a label is involved (sub-plan C, backend decision 10).
#[test]
fn a_link_time_multiplication_survives_to_ld65() {
    let (bytes, _) = compile_and_link(
        "relocatable-multiply",
        "reloc.k816",
        "func multiply @a16 @i16 {\n  spot:\n    lda #&<spot * 2\n}\n",
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(bytes[0], 0xA9, "{bytes:02X?}");
    // `spot` lands at $8000 — nothing precedes the body — so the low byte is
    // $00 and the product is 0. The point is that the multiplication reached
    // ld65 at all rather than being rejected for naming a label.
    assert_eq!(u16::from_le_bytes([bytes[1], bytes[2]]), 0x0000);
}

// ---------------------------------------------------------------------------
// Derived linkage across units (sub-plan C, task C7).
// ---------------------------------------------------------------------------

/// Compiles `main`, assembles both units, links them, and returns the bytes.
///
/// Each unit names the other, which is how the compiler is told they belong
/// together; it still works out what crosses between them.
fn link_two_units(test: &str, main: (&str, &str), other: (&str, &str)) -> Vec<u8> {
    let mut map = SourceMap::new();
    let main_file = map.add_file(main.0, format!("import \"{}\"\n{}", other.0, main.1));
    let other_file = map.add_file(other.0, format!("import \"{}\"\n{}", main.0, other.1));

    let dir = work_dir(test);
    let mut objects = Vec::new();
    for (name, file) in [(main.0, main_file), (other.0, other_file)] {
        let result = compile_to_ca65(
            &map,
            file,
            &Options {
                optimize: false,
                ..Default::default()
            },
        );
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
            "func caller @a8 @i16 {\n    jsr helper\n    lda CURSOR\n    lda BANKED\n}\n",
        ),
        (
            "b.k816",
            "var dp CURSOR = $10\nvar far BANKED = $030000\nfunc helper @a8 {\n    nop\n}\n",
        ),
    );

    assert_eq!(
        bytes,
        vec![
            // No entry setup: this unit's caller is not `main`, so nothing
            // here is an entry point and nothing declares one.
            0x20, 0x0A, 0x80, // jsr helper — absolute, an ordinary import
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
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let errors: Vec<_> = result.diagnostics.iter().filter(|d| d.is_error()).collect();
    assert!(errors.is_empty(), "{name}: {errors:?}");
    result.assembly.expect("compilation succeeds")
}

/// Compiles a fixture that reports *warnings* on purpose, then assembles and
/// links it.
///
/// `compile_and_link` asserts no diagnostics at all, which is right for a
/// fixture whose whole output is supposed to be quiet. The optimizer's
/// fixtures are not: `opt-barriers.k816` carries a `plp` and an unfolded mask
/// so that `K816-W0124` is part of what it describes, and `opt-liveout.k816`
/// reads Y after a call that clobbers it so that `K816-W0123` is the argument
/// it makes. Errors are still fatal.
fn link_fixture(test: &str, name: &str, text: &str, options: &Options) -> Vec<u8> {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, options);
    let errors: Vec<_> = result.diagnostics.iter().filter(|d| d.is_error()).collect();
    assert!(errors.is_empty(), "{name}: {errors:?}");
    assemble_and_link(
        test,
        &result.assembly.expect("compilation succeeds"),
        "slice",
    )
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
            // No entry setup: `caller` is not `main`, and no entry point
            // gets one in any case.
            0x20, 0x0C, 0x80, // jsr helper
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
    let (bytes, _) = compile_and_link(
        test,
        name,
        text,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
///
/// "Differs" is measured from a *known* state here, and that is the entry
/// ABI's doing: `main` is unannotated, so it starts at what `abi.rs` says the
/// environment provides — 8-bit on both axes — rather than at unknown. The
/// bridge to `narrow @a8 @i8` is one instruction because of it, where an
/// unknown index width would have needed a `sep #$10` to establish something
/// the environment had already established.
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
        &bytes[main_at..main_at + 21],
        &[
            0xC2, 0x20, // rep #$20 — bridge for wide_a @a16
            0x20, 0x00, 0x80, // jsr wide_a
            0xE2, 0x20, // sep #$20 — bridge for narrow @a8 @i8: the
            //            accumulator only, since the index registers are
            //            still the 8 bits the environment handed over
            0x20, 0x02, 0x80, // jsr narrow
            0xC2, 0x20, // rep #$20 — bridge for wide_all @a16 @i16
            0xC2, 0x10, // rep #$10
            0x20, 0x04, 0x80, // jsr wide_all
            0x20, 0x06, 0x80, // jsr uncolored — no bridge at all
            0x60, // rts
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
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());

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
    let result = compile_to_ca65(
        &map,
        b,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
        "import \"far-lib.k816\"\nfunc main @a8 @i8 {\n    call far lib_init\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![], "the matching pair compiles");
    let assembly = result.assembly.expect("compilation succeeds");
    assert!(
        assembly.contains(".import lib_init : far"),
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
        "import \"far-lib.k816\"\nfunc main @a8 @i8 {\n    call lib_init\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
    let result = compile_to_ca65(
        &map,
        b,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
    let user = map.add_file(
        "user.k816",
        "import \"layout-unit-a.k816\"\nfunc main @a8 @i8 {\n    a = TABLE.hi\n}\n",
    );
    map.add_file(
        "layout-unit-a.k816",
        include_str!("golden/layout-unit-a.k816"),
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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
        compile_to_ca65(
            &map,
            file,
            &Options {
                optimize: false,
                ..Default::default()
            },
        )
        .diagnostics
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
        compile_to_ca65(
            &map,
            file,
            &Options {
                optimize: false,
                ..Default::default()
            },
        )
        .diagnostics
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

/// What a data block emits, entry by entry.
///
/// The gate is bytes: the width a typed entry gives each value, the mapping a
/// string goes through, how many copies a `repeat` makes, and what each step
/// of a `for … eval` folds to. Everything here is arithmetic K816 performs at
/// compile time, so everything here is checkable without running anything.
#[test]
fn every_data_entry_emits_the_bytes_it_describes() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "data-entries.k816",
        include_str!("golden/data-entries.k816"),
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![], "the fixture must compile");
    let assembly = result.assembly.expect("compilation succeeds");

    for expected in [
        // The keyword sets the width, and `?` is that width's zero.
        ".byte    $20, $00, $01, $00",
        ".word    $00, $1234, ptr, $00",
        ".faraddr $01C000, $00",
        // An address-byte operator selects a byte; the entry still sizes it.
        ".byte    .lobyte(ptr), .hibyte(ptr)",
        // A charset is an index table: H is 8, E is 5, L is 12, O is 15.
        ".byte    $08, $05, $0C, $0C, $0F",
        // Without one, a string is its characters' own values.
        ".byte    $41, $42",
        // `repeat 2 { byte 1 2 }` keeps the entry's width in every copy.
        ".byte    $01, $02",
        // Generated: i*3 forward, j backwards, one step, min, and a ternary.
        ".byte    $00, $03, $06, $09, $0C",
        ".byte    $04, $03, $02, $01, $00",
        ".byte    $07",
        ".byte    $00, $64, $C8, $FF",
        ".byte    $0F, $0F, $F0, $F0",
    ] {
        assert!(
            assembly.contains(expected),
            "expected `{expected}`:\n{assembly}"
        );
    }

    // Repetition is copies, not a directive: three of one byte, and six from
    // a `repeat` inside a `repeat`.
    let repeated = assembly.find("repeated:").expect("the repeated block");
    let table = assembly.find("table:").expect("the generated block");
    let body = &assembly[repeated..table];
    assert_eq!(body.matches(".byte    $04").count(), 3, "{body}");
    assert_eq!(body.matches(".byte    $AA").count(), 6, "{body}");
    // A `charset` inside a `repeat` body is scoped to it: `z` is 2 in the
    // copies and its own value after them.
    assert_eq!(body.matches(".byte    $02, $01").count(), 2, "{body}");
    assert!(body.contains(".byte    $7A"), "{body}");

    // A `code { }` subblock puts instructions among the bytes, and they are
    // emitted with a width state like any other instruction.
    let mixed = assembly.find("mixed:").expect("the mixed block");
    let mixed = &assembly[mixed..];
    assert!(
        mixed.contains("lda     #$20") && mixed.contains("ldy     #$03"),
        "the subblocks' instructions are here:\n{mixed}"
    );
    // `nocross code { }` asks the page-fit question the statement form does.
    assert!(
        mixed.contains(".assert  .hibyte(*"),
        "the guarded subblock carries its assertion:\n{mixed}"
    );
    // And an inline subblock is ordinary code of its function: sized by the
    // contract above it, with the jump around the data it sits in.
    let caller = assembly.find("caller:").expect("the function");
    let caller = &assembly[caller..];
    assert!(
        caller.contains("jmp     :+") && caller.contains("lda     #$07"),
        "the inline subblock lowers inside the function:\n{caller}"
    );

    // A `nocross` run is measured without folding anything: a `charset` is
    // no bytes, a string is one value per character, and a typed entry is
    // its width times its values — 4 + 4 + 1 here.
    let guarded = assembly.find("guarded:").expect("the guarded block");
    assert!(
        assembly[guarded..].contains(".hibyte(* - 9)"),
        "the guard covers the whole run:\n{}",
        &assembly[guarded..]
    );

    assemble_and_link("data-entries", &assembly, "slice");
}

/// The paired HLA fixtures reach the assembler, not just the comparison.
///
/// `tests/hla.rs` proves each HLA body lowers to the same `KOp`s as the
/// native body beside it, which is a claim about *meaning* and says nothing
/// about whether ca65 accepts the result. For branches that gap is the whole
/// of decision 7: a relative branch out of range is not a K816 diagnostic, it
/// is an assembler error, and a compiler that emits the short form and hopes
/// produces a program that does not build.
#[test]
fn the_paired_hla_fixtures_assemble() {
    use k816::diagnostic::{Diagnostic, DiagnosticCode};
    for (name, source) in [
        ("hla-chains", include_str!("golden/hla-chains.k816") as &str),
        ("hla-alu", include_str!("golden/hla-alu.k816")),
        ("hla-branches", include_str!("golden/hla-branches.k816")),
    ] {
        let mut map = SourceMap::new();
        let file = map.add_file(format!("{name}.k816"), source);
        let result = compile_to_ca65(
            &map,
            file,
            &Options {
                optimize: false,
                ..Default::default()
            },
        );
        // `hla-branches` exercises `>` and `<=`, which is what G8's
        // efficiency warning is about. It is a warning, so the file still
        // compiles; nothing else may appear.
        let unexpected: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.code != DiagnosticCode::INEFFICIENT_BRANCH)
            .collect();
        assert_eq!(unexpected, Vec::<&Diagnostic>::new(), "{name} must compile");
        let assembly = result.assembly.expect("compilation succeeds");
        assemble_and_link(name, &assembly, "slice");
    }
}

/// An allocated direct-page slot never lands on a pinned one.
///
/// A pinned declaration is an equate and reserves no linker space, so nothing
/// stands in the allocator's way: `var dp p = $00` and `var dp q` both came
/// out at offset zero. The pool now reserves the pinned bytes as filler, so
/// ld65's cursor steps over them.
#[test]
fn the_direct_page_pool_steps_over_its_pinned_slots() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "pool.k816",
        "var dp pinned = $00\n\
         var dp gap = $02\n\
         var dp first\n\
         var dp second\n\
         func run @a8 @i8 {\n\
         \x20   a = pinned\n\
         \x20   a = gap\n\
         \x20   a = first\n\
         \x20   a = second\n\
         }\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![], "the fixture must compile");
    let assembly = result.assembly.expect("compilation succeeds");

    // The pool is laid out in order from the page base, so the filler is what
    // decides where each allocated slot lands: one byte over the pinned $00,
    // then `first` at $01, then one byte over the pinned $02, then `second`.
    let pool = assembly
        .find(".segment \"ZEROPAGE\"")
        .expect("the pool has a segment");
    let pool: Vec<&str> = assembly[pool..]
        .lines()
        .filter_map(|line| line.split(';').next())
        .map(str::trim)
        .filter(|line| line.starts_with(".res") || line.ends_with(':'))
        .collect();
    assert_eq!(
        pool,
        [
            ".res     $01",
            "first:",
            ".res     $01",
            ".res     $01",
            "second:",
            ".res     $01",
        ],
        "{assembly}"
    );

    // And ld65 agrees: linked, the four are at four different addresses.
    let bytes = assemble_and_link("dp-pool", &assembly, "vars");
    // `lda z:pinned` is $A5 $00; the three that follow each name a distinct
    // one-byte address, and none of them is $00 or $02.
    let loads: Vec<u8> = bytes
        .chunks(2)
        .take(4)
        .filter(|chunk| chunk[0] == 0xA5)
        .map(|chunk| chunk[1])
        .collect();
    assert_eq!(loads.len(), 4, "{bytes:02X?}");
    assert_eq!(loads[0], 0x00, "the pinned one keeps its address");
    assert_eq!(loads[1], 0x02, "and so does the second");
    assert!(
        loads[2] != 0x00 && loads[2] != 0x02 && loads[3] != 0x00 && loads[3] != 0x02,
        "the allocated ones avoid both: {loads:02X?}"
    );
    assert_ne!(loads[2], loads[3], "and each other: {loads:02X?}");
}

/// A branch inside a top-level `data { code { } }` is relaxed like any other.
///
/// The subblock gets a `Lowering` of its own, and relaxation is per-stream —
/// so a conditional over 200 bytes of code kept its short branch and ca65
/// refused the file. Assembled here, because ca65 is the only thing that can
/// answer whether a relative branch reaches.
#[test]
fn a_branch_inside_a_data_code_subblock_is_relaxed() {
    let body = "nop\n        ".repeat(200);
    let source = format!(
        "data blk {{\n  0\n  code {{\n    @a8 {{\n      a?0\n      == {{\n        {body}\n      }}\n    }}\n  }}\n}}\n"
    );
    let mut map = SourceMap::new();
    let file = map.add_file("data-branch.k816", &source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![], "the fixture must compile");
    let assembly = result.assembly.expect("compilation succeeds");
    assert!(
        assembly.contains("jmp     "),
        "the branch is widened to a jump:\n{assembly}"
    );
    assemble_and_link("data-branch", &assembly, "slice");
}

/// A `nocross` run is measured after relaxation, not before.
///
/// Relaxation adds three bytes to every branch it widens, so a run measured
/// while it still held short branches asserted a page fit for bytes the
/// assembler never saw — and let an oversized run past the check entirely.
#[test]
fn a_nocross_run_is_measured_after_its_branches_settle() {
    let body = "nop\n        ".repeat(250);
    let source = format!(
        "func f @a8 @i8 {{\n    nocross {{\n        a?0\n        == {{\n        {body}\n        }}\n    }}\n}}\n"
    );
    let mut map = SourceMap::new();
    let file = map.add_file("nocross.k816", &source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(
        codes,
        ["K816-E0112"],
        "253 written bytes become 258 once the branch widens, which is a page over"
    );

    // A run that still fits carries the *relaxed* length in its guard.
    let body = "nop\n        ".repeat(200);
    let source = format!(
        "func f @a8 @i8 {{\n    nocross {{\n        a?0\n        == {{\n        {body}\n        }}\n    }}\n}}\n"
    );
    let mut map = SourceMap::new();
    let file = map.add_file("nocross-fits.k816", &source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![]);
    let assembly = result.assembly.expect("compiles");
    // 200 nops, `cmp #$00`, and the widened branch's five bytes.
    assert!(
        assembly.contains(".hibyte(* - 208)"),
        "the guard counts the relaxed run:\n{}",
        assembly
            .lines()
            .filter(|l| l.contains("assert"))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

/// A widened branch does not lose the damaged-register warning behind it.
///
/// Lowering records a checked call by its *index* in the operation stream,
/// and relaxation rewrites that stream — so an index taken before and read
/// after pointed at a different instruction, and the damage scan looked at
/// the wrong one and said nothing.
#[test]
fn a_widened_branch_does_not_hide_a_damaged_register() {
    let body = "nop\n        ".repeat(200);
    let with_branch = format!(
        "var mem = $10\n\n\
         func callee @a8 @i8 (a) -> a {{\n    x++\n}}\n\n\
         func caller @a8 @i8 {{\n\
         \x20   x = mem\n\
         \x20   a?0\n\
         \x20   == {{\n        {body}\n    }}\n\
         \x20   a = #1\n\
         \x20   callee a -> a\n\
         \x20   mem = x\n\
         }}\n"
    );
    let mut map = SourceMap::new();
    let file = map.add_file("damage.k816", &with_branch);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(
        codes,
        ["K816-W0123"],
        "the callee still steps X, and the caller still reads it"
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
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(
        warnings,
        Vec::<&str>::new(),
        "the `rti` exit restores what it interrupted, so it preserves"
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
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
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

// ---------------------------------------------------------------------------
// The optimizer's own fixtures, at the bytes (sub-plan F).
// ---------------------------------------------------------------------------

/// The barrier fixture assembles, and optimizing it changes not one byte.
///
/// The `KOp`-level claim is in `tests/optimizer.rs`, where the log is asserted
/// empty. This is the same claim through ca65: a pass that produced plausible
/// text and different sizes would pass there and fail here.
#[test]
fn the_barrier_fixture_assembles_and_the_optimizer_changes_no_byte() {
    let source = include_str!("golden/opt-barriers.k816");
    let unoptimized = link_fixture(
        "opt-barriers-unopt",
        "opt-barriers.k816",
        source,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let optimized = link_fixture(
        "opt-barriers-opt",
        "opt-barriers.k816",
        source,
        &Options {
            optimize: true,
            ..Default::default()
        },
    );
    assert_eq!(optimized, unoptimized, "{optimized:02X?}");
}

/// Nothing is dead at a function exit, at the bytes.
///
/// The whole file loses exactly two bytes, and they are the mid-function
/// `lda #$07` of `middle`. Every load, `ldy`, and `clc` that sits at an exit
/// stays: the live-out seed is everything, and a declared `-> a` narrows
/// nothing (`plans/05-optimizer.md` decision 12).
#[test]
fn the_live_out_fixture_keeps_everything_a_return_could_hand_back() {
    let source = include_str!("golden/opt-liveout.k816");
    let unoptimized = link_fixture(
        "opt-liveout-unopt",
        "opt-liveout.k816",
        source,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let optimized = link_fixture(
        "opt-liveout-opt",
        "opt-liveout.k816",
        source,
        &Options {
            optimize: true,
            ..Default::default()
        },
    );
    assert_eq!(
        unoptimized.len() - optimized.len(),
        2,
        "only the mid-function `lda #$07` goes: {unoptimized:02X?} -> {optimized:02X?}"
    );

    // `produce`: adc #$01, ldy #$09, rts — the declared output *and* the
    // undeclared one.
    assert_eq!(&optimized[..5], &[0x69, 0x01, 0xA0, 0x09, 0x60]);
    // `carry`: lda #$01, sta $0400, clc, rts — the trailing `clc` survives.
    assert_eq!(
        &optimized[5..12],
        &[0xA9, 0x01, 0x8D, 0x00, 0x04, 0x18, 0x60]
    );
    // `middle`, at the end: lda #$08, sta $0400, rts — `lda #$07` is gone.
    assert_eq!(
        &optimized[optimized.len() - 6..],
        &[0xA9, 0x08, 0x8D, 0x00, 0x04, 0x60]
    );
}

/// **Moved by sub-plan F, tasks F1 and F2**, and every difference from the
/// unoptimized gate above is one of two claims.
///
/// `skipped` loads the accumulator, branches away, and the target reloads it,
/// so the first load is dead on every path (F1). `resolved` writes a
/// `rep #$20` immediately above a *label bridge* that establishes the same
/// width for every arrival, so the first one is redundant on both paths and
/// its M bit is unobserved before the bridge re-establishes it (F2) — and it
/// is a user-written instruction, treated exactly like a generated one, which
/// `doc/design.md` §8 makes a bullet.
///
/// Two things stay. The **label bridge itself** is never eliminated: the
/// arrivals it exists for are branches from states F is not looking at, and
/// liveness below the label says nothing about them. And the user-written
/// `rep #$20` in `skipped` that no path reaches stays exactly where it was
/// written — the unreachable pass removes compiler-generated operations only.
#[test]
fn the_optimized_branch_fixture_loses_only_what_no_path_depends_on() {
    let bytes = link_fixture(
        "width-branch-opt",
        "width-branch.k816",
        include_str!("golden/width-branch.k816"),
        &Options {
            optimize: true,
            ..Default::default()
        },
    );
    // `skipped`: bra (2), the unreachable user rep #$20 (2), lda #$01 (2),
    // rts (1). Only the load the branch leaves behind is gone.
    assert_eq!(
        &bytes[..7],
        &[0x80, 0x02, 0xC2, 0x20, 0xA9, 0x01, 0x60],
        "{bytes:02X?}"
    );
    // `taken` is byte-for-byte the unoptimized gate: its `rep #$20` is on
    // every path to a `lda #$01` that reads M, so nothing there is dead.
    assert_eq!(
        &bytes[7..17],
        &[0xA9, 0x01, 0xC2, 0x20, 0x80, 0x00, 0xA9, 0x01, 0x00, 0x60],
        "{bytes:02X?}"
    );
    // `resolved`: bcc (2, now reaching two bytes nearer), the label bridge
    // rep #$20 (2), lda #$0102 (3), rts (1).
    assert_eq!(
        &bytes[17..],
        &[0x90, 0x00, 0xC2, 0x20, 0xA9, 0x02, 0x01, 0x60],
        "{bytes:02X?}"
    );
}

// ---------------------------------------------------------------------------
// What the optimizer changed about the width model's own fixtures (F2).
// ---------------------------------------------------------------------------

/// The instruction lines of an emitted assembly, in order, without comments.
fn instruction_lines(assembly: &str) -> Vec<String> {
    assembly
        .lines()
        .map(|line| line.split(';').next().unwrap_or_default().trim_end())
        .filter(|line| line.starts_with("        ") && !line.trim_start().starts_with('.'))
        .map(|line| line.trim().to_owned())
        .collect()
}

/// Compiles `text` twice, links both, and returns the lines the optimizer
/// removed, the lines it put in their place, and the bytes the difference
/// cost.
///
/// A line-level diff on its own would pass on output ca65 then sizes
/// differently, which is the exact failure the width model exists to prevent —
/// so both halves go through the real assembler and the linker, and the byte
/// count is asserted beside the lines.
fn optimizer_effect(test: &str, name: &str, text: &str) -> (Vec<String>, Vec<String>, usize) {
    optimizer_effect_with(test, name, text, "slice")
}

/// The same, for a fixture that needs a different ld65 configuration.
fn optimizer_effect_with(
    test: &str,
    name: &str,
    text: &str,
    config: &str,
) -> (Vec<String>, Vec<String>, usize) {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let compile = |optimize: bool| -> String {
        let result = compile_to_ca65(
            &map,
            file,
            &Options {
                optimize,
                ..Default::default()
            },
        );
        let errors: Vec<_> = result.diagnostics.iter().filter(|d| d.is_error()).collect();
        assert!(errors.is_empty(), "{name}: {errors:?}");
        result.assembly.expect("compilation succeeds")
    };
    let before = compile(false);
    let after = compile(true);
    let before_bytes = assemble_and_link(&format!("{test}-unopt"), &before, config);
    let after_bytes = assemble_and_link(&format!("{test}-opt"), &after, config);

    let mut remaining = instruction_lines(&after);
    let mut removed: Vec<String> = Vec::new();
    for line in instruction_lines(&before) {
        match remaining.iter().position(|kept| *kept == line) {
            Some(index) => {
                remaining.remove(index);
            }
            None => removed.push(line),
        }
    }
    // A multiset difference, sorted: identical lines in different functions
    // cancel against each other, so the order they came out in says nothing.
    // What the assertions name is *which* instructions the optimizer stopped
    // emitting and which it started, and the byte count is what pins the
    // sizes.
    removed.sort();
    remaining.sort();
    (removed, remaining, before_bytes.len() - after_bytes.len())
}

/// `block-exit.k816`: the scoped-block operations whose bodies observe
/// nothing go, and every landing pad stays.
///
/// A block whose body is one `bra` observes neither axis, so its *enter* is
/// dead: the pad on the taken edge re-establishes the width for everything
/// below, and the pad is the only path out. `native_exit` has one such block
/// and `nested_exit` has two nested ones. The `sep #$20` that
/// `generated_exit` restores on its *back edge* is dead for the same reason
/// one step around — every path from it re-enters the loop through the block's
/// own `rep #$20`.
///
/// Every landing pad survives. The pad is what makes a branch out of a scoped
/// block correct, and none of these removals is on that edge: `nested_exit`'s
/// pair of restores is *folded*, not dropped.
///
/// **Sub-plan G, task G8** adds two more removals, and they are pass 9's:
/// the pad's own `jmp .out` in `native_exit` and in `nested_exit` is a jump
/// to the label immediately below it. What survives beside them is the user's
/// `bra .out`, which pass 9 declines by origin even though the edge split has
/// since moved its target to the pad directly underneath — the visible price
/// of not deleting an instruction the author typed.
#[test]
fn the_optimized_block_exit_fixture_drops_the_enters_no_body_observes() {
    let (removed, added, bytes) = optimizer_effect(
        "block-exit",
        "block-exit.k816",
        include_str!("golden/block-exit.k816"),
    );
    // Sorted: the two pads' jumps to the label below them, `native_exit`'s
    // block enter, `generated_exit`'s back-edge restore, `nested_exit`'s two
    // enters, and its two pad restores, which fold rather than disappear.
    assert_eq!(
        removed,
        vec![
            "jmp     @out",
            "jmp     @out",
            "rep     #$10",
            "rep     #$20",
            "rep     #$20",
            "sep     #$10",
            "sep     #$20",
            "sep     #$20",
        ],
        "{removed:?}"
    );
    assert_eq!(
        added,
        vec!["sep     #$30"],
        "the pad's restores fold into one; nothing else is put back: {added:?}"
    );
    assert_eq!(
        bytes,
        12 - 2 + 6,
        "six two-byte instructions out, one back in, and two three-byte jumps"
    );
}

/// `width-bridging.k816`: one bridge bit goes, and it is the one no
/// instruction in the program depends on.
///
/// Every body in that fixture is a `nop` except `already_wide`, which reads M
/// through `lda #$1234`. So the accumulator axis is observed and every `rep`
/// and `sep` that carries M stays; the *index* axis is observed nowhere, and
/// the widening `rep #$10` before `call wide_all` goes with it.
///
/// The narrowing `sep #$10` before `call narrow` stays, and the asymmetry is
/// the truncation rule rather than an inconsistency: narrowing the index
/// registers zeroes their high bytes, which is a register write, and every
/// register is live at a function's exit.
#[test]
fn the_optimized_bridging_fixture_drops_only_the_axis_nothing_observes() {
    let (removed, added, bytes) = optimizer_effect(
        "width-bridging",
        "width-bridging.k816",
        include_str!("golden/width-bridging.k816"),
    );
    assert_eq!(removed, vec!["rep     #$10"], "{removed:?}");
    assert_eq!(added, Vec::<String>::new());
    assert_eq!(bytes, 2);
}

/// `width-loop.k816`: the `rep #$20` inside the diverging loop goes.
///
/// `diverging` is `nop`, `rep #$20`, `bra .head` — an infinite loop whose only
/// path from the mode change leads back through the mode change. Nothing reads
/// M anywhere on it, and the function has no reachable return for an exit
/// contract to promise anything at.
///
/// The fixture's own claim is untouched: it exists to prove the width solver
/// reaches a fixpoint over the back edge, and the unoptimized gate beside this
/// one is what checks that.
#[test]
fn the_optimized_loop_fixture_drops_the_mode_change_its_own_loop_undoes() {
    let (removed, added, bytes) = optimizer_effect(
        "width-loop",
        "width-loop.k816",
        include_str!("golden/width-loop.k816"),
    );
    // And both functions loop forever, so the implicit `rts` the compiler put
    // after each of them is unreachable. It removes compiler-generated
    // operations only, which is the master plan's own wording.
    assert_eq!(removed, vec!["rep     #$20", "rts", "rts"], "{removed:?}");
    assert_eq!(added, Vec::<String>::new());
    assert_eq!(bytes, 2 + 1 + 1);
}

/// `native-modes.k816`: dead register and flag writes, and one redundant mode
/// instruction.
///
/// The fixture writes every transfer and every flag operation in a row, so
/// most of them overwrite each other: `txa` before `tya`, `tyx` before `tsx`,
/// `txs` before `tcs`, `clc` before `sec`, `cld` before `sed`, `cli` before
/// `sei`. The `sep #$20` goes because the `rep #$20` on the next line
/// establishes M without reading it — which is only visible once `SEP`/`REP`
/// stop claiming to read every P flag.
///
/// The `lda #$01` in `stack_forms` goes because `pla` overwrites A below it.
#[test]
fn the_optimized_native_modes_fixture_drops_what_the_next_line_overwrites() {
    let (removed, added, bytes) = optimizer_effect(
        "native-modes",
        "native-modes.k816",
        include_str!("golden/native-modes.k816"),
    );
    assert_eq!(
        removed,
        vec![
            "clc",          // before `sec`
            "cld",          // before `sed`
            "cli",          // before `sei`
            "lda     #$01", // before the `pla` in stack_forms
            // The implicit return after `control`'s unconditional `bra`. The
            // *user's* unreachable instructions below that branch — a `brl`,
            // four jumps, a `jsr`, a `jsl`, a `brk`, and a `cop` — all stay,
            // because this pass removes compiler-generated operations only.
            "rts",
            "sep     #$20", // before the `rep #$20` on the next line
            "txa",          // before `tya`
            "txs",          // before `tcs`
            "tyx",          // before `tsx`
        ],
        "{removed:?}"
    );
    assert_eq!(added, Vec::<String>::new());
    assert_eq!(bytes, 2 + 1 + 1 + 1 + 1 + 1 + 1 + 2 + 1);
}

/// The mode fixture, at the bytes: two changes, and nothing else.
#[test]
fn the_mode_fixture_strips_one_mask_bit_and_removes_one_redundant_change() {
    let (removed, added, bytes) = optimizer_effect(
        "opt-modes",
        "opt-modes.k816",
        include_str!("golden/opt-modes.k816"),
    );
    // `rep #$21` becomes `rep #$01`: the M bit is unobserved, the carry clear
    // is not the width pass's to touch. And the `sep #$20` above the fixed
    // label bridge goes, because the bridge re-establishes the width for every
    // arrival.
    // Sorted: the bridge to `observes_x` (two instructions, which fold into
    // one), the `rep #$21` whose M bit is stripped, and the redundant change
    // above the fixed label bridge.
    assert_eq!(
        removed,
        vec![
            "rep     #$10",
            "rep     #$20",
            "rep     #$21",
            // The implicit return below `main`'s indirect jump, which no edge
            // reaches. The `rep #$30` above that jump stays: a transfer K816
            // cannot name observes both axes.
            "rts",
            "sep     #$20",
        ],
        "{removed:?}"
    );
    assert_eq!(
        added,
        vec![
            "rep     #$01", // the stripped mask, carry clear intact
            "rep     #$30", // the folded bridge, which is the document's own table
        ],
        "{added:?}"
    );
    // The mask rewrite is the same length; the removal, the fold, and the
    // unreachable return cost two, two, and one.
    assert_eq!(bytes, 5);
}

/// The cross-unit pair links, and both of unit A's bridges are still there.
///
/// Nothing unit A can see reads the M or the X flag; unit B does, and it is
/// also where the callee lives. "The axis is observed nowhere" is a claim
/// about the whole program, and this is the program.
#[test]
fn the_cross_unit_mode_pair_links_with_both_bridges_intact() {
    let mut map = SourceMap::new();
    let a = map.add_file(
        "opt-crossunit-a.k816",
        include_str!("golden/opt-crossunit-a.k816"),
    );
    let b = map.add_file(
        "opt-crossunit-b.k816",
        include_str!("golden/opt-crossunit-b.k816"),
    );
    let assemble = |file, name: &str| -> PathBuf {
        let result = compile_to_ca65(
            &map,
            file,
            &Options {
                optimize: true,
                ..Default::default()
            },
        );
        let errors: Vec<_> = result.diagnostics.iter().filter(|d| d.is_error()).collect();
        assert!(errors.is_empty(), "{name}: {errors:?}");
        let assembly = result.assembly.expect("compilation succeeds");
        if name == "a" {
            // Unit A's bridge to the callee is intact, folded into the one
            // instruction the document's folding table asks for.
            assert!(assembly.contains("rep     #$30"), "{assembly}");
        }
        let dir = work_dir(&format!("opt-crossunit-{name}"));
        let asm_path = dir.join("out.s");
        let obj_path = dir.join("out.o");
        std::fs::write(&asm_path, assembly).expect("write assembly");
        run_ok(
            Command::new(tool("CA65", "ca65"))
                .arg(&asm_path)
                .args(["-o".as_ref(), obj_path.as_os_str()]),
            "ca65",
        );
        obj_path
    };
    let object_a = assemble(a, "a");
    let object_b = assemble(b, "b");

    let dir = work_dir("opt-crossunit-link");
    let bin_path = dir.join("out.bin");
    let config = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden/slice.cfg");
    run_ok(
        Command::new(tool("LD65", "ld65"))
            .args(["-C".as_ref(), config.as_os_str()])
            .args(["-o".as_ref(), bin_path.as_os_str()])
            .arg(&object_a)
            .arg(&object_b),
        "ld65",
    );
    assert!(
        !std::fs::read(&bin_path)
            .expect("read linked binary")
            .is_empty()
    );
}

/// The folding fixture, at the bytes.
///
/// Four merges and one mask rewrite, and three runs that must not merge: the
/// opposite-direction pair, the pair one operation apart, and the pair whose
/// composed result would drop a live index truncation.
#[test]
fn the_folding_fixture_merges_only_what_the_composed_transform_allows() {
    let (removed, added, bytes) = optimizer_effect(
        "opt-folding",
        "opt-folding.k816",
        include_str!("golden/opt-folding.k816"),
    );
    // Sorted, and identical lines in different functions cancel: the
    // `rep #$01` and `sep #$01` of `identical_effects_opposite_results` are
    // matched against the `rep #$01` that `cancelled_with_a_carry` gains and
    // the `sep #$01` the carry pair folds to, so what is left is the four
    // merged halves plus the stripped `rep #$21`.
    assert_eq!(
        removed,
        vec![
            "rep     #$10", // widen_both
            "rep     #$20",
            "rep     #$21", // cancelled_with_a_carry, whose M bit is stripped
            "sep     #$10", // narrow_both
            "sep     #$20",
        ],
        "{removed:?}"
    );
    assert_eq!(
        added,
        vec![
            "rep     #$30", // widen_both
            "sep     #$30", // narrow_both
        ],
        "{added:?}"
    );
    // Three merges of two-byte pairs; the mask rewrite is the same length.
    assert_eq!(bytes, 6);
}

/// The value fixture, at the bytes: three removals, and the five cases that
/// must not move.
///
/// A repeated immediate load goes; a `cmp` between two identical loads keeps
/// the second, because the load reports on N and Z and the comparison rewrote
/// them. A `clc` after a mask that already cleared the carry goes; the one
/// after an `adc` stays. An `xba` over a low-byte-only accumulator leaves a
/// half the tracker never saw, so neither the store after it nor the load
/// after that is touched — and where sixteen known bits move into sixteen-bit
/// index registers, the `ldx` after them goes. Both reads of the hardware
/// register stay.
#[test]
fn the_value_fixture_coalesces_only_what_the_lanes_cover() {
    let (removed, added, bytes) = optimizer_effect(
        "opt-values",
        "opt-values.k816",
        include_str!("golden/opt-values.k816"),
    );
    assert_eq!(
        removed,
        vec![
            "clc",          // repeated_clear
            "lda     #$07", // repeated_load
            // …and the two loads a memory read and a mask that writes N and Z
            // each left dead behind them. Their *second* halves stay: neither
            // a memory read nor a flag-writing mask leaves the tracker able to
            // say the load after it changes nothing.
            "lda     #$07",
            "lda     #$07",
            "ldx     #$0000", // lanes_agree, where sixteen known bits moved
            // …and the three stores of a known-zero accumulator, which the
            // zero-store peephole rewrites on its way past. The fourth store
            // in the fixture is `lanes`, immediately after its `xba`: the
            // accumulator holds the half no instruction wrote there, so it
            // stays a `sta`. A tracker without lanes would rewrite it.
            "sta     a:CELL,x",
            "sta     a:CELL,x",
            "sta     a:CELL,x",
        ],
        "{removed:?}"
    );
    assert_eq!(
        added,
        vec!["stz     a:CELL,x", "stz     a:CELL,x", "stz     a:CELL,x",],
        "{added:?}"
    );
    // `STZ` is the same length as `STA`, so only the deletions cost bytes: a
    // one-byte `clc`, three two-byte loads, and a three-byte `ldx`.
    assert_eq!(bytes, 1 + 3 * 2 + 3);
}

/// **Moved by sub-plan F, task F4.** `subscripts.k816` writes a run of
/// `a = NAME:sizeof` statements whose results are never stored, so each load
/// is overwritten by the next; and once the dead ones are gone, one of the
/// survivors turns out to load a value the accumulator already holds.
///
/// The fixture's own claim is the folded *offsets*, and its unoptimized gate
/// is what checks them. This is the second gate beside it.
#[test]
fn the_optimized_subscripts_fixture_drops_the_loads_nothing_stores() {
    let (removed, added, bytes) = optimizer_effect_with(
        "subscripts",
        "subscripts.k816",
        include_str!("golden/subscripts.k816"),
        "vars",
    );
    assert_eq!(
        removed,
        vec![
            "lda     #$0008",
            "lda     #$02",
            "lda     #$02",
            "lda     #$04", // the one the accumulator already held
            "lda     #$05",
            "lda     #$06",
            "lda     #$20",
            "lda     #.loword((COMP + $08))",
            "lda     #.loword((COMP + $10))",
        ],
        "{removed:?}"
    );
    assert_eq!(added, Vec::<String>::new());
    // Six two-byte loads and three three-byte ones: the 16-bit `#$0008` and
    // the two link-time `.loword` operands, which ca65 sizes as addresses.
    assert_eq!(bytes, 6 * 2 + 3 * 3);
}

/// The zero-store fixture, at the bytes.
///
/// `STZ` and `STA` are the same length in every mode both have, so the six
/// rewrites cost nothing; the two bytes are the one `lda #$00` the rewrite
/// orphaned. The `lda #$00` at the function's end stays, and so do the two
/// stores in modes `STZ` lacks.
#[test]
fn the_zero_store_fixture_rewrites_per_store_and_drops_one_load() {
    let (removed, added, bytes) = optimizer_effect_with(
        "opt-zero-store",
        "opt-zero-store.k816",
        include_str!("golden/opt-zero-store.k816"),
        "vars",
    );
    assert_eq!(
        removed,
        vec![
            // Four identical loads the coalescing pass had already collapsed
            // to one, plus the one this rewrite orphaned.
            "lda     #$00",
            "lda     #$00",
            "lda     #$00",
            "lda     #$00",
            "lda     #$00",
            // …and the six stores that became `stz`.
            "sta     a:abs1",
            "sta     a:abs1",
            "sta     a:abs1,x",
            "sta     z:zp",
            "sta     z:zp",
            "sta     z:zp,x",
        ],
        "{removed:?}"
    );
    assert_eq!(
        added,
        vec![
            "stz     a:abs1",
            "stz     a:abs1",
            "stz     a:abs1,x",
            "stz     z:zp",
            "stz     z:zp",
            "stz     z:zp,x",
        ],
        "{added:?}"
    );
    // `STZ` is the same length as `STA`, so only the five loads cost bytes.
    assert_eq!(bytes, 5 * 2);
}

/// The branch fixture, through ca65.
///
/// This is the gate that matters for shrinking: a wrong bound is a `Range
/// error` from the assembler, which is the one mercy in the pass. Both forms
/// are assembled and linked, and the optimized one is smaller by exactly the
/// idiom's overhead per shrunk branch — three bytes each — on top of what the
/// dead-write pass took.
#[test]
fn every_shrunk_branch_is_one_ca65_accepts() {
    let (removed, added, bytes) = optimizer_effect(
        "opt-branch-range",
        "opt-branch-range.k816",
        include_str!("golden/opt-branch-range.k816"),
    );
    // Each shrink turns `bcs :+` + `jmp target` into `bcc target`: two lines
    // out, one line in, three bytes saved. There are three of them.
    assert_eq!(
        removed
            .iter()
            .filter(|line| line.starts_with("bcs"))
            .count(),
        3
    );
    assert_eq!(
        removed
            .iter()
            .filter(|line| line.starts_with("jmp"))
            .count(),
        3
    );
    assert_eq!(
        added.iter().filter(|line| line.starts_with("bcc")).count(),
        3
    );
    // The rest of the difference is the dead `ldx` run each body is made of.
    // Every one of them is overwritten by the next; what survives is the last
    // load on each path, which is live at the function's exit.
    let dead_loads = removed
        .iter()
        .filter(|line| line.starts_with("ldx"))
        .count();
    assert_eq!(dead_loads, 174);
    assert_eq!(bytes, 3 * 3 + dead_loads * 3);
}

/// `opt-tail-labels.k816`: the two G8 passes, through ca65 and ld65.
///
/// `tests/optimizer.rs` asserts which pass fired on which function; this
/// asserts that the bytes agree. Four of the file's seven functions are cases
/// that must *not* change, so the removal counts are one apiece: a fixture
/// where every case fires cannot tell a pass that is too eager from one that
/// is right.
///
/// The second `rts` is F's: `frozen_tail` ends in a `nocross` guard, and the
/// implicit return the compiler puts *after* the guard is outside the run and
/// unreachable. Everything inside the run survives, which is the case the
/// file carries for both new passes at once.
#[test]
fn the_tail_label_fixture_loses_one_branch_and_one_return() {
    let (removed, added, bytes) = optimizer_effect(
        "opt-tail-labels",
        "opt-tail-labels.k816",
        include_str!("golden/opt-tail-labels.k816"),
    );
    assert_eq!(removed, vec!["beq     @next", "rts", "rts"], "{removed:?}");
    assert_eq!(added, Vec::<String>::new());
    assert_eq!(
        bytes,
        2 + 1 + 1,
        "a relative branch, pass 10's return, and an unreachable one"
    );
}

/// **Moved by sub-plan F, task F7.** `branch-range.k816` is E's own fixture:
/// a body of `mem++` statements long enough that the conditional cannot reach
/// its target. Nothing in it is dead, so nothing shrinks, and its long form is
/// still the long form after the optimizer has run.
#[test]
fn the_e_branch_fixture_keeps_its_long_form() {
    let (removed, added, bytes) = optimizer_effect(
        "branch-range",
        "branch-range.k816",
        include_str!("golden/branch-range.k816"),
    );
    assert_eq!(removed, Vec::<String>::new());
    assert_eq!(added, Vec::<String>::new());
    assert_eq!(bytes, 0);
}

// ---------------------------------------------------------------------------
// The completeness net (sub-plan F, decision 21).
// ---------------------------------------------------------------------------

/// What the optimizer changes about **every** golden fixture, and why.
///
/// The tests above assert particular differences through ca65 and ld65, which
/// is what catches a wrong *size*. This catches a wrong *set*: a fixture whose
/// optimized output starts differing for a reason nobody stated. F is the
/// first sub-plan whose entire output is changes to working output, and
/// non-negotiable 15's warning is that accepting a golden diff because "the
/// optimizer got better" is how a wrong-bytes bug ships.
///
/// Each row is `(fixture, removed, added, why)`, counted rather than ordered:
/// the same instruction removed from two functions is one entry with a count
/// of two, and an even count across a paired HLA/native fixture is the
/// identical-treatment claim showing through.
type Effect = (
    &'static str,
    &'static [(&'static str, usize)],
    &'static [(&'static str, usize)],
    &'static str,
);

const FIXTURE_EFFECTS: &[Effect] = &[
    (
        "block-exit.k816",
        &[
            ("jmp     @out", 2),
            ("rep     #$10", 1),
            ("rep     #$20", 2),
            ("sep     #$10", 1),
            ("sep     #$20", 2),
        ],
        &[("sep     #$30", 1)],
        "three block enters whose bodies observe nothing, a restore the loop re-enters through, the nested pad's pair folded, and each pad's jump to the label immediately below it",
    ),
    (
        "opt-tail-labels.k816",
        &[("beq     @next", 1), ("rts", 2)],
        &[],
        "pass 9's branch to the label on the next line, pass 10's second return, and one unreachable implicit return after a `nocross` guard; the four cases beside them are the ones that must not change",
    ),
    (
        "far-calls.k816",
        &[("bcc     @done", 1), ("jmp     a:@direct", 1), ("rts", 3)],
        &[],
        "three implicit returns after functions that leave by `goto` and never come back, and two jumps to a label the fixture writes on the very next line",
    ),
    (
        "hla-alu.k816",
        &[
            ("cmp     #$3C", 2),
            ("cpx     #$01", 2),
            ("sec", 2),
            ("sed", 2),
            ("sei", 2),
            ("sep     #$10", 2),
            ("sep     #$20", 2),
        ],
        &[("sep     #$30", 2)],
        "a flag set overwritten by the clear on the next line, twice over; two compares whose N, Z, and C the next compare rewrites; and the block restore folded. Every count is even: the shorthand half and the native half beside it are treated identically",
    ),
    (
        "hla-chains.k816",
        &[("lda     #$00", 2), ("sta     z:dst0", 2)],
        &[("stz     z:dst0", 2)],
        "a `dst0 = a = 0` chain in each half becomes `stz`, and the load it fed goes with it",
    ),
    (
        "inline-expansion.k816",
        &[("lda     #$0010", 1)],
        &[],
        "one dead load inside an expansion, which is the same treatment a written one gets",
    ),
    (
        "native-modes.k816",
        &[
            ("clc", 1),
            ("cld", 1),
            ("cli", 1),
            ("lda     #$01", 1),
            ("rts", 1),
            ("sep     #$20", 1),
            ("txa", 1),
            ("txs", 1),
            ("tyx", 1),
        ],
        &[],
        "seven writes the next line overwrites, one redundant `sep`, and the implicit return after an unconditional `bra`",
    ),
    (
        "native-reloc.k816",
        &[
            ("lda     #((table + $01) .bitand $0F)", 1),
            ("lda     #.hibyte(table)", 1),
            ("lda     #.lobyte(table)", 1),
            ("rts", 1),
        ],
        &[],
        "three dead loads and an implicit return no edge reaches; not one relocatable operand changes",
    ),
    (
        "opt-branch-range.k816",
        &[
            ("bcs     :+", 3),
            ("jmp     :++", 2),
            ("jmp     :++++", 1),
            ("ldx     #$0102", 174),
        ],
        &[("bcc     :+", 2), ("bcc     :++", 1)],
        "a dead `ldx` run, and the three long-form branches whose distance it closed",
    ),
    (
        "opt-folding.k816",
        &[
            ("rep     #$10", 1),
            ("rep     #$20", 1),
            ("rep     #$21", 1),
            ("sep     #$10", 1),
            ("sep     #$20", 1),
        ],
        &[("rep     #$30", 1), ("sep     #$30", 1)],
        "the document's folding table, plus the mask whose M bit is stripped and whose carry clear stays",
    ),
    (
        "opt-liveout.k816",
        &[("lda     #$07", 1)],
        &[],
        "the one load that is not at a function exit; every other load, `ldy`, and `clc` in the file survives its return",
    ),
    (
        "opt-modes.k816",
        &[
            ("rep     #$10", 1),
            ("rep     #$20", 1),
            ("rep     #$21", 1),
            ("rts", 1),
            ("sep     #$20", 1),
        ],
        &[("rep     #$01", 1), ("rep     #$30", 1)],
        "a stripped mask, a redundant change above a fixed label bridge, a folded bridge, and an unreachable implicit return",
    ),
    (
        "opt-values.k816",
        &[
            ("clc", 1),
            ("lda     #$07", 3),
            ("ldx     #$0000", 1),
            ("sta     a:CELL,x", 3),
        ],
        &[("stz     a:CELL,x", 3)],
        "a repeated load, two more that a memory read and an N/Z-writing mask left dead behind them, a redundant `clc`, an `ldx` sixteen known bits made redundant, and three stores of a known-zero accumulator — the fourth follows an `xba` over a half nothing wrote, and a tracker with lanes leaves it alone",
    ),
    (
        "opt-zero-store.k816",
        &[
            ("lda     #$00", 5),
            ("sta     a:abs1", 2),
            ("sta     a:abs1,x", 1),
            ("sta     z:zp", 2),
            ("sta     z:zp,x", 1),
        ],
        &[
            ("stz     a:abs1", 2),
            ("stz     a:abs1,x", 1),
            ("stz     z:zp", 2),
            ("stz     z:zp,x", 1),
        ],
        "six stores in modes `STZ` has, and the loads that fed them where A is not live at an exit",
    ),
    (
        "slice-native.k816",
        &[("lda     #$00", 1)],
        &[],
        "the slice's dead load, written as a native instruction rather than as a chain",
    ),
    (
        "slice.k816",
        &[("lda     #$00", 1)],
        &[],
        "the slice's dead load: `lda #$01` overwrites A, N, and Z before anything reads them",
    ),
    (
        "subscripts.k816",
        &[
            ("lda     #$0008", 1),
            ("lda     #$02", 2),
            ("lda     #$04", 1),
            ("lda     #$05", 1),
            ("lda     #$06", 1),
            ("lda     #$20", 1),
            ("lda     #.loword((COMP + $08))", 1),
            ("lda     #.loword((COMP + $10))", 1),
        ],
        &[],
        "a run of `NAME:sizeof` loads nothing stores, and one survivor the accumulator already held",
    ),
    (
        "width-blocks.k816",
        &[
            ("jmp     @out", 1),
            ("rep     #$10", 2),
            ("rep     #$20", 2),
            ("sep     #$10", 1),
            ("sep     #$20", 1),
        ],
        &[("rep     #$30", 1), ("sep     #$30", 1)],
        "two block enters whose bodies observe the axis they establish, the documented block's pair folded on both ends, and the pad's jump to the label immediately below it",
    ),
    (
        "width-branch.k816",
        &[("lda     #$01", 1), ("rep     #$20", 1)],
        &[],
        "the load a branch leaves behind, and a user `rep` a fixed label bridge re-establishes",
    ),
    (
        "width-bridging.k816",
        &[("rep     #$10", 1)],
        &[],
        "the one bridge bit on the axis no instruction in the program observes",
    ),
    (
        "width-exit.k816",
        &[("rep     #$10", 1), ("rep     #$20", 1)],
        &[("rep     #$30", 1)],
        "one fold; every operation a declared or inferred exit rests on stays",
    ),
    (
        "width-loop.k816",
        &[("rep     #$20", 1), ("rts", 2)],
        &[],
        "the mode change the loop's own back edge undoes, and the implicit returns after two loops that never end",
    ),
    (
        "width-native-call.k816",
        &[("rts", 1)],
        &[],
        "the implicit return after a tail jump out of the function",
    ),
];

/// A fixture that compiles alone but is not in the table above changes
/// nothing, and one that is changes exactly what its row says.
#[test]
fn no_golden_fixtures_optimized_output_changes_without_a_stated_reason() {
    /// `(fixture, why it cannot be compiled alone)`.
    const SKIPPED: &[(&str, &str)] = &[
        ("contract-unit-b.k816", "one unit of a cross-unit pair"),
        ("layout-unit-b.k816", "one unit of a cross-unit pair"),
        ("opt-crossunit-a.k816", "one unit of a cross-unit pair"),
        ("opt-crossunit-b.k816", "one unit of a cross-unit pair"),
        ("unit-a.k816", "one unit of a cross-unit pair"),
        ("nocross-oversized.k816", "pins a refusal: K816-E0112"),
        (
            "width-emulation.k816",
            "pins a refusal under unknown emulation",
        ),
    ];
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let mut names: Vec<String> = std::fs::read_dir(&dir)
        .expect("read tests/golden")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().to_string_lossy().into_owned();
            (name.ends_with(".k816") && !SKIPPED.iter().any(|(skip, _)| *skip == name))
                .then_some(name)
        })
        .collect();
    names.sort();

    for name in &names {
        let text = std::fs::read_to_string(dir.join(name)).expect("read fixture");
        let compile = |optimize: bool| -> Vec<String> {
            let mut map = SourceMap::new();
            let file = map.add_file(name.as_str(), text.as_str());
            let result = compile_to_ca65(
                &map,
                file,
                &Options {
                    optimize,
                    ..Default::default()
                },
            );
            result
                .assembly
                .map(|a| instruction_lines(&a))
                .unwrap_or_default()
        };
        let before = compile(false);
        if before.is_empty() {
            continue; // Refused, or nothing but declarations.
        }
        let after = compile(true);

        let count = |lines: &[String]| {
            let mut counts: std::collections::BTreeMap<String, usize> =
                std::collections::BTreeMap::new();
            for line in lines {
                *counts.entry(line.clone()).or_default() += 1;
            }
            counts
        };
        let (mut before, mut after) = (count(&before), count(&after));
        for (line, taken) in after.clone() {
            if let Some(had) = before.get_mut(&line) {
                let shared = (*had).min(taken);
                *had -= shared;
                *after.get_mut(&line).expect("just cloned") -= shared;
            }
        }
        let listed = |counts: std::collections::BTreeMap<String, usize>| -> Vec<(String, usize)> {
            counts.into_iter().filter(|(_, n)| *n > 0).collect()
        };
        let (removed, added) = (listed(before), listed(after));

        match FIXTURE_EFFECTS.iter().find(|(fixture, ..)| fixture == name) {
            Some((_, expected_removed, expected_added, why)) => {
                assert!(why.len() > 30, "{name} says too little: {why}");
                let owned = |rows: &[(&str, usize)]| -> Vec<(String, usize)> {
                    rows.iter()
                        .map(|(line, n)| ((*line).to_owned(), *n))
                        .collect()
                };
                assert_eq!(removed, owned(expected_removed), "{name}: {why}");
                assert_eq!(added, owned(expected_added), "{name}: {why}");
            }
            None => assert!(
                removed.is_empty() && added.is_empty(),
                "{name} changed under the optimizer with no row saying why: \
                 -{removed:?} +{added:?}"
            ),
        }
    }

    // The table and the skip list cannot outlive the files they name.
    for (fixture, ..) in FIXTURE_EFFECTS {
        assert!(
            dir.join(fixture).exists(),
            "{fixture} is listed but is gone"
        );
    }
    for (fixture, why) in SKIPPED {
        assert!(
            dir.join(fixture).exists(),
            "{fixture} is skipped but is gone"
        );
        assert!(!why.is_empty(), "{fixture} says nothing");
    }
}

/// **Calling `main` publishes its exit contract**, and the caller's immediate
/// is sized by it.
///
/// The optimizer treats an entry point's inferred exit as constraining nothing,
/// because the startup code cannot be bridged — but only while nothing in the
/// workspace names it. K65 does not refuse a call to `main`, and once one
/// exists the exit is consumed like any other function's.
///
/// This goes through ca65 because the failure is silent at every other level:
/// the `KOp` stream reads fine, the assembly assembles, and the accumulator is
/// eight bits wide where a three-byte immediate was emitted.
#[test]
fn calling_an_entry_point_keeps_the_width_its_caller_was_compiled_against() {
    let source = "\
func main { rep #$20 }

func caller @a8 @i8 {
    call main
    lda #$1234
}
";
    let (removed, added, bytes) = optimizer_effect("called-entry", "called-entry.k816", source);
    assert_eq!(removed, Vec::<String>::new(), "nothing is dead here");
    assert_eq!(added, Vec::<String>::new());
    assert_eq!(bytes, 0);

    let unoptimized = link_fixture(
        "called-entry-bytes",
        "called-entry.k816",
        source,
        &Options {
            optimize: true,
            ..Default::default()
        },
    );
    assert_eq!(
        unoptimized,
        vec![
            0xC2, 0x20, // main: rep #$20 — the instruction `-> @a16` rests on
            0x60, //       rts
            0x20, 0x00, 0x80, // caller: jsr main
            0xA9, 0x34, 0x12, // lda #$1234 — three bytes, and the CPU agrees
            0x60,
        ],
        "{unoptimized:02X?}"
    );
}

// ---------------------------------------------------------------------------
// Data blocks after sub-plan G's task G6
// ---------------------------------------------------------------------------

/// A `nocross { … }` group guards the entries the braces hold, and nothing
/// else.
///
/// The bare directive guards "the leading run of statically-sizeable entries
/// after me", which ends wherever a `code` or a placement directive begins.
/// A group says where instead — so the guard covers three bytes here, not the
/// four the bare form would take, and not the whole section.
#[test]
fn a_nocross_group_guards_its_own_entries() {
    let assembly = compile_fixture(
        "nocross-group.k816",
        "data dd {\n\
         \x20 1\n\
         \x20 nocross { 2 3 4 }\n\
         \x20 5\n\
         }\n",
    );
    let asserts: Vec<&str> = assembly
        .lines()
        .filter(|line| line.contains(".assert"))
        .collect();
    assert_eq!(asserts.len(), 1, "one guard, for one group:\n{assembly}");
    assert!(
        asserts[0].contains('3') || asserts[0].contains("2"),
        "the guard covers the group's own length:\n{}",
        asserts[0]
    );
    // The braces move no byte: the entries emit in source order, around the
    // group rather than after it.
    let data: Vec<&str> = assembly
        .lines()
        .filter(|line| line.trim_start().starts_with(".byte"))
        .collect();
    assert_eq!(data.len(), 5, "five entries, five bytes:\n{assembly}");
}

/// A preprocessor region opened inside a `nocross` group closes inside it.
///
/// The group is a brace, and a brace is where `preprocess.rs` ends a region:
/// an `#if` that reaches the closing brace without its `#endif` is
/// `K816-E0152`, exactly as it is at every other level.
#[test]
fn a_directive_inside_a_nocross_group_pairs_inside_it() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "group-directive.k816",
        "data dd {\n\
         \x20 nocross {\n\
         \x20   #if 1\n\
         \x20   7\n\
         \x20   #endif\n\
         \x20 }\n\
         }\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![], "a paired region is quiet");
    assert!(
        result
            .assembly
            .expect("it compiles")
            .contains(".byte    $07"),
        "the live entry survives"
    );

    let mut map = SourceMap::new();
    let file = map.add_file(
        "group-unbalanced.k816",
        "data dd {\n\
         \x20 nocross {\n\
         \x20   #if 1\n\
         \x20   7\n\
         \x20 }\n\
         \x20 #endif\n\
         }\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(
        codes,
        ["K816-E0152", "K816-E0152"],
        "the `#if` cannot cross the brace, and the `#endif` has nothing above it"
    );
}

/// A `binary` reaches ca65 as `.incbin` with the path the source wrote.
///
/// Two claims in one, and the second is the one decision 8 rests on: K816
/// does not rewrite the path — ca65 resolves it against its own search list
/// — and it does not read the file either, because nothing here needs the
/// bytes at compile time.
#[test]
fn a_binary_is_an_incbin_with_the_path_as_written() {
    let assembly = compile_fixture(
        "tests/assets/incbin.k816",
        "data blob {\n  binary \"table.bin\"\n}\n",
    );
    assert!(
        assembly.contains(".incbin  \"table.bin\""),
        "the path reaches ca65 untouched:\n{assembly}"
    );
    assert!(
        !assembly.contains(".byte"),
        "nothing needed the bytes, so nothing read them:\n{assembly}"
    );
}

/// A section's `evaluator [ ]` transform runs per generated byte, and what it
/// assigns does not outlive it.
///
/// The second half is K816's one divergence on this construct
/// (`data-evaluator-entry`), and it is here rather than in the compatibility
/// corpus because there is nothing to compare it against: the original leaks
/// the assignment, K816 does not, and the row says why.
#[test]
fn a_transform_runs_per_byte_and_keeps_its_assignments() {
    let assembly = compile_fixture(
        "transform.k816",
        "data dd {\n\
         \x20 evaluator [ x * 2 + 1 ]\n\
         \x20 for i = 0 .. 3 eval [ i ]\n\
         \x20 9\n\
         }\n",
    );
    let data: Vec<String> = assembly
        .lines()
        .filter(|line| line.trim_start().starts_with(".byte"))
        .map(|line| {
            let values = line.split(';').next().unwrap_or(line);
            values
                .split_whitespace()
                .skip(1)
                .collect::<Vec<_>>()
                .join("")
        })
        .collect();
    assert_eq!(
        data,
        ["$01,$03,$05,$07", "$09"],
        "generated bytes go through it and a literal does not:\n{assembly}"
    );

    let mut map = SourceMap::new();
    let file = map.add_file(
        "transform-scope.k816",
        "data dd {\n\
         \x20 evaluator [ SIDE = 7 ]\n\
         \x20 for i = 0 .. 1 eval [ i ]\n\
         }\n\
         data e {\n\
         \x20 [ SIDE ]\n\
         }\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(
        codes,
        ["K816-E0101"],
        "a transform runs against a copy per byte, so `SIDE` never escapes"
    );
}

/// A `code { }` subblock inside a `nocross` group is analysed, and it is
/// analysed as *itself*.
///
/// The ordinal a subblock's width analysis is keyed by is its index in the
/// order the resolver pushed it, and the resolver splices a group's entries
/// into the section where they stand. A walk that stopped at the group's
/// brace would skip the subblock inside it — leaving its statements with no
/// tracked width — and hand every subblock after it someone else's widths.
/// Both come out as `K816-E0900`, which is a compiler bug reported as one.
#[test]
fn a_code_subblock_inside_a_nocross_group_gets_its_own_widths() {
    let assembly = compile_fixture(
        "group-code.k816",
        "data dd {\n\
         \x20 nocross { code { @a16 a = $1234 } }\n\
         \x20 code { @a8 a = $12 }\n\
         }\n",
    );
    let loads: Vec<&str> = assembly
        .lines()
        .filter(|line| line.contains("lda"))
        .map(|line| line.split(';').next().unwrap_or(line).trim())
        .collect();
    assert_eq!(
        loads,
        ["lda     #$1234", "lda     #$12"],
        "each subblock takes the width it declares, in source order:\n{assembly}"
    );
}

/// A refused evaluator function inside a section transform is reported.
///
/// The interpreter answers a reserved call with "already reported", so the
/// transform keeps the byte it was handed and the section emits untransformed
/// — which makes a missed *structural* report a wrong image rather than a
/// missing message.
#[test]
fn a_refused_function_inside_a_transform_is_reported() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "transform-rnd.k816",
        "data dd {\n\
         \x20 evaluator [ rnd() ]\n\
         \x20 for i = 0 .. 2 eval [ i ]\n\
         }\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(
        codes,
        ["K816-K65-0002"],
        "the refusal has to arrive, or the section emits untransformed"
    );
}

/// A section's bytes begin at its own label, not at the top of what the block
/// emitted.
///
/// `align 16 + 4` lowers to `.align 16` and a `.res 4`, and the label follows
/// both — the block's name labels the first emitted byte, not the padding
/// that puts it where the source asked. Counting the filler answered about
/// four bytes the label does not cover. The original cannot make the mistake:
/// an alignment is section metadata there and never a code symbol.
#[test]
fn a_sections_bytes_start_at_its_label() {
    let assembly = compile_fixture(
        "aligned-section.k816",
        "data t {\n\
         \x20 align 16 + 4\n\
         \x20 7\n\
         }\n\
         data probe {\n\
         \x20 [ size(t) ]\n\
         \x20 [ index(t, 0) ]\n\
         \x20 [ index(t, 4) ]\n\
         }\n",
    );
    let answers: Vec<String> = assembly
        .lines()
        .filter(|line| line.contains("[ size(t) ]") || line.contains("[ index(t,"))
        .map(|line| {
            line.split(';')
                .next()
                .unwrap_or(line)
                .split_whitespace()
                .nth(1)
                .unwrap_or_default()
                .to_owned()
        })
        .collect();
    assert_eq!(
        answers,
        ["$01", "$07", "$00"],
        "the filler precedes the label and is not the section's:\n{assembly}"
    );

    // Placement *after* the label is refused rather than guessed: `.align`
    // emits padding only ld65 sizes.
    let mut map = SourceMap::new();
    let file = map.add_file(
        "aligned-mid.k816",
        "data t {\n  1\n  align 4\n  2\n}\ndata probe {\n  [ size(t) ]\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_str() == "K816-E0154"),
        "a mid-section alignment leaves no compile-time bytes"
    );
}

/// `binary NAME` names a file called exactly `NAME`.
///
/// `BinLoad -> binary Ident` passes the identifier's own text to
/// `loadbinary` with nothing appended — unlike the `image Ident` rule beside
/// it, which appends `.bmp`.
#[test]
fn a_binary_entry_takes_a_bare_identifier() {
    let assembly = compile_fixture(
        "tests/assets/bare-binary.k816",
        "data blob {\n  binary table\n}\n",
    );
    assert!(
        assembly.contains(".incbin  \"table\""),
        "the identifier's text is the path:\n{assembly}"
    );
}

/// A data section outlives the unit that wrote it.
///
/// `Eval_Index_1D` resolves through the compiler's global section list, so a
/// project is one namespace for section *bytes* as well as for section names
/// — measured (`tests/compat/k65/section-reads-across-units/`). K816 reported
/// the name as undeclared instead, because foreign names are declared after
/// the unit's own and evaluation happens long before that.
///
/// What must not change with the fix: a foreign symbol's *address* still does
/// not cross, and a local declaration still wins its name.
#[test]
fn a_section_can_be_read_from_another_unit() {
    let mut map = SourceMap::new();
    map.add_file("shared-defs.k816", "data shared { 10 20 30 }\n");
    let reader = map.add_file(
        "shared-reader.k816",
        "import \"shared-defs.k816\"\n\
         data probe {\n  [ index(shared, 1) ]\n  [ size(shared) ]\n}\n",
    );
    let linkage = k816::linkage::Linkage::resolve(&map, EntryEnvironment::default());
    let result = compile_to_ca65(
        &map,
        reader,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![], "a foreign section reads");
    let assembly = result.assembly.expect("compiles");
    assert!(
        assembly.contains(".byte    $14") && assembly.contains(".byte    $03"),
        "the answers are the other unit's bytes:\n{assembly}"
    );
    // The bytes crossed; the linkage is what carried them.
    assert!(
        linkage.sections(reader).any(|(name, _)| name == "shared"),
        "the workspace keeps what each unit assembled"
    );

    // A section its own unit could not decide crosses as undecided, so the
    // read says which of the five refusals it is rather than "not declared".
    let mut map = SourceMap::new();
    map.add_file(
        "opaque-defs.k816",
        "data lbl { 1 }\ndata opaque { &&lbl }\n",
    );
    let reader = map.add_file(
        "opaque-reader.k816",
        "import \"opaque-defs.k816\"\ndata probe {\n  [ index(opaque, 0) ]\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        reader,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(codes, ["K816-E0154"]);
    assert!(
        result.diagnostics[0]
            .title
            .contains("only the linker knows"),
        "{:?}",
        result.diagnostics[0].title
    );
}

/// G7, decision 24: an `inline` another unit declares is expanded here.
///
/// An `inline` is the one section kind that does not link — it has no address
/// for ld65 to resolve — so what crosses the boundary is the *body*, copied
/// into the program of the unit that expands it before a symbol is declared.
/// Both predecessors do this: the original compiles a project's units into one
/// state, and the legacy K816 merged the declaration into the using unit's
/// model. K816 accepted the use and then had no body to expand, which reached
/// `K816-E0900` from ordinary source.
///
/// What the copy has to carry with it, and each half is a separate way to get
/// this wrong: the aliases the body declares, bound per use site; the
/// transitive closure, because a borrowed body may expand a third unit's
/// section; and the names the body reads, which become this unit's imports.
#[test]
fn an_inline_crosses_a_unit_boundary_with_its_body() {
    // The defining unit's file-top widths, and the borrowing unit's are the
    // other ones on purpose: an `inline` body is classified once, against the
    // section's own contract, and in its own unit that contract includes the
    // `@a8` above it. Take the borrowing unit's instead and `#$01` becomes
    // `#$0001`.
    let mut map = SourceMap::new();
    map.add_file(
        "bump-defs.k816",
        "@a8\n@i8\nvar counter = $40\n\
         inline sub (a, #step) -> a {\n    c+\n    a - #step\n}\n\
         inline twice (a) -> a {\n    sub a, #1 -> a\n    sub a, #1 -> a\n    counter ++\n}\n",
    );
    let user = map.add_file(
        "bump-user.k816",
        "import \"bump-defs.k816\"\n@a16\n@i16\nfunc main @a8 @i8 {\n    a = 10\n    twice a -> a\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![], "a foreign inline expands");
    let assembly = result.assembly.expect("compiles");
    // Two expansions of `sub`, reached only through `twice`: the closure is
    // transitive or this is one `sbc`.
    assert_eq!(
        assembly.matches("sbc     #$01").count(),
        2,
        "the alias is bound at each expansion:\n{assembly}"
    );
    // The body's own reads are this unit's imports now, because this is where
    // its operations landed.
    assert!(
        assembly.contains(".importzp counter"),
        "a borrowed body's names are imported by the unit that expands it:\n{assembly}"
    );
    // Nothing is called: an `inline` has no address, and none was invented.
    assert!(
        !assembly.contains("jsr") && !assembly.contains("import twice"),
        "the section is expanded, not linked:\n{assembly}"
    );
    // The debug line names the file the operation was written in, which is
    // the other unit. A `.dbg line` naming this unit and counting lines in
    // another would point ld65's database at whatever is on that line here.
    assert!(
        assembly.contains(".dbg    file, \"bump-defs.k816\", 0, 0")
            && assembly.contains(".dbg    line, \"bump-defs.k816\""),
        "a borrowed operation keeps its own source:\n{assembly}"
    );
}

/// A borrowed body's own mistakes are its own unit's to report.
///
/// Sub-plan E's decision 12 keeps an `inline`'s analysis at its declaration so
/// that the diagnostic count does not multiply with the *call* count. Copying
/// the body into every unit that expands it reopens exactly that, against the
/// *unit* count — so a complaint the defining unit already makes is dropped
/// here, and one line takes its place saying where to look.
///
/// The line has to be an error: without it this unit would emit an expansion
/// of a body nothing checked. And the filter has to be per section, and by
/// what the owner actually reports — a name that resolves in the defining unit
/// and not here is the case it must not swallow.
#[test]
fn a_borrowed_body_is_diagnosed_by_the_unit_that_wrote_it() {
    let mut map = SourceMap::new();
    map.add_file(
        "twin-defs.k816",
        "inline good @a8 @i8 {\n    a ++\n}\n\
         inline bad @a8 @i8 {\n    a = MISSING_NAME\n}\n",
    );
    let user = map.add_file(
        "twin-user.k816",
        "import \"twin-defs.k816\"\nfunc main @a8 @i8 {\n    good\n    bad\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    // One line, not the body's own `K816-E0101`, and not one for `good`.
    assert_eq!(codes, ["K816-E0156"]);
    assert!(
        result.diagnostics[0].title.contains("`bad`")
            && result.diagnostics[0]
                .help
                .as_deref()
                .is_some_and(|help| help.contains("twin-defs.k816")),
        "the line names the section and the unit to read: {:?}",
        result.diagnostics[0]
    );

    // A layout does not cross a unit boundary, so `slot.hi` resolves where the
    // body is written and not where it is expanded. The owner makes no such
    // complaint, so this one survives with its own message rather than being
    // collapsed into the line above.
    let mut map = SourceMap::new();
    map.add_file(
        "layout-defs.k816",
        "var slot[ .lo:byte .hi:byte ]\ninline peek @a8 @i8 {\n    a = slot.hi\n}\n",
    );
    let user = map.add_file(
        "layout-user.k816",
        "import \"layout-defs.k816\"\nfunc main @a8 @i8 {\n    peek\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(codes, ["K816-E0115"], "what borrowing revealed stays");
}

/// G7, and the original's own spelling: `far goto NAME` is `goto far NAME`.
///
/// `Instr -> far goto Arg` (`compiler.inc:156`). Without the alias the two
/// words reach the keyword-less `far NAME` call reading — `goto` lexes as an
/// identifier where a rule asks for one — and a valid K65 line becomes a call
/// to an undeclared `goto` followed by a near call to the target. Decision 0
/// forbids exactly that: a K65 form K816 does not accept is refused by name,
/// never given a different meaning.
#[test]
fn a_far_goto_takes_the_k65_word_order() {
    for source in [
        "far func banked @a8 @i8 {\n    a = 1\n}\nfunc jumps @a8 @i8 {\n    far goto banked\n}\n",
        "far func banked @a8 @i8 {\n    a = 1\n}\nfunc jumps @a8 @i8 {\n    goto far banked\n}\n",
    ] {
        let mut map = SourceMap::new();
        let file = map.add_file("far-goto.k816", source);
        let result = compile_to_ca65(
            &map,
            file,
            &Options {
                optimize: false,
                ..Default::default()
            },
        );
        assert_eq!(result.diagnostics, vec![], "{source}");
        assert!(
            result
                .assembly
                .expect("compiles")
                .contains("jml     f:banked"),
            "both spellings are the same long jump: {source}"
        );
    }
}

/// A borrowed copy is not a second node in the workspace graph.
///
/// `classify_exits` keys the call graph, the observation fixpoint and the
/// contract table by *name*, on the stated premise that a name is unique
/// across the workspace because two units defining one is already an error.
/// Copying an `inline` body broke that premise: the section is a function of
/// every model that expands it, so a second node overwrote the first
/// according to traversal order — and the winner decided what an unrelated
/// unit's function advertised as its exit.
///
/// The failure is a miscompile, not a diagnostic: `main` believed `wrapper`
/// left the accumulator 16-bit, sized its immediate to three bytes, and
/// `wrapper` emitted no `rep` at all. The sibling that caused it does not
/// call `wrapper` and is not even linked with it.
#[test]
fn a_borrowed_copy_is_not_a_workspace_node() {
    let sources = |sibling: bool| {
        let mut map = SourceMap::new();
        map.add_file(
            "graph-defs.k816",
            "inline shared @a8 @i8 {\n    a = 0\n}\nfunc wrapper @a8 @i8 {\n    shared\n}\n",
        );
        if sibling {
            map.add_file(
                "graph-sibling.k816",
                "import \"graph-defs.k816\"\n@a16\n@i16\nfunc sibling {\n    shared\n}\n",
            );
        }
        let caller = map.add_file(
            "graph-main.k816",
            "import \"graph-defs.k816\"\n@a8\n@i8\nmain {\n    call wrapper\n    a = 1\n}\n",
        );
        let result = compile_to_ca65(
            &map,
            caller,
            &Options {
                optimize: false,
                ..Default::default()
            },
        );
        assert_eq!(result.diagnostics, vec![], "sibling={sibling}");
        result.assembly.expect("compiles")
    };
    let alone = sources(false);
    let with_sibling = sources(true);
    assert!(
        alone.contains("lda     #$01"),
        "the accumulator is 8-bit here:\n{alone}"
    );
    // A unit that merely borrows the same body, and calls nothing in this
    // one, cannot change a byte of it.
    assert_eq!(
        alone, with_sibling,
        "a borrowing sibling changed an unrelated unit's output"
    );
}

/// A borrowed section's contract is finished when it arrives.
///
/// The module defaults that apply to a body are its *own* unit's; they are
/// folded into the copy where it is taken. An axis the declaration leaves
/// unspecified is unspecified at home too, and merging the *borrowing* unit's
/// defaults into it gives the body a width nobody wrote — and its caller a
/// bridge nobody asked for, which is left in force after the body ends.
#[test]
fn a_borrowed_body_does_not_take_the_borrowers_defaults() {
    let mut map = SourceMap::new();
    map.add_file("plain-defs.k816", "inline borrowed {\n    nop\n}\n");
    let user = map.add_file(
        "plain-user.k816",
        "import \"plain-defs.k816\"\n@a16\n@i16\nfunc user @a8 @i8 {\n    borrowed\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![]);
    let assembly = result.assembly.expect("compiles");
    assert!(
        !assembly.contains("rep") && !assembly.contains("sep"),
        "a body that states no width asks for no bridge:\n{assembly}"
    );
    // The same source compiled in one unit is the answer to compare against:
    // borrowing may not change what a body means.
    let mut map = SourceMap::new();
    let together = map.add_file(
        "plain-together.k816",
        "inline borrowed {\n    nop\n}\nfunc user @a8 @i8 {\n    borrowed\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        together,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let local = result.assembly.expect("compiles");
    let body = |text: &str| {
        text.lines()
            .filter(|line| line.trim_start().starts_with("nop"))
            .count()
    };
    assert_eq!(body(&assembly), body(&local), "{assembly}\n---\n{local}");
}

/// A borrowed body reads its own unit's compile-time values.
///
/// Its brackets were written where its `const`s are, and the body arrives
/// having never been evaluated. `declare_imports` cannot help: it runs *after*
/// `declare_items`, which is where brackets run. So the workspace's `const`
/// values are seeded beside its section bytes, consulted only after this
/// unit's symbol table has said no — the rule that keeps a local declaration
/// winning its name.
///
/// A name a top-level evaluator block or a `#if` condition assigned is a
/// `const` symbol by the time it is a definition, so both spellings answer.
#[test]
fn a_borrowed_body_reads_its_own_units_constants() {
    for (defs, expected) in [
        (
            "const VALUE = 7\ninline borrowed {\n    a = [ VALUE ]\n}\n",
            "lda     #$07",
        ),
        (
            "[ VALUE = 3 * 4 ]\ninline borrowed {\n    a = [ VALUE ]\n}\n",
            "lda     #$0C",
        ),
        (
            "#if [ VALUE = 7 ]\n#endif\ninline borrowed {\n    a = [ VALUE ]\n}\n",
            "lda     #$07",
        ),
    ] {
        let mut map = SourceMap::new();
        map.add_file("const-defs.k816", format!("@a8\n@i8\n{defs}"));
        let user = map.add_file(
            "const-user.k816",
            "import \"const-defs.k816\"\n@a8\n@i8\nmain {\n    borrowed\n}\n",
        );
        let result = compile_to_ca65(
            &map,
            user,
            &Options {
                optimize: false,
                ..Default::default()
            },
        );
        assert_eq!(result.diagnostics, vec![], "{defs}");
        assert!(
            result.assembly.expect("compiles").contains(expected),
            "{defs} should fold to {expected}"
        );
    }
}

/// A body the parser had to recover inside is never expanded.
///
/// Recovery produces a *shorter* body, not a broken one, so copying it would
/// put the compiler's guess in the borrower's object and say nothing: the
/// `inline` below expanded as no instructions at all and the unit emitted a
/// clean object. The parse diagnostics belong to the defining unit, so the
/// borrower gets the same line it gets for a body that does not compile.
#[test]
fn a_body_the_parser_recovered_inside_is_not_expanded() {
    let mut map = SourceMap::new();
    map.add_file(
        "broken-defs.k816",
        "@a8\n@i8\ninline broken {\n    =\n}\ninline sound {\n    nop\n}\n",
    );
    let user = map.add_file(
        "broken-user.k816",
        "import \"broken-defs.k816\"\n@a8\n@i8\nmain {\n    broken\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes: Vec<&str> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(codes, ["K816-E0156"]);
    assert!(result.assembly.is_none(), "nothing is emitted");

    // Its neighbour in the same file parses, so it is borrowed as usual: one
    // unparsable section does not condemn the file it is in.
    let mut map = SourceMap::new();
    map.add_file(
        "broken-defs.k816",
        "@a8\n@i8\ninline broken {\n    =\n}\ninline sound {\n    nop\n}\n",
    );
    let user = map.add_file(
        "sound-user.k816",
        "import \"broken-defs.k816\"\n@a8\n@i8\nmain {\n    sound\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        user,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![]);
    assert!(result.assembly.expect("compiles").contains("nop"));
}

/// A unit-level name inside an `inline` is refused whatever declares it.
///
/// `declare_labels_in` has always refused a *global label* there, and says
/// why: the body is copied per use site, so a unit-level name in it is
/// exported and never defined. A `data` block's own name and a `label:` entry
/// inside one are unit-level names by exactly the same argument, and the rule
/// reached neither — sub-plan E's, not G7's, and found by probing G7's
/// neighbourhood for the same class of defect.
///
/// The failure had no diagnostic at all: ca65 assembles the `.export`, and
/// ld65 answers "Exported symbol `tbl` was never defined". Once two units
/// borrow the body it is two objects exporting one name.
#[test]
fn a_unit_level_name_inside_an_inline_is_refused() {
    for source in [
        "inline emit {\n    data tbl {\n        7 8\n    }\n}\nfunc a1 @a8 @i8 {\n    emit\n}\nmain @a8 @i8 {\n    emit\n}\n",
        "inline emit {\n    data {\n        spot: 7 8\n    }\n}\nmain @a8 @i8 {\n    emit\n}\n",
    ] {
        let mut map = SourceMap::new();
        let file = map.add_file("named-in-inline.k816", source);
        let result = compile_to_ca65(
            &map,
            file,
            &Options {
                optimize: false,
                ..Default::default()
            },
        );
        let codes: Vec<&str> = result
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str())
            .collect();
        assert_eq!(codes, ["K816-E0102"], "{source}");
        assert!(
            result.diagnostics[0]
                .help
                .as_deref()
                .is_some_and(|help| help.contains("anonymous")),
            "the help names what does work: {:?}",
            result.diagnostics[0].help
        );
    }

    // An *anonymous* block is the shape that works, and it has to keep
    // working: nothing is named, so nothing is exported, and the bytes are
    // stepped over at each use site.
    let mut map = SourceMap::new();
    let file = map.add_file(
        "anon-in-inline.k816",
        "inline emit {\n    data {\n        7 8\n    }\n}\nmain @a8 @i8 {\n    emit\n    emit\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![]);
    let assembly = result.assembly.expect("compiles");
    assert_eq!(
        assembly.matches(".byte").count(),
        4,
        "both expansions carry their own copy:\n{assembly}"
    );
}

/// What a borrowed `inline` declaration depends on, one case each.
///
/// The four defects §G.10 records were all the same omission — a declaration
/// is not self-contained — and none of them had a fixture. This is that list
/// turned into cases, so that adding a fifth dependency means adding a row
/// here rather than discovering it in review.
///
/// The `evalfunc` row is a *limit*, pinned deliberately: the evaluator's
/// functions live in its own table rather than in the symbol table, so they
/// do not cross. It fails as an error rather than as a byte, and it is a
/// decision rather than an accident because this asserts it.
#[test]
fn a_borrowed_declaration_carries_what_it_depends_on() {
    /// What the borrowing unit should see: a needle in its assembly, or the
    /// diagnostics it reports instead.
    type Expected = Result<&'static str, &'static [&'static str]>;
    /// What the defining unit provides, the borrowed body, and the answer.
    type Case = (&'static str, &'static str, Expected);
    let cases: &[Case] = &[
        // Its own module's widths, on an axis the section does not state.
        ("@a8\n@i8\n", "a = 1", Ok("lda     #$01")),
        // A `const` of its own unit, read through a bracket.
        (
            "@a8\n@i8\nconst NINE = 9\n",
            "a = [ NINE ]",
            Ok("lda     #$09"),
        ),
        // A name a top-level evaluator block assigned, which is a `const` by
        // the time it is a definition.
        (
            "@a8\n@i8\n[ NINE = 9 ]\n",
            "a = [ NINE ]",
            Ok("lda     #$09"),
        ),
        // A `var` of its own unit, which becomes this unit's import.
        (
            "@a8\n@i8\nvar slot = $40\n",
            "slot = a",
            Ok("sta     z:slot"),
        ),
        // A data section of its own unit, read at compile time.
        (
            "@a8\n@i8\ndata tab {\n  4 5 6\n}\n",
            "a = [ index(tab, 1) ]",
            Ok("lda     #$05"),
        ),
        // The limit: an `evalfunc` does not cross, and says so as an error.
        (
            "@a8\n@i8\nevalfunc twice(n) [ n * 2 ]\n",
            "a = [ twice(4) ]",
            Err(&["K816-E0101"]),
        ),
    ];
    for (provides, body, expected) in cases {
        let mut map = SourceMap::new();
        map.add_file(
            "carry-defs.k816",
            format!("{provides}inline borrowed {{\n    {body}\n}}\n"),
        );
        let user = map.add_file(
            "carry-user.k816",
            "import \"carry-defs.k816\"\n@a16\n@i16\nmain @a8 @i8 {\n    borrowed\n}\n",
        );
        let result = compile_to_ca65(
            &map,
            user,
            &Options {
                optimize: false,
                ..Default::default()
            },
        );
        let codes: Vec<&str> = result
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str())
            .collect();
        match expected {
            Ok(needle) => {
                assert_eq!(codes, Vec::<&str>::new(), "{body}");
                assert!(
                    result.assembly.expect("compiles").contains(needle),
                    "{body} should emit {needle}"
                );
            }
            Err(wanted) => assert_eq!(&codes[..], *wanted, "{body}"),
        }
    }
}
/// Narrowing after a wide load is a construct, not a dead value.
///
/// `@a16`, `lda`, `@a8`, `sta`, `xba`, `sta` is how a program splits a
/// sixteen-bit value into two bytes: the high half is reached by exchanging
/// it into the low one, not by being read where it was written. The legacy
/// compiler reported "accumulator value is dead after switching to @a8" here
/// and K816 declines the check on purpose — this is the program that would
/// have been refused, pinned at the bytes so a later liveness rule cannot
/// take it away by accident.
#[test]
fn a_wide_load_narrowed_and_exchanged_is_two_stores() {
    let (bytes, _) = compile_and_link(
        "split-halves",
        "split.k816",
        "var lo = $80\nvar hi = $81\n\n\
         func split @a16 @i8 {\n\
         \x20   lda #$1234\n    @a8\n    lo = a\n    xba\n    hi = a\n}\n",
        &Options {
            optimize: false,
            ..Default::default()
        },
    );

    assert_eq!(
        &bytes[..],
        &[
            0xA9, 0x34, 0x12, // lda #$1234, sixteen bits of it
            0xE2, 0x20, //       sep #$20
            0x85, 0x80, //       sta $80 — the low half
            0xEB, //             xba
            0x85, 0x81, //       sta $81 — the half a dead-value rule would
            0x60, //             have called unreachable
        ],
        "{bytes:02X?}"
    );
}

/// A callee with no width annotation runs at its caller's width, and the
/// bytes say so.
///
/// `K816-E0104` refuses a width-dependent *immediate* under an unknown width
/// and nothing else, which is what lets a function with no annotation exist
/// at all — and what a K65 source is made of, since a 6502 program annotates
/// nothing. The claim that rule rests on is here at the bytes: the callee
/// establishes no width, so it emits no mode operation and no assumption
/// directive, and the caller's belief survives the call, so the immediate
/// after it is still three bytes.
#[test]
fn an_unannotated_callee_runs_at_its_callers_width() {
    const SOURCE: &str = "var slot = $80\n\n\
                          func helper {\n    slot = a\n}\n\n\
                          func caller @a16 @i16 {\n\
                          \x20   lda #$1234\n    call helper\n    lda #$5678\n    slot = a\n}\n";
    let (bytes, _) = compile_and_link(
        "unannotated-callee",
        "context.k816",
        SOURCE,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );

    assert_eq!(
        &bytes[..],
        &[
            // helper: `sta $80` and `rts`, and nothing before them.
            0x85, 0x80, 0x60, //
            // caller: a 16-bit immediate, the call, and a 16-bit immediate
            // *after* it — the width the caller established is still in force.
            0xA9, 0x34, 0x12, //
            0x20, 0x00, 0x80, //
            0xA9, 0x78, 0x56, //
            0x85, 0x80, 0x60,
        ],
        "{bytes:02X?}"
    );

    // And the assembler is told the same thing: `caller` states its contract,
    // `helper` states nothing, so ca65 sizes `helper` under whatever is in
    // force where it is reached rather than under a claim K816 invented.
    let assembly = compile_fixture("context.k816", SOURCE);
    let helper = assembly
        .split_once("helper:")
        .expect("the callee")
        .1
        .split_once("caller:")
        .expect("the caller after it")
        .0;
    for directive in [".a8", ".a16", ".i8", ".i16", "rep", "sep"] {
        assert!(
            !helper.contains(directive),
            "`{directive}` in an unannotated body:\n{helper}"
        );
    }
}

/// A label inside a body is the address a code byte is read through.
///
/// The name of a `func` is not (`K816-E0115`, and the manifest row
/// `k816-function-name-is-not-a-place`): a body is an operation stream the
/// optimizer rewrites, so the offset of its first byte is not one the
/// compiler can keep. A label is what the author places, and the diagnostic's
/// help sends the reader here — so the spelling it recommends is gated at the
/// bytes rather than left as advice.
#[test]
fn a_label_in_a_body_is_the_address_a_code_byte_is_read_through() {
    let (bytes, _) = compile_and_link(
        "code-byte-label",
        "patch.k816",
        "var slot = $80\n\n\
         func target @a8 @i8 {\npatch:\n    slot = a\n}\n\n\
         func main @a8 @i8 {\n    lda patch\n    slot = a\n}\n",
        &Options {
            optimize: false,
            ..Default::default()
        },
    );

    // `target` is first, so `patch` is the load address itself: $8000 under
    // `tests/golden/slice.cfg`.
    assert_eq!(
        &bytes[..8],
        &[0x85, 0x80, 0x60, 0xAD, 0x00, 0x80, 0x85, 0x80],
        "sta $80 / rts, then lda $8000 / sta $80: {bytes:02X?}"
    );
}

/// A stack offset is a number, and it reaches the object file as one.
///
/// `lda off,S` encodes one byte counted from the stack pointer: the operand
/// is a position in a frame, not a location in memory, so a `const` is what
/// belongs there. K816 refused one until the legacy corpus was surveyed —
/// `K816-E0110`, the rule for operands that really are addresses — and
/// accepted a `var` in its place, which is the same mistake read backwards:
/// ld65 would have chosen the address and the frame would have been indexed
/// by its low byte.
///
/// The legacy fixture `syntax-native-native` is the oracle for the bytes:
/// `const nameptr = $12` with `lda nameptr,s` is `A3 12`.
#[test]
fn a_stack_offset_links_as_the_number_it_names() {
    let (bytes, _) = compile_and_link(
        "stack-offset",
        "stack.k816",
        "const nameptr = $12\nconst deep = $04\n\
         func frame @a8 @i8 {\n    lda nameptr,s\n    a = (deep,s),y\n}\n",
        &Options {
            optimize: false,
            ..Default::default()
        },
    );

    assert_eq!(
        &bytes[..5],
        &[0xA3, 0x12, 0xB3, 0x04, 0x60],
        "lda $12,S / lda ($04,S),Y / rts: {bytes:02X?}"
    );
}

/// An `inline`'s place alias is a native operand like any variable, and a
/// value alias in that position is told where its `#` went.
///
/// `sta where` on a `(a, #where)` parameter used to trip the section-name
/// check's own assertion — a value alias is not a section and the check was
/// written for one — which `doc/ca65-to-k816.md`'s macro pair found. The
/// place form is what the guide shows, and it links to the same bytes as
/// the ca65 macro it replaces.
#[test]
fn a_native_operand_may_name_an_inline_place_alias_but_not_a_value_alias() {
    let place = "var out = $0200\ninline store @a8 (a, where) -> a {\n    sta where\n}\nfunc start @a8 @i8 {\n    lda #$42\n    store a, #out -> a\n}\n";
    let mut map = SourceMap::new();
    let file = map.add_file("place.k816", place);
    let result = compile_to_ca65(&map, file, &Options::default());
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
    let assembly = result.assembly.expect("compiles");
    assert!(assembly.contains("sta     a:out"), "{assembly}");

    let value = place.replace("(a, where)", "(a, #where)");
    let mut map = SourceMap::new();
    let file = map.add_file("value.k816", value);
    let result = compile_to_ca65(&map, file, &Options::default());
    let kinds: Vec<&str> = result.diagnostics.iter().map(|d| d.code.as_str()).collect();
    assert_eq!(kinds, ["K816-E0115"], "{:?}", result.diagnostics);
    let found = &result.diagnostics[0];
    assert!(found.title.contains("`#where`"), "{}", found.title);
    assert!(
        found
            .help
            .as_deref()
            .is_some_and(|help| help.contains("without `#`")),
        "{found:?}"
    );
}
