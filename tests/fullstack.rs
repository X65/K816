/*****************************************************************************/
/*                                                                           */
/*                                fullstack.rs                               */
/*                                                                           */
/*      Full-stack test: .k816 + hand-written ca65 .asm -> 65816 binary      */
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

//! Builds `examples/fullstack/` — the K816 module compiled by the *real*
//! `k816` CLI binary, linked with the hand-written ca65 startup module —
//! and structurally verifies the resulting 32 KiB 65816 ROM image: layout,
//! cross-module symbol resolution, vector table, optimizer effect, and
//! debug-database source ownership. (No execution: this tree has no 65816
//! simulator yet.)

mod common;

use std::path::PathBuf;
use std::process::Command;

use common::{run_ok, tool, toolchain, work_dir};

const ROM_BASE: usize = 0x8000;
const ROM_SIZE: usize = 0x8000;

/// The startup stub at $8000 followed by main's *optimized* code at $8008.
/// The `jsr $8008` operand is the linker-resolved cross-module reference to
/// the K816 `main`; the dead `lda #$00` is absent.
///
/// `main @a8 @i16` emits only the `REP` half of its entry setup, and this is
/// the program that shows why that is sound on real linked bytes: the
/// `clc`/`xce` pair above leaves emulation mode, where the M and X flags are
/// forced to one, so the accumulator is already 8-bit when `main` is entered.
/// The `lda #$42` below is still assembled as the two-byte form, from the
/// width claim rather than from an instruction.
const CODE_PREFIX: &[u8] = &[
    0x78, // sei
    0x18, // clc
    0xFB, // xce               — enter native mode
    0x20, 0x08, 0x80, // jsr $8008  — k816 main, resolved across modules
    0xDB, // stp
    0x40, // rti               — shared stray-interrupt handler ($8007)
    // No entry setup at all: `main @a8` restates what the stub provides, and
    // an annotation on `main` is a claim about the entry ABI rather than a
    // request K816 satisfies with a `rep`.
    0xA9, 0x42, // lda #$42    — the dead lda #$00 is optimized away
    0x8D, 0x00, 0x02, // sta $0200
    0x60, // rts
];

const RESET: u16 = 0x8000;
const IRQ_STUB: u16 = 0x8007;

/// The WDC vector table at $FFE4..=$FFFF as (offset, expected target).
const VECTORS: &[(usize, u16)] = &[
    (0xFFE4, IRQ_STUB), // native COP
    (0xFFE6, IRQ_STUB), // native BRK
    (0xFFE8, IRQ_STUB), // native ABORT
    (0xFFEA, IRQ_STUB), // native NMI
    (0xFFEC, 0),        // reserved
    (0xFFEE, IRQ_STUB), // native IRQ
    (0xFFF0, 0),        // reserved
    (0xFFF2, 0),        // reserved
    (0xFFF4, IRQ_STUB), // emulation COP
    (0xFFF6, 0),        // reserved
    (0xFFF8, IRQ_STUB), // emulation ABORT
    (0xFFFA, IRQ_STUB), // emulation NMI
    (0xFFFC, RESET),    // emulation RESET
    (0xFFFE, IRQ_STUB), // emulation IRQ/BRK
];

fn example_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/fullstack")
}

/// Builds the example app by hand — `k816 compile`, two `ca65`, an `ld65` —
/// driving the CLI binary Cargo built for this test run.
///
/// This is what `examples/fullstack/Makefile` spelled out in four rules
/// before sub-plan I replaced them with one `cl65` line. It stays because it
/// is the *oracle*: [`build_image_through_cl65`] below is the same program
/// built by the driver, and the two are required to agree.
fn build_image(test: &str) -> (Vec<u8>, String) {
    let example = example_dir();
    let dir = work_dir(test);
    let main_s = dir.join("main.s");
    let main_o = dir.join("main.o");
    let startup_o = dir.join("startup.o");
    let bin_path = dir.join("fullstack.bin");
    let dbg_path = dir.join("fullstack.dbg");

    run_ok(
        Command::new(env!("CARGO_BIN_EXE_k816"))
            .arg("compile")
            .arg(example.join("main.k816"))
            .args(["-o".as_ref(), main_s.as_os_str()]),
        "k816 compile",
    );
    run_ok(
        Command::new(tool("CA65", "ca65"))
            .arg(&main_s)
            .args(["-o".as_ref(), main_o.as_os_str()]),
        "ca65 on the k816-generated module",
    );
    run_ok(
        Command::new(tool("CA65", "ca65"))
            .arg(example.join("startup.asm"))
            .args(["-o".as_ref(), startup_o.as_os_str()]),
        "ca65 on the hand-written startup module",
    );
    run_ok(
        Command::new(tool("LD65", "ld65"))
            .args(["-C".as_ref(), example.join("fullstack.cfg").as_os_str()])
            .args(["-o".as_ref(), bin_path.as_os_str()])
            .args(["--dbgfile".as_ref(), dbg_path.as_os_str()])
            .arg(&startup_o)
            .arg(&main_o),
        "ld65 linking both modules",
    );

    (
        std::fs::read(&bin_path).expect("read linked image"),
        std::fs::read_to_string(&dbg_path).expect("read debug file"),
    )
}

/// Builds the same example with one `cl65` command line — the Makefile's.
///
/// `cl65` finds its tools beside itself, so it is run from the staged
/// directory `common::toolchain` builds; otherwise this would exercise
/// whatever `make k816` last installed rather than the compiler under test.
fn build_image_through_cl65(test: &str) -> (Vec<u8>, String) {
    let example = example_dir();
    let dir = work_dir(test);
    let bin_path = dir.join("fullstack.bin");
    let dbg_path = dir.join("fullstack.dbg");

    run_ok(
        Command::new(toolchain().join("cl65"))
            // `-t none` because cl65 defaults to the C64, and
            // `--no-target-lib` because -C replaces the target's
            // configuration but not its library.
            .args(["-t", "none", "--no-target-lib"])
            .args(["-C".as_ref(), example.join("fullstack.cfg").as_os_str()])
            // Two argv elements: cl65 reads `-Wl` only when it is the whole
            // argument, and `-Wl--dbgfile,…` falls into the "anything else
            // after -W is a cc65 warning" branch and is silently dropped.
            .arg("-Wl")
            .arg(format!("--dbgfile,{}", dbg_path.display()))
            .args(["-o".as_ref(), bin_path.as_os_str()])
            // The startup module first: it owns the STARTUP segment and the
            // reset vector, and cl65 links each object where its source
            // appears on the command line.
            .arg(example.join("startup.asm"))
            .arg(example.join("main.k816")),
        "cl65 building the full-stack example",
    );

    (
        std::fs::read(&bin_path).expect("read linked image"),
        std::fs::read_to_string(&dbg_path).expect("read debug file"),
    )
}

fn word_at(image: &[u8], address: usize) -> u16 {
    let offset = address - ROM_BASE;
    u16::from_le_bytes([image[offset], image[offset + 1]])
}

#[test]
fn the_linked_image_is_a_complete_65816_rom() {
    let (image, _) = build_image("fullstack-rom");

    // A full 32 KiB ROM image, padded with the configured fill byte.
    assert_eq!(image.len(), ROM_SIZE);
    assert_eq!(&image[..CODE_PREFIX.len()], CODE_PREFIX);
    let gap = &image[CODE_PREFIX.len()..0xFFE4 - ROM_BASE];
    assert!(gap.iter().all(|&byte| byte == 0xFF), "fill area disturbed");

    // The complete WDC vector table, with RESET entering the startup stub.
    for &(address, target) in VECTORS {
        assert_eq!(word_at(&image, address), target, "vector at ${address:04X}");
    }
}

#[test]
fn debug_database_names_both_source_modules() {
    let (_, dbg) = build_image("fullstack-dbg");

    let file_lines: Vec<&str> = dbg
        .lines()
        .filter(|line| line.starts_with("file"))
        .collect();
    let find_id = |suffix: &str| -> Option<String> {
        file_lines
            .iter()
            .find(|line| line.contains(suffix))
            .and_then(|line| {
                line.split(',')
                    .find_map(|field| field.strip_prefix("file\tid="))
            })
            .map(str::to_owned)
    };

    // Both modules are in the linker's file table…
    let k816_id = find_id("main.k816\"").expect("main.k816 in file table");
    assert!(
        find_id("startup.asm\"").is_some(),
        "startup.asm in file table: {file_lines:?}"
    );

    // …and the K816 source owns generated code lines (`.dbg line` records).
    assert!(
        dbg.lines().any(|line| {
            line.starts_with("line")
                && line.contains(&format!("file={k816_id}"))
                && line.contains("type=1")
        }),
        "no K816-owned line records in:\n{dbg}"
    );
}

/// The driver builds the same ROM, and the same debug ownership, as the hand
/// recipe it replaced.
///
/// `examples/fullstack/Makefile` is one `cl65` rule now, and this is what
/// says the rule is the four it replaced: the image is byte-identical, and it
/// still passes the two structural assertions above. If the driver ever adds
/// something to or loses something from the pipeline, this is where a program
/// with a hand-written module, a cross-module call and a vector table says so.
#[test]
fn the_driver_builds_the_same_rom_and_names_the_same_sources() {
    let (by_hand, _) = build_image("fullstack-manual-for-driver");
    let (through_driver, dbg) = build_image_through_cl65("fullstack-driver");

    assert_eq!(
        through_driver, by_hand,
        "cl65's image is not the manual path's"
    );

    // The same two assertions the manual path is held to, restated against
    // the driver's bytes rather than inferred from the equality above: an
    // equality between two wrong images would satisfy it.
    assert_eq!(through_driver.len(), ROM_SIZE);
    assert_eq!(&through_driver[..CODE_PREFIX.len()], CODE_PREFIX);
    for &(address, target) in VECTORS {
        assert_eq!(
            word_at(&through_driver, address),
            target,
            "vector at ${address:04X}"
        );
    }

    // And both sources are in the linker's file table. The K816 module gets
    // there through the `.dbg file` k816 emits, which names the source rather
    // than the intermediate assembly cl65 handed ca65 — so a driver that lost
    // the source's identity would show up here even though the bytes matched.
    for source in ["main.k816\"", "startup.asm\""] {
        assert!(
            dbg.lines()
                .any(|line| line.starts_with("file") && line.contains(source)),
            "{source} is not in the debug file table"
        );
    }
}
