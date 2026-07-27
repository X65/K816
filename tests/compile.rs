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

use std::path::PathBuf;
use std::process::Command;

use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

fn tool(env: &str, name: &str) -> PathBuf {
    if let Ok(path) = std::env::var(env) {
        return PathBuf::from(path);
    }
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../bin")
        .join(name)
}

fn work_dir(test: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(test);
    std::fs::create_dir_all(&dir).expect("create work dir");
    dir
}

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

    let ca65 = Command::new(tool("CA65", "ca65"))
        .arg(&asm_path)
        .args(["-o".as_ref(), obj_path.as_os_str()])
        .output()
        .expect("run ca65 (build it with `make -C ../.. ca65` first)");
    assert!(
        ca65.status.success(),
        "ca65 failed:\n{}",
        String::from_utf8_lossy(&ca65.stderr)
    );

    let config = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden/slice.cfg");
    let ld65 = Command::new(tool("LD65", "ld65"))
        .args(["-C".as_ref(), config.as_os_str()])
        .args(["-o".as_ref(), bin_path.as_os_str()])
        .args(["--dbgfile".as_ref(), dbg_path.as_os_str()])
        .arg(&obj_path)
        .output()
        .expect("run ld65 (build it with `make -C ../.. ld65` first)");
    assert!(
        ld65.status.success(),
        "ld65 failed:\n{}",
        String::from_utf8_lossy(&ld65.stderr)
    );

    (
        std::fs::read(&bin_path).expect("read linked binary"),
        std::fs::read_to_string(&dbg_path).expect("read debug file"),
    )
}

/// The unoptimized slice: entry setup, both loads, store, return.
const UNOPTIMIZED_BYTES: &[u8] = &[
    0xE2, 0x20, // sep #$20  — entry @a8
    0xC2, 0x10, // rep #$10  — entry @i16
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
