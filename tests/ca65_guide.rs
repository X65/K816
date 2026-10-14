/*****************************************************************************/
/*                                                                           */
/*                               ca65_guide.rs                               */
/*                                                                           */
/*      doc/ca65-to-k816.md: every paired listing assembles to one image     */
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

//! `doc/ca65-to-k816.md` says what K816 writes where ca65 writes each
//! thing, and a table of spellings is exactly the document that rots: a
//! row is written from memory, the compiler changes, and a reader converting
//! a file by hand trusts it. So every pair of listings the guide names —
//! a `ca65 NAME` block and a `k65 NAME` block — is assembled here, both
//! sides, and linked with one configuration to one image; the two images
//! must be the same bytes. A K816 listing with no partner must at least
//! compile.

mod common;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use common::{run_ok, tool, work_dir};

struct Block {
    language: String,
    name: Option<String>,
    body: String,
    line: usize,
}

fn guide() -> String {
    std::fs::read_to_string(Path::new(env!("CARGO_MANIFEST_DIR")).join("doc/ca65-to-k816.md"))
        .expect("read doc/ca65-to-k816.md")
}

/// Every fenced block, with the first word of its info string as the
/// language and the second, when present, as the pair name.
fn blocks(text: &str) -> Vec<Block> {
    let mut found = Vec::new();
    let mut lines = text.lines().enumerate();
    while let Some((index, line)) = lines.next() {
        let Some(info) = line.strip_prefix("```") else {
            continue;
        };
        let mut words = info.split_whitespace();
        let Some(language) = words.next() else {
            continue;
        };
        let name = words.next().map(str::to_owned);
        let mut body = String::new();
        for (_, line) in lines.by_ref() {
            if line.trim_end() == "```" {
                break;
            }
            body.push_str(line);
            body.push('\n');
        }
        found.push(Block {
            language: language.to_owned(),
            name,
            body,
            line: index + 1,
        });
    }
    found
}

fn config() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden/ca65-guide.cfg")
}

/// Assembles `source` and links it alone; the image's bytes.
fn link(source: &Path, what: &str) -> Vec<u8> {
    let object = source.with_extension("o");
    run_ok(
        Command::new(tool("CA65", "ca65"))
            .args(["--cpu", "65816"])
            .arg(source)
            .arg("-o")
            .arg(&object),
        &format!("ca65 over {what}"),
    );
    let image = source.with_extension("bin");
    run_ok(
        Command::new(tool("LD65", "ld65"))
            .arg("-C")
            .arg(config())
            .arg("-o")
            .arg(&image)
            .arg(&object),
        &format!("ld65 over {what}"),
    );
    std::fs::read(&image).expect("read the image")
}

/// Compiles a K816 listing to assembly beside it, panicking with the
/// compiler's own words when it is refused.
fn compile(source: &Path, what: &str) -> PathBuf {
    let assembly = source.with_extension("s");
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .arg(source)
        .arg("-o")
        .arg(&assembly)
        .output()
        .expect("run k816");
    assert!(
        output.status.success(),
        "{what} does not compile:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assembly
}

fn hex(bytes: &[u8]) -> String {
    bytes
        .chunks(16)
        .map(|row| {
            row.iter()
                .map(|byte| format!("{byte:02X}"))
                .collect::<Vec<_>>()
                .join(" ")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Every named pair links to the same bytes, and every name has both
/// halves.
#[test]
fn every_named_pair_assembles_to_the_same_image() {
    let text = guide();
    let mut pairs: BTreeMap<String, (Option<Block>, Option<Block>)> = BTreeMap::new();
    for block in blocks(&text) {
        let Some(name) = block.name.clone() else {
            continue;
        };
        let entry = pairs.entry(name.clone()).or_insert((None, None));
        match block.language.as_str() {
            "ca65" => {
                assert!(entry.0.is_none(), "two `ca65 {name}` blocks");
                entry.0 = Some(block);
            }
            "k65" => {
                assert!(entry.1.is_none(), "two `k65 {name}` blocks");
                entry.1 = Some(block);
            }
            other => panic!(
                "line {}: a named `{other}` block; names pair `ca65` with `k65`",
                block.line
            ),
        }
    }
    assert!(
        pairs.len() >= 12,
        "the guide should hold its paired listings; found {}",
        pairs.len()
    );

    let root = work_dir("ca65-guide");
    let mut failures: Vec<String> = Vec::new();
    for (name, (ca65, k65)) in &pairs {
        let ca65 = ca65
            .as_ref()
            .unwrap_or_else(|| panic!("`k65 {name}` has no `ca65 {name}` partner"));
        let k65 = k65
            .as_ref()
            .unwrap_or_else(|| panic!("`ca65 {name}` has no `k65 {name}` partner"));
        let dir = root.join(name);
        std::fs::create_dir_all(&dir).expect("create the pair's directory");

        let asm = dir.join("ca65.asm");
        std::fs::write(&asm, &ca65.body).expect("write the ca65 listing");
        let expected = link(&asm, &format!("`ca65 {name}` (line {})", ca65.line));

        let k816 = dir.join("k816.k816");
        std::fs::write(&k816, &k65.body).expect("write the K816 listing");
        let assembly = compile(&k816, &format!("`k65 {name}` (line {})", k65.line));
        let actual = link(&assembly, &format!("`k65 {name}` (line {})", k65.line));

        if expected != actual {
            // The images are padded to the configuration's layout; the
            // bytes that matter are the ones before the trailing zeros.
            let trim =
                |bytes: &[u8]| bytes.len() - bytes.iter().rev().take_while(|&&b| b == 0).count();
            let shown = trim(&expected).max(trim(&actual));
            failures.push(format!(
                "pair `{name}` (lines {} and {}) links to different bytes\nca65:\n{}\nK816:\n{}\nK816 assembly:\n{}",
                ca65.line,
                k65.line,
                hex(&expected[..shown.min(expected.len())]),
                hex(&actual[..shown.min(actual.len())]),
                std::fs::read_to_string(&assembly).unwrap_or_default()
            ));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

/// Every named ca65 listing, converted by `k816 convert`, links to the same
/// bytes as the listing itself.
///
/// The guide's pairs say what K816 writes where ca65 writes each thing; the
/// converter is the same table run by a program, so each pair's ca65 half is
/// also the converter's input and the ca65 build its oracle. A pair the
/// converter writes differently from the guide's own listing is fine — the
/// bytes are the claim, not the spelling.
#[test]
fn every_named_ca65_listing_converts_to_the_same_image() {
    let text = guide();
    let root = work_dir("ca65-guide-converted");
    let mut failures: Vec<String> = Vec::new();
    let mut count = 0;
    for block in blocks(&text) {
        let (Some(name), "ca65") = (block.name.clone(), block.language.as_str()) else {
            continue;
        };
        count += 1;
        let dir = root.join(&name);
        std::fs::create_dir_all(&dir).expect("create the listing's directory");
        let asm = dir.join(format!("{name}.asm"));
        std::fs::write(&asm, &block.body).expect("write the ca65 listing");
        let expected = link(&asm, &format!("`ca65 {name}` (line {})", block.line));

        let request = k816::convert::Request {
            inputs: vec![asm.clone()],
            include_dirs: Vec::new(),
        };
        let conversion =
            k816::convert::convert(&request, &|path| std::fs::read_to_string(path).ok())
                .unwrap_or_else(|error| panic!("`ca65 {name}` does not convert: {error}"));
        assert_eq!(conversion.units.len(), 1, "`ca65 {name}` is one unit");
        let unit = &conversion.units[0];
        std::fs::write(&unit.path, &unit.text).expect("write the converted unit");
        let assembly = compile(
            &unit.path,
            &format!("`ca65 {name}` converted (line {})", block.line),
        );
        let actual = link(&assembly, &format!("`ca65 {name}` converted"));
        if expected != actual {
            let trim =
                |bytes: &[u8]| bytes.len() - bytes.iter().rev().take_while(|&&b| b == 0).count();
            let shown = trim(&expected).max(trim(&actual));
            failures.push(format!(
                "`ca65 {name}` (line {}) converts to different bytes\nca65:\n{}\nconverted:\n{}\nas converted:\n{}",
                block.line,
                hex(&expected[..shown.min(expected.len())]),
                hex(&actual[..shown.min(actual.len())]),
                unit.text
            ));
        }
    }
    assert!(count >= 12, "the guide's ca65 listings were not found");
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

/// A K816 listing without a partner still compiles: it is a claim too.
#[test]
fn every_unpaired_k816_listing_compiles() {
    let text = guide();
    let root = work_dir("ca65-guide-unpaired");
    let mut count = 0;
    for block in blocks(&text) {
        if block.language != "k65" || block.name.is_some() {
            continue;
        }
        count += 1;
        let source = root.join(format!("line-{}.k816", block.line));
        std::fs::write(&source, &block.body).expect("write the listing");
        compile(
            &source,
            &format!("the `k65` listing at line {}", block.line),
        );
    }
    assert!(count >= 1, "the guide's unpaired listings were not found");
}

/// The diagnostics that point at this document point at a file that
/// exists, by the path they print.
#[test]
fn the_ca65_diagnostics_point_at_this_document() {
    let source =
        std::fs::read_to_string(Path::new(env!("CARGO_MANIFEST_DIR")).join("src/parser/ca65.rs"))
            .expect("read src/parser/ca65.rs");
    let path = source
        .lines()
        .find_map(|line| line.trim().strip_prefix("const GUIDE: &str = \""))
        .and_then(|rest| rest.split('"').next())
        .expect("`const GUIDE` in src/parser/ca65.rs");
    assert_eq!(path, "doc/ca65-to-k816.md");
    assert!(Path::new(env!("CARGO_MANIFEST_DIR")).join(path).exists());
}
