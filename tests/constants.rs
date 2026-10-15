/*****************************************************************************/
/*                                                                           */
/*                                constants.rs                               */
/*                                                                           */
/*             A constant's size: taken at each use, or declared             */
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

//! Sub-plan P: a constant's size (`plans/15-constant-sizes.md`).
//!
//! A `const` written without a size has none. Each use gives it the size
//! that use needs, the way a widthless `inline` takes the width of the place
//! it is expanded: a value fits a use *n* bits wide when it lies in
//! −2^(n−1) … 2^n−1, and the use emits its low *n* bits. So `const EUNKNOWN
//! = -1` is `$FF` in an 8-bit immediate, `$FFFF` in `pea` and `$FFFFFF` in a
//! `far` entry, and 256 is refused in a byte the way 300 always was.
//!
//! The matrix below is every kind of use against the values on both sides of
//! every boundary. A value that fits is compiled, assembled and linked, and
//! checked at the bytes ld65 wrote — with ca65 and ld65 required to say
//! nothing, because the export of every one of those constants goes through
//! ca65 too. A value that does not fit is checked at the code its use
//! reports.

mod common;

use std::process::Command;

use common::{tool, work_dir};
use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

/// The values on both sides of every boundary a use has: a byte's, a word's
/// and a `far` value's, at both ends.
const VALUES: &[i64] = &[
    -8_388_609, -8_388_608, -32_769, -32_768, -129, -128, -1, 0, 1, 127, 128, 255, 256, 32_767,
    32_768, 65_535, 65_536, 16_777_215, 16_777_216,
];

/// A place a constant is used, and what that place makes of it.
struct Use {
    /// How a failure names it.
    what: &'static str,
    /// The program around the uses, with `{uses}` where they go.
    program: &'static str,
    /// One use of the constant `{c}`.
    line: &'static str,
    /// The bytes one use emits before the value.
    opcode: &'static [u8],
    /// The ranges the value must fit, in the order they are checked, each
    /// with the code a value outside it is reported as.
    checks: &'static [(u32, &'static str)],
    /// How many bytes of the value one use emits.
    emits: u32,
}

const A8: &str = "naked uses @a8 @i8 {\n{uses}}\n";
const A16: &str = "naked uses @a16 @i16 {\n{uses}}\n";
const DATA: &str = "data uses {\n{uses}}\n";
/// A narrowed parameter, expanded where the accumulator is wider than the
/// narrowing: the narrowing is a range the argument must lie in, and the
/// operand in the body is what gives it a width.
const BYTE_PARAMETER: &str =
    "inline put (#v:byte) {\n    lda #v\n}\n\nnaked uses @a16 @i8 {\n{uses}}\n";
/// And the other way round: a word's range, used in a byte.
const WORD_PARAMETER: &str =
    "inline put (#v:word) {\n    lda #v\n}\n\nnaked uses @a8 @i8 {\n{uses}}\n";

const IMMEDIATE: &str = "K816-E0103";
const DATUM: &str = "K816-E0109";
const ARGUMENT: &str = "K816-E0170";

const USES: &[Use] = &[
    Use {
        what: "`lda #` at @a8",
        program: A8,
        line: "lda #{c}",
        opcode: &[0xA9],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
    },
    Use {
        what: "`lda #` at @a16",
        program: A16,
        line: "lda #{c}",
        opcode: &[0xA9],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
    },
    Use {
        what: "`ldx #` at @i8",
        program: A8,
        line: "ldx #{c}",
        opcode: &[0xA2],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
    },
    Use {
        what: "`ldy #` at @i16",
        program: A16,
        line: "ldy #{c}",
        opcode: &[0xA0],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
    },
    Use {
        what: "`a = ` at @a8",
        program: A8,
        line: "a = {c}",
        opcode: &[0xA9],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
    },
    Use {
        what: "`a = ` at @a16",
        program: A16,
        line: "a = {c}",
        opcode: &[0xA9],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
    },
    Use {
        what: "`a+` at @a8",
        program: A8,
        line: "a+{c}",
        opcode: &[0x69],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
    },
    Use {
        what: "`x?` at @i16",
        program: A16,
        line: "x?{c}",
        opcode: &[0xE0],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
    },
    Use {
        what: "`y?` at @i8",
        program: A8,
        line: "y?{c}",
        opcode: &[0xC0],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
    },
    Use {
        what: "`pea #`, always a word",
        program: A8,
        line: "pea #{c}",
        opcode: &[0xF4],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
    },
    Use {
        what: "`push`, which is `pea`",
        program: A8,
        line: "push {c}",
        opcode: &[0xF4],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
    },
    Use {
        what: "`cop #`, always a byte",
        program: A8,
        line: "cop #{c}",
        opcode: &[0x02],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
    },
    Use {
        what: "an untyped data entry",
        program: DATA,
        line: "{c}",
        opcode: &[],
        checks: &[(1, DATUM)],
        emits: 1,
    },
    Use {
        what: "a `byte` entry",
        program: DATA,
        line: "byte {c}",
        opcode: &[],
        checks: &[(1, DATUM)],
        emits: 1,
    },
    Use {
        what: "a `word` entry",
        program: DATA,
        line: "word {c}",
        opcode: &[],
        checks: &[(2, DATUM)],
        emits: 2,
    },
    Use {
        what: "a `far` entry",
        program: DATA,
        line: "far {c}",
        opcode: &[],
        checks: &[(3, DATUM)],
        emits: 3,
    },
    Use {
        what: "a `#v:byte` argument used at @a16",
        program: BYTE_PARAMETER,
        line: "put #{c}",
        opcode: &[0xA9],
        checks: &[(1, ARGUMENT)],
        emits: 2,
    },
    Use {
        what: "a `#v:word` argument used at @a8",
        program: WORD_PARAMETER,
        line: "put #{c}",
        opcode: &[0xA9],
        checks: &[(2, ARGUMENT), (1, IMMEDIATE)],
        emits: 1,
    },
];

/// Whether `value` fits a use `bytes` wide.
fn fits(value: i64, bytes: u32) -> bool {
    let bits = bytes * 8;
    (-(1i64 << (bits - 1))..(1i64 << bits)).contains(&value)
}

/// The low `bytes` bytes of `value`, little-endian: what a use emits.
fn encoded(value: i64, bytes: u32) -> Vec<u8> {
    (0..bytes).map(|byte| (value >> (8 * byte)) as u8).collect()
}

/// `value` as K816 source spells it.
fn spelled(value: i64) -> String {
    if value < 0 {
        format!("-{}", -value)
    } else {
        format!("${value:X}")
    }
}

/// One `const` per value, `C0` … in order, and `the` use of each.
fn program(the: &Use, consts: &[(String, String)]) -> String {
    let mut out = String::new();
    for (name, value) in consts {
        out.push_str(&format!("const {name} = {value}\n"));
    }
    out.push('\n');
    let uses: String = consts
        .iter()
        .map(|(name, _)| format!("    {}\n", the.line.replace("{c}", name)))
        .collect();
    out.push_str(&the.program.replace("{uses}", &uses));
    out
}

/// The assembly `source` compiles to, unoptimized, and every code it reports.
fn compile(name: &str, source: &str) -> (Option<String>, Vec<String>) {
    let mut map = SourceMap::new();
    let file = map.add_file(name, source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.0.to_owned())
        .collect();
    (result.assembly, codes)
}

/// Assembles and links `assembly` at `$8000`, requiring ca65 and ld65 to say
/// nothing at all — a warning is a finding here, not noise.
fn link_quietly(test: &str, assembly: &str) -> Vec<u8> {
    let dir = work_dir(test);
    let source = dir.join("out.s");
    let object = dir.join("out.o");
    let binary = dir.join("out.bin");
    std::fs::write(&source, assembly).expect("write assembly");
    let config =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden/slice.cfg");
    for (what, command) in [
        (
            "ca65",
            Command::new(tool("CA65", "ca65"))
                .arg(&source)
                .arg("-o")
                .arg(&object)
                .output(),
        ),
        (
            "ld65",
            Command::new(tool("LD65", "ld65"))
                .arg("-C")
                .arg(&config)
                .arg("-o")
                .arg(&binary)
                .arg(&object)
                .output(),
        ),
    ] {
        let output = command.unwrap_or_else(|error| panic!("cannot run {what}: {error}"));
        let said = String::from_utf8_lossy(&output.stderr);
        assert!(
            output.status.success() && said.is_empty(),
            "{test}: {what} said:\n{said}\n{assembly}"
        );
    }
    std::fs::read(&binary).expect("read linked binary")
}

/// The matrix: every use, every boundary value.
#[test]
fn every_use_gives_a_size_less_constant_the_width_it_needs() {
    for (index, the) in USES.iter().enumerate() {
        let mut fitting = Vec::new();
        let mut expected = Vec::new();
        for &value in VALUES {
            let refused = the
                .checks
                .iter()
                .find(|(bytes, _)| !fits(value, *bytes))
                .map(|(_, code)| *code);
            let Some(code) = refused else {
                fitting.push((format!("C{}", fitting.len()), spelled(value)));
                expected.extend_from_slice(the.opcode);
                expected.extend(encoded(value, the.emits));
                continue;
            };
            let source = program(the, &[("VALUE".to_owned(), spelled(value))]);
            let (_, codes) = compile("refused.kk", &source);
            assert_eq!(
                codes,
                [code],
                "{}: {value} is outside the use's range\n{source}",
                the.what
            );
        }
        let source = program(the, &fitting);
        let (assembly, codes) = compile("fitting.kk", &source);
        assert_eq!(codes, Vec::<String>::new(), "{}:\n{source}", the.what);
        let bytes = link_quietly(
            &format!("constants-use-{index}"),
            &assembly.expect("compiles"),
        );
        assert_eq!(bytes, expected, "{}: the linked bytes\n{source}", the.what);
    }
}

/// A use's own value need not be a constant's: a literal is the same number
/// and takes the same width. A data entry's values are atoms, so a negative
/// one there is a bracket.
#[test]
fn a_negative_literal_takes_its_width_where_it_is_written() {
    let source = "naked uses @a8 @i16 {\n    lda #-1\n    ldx #-1\n    pea #-2\n}\n\n\
                  data table {\n    [0 - 128]\n    word [0 - 1]\n    far [0 - 2]\n}\n";
    let (assembly, codes) = compile("literal.kk", source);
    assert_eq!(codes, Vec::<String>::new());
    let bytes = link_quietly("constants-literal", &assembly.expect("compiles"));
    assert_eq!(
        bytes,
        [
            0xA9, 0xFF, 0xA2, 0xFF, 0xFF, 0xF4, 0xFE, 0xFF, 0x80, 0xFF, 0xFF, 0xFE, 0xFF, 0xFF
        ]
    );
}

/// `$FFFFFFFF` and −1 are one value: a number is 32 bits of two's
/// complement, and a pattern with its top bit set is the negative it spells
/// (`plans/15-constant-sizes.md` decision 3).
#[test]
fn a_pattern_with_its_top_bit_set_is_the_negative_it_spells() {
    let source = "const ALL = $FFFFFFFF\nconst LOW = $FFFFFF80\n\n\
                  naked uses @a8 @i8 {\n    lda #ALL\n    lda #LOW\n}\n";
    let (assembly, codes) = compile("pattern.kk", source);
    assert_eq!(codes, Vec::<String>::new());
    let bytes = link_quietly("constants-pattern", &assembly.expect("compiles"));
    assert_eq!(bytes, [0xA9, 0xFF, 0xA9, 0x80]);

    let (_, codes) = compile(
        "pattern.kk",
        "const BELOW = $FFFFFF7F\n\nnaked uses @a8 @i8 {\n    lda #BELOW\n}\n",
    );
    assert_eq!(codes, [IMMEDIATE], "-129 does not fit a byte");
}

/// The message names the value as it was written, and the range the use has.
#[test]
fn a_value_that_does_not_fit_is_named_as_written() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "message.kk",
        "const LOW = -129\n\nnaked uses @a8 @i8 {\n    lda #LOW\n}\n\ndata t {\n    word [0 - 32769]\n}\n",
    );
    let result = compile_to_ca65(&map, file, &Options::default());
    let said: Vec<(String, String)> = result
        .diagnostics
        .iter()
        .map(|diagnostic| (diagnostic.title.clone(), diagnostic.primary.message.clone()))
        .collect();
    assert_eq!(
        said,
        [
            (
                "-32769 does not fit a 2-byte value".to_owned(),
                "this entry emits 2 bytes per value, -32768 … $FFFF".to_owned()
            ),
            (
                "immediate -129 does not fit in the selected width".to_owned(),
                "here it must lie in -128 … $FF".to_owned()
            ),
        ]
    );
}

/// Inside the evaluator a negative constant is negative: `[E + 1]` is zero,
/// not one past `$FFFFFFFF`, which was "not a K65 number" before.
#[test]
fn a_negative_constant_is_negative_in_the_evaluator() {
    let source = "const E = -1\nconst NEXT = [E + 1]\n\n\
                  data table {\n    [E < 0]\n    NEXT\n    [E * 2]\n    word [E]\n}\n";
    let (assembly, codes) = compile("eval.kk", source);
    assert_eq!(codes, Vec::<String>::new());
    let bytes = link_quietly("constants-eval", &assembly.expect("compiles"));
    assert_eq!(bytes, [0x01, 0x00, 0xFE, 0xFF, 0xFF]);
}

/// A negative constant another unit declares folds here as the value it is:
/// in an operand, in a data entry, in another `const`, and in a bracket —
/// the last through the values the evaluator is seeded with, which is a
/// second route to the same number.
#[test]
fn a_negative_constant_crosses_an_import_as_its_value() {
    let mut map = SourceMap::new();
    map.add_file("defs.kk", "const EUNKNOWN = -1\n");
    let entry = map.add_file(
        "main.kk",
        "import \"defs.kk\"\n\nconst AFTER = EUNKNOWN + 1\nconst BRACKET = [EUNKNOWN + 2]\n\n\
         naked uses @a16 @i8 {\n    lda #EUNKNOWN\n}\n\n\
         data table {\n    EUNKNOWN\n    AFTER\n    BRACKET\n    far EUNKNOWN\n}\n",
    );
    let result = compile_to_ca65(
        &map,
        entry,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    assert_eq!(result.diagnostics, vec![]);
    let assembly = result.assembly.expect("compiles");
    assert!(
        !assembly.contains(".import EUNKNOWN"),
        "a K816 constant folds; nothing imports it:\n{assembly}"
    );
    let bytes = link_quietly("constants-import", &assembly);
    assert_eq!(
        bytes,
        [0xA9, 0xFF, 0xFF, 0xFF, 0x00, 0x01, 0xFF, 0xFF, 0xFF]
    );
}

/// A size-less constant is exported `: abs` when it fits `$0000` … `$FFFF`,
/// which is what a ca65 `.import` expects, `: far` when it fits 24 bits, and
/// with no size otherwise — written as the number it is. ca65 assembles
/// every line quietly, which the matrix above checks for every one of these
/// values too.
#[test]
fn a_size_less_constant_is_exported_at_abs_or_with_no_size() {
    for &value in VALUES {
        let source = format!("const VALUE = {}\n", spelled(value));
        let (assembly, codes) = compile("export.kk", &source);
        assert_eq!(codes, Vec::<String>::new());
        let assembly = assembly.expect("compiles");
        let expected = match value {
            0..=0xFFFF => format!(".export VALUE : abs = ${value:04X}"),
            0x1_0000..=0xFF_FFFF => format!(".export VALUE : far = ${value:04X}"),
            ..0 => format!(".export VALUE = {value}"),
            _ => format!(".export VALUE = ${value:06X}"),
        };
        assert!(
            assembly.contains(&expected),
            "{value}: `{expected}` in\n{assembly}"
        );
        let _ = link_quietly("constants-export", &assembly);
    }
}
