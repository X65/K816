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
    /// Whether a constant declared `:byte` is refused here whatever its
    /// value: `push` writes `PEA`, which pushes two bytes
    /// (`plans/15-constant-sizes.md` decision 7).
    refuses_a_byte: bool,
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
const PUSHED_BYTE: &str = "K816-E0175";
const OUTSIDE_ITS_SIZE: &str = "K816-E0178";
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
        refuses_a_byte: false,
    },
    Use {
        what: "`lda #` at @a16",
        program: A16,
        line: "lda #{c}",
        opcode: &[0xA9],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
        refuses_a_byte: false,
    },
    Use {
        what: "`ldx #` at @i8",
        program: A8,
        line: "ldx #{c}",
        opcode: &[0xA2],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
        refuses_a_byte: false,
    },
    Use {
        what: "`ldy #` at @i16",
        program: A16,
        line: "ldy #{c}",
        opcode: &[0xA0],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
        refuses_a_byte: false,
    },
    Use {
        what: "`a = ` at @a8",
        program: A8,
        line: "a = {c}",
        opcode: &[0xA9],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
        refuses_a_byte: false,
    },
    Use {
        what: "`a = ` at @a16",
        program: A16,
        line: "a = {c}",
        opcode: &[0xA9],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
        refuses_a_byte: false,
    },
    Use {
        what: "`a+` at @a8",
        program: A8,
        line: "a+{c}",
        opcode: &[0x69],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
        refuses_a_byte: false,
    },
    Use {
        what: "`x?` at @i16",
        program: A16,
        line: "x?{c}",
        opcode: &[0xE0],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
        refuses_a_byte: false,
    },
    Use {
        what: "`y?` at @i8",
        program: A8,
        line: "y?{c}",
        opcode: &[0xC0],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
        refuses_a_byte: false,
    },
    Use {
        what: "`pea #`, always a word",
        program: A8,
        line: "pea #{c}",
        opcode: &[0xF4],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
        refuses_a_byte: false,
    },
    Use {
        what: "`push`, which is `pea`",
        program: A8,
        line: "push {c}",
        opcode: &[0xF4],
        checks: &[(2, IMMEDIATE)],
        emits: 2,
        refuses_a_byte: true,
    },
    Use {
        what: "`cop #`, always a byte",
        program: A8,
        line: "cop #{c}",
        opcode: &[0x02],
        checks: &[(1, IMMEDIATE)],
        emits: 1,
        refuses_a_byte: false,
    },
    Use {
        what: "an untyped data entry",
        program: DATA,
        line: "{c}",
        opcode: &[],
        checks: &[(1, DATUM)],
        emits: 1,
        refuses_a_byte: false,
    },
    Use {
        what: "a `byte` entry",
        program: DATA,
        line: "byte {c}",
        opcode: &[],
        checks: &[(1, DATUM)],
        emits: 1,
        refuses_a_byte: false,
    },
    Use {
        what: "a `word` entry",
        program: DATA,
        line: "word {c}",
        opcode: &[],
        checks: &[(2, DATUM)],
        emits: 2,
        refuses_a_byte: false,
    },
    Use {
        what: "a `far` entry",
        program: DATA,
        line: "far {c}",
        opcode: &[],
        checks: &[(3, DATUM)],
        emits: 3,
        refuses_a_byte: false,
    },
    Use {
        what: "a `#v:byte` argument used at @a16",
        program: BYTE_PARAMETER,
        line: "put #{c}",
        opcode: &[0xA9],
        checks: &[(1, ARGUMENT)],
        emits: 2,
        refuses_a_byte: false,
    },
    Use {
        what: "a `#v:word` argument used at @a8",
        program: WORD_PARAMETER,
        line: "put #{c}",
        opcode: &[0xA9],
        checks: &[(2, ARGUMENT), (1, IMMEDIATE)],
        emits: 1,
        refuses_a_byte: false,
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

/// The sizes a declaration may state, and how many bytes each is.
const SIZES: &[(&str, u32)] = &[("byte", 1), ("word", 2), ("far", 3)];

/// One `const` per value, `C0` … in order, each declared at `size` if one is
/// given, and `the` use of each.
fn program(the: &Use, size: Option<&str>, consts: &[(String, String)]) -> String {
    let suffix = size.map_or(String::new(), |size| format!(":{size}"));
    let mut out = String::new();
    for (name, value) in consts {
        out.push_str(&format!("const {name}{suffix} = {value}\n"));
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

/// Links `assembly` with a ca65 module assembled from `other`, `assembly`
/// first, requiring both tools to say nothing.
fn link_with_ca65(test: &str, assembly: &str, other: &str) -> Vec<u8> {
    let dir = work_dir(test);
    let config =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden/slice.cfg");
    let mut objects = Vec::new();
    for (name, text) in [("k816", assembly), ("ca65", other)] {
        let source = dir.join(format!("{name}.s"));
        let object = dir.join(format!("{name}.o"));
        std::fs::write(&source, text).expect("write assembly");
        quiet(
            test,
            "ca65",
            Command::new(tool("CA65", "ca65"))
                .arg(&source)
                .arg("-o")
                .arg(&object),
        );
        objects.push(object);
    }
    let binary = dir.join("out.bin");
    quiet(
        test,
        "ld65",
        Command::new(tool("LD65", "ld65"))
            .arg("-C")
            .arg(&config)
            .arg("-o")
            .arg(&binary)
            .args(&objects),
    );
    std::fs::read(&binary).expect("read linked binary")
}

/// Runs `command` and requires it to succeed and say nothing.
fn quiet(test: &str, what: &str, command: &mut Command) {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("cannot run {what}: {error}"));
    let said = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success() && said.is_empty(),
        "{test}: {what} said:\n{said}"
    );
}

/// The matrix: every use, every boundary value — of constants declared at
/// `size`, or with no size, and only the values a declaration of that size
/// admits. A size is a range and not a width, so each use gives a sized
/// constant its own width exactly as it gives one with no size: the value
/// stays the value.
fn matrix(size: Option<(&str, u32)>) {
    let values: Vec<i64> = VALUES
        .iter()
        .copied()
        .filter(|&value| size.is_none_or(|(_, bytes)| fits(value, bytes)))
        .collect();
    for (index, the) in USES.iter().enumerate() {
        let mut fitting = Vec::new();
        let mut expected = Vec::new();
        for &value in &values {
            let refused = if the.refuses_a_byte && size.is_some_and(|(name, _)| name == "byte") {
                Some(PUSHED_BYTE)
            } else {
                the.checks
                    .iter()
                    .find(|(bytes, _)| !fits(value, *bytes))
                    .map(|(_, code)| *code)
            };
            let Some(code) = refused else {
                fitting.push((format!("C{}", fitting.len()), spelled(value)));
                expected.extend_from_slice(the.opcode);
                expected.extend(encoded(value, the.emits));
                continue;
            };
            let source = program(
                the,
                size.map(|(name, _)| name),
                &[("VALUE".to_owned(), spelled(value))],
            );
            let (_, codes) = compile("refused.kk", &source);
            assert_eq!(
                codes,
                [code],
                "{}: {value} is outside the use's range\n{source}",
                the.what
            );
        }
        if fitting.is_empty() {
            continue;
        }
        let source = program(the, size.map(|(name, _)| name), &fitting);
        let (assembly, codes) = compile("fitting.kk", &source);
        assert_eq!(codes, Vec::<String>::new(), "{}:\n{source}", the.what);
        let bytes = link_quietly(
            &format!(
                "constants-{}-use-{index}",
                size.map_or("none", |(name, _)| name)
            ),
            &assembly.expect("compiles"),
        );
        assert_eq!(bytes, expected, "{}: the linked bytes\n{source}", the.what);
    }
}

#[test]
fn every_use_gives_a_size_less_constant_the_width_it_needs() {
    matrix(None);
}

#[test]
fn every_use_gives_a_byte_constant_its_own_width() {
    matrix(Some(SIZES[0]));
}

#[test]
fn every_use_gives_a_word_constant_its_own_width() {
    matrix(Some(SIZES[1]));
}

#[test]
fn every_use_gives_a_far_constant_its_own_width() {
    matrix(Some(SIZES[2]));
}

/// A declared size is a range: the value must lie in −2^(n−1) … 2^n−1 for
/// its *n* bits, which is the fit rule a use applies, applied once where the
/// constant is declared.
#[test]
fn a_declared_size_is_a_range_the_value_must_lie_in() {
    for &(size, bytes) in SIZES {
        for &value in VALUES {
            let source = format!("const VALUE:{size} = {}\n", spelled(value));
            let (_, codes) = compile("declared.kk", &source);
            let expected: &[&str] = if fits(value, bytes) {
                &[]
            } else {
                &[OUTSIDE_ITS_SIZE]
            };
            assert_eq!(codes, expected, "{source}");
        }
    }
}

/// The group's auto-increment can carry a sized entry out of its range, and
/// the error is on the entry that has to change.
#[test]
fn a_group_entry_its_increment_carries_out_of_range_is_the_one_reported() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "group.kk",
        "const LAST:byte = 255, NEXT:byte, WIDER:word\nconst BIG:byte = 300\n",
    );
    let result = compile_to_ca65(&map, file, &Options::default());
    let reported: Vec<(&str, String, String, String)> = result
        .diagnostics
        .iter()
        .map(|diagnostic| {
            (
                diagnostic.code.0,
                diagnostic.title.clone(),
                map.file(file).span_text(diagnostic.primary.span).to_owned(),
                map.file(file)
                    .span_text(diagnostic.secondary[0].span)
                    .to_owned(),
            )
        })
        .collect();
    assert_eq!(
        reported,
        [
            (
                OUTSIDE_ITS_SIZE,
                "$0100 does not fit `NEXT:byte`".to_owned(),
                "NEXT".to_owned(),
                ":byte".to_owned()
            ),
            (
                OUTSIDE_ITS_SIZE,
                "$012C does not fit `BIG:byte`".to_owned(),
                "300".to_owned(),
                ":byte".to_owned()
            ),
        ]
    );
}

/// A sized constant is exported at its size, as its low bits at that size,
/// and ca65 assembles each line quietly: `.export E : zp = -1` would be
/// "long but exported zeropage" (`plans/15-constant-sizes.md` decision 8).
#[test]
fn a_sized_constant_is_exported_at_its_size() {
    for &(size, bytes) in SIZES {
        for &value in VALUES.iter().filter(|&&value| fits(value, bytes)) {
            let source = format!("const VALUE:{size} = {}\n", spelled(value));
            let (assembly, codes) = compile("export.kk", &source);
            assert_eq!(codes, Vec::<String>::new());
            let assembly = assembly.expect("compiles");
            let bits = value & ((1 << (8 * bytes)) - 1);
            let expected = match size {
                "byte" => format!(".export VALUE : zp = ${bits:02X}"),
                "word" => format!(".export VALUE : abs = ${bits:04X}"),
                _ => format!(".export VALUE : far = ${bits:06X}"),
            };
            assert!(
                assembly.contains(&expected),
                "{source}: `{expected}` in\n{assembly}"
            );
            let _ = link_quietly("constants-sized-export", &assembly);
        }
    }
}

/// ca65 code imports a sized constant at its size and uses it the way that
/// size allows — a `:byte` one in an 8-bit immediate — and ld65 links the
/// two quietly, which an absolute export of a byte would not.
#[test]
fn ca65_imports_a_sized_constant_at_its_size() {
    let k816 = "const ERROR:byte = -1\nconst PAGE:word = $1234\nconst BANK:far = $123456\n";
    let ca65 = ".setcpu \"65816\"\n.importzp ERROR\n.import PAGE\n.import BANK : far\n\
                .segment \"CODE\"\n.a8\nlda #ERROR\n.a16\nlda #PAGE\n.faraddr BANK\n";
    let (assembly, codes) = compile("sizes.kk", k816);
    assert_eq!(codes, Vec::<String>::new());
    let bytes = link_with_ca65("constants-ca65-import", &assembly.expect("compiles"), ca65);
    assert_eq!(bytes, [0xA9, 0xFF, 0xA9, 0x34, 0x12, 0x56, 0x34, 0x12]);
}

/// `extern const NAME:byte` is an `.importzp`, `:word` a plain `.import` and
/// `:far` one at `: far`; with no size it stays the plain `.import` it was.
#[test]
fn an_extern_const_is_imported_at_the_size_it_declares() {
    let source = "extern const ROWS:byte\nextern const PAGE:word\nextern const BANK:far\n\
                  extern const PLAIN\n\n\
                  data table {\n    far ROWS PAGE BANK PLAIN\n}\n";
    let (assembly, codes) = compile("externs.kk", source);
    assert_eq!(codes, Vec::<String>::new());
    let assembly = assembly.expect("compiles");
    for line in [
        ".importzp ROWS",
        ".import PAGE ",
        ".import BANK : far",
        ".import PLAIN ",
    ] {
        assert!(assembly.contains(line), "`{line}` in\n{assembly}");
    }
}

/// Two `extern const` declarations of one name that disagree about its size
/// disagree about the import, which is `K816-E0165` like any other.
#[test]
fn two_extern_consts_of_one_name_must_agree_about_its_size() {
    let mut map = SourceMap::new();
    map.add_file("a.kk", "extern const ROWS:byte\n");
    let entry = map.add_file("main.kk", "import \"a.kk\"\nextern const ROWS\n");
    let codes: Vec<&str> = compile_to_ca65(&map, entry, &Options::default())
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.0)
        .collect();
    assert_eq!(codes, ["K816-E0165"]);
}

/// A `:byte` constant is a one-byte constant to `push`, which pushes two, and
/// the fix is the word view — which pushes the value at sixteen bits, so a
/// `:byte` -1 is `$FFFF` on the stack.
#[test]
fn a_byte_constant_is_pushed_through_its_word_view() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "push.kk",
        "const ERROR:byte = -1\n\nnaked uses @a8 @i8 {\n    push ERROR\n}\n",
    );
    let result = compile_to_ca65(&map, file, &Options::default());
    let [diagnostic] = result.diagnostics.as_slice() else {
        panic!("one diagnostic: {:?}", result.diagnostics);
    };
    assert_eq!(diagnostic.code.0, PUSHED_BYTE);
    assert_eq!(map.file(file).span_text(diagnostic.primary.span), "ERROR");
    assert_eq!(diagnostic.fixes[0].edits[0].replacement, "ERROR:word");

    let (assembly, codes) = compile(
        "push.kk",
        "const ERROR:byte = -1\n\nnaked uses @a8 @i8 {\n    push ERROR:word\n}\n",
    );
    assert_eq!(codes, Vec::<String>::new());
    let bytes = link_quietly("constants-push-view", &assembly.expect("compiles"));
    assert_eq!(bytes, [0xF4, 0xFF, 0xFF]);
}

/// A size crosses `import` with the constant: the importer folds the value
/// and knows the size, so `push` refuses another unit's `:byte` constant
/// exactly as its own.
#[test]
fn a_declared_size_crosses_an_import() {
    let mut map = SourceMap::new();
    map.add_file("defs.kk", "const ERROR:byte = -1\n");
    let entry = map.add_file(
        "main.kk",
        "import \"defs.kk\"\n\nnaked uses @a16 @i8 {\n    lda #ERROR\n    push ERROR\n}\n",
    );
    let codes: Vec<&str> = compile_to_ca65(&map, entry, &Options::default())
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.0)
        .collect();
    assert_eq!(codes, [PUSHED_BYTE]);
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
