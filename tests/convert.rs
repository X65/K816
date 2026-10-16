/*****************************************************************************/
/*                                                                           */
/*                                convert.rs                                 */
/*                                                                           */
/*           k816 convert: what it writes, and what that builds to           */
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

//! `k816 convert` over a small ca65 program that uses what a real one does —
//! a `.struct` at an `.org` every unit reads, a `.tag` table, an `.enum`,
//! width macros, `.macpack generic`, unnamed and cheap labels, an indirect
//! dispatch and an interrupt handler — checked three ways: the units match
//! the ones in `tests/convert/expected/`, they build to the bytes the ca65
//! program builds to, and the command puts them where it says it does.
//!
//! `UPDATE_CONVERT_GOLDEN=1 cargo test --test convert` rewrites the expected
//! units; the byte comparison is what says the rewrite is still right.

mod common;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use common::{run_ok, toolchain, work_dir};
use k816::convert::{Conversion, Request, convert};

fn program_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/convert/program")
}

fn expected_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/convert/expected")
}

fn program_inputs() -> Vec<PathBuf> {
    let dir = program_dir();
    vec![dir.join("main.asm"), dir.join("queue.asm")]
}

fn converted(inputs: &[PathBuf], include_dirs: &[PathBuf]) -> Conversion {
    let request = Request {
        inputs: inputs.to_vec(),
        include_dirs: include_dirs.to_vec(),
    };
    convert(&request, &|path| std::fs::read_to_string(path).ok()).expect("the program converts")
}

/// One ca65 source converted on its own: the unit's text, and the notes.
fn one(test: &str, source: &str) -> (String, Vec<String>) {
    let dir = work_dir(test);
    let path = dir.join("unit.asm");
    std::fs::write(&path, source).expect("write the source");
    let conversion = converted(&[path], &[]);
    let unit = conversion
        .units
        .iter()
        .find(|unit| unit.path.file_name() == Some("unit.kk".as_ref()))
        .expect("the unit");
    let notes = conversion
        .notes
        .iter()
        .map(|note| note.message.clone())
        .collect();
    (unit.text.clone(), notes)
}

fn k816(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("convert")
        .args(args)
        .current_dir(dir)
        .output()
        .expect("run k816")
}

/// A scratch copy of the fixture, so that a conversion beside its sources
/// writes nowhere in the tree.
fn fixture_copy(test: &str) -> PathBuf {
    let dir = work_dir(test);
    for entry in std::fs::read_dir(&dir).expect("read the scratch directory") {
        let path = entry.expect("an entry").path();
        if path.is_file() {
            std::fs::remove_file(&path).expect("clear the scratch directory");
        }
    }
    for entry in std::fs::read_dir(program_dir()).expect("read the fixture") {
        let path = entry.expect("an entry").path();
        std::fs::copy(&path, dir.join(path.file_name().expect("a name"))).expect("copy");
    }
    dir
}

const UNITS: [&str; 5] = ["generic.kk", "hw.kk", "macros.kk", "main.kk", "queue.kk"];

// ---------------------------------------------------------------------------
// The program
// ---------------------------------------------------------------------------

/// The units the fixture converts to, and that it converts without a note:
/// nothing in it is a place the conversion could not be faithful.
#[test]
fn the_program_converts_to_the_expected_units() {
    let conversion = converted(&program_inputs(), &[]);
    assert_eq!(conversion.notes, vec![]);
    let update = std::env::var_os("UPDATE_CONVERT_GOLDEN").is_some();
    let mut names = Vec::new();
    for unit in &conversion.units {
        let name = unit.path.file_name().expect("a name");
        names.push(name.to_string_lossy().into_owned());
        let expected = expected_dir().join(name);
        if update {
            std::fs::create_dir_all(expected_dir()).expect("create the expected directory");
            std::fs::write(&expected, &unit.text).expect("write an expected unit");
            continue;
        }
        let want = std::fs::read_to_string(&expected).unwrap_or_default();
        assert!(
            want == unit.text,
            "{} differs from {}; UPDATE_CONVERT_GOLDEN=1 rewrites it\n{}",
            unit.path.display(),
            expected.display(),
            unit.text
        );
    }
    names.sort();
    assert_eq!(names, UNITS);
    // And nothing in the expected directory that is no unit any more.
    let mut kept: Vec<String> = std::fs::read_dir(expected_dir())
        .expect("read the expected units")
        .map(|entry| {
            entry
                .expect("an entry")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .collect();
    kept.sort();
    assert_eq!(kept, UNITS);
}

/// The converted program links against the original's config to the bytes
/// the original does. This is the claim the expected units rest on.
#[test]
fn the_converted_program_builds_to_the_original_bytes() {
    let dir = program_dir();
    let work = work_dir("convert-program-bytes");
    let cl65 = toolchain().join("cl65");
    let config = dir.join("program.cfg");
    let reference = work.join("ca65.bin");
    let mut command = Command::new(&cl65);
    command
        .args(["-t", "none", "--no-target-lib", "--cpu", "65816", "-C"])
        .arg(&config)
        .arg("--asm-include-dir")
        .arg(&dir)
        // `.macpack generic` is a file in cc65's `asminc/`, which a staged
        // `ca65` finds through no path of its own.
        .arg("--asm-include-dir")
        .arg(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../asminc"))
        .arg("-o")
        .arg(&reference)
        .args(program_inputs())
        .current_dir(&work);
    run_ok(&mut command, "cl65 on the ca65 program");

    let units = work.join("converted");
    std::fs::create_dir_all(&units).expect("create the unit directory");
    let mut paths = Vec::new();
    for unit in &converted(&program_inputs(), &[]).units {
        let path = units.join(unit.path.file_name().expect("a name"));
        std::fs::write(&path, &unit.text).expect("write a unit");
        paths.push(path);
    }
    let image = work.join("k816.bin");
    let mut command = Command::new(&cl65);
    command
        .args(["-t", "none", "--no-target-lib", "--cpu", "65816", "-C"])
        .arg(&config)
        .arg("-o")
        .arg(&image)
        .args(&paths)
        .current_dir(&work);
    run_ok(&mut command, "cl65 on the converted program");

    let (want, got) = (
        std::fs::read(&reference).expect("the ca65 image"),
        std::fs::read(&image).expect("the converted image"),
    );
    assert!(!want.is_empty());
    assert!(
        want == got,
        "the images differ:\nca65: {want:02X?}\nk816: {got:02X?}"
    );
}

// ---------------------------------------------------------------------------
// The rules a header and an access follow
// ---------------------------------------------------------------------------

/// A routine that ends in `rti` is an interrupt handler, which K816 enters at
/// the environment's widths: a header on one would be `K816-E0143`. What the
/// original assumed before its first width change is reported instead.
#[test]
fn an_interrupt_handler_states_no_width_and_says_what_it_assumed() {
    let (text, notes) = one(
        "convert-interrupt",
        ".p816\n.a16\n.code\nhandler:\n        pha\n        sep #$20\n        .a8\n        pla\n        rti\n",
    );
    assert!(text.contains("naked handler {"), "{text}");
    assert_eq!(notes.len(), 1, "{notes:?}");
    assert!(notes[0].contains("`@a16`"), "{notes:?}");

    // One that sets its widths first assumed nothing, and is not remarked on.
    let (text, notes) = one(
        "convert-interrupt-set",
        ".p816\n.a16\n.code\nhandler:\n        rep #$30\n        pha\n        pla\n        rti\n",
    );
    assert!(text.contains("naked handler {"), "{text}");
    assert_eq!(notes, Vec::<String>::new());
}

/// A register wider or narrower than the field it moves gets a typed view,
/// which is what the original meant: ca65 moves the bytes the width says.
/// An instruction that moves its datum through no register needs none.
#[test]
fn an_access_wider_than_its_field_takes_a_typed_view() {
    let source = "\
.p816
.struct IO
        .org $D000
    status  .byte
    data    .byte
    vector  .word
.endstruct
.code
reader:
        rep #$30
        .a16
        .i16
        lda IO::status
        ldx IO::data
        stz IO::status
        lda IO::vector
        sep #$20
        .a8
        lda IO::vector
        lda IO::status
        rts
";
    let (text, _) = one("convert-view", source);
    for line in [
        "lda IO.status:word",
        "ldx IO.data:word",
        "stz IO.status\n",
        "lda IO.vector\n",
        "lda IO.vector:byte",
        "lda IO.status\n",
    ] {
        assert!(text.contains(line), "no `{}` in\n{text}", line.trim());
    }
}

/// A call or a tail jump relies on the caller's width only on the axes the
/// callee states: the bridge before it establishes nothing else, so a header
/// claiming the rest would claim what the original never relied on.
#[test]
fn a_call_relies_only_on_the_widths_its_callee_states() {
    let source = "\
.p816
.code
.a16
.i16
wide:
        lda #$1234
        rts
plain:
        nop
        rts
to_wide:
        jmp wide
to_plain:
        jmp plain
calls_wide:
        jsr wide
        rts
";
    let (text, _) = one("convert-callee", source);
    for line in [
        "func wide @a16 {",
        "func plain {",
        "naked to_wide @a16 {",
        "naked to_plain {",
        "func calls_wide @a16 {",
    ] {
        assert!(text.contains(line), "no `{line}` in\n{text}");
    }
}

/// A call K816 bridges where the original did not is a note: the converted
/// program sets the callee's width first, a `sep`/`rep` the ca65 build does
/// not have, which is where the source called a routine at a width its own
/// header does not state.
#[test]
fn a_call_that_will_be_bridged_is_noted() {
    let source = "\
.p816
.code
.a16
wide:
        lda #$1234
        rts
.a8
narrow_caller:
        lda #1
        jsr wide
        rts
.a16
wide_caller:
        lda #1
        jsr wide
        rts
";
    let (_, notes) = one("convert-bridge", source);
    assert_eq!(notes.len(), 1, "{notes:?}");
    assert!(
        notes[0].contains("`wide`") && notes[0].contains("`@a16` where the original is at `@a8`"),
        "{notes:?}"
    );
}

// ---------------------------------------------------------------------------
// Regressions: one source per defect the converter has had, each converted,
// compiled, and linked against the guide's memory map to the bytes the ca65
// source links to — the shape the defect broke is asserted beside the bytes.
// ---------------------------------------------------------------------------

fn guide_config() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden/ca65-guide.cfg")
}

/// Links `inputs` with the staged `cl65` against the guide's memory map.
fn link(dir: &Path, inputs: &[PathBuf], image: &str, what: &str) -> Vec<u8> {
    let mut command = Command::new(toolchain().join("cl65"));
    command
        .args(["-t", "none", "--no-target-lib", "--cpu", "65816", "-C"])
        .arg(guide_config())
        .arg("--asm-include-dir")
        .arg(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../asminc"))
        .arg("-o")
        .arg(dir.join(image))
        .args(inputs)
        .current_dir(dir);
    run_ok(&mut command, what);
    std::fs::read(dir.join(image)).expect("the image")
}

/// A ca65 program of `files`, converted from `inputs`: the converted units
/// build to the bytes the sources do. Returns each unit's text by file
/// name, and the notes.
fn faithful_program(
    test: &str,
    files: &[(&str, &str)],
    inputs: &[&str],
) -> (BTreeMap<String, String>, Vec<String>) {
    let dir = work_dir(test);
    for entry in std::fs::read_dir(&dir).expect("read the scratch directory") {
        let path = entry.expect("an entry").path();
        if path.is_file() {
            std::fs::remove_file(&path).expect("clear the scratch directory");
        }
    }
    for (name, text) in files {
        std::fs::write(dir.join(name), text).expect("write a source");
    }
    let sources: Vec<PathBuf> = inputs.iter().map(|name| dir.join(name)).collect();
    let want = link(&dir, &sources, "ca65.bin", &format!("cl65 on `{test}`"));
    let conversion = converted(&sources, &[]);
    let mut units = BTreeMap::new();
    let mut paths = Vec::new();
    for unit in &conversion.units {
        std::fs::write(&unit.path, &unit.text).expect("write a unit");
        paths.push(unit.path.clone());
        let name = unit.path.file_name().expect("a name").to_string_lossy();
        units.insert(name.into_owned(), unit.text.clone());
    }
    let got = link(
        &dir,
        &paths,
        "k816.bin",
        &format!("cl65 on `{test}` converted"),
    );
    assert!(
        want == got,
        "`{test}` converts to different bytes\nca65: {want:02X?}\nk816: {got:02X?}\n{units:#?}"
    );
    let notes = conversion
        .notes
        .iter()
        .map(|note| note.message.clone())
        .collect();
    (units, notes)
}

/// One source, as [`faithful_program`]: the unit's text, and the notes.
fn faithful(test: &str, source: &str) -> (String, Vec<String>) {
    let (units, notes) = faithful_program(test, &[("unit.asm", source)], &["unit.asm"]);
    (units["unit.kk"].clone(), notes)
}

#[track_caller]
fn has(text: &str, wanted: &str) {
    assert!(text.contains(wanted), "no `{wanted}` in\n{text}");
}

/// The lexer had `==` and `:=` but not `=`, so an equate did not read.
#[test]
fn an_equate_is_read_with_its_equals_sign() {
    let (text, _) = faithful(
        "regress-equals",
        "COUNT = 3\n.code\n.a8\nfirst:  lda #COUNT\n        rts\n",
    );
    has(&text, "const COUNT = 3");
    has(&text, "lda #COUNT");
}

/// `beq :+` was read as a label named `beq`: a ca65 label's colon is
/// attached to its name, and one after a space is an unnamed reference.
#[test]
fn a_branch_to_an_unnamed_label_is_not_a_label_named_for_the_mnemonic() {
    let (text, _) = faithful(
        "regress-unnamed",
        ".code\n.a8\n.i8\nfirst:  ldx #3\n:       dex\n        bne :-\n        beq :+\n        nop\n:       rts\n",
    );
    // The two unnamed labels were named, and the names are then raised away:
    // the loop and the skip they were read as are the program's own.
    for line in ["loop {", "} while !=", "!= {"] {
        has(&text, line);
    }
    assert!(!text.contains("beq:"), "{text}");
}

/// A loop label after a terminator that only its own routine's branches
/// reach is a local label. The records an operand leaves for the width of
/// its access once counted as uses from outside the routine, and every such
/// label became a routine of its own.
#[test]
fn a_label_only_its_own_branches_reach_stays_local_after_a_terminator() {
    let (text, _) = faithful(
        "regress-local-after-terminator",
        ".zeropage\ncount:  .res 1\n.code\n.a8\n.i8\nfirst:  ldx #3\n        bra test\nloop:   dex\n        lda count\ntest:   cpx #0\n        bne loop\n        rts\n",
    );
    for line in ["func first", ".loop:", ".test:", "bra .test", "bne .loop"] {
        has(&text, line);
    }
    assert_eq!(text.matches("func ").count(), 1, "{text}");
}

/// A global label after a terminator that nothing names starts a routine:
/// nothing ties it to the routine above, and ca65 ends a cheap local's
/// scope at it.
#[test]
fn an_unreferenced_label_after_a_return_starts_a_routine() {
    let (text, _) = faithful(
        "regress-unreferenced",
        ".code\n.a8\nfirst:  lda #1\n        rts\nunused: lda #2\n        rts\n",
    );
    has(&text, "func first @a8 {");
    has(&text, "func unused @a8 {");
}

/// `rts` / `.a8` / `g:` — a width directive between two routines runs
/// nothing, and once ended the terminator's run, so `g` became a local label
/// of `f`, with `f`'s `rts` left in the middle and `g`'s first immediate
/// unsizeable.
#[test]
fn a_width_directive_between_routines_does_not_join_them() {
    let (text, _) = faithful(
        "regress-directive-between",
        ".code\n.a16\nfirst:  lda #$1234\n        rts\n.a8\nsecond: lda #1\n        rts\n",
    );
    has(&text, "func first @a16 {");
    has(&text, "func second @a8 {");
    assert!(!text.contains(".second:"), "{text}");
    assert!(
        !text.contains("rts"),
        "both returns are their sections' own:\n{text}"
    );
}

/// Dropped `.export`/`.import` lines left their blank lines behind, in runs.
#[test]
fn dropped_linkage_lines_leave_no_run_of_blank_lines() {
    let (units, _) = faithful_program(
        "regress-linkage",
        &[
            (
                "unit.asm",
                ".export first\n\n.import second\n\n\n.code\nfirst:  jsr second\n        rts\n",
            ),
            ("lib.asm", ".export second\n.code\nsecond: rts\n"),
        ],
        &["unit.asm", "lib.asm"],
    );
    let text = &units["unit.kk"];
    assert!(!text.contains("\n\n\n"), "{text}");
}

/// The imports a unit needs come before anything that uses them; the first
/// draft wrote them at the end.
#[test]
fn imports_come_before_the_code() {
    let conversion = converted(&program_inputs(), &[]);
    let main = conversion
        .units
        .iter()
        .find(|unit| unit.path.ends_with("main.kk"))
        .expect("main.kk");
    let last_import = main.text.rfind("import ").expect("an import");
    let first_code = main.text.find("segment ").expect("a segment");
    assert!(last_import < first_code, "{}", main.text);
}

/// A name K816 reserves is renamed, and the probe that decides asks the
/// compiler about the name *as an operand* too: `address` is fine as a
/// declaration and not in `jsr address`.
#[test]
fn a_name_k816_reserves_as_an_operand_is_renamed() {
    let (text, notes) = faithful(
        "regress-address",
        ".code\n.a8\naddress:\n        lda #1\n        rts\nfirst:  jsr address\n        rts\n",
    );
    has(&text, "func address_ @a8 {");
    has(&text, "    address_\n");
    assert!(
        notes.iter().any(|note| note.contains("`address_` here")),
        "{notes:?}"
    );
}

/// An instruction that reads a routine's own bytes is one K816 refuses, so
/// the converter says so at the line.
#[test]
fn reading_a_routines_bytes_is_noted() {
    let (_, notes) = one(
        "regress-code-read",
        ".code\n.a8\nroutine:\n        lda #1\n        rts\nfirst:  lda routine\n        rts\n",
    );
    assert!(
        notes
            .iter()
            .any(|note| note.contains("reads the bytes of the routine `routine`")),
        "{notes:?}"
    );
}

/// An equate after a routine's last way out is the next thing in the file:
/// the first draft pulled it, and the comments after it, into the routine.
#[test]
fn an_equate_after_a_return_is_not_part_of_the_routine() {
    let (text, _) = faithful(
        "regress-equate-after",
        ".code\n.a8\nfirst:  lda #1\n        rts\nLIMIT = 4               ; the most there can be\n; A second routine.\nsecond: lda #LIMIT\n        rts\n",
    );
    has(
        &text,
        "func first @a8 {\n    lda #1\n}\nconst LIMIT = 4         // the most there can be\n// A second routine.\nfunc second @a8 {",
    );
}

/// The header scan stopped at the first `bra`, and never reached the tail
/// `jmp` whose callee states the width the routine relies on.
#[test]
fn the_header_scan_reads_past_a_branch_to_the_tail_jump() {
    let (text, _) = faithful(
        "regress-bra-tail",
        ".code\n.a16\n.i16\nwide:   lda #$1234\n        rts\nfirst:  bra skip\n        nop\nskip:   jmp wide\n",
    );
    has(&text, "naked first @a16 {");
}

/// The header scan did not look inside a conditional, and missed the
/// immediate there that relies on the width.
#[test]
fn the_header_scan_reads_inside_a_conditional() {
    let (text, _) = faithful(
        "regress-if-header",
        "DEBUG = 1\n.code\n.a16\nfirst:\n.if DEBUG\n        lda #$1234\n.endif\n        rts\n",
    );
    has(&text, "func first @a16 {");
}

/// An unnamed label referenced only from a macro's argument — `blt :-` —
/// was named `.next`, because the reference scan did not read arguments.
#[test]
fn an_unnamed_label_named_in_a_macro_argument_is_named() {
    let (text, _) = faithful(
        "regress-macro-argument",
        ".macro blt target\n        bcc target\n.endmacro\n.code\n.a8\n.i8\nfirst:  ldx #3\n:       dex\n        cpx #1\n        blt :-\n        blt :+\n        nop\n:       rts\n",
    );
    has(&text, "blt #.loop");
    has(&text, "blt #.skip");
    assert!(!text.contains(".next"), "{text}");
}

/// Two bytes the program moves sixteen bits at a time are a `:word`, read
/// past the plain address record each access also leaves; two moved a byte
/// at a time are a span.
#[test]
fn storage_moved_sixteen_bits_at_a_time_is_a_word() {
    let (text, _) = faithful(
        "regress-word-storage",
        ".zeropage\nptr:    .res 2\nbytes:  .res 2\n.code\n.a16\n.i16\nfirst:  lda ptr\n        sta ptr\n        lda #ptr\n        rts\n.a8\nsecond: lda bytes\n        sta bytes+1\n        rts\n",
    );
    has(&text, "var dp ptr:word");
    has(&text, "var dp bytes[2]");
}

/// A width directive at a routine's top that its header states is the
/// header, not a second statement of it.
#[test]
fn a_header_width_is_not_asserted_again_in_the_body() {
    let (text, _) = faithful(
        "regress-header-twice",
        ".code\nfirst:\n.a16\n.i16\n        lda #$1234\n        ldx #$1234\n        rts\n",
    );
    has(&text, "func first @a16 @i16 {");
    assert_eq!(text.matches("@a16").count(), 1, "{text}");
}

/// A `.byte 0,0` entry stays one entry on one line.
#[test]
fn a_byte_list_stays_on_its_line() {
    let (text, _) = faithful(
        "regress-byte-list",
        ".rodata\npair:   .byte 0,0\nthree:  .byte 1, 2, 3\n",
    );
    has(&text, "    pair:   0 0\n");
    has(&text, "    three:  1 2 3\n");
}

/// A label on the line of its data keeps the gap after it, so a column of
/// one-liners stays a column.
#[test]
fn one_liner_data_labels_keep_their_column() {
    let (text, _) = faithful(
        "regress-one-liner",
        ".rodata\nstr_A:      .byte \"abc\", 0\nstr_LONGER: .byte \"de\", 0\n",
    );
    has(&text, "    str_A:      \"abc\" 0\n");
    has(&text, "    str_LONGER: \"de\" 0\n");
}

/// A `.define` keeps the spaces around its `=`, and a comment that is
/// already `; // text` is not doubled into `// // text`.
#[test]
fn a_define_keeps_its_spacing_and_a_double_slash_comment_is_not_doubled() {
    let (text, _) = faithful(
        "regress-define-comment",
        ".define LIMIT 200       ; the most\n; // already a K816-style comment\n.code\n.a8\nfirst:  lda #LIMIT      ; // trailing\n        rts\n",
    );
    has(&text, "const LIMIT = 200       // the most\n");
    has(&text, "\n// already a K816-style comment\n");
    assert!(!text.contains("// //"), "{text}");
}

/// A comment on a line that holds a label and an instruction is said once,
/// with the instruction — not again on the routine's opening line.
#[test]
fn a_comment_on_a_label_and_instruction_line_is_said_once() {
    let (text, _) = faithful(
        "regress-label-line-comment",
        ".code\n.a8\n.i8\nfirst:  ldx #3          ; start\nloop:   dex             ; count down\n        bne loop\n        rts\n",
    );
    has(&text, "func first @i8 {\n    ldx #3          // start\n");
    assert_eq!(text.matches("start").count(), 1, "{text}");
}

/// A `jsl` to a routine that returns with `rtl` is `far NAME`.
#[test]
fn a_far_call_is_far_name() {
    let (text, _) = faithful(
        "regress-far-call",
        ".code\n.a8\nfirst:  jsl second\n        rts\nsecond: lda #1\n        rtl\n",
    );
    has(&text, "    far second\n");
    has(&text, "far func second @a8 {");
}

/// A data `.repeat` is data, and one whose body reads its counter is
/// written out a pass per line, since K816's `repeat` has no counter. The
/// counter case used to end the data block and land in a routine of its own
/// as unconvertible lines.
#[test]
fn a_data_repeat_that_reads_its_counter_is_written_out() {
    let (text, notes) = faithful(
        "regress-data-repeat",
        ".rodata\ntable:\n.repeat 4, i\n        .byte i * 2\n.endrep\nfill:\n.repeat 3\n        .byte $EA\n.endrep\n",
    );
    has(&text, "data table {\n    0\n    2\n    4\n    6\n");
    has(&text, "repeat 3 { $EA }");
    assert_eq!(notes, Vec::<String>::new());
}

/// An `.align` written before a label is the first entry of the label's
/// block, so the label is what lands on the boundary — here three bytes
/// past a one-byte routine.
#[test]
fn an_align_before_a_label_is_the_first_entry_of_its_block() {
    let (text, _) = faithful(
        "regress-align",
        ".code\nfirst:  rts\n.align 4\ntbl:    .byte 1, 2, 3\n",
    );
    has(&text, "data tbl {\n    align 4\n");
}

/// A width block restores the width the compiler knew at its opening, and
/// nothing where it knew none. ca65 was told `.a16` here, but nothing in the
/// routine relies on it before the `sep`, so the header states no width and
/// K816 knows none: written as `@a8 { … }`, the block would restore nothing
/// and the original's `rep #$20` would be gone. So the change is two
/// statements.
#[test]
fn a_width_change_with_no_known_width_around_it_is_not_a_block() {
    let (text, _) = faithful(
        "regress-unknown-outer",
        ".code\n.a16\nfirst:  nop\n        sep #$20\n        .a8\n        lda #1\n        rep #$20\n        .a16\n        rts\n",
    );
    has(&text, "func first {");
    assert!(!text.contains("@a8 {"), "{text}");
    has(&text, "    @a8\n    lda #1\n    @a16\n");
}

/// A data block names another unit's routines and data: the names are
/// declared before the unit's own data is resolved, and reading an
/// imported data label is reading that unit's bytes.
#[test]
fn data_naming_another_units_routines_converts_and_links() {
    let (units, _) = faithful_program(
        "regress-imported-data",
        &[
            (
                "main.asm",
                ".import lib_a, lib_b, lib_table\n.rodata\nhandlers: .addr lib_a, lib_b, lib_table\n.code\n.a8\nmain:   jsr lib_a\n        lda lib_table\n        rts\n",
            ),
            (
                "lib.asm",
                ".export lib_a, lib_b, lib_table\n.code\nlib_a:  rts\nlib_b:  rts\n.rodata\nlib_table: .byte 1, 2\n",
            ),
        ],
        &["main.asm", "lib.asm"],
    );
    has(&units["main.kk"], "word lib_a lib_b lib_table");
}

/// Routines that call and tail-jump to each other, and to themselves, in a
/// cycle: the header scan follows each callee once. It first followed every
/// path to a depth of sixteen — three calls a level is forty million scans,
/// and the test that met it never finished.
#[test]
fn a_cycle_of_calls_does_not_stall_the_header_scan() {
    // Twenty routines, so that a scan which follows every path — even one
    // that stops at a cycle — is three calls a level, twenty levels deep.
    const ROUTINES: usize = 20;
    let mut source = String::from(".code\n.a16\nwide:   lda #$1234\n        rts\n");
    for index in 0..ROUTINES {
        let next = (index + 1) % ROUTINES;
        let after = (index + 2) % ROUTINES;
        source.push_str(&format!(
            "r{index}:     jsr r{next}\n        jsr r{after}\n        jsr r{index}\n        jsr wide\n        jmp r{next}\n"
        ));
    }
    let (sender, receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let _ = sender.send(one("regress-call-cycle", &source).0);
    });
    let text = receiver
        .recv_timeout(std::time::Duration::from_secs(20))
        .expect("the conversion finishes");
    // `wide` states `@a16`, so each routine that calls it relies on the
    // width it had there; the cycle itself states nothing.
    for index in 0..ROUTINES {
        has(&text, &format!("naked r{index} @a16 {{"));
    }
}

// ---------------------------------------------------------------------------
// The command
// ---------------------------------------------------------------------------

/// Without `--out-dir` each unit is written beside its source, and a second
/// run refuses to replace what the first wrote until told to.
#[test]
fn convert_writes_beside_its_sources_and_replaces_only_with_force() {
    let dir = fixture_copy("convert-beside");
    let output = k816(&dir, &["main.asm", "queue.asm"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    for unit in UNITS {
        let want = std::fs::read_to_string(expected_dir().join(unit)).expect("an expected unit");
        let got = std::fs::read_to_string(dir.join(unit)).expect("a written unit");
        assert_eq!(got, want, "{unit}");
    }

    std::fs::write(dir.join("main.kk"), "// edited\n").expect("edit a unit");
    let output = k816(&dir, &["main.asm", "queue.asm"]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("`--force` replaces it"), "{stderr}");
    // Refused before anything was written, not halfway through.
    assert_eq!(
        std::fs::read_to_string(dir.join("main.kk")).expect("the edited unit"),
        "// edited\n"
    );

    let output = k816(&dir, &["--force", "main.asm", "queue.asm"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        std::fs::read_to_string(dir.join("main.kk")).expect("the rewritten unit"),
        std::fs::read_to_string(expected_dir().join("main.kk")).expect("the expected unit")
    );
}

/// `--out-dir` keeps the tree's shape under the directory every unit is
/// under — including a file an `-I` found outside the inputs' directory —
/// and writes nothing beside a source.
#[test]
fn convert_out_dir_writes_only_under_it() {
    let dir = work_dir("convert-out-dir");
    let source = dir.join("src");
    let out = dir.join("out");
    let _ = std::fs::remove_dir_all(&source);
    let _ = std::fs::remove_dir_all(&out);
    std::fs::create_dir_all(source.join("kernel")).expect("create the source tree");
    std::fs::write(
        source.join("kernel/main.asm"),
        ".include \"zp.inc\"\n.code\nmain:\n        lda counter\n        rts\n",
    )
    .expect("write the program");
    std::fs::write(source.join("zp.inc"), ".zeropage\ncounter: .res 1\n")
        .expect("write the include");

    // The input relative, the include directory absolute.
    let include = source.to_string_lossy().into_owned();
    let output = k816(
        &dir,
        &["-I", &include, "--out-dir", "out", "src/kernel/main.asm"],
    );
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(out.join("kernel/main.kk").is_file());
    assert!(out.join("zp.kk").is_file());
    let mut beside: Vec<PathBuf> = Vec::new();
    for path in [source.join("kernel"), source.clone()] {
        for entry in std::fs::read_dir(&path).expect("read the source tree") {
            let path = entry.expect("an entry").path();
            if path.extension().is_some_and(|extension| extension == "kk") {
                beside.push(path);
            }
        }
    }
    assert_eq!(beside, Vec::<PathBuf>::new());
    let main = std::fs::read_to_string(out.join("kernel/main.kk")).expect("the unit");
    assert!(main.contains("import \"../zp.kk\""), "{main}");
}

/// `-o` names the one unit a conversion writes, `-` is standard output, and
/// a conversion that writes several is refused rather than half-written.
#[test]
fn convert_o_names_one_unit() {
    let dir = fixture_copy("convert-one");
    let output = k816(&dir, &["-o", "-", "hw.inc"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let want = std::fs::read_to_string(expected_dir().join("hw.kk")).expect("the expected unit");
    assert_eq!(String::from_utf8_lossy(&output.stdout), want);

    let output = k816(&dir, &["-o", "layout.kk", "hw.inc"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        std::fs::read_to_string(dir.join("layout.kk")).expect("the unit"),
        want
    );

    let output = k816(&dir, &["-o", "all.kk", "main.asm", "queue.asm"]);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("use `--out-dir`"));
    assert!(!dir.join("all.kk").exists());
}

/// `--check` compiles every unit it wrote, as one program, and fails with
/// the compiler's diagnostic where K816 refuses what the source did.
#[test]
fn convert_check_compiles_what_it_wrote() {
    let dir = fixture_copy("convert-check");
    let output = k816(&dir, &["--check", "main.asm", "queue.asm"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    // A `jsr` to a routine that returns with `rtl` leaves a stack the routine
    // does not return from; the converter keeps it for the compiler to refuse.
    std::fs::write(
        dir.join("far.asm"),
        ".p816\n.a8\n.i8\n.code\nmain:   jsr far_routine\n        rts\nfar_routine:\n        lda #1\n        rtl\n",
    )
    .expect("write the source");
    let output = k816(&dir, &["--check", "far.asm"]);
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("K816-E0109"),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// An `.inc` is one unit with the `.asm` of its stem only when that `.asm`
/// includes it. Given both and no include, each would be `NAME.kk`, and the
/// second would silently replace the first.
#[test]
fn two_units_with_one_name_are_refused() {
    let dir = work_dir("convert-collision");
    let (asm, inc) = (dir.join("task.asm"), dir.join("task.inc"));
    std::fs::write(&asm, ".code\ntask_run:\n        rts\n").expect("write the source");
    std::fs::write(&inc, "TASKS_NO = 4\n").expect("write the include");
    let request = Request {
        inputs: vec![asm, inc],
        include_dirs: Vec::new(),
    };
    let error = convert(&request, &|path| std::fs::read_to_string(path).ok())
        .expect_err("two units, one name");
    assert!(error.contains("would both become `task.kk`"), "{error}");
}

// ---------------------------------------------------------------------------
// Raising (`plans/14-readable-hla.md` decision 15): the converted program's
// labels and branches into K816's loops and blocks, wherever that changes no
// byte. Every case below is `faithful`, so each one also links to the bytes
// the ca65 source does.

/// A label a conditional branch at the end of a run returns to is a loop.
#[test]
fn a_label_a_branch_returns_to_is_a_loop() {
    let (text, _) = faithful(
        "raise-loop",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  ldx #8\n@next:  inc total\n        dex\n        bne @next\n        rts\n",
    );
    has(&text, "loop {");
    has(&text, "} while !=");
    assert!(!text.contains(".next"), "{text}");
}

/// A conditional branch over a run to a label nothing else names is a block
/// the run sits in, run when the branch is not taken — and the comment that
/// said where the branch goes is said where the block ends.
#[test]
fn a_branch_over_a_run_is_a_block_and_its_comment_closes_it() {
    let (text, _) = faithful(
        "raise-block",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  lda total\n        beq @done       ; nothing to count\n        inc total\n@done:  rts\n",
    );
    has(&text, "!= {");
    has(&text, "} // nothing to count");
    assert!(!text.contains(".done"), "{text}");
}

/// A carry a compare left reads as the relation, and any other carry as the
/// flag: `bcc` past a run after `cmp` is `>= { … }`, and after `lsr` the
/// block runs on `!c?`.
#[test]
fn a_carry_after_a_compare_is_a_relation_and_any_other_is_the_flag() {
    let (text, _) = faithful(
        "raise-carry",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  lda total\n        cmp #10\n        bcc @small\n        inc total\n@small: lsr\n        bcs @odd\n        inc total\n@odd:   rts\n",
    );
    has(&text, ">= {");
    has(&text, "!c? {");
}

/// Inside a loop, a conditional branch to the label right after it is a
/// `break` and one back to its top a `repeat`; the labels go when nothing
/// else names them.
#[test]
fn a_branch_out_of_a_loop_is_a_break_and_one_back_to_its_top_a_repeat() {
    let (text, _) = faithful(
        "raise-break",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  ldx #8\n@top:   lda total\n        beq @out\n        lsr total\n        bcs @top\n        dex\n        bne @top\n@out:   rts\n",
    );
    for line in ["loop {", "== break", "c? repeat", "} while !="] {
        has(&text, line);
    }
    assert!(!text.contains(".top") && !text.contains(".out"), "{text}");
}

/// A loop closed by `bra` is `loop { … } always`, and the branch out of it a
/// `break`: K816 closes `always` with a `JMP`, which the optimizer shortens to
/// the `BRA` the source wrote wherever the target is in reach. Before that
/// optimisation the measurement refused it, a byte longer.
#[test]
fn a_loop_closed_by_bra_is_an_always_loop() {
    let (text, _) = faithful(
        "raise-bra",
        ".code\n.a8\n.i8\nfirst:  ldx #8\n@top:   dex\n        beq @out\n        bra @top\n@out:   rts\n",
    );
    for line in ["loop {", "== break", "} always"] {
        has(&text, line);
    }
    assert!(!text.contains(".top") && !text.contains(".out"), "{text}");
}

/// A branch over a run that ends in a jump over a second run, to the label
/// that begins it, is `COND { … } else { … }`; the branch's comment says where
/// it lands, at the `} else {`.
#[test]
fn a_branch_over_a_run_that_jumps_over_another_is_an_else() {
    let (text, _) = faithful(
        "raise-else",
        ".code\n.a8\n.i8\nfirst:  cmp #10\n        bcc @digit      ; use digit\n        adc #6\n        bra @done\n@digit: adc #'0'\n@done:  rts\n",
    );
    for line in [">= {", "} else { // use digit", "adc #'0'"] {
        has(&text, line);
    }
    assert!(
        !text.contains(".digit") && !text.contains(".done"),
        "{text}"
    );
}

/// Inside a loop, an unconditional jump back to its top that is not the jump
/// closing it is a bare `repeat`, and one past its end a bare `break`.
#[test]
fn an_unconditional_jump_inside_a_loop_is_a_bare_break_or_repeat() {
    let (text, _) = faithful(
        "raise-bare",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  ldx #8\n@top:   lda total\n        beq @idle\n        dec total\n        bra @top\n@idle:  dex\n        bne @more\n        bra @out\n@more:  inc total\n        cpx #2\n        bne @top\n@out:   rts\n",
    );
    let bare = |word: &str| text.lines().any(|line| line.trim() == word);
    assert!(bare("repeat") && bare("break"), "{text}");
    has(&text, "} while x?#2 !=");
    assert!(!text.contains(".top") && !text.contains(".out"), "{text}");
}

/// A branch into the middle of a run from outside it keeps the run's labels:
/// neither a loop nor a block can be entered anywhere but its top.
#[test]
fn a_branch_into_a_run_from_outside_keeps_its_labels() {
    let (text, _) = faithful(
        "raise-entered",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  ldx #8\n        bra @mid\n@top:   inc total\n@mid:   dex\n        bne @top\n        rts\n",
    );
    has(&text, ".top:");
    has(&text, ".mid:");
    has(&text, "bne .top");
    assert!(!text.contains("loop {"), "{text}");
}

/// A raise is measured, not assumed: here the label states an entry width
/// the skipping branch does not arrive at, so K816 switches at the branch
/// (decision 6 of the width plan) and the routine leaves 16 bits wide. As a
/// block, the two paths would meet after it at different widths, with no
/// switch and a routine that leaves at a width nobody can name — a program
/// that compiles, and is a different one — so the labels stay. A raise that
/// does not compile at all is refused before it is compared; this one is
/// refused by the comparison.
#[test]
fn a_raise_that_would_change_the_program_keeps_its_labels() {
    let (text, _) = one(
        "raise-refused",
        ".code\n.a8\n.i8\nfirst:  lda #1\n        beq @skip\n        rep #$20\n        .a16\n@skip:  nop\n        rts\n",
    );
    has(&text, "beq .skip");
    has(&text, ".skip:");
    assert!(!text.contains("!= {"), "{text}");
}

/// A comment after a raised branch keeps the space before it. A mnemonic's
/// span runs to its comment, spaces included, and the first draft replaced
/// the spaces with the branch: `} while !=// again`.
#[test]
fn a_raised_branch_keeps_the_space_before_its_comment() {
    let (text, _) = faithful(
        "raise-spacing",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  ldx #8\n@next:  inc total\n        dex\n        bne @next       ; again\n        rts\n",
    );
    let line = text
        .lines()
        .find(|line| line.contains("} while !="))
        .unwrap_or_else(|| panic!("no loop in\n{text}"));
    assert!(
        line.contains("!= ") && line.ends_with("// again"),
        "{line:?}"
    );
    assert!(!line.contains("!=//"), "{line:?}");
}

/// A skipping branch's comment and the label's own are said once, together,
/// at the closing brace — the first draft wrote both, `// a // b`.
#[test]
fn a_skip_and_its_labels_comments_are_one_comment_at_the_close() {
    let (text, _) = faithful(
        "raise-two-comments",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  lda total\n        beq @done       ; nothing to count\n        inc total\n@done:                  ; counted\n        rts\n",
    );
    has(&text, "} // nothing to count; counted");
    assert!(!text.contains("// nothing to count //"), "{text}");
}

/// A loop and a skip that cross — a branch inside the loop over its back
/// edge to a label after it — cannot both be raised, and the loop is the one
/// that is. The first draft raised the smaller skip and lost the loop.
#[test]
fn a_loop_is_raised_before_a_skip_that_crosses_it() {
    let (text, _) = faithful(
        "raise-crossing",
        ".zeropage\nslots:  .res 8\n.code\n.a8\n.i8\nfirst:  ldx #0\n@loop:  lda slots,x\n        beq @found\n        inx\n        cpx #8\n        bne @loop\n        sec\n        rts\n@found: clc\n        rts\n",
    );
    has(&text, "loop {");
    has(&text, "} while x?#8 !=");
    has(&text, "beq .found");
    assert!(!text.contains(".loop"), "{text}");
}

/// A branch from inside a loop to the label after it is a `break` even when
/// something before the loop branches there too: the label stays for that
/// use, and the branch over the loop is then a block of its own. The first
/// draft raised a `break` only when the loop's branches were the label's
/// only uses.
#[test]
fn a_break_is_raised_when_the_exit_is_also_reached_from_outside() {
    let (text, _) = faithful(
        "raise-shared-exit",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  ldx #8\n        lda total\n        beq @out\n@top:   lda total\n        beq @out\n        dex\n        bne @top\n@out:   rts\n",
    );
    for line in ["!= {", "loop {", "== break", "} while !="] {
        has(&text, line);
    }
    assert!(!text.contains(".out") && !text.contains(".top"), "{text}");
}

/// A loop the source closed with `jmp` keeps its label: the converted `jmp`
/// stays a `JMP`, because the optimizer never swaps a jump the source wrote,
/// and K816's `always` would be the shorter `BRA` — a different program,
/// which the measurement refuses. The bytes are the source's either way.
#[test]
fn a_loop_the_source_closed_with_jmp_keeps_its_label() {
    let (text, _) = faithful(
        "raise-jmp",
        ".code\n.a8\n.i8\nfirst:  ldx #8\n@top:   dex\n        beq @out\n        jmp @top\n@out:   rts\n",
    );
    has(&text, ".top:");
    has(&text, "jmp .top");
    assert!(!text.contains("} always"), "{text}");
}

// ---------------------------------------------------------------------------
// Sub-plan Q: the converter writes `if`, and folds a compare into it
// (`plans/16-if-statement.md` decision 8). Every case is linked by
// `faithful`, so each rewrite is also byte-identical to ca65's image.

/// A raised block is `if COND {`, and the compare right before its branch
/// is written in the `if`.
#[test]
fn a_raised_block_writes_if_and_folds_its_compare() {
    let (text, _) = faithful(
        "if-block-fold",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  lda total\n        cmp #10\n        bcs @big\n        inc total\n@big:   rts\n",
    );
    has(&text, "if a?#10 < {");
    assert!(!text.contains("cmp #10"), "{text}");
}

/// The same for a choice, `cpx` and `bit` included: each compare names its
/// register, and the bit test is `a&?`.
#[test]
fn a_raised_choice_folds_cpx_and_bit() {
    let (text, _) = faithful(
        "if-choice-fold",
        ".zeropage\ntotal:  .res 1\nflags:  .res 1\n.code\n.a8\n.i8\nfirst:  cpx #4\n        bne @other\n        inc total\n        bra @next\n@other: dec total\n@next:  lda #1\n        bit flags\n        beq @done\n        inc total\n@done:  rts\n",
    );
    has(&text, "if x?#4 == {");
    has(&text, "} else {");
    has(&text, "if a&?flags != {");
}

/// A compare with a comment of its own stays a statement: the comment would
/// have nowhere to go but the line the branch's already has.
#[test]
fn a_compare_with_its_own_comment_is_not_folded() {
    let (text, _) = faithful(
        "if-fold-comment",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  lda total\n        cmp #10         ; the limit\n        bcs @big\n        inc total\n@big:   rts\n",
    );
    has(&text, "cmp #10         // the limit");
    has(&text, "if < {");
}

/// A compare of a number stays a statement: `cmp $10` reads an address, and
/// `a?$10` in K816's HLA would compare with the value.
#[test]
fn a_compare_of_a_number_is_not_folded() {
    let (text, _) = faithful(
        "if-fold-number",
        ".code\n.a8\n.i8\nfirst:  lda #3\n        cmp $10\n        bcs @big\n        inx\n@big:   rts\n",
    );
    has(&text, "cmp $10");
    has(&text, "if < {");
}

/// Inside a loop, a conditional `break` or `repeat` folds the compare before
/// it, and the closing branch's compare goes in the `while`.
#[test]
fn a_loops_exits_and_test_fold_their_compares() {
    let (text, _) = faithful(
        "if-loop-fold",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  ldx #0\n@top:   inx\n        cpx #3\n        beq @top\n        lda total\n        cmp #9\n        bcs @out\n        inc total\n        cpx #8\n        bne @top\n@out:   rts\n",
    );
    has(&text, "if x?#3 == repeat");
    has(&text, "if a?#9 >= break");
    has(&text, "} while x?#8 !=");
}

/// An `else` whose whole body is one `if` is `} else if`. Each level of
/// the nested choice has its own exit label, as assembly written for it
/// does.
#[test]
fn an_else_holding_only_an_if_is_an_else_if() {
    let (text, _) = faithful(
        "if-chain",
        ".zeropage\ntotal:  .res 1\n.code\n.a8\n.i8\nfirst:  lda total\n        cmp #1\n        bne @two\n        ldx #10\n        bra @done\n@two:   cmp #2\n        bne @three\n        ldx #20\n        bra @inner\n@three: ldx #30\n@inner:\n@done:  stx total\n        rts\n",
    );
    has(&text, "if a?#1 == {");
    has(&text, "} else if a?#2 == {");
    has(&text, "} else {");
}
