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
