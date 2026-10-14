/*****************************************************************************/
/*                                                                           */
/*                                 driver.rs                                 */
/*                                                                           */
/*        cl65 as the way a K816 program is built, held to the manual path   */
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

//! Sub-plan I's gates over `cl65`.
//!
//! The driver owns no language semantics, no diagnostic, no lowering and no
//! emitted byte, and this file is what says so: **if building a program
//! through `cl65` produces different bytes from building it by hand, the
//! driver is wrong, never the compiler.** So almost every test here is an
//! identity — the same program built twice, once by the manual `k816
//! compile` → `ca65` → `ld65` recipe and once by one `cl65` command line —
//! and what it compares is the *linked image*.
//!
//! Raw ca65 objects are deliberately not compared. `.debuginfo on` records
//! the generated assembly's path and mtime, and `cl65`'s intermediate carries
//! a PID in its name; those differ from the manual harness's by construction
//! and say nothing about the driver.
//!
//! Every output — manual assembly, driver `-o`, intermediate and final image
//! — lives in a case-specific child of `work_dir`, so no gate writes a `.s`
//! or an `.o` beside a corpus source and two runs over the same unit set
//! cannot collide.

mod common;

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use common::{run_ok, toolchain, work_dir};

#[path = "common/corpus.rs"]
mod corpus;

use corpus::{compiles, programs, stem};

fn cl65() -> Command {
    Command::new(toolchain().join("cl65"))
}

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// The options every K816 build states, and why (`cl65.sgml`, "K816 files").
///
/// `-t none` because cl65's default target is the C64, and ld65 would look
/// for `c64.cfg`; `--no-target-lib` because `-C` replaces the target's
/// configuration but not its library, and there is no `none.lib`.
const TARGET_ARGS: &[&str] = &["-t", "none", "--no-target-lib"];

/// Runs a command, returning its output and requiring nothing of the status.
fn run(command: &mut Command, what: &str) -> Output {
    command
        .output()
        .unwrap_or_else(|error| panic!("cannot run {what}: {error}"))
}

// ---------------------------------------------------------------------------
// The manual path — the oracle every identity below is against
// ---------------------------------------------------------------------------

/// Builds `units` by hand into `directory`, returning the linked image.
///
/// This is `examples/fullstack/Makefile` before sub-plan I, and the recipe
/// `tests/compat.rs` and `tests/fullstack.rs` each carry: every unit compiled
/// on its own, each assembly assembled, the objects linked against
/// `config`. It is written out once more here rather than
/// shared with those suites because an oracle that runs the thing under test
/// is not an oracle.
fn build_by_hand(directory: &Path, units: &[PathBuf], config: &Path, extra: &[&Path]) -> Vec<u8> {
    let mut objects: Vec<PathBuf> = Vec::new();
    for (index, unit) in units.iter().enumerate() {
        let assembly = directory.join(format!("manual-{index}.s"));
        let object = directory.join(format!("manual-{index}.o"));
        // One file per invocation, and nothing naming the others: which
        // units a source belongs with is stated in the source. This is what
        // the manual path *is* now, so the driver having nothing to compute
        // is the two paths agreeing rather than one of them cheating.
        let mut compile = Command::new(env!("CARGO_BIN_EXE_k816"));
        compile.arg("compile").arg(unit);
        compile.args(["-o".as_ref(), assembly.as_os_str()]);
        run_ok(&mut compile, "k816 compile");

        run_ok(
            Command::new(toolchain().join("ca65"))
                .arg(&assembly)
                // The same directory cl65 passes for a K816 file: a `binary`
                // path was checked against the source's own directory and
                // reaches the assembly untouched.
                .args([
                    "--bin-include-dir".as_ref(),
                    unit.parent().unwrap_or(Path::new(".")).as_os_str(),
                ])
                .args(["-o".as_ref(), object.as_os_str()]),
            "ca65 on a k816-generated module",
        );
        objects.push(object);
    }
    for (index, source) in extra.iter().enumerate() {
        let object = directory.join(format!("manual-extra-{index}.o"));
        run_ok(
            Command::new(toolchain().join("ca65"))
                .arg(source)
                .args(["-o".as_ref(), object.as_os_str()]),
            "ca65 on a hand-written module",
        );
        objects.push(object);
    }

    let image = directory.join("manual.bin");
    let mut link = Command::new(toolchain().join("ld65"));
    link.args(["-C".as_ref(), config.as_os_str()])
        .args(["-o".as_ref(), image.as_os_str()]);
    for object in &objects {
        link.arg(object);
    }
    run_ok(&mut link, "ld65 linking the manual path");
    std::fs::read(&image).expect("read the manual image")
}

/// Builds the same program with one `cl65` command line.
///
/// The link order is the argument order, which is why `extra` comes first
/// here and last in [`build_by_hand`]: `examples/fullstack` puts its startup
/// module in the first segment, and cl65 adds each object where its source
/// appears.
fn build_through_driver(
    directory: &Path,
    units: &[PathBuf],
    config: &Path,
    extra: &[&Path],
) -> Vec<u8> {
    let image = directory.join("driver.bin");
    let mut command = cl65();
    command
        .args(TARGET_ARGS)
        .args(["-C".as_ref(), config.as_os_str()])
        .args(["-o".as_ref(), image.as_os_str()]);
    for source in extra {
        command.arg(source);
    }
    for unit in units {
        command.arg(unit);
    }
    run_ok(&mut command, "cl65 building a K816 program");
    std::fs::read(&image).expect("read the driver image")
}

// ---------------------------------------------------------------------------
// I1 — the driver recognises the extensions and drives the tools
// ---------------------------------------------------------------------------

/// `cl65 -S` leaves exactly the assembly `k816 compile` writes.
///
/// This is what "generated assembly remains inspectable" means at the driver:
/// not that some assembly appears, but that it is the compiler's own bytes,
/// so anything a reader learns from `k816 compile` output holds for a `cl65`
/// build.
#[test]
fn dash_capital_s_leaves_the_compilers_own_assembly() {
    let directory = work_dir("driver-dash-s");
    let source = root().join("examples/fullstack/main.k816");
    let through_driver = directory.join("driver.s");
    let by_hand = directory.join("manual.s");

    run_ok(
        cl65()
            .arg("-S")
            .args(["-o".as_ref(), through_driver.as_os_str()])
            .arg(&source),
        "cl65 -S",
    );
    run_ok(
        Command::new(env!("CARGO_BIN_EXE_k816"))
            .arg("compile")
            .arg(&source)
            .args(["-o".as_ref(), by_hand.as_os_str()]),
        "k816 compile",
    );

    assert_eq!(
        std::fs::read_to_string(&through_driver).expect("read the driver's assembly"),
        std::fs::read_to_string(&by_hand).expect("read the compiler's assembly"),
        "`cl65 -S` and `k816 compile` disagree about the generated assembly"
    );
}

/// `-c` keeps the object and removes the intermediate, as it does for C.
///
/// The `.s` cl65 hands ca65 is a temporary with a PID in its name; under `-c`
/// the object it produces is the product and the assembly goes. That is the
/// other half of decision 10 — `-S` above is the half where the assembly is
/// the product — and together they are why a K816 file behaves like a C file
/// in a Makefile that already knows cl65.
#[test]
fn dash_c_keeps_the_object_and_removes_the_intermediate() {
    let directory = work_dir("driver-dash-c");
    let object = directory.join("slice.o");
    let _ = std::fs::remove_file(&object);

    run_ok(
        cl65()
            .arg("-c")
            .args(["-o".as_ref(), object.as_os_str()])
            .arg(root().join("tests/golden/slice.k816")),
        "cl65 -c",
    );
    assert!(object.exists(), "cl65 -c produced no object file");

    let leftovers: Vec<String> = std::fs::read_dir(&directory)
        .expect("read the scratch directory")
        .map(|entry| {
            entry
                .expect("read an entry")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .filter(|name| name.ends_with(".s"))
        .collect();
    assert!(
        leftovers.is_empty(),
        "the intermediate assembly was left behind: {leftovers:?}"
    );
}

/// A K816 file produces a dependency file, as a C or assembly file does.
///
/// This is what a generator-driven build needs and the one thing a
/// transpile-then-assemble pipeline forgets: Ninja treats a declared depfile
/// that was not written as a hard error, so a `.k816` that produced none
/// would fail the build rather than merely rebuild too often. cc65 writes a C
/// file's dependencies and ca65 an assembly file's; k816 writes a K816 file's,
/// because the units it imports and the files a `binary` includes are known
/// to nobody else.
///
/// The rule's target is the *object*, not the intermediate assembly `cl65`
/// removes — the same choice `Compile()` makes for C, and the reason
/// `CompileK816` passes `--dep-target`.
#[test]
fn a_k816_source_produces_a_dependency_file_like_any_other() {
    let directory = work_dir("driver-create-dep");
    let golden = root().join("tests/golden");

    // The shape CMake generates: one source per invocation, nothing naming
    // the other units, and a depfile the generator reads. That is the whole
    // integration now — the source says which units it belongs with, so
    // there is no sibling list for a generator to compute.
    let dep = directory.join("unit-a.d");
    let object = directory.join("unit-a.o");
    for path in [&dep, &object] {
        let _ = std::fs::remove_file(path);
    }
    run_ok(
        cl65()
            .arg("-c")
            .args(["--create-dep".as_ref(), dep.as_os_str()])
            .args(["-o".as_ref(), object.as_os_str()])
            .arg(golden.join("unit-a.k816")),
        "cl65 -c with a dependency file",
    );

    let text = std::fs::read_to_string(&dep).expect("cl65 wrote no dependency file");
    let (target, deps) = text
        .split_once(":\t")
        .expect("`target:<tab>deps`, as cc65 writes it");
    assert_eq!(
        target,
        object.to_string_lossy(),
        "the rule names the intermediate rather than the object"
    );
    for unit in ["unit-a.k816", "unit-b.k816"] {
        assert!(deps.contains(unit), "{unit} is not a dependency:\n{text}");
    }

    // Without `-o`, the object is named after the source and so is the rule.
    // That is the branch a Makefile with a suffix rule takes, and it is a
    // different line of `CompileK816` from the one above.
    let staged = directory.join("staged");
    std::fs::create_dir_all(&staged).expect("create the staging directory");
    for unit in ["unit-a.k816", "unit-b.k816"] {
        std::fs::copy(golden.join(unit), staged.join(unit)).expect("stage a unit");
    }
    let implied = staged.join("implied.d");
    run_ok(
        cl65()
            .current_dir(&staged)
            .arg("-c")
            .args(["--create-dep".as_ref(), implied.as_os_str()])
            .args(["unit-a.k816", "unit-b.k816"]),
        "cl65 -c with no output name",
    );
    let implied_text = std::fs::read_to_string(&implied).expect("no dependency file");
    assert!(
        implied_text.starts_with("unit-b.o:\t"),
        "with no -o the rule should name the source's own object:\n{implied_text}"
    );
    assert!(staged.join("unit-a.o").exists() && staged.join("unit-b.o").exists());

    // And the same invocation over a hand-written module, so the two are
    // held to one shape rather than to two descriptions of one.
    let asm_dep = directory.join("startup.d");
    let _ = std::fs::remove_file(&asm_dep);
    run_ok(
        cl65()
            .arg("-c")
            .args(["--create-dep".as_ref(), asm_dep.as_os_str()])
            .args(["-o".as_ref(), directory.join("startup.o").as_os_str()])
            .arg(root().join("examples/fullstack/startup.asm")),
        "cl65 -c on a hand-written module",
    );
    let asm_text = std::fs::read_to_string(&asm_dep).expect("ca65 wrote no dependency file");
    assert!(
        asm_text.contains(":\t") && asm_text.contains("startup.asm"),
        "the assembly path's dependency file has a different shape:\n{asm_text}"
    );
}

/// An option value that happens to be K816-named is just an option value.
///
/// The pre-scan sub-plan I added had a documented caveat: it could not know
/// which options take a separate value, so `-o out.k65` after the source was
/// counted as a unit and the build failed on a file nobody meant. Sub-plan K
/// deleted the scan along with `--unit`, and this is the case that used to
/// fail, kept because a caveat that has stopped costing anything is worth a
/// test rather than a sentence.
#[test]
fn an_output_named_like_a_unit_is_still_an_output() {
    let directory = work_dir("driver-prescan");
    let source = root().join("tests/golden/slice.k816");

    // Before the source, which always worked.
    let before = directory.join("before.k65");
    let _ = std::fs::remove_file(&before);
    run_ok(
        cl65()
            .arg("-S")
            .args(["-o".as_ref(), before.as_os_str()])
            .arg(&source),
        "cl65 -S with a K816-named output before the source",
    );
    assert!(before.exists(), "the output was not written");

    // After it, which used to be "cannot read after.k65" because the scan
    // took the value for a unit. It is an ordinary run now. Where the
    // assembly lands is cl65's own left-to-right rule and the same for every
    // language it drives, so the claim here is only that the build succeeds.
    let after = directory.join("after.k65");
    let _ = std::fs::remove_file(&after);
    run_ok(
        cl65()
            .arg("-S")
            .arg(&source)
            .args(["-o".as_ref(), after.as_os_str()]),
        "cl65 -S with a K816-named output after the source",
    );
}

/// Two K816 files that import each other link to the manual path's image.
///
/// `cl65` is handed both and passes the compiler one file at a time with
/// nothing about the other; the units resolve each other because they say so
/// in their own text. `unit-a.k816` calls `helper` and reads `CURSOR` and
/// `BANKED`, all of which `unit-b.k816` defines, and the addressing mode of
/// each reference depends on the sibling's declaration — so an image that
/// links at all is already evidence, and one that matches the manual path
/// byte for byte is the claim.
#[test]
fn two_units_that_import_each_other_link_to_the_manual_image() {
    let directory = work_dir("driver-pair");
    let golden = root().join("tests/golden");
    let units = vec![golden.join("unit-a.k816"), golden.join("unit-b.k816")];
    let config = root().join("tests/driver.cfg");

    let by_hand = build_by_hand(&directory, &units, &config, &[]);
    let through_driver = build_through_driver(&directory, &units, &config, &[]);
    assert_eq!(
        through_driver, by_hand,
        "the driver's image differs from the manual path's"
    );
    assert!(!by_hand.is_empty(), "the pair linked to nothing");
}

/// `.k65`, `.k816` and `.kk` are one language and one compiler.
///
/// The extension policy (`plans/feature-manifest.md` §5) is that every
/// spelling is accepted by the same parser with no per-extension dialect,
/// and `cl65` maps all of them onto one file type for that reason. The two
/// units are the pair above, renamed to the other two extensions: the image
/// is the same bytes, which is what "no dialect switch" means when a build
/// states it.
#[test]
fn the_extensions_mix_on_one_command_line_and_mean_the_same_thing() {
    let directory = work_dir("driver-mixed-extensions");
    let golden = root().join("tests/golden");
    let config = root().join("tests/driver.cfg");

    // The rename is the point, so the copies are the fixture. Nothing is
    // added under `tests/golden/`: this is a scratch copy of a program that
    // is already in the corpus.
    let renamed = vec![directory.join("mixed-a.k65"), directory.join("mixed-b.kk")];
    for (source, destination) in ["unit-a.k816", "unit-b.k816"].iter().zip(&renamed) {
        // Rewritten rather than copied, because the units name each other in
        // their own text now: a renamed unit's `import` has to be renamed
        // with it, which is a property of stating a program in its sources
        // and not of the extension under test.
        let text = std::fs::read_to_string(golden.join(source))
            .expect("read a unit")
            .replace("unit-a.k816", "mixed-a.k65")
            .replace("unit-b.k816", "mixed-b.kk");
        std::fs::write(destination, text).expect("stage a renamed unit");
    }

    let by_hand = build_by_hand(&directory, &renamed, &config, &[]);
    let through_driver = build_through_driver(&directory, &renamed, &config, &[]);
    assert_eq!(
        through_driver, by_hand,
        "the driver's image differs from the manual path's"
    );

    // …and the same bytes the pair produces under its own names, because an
    // extension is not a dialect.
    let original = work_dir("driver-mixed-extensions-original");
    let units = vec![golden.join("unit-a.k816"), golden.join("unit-b.k816")];
    assert_eq!(
        through_driver,
        build_through_driver(&original, &units, &config, &[]),
        "renaming the units to .k65 and .kk changed the program"
    );
}

/// A `.kk` source takes both languages a `.k816` does.
///
/// `kk-k65.kk` is portable K65 and `kk-native.kk` is the same program in
/// K816's native form, importing `kk-lib.k816` for its variable. `cl65` has
/// to recognize the extension to build either, `--strict-k65` has to accept
/// the first and refuse the second, and the two images have to be the same
/// bytes - which is what "the extension is not a dialect" means here.
#[test]
fn a_kk_source_takes_k65_and_full_k816_alike() {
    let golden = root().join("tests/golden");
    let config = root().join("tests/driver.cfg");

    let k65 = golden.join("kk-k65.kk");
    let (stderr, ok) = compile_through_driver("driver-kk-strict", &k65, &["-Wk", "--strict-k65"]);
    assert!(ok, "the K65 half is not portable K65:\n{stderr}");
    let native = golden.join("kk-native.kk");
    let (stderr, ok) =
        compile_through_driver("driver-kk-strict", &native, &["-Wk", "--strict-k65"]);
    assert!(!ok, "`--strict-k65` accepted the native half");
    assert!(
        stderr.contains("K816-E0160"),
        "no extension was refused:\n{stderr}"
    );

    let directory = work_dir("driver-kk-k65");
    let from_k65 = build_through_driver(&directory, &[k65], &config, &[]);
    let directory = work_dir("driver-kk-native");
    let units = vec![native, golden.join("kk-lib.k816")];
    let from_native = build_through_driver(&directory, &units, &config, &[]);
    assert!(!from_k65.is_empty(), "the K65 half linked to nothing");
    assert_eq!(
        from_native, from_k65,
        "the two spellings of one program differ"
    );
}

/// A `binary` entry survives the driver's intermediate landing elsewhere.
///
/// K816 emits `.incbin` with the path the source wrote and checked it against
/// the source's own directory; ca65 resolves the same name against its own
/// search list, and the generated assembly is in a scratch directory. Passing
/// `--bin-include-dir <dir of the source>` is therefore the driver's job
/// (`plans/06-compatibility.md` decision 8), and this is the case that fails
/// without it — in the assembler, not the compiler.
#[test]
fn a_binary_inclusion_assembles_from_a_scratch_output_directory() {
    let directory = work_dir("driver-binary-inclusion");
    let fixture = root().join("tests/compat/k65/binary-inclusion");
    let units = vec![fixture.join("input.k65")];
    let config = root().join("tests/compat/atarixl.cfg");

    let through_driver = build_through_driver(&directory, &units, &config, &[]);
    let by_hand = build_by_hand(&directory, &units, &config, &[]);
    assert_eq!(
        through_driver, by_hand,
        "the driver's image differs from the manual path's"
    );

    // The included bytes really are in there — otherwise both sides could be
    // equally empty and the identity would prove nothing.
    let table = std::fs::read(fixture.join("table.bin")).expect("read table.bin");
    assert!(
        through_driver
            .windows(table.len())
            .any(|window| window == table),
        "the linked image does not contain the included file's bytes"
    );
}

// ---------------------------------------------------------------------------
// I1 — options reach the compiler through -Wk, proven by their effect
// ---------------------------------------------------------------------------
//
// Each of these is asserted on the *outcome* rather than on `cl65 -v`'s
// "Executing:" line: a gate on the intermediate passes when the argument is
// forwarded to a compiler that ignores it (`memory/subplan-h-implemented`).
//
// They also prove, together, that `compile` precedes every forwarded option.
// k816 dispatches the subcommand before it parses anything, so an option that
// arrived first would end the run with "unknown option" rather than with the
// diagnostic each case names.

/// An entry point that declares a sixteen-bit accumulator, which the default
/// environment (`native,a8,i8`) does not provide.
const WIDE_MAIN: &str = "func main @a16 {\n    return\n}\n";

/// A program that widens the accumulator itself and then needs the width.
/// Legal in native mode, impossible in emulation mode.
const WIDENING_MAIN: &str = "func main {\n    rep #$20\n    a = #$0102\n    return\n}\n";

/// Ordinary K816 that the original K65 language does not have: a `segment`
/// directive, a width annotation, and a `$` literal.
const EXTENSIONS: &str = "segment CODE\n\nfunc main @a8 @i8 {\n    a = #$01\n    return\n}\n";

/// Writes `text` into a case-specific scratch directory and returns its path.
fn scratch_source(test: &str, name: &str, text: &str) -> PathBuf {
    let path = work_dir(test).join(name);
    std::fs::write(&path, text).expect("write a scratch source");
    path
}

/// Runs `cl65 -S` over `source` with `arguments` in front of it, and returns
/// the run's stderr and whether it succeeded.
fn compile_through_driver(test: &str, source: &Path, arguments: &[&str]) -> (String, bool) {
    let output = run(
        cl65()
            .arg("-S")
            .args(["-o".as_ref(), work_dir(test).join("out.s").as_os_str()])
            .args(arguments)
            .arg(source),
        "cl65 -S",
    );
    (
        String::from_utf8_lossy(&output.stderr).into_owned(),
        output.status.success(),
    )
}

/// `-Wk --entry=emulation` reaches the compiler and changes what it accepts.
///
/// The program widens the accumulator with an explicit `rep` and then loads a
/// sixteen-bit immediate. That is legal on the default machine — native mode,
/// where a `rep` does something — and impossible in emulation mode, where
/// both registers are eight bits wide and nothing in the body can widen them.
/// So the same source compiles through `cl65` with no options and is refused
/// with the flag, which is the forwarded value arriving and being believed.
#[test]
fn a_forwarded_entry_environment_changes_what_the_compiler_accepts() {
    let source = scratch_source("driver-entry-emulation", "widen.k816", WIDENING_MAIN);

    let (stderr, ok) = compile_through_driver("driver-entry-emulation", &source, &[]);
    assert!(
        ok,
        "the default machine is native, where the `rep` widens:\n{stderr}"
    );

    let (stderr, ok) = compile_through_driver(
        "driver-entry-emulation",
        &source,
        &["-Wk", "--entry=emulation"],
    );
    assert!(!ok, "`--entry=emulation` did not reach the compiler");
    assert!(
        stderr.contains("K816-E0103"),
        "the run failed for some other reason:\n{stderr}"
    );
}

/// A comma-bearing value arrives as one argument.
///
/// This is the reason `-Wk` is `CmdAddArg` and not `CmdAddArgList`: the other
/// pass-throughs split on commas, and `native,a16,i16` split three ways would
/// reach k816 as three arguments and end the run with "more than one input
/// file". So the assertion is on the *contract outcome* — `@a16` accepted
/// where the default refuses `@i16` code — and on the absence of that error.
#[test]
fn a_comma_bearing_option_value_is_one_argument() {
    let source = scratch_source("driver-entry-axes", "wide.k816", WIDE_MAIN);

    // Without the flag the same source is refused: `main @a16` claims a width
    // the default environment does not provide. So a successful run is the
    // forwarded value and nothing else.
    let (stderr, ok) = compile_through_driver("driver-entry-axes", &source, &[]);
    assert!(
        !ok,
        "the default environment should refuse `@a16`:\n{stderr}"
    );

    let (stderr, ok) = compile_through_driver(
        "driver-entry-axes",
        &source,
        &["-Wk", "--entry=native,a16,i16"],
    );
    assert!(
        ok,
        "`--entry=native,a16,i16` did not arrive as one argument:\n{stderr}"
    );
    assert!(
        !stderr.contains("more than one input file"),
        "the value was split at its commas:\n{stderr}"
    );

    // The same value in two argv elements, which is the spelling the driver
    // documents as the alternative.
    let (stderr, ok) = compile_through_driver(
        "driver-entry-axes",
        &source,
        &["-Wk", "--entry", "-Wk", "native,a16,i16"],
    );
    assert!(ok, "two -Wk did not make two arguments:\n{stderr}");
}

/// `-Wk --strict-k65` reaches the compiler and reports the extension rows.
#[test]
fn a_forwarded_reporting_mode_reaches_the_compiler() {
    let source = scratch_source("driver-strict", "extensions.k816", EXTENSIONS);

    let (stderr, ok) = compile_through_driver("driver-strict", &source, &[]);
    assert!(ok, "the program should compile without the flag:\n{stderr}");
    assert!(
        !stderr.contains("K816-K65-"),
        "the flag is on without being asked for:\n{stderr}"
    );

    let (stderr, ok) = compile_through_driver("driver-strict", &source, &["-Wk", "--strict-k65"]);
    assert!(!ok, "`--strict-k65` did not reach the compiler");
    assert!(
        stderr.contains("K816-K65-"),
        "no compatibility row was reported:\n{stderr}"
    );
}

/// `-Wk --message-format=json` puts the compiler's JSON on cl65's stderr.
///
/// cl65 does not pipe its children's output anywhere, so the format the
/// compiler was asked for is the format the build produces — which is what
/// makes a `cl65` build usable from an editor or a CI parser.
#[test]
fn a_forwarded_message_format_reaches_cl65s_own_stderr() {
    let source = scratch_source(
        "driver-json",
        "broken.k816",
        "func main @a8 @i8 {\n    SCREEN = a\n    return\n}\n",
    );

    let (stderr, ok) =
        compile_through_driver("driver-json", &source, &["-Wk", "--message-format=json"]);
    assert!(
        !ok,
        "the program has an undeclared name and should not compile"
    );
    let first = stderr.lines().next().unwrap_or_default();
    assert!(
        first.starts_with('{') && first.contains("\"code\""),
        "stderr does not start with a JSON diagnostic:\n{stderr}"
    );
}

/// A refused option value ends the `cl65` run with the compiler's words.
///
/// `cl65` adds no diagnostic of its own for a K816 file: `ExecProgram` exits
/// with the child's status, so what a user sees is what `k816` printed.
#[test]
fn a_refused_option_ends_the_run_with_the_compilers_words() {
    let source = scratch_source("driver-refused-entry", "main.k816", WIDE_MAIN);

    let (stderr, ok) = compile_through_driver(
        "driver-refused-entry",
        &source,
        &["-Wk", "--entry=emulation,a16"],
    );
    assert!(
        !ok,
        "`emulation,a16` names no machine and should be refused"
    );
    assert!(
        stderr.contains("k816:") && stderr.contains("emulation"),
        "the refusal is not the compiler's:\n{stderr}"
    );
}

/// A K816 diagnostic ends the `cl65` run non-zero, with nothing added.
#[test]
fn a_failed_compilation_ends_the_driver_run() {
    let source = scratch_source(
        "driver-failing-compile",
        "broken.k816",
        "func main @a8 @i8 {\n    SCREEN = a\n    return\n}\n",
    );

    let (stderr, ok) = compile_through_driver("driver-failing-compile", &source, &[]);
    assert!(!ok, "an undeclared name should end the run");
    assert!(
        stderr.contains("K816-E") && stderr.contains("SCREEN"),
        "the driver did not pass the compiler's diagnostic through:\n{stderr}"
    );
    assert!(
        !stderr.contains("cl65:"),
        "the driver added a message of its own:\n{stderr}"
    );
}

// ---------------------------------------------------------------------------
// I2 — the whole corpus, and the compat images, through the driver
// ---------------------------------------------------------------------------

/// The driver adds nothing to, and loses nothing from, the manual path.
///
/// This is sub-plan I's whole correctness claim, over every program in the
/// tree rather than over an example of one. *Adds nothing*: no `-t` reaches
/// k816, no argument is reordered, no segment moves. *Loses nothing*: no unit
/// is dropped, no `--bin-include-dir` is missing, no object is left out of
/// the link. Either would show up here as different bytes, because the two
/// sides differ in nothing else — same compiler, same assembler, same linker,
/// same configuration, same order.
#[test]
fn every_program_in_the_tree_links_to_the_same_bytes_either_way() {
    let mut built = 0;
    let mut skipped = 0;
    for program in programs() {
        let directory = work_dir(&format!("driver-corpus/{}", stem(&program.name)));
        if !compiles(&directory, &program) {
            skipped += 1;
            continue;
        }
        let extra: Vec<&Path> = program.extra.iter().map(PathBuf::as_path).collect();
        // The units as a build would see them: a K65 fixture that names no
        // siblings is staged with the `import` lines a K816 port of it would
        // carry, because a program has to say what it is either way.
        let units = corpus::stated_units(&directory, &program.units);
        let by_hand = build_by_hand(&directory, &units, &program.config, &extra);
        let through_driver = build_through_driver(&directory, &units, &program.config, &extra);
        assert_eq!(
            through_driver, by_hand,
            "{}: the driver's image is not the manual path's.\n\
             The driver owns no emitted byte: if these differ, cl65 is wrong.",
            program.name
        );
        built += 1;
    }
    assert!(
        built > 60,
        "only {built} programs were built through the driver ({skipped} skipped) - \
         the corpus enumeration or the compile probe is broken"
    );
}

/// Reads a committed `.hex` golden: `origin AAAA`, then hex bytes per line
/// behind an address column, `#` comments ignored.
///
/// The same format `tests/compat.rs` writes and reads. Only the bytes are
/// needed here — the origin is the configuration's, and both sides use the
/// fixture's own configuration.
fn read_hex(path: &Path) -> Vec<u8> {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("cannot read {}: {error}", path.display()));
    let mut bytes = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') || line.starts_with("origin ") {
            continue;
        }
        for field in line.split_whitespace().skip(1) {
            bytes.push(
                u8::from_str_radix(field, 16)
                    .unwrap_or_else(|_| panic!("{}: `{field}` is not hex", path.display())),
            );
        }
    }
    bytes
}

/// The image with its trailing fill removed, as `tests/compat.rs` trims it.
fn without_fill(mut bytes: Vec<u8>) -> Vec<u8> {
    while bytes.last() == Some(&0) {
        bytes.pop();
    }
    bytes
}

/// Every compat fixture that commits an image reproduces it through `cl65`.
///
/// `tests/compat.rs` builds those images from the *library* API and asserts
/// them against the golden; this builds them from the driver. The two chains
/// share only the golden, so a fixture that agreed with itself and not with
/// the committed bytes would be caught in one of them — and the manual recipe
/// in `tests/compat.rs` stays where it is, because an oracle that runs the
/// thing under test is not an oracle.
#[test]
fn every_committed_compat_image_is_reproduced_through_the_driver() {
    let mut compared = 0;
    for program in programs() {
        let Some(rest) = program.name.strip_prefix("tests/compat/") else {
            continue;
        };
        let fixture = program.units[0].parent().expect("a fixture directory");
        let golden = fixture.join("expected.hex");
        if !golden.exists() {
            continue;
        }
        let name = rest.rsplit_once('/').map_or(rest, |(dir, _)| dir);
        let directory = work_dir(&format!("driver-compat/{}", stem(name)));
        if !compiles(&directory, &program) {
            continue;
        }

        let image = without_fill(build_through_driver(
            &directory,
            &corpus::stated_units(&directory, &program.units),
            &program.config,
            &[],
        ));
        assert_eq!(
            image,
            read_hex(&golden),
            "{name}: the image cl65 builds is not the one committed beside the \
             fixture"
        );
        compared += 1;
    }
    assert!(
        compared > 20,
        "only {compared} committed images were compared - the fixture walk is broken"
    );
}

// ---------------------------------------------------------------------------
// I2 — the examples, built the way their build files build them
// ---------------------------------------------------------------------------

/// Copies a directory tree into `destination`, which is created.
fn copy_tree(source: &Path, destination: &Path) {
    std::fs::create_dir_all(destination).expect("create a staging directory");
    for entry in std::fs::read_dir(source).expect("read an example directory") {
        let entry = entry.expect("read an example entry");
        let target = destination.join(entry.file_name());
        if entry.path().is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), &target).expect("stage an example file");
        }
    }
}

/// Stages an example under `work_dir` and runs its build file there.
///
/// The build file is what a user runs, so it is what is gated — not a command
/// line the test spells out beside it, which is the arrangement that lets the
/// two drift. It runs against a *copy* so that a build never writes a product
/// into the source tree.
fn make(test: &str, example: &str, subdirectory: &str) -> (PathBuf, Output) {
    let staged = work_dir(test).join("example");
    let _ = std::fs::remove_dir_all(&staged);
    copy_tree(&root().join(example), &staged);
    let directory = staged.join(subdirectory);
    let output = run(
        Command::new("make")
            .arg("-C")
            .arg(&directory)
            .arg(format!("CL65={}", toolchain().join("cl65").display())),
        "make",
    );
    (directory, output)
}

/// Where `main` ended up, from a second link of the same staged example.
///
/// A probe rather than a build: the same units and the same configuration,
/// with `-Ln` asking ld65 for a VICE label file, because the run address in
/// the executable is only interesting if it is the address of `main`. The
/// example's own build file has no reason to ask for labels.
fn linked_address_of_main(directory: &Path) -> u16 {
    let labels = directory.join("labels.txt");
    run_ok(
        cl65()
            .current_dir(directory)
            .args(TARGET_ARGS)
            .args(["-C", "../atarixl.cfg"])
            .args(["-Ln".as_ref(), labels.as_os_str()])
            .args(["-o".as_ref(), directory.join("probe.xex").as_os_str()])
            .args(["main.k65", "../include/defs.k65"]),
        "cl65 linking the example again for its labels",
    );
    let text = std::fs::read_to_string(&labels).expect("read the label file");
    // `al 00201C .main`
    let address = text
        .lines()
        .find_map(|line| {
            let mut fields = line.split_whitespace();
            let address = fields.nth(1)?;
            (fields.next()? == ".main").then_some(address)
        })
        .unwrap_or_else(|| panic!("no `main` in the label file:\n{text}"));
    u32::from_str_radix(address, 16).expect("a hex address") as u16
}

/// `examples/fullstack` builds through its one `cl65` rule.
///
/// The four hand-written rules this replaced — `k816 compile`, two `ca65`, an
/// `ld65` — are what `cl65` runs, and `tests/fullstack.rs` is what says the
/// image is right. What this adds is that the *Makefile* still works.
#[test]
fn the_fullstack_example_builds_through_its_makefile() {
    let (directory, output) = make("driver-example-fullstack", "examples/fullstack", ".");
    assert!(
        output.status.success(),
        "examples/fullstack did not build:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let image = std::fs::read(directory.join("fullstack.bin")).expect("read fullstack.bin");
    assert_eq!(image.len(), 0x8000, "the ROM image is not 32 KiB");
    // The reset vector, which is the one byte pair that says the link put the
    // startup module where the machine looks for it.
    assert_eq!(
        &image[0x7FFC..0x7FFE],
        &[0x00, 0x80],
        "the reset vector moved"
    );
}

/// The Atari XL examples link into loadable executables — and one does not.
///
/// This closes sub-plan F's handoff (`postmortem.md` §F.3.2): the four
/// examples had no link step at all, because each `makefile` called the
/// *original* K65 with a project file, and the corpus gate could only compare
/// instruction streams. They link now, against a configuration of their own,
/// and `main` is the run address without a startup stub — ld65's `atari`
/// format writes the RUNAD segment the loader reads.
///
/// `starfield` is the fourth, and it does not build: its `for … eval [ rnd()
/// & 255 ]` tables ask for different bytes on every build, which K816 refuses
/// by decision (`K816-K65-0002`, sub-plan E decision 18). That is a product
/// boundary rather than a gap, so it is asserted rather than skipped — a
/// refusal nobody checks is a refusal that can quietly become something else.
#[test]
fn the_atari_examples_link_into_loadable_executables() {
    for name in ["dli-example", "landscape", "rainbow"] {
        let (directory, output) = make(
            &format!("driver-example-atari-{name}"),
            "examples/k65/atari-xl",
            name,
        );
        assert!(
            output.status.success(),
            "{name} did not build:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );

        let xex = std::fs::read(directory.join("main.xex")).expect("read main.xex");
        // $FFFF, then the load chunk's first and last address, then the
        // bytes, then a RUNAD chunk: $02E0, $02E1, and the run address.
        assert_eq!(
            &xex[..2],
            &[0xFF, 0xFF],
            "{name}: no Atari executable header"
        );
        let start = u16::from_le_bytes([xex[2], xex[3]]);
        let end = u16::from_le_bytes([xex[4], xex[5]]);
        assert_eq!(start, 0x2000, "{name}: the load address is not $2000");
        assert_eq!(
            usize::from(end - start) + 1,
            xex.len() - 6 - 6,
            "{name}: the load chunk's length does not match the file"
        );

        let trailer = &xex[xex.len() - 6..];
        assert_eq!(
            &trailer[..4],
            &[0xE0, 0x02, 0xE1, 0x02],
            "{name}: the file does not end with a RUNAD chunk"
        );
        assert_eq!(
            u16::from_le_bytes([trailer[4], trailer[5]]),
            linked_address_of_main(&directory),
            "{name}: the run address is not `main`. The original's linker \
             prepended a stub to arrange this; the configuration's `runad` is \
             what replaces it, and it names a symbol rather than an address."
        );
    }

    let (_, output) = make(
        "driver-example-atari-starfield",
        "examples/k65/atari-xl",
        "starfield",
    );
    assert!(!output.status.success(), "starfield built, and it must not");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        stderr.matches("K816-K65-0002").count(),
        2,
        "starfield's two refused `rnd()` calls are not what stopped it:\n{stderr}"
    );
}

/// One `cl65` command line with a `.k816`, a `.c` and a `.s` on it links.
///
/// The claim sub-plan K exists for, and the one no earlier sub-plan could
/// make: a K816 program that *meets another language*. `main.k816` calls a
/// function cc65 compiled and one ca65 assembled, and writes a variable the C
/// file declares — none of which any K816 unit defines, and all of which ld65
/// resolves because the `extern` declarations said what to ask it for.
///
/// The address size is what carries: `_count` is imported absolute and
/// `zpslot` zeropage, so the two stores are a three-byte and a two-byte
/// instruction. Getting either wrong would be an assembly error rather than
/// a wrong byte, which is the property ca65's validation buys.
#[test]
fn a_k816_unit_calls_a_c_function_and_a_ca65_module_through_extern() {
    let directory = work_dir("driver-ffi");
    let config = root().join("tests/driver.cfg");

    let k816_source = directory.join("main.k816");
    std::fs::write(
        &k816_source,
        "// The K816 side of a three-language program.  Nothing here is\n\
         // defined by any K816 unit.\n\
         extern func _blink\n\
         extern func puts_asm\n\
         extern var _count\n\
         extern var dp zpslot\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   call _blink\n\
         \x20   a = #$07\n\
         \x20   _count = a\n\
         \x20   zpslot = a\n\
         \x20   call puts_asm\n\
         \x20   rts\n\
         }\n",
    )
    .expect("write the K816 unit");

    let c_source = directory.join("blink.c");
    std::fs::write(
        &c_source,
        "unsigned char count;\nvoid blink(void) { ++count; }\n",
    )
    .expect("write the C unit");

    // cc65's mangling is a leading underscore, which is the whole of the
    // convention a K816 source has to know to name a C symbol.
    let asm_source = directory.join("puts.s");
    std::fs::write(
        &asm_source,
        ".setcpu \"65816\"\n\
         .export puts_asm\n\
         .exportzp zpslot\n\
         .segment \"ZEROPAGE\": zeropage\n\
         zpslot: .res 1\n\
         .segment \"CODE\"\n\
         puts_asm:\n\
         \x20       nop\n\
         \x20       rts\n",
    )
    .expect("write the assembly unit");

    let image = directory.join("ffi.bin");
    run_ok(
        cl65()
            .args(TARGET_ARGS)
            .args(["-C".as_ref(), config.as_os_str()])
            // cc65's output opens with `.macpack longbranch`, which ca65
            // finds through its *assembler* include path — a staged tool has
            // none of its own, since it is not where it was installed.
            .args([
                "--asm-include-dir".as_ref(),
                root().join("../../asminc").as_os_str(),
            ])
            .args(["-o".as_ref(), image.as_os_str()])
            .arg(&k816_source)
            .arg(&c_source)
            .arg(&asm_source),
        "cl65 linking K816, C and assembly together",
    );
    assert!(
        std::fs::metadata(&image).expect("the linked image").len() > 0,
        "the three objects link"
    );

    // And the addressing modes the declarations bought.
    let assembly = directory.join("main.s");
    run_ok(
        cl65()
            .arg("-S")
            .args(["-o".as_ref(), assembly.as_os_str()])
            .arg(&k816_source),
        "cl65 -S on the K816 unit",
    );
    let text = std::fs::read_to_string(&assembly).expect("read the assembly");
    assert!(text.contains("sta     a:_count"), "{text}");
    assert!(text.contains("sta     z:zpslot"), "{text}");
    assert!(text.contains(".importzp zpslot"), "{text}");
}
