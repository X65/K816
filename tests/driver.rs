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
use std::sync::OnceLock;

use common::{run_ok, tool, work_dir};

#[path = "common/corpus.rs"]
mod corpus;

/// The tools, gathered so that `cl65` finds *this* build's `k816`.
///
/// `cl65` prefixes each tool it runs with its own directory (`CmdInit`), so a
/// `cl65` run from the cc65 tree's `bin/` runs the `k816` installed there —
/// which is whatever `make k816` last copied, not what Cargo just built. A
/// gate that compared an installed compiler's output with the one under test
/// would fail for a reason that has nothing to do with the driver, so the
/// four tools are copied into one scratch directory and `cl65` is run from
/// there. Copied rather than symlinked because a copy is what every platform
/// has.
fn toolchain() -> &'static Path {
    static TOOLCHAIN: OnceLock<PathBuf> = OnceLock::new();
    TOOLCHAIN
        .get_or_init(|| {
            let dir = work_dir("driver-toolchain");
            for (source, name) in [
                (PathBuf::from(env!("CARGO_BIN_EXE_k816")), "k816"),
                (tool("CA65", "ca65"), "ca65"),
                (tool("LD65", "ld65"), "ld65"),
                (tool("CL65", "cl65"), "cl65"),
            ] {
                let destination = dir.join(name);
                let _ = std::fs::remove_file(&destination);
                std::fs::copy(&source, &destination).unwrap_or_else(|error| {
                    panic!(
                        "cannot stage {} (build it with `make -C ../.. bin`): {error}",
                        source.display()
                    )
                });
            }
            dir
        })
        .as_path()
}

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
/// with the others as `--unit` siblings, each assembly assembled, the objects
/// linked against `config`. It is written out once more here rather than
/// shared with those suites because an oracle that runs the thing under test
/// is not an oracle.
fn build_by_hand(directory: &Path, units: &[PathBuf], config: &Path, extra: &[&Path]) -> Vec<u8> {
    let mut objects: Vec<PathBuf> = Vec::new();
    for (index, unit) in units.iter().enumerate() {
        let assembly = directory.join(format!("manual-{index}.s"));
        let object = directory.join(format!("manual-{index}.o"));
        let mut compile = Command::new(env!("CARGO_BIN_EXE_k816"));
        compile.arg("compile").arg(unit);
        for sibling in units.iter().filter(|sibling| *sibling != unit) {
            compile.args(["--unit".as_ref(), sibling.as_os_str()]);
        }
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

/// Two K816 files on one command line are two units of one program.
///
/// K65 has no import syntax, so this is the whole mechanism by which a name
/// one unit defines resolves in another: `cl65` passes each K816 file to the
/// compiler with the others as `--unit` siblings. `unit-a.k816` calls
/// `helper` and reads `CURSOR` and `BANKED`, all of which `unit-b.k816`
/// defines, and the addressing mode of each reference depends on the
/// sibling's declaration — so an image that links at all is already evidence,
/// and one that matches the manual path byte for byte is the claim.
#[test]
fn two_units_on_one_command_line_resolve_each_other() {
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

/// `.k65` and `.k816` are one language and one compiler.
///
/// The extension policy (`plans/feature-manifest.md` §5) is that both
/// spellings are accepted by the same parser with no per-extension dialect,
/// and `cl65` maps both onto one file type for that reason. The two units are
/// the pair above, one of them renamed: the image is the same bytes, which is
/// what "no dialect switch" means when a build states it.
#[test]
fn the_two_extensions_mix_on_one_command_line_and_mean_the_same_thing() {
    let directory = work_dir("driver-mixed-extensions");
    let golden = root().join("tests/golden");
    let config = root().join("tests/driver.cfg");

    // The rename is the point, so the copies are the fixture. Nothing is
    // added under `tests/golden/`: this is a scratch copy of a program that
    // is already in the corpus.
    let renamed = vec![
        directory.join("mixed-a.k65"),
        directory.join("mixed-b.k816"),
    ];
    for (source, destination) in ["unit-a.k816", "unit-b.k816"].iter().zip(&renamed) {
        std::fs::copy(golden.join(source), destination).expect("stage a renamed unit");
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
        "renaming a unit from .k816 to .k65 changed the program"
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
