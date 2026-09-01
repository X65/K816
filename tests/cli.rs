/*****************************************************************************/
/*                                                                           */
/*                                   cli.rs                                  */
/*                                                                           */
/*        CLI diagnostic formats: human, compact, json, non-TTY ASCII        */
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

//! `--message-format` behavior of the `k816` CLI, and the non-TTY fallback:
//! piped stderr gets plain ASCII without ANSI colors.

use std::path::PathBuf;
use std::process::Command;

/// The slice without its declaration: exactly one semantic error.
// `@a8 @i8` restates the entry ABI, so the undeclared `SCREEN` is the only
// thing wrong with this file — which is what every test below reads.
const BROKEN: &str = "func main @a8 @i8 {\n    SCREEN = a\n    return\n}\n";
/// Missing `}`: a syntax error carrying a machine-applicable fix.
const OPEN_BRACE: &str = "func main @a8 {\n    return\n";

/// Runs `k816 compile` on `text` with `args`, returning (stderr, success).
fn compile(test: &str, text: &str, args: &[&str]) -> (String, bool) {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(test);
    std::fs::create_dir_all(&dir).expect("create work dir");
    let source = dir.join("input.k816");
    std::fs::write(&source, text).expect("write source");

    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .args(args)
        .arg(&source)
        .args(["-o".as_ref(), dir.join("out.s").as_os_str()])
        .output()
        .expect("run k816");
    (
        String::from_utf8(output.stderr).expect("stderr is UTF-8"),
        output.status.success(),
    )
}

#[test]
fn piped_stderr_is_plain_ascii_without_colors() {
    // The test harness pipes stderr, so this exercises the non-TTY path.
    let (stderr, success) = compile("cli-human", BROKEN, &[]);
    assert!(!success);
    assert!(stderr.contains("[K816-E0101] Error:"));
    assert!(!stderr.contains('\u{1b}'), "ANSI escape in: {stderr}");
    assert!(
        stderr.contains(",-[") && stderr.contains("`----"),
        "expected ASCII box drawing in: {stderr}"
    );
    assert!(
        !stderr.contains('╭') && !stderr.contains('│'),
        "Unicode box drawing leaked into non-TTY output: {stderr}"
    );
}

#[test]
fn compact_format_is_denser_than_human() {
    let (human, _) = compile("cli-human-lines", BROKEN, &[]);
    let (compact, success) = compile("cli-compact", BROKEN, &["--message-format=compact"]);
    assert!(!success);
    assert!(compact.contains("[K816-E0101] Error:"));
    assert!(
        compact.lines().count() < human.lines().count(),
        "compact ({}) not denser than human ({})",
        compact.lines().count(),
        human.lines().count()
    );
}

#[test]
fn json_format_emits_one_parseable_object_per_diagnostic() {
    let (stderr, success) = compile("cli-json", OPEN_BRACE, &["--message-format=json"]);
    assert!(!success);

    let diagnostics: Vec<serde_json::Value> = stderr
        .lines()
        .map(|line| serde_json::from_str(line).expect("each line is a JSON object"))
        .collect();
    assert_eq!(diagnostics.len(), 1);

    let diagnostic = &diagnostics[0];
    assert_eq!(diagnostic["code"], "K816-E0002");
    assert_eq!(diagnostic["severity"], "error");
    assert_eq!(diagnostic["title"], "missing `}`");
    assert_eq!(diagnostic["primary"]["line"], 2);
    assert_eq!(diagnostic["primary"]["column"], 11);
    assert_eq!(diagnostic["primary"]["start"], diagnostic["primary"]["end"]);

    let fix = &diagnostic["fixes"][0];
    assert_eq!(fix["applicability"], "machine-applicable");
    assert_eq!(fix["edits"][0]["replacement"], "}");
}

#[test]
fn json_spans_carry_both_ends_as_line_and_column() {
    let (stderr, _) = compile("cli-json-span", BROKEN, &["--message-format=json"]);
    let diagnostic: serde_json::Value =
        serde_json::from_str(stderr.lines().next().expect("a diagnostic")).expect("JSON object");

    let span = &diagnostic["primary"];
    assert_eq!(span["line"], 2);
    assert_eq!(span["column"], 5);
    assert_eq!(span["end_line"], 2);
    // `SCREEN` is six bytes wide, so the end column trails the start by six.
    assert_eq!(span["end_column"], 11);
}

#[test]
fn json_mode_keeps_tool_errors_on_the_json_stream() {
    // A consumer parses every stderr line; a plain-text `cannot read` would
    // break it. Missing input files are the common case.
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-json-tool-error");
    std::fs::create_dir_all(&dir).expect("create work dir");
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .args(["compile", "--message-format=json"])
        .arg(dir.join("does-not-exist.k816"))
        .output()
        .expect("run k816");
    assert!(!output.status.success());

    let stderr = String::from_utf8(output.stderr).expect("stderr is UTF-8");
    let error: serde_json::Value =
        serde_json::from_str(stderr.lines().next().expect("an error line")).expect("JSON object");
    assert_eq!(error["severity"], "error");
    assert!(
        error["message"]
            .as_str()
            .expect("message is a string")
            .contains("cannot read"),
        "unexpected message in: {stderr}"
    );
}

#[test]
fn both_flag_spellings_and_bad_values_are_handled() {
    // `--message-format json` (space-separated) works like the `=` form.
    let (stderr, _) = compile("cli-json-space", BROKEN, &["--message-format", "json"]);
    assert!(serde_json::from_str::<serde_json::Value>(stderr.lines().next().unwrap()).is_ok());

    let (stderr, success) = compile("cli-bad-format", BROKEN, &["--message-format=yaml"]);
    assert!(!success);
    assert!(stderr.contains("unknown message format `yaml`"));
}

/// K65 has no import syntax, so naming the sibling units is how a workspace
/// is described until sub-plan I gives the driver a project surface.
#[test]
fn the_unit_flag_makes_a_sibling_units_names_resolve() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-unit-flag");
    std::fs::create_dir_all(&dir).expect("create work dir");
    let a = dir.join("a.k816");
    let b = dir.join("b.k816");
    let out = dir.join("a.s");
    std::fs::write(&a, "func main @a8 {\n    jsr helper\n}\n").expect("write a");
    std::fs::write(&b, "func helper @a8 {\n    nop\n}\n").expect("write b");

    let k816 = |args: &[&std::ffi::OsStr]| {
        Command::new(env!("CARGO_BIN_EXE_k816"))
            .arg("compile")
            .args(args)
            .args(["-o".as_ref(), out.as_os_str()])
            .output()
            .expect("run k816")
    };

    // Without the sibling, `helper` is simply not declared: K816 does not
    // invent an import to make an unresolved name someone else's problem.
    let alone = k816(&[a.as_os_str()]);
    assert!(!alone.status.success());
    let stderr = String::from_utf8_lossy(&alone.stderr).into_owned();
    assert!(stderr.contains("K816-E0101"), "{stderr}");

    // With it, the reference resolves and the linkage is derived.
    let together = k816(&["--unit".as_ref(), b.as_os_str(), a.as_os_str()]);
    assert!(
        together.status.success(),
        "{}",
        String::from_utf8_lossy(&together.stderr)
    );
    let assembly = std::fs::read_to_string(&out).expect("read output");
    assert!(assembly.contains(".import helper "), "{assembly}");
    assert!(assembly.contains(".export main "), "{assembly}");
}

/// The `--unit` surface carries width contracts across the boundary too, not
/// just names.
///
/// A callee in a sibling unit that leaves the accumulator 8-bit has to reach
/// the caller's *encoding*: without the sibling the reference does not
/// resolve at all, and with it the immediate after the call is the two-byte
/// form rather than the three-byte one the caller's own `@a16` would imply.
#[test]
fn the_unit_flag_carries_width_contracts_across_the_boundary() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-unit-contract");
    std::fs::create_dir_all(&dir).expect("create work dir");
    let caller = dir.join("caller.k816");
    let callee = dir.join("callee.k816");
    let out = dir.join("caller.s");
    std::fs::write(
        &caller,
        "func caller @a16 {\n    call narrows\n    lda #$01\n}\n",
    )
    .expect("write caller");
    std::fs::write(&callee, "func narrows @a16 -> @a8 {\n    sep #$20\n}\n").expect("write callee");

    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .args(["--unit".as_ref(), callee.as_os_str()])
        .args(["-o".as_ref(), out.as_os_str()])
        .arg(&caller)
        .output()
        .expect("run k816");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let assembly = std::fs::read_to_string(&out).expect("read output");
    let call = assembly.find("jsr").expect("the call");
    let load = assembly.find("lda").expect("the load");
    assert!(
        assembly[call..load].contains("        .a8"),
        "the sibling's exit contract is claimed before the load: {assembly}"
    );
}

// ---------------------------------------------------------------------------
// `--strict-k65` (sub-plan G, task G10b)
// ---------------------------------------------------------------------------

/// A program that is *ordinary* K816 and not K65 at all: `segment`, a width
/// annotation, a `$` literal, and a call contract.
const EXTENSIONS: &str = "segment CODE\n\nfunc go @a8 @i8 (a) -> a {\n\
                          \x20   a + #$01\n}\n";

/// A program the original K65 compiler would accept as written.
const PORTABLE: &str = "const MASK = 0x0F\n\nmain {\n\
                        \x20   a + MASK\n    return\n}\n";

/// The flag is off by default, on with `--strict-k65`, and on for `emit-ir`
/// too — one reporting mode across both commands, which is what "no separate
/// compiler architecture" means in practice.
#[test]
fn strict_k65_is_opt_in_and_reaches_both_commands() {
    let (quiet, success) = compile("cli-strict-off", EXTENSIONS, &[]);
    assert!(success, "the extensions compile without the flag: {quiet}");
    assert!(!quiet.contains("K816-E0160"), "{quiet}");

    let (strict, success) = compile("cli-strict-on", EXTENSIONS, &["--strict-k65"]);
    assert!(
        !success,
        "an extension under the flag is an error: {strict}"
    );
    assert!(strict.contains("K816-E0160"), "{strict}");
    assert!(
        strict.contains("`segment` is not in the original K65 language"),
        "the message names the construct: {strict}"
    );

    // `emit-ir` takes it too. The IR still reaches stdout only when the unit
    // compiles, so the check is on the diagnostic rather than on the dump.
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-strict-ir");
    std::fs::create_dir_all(&dir).expect("create work dir");
    let source = dir.join("input.k816");
    std::fs::write(&source, EXTENSIONS).expect("write source");
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .args(["emit-ir", "--strict-k65"])
        .arg(&source)
        .output()
        .expect("run k816");
    let stderr = String::from_utf8(output.stderr).expect("stderr is UTF-8");
    assert!(stderr.contains("K816-E0160"), "{stderr}");
}

/// A K65 program stays silent under the flag, which is the half that makes it
/// useful: a mode that complains about everything tells nobody anything.
#[test]
fn strict_k65_says_nothing_about_a_k65_program() {
    let (stderr, success) = compile("cli-strict-portable", PORTABLE, &["--strict-k65"]);
    assert!(success, "{stderr}");
    assert_eq!(stderr.trim(), "", "{stderr}");
}

// ---------------------------------------------------------------------------
// The entry environment (sub-plan G, task G11)
// ---------------------------------------------------------------------------

/// A program whose entry point states nothing and loads a sixteen-bit
/// immediate: legal under `--entry a16`, out of range under the default.
const WIDE_ENTRY: &str = "func main {\n    a = #$0102\n    return\n}\n";

/// Every spelling of `--entry`, and what each one does to the same source.
///
/// The axes are optional and ordered, so most of these differ only in what
/// they leave out; the point of listing them is that leaving an axis out has
/// to mean *the default*, not *unknown*, or a source that states nothing
/// would stop compiling the moment any axis was named.
#[test]
fn the_entry_flag_takes_each_axis_and_defaults_the_rest() {
    for spelling in [
        "a16",
        "native,a16",
        "a16,i8",
        "native,a16,i16",
        "--entry=a16",
    ] {
        let (stderr, success) = if let Some(joined) = spelling.strip_prefix("--entry=") {
            compile(
                "cli-entry-joined",
                WIDE_ENTRY,
                &[&format!("--entry={joined}")],
            )
        } else {
            compile("cli-entry-wide", WIDE_ENTRY, &["--entry", spelling])
        };
        assert!(success, "`--entry {spelling}`: {stderr}");
    }

    // The default is unchanged by the flag existing, and it is the one that
    // refuses this immediate.
    let (stderr, success) = compile("cli-entry-default", WIDE_ENTRY, &[]);
    assert!(!success, "{stderr}");
    assert!(stderr.contains("K816-E0103"), "{stderr}");
    let (stderr, success) = compile("cli-entry-native", WIDE_ENTRY, &["--entry", "native,a8,i8"]);
    assert!(!success, "{stderr}");
    assert!(stderr.contains("K816-E0103"), "{stderr}");
}

/// `emulation` states a machine, and the machine decides the widths.
///
/// Both registers are eight bits wide in emulation mode and nothing in the
/// body can widen them, so this is the one axis that is not merely a
/// preference: a `rep` is a no-op and the immediate that needed it is
/// refused.
#[test]
fn an_emulation_entry_pins_both_widths_at_eight() {
    let source = "func main {\n    rep #$20\n    a = #$0102\n    return\n}\n";
    let (stderr, success) = compile("cli-entry-emu-native", source, &["--entry", "native"]);
    assert!(success, "the `rep` widens in native mode: {stderr}");

    let (stderr, success) = compile("cli-entry-emu", source, &["--entry", "emulation"]);
    assert!(!success, "{stderr}");
    assert!(stderr.contains("K816-E0103"), "{stderr}");

    // And an annotation that claims otherwise is the ordinary ABI mismatch,
    // not a special case of the flag.
    let claim = "func main @a16 {\n    a = #$0102\n    return\n}\n";
    let (stderr, success) = compile("cli-entry-emu-claim", claim, &["--entry", "emulation"]);
    assert!(!success, "{stderr}");
    assert!(stderr.contains("K816-E0143"), "{stderr}");
}

/// The refusals, each naming what is wrong with the spelling.
///
/// `emulation,a16` is the one decision 21 singles out: it is a command-line
/// error rather than a silent correction, because a build that believed it
/// asked for sixteen bits and got eight would find out in the bytes.
#[test]
fn the_entry_flag_refuses_a_spelling_that_names_no_machine() {
    for (spelling, expected) in [
        ("emulation,a16", "forces an 8-bit accumulator"),
        ("emulation,i16", "forces an 8-bit index register"),
        ("a8,native", "comes after the accumulator axis"),
        ("a8,a16", "states the accumulator axis twice"),
        ("native,,a8", "an axis is missing between the commas"),
        ("a32", "unknown axis `a32`"),
        ("", "no axis named"),
    ] {
        let (stderr, success) = compile("cli-entry-bad", WIDE_ENTRY, &["--entry", spelling]);
        assert!(!success, "`--entry {spelling}` was accepted: {stderr}");
        assert!(
            stderr.contains("K816-E0162") && stderr.contains(expected),
            "`--entry {spelling}`: {stderr}"
        );
    }

    // A missing value is a missing argument, not a malformed one. Invoked
    // directly, because the helper appends the source path and `--entry`
    // would eat it — the same as every other flag that takes a value.
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .args(["compile", "--entry"])
        .output()
        .expect("run k816");
    let stderr = String::from_utf8(output.stderr).expect("stderr is UTF-8");
    assert!(!output.status.success(), "{stderr}");
    assert!(stderr.contains("`--entry` needs a value"), "{stderr}");
}

/// The refusal is a diagnostic on the JSON stream too, code and all.
///
/// `K816-E0162` has no span, which is why it is a tool error and not a
/// `Diagnostic`; a consumer parsing the stream still gets an object with the
/// same `severity`, `code` and `message` keys the real ones carry.
#[test]
fn the_entry_refusal_carries_its_code_in_json_mode() {
    let (stderr, success) = compile(
        "cli-entry-json",
        WIDE_ENTRY,
        &["--message-format", "json", "--entry", "emulation,a16"],
    );
    assert!(!success, "{stderr}");
    let object: serde_json::Value =
        serde_json::from_str(stderr.trim()).unwrap_or_else(|e| panic!("{e}: {stderr}"));
    assert_eq!(object["severity"], "error");
    assert_eq!(object["code"], "K816-E0162");
    assert!(
        object["message"]
            .as_str()
            .expect("a message")
            .contains("`--entry`"),
        "{object}"
    );
}

// ---------------------------------------------------------------------------
// The explanation surface (sub-plan F, task F8)
// ---------------------------------------------------------------------------

/// Runs `k816 emit-ir` on `text` with `args`, returning (stdout, success).
fn emit_ir(test: &str, text: &str, args: &[&str]) -> (String, bool) {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(test);
    std::fs::create_dir_all(&dir).expect("create work dir");
    let source = dir.join("input.k816");
    std::fs::write(&source, text).expect("write source");

    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("emit-ir")
        .args(args)
        .arg(&source)
        .output()
        .expect("run k816");
    (
        String::from_utf8(output.stdout).expect("stdout is UTF-8"),
        output.status.success(),
    )
}

/// A dead load, so there is exactly one thing for the optimizer to explain.
const DEAD_LOAD: &str = "var SCREEN = $0400\n\nfunc main @a8 @i8 {\n\
                         \x20   a = #$00\n    a = #$01\n    SCREEN = a\n}\n";

/// `--explain` answers "where did my instruction go", which is why K816 ships
/// no diagnostic for a correct optimization: a warning on every removal would
/// be noise on every build.
#[test]
fn explain_names_the_pass_the_site_and_the_justification() {
    let (stdout, success) = emit_ir("cli-explain", DEAD_LOAD, &["--after-opt", "--explain"]);
    assert!(success);

    // The dump comes first and is unchanged by the flag.
    let (dump, explanation) = stdout
        .split_once("optimizer:")
        .unwrap_or_else(|| panic!("no explanation in: {stdout}"));
    assert!(dump.contains("lda #$01"), "{dump}");
    assert!(!dump.contains("lda #$00"), "the dead load is gone: {dump}");

    assert!(
        explanation.contains("remove-overwritten"),
        "the pass is named: {explanation}"
    );
    assert!(
        explanation.contains("main+"),
        "the site is named: {explanation}"
    );
    assert!(
        explanation.contains("lda #$00"),
        "the operation is named: {explanation}"
    );
    assert!(
        explanation.contains("writes A N Z, all overwritten by `lda #$01` before any read"),
        "the justification is the one the pass recorded: {explanation}"
    );
}

/// The flag says nothing when there is nothing to say, and says so.
#[test]
fn explain_is_honest_about_an_optimizer_that_did_not_run() {
    // Nothing to remove: the log is empty and the surface says so rather than
    // printing a heading with no rows.
    let quiet = "var SCREEN = $0400\n\nfunc main @a8 @i8 {\n    a = #$01\n    SCREEN = a\n}\n";
    let (stdout, success) = emit_ir("cli-explain-quiet", quiet, &["--after-opt", "--explain"]);
    assert!(success);
    assert!(stdout.contains("optimizer: no changes"), "{stdout}");

    // And without `--after-opt` there is no optimizer run at all.
    let (stdout, success) = emit_ir("cli-explain-unopt", DEAD_LOAD, &["--explain"]);
    assert!(success);
    assert!(
        stdout.contains("lda #$00"),
        "the dump is unoptimized: {stdout}"
    );
    assert!(
        stdout.contains("optimizer: not run (add --after-opt)"),
        "{stdout}"
    );
}

/// The three commands phase 9 asks for keep working, unchanged.
#[test]
fn the_inspection_commands_still_answer_what_they_answered() {
    let (before, success) = emit_ir("cli-ir-before", DEAD_LOAD, &[]);
    assert!(success);
    assert!(before.contains("lda #$00") && before.contains("lda #$01"));

    let (after, success) = emit_ir("cli-ir-after", DEAD_LOAD, &["--after-opt"]);
    assert!(success);
    assert!(!after.contains("lda #$00") && after.contains("lda #$01"));
    assert!(
        !after.contains("optimizer:"),
        "no explanation unasked: {after}"
    );

    let (stderr, success) = compile("cli-no-opt", DEAD_LOAD, &["--no-opt"]);
    assert!(success, "{stderr}");
}

// ---------------------------------------------------------------------------
// The language server's command line (sub-plan H, task H0)
// ---------------------------------------------------------------------------

/// Runs `k816 lsp` with `args` and no protocol traffic, returning
/// (stderr, success).
///
/// A refused flag never reaches the transport, so the server exits before
/// reading stdin; a flag it accepts leaves it waiting, which is why every
/// case here is one it refuses.
fn lsp_refusal(args: &[&str]) -> (String, bool) {
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("lsp")
        .args(args)
        .output()
        .expect("run k816");
    (
        String::from_utf8(output.stderr).expect("stderr is UTF-8"),
        output.status.success(),
    )
}

/// `lsp` reads `--entry` through the same arm `compile` does, so a spelling
/// that names no machine is refused with the same code and the same words.
#[test]
fn the_lsp_refuses_an_entry_spelling_the_same_way_compile_does() {
    let (compile_stderr, compile_ok) = compile("cli-lsp-entry", WIDE_ENTRY, &["--entry", "a32"]);
    let (lsp_stderr, lsp_ok) = lsp_refusal(&["--entry", "a32"]);
    assert!(!compile_ok && !lsp_ok, "{compile_stderr}{lsp_stderr}");
    assert_eq!(compile_stderr, lsp_stderr);
    assert!(lsp_stderr.contains("K816-E0162"), "{lsp_stderr}");

    let (stderr, success) = lsp_refusal(&["--entry"]);
    assert!(!success, "{stderr}");
    assert!(stderr.contains("`--entry` needs a value"), "{stderr}");
}

/// The options that describe one compilation are not the server's to take:
/// its documents arrive over the protocol, and naming a unit on its command
/// line would be a second way to describe the program (decision 2).
#[test]
fn the_lsp_takes_no_input_file_and_no_compilation_options() {
    let (stderr, success) = lsp_refusal(&["some.k816"]);
    assert!(!success, "{stderr}");
    assert!(stderr.contains("`lsp` takes no input file"), "{stderr}");

    for flag in [
        vec!["--unit", "other.k816"],
        vec!["--unit=other.k816"],
        vec!["-o", "out.s"],
        vec!["--no-opt"],
        vec!["--after-opt"],
    ] {
        let (stderr, success) = lsp_refusal(&flag);
        assert!(!success, "`lsp {}` was accepted: {stderr}", flag.join(" "));
        assert!(
            stderr.contains(&format!("unknown option `{}`", flag[0])),
            "`lsp {}`: {stderr}",
            flag.join(" ")
        );
    }
}

/// The usage text names the flags the server takes, so a user who reaches
/// for `k816.server.args` can find out what to put there.
#[test]
fn the_usage_text_names_the_servers_flags() {
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .output()
        .expect("run k816");
    let stderr = String::from_utf8(output.stderr).expect("stderr is UTF-8");
    assert!(!output.status.success());
    assert!(
        stderr.contains("lsp [--strict-k65] [--entry AXES]"),
        "{stderr}"
    );
}
