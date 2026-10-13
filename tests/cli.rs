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

use std::path::{Path, PathBuf};
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

/// `--unit` is not an option of any command.
///
/// Sub-plan K removed it: a program is stated in its sources, and a flag
/// that also stated one would be a second place for a build system to get it
/// wrong. The check is that the spelling is *rejected* rather than quietly
/// ignored, which is what an unknown option does everywhere else.
#[test]
fn unit_is_not_an_option_of_any_command() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-no-unit-flag");
    std::fs::create_dir_all(&dir).expect("create work dir");
    let a = dir.join("a.k816");
    std::fs::write(&a, "func main @a8 {\n    nop\n}\n").expect("write a");

    for command in ["compile", "emit-ir"] {
        for flag in [
            vec!["--unit".to_owned(), "b.k816".to_owned()],
            vec!["--unit=b.k816".to_owned()],
        ] {
            let output = Command::new(env!("CARGO_BIN_EXE_k816"))
                .arg(command)
                .args(&flag)
                .arg(&a)
                .output()
                .expect("run k816");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(!output.status.success(), "{command} {flag:?}: {stderr}");
            assert!(
                stderr.contains("unknown option"),
                "{command} {flag:?}: {stderr}"
            );
        }
    }
}

/// An `import` carries width contracts across the boundary too, not just
/// names.
///
/// A callee in another unit that leaves the accumulator 8-bit has to reach
/// the caller's *encoding*: without the line the reference does not resolve
/// at all, and with it the immediate after the call is the two-byte form
/// rather than the three-byte one the caller's own `@a16` would imply.
#[test]
fn an_import_carries_width_contracts_across_the_boundary() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-unit-contract");
    std::fs::create_dir_all(&dir).expect("create work dir");
    let caller = dir.join("caller.k816");
    let callee = dir.join("callee.k816");
    let out = dir.join("caller.s");
    std::fs::write(
        &caller,
        "import \"callee.k816\"\n\nfunc caller @a16 {\n    call narrows\n    lda #$01\n}\n",
    )
    .expect("write caller");
    std::fs::write(&callee, "func narrows @a16 -> @a8 {\n    sep #$20\n}\n").expect("write callee");

    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
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
/// its documents arrive over the protocol, and which units they belong with
/// arrives in their own text. `--unit` is here among the refusals because it
/// is an option no command takes any more, and a spelling that means nothing
/// should say so rather than be ignored.
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

// ---------------------------------------------------------------------------
// The README, held to the binary it describes
// ---------------------------------------------------------------------------
//
// Nothing gated `README.md` until now, and it had rotted all the way through:
// its usage block advertised `link`, `init`, `build`, `run` and `clean`, a
// `-T` linker config and an `--output-format`, and its development section
// named a `k816-golden-tests` crate and a `K816_BLESS` variable. None of that
// has ever existed in this compiler — it is the legacy tool's command line,
// carried forward unread. Three of the six language examples were wrong too.
//
// So the README is now tested the way `doc/` is: the usage block *is* the
// binary's own output, and every example in it compiles.

/// The README section, by heading, from `##` to the next `##`.
fn readme_section(heading: &str) -> String {
    let readme = std::fs::read_to_string(readme_path()).expect("read README.md");
    let start = readme
        .find(&format!("\n## {heading}\n"))
        .unwrap_or_else(|| panic!("README.md has no `## {heading}` section"));
    let rest = &readme[start + 1..];
    let end = rest[3..]
        .find("\n## ")
        .map_or(rest.len(), |offset| offset + 4);
    rest[..end].to_owned()
}

fn readme_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("README.md")
}

/// The bodies of the fenced blocks in `text` whose info string is `language`.
fn fenced_blocks(text: &str, language: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let mut lines = text.lines();
    while let Some(line) = lines.next() {
        if line.trim_end() != format!("```{language}") {
            continue;
        }
        let mut body = String::new();
        for line in lines.by_ref() {
            if line.trim_end() == "```" {
                break;
            }
            body.push_str(line);
            body.push('\n');
        }
        blocks.push(body);
    }
    blocks
}

/// The Zed settings the README shows are settings the server takes.
///
/// A snippet that names a key the server does not read, or a value it
/// refuses, is copied into an editor and silently does nothing. And a
/// `binary.path` without `["lsp"]` beside it starts the compiler instead of
/// the server: Zed passes a configured binary its configured arguments and
/// nothing else.
#[test]
fn the_readme_zed_settings_are_what_the_server_reads() {
    use k816::abi::EntryEnvironment;
    use k816::lsp::config;

    let section = readme_section("Editor support");
    let blocks = fenced_blocks(&section, "json");
    assert_eq!(blocks.len(), 1, "one Zed settings block in Editor support");
    let settings: serde_json::Value =
        serde_json::from_str(&blocks[0]).expect("the README's Zed settings are JSON");

    let server = &settings["lsp"]["k816"];
    let options = &server["initialization_options"];
    let entry = options[config::ENTRY]
        .as_str()
        .expect("the snippet states an entry");
    let strict = options[config::STRICT_K65]
        .as_bool()
        .expect("the snippet states strict-k65");

    // Start from the opposite of what the snippet says, so a key that was
    // ignored rather than applied cannot pass by matching a default.
    let mut applied = k816::Options {
        strict_k65: !strict,
        entry: EntryEnvironment::parse("native,a16,i16").expect("a spelling"),
        ..k816::Options::default()
    };
    if applied.entry == EntryEnvironment::parse(entry).expect("the snippet's entry parses") {
        applied.entry = EntryEnvironment::parse("native,a8,i8").expect("a spelling");
    }
    let messages = config::apply(&mut applied, options);
    assert_eq!(
        messages,
        Vec::new(),
        "the server has something to say about the snippet"
    );
    assert_eq!(applied.strict_k65, strict);
    assert_eq!(applied.entry, EntryEnvironment::parse(entry).unwrap());

    if let Some(binary) = server.get("binary") {
        assert_eq!(
            binary["arguments"],
            serde_json::json!(["lsp"]),
            "a configured binary is started with these arguments and no others"
        );
    }

    let language = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("editors/zed-k816/languages/K816/config.toml"),
    )
    .expect("read the Zed language config");
    let name = language
        .lines()
        .find_map(|line| line.strip_prefix("name = \""))
        .and_then(|rest| rest.strip_suffix('"'))
        .expect("the language config names its language");
    assert!(
        settings["languages"].get(name).is_some(),
        "the snippet's language settings are keyed by the language's name, `{name}`"
    );
}

/// The usage block in the README is the usage text the binary prints.
///
/// Not "describes the same commands" — the same bytes. A paraphrase is what
/// let the old block drift into documenting a different program, and a
/// paraphrase is the only thing this cannot tell apart from an edit.
#[test]
fn the_readme_usage_block_is_the_binarys_own_usage() {
    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .output()
        .expect("run k816");
    let printed = String::from_utf8(output.stderr).expect("stderr is UTF-8");

    let section = readme_section("CLI Usage");
    let blocks = fenced_blocks(&section, "text");
    assert_eq!(
        blocks.len(),
        1,
        "`## CLI Usage` should hold exactly one ```text block, the usage text"
    );
    assert_eq!(
        blocks[0].trim_end(),
        printed.trim_end(),
        "README.md's usage block is not what `k816` prints. Paste the current \
         output of `cargo run --quiet --` into the ```text block under \
         `## CLI Usage`."
    );
}

/// Every K65 example in the README compiles.
///
/// Each block is a whole compilation unit, which is what makes this checkable
/// at all — a reader who pastes one into a file gets what the README showed
/// them. It is also the only thing that would have caught the three that were
/// wrong: an `inline` whose body sized an index immediate with no index
/// contract to size it by, an `a = a + 1` the grammar has never had, and a
/// comment promising a `REP #$30` the optimizer is right to leave out.
#[test]
fn every_readme_example_compiles() {
    let readme = std::fs::read_to_string(readme_path()).expect("read README.md");
    let examples = fenced_blocks(&readme, "k65");
    assert!(
        examples.len() >= 6,
        "expected the README's language examples, found {}",
        examples.len()
    );

    for (index, example) in examples.iter().enumerate() {
        let (stderr, success) = compile(&format!("readme-{index}"), example, &[]);
        assert!(
            success,
            "README example {} does not compile:\n\n{example}\n{stderr}",
            index + 1
        );
    }
}

/// `--create-dep` reports what the compilation read, in make's shape.
///
/// A build system needs three things a `.s` cannot tell it: the source, the
/// units it imports, and every file a `binary` includes.
/// The last is the one nothing else could supply — the emitted `.incbin`
/// carries the path *as the source wrote it*, and where that resolves to is
/// K816's answer, not the assembler's.
#[test]
fn create_dep_names_the_source_its_units_and_its_binaries() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-create-dep");
    std::fs::create_dir_all(&dir).expect("create the work dir");
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture = root.join("tests/compat/k65/binary-inclusion");
    let dep = dir.join("out.d");

    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .arg(fixture.join("input.k65"))
        .args(["--create-dep".as_ref(), dep.as_os_str()])
        .args(["-o".as_ref(), dir.join("out.s").as_os_str()])
        .output()
        .expect("run k816");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let text = std::fs::read_to_string(&dep).expect("read the dependency file");
    let (rule, phony) = text
        .trim_end()
        .split_once("\n\n")
        .expect("a rule, a blank line, and a phony rule");
    let (target, deps) = rule.split_once(":\t").expect("`target:<tab>deps`");

    assert_eq!(
        target,
        dir.join("out.s").to_string_lossy(),
        "the target is -o"
    );
    let deps: Vec<&str> = deps.split(' ').collect();
    assert!(
        deps.contains(&fixture.join("input.k65").to_string_lossy().as_ref()),
        "the source is not a dependency: {deps:?}"
    );
    assert!(
        deps.contains(&fixture.join("table.bin").to_string_lossy().as_ref()),
        "the included binary is not a dependency: {deps:?}"
    );

    // The phony half, which is what keeps `make` going when a file named
    // here is deleted rather than edited — the same rule `gcc -MP` writes.
    assert_eq!(
        phony.trim_end(),
        format!("{}:", deps.join(" ")),
        "the phony rule does not name every dependency"
    );
}

/// `--dep-target` names something other than `-o`, and the units are listed.
///
/// `cl65` passes the *object* here, because the intermediate assembly it
/// names with `-o` is removed after assembly and nothing tracks it.
#[test]
fn dep_target_renames_the_rule_and_units_are_dependencies() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-dep-target");
    std::fs::create_dir_all(&dir).expect("create the work dir");
    let golden = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let dep = dir.join("pair.d");

    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .arg(golden.join("unit-a.k816"))
        .args(["--create-dep".as_ref(), dep.as_os_str()])
        .args(["--dep-target", "pair.o"])
        .args(["-o".as_ref(), dir.join("pair.s").as_os_str()])
        .output()
        .expect("run k816");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let text = std::fs::read_to_string(&dep).expect("read the dependency file");
    assert!(text.starts_with("pair.o:\t"), "{text}");
    assert!(text.contains("unit-a.k816"), "{text}");
    assert!(
        text.contains("unit-b.k816"),
        "an imported unit is a dependency - editing it changes this object, \
         and nothing on the command line named it:\n{text}"
    );
}

/// A compilation that fails writes no dependency file.
///
/// One that outlived a failed run would tell the next build the target is
/// current, which is the one way a dependency file can do harm.
#[test]
fn a_failed_compilation_writes_no_dependency_file() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-dep-failure");
    std::fs::create_dir_all(&dir).expect("create the work dir");
    let source = dir.join("broken.k816");
    std::fs::write(&source, BROKEN).expect("write the source");
    let dep = dir.join("broken.d");
    let _ = std::fs::remove_file(&dep);

    let output = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .arg(&source)
        .args(["--create-dep".as_ref(), dep.as_os_str()])
        .args(["-o".as_ref(), dir.join("broken.s").as_os_str()])
        .output()
        .expect("run k816");
    assert!(!output.status.success());
    assert!(!dep.exists(), "a failed compilation left a dependency file");
}

// ---------------------------------------------------------------------------
// Two names that must not come back
// ---------------------------------------------------------------------------
//
// Sub-plan I removed an acceptance gate named after a project outside this
// repository (decision 4) and let the legacy compiler's history go (decision
// 5). Both removals are the kind that a paragraph pasted from an old document
// silently undoes, so both are swept for rather than merely done.
//
// Each needle is assembled from pieces at run time. A sweep whose only hit is
// its own source is a sweep that can never fail, and writing the literal here
// would be exactly that.

/// Everything this repository writes, for the sweep that must find no hit
/// anywhere. `plans/` is here because it is prose about the compiler too.
const SWEPT_ROOTS: &[&str] = &[
    "README.md",
    "AGENTS.md",
    "doc",
    "plans",
    "src",
    "tests",
    "examples",
];

/// Every file under `roots`, in a stable order.
///
/// `target/` and `.git/` are never roots, so nothing generated or checked out
/// is walked; everything else under a root is, including fixtures, because the
/// point of a sweep is that *no* file carries the text.
fn swept_files(roots: &[&str]) -> Vec<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    let mut stack: Vec<PathBuf> = roots.iter().map(|name| root.join(name)).collect();
    while let Some(path) = stack.pop() {
        if path.is_dir() {
            let mut entries: Vec<PathBuf> = std::fs::read_dir(&path)
                .unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
                .map(|entry| entry.expect("read a directory entry").path())
                .collect();
            entries.sort();
            stack.extend(entries);
        } else if path.is_file() {
            files.push(path);
        }
    }
    files.sort();
    files
}

/// Case-insensitive byte search: is `needle` (ASCII) anywhere in `haystack`?
///
/// Files are read as bytes rather than as text because the roots hold
/// fixtures that are not UTF-8, and a sweep that skipped them would be a
/// sweep with a hole in it.
fn contains_ignore_ascii_case(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if needle.is_empty() || haystack.len() < needle.len() {
        return None;
    }
    (0..=haystack.len() - needle.len()).find(|&start| {
        haystack[start..start + needle.len()]
            .iter()
            .zip(needle)
            .all(|(a, b)| a.eq_ignore_ascii_case(b))
    })
}

/// Reports every (file, needle) pair a sweep found, with the line it is on.
///
/// `fold_case` is the difference between the two sweeps below: a project name
/// can be written in any case, while a command spelling cannot — `k816 build`
/// is a command and "a K816 build" is a sentence, and a case-folding sweep
/// cannot tell them apart.
fn sweep(
    roots: &[&str],
    needles: &[String],
    fold_case: bool,
    allow: &dyn Fn(&Path, &str) -> bool,
) -> Vec<String> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut hits = Vec::new();
    for path in swept_files(roots) {
        let bytes = match std::fs::read(&path) {
            Ok(bytes) => bytes,
            Err(error) => panic!("read {}: {error}", path.display()),
        };
        let relative = path.strip_prefix(&root).unwrap_or(&path);
        for needle in needles {
            if allow(relative, needle) {
                continue;
            }
            let found = if fold_case {
                contains_ignore_ascii_case(&bytes, needle.as_bytes())
            } else {
                bytes
                    .windows(needle.len())
                    .position(|window| window == needle.as_bytes())
            };
            let Some(offset) = found else {
                continue;
            };
            let line = bytes[..offset].iter().filter(|&&b| b == b'\n').count() + 1;
            hits.push(format!("{}:{line}: `{needle}`", relative.display()));
        }
    }
    hits
}

/// Nothing in the tree names the operating-system project the removed phase-12
/// acceptance gate was named after.
///
/// Sub-plan I decision 4 removed the gate — the acceptance is the
/// compatibility corpus and the examples built through `cl65` — and rewrote
/// the three places that named the project *descriptively* to "a program an
/// operating system loads", which is the phrasing `implementation.md`
/// decision 16 already used. The argument survives; the name does not.
#[test]
fn no_document_names_the_operating_system_project() {
    let stem = format!("{}{}", "o", "s");
    let needles: Vec<String> = ["/", "-"]
        .iter()
        .map(|separator| format!("{stem}{separator}816"))
        .collect();
    let hits = sweep(SWEPT_ROOTS, &needles, true, &|_, _| false);
    assert!(
        hits.is_empty(),
        "the removed acceptance gate's project is named again:\n  {}\n\
         Sub-plan I decision 4 took the name out of this repository. Say what \
         the sentence means instead — \"a program an operating system loads\" \
         is the phrasing `implementation.md` decision 16 uses.",
        hits.join("\n  ")
    );
}

/// Nothing a user reads describes the legacy compiler's vocabulary.
///
/// The legacy K816 was a different program with a different command line: it
/// had `build`, `link`, `init` and `metadata` subcommands, a `k816.toml`
/// project file, a RON link script, and an o65 object writer. None of that
/// exists in this compiler, and §H.15 is the record of what happens when a
/// document keeps saying otherwise — `README.md` advertised five commands
/// that have never existed here, carried forward unread until something
/// checked.
///
/// So this is the sweep that keeps a paragraph pasted from the legacy README
/// out next time. It is over what a *user* reads — the README, `AGENTS.md`,
/// `doc/` and the examples — and not over `plans/`, which is a history of
/// decisions and names the legacy tool on purpose.
#[test]
fn no_document_describes_the_legacy_tools_vocabulary() {
    // The command spellings, and the files that were the legacy build's
    // vocabulary. Matched case-sensitively: "a K816 build" is a sentence.
    let needles: Vec<String> = [
        "link.ron",
        ".ld.ron",
        "k816.toml",
        "k816 build",
        "k816 link",
        "k816 init",
        "k816 metadata",
        "o65 object",
    ]
    .iter()
    .map(|needle| (*needle).to_owned())
    .collect();

    // No allow-list entries. `doc/design.md` §13 describes the retirement in
    // terms of what was archived — "the custom O65 backend", "the custom
    // linker" — rather than in the legacy command line's words, so nothing a
    // user reads needs an exception. One added here has to say which sentence
    // needs it and why that sentence is history rather than instruction.
    let hits = sweep(
        &["README.md", "AGENTS.md", "doc", "examples"],
        &needles,
        false,
        &|_, _| false,
    );
    assert!(
        hits.is_empty(),
        "the legacy tool's vocabulary is back:\n  {}\n\
         This compiler has `compile`, `emit-ir`, `fmt` and `lsp`, and a build \
         is `cl65 -t none --no-target-lib -C layout.cfg`. If the sentence is \
         about history, say so and add it to this sweep's allow-list.",
        hits.join("\n  ")
    );
}

/// The README's `cl65` line is the line the example's Makefile runs.
///
/// Neither gate above reads a ```` ```bash ```` block: the usage block is
/// byte-compared against the binary and the ```` ```k65 ```` blocks are
/// compiled, and a shell command is neither. This section has exactly one
/// claim that could drift — the command a K816 build states — so it is
/// compared with the build file that runs it, which `tests/driver.rs` in turn
/// executes.
#[test]
fn the_readme_build_line_is_the_examples_own() {
    let section = readme_section("Building with cl65");
    let shown = fenced_blocks(&section, "bash")
        .into_iter()
        .find(|block| block.contains("fullstack.cfg"))
        .expect("`## Building with cl65` shows the full-stack build");

    let makefile = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/fullstack/Makefile"),
    )
    .expect("read the example's Makefile");
    let recipe = makefile
        .lines()
        .find(|line| line.contains("$(CL65)"))
        .expect("the Makefile has a cl65 recipe")
        .trim()
        .replace("$(CL65)", "cl65")
        .replace("$@", "fullstack.bin");

    assert_eq!(
        shown.trim(),
        recipe,
        "README.md's `## Building with cl65` shows a command the example does \
         not run. The README says it is the Makefile's line, so make it be one."
    );
}

/// A unit that `import`s another needs no flag naming it.
///
/// The whole point of the construct: the program is described in the source,
/// so a build system that compiles one file per invocation has nothing to
/// compute. Without the line the name is simply not declared — K816 still
/// does not invent an import to make an unresolved name someone else's
/// problem.
#[test]
fn an_import_loads_its_target_without_a_unit_flag() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-import-loads");
    std::fs::create_dir_all(&dir).expect("create work dir");
    let a = dir.join("a.k816");
    let out = dir.join("a.s");
    std::fs::write(dir.join("b.k816"), "func helper @a8 {\n    nop\n}\n").expect("write b");

    let compile = |text: &str| {
        std::fs::write(&a, text).expect("write a");
        Command::new(env!("CARGO_BIN_EXE_k816"))
            .arg("compile")
            .arg(&a)
            .args(["-o".as_ref(), out.as_os_str()])
            .output()
            .expect("run k816")
    };

    let alone = compile("func main @a8 {\n    jsr helper\n}\n");
    assert!(!alone.status.success());
    assert!(
        String::from_utf8_lossy(&alone.stderr).contains("K816-E0101"),
        "{}",
        String::from_utf8_lossy(&alone.stderr)
    );

    let named = compile("import \"b.k816\"\n\nfunc main @a8 {\n    jsr helper\n}\n");
    assert!(
        named.status.success(),
        "{}",
        String::from_utf8_lossy(&named.stderr)
    );
    let assembly = std::fs::read_to_string(&out).expect("read output");
    assert!(assembly.contains(".import helper "), "{assembly}");
}

/// An `import` inside a region the preprocessor drops is not a file this
/// build reads.
///
/// Which is why the loader preprocesses rather than scanning for the item.
/// The half that shows it is the *dependency file*: a path in a dead region
/// that happens to name a real file would otherwise be opened, listed as a
/// prerequisite, and rebuild a target that never read it. A path that names
/// nothing is the easy half and is silent either way, because a loader that
/// cannot read adds nothing and the compiler resolves its edges from the
/// preprocessed program regardless.
#[test]
fn an_import_inside_a_dead_region_is_not_loaded() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-import-dead");
    std::fs::create_dir_all(&dir).expect("create work dir");
    std::fs::write(dir.join("unused.k816"), "func unused @a8 {\n    nop\n}\n")
        .expect("write the file the dead region names");
    let a = dir.join("a.k816");
    std::fs::write(
        &a,
        "#if 0\n\
         import \"unused.k816\"\n\
         #endif\n\
         #if 1\n\
         import \"nowhere.k816\"\n\
         #endif\n\
         \n\
         func main @a8 {\n\
         \x20   nop\n\
         }\n",
    )
    .expect("write a");
    let dep = dir.join("a.d");
    // The work directory outlives the run, so a file left by the previous
    // one would make the assertion below true for the wrong reason.
    let _ = std::fs::remove_file(&dep);
    let result = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .arg(&a)
        .args(["--create-dep".as_ref(), dep.as_os_str()])
        .args(["-o".as_ref(), dir.join("a.s").as_os_str()])
        .output()
        .expect("run k816");

    // The *live* region's import names nothing, so the build is refused --
    // which is the other half of the same claim: a directive selects which
    // imports are real, in both directions.
    assert!(!result.status.success());
    let stderr = String::from_utf8_lossy(&result.stderr);
    assert!(stderr.contains("K816-E0166"), "{stderr}");
    assert!(
        !stderr.contains("unused.k816"),
        "the dead region's path was looked for: {stderr}"
    );
    assert!(
        !dep.exists(),
        "a failed compilation writes no dependency file"
    );

    // With the live one gone, the build succeeds and the dead region's file
    // is not a prerequisite of it.
    std::fs::write(
        &a,
        "#if 0\n\
         import \"unused.k816\"\n\
         #endif\n\
         \n\
         func main @a8 {\n\
         \x20   nop\n\
         }\n",
    )
    .expect("rewrite a");
    let result = Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("compile")
        .arg(&a)
        .args(["--create-dep".as_ref(), dep.as_os_str()])
        .args(["-o".as_ref(), dir.join("a.s").as_os_str()])
        .output()
        .expect("run k816");
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    let written = std::fs::read_to_string(&dep).expect("read the dependency file");
    assert!(
        !written.contains("unused.k816"),
        "a file the build never read is a prerequisite of it: {written}"
    );
}

/// A cycle of imports terminates, and both units compile.
#[test]
fn a_cycle_of_imports_terminates_through_the_driver() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-import-cycle");
    std::fs::create_dir_all(&dir).expect("create work dir");
    std::fs::write(
        dir.join("a.k816"),
        "import \"b.k816\"\n\nfunc entry @a8 {\n    jsr other\n}\n",
    )
    .expect("write a");
    std::fs::write(
        dir.join("b.k816"),
        "import \"a.k816\"\n\nvar dp shared = $20\n\nfunc other @a8 {\n    nop\n}\n",
    )
    .expect("write b");
    for unit in ["a.k816", "b.k816"] {
        let result = Command::new(env!("CARGO_BIN_EXE_k816"))
            .arg("compile")
            .arg(dir.join(unit))
            .args(["-o".as_ref(), dir.join("out.s").as_os_str()])
            .output()
            .expect("run k816");
        assert!(
            result.status.success(),
            "{unit}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
    }
}

/// Every file an `import` reached is a dependency, transitively.
///
/// The whole closure and not only the direct edges: a unit two imports away
/// is not visible to the entry, but its body decided a contract the entry
/// compiled against, so a build that rebuilt only on direct imports would
/// ship a stale object. And a file named *both* ways is listed once, because
/// it is one file.
#[test]
fn imports_are_dependencies_transitively() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("cli-import-deps");
    std::fs::create_dir_all(&dir).expect("create work dir");
    std::fs::write(
        dir.join("main.k816"),
        "import \"mid.k816\"\n\nfunc main @a8 {\n    jsr middle\n}\n",
    )
    .expect("write main");
    std::fs::write(
        dir.join("mid.k816"),
        "import \"deep.k816\"\n\nfunc middle @a8 {\n    jsr deep\n}\n",
    )
    .expect("write mid");
    std::fs::write(dir.join("deep.k816"), "func deep @a8 {\n    nop\n}\n").expect("write deep");

    // Run from the directory with a bare source name, which is what a build
    // generator does and what makes the naming assertion below meaningful.
    let result = Command::new(env!("CARGO_BIN_EXE_k816"))
        .current_dir(&dir)
        .args([
            "compile",
            "main.k816",
            "--create-dep",
            "main.d",
            "-o",
            "main.s",
        ])
        .output()
        .expect("run k816");
    let dep = dir.join("main.d");
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    let written = std::fs::read_to_string(&dep).expect("read the dependency file");
    let rule = written.lines().next().expect("a rule");
    for name in ["main.k816", "mid.k816", "deep.k816"] {
        assert!(rule.contains(name), "`{name}` is missing from: {rule}");
    }
    // Named the way the importing source named them, not by the absolute
    // path the loader opened. A prerequisite is something a person reads and
    // a build system matches against its own rules, and neither is helped by
    // a path nobody wrote — the file's *identity* is canonical so that two
    // spellings are one unit, and its *name* is what it was called.
    let prerequisites = rule.split_once(":\t").expect("a target and its list").1;
    for prerequisite in prerequisites.split_whitespace() {
        assert!(
            !std::path::Path::new(prerequisite).is_absolute(),
            "`{prerequisite}` is absolute, and nothing on the command line was: {rule}"
        );
    }
    assert_eq!(
        rule.matches("mid.k816").count(),
        1,
        "one file, one prerequisite: {rule}"
    );
    assert_eq!(
        rule.matches("deep.k816").count(),
        1,
        "and the transitive one is listed once too: {rule}"
    );
}
