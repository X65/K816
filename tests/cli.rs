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
const BROKEN: &str = "func main @a8 @i16 {\n    SCREEN = a\n    return\n}\n";
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
