/*****************************************************************************/
/*                                                                           */
/*                                   mod.rs                                  */
/*                                                                           */
/*            Shared helpers for tests that drive ca65 and ld65              */
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

//! The cc65 tools are located in the cc65 tree's `bin/` (`../../bin` from
//! this crate), overridable via the `CA65`/`LD65`/`CL65` environment
//! variables.
//!
//! `cl65` finds `k816` beside itself, so the driver gates in
//! `tests/driver.rs` exercise the *installed* compiler rather than the one
//! Cargo just built. `make -C ../.. k816` is what keeps the two the same.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

pub fn tool(env: &str, name: &str) -> PathBuf {
    if let Ok(path) = std::env::var(env) {
        return PathBuf::from(path);
    }
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../bin")
        .join(name)
}

/// A per-test scratch directory under Cargo's target tmp dir.
pub fn work_dir(test: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(test);
    std::fs::create_dir_all(&dir).expect("create work dir");
    dir
}

/// Runs `command`, panicking with its stderr on failure.
pub fn run_ok(command: &mut Command, what: &str) {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("cannot run {what}: {error}"));
    assert!(
        output.status.success(),
        "{what} failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

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
// Only the three suites that drive `cl65` stage a toolchain, so this is dead
// code in the others — `mod common` is compiled into each of them separately.
#[allow(dead_code)]
pub fn toolchain() -> &'static Path {
    static TOOLCHAIN: OnceLock<PathBuf> = OnceLock::new();
    TOOLCHAIN
        .get_or_init(|| {
            let dir = work_dir("staged-toolchain");
            for (source, name) in [
                (PathBuf::from(env!("CARGO_BIN_EXE_k816")), "k816"),
                (tool("CA65", "ca65"), "ca65"),
                (tool("LD65", "ld65"), "ld65"),
                (tool("CL65", "cl65"), "cl65"),
                // For the one program that meets another language: `cl65`
                // finds its tools beside itself, so a C file on the command
                // line needs cc65 staged here too.
                (tool("CC65", "cc65"), "cc65"),
            ] {
                // Copy under a unique name and rename over the destination:
                // the three test binaries that stage tools share this
                // directory, and a half-written file that another one is
                // about to execute is the one failure mode worth ruling out.
                let destination = dir.join(name);
                let staging = dir.join(format!("{name}.{}.tmp", std::process::id()));
                std::fs::copy(&source, &staging).unwrap_or_else(|error| {
                    panic!(
                        "cannot stage {} (build it with `make -C ../.. bin`): {error}",
                        source.display()
                    )
                });
                std::fs::rename(&staging, &destination).expect("install a staged tool");
            }
            dir
        })
        .as_path()
}
