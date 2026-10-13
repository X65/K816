/*****************************************************************************/
/*                                                                           */
/*                                   lib.rs                                  */
/*                                                                           */
/*     The Zed extension's one job: how to start the K816 language server    */
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

//! What Zed asks an extension for and K816 has an answer to: the command that
//! starts the language server.
//!
//! The server is found on `PATH` and never downloaded — there are no releases
//! to download it from, and a `k816` someone built is the one whose
//! diagnostics match their build. The extension sends no initialization
//! options of its own, so the user's `lsp.k816.initialization_options` is the
//! one place `strict-k65` and `entry` are stated, in the server's own
//! spellings: an editor that states nothing gets what a build that states
//! nothing gets (`src/lsp/config.rs`).

use zed_extension_api as zed;

/// What the extension says when there is no `k816` to start.
///
/// The last sentence is the one that matters most: Zed starts a configured
/// `binary.path` with `binary.arguments` *only* and never asks the extension,
/// so a path without `["lsp"]` starts the compiler, which waits for a file,
/// and the editor waits for an `initialize` answer that never comes.
const MISSING: &str = "k816 is not on PATH. It is built from the cc65 fork: \
    `make -C .. k816` in src/k816 (or `make -C ../.. bin` for the whole \
    toolchain) puts it in the fork's bin/ directory. Add that directory to \
    the PATH your login shell sets, or set `lsp.k816.binary.path` in Zed's \
    settings together with `lsp.k816.binary.arguments = [\"lsp\"]`.";

struct K816Extension;

impl zed::Extension for K816Extension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> zed::Result<zed::Command> {
        let command = worktree.which("k816").ok_or_else(|| MISSING.to_owned())?;
        Ok(zed::Command {
            command,
            args: vec!["lsp".to_owned()],
            env: Vec::new(),
        })
    }
}

zed::register_extension!(K816Extension);
