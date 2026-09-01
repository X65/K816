/*****************************************************************************/
/*                                                                           */
/*                                 config.rs                                 */
/*                                                                           */
/*        Workspace configuration carried over the protocol, in the          */
/*                            CLI's spellings                                */
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

//! The configuration seam: how an editor states the same things
//! `k816 compile` is told on its command line.
//!
//! The keys are the flags' own spellings — `"strict-k65"` and `"entry"` —
//! and the values are read by the same library functions `main.rs` calls, so
//! there is one vocabulary and one refusal. An editor that states nothing
//! gets what a build that states nothing gets; an editor that states
//! `--strict-k65` sees exactly the diagnostics that build sees
//! (`plans/07-lsp-tooling.md` decision 1).
//!
//! **An absent key changes nothing.** The object an editor sends is a
//! statement about the keys it mentions, not a complete description of the
//! configuration: `didChangeConfiguration` carrying only `"entry"` must not
//! silently reset a `--strict-k65` the command line established. That is why
//! [`apply`] mutates the options it is given rather than building a fresh
//! set from defaults — two sources of one fact are safe only while neither
//! of them is a default (`memory/entry-abi-assumption-conflict`).

use serde_json::Value;

use crate::Options;
use crate::abi::{EntryEnvironment, EntrySpelling};
use crate::diagnostic::DiagnosticCode;

/// The configuration key naming `--strict-k65`.
pub const STRICT_K65: &str = "strict-k65";
/// The configuration key naming `--entry`.
pub const ENTRY: &str = "entry";
/// The section a client that sends its whole settings tree nests them under.
pub const SECTION: &str = "k816";

/// Something the editor should be told about the configuration it sent.
///
/// Not a [`crate::diagnostic::Diagnostic`]: a diagnostic anchors a message to
/// a span in a source file, and a settings object has neither. It carries a
/// code anyway, for the same reason [`EntrySpelling`] does — the pairing of
/// code and wording belongs beside the rule, not in the transport.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConfigMessage {
    /// `true` for a refusal (the value did not apply), `false` for a warning
    /// (something was ignored).
    pub is_error: bool,
    /// The code this message carries, or `None` for the unknown-key warning,
    /// which is about the protocol rather than about the language.
    pub code: Option<DiagnosticCode>,
    pub text: String,
}

impl ConfigMessage {
    /// The message as one line, code first, the way the CLI renders a tool
    /// error.
    pub fn render(&self) -> String {
        match self.code {
            Some(code) => format!("{}: {}", code.as_str(), self.text),
            None => self.text.clone(),
        }
    }
}

/// Reads a configuration object into `options` and returns what the editor
/// should be shown.
///
/// `value` is either the configuration itself or a settings tree with the
/// configuration under `"k816"`; both are accepted because VS Code sends the
/// whole tree and a hand-written client sends the object. A key the server
/// does not know is a warning naming it and the two that are known; a value
/// of the wrong JSON type is a refusal and leaves the option alone, as does
/// an `entry` string `EntryEnvironment::parse` refuses.
pub fn apply(options: &mut Options, value: &Value) -> Vec<ConfigMessage> {
    let Some(object) = section(value) else {
        // Not an object at all (`null` is what a client sends for "no
        // initialization options"), or a tree with no `k816` section in it.
        // Neither is a statement about anything, so nothing changes.
        return Vec::new();
    };

    let mut messages = Vec::new();
    for (key, entry) in object {
        match key.as_str() {
            STRICT_K65 => match entry.as_bool() {
                Some(flag) => options.strict_k65 = flag,
                None => messages.push(ConfigMessage {
                    is_error: true,
                    code: None,
                    text: format!(
                        "`{STRICT_K65}` takes true or false, not {}; the setting is unchanged",
                        type_name(entry)
                    ),
                }),
            },
            ENTRY => match entry.as_str() {
                Some(text) => match EntryEnvironment::parse(text) {
                    Ok(environment) => options.entry = environment,
                    Err(refusal) => messages.push(refused_entry(&refusal)),
                },
                None => messages.push(ConfigMessage {
                    is_error: true,
                    code: Some(EntrySpelling::CODE),
                    text: format!(
                        "`{ENTRY}` takes a string like `native,a8,i8`, not {}; the setting is \
                         unchanged",
                        type_name(entry)
                    ),
                }),
            },
            other => messages.push(ConfigMessage {
                is_error: false,
                code: None,
                text: format!(
                    "unknown k816 setting `{other}`; this server reads `{STRICT_K65}` and \
                     `{ENTRY}`"
                ),
            }),
        }
    }
    messages
}

/// A refused `--entry`/`entry` value, worded by the library that refused it.
fn refused_entry(refusal: &EntrySpelling) -> ConfigMessage {
    ConfigMessage {
        is_error: true,
        code: Some(EntrySpelling::CODE),
        text: format!("`{ENTRY}`: {refusal}"),
    }
}

/// The object holding the settings: `value` itself, or its `"k816"` member.
///
/// Descending when the member exists is what keeps a client's unrelated
/// settings from each producing an unknown-key warning; a client that sends
/// the bare object gets its keys checked, which is what it asked for by
/// sending them.
fn section(value: &Value) -> Option<&serde_json::Map<String, Value>> {
    let object = value.as_object()?;
    match object.get(SECTION) {
        Some(nested) => nested.as_object(),
        None => Some(object),
    }
}

/// The JSON type of `value`, for a message about a value of the wrong one.
fn type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "a boolean",
        Value::Number(_) => "a number",
        Value::String(_) => "a string",
        Value::Array(_) => "an array",
        Value::Object(_) => "an object",
    }
}
