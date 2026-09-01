/*****************************************************************************/
/*                                                                           */
/*                                workspace.rs                               */
/*                                                                           */
/*            Shared analysis entry over a set of open documents             */
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

//! The document workspace: the store behind the LSP (and any other tool
//! holding live documents).
//!
//! It never analyzes on its own — analysis goes through the same shared
//! frontend APIs the CLI uses ([`crate::analyze_workspace`]), so the two can
//! never disagree. Documents are re-analyzed from scratch on every change;
//! K816 sources are small (see `doc/design.md` §10), and incremental
//! semantic analysis is deliberately deferred until profiling demands it.

use std::collections::BTreeMap;

use crate::source::{FileId, SourceMap};
use crate::{Analysis, Options};

/// Live documents, keyed by their URI (or path) string, and the options they
/// are analyzed under.
///
/// The options live here rather than at each `analyze` call because they are
/// a property of the *project* the documents belong to, not of one analysis:
/// an editor states them once (through `initializationOptions`, through
/// `workspace/didChangeConfiguration`, or on the `k816 lsp` command line) and
/// every re-analysis after that has to use the same ones. A parameter would
/// let one caller pass `--strict-k65` and the next forget, which is exactly
/// the disagreement between a build and an editor that sub-plan H exists to
/// close (`plans/07-lsp-tooling.md` decision 1).
#[derive(Debug, Default)]
pub struct Workspace {
    documents: BTreeMap<String, String>,
    options: Options,
}

impl Workspace {
    pub fn new() -> Self {
        Self::default()
    }

    /// A workspace whose documents are analyzed under `options`.
    pub fn with_options(options: Options) -> Self {
        Self {
            documents: BTreeMap::new(),
            options,
        }
    }

    /// The options every analysis of this workspace runs under.
    pub fn options(&self) -> &Options {
        &self.options
    }

    /// Replaces the options wholesale.
    ///
    /// Wholesale rather than field by field: the last statement of the
    /// configuration wins, and a caller that wants to change one axis reads
    /// [`Self::options`] first. Both sources of the configuration — the
    /// command line at startup and the protocol afterwards — go through this
    /// one door, so there is never a *default* competing with a statement.
    pub fn set_options(&mut self, options: Options) {
        self.options = options;
    }

    /// Opens a document or replaces the text of an already open one.
    pub fn update(&mut self, name: impl Into<String>, text: impl Into<String>) {
        self.documents.insert(name.into(), text.into());
    }

    pub fn close(&mut self, name: &str) {
        self.documents.remove(name);
    }

    pub fn is_empty(&self) -> bool {
        self.documents.is_empty()
    }

    /// Analyzes every open document through the shared frontend and returns
    /// the source map (for span translation) alongside the analysis.
    pub fn analyze(&self) -> (SourceMap, Analysis) {
        let mut map = SourceMap::new();
        for (name, text) in &self.documents {
            map.add_file(name.clone(), text.clone());
        }
        let analysis = crate::analyze_workspace(&map, &self.options);
        (map, analysis)
    }
}

/// The [`FileId`] of the document named `name` inside a map returned by
/// [`Workspace::analyze`].
pub fn find_file(map: &SourceMap, name: &str) -> Option<FileId> {
    map.files()
        .find(|file| file.name() == name)
        .map(|file| file.id())
}
