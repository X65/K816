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

/// Live documents, keyed by their URI (or path) string.
#[derive(Debug, Default)]
pub struct Workspace {
    documents: BTreeMap<String, String>,
}

impl Workspace {
    pub fn new() -> Self {
        Self::default()
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
    pub fn analyze(&self, options: &Options) -> (SourceMap, Analysis) {
        let mut map = SourceMap::new();
        for (name, text) in &self.documents {
            map.add_file(name.clone(), text.clone());
        }
        let analysis = crate::analyze_workspace(&map, options);
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
