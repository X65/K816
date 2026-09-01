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
//! frontend APIs the CLI uses ([`crate::analyze_parsed`]), so the two can
//! never disagree. What the store adds is *retention*: it owns the source
//! map, so a document keeps its [`FileId`] across an edit, and it keeps the
//! syntax tree of each document's last parse so the next one can be
//! incremental.
//!
//! Semantic analysis is not incremental and is not cached: every change
//! re-analyzes the whole workspace and the previous [`Analysis`] is dropped.
//! K816 sources are small (`doc/design.md` §10), and the number that would
//! justify anything else is `plans/07-lsp-tooling.md` H9's to measure.

use std::collections::BTreeMap;

use crate::parser::{SourceEdit, SyntaxTree};
use crate::source::{FileId, SourceMap};
use crate::{Analysis, Options};

/// One open document: where its text lives, and the tree it last parsed to.
#[derive(Debug)]
struct Document {
    /// This document's identity in the workspace's [`SourceMap`], which is
    /// also where its text lives — the map is the single owner of source
    /// text, here as everywhere else.
    file: FileId,
    /// The tree of the last parse, or `None` when the text has changed in a
    /// way the tree cannot describe.
    ///
    /// Tree-sitter can only reuse a tree that has been told what moved, so a
    /// whole-text replacement drops it. Cloning one is a refcount bump, which
    /// is what lets the store keep the tree it just handed to an `Analysis`.
    tree: Option<SyntaxTree>,
}

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
    /// Every open document's text, and the identity each span points into.
    map: SourceMap,
    documents: BTreeMap<String, Document>,
    options: Options,
    /// How many times [`Self::analyze`] has run; see [`Self::analyses`].
    analyses: u64,
}

impl Workspace {
    pub fn new() -> Self {
        Self::default()
    }

    /// A workspace whose documents are analyzed under `options`.
    pub fn with_options(options: Options) -> Self {
        Self {
            options,
            ..Self::default()
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

    /// The source map holding every open document's text.
    ///
    /// Every [`Span`](crate::source::Span) an [`Analysis`] carries points
    /// into it, so it is what a caller translates positions against.
    pub fn map(&self) -> &SourceMap {
        &self.map
    }

    /// The identity of the open document named `name`.
    pub fn file(&self, name: &str) -> Option<FileId> {
        self.documents.get(name).map(|document| document.file)
    }

    /// Opens a document or replaces the text of an already open one.
    pub fn update(&mut self, name: impl Into<String>, text: impl Into<String>) {
        let name = name.into();
        match self.documents.get_mut(&name) {
            Some(document) => {
                self.map.replace_file(document.file, text);
                // A whole-text replacement says nothing about what moved, so
                // the retained tree cannot be reused for it.
                document.tree = None;
            }
            None => {
                let file = self.map.add_file(name.clone(), text);
                self.documents.insert(name, Document { file, tree: None });
            }
        }
    }

    /// Applies one text replacement to an open document, incrementally.
    ///
    /// The difference from [`Self::update`] is the whole of what makes the
    /// next parse incremental: an edit says *what moved*, so the retained
    /// tree can be told and reused, while a whole-text replacement says only
    /// what the text now is. A client that sends ranged content changes gets
    /// the first; one that sends whole documents gets the second, and both
    /// are correct.
    ///
    /// Changes arrive in order and each is against the text the previous ones
    /// left, so a caller applying several converts each one's range against
    /// [`Self::map`] *after* the preceding call.
    ///
    /// Nothing here validates the edit: `edit.span` is already clamped to the
    /// document by the conversion that produced it, and there is no way to
    /// report a refusal from inside a notification anyway (decision 18).
    pub fn apply_edit(&mut self, name: &str, edit: &SourceEdit) {
        let Self { map, documents, .. } = self;
        let Some(document) = documents.get_mut(name) else {
            return;
        };
        debug_assert_eq!(edit.span.file, document.file);
        let before = map.file(document.file);
        let after = edit.apply(before);
        if let Some(tree) = &mut document.tree {
            // Recorded against the text the tree was parsed from, which is
            // still what the map holds — hence before the replacement.
            tree.edit(before, edit);
        }
        map.replace_file(document.file, after);
    }

    pub fn close(&mut self, name: &str) {
        if self.documents.remove(name).is_some() {
            self.rebuild();
        }
    }

    pub fn is_empty(&self) -> bool {
        self.documents.is_empty()
    }

    /// Rebuilds the source map over the documents that remain, reassigning
    /// their identities.
    ///
    /// A [`FileId`] is a position in the map, so closing a document moves
    /// every later one. The retained trees survive it — a syntax tree records
    /// byte offsets and node kinds and knows nothing about which file it came
    /// from — so a close costs the *other* documents nothing.
    fn rebuild(&mut self) {
        let mut surviving: Vec<(String, String)> = self
            .documents
            .iter()
            .map(|(name, document)| (name.clone(), document.file))
            .collect::<Vec<_>>()
            .into_iter()
            // In the order they had, so the analysis order does not jump
            // around when an unrelated document closes.
            .map(|(name, file)| (name, self.map.file(file).text().to_owned()))
            .collect();
        surviving.sort_by_key(|(name, _)| self.documents[name.as_str()].file.index());

        self.map = SourceMap::new();
        for (name, text) in surviving {
            let file = self.map.add_file(name.clone(), text);
            self.documents
                .get_mut(&name)
                .expect("collected from this map")
                .file = file;
        }
    }

    /// How many analyses this workspace has run.
    ///
    /// A test hook, and the only one: the server coalesces a burst of
    /// notifications into a single analysis (decision 3), and "one analysis"
    /// is not observable from the diagnostics it produces — the same answer
    /// arrives whether it was computed once or five times.
    pub fn analyses(&self) -> u64 {
        self.analyses
    }

    /// Analyzes every open document through the shared frontend.
    ///
    /// Each document is parsed against the tree it last produced, and the
    /// fresh tree is retained for the next analysis. The spans the result
    /// carries point into [`Self::map`].
    pub fn analyze(&mut self) -> Analysis {
        self.analyses += 1;
        let Self {
            map,
            documents,
            options,
            ..
        } = self;
        let mut by_file: BTreeMap<FileId, &mut Document> = documents
            .values_mut()
            .map(|document| (document.file, document))
            .collect();

        crate::analyze_parsed(map, options, |source| match by_file.get_mut(&source.id()) {
            Some(document) => {
                let result = crate::parser::parse_source(source, document.tree.as_ref());
                document.tree = Some(result.tree.clone());
                result
            }
            // A file in the map that is not an open document cannot happen
            // while `update`, `close` and `rebuild` are the only writers; a
            // fresh parse is the answer that is never wrong if it does.
            None => crate::parser::parse_source(source, None),
        })
    }
}

/// The [`FileId`] of the document named `name` inside `map`.
pub fn find_file(map: &SourceMap, name: &str) -> Option<FileId> {
    map.files()
        .find(|file| file.name() == name)
        .map(|file| file.id())
}
