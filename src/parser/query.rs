/*****************************************************************************/
/*                                                                           */
/*                                 query.rs                                  */
/*                                                                           */
/*        Tree-sitter queries, compiled against the committed grammar        */
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

//! Compiling and running Tree-sitter queries against the committed grammar.
//!
//! Here rather than in a test helper because `tree_sitter` types stop at
//! `src/parser/` (A3's rule) and there are two consumers: `tests/queries.rs`,
//! which compiles all five query files so a grammar edit that invalidates one
//! fails `cargo test`, and `src/lsp/tokens.rs`, which runs `highlights.scm`
//! over a document and maps its captures onto the semantic-token legend.
//!
//! **The queries are the highlighting, in both formats.** Editors that read
//! Tree-sitter queries use the files directly; editors that do not are served
//! semantic tokens derived from the *same* query. Describing the highlighting
//! twice is what `implementation.md` §3 decision 12 refuses
//! (`plans/07-lsp-tooling.md` decisions 6 and 14).

use crate::source::{FileId, Span};

use super::cst::SyntaxTree;

/// One capture: the name the query gave it, and where it matched.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Capture {
    /// The capture name without its `@`, as the query wrote it.
    pub name: String,
    pub span: Span,
    /// Which pattern of the query matched, counting from zero.
    ///
    /// Tree-sitter's own highlighter resolves two captures of one node by
    /// letting the *later* pattern win, and a semantic-token stream has to
    /// pick one too — the protocol forbids overlapping tokens. Carrying the
    /// index is how the two formats reach the same answer from the same file
    /// (`plans/07-lsp-tooling.md` decision 6).
    pub pattern: usize,
}

/// Why a query source could not be compiled.
///
/// Carries the message Tree-sitter produced, which names the offending node
/// or capture — that sentence *is* the drift report when a grammar edit
/// invalidates a query.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QueryError {
    pub message: String,
    /// Byte offset into the query source where it went wrong.
    pub offset: usize,
}

impl std::fmt::Display for QueryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at byte {}: {}", self.offset, self.message)
    }
}

/// A compiled query.
pub struct Query {
    query: tree_sitter::Query,
}

impl Query {
    /// Compiles `source` against the generated K65/K816 grammar.
    ///
    /// Fails on a node kind, a field, or a predicate the grammar does not
    /// have, which is the whole reason the query files are compiled under
    /// `cargo test`: a query naming a renamed node is silently *empty* at
    /// run time and loudly wrong here.
    pub fn compile(source: &str) -> Result<Self, QueryError> {
        tree_sitter::Query::new(&super::language(), source)
            .map(|query| Self { query })
            .map_err(|error| QueryError {
                message: error.to_string(),
                offset: error.offset,
            })
    }

    /// Every capture name the query uses, in the order it declared them.
    pub fn capture_names(&self) -> Vec<String> {
        self.query
            .capture_names()
            .iter()
            .map(|name| (*name).to_owned())
            .collect()
    }

    /// Every capture in `tree`, in source order.
    ///
    /// Sorted by span and then by name so a snapshot is stable: Tree-sitter
    /// yields matches in pattern order within a node, which is an artifact of
    /// how the query was written rather than a fact about the program.
    pub fn captures(&self, tree: &SyntaxTree, file: FileId, source: &str) -> Vec<Capture> {
        let mut cursor = tree_sitter::QueryCursor::new();
        let names = self.query.capture_names();
        let mut found = Vec::new();
        let mut matches = cursor.matches(&self.query, tree.root(), source.as_bytes());
        while let Some(matched) = {
            use tree_sitter::StreamingIterator as _;
            matches.next()
        } {
            for capture in matched.captures {
                found.push(Capture {
                    name: names[capture.index as usize].to_owned(),
                    span: Span::new(
                        file,
                        capture.node.start_byte() as u32,
                        capture.node.end_byte() as u32,
                    ),
                    pattern: matched.pattern_index,
                });
            }
        }
        found.sort();
        found.dedup();
        found
    }
}
