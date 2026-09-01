/*****************************************************************************/
/*                                                                           */
/*                                context.rs                                 */
/*                                                                           */
/*             What is under this byte, asked once for everything            */
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

//! "What is under this byte?" — asked once, answered for every consumer.
//!
//! Hover, definition, references, rename and completion all begin with the
//! same question and would otherwise each answer it their own way. One
//! function means one answer: if a construct is invisible to the editor, it
//! is invisible everywhere at once, which is a bug someone notices.
//!
//! **Two trees, deliberately.** The CST node comes from the *parse* — the
//! tree of the text the editor is showing — while the statement, the
//! function and the symbol come from the program the compiler actually
//! walked, which is the preprocessed one where a `#if` removed anything. Both
//! are right about different questions: a region an `#if` did not take is
//! still text the cursor can sit in, and it has no statement, no width and no
//! symbol (`postmortem.md` §G.6.9, `plans/07-lsp-tooling.md`'s risk note on
//! the preprocessed program's holes).

use crate::AnalyzedUnit;
use crate::ast::{ItemKind, StmtId};
use crate::parser::CstNode;
use crate::source::FileId;
use crate::symbol::SymbolId;

/// What the model holds at one byte offset of one document.
#[derive(Debug)]
pub struct Context<'a> {
    pub unit: &'a AnalyzedUnit,
    /// The byte offset asked about.
    pub offset: u32,
    /// Every CST node covering the offset, outermost (`source_file`) first.
    ///
    /// From the *parsed* tree, so it answers for text a directive removed
    /// as well as for text it kept.
    pub path: Vec<CstNode>,
    /// The innermost statement whose span covers the offset, in the program
    /// the compiler walked. `None` outside any function or data body, and
    /// inside a region the preprocessor removed.
    pub statement: Option<StmtId>,
    /// The function whose body the offset is inside.
    pub function: Option<SymbolId>,
    /// The symbol whose *declaring* name the offset sits in.
    ///
    /// H3 adds the other half — the symbol a *use* of a name resolves to —
    /// through the reference index; until then a use answers `None` rather
    /// than being guessed at from its text.
    pub declaration: Option<SymbolId>,
}

impl Context<'_> {
    /// The innermost CST node covering the offset.
    pub fn node(&self) -> Option<&CstNode> {
        self.path.last()
    }

    /// The innermost node's grammar name, or `""` when there is none.
    pub fn node_kind(&self) -> &str {
        self.node().map_or("", |node| node.kind.as_str())
    }

    /// The innermost *named* node covering the offset.
    ///
    /// Punctuation and keywords the grammar spells inline are anonymous; a
    /// consumer that wants "which construct is this" wants the named one
    /// around them.
    pub fn named_node(&self) -> Option<&CstNode> {
        self.path.iter().rev().find(|node| node.named)
    }

    /// Whether the offset sits on a token rather than in the whitespace
    /// between two.
    pub fn on_token(&self) -> bool {
        self.node().is_some_and(CstNode::is_leaf)
    }
}

/// What the analysis holds at `offset` of `file`.
///
/// `None` when `file` is not one of the analyzed units — a request for a
/// document the workspace does not hold, which is an answer and not an error
/// (decision 18).
pub fn at(analysis: &crate::Analysis, file: FileId, offset: u32) -> Option<Context<'_>> {
    let unit = analysis.unit(file)?;
    let path = unit.parse.tree.node_path(file, offset);

    let mut statement = None;
    let mut function = None;
    for (index, item) in unit.program().items.iter().enumerate() {
        if !item.span.contains(offset) {
            continue;
        }
        match &item.kind {
            ItemKind::Func(declaration) => {
                function = unit
                    .model
                    .functions
                    .iter()
                    .find(|info| info.item_index == index)
                    .map(|info| info.symbol);
                statement = innermost(offset, |visit| {
                    crate::ast::visit_statements(&declaration.body, visit);
                });
            }
            ItemKind::Data(data) => {
                statement = innermost(offset, |visit| {
                    crate::ast::visit_data_statements(data, visit);
                });
            }
            _ => {}
        }
        break;
    }

    let declaration = unit
        .model
        .symbols
        .iter()
        .find(|(_, symbol)| symbol.declaration.file == file && symbol.declaration.contains(offset))
        .map(|(id, _)| id);

    Some(Context {
        unit,
        offset,
        path,
        statement,
        function,
        declaration,
    })
}

/// The deepest statement covering `offset` in a pre-order walk.
///
/// Pre-order visits a statement before what nests inside it, and at most one
/// child of a statement can cover a given offset — so the last statement the
/// walk reports as covering it is the innermost one.
fn innermost(
    offset: u32,
    walk: impl FnOnce(&mut &mut dyn FnMut(&crate::ast::Statement)),
) -> Option<StmtId> {
    let mut found = None;
    let mut visit = |statement: &crate::ast::Statement| {
        if statement.span.contains(offset) && statement.id.is_assigned() {
            found = Some(statement.id);
        }
    };
    let mut erased: &mut dyn FnMut(&crate::ast::Statement) = &mut visit;
    walk(&mut erased);
    found
}
