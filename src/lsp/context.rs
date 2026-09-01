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
use crate::ast::{Block, DataBlock, DataEntryKind, ItemKind, Statement, StatementKind, StmtId};
use crate::parser::CstNode;
use crate::source::{FileId, Span};
use crate::symbol::{LayoutId, SymbolId};

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
    pub declaration: Option<SymbolId>,
    /// The symbol a *use* of a name at this offset resolves to, from the
    /// reference index the semantic pass recorded while resolving names.
    pub reference: Option<SymbolId>,
    /// The field a `.field` step at this offset selected. A field is not a
    /// symbol — it has no scope of its own — so it is named by its layout and
    /// its position in it.
    pub field: Option<(LayoutId, usize)>,
}

impl<'a> Context<'a> {
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

    /// The symbol this position names, whether it declares one or uses one.
    pub fn symbol(&self) -> Option<SymbolId> {
        self.declaration.or(self.reference)
    }

    /// The span of the name this position sits in, if it sits in one.
    ///
    /// Either a declaring name or a reference-index key — which are the two
    /// spans a rename may edit and the two `prepareRename` answers on.
    pub fn name_span(&self) -> Option<Span> {
        let file = self.file();
        let covering = |span: &Span| span.file == file && span.contains(self.offset);
        if let Some((_, symbol)) = self
            .unit
            .model
            .symbols
            .iter()
            .find(|(_, symbol)| covering(&symbol.declaration))
        {
            return Some(symbol.declaration);
        }
        if let Some(span) = self
            .unit
            .model
            .references
            .keys()
            .find(|span| covering(span))
            .or_else(|| {
                self.unit
                    .model
                    .field_references
                    .keys()
                    .find(|span| covering(span))
            })
        {
            return Some(*span);
        }
        // A field's declaring `.name`, which is narrower than the entry the
        // model records for "declared here" — that one covers the width too,
        // and an editor replacing it would delete `:byte` with the name.
        let (layout, index) = self.field?;
        self.unit
            .model
            .symbols
            .layout(layout)
            .fields
            .get(index)
            .map(|field| field.name_span)
            .filter(covering)
    }

    /// The file the offset belongs to.
    pub fn file(&self) -> FileId {
        self.unit.file
    }

    /// The statement `id` names, in the program the compiler walked.
    ///
    /// The walk is here rather than generic because a consumer wants a borrow
    /// of the *program*, and `ast::visit_statements` hands out a borrow of the
    /// closure's own frame.
    pub fn statement_node(&self, id: StmtId) -> Option<&'a Statement> {
        let mut found: Option<&'a Statement> = None;
        for item in &self.unit.program().items {
            let mut visit = |candidate: &'a Statement| {
                if candidate.id == id {
                    found = Some(candidate);
                }
            };
            match &item.kind {
                ItemKind::Func(function) => collect(&function.body, &mut visit),
                ItemKind::Data(data) => collect_data(data, &mut visit),
                _ => {}
            }
        }
        found
    }
}

/// [`crate::ast::visit_statements`], with the lifetime the caller needs: the
/// generic walk hands out a borrow of the closure's own frame, and a consumer
/// of this one wants a borrow of the program.
fn collect<'a>(block: &'a Block, visit: &mut impl FnMut(&'a Statement)) {
    let mut stack: Vec<&'a Statement> = block.statements.iter().collect();
    while let Some(statement) = stack.pop() {
        visit(statement);
        match &statement.kind {
            StatementKind::WidthBlock(width) => stack.extend(width.body.statements.iter()),
            StatementKind::Nocross(body) => stack.extend(body.statements.iter()),
            StatementKind::Loop(loop_statement) => {
                stack.extend(loop_statement.body.statements.iter());
            }
            StatementKind::Conditional(conditional) => {
                stack.extend(conditional.body.statements.iter());
                if let Some(otherwise) = &conditional.otherwise {
                    stack.extend(otherwise.statements.iter());
                }
            }
            StatementKind::ConditionalJump(jump) => stack.push(&jump.jump),
            StatementKind::Data(data) => collect_data(data, visit),
            _ => {}
        }
    }
}

fn collect_data<'a>(data: &'a DataBlock, visit: &mut impl FnMut(&'a Statement)) {
    for entry in &data.entries {
        match &entry.kind {
            DataEntryKind::Code { body, .. } => collect(body, visit),
            DataEntryKind::Repeat { body, .. } => collect_data(body, visit),
            DataEntryKind::NocrossGroup(entries) => {
                for entry in entries {
                    if let DataEntryKind::Code { body, .. } = &entry.kind {
                        collect(body, visit);
                    }
                }
            }
            _ => {}
        }
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
    let covering = |span: &Span| span.file == file && span.contains(offset);
    let reference = unit
        .model
        .references
        .iter()
        .find(|(span, _)| covering(span))
        .map(|(_, id)| *id);
    let field = unit
        .model
        .field_references
        .iter()
        .find(|(span, _)| covering(span))
        .map(|(_, field)| *field)
        // A field's *declaring* `.name` is not a reference and not a symbol —
        // a field has no `SymbolId` — so it is found the way a symbol's
        // declaration is: by walking what declares it.
        .or_else(|| declared_field(unit, offset));

    Some(Context {
        unit,
        offset,
        path,
        statement,
        function,
        declaration,
        reference,
        field,
    })
}

/// The field whose declaring `.name` covers `offset`.
///
/// The layouts are walked rather than the AST: a layout is the thing a
/// reference names, and the AST's field list would have to be matched back to
/// one anyway.
fn declared_field(unit: &AnalyzedUnit, offset: u32) -> Option<(LayoutId, usize)> {
    for (id, _) in unit.model.symbols.iter() {
        let layout = match unit.model.symbols.kind(id) {
            crate::symbol::SymbolKind::Var {
                layout: Some(layout),
                ..
            }
            | crate::symbol::SymbolKind::AbstractVar { layout } => layout,
            _ => continue,
        };
        let found = unit
            .model
            .symbols
            .layout(layout)
            .fields
            .iter()
            .position(|field| field.name_span.contains(offset));
        if let Some(index) = found {
            return Some((layout, index));
        }
    }
    None
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
