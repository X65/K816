use k816_core::ast::{
    self, DataArg, DataCommand, Expr, File, HlaBranchForm, HlaCompareOp, HlaCondition, HlaRhs,
    HlaStmt,
    Instruction, Item, NamedDataEntry, NumFmt, Operand, OperandAddrMode, Stmt,
};
use k816_core::span::{Span, Spanned};
use pretty::{Arena, DocAllocator, DocBuilder};
use std::fmt::Write;

type Doc<'a> = DocBuilder<'a, Arena<'a>>;
const INDENT_WIDTH: isize = 4;
const INDENT_STR: &str = "    ";

pub fn format_ast(file: &File, source: &str) -> String {
    let arena = Arena::new();
    let doc = build_file_doc(&arena, file, source);
    let mut output = String::new();
    let _ = doc.render_fmt(usize::MAX, &mut output);
    trim_trailing_whitespace_per_line(&mut output);
    align_trailing_comments(&mut output);
    output
}

fn reg_width_token(prefix: char, width: k816_core::ast::RegWidth) -> &'static str {
    match (prefix, width) {
        ('a', k816_core::ast::RegWidth::W8) => "@a8",
        ('a', k816_core::ast::RegWidth::W16) => "@a16",
        ('i', k816_core::ast::RegWidth::W8) => "@i8",
        ('i', k816_core::ast::RegWidth::W16) => "@i16",
        _ => "",
    }
}

fn format_mode_contract_inline(
    a_width: Option<k816_core::ast::RegWidth>,
    i_width: Option<k816_core::ast::RegWidth>,
) -> String {
    let mut parts = Vec::new();
    if let Some(w) = a_width {
        parts.push(reg_width_token('a', w));
    }
    if let Some(w) = i_width {
        parts.push(reg_width_token('i', w));
    }
    parts.join(" ")
}

fn mode_contract_lines(
    a_width: Option<k816_core::ast::RegWidth>,
    i_width: Option<k816_core::ast::RegWidth>,
) -> Vec<&'static str> {
    let mut lines = Vec::new();
    if let Some(w) = a_width {
        lines.push(reg_width_token('a', w));
    }
    if let Some(w) = i_width {
        lines.push(reg_width_token('i', w));
    }
    lines
}

fn code_block_uses_func_keyword(source: &str, span_start: usize, name_span: Option<Span>) -> bool {
    let Some(name_span) = name_span else {
        return true;
    };
    if span_start >= source.len() {
        return true;
    }
    let start = span_start.min(source.len());
    let end = name_span.start.min(source.len());
    if start >= end {
        return true;
    }
    source[start..end]
        .split(|ch: char| ch.is_whitespace())
        .any(|token| token == "func")
}

/// Check whether byte offsets `a` and `b` are on the same line in `source`.
fn is_same_line(source: &str, a: usize, b: usize) -> bool {
    a <= b && b <= source.len() && !source[a..b].contains('\n')
}

/// Check whether there is a blank line (two consecutive newlines) in source between `from` and `to`.
fn has_blank_line(source: &str, from: usize, to: usize) -> bool {
    let from = from.min(source.len());
    let to = to.min(source.len());
    if from >= to {
        return false;
    }
    let bytes = source[from..to].as_bytes();
    let mut i = 0usize;
    while i + 1 < bytes.len() {
        if bytes[i] == b'\n' {
            let mut j = i + 1;
            while j < bytes.len()
                && matches!(bytes[j], b' ' | b'\t' | b'\r' | 0x0C)
            {
                j += 1;
            }
            if j < bytes.len() && bytes[j] == b'\n' {
                return true;
            }
        }
        i += 1;
    }
    false
}

fn find_brace_body_start(source: &str, block_start: usize, block_end: usize) -> usize {
    let start = block_start.min(source.len());
    let end = block_end.min(source.len());
    if start >= end {
        return start;
    }
    let region = &source[start..end];
    region
        .find('{')
        .map(|idx| start + idx + 1)
        .unwrap_or(start)
}

/// Count semicolons that appear at the beginning of a gap (after spaces/tabs).
/// Stops at the first non-space/tab/semicolon character.
fn count_leading_gap_semicolons(source: &str, from: usize, to: usize) -> usize {
    let from = from.min(source.len());
    let to = to.min(source.len());
    if from >= to {
        return 0;
    }
    let mut count = 0usize;
    for &b in source.as_bytes()[from..to].iter() {
        match b {
            b' ' | b'\t' => {}
            b';' => count += 1,
            _ => break,
        }
    }
    count
}

fn collect_standalone_semicolon_lines<'a>(
    arena: &'a Arena<'a>,
    source: &str,
    from: usize,
    to: usize,
) -> Vec<LayoutEntry<'a>> {
    let from = from.min(source.len());
    let to = to.min(source.len());
    if from >= to {
        return Vec::new();
    }

    let mut out = Vec::new();
    let mut line_start = from;
    while line_start < to {
        let rel_end = source[line_start..to].find('\n');
        let line_end = rel_end.map_or(to, |idx| line_start + idx);
        let at_line_start = line_start == 0 || source.as_bytes()[line_start - 1] == b'\n';
        if at_line_start {
            let line = &source[line_start..line_end];
            let trimmed = line.trim_matches(|ch| matches!(ch, ' ' | '\t' | '\r'));
            if !trimmed.is_empty() && trimmed.bytes().all(|b| b == b';') {
                let leading_ws = line
                    .bytes()
                    .take_while(|b| matches!(b, b' ' | b'\t' | b'\r'))
                    .count();
                let start = line_start + leading_ws;
                let end = start + trimmed.len();
                out.push(LayoutEntry {
                    doc: arena.text(trimmed.to_string()),
                    start,
                    end,
                });
            }
        }

        if line_end >= to {
            break;
        }
        line_start = line_end + 1;
    }

    out
}

struct LayoutEntry<'a> {
    doc: Doc<'a>,
    start: usize,
    end: usize,
}

fn join_layout_entries<'a>(
    arena: &'a Arena<'a>,
    source: &str,
    entries: Vec<LayoutEntry<'a>>,
) -> Doc<'a> {
    let mut iter = entries.into_iter();
    let Some(first) = iter.next() else {
        return arena.nil();
    };

    let mut out = first.doc;
    let mut prev_end = first.end;

    for entry in iter {
        if has_blank_line(source, prev_end, entry.start) {
            out = out
                .append(arena.hardline())
                .append(arena.hardline())
                .append(entry.doc);
        } else if is_same_line(source, prev_end, entry.start) {
            out = out.append(arena.text(" ")).append(entry.doc);
        } else {
            out = out.append(arena.hardline()).append(entry.doc);
        }
        prev_end = entry.end;
    }

    out
}

fn trim_trailing_whitespace_per_line(output: &mut String) {
    let mut result = String::with_capacity(output.len());
    for line in output.lines() {
        result.push_str(line.trim_end_matches([' ', '\t']));
        result.push('\n');
    }
    *output = result;
}

// ---------------------------------------------------------------------------
// Comment interleaving
// ---------------------------------------------------------------------------

/// A cursor into `File.comments` used during doc building.
/// All comments are sorted by position (from the lexer).
struct CommentCursor<'a> {
    comments: &'a [ast::Comment],
    idx: usize,
}

impl<'a> CommentCursor<'a> {
    fn new(comments: &'a [ast::Comment]) -> Self {
        Self { comments, idx: 0 }
    }

    /// Emit all standalone comments whose span ends before `before`.
    /// Returns `(Doc, Span)` pairs so callers can detect blank lines.
    fn drain_before<'b>(
        &mut self,
        arena: &'b Arena<'b>,
        source: &str,
        before: usize,
        prev_end: Option<usize>,
    ) -> Vec<(Doc<'b>, Span)> {
        let mut docs = Vec::new();
        while self.idx < self.comments.len() && self.comments[self.idx].span.end <= before {
            let comment = &self.comments[self.idx];
            // Skip if this is a trailing comment (same line as previous node)
            if let Some(prev) = prev_end {
                if is_same_line(source, prev, comment.span.start) {
                    self.idx += 1;
                    continue;
                }
            }
            docs.push((arena.text(comment.text.clone()), comment.span));
            self.idx += 1;
        }
        docs
    }

    /// If the next comment is a trailing comment (same source line as `after`),
    /// consume it and return its text.
    fn take_trailing(&mut self, source: &str, after: usize) -> Option<&'a str> {
        if self.idx < self.comments.len() {
            let comment = &self.comments[self.idx];
            if is_same_line(source, after, comment.span.start) {
                self.idx += 1;
                return Some(&comment.text);
            }
        }
        None
    }
}

// ---------------------------------------------------------------------------
// HLA block nesting
// ---------------------------------------------------------------------------

/// Intermediate representation: flat HLA statements grouped into nested blocks.
enum FmtStmt<'a> {
    Plain(&'a Spanned<Stmt>),
    HlaBlock {
        open: &'a Spanned<Stmt>,
        body: Vec<FmtStmt<'a>>,
        close: &'a Spanned<Stmt>,
    },
}

impl<'a> FmtStmt<'a> {
    fn span(&self) -> &Span {
        match self {
            FmtStmt::Plain(s) => &s.span,
            FmtStmt::HlaBlock { open, .. } => &open.span,
        }
    }

    fn span_end(&self) -> usize {
        match self {
            FmtStmt::Plain(s) => s.span.end,
            FmtStmt::HlaBlock { close, .. } => close.span.end,
        }
    }
}

fn is_hla_open(stmt: &Stmt) -> bool {
    matches!(stmt, Stmt::Hla(HlaStmt::DoOpen))
}

fn is_hla_close(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Hla(
            HlaStmt::DoCloseNFlagClear
                | HlaStmt::DoCloseNFlagSet
                | HlaStmt::DoCloseWithOp { .. }
                | HlaStmt::DoClose { .. }
                | HlaStmt::DoCloseAlways
                | HlaStmt::DoCloseNever
                | HlaStmt::DoCloseBranch { .. }
        )
    )
}

/// Convert a flat statement list with DoOpen/DoClose markers into a nested tree.
fn nest_hla<'a>(stmts: &'a [Spanned<Stmt>]) -> Vec<FmtStmt<'a>> {
    let mut result = Vec::new();
    let mut i = 0;
    nest_hla_inner(stmts, &mut i, &mut result);
    result
}

fn nest_hla_inner<'a>(stmts: &'a [Spanned<Stmt>], i: &mut usize, out: &mut Vec<FmtStmt<'a>>) {
    while *i < stmts.len() {
        let stmt = &stmts[*i];
        if is_hla_close(&stmt.node) {
            // Will be consumed by the caller as the close of an HlaBlock
            return;
        }
        if is_hla_open(&stmt.node) {
            let open = stmt;
            *i += 1;
            let mut body = Vec::new();
            nest_hla_inner(stmts, i, &mut body);
            // *i now points at the DoClose statement
            let close = if *i < stmts.len() {
                let c = &stmts[*i];
                *i += 1;
                c
            } else {
                // Unclosed block — use open as close (shouldn't happen in valid code)
                open
            };
            out.push(FmtStmt::HlaBlock { open, body, close });
        } else {
            out.push(FmtStmt::Plain(stmt));
            *i += 1;
        }
    }
}

// ---------------------------------------------------------------------------
// File-level doc builder
// ---------------------------------------------------------------------------

fn build_file_doc<'a>(arena: &'a Arena<'a>, file: &File, source: &str) -> Doc<'a> {
    let mut parts: Vec<Doc<'a>> = Vec::new();
    let mut cursor = CommentCursor::new(&file.comments);
    let mut prev_was_block = false;
    let mut prev_end: Option<usize> = None;

    for line in mode_contract_lines(file.mode_default.a_width, file.mode_default.i_width) {
        parts.push(arena.text(line));
        parts.push(arena.hardline());
    }

    for (item_idx, item) in file.items.iter().enumerate() {
        let is_block = is_block_item(&item.node);

        // Standalone comments before this item
        let gap_comments = cursor.drain_before(arena, source, item.span.start, prev_end);
        let has_gap_comments = !gap_comments.is_empty();
        let mut effective_prev_end = prev_end;
        for (gc_doc, gc_span) in &gap_comments {
            // Preserve blank lines from source between items/comments
            if let Some(pe) = effective_prev_end {
                if has_blank_line(source, pe, gc_span.start) {
                    parts.push(arena.hardline());
                }
            }
            parts.push(gc_doc.clone());
            parts.push(arena.hardline());
            effective_prev_end = Some(gc_span.end);
        }

        // Blank line before this item: forced for blocks, or preserved from source
        if !parts.is_empty() {
            if (is_block || prev_was_block) && !has_gap_comments {
                parts.push(arena.hardline());
            } else if let Some(pe) = effective_prev_end {
                if has_blank_line(source, pe, item.span.start) {
                    parts.push(arena.hardline());
                }
            }
        }

        // Format the item
        let next_item_start = file
            .items
            .get(item_idx + 1)
            .map(|next| next.span.start)
            .unwrap_or(source.len());
        let trailing_semicolons =
            count_leading_gap_semicolons(source, item.span.end, next_item_start);
        let mut item_doc = build_item_doc(
            arena,
            &mut cursor,
            source,
            &item.node,
            item.span.start,
            item.span.end,
        );
        if trailing_semicolons > 0 {
            item_doc = item_doc.append(arena.text(";".repeat(trailing_semicolons)));
        }

        // Trailing comment
        if !is_block {
            if let Some(tc) = cursor.take_trailing(source, item.span.end) {
                parts.push(item_doc.append(arena.text(" ")).append(arena.text(tc.to_string())));
            } else {
                parts.push(item_doc);
            }
        } else {
            parts.push(item_doc);
        }
        parts.push(arena.hardline());

        prev_end = Some(item.span.end);
        prev_was_block = is_block;
    }

    // Trailing comments after all items
    let eof_comments = cursor.drain_before(arena, source, usize::MAX, prev_end);
    for (gc_doc, _gc_span) in eof_comments {
        parts.push(gc_doc);
        parts.push(arena.hardline());
    }

    arena.concat(parts)
}

fn is_block_item(item: &Item) -> bool {
    matches!(
        item,
        Item::DataBlock(_) | Item::NamedDataBlock(_) | Item::CodeBlock(_)
    )
}

// ---------------------------------------------------------------------------
// Item-level doc builder
// ---------------------------------------------------------------------------

fn build_item_doc<'a>(
    arena: &'a Arena<'a>,
    cursor: &mut CommentCursor,
    source: &str,
    item: &Item,
    span_start: usize,
    span_end: usize,
) -> Doc<'a> {
    match item {
        Item::Segment(s) => arena.text(format!("segment {}", s.name)),
        Item::Const(c) => arena.text(format_const(c)),
        Item::ConstGroup(consts) => arena.text(format_const_group(consts)),
        Item::EvaluatorBlock(b) => arena.text(format!("[{}]", b.text)),
        Item::Var(v) => {
            if let Some(decl) = extract_image_binary_decl_from_source(source, span_start, span_end) {
                arena.text(decl)
            } else {
                arena.text(format_var(v))
            }
        }
        Item::DataBlock(block) => {
            let header = arena.text("data {");
            let header = append_brace_trailing_comment(header, arena, cursor, source, span_start);
            let body = build_spanned_body(
                arena,
                cursor,
                source,
                &block.commands,
                span_start,
                span_end,
                |cmd, item_start, item_end| {
                    format_data_command_in_span(cmd, source, item_start, item_end)
                },
            );
            header
                .append(arena.hardline().append(body).nest(INDENT_WIDTH))
                .append(arena.hardline())
                .append(arena.text("}"))
        }
        Item::NamedDataBlock(block) => {
            let header = arena.text(format!("data {} {{", block.name));
            let header = append_brace_trailing_comment(header, arena, cursor, source, span_start);
            let body = build_spanned_body(
                arena,
                cursor,
                source,
                &block.entries,
                span_start,
                span_end,
                |entry, item_start, item_end| {
                    format_named_data_entry_in_span(entry, source, item_start, item_end)
                },
            );
            header
                .append(arena.hardline().append(body).nest(INDENT_WIDTH))
                .append(arena.hardline())
                .append(arena.text("}"))
        }
        Item::CodeBlock(block) => {
            let mut header_parts = Vec::new();
            if block.is_far {
                header_parts.push("far".to_string());
            }
            if block.is_naked {
                header_parts.push("naked".to_string());
            }
            if block.is_inline {
                header_parts.push("inline".to_string());
            }
            let has_modifier = block.is_far || block.is_naked || block.is_inline;
            let emit_func = if has_modifier {
                code_block_uses_func_keyword(source, span_start, block.name_span)
            } else {
                true
            };
            if emit_func {
                header_parts.push("func".to_string());
            }
            header_parts.push(block.name.clone());
            let mode_contract =
                format_mode_contract_inline(block.mode_contract.a_width, block.mode_contract.i_width);
            if !mode_contract.is_empty() {
                header_parts.push(mode_contract);
            }
            let mut header_str = header_parts.join(" ");
            header_str.push_str(" {");
            let header = arena.text(header_str);
            let header = append_brace_trailing_comment(header, arena, cursor, source, span_start);
            let body = build_stmts_doc(arena, cursor, source, &block.body, span_start, span_end);
            header
                .append(arena.hardline().append(body).nest(INDENT_WIDTH))
                .append(arena.hardline())
                .append(arena.text("}"))
        }
        Item::Statement(stmt) => {
            arena.text(format_stmt_text_in_span(stmt, source, span_start, span_end))
        }
    }
}

/// After emitting a block header like `data {`, check for a trailing comment
/// after the `{` on the same line.
fn append_brace_trailing_comment<'a>(
    header: Doc<'a>,
    arena: &'a Arena<'a>,
    cursor: &mut CommentCursor,
    source: &str,
    span_start: usize,
) -> Doc<'a> {
    // Find the `{` in source and check for trailing comment after it
    if let Some(brace_offset) = source[span_start..].find('{') {
        let brace_pos = span_start + brace_offset + 1;
        if let Some(tc) = cursor.take_trailing(source, brace_pos) {
            return header.append(arena.text(" ")).append(arena.text(tc.to_string()));
        }
    }
    header
}

// ---------------------------------------------------------------------------
// Statement list doc builder (with HLA nesting)
// ---------------------------------------------------------------------------

fn build_stmts_doc<'a>(
    arena: &'a Arena<'a>,
    cursor: &mut CommentCursor,
    source: &str,
    stmts: &[Spanned<Stmt>],
    block_start: usize,
    block_end: usize,
) -> Doc<'a> {
    let nested = nest_hla(stmts);
    let body_start = find_brace_body_start(source, block_start, block_end);
    let body_end = find_brace_body_end(source, block_end);
    let mut entries: Vec<LayoutEntry<'a>> = Vec::new();
    let mut prev_end: Option<usize> = None;

    for (idx, fmt_stmt) in nested.iter().enumerate() {
        let gap_start = prev_end.unwrap_or(body_start);
        entries.extend(collect_standalone_semicolon_lines(
            arena,
            source,
            gap_start,
            fmt_stmt.span().start,
        ));

        // Gap comments before this statement
        let gap = cursor.drain_before(arena, source, fmt_stmt.span().start, prev_end);
        for (doc, span) in gap {
            entries.push(LayoutEntry {
                doc,
                start: span.start,
                end: span.end,
            });
        }

        let next_stmt_start = nested
            .get(idx + 1)
            .map(|next| next.span().start)
            .unwrap_or(body_end);
        let trailing_semicolons =
            count_leading_gap_semicolons(source, fmt_stmt.span_end(), next_stmt_start);
        let mut stmt_doc = build_fmt_stmt_doc(arena, cursor, source, fmt_stmt);
        if trailing_semicolons > 0 {
            stmt_doc = stmt_doc.append(arena.text(";".repeat(trailing_semicolons)));
        }
        let layout_start = fmt_stmt.span().start;
        let layout_end = fmt_stmt.span_end();

        // Trailing comment
        if let Some(tc) = cursor.take_trailing(source, fmt_stmt.span_end()) {
            entries.push(LayoutEntry {
                doc: stmt_doc.append(arena.text(" ")).append(arena.text(tc.to_string())),
                start: layout_start,
                end: layout_end,
            });
        } else {
            entries.push(LayoutEntry {
                doc: stmt_doc,
                start: layout_start,
                end: layout_end,
            });
        }

        prev_end = Some(fmt_stmt.span_end());
    }

    let trailing_gap_start = prev_end.unwrap_or(body_start);
    entries.extend(collect_standalone_semicolon_lines(
        arena,
        source,
        trailing_gap_start,
        body_end,
    ));

    // Trailing comments before closing brace
    let gap = cursor.drain_before(arena, source, body_end, prev_end);
    for (doc, span) in gap {
        entries.push(LayoutEntry {
            doc,
            start: span.start,
            end: span.end,
        });
    }

    // Preserve one blank line before `}` when source had any blank-line gap there.
    if prev_end.is_some_and(|pe| has_blank_line(source, pe, body_end)) {
        entries.push(LayoutEntry {
            doc: arena.nil(),
            start: body_end,
            end: body_end,
        });
    }

    join_layout_entries(arena, source, entries)
}

fn build_fmt_stmt_doc<'a>(
    arena: &'a Arena<'a>,
    cursor: &mut CommentCursor,
    source: &str,
    fmt_stmt: &FmtStmt,
) -> Doc<'a> {
    match fmt_stmt {
        FmtStmt::Plain(stmt) => match &stmt.node {
            Stmt::Hla(HlaStmt::PrefixConditional {
                skip_mnemonic,
                form,
                body,
                else_body,
            }) => {
                let condition = prefix_condition_text(skip_mnemonic, *form);
                let snippet = &source[stmt.span.start.min(source.len())..stmt.span.end.min(source.len())];
                let single_line = !snippet.contains('\n') && !snippet.contains('\r');

                if single_line {
                    let body_text = body
                        .iter()
                        .map(|s| format_stmt_text_in_span(&s.node, source, s.span.start, s.span.end))
                        .collect::<Vec<_>>()
                        .join(" ");
                    let mut text = format!("{condition}{{ {body_text} }}");
                    if let Some(else_body) = else_body {
                        let else_text = else_body
                            .iter()
                            .map(|s| format_stmt_text_in_span(&s.node, source, s.span.start, s.span.end))
                            .collect::<Vec<_>>()
                            .join(" ");
                        text.push_str(&format!(" else {{ {else_text} }}"));
                    }
                    arena.text(text)
                } else {
                    let body_entries = body
                        .iter()
                        .map(|s| LayoutEntry {
                            doc: arena.text(format_stmt_text_in_span(
                                &s.node,
                                source,
                                s.span.start,
                                s.span.end,
                            )),
                            start: s.span.start,
                            end: s.span.end,
                        })
                        .collect::<Vec<_>>();
                    let mut doc = arena
                        .text(format!("{condition} {{"))
                        .append(arena.hardline().append(join_layout_entries(arena, source, body_entries)).nest(INDENT_WIDTH))
                        .append(arena.hardline())
                        .append(arena.text("}"));
                    if let Some(else_body) = else_body {
                        let else_entries = else_body
                            .iter()
                            .map(|s| LayoutEntry {
                                doc: arena.text(format_stmt_text_in_span(
                                    &s.node,
                                    source,
                                    s.span.start,
                                    s.span.end,
                                )),
                                start: s.span.start,
                                end: s.span.end,
                            })
                            .collect::<Vec<_>>();
                        doc = doc
                            .append(arena.text(" else {"))
                            .append(arena.hardline().append(join_layout_entries(arena, source, else_entries)).nest(INDENT_WIDTH))
                            .append(arena.hardline())
                            .append(arena.text("}"));
                    }
                    doc
                }
            }
            _ => arena.text(format_stmt_text_in_span(
                &stmt.node,
                source,
                stmt.span.start,
                stmt.span.end,
            )),
        },
        FmtStmt::HlaBlock { open, body, close } => {
            let open_doc = arena.text("{");
            // Trailing comment on the `{` line
            let (open_doc, open_has_trailing_comment) =
                if let Some(tc) = cursor.take_trailing(source, open.span.end) {
                    (
                        open_doc.append(arena.text(" ")).append(arena.text(tc.to_string())),
                        true,
                    )
                } else {
                    (open_doc, false)
                };

            // Build body entries
            let mut entries: Vec<LayoutEntry<'a>> = Vec::new();
            let mut prev_end: Option<usize> = Some(open.span.end);

            for child in body {
                let gap = cursor.drain_before(arena, source, child.span().start, prev_end);
                for (doc, span) in gap {
                    entries.push(LayoutEntry {
                        doc,
                        start: span.start,
                        end: span.end,
                    });
                }

                let child_doc = build_fmt_stmt_doc(arena, cursor, source, child);
                let layout_start = child.span().start;
                let layout_end = child.span_end();
                if let Some(tc) = cursor.take_trailing(source, child.span_end()) {
                    entries.push(LayoutEntry {
                        doc: child_doc.append(arena.text(" ")).append(arena.text(tc.to_string())),
                        start: layout_start,
                        end: layout_end,
                    });
                } else {
                    entries.push(LayoutEntry {
                        doc: child_doc,
                        start: layout_start,
                        end: layout_end,
                    });
                }

                prev_end = Some(child.span_end());
            }

            // Gap comments before close
            let gap = cursor.drain_before(arena, source, close.span.start, prev_end);
            for (doc, span) in gap {
                entries.push(LayoutEntry {
                    doc,
                    start: span.start,
                    end: span.end,
                });
            }
            if prev_end.is_some_and(|pe| has_blank_line(source, pe, close.span.start)) {
                entries.push(LayoutEntry {
                    doc: arena.nil(),
                    start: close.span.start,
                    end: close.span.start,
                });
            }

            let close_doc = arena.text(format_stmt_text_in_span(
                &close.node,
                source,
                close.span.start,
                close.span.end,
            ));

            if entries.is_empty() && !open_has_trailing_comment {
                // Compact truly empty blocks to `{}` forms like `{} always`.
                return open_doc.append(close_doc);
            }

            let body_doc = join_layout_entries(arena, source, entries);

            open_doc
                .append(arena.hardline().append(body_doc).nest(INDENT_WIDTH))
                .append(arena.hardline())
                .append(close_doc)
        }
    }
}

// ---------------------------------------------------------------------------
// Spanned body (data entries) doc builder
// ---------------------------------------------------------------------------

fn build_spanned_body<'a, T, F>(
    arena: &'a Arena<'a>,
    cursor: &mut CommentCursor,
    source: &str,
    items: &[Spanned<T>],
    _block_start: usize,
    block_end: usize,
    format_fn: F,
) -> Doc<'a>
where
    F: Fn(&T, usize, usize) -> String,
{
    let body_end = find_brace_body_end(source, block_end);
    let mut entries: Vec<LayoutEntry<'a>> = Vec::new();
    let mut prev_end: Option<usize> = None;

    for item in items {
        let gap = cursor.drain_before(arena, source, item.span.start, prev_end);
        for (doc, span) in gap {
            entries.push(LayoutEntry {
                doc,
                start: span.start,
                end: span.end,
            });
        }

        let entry_doc = arena.text(format_fn(&item.node, item.span.start, item.span.end));

        if let Some(tc) = cursor.take_trailing(source, item.span.end) {
            entries.push(LayoutEntry {
                doc: entry_doc.append(arena.text(" ")).append(arena.text(tc.to_string())),
                start: item.span.start,
                end: item.span.end,
            });
        } else {
            entries.push(LayoutEntry {
                doc: entry_doc,
                start: item.span.start,
                end: item.span.end,
            });
        }

        prev_end = Some(item.span.end);
    }

    // Trailing comments before closing brace
    let gap = cursor.drain_before(arena, source, body_end, prev_end);
    for (doc, span) in gap {
        entries.push(LayoutEntry {
            doc,
            start: span.start,
            end: span.end,
        });
    }
    if prev_end.is_some_and(|pe| has_blank_line(source, pe, body_end)) {
        entries.push(LayoutEntry {
            doc: arena.nil(),
            start: body_end,
            end: body_end,
        });
    }

    join_layout_entries(arena, source, entries)
}

// ---------------------------------------------------------------------------
// Source position helpers
// ---------------------------------------------------------------------------

fn extract_directive_operand_from_source(
    source: &str,
    span_start: usize,
    span_end: usize,
    keyword: &str,
) -> Option<String> {
    if span_start >= span_end || span_start >= source.len() {
        return None;
    }
    let end = span_end.min(source.len());
    let snippet = source[span_start..end].trim();
    if let Some(rest) = snippet.strip_prefix(keyword) {
        let rest = rest.trim();
        if rest.is_empty() {
            return None;
        }
        return Some(rest.to_string());
    }
    if !snippet.is_empty() && snippet != keyword && !snippet.contains('\n') && !snippet.contains('\r') {
        // Some parser spans for directive statements cover only the operand expression.
        return Some(snippet.to_string());
    }

    // If span is broader than one token (or one line), scan covered lines
    // and preserve the first `keyword <operand>` spelling found.
    let line_start = source[..span_start.min(source.len())]
        .rfind('\n')
        .map_or(0, |idx| idx + 1);
    let mut line_end = span_end.min(source.len());
    if let Some(idx) = source[line_end..].find('\n') {
        line_end += idx;
    } else {
        line_end = source.len();
    }
    let region = source[line_start..line_end].trim();
    for line in region.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix(keyword) {
            let rest = rest.trim();
            if !rest.is_empty() {
                return Some(rest.to_string());
            }
        }
    }
    None
}

fn extract_image_binary_decl_from_source(
    source: &str,
    span_start: usize,
    span_end: usize,
) -> Option<String> {
    if span_start >= span_end || span_start >= source.len() {
        return None;
    }
    let end = span_end.min(source.len());
    let snippet = source[span_start..end].trim();
    if snippet.is_empty() {
        return None;
    }
    let head = snippet.split_whitespace().next()?;
    if head.eq_ignore_ascii_case("image") || head.eq_ignore_ascii_case("binary") {
        Some(snippet.to_string())
    } else {
        None
    }
}

fn normalize_preserved_numeric_operand_text(operand: &str) -> String {
    let text = operand.trim();
    if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X"))
        && !hex.is_empty()
        && hex.bytes().all(|b| b.is_ascii_hexdigit())
        && let Ok(value) = i64::from_str_radix(hex, 16)
    {
        return format_number(value, NumFmt::Hex(hex.len() as u8));
    }
    if let Some(hex) = text.strip_prefix('$')
        && !hex.is_empty()
        && hex.bytes().all(|b| b.is_ascii_hexdigit())
        && let Ok(value) = i64::from_str_radix(hex, 16)
    {
        return format_number(value, NumFmt::Dollar(hex.len() as u8));
    }
    text.to_string()
}

/// Find the byte offset of the closing `}`.
fn find_brace_body_end(source: &str, block_end: usize) -> usize {
    let region = &source[..block_end.min(source.len())];
    region.rfind('}').unwrap_or(block_end)
}

// ---------------------------------------------------------------------------
// Pure text formatting (no Doc, no comments — reused from before)
// ---------------------------------------------------------------------------

fn format_stmt_text_in_span(stmt: &Stmt, source: &str, span_start: usize, span_end: usize) -> String {
    match stmt {
        // Preserve original address operand spelling (hex prefix, width, expression form)
        // instead of collapsing to evaluated decimal.
        Stmt::Address(value) => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "address")
            {
                format!("address {}", normalize_preserved_numeric_operand_text(&operand))
            } else {
                format!("address {value}")
            }
        }
        Stmt::Align { boundary, offset } => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "align")
            {
                format!("align {}", normalize_preserved_numeric_operand_text(&operand))
            } else if *offset == 0 {
                format!("align {boundary}")
            } else {
                format!("align {boundary} + {offset}")
            }
        }
        Stmt::Nocross(_value) => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "nocross")
            {
                format!("nocross {}", normalize_preserved_numeric_operand_text(&operand))
            } else {
                "nocross".to_string()
            }
        }
        Stmt::Call(call) => {
            // `call` is optional in call syntax; preserve omitted form (`far target`)
            // when it was not present in the original source spelling.
            if stmt_starts_with_keyword(source, span_start, span_end, "call") {
                if call.is_far {
                    format!("call far {}", call.target)
                } else {
                    format!("call {}", call.target)
                }
            } else if call.is_far {
                format!("far {}", call.target)
            } else {
                call.target.clone()
            }
        }
        _ => format_stmt_text(stmt),
    }
}

fn stmt_starts_with_keyword(source: &str, span_start: usize, span_end: usize, keyword: &str) -> bool {
    if span_start >= span_end || span_start >= source.len() {
        return false;
    }
    let end = span_end.min(source.len());
    let snippet = &source[span_start..end];
    let bytes = snippet.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() && (bytes[i].is_ascii_whitespace() || bytes[i] == b';') {
        i += 1;
    }
    let start = i;
    while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
        i += 1;
    }
    start < i && snippet[start..i].eq_ignore_ascii_case(keyword)
}

fn format_stmt_text(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Segment(s) => format!("segment {}", s.name),
        Stmt::Label(label) => format!("{}:", label.name),
        Stmt::Var(var) => format_var(var),
        Stmt::DataBlock(block) => {
            let mut out = "data {\n".to_string();
            for cmd in &block.commands {
                let _ = writeln!(out, "{INDENT_STR}{}", format_data_command(&cmd.node));
            }
            out.push('}');
            out
        }
        Stmt::Address(value) => format!("address {value}"),
        Stmt::Align { boundary, offset } => {
            if *offset == 0 {
                format!("align {boundary}")
            } else {
                format!("align {boundary} + {offset}")
            }
        }
        Stmt::Nocross(value) => format!("nocross {value}"),
        Stmt::Instruction(instr) => format_instruction(instr),
        Stmt::Call(call) => {
            if call.is_far {
                format!("call far {}", call.target)
            } else {
                format!("call {}", call.target)
            }
        }
        Stmt::ModeSet { a_width, i_width } => {
            format_mode_contract_inline(*a_width, *i_width)
        }
        Stmt::ModeScopedBlock {
            a_width,
            i_width,
            body,
        } => {
            let mut parts = Vec::new();
            let mode = format_mode_contract_inline(*a_width, *i_width);
            if !mode.is_empty() {
                parts.push(mode);
            }
            parts.push("{".to_string());
            let mut out = parts.join(" ");
            out.push('\n');
            for stmt in body {
                let _ = writeln!(out, "{INDENT_STR}{}", format_stmt_text(&stmt.node));
            }
            out.push('}');
            out
        }
        Stmt::SwapAB => "b><a".to_string(),
        Stmt::Hla(stmt) => format_hla_stmt(stmt),
        Stmt::Empty => String::new(),
    }
}

fn format_instruction(instr: &Instruction) -> String {
    match &instr.operand {
        None => instr.mnemonic.clone(),
        Some(Operand::Immediate { expr, .. }) => {
            format!("{} #{}", instr.mnemonic, format_expr(expr))
        }
        Some(Operand::Value {
            expr,
            force_far,
            index,
            addr_mode,
        }) => {
            let value = format_address_operand(expr, *index, *addr_mode);
            if *force_far {
                format!("{} far {}", instr.mnemonic, value)
            } else {
                format!("{} {}", instr.mnemonic, value)
            }
        }
        Some(Operand::Auto { expr }) => {
            // Reconstruct HLA register-load sugar: ldx expr → x = expr
            match instr.mnemonic.as_str() {
                "lda" => format!("a = {}", format_expr(expr)),
                "ldx" => format!("x = {}", format_expr(expr)),
                "ldy" => format!("y = {}", format_expr(expr)),
                "sta" => format!("{} = a", format_expr(expr)),
                "stx" => format!("{} = x", format_expr(expr)),
                "sty" => format!("{} = y", format_expr(expr)),
                _ => format!("{} {}", instr.mnemonic, format_expr(expr)),
            }
        }
    }
}

fn format_var(var: &k816_core::ast::VarDecl) -> String {
    let mut out = format!("var {}", var.name);
    if let Some(width) = var.data_width {
        out.push(':');
        out.push_str(match width {
            k816_core::ast::DataWidth::Byte => "byte",
            k816_core::ast::DataWidth::Word => "word",
            k816_core::ast::DataWidth::Far => "far",
        });
    }
    if let Some(fields) = &var.symbolic_subscript_fields {
        out.push('[');
        let rendered = fields
            .iter()
            .map(|field| {
                let mut value = format!(".{}", field.name);
                if let Some(count) = &field.count {
                    value.push('[');
                    value.push_str(&format_expr(count));
                    value.push(']');
                }
                if let Some(width) = field.data_width {
                    value.push(':');
                    value.push_str(match width {
                        k816_core::ast::DataWidth::Byte => "byte",
                        k816_core::ast::DataWidth::Word => "word",
                        k816_core::ast::DataWidth::Far => "far",
                    });
                }
                value
            })
            .collect::<Vec<_>>()
            .join(", ");
        out.push_str(&rendered);
        out.push(']');
    } else if let Some(array_len) = &var.array_len {
        out.push('[');
        out.push_str(&format_expr(array_len));
        out.push(']');
    }
    if let Some(expr) = &var.initializer {
        out.push_str(" = ");
        out.push_str(&format_expr(expr));
    }
    out
}

fn format_const(const_decl: &k816_core::ast::ConstDecl) -> String {
    format!("const {}", format_const_binding(const_decl))
}

fn format_const_group(consts: &[k816_core::ast::ConstDecl]) -> String {
    let body = consts
        .iter()
        .map(format_const_binding)
        .collect::<Vec<_>>()
        .join(", ");
    format!("const {body}")
}

fn format_const_binding(const_decl: &k816_core::ast::ConstDecl) -> String {
    format!(
        "{} = {}",
        const_decl.name,
        format_expr(&const_decl.initializer)
    )
}

fn format_data_command(command: &DataCommand) -> String {
    match command {
        DataCommand::Align(value) => format!("align {value}"),
        DataCommand::Address(value) => format!("address {value}"),
        DataCommand::Nocross(value) => format!("nocross {value}"),
        DataCommand::Bytes(values) => values
            .iter()
            .map(|v| format!("${v:02X}"))
            .collect::<Vec<_>>()
            .join(" "),
        DataCommand::Convert { kind, args } => {
            let args = args
                .iter()
                .map(format_data_arg)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{kind}({args})")
        }
        DataCommand::Ignored => "<ignored>".to_string(),
    }
}

fn format_data_command_in_span(
    command: &DataCommand,
    source: &str,
    span_start: usize,
    span_end: usize,
) -> String {
    match command {
        DataCommand::Address(value) => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "address")
            {
                format!("address {}", normalize_preserved_numeric_operand_text(&operand))
            } else {
                format!("address {value}")
            }
        }
        DataCommand::Align(value) => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "align")
            {
                format!("align {}", normalize_preserved_numeric_operand_text(&operand))
            } else {
                format!("align {value}")
            }
        }
        DataCommand::Nocross(_value) => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "nocross")
            {
                format!("nocross {}", normalize_preserved_numeric_operand_text(&operand))
            } else {
                "nocross".to_string()
            }
        }
        _ => format_data_command(command),
    }
}

fn format_data_arg(arg: &DataArg) -> String {
    match arg {
        DataArg::Int(value) => value.to_string(),
        DataArg::Str(value) => format!("\"{}\"", value.replace('"', "\\\"")),
    }
}

#[cfg(test)]
mod tests {
    use super::format_ast;
    use k816_core::parser;
    use k816_core::span::SourceId;

    #[test]
    fn keeps_function_mode_contract_in_header() {
        let source = "func main @a8 @i8 {\n  nop\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("func main @a8 @i8 {"));
    }

    #[test]
    fn keeps_module_mode_defaults_at_file_top() {
        let source = "@a16\n@i16\n\nfunc main {\n  nop\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.starts_with("@a16\n@i16\n"));
    }

    #[test]
    fn keeps_far_call_spelling() {
        let source = "func main {\n  call far target\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("call far target"));
    }

    #[test]
    fn keeps_far_call_without_call_keyword_when_omitted() {
        let source = "func main {\n  far target\n  call far explicit\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("\n    far target\n"));
        assert!(formatted.contains("\n    call far explicit\n"));
    }

    #[test]
    fn preserves_top_level_image_and_binary_declarations() {
        let source = "image SpriteSheet = \"sprites.bmp\"\nimage \"RawSprite\" = \"raw.bmp\"\nbinary Blob = \"payload.bin\"\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("image SpriteSheet = \"sprites.bmp\""));
        assert!(formatted.contains("image \"RawSprite\" = \"raw.bmp\""));
        assert!(formatted.contains("binary Blob = \"payload.bin\""));
    }

    #[test]
    fn uses_four_space_default_indent() {
        let source = "func main {\n  nop\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("\n    nop\n"));
    }

    #[test]
    fn keeps_address_operand_spelling() {
        let source = "func main {\n  address 0x2000\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("address 0x2000"));
    }

    #[test]
    fn reformats_five_digit_hex_address_to_six_digits() {
        let source = "func main {\n  address 0x10000\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("address 0x010000"));
    }

    #[test]
    fn extracts_operand_when_span_points_to_expression_only() {
        let source = "address 0x2000";
        let start = "address ".len();
        let end = source.len();
        let operand = super::extract_directive_operand_from_source(source, start, end, "address");
        assert_eq!(operand.as_deref(), Some("0x2000"));
    }

    #[test]
    fn compacts_empty_hla_blocks() {
        let source = "func main {\n  {} always\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("\n    {} always\n"));
    }

    #[test]
    fn preserves_single_blank_lines_and_compacts_runs() {
        let source = "func main {\n  nop\n\n  lda #1\n\n\n  tax\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        let normalized = format!(
            "{}\n",
            formatted
                .lines()
                .map(str::trim_end)
                .collect::<Vec<_>>()
                .join("\n")
        );
        assert!(normalized.contains("\n    nop\n\n    lda #1\n\n    tax\n"));
    }

    #[test]
    fn preserves_blank_line_when_source_line_has_only_spaces() {
        let source = "func main {\n  nop\n    \n  lda #1\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        let normalized = format!(
            "{}\n",
            formatted
                .lines()
                .map(str::trim_end)
                .collect::<Vec<_>>()
                .join("\n")
        );
        assert!(normalized.contains("\n    nop\n\n    lda #1\n"));
    }

    #[test]
    fn keeps_naked_without_func_keyword() {
        let source = "naked handler {\n  nop\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("naked handler {"));
    }

    #[test]
    fn keeps_explicit_naked_func_keyword() {
        let source = "naked func handler {\n  nop\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("naked func handler {"));
    }

    #[test]
    fn keeps_zero_run_literals_with_pow2_width() {
        let source = "func main {\n  lda #00\n  lda #000\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("lda #00"));
        assert!(formatted.contains("lda #0000"));
    }

    #[test]
    fn preserves_char_literals() {
        let source = "func main {\n  a='A'\n  a='0'\n  a='\\n'\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("a = 'A'"));
        assert!(formatted.contains("a = '0'"));
        assert!(formatted.contains("a = '\\n'"));
    }

    #[test]
    fn never_emits_trailing_whitespace() {
        let source = "func main {\n  nop\n\n  lda #1\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(
            formatted
                .lines()
                .all(|line| !line.ends_with(' ') && !line.ends_with('\t'))
        );
    }

    #[test]
    fn preserves_same_line_statements() {
        let source = "func main {\n  @a8 @i8\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("\n    @a8 @i8\n"));
    }

    #[test]
    fn preserves_explicit_semicolons() {
        let source = "var zp = 0x80; // line comment\n\nfunc main {\n  ;\n  a=0;\n  x=a; y=a;;\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("var zp = 0x80;"));
        assert!(formatted.contains("// line comment"));
        assert!(formatted.contains("\n    ;\n"));
        assert!(formatted.contains("\n    a = 0;\n"));
        assert!(formatted.contains("\n    x = a; y = a;;\n"));
    }

    #[test]
    fn preserves_comma_separated_const_declarations() {
        let source = "const A = 1, B = 2\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("const A = 1, B = 2"));
    }

    #[test]
    fn preserves_packed_address_operators() {
        let source = "data bytes {\n  &&ptr\n  &&&ptr\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("\n    &&ptr\n"));
        assert!(formatted.contains("\n    &&&ptr\n"));
    }

    #[test]
    fn preserves_bracketed_eval_ident_in_data_entries() {
        let source = "data text_data {\n  evaluator [ SCALE = 2 ]\n  [SCALE]\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("evaluator [ SCALE = 2 ]"));
        assert!(formatted.contains("\n    [SCALE]\n"));
    }

    #[test]
    fn preserves_bracketed_eval_ident_in_register_assignments() {
        let source = "[ A = 20, B = 30 ]\nfunc main {\n  a=[A]\n  x=[B]\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("\n    a = [A]\n"));
        assert!(formatted.contains("\n    x = [B]\n"));
    }

    #[test]
    fn preserves_data_placement_directive_operands() {
        let source = "data aligned_offs {\n  align 256 + 8\n  5 6 7 8\n}\n\ndata fixed_addr {\n  address 0x5000\n  9 10\n}\n\ndata no_cross {\n  nocross\n  11 12 13\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("\n    align 256 + 8\n"));
        assert!(formatted.contains("\n    address 0x5000\n"));
        assert!(formatted.contains("\n    nocross\n"));
        assert!(!formatted.contains("align 264"));
        assert!(!formatted.contains("address 20480"));
        assert!(!formatted.contains("nocross 256"));
    }

    #[test]
    fn does_not_fold_flag_branch_goto_to_symbolic_form() {
        let source = "func main {\n  c-? goto target\n  < goto target\n  v+ goto target\n  <<= goto target\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("\n    c-? goto target\n"));
        assert!(formatted.contains("\n    < goto target\n"));
        assert!(formatted.contains("\n    v+ goto target\n"));
        assert!(formatted.contains("\n    <<= goto target\n"));
    }

    #[test]
    fn does_not_fold_prefix_flag_forms_or_body() {
        let source = "func main {\n  c-?{ a=1 }\n  <{ a=2 }\n  v+?{ a=3 }\n  >>={ a=4 }\n}\n";
        let ast = parser::parse(SourceId(0), source).expect("source should parse");
        let formatted = format_ast(&ast, source);
        assert!(formatted.contains("c-?{ a = 1 }"));
        assert!(formatted.contains("<{ a = 2 }"));
        assert!(formatted.contains("v+?{ a = 3 }"));
        assert!(formatted.contains(">>={ a = 4 }"));
        assert!(!formatted.contains("prefix("));
        assert!(!formatted.contains("..."));
    }
}

/// Minimum hex digits needed to represent the value (at least 2).
fn min_hex_width(value: i64) -> usize {
    let bits = 64 - (value as u64).leading_zeros().max(1) as usize;
    let bytes = (bits + 7) / 8;
    (bytes * 2).max(2)
}

/// Minimum binary digits needed to represent the value (at least 1).
fn min_bin_width(value: i64) -> usize {
    (64 - (value as u64).leading_zeros().max(1)) as usize
}

/// Round up to the next power of two (1 → 1, 2 → 2, 3 → 4, 5 → 8, ...).
fn ceil_pow2(n: usize) -> usize {
    if n == 0 { return 1; }
    n.next_power_of_two()
}

fn normalized_hex_width(orig_w: usize) -> usize {
    if (5..=6).contains(&orig_w) {
        6
    } else {
        ceil_pow2(orig_w)
    }
}

fn format_number(value: i64, fmt: NumFmt) -> String {
    match fmt {
        NumFmt::Dec => value.to_string(),
        NumFmt::Char => format_char_literal(value),
        NumFmt::Zero(orig_w) => {
            let w = ceil_pow2(orig_w as usize).max(1);
            "0".repeat(w)
        }
        NumFmt::Dollar(orig_w) => {
            let w = normalized_hex_width(orig_w as usize).max(min_hex_width(value));
            format!("${:0>w$X}", value, w = w)
        }
        NumFmt::Hex(orig_w) => {
            let w = normalized_hex_width(orig_w as usize).max(min_hex_width(value));
            format!("0x{:0>w$X}", value, w = w)
        }
        NumFmt::Percent(orig_w) => {
            let w = ceil_pow2(orig_w as usize).max(min_bin_width(value));
            format!("%{:0>w$b}", value, w = w)
        }
        NumFmt::Bin(orig_w) => {
            let w = ceil_pow2(orig_w as usize).max(min_bin_width(value));
            format!("0b{:0>w$b}", value, w = w)
        }
    }
}

fn format_char_literal(value: i64) -> String {
    let Ok(codepoint) = u32::try_from(value) else {
        return value.to_string();
    };
    let Some(ch) = char::from_u32(codepoint) else {
        return value.to_string();
    };

    let escaped = match ch {
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        '\0' => "\\0".to_string(),
        c if c.is_control() => return value.to_string(),
        c => c.to_string(),
    };

    format!("'{escaped}'")
}

fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::Number(value, fmt) => format_number(*value, *fmt),
        Expr::Ident(value) => value.clone(),
        Expr::IdentSpanned { name, .. } => name.clone(),
        Expr::EvalText(value) => format!("[{value}]"),
        Expr::Index { base, index } => format!("{}[{}]", format_expr(base), format_expr(index)),
        Expr::Binary { op, lhs, rhs } => {
            let op = match op {
                k816_core::ast::ExprBinaryOp::Add => "+",
                k816_core::ast::ExprBinaryOp::Sub => "-",
                k816_core::ast::ExprBinaryOp::Mul => "*",
            };
            format!("{} {op} {}", format_expr(lhs), format_expr(rhs))
        }
        Expr::Unary { op, expr } => {
            match op {
                k816_core::ast::ExprUnaryOp::LowByte => format!("&<{}", format_expr(expr)),
                k816_core::ast::ExprUnaryOp::HighByte => format!("&>{}", format_expr(expr)),
                k816_core::ast::ExprUnaryOp::WordLittleEndian => format!("&&{}", format_expr(expr)),
                k816_core::ast::ExprUnaryOp::FarLittleEndian => {
                    format!("&&&{}", format_expr(expr))
                }
                k816_core::ast::ExprUnaryOp::EvalBracketed => format!("[{}]", format_expr(expr)),
            }
        }
        Expr::TypedView { expr, width } => {
            let suffix = match width {
                k816_core::ast::DataWidth::Byte => ":byte",
                k816_core::ast::DataWidth::Word => ":word",
                k816_core::ast::DataWidth::Far => ":far",
            };
            format!("{}{suffix}", format_expr(expr))
        }
    }
}

fn format_named_data_entry(entry: &NamedDataEntry) -> String {
    match entry {
        NamedDataEntry::Segment(segment) => format!("segment {}", segment.name),
        NamedDataEntry::Label(name) => format!("{name}:"),
        NamedDataEntry::Address(value) => format!("address {value}"),
        NamedDataEntry::Align(value) => format!("align {value}"),
        NamedDataEntry::Nocross(value) => format!("nocross {value}"),
        NamedDataEntry::Bytes(values) => {
            values.iter().map(format_expr).collect::<Vec<_>>().join(" ")
        }
        NamedDataEntry::Words(values) => {
            format!(
                "word {}",
                values.iter().map(format_expr).collect::<Vec<_>>().join(" ")
            )
        }
        NamedDataEntry::Fars(values) => {
            format!(
                "far {}",
                values.iter().map(format_expr).collect::<Vec<_>>().join(" ")
            )
        }
        NamedDataEntry::ForEvalRange(range) => format!(
            "for {}={}..{} eval [{}]",
            range.iterator,
            format_expr(&range.start),
            format_expr(&range.end),
            range.eval
        ),
        NamedDataEntry::String(value) => format!("\"{}\"", value.replace('\"', "\\\"")),
        NamedDataEntry::Repeat { count, .. } => format!("repeat {count} {{ ... }}"),
        NamedDataEntry::Code(_) => "code { ... }".to_string(),
        NamedDataEntry::Evaluator(text) => format!("evaluator [{text}]"),
        NamedDataEntry::Charset(value) => format!("charset \"{}\"", value.replace('\"', "\\\"")),
    }
}

fn format_named_data_entry_in_span(
    entry: &NamedDataEntry,
    source: &str,
    span_start: usize,
    span_end: usize,
) -> String {
    match entry {
        NamedDataEntry::Address(value) => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "address")
            {
                format!("address {}", normalize_preserved_numeric_operand_text(&operand))
            } else {
                format!("address {value}")
            }
        }
        NamedDataEntry::Align(value) => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "align")
            {
                format!("align {}", normalize_preserved_numeric_operand_text(&operand))
            } else {
                format!("align {value}")
            }
        }
        NamedDataEntry::Nocross(_value) => {
            if let Some(operand) =
                extract_directive_operand_from_source(source, span_start, span_end, "nocross")
            {
                format!("nocross {}", normalize_preserved_numeric_operand_text(&operand))
            } else {
                "nocross".to_string()
            }
        }
        _ => format_named_data_entry(entry),
    }
}

fn format_hla_stmt(stmt: &HlaStmt) -> String {
    match stmt {
        HlaStmt::RegisterAssign { register, rhs } => {
            format!(
                "{} = {}",
                format_hla_cpu_register(*register),
                format_hla_operand_expr(rhs)
            )
        }
        HlaStmt::RegisterStore { dest, src } => {
            format!(
                "{} = {}",
                format_hla_operand_expr(dest),
                format_hla_cpu_register(*src)
            )
        }
        HlaStmt::RegisterTransfer { dest, src } => format!(
            "{} = {}",
            format_hla_cpu_register(*dest),
            format_hla_cpu_register(*src)
        ),
        HlaStmt::AssignmentChain { idents, tail_expr } => {
            let mut parts = idents.clone();
            if let Some(expr) = tail_expr {
                parts.push(format_hla_operand_expr(expr));
            }
            parts.join(" = ")
        }
        HlaStmt::AccumulatorAlu { op, rhs } => {
            let op = match op {
                k816_core::ast::HlaAluOp::Add => "+",
                k816_core::ast::HlaAluOp::Sub => "-",
                k816_core::ast::HlaAluOp::And => "&",
                k816_core::ast::HlaAluOp::Or => "|",
                k816_core::ast::HlaAluOp::Xor => "^",
            };
            format!("a{op}{}", format_hla_operand_expr(rhs))
        }
        HlaStmt::AccumulatorBitTest { rhs } => format!("a&?{}", format_hla_operand_expr(rhs)),
        HlaStmt::IndexCompare { register, rhs } => {
            let register = match register {
                k816_core::ast::IndexRegister::X => "x",
                k816_core::ast::IndexRegister::Y => "y",
            };
            format!("{register}?{}", format_hla_operand_expr(rhs))
        }
        HlaStmt::IncDec { op, target } => {
            let suffix = match op {
                k816_core::ast::HlaIncDecOp::Inc => "++",
                k816_core::ast::HlaIncDecOp::Dec => "--",
            };
            let target = match target {
                k816_core::ast::HlaIncDecTarget::Register(k816_core::ast::IndexRegister::X) => {
                    "x".to_string()
                }
                k816_core::ast::HlaIncDecTarget::Register(k816_core::ast::IndexRegister::Y) => {
                    "y".to_string()
                }
                k816_core::ast::HlaIncDecTarget::Address(address) => format_hla_operand_expr(address),
            };
            format!("{target}{suffix}")
        }
        HlaStmt::ShiftRotate { op, target } => {
            let suffix = match op {
                k816_core::ast::HlaShiftOp::Asl => "<<",
                k816_core::ast::HlaShiftOp::Lsr => ">>",
                k816_core::ast::HlaShiftOp::Rol => "<<<",
                k816_core::ast::HlaShiftOp::Ror => ">>>",
            };
            let target = match target {
                k816_core::ast::HlaShiftTarget::Accumulator => "a".to_string(),
                k816_core::ast::HlaShiftTarget::Address(address) => format_hla_operand_expr(address),
            };
            format!("{target}{suffix}")
        }
        HlaStmt::FlagSet { flag, set } => {
            let flag = match flag {
                k816_core::ast::HlaFlag::Carry => "c",
                k816_core::ast::HlaFlag::Decimal => "d",
                k816_core::ast::HlaFlag::Interrupt => "i",
                k816_core::ast::HlaFlag::Overflow => "v",
            };
            let sign = if *set { "+" } else { "-" };
            format!("{flag}{sign}")
        }
        HlaStmt::StackOp { target, push } => {
            let target = match target {
                k816_core::ast::HlaStackTarget::A => "a",
                k816_core::ast::HlaStackTarget::P => "p",
            };
            let suffix = if *push { "!!" } else { "??" };
            format!("{target}{suffix}")
        }
        HlaStmt::Goto {
            target,
            indirect,
            far,
        } => {
            let target = if *indirect {
                format!("({})", format_expr(target))
            } else {
                format_expr(target)
            };
            if *far {
                format!("far goto {target}")
            } else {
                format!("goto {target}")
            }
        }
        HlaStmt::BranchGoto {
            mnemonic,
            target,
            form,
        } => {
            let condition = match form {
                HlaBranchForm::FlagQuestion => mnemonic_to_flag_question(mnemonic)
                    .unwrap_or(mnemonic.as_str())
                    .to_string(),
                HlaBranchForm::FlagPlain => mnemonic_to_flag_plain(mnemonic)
                    .unwrap_or(mnemonic.as_str())
                    .to_string(),
                HlaBranchForm::Symbolic => mnemonic_to_condition(mnemonic).to_string(),
            };
            format!("{condition} goto {}", format_expr(target))
        }
        HlaStmt::Return { interrupt } => {
            if *interrupt {
                "return_i".to_string()
            } else {
                "return".to_string()
            }
        }
        HlaStmt::XAssignImmediate { rhs } => format!("x = {}", format_expr(rhs)),
        HlaStmt::XIncrement => "x++".to_string(),
        HlaStmt::StoreFromA { dests, rhs, .. } => {
            let dest_chain = dests.join(" = ");
            format!("{dest_chain} = a = {}", format_hla_rhs(rhs))
        }
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => format!("{{ a&?{symbol} }} n-?"),
        HlaStmt::ConditionSeed { lhs, rhs } => {
            let lhs = match lhs {
                k816_core::ast::HlaRegister::A => "a",
            };
            format!("{lhs}?{}", format_hla_operand_expr(rhs))
        }
        HlaStmt::DoOpen => "{".to_string(),
        HlaStmt::DoCloseNFlagClear => "} n-?".to_string(),
        HlaStmt::DoCloseNFlagSet => "} n+?".to_string(),
        HlaStmt::DoCloseWithOp { op } => format!("}} {}", format_hla_op(*op)),
        HlaStmt::DoClose { condition } => format!("}} {}", format_hla_condition(condition)),
        HlaStmt::DoCloseAlways => "} always".to_string(),
        HlaStmt::DoCloseNever => "} never".to_string(),
        HlaStmt::DoCloseBranch { mnemonic } => format!("}} {mnemonic}"),
        HlaStmt::LoopBreak { mnemonic } => {
            if mnemonic == "bra" {
                "break".to_string()
            } else {
                format!("{} break", mnemonic_to_condition(mnemonic))
            }
        }
        HlaStmt::LoopRepeat { mnemonic } => {
            if mnemonic == "bra" {
                "repeat".to_string()
            } else {
                format!("{} repeat", mnemonic_to_condition(mnemonic))
            }
        }
        HlaStmt::NeverBlock { .. } => "never { ... }".to_string(),
        HlaStmt::RepeatNop(n) => {
            if *n == 1 {
                "*".to_string()
            } else {
                format!("* {n}")
            }
        }
        HlaStmt::PrefixConditional {
            skip_mnemonic,
            form,
            else_body,
            ..
        } => {
            let condition = prefix_condition_text(skip_mnemonic, *form);
            if else_body.is_some() {
                format!("{condition}{{ ... }} else {{ ... }}")
            } else {
                format!("{condition}{{ ... }}")
            }
        }
    }
}

fn format_hla_cpu_register(register: k816_core::ast::HlaCpuRegister) -> &'static str {
    match register {
        k816_core::ast::HlaCpuRegister::A => "a",
        k816_core::ast::HlaCpuRegister::B => "b",
        k816_core::ast::HlaCpuRegister::C => "c",
        k816_core::ast::HlaCpuRegister::D => "d",
        k816_core::ast::HlaCpuRegister::S => "s",
        k816_core::ast::HlaCpuRegister::X => "x",
        k816_core::ast::HlaCpuRegister::Y => "y",
    }
}

fn format_hla_operand_expr(operand: &k816_core::ast::HlaOperandExpr) -> String {
    format_address_operand(&operand.expr, operand.index, operand.addr_mode)
}

fn format_hla_rhs(rhs: &HlaRhs) -> String {
    match rhs {
        // HLA syntax never uses explicit `#` — addressing mode is semantic.
        HlaRhs::Immediate(expr) => format_expr(expr),
        HlaRhs::Value {
            expr,
            index,
            addr_mode,
        } => format_address_operand(expr, *index, *addr_mode),
    }
}

fn format_hla_condition(condition: &HlaCondition) -> String {
    let lhs = match condition.lhs {
        k816_core::ast::HlaRegister::A => "a",
    };
    let op = format_hla_op(condition.op);
    match &condition.rhs {
        Some(rhs) => format!("{lhs}?{} {op}", format_expr(rhs)),
        None => format!("{lhs}?{op}"),
    }
}

fn format_hla_op(op: HlaCompareOp) -> &'static str {
    match op {
        HlaCompareOp::Eq => "==",
        HlaCompareOp::Ne => "!=",
        HlaCompareOp::Lt => "<",
        HlaCompareOp::Le => "<=",
        HlaCompareOp::Gt => ">",
        HlaCompareOp::Ge => ">=",
    }
}

fn mnemonic_to_condition(mnemonic: &str) -> &str {
    match mnemonic {
        "bcc" => "<",
        "bcs" => ">=",
        "beq" => "==",
        "bne" => "!=",
        "bmi" => "<0",
        "bpl" => ">=0",
        "bvs" => "<<=",
        "bvc" => ">>=",
        _ => mnemonic,
    }
}

fn mnemonic_to_flag_question(mnemonic: &str) -> Option<&'static str> {
    match mnemonic {
        "bcc" => Some("c-?"),
        "bcs" => Some("c+?"),
        "beq" => Some("z+?"),
        "bne" => Some("z-?"),
        "bmi" => Some("n+?"),
        "bpl" => Some("n-?"),
        "bvs" => Some("v+?"),
        "bvc" => Some("v-?"),
        _ => None,
    }
}

fn mnemonic_to_flag_plain(mnemonic: &str) -> Option<&'static str> {
    match mnemonic {
        "bvs" => Some("v+"),
        "bvc" => Some("v-"),
        _ => None,
    }
}

fn invert_branch_mnemonic(mnemonic: &str) -> &str {
    match mnemonic {
        "bcc" => "bcs",
        "bcs" => "bcc",
        "beq" => "bne",
        "bne" => "beq",
        "bmi" => "bpl",
        "bpl" => "bmi",
        "bvs" => "bvc",
        "bvc" => "bvs",
        _ => mnemonic,
    }
}

fn prefix_condition_text(skip_mnemonic: &str, form: HlaBranchForm) -> String {
    let execute_mnemonic = invert_branch_mnemonic(skip_mnemonic);
    match form {
        HlaBranchForm::FlagQuestion => mnemonic_to_flag_question(execute_mnemonic)
            .unwrap_or(execute_mnemonic)
            .to_string(),
        HlaBranchForm::FlagPlain => mnemonic_to_flag_plain(execute_mnemonic)
            .unwrap_or(execute_mnemonic)
            .to_string(),
        HlaBranchForm::Symbolic => mnemonic_to_condition(execute_mnemonic).to_string(),
    }
}

fn format_address_operand(
    expr: &Expr,
    index: Option<k816_core::ast::IndexRegister>,
    addr_mode: OperandAddrMode,
) -> String {
    let expr = format_expr(expr);
    match addr_mode {
        OperandAddrMode::Direct => match index {
            None => expr,
            Some(k816_core::ast::IndexRegister::X) => format!("{expr},x"),
            Some(k816_core::ast::IndexRegister::Y) => format!("{expr},y"),
        },
        OperandAddrMode::Indirect => format!("({expr})"),
        OperandAddrMode::IndexedIndirectX => format!("({expr},x)"),
        OperandAddrMode::IndirectIndexedY => format!("({expr}),y"),
    }
}

// ---------------------------------------------------------------------------
// Trailing comment alignment
// ---------------------------------------------------------------------------

/// Post-process rendered output to align trailing `//` comments to 4-space
/// tab boundaries.
fn align_trailing_comments(output: &mut String) {
    let mut result = String::with_capacity(output.len() + 64);
    for line in output.lines() {
        if let Some(pos) = find_trailing_comment_pos(line) {
            let code = line[..pos].trim_end();
            let comment = &line[pos..];
            let col = code.len();
            // Next 4-column tab stop, with at least 1 space gap
            let target = ((col / 4) + 1) * 4;
            result.push_str(code);
            for _ in 0..(target - col) {
                result.push(' ');
            }
            result.push_str(comment);
        } else {
            result.push_str(line);
        }
        result.push('\n');
    }
    *output = result;
}

/// Find the byte offset of a trailing `//` comment on a line, skipping
/// comment-only lines and `//` inside string literals.
fn find_trailing_comment_pos(line: &str) -> Option<usize> {
    let bytes = line.as_bytes();
    let mut in_string = false;
    let mut has_code = false;
    let mut i = 0;

    while i < bytes.len() {
        if in_string {
            if bytes[i] == b'\\' {
                i += 2;
                continue;
            }
            if bytes[i] == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }

        if bytes[i] == b'"' {
            in_string = true;
            has_code = true;
            i += 1;
            continue;
        }

        if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
            return if has_code { Some(i) } else { None };
        }

        if !bytes[i].is_ascii_whitespace() {
            has_code = true;
        }

        i += 1;
    }

    None
}
