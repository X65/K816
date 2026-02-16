use k816_core::ast::{
    self, DataArg, DataCommand, Expr, File, HlaCompareOp, HlaCondition, HlaRhs, HlaStmt,
    Instruction, Item, NamedDataEntry, NumFmt, Operand, OperandAddrMode, Stmt,
};
use k816_core::span::{Span, Spanned};
use pretty::{Arena, DocAllocator, DocBuilder};
use std::fmt::Write;

type Doc<'a> = DocBuilder<'a, Arena<'a>>;

pub fn format_ast(file: &File, source: &str) -> String {
    let arena = Arena::new();
    let doc = build_file_doc(&arena, file, source);
    let mut output = String::new();
    let _ = doc.render_fmt(usize::MAX, &mut output);
    align_trailing_comments(&mut output);
    output
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
    let region = &source[from..to];
    region.contains("\n\n") || region.contains("\n\r\n")
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

    for item in &file.items {
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
        let item_doc = build_item_doc(arena, &mut cursor, source, &item.node, item.span.start, item.span.end);

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
        Item::EvaluatorBlock(b) => arena.text(format!("[{}]", b.text)),
        Item::Var(v) => arena.text(format_var(v)),
        Item::DataBlock(block) => {
            let header = arena.text("data {");
            let header = append_brace_trailing_comment(header, arena, cursor, source, span_start);
            let body = build_spanned_body(arena, cursor, source, &block.commands, span_start, span_end, |cmd| {
                format_data_command(cmd)
            });
            header
                .append(arena.hardline().append(body).nest(2))
                .append(arena.hardline())
                .append(arena.text("}"))
        }
        Item::NamedDataBlock(block) => {
            let header = arena.text(format!("data {} {{", block.name));
            let header = append_brace_trailing_comment(header, arena, cursor, source, span_start);
            let body = build_spanned_body(arena, cursor, source, &block.entries, span_start, span_end, |entry| {
                format_named_data_entry(entry)
            });
            header
                .append(arena.hardline().append(body).nest(2))
                .append(arena.hardline())
                .append(arena.text("}"))
        }
        Item::CodeBlock(block) => {
            let mut header_str = String::new();
            if block.is_far { header_str.push_str("far "); }
            if block.is_naked { header_str.push_str("naked "); }
            if block.is_inline { header_str.push_str("inline "); }
            header_str.push_str("func ");
            header_str.push_str(&block.name);
            header_str.push_str(" {");
            let header = arena.text(header_str);
            let header = append_brace_trailing_comment(header, arena, cursor, source, span_start);
            let body = build_stmts_doc(arena, cursor, source, &block.body, span_start, span_end);
            header
                .append(arena.hardline().append(body).nest(2))
                .append(arena.hardline())
                .append(arena.text("}"))
        }
        Item::Statement(stmt) => arena.text(format_stmt_text(stmt)),
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
    _block_start: usize,
    block_end: usize,
) -> Doc<'a> {
    let nested = nest_hla(stmts);
    let body_end = find_brace_body_end(source, block_end);
    let mut lines: Vec<Doc<'a>> = Vec::new();
    let mut prev_end: Option<usize> = None;

    for fmt_stmt in &nested {
        // Gap comments before this statement
        let gap = cursor.drain_before(arena, source, fmt_stmt.span().start, prev_end);
        lines.extend(gap.into_iter().map(|(doc, _)| doc));

        let stmt_doc = build_fmt_stmt_doc(arena, cursor, source, fmt_stmt);

        // Trailing comment
        if let Some(tc) = cursor.take_trailing(source, fmt_stmt.span_end()) {
            lines.push(stmt_doc.append(arena.text(" ")).append(arena.text(tc.to_string())));
        } else {
            lines.push(stmt_doc);
        }

        prev_end = Some(fmt_stmt.span_end());
    }

    // Trailing comments before closing brace
    let gap = cursor.drain_before(arena, source, body_end, prev_end);
    lines.extend(gap.into_iter().map(|(doc, _)| doc));

    arena.intersperse(lines, arena.hardline())
}

fn build_fmt_stmt_doc<'a>(
    arena: &'a Arena<'a>,
    cursor: &mut CommentCursor,
    source: &str,
    fmt_stmt: &FmtStmt,
) -> Doc<'a> {
    match fmt_stmt {
        FmtStmt::Plain(stmt) => arena.text(format_stmt_text(&stmt.node)),
        FmtStmt::HlaBlock { open, body, close } => {
            let open_doc = arena.text("{");
            // Trailing comment on the `{` line
            let open_doc = if let Some(tc) = cursor.take_trailing(source, open.span.end) {
                open_doc.append(arena.text(" ")).append(arena.text(tc.to_string()))
            } else {
                open_doc
            };

            // Build body lines
            let mut lines: Vec<Doc<'a>> = Vec::new();
            let mut prev_end: Option<usize> = Some(open.span.end);

            for child in body {
                let gap = cursor.drain_before(arena, source, child.span().start, prev_end);
                lines.extend(gap.into_iter().map(|(doc, _)| doc));

                let child_doc = build_fmt_stmt_doc(arena, cursor, source, child);
                if let Some(tc) = cursor.take_trailing(source, child.span_end()) {
                    lines.push(child_doc.append(arena.text(" ")).append(arena.text(tc.to_string())));
                } else {
                    lines.push(child_doc);
                }

                prev_end = Some(child.span_end());
            }

            // Gap comments before close
            let gap = cursor.drain_before(arena, source, close.span.start, prev_end);
            lines.extend(gap.into_iter().map(|(doc, _)| doc));

            let body_doc = arena.intersperse(lines, arena.hardline());
            let close_doc = arena.text(format_stmt_text(&close.node));

            open_doc
                .append(arena.hardline().append(body_doc).nest(2))
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
    F: Fn(&T) -> String,
{
    let body_end = find_brace_body_end(source, block_end);
    let mut lines: Vec<Doc<'a>> = Vec::new();
    let mut prev_end: Option<usize> = None;

    for item in items {
        let gap = cursor.drain_before(arena, source, item.span.start, prev_end);
        lines.extend(gap.into_iter().map(|(doc, _)| doc));

        let entry_doc = arena.text(format_fn(&item.node));

        if let Some(tc) = cursor.take_trailing(source, item.span.end) {
            lines.push(entry_doc.append(arena.text(" ")).append(arena.text(tc.to_string())));
        } else {
            lines.push(entry_doc);
        }

        prev_end = Some(item.span.end);
    }

    // Trailing comments before closing brace
    let gap = cursor.drain_before(arena, source, body_end, prev_end);
    lines.extend(gap.into_iter().map(|(doc, _)| doc));

    arena.intersperse(lines, arena.hardline())
}

// ---------------------------------------------------------------------------
// Source position helpers
// ---------------------------------------------------------------------------

/// Find the byte offset of the closing `}`.
fn find_brace_body_end(source: &str, block_end: usize) -> usize {
    let region = &source[..block_end.min(source.len())];
    region.rfind('}').unwrap_or(block_end)
}

// ---------------------------------------------------------------------------
// Pure text formatting (no Doc, no comments — reused from before)
// ---------------------------------------------------------------------------

fn format_stmt_text(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Segment(s) => format!("segment {}", s.name),
        Stmt::Label(label) => format!("{}:", label.name),
        Stmt::Var(var) => format_var(var),
        Stmt::DataBlock(block) => {
            let mut out = "data {\n".to_string();
            for cmd in &block.commands {
                let _ = writeln!(out, "  {}", format_data_command(&cmd.node));
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
        Stmt::Call(call) => format!("call {}", call.target),
        Stmt::ModeSet { a_width, i_width } => {
            let mut parts = Vec::new();
            if let Some(w) = a_width {
                parts.push(match w {
                    k816_core::ast::RegWidth::W8 => "@a8",
                    k816_core::ast::RegWidth::W16 => "@a16",
                });
            }
            if let Some(w) = i_width {
                parts.push(match w {
                    k816_core::ast::RegWidth::W8 => "@i8",
                    k816_core::ast::RegWidth::W16 => "@i16",
                });
            }
            parts.join(" ")
        }
        Stmt::ModeScopedBlock {
            a_width,
            i_width,
            body,
        } => {
            let mut parts = Vec::new();
            if let Some(w) = a_width {
                parts.push(match w {
                    k816_core::ast::RegWidth::W8 => "@a8",
                    k816_core::ast::RegWidth::W16 => "@a16",
                });
            }
            if let Some(w) = i_width {
                parts.push(match w {
                    k816_core::ast::RegWidth::W8 => "@i8",
                    k816_core::ast::RegWidth::W16 => "@i16",
                });
            }
            parts.push("{");
            let mut out = parts.join(" ");
            out.push('\n');
            for stmt in body {
                let _ = writeln!(out, "  {}", format_stmt_text(&stmt.node));
            }
            out.push('}');
            out
        }
        Stmt::SwapAB => "b><a".to_string(),
        Stmt::TransferChain(instrs) => instrs
            .iter()
            .map(format_instruction)
            .collect::<Vec<_>>()
            .join(" ; "),
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
    format!(
        "const {} = {}",
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

fn format_data_arg(arg: &DataArg) -> String {
    match arg {
        DataArg::Int(value) => value.to_string(),
        DataArg::Str(value) => format!("\"{}\"", value.replace('"', "\\\"")),
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

fn format_number(value: i64, fmt: NumFmt) -> String {
    match fmt {
        NumFmt::Dec => value.to_string(),
        NumFmt::Dollar(orig_w) => {
            let w = ceil_pow2(orig_w as usize).max(min_hex_width(value));
            format!("${:0>w$X}", value, w = w)
        }
        NumFmt::Hex(orig_w) => {
            let w = ceil_pow2(orig_w as usize).max(min_hex_width(value));
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
            let op = match op {
                k816_core::ast::ExprUnaryOp::LowByte => "&<",
                k816_core::ast::ExprUnaryOp::HighByte => "&>",
            };
            format!("{op}{}", format_expr(expr))
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

fn format_hla_stmt(stmt: &HlaStmt) -> String {
    match stmt {
        HlaStmt::XAssignImmediate { rhs } => format!("x = {}", format_expr(rhs)),
        HlaStmt::XIncrement => "x++".to_string(),
        HlaStmt::StoreFromA { dests, rhs } => {
            let dest_chain = dests.join(" = ");
            format!("{dest_chain} = a = {}", format_hla_rhs(rhs))
        }
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => format!("{{ a&?{symbol} }} n-?"),
        HlaStmt::ConditionSeed { lhs, rhs } => {
            let lhs = match lhs {
                k816_core::ast::HlaRegister::A => "a",
            };
            format!("{lhs}?{}", format_expr(rhs))
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
        HlaStmt::RepeatNop(n) => format!("* {n}"),
        HlaStmt::PrefixConditional {
            skip_mnemonic,
            else_body,
            ..
        } => {
            if else_body.is_some() {
                format!("prefix({skip_mnemonic}) {{ ... }} else {{ ... }}")
            } else {
                format!("prefix({skip_mnemonic}) {{ ... }}")
            }
        }
    }
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
