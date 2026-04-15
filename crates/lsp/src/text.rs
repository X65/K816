use super::ByteRange;

#[derive(Debug)]
pub(super) struct TokenMatch {
    pub(super) text: String,
    pub(super) start: usize,
    pub(super) end: usize,
}

pub(super) fn token_at_offset(text: &str, offset: usize) -> Option<TokenMatch> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return None;
    }

    let mut index = offset.min(bytes.len().saturating_sub(1));
    if !is_ident_byte(bytes[index]) {
        if index > 0 && is_ident_byte(bytes[index - 1]) {
            index -= 1;
        } else {
            return None;
        }
    }

    let mut start = index;
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }

    let mut end = index + 1;
    while end < bytes.len() && is_ident_byte(bytes[end]) {
        end += 1;
    }

    // Skip tokens preceded by a colon — they are address/width modifiers (:abs, :byte, :word),
    // not symbol references.
    if start > 0 && text.as_bytes()[start - 1] == b':' {
        return None;
    }

    Some(TokenMatch {
        text: text[start..end].to_string(),
        start,
        end,
    })
}

/// Detect a colon-prefixed keyword at `offset` (e.g. `:sizeof`, `:offsetof`).
/// Returns a `TokenMatch` whose text includes the leading colon.
pub(super) fn colon_keyword_at_offset(text: &str, offset: usize) -> Option<TokenMatch> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return None;
    }

    let index = offset.min(bytes.len().saturating_sub(1));
    // Allow cursor on the colon itself or on the ident part.
    if !is_ident_byte(bytes[index]) && bytes[index] != b':' {
        return None;
    }

    // Walk to find the ident portion boundaries.
    let mut start = index;
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = index;
    if is_ident_byte(bytes[end]) {
        end += 1;
    }
    while end < bytes.len() && is_ident_byte(bytes[end]) {
        end += 1;
    }

    // The token must be preceded by a colon.
    let colon_start = if start > 0 && bytes[start - 1] == b':' {
        start - 1
    } else if bytes[start] == b':' && start + 1 < bytes.len() && is_ident_byte(bytes[start + 1]) {
        // Cursor is on the colon itself — scan the ident after it.
        let ident_start = start + 1;
        let mut ident_end = ident_start;
        while ident_end < bytes.len() && is_ident_byte(bytes[ident_end]) {
            ident_end += 1;
        }
        return Some(TokenMatch {
            text: text[start..ident_end].to_string(),
            start,
            end: ident_end,
        });
    } else {
        return None;
    };

    Some(TokenMatch {
        text: text[colon_start..end].to_string(),
        start: colon_start,
        end,
    })
}

/// Which segment of a qualified `VAR.field1.field2` token the cursor is on.
#[derive(Debug)]
pub(super) enum QualifiedSegment<'a> {
    /// Cursor is on the variable name (before the first dot).
    Var {
        name: &'a str,
        range_start: usize,
        range_end: usize,
    },
    /// Cursor is on a dot-delimited field segment.
    Field {
        var_name: &'a str,
        /// Cumulative field key from first dot to this segment (e.g. `"message.from"`).
        field_key: &'a str,
        range_start: usize,
        range_end: usize,
    },
}

/// Given a qualified token like `VAR.field1.field2` starting at `token_start` in the
/// document, determine which dot-separated segment the cursor at `offset` is on.
/// Returns `None` for standalone `.field` tokens (no var prefix) or tokens without dots.
pub(super) fn resolve_qualified_segment<'a>(
    token_text: &'a str,
    token_start: usize,
    offset: usize,
) -> Option<QualifiedSegment<'a>> {
    let first_dot = token_text.find('.')?;
    let var_name = &token_text[..first_dot];
    if var_name.is_empty() {
        return None;
    }

    let cursor = offset.saturating_sub(token_start);

    if cursor < first_dot {
        return Some(QualifiedSegment::Var {
            name: var_name,
            range_start: token_start,
            range_end: token_start + first_dot,
        });
    }

    // Walk dot-separated segments to find which one the cursor is in.
    let mut seg_dot = first_dot;
    for segment in token_text[first_dot + 1..].split('.') {
        let seg_end = seg_dot + 1 + segment.len();
        if cursor < seg_end || seg_end == token_text.len() {
            let field_key = &token_text[first_dot + 1..seg_end];
            return Some(QualifiedSegment::Field {
                var_name,
                field_key,
                range_start: token_start + seg_dot,
                range_end: token_start + seg_end,
            });
        }
        seg_dot = seg_end;
    }

    None
}

/// Given a full field key like `"message.from"` and a segment name like `"message"`,
/// return the cumulative key up to and including that segment.
/// E.g. `("a.b.c", "b")` → `Some("a.b")`, `("a.b.c", "c")` → `Some("a.b.c")`.
pub(super) fn cumulative_field_key<'a>(full_key: &'a str, segment: &str) -> Option<&'a str> {
    let mut end = 0;
    for part in full_key.split('.') {
        end += part.len();
        if part == segment {
            return Some(&full_key[..end]);
        }
        end += 1; // skip the dot separator
    }
    None
}

/// Result of resolving a subscript field reference from the AST.
#[derive(Debug)]
pub(super) struct ResolvedFieldRef {
    /// Variable name (e.g. `"TASKS"`).
    pub(super) var_name: String,
    /// Full dot-separated field key without leading dot (e.g. `"message.from"`).
    pub(super) field_key: String,
}

/// Walk the parsed AST to find an `IdentSpanned` that contains `offset` and whose name
/// has a dot-separated field path (i.e. `"VAR.field1.field2"`).
/// Returns the decomposed var name and field key.
pub(super) fn resolve_field_from_ast(
    ast: &k816_core::ast::File,
    offset: usize,
) -> Option<ResolvedFieldRef> {
    use k816_core::ast::*;
    use k816_core::span::Spanned;

    fn check_expr(expr: &Expr, offset: usize) -> Option<ResolvedFieldRef> {
        match expr {
            Expr::IdentSpanned {
                name, start, end, ..
            } if *start <= offset && offset <= *end => {
                let dot = name.find('.')?;
                let var_name = &name[..dot];
                if var_name.is_empty() {
                    return None;
                }
                Some(ResolvedFieldRef {
                    var_name: var_name.to_string(),
                    field_key: name[dot + 1..].to_string(),
                })
            }
            Expr::Index { base, index } => {
                check_expr(base, offset).or_else(|| check_expr(index, offset))
            }
            Expr::Binary { lhs, rhs, .. } => {
                check_expr(lhs, offset).or_else(|| check_expr(rhs, offset))
            }
            Expr::Unary { expr, .. }
            | Expr::TypedView { expr, .. }
            | Expr::AddressHint { expr, .. } => check_expr(expr, offset),
            _ => None,
        }
    }

    fn check_operand(op: &Operand, offset: usize) -> Option<ResolvedFieldRef> {
        match op {
            Operand::Immediate { expr, .. }
            | Operand::Value { expr, .. }
            | Operand::Auto { expr } => check_expr(expr, offset),
            Operand::BlockMove { src, dst } => {
                check_expr(src, offset).or_else(|| check_expr(dst, offset))
            }
        }
    }

    fn check_hla_operand(op: &HlaOperandExpr, offset: usize) -> Option<ResolvedFieldRef> {
        check_expr(&op.expr, offset)
    }

    fn check_stmt(stmt: &Stmt, offset: usize) -> Option<ResolvedFieldRef> {
        match stmt {
            Stmt::Instruction(instr) => instr
                .operand
                .as_ref()
                .and_then(|o| check_operand(o, offset)),
            Stmt::Hla(hla) => check_hla_stmt(hla, offset),
            Stmt::ModeScopedBlock { body, .. } => check_stmts(body, offset),
            _ => None,
        }
    }

    fn check_hla_stmt(hla: &HlaStmt, offset: usize) -> Option<ResolvedFieldRef> {
        match hla {
            HlaStmt::RegisterAssign { rhs, .. }
            | HlaStmt::RegisterStore { dest: rhs, .. }
            | HlaStmt::AccumulatorAlu { rhs, .. }
            | HlaStmt::AccumulatorBitTest { rhs }
            | HlaStmt::IndexCompare { rhs, .. }
            | HlaStmt::ConditionSeed { rhs, .. } => check_hla_operand(rhs, offset),
            HlaStmt::AssignmentChain { tail_expr, .. } => tail_expr
                .as_ref()
                .and_then(|e| check_hla_operand(e, offset)),
            HlaStmt::StoreFromA { rhs, .. } => match rhs {
                HlaRhs::Immediate(e) | HlaRhs::Value { expr: e, .. } => check_expr(e, offset),
            },
            HlaStmt::Goto { target, .. } | HlaStmt::BranchGoto { target, .. } => {
                check_expr(target, offset)
            }
            HlaStmt::XAssignImmediate { rhs } => check_expr(rhs, offset),
            HlaStmt::NeverBlock { body, .. } | HlaStmt::PrefixConditional { body, .. } => {
                check_stmts(body, offset)
            }
            _ => None,
        }
    }

    fn check_stmts(stmts: &[Spanned<Stmt>], offset: usize) -> Option<ResolvedFieldRef> {
        for stmt in stmts {
            if let Some(r) = check_stmt(&stmt.node, offset) {
                return Some(r);
            }
        }
        None
    }

    for item in &ast.items {
        let result = match &item.node {
            Item::CodeBlock(block) => check_stmts(&block.body, offset),
            Item::Statement(stmt) => check_stmt(stmt, offset),
            Item::Const(c) => check_expr(&c.initializer, offset),
            Item::ConstGroup(cs) => cs.iter().find_map(|c| check_expr(&c.initializer, offset)),
            Item::Var(v) => v.initializer.as_ref().and_then(|e| check_expr(e, offset)),
            _ => None,
        };
        if result.is_some() {
            return result;
        }
    }

    None
}

pub(super) fn token_prefix_at_offset(text: &str, offset: usize) -> String {
    let bytes = text.as_bytes();
    let mut start = offset.min(bytes.len());
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }
    text[start..offset.min(text.len())].to_string()
}

pub(super) fn token_matches_in_range(text: &str, start: usize, end: usize) -> Vec<TokenMatch> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return Vec::new();
    }
    let mut out = Vec::new();
    let mut index = start.min(bytes.len());
    let end = end.min(bytes.len());

    while index < end {
        if !is_ident_byte(bytes[index]) {
            index += 1;
            continue;
        }

        let token_start = index;
        while index < end && is_ident_byte(bytes[index]) {
            index += 1;
        }
        let token_end = index;
        if token_end <= token_start {
            continue;
        }

        let token_text = &text[token_start..token_end];
        let first = token_text.as_bytes()[0];
        if first.is_ascii_digit() || first == b'@' {
            continue;
        }

        out.push(TokenMatch {
            text: token_text.to_string(),
            start: token_start,
            end: token_end,
        });
    }

    out
}

pub(super) fn is_ident_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_' || byte == b'.' || byte == b'@'
}

pub(super) fn in_symbol_completion_context(text: &str, offset: usize) -> bool {
    if !token_prefix_at_offset(text, offset).is_empty() {
        return true;
    }

    let offset = offset.min(text.len());
    let line_start = text[..offset].rfind('\n').map_or(0, |line| line + 1);
    let line_prefix = &text[line_start..offset];
    let trimmed = line_prefix.trim_start();
    if trimmed.is_empty() {
        return false;
    }

    if line_prefix.chars().last().is_some_and(char::is_whitespace) {
        return true;
    }

    trimmed.split_whitespace().count() > 1
}

pub(super) fn numeric_literal_at_offset(text: &str, offset: usize) -> Option<(ByteRange, i64)> {
    let source_id = k816_core::span::SourceId(0);
    let (tokens, _) = k816_core::lexer::lex_lenient(source_id, text);
    let offset = offset.min(text.len());
    tokens.into_iter().find_map(|token| {
        if offset < token.span.start || offset >= token.span.end {
            return None;
        }
        match token.kind {
            k816_core::lexer::TokenKind::Number(num) => Some((
                ByteRange {
                    start: token.span.start,
                    end: token.span.end,
                },
                num.value,
            )),
            _ => None,
        }
    })
}

pub(super) fn evaluator_call_at_offset(text: &str, offset: usize) -> Option<(String, u32)> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return None;
    }
    let mut index = offset.min(bytes.len());
    let mut paren_depth = 0usize;

    while index > 0 {
        index -= 1;
        match bytes[index] {
            b')' => paren_depth += 1,
            b'(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                    continue;
                }

                let mut name_end = index;
                while name_end > 0 && bytes[name_end - 1].is_ascii_whitespace() {
                    name_end -= 1;
                }
                let mut name_start = name_end;
                while name_start > 0 && is_ident_byte(bytes[name_start - 1]) {
                    name_start -= 1;
                }
                if name_start == name_end {
                    continue;
                }

                let name = text[name_start..name_end].to_string();
                let prefix = &text[..name_start];
                let open_eval = prefix.rfind('[');
                let close_eval = prefix.rfind(']');
                let Some(open_idx) = open_eval else {
                    continue;
                };
                if close_eval.is_some_and(|close_idx| close_idx > open_idx) {
                    continue;
                }

                let mut active_param = 0u32;
                let mut nested = 0usize;
                for ch in text[index + 1..offset.min(text.len())].chars() {
                    match ch {
                        '(' => nested += 1,
                        ')' => nested = nested.saturating_sub(1),
                        ',' if nested == 0 => active_param = active_param.saturating_add(1),
                        _ => {}
                    }
                }
                return Some((name, active_param));
            }
            _ => {}
        }
    }

    None
}
