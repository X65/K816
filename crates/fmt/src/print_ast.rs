use k816_core::ast::{File, HlaStmt, Item, NamedDataEntry, Stmt};
use k816_core::parser;
use k816_core::span::{SourceId, Span, Spanned};
use pretty::{Arena, DocAllocator};
use std::collections::BTreeMap;

const INDENT_WIDTH: usize = 4;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RegionKind {
    RootBlock,
    Var,
    Eval,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Region {
    start: usize,
    end: usize,
    kind: RegionKind,
}

#[derive(Debug, Clone, Copy)]
struct DelimPair {
    open_idx: usize,
    close_idx: usize,
    open_line: usize,
    close_line: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EqStyle {
    Spaced,
    Compact,
}

pub fn format_ast(_file: &File, source: &str) -> String {
    format_source(source)
}

pub fn format_source(source: &str) -> String {
    let (ast, _diagnostics) = parser::parse_lenient_raw(SourceId(0), source);
    let mut regions = if let Some(file) = ast.as_ref() {
        regions_from_ast(file)
    } else {
        discover_regions_textual(source)
    };
    regions = normalize_regions(regions);

    let mut text = apply_region_formatting(source, &regions);

    // Rule 4 is only enforced when raw AST spans are available.
    if let (Some(ast_after), detected_regions) = {
        let (ast_after, _diag) = parser::parse_lenient_raw(SourceId(0), &text);
        let detected_regions = ast_after
            .as_ref()
            .map(regions_from_ast)
            .unwrap_or_default();
        (ast_after, detected_regions)
    } {
        text = apply_rule4_spacing(&text, &ast_after, &normalize_regions(detected_regions));
    }

    text = ensure_blank_line_between_root_blocks(&text);
    text = compact_empty_lines(&text);
    text = trim_trailing_whitespace_per_line(&text);
    text = ensure_final_newline(&text);

    // Keep pretty crate as output composition base.
    let arena = Arena::<()>::new();
    let doc = arena.text(text);
    let mut rendered = String::new();
    let _ = doc.render_fmt(usize::MAX, &mut rendered);
    rendered
}

fn regions_from_ast(file: &File) -> Vec<Region> {
    let mut out = Vec::new();
    for item in &file.items {
        let kind = match &item.node {
            Item::CodeBlock(_) | Item::DataBlock(_) | Item::NamedDataBlock(_) => Some(RegionKind::RootBlock),
            Item::Var(_) => Some(RegionKind::Var),
            Item::EvaluatorBlock(_) => Some(RegionKind::Eval),
            _ => None,
        };
        if let Some(kind) = kind
            && item.span.start < item.span.end
        {
            out.push(Region {
                start: item.span.start,
                end: item.span.end,
                kind,
            });
        }
    }
    out
}

fn normalize_regions(mut regions: Vec<Region>) -> Vec<Region> {
    regions.sort_by_key(|r| (r.start, r.end));
    let mut out = Vec::with_capacity(regions.len());
    let mut last_end = 0usize;
    for region in regions {
        if region.start >= region.end {
            continue;
        }
        if out.is_empty() || region.start >= last_end {
            last_end = region.end;
            out.push(region);
        }
    }
    out
}

fn apply_region_formatting(source: &str, regions: &[Region]) -> String {
    if regions.is_empty() {
        return source.to_string();
    }

    let mut text = source.to_string();
    for region in regions.iter().rev() {
        if region.end > text.len() || region.start >= region.end {
            continue;
        }
        let slice = &text[region.start..region.end];
        let formatted = format_region(slice, region.kind);
        text.replace_range(region.start..region.end, &formatted);
    }
    text
}

fn format_region(input: &str, kind: RegionKind) -> String {
    let normalized = if kind == RegionKind::RootBlock {
        normalize_root_block_header_spaces(input)
    } else {
        input.to_string()
    };
    let with_multiline_delims = enforce_multiline_delimiter_newlines(&normalized);
    indent_by_multiline_delimiter_depth(&with_multiline_delims)
}

fn normalize_root_block_header_spaces(input: &str) -> String {
    let Some(line_end) = input.find('\n') else {
        return collapse_code_whitespace(input);
    };
    let (first, rest) = input.split_at(line_end);
    let mut out = collapse_code_whitespace(first);
    out.push_str(rest);
    out
}

fn collapse_code_whitespace(line: &str) -> String {
    let (code, trailing_comment) = match line.find("//") {
        Some(idx) => (&line[..idx], Some(&line[idx..])),
        None => (line, None),
    };
    let collapsed = code.split_whitespace().collect::<Vec<_>>().join(" ");
    let mut out = collapsed;
    if let Some(comment) = trailing_comment {
        if !out.is_empty() && !out.ends_with(' ') {
            out.push(' ');
        }
        out.push_str(comment.trim_start_matches([' ', '\t']));
    }
    out
}

fn enforce_multiline_delimiter_newlines(input: &str) -> String {
    let mut text = input.to_string();

    loop {
        let pairs = parse_delimiter_pairs(&text);
        let mut inserts = Vec::<usize>::new();

        for pair in pairs {
            if pair.open_line == pair.close_line {
                continue;
            }

            let open_insert = pair.open_idx + 1;
            if open_insert <= text.len()
                && text.as_bytes().get(open_insert).copied() != Some(b'\n')
            {
                inserts.push(open_insert);
            }

            if pair.close_idx <= text.len() && !has_newline_before_closer(&text, pair.close_idx) {
                inserts.push(pair.close_idx);
            }
        }

        if inserts.is_empty() {
            break;
        }

        inserts.sort_unstable();
        inserts.dedup();
        for &idx in inserts.iter().rev() {
            if idx <= text.len() {
                text.insert(idx, '\n');
            }
        }
    }

    text
}

fn has_newline_before_closer(text: &str, close_idx: usize) -> bool {
    if close_idx == 0 || close_idx > text.len() {
        return true;
    }
    let bytes = text.as_bytes();
    let mut i = close_idx;
    while i > 0 {
        match bytes[i - 1] {
            b' ' | b'\t' => i -= 1,
            b'\n' => return true,
            _ => return false,
        }
    }
    true
}

fn indent_by_multiline_delimiter_depth(input: &str) -> String {
    let pairs = parse_delimiter_pairs(input);
    let mut line_count = 1usize;
    for &b in input.as_bytes() {
        if b == b'\n' {
            line_count += 1;
        }
    }

    let mut opens = vec![0usize; line_count];
    let mut closes = vec![0usize; line_count];
    for pair in pairs {
        if pair.open_line == pair.close_line {
            continue;
        }
        if pair.open_line < opens.len() {
            opens[pair.open_line] += 1;
        }
        if pair.close_line < closes.len() {
            closes[pair.close_line] += 1;
        }
    }

    let mut depth: isize = 0;
    let mut out_lines = Vec::with_capacity(line_count);
    let mut line_idx = 0usize;

    for line in input.split('\n') {
        let dedent = closes.get(line_idx).copied().unwrap_or(0) as isize;
        let line_depth = (depth - dedent).max(0) as usize;

        let body = line.trim_start_matches([' ', '\t']);
        if body.is_empty() {
            out_lines.push(String::new());
        } else {
            let mut out = String::with_capacity(line.len() + INDENT_WIDTH * line_depth);
            out.push_str(&" ".repeat(line_depth * INDENT_WIDTH));
            out.push_str(body);
            out_lines.push(out);
        }

        let open_count = opens.get(line_idx).copied().unwrap_or(0) as isize;
        depth = line_depth as isize + open_count;
        line_idx += 1;
    }

    let mut out = out_lines.join("\n");
    if input.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn parse_delimiter_pairs(text: &str) -> Vec<DelimPair> {
    #[derive(Debug, Clone, Copy)]
    enum State {
        Code,
        LineComment,
        BlockComment,
        String,
        Char,
    }

    let bytes = text.as_bytes();
    let mut state = State::Code;
    let mut i = 0usize;
    let mut line = 0usize;
    let mut stack: Vec<(u8, usize, usize)> = Vec::new();
    let mut pairs = Vec::new();

    while i < bytes.len() {
        let b = bytes[i];

        match state {
            State::Code => {
                if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    state = State::LineComment;
                    i += 2;
                    continue;
                }
                if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    state = State::BlockComment;
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    state = State::String;
                    i += 1;
                    continue;
                }
                if b == b'\'' {
                    state = State::Char;
                    i += 1;
                    continue;
                }

                match b {
                    b'{' | b'(' | b'[' => stack.push((b, i, line)),
                    b'}' | b')' | b']' => {
                        let expected = match b {
                            b'}' => b'{',
                            b')' => b'(',
                            b']' => b'[',
                            _ => unreachable!(),
                        };
                        if let Some(pos) = stack.iter().rposition(|(open, _, _)| *open == expected) {
                            let (_, open_idx, open_line) = stack.remove(pos);
                            pairs.push(DelimPair {
                                open_idx,
                                close_idx: i,
                                open_line,
                                close_line: line,
                            });
                        }
                    }
                    _ => {}
                }

                if b == b'\n' {
                    line += 1;
                }
                i += 1;
            }
            State::LineComment => {
                if b == b'\n' {
                    line += 1;
                    state = State::Code;
                }
                i += 1;
            }
            State::BlockComment => {
                if b == b'\n' {
                    line += 1;
                    i += 1;
                    continue;
                }
                if b == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    state = State::Code;
                    i += 2;
                    continue;
                }
                i += 1;
            }
            State::String => {
                if b == b'\\' && i + 1 < bytes.len() {
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    state = State::Code;
                }
                if b == b'\n' {
                    line += 1;
                }
                i += 1;
            }
            State::Char => {
                if b == b'\\' && i + 1 < bytes.len() {
                    i += 2;
                    continue;
                }
                if b == b'\'' {
                    state = State::Code;
                }
                if b == b'\n' {
                    line += 1;
                }
                i += 1;
            }
        }
    }

    pairs
}

fn apply_rule4_spacing(source: &str, ast: &File, regions: &[Region]) -> String {
    if regions.is_empty() {
        return source.to_string();
    }

    let stmt_spans = collect_statement_spans(ast);
    if stmt_spans.is_empty() {
        return source.to_string();
    }

    let line_starts = line_start_offsets(source);
    let mut spans_by_line: BTreeMap<usize, Vec<Span>> = BTreeMap::new();

    for span in stmt_spans {
        if span.start >= span.end || span.end > source.len() {
            continue;
        }
        if !offset_in_any_region(span.start, regions) {
            continue;
        }
        let start_line = line_index_for_offset(&line_starts, span.start);
        let end_line = line_index_for_offset(&line_starts, span.end.saturating_sub(1));
        if start_line != end_line {
            continue;
        }
        spans_by_line.entry(start_line).or_default().push(span);
    }

    if spans_by_line.is_empty() {
        return source.to_string();
    }

    let mut out = String::with_capacity(source.len() + 32);
    for line_idx in 0..line_starts.len() {
        let line_start = line_starts[line_idx];
        let line_end = if line_idx + 1 < line_starts.len() {
            line_starts[line_idx + 1].saturating_sub(1)
        } else {
            source.len()
        };
        let line = &source[line_start..line_end];

        if let Some(spans) = spans_by_line.get(&line_idx) {
            let mut spans = spans.clone();
            spans.sort_by_key(|s| (s.start, s.end));
            spans.retain(|s| s.start >= line_start && s.end <= line_end);
            spans = non_overlapping_spans(spans);
            out.push_str(&rewrite_line_with_statement_spans(line, line_start, &spans));
        } else {
            out.push_str(line);
        }

        if line_end < source.len() {
            out.push('\n');
        }
    }

    out
}

fn non_overlapping_spans(spans: Vec<Span>) -> Vec<Span> {
    let mut out = Vec::with_capacity(spans.len());
    let mut last_end = 0usize;
    for span in spans {
        if out.is_empty() || span.start >= last_end {
            last_end = span.end;
            out.push(span);
        }
    }
    out
}

fn rewrite_line_with_statement_spans(line: &str, line_start: usize, spans: &[Span]) -> String {
    if spans.is_empty() {
        return line.to_string();
    }

    let mut out = String::with_capacity(line.len() + 16);

    let first_start = spans[0].start - line_start;
    out.push_str(&line[..first_start]);

    for (idx, span) in spans.iter().enumerate() {
        let stmt_start = span.start - line_start;
        let stmt_end = span.end - line_start;
        let stmt = &line[stmt_start..stmt_end];
        let normalized_stmt = normalize_statement_equals_spacing(stmt);
        let style = statement_style(&normalized_stmt);
        out.push_str(&normalized_stmt);

        if let Some(next) = spans.get(idx + 1) {
            let next_start = next.start - line_start;
            let gap = &line[stmt_end..next_start];
            let next_stmt_start = next.start - line_start;
            let next_stmt_end = next.end - line_start;
            let next_stmt = &line[next_stmt_start..next_stmt_end];
            let keep_compact_empty_block = gap.is_empty()
                && normalized_stmt.trim_end().ends_with('{')
                && next_stmt.trim_start().starts_with('}');
            if keep_compact_empty_block {
                out.push_str(gap);
            } else {
                out.push_str(&normalize_statement_gap(gap, style));
            }
        } else {
            out.push_str(&line[stmt_end..]);
        }
    }

    out
}

fn statement_style(stmt: &str) -> EqStyle {
    let bytes = stmt.as_bytes();
    let mut has_assignment = false;
    let mut spaced = false;

    for i in 0..bytes.len() {
        if !is_assignment_equals(bytes, i) {
            continue;
        }
        has_assignment = true;
        let left_spaced = i > 0 && matches!(bytes[i - 1], b' ' | b'\t');
        let right_spaced = i + 1 < bytes.len() && matches!(bytes[i + 1], b' ' | b'\t');
        if left_spaced && right_spaced {
            spaced = true;
        }
    }

    if has_assignment && spaced {
        EqStyle::Spaced
    } else {
        EqStyle::Compact
    }
}

fn normalize_statement_equals_spacing(stmt: &str) -> String {
    let bytes = stmt.as_bytes();
    let mut out = String::with_capacity(stmt.len() + 4);
    let mut i = 0usize;

    while i < bytes.len() {
        if is_assignment_equals(bytes, i) {
            let left_spaced = i > 0 && matches!(bytes[i - 1], b' ' | b'\t');
            let right_spaced = i + 1 < bytes.len() && matches!(bytes[i + 1], b' ' | b'\t');
            let mixed_spacing = left_spaced ^ right_spaced;

            if mixed_spacing {
                while out.ends_with(' ') || out.ends_with('\t') {
                    out.pop();
                }
                if !out.is_empty() {
                    out.push(' ');
                }
                out.push('=');
                out.push(' ');
                i += 1;
                while i < bytes.len() && matches!(bytes[i], b' ' | b'\t') {
                    i += 1;
                }
                continue;
            }
        }

        out.push(bytes[i] as char);
        i += 1;
    }

    out
}

fn is_assignment_equals(bytes: &[u8], i: usize) -> bool {
    if bytes.get(i) != Some(&b'=') {
        return false;
    }
    if i > 0 {
        match bytes[i - 1] {
            b'=' | b'!' | b'<' | b'>' => return false,
            _ => {}
        }
    }
    if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
        return false;
    }
    true
}

fn normalize_statement_gap(gap: &str, style: EqStyle) -> String {
    if gap.contains('\n') {
        return gap.to_string();
    }
    if !gap
        .bytes()
        .all(|b| matches!(b, b' ' | b'\t' | b';'))
    {
        return gap.to_string();
    }

    if gap.contains(';') {
        if style == EqStyle::Compact {
            return gap.to_string();
        }
        let Some(last_sc) = gap.rfind(';') else {
            return gap.to_string();
        };
        let prefix = &gap[..=last_sc];
        let suffix = &gap[last_sc + 1..];
        if suffix.bytes().any(|b| matches!(b, b' ' | b'\t')) {
            gap.to_string()
        } else {
            let mut out = String::with_capacity(gap.len() + 1);
            out.push_str(prefix);
            out.push(' ');
            out
        }
    } else {
        let required = match style {
            EqStyle::Spaced => 2,
            EqStyle::Compact => 1,
        };
        let width = gap.bytes().fold(0usize, |acc, b| {
            acc + match b {
                b'\t' => INDENT_WIDTH,
                _ => 1,
            }
        });
        if width >= required {
            gap.to_string()
        } else {
            " ".repeat(required)
        }
    }
}

fn collect_statement_spans(file: &File) -> Vec<Span> {
    let mut out = Vec::new();
    for item in &file.items {
        match &item.node {
            Item::CodeBlock(block) => {
                for stmt in &block.body {
                    collect_stmt_spans_from_stmt(stmt, &mut out);
                }
            }
            Item::NamedDataBlock(block) => {
                for entry in &block.entries {
                    if let NamedDataEntry::Code(stmts) = &entry.node {
                        for stmt in stmts {
                            collect_stmt_spans_from_stmt(stmt, &mut out);
                        }
                    }
                }
            }
            Item::Statement(stmt) => collect_stmt_spans_from_node(stmt, item.span, &mut out),
            _ => {}
        }
    }
    out
}

fn collect_stmt_spans_from_stmt(stmt: &Spanned<Stmt>, out: &mut Vec<Span>) {
    collect_stmt_spans_from_node(&stmt.node, stmt.span, out)
}

fn collect_stmt_spans_from_node(stmt: &Stmt, span: Span, out: &mut Vec<Span>) {
    out.push(span);
    match stmt {
        Stmt::ModeScopedBlock { body, .. } => {
            for child in body {
                collect_stmt_spans_from_stmt(child, out);
            }
        }
        Stmt::Hla(hla) => match hla {
            HlaStmt::NeverBlock { body } => {
                for child in body {
                    collect_stmt_spans_from_stmt(child, out);
                }
            }
            HlaStmt::PrefixConditional {
                body,
                else_body,
                ..
            } => {
                for child in body {
                    collect_stmt_spans_from_stmt(child, out);
                }
                if let Some(else_body) = else_body {
                    for child in else_body {
                        collect_stmt_spans_from_stmt(child, out);
                    }
                }
            }
            _ => {}
        },
        _ => {}
    }
}

fn offset_in_any_region(offset: usize, regions: &[Region]) -> bool {
    regions
        .iter()
        .any(|region| offset >= region.start && offset < region.end)
}

fn ensure_blank_line_between_root_blocks(source: &str) -> String {
    let mut text = source.to_string();

    loop {
        let regions = discover_regions_textual(&text)
            .into_iter()
            .filter(|r| r.kind == RegionKind::RootBlock)
            .collect::<Vec<_>>();
        if regions.len() < 2 {
            break;
        }

        let line_starts = line_start_offsets(&text);
        let mut lines = split_lines_preserving_terminal_state(&text);
        let mut changed = false;

        for pair in regions.windows(2) {
            let prev = pair[0];
            let next = pair[1];
            if prev.end == 0 || next.start >= text.len() {
                continue;
            }

            let prev_line = line_index_for_offset(&line_starts, prev.end.saturating_sub(1));
            let next_line = line_index_for_offset(&line_starts, next.start);
            if next_line <= prev_line + 1 {
                lines.insert(next_line, String::new());
                changed = true;
                break;
            }

            let has_blank = ((prev_line + 1)..next_line)
                .any(|idx| lines.get(idx).is_some_and(|line| line.trim().is_empty()));
            if !has_blank {
                lines.insert(next_line, String::new());
                changed = true;
                break;
            }
        }

        if !changed {
            break;
        }

        text = join_lines_with_terminal_state(&lines, source.ends_with('\n'));
    }

    text
}

fn compact_empty_lines(source: &str) -> String {
    let trailing_newline = source.ends_with('\n');
    let mut out = Vec::<String>::new();
    let mut prev_blank = false;

    for line in source.split('\n') {
        let is_blank = line.trim().is_empty();
        if is_blank {
            if prev_blank {
                continue;
            }
            prev_blank = true;
            out.push(String::new());
        } else {
            prev_blank = false;
            out.push(line.to_string());
        }
    }

    join_lines_with_terminal_state(&out, trailing_newline)
}

fn trim_trailing_whitespace_per_line(source: &str) -> String {
    let trailing_newline = source.ends_with('\n');
    let mut out = Vec::<String>::new();
    for line in source.split('\n') {
        out.push(line.trim_end_matches([' ', '\t']).to_string());
    }
    join_lines_with_terminal_state(&out, trailing_newline)
}

fn ensure_final_newline(source: &str) -> String {
    let mut out = source.to_string();
    if !out.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn line_start_offsets(text: &str) -> Vec<usize> {
    let mut starts = vec![0usize];
    for (idx, ch) in text.char_indices() {
        if ch == '\n' {
            starts.push(idx + 1);
        }
    }
    starts
}

fn line_index_for_offset(line_starts: &[usize], offset: usize) -> usize {
    line_starts
        .partition_point(|start| *start <= offset)
        .saturating_sub(1)
}

fn split_lines_preserving_terminal_state(source: &str) -> Vec<String> {
    source.split('\n').map(|line| line.to_string()).collect()
}

fn join_lines_with_terminal_state(lines: &[String], trailing_newline: bool) -> String {
    let mut out = lines.join("\n");
    if trailing_newline && !out.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn discover_regions_textual(source: &str) -> Vec<Region> {
    let line_starts = line_start_offsets(source);
    let line_depths = top_level_curly_depths_by_line(source);
    let mut regions = Vec::new();

    for line_idx in 0..line_starts.len() {
        if line_depths.get(line_idx).copied().unwrap_or(0) != 0 {
            continue;
        }

        let line_start = line_starts[line_idx];
        let line_end = if line_idx + 1 < line_starts.len() {
            line_starts[line_idx + 1].saturating_sub(1)
        } else {
            source.len()
        };
        if line_start >= line_end || line_end > source.len() {
            continue;
        }

        let line = &source[line_start..line_end];
        let trimmed = line.trim_start();
        if trimmed.is_empty() {
            continue;
        }

        if starts_with_root_block_keyword(trimmed)
            && let Some(open_rel) = line.find('{')
        {
            let open_idx = line_start + open_rel;
            if let Some(close_idx) = find_matching_delimiter(source, open_idx, b'{', b'}') {
                regions.push(Region {
                    start: line_start,
                    end: line_end_for_offset(source, close_idx),
                    kind: RegionKind::RootBlock,
                });
                continue;
            }
        }

        if trimmed.starts_with("var ") {
            if let Some(open_rel) = line.find('[') {
                let open_idx = line_start + open_rel;
                if let Some(close_idx) = find_matching_delimiter(source, open_idx, b'[', b']') {
                    let close_line = line_index_for_offset(&line_starts, close_idx);
                    if close_line != line_idx {
                        regions.push(Region {
                            start: line_start,
                            end: line_end_for_offset(source, close_idx),
                            kind: RegionKind::Var,
                        });
                        continue;
                    }
                }
            }
            regions.push(Region {
                start: line_start,
                end: line_end,
                kind: RegionKind::Var,
            });
            continue;
        }

        if trimmed.starts_with('[')
            && let Some(open_rel) = line.find('[')
        {
            let open_idx = line_start + open_rel;
            if let Some(close_idx) = find_matching_delimiter(source, open_idx, b'[', b']') {
                regions.push(Region {
                    start: line_start,
                    end: line_end_for_offset(source, close_idx),
                    kind: RegionKind::Eval,
                });
                continue;
            }
        }
    }

    normalize_regions(regions)
}

fn starts_with_root_block_keyword(trimmed_line: &str) -> bool {
    trimmed_line.starts_with("func ")
        || trimmed_line.starts_with("naked ")
        || trimmed_line.starts_with("data ")
}

fn line_end_for_offset(source: &str, offset: usize) -> usize {
    if offset >= source.len() {
        return source.len();
    }
    source[offset..]
        .find('\n')
        .map(|idx| offset + idx)
        .unwrap_or(source.len())
}

fn find_matching_delimiter(source: &str, open_idx: usize, open: u8, close: u8) -> Option<usize> {
    #[derive(Debug, Clone, Copy)]
    enum State {
        Code,
        LineComment,
        BlockComment,
        String,
        Char,
    }

    let bytes = source.as_bytes();
    if open_idx >= bytes.len() || bytes[open_idx] != open {
        return None;
    }

    let mut state = State::Code;
    let mut depth = 0usize;
    let mut i = open_idx;

    while i < bytes.len() {
        let b = bytes[i];
        match state {
            State::Code => {
                if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    state = State::LineComment;
                    i += 2;
                    continue;
                }
                if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    state = State::BlockComment;
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    state = State::String;
                    i += 1;
                    continue;
                }
                if b == b'\'' {
                    state = State::Char;
                    i += 1;
                    continue;
                }
                if b == open {
                    depth += 1;
                } else if b == close {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return Some(i);
                    }
                }
                i += 1;
            }
            State::LineComment => {
                if b == b'\n' {
                    state = State::Code;
                }
                i += 1;
            }
            State::BlockComment => {
                if b == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    state = State::Code;
                    i += 2;
                    continue;
                }
                i += 1;
            }
            State::String => {
                if b == b'\\' && i + 1 < bytes.len() {
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    state = State::Code;
                }
                i += 1;
            }
            State::Char => {
                if b == b'\\' && i + 1 < bytes.len() {
                    i += 2;
                    continue;
                }
                if b == b'\'' {
                    state = State::Code;
                }
                i += 1;
            }
        }
    }

    None
}

fn top_level_curly_depths_by_line(source: &str) -> Vec<usize> {
    #[derive(Debug, Clone, Copy)]
    enum State {
        Code,
        LineComment,
        BlockComment,
        String,
        Char,
    }

    let bytes = source.as_bytes();
    let mut state = State::Code;
    let mut depth = 0usize;
    let mut line_depths = vec![0usize];
    let mut i = 0usize;

    while i < bytes.len() {
        let b = bytes[i];
        match state {
            State::Code => {
                if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    state = State::LineComment;
                    i += 2;
                    continue;
                }
                if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    state = State::BlockComment;
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    state = State::String;
                    i += 1;
                    continue;
                }
                if b == b'\'' {
                    state = State::Char;
                    i += 1;
                    continue;
                }
                if b == b'{' {
                    depth += 1;
                } else if b == b'}' {
                    depth = depth.saturating_sub(1);
                }
                if b == b'\n' {
                    line_depths.push(depth);
                }
                i += 1;
            }
            State::LineComment => {
                if b == b'\n' {
                    state = State::Code;
                    line_depths.push(depth);
                }
                i += 1;
            }
            State::BlockComment => {
                if b == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    state = State::Code;
                    i += 2;
                    continue;
                }
                if b == b'\n' {
                    line_depths.push(depth);
                }
                i += 1;
            }
            State::String => {
                if b == b'\\' && i + 1 < bytes.len() {
                    i += 2;
                    continue;
                }
                if b == b'"' {
                    state = State::Code;
                }
                if b == b'\n' {
                    line_depths.push(depth);
                }
                i += 1;
            }
            State::Char => {
                if b == b'\\' && i + 1 < bytes.len() {
                    i += 2;
                    continue;
                }
                if b == b'\'' {
                    state = State::Code;
                }
                if b == b'\n' {
                    line_depths.push(depth);
                }
                i += 1;
            }
        }
    }

    line_depths
}

#[cfg(test)]
mod tests {
    use super::format_source;

    #[test]
    fn uses_four_space_indent_for_multiline_block_pairs() {
        let source = "func   main  {\nnop\n}\n";
        let formatted = format_source(source);
        assert!(formatted.contains("\n    nop\n"));
    }

    #[test]
    fn preserves_non_whitespace_tokens_in_image_binary_decls() {
        let source = "image SpriteSheet = \"sprites.bmp\"\nimage \"RawSprite\" = \"raw.bmp\"\nbinary Blob = \"payload.bin\"\n";
        let formatted = format_source(source);
        assert!(formatted.contains("image SpriteSheet = \"sprites.bmp\""));
        assert!(formatted.contains("image \"RawSprite\" = \"raw.bmp\""));
        assert!(formatted.contains("binary Blob = \"payload.bin\""));
    }

    #[test]
    fn compacts_consecutive_empty_lines() {
        let source = "func main {\n    nop\n\n\n    lda #1\n}\n";
        let formatted = format_source(source);
        assert!(!formatted.contains("\n\n\n"));
    }

    #[test]
    fn enforces_rule4_minimum_separation_for_spaced_style() {
        let source = "func main {\n  a = 0 x = 1\n}\n";
        let formatted = format_source(source);
        assert!(formatted.contains("a = 0  x = 1") || formatted.contains("a = 0; x = 1"));
    }

    #[test]
    fn normalizes_mixed_equals_spacing() {
        let source = "func main {\n  a= x\n  y =z\n}\n";
        let formatted = format_source(source);
        assert!(formatted.contains("a = x"));
        assert!(formatted.contains("y = z"));
    }

    #[test]
    fn preserves_compact_equals_spacing() {
        let source = "func main {\n  a=1\n  b=2 c=3\n}\n";
        let formatted = format_source(source);
        assert!(formatted.contains("\n    a=1\n"));
        assert!(formatted.contains("\n    b=2 c=3\n"));
    }

    #[test]
    fn keeps_same_line_delimiters_as_is() {
        let source = "func main {\n  {} always\n}\n";
        let formatted = format_source(source);
        assert!(formatted.contains("{} always"));
    }

    #[test]
    fn formats_symbolic_var_bracket_block() {
        let source = "var foo[\n.bar :byte\n] = 0x2000\n";
        let formatted = format_source(source);
        assert!(formatted.contains("var foo[\n    .bar :byte\n] = 0x2000"));
    }

    #[test]
    fn keeps_call_keyword_omission() {
        let source = "func main {\n    far far_target\n    call near_target\n}\n";
        let formatted = format_source(source);
        assert!(formatted.contains("far far_target"));
        assert!(formatted.contains("call near_target"));
    }

    #[test]
    fn is_idempotent() {
        let source = "func main {\n  a= x y=z\n}\n";
        let once = format_source(source);
        let twice = format_source(&once);
        assert_eq!(once, twice);
    }

    #[test]
    fn trims_trailing_whitespace_and_ensures_terminal_newline() {
        let source = "func main {   \n  a=1\t  \n}\t";
        let formatted = format_source(source);
        assert!(formatted.ends_with('\n'));
        for line in formatted.lines() {
            assert!(!line.ends_with(' '));
            assert!(!line.ends_with('\t'));
        }
    }
}
