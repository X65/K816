/*****************************************************************************/
/*                                                                           */
/*                                structure.rs                               */
/*                                                                           */
/*          Raising a converted unit's labels into loops and blocks          */
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

//! A converted unit's labels and branches, raised into K816's structure.
//!
//! The converter writes the program's shape as the source had it: a label
//! and a branch to it. This pass then reads each unit back as K816 and turns
//! the shapes K816 has a statement for into that statement — a label a
//! conditional branch at the end of a run returns to is a
//! `loop { … } while …`, a conditional branch over a run to a label nothing
//! else names is a block the run sits in, and inside a loop a conditional
//! branch to its top is a `repeat` and to the label just after it a `break`
//! (`plans/14-readable-hla.md` decision 15).
//!
//! **Only where it changes no byte.** Each rewrite is compiled beside the
//! unit it would replace, and kept only when the two emit the same
//! instructions to the same places with the same diagnostics; otherwise the
//! labels stay. That is a measurement, not a rule about which shapes are
//! safe: an unconditional back edge or an `else` is K816's `JMP` where the
//! source wrote `BRA`, and the measurement is what says so.
//!
//! A condition is spelled the way the flags it tests were set: a carry a
//! compare left is the relation (`<`, `>=`), and any other carry the flag
//! (`c?`, `!c?`); zero and sign are the relations K65 has always used for
//! them (`==`, `!=`, `<0`, `>=0`), and overflow has only its flag.

use std::collections::HashSet;

use super::Converted;
use crate::ast::{
    ArithmeticOp, AstFile, Block, ExprKind, HlaStatement, ItemKind, LabelScope, NativeOperand,
    Statement, StatementKind,
};
use crate::names::{NameRole, visit_block};
use crate::source::{FileId, SourceMap, Span};

/// What the pass did to a program.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Raised {
    /// Labels a loop or a block replaced.
    pub raised: usize,
    /// Local labels left, because no statement says the same thing in the
    /// same bytes.
    pub kept: usize,
}

/// Raises every unit of `units` in place.
pub fn raise(units: &mut [Converted]) -> Raised {
    let mut map = SourceMap::new();
    let files: Vec<FileId> = units
        .iter()
        .map(|unit| {
            map.add_file_at(
                unit.path.display().to_string(),
                &unit.path,
                unit.text.clone(),
            )
        })
        .collect();
    let mut total = Raised::default();
    for (unit, &file) in units.iter_mut().zip(&files) {
        let mut refused: HashSet<String> = HashSet::new();
        let Some(mut current) = compiled(&map, file) else {
            total.kept += local_labels(&unit.text);
            continue;
        };
        loop {
            let mut accepted = false;
            for candidate in candidates(&map, file) {
                if refused.contains(&candidate.key) {
                    continue;
                }
                let text = super::formatted(&unit.path, apply(&unit.text, &candidate.edits));
                map.replace_file(file, text.clone());
                match compiled(&map, file) {
                    Some(raised) if raised == current => {
                        unit.text = text;
                        current = raised;
                        total.raised += candidate.labels;
                        accepted = true;
                        break;
                    }
                    _ => {
                        map.replace_file(file, unit.text.clone());
                        refused.insert(candidate.key);
                    }
                }
            }
            if !accepted {
                break;
            }
        }
        total.kept += local_labels(&unit.text);
    }
    total
}

/// What a unit compiles to, in a form two spellings of it can be compared
/// in: the diagnostics it is told, and its assembly with every label
/// reference turned into the place it names. `None` when it does not
/// compile, which no rewrite of it may then be measured against.
/// A compiled unit, as [`compiled`] compares it: its diagnostics' codes and
/// titles, and its canonical assembly.
type Compiled = (Vec<(String, String)>, Vec<String>);

fn compiled(map: &SourceMap, file: FileId) -> Option<Compiled> {
    let result = crate::compile_to_ca65(map, file, &crate::Options::default());
    let mut diagnostics: Vec<(String, String)> = result
        .diagnostics
        .iter()
        .map(|diagnostic| {
            (
                diagnostic.code.as_str().to_owned(),
                diagnostic.title.clone(),
            )
        })
        .collect();
    diagnostics.sort();
    Some((diagnostics, canonical(&result.assembly?)))
}

/// The assembly's lines with comments, debug lines and label names taken
/// out: a label definition becomes the position of the line after it, and a
/// reference to it — `@name`, or an anonymous `:+` / `:-` — that position.
///
/// A width directive emits no byte, and the emitter places one where its own
/// account of ca65's state needs it, which a loop and a label can give
/// differently. What a directive decides is how wide an immediate assembles,
/// so the directives go and each width-dependent immediate carries the width
/// in force instead: two spellings compare equal exactly when they assemble
/// to the same bytes.
fn canonical(assembly: &str) -> Vec<String> {
    struct Line {
        text: String,
        scope: usize,
        anonymous_before: usize,
    }
    let mut lines: Vec<Line> = Vec::new();
    let mut locals: std::collections::HashMap<(usize, String), usize> = Default::default();
    let mut anonymous: Vec<usize> = Vec::new();
    let mut scope = 0;
    let (mut accumulator, mut index) = ("m?", "x?");
    for raw in assembly.lines() {
        let mut text = without_comment(raw).trim().to_owned();
        if text.is_empty() || text.starts_with(".dbg") {
            continue;
        }
        match text.as_str() {
            ".a8" => accumulator = "m8",
            ".a16" => accumulator = "m16",
            ".i8" => index = "x8",
            ".i16" => index = "x16",
            _ => {}
        }
        if matches!(text.as_str(), ".a8" | ".a16" | ".i8" | ".i16") {
            continue;
        }
        if let Some((mnemonic, operand)) = text.split_once(char::is_whitespace)
            && operand.trim_start().starts_with('#')
        {
            let width = match mnemonic.to_ascii_lowercase().as_str() {
                "lda" | "adc" | "and" | "cmp" | "eor" | "ora" | "sbc" | "bit" => accumulator,
                "ldx" | "ldy" | "cpx" | "cpy" => index,
                _ => "",
            };
            if !width.is_empty() {
                text = format!("{text} @{width}");
            }
        }
        if text == ":" {
            anonymous.push(lines.len());
            continue;
        }
        if let Some(name) = text.strip_suffix(':')
            && !name.contains(char::is_whitespace)
        {
            if let Some(local) = name.strip_prefix('@') {
                locals.insert((scope, local.to_owned()), lines.len());
                continue;
            }
            scope += 1;
        }
        lines.push(Line {
            text: text.split_whitespace().collect::<Vec<_>>().join(" "),
            scope,
            anonymous_before: anonymous.len(),
        });
    }
    lines
        .iter()
        .map(|line| {
            line.text
                .split(' ')
                .map(|word| {
                    let (word, tail) = match word.find([',', ')']) {
                        Some(at) => word.split_at(at),
                        None => (word, ""),
                    };
                    let place = if let Some(local) = word.strip_prefix('@') {
                        locals.get(&(line.scope, local.to_owned())).copied()
                    } else if let Some(plus) = word.strip_prefix(':')
                        && !plus.is_empty()
                        && plus.chars().all(|c| c == '+')
                    {
                        anonymous
                            .get(line.anonymous_before + plus.len() - 1)
                            .copied()
                    } else if let Some(minus) = word.strip_prefix(':')
                        && !minus.is_empty()
                        && minus.chars().all(|c| c == '-')
                    {
                        line.anonymous_before
                            .checked_sub(minus.len())
                            .and_then(|index| anonymous.get(index).copied())
                    } else {
                        None
                    };
                    match place {
                        Some(place) => format!("<{place}>{tail}"),
                        None => format!("{word}{tail}"),
                    }
                })
                .collect::<Vec<_>>()
                .join(" ")
        })
        .collect()
}

/// `line` up to its `;` comment, a `;` inside a string excepted.
fn without_comment(line: &str) -> &str {
    let mut quoted = false;
    for (at, character) in line.char_indices() {
        match character {
            '"' => quoted = !quoted,
            ';' if !quoted => return &line[..at],
            _ => {}
        }
    }
    line
}

/// How many local labels a unit's text defines.
fn local_labels(text: &str) -> usize {
    text.lines()
        .filter(|line| {
            let line = line.trim_start();
            line.starts_with('.')
                && line.split_once(':').is_some_and(|(name, _)| {
                    name[1..]
                        .chars()
                        .all(|c| c.is_ascii_alphanumeric() || c == '_')
                })
        })
        .count()
}

/// One rewrite the pass may try.
struct Candidate {
    /// What it is, for remembering that it was measured and refused.
    key: String,
    /// The labels it removes.
    labels: usize,
    /// Loops before blocks, then the innermost first. A block and a loop can
    /// cross — a branch inside the loop over its back edge to a label after
    /// it — and only one of the two can then be raised; the loop is the one
    /// that says more.
    order: (u8, u32),
    edits: Vec<(u32, u32, String)>,
}

/// `text` with `edits` applied; a line an edit empties is removed.
fn apply(text: &str, edits: &[(u32, u32, String)]) -> String {
    let mut edits = edits.to_vec();
    edits.sort_by_key(|(start, end, _)| std::cmp::Reverse((*start, *end)));
    let mut text = text.to_owned();
    for (start, end, replacement) in edits {
        let start = start as usize;
        // A mnemonic's span runs to its line's end or its comment, spaces
        // included; the spaces belong to the comment's column.
        let end = start + text[start..end as usize].trim_end().len();
        if replacement.is_empty() {
            let line_start = text[..start].rfind('\n').map_or(0, |at| at + 1);
            let line_end = text[end..].find('\n').map_or(text.len(), |at| end + at + 1);
            if text[line_start..start].trim().is_empty() && text[end..line_end].trim().is_empty() {
                text.replace_range(line_start..line_end, "");
                continue;
            }
        }
        text.replace_range(start..end, &replacement);
    }
    text
}

/// A jump a converted unit writes: a native branch or `jmp` to a local label.
struct Jump<'a> {
    mnemonic: String,
    target: &'a str,
    conditional: bool,
}

fn jump(statement: &Statement) -> Option<Jump<'_>> {
    let StatementKind::Native(native) = &statement.kind else {
        return None;
    };
    let mnemonic = native.mnemonic.text.to_ascii_lowercase();
    let conditional = matches!(
        mnemonic.as_str(),
        "bcc" | "bcs" | "beq" | "bne" | "bmi" | "bpl" | "bvc" | "bvs"
    );
    if !conditional && !matches!(mnemonic.as_str(), "bra" | "brl" | "jmp") {
        return None;
    }
    let Some(NativeOperand::Address(address)) = &native.operand else {
        return None;
    };
    if address.encoding.is_some() || address.offset_marker.is_some() || address.index.is_some() {
        return None;
    }
    let ExprKind::LocalSymbol(name) = &address.value.kind else {
        return None;
    };
    Some(Jump {
        mnemonic,
        target: &name.text,
        conditional,
    })
}

/// The condition a branch is taken under, spelled for the flags it reads.
fn taken(mnemonic: &str, after_compare: bool) -> &'static str {
    match (mnemonic, after_compare) {
        ("bcc", true) => "<",
        ("bcc", false) => "!c?",
        ("bcs", true) => ">=",
        ("bcs", false) => "c?",
        ("beq", _) => "==",
        ("bne", _) => "!=",
        ("bmi", _) => "<0",
        ("bpl", _) => ">=0",
        ("bvs", _) => "v?",
        _ => "!v?",
    }
}

/// What a jump becomes inside a loop: `COND break` or `COND repeat` for a
/// conditional branch, the bare word for an unconditional one.
fn exit_word(jump: &Jump<'_>, previous: Option<&Statement>, word: &str) -> String {
    if jump.conditional {
        format!("{} {word}", taken(&jump.mnemonic, compares(previous)))
    } else {
        word.to_owned()
    }
}

/// The branch taken exactly when `mnemonic` is not.
fn inverse(mnemonic: &str) -> &'static str {
    match mnemonic {
        "bcc" => "bcs",
        "bcs" => "bcc",
        "beq" => "bne",
        "bne" => "beq",
        "bmi" => "bpl",
        "bpl" => "bmi",
        "bvs" => "bvc",
        _ => "bvs",
    }
}

/// Whether the statement before a branch is a compare, which decides how a
/// carry test reads.
fn compares(previous: Option<&Statement>) -> bool {
    match previous.map(|statement| &statement.kind) {
        Some(StatementKind::Native(native)) => matches!(
            native.mnemonic.text.to_ascii_lowercase().as_str(),
            "cmp" | "cpx" | "cpy"
        ),
        Some(StatementKind::Hla(HlaStatement::Arithmetic(arithmetic))) => {
            arithmetic.op == ArithmeticOp::Compare
        }
        _ => false,
    }
}

/// Every local label one function body defines and every place it is named.
struct Labels {
    uses: std::collections::HashMap<String, Vec<Span>>,
}

impl Labels {
    fn of(body: &Block) -> Self {
        let mut uses: std::collections::HashMap<String, Vec<Span>> = Default::default();
        visit_block(body, &mut |name, role| {
            if role == NameRole::Symbol && name.text.starts_with('.') {
                uses.entry(name.text.clone()).or_default().push(name.span);
            }
        });
        Self { uses }
    }

    fn uses(&self, label: &str) -> &[Span] {
        self.uses.get(label).map_or(&[], Vec::as_slice)
    }
}

/// Every rewrite a unit offers, innermost first.
fn candidates(map: &SourceMap, file: FileId) -> Vec<Candidate> {
    let source = map.file(file);
    let parse = crate::parser::parse_source(source, None);
    let mut found = Vec::new();
    collect(&parse.file, source.text(), &mut found);
    found.sort_by_key(|candidate| candidate.order);
    found
}

fn collect(file: &AstFile, text: &str, found: &mut Vec<Candidate>) {
    for item in &file.items {
        if let ItemKind::Func(function) = &item.kind {
            let labels = Labels::of(&function.body);
            in_block(&function.name.text, &function.body, &labels, text, found);
        }
    }
}

/// The candidates in one statement list, and in every list nested in it.
fn in_block(
    function: &str,
    block: &Block,
    labels: &Labels,
    text: &str,
    found: &mut Vec<Candidate>,
) {
    let statements = &block.statements;
    for statement in statements {
        for nested in nested_blocks(statement) {
            in_block(function, nested, labels, text, found);
        }
    }
    for (index, statement) in statements.iter().enumerate() {
        if let StatementKind::Label(label) = &statement.kind
            && label.scope == LabelScope::Local
            && let Some(candidate) = a_loop(function, statements, index, &label.name.text, labels)
        {
            found.push(candidate);
        }
        if let Some(jump) = jump(statement)
            && jump.conditional
        {
            if let Some(candidate) = a_choice(function, statements, index, &jump, labels, text) {
                found.push(candidate);
            }
            if let Some(candidate) = a_block(function, statements, index, &jump, labels, text) {
                found.push(candidate);
            }
        }
    }
}

fn nested_blocks(statement: &Statement) -> Vec<&Block> {
    match &statement.kind {
        StatementKind::WidthBlock(width) => vec![&width.body],
        StatementKind::Conditional(conditional) => std::iter::once(&conditional.body)
            .chain(&conditional.otherwise)
            .collect(),
        StatementKind::Loop(loop_statement) => vec![&loop_statement.body],
        StatementKind::Nocross(block) => vec![block],
        _ => Vec::new(),
    }
}

/// Whether `span` lies inside `region`.
fn inside(span: Span, region: (u32, u32)) -> bool {
    span.start >= region.0 && span.end <= region.1
}

/// Every label a run of statements defines, at any depth.
fn defined(statements: &[Statement], out: &mut Vec<String>) {
    for statement in statements {
        if let StatementKind::Label(label) = &statement.kind {
            out.push(label.name.text.clone());
        }
        for nested in nested_blocks(statement) {
            defined(&nested.statements, out);
        }
    }
}

/// True when no label the run defines is named from outside it — a branch
/// into the middle of a loop or a block is the one thing neither can say.
/// A global label is never moved into one: other units may name it.
fn closed(statements: &[Statement], region: (u32, u32), labels: &Labels) -> bool {
    let mut names = Vec::new();
    defined(statements, &mut names);
    names.iter().all(|name| {
        name.starts_with('.') && labels.uses(name).iter().all(|&span| inside(span, region))
    })
}

/// The statements a branch use sits in, found by its operand's span: the
/// branch, the statement before it in its own list, and whether a nested
/// `loop` stands between it and `run` (where `break` and `repeat` would
/// bind to that loop instead).
fn locate(
    run: &[Statement],
    operand: Span,
    in_loop: bool,
) -> Option<(&Statement, Option<&Statement>, bool)> {
    for (index, statement) in run.iter().enumerate() {
        if jump(statement).is_some() && inside(operand, (statement.span.start, statement.span.end))
        {
            return Some((statement, index.checked_sub(1).map(|at| &run[at]), in_loop));
        }
        for nested in nested_blocks(statement) {
            let nested_loop = in_loop || matches!(statement.kind, StatementKind::Loop(_));
            if let Some(found) = locate(&nested.statements, operand, nested_loop) {
                return Some(found);
            }
        }
    }
    None
}

/// `label:` at `index`, closed by a conditional branch back to it later in
/// the same list: `loop { … } while COND`. Every other use of the label has
/// to be a conditional branch inside the run, which becomes `COND repeat`;
/// a label right after the closing branch that only conditional branches
/// inside the run name is the loop's exit, and those become `COND break`.
fn a_loop(
    function: &str,
    statements: &[Statement],
    index: usize,
    label: &str,
    labels: &Labels,
) -> Option<Candidate> {
    let uses = labels.uses(label);
    let close = statements
        .iter()
        .enumerate()
        .skip(index + 1)
        .filter(|(_, statement)| jump(statement).is_some_and(|jump| jump.target == label))
        .map(|(at, _)| at)
        .next_back()?;
    let closing = jump(&statements[close])?;
    let start = statements[index].span.start;
    let region = (start, statements[close].span.end);
    if !uses.iter().all(|&span| inside(span, region)) {
        return None;
    }
    let body = &statements[index + 1..close];
    if !closed(body, region, labels) {
        return None;
    }
    let mut edits = vec![
        (
            statements[index].span.start,
            statements[index].span.end,
            "loop {".to_owned(),
        ),
        (
            statements[close].span.start,
            statements[close].span.end,
            if closing.conditional {
                format!(
                    "}} while {}",
                    taken(
                        &closing.mnemonic,
                        compares(close.checked_sub(1).map(|at| &statements[at]))
                    )
                )
            } else {
                "} always".to_owned()
            },
        ),
    ];
    for &span in uses {
        if inside(
            span,
            (statements[close].span.start, statements[close].span.end),
        ) {
            continue;
        }
        let (branch, previous, nested_loop) = locate(body, span, false)?;
        let repeat = jump(branch)?;
        if nested_loop {
            return None;
        }
        edits.push((
            branch.span.start,
            branch.span.end,
            exit_word(&repeat, previous, "repeat"),
        ));
    }
    let mut removed = 1;
    if let Some(after) = statements.get(close + 1)
        && let StatementKind::Label(exit) = &after.kind
        && exit.scope == LabelScope::Local
    {
        // Each branch or jump to it from inside the loop is a `break`; the
        // label goes only when nothing else names it.
        let mut converted = 0;
        let exits = labels.uses(&exit.name.text);
        for &span in exits {
            let Some((branch, previous, nested_loop)) = locate(body, span, false) else {
                continue;
            };
            let Some(jump) = jump(branch) else { continue };
            if nested_loop {
                continue;
            }
            edits.push((
                branch.span.start,
                branch.span.end,
                exit_word(&jump, previous, "break"),
            ));
            converted += 1;
        }
        if converted > 0 && converted == exits.len() {
            edits.push((after.span.start, after.span.end, String::new()));
            removed += 1;
        }
    }
    Some(Candidate {
        key: format!("{function} loop {label}"),
        labels: removed,
        order: (0, region.1 - region.0),
        edits,
    })
}

/// A conditional branch at `index` over a run to a label nothing else names:
/// `COND { … }`, run when the branch is not taken.
fn a_block(
    function: &str,
    statements: &[Statement],
    index: usize,
    branch: &Jump<'_>,
    labels: &Labels,
    text: &str,
) -> Option<Candidate> {
    let close = statements.iter().enumerate().skip(index + 1).find_map(|(at, statement)| {
        matches!(&statement.kind, StatementKind::Label(label) if label.name.text == branch.target)
            .then_some(at)
    })?;
    if labels.uses(branch.target).len() != 1 {
        return None;
    }
    let region = (statements[index].span.start, statements[close].span.end);
    if !closed(&statements[index + 1..close], region, labels) {
        return None;
    }
    let condition = taken(
        inverse(&branch.mnemonic),
        compares(index.checked_sub(1).map(|at| &statements[at])),
    );
    // A comment after the branch says what happens when it is taken, which
    // is where the block ends now and not where it begins.
    let opener = statements[index].span;
    let line_end = text[opener.end as usize..]
        .find('\n')
        .map_or(text.len(), |at| opener.end as usize + at);
    let comment = text[opener.end as usize..line_end].trim();
    // And one the label carried is said beside it, in the same comment.
    let label = statements[close].span;
    let label_end = text[label.end as usize..]
        .find('\n')
        .map_or(text.len(), |at| label.end as usize + at);
    let own = text[label.end as usize..label_end].trim();
    let mut said: Vec<&str> = Vec::new();
    let mut opened = opener.end;
    if let Some(comment) = comment.strip_prefix("//") {
        said.push(comment.trim());
        opened = line_end as u32;
    }
    let mut closed_at = label.end;
    if let Some(own) = own.strip_prefix("//") {
        said.push(own.trim());
        closed_at = label_end as u32;
    }
    let closing = if said.is_empty() {
        "}".to_owned()
    } else {
        format!("}} // {}", said.join("; "))
    };
    Some(Candidate {
        key: format!("{function} block {}", branch.target),
        labels: 1,
        order: (2, region.1 - region.0),
        edits: vec![
            (opener.start, opened, format!("{condition} {{")),
            (label.start, closed_at, closing),
        ],
    })
}

#[cfg(test)]
mod tests {
    use super::canonical;

    /// Two spellings of one program's labels compare equal, and a branch to
    /// a different place does not.
    #[test]
    fn labels_compare_by_the_place_they_name() {
        let named = "f:\n@top:\n  dex ; comment\n  bne @top\n@out:\n  rts\n";
        let anonymous = ".dbg line, \"x\", 1\nf:\n:\n  dex\n  bne :-\n:\n  rts\n";
        assert_eq!(canonical(named), canonical(anonymous));
        let elsewhere = "f:\n@top:\n  dex\n  bne @out\n@out:\n  rts\n";
        assert_ne!(canonical(named), canonical(elsewhere));
    }

    /// A cheap local is scoped by the global labels around it, as ca65 scopes
    /// it, and a `;` inside a string is not a comment.
    #[test]
    fn a_cheap_local_is_scoped_and_a_string_keeps_its_semicolon() {
        let two = "f:\n@loop:\n  bne @loop\ng:\n@loop:\n  bne @loop\n";
        let lines = canonical(two);
        assert_eq!(lines[1], "bne <1>");
        assert_eq!(lines[3], "bne <3>");
        assert_eq!(canonical("  .byte \"a;b\" ; c\n"), [".byte \"a;b\""]);
    }

    /// `:++` and `:--` count anonymous labels in their own direction.
    #[test]
    fn anonymous_references_count_in_their_direction() {
        let text = "f:\n:\n  nop\n:\n  bne :--\n  beq :+\n  bra :++\n:\n  nop\n:\n  rts\n";
        let lines = canonical(text);
        assert_eq!(lines[2], "bne <1>");
        assert_eq!(lines[3], "beq <5>");
        assert_eq!(lines[4], "bra <6>");
    }
}

/// The comment after `statement` on its line, without its `//`, and where
/// the line ends.
fn trailing_comment(text: &str, statement: Span) -> (Option<&str>, u32) {
    let end = statement.end as usize;
    let line_end = text[end..].find('\n').map_or(text.len(), |at| end + at);
    match text[end..line_end].trim().strip_prefix("//") {
        Some(comment) => (Some(comment.trim()), line_end as u32),
        None => (None, statement.end),
    }
}

/// A conditional branch at `index` over a run that ends in a jump over a
/// second run, to the label that begins the second: `COND { … } else { … }`.
/// The branch is the `else`'s way in and the jump the `then`'s way out, so
/// both labels go — each must be named by its own transfer only — and the
/// comments the branch, the jump and the `else` label carried are said at
/// the `} else {`, where the branch lands.
fn a_choice(
    function: &str,
    statements: &[Statement],
    index: usize,
    branch: &Jump<'_>,
    labels: &Labels,
    text: &str,
) -> Option<Candidate> {
    let otherwise = statements.iter().enumerate().skip(index + 1).find_map(|(at, statement)| {
        matches!(&statement.kind, StatementKind::Label(label) if label.name.text == branch.target)
            .then_some(at)
    })?;
    let over = otherwise.checked_sub(1).filter(|&at| at > index)?;
    let skip = jump(&statements[over]).filter(|jump| !jump.conditional)?;
    let end = statements
        .iter()
        .enumerate()
        .skip(otherwise + 1)
        .find_map(|(at, statement)| {
            matches!(&statement.kind, StatementKind::Label(label) if label.name.text == skip.target)
                .then_some(at)
        })?;
    if labels.uses(branch.target).len() != 1 || labels.uses(skip.target).len() != 1 {
        return None;
    }
    let region = (statements[index].span.start, statements[end].span.end);
    if !closed(&statements[index + 1..over], region, labels)
        || !closed(&statements[otherwise + 1..end], region, labels)
    {
        return None;
    }
    let condition = taken(
        inverse(&branch.mnemonic),
        compares(index.checked_sub(1).map(|at| &statements[at])),
    );
    let mut said: Vec<&str> = Vec::new();
    let (comment, opened) = trailing_comment(text, statements[index].span);
    said.extend(comment);
    let (comment, jumped) = trailing_comment(text, statements[over].span);
    said.extend(comment);
    let (comment, labelled) = trailing_comment(text, statements[otherwise].span);
    said.extend(comment);
    let middle = if said.is_empty() {
        "} else {".to_owned()
    } else {
        format!("}} else {{ // {}", said.join("; "))
    };
    Some(Candidate {
        key: format!("{function} choice {}", branch.target),
        labels: 2,
        order: (1, region.1 - region.0),
        edits: vec![
            (
                statements[index].span.start,
                opened,
                format!("{condition} {{"),
            ),
            (statements[over].span.start, jumped, middle),
            (statements[otherwise].span.start, labelled, String::new()),
            (
                statements[end].span.start,
                statements[end].span.end,
                "}".to_owned(),
            ),
        ],
    })
}

#[cfg(test)]
mod width_tests {
    use super::canonical;

    /// A width directive emits nothing: where it stands does not matter, and
    /// what it decides — an immediate's width — does.
    #[test]
    fn a_directive_counts_only_through_the_immediates_it_sizes() {
        let here = "f:\n.a16\n  rep #$20\n.a16\n  sta mem\n  lda #$01\n";
        let moved = "f:\n.a16\n  rep #$20\n  sta mem\n  lda #$01\n";
        assert_eq!(canonical(here), canonical(moved));
        let narrower = "f:\n.a16\n  rep #$20\n.a8\n  sta mem\n  lda #$01\n";
        assert_ne!(canonical(here), canonical(narrower));
        let index = "f:\n.i8\n  ldx #$01\n";
        assert_ne!(canonical(index), canonical("f:\n.i16\n  ldx #$01\n"));
        assert_eq!(canonical(index), canonical("f:\n.a16\n.i8\n  ldx #$01\n"));
    }
}
