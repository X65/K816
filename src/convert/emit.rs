/*****************************************************************************/
/*                                                                           */
/*                                  emit.rs                                  */
/*                                                                           */
/*                    One converted unit, written as K816                    */
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

//! Writes one unit.
//!
//! The unit's items are walked in order, and each run of them becomes the
//! K816 construct it is: a section for the code between two entry points, a
//! data block for a run of data, a `var` for reserved storage, a declaration
//! for an equate, a layout, an enumeration, a macro. Comments stay on the
//! lines they were on, at the column they were at relative to the code.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use super::Note;
use super::expr::{self, Expr};
use super::items::{self, Axes, Item, Macro, MemberKind, Struct, Width, Widths};
use super::lex::{Tok, Token};
use super::program::{self, Kind, Program, Reader, Role};
use super::render::{self, Meaning, Names, Operand};
use super::source::{self, Label, Line};

/// What writing a unit produced.
pub struct Written {
    pub text: String,
    pub notes: Vec<Note>,
    /// Whether the unit expands `.macpack generic`'s `add` or `sub`.
    pub uses_generic: bool,
}

/// Writes unit `unit` of `program`.
pub fn unit(
    program: &Program,
    unit: usize,
    generic: &Path,
    include_dirs: &[PathBuf],
    read: Reader<'_>,
) -> Written {
    let mut writer = Writer {
        program,
        unit,
        out: Vec::new(),
        notes: Vec::new(),
        segment: "CODE".to_owned(),
        source_segment: "CODE".to_owned(),
        org: None,
        uses_generic: false,
        locals: BTreeMap::new(),
        unnamed: Vec::new(),
        position: 0,
        here: None,
        macros_used: BTreeSet::new(),
        last_comment: None,
        pending_align: None,
        known: Axes::default(),
        header: Axes::default(),
    };
    let items = &program.units[unit].items;
    // The imports go after the comment block the file opens with: what a
    // unit sees is the first thing it says, as a ca65 file's `.include`s
    // and `.import`s usually are.
    let anchor = items
        .iter()
        .position(|item| !matches!(item, Item::Trivia(_)))
        .unwrap_or(items.len());
    writer.top(&items[..anchor]);
    let split = writer.out.len();
    writer.top(&items[anchor..]);
    let preamble = writer.preamble(generic, include_dirs, read);
    let mut lines: Vec<String> = writer.out[..split].to_vec();
    if !preamble.is_empty() {
        if lines.last().is_some_and(|line| !line.is_empty()) {
            lines.push(String::new());
        }
        lines.extend(preamble);
    }
    lines.extend(writer.out[split..].iter().cloned());
    let text = tidy(&lines);
    Written {
        text,
        notes: writer.notes,
        uses_generic: writer.uses_generic,
    }
}

/// The lines as a file: no run of blank lines, none just inside a brace,
/// none at either end, and one newline at the end.
fn tidy(lines: &[String]) -> String {
    let mut kept: Vec<&str> = Vec::new();
    for (index, line) in lines.iter().enumerate() {
        if line.is_empty() {
            let after_open = kept.last().is_none_or(|previous| {
                previous.is_empty() || previous.ends_with('{') || previous.ends_with('[')
            });
            let before_close = lines[index + 1..]
                .iter()
                .find(|next| !next.is_empty())
                .is_some_and(|next| {
                    matches!(next.trim(), "}" | "]")
                        || next.trim_start().starts_with("] ")
                        || next.trim_start().starts_with("} ")
                });
            if after_open || before_close {
                continue;
            }
        }
        kept.push(line);
    }
    while kept.last().is_some_and(|line| line.is_empty()) {
        kept.pop();
    }
    let mut text = kept.join("\n");
    text.push('\n');
    text
}

struct Writer<'p> {
    program: &'p Program,
    unit: usize,
    out: Vec<String>,
    notes: Vec<Note>,
    /// The segment the K816 text has selected.
    segment: String,
    /// The segment the ca65 source is in.
    source_segment: String,
    /// The address an `.org` set, advanced by what was placed since.
    org: Option<i64>,
    uses_generic: bool,
    /// Inside a section: the K816 name of each label it scopes, by the
    /// ca65 spelling — `loop` and `@loop` alike.
    locals: BTreeMap<String, String>,
    /// Inside a section: the K816 name of each unnamed label, in order.
    unnamed: Vec<String>,
    /// Inside a section: how many unnamed labels precede the item being
    /// written, for resolving `:+` and `:-`.
    position: usize,
    /// The label `*` names on the instruction being written.
    here: Option<String>,
    /// Macros this unit expands, for its imports.
    macros_used: BTreeSet<String>,
    /// The comment column of the last line written, as the source had it and
    /// as it was written, for a comment line that continues it.
    last_comment: Option<(usize, usize)>,
    /// An `.align` written before a label, for the block or section the label
    /// opens to state first.
    pending_align: Option<(String, Line)>,
    /// The widths the compiler knows at the statement being written, as far
    /// as the conversion can tell: the section's header, and what a width
    /// statement since the last label set. A width block is written only
    /// where its axis is known, because one whose outer width the compiler
    /// does not know restores nothing.
    known: Axes,
    /// The section's header, which a label falls back to.
    header: Axes,
}

/// A section's range of items and what it is.
struct Section {
    name: String,
    keyword: &'static str,
    header: Axes,
    /// Whether the last item is the `rts`/`rtl` the keyword implies.
    drop_last: Option<usize>,
}

impl Writer<'_> {
    fn note(&mut self, line: &Line, message: impl Into<String>) {
        self.notes
            .push(Note::new(self.path_of(line), line.number, message));
    }

    /// The file a line is from: the unit's own, or its merged include.
    fn path_of(&self, line: &Line) -> &Path {
        let record = &self.program.units[self.unit];
        if let Some(merged) = record.merged
            && self.program.files[merged]
                .lines
                .get(line.number - 1)
                .is_some_and(|candidate| candidate.text == line.text)
            && self.program.files[record.file]
                .lines
                .get(line.number - 1)
                .is_none_or(|candidate| candidate.text != line.text)
        {
            return &self.program.files[merged].path;
        }
        &self.program.files[record.file].path
    }

    /// Writes one line: `code` at `depth`, with `line`'s comment kept at the
    /// column it had relative to the code.
    fn emit(&mut self, depth: usize, code: &str, line: Option<&Line>) {
        self.emit_at(depth, code, line, None);
    }

    /// [`Self::emit`], with the comment at `column` when one is given.
    fn emit_at(&mut self, depth: usize, code: &str, line: Option<&Line>, column: Option<usize>) {
        let indent = depth * 4;
        let mut text = format!("{}{code}", " ".repeat(indent));
        if let Some(line) = line
            && let Some((original, comment)) = &line.comment
        {
            if code.is_empty() {
                // A comment that continues the one on the line above, in its
                // column, stays in that column.
                let continues = self
                    .last_comment
                    .is_some_and(|(previous, _)| previous == *original && line.indent == *original);
                let at = match self.last_comment {
                    Some((_, written)) if continues => written,
                    _ => indent,
                };
                text = format!("{}{}", " ".repeat(at), comment_text(comment));
                if !continues {
                    self.last_comment = None;
                }
            } else {
                let start = indent + (code.len() - code.trim_start().len());
                let delta = line.body_column as isize - start as isize;
                let wanted = column.unwrap_or_else(|| (*original as isize - delta).max(0) as usize);
                let at = wanted.max(text.len() + 1);
                text.push_str(&" ".repeat(at - text.len()));
                self.last_comment = Some((*original, at));
                text.push_str(&comment_text(comment));
            }
        } else {
            self.last_comment = None;
        }
        self.out.push(text.trim_end().to_owned());
    }

    /// The column the comments of a run of lines line up in: where the first
    /// would go, or past the longest code.
    fn comment_column(&self, depth: usize, lines: &[(String, Option<&Line>)]) -> Option<usize> {
        let indent = depth * 4;
        let mut column = None;
        for (code, line) in lines {
            let Some(line) = line else { continue };
            let Some((original, _)) = &line.comment else {
                continue;
            };
            let start = indent + (code.len() - code.trim_start().len());
            let delta = line.body_column as isize - start as isize;
            let wanted = (*original as isize - delta).max(0) as usize;
            column = Some(column.map_or(wanted, |current: usize| current.max(wanted)));
        }
        let longest = lines
            .iter()
            .filter(|(_, line)| line.is_some_and(|line| line.comment.is_some()))
            .map(|(code, _)| indent + code.len() + 1)
            .max()?;
        Some(column.unwrap_or(longest).max(longest))
    }

    /// A comment-only or blank line.
    fn trivia(&mut self, depth: usize, line: &Line) {
        if line.comment.is_some() {
            self.emit(depth, "", Some(line));
        } else if let Some(why) = self.program.files.iter().find_map(|file| {
            (file.path == self.path_of(line))
                .then(|| file.unread.get(&line.number).cloned())
                .flatten()
        }) {
            self.note(line, format!("the line could not be read: {why}"));
            self.emit(
                depth,
                &format!("// k816 convert: {}", line.text.trim()),
                None,
            );
        } else {
            self.out.push(String::new());
        }
    }

    /// Selects `segment` before the construct about to be written.
    fn select(&mut self, depth: usize) {
        if self.source_segment == "ZEROPAGE" {
            return;
        }
        if self.segment != self.source_segment {
            self.segment.clone_from(&self.source_segment);
            let segment = self.segment.clone();
            self.emit(depth, &format!("segment {segment}"), None);
        }
    }

    /// The top level: everything that is not inside a section.
    fn top(&mut self, items: &[Item]) {
        let mut index = 0;
        while index < items.len() {
            index = self.top_item(items, index);
        }
    }

    /// Writes the construct starting at `items[index]`, and returns where
    /// the next one starts.
    fn top_item(&mut self, items: &[Item], index: usize) -> usize {
        let item = &items[index];
        match item {
            Item::Trivia(line) => self.trivia(0, line),
            Item::Linkage { line, .. }
            | Item::Include(_, line)
            | Item::Ignored(line)
            | Item::Macpack(_, line) => {
                if line.comment.is_some() {
                    self.emit(0, "", Some(line));
                }
            }
            Item::Segment(name, line) => {
                self.source_segment.clone_from(name);
                self.org = None;
                if line.comment.is_some() {
                    self.emit(0, "", Some(line));
                }
            }
            Item::Org(address, line) => {
                self.org = address
                    .as_ref()
                    .and_then(|address| address.fold(&|path| self.program.value(path)));
                if address.is_some() && self.org.is_none() {
                    self.note(
                        line,
                        "an `.org` whose address does not fold is not converted",
                    );
                }
                if line.comment.is_some() {
                    self.emit(0, "", Some(line));
                }
            }
            Item::Assign {
                name, value, line, ..
            } => self.equate(name, value, line),
            Item::Define { name, value, line } => match expr::parse_all(value) {
                Ok(value) => self.definition(name, &value, line),
                Err(why) => self.unsupported(0, line, &why),
            },
            Item::Struct(definition) => self.layout(definition),
            Item::Enum(enumeration) => self.enumeration(enumeration),
            Item::Macro(definition) => self.inline(definition),
            Item::Proc {
                name,
                items: body,
                line,
                end,
            } => {
                self.proc(name, body, line, end);
            }
            Item::Label(Label::Named(name), line) => {
                return self.named(items, index, name, line);
            }
            Item::Label(_, line) => {
                // Code with no named label before it is not in a section.
                return self.anonymous_code(items, index, line);
            }
            Item::Data {
                directive,
                args,
                line,
            } if directive == "align"
                && matches!(next_meaningful(items, index + 1), Some(Item::Label(..))) =>
            {
                self.pending_align = Some((tokens_text(args), line.clone()));
            }
            Item::Data { .. } => return self.data_run(items, index),
            Item::Instruction { line, .. }
            | Item::WidthSet { line, .. }
            | Item::MacroUse { line, .. }
            | Item::Repeat { line, .. } => {
                return self.anonymous_code(items, index, line);
            }
            Item::WidthAssert { line, .. } => {
                // A width directive between routines states nothing K816
                // keeps: each section states its own.
                if line.comment.is_some() {
                    self.emit(0, "", Some(line));
                }
            }
            Item::If { branches, end } => {
                self.note(
                    &branches[0].line,
                    "a `.if` outside a routine is written with K816's `#if`",
                );
                self.conditional(0, branches, end, &mut |writer, items| writer.top(items));
            }
            Item::Unsupported(line, why) => self.unsupported(0, line, why),
        }
        index + 1
    }

    fn unsupported(&mut self, depth: usize, line: &Line, why: &str) {
        self.note(line, why.to_owned());
        self.emit(
            depth,
            &format!("// k816 convert: {why}: {}", line.text.trim()),
            None,
        );
    }

    /// The imports and externs the unit needs.
    fn preamble(
        &mut self,
        generic: &Path,
        include_dirs: &[PathBuf],
        read: Reader<'_>,
    ) -> Vec<String> {
        let program = self.program;
        let unit = self.unit;
        let here = program.units[unit]
            .output
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_default();
        let mut imported: BTreeSet<usize> = BTreeSet::new();
        let mut order: Vec<usize> = Vec::new();
        let mut add = |owner: usize, order: &mut Vec<usize>| {
            if owner != unit && imported.insert(owner) {
                order.push(owner);
            }
        };
        // The includes first, in the order they were written.
        program::walk(&program.units[unit].items, &mut |item| {
            if let Item::Include(text, _) = item
                && let Some(owner) = program.included_unit(unit, text, include_dirs, read)
            {
                add(owner, &mut order);
            }
        });
        // Then the owner of every name the unit reads.
        let mut externs: BTreeMap<String, BTreeSet<Role>> = BTreeMap::new();
        for (name, uses) in &program.uses {
            let roles: BTreeSet<Role> = uses
                .iter()
                .filter(|used| used.unit == unit && used.role != Role::Access)
                .map(|used| used.role)
                .collect();
            if roles.is_empty() {
                continue;
            }
            match program.definition(unit, name) {
                Some(definition) => {
                    let owner = match &definition.kind {
                        Kind::Element { table, .. } => {
                            program.owner(table).unwrap_or(definition.unit)
                        }
                        _ => definition.unit,
                    };
                    add(owner, &mut order);
                }
                None => {
                    if program
                        .imports
                        .get(&unit)
                        .is_some_and(|imports| imports.contains_key(name))
                    {
                        externs.insert(name.clone(), roles);
                    }
                }
            }
        }
        for name in &self.macros_used {
            if let Some(owner) = program.owner(name) {
                add(owner, &mut order);
            }
        }
        let mut lines = Vec::new();
        for owner in order {
            let target = &program.units[owner].output;
            lines.push(format!(
                "import \"{}\"",
                crate::imports::relative(&here, target)
            ));
        }
        if self.uses_generic {
            lines.push(format!(
                "import \"{}\"",
                crate::imports::relative(&here, generic)
            ));
        }
        for (name, roles) in externs {
            let zero_page = program
                .imports
                .get(&unit)
                .and_then(|imports| imports.get(&name))
                .copied()
                .unwrap_or(false);
            let declaration = if roles
                .iter()
                .any(|role| matches!(role, Role::Call | Role::Jump))
            {
                format!("extern func {name}")
            } else if roles
                .iter()
                .any(|role| matches!(role, Role::Address | Role::Indexed))
                || zero_page
            {
                if zero_page {
                    format!("extern var dp {name}")
                } else {
                    format!("extern var {name}")
                }
            } else {
                format!("extern const {name}")
            };
            lines.push(declaration);
        }
        if !lines.is_empty() {
            lines.push(String::new());
        }
        lines
    }

    // -----------------------------------------------------------------
    // Declarations
    // -----------------------------------------------------------------

    /// `name = value`: a `const`, or a `var` where the name is an address.
    fn equate(&mut self, name: &str, value: &Expr, line: &Line) {
        let address = self.program.addresses.contains(name);
        let keyword = if address { "var" } else { "const" };
        let spelled = self.program.spelling(self.unit, name);
        let rendered = match render::expression(value, &self.names()) {
            Ok(rendered) => rendered,
            Err(why) => return self.unsupported(0, line, &why),
        };
        let (before, after) = assignment_gaps(line, name);
        self.emit(
            0,
            &format!("{keyword} {spelled}{before}={after}{rendered}"),
            Some(line),
        );
    }

    /// `.define NAME value` as a `const`, with the gap the source left
    /// between the name and the value before the `=`.
    fn definition(&mut self, name: &str, value: &Expr, line: &Line) {
        let address = self.program.addresses.contains(name);
        let keyword = if address { "var" } else { "const" };
        let spelled = self.program.spelling(self.unit, name);
        let rendered = match render::expression(value, &self.names()) {
            Ok(rendered) => rendered,
            Err(why) => return self.unsupported(0, line, &why),
        };
        let (gap, _) = assignment_gaps(line, name);
        let gap = if gap.is_empty() { " ".to_owned() } else { gap };
        self.emit(
            0,
            &format!("{keyword} {spelled}{gap}= {rendered}"),
            Some(line),
        );
    }

    /// A `.struct` as a field layout.
    fn layout(&mut self, definition: &Struct) {
        let address = definition
            .members
            .iter()
            .find_map(|member| match &member.kind {
                MemberKind::Org(address) => Some(address.clone()),
                _ => None,
            });
        let name = self.program.spelling(self.unit, &definition.name);
        let opener = if address.is_some() {
            format!("var {name}[")
        } else {
            format!("abstract var {name}[")
        };
        self.emit(0, &opener, Some(&definition.line));
        self.fields(definition, 1);
        let closer = match &address {
            Some(address) => match render::expression(address, &self.names()) {
                Ok(address) => format!("] = {address}"),
                Err(why) => {
                    self.note(&definition.line, why);
                    "]".to_owned()
                }
            },
            None => "]".to_owned(),
        };
        self.emit(0, &closer, Some(&definition.end));
    }

    /// A struct's members as fields, one per line, at `depth`.
    fn fields(&mut self, definition: &Struct, depth: usize) {
        let lookup = |path: &[String]| self.program.value(path);
        let width = definition
            .members
            .iter()
            .filter(|member| !matches!(member.kind, MemberKind::Trivia | MemberKind::Org(_)))
            .map(|member| member.name.as_ref().map_or(12, String::len))
            .max()
            .unwrap_or(0);
        // The code of each field first, so the comments can line up.
        let mut rows: Vec<(Option<String>, &items::Member)> = Vec::new();
        let mut offset = 0i64;
        for member in &definition.members {
            let size = self.program.member_size(member, &lookup);
            let name = member
                .name
                .clone()
                .unwrap_or_else(|| format!("_reserved_{offset:X}"));
            let padded = format!(".{name:<width$}");
            let code = match &member.kind {
                MemberKind::Trivia | MemberKind::Org(_) | MemberKind::Tag(_) => None,
                MemberKind::Data {
                    size: element,
                    count,
                } => {
                    let kind = match element {
                        1 => ":byte",
                        3 => ":far",
                        _ => ":word",
                    };
                    let count = match (element, count) {
                        (4, None) => " [2]".to_owned(),
                        (4, Some(count)) => match render::expression(count, &self.names()) {
                            Ok(count) => format!(" [({count}) * 2]"),
                            Err(_) => String::new(),
                        },
                        (_, None) => String::new(),
                        (_, Some(count)) => match render::expression(count, &self.names()) {
                            Ok(count) => format!(" [{count}]"),
                            Err(why) => {
                                self.note(&member.line, why);
                                String::new()
                            }
                        },
                    };
                    Some(format!("{padded} {kind}{count}"))
                }
            };
            rows.push((code, member));
            offset += size.unwrap_or(0);
        }
        let aligned: Vec<(String, Option<&Line>)> = rows
            .iter()
            .filter_map(|(code, member)| code.clone().map(|code| (code, Some(&member.line))))
            .collect();
        let column = self.comment_column(depth, &aligned);
        let mut offset = 0i64;
        for (code, member) in rows {
            let size = self.program.member_size(member, &lookup);
            match (&member.kind, code) {
                (MemberKind::Trivia, _) => {
                    if member.line.comment.is_some() {
                        self.emit(depth, "", Some(&member.line));
                    } else {
                        self.out.push(String::new());
                    }
                }
                (MemberKind::Org(_), _) => {
                    if member.line.comment.is_some() {
                        self.emit(depth, "", Some(&member.line));
                    }
                }
                (MemberKind::Tag(tag), _) => match self.program.structs.get(tag) {
                    Some(nested) => {
                        let nested = nested.clone();
                        let name = member
                            .name
                            .clone()
                            .unwrap_or_else(|| format!("_reserved_{offset:X}"));
                        self.emit_at(depth, &format!(".{name}["), Some(&member.line), column);
                        self.fields(&nested, depth + 1);
                        self.emit(depth, "]", None);
                    }
                    None => self.unsupported(
                        depth,
                        &member.line,
                        &format!("`.tag {tag}` names no struct this converter read"),
                    ),
                },
                (_, Some(code)) => self.emit_at(depth, &code, Some(&member.line), column),
                (_, None) => {}
            }
            offset += size.unwrap_or(0);
        }
    }

    /// A `.enum` as one `const` list.
    fn enumeration(&mut self, enumeration: &items::Enum) {
        let Some(last) = enumeration
            .members
            .iter()
            .rposition(|member| member.name.is_some())
        else {
            return;
        };
        if enumeration.line.comment.is_some() {
            self.emit(0, "", Some(&enumeration.line));
        }
        let first = enumeration
            .members
            .iter()
            .position(|member| member.name.is_some())
            .unwrap_or(0);
        let mut rows: Vec<(Option<String>, &items::EnumMember)> = Vec::new();
        for (index, member) in enumeration.members.iter().enumerate() {
            let Some(name) = &member.name else {
                rows.push((None, member));
                continue;
            };
            let spelled = match &enumeration.name {
                Some(scope) => self
                    .program
                    .spelling(self.unit, &format!("{scope}::{name}")),
                None => self.program.spelling(self.unit, name),
            };
            let lead = if index == first { "const " } else { "      " };
            let mut code = format!("{lead}{spelled}");
            if let Some(value) = &member.value {
                match render::expression(value, &self.names()) {
                    Ok(rendered) => {
                        let (before, after) = assignment_gaps(&member.line, name);
                        code = format!("{code}{before}={after}{rendered}");
                    }
                    Err(why) => self.note(&member.line, why),
                }
            }
            if index != last {
                code.push(',');
            }
            rows.push((Some(code), member));
        }
        let aligned: Vec<(String, Option<&Line>)> = rows
            .iter()
            .filter_map(|(code, member)| {
                code.clone()
                    .map(|code| (code.trim_start().to_owned(), Some(&member.line)))
            })
            .collect();
        // The names start six columns in, after `const `, on every line.
        let column = self.comment_column(0, &aligned).map(|column| column + 6);
        for (code, member) in rows {
            match code {
                Some(code) => self.emit_at(0, &code, Some(&member.line), column),
                None if member.line.comment.is_some() => {
                    self.last_comment = None;
                    let text = format!(
                        "      {}",
                        comment_text(
                            &member
                                .line
                                .comment
                                .as_ref()
                                .map(|(_, text)| text.clone())
                                .unwrap_or_default()
                        )
                    );
                    self.out.push(text);
                }
                None => self.out.push(String::new()),
            }
        }
    }

    /// A `.macro` as an `inline`, or as nothing when it is a width macro.
    fn inline(&mut self, definition: &Macro) {
        if items::width_macro(definition).is_some() {
            let (mnemonic, _, axes) = items::width_macro(definition).unwrap_or_default();
            let _ = mnemonic;
            self.emit(
                0,
                &format!(
                    "// `{}`: a width macro, written `{}` wherever it was used",
                    definition.name,
                    axes.annotations()
                ),
                None,
            );
            return;
        }
        let params: Vec<String> = definition
            .params
            .iter()
            .map(|param| {
                let spelled = alias_spelling(param);
                match program::param_role(definition, param) {
                    Role::Address | Role::Call | Role::Jump => spelled,
                    _ => format!("#{spelled}"),
                }
            })
            .collect();
        let name = self.program.spelling(self.unit, &definition.name);
        let header = if params.is_empty() {
            format!("inline {name} {{")
        } else {
            format!("inline {name} ({}) {{", params.join(", "))
        };
        self.emit(0, &header, Some(&definition.line));
        let renamed: BTreeMap<String, String> = definition
            .params
            .iter()
            .map(|param| (param.clone(), alias_spelling(param)))
            .collect();
        let lines: Vec<Line> = definition
            .body
            .iter()
            .map(|line| rename_tokens(line, &renamed))
            .collect();
        let lookup = |name: &str| self.program.macros.get(name).cloned();
        let mut builder = items::Builder {
            widths: Widths::default(),
            macros: &lookup,
        };
        let body = builder.items(&lines, &mut 0, &[]).0;
        let body = self.unroll_param_repeats(&body, definition);
        // An `inline` states no width, and runs at its use site's.
        self.header = Axes::default();
        self.known = Axes::default();
        self.body(&body, 1, None);
        self.emit(0, "}", Some(&definition.end));
    }

    /// A `.repeat` over a parameter, in a macro body of one implied
    /// instruction, is that instruction with a count: `lsr * n`.
    fn unroll_param_repeats(&self, body: &[Item], definition: &Macro) -> Vec<Item> {
        body.iter()
            .map(|item| match item {
                Item::Repeat {
                    count, items, line, ..
                } => {
                    let instructions: Vec<&Item> = items
                        .iter()
                        .filter(|inner| !matches!(inner, Item::Trivia(_)))
                        .collect();
                    if let (
                        Some(name),
                        [
                            Item::Instruction {
                                mnemonic,
                                operand,
                                widths,
                                ..
                            },
                        ],
                    ) = (count.name(), instructions.as_slice())
                        && operand.is_empty()
                        && definition.params.iter().any(|param| param == name)
                    {
                        let mut repeated = line.clone();
                        repeated.comment = None;
                        return Item::Instruction {
                            mnemonic: format!("{mnemonic} * {}", alias_spelling(name)),
                            operand: Vec::new(),
                            widths: *widths,
                            line: repeated,
                        };
                    }
                    item.clone()
                }
                other => other.clone(),
            })
            .collect()
    }

    // -----------------------------------------------------------------
    // Labels at the top level
    // -----------------------------------------------------------------

    fn named(&mut self, items: &[Item], index: usize, name: &str, line: &Line) -> usize {
        let Some(definition) = self.program.definition(self.unit, name).cloned() else {
            return index + 1;
        };
        match &definition.kind {
            Kind::Code => {
                if self.program.starts.contains(&(self.unit, name.to_owned())) {
                    self.section(items, index)
                } else {
                    self.anonymous_code(items, index, line)
                }
            }
            Kind::Data => self.data_run(items, index),
            Kind::Storage { zero_page } => self.storage(items, index, name, *zero_page),
            Kind::Table { layout, count } => self.table(items, index, name, layout, count),
            Kind::Element { .. } => index + 1,
            Kind::Pinned { address } => self.pinned(items, index, name, *address),
            _ => index + 1,
        }
    }

    /// `name: .res N` as `var name[N]`.
    fn storage(&mut self, items: &[Item], index: usize, name: &str, zero_page: bool) -> usize {
        let Item::Label(_, line) = &items[index] else {
            return index + 1;
        };
        let mut at = index + 1;
        while matches!(items.get(at), Some(Item::Trivia(_))) {
            at += 1;
        }
        let spelled = self.program.spelling(self.unit, name);
        let class = if zero_page { "var dp" } else { "var" };
        let Some(Item::Data {
            directive,
            args,
            line: data,
        }) = items.get(at)
        else {
            return index + 1;
        };
        let size = match directive.as_str() {
            "res" => {
                let parts = source::split_commas(args);
                if parts.len() > 1 && !zero_page {
                    // A fill value makes it data, not storage.
                    return self.data_run(items, index);
                }
                match parts.first().map(|count| expr::parse_all(count)) {
                    Some(Ok(count)) => render::expression(&count, &self.names()).ok(),
                    _ => None,
                }
            }
            "tag" => match args.as_slice() {
                [
                    Token {
                        tok: Tok::Ident(layout),
                        ..
                    },
                ] => Some(format!(
                    "{}:sizeof",
                    self.program.spelling(self.unit, layout)
                )),
                _ => None,
            },
            _ => {
                // Initialized data in the zero page: storage, and the value is
                // the loader's — which K816 does not write.
                let bytes = data_size(directive, args);
                self.note(
                    data,
                    "initialized data in the zero page is reserved, and its value is not kept",
                );
                Some(bytes.to_string())
            }
        };
        let Some(size) = size else {
            self.unsupported(0, data, "a reservation this converter cannot size");
            return at + 1;
        };
        self.select(0);
        // Two bytes the whole program moves sixteen bits at a time is a word,
        // and saying so is a claim the compiler then checks at every access.
        let declaration = if size == "1" {
            format!("{class} {spelled}")
        } else if size == "2" && self.program.is_word(name) {
            format!("{class} {spelled}:word")
        } else {
            format!("{class} {spelled}[{size}]")
        };
        let carrier = if data.number == line.number {
            data
        } else {
            line
        };
        self.emit(0, &declaration, Some(carrier));
        at + 1
    }

    /// A run of `.tag` storage, or one `.res` of several of a layout, as
    /// one table.
    fn table(
        &mut self,
        items: &[Item],
        index: usize,
        name: &str,
        layout: &str,
        count: &Expr,
    ) -> usize {
        let Some(definition) = self.program.structs.get(layout).cloned() else {
            return index + 1;
        };
        let reserved = items[index + 1..]
            .iter()
            .find(|item| !matches!(item, Item::Trivia(_)))
            .is_some_and(|item| matches!(item, Item::Data { directive, .. } if directive == "res"));
        let mut at = index + 1;
        if reserved {
            while !matches!(&items[at], Item::Data { .. }) {
                at += 1;
            }
            at += 1;
        } else {
            let wanted = count.fold(&|_| None).unwrap_or(0);
            let mut tags = 0;
            while at < items.len() && tags < wanted {
                if matches!(&items[at], Item::Data { directive, .. } if directive == "tag") {
                    tags += 1;
                }
                at += 1;
            }
        }
        self.select(0);
        let spelled = self.program.spelling(self.unit, name);
        let count = render::expression(count, &self.names()).unwrap_or_else(|_| "1".to_owned());
        let line = items[index].line().clone();
        self.emit(0, &format!("var {spelled}["), Some(&line));
        self.fields(&definition, 1);
        self.emit(0, &format!("] * {count}"), None);
        at
    }

    /// A label under `.org` as a pinned `var`.
    fn pinned(&mut self, items: &[Item], index: usize, name: &str, address: i64) -> usize {
        let line = items[index].line().clone();
        let spelled = self.program.spelling(self.unit, name);
        let mut at = index + 1;
        let mut size = None;
        if let Some(Item::Data {
            directive, args, ..
        }) = items.get(at)
            && directive == "res"
            && let Some(Ok(count)) = source::split_commas(args)
                .first()
                .map(|count| expr::parse_all(count))
        {
            size = render::expression(&count, &self.names()).ok();
            at += 1;
        }
        let declaration = match size {
            Some(size) => format!("var {spelled}[{size}] = ${address:04X}"),
            None => format!("var {spelled} = ${address:04X}"),
        };
        self.emit(0, &declaration, Some(&line));
        at
    }

    // -----------------------------------------------------------------
    // Data
    // -----------------------------------------------------------------

    /// A run of data: labels and data directives, until something else.
    fn data_run(&mut self, items: &[Item], index: usize) -> usize {
        let mut at = index;
        let mut open = false;
        let mut last_typed: Option<&'static str> = None;
        let mut pending_trivia: Vec<Line> = Vec::new();
        while at < items.len() {
            match &items[at] {
                Item::Trivia(line) => {
                    if open {
                        pending_trivia.push(line.clone());
                    } else {
                        self.trivia(0, line);
                    }
                }
                Item::Label(Label::Named(name), line) => {
                    let kind = self
                        .program
                        .definition(self.unit, name)
                        .map(|d| d.kind.clone());
                    if kind != Some(Kind::Data) {
                        break;
                    }
                    let one_liner = matches!(items.get(at + 1), Some(Item::Data { line: data, .. }) if data.number == line.number);
                    let spelled = self.program.spelling(self.unit, name);
                    let continues = open && one_liner && pending_trivia.is_empty();
                    if continues {
                        // The entry follows on the label's own line.
                        last_typed = None;
                        let prefix = format!("{spelled}:{}", label_gap(line, name));
                        at += 1;
                        self.labeled_entry(&prefix, items.get(at), &mut last_typed);
                        at += 1;
                        continue;
                    }
                    if open {
                        self.close_block(&mut pending_trivia);
                    }
                    self.select(0);
                    let anonymous = one_liner
                        && matches!(items.get(at + 2), Some(Item::Label(Label::Named(_), next)) if items.get(at + 3).is_some_and(|after| matches!(after, Item::Data { line, .. } if line.number == next.number)));
                    if anonymous {
                        self.emit(0, "data {", None);
                        self.state_align(1);
                        open = true;
                        last_typed = None;
                        let prefix = format!("{spelled}:{}", label_gap(line, name));
                        at += 1;
                        self.labeled_entry(&prefix, items.get(at), &mut last_typed);
                        at += 1;
                        continue;
                    }
                    self.emit(
                        0,
                        &format!("data {spelled} {{"),
                        if one_liner { None } else { Some(line) },
                    );
                    self.state_align(1);
                    open = true;
                    last_typed = None;
                }
                // K816's data `repeat` has no counter, so a body that reads
                // one is written out, a line per pass, as ca65 assembles it.
                Item::Repeat {
                    count,
                    var: Some(var),
                    items: body,
                    line,
                    ..
                } if open && program::is_data_repeat(body) => {
                    for line in std::mem::take(&mut pending_trivia) {
                        self.trivia(1, &line);
                    }
                    let Some(times) = count.fold(&|path| self.program.value(path)) else {
                        self.unsupported(1, line, "a data `.repeat` whose count only ld65 knows");
                        at += 1;
                        continue;
                    };
                    let mut typed = None;
                    for index in 0..times {
                        let mut entries = Vec::new();
                        for inner in body {
                            if let Item::Data {
                                directive,
                                args,
                                line: data,
                            } = substitute_item(inner, var, index)
                            {
                                let args = fold_literals(&args);
                                entries.extend(self.entries(&directive, &args, &data, &mut typed));
                            }
                        }
                        let carrier = (index == 0).then_some(line);
                        self.emit(1, &entries.join(" "), carrier);
                    }
                    last_typed = typed;
                }
                Item::Repeat {
                    count,
                    var: None,
                    items: body,
                    line,
                    end,
                } if open && program::is_data_repeat(body) => {
                    for line in std::mem::take(&mut pending_trivia) {
                        self.trivia(1, &line);
                    }
                    let count = render::expression(count, &self.names()).unwrap_or_else(|why| {
                        self.note(line, why);
                        "1".to_owned()
                    });
                    let mut entries = Vec::new();
                    let mut typed = None;
                    for inner in body {
                        if let Item::Data {
                            directive,
                            args,
                            line: data,
                        } = inner
                        {
                            entries.extend(self.entries(directive, args, data, &mut typed));
                        }
                    }
                    if entries.len() == 1 {
                        self.emit(
                            1,
                            &format!("repeat {count} {{ {} }}", entries[0]),
                            Some(line),
                        );
                    } else {
                        self.emit(1, &format!("repeat {count} {{"), Some(line));
                        for entry in entries {
                            self.emit(2, &entry, None);
                        }
                        self.emit(1, "}", Some(end));
                    }
                    last_typed = Some("byte");
                }
                Item::Data {
                    directive,
                    args,
                    line,
                } => {
                    if !open {
                        self.select(0);
                        self.emit(0, "data {", None);
                        self.state_align(1);
                        open = true;
                        last_typed = None;
                    }
                    for line in std::mem::take(&mut pending_trivia) {
                        self.trivia(1, &line);
                    }
                    let entries = self.entries(directive, args, line, &mut last_typed);
                    let mut first = true;
                    for entry in entries {
                        self.emit(1, &entry, if first { Some(line) } else { None });
                        first = false;
                    }
                }
                _ => break,
            }
            at += 1;
        }
        if open {
            self.close_block(&mut pending_trivia);
        }
        at
    }

    /// A data directive written after `prefix`, a label on its line.
    fn labeled_entry(
        &mut self,
        prefix: &str,
        item: Option<&Item>,
        last_typed: &mut Option<&'static str>,
    ) {
        let Some(Item::Data {
            directive,
            args,
            line,
        }) = item
        else {
            self.emit(1, prefix.trim_end(), None);
            return;
        };
        let entries = self.entries(directive, args, line, last_typed);
        let mut first = true;
        for entry in entries {
            if first {
                self.emit(1, &format!("{prefix}{entry}"), Some(line));
                first = false;
            } else {
                self.emit(1, &entry, None);
            }
        }
    }

    fn close_block(&mut self, pending_trivia: &mut Vec<Line>) {
        self.emit(0, "}", None);
        for line in std::mem::take(pending_trivia) {
            self.trivia(0, &line);
        }
    }

    /// One data directive as K816 data entries.
    fn entries(
        &mut self,
        directive: &str,
        args: &[Token],
        line: &Line,
        last_typed: &mut Option<&'static str>,
    ) -> Vec<String> {
        let mut problems: Vec<String> = Vec::new();
        let out = self.entry_values(directive, args, last_typed, &mut problems);
        for problem in problems {
            self.note(line, problem);
        }
        out
    }

    fn entry_values(
        &self,
        directive: &str,
        args: &[Token],
        last_typed: &mut Option<&'static str>,
        problems: &mut Vec<String>,
    ) -> Vec<String> {
        let parts = source::split_commas(args);
        let names = self.names();
        let mut out: Vec<String> = Vec::new();
        let mut current: Option<(&'static str, Vec<String>)> = None;
        let flush = |current: &mut Option<(&'static str, Vec<String>)>, out: &mut Vec<String>| {
            if let Some((keyword, values)) = current.take() {
                if keyword.is_empty() {
                    out.push(values.join(" "));
                } else {
                    out.push(format!("{keyword} {}", values.join(" ")));
                }
            }
        };
        let push = |keyword: &'static str,
                    value: String,
                    current: &mut Option<(&'static str, Vec<String>)>,
                    out: &mut Vec<String>,
                    last: &mut Option<&'static str>| {
            // After a typed entry, bare bytes would be taken as more of it, so
            // they open a `byte` entry; once one is open they join it.
            let effective = match (keyword, &*current) {
                ("", Some(("byte", _))) => "byte",
                ("", _) if matches!(last, Some("word" | "far")) => "byte",
                _ => keyword,
            };
            match current {
                Some((open, values)) if *open == effective => values.push(value),
                _ => {
                    flush(current, out);
                    *current = Some((effective, vec![value]));
                }
            }
            *last = Some(if effective.is_empty() {
                "byte"
            } else {
                effective
            });
        };
        match directive {
            "byte" | "byt" | "db" | "asciiz" => {
                for part in &parts {
                    let Ok(value) = expr::parse_all(part) else {
                        problems.push("a value this converter cannot read".into());
                        continue;
                    };
                    match &value {
                        Expr::Str { bytes, .. } => {
                            push("", k816_string(bytes), &mut current, &mut out, last_typed)
                        }
                        Expr::Number { text, .. } | Expr::Char { text, .. } => {
                            push("", text.clone(), &mut current, &mut out, last_typed)
                        }
                        other => match byte_value(other, &names) {
                            Ok(rendered) => {
                                push("byte", rendered, &mut current, &mut out, last_typed)
                            }
                            Err(why) => problems.push(why),
                        },
                    }
                }
                if directive == "asciiz" {
                    push("", "0".to_owned(), &mut current, &mut out, last_typed);
                }
            }
            "word" | "addr" | "dw" | "faraddr" => {
                let keyword = if directive == "faraddr" {
                    "far"
                } else {
                    "word"
                };
                for part in &parts {
                    match expr::parse_all(part).and_then(|value| word_value(&value, &names)) {
                        Ok(rendered) => push(keyword, rendered, &mut current, &mut out, last_typed),
                        Err(why) => problems.push(why),
                    }
                }
            }
            "dbyt" => {
                for part in &parts {
                    match expr::parse_all(part).and_then(|value| word_value(&value, &names)) {
                        Ok(rendered) => {
                            push(
                                "byte",
                                format!("&>{rendered}"),
                                &mut current,
                                &mut out,
                                last_typed,
                            );
                            push(
                                "byte",
                                format!("&<{rendered}"),
                                &mut current,
                                &mut out,
                                last_typed,
                            );
                        }
                        Err(why) => problems.push(why),
                    }
                }
            }
            "dword" => {
                for part in &parts {
                    match expr::parse_all(part)
                        .ok()
                        .and_then(|value| value.fold(&|path| self.program.value(path)))
                    {
                        Some(value) => {
                            push(
                                "word",
                                format!("${:04X}", value & 0xFFFF),
                                &mut current,
                                &mut out,
                                last_typed,
                            );
                            push(
                                "word",
                                format!("${:04X}", (value >> 16) & 0xFFFF),
                                &mut current,
                                &mut out,
                                last_typed,
                            );
                        }
                        None => problems
                            .push("a `.dword` of a value only ld65 knows is not converted".into()),
                    }
                }
            }
            "res" => {
                let count = parts.first().map(|count| expr::parse_all(count));
                let fill = parts.get(1).map(|fill| expr::parse_all(fill));
                match (count, fill) {
                    (Some(Ok(count)), None) => {
                        let rendered =
                            render::expression(&count, &names).unwrap_or_else(|_| "1".to_owned());
                        flush(&mut current, &mut out);
                        out.push(format!("repeat {rendered} {{ ? }}"));
                        *last_typed = Some("byte");
                    }
                    (Some(Ok(count)), Some(Ok(fill))) => {
                        let rendered =
                            render::expression(&count, &names).unwrap_or_else(|_| "1".to_owned());
                        let value = match &fill {
                            Expr::Number { text, .. } | Expr::Char { text, .. } => text.clone(),
                            other => match byte_value(other, &names) {
                                Ok(value) => format!("byte {value}"),
                                Err(why) => {
                                    problems.push(why);
                                    "0".to_owned()
                                }
                            },
                        };
                        flush(&mut current, &mut out);
                        out.push(format!("repeat {rendered} {{ {value} }}"));
                        *last_typed = Some("byte");
                    }
                    _ => problems.push("a `.res` this converter cannot read".into()),
                }
            }
            "tag" => match args {
                [
                    Token {
                        tok: Tok::Ident(layout),
                        ..
                    },
                ] => {
                    flush(&mut current, &mut out);
                    out.push(format!(
                        "repeat {}:sizeof {{ ? }}",
                        self.program.spelling(self.unit, layout)
                    ));
                    *last_typed = Some("byte");
                }
                _ => problems.push("a `.tag` names no struct".into()),
            },
            "align" => {
                flush(&mut current, &mut out);
                out.push(format!("align {}", tokens_text(args)));
            }
            "incbin" => {
                flush(&mut current, &mut out);
                out.push(format!("binary {}", tokens_text(args)));
            }
            _ => problems.push(format!("`.{directive}` in a data block is not converted")),
        }
        flush(&mut current, &mut out);
        out
    }

    // -----------------------------------------------------------------
    // Sections
    // -----------------------------------------------------------------

    /// The end of the section that starts at `index`.
    fn section_end(&self, items: &[Item], index: usize) -> usize {
        section_end(self.program, self.unit, items, index)
    }

    /// A section from a code label.
    fn section(&mut self, items: &[Item], index: usize) -> usize {
        let Item::Label(Label::Named(name), line) = &items[index] else {
            return index + 1;
        };
        let end = self.section_end(items, index);
        let body = &items[index + 1..end];
        // Equates and declarations inside the code go before the section.
        for item in body {
            match item {
                Item::Assign {
                    name, value, line, ..
                } => self.equate(name, value, line),
                Item::Define { name, value, line } => {
                    if let Ok(value) = expr::parse_all(value) {
                        self.equate(name, &value, line);
                    }
                }
                _ => {}
            }
        }
        let section = self.describe(name, body, line);
        self.open_section(&section, line);
        self.body_with(body, section.drop_last, Some(name), section.header);
        self.emit(0, "}", None);
        end
    }

    /// `.proc NAME` as a section.
    fn proc(&mut self, name: &str, body: &[Item], line: &Line, end: &Line) {
        let section = self.describe(name, body, line);
        self.open_section(&section, line);
        self.body_with(body, section.drop_last, Some(name), section.header);
        self.emit(0, "}", Some(end));
    }

    /// Code no named label begins: a section of its own, named after the
    /// line it starts on.
    fn anonymous_code(&mut self, items: &[Item], index: usize, line: &Line) -> usize {
        let name = format!("code_{}", line.number);
        self.note(
            line,
            format!("code with no label before it is in a section of its own, `{name}`"),
        );
        let mut end = index + 1;
        while end < items.len() {
            if matches!(&items[end], Item::Label(Label::Named(label), _) if self.program.starts.contains(&(self.unit, label.clone())))
                || matches!(
                    &items[end],
                    Item::Data { .. } | Item::Segment(..) | Item::Proc { .. }
                )
            {
                break;
            }
            end += 1;
        }
        let body = &items[index..end];
        let section = self.describe(&name, body, line);
        self.select(0);
        let header = section.header.annotations();
        let header = if header.is_empty() {
            String::new()
        } else {
            format!(" {header}")
        };
        self.emit(0, &format!("{} {name}{header} {{", section.keyword), None);
        self.body_with(body, section.drop_last, None, section.header);
        self.emit(0, "}", None);
        end
    }

    fn open_section(&mut self, section: &Section, line: &Line) {
        self.select(0);
        let spelled = self.program.spelling(self.unit, &section.name);
        let header = section.header.annotations();
        let header = if header.is_empty() {
            String::new()
        } else {
            format!(" {header}")
        };
        // A label that shares its line with an instruction or a macro use
        // leaves the line's comment to that statement, which is emitted
        // with it; on the opening line too it would be said twice.
        let opener = match &line.body {
            Some(source::Body::Instruction { .. } | source::Body::Name { .. }) => Line {
                comment: None,
                ..line.clone()
            },
            _ => line.clone(),
        };
        self.emit(
            0,
            &format!("{} {spelled}{header} {{", section.keyword),
            Some(&opener),
        );
        self.state_align(1);
    }

    /// An `.align` written before the label just opened, as its first entry.
    fn state_align(&mut self, depth: usize) {
        if let Some((boundary, line)) = self.pending_align.take() {
            self.emit(depth, &format!("align {boundary}"), Some(&line));
        }
    }

    /// What a section is: its keyword, its header, and whether its last
    /// instruction is the return the keyword implies.
    fn describe(&mut self, name: &str, body: &[Item], line: &Line) -> Section {
        let last = body.iter().enumerate().rev().find(|(_, item)| {
            !matches!(
                item,
                Item::Trivia(_)
                    | Item::Label(..)
                    | Item::WidthAssert { .. }
                    | Item::Assign { .. }
                    | Item::Define { .. }
            )
        });
        let (keyword, drop_last) = match last {
            Some((
                index,
                Item::Instruction {
                    mnemonic, operand, ..
                },
            )) if mnemonic == "rts" && operand.is_empty() => ("func", Some(index)),
            Some((
                index,
                Item::Instruction {
                    mnemonic, operand, ..
                },
            )) if mnemonic == "rtl" && operand.is_empty() => ("far func", Some(index)),
            _ => ("naked", None),
        };
        let mut header = header(self.program, self.unit, body);
        // The hardware enters an interrupt handler, and K816 enters one at
        // the environment's widths: a header on one is an assertion about the
        // environment, not a contract a caller meets. Whatever the original
        // assumed before its first width change, it assumed of a state the
        // interrupted code left, which nothing here can state.
        if returns_from_interrupt(self.program, body) {
            let assumed = header.annotations();
            if !assumed.is_empty() {
                self.note(
                    line,
                    format!(
                        "`{name}` returns with `rti` and reads a width before it sets one; \
                         the original assumed `{assumed}` there, and K816 enters an interrupt \
                         handler at the environment's widths"
                    ),
                );
            }
            header = Axes::default();
        }
        Section {
            name: name.to_owned(),
            keyword,
            header,
            drop_last,
        }
    }

    fn body_with(
        &mut self,
        body: &[Item],
        drop_last: Option<usize>,
        section: Option<&str>,
        header: Axes,
    ) {
        self.header = header;
        self.known = header;
        let mut flat = flatten(body, self.program);
        // An assertion at the routine's top that the header states is the
        // header; writing it again inside says nothing.
        let mut index = 0;
        while index < flat.len() {
            match &flat[index] {
                Item::Trivia(_) => index += 1,
                Item::WidthAssert { axes, line }
                    if axes.a.is_none_or(|a| header.a == Some(a))
                        && axes.i.is_none_or(|i| header.i == Some(i)) =>
                {
                    let line = line.clone();
                    flat[index] = Item::Trivia(Line {
                        labels: Vec::new(),
                        body: None,
                        ..line
                    });
                    if flat[index].line().comment.is_none() {
                        flat.remove(index);
                    } else {
                        index += 1;
                    }
                }
                _ => break,
            }
        }
        let drop = drop_last.and_then(|index| {
            // The index is into `body`; find the same item in the flattened
            // copy, which only expands repeats and so keeps top-level order.
            let target = body[index].line().number;
            flat.iter().rposition(
                |item| matches!(item, Item::Instruction { line, .. } if line.number == target),
            )
        });
        self.body(&flat, 1, drop);
        let _ = section;
    }

    /// The statements of a section or an `inline`, at `depth`.
    fn body(&mut self, items: &[Item], depth: usize, drop: Option<usize>) {
        let saved = (
            std::mem::take(&mut self.locals),
            std::mem::take(&mut self.unnamed),
            self.position,
        );
        self.scope(items);
        self.position = 0;
        let mut index = 0;
        while index < items.len() {
            if Some(index) == drop {
                let item = &items[index];
                if item.line().comment.is_some() {
                    self.emit(depth, "", Some(item.line()));
                }
                index += 1;
                continue;
            }
            index = self.statement(items, index, depth);
        }
        self.locals = saved.0;
        self.unnamed = saved.1;
        self.position = saved.2;
    }

    /// The K816 names of the labels a body scopes.
    fn scope(&mut self, items: &[Item]) {
        let mut taken: BTreeSet<String> = BTreeSet::new();
        let mut unnamed_refs: Vec<(usize, i32)> = Vec::new();
        let mut count = 0usize;
        for item in items {
            match item {
                Item::Label(Label::Named(name), _) => {
                    if self
                        .program
                        .global_labels
                        .contains(&(self.unit, name.clone()))
                        || self.program.starts.contains(&(self.unit, name.clone()))
                        || self
                            .program
                            .definition(self.unit, name)
                            .is_some_and(|d| d.unit == self.unit && !matches!(d.kind, Kind::Code))
                    {
                        continue;
                    }
                    let local = unique(&format!(".{name}"), &mut taken);
                    self.locals.insert(name.clone(), local);
                }
                Item::Label(Label::Cheap(name), _) => {
                    let local = unique(&format!(".{name}"), &mut taken);
                    self.locals.insert(format!("@{name}"), local);
                }
                Item::Label(Label::Unnamed, _) => count += 1,
                Item::Instruction {
                    mnemonic, operand, ..
                } => {
                    if let Ok(shape) = render::operand(mnemonic, operand) {
                        let mut steps = Vec::new();
                        shape_unnamed(&shape, &mut steps);
                        for step in steps {
                            unnamed_refs.push((count, step));
                        }
                    }
                }
                // `blt :+` and a macro given a label are references too.
                Item::MacroUse { args, .. } => {
                    for arg in args {
                        if let Ok(value) = expr::parse_all(arg) {
                            let mut steps = Vec::new();
                            unnamed_in(&value, &mut steps);
                            for step in steps {
                                unnamed_refs.push((count, step));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        // An unnamed label is named for how it is reached: from below only is
        // a loop, from above only a skip.
        let mut backward = vec![false; count];
        let mut forward = vec![false; count];
        for (position, step) in unnamed_refs {
            let target = if step > 0 {
                position + step as usize - 1
            } else {
                position.wrapping_sub((-step) as usize)
            };
            if target < count {
                if step > 0 {
                    forward[target] = true;
                } else {
                    backward[target] = true;
                }
            }
        }
        self.unnamed = (0..count)
            .map(|index| {
                let base = match (backward[index], forward[index]) {
                    (true, false) => ".loop",
                    (false, true) => ".skip",
                    _ => ".next",
                };
                unique(base, &mut taken)
            })
            .collect();
    }

    /// Writes the statement at `items[index]`, and returns where the next
    /// one starts.
    fn statement(&mut self, items: &[Item], index: usize, depth: usize) -> usize {
        let item = &items[index];
        match item {
            Item::Trivia(line) => self.trivia(depth, line),
            Item::Label(label, line) => {
                // A label joins whatever reaches it; what the conversion can
                // vouch for there is the header.
                self.known = self.header;
                let text = match label {
                    Label::Named(name) => self
                        .locals
                        .get(name)
                        .cloned()
                        .unwrap_or_else(|| self.program.spelling(self.unit, name)),
                    Label::Cheap(name) => self
                        .locals
                        .get(&format!("@{name}"))
                        .cloned()
                        .unwrap_or_else(|| format!(".{name}")),
                    Label::Unnamed => {
                        let name = self
                            .unnamed
                            .get(self.position)
                            .cloned()
                            .unwrap_or_else(|| ".next".to_owned());
                        self.position += 1;
                        name
                    }
                };
                // A label alone on its line keeps the line's comment, and
                // sits at column 0, as `k816 fmt` puts every label.
                let alone = line.body.is_none() && line.labels.len() == 1;
                let _ = depth;
                self.emit(0, &format!("{text}:"), alone.then_some(line));
            }
            Item::Instruction {
                mnemonic,
                operand,
                widths,
                line,
            } => {
                for (number, code) in self
                    .instruction(mnemonic, operand, *widths, line)
                    .into_iter()
                    .enumerate()
                {
                    self.emit(depth, &code, (number == 0).then_some(line));
                }
            }
            Item::WidthSet { mnemonic, mask, .. } => {
                let set = items::mask_axes(mnemonic, *mask);
                if let Some(next) = self.width_block(items, index, depth) {
                    return next;
                }
                self.known.a = set.a.or(self.known.a);
                self.known.i = set.i.or(self.known.i);
                let Item::WidthSet {
                    axes,
                    mnemonic,
                    mask,
                    more,
                    line,
                } = item
                else {
                    unreachable!("matched above");
                };
                let pure = mask & !0x30 == 0 && items::mask_axes(mnemonic, *mask) == *axes;
                if pure {
                    let carrier = if line.comment.is_some() {
                        line
                    } else {
                        more.iter()
                            .find(|line| line.comment.is_some())
                            .unwrap_or(line)
                    };
                    self.emit(depth, &axes.annotations(), Some(carrier));
                } else if more.is_empty() {
                    self.emit(depth, &format!("{mnemonic} #${mask:02X}"), Some(line));
                } else {
                    let operand = match &line.body {
                        Some(source::Body::Instruction { operand, .. }) => tokens_text(operand),
                        _ => format!("#${mask:02X}"),
                    };
                    self.emit(depth, &format!("{mnemonic} {operand}"), Some(line));
                    for extra in more {
                        if extra.comment.is_some() {
                            self.emit(depth, "", Some(extra));
                        }
                    }
                }
            }
            Item::WidthAssert { axes, line } => {
                self.known.a = axes.a.or(self.known.a);
                self.known.i = axes.i.or(self.known.i);
                self.emit(depth, &axes.annotations(), Some(line));
            }
            Item::MacroUse {
                name, args, line, ..
            } => {
                // A macro body that sets a width leaves it known.
                if let Some(definition) = self.program.macros.get(name) {
                    for body in &definition.body {
                        if let Some(source::Body::Instruction { mnemonic, operand }) = &body.body
                            && (mnemonic == "rep" || mnemonic == "sep")
                            && let Some(mask) = items::immediate(operand)
                        {
                            let set = items::mask_axes(mnemonic, mask);
                            self.known.a = set.a.or(self.known.a);
                            self.known.i = set.i.or(self.known.i);
                        }
                    }
                }
                self.macro_use(name, args, line, depth);
            }
            Item::If { branches, end } => {
                let mut nested =
                    |writer: &mut Self, items: &[Item]| writer.body_inline(items, depth + 1);
                self.conditional(depth, branches, end, &mut nested);
            }
            Item::Assign { .. } | Item::Define { .. } => {}
            Item::Ignored(line) | Item::Linkage { line, .. } => {
                if line.comment.is_some() {
                    self.emit(depth, "", Some(line));
                }
            }
            Item::Unsupported(line, why) => self.unsupported(depth, line, why),
            other => {
                let line = other.line().clone();
                self.unsupported(depth, &line, "this cannot appear inside a routine here");
            }
        }
        index + 1
    }

    /// Items written in the current scope, at `depth`, without a new one.
    fn body_inline(&mut self, items: &[Item], depth: usize) {
        let mut index = 0;
        while index < items.len() {
            index = self.statement(items, index, depth);
        }
    }

    /// `_a8 … _a16` as `@a8 { … }`, when the region between them is
    /// straight-line code that sets nothing else on that axis.
    fn width_block(&mut self, items: &[Item], index: usize, depth: usize) -> Option<usize> {
        let Item::WidthSet {
            axes,
            mnemonic,
            mask,
            line,
            more,
        } = &items[index]
        else {
            return None;
        };
        if mask & !0x30 != 0 || items::mask_axes(mnemonic, *mask) != *axes {
            return None;
        }
        // The compiler restores the width it knew at the block's opening, and
        // nothing where it knew none.
        if axes.a.is_some() && self.known.a.is_none() || axes.i.is_some() && self.known.i.is_none()
        {
            return None;
        }
        let before = widths_before(items, index)?;
        let restore = Axes {
            a: axes.a.map(|_| before.a),
            i: axes.i.map(|_| before.i),
        };
        if restore == *axes {
            return None;
        }
        for (at, item) in items.iter().enumerate().skip(index + 1) {
            match item {
                Item::WidthSet {
                    axes: closing,
                    mask: closing_mask,
                    mnemonic: closing_mnemonic,
                    ..
                } => {
                    let pure = closing_mask & !0x30 == 0
                        && items::mask_axes(closing_mnemonic, *closing_mask) == *closing;
                    if pure && *closing == restore {
                        if at == index + 1 {
                            return None;
                        }
                        let carrier = if line.comment.is_some() {
                            line
                        } else {
                            more.iter()
                                .find(|line| line.comment.is_some())
                                .unwrap_or(line)
                        };
                        self.emit(depth, &format!("{} {{", axes.annotations()), Some(carrier));
                        self.body_inline(&items[index + 1..at], depth + 1);
                        let end_line = items[at].line().clone();
                        self.emit(depth, "}", Some(&end_line));
                        return Some(at + 1);
                    }
                    return None;
                }
                Item::Label(..)
                | Item::WidthAssert { .. }
                | Item::If { .. }
                | Item::Unsupported(..) => return None,
                Item::Instruction { mnemonic, .. } => {
                    if program::is_terminator(mnemonic)
                        || program::is_branch(mnemonic)
                        || matches!(
                            mnemonic.as_str(),
                            "jmp"
                                | "jml"
                                | "rti"
                                | "brk"
                                | "cop"
                                | "plp"
                                | "rep"
                                | "sep"
                                | "xce"
                                | "wai"
                                | "stp"
                        )
                    {
                        return None;
                    }
                }
                Item::MacroUse { name, .. } => {
                    if let Some(definition) = self.program.macros.get(name)
                        && definition.body.iter().any(|line| !line.labels.is_empty() || matches!(&line.body, Some(source::Body::Instruction { mnemonic, .. }) if program::is_branch(mnemonic) || program::is_terminator(mnemonic) || mnemonic == "rep" || mnemonic == "sep"))
                    {
                        return None;
                    }
                    if !self.program.macros.contains_key(name)
                        && program::generic_macro(name).is_some_and(|m| m != "add" && m != "sub")
                    {
                        return None;
                    }
                }
                Item::Trivia(_) => {}
                _ => return None,
            }
        }
        None
    }

    /// One instruction, as one or more K816 lines.
    fn instruction(
        &mut self,
        mnemonic: &str,
        tokens: &[Token],
        widths: Widths,
        line: &Line,
    ) -> Vec<String> {
        // A repeat count an `inline` body was given: `lsr * n`.
        if mnemonic.contains(" * ") {
            return vec![mnemonic.to_owned()];
        }
        let shape = match render::operand(mnemonic, tokens) {
            Ok(shape) => shape,
            Err(why) => {
                self.note(line, why.clone());
                return vec![format!("// k816 convert: {why}: {}", line.text.trim())];
            }
        };
        self.note_bridge(mnemonic, &shape, widths, line);
        self.note_code_read(mnemonic, &shape, line);
        let mut lines = Vec::new();
        if shape_has_here(&shape) {
            let label = unique(".here", &mut self.locals.values().cloned().collect());
            lines.push(format!("{label}:"));
            self.here = Some(label);
        }
        let result = self.render_instruction(mnemonic, &shape, widths);
        self.here = None;
        match result {
            Ok(code) => lines.push(code),
            Err(why) => {
                self.note(line, why.clone());
                lines.push(format!("// k816 convert: {why}: {}", line.text.trim()));
            }
        }
        lines
    }

    /// An instruction that reads a routine's own bytes — `lda routine` — which
    /// K816 refuses (`K816-E0115`): the optimizer rewrites a section's body,
    /// so the address of its first byte is not one it keeps. The line is
    /// converted as written, and the compiler says what to do instead.
    fn note_code_read(&mut self, mnemonic: &str, shape: &Operand, line: &Line) {
        let Operand::Direct { expr, .. } = shape else {
            return;
        };
        if matches!(mnemonic, "jsr" | "jsl" | "jmp" | "jml") {
            return;
        }
        let Some(name) = expr.name() else {
            return;
        };
        if self.is_section(name) {
            self.note(
                line,
                format!(
                    "`{mnemonic}` reads the bytes of the routine `{name}`, which K816 does \
                     not allow; put a label at the place to read and read through it"
                ),
            );
        }
    }

    /// A call that K816 bridges where the original did not: the callee's
    /// header states a width the call site does not have, so the converted
    /// program sets it before the call — a `sep`/`rep` the ca65 build lacks,
    /// and a place the source disagreed with itself.
    fn note_bridge(&mut self, mnemonic: &str, shape: &Operand, widths: Widths, line: &Line) {
        let Operand::Direct {
            prefix: None,
            expr,
            index: None,
        } = shape
        else {
            return;
        };
        if !matches!(mnemonic, "jsr" | "jsl") {
            return;
        }
        let Some(name) = expr.name() else {
            return;
        };
        if !self.is_section(name) {
            return;
        }
        let callee = callee_header(self.program, self.unit, name, &mut Callees::default());
        let mut differ = Vec::new();
        for (axis, wanted, have, stated) in [
            ("a", callee.a, widths.a, widths.a_stated),
            ("i", callee.i, widths.i, widths.i_stated),
        ] {
            let Some(wanted) = wanted else {
                continue;
            };
            if !stated {
                differ.push(format!(
                    "`@{axis}{}` where the original states none",
                    wanted.bits()
                ));
            } else if have != wanted {
                differ.push(format!(
                    "`@{axis}{}` where the original is at `@{axis}{}`",
                    wanted.bits(),
                    have.bits()
                ));
            }
        }
        if !differ.is_empty() {
            self.note(
                line,
                format!(
                    "the call to `{name}` is bridged to {}, so the converted program sets the \
                     width before it and the original does not",
                    differ.join(" and ")
                ),
            );
        }
    }

    fn render_instruction(
        &mut self,
        mnemonic: &str,
        shape: &Operand,
        widths: Widths,
    ) -> Result<String, String> {
        let names = self.names();
        let mnemonic = match mnemonic {
            "ina" => "inc",
            "dea" => "dec",
            other => other,
        };
        Ok(match shape {
            Operand::None => match mnemonic {
                "brk" => "brk #0".to_owned(),
                "cop" => "cop #0".to_owned(),
                _ => mnemonic.to_owned(),
            },
            Operand::Accumulator(register) => format!("{mnemonic} {register}"),
            Operand::Immediate(expr) => {
                format!("{mnemonic} #{}", render::expression(expr, &names)?)
            }
            Operand::Direct {
                prefix,
                expr,
                index,
            } => {
                // A call to a section is a call: the plain name.
                if (mnemonic == "jsr" || mnemonic == "jsl")
                    && prefix.is_none()
                    && index.is_none()
                    && let Some(name) = expr.name()
                    && self.is_section(name)
                {
                    let owner = self
                        .program
                        .definition(self.unit, name)
                        .map_or(self.unit, |d| d.unit);
                    let far = is_far_section(self.program, owner, name);
                    // A `jsl` to a `far func` is `far NAME`; a `jsr` to one, or
                    // a `jsl` to a near one, stays as written for the compiler
                    // to refuse, since the stack the two leave is not the same.
                    if far == (mnemonic == "jsl") {
                        let spelled = self.program.spelling(self.unit, name);
                        return Ok(if far {
                            format!("far {spelled}")
                        } else {
                            spelled
                        });
                    }
                }
                let rendered = render::expression(expr, &names)?;
                let value = match mnemonic {
                    "pea" | "cop" | "brk" | "wdm" => return Ok(format!("{mnemonic} #{rendered}")),
                    _ => rendered,
                };
                let marker = if matches!(index, Some('x' | 'y')) && self.const_rooted(expr) {
                    "&"
                } else {
                    ""
                };
                let prefix = match prefix {
                    Some('z') => "dp ",
                    Some('a') => "abs ",
                    Some('f') => "far ",
                    _ => "",
                };
                let index = match index {
                    Some(register) => format!(",{register}"),
                    None => String::new(),
                };
                let view = typed_view(mnemonic, expr, widths, &names);
                format!("{mnemonic} {prefix}{marker}{value}{view}{index}")
            }
            Operand::Indirect {
                prefix,
                expr,
                inner,
                outer,
            } => {
                let rendered = render::expression(expr, &names)?;
                let prefix = match prefix {
                    Some('z') => "dp ",
                    Some('a') => "abs ",
                    _ => "",
                };
                let inner = inner
                    .map(|register| format!(",{register}"))
                    .unwrap_or_default();
                let outer = outer
                    .map(|register| format!(",{register}"))
                    .unwrap_or_default();
                format!("{mnemonic} ({prefix}{rendered}{inner}){outer}")
            }
            Operand::Long { prefix, expr, y } => {
                let rendered = render::expression(expr, &names)?;
                let prefix = match prefix {
                    Some('z') => "dp ",
                    Some('a') => "abs ",
                    _ => "",
                };
                format!(
                    "{mnemonic} [{prefix}{rendered}]{}",
                    if *y { ",y" } else { "" }
                )
            }
            Operand::Move(source, destination) => format!(
                "{mnemonic} {},{}",
                render::expression(source, &names)?,
                render::expression(destination, &names)?
            ),
        })
    }

    /// Whether `name` is a section a bare call reaches.
    fn is_section(&self, name: &str) -> bool {
        match self.program.definition(self.unit, name) {
            Some(definition) => {
                definition.kind == Kind::Code
                    && (self
                        .program
                        .starts
                        .contains(&(definition.unit, name.to_owned()))
                        || self.program.units[definition.unit].items.iter().any(
                            |item| matches!(item, Item::Proc { name: proc, .. } if proc == name),
                        ))
            }
            None => false,
        }
    }

    /// Whether an address is a `const`'s value rather than a place: the
    /// operand K816 writes with `&` when an index register is added to it.
    fn const_rooted(&self, expr: &Expr) -> bool {
        let mut paths = Vec::new();
        expr.symbols(&mut paths);
        !paths.is_empty()
            && paths.iter().all(|path| match path {
                [name] => match self.program.definition(self.unit, name).map(|d| &d.kind) {
                    Some(Kind::Equate) => !self.program.addresses.contains(name),
                    Some(Kind::EnumMember) => true,
                    _ => false,
                },
                _ => false,
            })
            && paths.iter().any(|path| path.len() == 1)
    }

    /// A macro invocation.
    fn macro_use(&mut self, name: &str, args: &[Vec<Token>], line: &Line, depth: usize) {
        if let Some(definition) = self.program.macros.get(name).cloned() {
            self.macros_used.insert(name.to_owned());
            // A place parameter given an immediate is a different operand in
            // the body; only expanding it says that.
            let mismatch = definition.params.iter().zip(args).any(|(param, arg)| {
                matches!(
                    program::param_role(&definition, param),
                    Role::Address | Role::Call | Role::Jump
                ) && arg
                    .first()
                    .is_some_and(|token| token.tok == Tok::Punct("#"))
            });
            if mismatch || args.len() > definition.params.len() {
                return self.expand(&definition, args, line, depth);
            }
            let mut rendered = Vec::new();
            for arg in args {
                let tokens = match arg.first() {
                    Some(Token {
                        tok: Tok::Punct("#"),
                        ..
                    }) => &arg[1..],
                    _ => &arg[..],
                };
                match expr::parse_all(tokens)
                    .and_then(|value| render::expression(&value, &self.names()))
                {
                    Ok(value) => rendered.push(format!("#{value}")),
                    Err(why) => return self.unsupported(depth, line, &why),
                }
            }
            let spelled = self.program.spelling(self.unit, name);
            let code = if rendered.is_empty() {
                spelled
            } else {
                format!("{spelled} {}", rendered.join(", "))
            };
            self.emit(depth, &code, Some(line));
            return;
        }
        let Some(generic) = program::generic_macro(name) else {
            return self.unsupported(
                depth,
                line,
                &format!("`{name}` is not an instruction or a macro this converter knows"),
            );
        };
        let operand: Vec<Token> = args
            .iter()
            .enumerate()
            .flat_map(|(index, arg)| {
                let mut tokens = Vec::new();
                if index > 0 {
                    tokens.push(Token {
                        tok: Tok::Punct(","),
                        start: 0,
                        end: 0,
                        space_before: false,
                    });
                }
                tokens.extend(arg.iter().cloned());
                tokens
            })
            .collect();
        match generic {
            "add" | "sub" => {
                let immediate = operand
                    .first()
                    .is_some_and(|token| token.tok == Tok::Punct("#"));
                if immediate {
                    match expr::parse_all(&operand[1..])
                        .and_then(|value| render::expression(&value, &self.names()))
                    {
                        Ok(value) => {
                            self.uses_generic = true;
                            self.emit(depth, &format!("{generic} #{value}"), Some(line));
                        }
                        Err(why) => self.unsupported(depth, line, &why),
                    }
                } else {
                    let (flag, op) = if generic == "add" {
                        ("clc", "adc")
                    } else {
                        ("sec", "sbc")
                    };
                    self.emit(depth, flag, Some(line));
                    for code in self.instruction(op, &operand, Widths::default(), line) {
                        self.emit(depth, &code, None);
                    }
                }
            }
            "bgt" | "ble" => {
                let skip = unique(".over", &mut self.locals.values().cloned().collect());
                let target = self.instruction("bcs", &operand, Widths::default(), line);
                if generic == "bgt" {
                    self.emit(depth, &format!("beq {skip}"), Some(line));
                    for code in target {
                        self.emit(depth, &code, None);
                    }
                } else {
                    let to = self.instruction("beq", &operand, Widths::default(), line);
                    for code in to {
                        self.emit(depth, &code, Some(line));
                    }
                    for code in self.instruction("bcc", &operand, Widths::default(), line) {
                        self.emit(depth, &code, None);
                    }
                    return;
                }
                self.emit(0, &format!("{skip}:"), None);
            }
            branch => {
                for (number, code) in self
                    .instruction(branch, &operand, Widths::default(), line)
                    .into_iter()
                    .enumerate()
                {
                    self.emit(depth, &code, (number == 0).then_some(line));
                }
            }
        }
    }

    /// A macro expanded where it was used, for a use its `inline` cannot
    /// express.
    fn expand(&mut self, definition: &Macro, args: &[Vec<Token>], line: &Line, depth: usize) {
        self.emit(depth, &format!("// {}", line.text.trim()), None);
        let substitutions: BTreeMap<String, Vec<Token>> = definition
            .params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .collect();
        let lines: Vec<Line> = definition
            .body
            .iter()
            .map(|body| substitute(body, &substitutions))
            .collect();
        let lookup = |name: &str| self.program.macros.get(name).cloned();
        let mut builder = items::Builder {
            widths: Widths::default(),
            macros: &lookup,
        };
        let items = builder.items(&lines, &mut 0, &[]).0;
        self.body_inline(&items, depth);
    }

    /// A `.if` chain as K816 `#if` directives.
    fn conditional(
        &mut self,
        depth: usize,
        branches: &[items::Branch],
        end: &Line,
        write: &mut dyn FnMut(&mut Self, &[Item]),
    ) {
        for (index, branch) in branches.iter().enumerate() {
            let condition =
                expr::parse_all(&branch.condition)
                    .ok()
                    .and_then(|value| match &value {
                        Expr::Number { text, .. } => Some(text.clone()),
                        Expr::Symbol(path) if path.len() == 1 => {
                            Some(self.program.spelling(self.unit, &path[0]))
                        }
                        _ => None,
                    });
            let directive = match (branch.directive.as_str(), index, &condition) {
                ("else", _, _) => "#else".to_owned(),
                ("if", 0, Some(condition)) => format!("#if {condition}"),
                ("elseif", _, Some(condition)) => format!("#elif {condition}"),
                _ => {
                    self.note(
                        &branch.line,
                        "a `.if` condition K816's `#if` cannot state is written as `#if 1`",
                    );
                    "#if 1".to_owned()
                }
            };
            self.emit(depth, &directive, Some(&branch.line));
            write(self, &branch.items);
        }
        self.emit(depth, "#endif", Some(end));
    }

    /// The names a renderer asks about, as this unit and section see them.
    fn names(&self) -> UnitNames<'_> {
        UnitNames { writer: self }
    }
}

/// The whitespace after `name:` on a line that goes on to a directive, so a
/// column of one-line data entries stays a column.
fn label_gap(line: &Line, name: &str) -> String {
    let Some(start) = line.text.find(&format!("{name}:")) else {
        return " ".to_owned();
    };
    let rest = &line.text[start + name.len() + 1..];
    let gap: String = rest
        .chars()
        .take_while(|c| *c == ' ' || *c == '\t')
        .collect();
    if gap.is_empty() {
        " ".to_owned()
    } else {
        gap.replace('\t', " ")
    }
}

/// The names as one unit's renderer sees them.
struct UnitNames<'w> {
    writer: &'w Writer<'w>,
}

impl Names for UnitNames<'_> {
    fn spelling(&self, name: &str) -> String {
        if let Some(local) = self.writer.locals.get(name) {
            return local.clone();
        }
        self.writer.program.spelling(self.writer.unit, name)
    }

    fn meaning(&self, name: &str) -> Meaning {
        let program = self.writer.program;
        match program.definition(self.writer.unit, name).map(|d| &d.kind) {
            Some(Kind::Element { table, index }) => Meaning::Element(format!(
                "{}[{index}]",
                program.spelling(self.writer.unit, table)
            )),
            Some(Kind::Layout { fixed: true }) => Meaning::FixedLayout,
            Some(Kind::Layout { fixed: false }) => Meaning::AbstractLayout,
            Some(Kind::Equate) if !program.addresses.contains(name) => Meaning::Value,
            Some(Kind::EnumMember) => Meaning::Value,
            _ => Meaning::Place,
        }
    }

    fn layout_of(&self, name: &str) -> Option<String> {
        let program = self.writer.program;
        match program.definition(self.writer.unit, name).map(|d| &d.kind) {
            Some(Kind::Table { layout, .. }) => Some(layout.clone()),
            Some(Kind::Element { table, .. }) => {
                match program.definition(self.writer.unit, table).map(|d| &d.kind) {
                    Some(Kind::Table { layout, .. }) => Some(layout.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn cheap(&self, name: &str) -> String {
        self.writer
            .locals
            .get(&format!("@{name}"))
            .cloned()
            .unwrap_or_else(|| format!(".{name}"))
    }

    fn unnamed(&self, steps: i32) -> String {
        let position = self.writer.position;
        let target = if steps > 0 {
            position + steps as usize - 1
        } else {
            position.wrapping_sub((-steps) as usize)
        };
        self.writer
            .unnamed
            .get(target)
            .cloned()
            .unwrap_or_else(|| ".next".to_owned())
    }

    fn here(&self) -> String {
        self.writer
            .here
            .clone()
            .unwrap_or_else(|| ".here".to_owned())
    }

    fn value(&self, path: &[String]) -> Option<i64> {
        self.writer.program.value(path)
    }

    fn field_width(&self, layout: &str, fields: &[String]) -> Option<u8> {
        let (field, deeper) = fields.split_first()?;
        let definition = self.writer.program.structs.get(layout)?;
        let member = definition
            .members
            .iter()
            .find(|member| member.name.as_deref() == Some(field.as_str()))?;
        match (&member.kind, deeper) {
            // A `.dword` is two words, which is how the layout declares it.
            (MemberKind::Data { size: 4, .. }, []) => Some(2),
            (MemberKind::Data { size, .. }, []) => u8::try_from(*size).ok(),
            (MemberKind::Tag(nested), _) if !deeper.is_empty() => self.field_width(nested, deeper),
            _ => None,
        }
    }

    fn is_byte_array(&self, layout: &str, field: &str) -> bool {
        self.writer
            .program
            .structs
            .get(layout)
            .is_some_and(|definition| {
                definition.members.iter().any(|member| {
                    member.name.as_deref() == Some(field)
                        && matches!(
                            &member.kind,
                            MemberKind::Data {
                                size: 1,
                                count: Some(_)
                            }
                        )
                })
            })
    }
}

/// Whether a section returns from an interrupt: an `rti` in its body, in a
/// branch of a conditional, or in a macro it uses.
fn returns_from_interrupt(program: &Program, body: &[Item]) -> bool {
    body.iter().any(|item| match item {
        Item::Instruction { mnemonic, .. } => mnemonic == "rti",
        Item::If { branches, .. } => branches
            .iter()
            .any(|branch| returns_from_interrupt(program, &branch.items)),
        Item::MacroUse { name, .. } => program.macros.get(name).is_some_and(|definition| {
            definition.body.iter().any(|line| {
                matches!(&line.body, Some(source::Body::Instruction { mnemonic, .. }) if mnemonic == "rti")
            })
        }),
        Item::Repeat { items, .. } => returns_from_interrupt(program, items),
        _ => false,
    })
}

/// The use-site view an access needs when the register moving its datum is
/// not as wide as the field it names: ca65 moves the bytes the width says,
/// K816 holds a field to its declared width, and `:word` says the original
/// meant the two bytes from there on. Empty where the two agree, and for an
/// instruction that moves its datum through no register (`stz`, `inc`).
fn typed_view(mnemonic: &str, expr: &Expr, widths: Widths, names: &dyn Names) -> String {
    let register = match mnemonic {
        "lda" | "sta" | "adc" | "sbc" | "and" | "ora" | "eor" | "cmp" | "bit" | "tsb" | "trb" => {
            widths.a
        }
        "ldx" | "ldy" | "stx" | "sty" | "cpx" | "cpy" => widths.i,
        _ => return String::new(),
    };
    let held = match register {
        Width::W8 => 1,
        Width::W16 => 2,
    };
    match render::declared_width(expr, names) {
        Some(declared) if declared != held => if held == 1 { ":byte" } else { ":word" }.to_owned(),
        _ => String::new(),
    }
}

/// The end of the section of `unit` that starts at `items[index]`.
fn section_end(program: &Program, unit: usize, items: &[Item], index: usize) -> usize {
    let mut at = index + 1;
    let mut after_terminator = false;
    while at < items.len() {
        match &items[at] {
            Item::Instruction { mnemonic, .. } => {
                after_terminator = program::is_terminator(mnemonic)
            }
            Item::WidthSet { .. }
            | Item::MacroUse { .. }
            | Item::Repeat { .. }
            | Item::If { .. } => after_terminator = false,
            // An equate after the routine's last way out is the next
            // thing in the file, not part of the routine.
            Item::Assign { .. } | Item::Define { .. } if after_terminator => break,
            Item::Label(Label::Named(name), _) => {
                if program.starts.contains(&(unit, name.clone())) {
                    break;
                }
                if !matches!(
                    program.definition(unit, name).map(|d| &d.kind),
                    Some(Kind::Code)
                ) {
                    break;
                }
            }
            Item::Data { .. }
            | Item::Segment(..)
            | Item::Org(..)
            | Item::Proc { .. }
            | Item::Struct(_)
            | Item::Enum(_)
            | Item::Macro(_) => break,
            _ => {}
        }
        at += 1;
    }
    // Comments and blank lines after the last code belong to whatever
    // follows, and so does a width directive: it states the width of
    // what comes after it, which is the next section's business.
    while at > index + 1
        && matches!(
            &items[at - 1],
            Item::Trivia(_) | Item::Linkage { .. } | Item::WidthAssert { .. }
        )
    {
        at -= 1;
    }
    at
}

/// The first item at or after `index` that is not a comment or blank line.
fn next_meaningful(items: &[Item], index: usize) -> Option<&Item> {
    items[index.min(items.len())..]
        .iter()
        .find(|item| !matches!(item, Item::Trivia(_)))
}

/// Whether the section `name` starts in its unit ends with `rtl`: a `far
/// func`, reached by `far name`.
fn is_far_section(program: &Program, unit: usize, name: &str) -> bool {
    let items = &program.units[unit].items;
    let Some(index) = items
        .iter()
        .position(|item| matches!(item, Item::Label(Label::Named(label), _) if label == name))
    else {
        return items.iter().any(|item| matches!(item, Item::Proc { name: proc, items, .. } if proc == name && last_mnemonic(items) == Some("rtl")));
    };
    let end = section_end(program, unit, items, index);
    last_mnemonic(&items[index + 1..end]) == Some("rtl")
}

/// The mnemonic of the last instruction in `items`.
fn last_mnemonic(items: &[Item]) -> Option<&str> {
    items.iter().rev().find_map(|item| match item {
        Item::Instruction { mnemonic, .. } => Some(mnemonic.as_str()),
        _ => None,
    })
}

/// A ca65 comment as a K816 one. One that already wrote `//` after its
/// `;` keeps that one.
fn comment_text(comment: &str) -> String {
    let extra = comment.chars().take_while(|c| *c == ';').count();
    let text = &comment[extra..];
    if extra == 0 && text.trim_start().starts_with("//") {
        return text.trim_start().to_owned();
    }
    format!("//{}{}", "/".repeat(extra), text)
}

/// The whitespace a source line wrote around the `=` of an assignment.
fn assignment_gaps(line: &Line, name: &str) -> (String, String) {
    let text = &line.text;
    let Some(start) = text.find(name) else {
        return (" ".to_owned(), " ".to_owned());
    };
    let rest = &text[start + name.len()..];
    let before: String = rest.chars().take_while(|c| c.is_whitespace()).collect();
    let after_eq = rest[before.len()..].trim_start_matches([':', '=']);
    let after: String = after_eq.chars().take_while(|c| c.is_whitespace()).collect();
    (
        if before.is_empty() {
            String::new()
        } else {
            before.replace('\t', " ")
        },
        if after.is_empty() {
            String::new()
        } else {
            after.replace('\t', " ")
        },
    )
}

/// `name`, or `name2`, `name3` … when one is taken.
fn unique(base: &str, taken: &mut BTreeSet<String>) -> String {
    if taken.insert(base.to_owned()) {
        return base.to_owned();
    }
    let mut counter = 2;
    loop {
        let candidate = format!("{base}{counter}");
        if taken.insert(candidate.clone()) {
            return candidate;
        }
        counter += 1;
    }
}

/// A macro parameter's K816 alias name.
fn alias_spelling(param: &str) -> String {
    if super::usable_name(param) {
        param.to_owned()
    } else {
        format!("{param}_")
    }
}

/// `line` with parameter names replaced by their K816 spellings.
fn rename_tokens(line: &Line, renamed: &BTreeMap<String, String>) -> Line {
    let mut line = line.clone();
    let rename = |tokens: &mut Vec<Token>| {
        for token in tokens.iter_mut() {
            if let Tok::Ident(name) = &token.tok
                && let Some(spelled) = renamed.get(name)
            {
                token.tok = Tok::Ident(spelled.clone());
            }
        }
    };
    match &mut line.body {
        Some(source::Body::Instruction { operand, .. }) => rename(operand),
        Some(source::Body::Directive { args, .. }) => rename(args),
        Some(source::Body::Name { rest, .. }) => rename(rest),
        Some(source::Body::Assign { value, .. }) => rename(value),
        None => {}
    }
    line
}

/// `line` with each parameter replaced by the tokens its argument was.
fn substitute(line: &Line, substitutions: &BTreeMap<String, Vec<Token>>) -> Line {
    let mut line = line.clone();
    let replace = |tokens: &mut Vec<Token>| {
        let mut out = Vec::new();
        for token in tokens.drain(..) {
            match &token.tok {
                Tok::Ident(name) if substitutions.contains_key(name) => {
                    let mut replacement = substitutions[name].clone();
                    if let Some(first) = replacement.first_mut() {
                        first.space_before = token.space_before;
                    }
                    out.extend(replacement);
                }
                _ => out.push(token),
            }
        }
        *tokens = out;
    };
    match &mut line.body {
        Some(source::Body::Instruction { operand, .. }) => replace(operand),
        Some(source::Body::Directive { args, .. }) => replace(args),
        Some(source::Body::Name { rest, .. }) => replace(rest),
        Some(source::Body::Assign { value, .. }) => replace(value),
        None => {}
    }
    line
}

/// Tokens written back as their source text, single-spaced.
fn tokens_text(tokens: &[Token]) -> String {
    let mut out = String::new();
    for token in tokens {
        if token.space_before && !out.is_empty() {
            out.push(' ');
        }
        out.push_str(&expr::describe(&token.tok));
    }
    out
}

/// The bytes a data directive's values occupy.
fn data_size(directive: &str, args: &[Token]) -> usize {
    let per = items::DATA_DIRECTIVES
        .iter()
        .find(|(name, _)| *name == directive)
        .map_or(1, |(_, size)| *size as usize);
    source::split_commas(args).len() * per.max(1)
}

/// A string as K816 writes one, from the bytes ca65 read.
fn k816_string(bytes: &[u8]) -> String {
    let mut out = String::from("\"");
    let mut pieces = Vec::new();
    for &byte in bytes {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\r' => out.push_str("\\r"),
            b'\t' => out.push_str("\\t"),
            0 => out.push_str("\\0"),
            0x20..=0x7E => out.push(char::from(byte)),
            other => {
                out.push('"');
                pieces.push(std::mem::replace(&mut out, "\"".to_owned()));
                pieces.push(format!("${other:02X}"));
            }
        }
    }
    out.push('"');
    if out != "\"\"" || pieces.is_empty() {
        pieces.push(out);
    }
    pieces.join(" ")
}

/// A byte-sized data value K816 accepts as an atom.
fn byte_value(value: &Expr, names: &dyn Names) -> Result<String, String> {
    match value {
        Expr::Symbol(_) | Expr::Number { .. } | Expr::Char { .. } => {
            render::expression(value, names)
        }
        Expr::Unary(expr::Unary::LowByte | expr::Unary::HighByte, _) => {
            render::expression(value, names)
        }
        _ => {
            // A compound value over constants: the evaluator computes it.
            let mut paths = Vec::new();
            value.symbols(&mut paths);
            let constants = paths.iter().all(|path| {
                names.meaning(&path[0]) == Meaning::Value || names.value(path).is_some()
            });
            if constants {
                Ok(format!("[{}]", render::expression(value, names)?))
            } else {
                Err(
                    "a data value over an address that K816 has no single-atom spelling for"
                        .to_owned(),
                )
            }
        }
    }
}

/// A word-sized data value K816 accepts as an atom.
fn word_value(value: &Expr, names: &dyn Names) -> Result<String, String> {
    match value {
        Expr::Symbol(path) if path.len() == 1 => render::expression(value, names),
        Expr::Number { .. } | Expr::Char { .. } => render::expression(value, names),
        _ => {
            let mut paths = Vec::new();
            value.symbols(&mut paths);
            let constants = paths.iter().all(|path| {
                names.meaning(&path[0]) == Meaning::Value
                    || (path.len() > 1 && names.meaning(&path[0]) == Meaning::AbstractLayout)
            });
            if constants {
                Ok(format!("[{}]", render::expression(value, names)?))
            } else {
                // An address and an offset: the 16-bit address-of operator
                // takes the whole expression.
                Ok(format!("&&{}", render::expression(value, names)?))
            }
        }
    }
}

/// Every unnamed reference an operand makes, as its step.
fn shape_unnamed(shape: &Operand, out: &mut Vec<i32>) {
    let mut visit = |expr: &Expr| unnamed_in(expr, out);
    match shape {
        Operand::Immediate(expr)
        | Operand::Direct { expr, .. }
        | Operand::Indirect { expr, .. }
        | Operand::Long { expr, .. } => visit(expr),
        Operand::Move(a, b) => {
            visit(a);
            visit(b);
        }
        Operand::None | Operand::Accumulator(_) => {}
    }
}

fn unnamed_in(expr: &Expr, out: &mut Vec<i32>) {
    match expr {
        Expr::Unnamed(step) => out.push(*step),
        Expr::Unary(_, inner) | Expr::Paren(inner) => unnamed_in(inner, out),
        Expr::Binary { left, right, .. } => {
            unnamed_in(left, out);
            unnamed_in(right, out);
        }
        Expr::Function { arguments, .. } => {
            for argument in arguments {
                unnamed_in(argument, out);
            }
        }
        _ => {}
    }
}

/// Whether an operand names `*`.
fn shape_has_here(shape: &Operand) -> bool {
    fn has(expr: &Expr) -> bool {
        match expr {
            Expr::Here => true,
            Expr::Unary(_, inner) | Expr::Paren(inner) => has(inner),
            Expr::Binary { left, right, .. } => has(left) || has(right),
            _ => false,
        }
    }
    match shape {
        Operand::Immediate(expr)
        | Operand::Direct { expr, .. }
        | Operand::Indirect { expr, .. }
        | Operand::Long { expr, .. } => has(expr),
        _ => false,
    }
}

/// The widths ca65 assumed just before `items[index]`, from the nearest
/// instruction or macro use after it — which recorded the state after
/// every directive between — undone by the set at `index` itself.
fn widths_before(items: &[Item], index: usize) -> Option<Widths> {
    // Walk back to the last item that recorded widths, and replay forward.
    let mut state = None;
    for item in items[..index].iter().rev() {
        match item {
            Item::Instruction { widths, .. } | Item::MacroUse { widths, .. } => {
                state = Some(*widths);
                break;
            }
            _ => {}
        }
    }
    let mut state = state?;
    let start = items[..index]
        .iter()
        .rposition(|item| matches!(item, Item::Instruction { .. } | Item::MacroUse { .. }))
        .map_or(0, |position| position + 1);
    for item in &items[start..index] {
        match item {
            Item::WidthSet { axes, .. } | Item::WidthAssert { axes, .. } => axes.apply(&mut state),
            _ => {}
        }
    }
    Some(state)
}

/// A section's header: for each axis, the width the original relied on at
/// entry — the first instruction that reads the flag comes before anything
/// sets it — or nothing where the body sets the width first.
pub fn header(program: &Program, unit: usize, body: &[Item]) -> Axes {
    scan_header(program, unit, body, &mut Callees::default())
}

/// The callees one header scan has reached: those it is inside, which a
/// call back into contributes nothing to, and those it has finished.
#[derive(Default)]
struct Callees {
    visiting: Vec<(usize, String)>,
    done: BTreeMap<(usize, String), Axes>,
}

/// The header of the section `name` starts, as a call to it sees it: what
/// [`header`] says, and nothing for an interrupt handler, which states none.
fn callee_header(program: &Program, unit: usize, name: &str, callees: &mut Callees) -> Axes {
    let Some(owner) = program
        .definition(unit, name)
        .map(|definition| definition.unit)
    else {
        return Axes::default();
    };
    let key = (owner, name.to_owned());
    if let Some(axes) = callees.done.get(&key) {
        return *axes;
    }
    // A cycle of tail jumps: the width the loop relies on is what its first
    // section's own instructions say, which the scan already inside it reads.
    if callees.visiting.contains(&key) {
        return Axes::default();
    }
    let items = &program.units[owner].items;
    let body = if let Some(index) = items
        .iter()
        .position(|item| matches!(item, Item::Label(Label::Named(label), _) if label == name))
    {
        &items[index + 1..section_end(program, owner, items, index)]
    } else if let Some(Item::Proc { items: body, .. }) = items
        .iter()
        .find(|item| matches!(item, Item::Proc { name: proc, .. } if proc == name))
    {
        body.as_slice()
    } else {
        return Axes::default();
    };
    if returns_from_interrupt(program, body) {
        return Axes::default();
    }
    callees.visiting.push(key.clone());
    let axes = scan_header(program, owner, body, callees);
    callees.visiting.pop();
    callees.done.insert(key, axes);
    axes
}

fn scan_header(program: &Program, unit: usize, body: &[Item], callees: &mut Callees) -> Axes {
    // Every branch of a conditional is code the entry may reach, so the
    // scan reads them all, in order.
    fn branches(items: Vec<Item>) -> Vec<Item> {
        let mut out = Vec::new();
        for item in items {
            match item {
                Item::If {
                    branches: chain, ..
                } => {
                    for branch in chain {
                        out.extend(branches(branch.items));
                    }
                }
                other => out.push(other),
            }
        }
        out
    }
    let flat = branches(flatten(body, program));
    let mut header = Axes::default();
    let (mut a_done, mut i_done) = (false, false);
    for item in &flat {
        match item {
            Item::WidthSet { axes, .. } => {
                a_done |= axes.a.is_some();
                i_done |= axes.i.is_some();
                // A `rep`/`sep` mask that sets more than its directives say
                // still sets the flag.
                if let Item::WidthSet { mnemonic, mask, .. } = item {
                    let set = items::mask_axes(mnemonic, *mask);
                    a_done |= set.a.is_some();
                    i_done |= set.i.is_some();
                }
            }
            Item::WidthAssert { axes, .. } => {
                if !a_done && let Some(a) = axes.a {
                    header.a = Some(a);
                    a_done = true;
                }
                if !i_done && let Some(i) = axes.i {
                    header.i = Some(i);
                    i_done = true;
                }
            }
            Item::Instruction {
                mnemonic,
                operand,
                widths,
                ..
            } => {
                let (reads_a, reads_i, sized_a, sized_i) = reads(mnemonic, operand);
                if (mnemonic == "rep" || mnemonic == "sep")
                    && let Some(mask) = items::immediate(operand)
                {
                    let set = items::mask_axes(mnemonic, mask);
                    a_done |= set.a.is_some();
                    i_done |= set.i.is_some();
                    continue;
                }
                if mnemonic == "plp" {
                    a_done = true;
                    i_done = true;
                }
                if !a_done && reads_a && (widths.a_stated || sized_a) {
                    header.a = Some(widths.a);
                    a_done = true;
                }
                if !i_done && reads_i && (widths.i_stated || sized_i) {
                    header.i = Some(widths.i);
                    i_done = true;
                }
                // A call bridges to its callee's contract, and a tail jump
                // arrives at it; either way the caller relied on the width
                // it had there — on the axes the callee states, and only
                // those, since a bridge establishes nothing else.
                if matches!(mnemonic.as_str(), "jsr" | "jsl" | "jmp" | "jml")
                    && let Ok(Operand::Direct {
                        expr, index: None, ..
                    }) = render::operand(mnemonic, operand)
                    && let Some(name) = expr.name()
                    && program
                        .definition(unit, name)
                        .is_some_and(|d| d.kind == Kind::Code)
                {
                    let callee = callee_header(program, unit, name, callees);
                    if !a_done && widths.a_stated && callee.a.is_some() {
                        header.a = Some(widths.a);
                        a_done = true;
                    }
                    if !i_done && widths.i_stated && callee.i.is_some() {
                        header.i = Some(widths.i);
                        i_done = true;
                    }
                }
            }
            Item::MacroUse { name, widths, .. } => {
                if let Some(definition) = program.macros.get(name) {
                    for line in &definition.body {
                        if let Some(source::Body::Instruction { mnemonic, operand }) = &line.body {
                            if (mnemonic == "rep" || mnemonic == "sep")
                                && let Some(mask) = items::immediate(operand)
                            {
                                let set = items::mask_axes(mnemonic, mask);
                                a_done |= set.a.is_some();
                                i_done |= set.i.is_some();
                                continue;
                            }
                            let (reads_a, reads_i, sized_a, sized_i) = reads(mnemonic, operand);
                            if !a_done && reads_a && (widths.a_stated || sized_a) {
                                header.a = Some(widths.a);
                                a_done = true;
                            }
                            if !i_done && reads_i && (widths.i_stated || sized_i) {
                                header.i = Some(widths.i);
                                i_done = true;
                            }
                        }
                    }
                } else if program::generic_macro(name)
                    .is_some_and(|generic| generic == "add" || generic == "sub")
                    && !a_done
                {
                    header.a = Some(widths.a);
                    a_done = true;
                }
            }
            _ => {}
        }
        if a_done && i_done {
            break;
        }
    }
    header
}

/// Which width flags an instruction reads, and which of those make its own
/// encoding depend on the width — an immediate on that axis.
fn reads(mnemonic: &str, operand: &[Token]) -> (bool, bool, bool, bool) {
    let immediate = operand
        .first()
        .is_some_and(|token| token.tok == Tok::Punct("#"));
    let indexed = operand.iter().any(|token| matches!(&token.tok, Tok::Ident(r) if r.eq_ignore_ascii_case("x") || r.eq_ignore_ascii_case("y")))
        && operand.iter().any(|token| token.tok == Tok::Punct(","));
    let accumulator = matches!(
        mnemonic,
        "lda"
            | "sta"
            | "adc"
            | "sbc"
            | "and"
            | "ora"
            | "eor"
            | "cmp"
            | "bit"
            | "stz"
            | "tsb"
            | "trb"
            | "pha"
            | "pla"
            | "txa"
            | "tya"
    ) || (matches!(
        mnemonic,
        "inc" | "dec" | "asl" | "lsr" | "rol" | "ror" | "ina" | "dea"
    ));
    let index = matches!(
        mnemonic,
        "ldx"
            | "ldy"
            | "stx"
            | "sty"
            | "cpx"
            | "cpy"
            | "inx"
            | "iny"
            | "dex"
            | "dey"
            | "phx"
            | "phy"
            | "plx"
            | "ply"
            | "tax"
            | "tay"
            | "txy"
            | "tyx"
            | "tsx"
            | "mvn"
            | "mvp"
    ) || indexed;
    let sized_a = immediate
        && matches!(
            mnemonic,
            "lda" | "adc" | "sbc" | "and" | "ora" | "eor" | "cmp" | "bit"
        );
    let sized_i = immediate && matches!(mnemonic, "ldx" | "ldy" | "cpx" | "cpy");
    (accumulator, index, sized_a, sized_i)
}

/// A body with its `.repeat`s over code written out, which is what K816
/// needs — it has no repetition of statements — and what makes the unnamed
/// labels inside resolve as ca65 resolves them.
fn flatten(items: &[Item], program: &Program) -> Vec<Item> {
    let mut out = Vec::new();
    for item in items {
        match item {
            Item::Repeat {
                count,
                var,
                items: body,
                line,
                end,
            } => {
                let Some(times) = count.fold(&|path| program.value(path)) else {
                    out.push(Item::Unsupported(
                        line.clone(),
                        "a `.repeat` whose count does not fold".to_owned(),
                    ));
                    continue;
                };
                for index in 0..times.max(0) {
                    if index == 0 && line.comment.is_some() {
                        out.push(Item::Trivia(Line {
                            labels: Vec::new(),
                            body: None,
                            ..line.clone()
                        }));
                    }
                    for inner in body {
                        out.push(match var {
                            Some(var) => substitute_item(inner, var, index),
                            None => inner.clone(),
                        });
                    }
                }
                if end.comment.is_some() {
                    out.push(Item::Trivia(Line {
                        labels: Vec::new(),
                        body: None,
                        ..end.clone()
                    }));
                }
            }
            other => out.push(other.clone()),
        }
    }
    out
}

/// An item of a `.repeat` body with its counter replaced by `index`.
/// A directive's arguments with every one that is arithmetic over numbers
/// alone written as the number it is: what a `.repeat` counter leaves
/// behind once it is substituted, `.byte 2 * 2` for `.byte i * 2`.
fn fold_literals(args: &[Token]) -> Vec<Token> {
    let parts = source::split_commas(args);
    let mut out = Vec::new();
    for (index, part) in parts.iter().enumerate() {
        if index > 0 {
            let comma = args
                .iter()
                .find(|token| token.tok == Tok::Punct(","))
                .cloned();
            out.extend(comma);
        }
        let folded = expr::parse_all(part).ok().and_then(|value| {
            let mut paths = Vec::new();
            value.symbols(&mut paths);
            if !paths.is_empty() {
                return None;
            }
            value.fold(&|_| None).filter(|number| *number >= 0)
        });
        match (folded, part.first()) {
            (Some(number), Some(first)) if part.len() > 1 => out.push(Token {
                tok: Tok::Number {
                    value: u32::try_from(number).unwrap_or(0),
                    text: number.to_string(),
                },
                ..first.clone()
            }),
            _ => out.extend(part.iter().cloned()),
        }
    }
    out
}

fn substitute_item(item: &Item, var: &str, index: i64) -> Item {
    let replace = |tokens: &[Token]| -> Vec<Token> {
        tokens
            .iter()
            .map(|token| match &token.tok {
                Tok::Ident(name) if name == var => Token {
                    tok: Tok::Number {
                        value: u32::try_from(index).unwrap_or(0),
                        text: index.to_string(),
                    },
                    ..token.clone()
                },
                _ => token.clone(),
            })
            .collect()
    };
    match item {
        Item::Instruction {
            mnemonic,
            operand,
            widths,
            line,
        } => Item::Instruction {
            mnemonic: mnemonic.clone(),
            operand: replace(operand),
            widths: *widths,
            line: line.clone(),
        },
        Item::Data {
            directive,
            args,
            line,
        } => Item::Data {
            directive: directive.clone(),
            args: replace(args),
            line: line.clone(),
        },
        other => other.clone(),
    }
}
