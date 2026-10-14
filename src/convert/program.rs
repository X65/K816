/*****************************************************************************/
/*                                                                           */
/*                                 program.rs                                */
/*                                                                           */
/*            Every file of a ca65 program, and what its names are           */
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

//! The whole program, because a unit's conversion depends on all of it.
//!
//! Which `import` a unit writes depends on which unit defines each name it
//! reads; whether a label starts a section depends on who jumps to it from
//! where; whether a ca65 equate becomes a `const` or a `var` depends on
//! whether any file uses it as an address. None of that is in one file, so
//! the program is loaded and read whole before any unit is written.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use super::Note;
use super::expr::{self, Expr};
use super::items::{self, Builder, Item, Macro, MemberKind, Struct};
use super::lex::{Tok, Token};
use super::render::{self, Operand};
use super::source::{self, Label, Line};

/// One file of the program.
#[derive(Debug)]
pub struct File {
    pub path: PathBuf,
    pub lines: Vec<Line>,
    /// Lines the lexer refused, by number, with why.
    pub unread: BTreeMap<usize, String>,
}

/// One unit the conversion writes: a `.kk` file.
#[derive(Debug)]
pub struct Unit {
    /// The file it is converted from.
    pub file: usize,
    /// An include of the same name beside it, converted into this unit.
    pub merged: Option<usize>,
    pub items: Vec<Item>,
    /// The `.kk` path.
    pub output: PathBuf,
}

/// What a unit-level name is.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    /// A code label. Which ones start a section is [`Program::starts`].
    Code,
    /// A label in a data block.
    Data,
    /// Storage reserved with `.res` (or `.tag` once), as a `var`.
    Storage { zero_page: bool },
    /// A run of `.tag` storage of one layout, or `.res N * .sizeof(S)`, as
    /// `var NAME[ … ] * COUNT`.
    Table { layout: String, count: Expr },
    /// One element of a table, spelled `TABLE[INDEX]`.
    Element { table: String, index: usize },
    /// A label at an address `.org` gave.
    Pinned { address: i64 },
    /// `name = value` and `.define`, whose conversion depends on their uses.
    Equate,
    /// A `.enum` member.
    EnumMember,
    /// A `.struct`.
    Layout { fixed: bool },
    /// A `.macro`.
    Macro,
}

/// One unit-level definition.
#[derive(Clone, Debug)]
pub struct Definition {
    pub unit: usize,
    pub kind: Kind,
    pub line: usize,
}

/// What a use of a name does with it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Role {
    /// `jsr NAME`, `jsl NAME`.
    Call,
    /// `jmp`, `jml`, or a branch.
    Jump,
    /// A memory operand, without an index register.
    Address,
    /// A memory operand indexed by `x` or `y`.
    Indexed,
    /// A stack-relative offset, `NAME,s`.
    Stack,
    Immediate,
    /// A value in a data directive.
    Data,
    /// Inside another definition's value, a count, a condition.
    Value,
    /// A second record of a memory operand, with the access it makes — read
    /// by [`Program::is_word`] and by nothing that decides structure.
    Access,
}

/// One use of a name.
#[derive(Clone, Debug)]
pub struct Use {
    pub unit: usize,
    pub role: Role,
    /// The top-level code label the use sits under, for the jumps that decide
    /// sections.
    pub under: Option<String>,
    /// For a memory operand that is exactly the name, plain or indexed: the
    /// mnemonic and the widths ca65 assembled it with — what decides whether
    /// a two-byte reservation is a word.
    pub access: Option<(String, items::Widths)>,
}

/// What [`Program::load`] could not read, and everything it did.
#[derive(Debug, Default)]
pub struct Program {
    pub files: Vec<File>,
    pub units: Vec<Unit>,
    pub macros: BTreeMap<String, Macro>,
    pub structs: BTreeMap<String, Struct>,
    pub definitions: BTreeMap<String, Definition>,
    /// Names each unit exports.
    pub exports: BTreeMap<String, usize>,
    /// Names each unit declares `.import`/`.importzp`, with whether the
    /// declaration was zero page.
    pub imports: BTreeMap<usize, BTreeMap<String, bool>>,
    pub uses: BTreeMap<String, Vec<Use>>,
    /// Code labels that start a section, by unit.
    pub starts: BTreeSet<(usize, String)>,
    /// Code labels kept as unit-level labels inside a section.
    pub global_labels: BTreeSet<(usize, String)>,
    /// Equates that are addresses, and become a `var`.
    pub addresses: BTreeSet<String>,
    /// The K816 spelling of every unit-level name that needs one.
    pub spellings: BTreeMap<String, String>,
    /// Whether a unit uses `.macpack generic`'s `add`/`sub` with an
    /// immediate, which the generated unit provides.
    pub generic: BTreeSet<usize>,
    pub notes: Vec<Note>,
}

/// How the program reads files, so that a test can hand it text.
pub type Reader<'r> = &'r dyn Fn(&Path) -> Option<String>;

impl Program {
    /// Loads `inputs`, every file they include, and everything about them.
    pub fn load(
        inputs: &[PathBuf],
        include_dirs: &[PathBuf],
        read: Reader<'_>,
    ) -> Result<Self, String> {
        let mut program = Self::default();
        let mut by_identity: BTreeMap<PathBuf, usize> = BTreeMap::new();
        let mut queue: std::collections::VecDeque<PathBuf> = inputs.iter().cloned().collect();
        while let Some(path) = queue.pop_front() {
            let identity = crate::imports::identity(&path);
            if by_identity.contains_key(&identity) {
                continue;
            }
            let text = read(&path).ok_or_else(|| format!("cannot read {}", path.display()))?;
            let index = program.files.len();
            by_identity.insert(identity, index);
            program.files.push(read_file(&path, &text));
            for line in &program.files[index].lines {
                if let Some((
                    "include",
                    [
                        Token {
                            tok: Tok::Str { text, .. },
                            ..
                        },
                    ],
                )) = line.directive()
                {
                    match resolve_include(&path, text, include_dirs, read) {
                        Some(target) => queue.push_back(target),
                        None => program.notes.push(Note::new(
                            &path,
                            line.number,
                            format!("`.include \"{text}\"` names no file this converter can find"),
                        )),
                    }
                }
            }
        }
        program.collect_macros();
        program.collect_structs();
        program.make_units(&by_identity, include_dirs, read);
        program.collect_definitions();
        program.collect_uses();
        program.decide_sections();
        program.classify_equates();
        program.spell_names();
        Ok(program)
    }

    /// The unit a name is defined in.
    pub fn owner(&self, name: &str) -> Option<usize> {
        self.definitions.get(name).map(|definition| definition.unit)
    }

    /// The K816 spelling of a unit-level name, as `unit` names it: its own
    /// definition of the name if it has one, which may have been renamed.
    pub fn spelling(&self, unit: usize, name: &str) -> String {
        let own = format!("{name}\u{0}{unit}");
        if let Some(spelled) = self.spellings.get(&own) {
            return spelled.clone();
        }
        self.spellings
            .get(name)
            .cloned()
            .unwrap_or_else(|| name.to_owned())
    }

    /// The definition `unit` means by `name`: its own, if two units define it.
    pub fn definition(&self, unit: usize, name: &str) -> Option<&Definition> {
        self.definitions
            .get(&format!("{name}\u{0}{unit}"))
            .or_else(|| self.definitions.get(name))
    }

    /// The file a unit's line is in: its own, or the include merged into it.
    pub fn unit_path(&self, unit: usize) -> &Path {
        &self.files[self.units[unit].file].path
    }

    fn collect_macros(&mut self) {
        for file in &self.files {
            let mut at = 0;
            while at < file.lines.len() {
                let line = &file.lines[at];
                at += 1;
                let Some(("macro" | "mac", args)) = line.directive() else {
                    continue;
                };
                let mut names = args.iter().filter_map(|token| match &token.tok {
                    Tok::Ident(name) => Some(name.clone()),
                    _ => None,
                });
                let Some(name) = names.next() else {
                    continue;
                };
                let params = names.collect();
                let mut body = Vec::new();
                let mut end = None;
                while at < file.lines.len() {
                    let candidate = &file.lines[at];
                    at += 1;
                    if matches!(candidate.directive(), Some(("endmacro" | "endmac", _))) {
                        end = Some(candidate.clone());
                        break;
                    }
                    body.push(candidate.clone());
                }
                let Some(end) = end else {
                    continue;
                };
                self.macros.entry(name.clone()).or_insert(Macro {
                    name,
                    params,
                    body,
                    line: line.clone(),
                    end,
                });
            }
        }
    }

    fn collect_structs(&mut self) {
        let macros: BTreeMap<String, Macro> = BTreeMap::new();
        for file in &self.files {
            let lookup = |name: &str| macros.get(name).cloned();
            let mut builder = Builder {
                widths: items::Widths::default(),
                macros: &lookup,
            };
            let items = builder.items(&file.lines, &mut 0, &[]).0;
            walk(&items, &mut |item| {
                if let Item::Struct(definition) = item {
                    self.structs
                        .entry(definition.name.clone())
                        .or_insert_with(|| definition.clone());
                }
            });
        }
    }

    /// One unit per file, but an include beside an `.asm` of the same stem,
    /// which that file includes, is part of it.
    fn make_units(
        &mut self,
        by_identity: &BTreeMap<PathBuf, usize>,
        include_dirs: &[PathBuf],
        read: Reader<'_>,
    ) {
        let mut merged_into: BTreeMap<usize, usize> = BTreeMap::new();
        for (index, file) in self.files.iter().enumerate() {
            if file.path.extension().and_then(|e| e.to_str()) != Some("asm") {
                continue;
            }
            for line in &file.lines {
                if let Some((
                    "include",
                    [
                        Token {
                            tok: Tok::Str { text, .. },
                            ..
                        },
                    ],
                )) = line.directive()
                    && let Some(target) = resolve_include(&file.path, text, include_dirs, read)
                    && let Some(&included) = by_identity.get(&crate::imports::identity(&target))
                    && target.file_stem() == file.path.file_stem()
                    && target.parent() == file.path.parent()
                {
                    merged_into.insert(included, index);
                }
            }
        }
        let macros = self.macros.clone();
        for index in 0..self.files.len() {
            if merged_into.contains_key(&index) {
                continue;
            }
            let merged = merged_into
                .iter()
                .find(|(_, into)| **into == index)
                .map(|(included, _)| *included);
            let lookup = |name: &str| macros.get(name).cloned();
            let mut builder = Builder {
                widths: items::Widths::default(),
                macros: &lookup,
            };
            let mut items = builder.items(&self.files[index].lines, &mut 0, &[]).0;
            if let Some(included) = merged {
                let path = self.files[index].path.clone();
                let include_items = {
                    let mut inner = Builder {
                        widths: items::Widths::default(),
                        macros: &lookup,
                    };
                    inner.items(&self.files[included].lines, &mut 0, &[]).0
                };
                let target = self.files[included].path.clone();
                let mut spliced = Vec::new();
                let mut include_items = Some(include_items);
                for item in items {
                    if let Item::Include(text, _) = &item
                        && let Some(resolved) = resolve_include(&path, text, include_dirs, read)
                        && crate::imports::identity(&resolved) == crate::imports::identity(&target)
                        && let Some(inner) = include_items.take()
                    {
                        spliced.extend(inner);
                        continue;
                    }
                    spliced.push(item);
                }
                items = spliced;
            }
            let output = self.files[index].path.with_extension("kk");
            self.units.push(Unit {
                file: index,
                merged,
                items,
                output,
            });
        }
    }

    /// The unit a file's text ended up in.
    pub fn unit_of_file(&self, file: usize) -> Option<usize> {
        self.units
            .iter()
            .position(|unit| unit.file == file || unit.merged == Some(file))
    }

    /// The unit an `.include` line of `unit` names.
    pub fn included_unit(
        &self,
        unit: usize,
        text: &str,
        include_dirs: &[PathBuf],
        read: Reader<'_>,
    ) -> Option<usize> {
        let from = &self.files[self.units[unit].file].path;
        let target = crate::imports::identity(&resolve_include(from, text, include_dirs, read)?);
        let file = self
            .files
            .iter()
            .position(|file| crate::imports::identity(&file.path) == target)?;
        self.unit_of_file(file)
    }

    fn define(&mut self, name: &str, unit: usize, kind: Kind, line: usize) {
        if let Some(existing) = self.definitions.get(name) {
            if existing.unit != unit {
                // A name two units both define is kept for the first and
                // renamed for the second by `spell_names`.
                let key = format!("{name}\u{0}{unit}");
                self.definitions
                    .insert(key, Definition { unit, kind, line });
            }
            return;
        }
        self.definitions
            .insert(name.to_owned(), Definition { unit, kind, line });
    }

    fn collect_definitions(&mut self) {
        for unit in 0..self.units.len() {
            let items = std::mem::take(&mut self.units[unit].items);
            let mut org: Option<i64> = None;
            let mut segment = "CODE".to_owned();
            let mut index = 0;
            while index < items.len() {
                let item = &items[index];
                match item {
                    Item::Segment(name, _) => segment.clone_from(name),
                    Item::Org(address, _) => {
                        org = address.as_ref().and_then(|address| address.fold(&|_| None));
                    }
                    Item::Label(Label::Named(name), line) => {
                        let kind = if let Some(address) = org {
                            Kind::Pinned { address }
                        } else {
                            match label_kind(&items[index + 1..], &segment) {
                                Kind::Storage { zero_page: false } => self
                                    .reserved_table(&items[index + 1..])
                                    .unwrap_or(Kind::Storage { zero_page: false }),
                                kind => kind,
                            }
                        };
                        self.define(name, unit, kind, line.number);
                    }
                    Item::Proc { name, line, .. } => {
                        self.define(name, unit, Kind::Code, line.number)
                    }
                    Item::Assign { name, line, .. } | Item::Define { name, line, .. } => {
                        self.define(name, unit, Kind::Equate, line.number);
                    }
                    Item::Enum(enumeration) => {
                        for member in &enumeration.members {
                            if let Some(name) = &member.name {
                                let spelled = match &enumeration.name {
                                    Some(scope) => format!("{scope}::{name}"),
                                    None => name.clone(),
                                };
                                self.define(&spelled, unit, Kind::EnumMember, member.line.number);
                            }
                        }
                    }
                    Item::Struct(definition) => {
                        let fixed = definition
                            .members
                            .iter()
                            .any(|member| matches!(member.kind, MemberKind::Org(_)));
                        self.define(
                            &definition.name,
                            unit,
                            Kind::Layout { fixed },
                            definition.line.number,
                        );
                    }
                    Item::Macro(definition) => {
                        self.define(&definition.name, unit, Kind::Macro, definition.line.number);
                    }
                    Item::Linkage {
                        directive, names, ..
                    } => {
                        for name in names {
                            match directive.as_str() {
                                "export" | "exportzp" | "global" | "globalzp" => {
                                    self.exports.insert(name.clone(), unit);
                                }
                                _ => {
                                    self.imports
                                        .entry(unit)
                                        .or_default()
                                        .insert(name.clone(), directive.ends_with("zp"));
                                }
                            }
                        }
                    }
                    _ => {}
                }
                index += 1;
            }
            self.find_tables(unit, &items);
            self.units[unit].items = items;
        }
    }

    /// `.res N * .sizeof(S)` reserves `N` of `S`, which is a table of them.
    fn reserved_table(&self, after: &[Item]) -> Option<Kind> {
        let Item::Data {
            directive, args, ..
        } = after.iter().find(|item| !matches!(item, Item::Trivia(_)))?
        else {
            return None;
        };
        if directive != "res" || source::split_commas(args).len() != 1 {
            return None;
        }
        let mut count = expr::parse_all(args).ok()?;
        while let Expr::Paren(inner) = count {
            count = *inner;
        }
        let Expr::Binary {
            op: expr::Binary::Mul,
            left,
            right,
            ..
        } = count
        else {
            return None;
        };
        let size_of = |expr: &Expr| match expr {
            Expr::Function { name, arguments } if name == "sizeof" => match arguments.as_slice() {
                [Expr::Symbol(path)] if path.len() == 1 && self.structs.contains_key(&path[0]) => {
                    Some(path[0].clone())
                }
                _ => None,
            },
            _ => None,
        };
        let (layout, count) = match (size_of(&left), size_of(&right)) {
            (Some(layout), None) => (layout, *right),
            (None, Some(layout)) => (layout, *left),
            _ => return None,
        };
        Some(Kind::Table { layout, count })
    }

    /// A run of `.tag` storage of one layout, two or more long, is a table:
    /// `TASKS: task00: .tag TCB … task1F: .tag TCB` is `var TASKS[ … ] * 32`,
    /// and each label names an element.
    fn find_tables(&mut self, unit: usize, items: &[Item]) {
        let mut index = 0;
        while index < items.len() {
            let Some((labels, layout, next)) = tagged(items, index) else {
                index += 1;
                continue;
            };
            let mut elements = vec![labels];
            let mut at = next;
            while let Some((labels, same, after)) = tagged(items, at)
                && same == layout
            {
                elements.push(labels);
                at = after;
            }
            if elements.len() >= 2 {
                let table = elements[0][0].clone();
                let count = elements.len();
                self.definitions.insert(
                    table.clone(),
                    Definition {
                        unit,
                        kind: Kind::Table {
                            layout: layout.clone(),
                            count: Expr::Number {
                                value: u32::try_from(count).unwrap_or(0),
                                text: count.to_string(),
                            },
                        },
                        line: items[index].line().number,
                    },
                );
                for (element, labels) in elements.iter().enumerate() {
                    for label in labels {
                        if *label != table {
                            self.definitions.insert(
                                label.clone(),
                                Definition {
                                    unit,
                                    kind: Kind::Element {
                                        table: table.clone(),
                                        index: element,
                                    },
                                    line: 0,
                                },
                            );
                        }
                    }
                }
                index = at;
            } else {
                index = next;
            }
        }
    }

    fn collect_uses(&mut self) {
        let mut uses: BTreeMap<String, Vec<Use>> = BTreeMap::new();
        for (unit, body) in self.units.iter().enumerate() {
            let mut under: Option<String> = None;
            collect(&body.items, unit, &mut under, &mut uses, &self.macros);
        }
        self.uses = uses;
    }

    /// Which code labels start a section.
    ///
    /// A section ends where control cannot fall through — an `rts`, a `jmp`,
    /// a `bra` — and the next label starts one when something outside it
    /// enters there: an export, a call, an address taken, a use from another
    /// unit, or a jump from another section. A label only its own section
    /// branches to stays inside it, as `.name`. A label control falls into
    /// cannot start a section — the K816 one before it would gain an `rts` —
    /// so one entered from outside stays a unit-level label inside.
    fn decide_sections(&mut self) {
        for unit in 0..self.units.len() {
            let items = std::mem::take(&mut self.units[unit].items);
            let candidates = boundaries(&items);
            let mut starts: BTreeSet<String> = candidates
                .iter()
                .filter(|(_, first)| *first)
                .map(|(name, _)| name.clone())
                .collect();
            loop {
                let section_of = sections(&items, &starts);
                let mut changed = false;
                for (name, _) in &candidates {
                    if starts.contains(name) {
                        continue;
                    }
                    if self.entered_from_outside(unit, name, &section_of) {
                        starts.insert(name.clone());
                        changed = true;
                    }
                }
                if !changed {
                    // Labels a terminator does not precede, that something
                    // outside their section still names, stay unit-level.
                    for (name, owner) in &section_of {
                        if starts.contains(name) || owner.is_none() {
                            continue;
                        }
                        if self.named_from_outside(unit, name, owner.as_deref(), &section_of) {
                            self.global_labels.insert((unit, name.clone()));
                        }
                    }
                    break;
                }
            }
            for name in starts {
                self.starts.insert((unit, name));
            }
            self.units[unit].items = items;
        }
    }

    /// Whether `name`, a label after a terminator, is entered from outside
    /// the section it would otherwise end up in — or from nowhere, which
    /// ties it to that section no more than to any other: control cannot
    /// fall into it, nothing branches to it, and ca65 ends the scope of a
    /// cheap local at it.
    fn entered_from_outside(
        &self,
        unit: usize,
        name: &str,
        section_of: &BTreeMap<String, Option<String>>,
    ) -> bool {
        let own = section_of.get(name).cloned().flatten();
        let mut named = self
            .uses
            .get(name)
            .into_iter()
            .flatten()
            .filter(|used| used.role != Role::Access)
            .peekable();
        self.exports.get(name) == Some(&unit)
            || named.peek().is_none()
            || named.any(|used| {
                used.unit != unit
                    || !matches!(used.role, Role::Jump)
                    || section_of_use(used, section_of) != own
            })
    }

    /// Whether `name`, a label inside `owner`, is named from anywhere but
    /// `owner`'s own branches.
    fn named_from_outside(
        &self,
        unit: usize,
        name: &str,
        owner: Option<&str>,
        section_of: &BTreeMap<String, Option<String>>,
    ) -> bool {
        self.exports.get(name) == Some(&unit)
            || self.uses.get(name).is_some_and(|uses| {
                uses.iter()
                    .filter(|used| used.role != Role::Access)
                    .any(|used| {
                        used.unit != unit
                            || !matches!(used.role, Role::Jump)
                            || section_of_use(used, section_of).as_deref() != owner
                    })
            })
    }

    /// Which equates are addresses: used as a memory operand without an
    /// index, or defined over a name that is one.
    fn classify_equates(&mut self) {
        let equates: Vec<String> = self
            .definitions
            .iter()
            .filter(|(_, definition)| definition.kind == Kind::Equate)
            .map(|(name, _)| name.clone())
            .collect();
        for name in &equates {
            if self.uses.get(name).is_some_and(|uses| {
                uses.iter()
                    .any(|used| matches!(used.role, Role::Address | Role::Call | Role::Jump))
            }) {
                self.addresses.insert(name.clone());
            }
        }
        // An equate over a place is a place, and so is one over that.
        loop {
            let mut changed = false;
            for name in &equates {
                if self.addresses.contains(name) {
                    continue;
                }
                let Some(value) = self.equate_value(name) else {
                    continue;
                };
                let mut paths = Vec::new();
                value.symbols(&mut paths);
                let over_place = paths.iter().any(|path| {
                    path.len() == 1
                        && (self.addresses.contains(&path[0])
                            || self.definitions.get(&path[0]).is_some_and(|definition| {
                                matches!(
                                    definition.kind,
                                    Kind::Pinned { .. }
                                        | Kind::Storage { .. }
                                        | Kind::Data
                                        | Kind::Code
                                )
                            }))
                });
                if over_place {
                    self.addresses.insert(name.clone());
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }

    /// Whether every access to `name` anywhere in the program moves it whole
    /// and sixteen bits wide — which makes a two-byte reservation a `:word`.
    ///
    /// A width suffix is a claim the compiler checks at every access
    /// (`K816-E0140`), so it is written only where the original never read
    /// the datum any other way: no eight-bit access, no access to a byte of
    /// it, no access K816 could not see the width of.
    pub fn is_word(&self, name: &str) -> bool {
        let Some(uses) = self.uses.get(name) else {
            return false;
        };
        let mut accessed = false;
        for used in uses {
            match (&used.access, used.role) {
                (Some((mnemonic, widths)), _) => {
                    let wide = match mnemonic.as_str() {
                        "lda" | "sta" | "adc" | "sbc" | "and" | "ora" | "eor" | "cmp" | "bit"
                        | "stz" | "inc" | "dec" | "asl" | "lsr" | "rol" | "ror" | "tsb" | "trb" => {
                            widths.a == items::Width::W16
                        }
                        "ldx" | "stx" | "cpx" | "ldy" | "sty" | "cpy" => {
                            widths.i == items::Width::W16
                        }
                        _ => false,
                    };
                    if !wide {
                        return false;
                    }
                    accessed = true;
                }
                // Its address, as a value, is not an access; a memory operand
                // is counted below against the accesses it should have.
                (
                    None,
                    Role::Immediate
                    | Role::Data
                    | Role::Value
                    | Role::Access
                    | Role::Address
                    | Role::Indexed,
                ) => {}
                // A call, a jump, a stack offset: not a datum at all.
                (None, Role::Call | Role::Jump | Role::Stack) => return false,
            }
        }
        // Every memory operand naming it was the name itself, so no access
        // reached a byte of it — `name+1`, `(name),y` — by another route.
        let operands = uses
            .iter()
            .filter(|used| matches!(used.role, Role::Address | Role::Indexed))
            .count();
        let whole = uses.iter().filter(|used| used.access.is_some()).count();
        accessed && operands == whole
    }

    /// The expression an equate was defined with.
    pub fn equate_value(&self, name: &str) -> Option<Expr> {
        let definition = self.definitions.get(name)?;
        let mut found = None;
        walk(&self.units[definition.unit].items, &mut |item| match item {
            Item::Assign {
                name: defined,
                value,
                ..
            } if defined == name && found.is_none() => {
                found = Some(value.clone());
            }
            Item::Define {
                name: defined,
                value,
                ..
            } if defined == name && found.is_none() => {
                found = expr::parse_all(value).ok();
            }
            _ => {}
        });
        found
    }

    /// A name's value, where ca65 would know it now.
    pub fn value(&self, path: &[String]) -> Option<i64> {
        self.value_guarded(path, 0)
    }

    fn value_guarded(&self, path: &[String], depth: u32) -> Option<i64> {
        if depth > 64 {
            return None;
        }
        let lookup = |inner: &[String]| self.value_guarded(inner, depth + 1);
        match path {
            [name] => {
                let definition = self.definitions.get(name)?;
                match &definition.kind {
                    Kind::Equate => self.equate_value(name)?.fold(&lookup),
                    Kind::EnumMember => self.enum_value(name, &lookup),
                    Kind::Pinned { address } => Some(*address),
                    _ => None,
                }
            }
            [layout, field] if self.structs.contains_key(layout) => {
                let definition = &self.structs[layout];
                if field == ".sizeof" {
                    return self.struct_size(definition, &lookup);
                }
                let (offset, _) = self.member_offset(definition, field, &lookup)?;
                // A field of a layout at an `.org` is an address in ca65, and
                // the offset is from there.
                let base = definition
                    .members
                    .iter()
                    .find_map(|member| match &member.kind {
                        MemberKind::Org(address) => address.fold(&lookup),
                        _ => None,
                    });
                Some(offset + base.unwrap_or(0))
            }
            [layout, field, sizeof] if sizeof == ".sizeof" && self.structs.contains_key(layout) => {
                let definition = &self.structs[layout];
                let (_, member) = self.member_offset(definition, field, &lookup)?;
                self.member_size(member, &lookup)
            }
            _ => None,
        }
    }

    fn enum_value(&self, name: &str, lookup: &dyn Fn(&[String]) -> Option<i64>) -> Option<i64> {
        let definition = self.definitions.get(name)?;
        let mut answer = None;
        walk(&self.units[definition.unit].items, &mut |item| {
            if let Item::Enum(enumeration) = item {
                let mut next = 0i64;
                for member in &enumeration.members {
                    let Some(member_name) = &member.name else {
                        continue;
                    };
                    let value = match &member.value {
                        Some(value) => value.fold(lookup),
                        None => Some(next),
                    };
                    let spelled = match &enumeration.name {
                        Some(scope) => format!("{scope}::{member_name}"),
                        None => member_name.clone(),
                    };
                    if spelled == name && answer.is_none() {
                        answer = value;
                    }
                    next = value.map_or(next, |value| value + 1);
                }
            }
        });
        answer
    }

    /// A struct's size.
    pub fn struct_size(
        &self,
        definition: &Struct,
        lookup: &dyn Fn(&[String]) -> Option<i64>,
    ) -> Option<i64> {
        let mut size = 0i64;
        for member in &definition.members {
            size += self.member_size(member, lookup)?;
        }
        Some(size)
    }

    /// The bytes one member occupies.
    pub fn member_size(
        &self,
        member: &items::Member,
        lookup: &dyn Fn(&[String]) -> Option<i64>,
    ) -> Option<i64> {
        Some(match &member.kind {
            MemberKind::Data { size, count } => {
                i64::from(*size)
                    * match count {
                        Some(count) => count.fold(lookup)?,
                        None => 1,
                    }
            }
            MemberKind::Tag(tag) => self.struct_size(self.structs.get(tag)?, lookup)?,
            MemberKind::Org(_) | MemberKind::Trivia => 0,
        })
    }

    /// A field's offset in a struct, and the member.
    fn member_offset<'s>(
        &self,
        definition: &'s Struct,
        field: &str,
        lookup: &dyn Fn(&[String]) -> Option<i64>,
    ) -> Option<(i64, &'s items::Member)> {
        let mut offset = 0i64;
        for member in &definition.members {
            if member.name.as_deref() == Some(field) {
                return Some((offset, member));
            }
            offset += self.member_size(member, lookup)?;
        }
        None
    }

    /// A K816 spelling for every unit-level name that cannot keep its own:
    /// one K816 reserves, and the second of two units' unexported names that
    /// collide — K816 exports every unit-level name, ca65 only what it is
    /// told to.
    fn spell_names(&mut self) {
        let mut taken: BTreeSet<String> = BTreeSet::new();
        let names: Vec<(String, Definition)> = self
            .definitions
            .iter()
            .map(|(name, definition)| (name.clone(), definition.clone()))
            .collect();
        for (key, definition) in &names {
            let (name, second) = match key.split_once('\u{0}') {
                Some((name, _)) => (name.to_owned(), true),
                None => (key.clone(), false),
            };
            if name.contains("::") || matches!(definition.kind, Kind::Element { .. } | Kind::Macro)
            {
                continue;
            }
            let mut spelled = name.clone();
            if second {
                let stem = self
                    .unit_path(definition.unit)
                    .file_stem()
                    .map(|stem| stem.to_string_lossy().into_owned())
                    .unwrap_or_default();
                spelled = format!("{stem}_{name}");
                self.notes.push(Note::new(
                    self.unit_path(definition.unit),
                    definition.line,
                    format!(
                        "`{name}` is defined by another unit too, and K816 exports every \
                         unit-level name: this one is `{spelled}`"
                    ),
                ));
            }
            if !super::usable_name(&spelled) {
                let mut candidate = format!("{spelled}_");
                while taken.contains(&candidate) {
                    candidate.push('_');
                }
                self.notes.push(Note::new(
                    self.unit_path(definition.unit),
                    definition.line,
                    format!("`{name}` is a name K816 reserves; it is `{candidate}` here"),
                ));
                spelled = candidate;
            }
            taken.insert(spelled.clone());
            if spelled != name || second {
                self.spellings.insert(key.clone(), spelled);
            }
        }
    }
}

/// Reads a file into lines, lexing each under the features in force there.
fn read_file(path: &Path, text: &str) -> File {
    let mut lines = Vec::new();
    let mut unread = BTreeMap::new();
    let mut escapes = false;
    for (index, raw) in text.lines().enumerate() {
        let trimmed = raw.trim_start().to_ascii_lowercase();
        if let Some(rest) = trimmed.strip_prefix(".feature")
            && rest.trim_start().starts_with("string_escapes")
        {
            escapes = !rest.trim_end().ends_with('-');
        }
        match source::read(index + 1, raw, escapes) {
            Ok(line) => lines.push(line),
            Err(why) => {
                unread.insert(index + 1, why);
                lines.push(Line {
                    number: index + 1,
                    text: raw.to_owned(),
                    indent: 0,
                    labels: Vec::new(),
                    body: None,
                    comment: None,
                    body_column: 0,
                });
            }
        }
    }
    File {
        path: path.to_path_buf(),
        lines,
        unread,
    }
}

/// Where `.include "text"` in `from` reaches: beside `from` first, then each
/// include directory in order, which is ca65's search.
pub fn resolve_include(
    from: &Path,
    text: &str,
    include_dirs: &[PathBuf],
    read: Reader<'_>,
) -> Option<PathBuf> {
    let beside = crate::imports::resolve(from, text);
    if read(&beside).is_some() {
        return Some(beside);
    }
    include_dirs
        .iter()
        .map(|dir| dir.join(text))
        .find(|candidate| read(candidate).is_some())
}

/// Calls `visit` on every item, nested ones included.
pub fn walk(items: &[Item], visit: &mut dyn FnMut(&Item)) {
    for item in items {
        visit(item);
        match item {
            Item::Proc { items, .. } | Item::Repeat { items, .. } => walk(items, visit),
            Item::If { branches, .. } => {
                for branch in branches {
                    walk(&branch.items, visit);
                }
            }
            _ => {}
        }
    }
}

/// What a label is, from what follows it.
fn label_kind(after: &[Item], segment: &str) -> Kind {
    for item in after {
        match item {
            Item::Trivia(_) | Item::Label(..) => {}
            Item::Data { directive, .. } if directive == "res" || directive == "tag" => {
                return Kind::Storage {
                    zero_page: segment == "ZEROPAGE",
                };
            }
            Item::Data { .. } if segment == "ZEROPAGE" => {
                return Kind::Storage { zero_page: true };
            }
            Item::Data { .. } => return Kind::Data,
            // A `.repeat` of data directives is data.
            Item::Repeat { items, .. } if is_data_repeat(items) => return Kind::Data,
            _ => return Kind::Code,
        }
    }
    Kind::Code
}

/// Whether a `.repeat` body is data directives and nothing else.
pub fn is_data_repeat(items: &[Item]) -> bool {
    let mut data = false;
    for item in items {
        match item {
            Item::Trivia(_) => {}
            Item::Data { .. } => data = true,
            _ => return false,
        }
    }
    data
}

/// `labels: .tag NAME` at `index`: the labels, the layout, and where the run
/// continues.
fn tagged(items: &[Item], index: usize) -> Option<(Vec<String>, String, usize)> {
    let mut labels = Vec::new();
    let mut at = index;
    while let Some(item) = items.get(at) {
        match item {
            Item::Label(Label::Named(name), _) => labels.push(name.clone()),
            Item::Trivia(_) if !labels.is_empty() => {}
            Item::Data {
                directive, args, ..
            } if directive == "tag" && !labels.is_empty() => {
                let [
                    Token {
                        tok: Tok::Ident(layout),
                        ..
                    },
                ] = args.as_slice()
                else {
                    return None;
                };
                return Some((labels, layout.clone(), at + 1));
            }
            _ => return None,
        }
        at += 1;
    }
    None
}

/// Every top-level code label, and whether it begins a run of code: the
/// first label after something that is not code, which always starts a
/// section. The rest follow a terminator, or not, which is what decides.
fn boundaries(items: &[Item]) -> Vec<(String, bool)> {
    let mut found = Vec::new();
    let mut in_code = false;
    let mut after_terminator = false;
    for item in items {
        match item {
            // A width directive runs nothing: it neither starts code nor
            // ends the run of a terminator, so `rts` / `.a8` / `next:` still
            // puts `next` after the terminator.
            Item::Trivia(_)
            | Item::Linkage { .. }
            | Item::Ignored(_)
            | Item::Macpack(..)
            | Item::WidthAssert { .. } => {}
            Item::Label(Label::Named(name), _) => {
                if !in_code {
                    found.push((name.clone(), true));
                } else if after_terminator {
                    found.push((name.clone(), false));
                }
            }
            Item::Label(..) => {}
            Item::Instruction { mnemonic, .. } => {
                in_code = true;
                after_terminator = is_terminator(mnemonic);
            }
            Item::Proc { .. } => {
                in_code = false;
            }
            Item::WidthSet { .. }
            | Item::MacroUse { .. }
            | Item::Repeat { .. }
            | Item::If { .. } => {
                in_code = true;
                after_terminator = false;
            }
            Item::Assign { .. }
            | Item::Define { .. }
            | Item::Macro(_)
            | Item::Struct(_)
            | Item::Enum(_) => {}
            _ => {
                in_code = false;
                after_terminator = false;
            }
        }
    }
    found
}

/// True for an instruction control never falls through.
pub fn is_terminator(mnemonic: &str) -> bool {
    matches!(
        mnemonic,
        "rts" | "rtl" | "rti" | "jmp" | "jml" | "bra" | "brl" | "stp"
    )
}

/// The section each top-level code label ends up in, given which labels
/// start one. A start is in its own; a label before any start is in none.
fn sections(items: &[Item], starts: &BTreeSet<String>) -> BTreeMap<String, Option<String>> {
    let mut found = BTreeMap::new();
    let mut current: Option<String> = None;
    for item in items {
        match item {
            Item::Label(Label::Named(name), _) => {
                if starts.contains(name) {
                    current = Some(name.clone());
                }
                found.insert(name.clone(), current.clone());
            }
            Item::Proc { name, .. } => {
                found.insert(name.clone(), Some(name.clone()));
            }
            Item::Data { .. } | Item::Segment(..) => current = None,
            _ => {}
        }
    }
    found
}

/// The section a use sits in.
fn section_of_use(used: &Use, section_of: &BTreeMap<String, Option<String>>) -> Option<String> {
    used.under
        .as_ref()
        .and_then(|label| section_of.get(label).cloned().flatten())
}

/// Records every use of a name in `items`.
fn collect(
    items: &[Item],
    unit: usize,
    under: &mut Option<String>,
    uses: &mut BTreeMap<String, Vec<Use>>,
    macros: &BTreeMap<String, Macro>,
) {
    let note =
        |uses: &mut BTreeMap<String, Vec<Use>>, expr: &Expr, role: Role, under: &Option<String>| {
            record(uses, unit, expr, role, under);
        };
    for item in items {
        match item {
            Item::Label(Label::Named(name), _) => *under = Some(name.clone()),
            Item::Instruction {
                mnemonic,
                operand,
                widths,
                ..
            } => {
                let Ok(shape) = render::operand(mnemonic, operand) else {
                    continue;
                };
                if let Operand::Direct { expr, index, .. } = &shape
                    && index != &Some('s')
                    && let Some(name) = expr.name()
                {
                    uses.entry(name.to_owned()).or_default().push(Use {
                        unit,
                        role: Role::Access,
                        under: under.clone(),
                        access: Some((mnemonic.clone(), *widths)),
                    });
                }
                match shape {
                    Operand::Immediate(expr) => note(uses, &expr, Role::Immediate, under),
                    Operand::Direct { expr, index, .. } => {
                        let role = match (mnemonic.as_str(), index) {
                            ("jsr" | "jsl", None) => Role::Call,
                            (m, None) if is_branch(m) || m == "jmp" || m == "jml" => Role::Jump,
                            (_, Some('s')) => Role::Stack,
                            (_, Some(_)) => Role::Indexed,
                            (_, None) if mnemonic == "pea" || mnemonic == "per" => Role::Immediate,
                            _ => Role::Address,
                        };
                        note(uses, &expr, role, under);
                    }
                    Operand::Indirect { expr, inner, .. } => {
                        let role = if inner == Some('s') {
                            Role::Stack
                        } else {
                            Role::Indexed
                        };
                        note(uses, &expr, role, under);
                    }
                    Operand::Long { expr, .. } => note(uses, &expr, Role::Indexed, under),
                    Operand::Move(source, destination) => {
                        note(uses, &source, Role::Value, under);
                        note(uses, &destination, Role::Value, under);
                    }
                    Operand::None | Operand::Accumulator(_) => {}
                }
            }
            Item::Data {
                directive, args, ..
            } => {
                for part in source::split_commas(args) {
                    if let Ok(expr) = expr::parse_all(part) {
                        let role = if directive == "res" || directive == "tag" {
                            Role::Value
                        } else {
                            Role::Data
                        };
                        note(uses, &expr, role, under);
                    } else if let [
                        Token {
                            tok: Tok::Ident(name),
                            ..
                        },
                    ] = part
                    {
                        uses.entry(name.clone()).or_default().push(Use {
                            unit,
                            role: Role::Value,
                            under: under.clone(),
                            access: None,
                        });
                    }
                }
            }
            Item::Assign { value, .. } => note(uses, value, Role::Value, &None),
            Item::Define { value, .. } => {
                if let Ok(expr) = expr::parse_all(value) {
                    note(uses, &expr, Role::Value, &None);
                }
            }
            Item::MacroUse { name, args, .. } => {
                if let Some(definition) = macros.get(name) {
                    for (param, arg) in definition.params.iter().zip(args) {
                        let role = param_role(definition, param);
                        let tokens = match arg.first() {
                            Some(Token {
                                tok: Tok::Punct("#"),
                                ..
                            }) => &arg[1..],
                            _ => &arg[..],
                        };
                        if let Ok(expr) = expr::parse_all(tokens) {
                            note(uses, &expr, role, under);
                        }
                    }
                    // The body's own names are used wherever it expands.
                    let lookup = |name: &str| macros.get(name).cloned();
                    let mut builder = Builder {
                        widths: items::Widths::default(),
                        macros: &lookup,
                    };
                    let body = builder.items(&definition.body, &mut 0, &[]).0;
                    let mut inner = under.clone();
                    collect(&body, unit, &mut inner, uses, macros);
                } else if let Some(generic) = generic_macro(name) {
                    let _ = generic;
                    for arg in args {
                        let tokens = match arg.first() {
                            Some(Token {
                                tok: Tok::Punct("#"),
                                ..
                            }) => &arg[1..],
                            _ => &arg[..],
                        };
                        if let Ok(expr) = expr::parse_all(tokens) {
                            note(uses, &expr, Role::Value, under);
                        }
                    }
                }
            }
            Item::Repeat { count, items, .. } => {
                note(uses, count, Role::Value, under);
                collect(items, unit, under, uses, macros);
            }
            Item::Proc { name, items, .. } => {
                *under = Some(name.clone());
                collect(items, unit, under, uses, macros);
            }
            Item::If { branches, .. } => {
                for branch in branches {
                    if let Ok(expr) = expr::parse_all(&branch.condition) {
                        note(uses, &expr, Role::Value, under);
                    }
                    collect(&branch.items, unit, under, uses, macros);
                }
            }
            Item::Struct(definition) => {
                for member in &definition.members {
                    match &member.kind {
                        MemberKind::Data {
                            count: Some(count), ..
                        } => note(uses, count, Role::Value, &None),
                        MemberKind::Org(address) => note(uses, address, Role::Value, &None),
                        _ => {}
                    }
                }
            }
            Item::Enum(enumeration) => {
                for member in &enumeration.members {
                    if let Some(value) = &member.value {
                        note(uses, value, Role::Value, &None);
                    }
                }
            }
            _ => {}
        }
    }
}

/// Records every name `expr` mentions, with the role the operand gives it.
fn record(
    uses: &mut BTreeMap<String, Vec<Use>>,
    unit: usize,
    expr: &Expr,
    role: Role,
    under: &Option<String>,
) {
    let mut paths = Vec::new();
    expr.symbols(&mut paths);
    for (index, path) in paths.iter().enumerate() {
        // Only the expression's first name has the operand's role; a name
        // inside arithmetic is a value, unless it is the base of an address.
        let role = if index == 0 || matches!(role, Role::Address | Role::Indexed) {
            role
        } else {
            Role::Value
        };
        uses.entry(path[0].clone()).or_default().push(Use {
            unit,
            role,
            under: under.clone(),
            access: None,
        });
    }
}

/// How a macro's body uses a parameter: as an address, or as a value.
pub fn param_role(definition: &Macro, param: &str) -> Role {
    for line in &definition.body {
        if let Some(source::Body::Instruction { mnemonic, operand }) = &line.body
            && operand
                .iter()
                .any(|token| token.tok == Tok::Ident(param.to_owned()))
            && operand.first().map(|token| &token.tok) != Some(&Tok::Punct("#"))
        {
            return match mnemonic.as_str() {
                "jsr" | "jsl" => Role::Call,
                "jmp" | "jml" => Role::Jump,
                _ => Role::Address,
            };
        }
    }
    Role::Value
}

/// True for a branch mnemonic, long or short.
pub fn is_branch(mnemonic: &str) -> bool {
    matches!(
        mnemonic,
        "bcc" | "bcs" | "beq" | "bmi" | "bne" | "bpl" | "bra" | "brl" | "bvc" | "bvs"
    )
}

/// `.macpack generic`'s macros: the name, and what it is.
pub fn generic_macro(name: &str) -> Option<&'static str> {
    match name.to_ascii_lowercase().as_str() {
        "add" => Some("add"),
        "sub" => Some("sub"),
        "bge" => Some("bcs"),
        "blt" => Some("bcc"),
        "bgt" => Some("bgt"),
        "ble" => Some("ble"),
        "bnz" => Some("bne"),
        "bze" => Some("beq"),
        _ => None,
    }
}
