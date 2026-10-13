/*****************************************************************************/
/*                                                                           */
/*                                  bind.rs                                  */
/*                                                                           */
/*       An interface unit of `extern` declarations, from an ld65 debug file */
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

//! `k816 bind`: the K816 side of a program's ca65 modules, written from the
//! debug file ld65 produced for it.
//!
//! A program being carried from ca65 to K816 one file at a time is, for the
//! whole of the carrying, two languages linked together. Each K816 unit
//! needs an `extern` for every symbol a ca65 module still provides, and
//! writing those by hand means reading every `.export` of every module and
//! getting each address size right — which is the thing an `extern` exists
//! to say and the thing ca65 has already decided. So this asks ld65 instead.
//! `cl65 -g` with `--dbgfile` leaves a text database naming every symbol,
//! its address size, its segment, the module that defined it and the
//! modules that import it; this module reads that database and writes the
//! declarations.
//!
//! **What it can and cannot know.** The database says what a symbol *is* —
//! a label in a code segment, a label with a size in a data segment, an
//! equate — and how wide its address is, which is everything an `extern`'s
//! *shape* needs. It does not say what register widths a routine expects or
//! leaves behind: those were never written anywhere ca65 could record them.
//! So every `extern func` comes out without a contract, which K816 reads as
//! "preserves every axis", and the comment beside it quotes the nearest
//! `.a8`/`.i8` directives above the routine in its source, when the source
//! is at hand, as the hint the author promotes to an annotation by hand. A
//! contract is a claim nothing verifies (`doc/syntax-reference.md`, "Extern
//! declarations"), so the tool does not make one up.
//!
//! The database format is line-oriented `kind\tkey=value,…`, read here the
//! way `tests/debug.rs` reads it rather than through `src/dbginfo/`: fifty
//! lines of parsing against a build dependency on the cc65 C library.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::path::{Component, Path, PathBuf};

// ---------------------------------------------------------------------------
// The database
// ---------------------------------------------------------------------------

/// One record: its `key=value` fields.
type Record = BTreeMap<String, String>;

#[derive(Debug)]
struct File {
    name: String,
    /// The modules whose assembly read this file: one for a module's own
    /// source, several for an include file.
    modules: Vec<u64>,
}

#[derive(Debug)]
struct Module {
    file: u64,
}

#[derive(Debug)]
struct Scope {
    name: String,
    module: u64,
    /// `struct` for a `.struct`, `scope` for a `.proc`/`.scope`, absent
    /// for a module's own top-level scope.
    kind: Option<String>,
    size: Option<u64>,
    parent: Option<u64>,
}

#[derive(Debug)]
struct Segment {
    /// `ro`, `rw`, `bss`, `zp`, …
    kind: String,
    start: u64,
}

#[derive(Debug)]
struct Span {
    segment: u64,
    start: u64,
    /// Present on a span ca65 gave a type: data. An instruction span has
    /// none.
    typed: bool,
}

#[derive(Debug, Clone)]
struct Symbol {
    name: String,
    addrsize: String,
    size: Option<u64>,
    scope: u64,
    /// The `line` record of the definition.
    def: Option<u64>,
    value: Option<u64>,
    segment: Option<u64>,
    /// `lab`, `equ` or `imp`.
    kind: String,
    /// For an `imp`: the `sym` id of the export it resolves to.
    export: Option<u64>,
}

#[derive(Debug)]
struct Line {
    file: u64,
    line: u64,
}

/// The debug database, indexed by id.
#[derive(Debug, Default)]
pub struct Database {
    files: BTreeMap<u64, File>,
    modules: BTreeMap<u64, Module>,
    scopes: BTreeMap<u64, Scope>,
    segments: BTreeMap<u64, Segment>,
    spans: Vec<Span>,
    symbols: BTreeMap<u64, Symbol>,
    lines: BTreeMap<u64, Line>,
}

impl Database {
    /// Parses the text of an ld65 `--dbgfile`.
    pub fn parse(text: &str) -> Result<Self, String> {
        let mut db = Self::default();
        for (number, line) in text.lines().enumerate() {
            let Some((kind, rest)) = line.split_once('\t') else {
                continue;
            };
            let mut fields = Record::new();
            for field in split_fields(rest) {
                if let Some((key, value)) = field.split_once('=') {
                    fields.insert(key.to_owned(), value.trim_matches('"').to_owned());
                }
            }
            let id = || {
                number_field(&fields, "id")
                    .ok_or_else(|| format!("line {}: a `{kind}` record with no id", number + 1))
            };
            match kind {
                "file" => {
                    db.files.insert(
                        id()?,
                        File {
                            name: text_field(&fields, "name"),
                            modules: fields
                                .get("mod")
                                .map(|list| {
                                    list.split('+').filter_map(|id| id.parse().ok()).collect()
                                })
                                .unwrap_or_default(),
                        },
                    );
                }
                "mod" => {
                    db.modules.insert(
                        id()?,
                        Module {
                            file: number_field(&fields, "file").unwrap_or(0),
                        },
                    );
                }
                "scope" => {
                    db.scopes.insert(
                        id()?,
                        Scope {
                            name: text_field(&fields, "name"),
                            module: number_field(&fields, "mod").unwrap_or(0),
                            kind: fields.get("type").cloned(),
                            size: number_field(&fields, "size"),
                            parent: number_field(&fields, "parent"),
                        },
                    );
                }
                "seg" => {
                    db.segments.insert(
                        id()?,
                        Segment {
                            kind: text_field(&fields, "type"),
                            start: number_field(&fields, "start").unwrap_or(0),
                        },
                    );
                }
                "span" => db.spans.push(Span {
                    segment: number_field(&fields, "seg").unwrap_or(0),
                    start: number_field(&fields, "start").unwrap_or(0),
                    typed: fields.contains_key("type"),
                }),
                "sym" => {
                    db.symbols.insert(
                        id()?,
                        Symbol {
                            name: text_field(&fields, "name"),
                            addrsize: text_field(&fields, "addrsize"),
                            size: number_field(&fields, "size"),
                            scope: number_field(&fields, "scope").unwrap_or(0),
                            def: fields
                                .get("def")
                                .and_then(|list| list.split('+').next())
                                .and_then(|id| id.parse().ok()),
                            value: number_field(&fields, "val"),
                            segment: number_field(&fields, "seg"),
                            kind: text_field(&fields, "type"),
                            export: number_field(&fields, "exp"),
                        },
                    );
                }
                "line" => {
                    db.lines.insert(
                        id()?,
                        Line {
                            file: number_field(&fields, "file").unwrap_or(0),
                            line: number_field(&fields, "line").unwrap_or(0),
                        },
                    );
                }
                _ => {}
            }
        }
        if db.symbols.is_empty() {
            return Err(
                "the debug file names no symbols: assemble with `-g` (`cl65 -g`, or \
                        `ca65 -g`) so that ld65 records them"
                    .to_owned(),
            );
        }
        Ok(db)
    }

    /// The source file a module was built from.
    fn module_file(&self, module: u64) -> Option<&str> {
        let file = self.modules.get(&module)?.file;
        self.files.get(&file).map(|file| file.name.as_str())
    }

    /// The module built from `path`, matched by suffix: the database records
    /// absolute paths and the author names a relative one.
    ///
    /// A K816 unit reaches ld65 as the `.s` it compiled to, so its module's
    /// own file is the assembly; the `.k816` is in the file table because
    /// every line of that assembly names it, and the file record says which
    /// module read it. Either spelling finds the module, and an include file
    /// read by several modules names none of them.
    fn module_for(&self, path: &Path) -> Result<u64, String> {
        let wanted: Vec<Component<'_>> = path.components().collect();
        let matches = |name: &str| {
            let recorded: Vec<Component<'_>> = Path::new(name).components().collect();
            recorded.len() >= wanted.len()
                && recorded[recorded.len() - wanted.len()..] == wanted[..]
        };
        if let Some(module) = self
            .modules
            .iter()
            .find(|(_, module)| {
                self.files
                    .get(&module.file)
                    .is_some_and(|file| matches(&file.name))
            })
            .map(|(id, _)| *id)
        {
            return Ok(module);
        }
        let Some(file) = self.files.values().find(|file| matches(&file.name)) else {
            return Err(format!(
                "no module of the program was built from `{}`; the debug file knows {}",
                path.display(),
                self.modules
                    .values()
                    .filter_map(|module| self.files.get(&module.file))
                    .map(|file| format!("`{}`", file.name))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        };
        match file.modules.as_slice() {
            [module] => Ok(*module),
            [] => Err(format!(
                "`{}` was read by no module of the program",
                path.display()
            )),
            many => Err(format!(
                "`{}` is read by {} modules, so it names no one of them: name a module's own \
                 source",
                path.display(),
                many.len()
            )),
        }
    }

    /// Where a symbol was defined: `file:line`, with the file as the
    /// database names it.
    fn definition(&self, symbol: &Symbol) -> Option<(&str, u64)> {
        let line = self.lines.get(&symbol.def?)?;
        let file = self.files.get(&line.file)?;
        Some((file.name.as_str(), line.line))
    }

    /// Whether a label's bytes are data: the span at its address carries a
    /// type, or its segment is not read-only.
    fn is_data(&self, symbol: &Symbol) -> bool {
        let Some(segment) = symbol.segment.and_then(|id| self.segments.get(&id)) else {
            return true;
        };
        segment.kind != "ro" || self.span_at(symbol).unwrap_or(false)
    }

    /// The span starting at a label's address, as whether it carries a type:
    /// `.byte`/`.word` data does, a `.res` reservation and an instruction do
    /// not. `None` when no span starts there.
    fn span_at(&self, symbol: &Symbol) -> Option<bool> {
        let segment_id = symbol.segment?;
        let segment = self.segments.get(&segment_id)?;
        let offset = symbol.value?.wrapping_sub(segment.start);
        self.spans
            .iter()
            .find(|span| span.segment == segment_id && span.start == offset)
            .map(|span| span.typed)
    }

    /// The K816 source a module was compiled from, if it was: the `.k816`
    /// or `.k65` file its assembly's `.dbg` lines name. A K816 unit reaches
    /// ld65 as a `.s`, so its module's own file is the assembly and its
    /// labels are defined on the assembly's lines; the file table is what
    /// still says where it came from.
    fn k816_source(&self, module: u64) -> Option<&str> {
        self.files
            .values()
            .filter(|file| file.modules.contains(&module) && is_k816_unit(&file.name))
            .map(|file| file.name.as_str())
            .next()
    }
}

fn split_fields(text: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut current = String::new();
    let mut quoted = false;
    for character in text.chars() {
        match character {
            '"' => {
                quoted = !quoted;
                current.push(character);
            }
            ',' if !quoted => fields.push(std::mem::take(&mut current)),
            _ => current.push(character),
        }
    }
    if !current.is_empty() {
        fields.push(current);
    }
    fields
}

fn number_field(record: &Record, key: &str) -> Option<u64> {
    let text = record.get(key)?;
    match text.strip_prefix("0x") {
        Some(hex) => u64::from_str_radix(hex, 16).ok(),
        None => text.parse().ok(),
    }
}

fn text_field(record: &Record, key: &str) -> String {
    record.get(key).cloned().unwrap_or_default()
}

// ---------------------------------------------------------------------------
// What to write
// ---------------------------------------------------------------------------

/// What `k816 bind` is asked for.
#[derive(Debug, Default, Clone)]
pub struct Request {
    /// Declare only what the module built from this source imports: the
    /// interface one converted file needs. Without it, every symbol a ca65
    /// module exports that some other module uses.
    pub for_module: Option<PathBuf>,
    /// Also the symbols nothing imports.
    pub all: bool,
    /// `.struct`s as field layouts, instead of symbols.
    pub layouts: bool,
    /// With `layouts`: a `.struct` at an `.org` address as one fixed-address
    /// `var` per member instead of a field list — the form that crosses an
    /// `import`.
    pub flat: bool,
    /// The file the output is written to, for the relative paths of the
    /// `import` lines it may hold; the current directory when absent.
    pub output: Option<PathBuf>,
    /// The command line that produced the output, quoted in its header.
    pub invocation: String,
}

/// One declaration, ready to print.
struct Declaration {
    text: String,
    comment: String,
}

/// Writes the interface unit.
pub fn bind(
    db: &Database,
    request: &Request,
    read: &dyn Fn(&Path) -> Option<String>,
) -> Result<String, String> {
    let mut out = String::new();
    let _ = writeln!(out, "// Generated by `{}`.", request.invocation);
    if request.layouts {
        let _ = writeln!(
            out,
            "// Every `.struct` of the program as a K816 field layout: at its `.org` address\n\
             // where it had one, `abstract` where it only named offsets. A member wider than\n\
             // three bytes — a `.tag` or a `.res` — is a byte array of that size.\n\
             //\n\
             // An `abstract var` layout crosses an `import`; a `var` layout at a fixed address\n\
             // is usable only in the unit that declares it (`doc/syntax-reference.md`,\n\
             // \"Abstract symbolic layouts\"). A hardware map that many units read is therefore\n\
             // either pasted into each, or written flat — `var NAME_member = $ADDR` per member,\n\
             // an exported equate like any other — with `--flat`."
        );
        write_layouts(db, &mut out, request.flat);
        return Ok(out);
    }

    let _ = writeln!(
        out,
        "// The symbols ca65 modules provide, as K816 declarations. An address size is what\n\
         // ld65 recorded; a register width is not in the debug file, so every `extern func`\n\
         // is written without a contract — which says the routine preserves every axis — and\n\
         // its comment quotes the `.a8`/`.i8` directives nearest above it in its source.\n\
         // Promote those to `@a8 @i8` by hand where the routine really expects or changes a\n\
         // width: an extern's contract is trusted, never checked."
    );

    let (symbols, imports) = select(db, request)?;
    let base = request
        .output
        .as_ref()
        .and_then(|path| path.parent())
        .map(Path::to_path_buf)
        .unwrap_or_default();

    // K816 units are imported, not declared: their contracts are derived
    // from their bodies, which is better than anything written here.
    let mut imported: BTreeSet<String> = BTreeSet::new();
    for unit in imports {
        imported.insert(relative(&base, Path::new(&unit)));
    }
    if !imported.is_empty() {
        let _ = writeln!(out);
        let _ = writeln!(
            out,
            "// Units already in K816, whose names resolve by import."
        );
        for unit in imported {
            let _ = writeln!(out, "import \"{unit}\"");
        }
    }

    // Grouped by the module that defines them, so a reader converting that
    // module next knows which lines go away.
    let mut by_file: BTreeMap<String, Vec<Declaration>> = BTreeMap::new();
    for symbol in symbols {
        let (file, line) = db
            .definition(&symbol)
            .map(|(file, line)| (file.to_owned(), line))
            .unwrap_or_else(|| ("?".to_owned(), 0));
        let declaration = declare(db, &symbol, &file, line, read);
        by_file.entry(file).or_default().push(declaration);
    }
    let prefix = common_prefix(by_file.keys().map(String::as_str));
    for (file, declarations) in by_file {
        let shown = file.strip_prefix(&prefix).unwrap_or(&file);
        let _ = writeln!(out);
        let _ = writeln!(out, "// From {shown}");
        let width = declarations
            .iter()
            .map(|declaration| declaration.text.len())
            .max()
            .unwrap_or(0);
        for declaration in declarations {
            let _ = writeln!(
                out,
                "{:<width$}  // {}",
                declaration.text,
                declaration.comment,
                width = width
            );
        }
    }
    Ok(out)
}

/// The symbols to declare, and the K816 units to import instead.
fn select(db: &Database, request: &Request) -> Result<(Vec<Symbol>, Vec<String>), String> {
    let mut chosen: BTreeMap<String, Symbol> = BTreeMap::new();
    let mut units: Vec<String> = Vec::new();

    // The defining symbol behind an import.
    let resolve = |symbol: &Symbol| -> Option<&Symbol> { db.symbols.get(&symbol.export?) };

    // The module a symbol belongs to decides what it is: one compiled from
    // a K816 source is imported, every other is declared.
    let consider =
        |definition: &Symbol, chosen: &mut BTreeMap<String, Symbol>, units: &mut Vec<String>| {
            let Some(scope) = db.scopes.get(&definition.scope) else {
                return;
            };
            if let Some(source) = db.k816_source(scope.module) {
                units.push(source.to_owned());
                return;
            }
            chosen
                .entry(definition.name.clone())
                .or_insert_with(|| definition.clone());
        };

    match &request.for_module {
        Some(path) => {
            let module = db.module_for(path)?;
            for symbol in db.symbols.values().filter(|symbol| symbol.kind == "imp") {
                let in_module = db
                    .scopes
                    .get(&symbol.scope)
                    .is_some_and(|scope| scope.module == module);
                if !in_module {
                    continue;
                }
                if let Some(definition) = resolve(symbol) {
                    consider(definition, &mut chosen, &mut units);
                }
            }
        }
        None => {
            // Every definition some other module imports — or, with
            // `--all`, every definition at a module's top level.
            let imported: BTreeSet<u64> = db
                .symbols
                .values()
                .filter_map(|symbol| (symbol.kind == "imp").then_some(symbol.export?))
                .collect();
            for (id, symbol) in &db.symbols {
                if symbol.kind == "imp" {
                    continue;
                }
                let top_level = db
                    .scopes
                    .get(&symbol.scope)
                    .is_some_and(|scope| scope.parent.is_none());
                if !top_level {
                    continue;
                }
                if request.all || imported.contains(id) {
                    consider(symbol, &mut chosen, &mut units);
                }
            }
        }
    }
    units.sort();
    units.dedup();
    Ok((chosen.into_values().collect(), units))
}

fn is_k816_unit(file: &str) -> bool {
    Path::new(file)
        .extension()
        .is_some_and(|extension| extension == "k816" || extension == "k65")
}

/// The declaration for one symbol.
fn declare(
    db: &Database,
    symbol: &Symbol,
    file: &str,
    line: u64,
    read: &dyn Fn(&Path) -> Option<String>,
) -> Declaration {
    let short = Path::new(file)
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| file.to_owned());
    let mut comment = format!("{short}:{line}");

    let text = if symbol.kind == "equ" {
        if let Some(value) = symbol.value {
            let _ = write!(comment, ", = ${value:X}");
        }
        format!("extern const {}", symbol.name)
    } else if db.is_data(symbol) {
        let storage = match symbol.addrsize.as_str() {
            "zeropage" => "dp ",
            "far" => "far ",
            _ => "",
        };
        // A width is a claim K816 checks every access against, so only the
        // sizes that can mean one thing make one: a byte is a byte; two
        // reserved bytes are a word, where two *initialized* bytes may be a
        // table; three bytes in the direct page are a pointer, and three
        // anywhere else are more likely a table than a far datum.
        let typed = db.span_at(symbol).unwrap_or(false);
        let width = match (symbol.size, symbol.addrsize.as_str(), typed) {
            (Some(1), _, _) => " : byte",
            (Some(2), _, false) => " : word",
            (Some(3), "zeropage", false) => " : far",
            (Some(size), _, _) => {
                let _ = write!(comment, ", {size} bytes");
                ""
            }
            (None, _, _) => "",
        };
        format!("extern var {storage}{}{width}", symbol.name)
    } else {
        let far = if symbol.addrsize == "far" { "far " } else { "" };
        if let Some(state) = width_state_above(read, file, line) {
            let _ = write!(comment, ", ca65 state above it: {state}");
        }
        format!("extern {far}func {}", symbol.name)
    };
    Declaration { text, comment }
}

/// The `.a8`/`.a16`/`.i8`/`.i16` directives, or the `sep`/`rep` masks,
/// nearest above `line` in `file`: the widths the ca65 source believed its
/// routine ran at. A hint for the author, quoted rather than declared.
fn width_state_above(
    read: &dyn Fn(&Path) -> Option<String>,
    file: &str,
    line: u64,
) -> Option<String> {
    let text = read(Path::new(file))?;
    let mut accumulator: Option<&str> = None;
    let mut index: Option<&str> = None;
    for source_line in text
        .lines()
        .take(line.saturating_sub(1) as usize)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
    {
        let code = source_line
            .split(';')
            .next()
            .unwrap_or("")
            .trim()
            .to_ascii_lowercase();
        let mut words = code.split_whitespace();
        let Some(first) = words.next() else {
            continue;
        };
        let operand = words.next().unwrap_or("");
        match first {
            ".a8" => accumulator.get_or_insert(".a8"),
            ".a16" => accumulator.get_or_insert(".a16"),
            ".i8" => index.get_or_insert(".i8"),
            ".i16" => index.get_or_insert(".i16"),
            "sep" | "rep" => {
                let Some(mask) = immediate_mask(operand) else {
                    continue;
                };
                let (a, i) = if first == "sep" {
                    (".a8", ".i8")
                } else {
                    (".a16", ".i16")
                };
                if mask & 0x20 != 0 {
                    accumulator.get_or_insert(a);
                }
                if mask & 0x10 != 0 {
                    index.get_or_insert(i);
                }
                continue;
            }
            _ => continue,
        };
        if accumulator.is_some() && index.is_some() {
            break;
        }
    }
    match (accumulator, index) {
        (None, None) => None,
        (a, i) => Some([a, i].into_iter().flatten().collect::<Vec<_>>().join(" ")),
    }
}

/// The value of a `#$30`, `#%00110000` or `#48` operand.
fn immediate_mask(operand: &str) -> Option<u64> {
    let text = operand.trim().strip_prefix('#')?;
    if let Some(hex) = text.strip_prefix('$') {
        u64::from_str_radix(hex, 16).ok()
    } else if let Some(binary) = text.strip_prefix('%') {
        u64::from_str_radix(binary, 2).ok()
    } else {
        text.parse().ok()
    }
}

/// Every `.struct` as a layout.
fn write_layouts(db: &Database, out: &mut String, flat: bool) {
    for (id, scope) in &db.scopes {
        if scope.kind.as_deref() != Some("struct") {
            continue;
        }
        let mut members: Vec<&Symbol> = db
            .symbols
            .values()
            .filter(|symbol| symbol.scope == *id && symbol.kind == "equ")
            .collect();
        members.sort_by_key(|member| member.value.unwrap_or(0));
        let size = scope.size.unwrap_or(0);
        // A `.struct` with an `.org` records absolute addresses for its
        // members; one without records offsets, all inside its own size.
        let fixed = members
            .iter()
            .any(|member| member.value.is_some_and(|value| value >= size.max(1)));
        let base = members.first().and_then(|member| member.value).unwrap_or(0);
        let shown = db
            .module_file(scope.module)
            .and_then(|file| Path::new(file).file_name())
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_default();
        let _ = writeln!(out);
        let _ = writeln!(
            out,
            "// `.struct {}` from {shown}, {size} bytes",
            scope.name
        );
        if fixed && flat {
            let width = members
                .iter()
                .map(|member| member.name.len())
                .max()
                .unwrap_or(0);
            for member in members {
                let Some(value) = member.value else {
                    continue;
                };
                let suffix = match member.size {
                    Some(1) => ":byte",
                    Some(2) => ":word",
                    Some(3) => ":far",
                    _ => "",
                };
                let _ = writeln!(
                    out,
                    "var {}_{:<width$}{suffix} = ${value:04X}",
                    scope.name,
                    member.name,
                    width = width
                );
            }
            continue;
        }
        if fixed {
            let _ = writeln!(out, "var {}[", scope.name);
        } else {
            let _ = writeln!(out, "abstract var {}[", scope.name);
        }
        let mut cursor = if fixed { base } else { 0 };
        for member in members {
            let Some(value) = member.value else {
                continue;
            };
            if value > cursor {
                let _ = writeln!(
                    out,
                    "    ._reserved_{:X} :byte [{}]",
                    cursor,
                    value - cursor
                );
            }
            let member_size = member.size.unwrap_or(1);
            let width = match member_size {
                1 => ":byte".to_owned(),
                2 => ":word".to_owned(),
                3 => ":far".to_owned(),
                n => format!(":byte [{n}]"),
            };
            let _ = writeln!(out, "    .{} {width}", member.name);
            cursor = value + member_size;
        }
        if fixed {
            let _ = writeln!(out, "] = ${base:04X}");
        } else {
            let _ = writeln!(out, "]");
        }
    }
}

/// The longest directory prefix shared by every path.
fn common_prefix<'a>(paths: impl Iterator<Item = &'a str>) -> String {
    let mut prefix: Option<Vec<&str>> = None;
    for path in paths {
        let parts: Vec<&str> = path
            .rsplitn(2, '/')
            .last()
            .into_iter()
            .flat_map(|dir| dir.split('/'))
            .collect();
        prefix = Some(match prefix {
            None => parts,
            Some(current) => current
                .iter()
                .zip(parts.iter())
                .take_while(|(a, b)| a == b)
                .map(|(a, _)| *a)
                .collect(),
        });
    }
    match prefix {
        Some(parts) if !parts.is_empty() => format!("{}/", parts.join("/")),
        _ => String::new(),
    }
}

/// `target` relative to the directory `base`, lexically: the string an
/// `import` in a file inside `base` writes to reach it.
fn relative(base: &Path, target: &Path) -> String {
    let base: Vec<Component<'_>> = base.components().collect();
    let target: Vec<Component<'_>> = target.components().collect();
    let shared = base
        .iter()
        .zip(target.iter())
        .take_while(|(a, b)| a == b)
        .count();
    let mut parts: Vec<String> = Vec::new();
    for _ in shared..base.len() {
        parts.push("..".to_owned());
    }
    for component in &target[shared..] {
        parts.push(component.as_os_str().to_string_lossy().into_owned());
    }
    parts.join("/")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_relative_path_walks_up_and_down() {
        assert_eq!(
            relative(Path::new("/p/src/kernel"), Path::new("/p/src/hw/ria.k816")),
            "../hw/ria.k816"
        );
        assert_eq!(
            relative(Path::new("/p/src"), Path::new("/p/src/a.k816")),
            "a.k816"
        );
        assert_eq!(relative(Path::new(""), Path::new("a.k816")), "a.k816");
    }

    #[test]
    fn the_width_hint_reads_directives_and_masks_nearest_above() {
        let read = |_: &Path| {
            Some(
                ".a8\n.i8\nfoo:\n    rep #$30\n    rts\n.a16\nbar:\n    sep #%00010000\nbaz:\n    nop\n"
                    .to_owned(),
            )
        };
        assert_eq!(width_state_above(&read, "x", 3).as_deref(), Some(".a8 .i8"));
        // `rep #$30` above `bar:` outranks the `.a8`/`.i8` further up, and
        // `.a16` on line 6 is nearer still for the accumulator.
        assert_eq!(
            width_state_above(&read, "x", 7).as_deref(),
            Some(".a16 .i16")
        );
        // `sep #$10` narrows the index only.
        assert_eq!(
            width_state_above(&read, "x", 9).as_deref(),
            Some(".a16 .i8")
        );
        assert_eq!(width_state_above(&read, "x", 1), None);
    }

    #[test]
    fn a_database_without_symbols_says_to_assemble_with_g() {
        let error = Database::parse("version\tmajor=2,minor=0\nfile\tid=0,name=\"a.asm\"\n")
            .expect_err("no symbols");
        assert!(error.contains("-g"), "{error}");
    }

    #[test]
    fn quoted_commas_stay_inside_their_field() {
        assert_eq!(
            split_fields("id=0,name=\"a,b\",size=1"),
            ["id=0", "name=\"a,b\"", "size=1"]
        );
    }
}
