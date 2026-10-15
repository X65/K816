/*****************************************************************************/
/*                                                                           */
/*                                   mod.rs                                  */
/*                                                                           */
/*                       ca65 sources converted to K816                      */
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

//! `k816 convert`: a ca65 program's `.asm` and `.inc` files, as `.kk` units.
//!
//! What it writes is the program in K816's own terms, not a transliteration.
//! A routine is a `func`, `naked` or `far func`; a width the source set is
//! `@a8` or an `@a8 { … }` block; a `.struct` is a field layout every importer
//! reads; a `jsr` is a bare call; a `.macro` is an `inline`; an `.include` and
//! an `.import` are `import`s. What it does not write is anything the source
//! did not say: a routine's header states a width only where the original
//! relied on one, and the program keeps its shape — its segments, its order,
//! its labels — so that the build can be compared with the original's byte
//! for byte (`doc/ca65-to-k816.md`, "The converter").
//!
//! Every place the conversion could not be faithful is a [`Note`]; the unit
//! still converts, with the original line kept as a comment.

mod emit;
pub mod expr;
pub mod items;
pub mod lex;
pub mod program;
pub mod render;
pub mod source;
pub mod structure;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

/// What to convert.
#[derive(Clone, Debug, Default)]
pub struct Request {
    /// The files to convert. Every `.include` they reach is converted too.
    pub inputs: Vec<PathBuf>,
    /// Where `.include` looks after the including file's own directory, in
    /// order — ca65's `-I`.
    pub include_dirs: Vec<PathBuf>,
}

/// One converted unit.
#[derive(Clone, Debug)]
pub struct Converted {
    /// The `.kk` file to write.
    pub path: PathBuf,
    /// The ca65 files it was converted from.
    pub sources: Vec<PathBuf>,
    pub text: String,
}

/// Something the conversion says about a line.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Note {
    pub path: PathBuf,
    pub line: usize,
    pub message: String,
}

impl Note {
    pub fn new(path: &Path, line: usize, message: impl Into<String>) -> Self {
        Self {
            path: path.to_path_buf(),
            line,
            message: message.into(),
        }
    }
}

/// The whole conversion.
#[derive(Clone, Debug, Default)]
pub struct Conversion {
    pub units: Vec<Converted>,
    pub notes: Vec<Note>,
}

/// Converts the program `request` names, reading files through `read`.
pub fn convert(request: &Request, read: program::Reader<'_>) -> Result<Conversion, String> {
    let program = program::Program::load(&request.inputs, &request.include_dirs, read)?;
    let mut conversion = Conversion {
        units: Vec::new(),
        notes: program.notes.clone(),
    };
    let generic = generic_unit(&request.inputs);
    let mut needs_generic = false;
    for unit in 0..program.units.len() {
        let written = emit::unit(&program, unit, &generic, &request.include_dirs, read);
        needs_generic |= written.uses_generic;
        conversion.notes.extend(written.notes);
        let record = &program.units[unit];
        let mut sources = vec![program.files[record.file].path.clone()];
        if let Some(merged) = record.merged {
            sources.push(program.files[merged].path.clone());
        }
        conversion.units.push(Converted {
            path: record.output.clone(),
            sources,
            text: formatted(&record.output, written.text),
        });
    }
    if needs_generic {
        conversion.units.push(Converted {
            path: generic,
            sources: Vec::new(),
            text: GENERIC.to_owned(),
        });
    }
    // An `.inc` is one unit with the `.asm` of its stem only when that `.asm`
    // includes it, so one the include does not reach — a path no `-I`
    // resolves — is a unit of its own with the same output.
    let mut outputs: std::collections::BTreeMap<&Path, &Converted> = Default::default();
    for unit in &conversion.units {
        if let Some(first) = outputs.insert(&unit.path, unit) {
            let named = |converted: &Converted| match converted.sources.first() {
                Some(source) => format!("`{}`", source.display()),
                None => "the `.macpack generic` unit".to_owned(),
            };
            let name = unit.path.file_name().unwrap_or_default().display();
            return Err(format!(
                "{} and {} would both become `{name}`; an `.include` that resolves \
                 with `-I` makes an `.inc` and the `.asm` of its stem one unit",
                named(first),
                named(unit),
            ));
        }
    }
    // The program's labels and branches, raised into K816's loops and blocks
    // wherever that changes no byte (`structure.rs`).
    structure::raise(&mut conversion.units);
    conversion
        .notes
        .sort_by(|a, b| (&a.path, a.line).cmp(&(&b.path, b.line)));
    Ok(conversion)
}

/// `text` as `k816 fmt` leaves it, so that what the converter writes is
/// already formatted; unchanged where it does not parse.
fn formatted(path: &Path, text: String) -> String {
    let mut map = crate::source::SourceMap::new();
    let file = map.add_file(path.display().to_string(), text.clone());
    let parse = crate::parser::parse_source(map.file(file), None);
    crate::parser::format::format(map.file(file), &parse.tree).unwrap_or(text)
}

/// `.macpack generic`'s `add` and `sub`, as `inline`s with no width: each
/// use runs at its use site's.
pub const GENERIC: &str = "\
// `.macpack generic`, as `k816 convert` writes it.
//
// Neither section states a width, so each runs at the width of the place
// it is expanded into: `add #4` is a two-byte `adc` under `@a8` and a
// three-byte one under `@a16`.

inline add (#value) {
    clc
    adc #value
}

inline sub (#value) {
    sec
    sbc #value
}
";

/// Where the generated `generic.kk` goes: the directory every input shares.
fn generic_unit(inputs: &[PathBuf]) -> PathBuf {
    shared_directory(inputs).join("generic.kk")
}

/// The directory every path in `paths` is under.
pub fn shared_directory(paths: &[PathBuf]) -> PathBuf {
    let mut shared: Option<PathBuf> = None;
    for path in paths {
        let dir = path.parent().map(Path::to_path_buf).unwrap_or_default();
        shared = Some(match shared {
            None => dir,
            Some(current) => current
                .components()
                .zip(dir.components())
                .take_while(|(a, b)| a == b)
                .map(|(a, _)| a.as_os_str().to_owned())
                .collect(),
        });
    }
    shared.unwrap_or_default()
}

/// Whether K816 accepts `name` as a unit-level name.
///
/// Asked of the compiler rather than of a list: K816 reserves its keywords,
/// every register and flag spelling, and every mnemonic (`K816-E0158`,
/// `K816-E0173`), and a list kept here would drift from the grammar it
/// copied. A declaration of the name is compiled, and the name is usable
/// when nothing is said about it.
pub fn usable_name(name: &str) -> bool {
    static CACHE: OnceLock<Mutex<HashMap<String, bool>>> = OnceLock::new();
    let cache = CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    if let Some(known) = cache.lock().ok().and_then(|cache| cache.get(name).copied()) {
        return known;
    }
    let mut map = crate::source::SourceMap::new();
    // As a declaration and as an operand, plain and indexed: `address` is a
    // fine variable name and a placement directive at the start of a body.
    let file = map.add_file(
        "probe.kk",
        format!(
            "var {name} = $1000\nfunc k816_probe @a8 @i8 {{\n    lda {name}\n    sta {name},x\n}}\n"
        ),
    );
    let result = crate::compile_to_ca65(&map, file, &crate::Options::default());
    let usable = result.diagnostics.is_empty();
    if let Ok(mut cache) = cache.lock() {
        cache.insert(name.to_owned(), usable);
    }
    usable
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_compiler_says_which_names_are_reserved() {
        assert!(usable_name("kernel_start"));
        for reserved in ["d", "x", "lda", "func", "var", "address"] {
            assert!(!usable_name(reserved), "`{reserved}` is reserved");
        }
    }
}
