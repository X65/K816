/*****************************************************************************/
/*                                                                           */
/*                                 dialect.rs                                */
/*                                                                           */
/*              K816 and K65 sources: what each dialect accepts              */
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

//! A source's dialect, and what each one accepts (`plans/14-readable-hla.md`).
//!
//! The grammar is one, so a K65 spelling parses in any file; the dialect is
//! what analysis is told, and it is read off the file's name.

use k816::source::{Dialect, SourceMap};
use k816::{Options, compile_to_ca65};

#[test]
fn a_sources_dialect_is_its_extension() {
    for (name, dialect) in [
        ("main.k65", Dialect::K65),
        ("MAIN.K65", Dialect::K65),
        ("file:///home/me/demo/main.k65", Dialect::K65),
        ("main.kk", Dialect::K816),
        ("main.k816", Dialect::K816),
        ("lib.k65/main.kk", Dialect::K816),
        ("main", Dialect::K816),
        ("main.k65.bak", Dialect::K816),
    ] {
        assert_eq!(Dialect::of_name(name), dialect, "{name}");
        let mut map = SourceMap::new();
        let file = map.add_file(name, "");
        assert_eq!(map.file(file).dialect(), dialect, "{name}");
    }
}

/// A program both dialects accept compiles to the same assembly under every
/// extension: the dialect decides what is refused, never what anything means.
#[test]
fn a_program_both_dialects_accept_compiles_alike_under_every_extension() {
    const PROGRAM: &str = "\
var counter = $10
var flags = $11

func main @a8 @i8 {
    x = #0
    a?#4
    >= { counter++ }
    lda flags
    and #$0F
    != goto .done
    flags = a = counter
.done:
}
";
    let assembly = |name: &str| {
        let mut map = SourceMap::new();
        let file = map.add_file(name, PROGRAM);
        let result = compile_to_ca65(&map, file, &Options::default());
        assert_eq!(result.diagnostics, vec![], "{name}");
        result.assembly.expect("compiles").replace(name, "UNIT")
    };
    let k65 = assembly("main.k65");
    assert_eq!(assembly("main.kk"), k65);
    assert_eq!(assembly("main.k816"), k65);
}

/// What `text` compiles to as a unit called `name`: the codes it was told,
/// and the assembly with comments and the unit's own name taken out, so two
/// spellings of one program compare equal.
fn code(name: &str, text: &str) -> (Vec<&'static str>, Vec<String>) {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let result = compile_to_ca65(&map, file, &Options::default());
    let codes = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    let assembly = result
        .assembly
        .unwrap_or_else(|| panic!("{name} compiles:\n{text}\n{:#?}", result.diagnostics));
    (codes, code_lines(&assembly, name))
}

/// Each of K816's spellings compiles to the bytes of the K65 spelling it
/// replaces: respelling changes nothing a program does
/// (`plans/14-readable-hla.md` decision 13). One pair per inventory row the
/// plan respells, and every flag both ways in every place a condition goes.
#[test]
fn every_respelled_form_compiles_to_the_bytes_of_its_k65_spelling() {
    let mut pairs: Vec<(String, String)> = vec![
        ("loop { x-- } while !=".into(), "{ x-- } !=".into()),
        ("loop { x++ } while >=0".into(), "{ x++ } >=0".into()),
        (
            "loop { a = flags } always".into(),
            "{ a = flags } always".into(),
        ),
        ("loop { < break } never".into(), "{ < break } never".into()),
        (
            "nocross loop { y++ } while !=".into(),
            "nocross { y++ } !=".into(),
        ),
        (
            "nocross loop { y++ } always".into(),
            "nocross { y++ } always".into(),
        ),
        (
            "loop { x++ \n == repeat \n y++ } while !=".into(),
            "{ x++ \n == repeat \n y++ } !=".into(),
        ),
        ("push a, x, y".into(), "a!! x!! y!!".into()),
        ("pull y, x, a".into(), "y?? x?? a??".into()),
        ("push p, dbr, dpr".into(), "p!! dbr!! dpr!!".into()),
        ("pull dpr, dbr, p".into(), "dpr?? dbr?? p??".into()),
        ("push a, $0012, x".into(), "a!! \n pea #$0012 \n x!!".into()),
        ("push LIMIT".into(), "pea #LIMIT".into()),
        ("push #LIMIT".into(), "pea #LIMIT".into()),
        ("push (&<flags):word".into(), "pea #(&<flags):word".into()),
    ];
    for (letter, flag) in [('c', 'c'), ('z', 'z'), ('n', 'n'), ('v', 'v')] {
        for (bare, signed) in [
            (format!("{letter}?"), format!("{flag}+?")),
            (format!("!{letter}?"), format!("{flag}-?")),
        ] {
            pairs.push((format!("{bare} {{ x++ }}"), format!("{signed}{{ x++ }}")));
            pairs.push((format!("{bare} goto .out"), format!("{signed} goto .out")));
            pairs.push((
                format!("loop {{ x++ \n {bare} break }} always"),
                format!("{{ x++ \n {signed} break }} always"),
            ));
            pairs.push((
                format!("loop {{ x++ }} while {bare}"),
                format!("{{ x++ }} {signed}"),
            ));
        }
    }
    let unit = |body: &str| {
        format!(
            "var flags = $10\nconst LIMIT = $1234\n\nfunc work @a8 @i8 {{\n    {body}\n.out:\n}}\n"
        )
    };
    for (readable, k65) in pairs {
        assert_eq!(
            code("unit.kk", &unit(&readable)),
            code("unit.k65", &unit(&k65)),
            "`{readable}` and `{k65}`"
        );
    }
}

/// `readable-hla.kk` is `readable-hla.k65` respelled, and nothing else: the
/// two compile to the same program. The pair is the witness the manifest's
/// three readable-spelling rows name.
#[test]
fn the_readable_golden_is_its_k65_twin_respelled() {
    let golden = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let read = |name: &str| std::fs::read_to_string(golden.join(name)).expect("read golden");
    let readable = code("readable-hla.kk", &read("readable-hla.kk"));
    let k65 = code("readable-hla.k65", &read("readable-hla.k65"));
    assert_eq!(readable.0, Vec::<&str>::new(), "the K816 spelling warns");
    assert_eq!(readable, k65);
}

/// The codes `body` is told in a `@a16 @i16` function, the fixes they carry,
/// and the program the first fix of each makes, applied.
fn pushed(body: &str) -> (Vec<&'static str>, Vec<String>) {
    let text =
        format!("var mem = $1234\nconst LIMIT = 5\n\nfunc work @a16 @i16 {{\n    {body}\n}}\n");
    let mut map = SourceMap::new();
    let file = map.add_file("push.kk", text.as_str());
    let result = compile_to_ca65(&map, file, &Options::default());
    let codes = result.diagnostics.iter().map(|d| d.code.as_str()).collect();
    let fixes = result
        .diagnostics
        .iter()
        .flat_map(|d| d.fixes.iter())
        .flat_map(|fix| fix.edits.iter())
        .map(|edit| edit.replacement.clone())
        .collect();
    (codes, fixes)
}

/// A pushed constant has to be written as the word `PEA` pushes
/// (`plans/14-readable-hla.md` decision 9): one written narrower is warned
/// about and rewritten as four hex digits, one that is a byte by construction
/// is refused and expanded with `:word`, and a sizeless one is taken at its
/// word.
#[test]
fn a_pushed_constant_is_written_as_the_word_it_pushes() {
    for (body, codes, fixes) in [
        ("push $12", vec!["K816-W0174"], vec!["$0012"]),
        ("push $012", vec!["K816-W0174"], vec!["$0012"]),
        ("push 0x12", vec!["K816-W0174"], vec!["$0012"]),
        ("push 18", vec!["K816-W0174"], vec!["$0012"]),
        ("push 0", vec!["K816-W0174"], vec!["$0000"]),
        ("push %00010010", vec!["K816-W0174"], vec!["$0012"]),
        ("push ($12)", vec!["K816-W0174"], vec!["$0012"]),
        ("push #$12", vec!["K816-W0174"], vec!["$0012"]),
        ("push a, $12, x", vec!["K816-W0174"], vec!["$0012"]),
        ("push $0012", vec![], vec![]),
        ("push 300", vec![], vec![]),
        ("push %0000000000010010", vec![], vec![]),
        ("push LIMIT", vec![], vec![]),
        ("push LIMIT + 1", vec![], vec![]),
        ("push &&mem", vec![], vec![]),
        ("push (&<mem):word", vec![], vec![]),
        ("push &<mem", vec!["K816-E0175"], vec!["(&<mem):word"]),
        ("push &>mem", vec!["K816-E0175"], vec!["(&>mem):word"]),
        ("push (&<mem)", vec!["K816-E0175"], vec!["(&<mem):word"]),
        ("push 'a'", vec!["K816-E0175"], vec!["('a'):word"]),
        ("push LIMIT:byte", vec!["K816-E0175"], vec!["LIMIT:word"]),
    ] {
        assert_eq!(
            pushed(body),
            (codes, fixes.into_iter().map(String::from).collect()),
            "{body}"
        );
    }
    // Each fix, applied, is a push the rule accepts — and the warning's
    // pushes the same word the narrow spelling did.
    for (fixed, narrow) in [("push $0012", "push $12"), ("push $0012", "push 18")] {
        assert_eq!(
            code(
                "unit.kk",
                &format!("func work @a16 @i16 {{\n    {fixed}\n}}\n")
            )
            .1,
            {
                let text = format!("func work @a16 @i16 {{\n    {narrow}\n}}\n");
                let mut map = SourceMap::new();
                let file = map.add_file("unit.kk", text.as_str());
                let result = compile_to_ca65(&map, file, &Options::default());
                code_lines(
                    &result.assembly.expect("a warning still compiles"),
                    "unit.kk",
                )
            },
            "{narrow}"
        );
    }
    for fixed in ["push (&<mem):word", "push ('a'):word", "push LIMIT:word"] {
        assert_eq!(pushed(fixed), (vec![], vec![]), "{fixed}");
    }
}

/// The assembly lines [`code`] compares, from an assembly text: no comments,
/// no unit name, and no source lines — those are debug information, not
/// bytes, and a K65 spelling can take more lines than the K816 one it is
/// compared with.
fn code_lines(assembly: &str, name: &str) -> Vec<String> {
    assembly
        .replace(name, "UNIT")
        .lines()
        .map(|line| line.split(';').next().unwrap_or_default().trim_end())
        .filter(|line| !line.trim().is_empty() && !line.trim_start().starts_with(".dbg"))
        .map(str::to_owned)
        .collect()
}
