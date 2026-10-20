/*****************************************************************************/
/*                                                                           */
/*                              preprocessor.rs                              */
/*                                                                           */
/*          What `#if`, `unroll` and the brackets read before analysis       */
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

//! The preprocessor decides `#if`, expands `unroll` and runs the brackets
//! before analysis exists, against values it folds itself, and nothing after
//! it re-checks what it decided. So the claim under these tests is that **a
//! name has one value in a unit**: whatever the preprocessor read, analysis
//! exports and emits.
//!
//! The defect these were written for: every `var` was declared to the
//! preprocessor with a one-byte placeholder, so `const TSZ = T:sizeof` was 1
//! there and the layout's real size everywhere else. `unroll TSZ` wrote one
//! copy beside `.export TSZ = $0040`, a guard `#if [ TSZ != 64 ]` fired on a
//! 64-byte layout, and a `:offsetof` constant was not declared at all.

use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

/// A 64-byte layout with a field at 2 and a nested one at 3, in each of the
/// three ways a `var` can hold one.
const LAYOUTS: [&str; 3] = [
    "abstract var T[ .a :word .m[ .x :byte .y :word ] .b :byte [59] ]",
    "var T[ .a :word .m[ .x :byte .y :word ] .b :byte [59] ] = $4000",
    "var T[ .a :word .m[ .x :byte .y :word ] .b :byte [59] ]",
];

fn compile(text: &str) -> k816::CompileResult {
    let mut map = SourceMap::new();
    let file = map.add_file("unit.kk", text);
    compile_to_ca65(&map, file, &Options::default())
}

/// The `asl`s `text` compiles to, which is what an `unroll` count wrote.
fn asls(text: &str) -> usize {
    let result = compile(text);
    assert_eq!(result.diagnostics, vec![], "{text}");
    let assembly = result.assembly.expect("compiles");
    assembly
        .lines()
        .filter(|line| line.split_whitespace().next() == Some("asl"))
        .count()
}

fn codes(text: &str) -> Vec<&'static str> {
    compile(text)
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect()
}

/// A layout query read by the preprocessor — through a `const`, a bracket or
/// both — is the layout's real size or offset, for every way of declaring the
/// layout and for a nested field too.
#[test]
fn a_layout_query_reads_the_same_before_analysis_as_after() {
    for layout in LAYOUTS {
        for (query, expected) in [
            ("T:sizeof", 64),
            ("T.b:offsetof", 5),
            ("T.m:sizeof", 3),
            ("T.m.y:offsetof", 3),
            ("T.m:offsetof", 2),
        ] {
            let header = format!("{layout}\nconst Q = {query}\n");
            for (reader, prelude) in [("Q", ""), ("[Q]", ""), ("PROBE", "[ PROBE = Q ]\n")] {
                let text =
                    format!("{header}{prelude}func f @a16 {{ unroll {reader} {{ asl }} }}\n");
                assert_eq!(asls(&text), expected, "{text}");
            }
            // And the value the unit exports is the value it unrolled by.
            let result = compile(&format!("{header}func f @a16 {{ asl }}\n"));
            let assembly = result.assembly.expect("compiles");
            assert!(
                assembly.contains(&format!(".export Q : abs = ${expected:04X}")),
                "{layout} / {query}:\n{assembly}"
            );
        }
    }
}

/// A guard against layout drift is checked against the layout, so the one
/// that should pass passes and the branch that should be taken is.
#[test]
fn a_condition_reads_a_layouts_real_size() {
    for layout in LAYOUTS {
        let guarded = format!(
            "{layout}\nconst TSZ = T:sizeof\n#if [ TSZ != 64 ]\n#error \"drift\"\n#endif\nfunc f @a16 {{ asl }}\n"
        );
        assert_eq!(codes(&guarded), Vec::<&str>::new(), "{guarded}");
        let branch = format!(
            "{layout}\nconst TSZ = T:sizeof\nfunc f @a16 {{\n#if [ TSZ == 64 ]\n    asl\n    asl\n#else\n    asl\n#endif\n}}\n"
        );
        assert_eq!(asls(&branch), 2, "{branch}");
    }
}

/// A field count, a raw span and an allocation count the shape folds — from a
/// `const` or a bracket above — reach the preprocessor's answer too: the
/// shape is the one computation both passes run, not a copy of it.
#[test]
fn a_shape_built_from_constants_is_the_same_shape_in_both_passes() {
    for (declaration, expected) in [
        (
            "const COUNT = 62\nabstract var T[ .a :word .b :byte [COUNT] ]",
            64,
        ),
        (
            "[ COUNT = 30 ]\nabstract var T[ .a :word .b :byte [COUNT] ]",
            32,
        ),
        // A bracket in the declaration itself runs before the shape folds.
        ("abstract var T[ .a :word .b :byte [[2 * 15]] ]", 32),
        ("const COUNT = 5\nvar T:word[COUNT]", 10),
        // `* count` is outside the element, so `:sizeof` is one element.
        ("const COUNT = 7\nvar T:word[3] * COUNT", 6),
        ("abstract var T:word[ .a .b:byte[ .c .d ] .e ]", 6),
    ] {
        let text = format!(
            "{declaration}\nconst TSZ = T:sizeof\nfunc f @a16 {{ unroll TSZ {{ asl }} }}\n"
        );
        assert_eq!(asls(&text), expected, "{text}");
    }
}

/// A task table indexed by shifting: a shift count derived from a layout's size
/// in a bracket, guarded by `#if`. It shifts by the size it exports, and the
/// guard fires for a size that is not a power of two.
#[test]
fn a_shift_count_derived_from_a_layout_shifts_by_the_layouts_size() {
    let source = |span: u32| {
        format!(
            "abstract var T[ .a :word .b :byte [{span}] ]\n\
             const TSZ = T:sizeof\n\
             [ SH = 0  while TSZ << SH < $100 {{ SH = SH + 1 }} ]\n\
             #if [ SH == 0 || TSZ << SH != $100 ]\n\
             #error \"T:sizeof must be a power of two smaller than a page\"\n\
             #endif\n\
             func f @a16 {{ unroll SH {{ asl }} }}\n"
        )
    };
    assert_eq!(asls(&source(62)), 2, "a 64-byte layout");
    assert_eq!(asls(&source(30)), 3, "a 32-byte layout");
    assert_eq!(codes(&source(40)), ["K816-E0150"], "a 42-byte layout");
}

/// A name analysis refuses to declare is not declared to the preprocessor
/// either. `N` is a flag, so `const N` is refused, and a reserved evaluator
/// name stays a local of its bracket; before, both were constants to an
/// `unroll` count and nothing else, which is a name with a value in one pass
/// and none in the other.
#[test]
fn a_reserved_name_is_declared_to_neither_pass() {
    assert_eq!(
        codes("const N = 3\nfunc f @a16 { unroll N { asl } }\n"),
        ["K816-E0101", "K816-E0158"],
    );
    // Seven copies, silently, before; now the count reads the flag `c`.
    assert_eq!(
        codes("[ c = 7 ]\nfunc f @a16 { unroll c { asl } }\n"),
        ["K816-E0158"],
    );
}

/// A value this pass cannot fold is refused where it is read, and said to be
/// unanswerable here rather than undeclared — never declared with the count
/// it could not read left out.
///
/// A section's bytes are assembled after every `#if` and `unroll` is
/// decided, so `[index(tab, 1)]` as a field count folds in analysis and not
/// here. Declaring `T` anyway would make it three bytes here and thirty-two
/// there: three copies, beside `.export TSZ = $0020`.
#[test]
fn a_shape_this_pass_cannot_fold_is_refused_rather_than_guessed() {
    let declarations = "data tab { 4 30 6 }\n\
                        abstract var T[ .a :word .b :byte [[index(tab, 1)]] ]\n\
                        const TSZ = T:sizeof\n";
    let result = compile(&format!(
        "{declarations}func f @a16 {{ unroll TSZ {{ asl }} }}\n"
    ));
    let refused: Vec<_> = result
        .diagnostics
        .iter()
        .map(|diagnostic| (diagnostic.code.as_str(), diagnostic.title.as_str()))
        .collect();
    assert_eq!(
        refused,
        [(
            "K816-E0108",
            "`TSZ` has no value where `#if` and `unroll` are decided"
        )]
    );
    // And analysis, which assembles the section, answers it.
    let result = compile(&format!("{declarations}func f @a16 {{ lda #TSZ }}\n"));
    assert_eq!(result.diagnostics, vec![]);
    assert!(
        result
            .assembly
            .expect("compiles")
            .contains("lda     #$0020")
    );
}

/// `doc/syntax-reference.md`, "Conditions": the layout guard it shows passes
/// on the layout it shows and fires on one a byte shorter.
#[test]
fn the_documented_layout_guard_checks_the_layout() {
    let document = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("doc/syntax-reference.md"),
    )
    .expect("the document reads");
    let example = document
        .split("```k65\n")
        .filter_map(|block| block.split_once("```").map(|(code, _)| code))
        .find(|code| code.contains("TCB_SIZE"))
        .expect("the guard example is in the document");
    assert_eq!(codes(example), Vec::<&str>::new(), "{example}");
    let shorter = example.replace("[61]", "[60]");
    assert_ne!(shorter, example, "the example sizes its layout with `[61]`");
    assert_eq!(codes(&shorter), ["K816-E0150"], "{shorter}");
}

/// Compiles `unit` in a program of `files`, each a name and a text.
fn program(files: &[(&str, &str)], unit: &str) -> k816::CompileResult {
    let mut map = SourceMap::new();
    for (name, text) in files {
        map.add_file(*name, *text);
    }
    let entry = map.add_file("unit.kk", unit);
    compile_to_ca65(&map, entry, &Options::default())
}

/// The `asl`s an assembly holds, which is what an `unroll` count wrote.
fn asls_in(result: k816::CompileResult, what: &str) -> usize {
    assert_eq!(result.diagnostics, vec![], "{what}");
    result
        .assembly
        .expect("compiles")
        .lines()
        .filter(|line| line.split_whitespace().next() == Some("asl"))
        .count()
}

/// An `import` makes available what typing the imported declarations would,
/// in `#if`, `unroll` counts and brackets as everywhere else: each use below
/// compiles over an import to what it compiles to with the declarations
/// written above it.
#[test]
fn an_import_makes_available_what_typing_its_declarations_would() {
    for (declarations, usage, expected) in [
        (
            "const COUNT = 3",
            "#if [ COUNT == 3 ]\nfunc f @a16 { unroll COUNT { asl } }\n#endif",
            3,
        ),
        (
            "abstract var R[ .a :word .b :byte [62] ]",
            "const RSZ = R:sizeof\nfunc f @a16 { unroll RSZ { asl } }",
            64,
        ),
        (
            "abstract var R[ .a :word .b :byte [62] ]",
            "const OFS = R.b:offsetof\nfunc f @a16 { unroll OFS { asl } }",
            2,
        ),
        (
            "var VR[ .a :word .b :byte ] = $4000",
            "#if [ VR:sizeof == 3 ]\nfunc f @a16 { unroll VR:sizeof { asl } }\n#endif",
            3,
        ),
        (
            "const COUNT = 30",
            "abstract var T[ .a :word .b :byte [[COUNT]] ]\nconst TSZ = T:sizeof\n\
             func f @a16 { unroll TSZ { asl } }",
            32,
        ),
        (
            "evalfunc dbl(q) [ q * 2 ]",
            "func f @a16 { unroll [dbl(2)] { asl } }",
            4,
        ),
        (
            "[ EV = 5 ]",
            "#if [ EV == 5 ]\nfunc f @a16 { unroll EV { asl } }\n#endif",
            5,
        ),
    ] {
        let inline = asls(&format!("{declarations}\n{usage}\n"));
        assert_eq!(inline, expected, "{declarations} / {usage} typed inline");
        let imported = program(
            &[("defs.kk", &format!("{declarations}\n"))],
            &format!("import \"defs.kk\"\n{usage}\n"),
        );
        assert_eq!(
            asls_in(imported, usage),
            expected,
            "{declarations} / {usage} imported"
        );
    }
    // A section's bytes cross too: an imported section was assembled by the
    // unit that declares it, before this one's directives are decided.
    let imported = program(
        &[("defs.kk", "data tab { 4 5 6 }\n")],
        "import \"defs.kk\"\n#if [ size(tab) == 3 ]\nfunc f @a16 { unroll [index(tab, 2)] { asl } }\n#endif\n",
    );
    assert_eq!(asls_in(imported, "an imported section"), 6);
}

/// A value reaches a unit through a chain of imports, one hop at a time: a
/// `const`, a layout and an evaluator name each folded from what the unit
/// before it imports. Each unit still sees only what it imports directly.
#[test]
fn a_value_folded_from_an_import_crosses_the_next_import() {
    let files = [
        (
            "far.kk",
            "const BASE = 3\nabstract var L[ .a :word .b :byte [BASE] ]\n",
        ),
        (
            "near.kk",
            "import \"far.kk\"\nconst NEXT = BASE + 1\nconst LS = L:sizeof\n\
             abstract var M[ .l :byte [LS] ]\n[ EV = NEXT * 2 ]\n",
        ),
    ];
    for (usage, expected) in [
        (
            "#if [ NEXT == 4 ]\nfunc f @a16 { unroll NEXT { asl } }\n#endif",
            4,
        ),
        ("func f @a16 { unroll LS { asl } }", 5),
        ("const MS = M:sizeof\nfunc f @a16 { unroll MS { asl } }", 5),
        ("func f @a16 { unroll EV { asl } }", 8),
    ] {
        let result = program(&files, &format!("import \"near.kk\"\n{usage}\n"));
        assert_eq!(asls_in(result, usage), expected, "{usage}");
    }
    // `far.kk` is two imports away, so its names are not this unit's.
    let result = program(
        &files,
        "import \"near.kk\"\nfunc f @a16 { unroll BASE { asl } }\n",
    );
    let codes: Vec<_> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str())
        .collect();
    assert_eq!(codes, ["K816-E0101"], "{:#?}", result.diagnostics);
}

/// `doc/syntax-reference.md`, "Metadata Query Suffixes": the bracket example
/// compiles against the document's `TASKS` and folds to the size it says,
/// and its guard fires when the offset it checks is not the layout's.
#[test]
fn the_documented_bracket_query_folds_the_layout() {
    let document = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("doc/syntax-reference.md"),
    )
    .expect("the document reads");
    let example = document
        .split("```k65\n")
        .filter_map(|block| block.split_once("```").map(|(code, _)| code))
        .find(|code| code.contains("TABLE_BYTES"))
        .expect("the bracket example is in the document");
    // The document's own declaration, from "Symbolic Subscripts".
    let tasks = "var TASKS[ .sp :word .state :byte .signals :byte ] = $4000\n";
    let result = compile(&format!("{tasks}{example}"));
    assert_eq!(result.diagnostics, vec![], "{example}");
    assert!(
        result
            .assembly
            .expect("compiles")
            .contains(".export TABLE_BYTES : abs = $0020"),
        "four bytes a task, eight tasks"
    );
    let moved = example.replace("!= 3", "!= 2");
    assert_ne!(moved, example, "the example checks the offset with `!= 3`");
    assert_eq!(codes(&format!("{tasks}{moved}")), ["K816-E0150"]);
}

/// An `import` inside a region whose condition reads an imported value is
/// decided with that value, and the branch not taken is not a file the
/// build reads.
///
/// Before, the loader preprocessed the unit alone, the condition read a name
/// it had no value for, and the region fell to its `#else`: the build read
/// the wrong file and then reported the right one missing.
#[test]
fn an_import_inside_a_region_is_decided_with_the_imported_value() {
    let files = std::collections::BTreeMap::from([
        ("board.kk", "const BOARD = 2\n"),
        ("board2.kk", "const PINS = 7\n"),
        ("board1.kk", "const PINS = 1\n"),
    ]);
    let mut map = SourceMap::new();
    let main = map.add_file(
        "main.kk",
        "import \"board.kk\"\n\
         #if [ BOARD == 2 ]\nimport \"board2.kk\"\n#else\nimport \"board1.kk\"\n#endif\n\
         func f @a16 { unroll PINS { asl } }\n",
    );
    let mut read = |path: &std::path::Path| {
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        files
            .get(name)
            .map(|text| (*text).to_owned())
            .ok_or_else(|| std::io::Error::from(std::io::ErrorKind::NotFound))
    };
    k816::load_imports(&mut map, &mut read);
    let names: Vec<_> = map.files().map(|file| file.name().to_owned()).collect();
    assert_eq!(names, ["main.kk", "board.kk", "board2.kk"]);
    let result = compile_to_ca65(&map, main, &Options::default());
    assert_eq!(asls_in(result, "the board the import chose"), 7);
}

/// An `evalfunc` this unit declares wins over an imported one of the same
/// name and count, as a layout does: neither emits anything, so neither is a
/// definition two units can clash over.
#[test]
fn a_local_evalfunc_wins_over_an_imported_one() {
    let result = program(
        &[("defs.kk", "evalfunc scale(q) [ q * 2 ]\n")],
        "import \"defs.kk\"\nevalfunc scale(q) [ q * 3 ]\nfunc f @a16 { unroll [scale(2)] { asl } }\n",
    );
    assert_eq!(asls_in(result, "the local `scale`"), 6);
}

/// A query inside a bracket answers what the same query outside one does,
/// an element index included — written as a number or as an evaluator name,
/// which is the one thing only the bracket can spell.
#[test]
fn a_query_inside_a_bracket_is_the_query_outside_it() {
    let layout = "var T[ .a :word .b :byte ] * 4\n[ IX = 2 ]\n";
    for (inside, outside) in [
        ("[T:sizeof]", "T:sizeof"),
        ("[T.b:offsetof]", "T.b:offsetof"),
        ("[T[2].b:offsetof]", "T[2].b:offsetof"),
        ("[T[IX].b:offsetof]", "T[2].b:offsetof"),
        ("[T[3]:sizeof]", "T[3]:sizeof"),
    ] {
        let text = |query: &str| {
            format!("{layout}const Q = {query}\nfunc f @a16 {{ unroll Q {{ asl }} }}\n")
        };
        assert_eq!(asls(&text(inside)), asls(&text(outside)), "{inside}");
    }
    assert_eq!(
        asls(&format!(
            "{layout}func f @a16 {{ unroll [T[IX].b:offsetof] {{ asl }} }}\n"
        )),
        8
    );
}

/// A layout this pass cannot shape is refused by its own name too, not only
/// through a `const` folded from it.
#[test]
fn a_layout_this_pass_cannot_shape_is_refused_where_it_is_read() {
    let result = compile(
        "data tab { 4 30 6 }\n\
         abstract var T[ .a :word .b :byte [[index(tab, 1)]] ]\n\
         func f @a16 { unroll T:sizeof { asl } }\n",
    );
    let titles: Vec<_> = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.title.as_str())
        .collect();
    assert_eq!(
        titles,
        ["`T` has no value where `#if` and `unroll` are decided"]
    );
}
