/*****************************************************************************/
/*                                                                           */
/*                                modules.rs                                 */
/*                                                                           */
/*        Which units a program has, and which of them each one sees         */
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

//! Sub-plan K's module system: `import`, and the visibility relation it
//! creates.
//!
//! The claim under all of this is that **a unit means one thing**. Before
//! `import`, a program was whatever files one command line named and every
//! one of them saw every other, so what a unit meant depended on the company
//! it was compiled in. A unit that names its own imports resolves the same
//! names whether it is the file being emitted or a file something else
//! reached, and that is checkable rather than merely intended — which is what
//! the corpus-wide test at the bottom of this file does.
//!
//! Tests that need files on disk build them under a scratch directory; the
//! rest build a `SourceMap` in memory, where a file's identity is the
//! lexical normalisation of its name.

use k816::abi::EntryEnvironment;
use k816::linkage::Linkage;
use k816::source::{FileId, SourceMap};

/// The diagnostic codes `map` produces while resolving, in order.
fn resolution_codes(map: &SourceMap) -> Vec<&'static str> {
    Linkage::resolve(map, EntryEnvironment::default())
        .diagnostics()
        .iter()
        .map(|diagnostic| diagnostic.code.0)
        .collect()
}

/// Every diagnostic code compiling `file` out of `map` produces.
fn codes_for(map: &SourceMap, file: FileId) -> Vec<String> {
    k816::compile_to_ca65(map, file, &k816::Options::default())
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.0.to_owned())
        .collect()
}

// ---------------------------------------------------------------------------
// Resolution
// ---------------------------------------------------------------------------

/// An `import` naming a file the build does not hold is an error at the
/// string.
///
/// Reported while the workspace is resolved rather than by whatever opened
/// the files, so that the CLI, the editor and a test that builds a map in
/// memory all say it the same way. The loader adds nothing where it cannot
/// read; an edge reaching no unit is what everyone then sees.
#[test]
fn an_import_that_cannot_be_read_is_e0166_at_the_string() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "main.k816",
        "import \"nowhere.k816\"\n\nfunc main @a8 @i8 {\n    return\n}\n",
    );
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());
    let reported: Vec<&k816::diagnostic::Diagnostic> = linkage.diagnostics().iter().collect();
    assert_eq!(reported.len(), 1, "{reported:?}");
    assert_eq!(reported[0].code.0, "K816-E0166");
    assert_eq!(
        map.span_text(reported[0].primary.span),
        "\"nowhere.k816\"",
        "the caret is the path, not the line"
    );
    assert!(
        reported[0]
            .notes
            .iter()
            .any(|note| note.contains("nowhere.k816") && note.contains("main.k816")),
        "the note says what was looked for and what it was relative to: {:?}",
        reported[0].notes
    );

    // And it reaches a compile, not only a resolve.
    assert!(codes_for(&map, file).contains(&"K816-E0166".to_owned()));
}

/// One file named twice is one unit.
///
/// Before `import` a unit had one name and there was no way to write a second
/// one; now an entry may import a sibling that imports it back, and every
/// path has as many spellings as it has `.` and `..` components. Loading a
/// file twice would give one program two copies of everything it declares,
/// reported — absurdly — as a name defined by more than one unit.
#[test]
fn two_spellings_of_one_file_are_one_unit() {
    let mut map = SourceMap::new();
    let lib = map.add_file("dir/lib.k816", "func helper @a8 @i8 {\n    return\n}\n");
    let main = map.add_file(
        "dir/main.k816",
        "import \"lib.k816\"\n\
         import \"./lib.k816\"\n\
         import \"../dir/lib.k816\"\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   call helper\n\
         }\n",
    );

    // Three spellings, one target, and no complaint about any of them.
    assert_eq!(resolution_codes(&map), Vec::<&str>::new());
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());
    assert_eq!(
        linkage.visible_from(main),
        [main, lib].into_iter().collect()
    );
    assert!(
        !codes_for(&map, main)
            .iter()
            .any(|code| code == "K816-E0102")
    );
}

/// An import cycle terminates, and both units see each other.
#[test]
fn a_cycle_of_imports_terminates() {
    let mut map = SourceMap::new();
    let a = map.add_file(
        "a.k816",
        "import \"b.k816\"\n\nfunc entry @a8 @i8 {\n    call other\n}\n",
    );
    let b = map.add_file(
        "b.k816",
        "import \"a.k816\"\n\nfunc other @a8 @i8 {\n    return\n}\n",
    );
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());
    assert_eq!(linkage.visible_from(a), [a, b].into_iter().collect());
    assert_eq!(linkage.visible_from(b), [a, b].into_iter().collect());
    assert_eq!(resolution_codes(&map), Vec::<&str>::new());
}

/// A unit that imports itself is not a duplicate of itself.
#[test]
fn a_self_import_is_harmless() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "solo.k816",
        "import \"solo.k816\"\n\nfunc main @a8 @i8 {\n    return\n}\n",
    );
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());
    assert_eq!(linkage.visible_from(file), [file].into_iter().collect());
    assert_eq!(resolution_codes(&map), Vec::<&str>::new());
}

// ---------------------------------------------------------------------------
// Visibility
// ---------------------------------------------------------------------------

/// A three-unit chain: A imports B, B imports C.
fn chain() -> (SourceMap, FileId, FileId, FileId) {
    let mut map = SourceMap::new();
    // Added imported rather than named, because a named file sees every
    // other named file until sub-plan K's task K5 removes `--unit`, and this
    // is a test about what `import` decides.
    let c = map.add_imported_file(
        "c.k816",
        std::path::Path::new("c.k816"),
        "func deep @a8 @i8 {\n    return\n}\n",
    );
    let b = map.add_imported_file(
        "b.k816",
        std::path::Path::new("b.k816"),
        "import \"c.k816\"\n\nfunc middle @a8 @i8 {\n    call deep\n}\n",
    );
    let a = map.add_file(
        "a.k816",
        "import \"b.k816\"\n\nfunc main @a8 @i8 {\n    call middle\n}\n",
    );
    (map, a, b, c)
}

/// The whole visibility rule, over a chain and a file nobody imports.
#[test]
fn visibility_is_the_unit_and_what_it_imports_directly() {
    let (mut map, a, b, c) = chain();
    let d = map.add_file("d.k816", "func alone @a8 @i8 {\n    return\n}\n");
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());

    // A sees itself and B — not C, which it never named.
    assert!(linkage.visible_from(a).contains(&b));
    assert!(
        !linkage.visible_from(a).contains(&c),
        "an import is not transitive"
    );
    // B sees itself and C. Not A: an edge points one way, and a unit that
    // could see whoever imported it would mean different things to different
    // importers.
    assert_eq!(linkage.visible_from(b), [b, c].into_iter().collect());
    // C imports nothing, so it sees only itself.
    assert_eq!(linkage.visible_from(c), [c].into_iter().collect());
    // And D, which the caller handed over beside A and which imports
    // nothing, sees only itself: being compiled together is not a
    // relationship. That is the clause `--unit` used to rest on, and its
    // absence is what makes a unit mean one thing.
    assert_eq!(linkage.visible_from(d), [d].into_iter().collect());
}

/// The closure is loaded and not visible, and both halves matter.
///
/// A name two imports away does not resolve — that is the visibility half.
/// The unit that owns it is still analyzed, because a callee's own callees
/// are what decide its exit contract, and dropping them would leave the
/// contract wrong rather than absent.
#[test]
fn a_transitive_import_is_loaded_but_not_visible() {
    let (map, a, b, c) = chain();
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());

    // Loaded: every unit of the closure was analyzed, so `deep` — which only
    // C defines and only B may see — has a classified contract.
    assert!(
        linkage.contract("deep").is_some(),
        "the closure is analyzed even where it is not visible"
    );
    assert!(linkage.contract("middle").is_some());
    let _ = (a, b, c);

    // Not visible: A calls `middle`, which resolves, and cannot name `deep`.
    let mut reaching = SourceMap::new();
    let deep = reaching.add_imported_file(
        "c.k816",
        std::path::Path::new("c.k816"),
        "func deep @a8 @i8 {\n    return\n}\n",
    );
    let middle = reaching.add_imported_file(
        "b.k816",
        std::path::Path::new("b.k816"),
        "import \"c.k816\"\n\nfunc middle @a8 @i8 {\n    call deep\n}\n",
    );
    let entry = reaching.add_file(
        "a.k816",
        "import \"b.k816\"\n\nfunc main @a8 @i8 {\n    call middle\n    call deep\n}\n",
    );
    let _ = (deep, middle);
    assert_eq!(
        codes_for(&reaching, entry),
        vec!["K816-E0101".to_owned()],
        "`middle` resolves and `deep` does not"
    );
}

/// An imported unit does not see the unit that imported it.
#[test]
fn an_imported_unit_does_not_see_its_importer() {
    let mut map = SourceMap::new();
    let lib = map.add_imported_file(
        "lib.k816",
        std::path::Path::new("lib.k816"),
        "func helper @a8 @i8 {\n    call only_in_main\n}\n",
    );
    map.add_file(
        "main.k816",
        "import \"lib.k816\"\n\nfunc only_in_main @a8 @i8 {\n    return\n}\n",
    );
    assert_eq!(
        codes_for(&map, lib),
        vec!["K816-E0101".to_owned()],
        "an edge points one way"
    );
}

// ---------------------------------------------------------------------------
// The claim the whole design rests on
// ---------------------------------------------------------------------------

/// **A unit means the same thing whichever file is the entry.**
///
/// Over every source in the tree that imports anything, and every file in its
/// closure: compile that file as the entry of a program discovered from
/// itself, and compile it again as a file something else reached, and require
/// identical assembly and identical diagnostics.
///
/// This is what direct-imports-only visibility buys, and it is checkable
/// rather than merely intended. Under whole-map visibility it is false: a
/// name is resolvable in `b.k816` because `a.k816` happened to import
/// `c.k816`, so `b.k816` compiled alone and `b.k816` compiled as part of
/// `a.k816`'s program are two different programs — and the second is the one
/// that ships, since a build compiles each unit as its own entry.
#[test]
fn a_unit_means_the_same_whichever_file_is_the_entry() {
    let mut compared = 0;
    for root in importing_sources() {
        let reached = program_from(&root);
        for name in reached.files().map(|file| file.name().to_owned()) {
            let alone = program_from(std::path::Path::new(&name));
            let (Some(in_context), Some(on_its_own)) = (
                reached.locate(&k816::imports::identity(std::path::Path::new(&name))),
                alone.locate(&k816::imports::identity(std::path::Path::new(&name))),
            ) else {
                panic!("`{name}` is not in one of the two maps");
            };
            let from_context = k816::compile_to_ca65(&reached, in_context, &Default::default());
            let from_itself = k816::compile_to_ca65(&alone, on_its_own, &Default::default());
            assert_eq!(
                codes(&from_context.diagnostics),
                codes(&from_itself.diagnostics),
                "`{name}` reports differently when it is reached through `{}`",
                root.display()
            );
            assert_eq!(
                from_context.assembly,
                from_itself.assembly,
                "`{name}` compiles differently when it is reached through `{}`",
                root.display()
            );
            compared += 1;
        }
    }
    assert!(
        compared >= 5,
        "the sweep found {compared} units; it is meant to walk every closure in the tree"
    );
}

/// A `const` an imported unit declares folds in a unit-level declaration,
/// not only inside an evaluator bracket.
///
/// Foreign names are declared *after* this unit's own items, so that a local
/// declaration always wins its name — which leaves a unit-level initializer
/// resolving at a moment when the foreign name is not in the table yet. The
/// evaluator is seeded with the workspace's `const` values before the walk
/// for exactly this reason, and the fold now asks it after the table misses.
///
/// Before that, `[ BASE * 2 ]` folded and `const D = BASE * 2` was
/// `K816-E0101 BASE is not declared` — two answers to one question, and the
/// second one false: the name is declared, in the file above.
#[test]
fn a_const_an_imported_unit_declares_folds_in_a_declaration() {
    let mut map = SourceMap::new();
    map.add_file("defs.k816", "const BASE = 7\nconst STEP = 2\n");
    let entry = map.add_file(
        "main.k816",
        "import \"defs.k816\"\n\n\
         const DERIVED = BASE + 1\n\
         const CHAINED = DERIVED + STEP\n\n\
         var slot = BASE + $100\n\n\
         func main @a8 @i8 {\n    a = #CHAINED\n}\n",
    );
    let result = k816::compile_to_ca65(&map, entry, &k816::Options::default());
    assert_eq!(result.diagnostics, vec![], "cross-unit `const` chain");
    let assembly = result.assembly.expect("compiles");
    for expected in [
        ".export DERIVED : abs = $0008",
        ".export CHAINED : abs = $000A",
        "lda     #$0A",
        // A `var` address is a compile-time value too, and reaches the same
        // fold by the same route.
        "slot            := $0107",
    ] {
        assert!(assembly.contains(expected), "`{expected}` in:\n{assembly}");
    }
}

/// An `inline` parameter wins its name against an imported `const`.
///
/// A parameter may be spelled anything — it is not in a namespace, which is
/// the exemption the reserved-name rule states — so a body may legitimately
/// bind `#BASE` while another unit exports a `const BASE`. The fold asks the
/// symbol table (and the section's own scope before it) and only then the
/// values imports carried in, so the argument wins and the import is not
/// even consulted.
///
/// This is the case that makes that order load-bearing rather than tidy:
/// reverse it and the expansion substitutes the *other unit's* number, which
/// assembles, links, and is wrong.
#[test]
fn an_inline_parameter_wins_its_name_against_an_imported_const() {
    let mut map = SourceMap::new();
    map.add_file("defs.k816", "const BASE = $70\n");
    let entry = map.add_file(
        "main.k816",
        "import \"defs.k816\"\n\nvar slot = $80\n\n\
         inline mask @a8 (a, #BASE) -> a {\n    a & #BASE\n}\n\n\
         func main @a8 @i8 {\n    a = #$FF\n    mask a, #$0F -> a\n    slot = a\n}\n",
    );
    let result = k816::compile_to_ca65(&map, entry, &k816::Options::default());
    assert_eq!(result.diagnostics, vec![]);
    let assembly = result.assembly.expect("compiles");
    assert!(
        assembly.contains("and     #$0F"),
        "the argument, not the imported `const`'s $70:\n{assembly}"
    );
    assert!(!assembly.contains("and     #$70"), "{assembly}");
}

/// A `const` reads what is declared above it, and a workspace does not change
/// that.
///
/// The foreign table the fold falls back to is seeded from what this unit may
/// *see*, which includes what it defines itself — so seeding it with this
/// unit's own values would let a declaration read one written below it, and
/// only when a workspace had been built at all. It did: `analyze_file` said
/// `K816-E0101` and `compile_to_ca65` folded the same two lines, which is one
/// program with two answers.
///
/// A declaration is answerable from the line after it. That is the order
/// `declare_items` walks in and the order the evaluator answers about a
/// section in, and it is now the order both entry points agree on.
#[test]
fn a_const_reads_only_what_is_declared_above_it() {
    let both = |source: &str| -> (Vec<&'static str>, Vec<String>) {
        let mut map = SourceMap::new();
        let id = map.add_file("order.k816", source);
        let parse = k816::parser::parse_source(map.file(id), None);
        let (_, alone) = k816::semantic::analyze_file(&parse.file);
        (
            alone.iter().map(|d| d.code.0).collect(),
            codes_for(&map, id),
        )
    };

    // Forward: refused, and refused the same way with a workspace as without.
    let (alone, workspace) = both("const LATER = EARLIER + 1\nconst EARLIER = 7\n");
    assert_eq!(alone, vec!["K816-E0101"], "without a workspace");
    assert_eq!(workspace, vec!["K816-E0101".to_owned()], "with one");

    // The same through an evaluator bracket, which reaches the table by the
    // other road and used to fold this to six.
    let (alone, workspace) = both("const VIA = [ LATER * 2 ]\nconst LATER = 3\n");
    assert!(!alone.is_empty(), "without a workspace");
    assert!(!workspace.is_empty(), "with one: {workspace:?}");

    // And in order, both roads still fold.
    let (alone, workspace) = both("const EARLIER = 7\nconst LATER = EARLIER + 1\n");
    assert_eq!(alone, Vec::<&str>::new());
    assert_eq!(workspace, Vec::<String>::new());
    let (alone, workspace) = both("const EARLIER = 3\nconst VIA = [ EARLIER * 2 ]\n");
    assert_eq!(alone, Vec::<&str>::new());
    assert_eq!(workspace, Vec::<String>::new());
}

/// The two spellings of one compile-time question agree.
///
/// A bracket is the evaluator and a declaration is the resolver, and both are
/// asking what a `const` is worth. The pair is written here rather than left
/// to the two tests above, because the failure it guards is precisely that
/// they diverge.
#[test]
fn a_bracket_and_a_declaration_fold_an_imported_const_alike() {
    let mut map = SourceMap::new();
    map.add_file("defs.k816", "const BASE = 7\n");
    let entry = map.add_file(
        "main.k816",
        "import \"defs.k816\"\n\n\
         const BY_DECLARATION = BASE * 2\n\
         const BY_BRACKET = [ BASE * 2 ]\n",
    );
    let result = k816::compile_to_ca65(&map, entry, &k816::Options::default());
    assert_eq!(result.diagnostics, vec![]);
    let assembly = result.assembly.expect("compiles");
    assert!(
        assembly.contains(".export BY_DECLARATION : abs = $000E")
            && assembly.contains(".export BY_BRACKET : abs = $000E"),
        "{assembly}"
    );
}

/// An `abstract var` an imported unit declares is visible, layout and all.
///
/// A layout has no address at all — its offsets are arithmetic settled where
/// they were written — so it is a compile-time fact of the same kind as a
/// `const`'s value, and `import` is how a unit says which of those it may
/// use. What crosses is the *data*: a `LayoutId` indexes one unit's arena, so
/// handing an importer the id would be a dangling index and a panic rather
/// than a diagnostic.
///
/// Before this, `DEV:sizeof` over an import was `K816-E0101 not declared` —
/// about a name declared in the file above — and the help offered
/// `var DEV = $0400`.
#[test]
fn a_layout_an_imported_unit_declares_crosses_with_its_offsets() {
    let mut map = SourceMap::new();
    map.add_file(
        "layout.k816",
        "abstract var DEV[\n    .open  :word\n    .close :word\n\
         \x20   .buf[\n        .lo :byte\n        .hi :byte\n    ]\n]\n",
    );
    let entry = map.add_file(
        "main.k816",
        "import \"layout.k816\"\n\nvar slot = $80\n\n\
         func main @a8 @i8 {\n\
         \x20   a = #DEV:sizeof\n    slot = a\n\
         \x20   a = #DEV.close:offsetof\n    slot = a\n\
         \x20   a = #DEV.buf.hi:offsetof\n    slot = a\n}\n",
    );
    let result = k816::compile_to_ca65(&map, entry, &k816::Options::default());
    assert_eq!(result.diagnostics, vec![], "an imported layout");
    let assembly = result.assembly.expect("compiles");
    // Six bytes in the layout; `.close` at two; `.buf.hi` at four plus one —
    // the nested list crossed with its own offsets, not merely its size.
    for expected in ["lda     #$06", "lda     #$02", "lda     #$05"] {
        assert!(assembly.contains(expected), "`{expected}` in:\n{assembly}");
    }
}

/// Crossing does not make it a place, and does not make it a definition.
///
/// The first half: an address use of an imported layout is `K816-E0136`,
/// which says what is wrong — it has no storage — where `K816-E0101` said the
/// name did not exist. The second: two units may still declare the same
/// layout, because a layout is not a definition and there is nothing for
/// ld65 to resolve twice. A local declaration wins its own name.
#[test]
fn an_imported_layout_is_not_a_place_and_not_a_definition() {
    let mut map = SourceMap::new();
    map.add_file("layout.k816", "abstract var DEV[ .open :word ]\n");
    let entry = map.add_file(
        "main.k816",
        "import \"layout.k816\"\n\nfunc main @a8 @i8 {\n    lda DEV\n}\n",
    );
    assert_eq!(
        codes_for(&map, entry),
        vec!["K816-E0136".to_owned()],
        "an address use of an imported layout"
    );

    let mut map = SourceMap::new();
    map.add_file("a.k816", "abstract var DEV[ .open :word ]\n");
    let entry = map.add_file(
        "main.k816",
        "abstract var DEV[ .open :word  .close :word ]\n\
         import \"a.k816\"\n\nvar slot = $80\n\n\
         func main @a8 @i8 {\n    a = #DEV:sizeof\n    slot = a\n}\n",
    );
    let result = k816::compile_to_ca65(&map, entry, &k816::Options::default());
    assert_eq!(result.diagnostics, vec![], "two units, one layout name");
    // Four, not two: the unit's own declaration wins its name.
    assert!(
        result.assembly.expect("compiles").contains("lda     #$04"),
        "the local layout has to win its own name"
    );
}

/// A `const` another unit *defines* leaves no `.import`, exactly as a local
/// one does.
///
/// Its value is a compile-time fact, so every use folded and the name does
/// not survive into the object at all — ca65 drops the unreferenced import,
/// which is why this was quiet. The line was still a false statement in a
/// file people read: "defined by defs.k816:1", about a name the assembly no
/// longer mentions. An `extern const` is the other kind and still imports.
#[test]
fn a_const_that_folded_across_an_import_is_not_imported() {
    let mut map = SourceMap::new();
    map.add_file("defs.k816", "const BASE = 7\n");
    let entry = map.add_file(
        "main.k816",
        "import \"defs.k816\"\n\nfunc main @a8 @i8 {\n    a = #BASE\n}\n",
    );
    let assembly = k816::compile_to_ca65(&map, entry, &k816::Options::default())
        .assembly
        .expect("compiles");
    assert!(assembly.contains("lda     #$07"), "{assembly}");
    assert!(!assembly.contains(".import BASE"), "{assembly}");
}

/// Only the entry file is emitted: an imported unit contributes names, not
/// bytes.
#[test]
fn only_the_entry_is_emitted() {
    let golden = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let map = program_from(&golden.join("import-main.k816"));
    let entry = map
        .locate(&k816::imports::identity(&golden.join("import-main.k816")))
        .expect("the entry");
    let assembly = k816::compile_to_ca65(&map, entry, &Default::default())
        .assembly
        .expect("compiles");
    // `widen` and `plain` are the sibling's sections, and `cursor` its
    // storage. The entry imports all three and defines none of them.
    for foreign in ["widen:", "plain:", ".res"] {
        assert!(
            !assembly.contains(foreign),
            "the sibling's `{foreign}` was emitted into the entry:\n{assembly}"
        );
    }
    assert!(assembly.contains(".import widen"), "{assembly}");
    assert!(assembly.contains(".importzp cursor"), "{assembly}");
}

/// Every `.k816`/`.k65` in the tree with an `import` item, as a path.
fn importing_sources() -> Vec<std::path::PathBuf> {
    fn walk(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, out);
                continue;
            }
            let extension = path.extension().and_then(|text| text.to_str());
            if !matches!(extension, Some("k816" | "k65")) {
                continue;
            }
            // Textual, on purpose: this walk decides what the *test* looks
            // at, and parsing every source in the tree to choose would make
            // the sweep depend on the thing it is checking.
            if std::fs::read_to_string(&path).is_ok_and(|text| {
                text.lines()
                    .any(|line| line.trim_start().starts_with("import "))
            }) {
                out.push(path);
            }
        }
    }
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut found = Vec::new();
    for area in ["tests", "examples"] {
        walk(&root.join(area), &mut found);
    }
    found.sort();
    found
}

/// The program `entry` describes: itself, and everything it imports.
fn program_from(entry: &std::path::Path) -> SourceMap {
    let mut map = SourceMap::new();
    let text = std::fs::read_to_string(entry).expect("read the entry");
    map.add_file_at(entry.to_string_lossy(), entry, text);
    k816::load_imports(&mut map, &mut |path| std::fs::read_to_string(path));
    map
}

fn codes(diagnostics: &[k816::diagnostic::Diagnostic]) -> Vec<&'static str> {
    diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.0)
        .collect()
}

/// A path resolves against the *importing* file's directory, two levels
/// down.
///
/// `tests/golden/import-nested/` is the fixture for it and was asserted by
/// nothing: `golden_sets` and the doc-coverage scan both read only the top
/// level of `tests/golden`, so a subdirectory escapes the census, the
/// idempotence sweep and the format round-trip. The corpus-wide
/// entry-independence gate did reach it and proves nothing about it — it
/// compares a file against itself, so a resolution rule that joined against
/// the wrong directory would make `glyphs` undeclared *both* ways and the
/// comparison would still hold.
///
/// What is checked here is the thing the fixture is for: `main.k816` imports
/// `sub/lib.k816`, and that file's own `import "deeper/data.k816"` reaches
/// `sub/deeper/data.k816` rather than `deeper/data.k816`.
#[test]
fn a_nested_import_resolves_against_its_own_directory() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden/import-nested");
    let map = program_from(&root.join("main.k816"));

    let names: Vec<String> = map
        .files()
        .map(|file| {
            std::path::Path::new(file.name())
                .strip_prefix(&root)
                .unwrap_or_else(|_| std::path::Path::new(file.name()))
                .to_string_lossy()
                .into_owned()
        })
        .collect();
    assert_eq!(
        names,
        vec!["main.k816", "sub/lib.k816", "sub/deeper/data.k816"],
        "each path resolved against the file that wrote it"
    );

    // And it compiles, which is what says the names in it resolved: `helper`
    // reads `glyphs`, and `glyphs` is two directories from the entry and
    // one from the file that imports it.
    let entry = map
        .locate(&k816::imports::identity(&root.join("main.k816")))
        .expect("the entry");
    assert_eq!(codes_for(&map, entry), Vec::<String>::new());
    let middle = map
        .locate(&k816::imports::identity(&root.join("sub/lib.k816")))
        .expect("the middle unit");
    assert_eq!(
        codes_for(&map, middle),
        Vec::<String>::new(),
        "`glyphs` resolved from `sub/lib.k816`, which named it `deeper/data.k816`"
    );
}
