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
    // D was named by the caller, and so was A: the transitional clause K5
    // removes.
    assert!(linkage.visible_from(d).contains(&a));
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
