/*****************************************************************************/
/*                                                                           */
/*                              incremental.rs                               */
/*                                                                           */
/*          Incremental reparse equals clean reparse, on three axes          */
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

//! The phase-3 architectural gate: for every edit scenario, reparsing the
//! edited tree must equal parsing the new text from scratch — on the CST dump,
//! on the owned `AstFile`, and on the diagnostics.
//!
//! The scenarios run over the slice program, `examples/hello_uart.k65`, and a
//! control-flow-heavy fixture, and cover every class the sub-plan lists:
//! typing a statement, deleting and restoring a brace, inserting an item,
//! editing inside a comment, a string, and a data block, editing at EOF, and
//! an edit that flips a construct between two rules.

use k816::parser::{SourceEdit, SyntaxTree, parse_source};
use k816::source::{FileId, SourceFile, SourceMap, Span};

const SLICE: &str = "var SCREEN = $0400

func work @a8 @i16 {
    a = #1
    SCREEN = a
    return
}
";

/// One source revision. Each revision owns its map, so a file keeps the same
/// [`FileId`] across revisions and spans stay directly comparable.
struct Revision {
    map: SourceMap,
    id: FileId,
}

impl Revision {
    fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let id = map.add_file("scenario.k816", text);
        Self { map, id }
    }

    fn file(&self) -> &SourceFile {
        self.map.file(self.id)
    }
}

/// One edit, addressed by text rather than by byte offset: the first
/// occurrence of `find` becomes `replace`.
#[derive(Clone, Debug)]
struct Edit {
    find: String,
    replace: String,
}

fn edit(find: &str, replace: &str) -> Edit {
    Edit {
        find: find.to_owned(),
        replace: replace.to_owned(),
    }
}

/// Types `text` one character at a time immediately before `anchor` — the
/// mid-typing scenario that matters most for an editor.
fn typing(anchor: &str, text: &str) -> Vec<Edit> {
    text.chars()
        .map(|character| edit(anchor, &format!("{character}{anchor}")))
        .collect()
}

/// Applies `edits` one at a time, asserting after each that the incremental
/// reparse is indistinguishable from a clean one.
fn assert_incremental_equals_clean(scenario: &str, source: &str, edits: Vec<Edit>) {
    let mut current = Revision::new(source);
    let mut tree: SyntaxTree = parse_source(current.file(), None).tree;

    for (step, edit) in edits.iter().enumerate() {
        let step = format!("{scenario} (edit {})", step + 1);
        let start = current
            .file()
            .text()
            .find(&edit.find)
            .unwrap_or_else(|| panic!("{step}: `{}` not found in source", edit.find));
        let span = Span::new(current.id, start as u32, (start + edit.find.len()) as u32);
        let source_edit = SourceEdit::new(span, edit.replace.clone());
        let next_text = source_edit.apply(current.file());
        tree.edit(current.file(), &source_edit);

        let next = Revision::new(&next_text);
        let incremental = parse_source(next.file(), Some(&tree));
        let clean = parse_source(next.file(), None);

        assert_eq!(
            incremental.tree.dump(),
            clean.tree.dump(),
            "{step}: CST differs after:\n{next_text}"
        );
        assert_eq!(
            incremental.file, clean.file,
            "{step}: AST differs after:\n{next_text}"
        );
        assert_eq!(
            incremental.diagnostics, clean.diagnostics,
            "{step}: diagnostics differ after:\n{next_text}"
        );

        tree = incremental.tree;
        current = next;
    }
}

#[test]
fn typing_a_statement_inside_a_function() {
    assert_incremental_equals_clean(
        "typing a statement",
        SLICE,
        typing("    return", "a = #2\n    "),
    );
}

#[test]
fn deleting_and_restoring_a_closing_brace() {
    assert_incremental_equals_clean(
        "delete and restore `}`",
        SLICE,
        vec![edit("}\n", "\n"), edit("\n\n", "\n}\n")],
    );
}

#[test]
fn inserting_an_item_between_two_items() {
    assert_incremental_equals_clean(
        "insert an item",
        SLICE,
        vec![edit("\nfunc work", "\nvar OTHER = $0500\n\nfunc work")],
    );
}

#[test]
fn editing_inside_a_comment() {
    let source = "// leading note\nvar SCREEN = $0400 // trailing note\n";
    assert_incremental_equals_clean(
        "edit inside a comment",
        source,
        vec![
            edit("leading", "LEADING"),
            edit("trailing note", "trailing note; var X = 1"),
            edit("// LEADING", "/* LEADING */ //"),
        ],
    );
}

#[test]
fn editing_at_end_of_file() {
    assert_incremental_equals_clean(
        "edit at EOF",
        SLICE,
        vec![
            edit("return\n}\n", "return\n}\n\nvar TAIL = $0500\n"),
            edit("var TAIL = $0500\n", "var TAIL = $0500"),
        ],
    );
}

/// The stress-test fixture, which exercises data blocks, a wait loop, an
/// assignment chain, and an evaluator bracket at once.
fn hello_uart() -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/hello_uart.k65");
    std::fs::read_to_string(path).expect("read the stress-test fixture")
}

const CONTROL_FLOW: &str = "func main @a8 @i8 {
    x = %0
    {
        { a&?READY } n-?
        DATA = a = text,x
        x++
        a?0
    } !=
    >={ x-- } else { y++ }
    < goto done
    { } always
}

data text {
    \"Hello\"
    $0D $0A $00
}
";

#[test]
fn editing_inside_a_string() {
    assert_incremental_equals_clean(
        "edit inside a string",
        CONTROL_FLOW,
        vec![
            edit("Hello", "Hello, World!"),
            edit("Hello, World!", "Hello, World!\\n"),
            // Closing the string is what makes this class interesting: the
            // edit changes where the string ends, not only its contents.
            edit("\"Hello, World!\\n\"", "\"Hello, World!\\n"),
        ],
    );
}

#[test]
fn editing_inside_a_data_block() {
    assert_incremental_equals_clean(
        "edit inside a data block",
        CONTROL_FLOW,
        vec![
            edit("$0D $0A $00", "$0D $0A"),
            edit("$0D $0A", "$0D $0A $00 $FF"),
            edit("data text {", "data text {\n    align 256"),
        ],
    );
}

#[test]
fn an_edit_that_flips_a_construct_between_two_rules() {
    // `{ ... } !=` is a loop and `!= { ... }` is a conditional, so moving the
    // condition across the brace changes which rule matched — the hardest
    // case for tree reuse, because the reused subtree is the block itself.
    assert_incremental_equals_clean(
        "flip loop and conditional",
        CONTROL_FLOW,
        vec![
            edit("    } !=\n", "    }\n"),
            edit(
                "    {\n        { a&?READY }",
                "    != {\n        { a&?READY }",
            ),
            edit("< goto done", "< break"),
            edit("{ } always", "{ } !="),
        ],
    );
}

#[test]
fn typing_a_statement_into_the_stress_test_fixture() {
    assert_incremental_equals_clean(
        "typing into hello_uart",
        &hello_uart(),
        typing("    x++", "a?$FF\n    "),
    );
}

#[test]
fn editing_the_stress_test_fixture_across_its_shapes() {
    assert_incremental_equals_clean(
        "edits across hello_uart",
        &hello_uart(),
        vec![
            edit("$0D $0A $00", "$0D $0A"),
            edit("Hello, World!", "Goodbye"),
            edit("UART example", "UART sample"),
            edit("word 0 0 0 0 0 0 main 0", "word 0 0 0 0 0 0 0 main"),
            edit("x = %0", "x = %1"),
            edit("[$FF]", "[$FE]"),
            edit("func main @a8 @i8 {", "func main @a8 {"),
        ],
    );
}

#[test]
fn an_edit_that_breaks_and_repairs_the_file() {
    // Diagnostics have to agree too, not just the trees.
    assert_incremental_equals_clean(
        "break and repair",
        SLICE,
        vec![
            edit("a = #1", "a = ="),
            edit("a = =", "a = #1"),
            edit("func work", "func"),
            edit("func @a8", "func work @a8"),
        ],
    );
}
