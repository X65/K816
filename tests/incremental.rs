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
//! The scenarios themselves live in `tests/common/scenarios.rs`, because
//! `tests/lsp.rs` runs the same list through the server's edit path
//! (`plans/07-lsp-tooling.md` H2). They run over the slice program,
//! `examples/hello_uart.k65`, and a control-flow-heavy fixture, and cover
//! every class the sub-plan lists: typing a statement, deleting and restoring
//! a brace, inserting an item, editing inside a comment, a string, and a data
//! block, editing at EOF, an edit that flips a construct between two rules,
//! and lines holding two- and four-byte characters.

use k816::parser::{SourceEdit, SyntaxTree, parse_source};
use k816::source::{FileId, SourceFile, SourceMap, Span};

#[path = "common/scenarios.rs"]
mod scenarios;

use scenarios::{Edit, Scenario};

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

/// The scenario of that name, which must exist.
fn scenario(name: &str) -> Scenario {
    scenarios::scenarios()
        .into_iter()
        .find(|scenario| scenario.name == name)
        .unwrap_or_else(|| panic!("no scenario named `{name}`"))
}

/// Applies a scenario's edits one at a time, asserting after each that the
/// incremental reparse is indistinguishable from a clean one.
fn assert_incremental_equals_clean(scenario: Scenario) {
    run(scenario.name, &scenario.source, scenario.edits);
}

fn run(scenario: &str, source: &str, edits: Vec<Edit>) {
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
    assert_incremental_equals_clean(scenario("typing a statement"));
}

#[test]
fn deleting_and_restoring_a_closing_brace() {
    assert_incremental_equals_clean(scenario("delete and restore `}`"));
}

#[test]
fn inserting_an_item_between_two_items() {
    assert_incremental_equals_clean(scenario("insert an item"));
}

#[test]
fn editing_inside_a_comment() {
    assert_incremental_equals_clean(scenario("edit inside a comment"));
}

#[test]
fn editing_at_end_of_file() {
    assert_incremental_equals_clean(scenario("edit at EOF"));
}

#[test]
fn editing_inside_a_string() {
    assert_incremental_equals_clean(scenario("edit inside a string"));
}

#[test]
fn editing_inside_a_data_block() {
    assert_incremental_equals_clean(scenario("edit inside a data block"));
}

#[test]
fn an_edit_that_flips_a_construct_between_two_rules() {
    assert_incremental_equals_clean(scenario("flip loop and conditional"));
}

#[test]
fn typing_a_statement_into_the_stress_test_fixture() {
    assert_incremental_equals_clean(scenario("typing into hello_uart"));
}

#[test]
fn editing_the_stress_test_fixture_across_its_shapes() {
    assert_incremental_equals_clean(scenario("edits across hello_uart"));
}

#[test]
fn an_edit_that_breaks_and_repairs_the_file() {
    assert_incremental_equals_clean(scenario("break and repair"));
}

/// The multi-byte scenarios, which the parser has to survive for the same
/// reason as any other and the server has to survive for one more: its edits
/// arrive as UTF-16 ranges.
#[test]
fn editing_lines_holding_multi_byte_characters() {
    for name in [
        "typing after a two-byte character",
        "typing around a four-byte character",
        "editing a string with multi-byte characters",
    ] {
        assert_incremental_equals_clean(scenario(name));
    }
}

/// Every scenario is reached by a test above, so a scenario added for the
/// server's sake is run against the parser too.
#[test]
fn every_scenario_is_covered_here() {
    const NAMED: &[&str] = &[
        "typing a statement",
        "delete and restore `}`",
        "insert an item",
        "edit inside a comment",
        "edit at EOF",
        "edit inside a string",
        "edit inside a data block",
        "flip loop and conditional",
        "typing into hello_uart",
        "edits across hello_uart",
        "break and repair",
        "typing after a two-byte character",
        "typing around a four-byte character",
        "editing a string with multi-byte characters",
    ];
    let names: Vec<&str> = scenarios::scenarios()
        .iter()
        .map(|scenario| scenario.name)
        .collect();
    assert_eq!(names, NAMED);
}
