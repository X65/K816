/*****************************************************************************/
/*                                                                           */
/*                                manifest.rs                                */
/*                                                                           */
/*        Feature-manifest audit: every syntax group has a corpus case       */
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

//! The coverage audit for the complete grammar.
//!
//! Every syntax group in `plans/feature-manifest.md` §2 is mapped here to the
//! Tree-sitter corpus case that proves the grammar parses it. The mapping is a
//! checked artifact rather than a prose checklist: a case that is renamed or
//! deleted fails this test, so the claim "every documented form is recognized
//! or explicitly reserved" cannot quietly stop being true.
//!
//! The manifest itself is not read at run time — `plans/` is deliberately not
//! committed — so the group names are transcribed here and the *cases* are
//! what get verified.

use std::collections::HashSet;
use std::path::PathBuf;

/// One manifest §2 row and the corpus case that covers its syntax.
const AUDIT: &[(&str, &str)] = &[
    ("Line and block comments", "Comment edge cases"),
    ("Identifiers", "Identifiers use the full name namespace"),
    ("Numeric literals, binary", "Binary literal forms"),
    ("Numeric literals, hexadecimal", "Hexadecimal literal forms"),
    ("Character literals", "Character literal escapes"),
    (
        "Float literals in evaluator brackets",
        "The full evaluator operator table",
    ),
    (
        "Expression operators, binary",
        "Every binary operator token",
    ),
    (
        "Expression operators, unary",
        "Unary operators repeat in the order written",
    ),
    (
        "Compound-expression position rules",
        "Raw values and address byte operators",
    ),
    (
        "`&&` address-of-vs-AND parse quirk",
        "Infix double ampersand is the address-of gotcha",
    ),
    (
        "`const` and enum-like groups",
        "Constant groups with enum-like auto-increment",
    ),
    (
        "Fixed-address `var NAME = $addr`",
        "Fixed-address var declaration",
    ),
    (
        "Allocated `var NAME`",
        "Linker-allocated variables have no initializer",
    ),
    ("Storage-class prefixes", "Storage class prefixes"),
    ("Typed width suffixes", "Typed width suffixes"),
    (
        "Array declaration `var NAME[N]`",
        "Array spans and allocation counts",
    ),
    (
        "Allocation count `* count`",
        "Array spans and allocation counts",
    ),
    (
        "Repeat element accessors",
        "Field access and repeat element accessors",
    ),
    (
        "Symbolic subscript field lists",
        "Symbolic subscript field list",
    ),
    (
        "Symbolic subscript access",
        "Field access and repeat element accessors",
    ),
    ("`abstract var` layouts", "Abstract layout declarations"),
    ("Metadata query suffixes", "Metadata query suffixes"),
    (
        "Address-encoding operand prefixes",
        "Address-encoding operand prefixes",
    ),
    ("`segment NAME` selection", "Segment selection"),
    ("Global labels", "Global and local labels"),
    ("Local labels", "Global and local labels"),
    (
        "Label+offset assignment targets",
        "Label plus offset assignment targets",
    ),
    ("`func NAME { }`", "Every code section form"),
    (
        "Width annotations on functions",
        "Empty function with width annotations",
    ),
    ("Module-level default widths", "Module-level default widths"),
    ("Scoped width blocks", "Scoped width blocks"),
    (
        "Function call contracts",
        "Call contracts on a section header",
    ),
    ("`naked` functions", "Every code section form"),
    ("`inline` functions", "Call contracts on a section header"),
    ("`far func`, `far` call", "Every code section form"),
    ("`call far` cross-unit calls", "Call statements"),
    ("HLA register loads", "Loads in every operand shape"),
    ("HLA stores", "Stores in every operand shape"),
    ("Assignment chaining", "Assignment chaining"),
    (
        "Register-to-register transfers",
        "Register to register transfers",
    ),
    ("Zero-store shortcut", "Stores in every operand shape"),
    (
        "HLA arithmetic and compares",
        "Arithmetic and logic shorthands",
    ),
    ("HLA bit test", "Compares and the bit test"),
    ("HLA shifts and rotates", "Shifts and rotates"),
    ("HLA increment and decrement", "Increment and decrement"),
    ("Flag shorthand ops", "Flag shorthands"),
    ("Stack shorthands", "Stack shorthands"),
    ("`b~a` / `a~b` XBA", "Accumulator exchange"),
    ("`return` / `return_i`", "Call statements"),
    ("Branch operator mapping", "Every branch condition spelling"),
    ("Conditional prefix form", "Prefix conditional form"),
    (
        "Conditional goto form",
        "Conditional goto, break, and repeat",
    ),
    ("Postfix loop form", "Postfix loop form"),
    ("`always` / `never` loops", "Postfix loop form"),
    ("`break` / `repeat`", "Conditional goto, break, and repeat"),
    ("`else`", "If-else"),
    ("`goto` and `call`", "Flow control instructions"),
    ("`nocross` code wrappers", "nocross code wrappers"),
    ("Wait-loop patterns", "A wait loop"),
    (
        "Efficiency warnings for `>` / `<=`",
        "Every branch condition spelling",
    ),
    ("NOP shorthand", "NOP and break shorthands"),
    (
        "`%` BRK shorthand",
        "A binary literal is not the break shorthand",
    ),
    (
        "Raw 6502 mnemonics",
        "Every classical mnemonic is recognized",
    ),
    ("Classical operand shapes", "Classical operand shapes"),
    ("65816 operand additions", "Stack-relative operands"),
    ("65816 indirect long operands", "Indirect long operands"),
    ("65816 indexed-offset marker", "The indexed-offset marker"),
    ("65C02 additions", "65C02 additions"),
    (
        "65816 bank/mode mnemonics",
        "65816 bank registers and mode switch",
    ),
    (
        "65816 control flow/mode/stack",
        "65816 control flow, mode and stack",
    ),
    ("65816 transfer mnemonics", "65816 transfer instructions"),
    ("Block moves", "Block moves take two operands"),
    (
        "Data blocks, named and anonymous",
        "Named and anonymous data blocks",
    ),
    ("`segment` placement rule in data", "Placement directives"),
    ("Placement directives", "Placement directives"),
    ("`nocross` in data blocks", "Placement directives"),
    (
        "Raw bytes and labels in data",
        "Raw values and address byte operators",
    ),
    (
        "Address byte operators",
        "Raw values and address byte operators",
    ),
    ("`?` undefined byte placeholder", "Typed entries"),
    ("Typed data entries", "Typed entries"),
    (
        "String data and `charset`",
        "charset, code subblocks, and repeats",
    ),
    (
        "`code { }` subblocks",
        "charset, code subblocks, and repeats",
    ),
    (
        "`repeat N { }` data repetition",
        "charset, code subblocks, and repeats",
    ),
    (
        "`for ... eval` generated data",
        "Generated data from an evaluator range",
    ),
    (
        "`image` in data blocks",
        "Reserved image forms are named nodes, never parse errors",
    ),
    ("`binary` in data blocks", "Binary inclusion"),
    (
        "Top-level `image` and `binary`",
        "Reserved image forms are named nodes, never parse errors",
    ),
    (
        "Evaluator brackets",
        "Evaluator brackets are expression atoms",
    ),
    (
        "Top-level evaluator blocks",
        "Generated data from an evaluator range",
    ),
    (
        "Evaluator operator set",
        "The full evaluator operator table",
    ),
    (
        "Evaluator math functions",
        "The full evaluator operator table",
    ),
    (
        "Evaluator side-effect functions",
        "Reserved evaluator functions",
    ),
    (
        "Preprocessor directives",
        "Preprocessor directives at the top level",
    ),
    (
        "Optional semicolons and empty statements",
        "Semicolons are optional separators and empty statements are ignored",
    ),
    (
        "Case-insensitive registers and flags",
        "Registers are case-insensitive",
    ),
    ("Variable+offset addressing", "Compound operand expressions"),
];

/// Every case name in the corpus, read from the same files the corpus runner
/// executes.
fn corpus_case_names() -> HashSet<String> {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tree-sitter-k65/test/corpus");
    let mut names = HashSet::new();
    for entry in std::fs::read_dir(&dir).expect("read corpus directory") {
        let path = entry.expect("corpus entry").path();
        if path.extension().is_none_or(|extension| extension != "txt") {
            continue;
        }
        let text = std::fs::read_to_string(&path).expect("read corpus file");
        let mut lines = text.lines().peekable();
        while let Some(line) = lines.next() {
            let is_rule = line.len() >= 3 && line.chars().all(|c| c == '=');
            if !is_rule {
                continue;
            }
            // The line after a `===` rule is the case name, unless we are
            // looking at the rule that closes a header.
            if let Some(name) = lines.peek()
                && !name.trim().is_empty()
                && !name.starts_with(':')
            {
                names.insert(name.trim().to_owned());
            }
        }
    }
    names
}

#[test]
fn every_manifest_syntax_group_has_a_corpus_case() {
    let names = corpus_case_names();
    let mut missing = Vec::new();
    for (group, case) in AUDIT {
        if !names.contains(*case) {
            missing.push(format!("  {group}\n    expected corpus case: `{case}`"));
        }
    }
    assert!(
        missing.is_empty(),
        "feature-manifest groups whose corpus case is missing or renamed:\n{}",
        missing.join("\n")
    );
}
