/*****************************************************************************/
/*                                                                           */
/*                                scenarios.rs                               */
/*                                                                           */
/*         The edit scenarios an incremental reparse has to survive          */
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

//! The edit scenarios, in one place, because two suites run them.
//!
//! `tests/incremental.rs` runs them against the *parser*: an edited reparse
//! has to equal a clean parse on the CST, the AST and the diagnostics.
//! `tests/lsp.rs` runs the same list against the *server's edit path*, where
//! the same edit arrives as a UTF-16 range in a `didChange` and has to reach
//! the same tree. Two lists would be two claims, and the weaker one would be
//! the one that quietly stopped covering anything.

/// One edit, addressed by text rather than by byte offset: the first
/// occurrence of `find` becomes `replace`.
#[derive(Clone, Debug)]
pub struct Edit {
    pub find: String,
    pub replace: String,
}

pub fn edit(find: &str, replace: &str) -> Edit {
    Edit {
        find: find.to_owned(),
        replace: replace.to_owned(),
    }
}

/// Types `text` one character at a time immediately before `anchor` — the
/// mid-typing scenario that matters most for an editor.
pub fn typing(anchor: &str, text: &str) -> Vec<Edit> {
    text.chars()
        .map(|character| edit(anchor, &format!("{character}{anchor}")))
        .collect()
}

/// One named scenario: a starting program and the edits applied to it, in
/// order, each against the text the previous ones left.
pub struct Scenario {
    pub name: &'static str,
    pub source: String,
    pub edits: Vec<Edit>,
}

pub const SLICE: &str = "var SCREEN = $0400

func work @a8 @i16 {
    a = #1
    SCREEN = a
    return
}
";

pub const CONTROL_FLOW: &str = "func main @a8 @i8 {
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

/// The stress-test fixture, which exercises data blocks, a wait loop, an
/// assignment chain, and an evaluator bracket at once.
pub fn hello_uart() -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/hello_uart.k65");
    std::fs::read_to_string(path).expect("read the stress-test fixture")
}

/// Every scenario, covering each class the sub-plan lists: typing a
/// statement, deleting and restoring a brace, inserting an item, editing
/// inside a comment, a string and a data block, editing at EOF, an edit that
/// flips a construct between two rules, and one that breaks and repairs the
/// file.
///
/// The multi-byte ones are last and are H2's addition: the UTF-16 conversion
/// the server does has no analogue in the parser test, and a byte-offset
/// mistake on a line holding `ż` corrupts the document *silently* — the
/// server simply ends up holding different text than the editor.
pub fn scenarios() -> Vec<Scenario> {
    let scenario = |name, source: &str, edits| Scenario {
        name,
        source: source.to_owned(),
        edits,
    };
    vec![
        scenario(
            "typing a statement",
            SLICE,
            typing("    return", "a = #2\n    "),
        ),
        scenario(
            "delete and restore `}`",
            SLICE,
            vec![edit("}\n", "\n"), edit("\n\n", "\n}\n")],
        ),
        scenario(
            "insert an item",
            SLICE,
            vec![edit("\nfunc work", "\nvar OTHER = $0500\n\nfunc work")],
        ),
        scenario(
            "edit inside a comment",
            "// leading note\nvar SCREEN = $0400 // trailing note\n",
            vec![
                edit("leading", "LEADING"),
                edit("trailing note", "trailing note; var XX = 1"),
                edit("// LEADING", "/* LEADING */ //"),
            ],
        ),
        scenario(
            "edit at EOF",
            SLICE,
            vec![
                edit("return\n}\n", "return\n}\n\nvar TAIL = $0500\n"),
                edit("var TAIL = $0500\n", "var TAIL = $0500"),
            ],
        ),
        scenario(
            "edit inside a string",
            CONTROL_FLOW,
            vec![
                edit("Hello", "Hello, World!"),
                edit("Hello, World!", "Hello, World!\\n"),
                // Closing the string is what makes this class interesting:
                // the edit changes where the string ends, not only its
                // contents.
                edit("\"Hello, World!\\n\"", "\"Hello, World!\\n"),
            ],
        ),
        scenario(
            "edit inside a data block",
            CONTROL_FLOW,
            vec![
                edit("$0D $0A $00", "$0D $0A"),
                edit("$0D $0A", "$0D $0A $00 $FF"),
                edit("data text {", "data text {\n    align 256"),
            ],
        ),
        // `{ ... } !=` is a loop and `!= { ... }` is a conditional, so moving
        // the condition across the brace changes which rule matched — the
        // hardest case for tree reuse, because the reused subtree is the
        // block itself.
        scenario(
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
        ),
        scenario(
            "typing into hello_uart",
            &hello_uart(),
            typing("    x++", "a?$FF\n    "),
        ),
        scenario(
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
        ),
        // Diagnostics have to agree too, not just the trees.
        scenario(
            "break and repair",
            SLICE,
            vec![
                edit("a = #1", "a = ="),
                edit("a = =", "a = #1"),
                edit("func work", "func"),
                edit("func @a8", "func work @a8"),
            ],
        ),
        // Typing *after* a two-byte character: every position on the line is
        // one UTF-16 unit short of its byte offset from here on.
        scenario(
            "typing after a two-byte character",
            "// żółć note\nvar SCREEN = $0400\n",
            typing(" note", "!"),
        ),
        // Typing *inside* a four-byte character's line, before and after it:
        // `𝕏` is two UTF-16 units and four bytes, so a conversion that
        // counted either one twice lands in the wrong place.
        scenario(
            "typing around a four-byte character",
            "// 𝕏 marks it\nvar SCREEN = $0400\n",
            vec![
                edit("// ", "// prefix "),
                edit(" marks", "𝕏 marks"),
                edit("it\n", "it 𝕏\n"),
                edit("𝕏𝕏", "𝕏"),
            ],
        ),
        // A multi-byte character inside a string, where the edit changes the
        // string's own length in bytes and in UTF-16 units differently.
        scenario(
            "editing a string with multi-byte characters",
            "data greeting {\n    \"Cześć\"\n    $00\n}\n",
            vec![
                edit("Cześć", "Cześć, świecie"),
                edit("świecie", "𝕏"),
                edit("Cześć, 𝕏", "hi"),
            ],
        ),
    ]
}
