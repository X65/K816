/*****************************************************************************/
/*                                                                           */
/*                                  ca65.rs                                  */
/*                                                                           */
/*          ca65 idioms recognized where the grammar found an error          */
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

//! ca65 idioms, recognized where the grammar found an error.
//!
//! A source being carried over from ca65 by hand fails to parse in a handful
//! of recurring shapes — a `;` comment, a `.byte` directive, `RIA::op`,
//! `#<addr`, a cheap local `@loop` — and the grammar's own account of each
//! is true and useless: "missing `:`", "expected a data entry". The grammar
//! cannot know the line is ca65. This module can, because the shapes are
//! unmistakable in the text, and it says what K816 spells instead.
//!
//! **This is a reading of the source text at a recovery point, not a second
//! grammar.** Nothing here changes what parses. An idiom is reported only
//! where Tree-sitter already produced an `ERROR` or `MISSING` node, and only
//! as a better explanation of that node: the span, the code and the fix are
//! different, the region is the same, and a line that parses — `;` is a
//! statement separator in K816, so `nop ; nop` is two instructions — is
//! never reported.
//!
//! The spellings offered are the ones `doc/ca65-to-k816.md` lists, and that
//! document's gate assembles each pair to the same bytes.

use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::source::{LineCol, SourceFile, Span};

/// Where the grammar stopped: the error's span, and whether it is an
/// `ERROR` region (malformed text) or a `MISSING` token (an inference).
#[derive(Clone, Copy)]
pub(super) struct Recovery {
    pub span: Span,
    /// True for a `MISSING` node, whose span is empty and sits *after* the
    /// text that confused the grammar.
    pub missing: bool,
    /// The kind of the node the recovery is parented to, `source_file` at
    /// the top level.
    pub parent: Option<&'static str>,
}

/// The diagnostic for a ca65 idiom at `recovery`, if the text holds one.
pub(super) fn recognize(recovery: Recovery, source: &SourceFile) -> Option<Diagnostic> {
    let line = Line::at(recovery.span.start, source);
    // Where on the line the grammar stopped. An idiom at or after this
    // column is the one being explained; one before it is the fallback,
    // because a recovery point can land a token or two past its cause.
    let at = (recovery.span.start - line.start) as usize;

    // A `;` before the error on the same line. First, because a comment can
    // hold anything — a directive, a `::`, a `#<` — and none of it is code.
    if let Some(column) = line.semicolon()
        && at >= column
    {
        return Some(comment(&line, column, source));
    }
    if let Some(directive) = line.directive_at(at).or_else(|| line.leading_directive()) {
        return Some(self::directive(&line, directive, source));
    }
    if let Some(column) = line.find("::", at) {
        return Some(scope_operator(&line, column, source));
    }
    if let Some((column, form)) = line.address_byte(at) {
        return Some(address_byte(&line, column, form, source));
    }
    if let Some((column, len)) = line.sizeof_call(at) {
        return Some(sizeof(&line, column, len, source));
    }
    if let Some((column, len)) = line.cheap_local(at) {
        return Some(cheap_local(&line, column, len, source));
    }
    if !recovery.missing
        && recovery.parent.is_none_or(|parent| parent == "source_file")
        && let Some(len) = line.leading_label()
    {
        return Some(code_outside_section(&line, len, source));
    }
    None
}

/// `;` followed by words the grammar read as bare calls, on one line.
///
/// The other way a ca65 comment survives: `lda #1 ; load one` parses. The
/// `;` is K816's statement separator and `load` and `one` are calls to
/// functions of those names, so nothing fails until semantic analysis
/// reports each word as undeclared — three complaints about one comment.
/// Two or more consecutive bare words after a `;`, with nothing else on the
/// line, is prose rather than K816, and is reported once, as a warning with
/// the same fix the syntax-error case gets. One word is not reported:
/// `nop ; inc_x` is a separator and a call, and a one-word comment is still
/// told apart by the undeclared-name diagnostic it earns.
pub(super) fn comments_read_as_calls(
    root: tree_sitter::Node<'_>,
    source: &SourceFile,
) -> Vec<Diagnostic> {
    let mut found = Vec::new();
    let mut stack = vec![root];
    while let Some(node) = stack.pop() {
        let mut cursor = node.walk();
        let children: Vec<tree_sitter::Node<'_>> = node.children(&mut cursor).collect();
        for (index, child) in children.iter().enumerate() {
            if child.is_named() {
                stack.push(*child);
                continue;
            }
            if child.kind() != ";" {
                continue;
            }
            let row = child.start_position().row;
            let same_line = |sibling: &tree_sitter::Node<'_>| sibling.start_position().row == row;
            let words = children[index + 1..]
                .iter()
                .take_while(|sibling| same_line(sibling) && sibling.kind() == "bare_call")
                .count();
            let rest_of_line_is_words = children
                .get(index + 1 + words)
                .is_none_or(|sibling| !same_line(sibling));
            if words >= 2 && rest_of_line_is_words {
                let line = Line::at(child.start_byte() as u32, source);
                let column = child.start_byte() - line.start as usize;
                found.push(comment_read_as_calls(&line, column, words, source));
            }
        }
    }
    found
}

// ---------------------------------------------------------------------------
// The line
// ---------------------------------------------------------------------------

/// The source line a recovery point sits on.
struct Line<'a> {
    text: &'a str,
    /// The byte offset of the line's first character in the file.
    start: u32,
}

impl<'a> Line<'a> {
    fn at(offset: u32, source: &'a SourceFile) -> Self {
        let location = source.line_col(offset);
        let line = location.line;
        Self {
            text: source.line_text(line),
            start: source.offset(LineCol { line, column: 0 }),
        }
    }

    /// The file offset of a column on this line.
    fn offset(&self, column: usize) -> u32 {
        self.start + column as u32
    }

    /// The span of `len` bytes at `column`.
    fn span(&self, column: usize, len: usize, source: &SourceFile) -> Span {
        Span::new(source.id(), self.offset(column), self.offset(column + len))
    }

    /// The span from `column` to the end of the line.
    fn span_to_end(&self, column: usize, source: &SourceFile) -> Span {
        self.span(column, self.text.len() - column, source)
    }

    /// The first `;` outside a string or character literal.
    fn semicolon(&self) -> Option<usize> {
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (column, character) in self.text.char_indices() {
            match quote {
                Some(open) => {
                    if escaped {
                        escaped = false;
                    } else if character == '\\' {
                        escaped = true;
                    } else if character == open {
                        quote = None;
                    }
                }
                None => match character {
                    '"' | '\'' => quote = Some(character),
                    ';' => return Some(column),
                    '/' if self.text[column..].starts_with("//") => return None,
                    _ => {}
                },
            }
        }
        None
    }

    /// The first non-blank column.
    fn indent(&self) -> usize {
        self.text.len() - self.text.trim_start().len()
    }

    /// A `.directive` as the first token on the line.
    fn leading_directive(&self) -> Option<Directive<'a>> {
        self.directive_at(self.indent())
    }

    /// A `.directive` starting exactly at `column`.
    fn directive_at(&self, column: usize) -> Option<Directive<'a>> {
        let rest = self.text.get(column..)?;
        let name = rest.strip_prefix('.')?;
        let len = name
            .char_indices()
            .find(|(_, c)| !c.is_ascii_alphanumeric() && *c != '_')
            .map_or(name.len(), |(index, _)| index);
        if len == 0 || !name.starts_with(|c: char| c.is_ascii_alphabetic()) {
            return None;
        }
        // `.sizeof(X)` and `.hibyte(X)` are functions, reported as what they
        // compute rather than as directives.
        if name[len..].starts_with('(') {
            return None;
        }
        // `.loop` on a line of its own is a K816 local label missing its
        // colon, not a directive: only ca65's own vocabulary counts.
        if !is_ca65_directive(&name[..len]) {
            return None;
        }
        Some(Directive {
            column,
            text: &rest[..len + 1],
            argument: rest[len + 1..].trim(),
        })
    }

    /// The code on this line: everything before a `//` comment.
    fn code(&self) -> &'a str {
        self.text.split("//").next().unwrap_or("")
    }

    /// The column of `needle` nearest the recovery point: the first
    /// occurrence covering or after `at`, else the last one before it.
    ///
    /// "Covering", because the grammar may stop *inside* the idiom — at the
    /// `<` of `#<addr` — and an occurrence that straddles the recovery point
    /// is the one being explained.
    fn find(&self, needle: &str, at: usize) -> Option<usize> {
        let at = at.saturating_sub(needle.len() - 1);
        let mut best: Option<usize> = None;
        for (column, _) in self.code().match_indices(needle) {
            if best.is_none_or(|found| nearer(column, found, at)) {
                best = Some(column);
            }
        }
        best
    }

    /// An immediate address-byte operator — `#<`, `#>` or `#^` — or one of
    /// the `.lobyte()`/`.hibyte()`/`.bankbyte()` functions, nearest `at`.
    fn address_byte(&self, at: usize) -> Option<(usize, AddressByte)> {
        let mut best: Option<(usize, AddressByte)> = None;
        let mut consider = |column: Option<usize>, form: AddressByte| {
            if let Some(column) = column
                && best.is_none_or(|(found, _)| nearer(column, found, at))
            {
                best = Some((column, form));
            }
        };
        consider(self.find("#<", at), AddressByte::Low);
        consider(self.find("#>", at), AddressByte::High);
        consider(self.find("#^", at), AddressByte::Bank);
        consider(self.find(".lobyte(", at), AddressByte::LowCall);
        consider(self.find(".hibyte(", at), AddressByte::HighCall);
        consider(self.find(".bankbyte(", at), AddressByte::BankCall);
        best
    }

    /// `.sizeof(NAME)`: the column of the `.` and the length through `)`.
    fn sizeof_call(&self, at: usize) -> Option<(usize, usize)> {
        let column = self.find(".sizeof(", at)?;
        let close = self.text[column..].find(')')?;
        Some((column, close + 1))
    }

    /// A ca65 cheap local label — `@name` where `name` is not a width
    /// annotation — as a definition or as an operand, nearest `at`: column
    /// and length.
    fn cheap_local(&self, at: usize) -> Option<(usize, usize)> {
        let mut best: Option<(usize, usize)> = None;
        for (column, len) in cheap_locals(self.code()) {
            if best.is_none_or(|(found, _)| nearer(column, found, at)) {
                best = Some((column, len));
            }
        }
        best
    }

    /// `name:` as the first token of the line: the length of the label
    /// including its colon.
    fn leading_label(&self) -> Option<usize> {
        let column = self.indent();
        let rest = &self.text[column..];
        let len = rest
            .char_indices()
            .find(|(_, c)| !c.is_ascii_alphanumeric() && *c != '_')
            .map_or(rest.len(), |(index, _)| index);
        if len == 0 || !rest.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
            return None;
        }
        // `name::` is the scope operator, reported above; `name:` is a
        // label.
        (rest[len..].starts_with(':') && !rest[len..].starts_with("::")).then_some(len + 1)
    }
}

/// ca65's control commands, without the dot and in lower case: the
/// vocabulary `doc/ca65.sgml` lists. A `.name` outside it is a K816 local
/// label reference, which the grammar reports on its own terms.
fn is_ca65_directive(name: &str) -> bool {
    const DIRECTIVES: &[&str] = &[
        "a16",
        "a8",
        "addr",
        "align",
        "asciiz",
        "asciz",
        "assert",
        "autoimport",
        "bankbyte",
        "bankbytes",
        "bss",
        "byt",
        "byte",
        "case",
        "charmap",
        "code",
        "condes",
        "constructor",
        "data",
        "dbg",
        "debuginfo",
        "define",
        "delmac",
        "delmacro",
        "destructor",
        "dbyt",
        "dword",
        "else",
        "elseif",
        "end",
        "endenum",
        "endif",
        "endmac",
        "endmacro",
        "endproc",
        "endrep",
        "endrepeat",
        "endscope",
        "endstruct",
        "endunion",
        "enum",
        "error",
        "exitmac",
        "exitmacro",
        "export",
        "exportzp",
        "faraddr",
        "fatal",
        "feature",
        "fileopt",
        "fopt",
        "forceimport",
        "global",
        "globalzp",
        "hibyte",
        "hibytes",
        "hiword",
        "i16",
        "i8",
        "if",
        "ifblank",
        "ifconst",
        "ifdef",
        "ifnblank",
        "ifndef",
        "ifnref",
        "ifp02",
        "ifp4510",
        "ifp816",
        "ifpc02",
        "ifpsc02",
        "ifref",
        "import",
        "importzp",
        "incbin",
        "include",
        "interruptor",
        "ismnem",
        "ismnemonic",
        "linecont",
        "list",
        "listbytes",
        "literal",
        "lobyte",
        "lobytes",
        "local",
        "localchar",
        "loword",
        "macpack",
        "macro",
        "org",
        "out",
        "p02",
        "p4510",
        "p816",
        "pagelen",
        "pagelength",
        "pc02",
        "popcpu",
        "popseg",
        "proc",
        "psc02",
        "pushcpu",
        "pushseg",
        "referto",
        "refto",
        "reloc",
        "repeat",
        "res",
        "rodata",
        "scope",
        "segment",
        "set",
        "setcpu",
        "smart",
        "struct",
        "tag",
        "undef",
        "undefine",
        "union",
        "warning",
        "word",
        "zeropage",
    ];
    let lower = name.to_ascii_lowercase();
    DIRECTIVES.contains(&lower.as_str())
}

/// Whether `candidate` explains a recovery at `at` better than `current`:
/// an occurrence at or after the recovery point beats one before it, and
/// among those on the same side the nearer one wins.
fn nearer(candidate: usize, current: usize, at: usize) -> bool {
    match (candidate >= at, current >= at) {
        (true, false) => true,
        (false, true) => false,
        (true, true) => candidate < current,
        (false, false) => candidate > current,
    }
}

/// Every `@name` in `text` that is not a width annotation: offset and
/// length, the `@` included.
fn cheap_locals(text: &str) -> impl Iterator<Item = (usize, usize)> + '_ {
    text.match_indices('@').filter_map(|(column, _)| {
        let name = &text[column + 1..];
        let len = name
            .char_indices()
            .find(|(_, c)| !c.is_ascii_alphanumeric() && *c != '_')
            .map_or(name.len(), |(index, _)| index);
        let name = &name[..len];
        let is_width = matches!(name, "a8" | "a16" | "i8" | "i16");
        (len > 0 && name.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') && !is_width)
            .then_some((column, len + 1))
    })
}

struct Directive<'a> {
    column: usize,
    /// The directive with its dot, `.byte`.
    text: &'a str,
    /// What follows it on the line, trimmed.
    argument: &'a str,
}

#[derive(Clone, Copy)]
enum AddressByte {
    Low,
    High,
    Bank,
    LowCall,
    HighCall,
    BankCall,
}

// ---------------------------------------------------------------------------
// The diagnostics
// ---------------------------------------------------------------------------

const GUIDE: &str = "doc/ca65-to-k816.md";

fn comment(line: &Line<'_>, column: usize, source: &SourceFile) -> Diagnostic {
    let semicolon = line.span(column, 1, source);
    Diagnostic::error(
        DiagnosticCode::CA65_COMMENT,
        "`;` begins a comment in ca65, and a K816 comment is `//`",
        Label::new(
            line.span_to_end(column, source),
            "read as a statement separator followed by code",
        ),
    )
    .with_note(
        "`;` separates statements in K816, so the text after it was parsed as \
         a statement and could not be",
    )
    .with_help("write the comment as `// …`")
    .with_fix(Fix {
        title: "replace `;` with `//`".to_owned(),
        // A `;` is legal K816, so this is the likely reading rather than the
        // only one.
        applicability: Applicability::MaybeIncorrect,
        edits: vec![TextEdit::replace(semicolon, "//")],
    })
    .with_docs(GUIDE)
}

fn comment_read_as_calls(
    line: &Line<'_>,
    column: usize,
    words: usize,
    source: &SourceFile,
) -> Diagnostic {
    let semicolon = line.span(column, 1, source);
    Diagnostic::warning(
        DiagnosticCode::CA65_COMMENT,
        "`;` begins a comment in ca65, and a K816 comment is `//`",
        Label::new(
            line.span_to_end(column, source),
            format!("read as a statement separator followed by {words} calls"),
        ),
    )
    .with_note(
        "`;` separates statements in K816, so each word after it is a call to a function of \
         that name, and each will be reported as undeclared",
    )
    .with_help("write the comment as `// …`")
    .with_fix(Fix {
        title: "replace `;` with `//`".to_owned(),
        applicability: Applicability::MaybeIncorrect,
        edits: vec![TextEdit::replace(semicolon, "//")],
    })
    .with_docs(GUIDE)
}

fn directive(line: &Line<'_>, directive: Directive<'_>, source: &SourceFile) -> Diagnostic {
    let span = line.span(directive.column, directive.text.len(), source);
    let name = directive.text;
    let mut diagnostic = Diagnostic::error(
        DiagnosticCode::CA65_DIRECTIVE,
        format!("`{name}` is a ca65 directive"),
        Label::new(span, "K816 has no `.` directives"),
    )
    .with_docs(GUIDE);

    match spelling(name, directive.argument) {
        Spelling::Write(help) => diagnostic = diagnostic.with_help(help),
        Spelling::Replace(replacement, help) => {
            diagnostic = diagnostic.with_help(help).with_fix(Fix {
                title: format!("replace `{name}` with `{replacement}`"),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::replace(span, replacement)],
            });
        }
        Spelling::Delete(why) => {
            diagnostic = diagnostic
                .with_note(why)
                .with_help("delete the line")
                .with_fix(Fix {
                    title: "delete the line".to_owned(),
                    applicability: Applicability::MaybeIncorrect,
                    edits: vec![TextEdit::delete(line.span_to_end(line.indent(), source))],
                });
        }
        Spelling::Unknown => {
            diagnostic = diagnostic.with_help(format!(
                "see {GUIDE} for what K816 writes where ca65 writes a directive"
            ));
        }
    }
    diagnostic
}

/// What K816 writes where ca65 writes a directive.
enum Spelling {
    /// Advice only: the K816 form is not a token-for-token replacement.
    Write(String),
    /// The directive token itself is replaced, and the line then parses.
    Replace(&'static str, &'static str),
    /// The directive has no K816 counterpart because K816 does its work
    /// without being asked.
    Delete(&'static str),
    Unknown,
}

fn spelling(name: &str, argument: &str) -> Spelling {
    let lower = name.to_ascii_lowercase();
    match lower.as_str() {
        ".a8" => Spelling::Replace(
            "@a8",
            "write `@a8`: a statement, or an annotation on the section",
        ),
        ".a16" => Spelling::Replace(
            "@a16",
            "write `@a16`: a statement, or an annotation on the section",
        ),
        ".i8" => Spelling::Replace(
            "@i8",
            "write `@i8`: a statement, or an annotation on the section",
        ),
        ".i16" => Spelling::Replace(
            "@i16",
            "write `@i16`: a statement, or an annotation on the section",
        ),
        ".segment" => Spelling::Write(format!(
            "write `segment {}` — the name is not quoted",
            argument.trim_matches('"')
        )),
        ".code" => Spelling::Replace("segment CODE", "write `segment CODE`"),
        ".data" => Spelling::Replace("segment DATA", "write `segment DATA`"),
        ".bss" => Spelling::Replace("segment BSS", "write `segment BSS`"),
        ".rodata" => Spelling::Replace("segment RODATA", "write `segment RODATA`"),
        ".zeropage" => Spelling::Write(
            "a direct-page variable is `var dp NAME`, in any segment; K816 reserves it in \
             `ZEROPAGE` itself"
                .to_owned(),
        ),
        ".byte" | ".byt" | ".res" | ".word" | ".addr" | ".faraddr" | ".dword" | ".asciiz"
        | ".asciz" | ".dbyt" => Spelling::Write(data_entry(&lower).to_owned()),
        ".proc" => Spelling::Write(format!(
            "write `func {argument} {{` and close the body with `}}` where `.endproc` was; \
             `naked {argument} {{` if the body must not end in `rts`"
        )),
        ".endproc" => Spelling::Replace("}", "close the `func` body with `}`"),
        ".macro" => Spelling::Write(format!(
            "write `inline {} {{` and close the body with `}}`; a parameter is `#name` for an \
             immediate or a bare name for an address",
            argument
                .split(|c: char| c == ',' || c.is_whitespace())
                .next()
                .unwrap_or("NAME")
        )),
        ".endmacro" | ".endmac" => Spelling::Replace("}", "close the `inline` body with `}`"),
        ".struct" => Spelling::Write(format!(
            "a layout is `var {argument}[ .field :byte … ] = $ADDR` at a fixed address, or \
             `abstract var {argument}[ … ]` for offsets alone; `k816 bind` writes one from a \
             linked program's debug file"
        )),
        ".endstruct" => Spelling::Replace("]", "close the field list with `]`"),
        ".tag" => Spelling::Write(format!(
            "a field of a nested layout's size is `.name :byte [{argument}:sizeof]`; a \
             variable of that layout repeats its fields, or is `var NAME * COUNT` over them"
        )),
        ".enum" => Spelling::Write(
            "write `const FIRST = 0, SECOND, THIRD`: a comma-separated `const` counts up"
                .to_owned(),
        ),
        ".endenum" => Spelling::Delete("the `const` group ends where its last name does"),
        ".include" => Spelling::Write(format!(
            "write `import {argument}` for a K816 unit; for a ca65 include file, generate its \
             declarations with `k816 bind` and import the result"
        )),
        ".import" | ".importzp" => Spelling::Write(
            "a name another object defines is `extern func NAME`, `extern var [dp] NAME` or \
             `extern const NAME`; `k816 bind` writes these from the linked program"
                .to_owned(),
        ),
        ".export" | ".exportzp" | ".global" | ".globalzp" => {
            Spelling::Delete("K816 exports every unit-level definition without being asked")
        }
        ".define" => Spelling::Write(format!(
            "write `const {}`",
            argument.replacen(' ', " = ", 1)
        )),
        ".org" => Spelling::Write(
            "a fixed address is written on the declaration — `var NAME = $ADDR`, or \
             `address $ADDR` as the first entry of a `data` block or a section"
                .to_owned(),
        ),
        ".reloc" => Spelling::Delete("an `address` placement ends with its section"),
        ".align" => Spelling::Replace("align", "write `align N` as the first entry of the block"),
        ".repeat" => Spelling::Write(format!(
            "write `repeat {argument} {{ … }}` inside a `data` block"
        )),
        ".endrep" | ".endrepeat" => Spelling::Replace("}", "close the `repeat` body with `}`"),
        ".if" | ".ifdef" | ".ifndef" => Spelling::Write(
            "write `#if EXPR` … `#endif`; a name is defined with `const` or in a `[ … ]` block"
                .to_owned(),
        ),
        ".else" => Spelling::Replace("#else", "write `#else`"),
        ".elseif" => Spelling::Replace("#elif", "write `#elif`"),
        ".endif" => Spelling::Replace("#endif", "write `#endif`"),
        ".macpack" => Spelling::Delete(
            "K816 has no macro packs; `add`/`sub` from `generic` are `clc adc`/`sec sbc`",
        ),
        ".feature" | ".setcpu" | ".p816" | ".pc02" | ".p02" | ".smart" | ".autoimport"
        | ".case" | ".debuginfo" | ".listbytes" | ".pagelength" | ".linecont" => {
            Spelling::Delete("K816 needs no assembler configuration; the CPU is the 65816")
        }
        ".local" => Spelling::Write(
            "a local label is `.name:` and is scoped to its section already".to_owned(),
        ),
        ".scope" | ".endscope" => Spelling::Write(
            "K816 has no scopes; a `func` already scopes its local labels".to_owned(),
        ),
        ".incbin" => Spelling::Write(format!("write `binary {argument}` in a `data` block")),
        ".assert" | ".error" | ".warning" | ".out" => {
            Spelling::Write("write `#error MESSAGE` under a `#if`".to_owned())
        }
        _ => Spelling::Unknown,
    }
}

fn data_entry(directive: &str) -> &'static str {
    match directive {
        ".byte" | ".byt" => {
            "write the values inside a `data NAME { … }` block, bare or after `byte`"
        }
        ".word" | ".addr" => "write `word VALUE …` inside a `data NAME { … }` block",
        ".faraddr" => "write `far VALUE …` inside a `data NAME { … }` block",
        ".dword" => "write two `word` entries, low word first, inside a `data` block",
        ".asciiz" | ".asciz" => "write `\"text\" 0` inside a `data NAME { … }` block",
        ".dbyt" => "write `byte &>VALUE &<VALUE` inside a `data` block: high byte first",
        ".res" => {
            "write `var NAME[N]` for N bytes, `var NAME:word * N` for N words, or `?` entries \
             inside a `data` block"
        }
        _ => "write the entry inside a `data NAME { … }` block",
    }
}

fn scope_operator(line: &Line<'_>, column: usize, source: &SourceFile) -> Diagnostic {
    let span = line.span(column, 2, source);
    let global = column == line.indent()
        || !line.text[..column].ends_with(|c: char| c.is_ascii_alphanumeric() || c == '_');
    if global {
        return Diagnostic::error(
            DiagnosticCode::CA65_SCOPE_OPERATOR,
            "`::` is ca65's scope operator",
            Label::new(span, "a unit-level name needs no prefix in K816"),
        )
        .with_help("delete the `::`")
        .with_fix(Fix {
            title: "delete `::`".to_owned(),
            applicability: Applicability::MachineApplicable,
            edits: vec![TextEdit::delete(span)],
        })
        .with_docs(GUIDE);
    }
    Diagnostic::error(
        DiagnosticCode::CA65_SCOPE_OPERATOR,
        "`::` is ca65's scope operator, and a K816 field is `NAME.field`",
        Label::new(span, "write `.`"),
    )
    .with_note(
        "a `.struct` becomes a field layout — `var NAME[ .field :byte … ] = $ADDR` — and its \
         fields are reached with `.`",
    )
    .with_help("replace `::` with `.`")
    .with_fix(Fix {
        title: "replace `::` with `.`".to_owned(),
        applicability: Applicability::MachineApplicable,
        edits: vec![TextEdit::replace(span, ".")],
    })
    .with_docs(GUIDE)
}

fn address_byte(
    line: &Line<'_>,
    column: usize,
    form: AddressByte,
    source: &SourceFile,
) -> Diagnostic {
    let (len, replacement, help) = match form {
        AddressByte::Low => (2, Some("#&<"), "write `#&<EXPR` for the low byte"),
        AddressByte::High => (2, Some("#&>"), "write `#&>EXPR` for the high byte"),
        AddressByte::Bank => (
            2,
            None,
            "write `#EXPR >> 16` for the bank byte; K816 has no bank-byte operator",
        ),
        AddressByte::LowCall => (8, Some("&<"), "write `&<EXPR` for the low byte"),
        AddressByte::HighCall => (8, Some("&>"), "write `&>EXPR` for the high byte"),
        AddressByte::BankCall => (
            10,
            None,
            "write `EXPR >> 16` for the bank byte; K816 has no bank-byte operator",
        ),
    };
    let span = line.span(column, len, source);
    let text = &line.text[column..column + len];
    let mut diagnostic = Diagnostic::error(
        DiagnosticCode::CA65_ADDRESS_BYTE,
        format!("`{text}` is ca65's address-byte operator"),
        Label::new(span, "K816 spells the address-byte operators `&<` and `&>`"),
    )
    .with_help(help)
    .with_docs(GUIDE);
    if let Some(replacement) = replacement {
        // The function forms wrap their argument in parentheses, which K816
        // reads as an ordinary parenthesized expression, so replacing the
        // name alone is enough.
        diagnostic = diagnostic.with_fix(Fix {
            title: format!("replace `{text}` with `{replacement}`"),
            applicability: Applicability::MachineApplicable,
            edits: vec![TextEdit::replace(span, replacement)],
        });
    }
    diagnostic
}

fn sizeof(line: &Line<'_>, column: usize, len: usize, source: &SourceFile) -> Diagnostic {
    let span = line.span(column, len, source);
    let argument = line.text[column + ".sizeof(".len()..column + len - 1].trim();
    let replacement = format!("{argument}:sizeof");
    Diagnostic::error(
        DiagnosticCode::CA65_SIZEOF,
        "`.sizeof()` is a ca65 function, and a K816 size is a suffix",
        Label::new(span, format!("write `{replacement}`")),
    )
    .with_note(
        "`NAME:sizeof` is a layout's element size and `NAME.field:offsetof` a field's offset",
    )
    .with_help(format!(
        "replace `.sizeof({argument})` with `{replacement}`"
    ))
    .with_fix(Fix {
        title: format!("replace with `{replacement}`"),
        applicability: Applicability::MachineApplicable,
        edits: vec![TextEdit::replace(span, replacement)],
    })
    .with_docs(GUIDE)
}

fn cheap_local(line: &Line<'_>, column: usize, len: usize, source: &SourceFile) -> Diagnostic {
    let span = line.span(column, len, source);
    let name = &line.text[column + 1..column + len];
    // The fix renames every use in the file, not the one at the caret: a
    // label with one spelling at its definition and another at a branch is
    // two undeclared names, and the diagnostic would come straight back.
    let edits: Vec<TextEdit> = cheap_locals(source.text())
        .filter(|(offset, found)| source.text()[offset + 1..offset + found] == *name)
        .map(|(offset, _)| {
            TextEdit::replace(
                Span::new(source.id(), offset as u32, offset as u32 + 1),
                ".",
            )
        })
        .collect();
    Diagnostic::error(
        DiagnosticCode::CA65_CHEAP_LOCAL,
        format!("`@{name}` is a ca65 cheap local label, and a K816 local label is `.{name}`"),
        Label::new(span, "write `.` where ca65 writes `@`"),
    )
    .with_note(
        "a K816 local label is scoped to its section, which is what a cheap local was for; \
         `@` introduces a width annotation here",
    )
    .with_help(format!(
        "replace `@{name}` with `.{name}`, at the definition and at every use"
    ))
    .with_fix(Fix {
        title: format!("replace `@{name}` with `.{name}`"),
        applicability: Applicability::MachineApplicable,
        edits,
    })
    .with_docs(GUIDE)
}

fn code_outside_section(line: &Line<'_>, len: usize, source: &SourceFile) -> Diagnostic {
    let column = line.indent();
    let span = line.span(column, len, source);
    let name = &line.text[column..column + len - 1];
    Diagnostic::error(
        DiagnosticCode::CA65_CODE_OUTSIDE_SECTION,
        format!("`{name}:` labels code outside a section"),
        Label::new(
            span,
            "a K816 label lives inside a `func`, `naked` or `data` block",
        ),
    )
    .with_note(
        "ca65 assembles a label followed by instructions anywhere; K816 code is a section \
         with a name, and the section's name is the label",
    )
    .with_help(format!(
        "write `func {name} {{` … `}}` around the code that follows — `naked {name}` if it must \
         not end in `rts`, `data {name} {{` if it is bytes"
    ))
    .with_docs(GUIDE)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::SourceMap;

    fn line(text: &str) -> (SourceMap, crate::source::FileId) {
        let mut map = SourceMap::new();
        let id = map.add_file("t.k816", text);
        (map, id)
    }

    #[test]
    fn a_semicolon_inside_a_string_or_char_literal_is_not_a_comment() {
        let (map, id) = line("    \"a;b\" ';' ; real\n");
        let l = Line::at(0, map.file(id));
        assert_eq!(l.semicolon(), Some(14));
        let (map, id) = line("    \"a;b\" 'x'\n");
        assert_eq!(Line::at(0, map.file(id)).semicolon(), None);
    }

    #[test]
    fn a_semicolon_after_a_line_comment_is_not_seen() {
        let (map, id) = line("nop // x; y\n");
        assert_eq!(Line::at(0, map.file(id)).semicolon(), None);
    }

    #[test]
    fn width_annotations_are_not_cheap_locals() {
        let (map, id) = line("    @a16 @i8 @loop:\n");
        let l = Line::at(0, map.file(id));
        assert_eq!(l.cheap_local(0), Some((13, 5)));
        let (map, id) = line("    @a16 { nop }\n");
        assert_eq!(Line::at(0, map.file(id)).cheap_local(0), None);
    }

    #[test]
    fn a_directive_is_the_first_token_only() {
        let (map, id) = line("  .byte 1, 2\n");
        let l = Line::at(0, map.file(id));
        let directive = l.leading_directive().expect("a directive");
        assert_eq!(
            (directive.column, directive.text, directive.argument),
            (2, ".byte", "1, 2")
        );
        let (map, id) = line("  lda .sizeof(X)\n");
        assert!(Line::at(0, map.file(id)).leading_directive().is_none());
        // A K816 local label is not a directive, colon or no colon.
        let (map, id) = line("  .loop:\n");
        assert!(Line::at(0, map.file(id)).leading_directive().is_none());
        let (map, id) = line("  .done\n");
        assert!(Line::at(0, map.file(id)).leading_directive().is_none());
        let (map, id) = line("  .A8\n");
        assert!(Line::at(0, map.file(id)).leading_directive().is_some());
    }

    #[test]
    fn the_scope_operator_is_told_from_a_global_prefix() {
        let (map, id) = line("sta RIA::op\n");
        let l = Line::at(0, map.file(id));
        assert_eq!(l.find("::", 0), Some(7));
        assert!(l.leading_label().is_none());
        let (map, id) = line("loop:   dex\n");
        assert_eq!(Line::at(0, map.file(id)).leading_label(), Some(5));
    }
}
