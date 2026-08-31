/*****************************************************************************/
/*                                                                           */
/*                            doc_compile_time.rs                            */
/*                                                                           */
/*     The coverage audit for the syntax reference's compile-time sections   */
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

//! The coverage audit for `doc/syntax-reference.md`'s two compile-time
//! sections: **"Compile-time Evaluator"** and **"Preprocessor Directives"**.
//!
//! The sibling of `tests/doc_coverage.rs`, in its shape and for its reason.
//! The width document's listings are an acceptance test a reader can check,
//! and sub-plan F made them one; these two sections are the same kind of
//! document — dense with small, exact claims about what a compile-time
//! construct *computes* — and until now nothing held them to it. A section
//! covered only by a test with a plausible name is not covered: the
//! document's own sentence is what runs.
//!
//! Each [`Case`] carries the **claim** it checks, quoted from the document,
//! so a reader comparing the two can see what a row is for and a reader
//! changing the document can see what it breaks. A case that no longer holds
//! is a defect in one of the two, and the fix is to work out which — never to
//! adjust the expectation until it passes.
//!
//! # What the cases are compiled as
//!
//! Sources are the document's listings, with two liberties, both taken so
//! that the *listing* is what is read rather than a rewrite of it:
//!
//! - **A byte is how the evaluator is observed.** An evaluator value is not
//!   an artifact by itself, so a claim about one is checked by putting it in
//!   a data entry and reading the assembled `.byte`. A section needs no
//!   reader and no `main` to be emitted, so nothing is added to the listing
//!   for that.
//! - **A name is sometimes changed.** `a`, `c`, `d`, `n`, `o`, `p` and the
//!   rest of the register and flag spellings cannot be *declared*
//!   (`K816-E0158`, sub-plan G decisions 29–33), so a listing that declares a
//!   short-named section is given a longer one. Evaluator names are
//!   unaffected and are left exactly as written.
//!
//! Two claims are checked by hand below the matrix rather than in it, because
//! what they assert is a diagnostic's *text* or its *position* rather than a
//! code: `print`'s `%lf` formatting, and `evalfunc`'s "the error lands on the
//! definition rather than on a call site".

use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

/// One documented claim, and the source that decides it.
struct Case {
    /// The `##` or `###` heading the claim is under, verbatim, so that
    /// `every_compile_time_section_is_accounted_for` can walk the document.
    section: &'static str,
    /// The sentence being checked, quoted from the document.
    claim: &'static str,
    source: &'static str,
    /// The bytes the source's data sections assemble to, in order.
    bytes: &'static [u8],
    /// The diagnostic codes it reports, in order. Empty means it is clean.
    codes: &'static [&'static str],
}

const EVALUATOR: &str = "Compile-time Evaluator";
const STATEMENTS: &str = "Statements";
const EVALFUNC: &str = "`evalfunc`";
const OPERATORS: &str = "Operators";
const FUNCTIONS: &str = "Functions";
const MEMBERS: &str = "Member access and method calls";
const PREPROCESSOR: &str = "Preprocessor Directives";
const WHERE: &str = "Where a region may sit";
const CONDITIONS: &str = "Conditions";
const ORDER: &str = "Order";
const NOT_TAKEN: &str = "What a region that is not taken does";
const INLINE_BODIES: &str = "`inline` bodies";

const CASES: &[Case] = &[
    // -----------------------------------------------------------------
    // Compile-time Evaluator — the opening prose
    // -----------------------------------------------------------------
    Case {
        section: EVALUATOR,
        claim: "In data blocks, a standalone eval-bracket entry emits byte-oriented output",
        source: "data NumberFour {\n  [2+2]\n}\n",
        bytes: &[0x04],
        codes: &[],
    },
    Case {
        section: EVALUATOR,
        claim: "The evaluator can also be invoked freely outside any section body to set \
                variables, which can be later used as compiler constants",
        source: "[ FOUR = 4 ]\n\
                 data FourFiveSix {\n  FOUR\n  [FIVE = FOUR+1]\n  [FIVE+1]\n}\n",
        bytes: &[0x04, 0x05, 0x06],
        codes: &[],
    },
    Case {
        section: EVALUATOR,
        claim: "a `const` group's entries are a sequence, so `const A = 3, B = [A + 1]` \
                reads `A`",
        // `A` and `B` are register spellings, so the names are longer here;
        // what the case is about is the *order*, which is untouched.
        source: "const FIRST = 3, SECOND = [FIRST + 1]\ndata seq {\n  FIRST\n  SECOND\n}\n",
        bytes: &[0x03, 0x04],
        codes: &[],
    },
    Case {
        section: EVALUATOR,
        claim: "assigning that same name again in a different block is rejected (`K816-E0149`)",
        source: "[ n = 1 ]\n[ n = 2 ]\n",
        bytes: &[],
        codes: &["K816-E0149"],
    },
    Case {
        section: EVALUATOR,
        claim: "Within one block a name may be assigned as often as it likes, which is what \
                makes a counted loop expressible",
        source: "[ n = 0  while n < 5 { n = n + 1 } ]\ndata counted {\n  [n]\n}\n",
        bytes: &[0x05],
        codes: &[],
    },
    Case {
        section: EVALUATOR,
        claim: "Assigning to a name the compiler already owns — a `const`, a `var`, a label \
                — is the same error",
        source: "const OWNED = 1\n[ OWNED = 2 ]\n",
        bytes: &[],
        codes: &["K816-E0149"],
    },
    // -----------------------------------------------------------------
    // Statements
    // -----------------------------------------------------------------
    Case {
        section: STATEMENTS,
        claim: "the statement-value table: `if C S` is 0 when C is false, `while C S` is 0 if \
                the body never ran, and a block's value is the last statement's",
        source: "data vals {\n  [ if 0 7 ]\n  [ if 1 7 else 9 ]\n  [ if 0 7 else 9 ]\n  \
                 [ m = 0  while m 3 ]\n  [ { 1 2 5 } ]\n}\n",
        bytes: &[0x00, 0x07, 0x09, 0x00, 0x05],
        codes: &[],
    },
    Case {
        section: STATEMENTS,
        claim: "A bracket holds a *run* of statements, not a single expression, and its value \
                is the last one's",
        // The document's own listing. Its value is the *last* statement's —
        // a false `if`, so 0 — while `FLAGS` is 1: which is the whole point
        // of the sentence, and would go unnoticed if only `FLAGS` were read.
        source: "const WIDE = 1, TALL = 0\n\
                 data flags {\n  \
                 [ FLAGS = 0  if WIDE FLAGS = FLAGS | 1  if TALL FLAGS = FLAGS | 2 ]\n  \
                 [FLAGS]\n}\n",
        bytes: &[0x00, 0x01],
        codes: &[],
    },
    Case {
        section: STATEMENTS,
        claim: "a `while` whose condition never goes false is `K816-E0147` rather than a \
                compiler that does not return",
        source: "data runaway {\n  [ n = 1  while n 0 ]\n}\n",
        bytes: &[],
        codes: &["K816-E0147"],
    },
    // -----------------------------------------------------------------
    // `evalfunc`
    // -----------------------------------------------------------------
    Case {
        section: EVALFUNC,
        claim: "the two declaration forms, a bracket body and a block body",
        source: "evalfunc dbl(q) [ q * 2 ]\n\
                 evalfunc lerp(lo, hi, t) { lo + (hi - lo) * t }\n\
                 data fns {\n  [ dbl(5) ]\n  [ lerp(0, 10, 0.5) ]\n}\n",
        bytes: &[0x0A, 0x05],
        codes: &[],
    },
    Case {
        section: EVALFUNC,
        claim: "A function is identified by its name and argument count together, so `f(a)` \
                and `f(a, b)` are two functions",
        source: "evalfunc f(q) [ q ]\n\
                 evalfunc f(q, r) [ q + r ]\n\
                 data arity {\n  [ f(3) ]\n  [ f(3, 4) ]\n}\n",
        bytes: &[0x03, 0x07],
        codes: &[],
    },
    Case {
        section: EVALFUNC,
        claim: "redeclaring either … is an error",
        source: "evalfunc g(q) [ q ]\nevalfunc g(q) [ q + 1 ]\n",
        bytes: &[],
        codes: &["K816-E0102"],
    },
    Case {
        section: EVALFUNC,
        claim: "or shadowing a library function at the same count, is an error. Every name in \
                the library is protected, `index` and `size` among them",
        source: "evalfunc abs(q) [ q ]\n",
        bytes: &[],
        codes: &["K816-E0102"],
    },
    Case {
        section: EVALFUNC,
        claim: "A body's calls are resolved where the body is written, so a function it uses \
                must be declared above it",
        source: "evalfunc up(q) [ down(q) ]\nevalfunc down(q) [ q ]\n",
        bytes: &[],
        codes: &["K816-E0101"],
    },
    Case {
        section: EVALFUNC,
        claim: "A callee does not see its caller's parameters — there is no scope chain",
        source: "evalfunc inner(r) [ q ]\n\
                 evalfunc outer(q) [ inner(1) ]\n\
                 data scoped {\n  [ outer(5) ]\n}\n",
        bytes: &[],
        codes: &["K816-E0101"],
    },
    Case {
        section: EVALFUNC,
        claim: "but global evaluator names and `const`s are visible everywhere",
        source: "[ GLOB = 7 ]\nconst KONST = 2\n\
                 evalfunc reads() [ GLOB + KONST ]\n\
                 data globals {\n  [ reads() ]\n}\n",
        bytes: &[0x09],
        codes: &[],
    },
    Case {
        section: EVALFUNC,
        claim: "Recursion works",
        source: "evalfunc fact(n) [ if n <= 1 1 else n * fact(n - 1) ]\n\
                 data recursion {\n  [ fact(5) ]\n}\n",
        bytes: &[0x78],
        codes: &[],
    },
    Case {
        section: EVALFUNC,
        claim: "and is bounded by a call-depth limit (`K816-E0147`) separate from the step \
                budget",
        source: "evalfunc down(n) [ down(n + 1) ]\ndata deep {\n  [ down(0) ]\n}\n",
        bytes: &[],
        codes: &["K816-E0147"],
    },
    // -----------------------------------------------------------------
    // Operators
    // -----------------------------------------------------------------
    Case {
        section: OPERATORS,
        claim: "`%` is a non-negative modulo, so `(0 - 7) % 3` is `2`; `/` is float division, \
                `[7 / 2]` is `3.5`, which becomes `4` at an integer boundary; `x op= e` reads \
                `x` before it evaluates `e`, so `[X = 1, X += (X = 5), X]` is `6`",
        source: "data ops {\n  [ (0 - 7) % 3 ]\n  [ 7 / 2 ]\n  [ X = 1, X += (X = 5), X ]\n}\n",
        bytes: &[0x02, 0x04, 0x06],
        codes: &[],
    },
    Case {
        section: OPERATORS,
        claim: "`&&` and `||` do not short-circuit. Both operands are evaluated",
        source: "data shortcut {\n  \
                 [ SEEN = 0, ((SEEN = SEEN + 1) && 0) && (SEEN = SEEN + 1), SEEN ]\n}\n",
        bytes: &[0x02],
        codes: &[],
    },
    Case {
        section: OPERATORS,
        claim: "The bitwise operators and the shifts truncate their operands toward zero, \
                which is not the rounding an integer boundary does: `[(0 - 2.7) | 0]` is `-2` \
                while `[0 - 2.7]` in a data entry is `-3`",
        // Both values are negative and a byte entry refuses one
        // (`K816-E0109`), so the pair is read through an offset that keeps
        // them in range: 8 witnesses -2 and 7 witnesses -3.
        source: "data rounding {\n  [ ((0 - 2.7) | 0) + 10 ]\n  [ (0 - 2.7) + 10 ]\n}\n",
        bytes: &[0x08, 0x07],
        codes: &[],
    },
    Case {
        section: OPERATORS,
        claim: "`?>` `?<` select greater/smaller value",
        source: "data select {\n  [ 2 ?> 9 ]\n  [ 2 ?< 9 ]\n}\n",
        bytes: &[0x09, 0x02],
        codes: &[],
    },
    // -----------------------------------------------------------------
    // Functions
    // -----------------------------------------------------------------
    Case {
        section: FUNCTIONS,
        claim: "the library table, one call per row that returns a number",
        source: "data library {\n  \
                 [ abs(0 - 5) ]  [ floor(2.9) ]  [ ceil(2.1) ]  [ round(2.5) ]\n  \
                 [ frac(2.75) * 100 ]  [ min(3, 9) ]  [ max(3, 9) ]  [ clamp(20, 0, 9) ]\n  \
                 [ sqrt(81) ]  [ pow(2, 5) ]  [ sin(0) ]  [ cos(0) ]\n  \
                 [ asin(0) ]  [ acos(1) ]\n}\n",
        bytes: &[
            0x05, 0x02, 0x03, 0x03, 0x4B, 0x03, 0x09, 0x09, 0x09, 0x20, 0x00, 0x01, 0x00, 0x00,
        ],
        codes: &[],
    },
    Case {
        section: FUNCTIONS,
        claim: "`pi` is a value rather than a function, and it is the literal `3.141593` — \
                six decimals, matching the original — not the machine constant",
        source: "data pival {\n  [ pi == 3.141593 ]\n}\n",
        bytes: &[0x01],
        codes: &[],
    },
    Case {
        section: FUNCTIONS,
        claim: "Strings support `+`, `==`, `!=`, `<`, `<=`, `>` and `>=`",
        source: "data strings {\n  [ \"abc\" < \"abd\" ]\n  [ \"a\" + \"b\" == \"ab\" ]\n  \
                 [ \"a\" != \"b\" ]\n}\n",
        bytes: &[0x01, 0x01, 0x01],
        codes: &[],
    },
    Case {
        section: FUNCTIONS,
        claim: "a string that reaches an integer context is `K816-E0148`, because a string is \
                a value inside an evaluation and never the value of one",
        source: "data stringout {\n  [ \"abc\" ]\n}\n",
        bytes: &[],
        codes: &["K816-E0148"],
    },
    Case {
        section: FUNCTIONS,
        claim: "`addbyte(sec, b)`: Refused — K816 does not let an evaluation change a \
                section's contents",
        source: "data one { $01 $02 }\ndata refused {\n  [ addbyte(one, 3) ]\n}\n",
        bytes: &[],
        codes: &["K816-K65-0002"],
    },
    Case {
        section: FUNCTIONS,
        claim: "`color(r, g, b)`, `color(x)` and `rnd()`: Refused",
        source: "data refused {\n  [ color(1, 2, 3) ]\n  [ color(4) ]\n  [ rnd() ]\n}\n",
        bytes: &[],
        codes: &["K816-K65-0002", "K816-K65-0002", "K816-K65-0002"],
    },
    Case {
        section: FUNCTIONS,
        claim: "`sec[i]` is exactly `index(sec, i)`, out of range is zero, and the index \
                takes `ToInt` rather than the boundary rule so `index(t, 1.7)` is `t[1]`",
        source: "data tab { $10 $20 $30 }\n\
                 data reads {\n  [ index(tab, 1) ]\n  [ tab[1] ]\n  [ size(tab) ]\n  \
                 [ index(tab, 99) ]\n  [ index(tab, 1.7) ]\n}\n",
        bytes: &[0x10, 0x20, 0x30, 0x20, 0x20, 0x03, 0x00, 0x20],
        codes: &[],
    },
    Case {
        section: FUNCTIONS,
        claim: "the section has to be written above the read",
        source: "data early {\n  [ size(late) ]\n}\ndata late { $01 $02 }\n",
        bytes: &[],
        codes: &["K816-E0154"],
    },
    Case {
        section: FUNCTIONS,
        claim: "`index(sec, x, y)`: the two-dimensional form on a section is `K816-E0155` \
                always",
        source: "data flat { $10 $20 }\ndata twodim {\n  [ index(flat, 0, 0) ]\n}\n",
        bytes: &[],
        codes: &["K816-E0155"],
    },
    Case {
        section: FUNCTIONS,
        claim: "There is no array literal: `[10, 20, 30]` is the comma pop operator and \
                evaluates to 30",
        source: "data commas {\n  [ 10, 20, 30 ]\n}\n",
        bytes: &[0x1E],
        codes: &[],
    },
    // -----------------------------------------------------------------
    // Member access and method calls
    // -----------------------------------------------------------------
    Case {
        section: MEMBERS,
        claim: "`x.f` is `f(x)` and `x.f(a)` is `f(x, a)` … a call on a literal needs \
                parentheses — `(2.75).frac()`",
        source: "data members {\n  [ (2.75).frac() * 100 ]\n  [ (0 - 3).abs() ]\n  \
                 [ (7).max(9) ]\n}\n",
        bytes: &[0x4B, 0x03, 0x09],
        codes: &[],
    },
    // -----------------------------------------------------------------
    // Preprocessor Directives — the opening listing
    // -----------------------------------------------------------------
    Case {
        section: PREPROCESSOR,
        claim: "`#elif COND`: the first true branch wins; the rest are skipped",
        source: "const DEBUG = 0\n\
                 data chain {\n  #if DEBUG\n  $AA\n  #elif 1\n  $BB\n  #else\n  $CC\n  \
                 #endif\n}\n",
        bytes: &[0xBB],
        codes: &[],
    },
    Case {
        section: PREPROCESSOR,
        claim: "`#error \"message\"` — an error carrying that message; `#warn \"message\"` — \
                a warning carrying that message",
        source: "#if 1\n#error \"stated plainly\"\n#warn \"and this\"\n#endif\n",
        bytes: &[],
        codes: &["K816-E0150", "K816-W0151"],
    },
    // -----------------------------------------------------------------
    // Where a region may sit
    // -----------------------------------------------------------------
    Case {
        section: WHERE,
        claim: "There are three such lists: the file's top-level items, the statements of one \
                block, and the entries of one data block",
        source: "const ON = 1\n\
                 #if ON\nvar slot :byte\n#endif\n\
                 data entries {\n#if ON\n  $01\n#endif\n}\n\
                 func main @a8 @i8 {\n#if ON\n  nop\n#endif\n  return\n}\n",
        bytes: &[0x01],
        codes: &[],
    },
    Case {
        section: WHERE,
        claim: "the document's own listing: `K816-E0152`, twice — the `#if` is never closed \
                and the `#endif` has no `#if` above it",
        source: "func main @a8 @i8 {\n  #if 1\n  return\n}\n#endif\n",
        bytes: &[],
        codes: &["K816-E0152", "K816-E0152"],
    },
    Case {
        section: WHERE,
        claim: "`K816-E0152` also covers … a second `#else` in one chain",
        source: "data chain {\n#if 0\n  $01\n#else\n  $02\n#else\n  $03\n#endif\n}\n",
        bytes: &[],
        codes: &["K816-E0152"],
    },
    Case {
        section: WHERE,
        claim: "A directive ends where its line does … `#error` above its message is \
                `K816-E0001`",
        source: "#if 1\n#error\n  \"message below\"\n#endif\n",
        bytes: &[],
        codes: &["K816-E0001"],
    },
    Case {
        section: WHERE,
        claim: "Only the inside of an evaluator bracket may wrap, because the `]` says where \
                it ends",
        source: "const LEVEL = 2, QUIET = 0\n\
                 data wrapped {\n#if [ LEVEL > 1 &&\n      !QUIET ]\n  $01\n#endif\n}\n",
        bytes: &[0x01],
        codes: &[],
    },
    Case {
        section: WHERE,
        claim: "Nesting is unlimited",
        source: "data nested {\n#if 1\n  #if 1\n    #if 1\n      $07\n    #endif\n  #endif\n\
                 #endif\n}\n",
        bytes: &[0x07],
        codes: &[],
    },
    // -----------------------------------------------------------------
    // Conditions
    // -----------------------------------------------------------------
    Case {
        section: CONDITIONS,
        claim: "A condition is a number, a name, `!` applied to either, or an evaluator \
                bracket … true when its value, taken through the integer boundary rule, is \
                not zero. So `#if [0.6]` is true and `#if [0.4]` is false",
        source: "[ TIER = 2 ]\nconst FEATURE = 1\n\
                 data conds {\n\
                 #if FEATURE\n  $01\n#endif\n\
                 #if TIER\n  $02\n#endif\n\
                 #if !FEATURE\n  $03\n#endif\n\
                 #if [ 0.6 ]\n  $04\n#endif\n\
                 #if [ 0.4 ]\n  $05\n#endif\n}\n",
        bytes: &[0x01, 0x02, 0x04],
        codes: &[],
    },
    Case {
        section: CONDITIONS,
        claim: "A condition may assign, and what it assigns outlives the directive",
        source: "#if [ TARGET = 3 ]\n#endif\ndata target {\n  [ TARGET ]\n}\n",
        bytes: &[0x03],
        codes: &[],
    },
    Case {
        section: CONDITIONS,
        claim: "A name nothing declares is `K816-E0101`, not a silent zero",
        source: "data undeclared {\n#if NOPE\n  $01\n#endif\n}\n",
        bytes: &[],
        codes: &["K816-E0101"],
    },
    // -----------------------------------------------------------------
    // Order
    // -----------------------------------------------------------------
    Case {
        section: ORDER,
        claim: "A directive is decided where it is written, against everything above it. An \
                evaluator bracket earlier in the same block has already run",
        // The bracket is itself an entry, so it emits its own value first —
        // 1, from the assignment — and `$42` follows because it ran.
        source: "data table {\n  [ FLAG = 1 ]\n  #if FLAG\n  $42\n  #endif\n}\n",
        bytes: &[0x01, 0x42],
        codes: &[],
    },
    // -----------------------------------------------------------------
    // What a region that is not taken does
    // -----------------------------------------------------------------
    Case {
        section: NOT_TAKEN,
        claim: "Nothing. Its declarations do not exist, its statements are not emitted, its \
                `#error` and `#warn` say nothing, and an evaluator bracket inside it does not \
                run. Its conditions are not evaluated either",
        source: "data dead {\n#if 0\n  #if NOPE\n  #endif\n  [ NEVER = 1 ]\n  \
                 #error \"not said\"\n  #warn \"not said either\"\n  $AA\n#endif\n  $BB\n}\n",
        bytes: &[0xBB],
        codes: &[],
    },
    Case {
        section: NOT_TAKEN,
        claim: "so a name it would have assigned stays unassigned",
        source: "#if 0\n[ NEVER = 1 ]\n#endif\ndata reads {\n  [ NEVER ]\n}\n",
        bytes: &[],
        codes: &["K816-E0101"],
    },
    Case {
        section: NOT_TAKEN,
        claim: "and neither is the condition of an `#elif` whose chain has already taken a \
                branch",
        source: "data chain {\n#if 1\n  $01\n#elif NOPE\n  $02\n#endif\n}\n",
        bytes: &[0x01],
        codes: &[],
    },
    // -----------------------------------------------------------------
    // `inline` bodies
    // -----------------------------------------------------------------
    Case {
        section: INLINE_BODIES,
        claim: "A directive inside an `inline` body is decided once, where the body is \
                written … and an `#error` or `#warn` in the body speaks once",
        source: "inline once {\n#if 1\n  nop\n#endif\n#warn \"spoken once\"\n}\n\
                 func main @a8 @i8 {\n  once\n  once\n  return\n}\n",
        bytes: &[],
        codes: &["K816-W0151"],
    },
];

/// Compiles `source` and returns (assembled bytes, diagnostic codes).
///
/// Unoptimized: nothing here is a claim about the optimizer, and a pass that
/// folded a data section away would make a row pass for the wrong reason.
fn compile(source: &str) -> (Vec<u8>, Vec<String>) {
    let mut map = SourceMap::new();
    let file = map.add_file("doc.k816", source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let codes = result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str().to_owned())
        .collect();
    let bytes = result
        .assembly
        .map(|assembly| {
            assembly
                .lines()
                .filter_map(|line| line.split(';').next())
                .filter_map(|line| line.trim().strip_prefix(".byte"))
                .flat_map(|values| values.split(','))
                .filter_map(|value| value.trim().strip_prefix('$'))
                .filter_map(|value| u8::from_str_radix(value, 16).ok())
                .collect()
        })
        .unwrap_or_default();
    (bytes, codes)
}

/// Every claim the two sections make, checked against the compiler.
#[test]
fn every_documented_compile_time_claim_holds() {
    let mut failures = Vec::new();
    for case in CASES {
        let (bytes, codes) = compile(case.source);
        if codes != case.codes {
            failures.push(format!(
                "  [{}] {}\n    expected {:?}, reported {:?}",
                case.section, case.claim, case.codes, codes
            ));
            continue;
        }
        if bytes != case.bytes {
            failures.push(format!(
                "  [{}] {}\n    expected bytes {:02X?}, assembled {:02X?}",
                case.section, case.claim, case.bytes, bytes
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "the document and the compiler disagree in {} place(s):\n{}\n\n\
         One of the two is wrong. Work out which — never adjust the expectation \
         until it passes.",
        failures.len(),
        failures.join("\n")
    );
}

/// Every heading in the two sections is named by at least one case.
///
/// The same gate `tests/doc_coverage.rs` runs over the width document, and for
/// the same reason: the matrix above is transcribed, so a section that grows a
/// listing nobody transcribes would stay green. Reading the document's own
/// headings is what makes a new one fail until someone decides what it claims.
#[test]
fn every_compile_time_section_is_accounted_for() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("doc/syntax-reference.md");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));

    /// A heading whose body is prose with nothing to compile. Empty on
    /// purpose: every heading in these two sections makes a checkable claim,
    /// and the list exists so that adding one is a decision rather than an
    /// omission.
    const PROSE_ONLY: &[&str] = &[];

    let mut headings = Vec::new();
    for parent in [EVALUATOR, PREPROCESSOR] {
        let body = text
            .split_once(&format!("\n## {parent}\n"))
            .unwrap_or_else(|| panic!("`{parent}` is not a `##` heading of the document"))
            .1;
        let body = body.split("\n## ").next().unwrap_or(body);
        headings.push(parent);
        headings.extend(body.lines().filter_map(|line| line.strip_prefix("### ")));
    }

    let missing: Vec<&str> = headings
        .iter()
        .filter(|heading| {
            !CASES.iter().any(|case| case.section == **heading) && !PROSE_ONLY.contains(heading)
        })
        .copied()
        .collect();
    assert!(
        missing.is_empty(),
        "sections of the syntax reference's compile-time chapters that no case checks: \
         {missing:?}"
    );

    // And the reverse, so a case cannot name a heading the document lost.
    for case in CASES {
        assert!(
            headings.contains(&case.section),
            "`{}` is not a heading of either compile-time section",
            case.section
        );
    }
}

/// `print`'s formatting, which is a claim about a diagnostic's *text*.
///
/// "A number splices into a string with `+`, formatted as `%lf`, so
/// `print(\"n=\" + 8)` says `n=8.000000`." The six decimals are the original's
/// `printf` format and not a choice, so the assertion is on the string.
#[test]
fn a_number_spliced_into_a_string_carries_the_documented_format() {
    let mut map = SourceMap::new();
    let file = map.add_file("doc.k816", "data note {\n  [ print(\"n=\" + 8) ]\n}\n");
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let note = result
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_str() == "K816-N0145")
        .expect("the note the document says `print` reports");
    assert_eq!(note.title, "n=8.000000");
}

/// `evalfunc`'s unresolved call, which is a claim about a diagnostic's
/// *position*.
///
/// "the error lands on the definition rather than on a call site." A code
/// alone cannot tell the two apart — both are `K816-E0101` — so the span is
/// what the assertion is on: the definition is line 1 and the call site is
/// line 3.
#[test]
fn an_unresolved_evalfunc_call_is_reported_at_the_definition() {
    let source = "evalfunc up(q) [ down(q) ]\n\
                  evalfunc down(q) [ q ]\n\
                  data uses {\n  [ up(1) ]\n}\n";
    let mut map = SourceMap::new();
    let file = map.add_file("doc.k816", source);
    let result = compile_to_ca65(
        &map,
        file,
        &Options {
            optimize: false,
            ..Default::default()
        },
    );
    let report = result
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_str() == "K816-E0101")
        .expect("the unresolved call");
    let line = map.file(file).line_col(report.primary.span.start).line;
    // Zero-based: line 0 is `evalfunc up(q) [ down(q) ]`, and the call site
    // `[ up(1) ]` is line 3.
    assert_eq!(
        line, 0,
        "the report should land on `up`'s body, not on the `up(1)` below it"
    );
}
