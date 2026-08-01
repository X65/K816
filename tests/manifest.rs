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
//! Every syntax group in `plans/feature-manifest.md` §2 appears here with a
//! source snippet written in that syntax, and each snippet is parsed. A group
//! is covered when its own syntax parses — not when a corpus case with a
//! plausible name exists, which is a weaker claim that stays true after the
//! form itself is dropped.
//!
//! Each row also names the corpus case that pins the resulting tree, and that
//! case has to exist. The two checks answer different questions: the snippet
//! proves the form is *accepted*, the case proves its *shape* is reviewed.
//!
//! The manifest itself is not read at run time — `plans/` is deliberately not
//! committed — so the group names and their syntax are transcribed here.

use std::collections::HashSet;
use std::path::PathBuf;

use k816::parser::parse_source;
use k816::source::SourceMap;

/// What a snippet must do beyond parsing.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Expect {
    /// Parses with no syntax diagnostics.
    Parses,
    /// Parses *and compiles*: the group's semantics have landed, so a
    /// snippet in that syntax reaches emitted assembly with no errors.
    Compiles,
    /// Parses, and reports itself as reserved legacy syntax.
    Reserved,
}

/// One manifest §2 row: the group, a snippet in that syntax, what the snippet
/// must do, and the corpus case that pins its tree.
struct Row {
    group: &'static str,
    source: &'static str,
    expect: Expect,
    case: &'static str,
}

/// The rows, in manifest order.
const AUDIT: &[Row] = &[
    Row {
        group: "Line and block comments",
        source: "// note\n/* block */\nvar A = 0\n",
        expect: Expect::Parses,
        case: "Comment edge cases",
    },
    Row {
        group: "Identifiers",
        source: "var _leading = 0\nvar with9digits = 1\n",
        expect: Expect::Parses,
        case: "Identifiers use the full name namespace",
    },
    Row {
        group: "Numeric literals, binary",
        source: "var A = %1010\nvar B = 0b1010\n",
        expect: Expect::Parses,
        case: "Binary literal forms",
    },
    Row {
        group: "Numeric literals, hexadecimal",
        source: "var A = $2A\nvar B = 0x2A\n",
        expect: Expect::Parses,
        case: "Hexadecimal literal forms",
    },
    Row {
        group: "Character literals",
        source: "var A = 'A'\nvar B = '\\n'\n",
        expect: Expect::Parses,
        case: "Character literal escapes",
    },
    Row {
        group: "Float literals in evaluator brackets",
        source: "data d {\n  [ 1.5 + .5 ]\n}\n",
        expect: Expect::Parses,
        case: "The full evaluator operator table",
    },
    Row {
        group: "Expression operators, binary",
        source: "var A = B * C + D << E & F ^ G | H\n",
        expect: Expect::Parses,
        case: "Every binary operator token",
    },
    Row {
        group: "Expression operators, unary",
        source: "var A = -~FOO\n",
        expect: Expect::Parses,
        case: "Unary operators repeat in the order written",
    },
    Row {
        group: "Compound-expression position rules",
        source: "data d {\n  byte FOO 0 1\n}\n",
        expect: Expect::Parses,
        case: "Raw values and address byte operators",
    },
    Row {
        group: "`&&` address-of-vs-AND parse quirk",
        source: "var A = FOO && BAR\n",
        expect: Expect::Parses,
        case: "Infix double ampersand is the address-of gotcha",
    },
    Row {
        group: "`const` and enum-like groups",
        source: "const A = 0, B, C\n",
        expect: Expect::Parses,
        case: "Constant groups with enum-like auto-increment",
    },
    Row {
        group: "Fixed-address `var NAME = $addr`",
        source: "var SCREEN = $0400\n",
        expect: Expect::Parses,
        case: "Fixed-address var declaration",
    },
    Row {
        group: "Allocated `var NAME`",
        source: "var bar\n",
        expect: Expect::Parses,
        case: "Linker-allocated variables have no initializer",
    },
    Row {
        group: "Storage-class prefixes",
        source: "var dp counter\nvar abs slot\nvar far handler = $030000\n",
        expect: Expect::Parses,
        case: "Storage class prefixes",
    },
    Row {
        group: "Typed width suffixes",
        source: "var w:word = $2000\nvar p:far = $3000\n",
        expect: Expect::Parses,
        case: "Typed width suffixes",
    },
    Row {
        group: "Array declaration `var NAME[N]`",
        source: "var buffer[32] = $0100\n",
        expect: Expect::Parses,
        case: "Array spans and allocation counts",
    },
    Row {
        group: "Allocation count `* count`",
        source: "var simple:byte * 32\n",
        expect: Expect::Parses,
        case: "Array spans and allocation counts",
    },
    Row {
        group: "Repeat element accessors",
        source: "func f {\n  lda #COMP[2].str[3]\n}\n",
        expect: Expect::Parses,
        case: "Field access and repeat element accessors",
    },
    Row {
        group: "Symbolic subscript field lists",
        source: "var T[\n  .sp :word\n  .data[\n    .lo :byte\n  ]\n] = $4000\n",
        expect: Expect::Parses,
        case: "Symbolic subscript field list",
    },
    Row {
        group: "Symbolic subscript access",
        source: "func f {\n  lda #T.sp\n  lda #T[.sp]\n}\n",
        expect: Expect::Parses,
        case: "Field access and repeat element accessors",
    },
    Row {
        group: "`abstract var` layouts",
        source: "abstract var D[\n  .open :word\n]\n",
        expect: Expect::Parses,
        case: "Abstract layout declarations",
    },
    Row {
        group: "Metadata query suffixes",
        source: "func f {\n  lda #T:sizeof\n  lda #T.sp:offsetof\n}\n",
        expect: Expect::Parses,
        case: "Metadata query suffixes",
    },
    Row {
        group: "Address-encoding operand prefixes",
        source: "func f {\n  lda dp addr\n  lda abs addr\n  lda far addr\n}\n",
        expect: Expect::Parses,
        case: "Address-encoding operand prefixes",
    },
    Row {
        group: "`segment NAME` selection",
        source: "segment fixed_lo\n",
        expect: Expect::Parses,
        case: "Segment selection",
    },
    Row {
        group: "Global labels",
        source: "func f {\n  hello:\n}\n",
        expect: Expect::Parses,
        case: "Global and local labels",
    },
    Row {
        group: "Local labels",
        source: "func f {\n  .loop:\n}\n",
        expect: Expect::Parses,
        case: "Global and local labels",
    },
    Row {
        group: "Label+offset assignment targets",
        source: "func f {\n  .LT+1 = a\n}\n",
        expect: Expect::Parses,
        case: "Label plus offset assignment targets",
    },
    Row {
        group: "`func NAME { }`",
        source: "func inc_x { x++ }\n",
        expect: Expect::Parses,
        case: "Every code section form",
    },
    Row {
        group: "Width annotations on functions",
        source: "func main @a8 @i16 { }\n",
        expect: Expect::Compiles,
        case: "Empty function with width annotations",
    },
    Row {
        group: "Module-level default widths",
        source: "@a8\n@i16\nfunc main { }\n",
        expect: Expect::Compiles,
        case: "Module-level default widths",
    },
    Row {
        group: "Scoped width blocks",
        source: "func f {\n  @a16 { lda #1 }\n}\n",
        expect: Expect::Compiles,
        case: "Scoped width blocks",
    },
    Row {
        group: "Bare mode-set statements",
        source: "func f {\n  @a16\n  lda #1\n}\n",
        expect: Expect::Compiles,
        case: "Bare mode-set statements",
    },
    Row {
        group: "Function call contracts, declaration",
        source: "func add @a16 (a, x) -> @a8 a, y {\n  sep #$20\n}\n",
        expect: Expect::Compiles,
        case: "Call contracts on a section header",
    },
    Row {
        group: "Function call contracts, call site",
        source: "func add (a, x) -> a, y {\n  nop\n}\n\nfunc f {\n  add a, x -> a, y\n}\n",
        expect: Expect::Compiles,
        case: "Call-site contracts",
    },
    Row {
        group: "Checked bare calls",
        source: "func f {\n  inc_x\n  call add\n}\n",
        expect: Expect::Parses,
        case: "Call statements",
    },
    Row {
        group: "`naked` functions",
        source: "naked dli { return_i }\n",
        expect: Expect::Parses,
        case: "Every code section form",
    },
    Row {
        group: "`inline` functions with aliases",
        source: "inline scale @a16 (a, #factor) -> a { }\n",
        expect: Expect::Parses,
        case: "Call contracts on a section header",
    },
    Row {
        group: "`far func` and the `far NAME` call",
        source: "far func long { x++ }\nfunc f {\n  far long\n}\n",
        expect: Expect::Parses,
        case: "The keyword-less far call",
    },
    Row {
        group: "`call far` cross-unit calls",
        source: "func f {\n  call far lib_init\n}\n",
        expect: Expect::Parses,
        case: "Call statements",
    },
    Row {
        group: "HLA register loads",
        source: "func f {\n  a = #1\n  a = mem,x\n  a = (mem),y\n}\n",
        expect: Expect::Parses,
        case: "Loads in every operand shape",
    },
    Row {
        group: "HLA stores",
        source: "func f {\n  mem = a\n  mem,x = a\n  (mem),y = a\n  mem = x\n}\n",
        expect: Expect::Parses,
        case: "Stores in every operand shape",
    },
    Row {
        group: "Assignment chaining",
        source: "func f {\n  dst0 = dst1 = a = src\n}\n",
        expect: Expect::Parses,
        case: "Assignment chaining",
    },
    Row {
        group: "Register-to-register transfers",
        source: "func f {\n  x = a\n  d = c\n  s = x = y\n}\n",
        expect: Expect::Parses,
        case: "Register to register transfers",
    },
    Row {
        group: "Zero-store shortcut",
        source: "func f {\n  mem = 0\n  mem,x = 0\n}\n",
        expect: Expect::Parses,
        case: "Stores in every operand shape",
    },
    Row {
        group: "HLA arithmetic and compares",
        source: "func f {\n  a+mem\n  a-#1\n  a?10\n  x?#1\n}\n",
        expect: Expect::Parses,
        case: "Arithmetic and logic shorthands",
    },
    Row {
        group: "HLA bit test",
        source: "func f {\n  a&?READY\n}\n",
        expect: Expect::Parses,
        case: "Compares and the bit test",
    },
    Row {
        group: "HLA shifts and rotates",
        source: "func f {\n  a<<\n  mem,x>>>\n}\n",
        expect: Expect::Parses,
        case: "Shifts and rotates",
    },
    Row {
        group: "HLA increment and decrement",
        source: "func f {\n  x++\n  mem,x--\n}\n",
        expect: Expect::Parses,
        case: "Increment and decrement",
    },
    Row {
        group: "Flag shorthand ops",
        source: "func f {\n  c+ c- d+ d- i+ i- v-\n}\n",
        expect: Expect::Parses,
        case: "Flag shorthands",
    },
    Row {
        group: "Stack shorthands",
        source: "func f {\n  a!! a?? p!! b!! d??\n}\n",
        expect: Expect::Parses,
        case: "Stack shorthands",
    },
    Row {
        group: "`b~a` / `a~b` XBA",
        source: "func f {\n  b~a\n  a~b\n}\n",
        expect: Expect::Parses,
        case: "Accumulator exchange",
    },
    Row {
        group: "`return` / `return_i`",
        source: "func f {\n  return\n}\nnaked g {\n  return_i\n}\n",
        expect: Expect::Parses,
        case: "Call statements",
    },
    Row {
        group: "Branch operator mapping",
        source: "func f {\n  { } <\n  { } >=0\n  { } c-?\n  { } <<=\n}\n",
        expect: Expect::Parses,
        case: "Every branch condition spelling",
    },
    Row {
        group: "Conditional prefix form",
        source: "func f {\n  >={ x++ }\n  c-?{ x++ }\n}\n",
        expect: Expect::Parses,
        case: "Prefix conditional form",
    },
    Row {
        group: "Conditional goto form",
        source: "func f {\n  < goto done\n  c-? goto done\n}\n",
        expect: Expect::Parses,
        case: "Conditional goto, break, and repeat",
    },
    Row {
        group: "Postfix loop form",
        source: "func f {\n  { x-- } !=\n}\n",
        expect: Expect::Parses,
        case: "Postfix loop form",
    },
    Row {
        group: "`always` / `never` loops",
        source: "func f {\n  { } always\n  { } never\n}\n",
        expect: Expect::Parses,
        case: "Postfix loop form",
    },
    Row {
        group: "`break` / `repeat`",
        source: "func f {\n  break\n  repeat\n  < break\n  != repeat\n}\n",
        expect: Expect::Parses,
        case: "Conditional goto, break, and repeat",
    },
    Row {
        group: "`else`",
        source: "func f {\n  =={ a = #1 } else { a = #2 }\n}\n",
        expect: Expect::Parses,
        case: "If-else",
    },
    Row {
        group: "`goto` and `call`",
        source: "func f {\n  goto label\n  goto (vec)\n  goto far label\n  call label\n}\n",
        expect: Expect::Parses,
        case: "Flow control instructions",
    },
    Row {
        group: "`nocross` code wrappers",
        source: "func f {\n  nocross { x++ } !=\n  nocross { y++ }\n}\n",
        expect: Expect::Parses,
        case: "nocross code wrappers",
    },
    Row {
        group: "Wait-loop patterns",
        source: "func f {\n  { a&?READY } n-?\n}\n",
        expect: Expect::Parses,
        case: "A wait loop",
    },
    Row {
        group: "Efficiency warnings for `>` / `<=`",
        source: "func f {\n  { x++ } >\n  { x++ } <=\n}\n",
        expect: Expect::Parses,
        case: "Every branch condition spelling",
    },
    Row {
        group: "NOP shorthand",
        source: "func f {\n  *\n  * 5\n  inx * 3\n}\n",
        expect: Expect::Parses,
        case: "NOP and break shorthands",
    },
    Row {
        group: "`%` BRK shorthand",
        source: "func f {\n  %\n  brk #0\n}\n",
        expect: Expect::Parses,
        case: "A binary literal is not the break shorthand",
    },
    Row {
        group: "Raw 6502 mnemonics",
        source: "func f {\n  adc x0 and x0 asl a bit x0 cmp x0 eor x0 lsr a ora x0 rol a sbc x0\n}\n",
        expect: Expect::Parses,
        case: "Every classical mnemonic is recognized",
    },
    Row {
        group: "Classical operand shapes",
        source: "func f {\n  nop\n  asl a\n  lda #1\n  lda addr,x\n  lda (ptr,x)\n  lda (ptr),y\n  jmp (vec)\n}\n",
        expect: Expect::Parses,
        case: "Classical operand shapes",
    },
    Row {
        group: "65816 stack-relative operands",
        source: "func f {\n  lda 4,s\n  lda (4,s),y\n}\n",
        expect: Expect::Parses,
        case: "Stack-relative operands",
    },
    Row {
        group: "65816 indirect long operands",
        source: "func f {\n  lda [ptr]\n  lda [ptr],y\n}\n",
        expect: Expect::Parses,
        case: "Indirect long operands",
    },
    Row {
        group: "65816 indexed-offset marker",
        source: "func f {\n  lda &FIELD, x\n  lda abs &FIELD, x\n}\n",
        expect: Expect::Parses,
        case: "The indexed-offset marker",
    },
    Row {
        group: "65C02 additions",
        source: "func f {\n  bra t phx phy plx ply stz a0 tsb a0 trb a0 wai stp\n}\n",
        expect: Expect::Parses,
        case: "65C02 additions",
    },
    Row {
        group: "65816 bank/mode mnemonics",
        source: "func f {\n  phb plb phd pld phk xce\n}\n",
        expect: Expect::Parses,
        case: "65816 bank registers and mode switch",
    },
    Row {
        group: "65816 control flow/mode/stack",
        source: "func f {\n  brl t jml t jsl t rtl pea #1 pei (p) per t cop #2 wdm #3 rep #$10 sep #$20\n}\n",
        expect: Expect::Parses,
        case: "65816 control flow, mode and stack",
    },
    Row {
        group: "65816 transfer mnemonics",
        source: "func f {\n  tcd tcs tdc tsc txy tyx xba\n}\n",
        expect: Expect::Parses,
        case: "65816 transfer instructions",
    },
    Row {
        group: "Block moves",
        source: "func f {\n  mvn $01,$02\n  mvp src,dst\n}\n",
        expect: Expect::Parses,
        case: "Block moves take two operands",
    },
    Row {
        group: "Data blocks, named and anonymous",
        source: "data D { 1 2 }\ndata { 3 4 }\n",
        expect: Expect::Parses,
        case: "Named and anonymous data blocks",
    },
    Row {
        group: "`segment` placement rule in data",
        source: "data D {\n  segment VECTORS\n  word 0\n}\n",
        expect: Expect::Parses,
        case: "Placement directives",
    },
    Row {
        group: "Placement directives",
        source: "data D {\n  align 16\n  address $5000\n  1\n}\n",
        expect: Expect::Parses,
        case: "Placement directives",
    },
    Row {
        group: "`nocross` in data blocks",
        source: "nocross data W { 1 2 }\ndata D {\n  nocross\n  nocross 8\n  1\n}\n",
        expect: Expect::Parses,
        case: "The nocross data wrapper",
    },
    Row {
        group: "Raw bytes and labels in data",
        source: "data D {\n  hello: 1 2 3\n  \"HI\" 0\n}\n",
        expect: Expect::Parses,
        case: "Raw values and address byte operators",
    },
    Row {
        group: "Address byte operators",
        source: "data D {\n  &<ptr &>ptr &&ptr &&&ptr\n  &<ptr+1\n}\n",
        expect: Expect::Parses,
        case: "An evaluator entry is not part of the value before it",
    },
    Row {
        group: "`?` undefined byte placeholder",
        source: "data D {\n  byte $7F ? ?\n  ?\n}\n",
        expect: Expect::Parses,
        case: "Typed entries",
    },
    Row {
        group: "Typed data entries",
        source: "data D {\n  byte 1\n  word V\n  far H\n}\n",
        expect: Expect::Parses,
        case: "Typed entries",
    },
    Row {
        group: "String data and `charset`",
        source: "data D {\n  charset \".ABC \"\n  \"AB\" 0\n}\n",
        expect: Expect::Parses,
        case: "charset, code subblocks, and repeats",
    },
    Row {
        group: "`code { }` subblocks",
        source: "data D {\n  code { a=1 }\n  nocross code { y=3 }\n}\n",
        expect: Expect::Parses,
        case: "charset, code subblocks, and repeats",
    },
    Row {
        group: "`repeat N { }` data repetition",
        source: "data D {\n  repeat 3 { 4 }\n}\n",
        expect: Expect::Parses,
        case: "charset, code subblocks, and repeats",
    },
    Row {
        group: "`for ... eval` generated data",
        source: "data D {\n  for x=0..213 eval [ x * 2 ]\n}\n",
        expect: Expect::Parses,
        case: "Generated data from an evaluator range",
    },
    Row {
        group: "`image` in data blocks",
        source: "data D {\n  image sprites 0 0 8> 16v tiles 8 0 31 inv\n}\n",
        expect: Expect::Reserved,
        case: "Reserved image forms are named nodes, never parse errors",
    },
    Row {
        group: "`binary` in data blocks",
        source: "data D {\n  binary \"t.bin\"\n}\n",
        expect: Expect::Parses,
        case: "Binary inclusion",
    },
    Row {
        group: "Top-level `image` and `binary`",
        source: "image S = \"s.bmp\"\nbinary B = \"p.bin\"\n",
        expect: Expect::Reserved,
        case: "Reserved image forms are named nodes, never parse errors",
    },
    Row {
        group: "Evaluator brackets",
        source: "var A = [$FF]\nfunc f {\n  a = [SIZE]\n}\n",
        expect: Expect::Parses,
        case: "Evaluator brackets are expression atoms",
    },
    Row {
        group: "Top-level evaluator blocks",
        source: "[ FOUR = 4, FIVE = FOUR+1 ]\n",
        expect: Expect::Parses,
        case: "Generated data from an evaluator range",
    },
    Row {
        group: "Evaluator operator set",
        source: "data D {\n  [ A ? B : C ]\n  [ A && B || C ]\n  [ 1 / 2 % 3 ]\n  [ N = 1, M += 2 ]\n}\n",
        expect: Expect::Parses,
        case: "The full evaluator operator table",
    },
    Row {
        group: "Evaluator math functions",
        source: "data D {\n  [ clamp(sin(x), 0, 1) ]\n}\n",
        expect: Expect::Parses,
        case: "The full evaluator operator table",
    },
    Row {
        group: "Evaluator side-effect functions",
        source: "data D {\n  [ 1 + size(section) ]\n  [ rnd() ]\n}\n",
        expect: Expect::Reserved,
        case: "Reserved evaluator functions",
    },
    Row {
        group: "Preprocessor directives",
        source: "#if FOO\nvar A = 1\n#elif BAR\n#else\n#endif\n#error \"x\"\n#warn \"y\"\n",
        expect: Expect::Parses,
        case: "Preprocessor directives at the top level",
    },
    Row {
        group: "Optional semicolons and empty statements",
        source: "var A = 0; var B = 1;;\n;\n",
        expect: Expect::Parses,
        case: "Semicolons are optional separators and empty statements are ignored",
    },
    Row {
        group: "Case-insensitive registers and flags",
        source: "func f {\n  A = #0\n  X = A\n  D = C\n}\n",
        expect: Expect::Parses,
        case: "Registers are case-insensitive",
    },
    Row {
        group: "Variable+offset addressing",
        source: "func f {\n  a = base+3\n  lda base-2,x\n}\n",
        expect: Expect::Parses,
        case: "Compound operand expressions",
    },
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
fn every_manifest_syntax_group_parses() {
    // The audit proper: each group's own syntax has to be accepted by the
    // grammar. A form that is dropped fails here, which naming a corpus case
    // cannot detect.
    let mut failures = Vec::new();
    for row in AUDIT {
        let mut map = SourceMap::new();
        let id = map.add_file("audit.k65", row.source);
        let parsed = parse_source(map.file(id), None);
        if !parsed.diagnostics.is_empty() {
            failures.push(format!(
                "  {}\n    syntax diagnostics: {:?}\n    source: {:?}",
                row.group,
                parsed
                    .diagnostics
                    .iter()
                    .map(|diagnostic| diagnostic.title.as_str())
                    .collect::<Vec<_>>(),
                row.source
            ));
            continue;
        }
        if row.expect == Expect::Compiles {
            // The group's semantics have landed, so its own syntax has to
            // survive the whole pipeline rather than merely parse.
            let result = k816::compile_to_ca65(&map, id, &k816::Options { optimize: false });
            let errors: Vec<&str> = result
                .diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.is_error())
                .map(|diagnostic| diagnostic.code.as_str())
                .collect();
            if !errors.is_empty() {
                failures.push(format!(
                    "  {}\n    expected it to compile, got {errors:?}\n    source: {:?}",
                    row.group, row.source
                ));
            }
            continue;
        }
        if row.expect != Expect::Reserved {
            continue;
        }
        // A reserved group must additionally report itself through the
        // `K816-K65-####` family, wherever in the tree it sits.
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        let reserved = diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_str().starts_with("K816-K65-"));
        if !reserved {
            failures.push(format!(
                "  {}\n    expected a K816-K65-#### diagnostic, got {:?}",
                row.group,
                diagnostics
                    .iter()
                    .map(|diagnostic| diagnostic.code.as_str())
                    .collect::<Vec<_>>()
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "feature-manifest groups whose syntax does not parse:\n{}",
        failures.join("\n")
    );
}

#[test]
fn every_manifest_syntax_group_has_a_corpus_case() {
    // The second half: the tree each group produces is pinned by a named
    // corpus case, so its *shape* is reviewed and not only its acceptance.
    let names = corpus_case_names();
    let missing: Vec<String> = AUDIT
        .iter()
        .filter(|row| !names.contains(row.case))
        .map(|row| format!("  {}\n    expected corpus case: `{}`", row.group, row.case))
        .collect();
    assert!(
        missing.is_empty(),
        "feature-manifest groups whose corpus case is missing or renamed:\n{}",
        missing.join("\n")
    );
}

/// The rows sub-plan C implements, and a snippet that must now *compile*
/// rather than merely parse.
///
/// This is the audit's second axis. Parsing proves the grammar recognizes a
/// form; it says nothing about whether the compiler does anything with it,
/// and a group can sit "covered" for a whole sub-plan while every use of it
/// reports itself as not-yet-implemented. Each row here is a
/// `native-translator` group whose semantics have landed, so the claim is
/// checked at the level the manifest actually makes it.
const COMPILES: &[(&str, &str)] = &[
    (
        "Character literals",
        "const A = 'A'\nconst NEXT = 'A' + 1\n",
    ),
    (
        "Expression operators, binary",
        "const A = $80\nconst B = A | 1 & 3 ^ 2 + 1 * 2 << 1 >> 1\n",
    ),
    ("Expression operators, unary", "const A = -~5\n"),
    (
        "`const` and enum-like auto-increment groups",
        "const A = 0, B, C\nconst D, E\n",
    ),
    (
        "Address-encoding operand prefixes and auto-shrink",
        "var ZP = $80\nfunc f @a8 { lda ZP  lda dp ZP  lda abs ZP  lda far ZP }\n",
    ),
    (
        "`segment NAME` selection",
        "segment FAST\nfunc f @a8 { nop }\n",
    ),
    ("Global labels", "func f @a8 {\n  spot:\n    bne spot\n}\n"),
    (
        "Local labels (function-scoped)",
        "func one @a8 {\n  .loop:\n    bne .loop\n}\nfunc two @a8 {\n  .loop:\n    bne .loop\n}\n",
    ),
    (
        "65816 operand additions",
        "var ZP = $80\nconst F = 2\nfunc f @a8 @i16 { lda 4,s  lda (4,s),y  lda [ZP]  lda [ZP],y  lda &F,x }\n",
    ),
    (
        "65C02 additions",
        "var ZP = $80\nfunc f @a8 {\n  spot:\n    bra spot\n    phx\n    ply\n    stz ZP\n    tsb ZP\n    trb ZP\n}\n",
    ),
    (
        "65816 transfer raw mnemonics",
        "func f @a8 { tcd  tcs  tdc  tsc  txy  tyx  xba }\n",
    ),
    (
        "65816 bank/mode raw mnemonics",
        "func f @a8 { phb  plb  phd  pld  phk  xce }\n",
    ),
    (
        "65816 control flow, mode and stack raw mnemonics",
        "var ZP = $80\nfunc f @a8 {\n  spot:\n    brl spot\n    rep #$20\n    sep #$20\n    pea #$1234\n    pei (ZP)\n    per spot\n    cop #$7F\n    wdm #$00\n}\n",
    ),
    ("Block moves", "func f @a8 { mvn $01,$02  mvp $01,$02 }\n"),
    (
        "Data blocks, named and anonymous",
        "data named {\n    1 2\n}\ndata {\n    3 4\n}\n",
    ),
    (
        "`segment` placement rule inside data blocks",
        "data d {\n    segment OTHER\n    1 2\n}\n",
    ),
    (
        "Placement directives `align N`",
        "data d {\n    align 16\n    1 2\n}\n",
    ),
    (
        "Raw byte values and labels in data blocks",
        "data d {\n    $7F 1\n    inner:\n    2 3\n}\n",
    ),
    (
        "Address byte operators",
        "var ptr = $2345\ndata d {\n    &<ptr &>ptr\n    &&ptr\n    &&&ptr\n}\n",
    ),
    ("`?` undefined byte placeholder", "data d {\n    ? ?\n}\n"),
    (
        "Optional semicolons and empty statements",
        "var A = $10;\nvar B = $20;\n",
    ),
    (
        "Variable+offset compile-time address arithmetic",
        "var base = $0400\nfunc f @a8 { lda base + 3 }\n",
    ),
];

/// The audit tracks *semantic* coverage as it lands, not only syntactic:
/// a group whose row says the native translator owns it has to compile.
#[test]
fn every_implemented_manifest_group_compiles() {
    let failures: Vec<String> = COMPILES
        .iter()
        .filter_map(|(group, source)| {
            let mut map = SourceMap::new();
            let id = map.add_file("audit.k65", *source);
            let parsed = parse_source(map.file(id), None);
            if !parsed.diagnostics.is_empty() {
                return Some(format!("  {group}\n    did not parse: {source:?}"));
            }
            let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
            let errors: Vec<&str> = diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.is_error())
                .map(|diagnostic| diagnostic.code.as_str())
                .collect();
            (!errors.is_empty())
                .then(|| format!("  {group}\n    {errors:?}\n    source: {source:?}"))
        })
        .collect();
    assert!(
        failures.is_empty(),
        "feature-manifest groups the native translator owns but does not compile:\n{}",
        failures.join("\n")
    );
}

/// No `native-translator` row may still report itself as not-yet-implemented.
///
/// The manifest contract is that the owner column means something; a group
/// C owns that still names sub-plan C in a diagnostic is a row that was
/// counted and not done.
#[test]
fn no_implemented_group_still_names_the_native_translator() {
    let failures: Vec<String> = COMPILES
        .iter()
        .filter_map(|(group, source)| {
            let mut map = SourceMap::new();
            let id = map.add_file("audit.k65", *source);
            let parsed = parse_source(map.file(id), None);
            let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
            diagnostics
                .iter()
                .any(|diagnostic| {
                    diagnostic
                        .notes
                        .iter()
                        .any(|note| note.contains("sub-plan C"))
                })
                .then(|| format!("  {group}\n    still defers to sub-plan C"))
        })
        .collect();
    assert!(
        failures.is_empty(),
        "groups C owns that still defer to C:\n{}",
        failures.join("\n")
    );
}
