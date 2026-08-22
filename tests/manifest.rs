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
    /// Parses *and compiles*: the group's semantics have landed, so a
    /// snippet in that syntax reaches emitted assembly with no errors.
    Compiles,
    /// Parses, and defers to a sub-plan that is not E — every remaining one
    /// is the evaluator, the preprocessor, or `binary` inclusion.
    ///
    /// This replaced a plain "parses" once sub-plan E finished: a row that
    /// only parsed used to mean "nobody has done this yet", which was true
    /// of most of the language and said nothing about who would. Naming the
    /// owner makes the row a claim that can go stale — when G lands, these
    /// fail and someone flips them.
    Deferred,
    /// Parses, and reports itself as reserved legacy syntax.
    Reserved,
    /// Parses, and answers `NOT_YET_IMPLEMENTED` from the AST builder — the
    /// grammar names the construct and no task has given it a meaning.
    ///
    /// This is sub-plan G1's state and only G1's: G1 is one grammar
    /// regeneration with no semantics attached, so every construct it teaches
    /// the parser is announced rather than silently dropped. Each row here
    /// becomes `Compiles` or `Reserved` as its task lands — G2 for the
    /// refusals and the declaration forms, G4 for the evaluator, G5 for the
    /// preprocessor, G6 for `evaluator [ ]` — and G10 is where none may be
    /// left.
    Parses,
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
        expect: Expect::Compiles,
        case: "Comment edge cases",
    },
    Row {
        group: "Identifiers",
        source: "var _leading = 0\nvar with9digits = 1\n",
        expect: Expect::Compiles,
        case: "Identifiers use the full name namespace",
    },
    Row {
        group: "Numeric literals, binary",
        source: "var A = %1010\nvar B = 0b1010\n",
        expect: Expect::Compiles,
        case: "Binary literal forms",
    },
    Row {
        group: "Numeric literals, hexadecimal",
        source: "var A = $2A\nvar B = 0x2A\n",
        expect: Expect::Compiles,
        case: "Hexadecimal literal forms",
    },
    Row {
        group: "Character literals",
        source: "var A = 'A'\nvar B = '\\n'\n",
        expect: Expect::Compiles,
        case: "Character literal escapes",
    },
    Row {
        group: "Float literals in evaluator brackets",
        source: "data d {\n  [ 1.5 + .5 ]\n}\n",
        expect: Expect::Deferred,
        case: "The full evaluator operator table",
    },
    Row {
        group: "Expression operators, binary",
        source: "const B = 1\nconst C = 2\nconst D = 3\nconst E = 1\nconst F = 4\nconst G = 5\nconst H = 6\nvar A = B * C + D << E & F ^ G | H\n",
        expect: Expect::Compiles,
        case: "Every binary operator token",
    },
    Row {
        group: "Expression operators, unary",
        source: "const FOO = 1\nvar A = -~FOO\n",
        expect: Expect::Compiles,
        case: "Unary operators repeat in the order written",
    },
    Row {
        group: "Compound-expression position rules",
        source: "const FOO = 1\ndata d {\n  byte FOO 0 1\n}\n",
        expect: Expect::Compiles,
        case: "Raw values and address byte operators",
    },
    Row {
        group: "`&&` address-of-vs-AND parse quirk",
        source: "const FOO = 1\nconst BAR = 2\nvar A = FOO && BAR\n",
        expect: Expect::Compiles,
        case: "Infix double ampersand is the address-of gotcha",
    },
    Row {
        group: "`const` and enum-like groups",
        source: "const A = 0, B, C\n",
        expect: Expect::Compiles,
        case: "Constant groups with enum-like auto-increment",
    },
    Row {
        group: "Fixed-address `var NAME = $addr`",
        source: "var SCREEN = $0400\n",
        expect: Expect::Compiles,
        case: "Fixed-address var declaration",
    },
    Row {
        group: "Allocated `var NAME`",
        source: "var bar\n",
        expect: Expect::Compiles,
        case: "Linker-allocated variables have no initializer",
    },
    Row {
        group: "Storage-class prefixes",
        source: "var dp counter\nvar abs slot\nvar far handler = $030000\n",
        expect: Expect::Compiles,
        case: "Storage class prefixes",
    },
    Row {
        group: "Typed width suffixes",
        source: "var w:word = $2000\nvar p:far = $3000\n",
        expect: Expect::Compiles,
        case: "Typed width suffixes",
    },
    Row {
        group: "Array declaration `var NAME[N]`",
        source: "var buffer[32] = $0100\n",
        expect: Expect::Compiles,
        case: "Array spans and allocation counts",
    },
    Row {
        group: "Allocation count `* count`",
        source: "var simple:byte * 32\n",
        expect: Expect::Compiles,
        case: "Array spans and allocation counts",
    },
    Row {
        group: "Repeat element accessors",
        source: "var COMP[\n  .str :byte [5]\n] * 4 = $1000\nfunc f @a8 {\n  lda COMP[2].str[3]\n}\n",
        expect: Expect::Compiles,
        case: "Field access and repeat element accessors",
    },
    Row {
        group: "Symbolic subscript field lists",
        source: "var T[\n  .sp :word\n  .data[\n    .lo :byte\n  ]\n] = $4000\n",
        expect: Expect::Compiles,
        case: "Symbolic subscript field list",
    },
    Row {
        group: "Symbolic subscript access",
        source: "var T[\n  .sp :word\n] = $4000\nfunc f @a16 {\n  lda #T.sp\n  lda #T[.sp]\n}\n",
        expect: Expect::Compiles,
        case: "Field access and repeat element accessors",
    },
    Row {
        group: "`abstract var` layouts",
        source: "abstract var D[\n  .open :word\n]\n",
        expect: Expect::Compiles,
        case: "Abstract layout declarations",
    },
    Row {
        group: "Metadata query suffixes",
        source: "var T[\n  .sp :word\n] = $4000\nfunc f @a8 {\n  lda #T:sizeof\n  lda #T.sp:offsetof\n}\n",
        expect: Expect::Compiles,
        case: "Metadata query suffixes",
    },
    Row {
        group: "Address-encoding operand prefixes",
        source: "var dp addr = $10\nfunc f @a8 @i8 {\n  lda dp addr\n  lda abs addr\n  lda far addr\n}\n",
        expect: Expect::Compiles,
        case: "Address-encoding operand prefixes",
    },
    Row {
        group: "`segment NAME` selection",
        source: "segment fixed_lo\n",
        expect: Expect::Compiles,
        case: "Segment selection",
    },
    Row {
        group: "Global labels",
        source: "func f {\n  hello:\n}\n",
        expect: Expect::Compiles,
        case: "Global and local labels",
    },
    Row {
        group: "Local labels",
        source: "func f {\n  .loop:\n}\n",
        expect: Expect::Compiles,
        case: "Global and local labels",
    },
    Row {
        group: "Label+offset assignment targets",
        source: "func f @a8 @i8 {\n  .LT:\n  nop\n  .LT+1 = a\n}\n",
        expect: Expect::Compiles,
        case: "Label plus offset assignment targets",
    },
    Row {
        group: "`func NAME { }`",
        source: "func inc_x { x++ }\n",
        expect: Expect::Compiles,
        case: "Every code section form",
    },
    Row {
        group: "Width annotations on functions",
        source: "func work @a8 @i16 { }\n",
        expect: Expect::Compiles,
        case: "Empty function with width annotations",
    },
    Row {
        group: "Module-level default widths",
        source: "@a8\n@i16\nfunc work { }\n",
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
        source: "func inc_x @a8 @i8 { nop }\nfunc add @a8 @i8 { nop }\nfunc f @a8 @i8 {\n  inc_x\n  call add\n}\n",
        expect: Expect::Compiles,
        case: "Call statements",
    },
    Row {
        group: "`naked` functions",
        source: "naked dli @a8 @i8 { return_i }\n",
        expect: Expect::Compiles,
        case: "Every code section form",
    },
    Row {
        group: "`inline` functions with aliases",
        source: "inline scale @a16 (a, #factor) -> a { lda #factor }\n\
                 func f @a16 @i8 {\n  scale a, #16 -> a\n}\n",
        expect: Expect::Compiles,
        case: "Call contracts on a section header",
    },
    Row {
        group: "`far func` and the `far NAME` call",
        source: "far func long @a8 @i8 { x++ }\nfunc f @a8 @i8 {\n  far long\n}\n",
        expect: Expect::Compiles,
        case: "The keyword-less far call",
    },
    Row {
        group: "`call far` cross-unit calls",
        source: "far func lib_init @a8 @i8 { nop }\nfunc f @a8 @i8 {\n  call far lib_init\n}\n",
        expect: Expect::Compiles,
        case: "Call statements",
    },
    Row {
        group: "HLA register loads",
        source: "var mem = $0400\nvar dp zp = $10\nfunc f @a8 @i8 {\n  a = #1\n  a = mem,x\n  a = (zp),y\n}\n",
        expect: Expect::Compiles,
        case: "Loads in every operand shape",
    },
    Row {
        group: "HLA stores",
        source: "var mem = $0400\nvar dp zp = $10\nfunc f @a8 @i8 {\n  mem = a\n  mem,x = a\n  (zp),y = a\n  mem = x\n}\n",
        expect: Expect::Compiles,
        case: "Stores in every operand shape",
    },
    Row {
        group: "Assignment chaining",
        source: "var dst0 = $0410\nvar dst1 = $0411\nvar src = $0412\nfunc f @a8 @i8 {\n  dst0 = dst1 = a = src\n}\n",
        expect: Expect::Compiles,
        case: "Assignment chaining",
    },
    Row {
        group: "Register-to-register transfers",
        source: "func f {\n  x = a\n  d = c\n  s = x = y\n}\n",
        expect: Expect::Compiles,
        case: "Register to register transfers",
    },
    Row {
        group: "Zero-store shortcut",
        source: "var mem = $0400\nfunc f @a8 @i8 {\n  mem = 0\n  mem,x = 0\n}\n",
        expect: Expect::Compiles,
        case: "Stores in every operand shape",
    },
    Row {
        group: "HLA arithmetic and compares",
        source: "var mem = $0400\nfunc f @a8 @i8 {\n  a+mem\n  a-#1\n  a?10\n  x?#1\n}\n",
        expect: Expect::Compiles,
        case: "Arithmetic and logic shorthands",
    },
    Row {
        group: "HLA bit test",
        source: "const READY = $80\nfunc f @a8 @i8 {\n  a&?READY\n}\n",
        expect: Expect::Compiles,
        case: "Compares and the bit test",
    },
    Row {
        group: "HLA shifts and rotates",
        source: "var mem = $0400\nfunc f @a8 @i8 {\n  a<<\n  mem,x>>>\n}\n",
        expect: Expect::Compiles,
        case: "Shifts and rotates",
    },
    Row {
        group: "HLA increment and decrement",
        source: "var mem = $0400\nfunc f @a8 @i8 {\n  x++\n  mem,x--\n}\n",
        expect: Expect::Compiles,
        case: "Increment and decrement",
    },
    Row {
        group: "Flag shorthand ops",
        source: "func f {\n  c+ c- d+ d- i+ i- v-\n}\n",
        expect: Expect::Compiles,
        case: "Flag shorthands",
    },
    Row {
        group: "Stack shorthands",
        source: "func f {\n  a!! a?? p!! b!! d??\n}\n",
        expect: Expect::Compiles,
        case: "Stack shorthands",
    },
    Row {
        group: "`b~a` / `a~b` XBA",
        source: "func f {\n  b~a\n  a~b\n}\n",
        expect: Expect::Compiles,
        case: "Accumulator exchange",
    },
    Row {
        group: "`return` / `return_i`",
        source: "func f @a8 @i8 {\n  return\n}\nnaked g @a8 @i8 {\n  return_i\n}\n",
        expect: Expect::Compiles,
        case: "Call statements",
    },
    Row {
        group: "Branch operator mapping",
        source: "func f {\n  { } <\n  { } >=0\n  { } c-?\n  { } <<=\n}\n",
        expect: Expect::Compiles,
        case: "Every branch condition spelling",
    },
    Row {
        group: "Conditional prefix form",
        source: "func f {\n  >={ x++ }\n  c-?{ x++ }\n}\n",
        expect: Expect::Compiles,
        case: "Prefix conditional form",
    },
    Row {
        group: "Conditional goto form",
        source: "func f @a8 @i8 {\n  < goto .done\n  c-? goto .done\n.done:\n}\n",
        expect: Expect::Compiles,
        case: "Conditional goto, break, and repeat",
    },
    Row {
        group: "Postfix loop form",
        source: "func f {\n  { x-- } !=\n}\n",
        expect: Expect::Compiles,
        case: "Postfix loop form",
    },
    Row {
        group: "`always` / `never` loops",
        source: "func f {\n  { } always\n  { } never\n}\n",
        expect: Expect::Compiles,
        case: "Postfix loop form",
    },
    Row {
        group: "`break` / `repeat`",
        source: "func f @a8 @i8 {\n  {\n    break\n    repeat\n    < break\n    != repeat\n  } always\n}\n",
        expect: Expect::Compiles,
        case: "Conditional goto, break, and repeat",
    },
    Row {
        group: "`else`",
        source: "func f @a8 @i8 {\n  =={ a = #1 } else { a = #2 }\n}\n",
        expect: Expect::Compiles,
        case: "If-else",
    },
    Row {
        group: "`goto` and `call`",
        source: "var vec = $80\nfunc f @a8 @i8 {\n  goto .label\n.label:\n  goto (vec)\n  goto far .label\n  call f\n}\n",
        expect: Expect::Compiles,
        case: "Flow control instructions",
    },
    Row {
        group: "`nocross` code wrappers",
        source: "func f {\n  nocross { x++ } !=\n  nocross { y++ }\n}\n",
        expect: Expect::Compiles,
        case: "nocross code wrappers",
    },
    Row {
        group: "Wait-loop patterns",
        source: "const READY = $80\nfunc f @a8 @i8 {\n  { a&?READY } n-?\n}\n",
        expect: Expect::Compiles,
        case: "A wait loop",
    },
    Row {
        group: "Efficiency warnings for `>` / `<=`",
        source: "func f {\n  { x++ } >\n  { x++ } <=\n}\n",
        expect: Expect::Compiles,
        case: "Every branch condition spelling",
    },
    Row {
        group: "NOP shorthand",
        source: "func f {\n  *\n  * 5\n  inx * 3\n}\n",
        expect: Expect::Compiles,
        case: "NOP and break shorthands",
    },
    Row {
        group: "`%` BRK shorthand",
        source: "func f {\n  %\n  brk #0\n}\n",
        expect: Expect::Compiles,
        case: "A binary literal is not the break shorthand",
    },
    Row {
        group: "Raw 6502 mnemonics",
        source: "var x0 = $0400\nfunc f @a8 @i8 {\n  adc x0 and x0 asl a bit x0 cmp x0 eor x0 lsr a ora x0 rol a sbc x0\n}\n",
        expect: Expect::Compiles,
        case: "Every classical mnemonic is recognized",
    },
    Row {
        group: "Classical operand shapes",
        source: "var addr = $0400\nvar dp ptr = $10\nvar dp vec = $12\nfunc f @a8 @i8 {\n  nop\n  asl a\n  lda #1\n  lda addr,x\n  lda (ptr,x)\n  lda (ptr),y\n  jmp (vec)\n}\n",
        expect: Expect::Compiles,
        case: "Classical operand shapes",
    },
    Row {
        group: "65816 stack-relative operands",
        source: "func f {\n  lda 4,s\n  lda (4,s),y\n}\n",
        expect: Expect::Compiles,
        case: "Stack-relative operands",
    },
    Row {
        group: "65816 indirect long operands",
        source: "var dp ptr = $10\nfunc f @a8 @i8 {\n  lda [ptr]\n  lda [ptr],y\n}\n",
        expect: Expect::Compiles,
        case: "Indirect long operands",
    },
    Row {
        group: "65816 indexed-offset marker",
        source: "const FIELD = 2\nfunc f @a8 @i8 {\n  lda &FIELD, x\n  lda abs &FIELD, x\n}\n",
        expect: Expect::Compiles,
        case: "The indexed-offset marker",
    },
    Row {
        group: "65C02 additions",
        source: "var a0 = $0400\nfunc f @a8 @i8 {\n  bra t\n  t:\n  phx phy plx ply stz a0 tsb a0 trb a0 wai stp\n}\n",
        expect: Expect::Compiles,
        case: "65C02 additions",
    },
    Row {
        group: "65816 bank/mode mnemonics",
        source: "func f {\n  phb plb phd pld phk xce\n}\n",
        expect: Expect::Compiles,
        case: "65816 bank registers and mode switch",
    },
    Row {
        group: "65816 control flow/mode/stack",
        source: "var dp p = $14\nfunc f @a8 @i8 {\n  brl t\n  t:\n  jml t jsl t rtl pea #1 pei (p) per t cop #2 wdm #3 rep #$10 sep #$20\n}\n",
        expect: Expect::Compiles,
        case: "65816 control flow, mode and stack",
    },
    Row {
        group: "65816 transfer mnemonics",
        source: "func f {\n  tcd tcs tdc tsc txy tyx xba\n}\n",
        expect: Expect::Compiles,
        case: "65816 transfer instructions",
    },
    Row {
        group: "Block moves",
        source: "const src = 1\nconst dst = 2\nfunc f @a8 @i8 {\n  mvn $01,$02\n  mvp src,dst\n}\n",
        expect: Expect::Compiles,
        case: "Block moves take two operands",
    },
    Row {
        group: "Data blocks, named and anonymous",
        source: "data D { 1 2 }\ndata { 3 4 }\n",
        expect: Expect::Compiles,
        case: "Named and anonymous data blocks",
    },
    Row {
        group: "`segment` placement rule in data",
        source: "data D {\n  segment VECTORS\n  word 0\n}\n",
        expect: Expect::Compiles,
        case: "Placement directives",
    },
    Row {
        group: "Placement directives",
        source: "data D {\n  align 16\n  address $5000\n  1\n}\n",
        expect: Expect::Compiles,
        case: "Placement directives",
    },
    Row {
        group: "`nocross` in data blocks",
        source: "nocross data W { 1 2 }\ndata D {\n  nocross\n  nocross 8\n  1\n}\n",
        expect: Expect::Compiles,
        case: "The nocross data wrapper",
    },
    Row {
        group: "Raw bytes and labels in data",
        source: "data D {\n  hello: 1 2 3\n  \"HI\" 0\n}\n",
        expect: Expect::Compiles,
        case: "Raw values and address byte operators",
    },
    Row {
        group: "Address byte operators",
        source: "var ptr = $0400\ndata D {\n  &<ptr &>ptr &&ptr &&&ptr\n  &<ptr+1\n}\n",
        expect: Expect::Compiles,
        case: "An evaluator entry is not part of the value before it",
    },
    Row {
        group: "`?` undefined byte placeholder",
        source: "data D {\n  byte $7F ? ?\n  ?\n}\n",
        expect: Expect::Compiles,
        case: "Typed entries",
    },
    Row {
        group: "Typed data entries",
        source: "const V = 1\nconst H = 2\ndata D {\n  byte 1\n  word V\n  far H\n}\n",
        expect: Expect::Compiles,
        case: "Typed entries",
    },
    Row {
        group: "String data and `charset`",
        source: "data D {\n  charset \".ABC \"\n  \"AB\" 0\n}\n",
        expect: Expect::Compiles,
        case: "charset, code subblocks, and repeats",
    },
    Row {
        group: "`code { }` subblocks",
        source: "data D {\n  code { @a8 { a=1 } }\n  nocross code { @i8 { y=3 } }\n}\n",
        expect: Expect::Compiles,
        case: "charset, code subblocks, and repeats",
    },
    Row {
        group: "`repeat N { }` data repetition",
        source: "data D {\n  repeat 3 { 4 }\n}\n",
        expect: Expect::Compiles,
        case: "charset, code subblocks, and repeats",
    },
    Row {
        group: "`for ... eval` generated data",
        source: "data D {\n  for x=0..100 eval [ x * 2 ]\n}\n",
        expect: Expect::Compiles,
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
        expect: Expect::Deferred,
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
        expect: Expect::Deferred,
        case: "Evaluator brackets are expression atoms",
    },
    Row {
        group: "Top-level evaluator blocks",
        source: "[ FOUR = 4, FIVE = FOUR+1 ]\n",
        expect: Expect::Deferred,
        case: "Generated data from an evaluator range",
    },
    Row {
        group: "Evaluator operator set",
        source: "data D {\n  [ A ? B : C ]\n  [ A && B || C ]\n  [ 1 / 2 % 3 ]\n  [ N = 1, M += 2 ]\n}\n",
        expect: Expect::Deferred,
        case: "The full evaluator operator table",
    },
    Row {
        group: "Evaluator math functions",
        source: "data D {\n  [ clamp(sin(x), 0, 1) ]\n}\n",
        expect: Expect::Deferred,
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
        expect: Expect::Deferred,
        case: "Preprocessor directives at the top level",
    },
    Row {
        group: "Optional semicolons and empty statements",
        source: "var A = 0; var B = 1;;\n;\n",
        expect: Expect::Compiles,
        case: "Semicolons are optional separators and empty statements are ignored",
    },
    Row {
        group: "Case-insensitive registers and flags",
        source: "func f @a8 @i8 {\n  A = #0\n  X = A\n  D = C\n}\n",
        expect: Expect::Compiles,
        case: "Registers are case-insensitive",
    },
    Row {
        group: "Variable+offset addressing",
        source: "var base = $0500\nfunc f @a8 @i8 {\n  a = base+3\n  lda base-2,x\n}\n",
        expect: Expect::Compiles,
        case: "Compound operand expressions",
    },
    // ---- The K65 compatibility surface, taught to the grammar by G1 -------
    //
    // Every row below parses and announces itself; the task that gives each a
    // meaning is named in `Expect::Parses`.
    Row {
        group: "`bank` selection",
        source: "bank core\n",
        expect: Expect::Parses,
        case: "Bank selection",
    },
    Row {
        group: "`var` groups and the `?` marker",
        source: "var anim = $80, cntX, cntY?\n",
        expect: Expect::Compiles,
        case: "Variable groups and the verbose marker",
    },
    Row {
        group: "`o`/`O` as the overflow flag",
        // `var o` rides along: the alias must not cost the name. A token for
        // `o` would win in the lexer wherever an identifier is also valid,
        // and this store would stop being one.
        source: "var o = $0400\nfunc f @a8 @i8 {\n  o-\n  o-? { nop }\n  o = a\n}\n",
        expect: Expect::Compiles,
        case: "The overflow flag is spelled o as well as v",
    },
    Row {
        group: "`align N + M`",
        source: "data table {\n  align 16 + 4\n  1 2 3\n}\n",
        expect: Expect::Parses,
        case: "A boundary with an offset",
    },
    Row {
        group: "Placement directives in a code section",
        source: "func f @a8 @i8 {\n  align 256\n  nop\n}\n",
        expect: Expect::Parses,
        case: "A code section opens with a placement directive",
    },
    Row {
        group: "Evaluator preprocessor conditions",
        source: "#if !DEBUG\n#endif\n",
        expect: Expect::Deferred,
        case: "A preprocessor condition is an evaluator expression",
    },
    Row {
        group: "Evaluator statements",
        source: "data d {\n  [ if x > 0 y = 1 else y = 2 ]\n}\n",
        expect: Expect::Parses,
        case: "Evaluator statements",
    },
    Row {
        group: "`evalfunc` definitions",
        source: "evalfunc twice(n) [ n * 2 ]\n",
        expect: Expect::Parses,
        case: "Evaluator functions",
    },
    Row {
        group: "Two-dimensional evaluator indexing",
        source: "data d {\n  [ grid[2, 3] ]\n}\n",
        expect: Expect::Parses,
        case: "Evaluator indexing",
    },
    Row {
        group: "Evaluator method calls",
        source: "data d {\n  [ tab.index(3) ]\n}\n",
        expect: Expect::Parses,
        case: "Evaluator member access and method calls",
    },
    Row {
        group: "The evaluator constant `pi`",
        source: "data d {\n  [ pi ]\n}\n",
        expect: Expect::Parses,
        case: "The evaluator constant pi",
    },
    Row {
        group: "`evaluator [ ]` data entries",
        source: "data d {\n  evaluator [ x * 2 ]\n}\n",
        expect: Expect::Parses,
        case: "An evaluator data entry",
    },
    Row {
        group: "Illegal-opcode HLA forms",
        source: "var mem = $0400\nfunc f @a8 @i8 {\n  a&.#$0f\n  mem = a&x\n}\n",
        expect: Expect::Parses,
        case: "The illegal-opcode HLA forms",
    },
    Row {
        group: "A statement-leading prefix",
        source: "var HMOVE = $0400\nfunc wsync @a8 @i8 {\n  nop\n}\nfunc f @a8 @i8 {\n  wsync HMOVE = a\n}\n",
        expect: Expect::Compiles,
        case: "A statement-leading prefix keeps its shape in the tree",
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
        // A `Parses` row's construct is announced by the AST builder, so its
        // deferral rides in with the syntax diagnostics and is not one.
        let syntax: Vec<&k816::diagnostic::Diagnostic> = parsed
            .diagnostics
            .iter()
            .filter(|diagnostic| {
                row.expect != Expect::Parses
                    || diagnostic.code != k816::diagnostic::DiagnosticCode::NOT_YET_IMPLEMENTED
            })
            .collect();
        if !syntax.is_empty() {
            failures.push(format!(
                "  {}\n    syntax diagnostics: {:?}\n    source: {:?}",
                row.group,
                syntax
                    .iter()
                    .map(|diagnostic| diagnostic.title.as_str())
                    .collect::<Vec<_>>(),
                row.source
            ));
            continue;
        }
        if row.expect == Expect::Parses {
            // Announced, and by G: a row that has gone quiet is a construct
            // that landed without anyone flipping the row, and one that names
            // a different owner is a mis-filed arm.
            let mine: Vec<&k816::diagnostic::Diagnostic> = parsed
                .diagnostics
                .iter()
                .filter(|diagnostic| {
                    diagnostic.code == k816::diagnostic::DiagnosticCode::NOT_YET_IMPLEMENTED
                        && diagnostic
                            .notes
                            .iter()
                            .any(|note| note.contains("sub-plan G "))
                })
                .collect();
            if mine.is_empty() {
                failures.push(format!(
                    "  {}\n    expected the builder to defer to sub-plan G, got {:?} — \
                     if the construct landed, flip the row\n    source: {:?}",
                    row.group,
                    parsed
                        .diagnostics
                        .iter()
                        .map(|diagnostic| diagnostic.code.as_str())
                        .collect::<Vec<_>>(),
                    row.source
                ));
            }
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
        if row.expect == Expect::Deferred {
            // Parsed, and handed to a named owner. The note
            // `compat::unimplemented` writes is what carries the name.
            let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
            let deferred: Vec<&k816::diagnostic::Diagnostic> = diagnostics
                .iter()
                .filter(|diagnostic| {
                    diagnostic.code == k816::diagnostic::DiagnosticCode::NOT_YET_IMPLEMENTED
                })
                .collect();
            if deferred.is_empty() {
                failures.push(format!(
                    "  {}\n    expected a deferral, got none — if it compiles now, \
                     flip the row to `Compiles`\n    source: {:?}",
                    row.group, row.source
                ));
                continue;
            }
            if let Some(mine) = deferred.iter().find(|diagnostic| {
                diagnostic
                    .notes
                    .iter()
                    .any(|note| note.contains("sub-plan E "))
            }) {
                failures.push(format!(
                    "  {}\n    deferred to sub-plan E, which has finished: {mine:?}",
                    row.group
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
