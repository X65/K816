/*****************************************************************************/
/*                                                                           */
/*                                 grammar.js                                */
/*                                                                           */
/*                    Tree-sitter grammar for K65/K816                       */
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

/**
 * Tree-sitter grammar for K65/K816.
 *
 * Grown group by group by sub-plan B from the vertical-slice subset sub-plan A
 * established. Implemented so far:
 *
 *   - the lexical layer: decimal / `$` / `0x` / `%` / `0b` numeric literals,
 *     character literals with escapes, identifiers, and comments;
 *   - the K65 expression layer: C-precedence binary operators, unary `-` and
 *     `~`, the address-byte operators, and `[ ... ]` evaluator brackets;
 *   - program structure: optional semicolons, the `var` family, `const`
 *     groups, `segment` selection, and global and local labels;
 *   - code: `func NAME @a8 @i16 { ... }`, the HLA forms `a = #imm` and
 *     `TARGET = a`, `return`, and the native instructions `lda #imm`,
 *     `sta NAME`, `rts`, `sep #imm`, `rep #imm`.
 *
 * Grammar regeneration is a maintainer operation: `npx tree-sitter-cli@0.26
 * generate` in this directory; the generated `src/parser.c` and metadata are
 * committed and compiled by the ordinary Cargo build, and `generated.lock`
 * (checked by `cargo test`) records that they are current.
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * Precedence of the K65 expression layer, lowest first.
 *
 * Levels 1-3 and 5-7 are the C-style table in syntax-reference.md "Binary
 * operators". The address-byte operators sit in between: `&<ptr+1` has to mean
 * the low byte of `ptr+1` (the documented "with offset arithmetic" form), so
 * they bind looser than the arithmetic operators, while the `&&` gotcha —
 * `FOO && BAR` meaning `FOO & (&&BAR)` — puts them tighter than `&`.
 *
 * The compile-time evaluator has a *different*, larger operator table over
 * partly the same tokens; it lives behind `[` and is not modelled here.
 */
const PREC = {
  or: 1,
  xor: 2,
  and: 3,
  addressOf: 4,
  shift: 5,
  add: 6,
  mul: 7,
  unary: 8,
};

export default grammar({
  name: 'k65',

  word: $ => $.identifier,

  extras: $ => [/\s/, $.line_comment, $.block_comment],

  rules: {
    // Semicolons are optional separators and empty statements are ignored, so
    // `;` is consumed by the item and statement lists themselves and leaves no
    // node behind.
    source_file: $ => repeat(choice($._item, ';')),

    _item: $ =>
      choice(
        $.var_declaration,
        $.const_declaration,
        $.segment_declaration,
        $.func_declaration,
      ),

    // The `var` family:
    //
    //   var SCREEN = $0400          fixed address
    //   var counter                 linker-allocated
    //   var dp scratch              storage class prefix
    //   var w:word = $2000          typed width
    //   var buffer[32]              raw span
    //   var simple:byte * 32        allocation count
    //
    // Symbolic subscript field lists (`var name[ .f :w ... ]`) are the next
    // group. The storage prefix is an aliased identifier rather than a
    // keyword: `dp` and `abs` are reserved *only* in the prefix slot, so
    // `var dp = 0` still declares a variable named `dp`, which a keyword token
    // would swallow. The builder checks that the prefix is one of the three
    // storage classes.
    var_declaration: $ =>
      seq(
        'var',
        optional(field('storage', alias($.identifier, $.storage_class))),
        field('name', $.identifier),
        optional(field('type', $.type_suffix)),
        optional(seq('[', field('size', $._expression), ']')),
        optional(seq('*', field('count', $._expression))),
        optional(seq('=', field('address', $._expression))),
      ),

    type_suffix: _ => seq(':', field('width', choice('byte', 'word', 'far'))),

    // const A = 0, B, C — a comma-separated group, where an entry without an
    // initializer continues the previous one (enum-like auto-increment).
    const_declaration: $ => seq('const', commaSep1($.const_entry)),

    const_entry: $ =>
      seq(
        field('name', $.identifier),
        optional(seq('=', field('value', $._expression))),
      ),

    // segment NAME — selects the output segment for what follows.
    segment_declaration: $ => seq('segment', field('name', $.identifier)),

    // func main @a8 @i16 { ... }
    func_declaration: $ =>
      seq(
        'func',
        field('name', $.identifier),
        repeat(field('width', $.width_annotation)),
        field('body', $.block),
      ),

    width_annotation: _ => token(choice('@a8', '@a16', '@i8', '@i16')),

    block: $ => seq('{', repeat(choice($._statement, ';')), '}'),

    _statement: $ =>
      choice(
        $.label,
        $.native_instruction,
        $.hla_load_immediate,
        $.hla_store,
        $.return_statement,
      ),

    // `name:` is a global label; `.name:` is local to the enclosing function,
    // so two functions may use the same local label name.
    label: $ =>
      seq(field('name', choice($.identifier, $.local_identifier)), ':'),

    // Native slice instructions, each with its single legal operand shape.
    native_instruction: $ =>
      choice(
        seq(
          field('mnemonic', alias(choice('lda', 'sep', 'rep'), $.mnemonic)),
          field('operand', $.immediate),
        ),
        seq(
          field('mnemonic', alias('sta', $.mnemonic)),
          field('operand', $.address),
        ),
        field('mnemonic', alias('rts', $.mnemonic)),
      ),

    // a = #imm
    hla_load_immediate: $ =>
      seq(field('register', $.register), '=', field('value', $.immediate)),

    // SCREEN = a, and the self-modifying-code form `.LT+1 = a`. The full set
    // of store shapes and assignment chaining is the HLA movement group.
    hla_store: $ =>
      seq(field('target', $._expression), '=', field('source', $.register)),

    return_statement: _ => 'return',

    register: _ => 'a',

    immediate: $ => seq('#', field('value', $._expression)),

    address: $ => field('symbol', $.identifier),

    // ---- The K65 expression layer -----------------------------------------
    //
    // Hidden, so an expression appears directly under the field that holds it
    // rather than behind a wrapper node.
    _expression: $ =>
      choice(
        $.number,
        $.char_literal,
        $.identifier,
        $.local_identifier,
        $.parenthesized_expression,
        $.unary_expression,
        $.address_of_expression,
        $.binary_expression,
        $.address_and_expression,
        $.eval_bracket,
      ),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    unary_expression: $ =>
      prec.right(
        PREC.unary,
        seq(field('operator', choice('-', '~')), field('operand', $._expression)),
      ),

    // &<expr (low byte), &>expr (high byte), &&expr (16-bit address),
    // &&&expr (24-bit address).
    address_of_expression: $ =>
      prec.right(
        PREC.addressOf,
        seq(
          field('operator', choice('&&&', '&&', '&<', '&>')),
          field('operand', $._expression),
        ),
      ),

    binary_expression: $ =>
      choice(
        ...[
          ['|', PREC.or],
          ['^', PREC.xor],
          ['&', PREC.and],
          ['<<', PREC.shift],
          ['>>', PREC.shift],
          ['+', PREC.add],
          ['-', PREC.add],
          ['*', PREC.mul],
        ].map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(
              field('left', $._expression),
              field('operator', operator),
              field('right', $._expression),
            ),
          ),
        ),
      ),

    // The documented `&&` gotcha (syntax-reference.md "`&&` gotcha"): the
    // lexer takes `&&` whole, so `FOO && BAR` is bitwise AND of FOO with the
    // 16-bit address of BAR. K65 has no logical AND in this layer — that
    // meaning of `&&` exists only inside evaluator brackets. The single token
    // carries both roles; the AST expands it into the two operations.
    address_and_expression: $ =>
      prec.left(
        PREC.and,
        seq(
          field('left', $._expression),
          field('operator', '&&'),
          field('right', $._expression),
        ),
      ),

    // `[ ... ]` invokes the compile-time evaluator. Its interior is a
    // *different* grammar with its own operator table and is written in the
    // data/evaluator group; until then the K65 expression layer stands in, so
    // the common `[$FF]` and `[CONST]` forms already parse.
    eval_bracket: $ => seq('[', field('expression', $._expression), ']'),

    // Decimal, `$`/`0x` hexadecimal, `%`/`0b` binary. The lexer's longest
    // match keeps `0b1010` and `0x2A` whole rather than splitting a leading
    // `0` off as a decimal literal.
    number: _ =>
      token(
        choice(
          /\$[0-9a-fA-F]+/,
          /0[xX][0-9a-fA-F]+/,
          /%[01]+/,
          /0[bB][01]+/,
          /[0-9]+/,
        ),
      ),

    // 'A', '\n', '\0', '\x41'. Only the escapes listed here are accepted, so
    // a typo like '\q' is reported as malformed source instead of silently
    // decoding to something else.
    char_literal: _ =>
      token(
        seq(
          "'",
          choice(
            /[^'\\\n]/,
            seq('\\', choice(/[abfnrtv0\\'"]/, seq('x', /[0-9a-fA-F][0-9a-fA-F]/))),
          ),
          "'",
        ),
      ),

    identifier: _ => /[A-Za-z_][A-Za-z0-9_]*/,

    // `.name` — a function-scoped local label, in both its declaration and
    // its references (`.LT+1`).
    local_identifier: _ => token(seq('.', /[A-Za-z_][A-Za-z0-9_]*/)),

    // `//` to end of line; a comment on the last line need not be terminated.
    line_comment: _ => token(seq('//', /[^\n]*/)),

    // Non-nesting C block comment: everything up to the first `*/`.
    block_comment: _ => token(seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')),
  },
});

/** One or more `rule`, separated by commas. */
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}
