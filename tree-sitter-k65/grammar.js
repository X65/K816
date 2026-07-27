/**
 * Tree-sitter grammar for K65/K816 — vertical-slice subset (sub-plan A).
 *
 * Covers exactly the constructs the first vertical slice compiles:
 * fixed-address `var NAME = $addr` declarations, `func NAME @a8 @i16 { ... }`,
 * the HLA forms `a = #imm` and `NAME = a`, `return`, and the native
 * equivalents `lda #imm`, `sta NAME`, `rts`, `sep #imm`, `rep #imm`.
 *
 * The complete grammar (all operand shapes, HLA statements, data blocks, and
 * the reserved legacy-K65 namespace) is sub-plan B. Grammar regeneration is a
 * maintainer operation: `npx tree-sitter-cli@0.26 generate` in this directory;
 * the generated `src/parser.c` and metadata are committed and compiled by the
 * ordinary Cargo build.
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: 'k65',

  word: $ => $.identifier,

  extras: $ => [/\s/, $.line_comment, $.block_comment],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice($.var_declaration, $.func_declaration),

    // var SCREEN = $0400 — fixed-address declaration only; allocated,
    // dp/abs/far, typed, and subscripted forms are sub-plan B/E.
    var_declaration: $ =>
      seq('var', field('name', $.identifier), '=', field('address', $.number)),

    // func main @a8 @i16 { ... }
    func_declaration: $ =>
      seq(
        'func',
        field('name', $.identifier),
        repeat(field('width', $.width_annotation)),
        field('body', $.block),
      ),

    width_annotation: _ => token(choice('@a8', '@a16', '@i8', '@i16')),

    block: $ => seq('{', repeat($._statement), '}'),

    _statement: $ =>
      choice(
        $.native_instruction,
        $.hla_load_immediate,
        $.hla_store,
        $.return_statement,
      ),

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

    // SCREEN = a
    hla_store: $ =>
      seq(field('target', $.identifier), '=', field('source', $.register)),

    return_statement: _ => 'return',

    register: _ => 'a',

    immediate: $ => seq('#', field('value', $.number)),

    address: $ => field('symbol', $.identifier),

    number: _ => token(choice(/\$[0-9a-fA-F]+/, /0x[0-9a-fA-F]+/, /[0-9]+/)),

    identifier: _ => /[A-Za-z_][A-Za-z0-9_]*/,

    line_comment: _ => token(seq('//', /[^\n]*/)),

    block_comment: _ => token(seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')),
  },
});
