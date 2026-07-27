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
 *   - symbolic subscripts: field lists with nesting and default widths,
 *     `abstract var` layouts, the field and repeat-element accessors, and the
 *     `:sizeof` / `:offsetof` and typed-view suffixes;
 *   - native instructions: the 6502, 65C02, and 65816 mnemonics with every
 *     operand shape — classical, stack-relative, indirect long, the
 *     indexed-offset marker, and the two-operand block moves — plus the
 *     `dp`/`abs`/`far` encoding prefixes and `* N` repetition;
 *   - HLA data movement: the assignment chain covering every load, store,
 *     and register transfer, plus the `a~b` exchange;
 *   - HLA arithmetic and shorthands: the ALU and compare forms, shifts and
 *     rotates, increment and decrement, the flag and stack shorthands, and
 *     the `*` NOP and `%` break markers;
 *   - code: `func NAME @a8 @i16 { ... }` and `return`.
 *
 * Grammar regeneration is a maintainer operation: `npx tree-sitter-cli@0.26
 * generate` in this directory; the generated `src/parser.c` and metadata are
 * committed and compiled by the ordinary Cargo build, and `generated.lock`
 * (checked by `cargo test`) records that they are current.
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * The 56 classical 6502 mnemonics. The 65C02 and 65816 additions arrive with
 * the next group. Mnemonics are lowercase: only registers and flags are
 * documented as case-insensitive.
 */
const MNEMONICS_6502 = [
  'adc', 'and', 'asl', 'bcc', 'bcs', 'beq', 'bit', 'bmi', 'bne', 'bpl',
  'brk', 'bvc', 'bvs', 'clc', 'cld', 'cli', 'clv', 'cmp', 'cpx', 'cpy',
  'dec', 'dex', 'dey', 'eor', 'inc', 'inx', 'iny', 'jmp', 'jsr', 'lda',
  'ldx', 'ldy', 'lsr', 'nop', 'ora', 'pha', 'php', 'pla', 'plp', 'rol',
  'ror', 'rti', 'rts', 'sbc', 'sec', 'sed', 'sei', 'sta', 'stx', 'sty',
  'tax', 'tay', 'tsx', 'txa', 'txs', 'tya',
];

/** The 65C02 additions. */
const MNEMONICS_65C02 = [
  'bra', 'phx', 'phy', 'plx', 'ply', 'stz', 'tsb', 'trb', 'wai', 'stp',
];

/**
 * The 65816 additions: bank and mode registers, long control flow, the stack
 * pushes, and the register transfers documented under "65816 Transfer
 * Instructions". `mvn` and `mvp` are not here — a block move takes two
 * operands and has its own rule.
 */
const MNEMONICS_65816 = [
  'phb', 'plb', 'phd', 'pld', 'phk', 'xce',
  'brl', 'jml', 'jsl', 'rtl', 'pea', 'pei', 'per', 'cop', 'wdm', 'rep', 'sep',
  'tcd', 'tcs', 'tdc', 'tsc', 'txy', 'tyx', 'xba',
];

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
  access: 9,
};

export default grammar({
  name: 'k65',

  word: $ => $.identifier,

  extras: $ => [/\s/, $.line_comment, $.block_comment],

  // Words that never lex as an identifier. Without this, recovery from a
  // malformed statement swallows the keyword that starts the *next* one —
  // `a = =` followed by `return` would take `return` as the operand it is
  // missing. Deliberately absent: `dp`, `abs`, `far`, `byte`, `word`, which
  // are reserved only in their own slots and stay available as names.
  reserved: {
    global: _ => ['var', 'const', 'segment', 'func', 'abstract', 'return'],
  },

  conflicts: $ => [
    // `foo:` is a label, `foo:sizeof` a metadata query, and `foo:byte` a typed
    // view — which one it is depends on the token *after* the colon, one more
    // than an LR(1) decision can see.
    [$.label, $._expression],
    // Statements have no terminator, so `nop` followed by `a = #1` could also
    // read as `nop a` followed by `= #1`, and `lda foo` followed by
    // `bar = a` as `lda foo bar` (a direct-page prefix) followed by `= a`.
    // Both readings are explored and only the one that goes on to parse
    // survives; where both would, the dynamic precedences above prefer the
    // shorter operand.
    [$.native_instruction],
    // `*` alone is one NOP and `* N` is N of them, so whether what follows is
    // this statement's count or the next statement is the same open question.
    [$.hla_nop],
    // `c+` is a flag operation and `a+mem` is arithmetic; the two look alike
    // until the token after the operator arrives.
    [$.hla_arithmetic, $.hla_flag],
    // A shift target and a NOP count accept only atoms, so an atom at the
    // start of a statement may belong to either that restricted rule or the
    // full expression grammar until the statement's shape is known.
    [$._atom, $._expression],
    // `mem<<` on one line and `mem,x>>>` on the next are two shift
    // statements, not the single shift `(mem << mem,x) >>>`; likewise `* 5`
    // and `* COUNT` are two NOP counts, not `5 * COUNT`. Both readings are
    // explored and the dynamic precedences prefer the one that ends the
    // statement at the operator.

    // Inside `(`, an identifier may be an encoding prefix or the operand
    // expression itself — the same one-token-late decision as above.
    [$.address, $._expression],
    [$.indirect, $._expression],
    [$.long_indirect, $._expression],
    // `lda [ptr]` is an indirect-long operand, not an evaluator bracket used
    // as an address; the dynamic precedences settle it.
    [$.long_indirect, $.eval_bracket],
    // `lda (foo)` is an indirect operand, not a parenthesized address — the
    // dynamic precedences settle it.
    [$.indirect, $.parenthesized_expression],
  ],

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
    // The storage prefix is an aliased identifier rather than a
    // keyword: `dp` and `abs` are reserved *only* in the prefix slot, so
    // `var dp = 0` still declares a variable named `dp`, which a keyword token
    // would swallow. The builder checks that the prefix is one of the three
    // storage classes.
    var_declaration: $ =>
      seq(
        optional(field('abstract', 'abstract')),
        'var',
        optional(field('storage', alias($.identifier, $.storage_class))),
        field('name', $.identifier),
        optional(field('type', $.type_suffix)),
        optional($._span_or_fields),
        optional(seq('*', field('count', $._expression))),
        optional(seq('=', field('address', $._expression))),
      ),

    // `[32]` is a raw span, `[ .x :word ]` a symbolic field list. Which one a
    // bracket opens is decided by whether a `.field` name follows.
    _span_or_fields: $ =>
      choice(seq('[', field('size', $._expression), ']'), field('fields', $.field_list)),

    field_list: $ => seq('[', repeat1($.field_declaration), ']'),

    // .name, .name :byte, .name :byte [count], .name [count], and nested
    // field lists. The declaration-level width is the default for fields that
    // do not state one.
    field_declaration: $ =>
      prec(
        PREC.access + 1,
        seq(
          field('name', $.local_identifier),
          optional(field('type', $.type_suffix)),
          optional($._span_or_fields),
        ),
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
        $.block_move,
        $.hla_assignment,
        $.hla_exchange,
        $.hla_arithmetic,
        $.hla_shift,
        $.hla_step,
        $.hla_flag,
        $.hla_stack,
        $.hla_nop,
        $.hla_break,
        $.return_statement,
      ),

    // `name:` is a global label; `.name:` is local to the enclosing function,
    // so two functions may use the same local label name.
    label: $ =>
      seq(field('name', choice($.identifier, $.local_identifier)), ':'),

    // A raw mnemonic with one of the classical operand shapes. Which shapes a
    // given mnemonic actually accepts is an ISA question, not a grammar one —
    // the grammar accepts any shape after any mnemonic and lets the semantic
    // layer reject illegal combinations, where the diagnostic can name the
    // legal ones.
    //
    // `<mnemonic> * N` repeats an implied-mode instruction N times. It is only
    // available *without* an operand, which is what keeps `lda foo * 3` from
    // being ambiguous between a repeat and a multiplication in the operand.
    native_instruction: $ =>
      choice(
        seq(field('mnemonic', $.mnemonic), field('operand', $._operand)),
        seq(
          field('mnemonic', $.mnemonic),
          optional(seq('*', field('repeat', $._expression))),
        ),
      ),

    mnemonic: _ =>
      token(choice(...MNEMONICS_6502, ...MNEMONICS_65C02, ...MNEMONICS_65816)),

    // mvn src,dst / mvp src,dst — the only two-operand instructions, so they
    // get their own rule rather than widening the general operand grammar to
    // accept `expr,expr` after every mnemonic.
    block_move: $ =>
      seq(
        field('mnemonic', alias(choice('mvn', 'mvp'), $.mnemonic)),
        field('source', $._expression),
        ',',
        field('destination', $._expression),
      ),

    // `asl a` names a register as its operand. There is one `register` rule
    // for both this and the assignment chain: two tokens over the same
    // spelling would fight in the lexer, and which of them a bare `a` after
    // a mnemonic is — this instruction's operand or the start of the next
    // statement — is a parse question the `native_instruction` conflict
    // already answers.
    _operand: $ =>
      choice($.register, $.immediate, $.address, $.indirect, $.long_indirect),

    // [dp|abs|far] expr [,X|,Y]
    //
    // The encoding prefix is an aliased identifier, not a keyword, because
    // `dp` and `abs` are reserved only in the prefix slot: `lda dp` loads the
    // variable `dp` while `lda dp dpa` forces direct-page encoding on `dpa`.
    // Telling those apart needs the token *after* the first identifier, so the
    // grammar declares the conflict and prefers the prefix-less reading when
    // both survive.
    address: $ =>
      choice(
        prec.dynamic(
          1,
          seq(
            optional(field('marker', '&')),
            field('value', $._expression),
            optional($._index_suffix),
          ),
        ),
        // `abs &FIELD_HP,x` would otherwise read as the expression
        // `abs & FIELD_HP` — a variable named `abs` bitwise-ANDed with the
        // field offset — so the prefixed marker form outranks it.
        prec.dynamic(
          2,
          seq(
            field('encoding', alias($.identifier, $.address_encoding)),
            field('marker', '&'),
            field('value', $._expression),
            optional($._index_suffix),
          ),
        ),
        seq(
          field('encoding', alias($.identifier, $.address_encoding)),
          field('value', $._expression),
          optional($._index_suffix),
        ),
      ),

    _index_suffix: $ => seq(',', field('index', $.index_register)),

    // `,S` shares this slot with the two index registers: `4,s` is
    // stack-relative rather than indexed, which the semantic layer tells
    // apart from the register kind.
    index_register: _ => choice('x', 'X', 'y', 'Y', 's', 'S'),

    // [expr] and [expr],Y — indirect long. Recognized only directly after a
    // raw mnemonic: inside an HLA assignment the same brackets belong to the
    // compile-time evaluator, which is why `a = [ptr]` evaluates and
    // `lda [ptr]` dereferences.
    long_indirect: $ =>
      prec.dynamic(
        3,
        seq(
          '[',
          optional(field('encoding', alias($.identifier, $.address_encoding))),
          field('value', $._expression),
          ']',
          optional(seq(',', field('outer_index', $.index_register))),
        ),
      ),

    // (expr), (expr,X), (expr),Y, and the 65816 (expr,S),Y
    indirect: $ =>
      prec.dynamic(
        2,
        seq(
          '(',
          optional(field('encoding', alias($.identifier, $.address_encoding))),
          field('value', $._expression),
          optional(seq(',', field('inner_index', $.index_register))),
          ')',
          optional(seq(',', field('outer_index', $.index_register))),
        ),
      ),

    // The HLA assignment chain, which is *one* rule for every load, store,
    // and register transfer:
    //
    //   a = #imm          x = a            dst0 = dst1 = a = src
    //   SCREEN = a        d = c            UART_DATA = a = text,x
    //   mem,x = a         s = x = y        .LT+1 = a
    //   mem = 0           a = (ptr),y
    //
    // The value flows from the rightmost operand leftwards through every
    // target, so the chain is a list of operands separated by `=` with at
    // least two entries. Which combinations are legal, and what they lower
    // to, is the HLA lowering work; the grammar only records the chain.
    hla_assignment: $ =>
      seq(
        field('operand', $._hla_operand),
        repeat1(seq('=', field('operand', $._hla_operand))),
      ),

    // `a~b` and `b~a` — exchange the accumulator halves (XBA).
    hla_exchange: $ =>
      seq(field('left', $.register), '~', field('right', $.register)),

    // a+mem, a-#1, a&mask, a|bits, a^bits, and the compares a?imm, x?imm,
    // y?imm, and the BIT form a&?mem. One rule for the whole family: they
    // differ only in the operator, and every one of them is a register, an
    // operator, and an operand.
    hla_arithmetic: $ =>
      seq(
        field('register', $.register),
        field('operator', choice('+', '-', '&?', '&', '|', '^', '?')),
        field('operand', $._hla_operand),
      ),

    // a<<, a>>, a<<<, a>>>, mem<<, mem,x>>> — shifts and rotates.
    //
    // `<<` and `>>` are also expression operators, so `mem<<` on one line
    // followed by `mem,x>>>` on the next would otherwise read as the single
    // shift `(mem << mem,x) >>>`. The statement outranks the expression: a
    // shift operator that closes a statement wins over one that continues an
    // operand.
    hla_shift: $ =>
      seq(
        field('target', $._shift_target),
        field('operator', choice('<<<', '>>>', '<<', '>>')),
      ),

    // x++, y--, mem++, mem,x--
    hla_step: $ =>
      seq(field('target', $._hla_target), field('operator', choice('++', '--'))),

    // c+ (SEC), c- (CLC), d+ (SED), d- (CLD), i+ (SEI), i- (CLI), v- (CLV).
    // The flag name shares the register token — `c` and `d` are both — so
    // which it is follows from the statement, and the builder checks that the
    // name is one of the four flags with a shorthand.
    hla_flag: $ =>
      seq(
        field('flag', alias($.register, $.flag_name)),
        field('operator', choice('+', '-')),
      ),

    // a!! (PHA), a?? (PLA), p!! (PHP), b!! (PHB), d?? (PLD), ...
    hla_stack: $ =>
      seq(field('register', $.register), field('operator', choice('!!', '??'))),

    // `*` is one NOP, `* N` is N of them.
    hla_nop: $ => seq('*', optional(field('count', $._atom))),

    // `%` is the one-character `brk #0` breakpoint marker. A binary literal
    // takes the same character, but only with digits attached.
    hla_break: _ => '%',

    _hla_target: $ => choice($.register, $.address, $.indirect),

    // The shift target is deliberately *not* the full operand grammar. `<<`
    // and `>>` are expression operators too, so a target that could contain
    // one at the top level would swallow the operator this statement ends
    // with: `mem<<` followed by `mem,x>>>` would become the single shift
    // `(mem << mem,x) >>>`. Atoms and index suffixes only, which is exactly
    // the set of shift targets the per-mnemonic tables document — `a<<`,
    // `mem<<`, `mem,x<<`. The NOP count is restricted the same way, and for
    // the same reason: the documentation asks only for a literal, a `const`,
    // or an inline parameter there.
    _shift_target: $ =>
      choice($.register, alias($._atom_address, $.address), $.indirect),

    _atom_address: $ => seq(field('value', $._atom), optional($._index_suffix)),

    _atom: $ =>
      choice(
        $.number,
        $.char_literal,
        $.identifier,
        $.local_identifier,
        $.parenthesized_expression,
        $.field_access,
        $.subscript_expression,
        $.metadata_query,
        $.eval_bracket,
      ),

    // The operand alphabet of an assignment chain: a register, an immediate,
    // or a memory reference in any of the shapes a raw mnemonic accepts.
    // `[expr]` is deliberately absent — inside an assignment those brackets
    // are the compile-time evaluator's, and reach here as an expression.
    _hla_operand: $ => choice($.register, $.immediate, $.address, $.indirect),

    return_statement: _ => 'return',

    // Register names are case-insensitive. `C` and `D` name both a register
    // and a flag; which one is meant follows from the statement, not the
    // spelling.
    register: _ =>
      prec(
        2,
        token(
          choice(
            'a', 'A', 'b', 'B', 'c', 'C', 'd', 'D',
            's', 'S', 'x', 'X', 'y', 'Y',
            // The status register, whose stack shorthand spells it either
            // way, and the two flags that are not registers at all. One token
            // for all of them keeps the lexer from having to choose between
            // two rules over the same spelling; the builder decides which
            // meaning the statement asked for.
            'p', 'P', 'flag', 'i', 'I', 'v', 'V',
          ),
        ),
      ),

    immediate: $ => seq('#', field('value', $._expression)),

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
        $.field_access,
        $.subscript_expression,
        $.metadata_query,
        $.typed_view,
        $.unary_expression,
        $.address_of_expression,
        $.binary_expression,
        $.address_and_expression,
        $.eval_bracket,
      ),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    // foo.field and COMP[2].str — the field name must follow the base with no
    // space, which is what keeps `X = FOO` followed by the label reference
    // `.LT+1` from being read as one field access.
    field_access: $ =>
      prec.left(
        PREC.access,
        seq(field('base', $._expression), field('field', $.field_name)),
      ),

    field_name: _ => token.immediate(seq('.', /[A-Za-z_][A-Za-z0-9_]*/)),

    // COMP[2] (a repeat element) and the equivalent field-selection spelling
    // COMP[.two]. Inside the brackets a `.name` is always a field, so it wins
    // over reading it as a local label used as an index.
    subscript_expression: $ =>
      prec.left(
        PREC.access,
        seq(
          field('base', $._expression),
          '[',
          choice(
            field('field', prec(1, $.local_identifier)),
            field('index', $._expression),
          ),
          ']',
        ),
      ),

    // NAME:sizeof, NAME.field:offsetof — compile-time metadata, binding at
    // atom level so `#TASKS:sizeof + 1` is `(TASKS:sizeof) + 1`.
    metadata_query: $ =>
      prec.left(
        PREC.access,
        seq(
          field('base', $._expression),
          ':',
          field('query', choice('sizeof', 'offsetof')),
        ),
      ),

    // Use-site typed views: `lda far x:byte` applies a data width to one
    // expression, independently of the address encoding.
    typed_view: $ =>
      prec.left(PREC.access, seq(field('base', $._expression), field('type', $.type_suffix))),

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
