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
 *   - code sections: `func`, `naked`, `inline`, `far func`, and the legacy
 *     bare `main`, with width annotations, call contracts, module-level
 *     default widths, call contracts on both the header and the call site,
 *     `call` / `call far` / `far NAME`, and `return` / `return_i`;
 *   - control flow: the prefix, goto, and postfix branch forms, `always` /
 *     `never` loops, `break` / `repeat`, `else`, `goto`, and `nocross`;
 *   - data blocks and the compile-time evaluator: placement directives,
 *     labels, typed and raw entries, `charset`, `code` subblocks, `repeat`,
 *     `for ... eval`, and the evaluator's own operator table;
 *   - the remaining namespace: preprocessor directives, `binary` inclusion,
 *     and the reserved `image` and evaluator side-effect forms, which are
 *     named nodes so legacy source never yields `ERROR`;
 *   - the K65 compatibility surface (sub-plan G1): `bank`, `var` groups and
 *     the `?` marker, `o`/`O` as the overflow flag, `align N + M`, an
 *     evaluator `#if` condition, the evaluator's statement layer (`if`,
 *     `else`, `while`, blocks) with `evalfunc`, `pi`, two-dimensional
 *     indexing and method calls, the `evaluator [ ]` data entry, the five
 *     illegal-opcode HLA forms, and the fix that makes a bare call followed
 *     by a statement two statements.
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
/**
 * The compile-time evaluator's operator table (syntax-reference.md
 * "Compile-time Evaluator" / "Operators"), which is a *different* language
 * from the K65 expression layer above even where the tokens coincide. `&&` is
 * logical AND here and 16-bit address-of there; `/`, `%`, `!`, `||`, `?:`,
 * subscripting, member access, and the assignment and increment families
 * exist only inside brackets.
 */
const EVAL_PREC = {
  comma: 1,
  assign: 2,
  ternary: 3,
  logicalOr: 4,
  logicalAnd: 5,
  bitOr: 6,
  bitXor: 7,
  bitAnd: 8,
  equality: 9,
  relational: 10,
  shift: 11,
  add: 12,
  mul: 13,
  prefix: 14,
  suffix: 15,
};

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
    // `o-` is a flag operation and `o - 1` an expression, so an identifier at
    // the start of a statement followed by `+` or `-` belongs to either until
    // the token after the operator arrives.
    [$.hla_flag, $.bare_call, $._expression, $._address_encoding],
    [$.hla_flag, $._expression],
    // A shift target and a NOP count accept only atoms, so an atom at the
    // start of a statement may belong to either that restricted rule or the
    // full expression grammar until the statement's shape is known.
    [$._atom, $._expression],
    // A bare identifier statement calls that function, but it is also how an
    // assignment target, a shift target, and a label all start.
    // `var dp counter` and `far func f` both alias an identifier into a
    // marker slot, which leaves the parser holding two identifiers before it
    // knows which is which.
    [$.var_entry],
    [$.bare_call, $._expression],
    // A statement-leading `far` opens either the keyword-less far call or an
    // operand prefix on an instruction whose mnemonic is still to come.
    [$.call_statement, $._address_encoding],
    [$.bare_call, $._expression, $._address_encoding],
    [$.bare_call, $._address_encoding],
    // A contract-bearing call reads its arguments after the name; a bare one
    // stops at the name and lets the next statement start.
    [$.bare_call],
    [$._expression, $._address_encoding],
    // A `[` after a mnemonic opens an indirect-long operand, whose interior
    // is a K65 expression; anywhere else it opens the evaluator, whose
    // interior is a different grammar over some of the same tokens.
    [$._expression, $._eval_expression],
    [$._expression, $._eval_expression, $._address_encoding],
    // `lda [ptr]` opens an indirect-long operand and `lda [x ~ 1]` an
    // evaluator bracket, and the identifier inside could still be either an
    // encoding prefix or the start of an evaluator statement.
    [$._eval_expression, $._address_encoding],
    // A comma inside `f(a, b)` separates arguments; elsewhere it is the
    // evaluator's expression-list operator.
    [$._eval_argument, $._eval_expression],
    // `nocross` guards the following run and `nocross code { }` wraps a
    // subblock — two readings of the same keyword, separated by what comes
    // after it. `nocross { ... }` around data entries needs no entry here:
    // the brace cannot start a data entry, so one token of lookahead
    // separates the group from the bare directive.
    [$.nocross_directive, $.code_subblock],
    // The loose `image` argument run ends where the next entry begins.
    [$.reserved_image_block],
    // `mem<<` on one line and `mem,x>>>` on the next are two shift
    // statements, not the single shift `(mem << mem,x) >>>`; likewise `* 5`
    // and `* COUNT` are two NOP counts, not `5 * COUNT`. Both readings are
    // explored and the dynamic precedences prefer the one that ends the
    // statement at the operator.

    // Inside `(`, an identifier may be an encoding prefix or the operand
    // expression itself — the same one-token-late decision as above.
    [$.address, $._expression],
    [$.bare_call, $._expression, $.address],
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
        $.module_width,
        $.func_declaration,
        $.data_block,
        $.eval_block,
        $.preprocessor_directive,
        $.binary_declaration,
        $.reserved_image_toplevel,
        $.bank_declaration,
        $.evalfunc_declaration,
        $.import_declaration,
        $.extern_declaration,
      ),

    // `evalfunc name(a, b) [ ... ]` — a user-defined evaluator function. The
    // body takes either spelling of a statement list.
    evalfunc_declaration: $ =>
      seq(
        'evalfunc',
        field('name', $.identifier),
        '(',
        optional(commaSep1(field('parameter', $.identifier))),
        ')',
        field('body', choice($.eval_bracket, $.eval_statement_block)),
      ),

    // `bank NAME` — the original's per-section bank selector. Recognized as
    // its own node so the compatibility layer can name it and point at
    // `segment`, rather than letting it arrive as a parse error.
    bank_declaration: $ => seq('bank', field('name', $.identifier)),

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
    // One `var` keyword introduces a comma-separated group, exactly as
    // `const` does: `var anim = $80, cntX, cntY` declares three. Each entry
    // carries its own storage class, width, span, count, and address, so the
    // group is shorthand for repeating the keyword and nothing more.
    var_declaration: $ =>
      seq(
        optional(field('abstract', 'abstract')),
        'var',
        commaSep1(field('entry', $.var_entry)),
      ),

    var_entry: $ =>
      seq(
        optional(field('storage', alias($.identifier, $.storage_class))),
        field('name', $.identifier),
        optional(field('type', $.type_suffix)),
        // A `[` here belongs to the declaration, not to a top-level
        // evaluator block that happens to start on the same line: both
        // readings parse `var buffer[32]`, and the declaration outranks the
        // split. Without this the ambiguity is resolved by whichever parse
        // state the preceding tokens left the parser in, which made a
        // *comment* earlier in the file change how the array declared.
        optional(prec.dynamic(1, $._span_or_fields)),
        optional(seq('*', field('count', $._expression))),
        optional(seq('=', field('address', $._expression))),
        // The original's `Verbose` marker. It set a listing flag and emitted
        // no byte, so it is recognized here and carried no further.
        optional(field('verbose', '?')),
      ),

    // `[32]` is a raw span, `[ .x :word ]` a symbolic field list. Which one a
    // bracket opens is decided by whether a `.field` name follows.
    //
    // Both spellings of the opening bracket are accepted — the plain one and
    // the one that follows its base with no space — because a *declaration's*
    // bracket may be written either way (`var buffer [32]`) while the postfix
    // subscript below may not. Where a field list and a subscript are both
    // possible — `.a[ … ]` inside a field list, say — the two readings then
    // meet as one token and `field_declaration`'s precedence decides, which is
    // what it did before the postfix became adjacency-sensitive.
    _span_or_fields: $ =>
      choice(
        seq($._open_bracket, field('size', $._expression), ']'),
        field('fields', $.field_list),
      ),

    // Entries may be separated by a newline, by a comma, or by both, and a
    // trailing comma is allowed. K65 is newline-insensitive everywhere else,
    // and the legacy K816's own fixtures wrote the comma form
    // (`var dp gpio[ .ddr:byte, .data:byte, ]`), so a field list is the one
    // place a newline used to be load-bearing.
    field_list: $ =>
      seq(
        $._open_bracket,
        repeat1(seq($.field_declaration, optional(','))),
        ']',
      ),

    _open_bracket: $ => choice('[', $._open_bracket_immediate),

    _open_bracket_immediate: _ => token.immediate('['),

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

    // `import "sprites.k816"` — names a unit this one may see. Sub-plan K.
    //
    // The path is a string literal, resolved relative to the *importing*
    // file, which is the rule `binary NAME = "file"` already uses. There is
    // no identifier spelling: `binary` accepts one because the original did
    // (`binary_entry`), and nothing here has that history.
    import_declaration: $ => seq('import', field('file', $.string)),

    // `extern func _puts @a8 -> @a16` — a symbol some *other* object
    // provides: a cc65-compiled C function, a hand-written ca65 routine, a
    // linker-placed variable, a constant an include file exports.
    //
    // One keyword introduces three forms, each reusing the declaration
    // vocabulary of its non-extern counterpart minus everything that would
    // describe storage K816 does not emit: no body, no address, no span, no
    // count, no initializer. What is left is exactly what ld65 cannot infer
    // and ca65 needs — the address size, and for a function the widths.
    extern_declaration: $ =>
      seq(
        'extern',
        field('item', choice($.extern_func, $.extern_var, $.extern_const)),
      ),

    // `prec.right` because an extern has no body to close it, so a `@a8`
    // after the name is ambiguous with the top-level `module_width` item
    // that could follow the declaration. It binds to the extern, which is
    // the only reading anyone means. The cost is that a module default
    // written *immediately* after an `extern func` is swallowed by it; write
    // it above the extern, which is where a default belongs anyway.
    extern_func: $ =>
      prec.right(
        seq(
          optional(field('far', alias('far', $.far_marker))),
          'func',
          field('name', $.identifier),
          repeat(field('width', $.width_annotation)),
          optional(seq('->', field('exit', $.extern_exit))),
        ),
      ),

    // Widths only. A `func`'s own contract may name registers and flags; an
    // extern's may not, because those are checked against a body and there
    // is none — so the grammar refuses them rather than a later pass.
    extern_exit: $ => prec.right(repeat1(field('width', $.width_annotation))),

    extern_var: $ =>
      seq(
        'var',
        optional(field('storage', alias($.identifier, $.storage_class))),
        field('name', $.identifier),
        optional(field('type', $.type_suffix)),
      ),

    extern_const: $ => seq('const', field('name', $.identifier)),

    // The code sections:
    //
    //   func NAME @a8 @i16 { ... }      a called function, auto-RTS
    //   naked NAME { ... }              no auto-RTS, no exit contract
    //   inline NAME { ... }             expanded at the call site
    //   far func NAME { ... }           JSL/RTL instead of JSR/RTS
    //   main { ... }                    the legacy spelling of the entry point
    //
    // followed by the optional call contract `(a, x) -> @a8 a, y`.
    func_declaration: $ =>
      seq(
        optional(field('far', alias('far', $.far_marker))),
        choice(
          seq(
            field('kind', choice('func', 'naked', 'inline')),
            field('name', $.identifier),
          ),
          // `main { ... }` — the entry point needs no keyword in original
          // K65, and every legacy program in `examples/k65/` uses that form.
          field('name', alias('main', $.identifier)),
        ),
        repeat(field('width', $.width_annotation)),
        optional(field('contract', $.call_contract)),
        field('body', $.block),
      ),

    // `(a, x) -> @a8 a, y`. `()` enables checked zero-input calls; `->` may
    // declare output registers, exit widths, or both. `inline` functions may
    // also name immediate aliases (`#factor`, `#name:byte`) and bare
    // identifier aliases. Whether a given contract is legal for a given
    // section kind is contract analysis, not grammar.
    call_contract: $ =>
      choice(
        seq(field('inputs', $.contract_inputs), optional($._contract_outputs)),
        $._contract_outputs,
      ),

    contract_inputs: $ => seq('(', optional(commaSep1($.contract_parameter)), ')'),

    contract_parameter: $ =>
      choice(
        field('register', $.register),
        seq(
          '#',
          field('alias', $.identifier),
          optional(field('type', $.type_suffix)),
        ),
        field('alias', $.identifier),
      ),

    _contract_outputs: $ => seq('->', field('outputs', $.contract_outputs)),

    // An output is a register or a flag: `(a) -> a, c` hands back the
    // accumulator and the carry. Which flags a clause may name is contract
    // analysis, not grammar — the four a callee can compute are `c z n v`,
    // and a mode bit like `i` is refused there with a message.
    //
    // A flag arrives as an identifier rather than a token, for the reason the
    // `register` rule gives: a letter in the lexer's reach is a letter every
    // rule that wants an identifier loses.
    contract_outputs: $ =>
      choice(
        seq(
          repeat1(field('width', $.width_annotation)),
          optional(commaSep1($._contract_output)),
        ),
        commaSep1($._contract_output),
      ),

    _contract_output: $ =>
      choice(field('register', $.register), field('flag', $.identifier)),

    // Module-level default widths sit at the top of a file on their own.
    // Each annotation is its own item, so a run of them needs no lookahead to
    // tell where the run ends and a `func` header's annotations begin.
    module_width: $ => field('width', $.width_annotation),

    // `@a16 { ... }` scopes a width to a block and restores it afterwards.
    width_block: $ =>
      seq(repeat1(field('width', $.width_annotation)), field('body', $.block)),

    // A bare `@a16` between statements sets the mode for the rest of the
    // enclosing block, with no restore — the difference from `width_block`,
    // and what makes the label-site bridging example in
    // `doc/register-width-aware-syntax.md` work.
    //
    // `@a16 {` is ambiguous between the two: a mode set may be followed by a
    // postfix loop, which also starts with `{`. A `{` after annotations always
    // opens a width block, which is the reading that already existed; the
    // negative dynamic precedence here is what settles it. A moded loop is
    // written `@a16 { { ... } != }`.
    //
    // A run of annotations is one mode set rather than several, so `prec.right`
    // keeps the repetition greedy. Statements have no terminator, so *every*
    // annotation before a `{` belongs to the width block it opens; a mode set
    // directly followed by a width block takes the `;` separator between them.
    mode_set: $ =>
      prec.dynamic(-1, prec.right(repeat1(field('width', $.width_annotation)))),

    // `call NAME` and `call far NAME`. A bare name is also a call; see
    // `bare_call`.
    //
    // The keyword-less `far NAME` spelling is the second arm. It is two
    // identifiers in a row, which is also `lda far addr` with the operand
    // prefix, an assignment target followed by a bare call, and half a dozen
    // other statement shapes; every attempt to *prefer* one reading broke
    // another, so it takes a negative dynamic precedence instead and wins only
    // where nothing else parses. The declared conflicts above list the readings
    // it is explored against.
    //
    // `far` is an aliased identifier here and on a section header, not a
    // keyword, for the same reason the encoding prefixes are: a keyword token
    // would win in the lexer wherever both are valid, and `lda far addr` is
    // an AbsoluteLong load rather than `lda` followed by a far call. The
    // builder checks that the marker really says `far`.
    call_statement: $ =>
      choice(
        seq(
          'call',
          optional(field('far', alias('far', $.far_marker))),
          field('target', $.identifier),
        ),
        prec.dynamic(
          -2,
          seq(field('far', alias('far', $.far_marker)), field('target', $.identifier)),
        ),
      ),

    // ---- Data blocks ------------------------------------------------------
    //
    // `data NAME { ... }` labels the first emitted byte; `data { ... }` does
    // not. Both accept the same entries, and an anonymous block inside a
    // function emits its bytes inline.
    // `nocross data { ... }` is the wrapper spelling of the page-fit request:
    // the same guarantee as a bare `nocross` inside the block, asked for from
    // outside it.
    data_block: $ =>
      seq(
        optional(field('nocross', 'nocross')),
        'data',
        optional(field('name', $.identifier)),
        field('body', $.data_body),
      ),

    data_body: $ => seq('{', repeat(choice($._data_entry, ';')), '}'),

    _data_entry: $ =>
      choice(
        $.segment_declaration,
        $.align_directive,
        $.address_directive,
        $.nocross_directive,
        $.nocross_data_group,
        $.charset_directive,
        $.data_label,
        $.typed_entry,
        $.code_subblock,
        $.data_repeat,
        $.for_eval,
        $.preprocessor_directive,
        $.binary_entry,
        $.reserved_image_block,
        $.evaluator_entry,
        $.data_value,
      ),

    // `evaluator [ ... ]` installs a per-byte transform over the bytes the
    // rest of the section generates; `x` is the byte.
    evaluator_entry: $ => seq('evaluator', field('body', $.eval_bracket)),

    // `nocross { ... }` around a group of data entries: the same page-fit
    // request as a bare `nocross`, bounded by the braces rather than by the
    // end of the statically-sizeable run (`compiler.inc:73`). The body is a
    // `data_body` because the group takes exactly the entries a section does.
    nocross_data_group: $ => seq('nocross', field('body', $.data_body)),

    // `align N` is a boundary; `align N + M` is a boundary *and* an offset —
    // the section starts at an address congruent to M modulo N
    // (`compiler.inc:135`, `setalign(a, offs)`). The `+` is not the
    // expression operator here, which is why the two-operand form is its own
    // alternative over atoms and outranks the arithmetic reading. A folded
    // sum is still available as `align (N + M)`.
    align_directive: $ =>
      choice(
        // Above `PREC.add`, because the competing reading is the expression
        // `16 + 4`: at the `+` the parser chooses between continuing that
        // sum and reducing the boundary, and the boundary wins.
        prec.dynamic(
          10,
          prec(
            PREC.access + 1,
            seq('align', field('boundary', $._atom), '+', field('offset', $._atom)),
          ),
        ),
        seq('align', field('boundary', $._expression)),
      ),

    address_directive: $ => seq('address', field('address', $._expression)),

    // `nocross` guards the run of statically-sizeable entries that follows
    // it, and takes no operand.
    //
    // The older `nocross EXPR` form is deliberately gone. K65 is
    // newline-insensitive, so `nocross` followed by a value could be read
    // either way, and the greedy reading swallowed the first byte of the
    // run — including in `examples/k65/atari-xl/dli-example/main.k65`, where
    // the display list starts `nocross` then `0x70 0x70 0x70`. The bare form
    // is the only one attested anywhere in `doc/k65/` or `examples/k65/`;
    // the sized one appeared solely in the syntax reference, which no longer
    // documents it.
    nocross_directive: _ => 'nocross',

    charset_directive: $ => seq('charset', field('mapping', $.string)),

    data_label: $ => seq(field('name', $.identifier), ':'),

    // `byte 1 2 3`, `word main 0`, `far handler`. Values are atoms only: a
    // compound expression in a value list has to go through a `const`.
    // A typed entry takes every value that follows it, and `prec.right` is
    // what says so. Without it the parser has a choice at each value
    // boundary — keep filling this entry, or end it and start an untyped
    // one — and resolved it from whatever parse state preceded, so a
    // *comment* earlier in the block decided whether `word 0 $1234 ptr` was
    // three words or one word and two bytes.
    //
    // Greedy is the only reading a newline-insensitive language can have:
    // `word 1 2 3` and `word 1 2` followed by `3` on the next line are the
    // same token sequence, so they cannot emit different bytes.
    typed_entry: $ =>
      prec.right(
        seq(
          field('type', choice('byte', 'word', 'far')),
          repeat1(field('value', $._data_atom)),
        ),
      ),

    // A value with no type keyword, which defaults to bytes.
    data_value: $ => field('value', $._data_atom),

    _data_atom: $ =>
      choice(
        $.number,
        $.char_literal,
        $.string,
        $.identifier,
        alias($._data_address_of, $.address_of_expression),
        $.eval_bracket,
        $.undefined_byte,
      ),

    // An address-byte operator in a data entry takes a symbol and, at most,
    // offset arithmetic — `&<ptr+1`, which the documentation shows. It
    // deliberately does *not* take the whole expression grammar: with
    // subscripting available, `&&ptr` on one line and the evaluator entry
    // `[2+2]` on the next would become one value, `&&(ptr[2+2])`, silently
    // losing an entry and changing the bytes emitted. Value lists are
    // atom-only by documentation, and this is where that has teeth.
    _data_address_of: $ =>
      prec.right(
        PREC.addressOf,
        seq(
          field('operator', choice('&&&', '&&', '&<', '&>')),
          field('operand', $._data_offset),
        ),
      ),

    _data_offset: $ =>
      choice(
        $.number,
        $.char_literal,
        $.identifier,
        $.local_identifier,
        $.parenthesized_expression,
        alias($._data_offset_arithmetic, $.binary_expression),
      ),

    _data_offset_arithmetic: $ =>
      prec.left(
        PREC.add,
        seq(
          field('left', $._data_offset),
          field('operator', choice('+', '-')),
          field('right', $._data_offset),
        ),
      ),

    // `?` reserves a slot whose value is deliberately unspecified.
    undefined_byte: _ => '?',

    // `nocross code { }` is one entry, and the attached reading wins by
    // dynamic precedence. Both readings are real — a bare `nocross` guards
    // the run that follows, and `code { }` is an entry on its own — so
    // without a preference the parser resolved it from whatever state
    // preceded, and a *comment* two entries earlier decided whether the
    // subblock got its page guard at all.
    code_subblock: $ =>
      choice(
        prec.dynamic(
          1,
          seq(field('nocross', 'nocross'), 'code', field('body', $.block)),
        ),
        seq('code', field('body', $.block)),
      ),

    data_repeat: $ =>
      seq('repeat', field('count', $._expression), field('body', $.data_body)),

    // `for x=0..213 eval [ ... ]` generates bytes from evaluator output.
    for_eval: $ =>
      seq(
        'for',
        field('variable', $.identifier),
        '=',
        field('from', $._expression),
        '..',
        field('to', $._expression),
        'eval',
        field('body', $.eval_bracket),
      ),

    // A top-level `[ NAME = expr, ... ]` block sets compiler constants.
    eval_block: $ => field('expression', $.eval_bracket),

    // ---- Preprocessor, binary inclusion, and the reserved namespace -------
    //
    // The directives are recognized flat rather than nested: an `#if` without
    // its `#endif` is ordinary in a file being edited, and a flat rule keeps
    // the rest of the file parsing. Evaluating them is compatibility work.
    preprocessor_directive: $ =>
      choice(
        seq(
          field('directive', alias(token(choice('#if', '#elif')), $.directive_name)),
          field('condition', $._pp_condition),
        ),
        field('directive', alias(token(choice('#else', '#endif')), $.directive_name)),
        seq(
          field('directive', alias(token(choice('#error', '#warn')), $.directive_name)),
          field('message', $.string),
        ),
      ),

    // A directive is confined to the line it starts on, so its condition is
    // the original's narrow `PPIfNumber` — `Number | AllIdent | ! PPIfNumber`
    // with `Number -> RawNumber | [ EvalCode ]` (`compiler.inc:23-25,328`) —
    // and not a full evaluator expression.
    //
    // The difference is not stylistic. K65 is newline-insensitive and
    // `EvElem [ … ]` is a postfix index, so a full expression here reads the
    // *next* line as a subscript: `#if 0` above a top-level `[ SIDE = 7 ]`
    // parsed as `0[SIDE = 7]` and reported an index into a data section.
    // Nothing in this grammar has a postfix, so the question cannot arise.
    // Anything richer than a name goes in brackets — `#if [A && B]` — which
    // is what the original requires as well.
    _pp_condition: $ =>
      choice(
        $.number,
        // `#if 0.4` is a lexical error in the original, which reads a
        // condition with `getnum` and rejects the `.`. K816 accepts it and
        // rounds, because the boundary rule it already has answers the
        // question and refusing would be a rule of its own.
        $.float,
        $.identifier,
        $.eval_bracket,
        alias($.pp_negation, $.eval_unary),
      ),

    pp_negation: $ =>
      prec.right(seq(field('operator', '!'), field('operand', $._pp_condition))),

    // `binary "file"` and `binary NAME = "file"` get real nodes: the mapping
    // to ca65's `.incbin` is deferred, not reinterpreted.
    //
    // Inside a data block the file may also be written as a bare identifier —
    // `BinLoad -> binary Ident` (`compiler.inc:71`), which passes the
    // identifier's own text to `loadbinary` with no extension appended, so
    // `binary table` includes a file named exactly `table`. Measured, because
    // the neighbouring `ImgLoad -> image Ident` *does* append `.bmp` and the
    // two rules look alike.
    binary_entry: $ => seq('binary', field('file', choice($.string, $.identifier))),

    binary_declaration: $ =>
      seq(
        'binary',
        field('name', $.identifier),
        '=',
        field('file', $.string),
      ),

    // `image` is reserved: recognized as a specific named node so legacy
    // source never yields `ERROR`, and never reinterpreted. Its arguments are
    // loose by design — the scan-mode words like `8>` and `16v` have no other
    // use in the language, and nothing here needs their structure.
    reserved_image_block: $ =>
      seq(
        field('keyword', 'image'),
        repeat1(field('argument', $._image_argument)),
      ),

    reserved_image_toplevel: $ =>
      seq(
        field('keyword', 'image'),
        field('name', choice($.identifier, $.string)),
        '=',
        field('file', $.string),
      ),

    _image_argument: $ => choice($.number, $.identifier, $.string, $.image_scan),

    /// `8>` / `16v` — a count and a scan direction.
    image_scan: _ => token(seq(/[0-9]+/, /[<>v^]/)),

    // ---- Branches, conditionals, and loops --------------------------------
    //
    // A condition is either a comparison operator or a flag test. The prefix
    // form inverts it — `>={ ... }` runs the block when A < 10, because the
    // branch it emits skips *over* the block.
    branch_condition: _ =>
      token(
        choice(
          // Longest first: `>=0` must not lex as `>=` then `0`, and `<<=`
          // must not lex as the shift operator `<<`.
          '>=0',
          '<0',
          '<<=',
          '>>=',
          '>=',
          '<=',
          '==',
          '!=',
          '<',
          '>',
          // Flag tests: `c+?`, `c-?`, `n+?`, ... for the six testable flags.
          // `o`/`O` is the original's spelling of the overflow flag and is
          // accepted wherever `v`/`V` is.
          seq(
            choice('c', 'C', 'n', 'N', 'z', 'Z', 'v', 'V', 'o', 'O'),
            choice('+', '-'),
            '?',
          ),
        ),
      ),

    // `>={ ... }` / `c-?{ ... }`, with an optional `else`.
    conditional_block: $ =>
      seq(
        field('condition', $.branch_condition),
        field('body', $.block),
        optional(field('else', $.else_clause)),
      ),

    else_clause: $ => seq('else', field('body', $.block)),

    // `< goto label`, `c-? goto done`, `< break`, `!= repeat`.
    conditional_jump: $ =>
      seq(field('condition', $.branch_condition), field('jump', $._jump)),

    _jump: $ => choice($.goto_statement, $.break_statement, $.repeat_statement),

    // `{ ... } !=`, `{ ... } always`, `{ ... } never`, and the `nocross`
    // wrapper form `nocross { ... } !=`.
    // A condition *after* a block belongs to that block, not to whatever
    // follows: `{ x++ } !=` is a loop, and only then may `{ ... }` start a new
    // conditional. Both readings parse — this is the documented
    // statement-boundary hazard — so the postfix form outranks the prefix one.
    postfix_loop: $ =>
      prec(
        1,
        prec.dynamic(
        1,
        seq(
          field('body', choice($.block, $.nocross_block)),
          field('condition', choice($.branch_condition, $.loop_condition)),
        ),
        ),
      ),

    loop_condition: _ => choice('always', 'never'),

    // `goto done`, `goto .loop`, `goto (vec)`, `goto far handler`. A local
    // label is as ordinary a target as a global one — the documentation's
    // own local-label example is `!= goto .loop` — so both name forms are
    // listed here rather than only the unit-level one.
    //
    // `far goto NAME` is the original's own order (`Instr -> far goto Arg`,
    // `compiler.inc:156`) and means the same jump, so it is an alias rather
    // than a second construct. Without the arm the two words reach
    // `call_statement`'s keyword-less `far NAME` reading — `goto` lexes as an
    // identifier where a rule asks for one — and a valid K65 line becomes a
    // call to an undeclared `goto` followed by a near call to the target.
    // That is the wrong meaning rather than a refusal, which is what
    // decision 0 forbids; the alias costs one `choice`.
    goto_statement: $ =>
      seq(
        choice(
          seq('goto', optional(field('far', alias('far', $.far_marker)))),
          seq(field('far', alias('far', $.far_marker)), 'goto'),
        ),
        choice(
          field('target', $.identifier),
          field('target', $.local_identifier),
          seq('(', field('target', $._expression), ')'),
        ),
      ),

    break_statement: _ => 'break',

    repeat_statement: _ => 'repeat',

    // `nocross { ... }` — advisory on code, asking that the block fit a page.
    nocross_block: $ => seq('nocross', field('body', $.block)),

    // `inc_x` on its own calls or inlines that function, and a
    // contract-bearing call makes the register flow explicit at the call
    // site: `add a, x -> a, y`, `scale a, #16 -> a`. A bare name is the
    // weakest possible claim on a token, so the whole form carries the lowest
    // dynamic precedence in the grammar and anything else that parses wins.
    bare_call: $ =>
      prec.dynamic(
        -2,
        seq(
          field('target', $.identifier),
          optional(field('arguments', $.call_arguments)),
          optional(seq('->', field('outputs', $.call_outputs))),
        ),
      ),

    call_arguments: $ => commaSep1(field('argument', $.call_argument)),

    // Registers and immediates only, which is not a parser convenience: K816
    // has no parameter-passing mechanism, so nothing else can cross a `JSR`.
    // The identifier aliases an `inline` may declare are bound during macro
    // expansion, at its header, never handed over at a call. See
    // syntax-reference.md "Why aliases are inline-only" and design.md §1.
    // It also keeps two consecutive bare calls unambiguous: with identifiers
    // accepted here, `inc_x` then `inc_y` would be one call passing a name.
    call_argument: $ =>
      choice(field('register', $.register), field('immediate', $.immediate)),

    // A call site restates what the callee declares, flags included, so this
    // takes the same shape as `contract_outputs`.
    call_outputs: $ => commaSep1($._contract_output),

    width_annotation: _ => token(choice('@a8', '@a16', '@i8', '@i16')),

    block: $ => seq('{', repeat(choice($._statement, ';')), '}'),

    _statement: $ =>
      choice(
        // `Align` sits between `SecStart` and the body of `func`, `naked`,
        // `main` *and* `data` in the original (`compiler.inc:121-136`), so a
        // code section may open with one. K816 accepts it as an ordinary
        // statement rather than a header slot: a placement directive part-way
        // through a body is nonsense the analysis can refuse with a sentence,
        // where a header-only rule refuses it with a parse error.
        $.align_directive,
        $.address_directive,
        $.label,
        $.native_instruction,
        $.block_move,
        $.hla_assignment,
        $.hla_exchange,
        $.hla_arithmetic,
        $.hla_illegal,
        $.hla_shift,
        $.hla_step,
        $.hla_flag,
        $.hla_stack,
        $.hla_stack_flag,
        $.hla_nop,
        $.hla_break,
        $.width_block,
        $.mode_set,
        $.call_statement,
        $.bare_call,
        $.conditional_block,
        $.conditional_jump,
        $.postfix_loop,
        $.goto_statement,
        $.break_statement,
        $.repeat_statement,
        $.nocross_block,
        $.data_block,
        $.preprocessor_directive,
        $.return_statement,
      ),

    // `name:` is a global label; `.name:` is local to the enclosing function,
    // so two functions may use the same local label name.
    //
    // A *register* spelling is accepted here and refused in `semantic.rs`
    // like every other reserved name. It has to be accepted first: `a` is a
    // token, because `a = 1` and `x++` need it to be, so `a:` would otherwise
    // fail to parse before any name was declared and report "expected a
    // statement" instead of the reservation. The flag letters are identifiers
    // and already reach the refusal (`postmortem.md` §G.21.5). One token of
    // lookahead separates the label from an assignment or a step, so this
    // costs no conflict.
    label: $ =>
      seq(
        field('name', choice($.identifier, $.local_identifier, $.register)),
        ':',
      ),

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
        // `lda dp addr` is a prefixed operand, not `lda dp` followed by a
        // bare call to `addr`, so the prefixed reading outranks the
        // prefix-less one wherever both parse. The reverse case — an
        // instruction with a plain operand followed by a bare call, as in
        // `lda foo` then `bar` — is genuinely ambiguous and resolves the same
        // way; write `call bar` or a `;` separator to force the split.
        seq(
          field('encoding', $._address_encoding),
          field('value', $._expression),
          optional($._index_suffix),
        ),
      ),

    // `dp` and `abs` reach the operand slot as identifiers, because they are
    // reserved only here and stay available as names. `far` cannot: it is a
    // keyword elsewhere — `far func`, `call far`, and the keyword-less far
    // call — and a keyword token wins in the lexer wherever both it and an
    // identifier are valid. Accepting both spellings in this one slot is what
    // lets `lda far addr` be an AbsoluteLong load *and* `far NAME` be a call.
    _address_encoding: $ =>
      choice(alias($.identifier, $.address_encoding), alias('far', $.address_encoding)),

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
          optional(field('encoding', $._address_encoding)),
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
          optional(field('encoding', $._address_encoding)),
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
    // `o`/`O` is the original's spelling of the overflow flag and reaches
    // this slot as an *identifier*, not as a token of its own: a token would
    // win in the lexer wherever an identifier is also valid, and
    // `var o = $0400` followed by `o = a` would stop being a store. The
    // builder checks that the name is one of the flags with a shorthand,
    // which it had to do for `c`, `d`, `i` and `v` already.
    hla_flag: $ =>
      seq(
        field(
          'flag',
          choice(alias($.register, $.flag_name), alias($.identifier, $.flag_name)),
        ),
        field('operator', choice('+', '-')),
      ),

    // a!! (PHA), a?? (PLA), p!! (PHP), b!! (PHB), d?? (PLD), ...
    hla_stack: $ =>
      seq(field('register', $.register), field('operator', choice('!!', '??'))),

    // `z??`, `n!!`, `v??`, `i!!` — the K65 spellings where the letter names a
    // *flag*. In the original every one of the seven letters is plain
    // `PLP`/`PHP`: the letter is decoration and all fourteen spellings mean
    // the same two instructions (`compiler.inc:167-180`, and the oracle
    // golden's `28 08`). K816 does not give them that meaning, so they are
    // recognized here as a named node and refused with a `K816-K65-####`
    // diagnostic rather than reaching `expected a statement`, which is
    // decision 0's second clause.
    //
    // Four letters, not seven. `b` and `d` are in `register` and keep K816's
    // meaning — `PLB`/`PLD`, the data-bank and direct-page registers the
    // 65816 has and the 6502 did not — so those two spellings are an
    // `intentionally-changed` row rather than a refusal. `c` is in `register`
    // too, and the builder turns its stack form into the same refusal, which
    // is where the register token's own comment says such choices belong.
    //
    // `i` and `v` are deliberately absent from `register` (`postmortem.md`
    // §G.1.16); they arrive here as their own literals, which no other rule
    // competes for because `??` and `!!` are single tokens.
    // One token, letter and operator together, and that is the whole trick.
    // Listing `i` and `v` as literals of their own would put them back in the
    // lexer's reach for every rule that wants an identifier — which is
    // exactly how `i+ d-` came to parse as the arithmetic `i + d`
    // (`postmortem.md` §G.1.16). `i??` as a single token cannot change how a
    // bare `i` lexes anywhere else.
    // `c` and `d` join them for the same reason, now that neither is a
    // register: they used to reach `hla_stack` as one and be refused there by
    // name. Six letters, and `b` is the seventh — it stays in `register` for
    // `a~b`, so `b!!` parses as a stack form and the builder refuses it.
    hla_stack_flag: _ =>
      token(
        choice(
          'z??', 'z!!', 'Z??', 'Z!!',
          'n??', 'n!!', 'N??', 'N!!',
          'v??', 'v!!', 'V??', 'V!!',
          'i??', 'i!!', 'I??', 'I!!',
          'c??', 'c!!', 'C??', 'C!!',
          'd??', 'd!!', 'D??', 'D!!',
        ),
      ),

    // `*` is one NOP, `* N` is N of them.
    hla_nop: $ => seq('*', optional(field('count', $._atom))),

    // `%` is the one-character `brk #0` breakpoint marker. A binary literal
    // takes the same character, but only with digits attached.
    hla_break: _ => '%',

    // The five HLA spellings of 6502 *illegal* opcodes. On the 65816 those
    // encodings are different, real instructions, so the forms are recognized
    // as one named node and refused rather than reinterpreted; `operator`
    // says which of the five it is.
    //
    //   a&.#$0F     ANC        a?--mem    DCP        a-++mem   ISC
    //   mem = a&x   SAX        x &= a-#1  AXS
    //
    // `prec(1)` settles the one place where the shorter reading is also
    // legal: after `mem = a`, an `&` could start a fresh address-marker
    // statement. The SAX form wins, which is what the original does.
    hla_illegal: $ =>
      prec(
        1,
        choice(
          seq(
            field('register', $.register),
            // One token each, so the tree names which of the three it is.
            // The original's grammar allows space inside `? --` and `- ++`
            // (`ArithOp -> ? --`); K816 takes them whole, which no attested
            // source notices and which keeps `--` and `++` unambiguous.
            field(
              'operator',
              alias(
                choice(token('&.'), token('?--'), token('-++')),
                $.illegal_operator,
              ),
            ),
            field('operand', $._hla_operand),
          ),
          // The target is spelled with `_hla_operand`, the assignment
          // chain's own alphabet, so that `mem = a` and `mem = a & x` share a
          // parse state up to the `&` instead of forking at the `=`.
          seq(
            field('target', $._hla_operand),
            field('operator', '='),
            field('left', $.register),
            '&',
            field('right', $.register),
          ),
          seq(
            field('target', $.register),
            field('operator', '&='),
            field('left', $.register),
            '-',
            field('operand', $._hla_operand),
          ),
        ),
      ),

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

    return_statement: _ => choice('return', 'return_i'),

    // Register names are case-insensitive.
    //
    // One rule decides what belongs here: **a single letter is a flag, and a
    // register that is not `a x y s` is spelled by its datasheet
    // abbreviation** (`plans/06-compatibility.md` decision 27). `a x y s` are
    // the four the original names (`compiler.inc:244-251`); everything longer
    // is a register the 6502 did not have.
    //
    // So `c` and `d` are *not* here. They are the carry and decimal flags,
    // which is what the original spells them (`Flag -> C|c|D|d|I|i|O|o`,
    // `:253-260`), and the registers that had taken those letters are `dpr`
    // and `dbr` now. That also removes the last of the ambiguity §G.1.16
    // describes: a letter that is both a flag and a register has two
    // complete readings wherever an expression can follow.
    register: _ =>
      prec(
        2,
        token(
          choice(
            'a', 'A', 's', 'S', 'x', 'X', 'y', 'Y',
            // The accumulator's high half, and the *only* thing it is: `XBA`
            // exchanges the halves of one register, which is why the effect
            // model has no B bit and why `b` is not a transfer operand, a
            // contract output, or a stack target (decision 30).
            'b', 'B',
            // The direct page and data bank, which used to be `d` and `b`.
            // `dp` is not available for the first: it is the storage-class
            // prefix in `var dp counter`, and a token would stop that being a
            // declaration.
            'dpr', 'DPR', 'dbr', 'DBR',
            // The status register. One spelling only: `flag` was an alias
            // for it, and an alias is a second way to say a thing that the
            // reader then has to know are the same thing.
            'p', 'P',
            // `c`, `d`, `i` and `v` are deliberately *not* here. They are
            // flags, and no rule but `hla_flag` has a use for them — while
            // every rule this token reaches gains them, which is what went
            // wrong: `i+ d-` (SEI then CLD) read as the arithmetic `i + d`,
            // because `hla_arithmetic` takes a register and `i` was one. They
            // reach the flag slot as identifiers (`postmortem.md` §G.1.16).
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
    //
    // The `[` binds to its base with no space between, exactly as `field_name`
    // above does, and for the same reason one level up: K65 is
    // newline-insensitive, so a postfix that may be separated from its base
    // reads the *next line* as its operand. `const FLAG = 0` above a top-level
    // `[ SIDE = 7 ]` parsed as `FLAG = 0[SIDE = 7]` and reported
    // `K816-E0001` — a program the original compiles, reaching a generic
    // parse error, which is what decision 0 exists to prevent
    // (`doc/k65-compat-manifest.md`, `newline-subscript`).
    //
    // This is the *K816* postfix — symbolic subscripts and repeat elements,
    // which K65 has not — so the rule is K816's to make. The evaluator's
    // `EvElem [ … ]` below is K65's own and stays newline-insensitive, because
    // there `t` above a `[0]` is `t[0]` in the original too.
    subscript_expression: $ =>
      prec.left(
        PREC.access,
        seq(
          field('base', $._expression),
          $._open_bracket_immediate,
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

    // `[ ... ]` invokes the compile-time evaluator, whose interior is its own
    // grammar over `EVAL_PREC` — not the K65 expression layer around it.
    // A bracket holds a *statement list*, not a single expression: the
    // original's `EvalCode` is a run of instructions, juxtaposed or separated
    // by `;`, and the bracket's value is the last one's.
    eval_bracket: $ => seq('[', repeat1($._eval_statement_item), ']'),

    // A `;` terminates a statement; it is never a statement of its own, so
    // `[ ; ]`, `[ ; 1 ]` and `[ 1 ;; ]` are not evaluator code. The original
    // is stricter still — `EvalCode -> EvalInstr ;` puts a semicolon only
    // after the *first* instruction — which is an accident of its LALR
    // grammar that nothing writes and nothing could depend on.
    _eval_statement_item: $ => seq($._eval_statement, optional(';')),

    _eval_statement: $ =>
      choice($.eval_if, $.eval_while, $.eval_statement_block, $._eval_expression),

    eval_statement_block: $ => seq('{', repeat1($._eval_statement_item), '}'),

    // `if C S`, `if C S else S`. `prec.right` attaches an `else` to the
    // nearest `if`, which is what the original's LALR shift does.
    eval_if: $ =>
      prec.right(
        seq(
          'if',
          field('condition', $._eval_expression),
          field('consequence', $._eval_statement),
          optional(seq('else', field('alternative', $._eval_statement))),
        ),
      ),

    eval_while: $ =>
      prec.right(
        seq(
          'while',
          field('condition', $._eval_expression),
          field('body', $._eval_statement),
        ),
      ),

    // ---- The compile-time evaluator ---------------------------------------

    // An argument list separates with commas, so the comma *operator* is not
    // available at the top level of an argument; `f(a, b)` is two arguments,
    // never one expression list.
    _eval_argument: $ =>
      choice(
        $.number,
        $.float,
        $.char_literal,
        $.string,
        $.identifier,
        $.eval_call,
        $.reserved_eval_call,
        $.eval_parenthesized,
        $.eval_unary,
        $.eval_binary,
        $.eval_ternary,
        $.eval_assignment,
        $.eval_subscript,
        $.eval_member,
        $.eval_method_call,
        $.eval_pi,
      ),

    _eval_expression: $ =>
      choice(
        $.number,
        $.float,
        $.char_literal,
        $.string,
        $.identifier,
        $.eval_call,
        $.reserved_eval_call,
        $.eval_parenthesized,
        $.eval_unary,
        $.eval_binary,
        $.eval_ternary,
        $.eval_assignment,
        $.eval_subscript,
        $.eval_member,
        $.eval_method_call,
        $.eval_pi,
        $.eval_list,
      ),

    eval_parenthesized: $ => seq('(', $._eval_expression, ')'),

    // The evaluator functions that mutate sections, print, or randomize.
    // They are reserved rather than rejected: the parser names them so the
    // compiler can say which construct it will not run.
    //
    // `size` was among them until G6 implemented it. It is an ordinary call
    // now, which is what keeps `size(t)` and `t.size` one complaint when the
    // section it names cannot be read: a method call is desugared in the
    // interpreter, and only the parenthesised spelling was ever a node here.
    reserved_eval_call: $ =>
      prec(
        EVAL_PREC.suffix,
        seq(
          field(
            'name',
            alias(
              choice('addbyte', 'color', 'rnd', 'print', 'error'),
              $.reserved_function,
            ),
          ),
          '(',
          optional(commaSep1(field('argument', $._eval_argument))),
          ')',
        ),
      ),

    eval_call: $ =>
      prec(
        EVAL_PREC.suffix,
        seq(
          field('name', $.identifier),
          '(',
          optional(commaSep1(field('argument', $._eval_argument))),
          ')',
        ),
      ),

    /// `pi` — the original's one built-in constant, a keyword rather than a
    /// name, so an evaluator variable cannot shadow it.
    eval_pi: _ => 'pi',

    // `data[i]` selects a byte of a section, `data[row, col]` a cell of a
    // two-dimensional one. An index is an *argument*, not a full expression:
    // the comma between two indices is the separator, never the evaluator's
    // comma operator.
    eval_subscript: $ =>
      prec(
        EVAL_PREC.suffix,
        seq(
          field('base', $._eval_expression),
          '[',
          field('index', $._eval_argument),
          optional(seq(',', field('index', $._eval_argument))),
          ']',
        ),
      ),

    eval_member: $ =>
      prec.left(
        EVAL_PREC.suffix,
        seq(field('base', $._eval_expression), field('field', $.field_name)),
      ),

    // `x.f(a)` — sugar for `f(x, a)`. The dot must follow the base with no
    // space, which is the same rule `field_access` uses in the K65 layer.
    //
    // Binds tighter than `eval_member`, which is the same rule minus the
    // argument list: at the `(` the parser chooses between reducing the
    // member access and shifting into the call, and the call wins.
    eval_method_call: $ =>
      prec(
        EVAL_PREC.suffix + 1,
        seq(
          field('base', $._eval_expression),
          field('method', $.field_name),
          '(',
          optional(commaSep1(field('argument', $._eval_argument))),
          ')',
        ),
      ),

    eval_unary: $ =>
      choice(
        // Suffix increment and decrement bind tightest.
        prec(
          EVAL_PREC.suffix,
          seq(field('operand', $._eval_expression), field('operator', choice('++', '--'))),
        ),
        prec.right(
          EVAL_PREC.prefix,
          seq(
            field('operator', choice('++', '--', '+', '-', '!', '~')),
            field('operand', $._eval_expression),
          ),
        ),
      ),

    eval_binary: $ =>
      choice(
        ...[
          ['*', EVAL_PREC.mul],
          ['/', EVAL_PREC.mul],
          ['%', EVAL_PREC.mul],
          ['+', EVAL_PREC.add],
          ['-', EVAL_PREC.add],
          ['<<', EVAL_PREC.shift],
          ['>>', EVAL_PREC.shift],
          ['<=', EVAL_PREC.relational],
          ['>=', EVAL_PREC.relational],
          ['?>', EVAL_PREC.relational],
          ['?<', EVAL_PREC.relational],
          ['<', EVAL_PREC.relational],
          ['>', EVAL_PREC.relational],
          ['==', EVAL_PREC.equality],
          ['!=', EVAL_PREC.equality],
          ['&&', EVAL_PREC.logicalAnd],
          ['||', EVAL_PREC.logicalOr],
          ['&', EVAL_PREC.bitAnd],
          ['^', EVAL_PREC.bitXor],
          ['|', EVAL_PREC.bitOr],
        ].map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(
              field('left', $._eval_expression),
              field('operator', operator),
              field('right', $._eval_expression),
            ),
          ),
        ),
      ),

    eval_ternary: $ =>
      prec.right(
        EVAL_PREC.ternary,
        seq(
          field('condition', $._eval_expression),
          '?',
          field('consequence', $._eval_expression),
          ':',
          field('alternative', $._eval_expression),
        ),
      ),

    eval_assignment: $ =>
      prec.right(
        EVAL_PREC.assign,
        seq(
          field('target', $._eval_expression),
          field(
            'operator',
            choice('=', '+=', '-=', '*=', '/=', '%=', '<<=', '>>=', '&=', '^=', '|='),
          ),
          field('value', $._eval_expression),
        ),
      ),

    // `,` returns the value of the last expression.
    eval_list: $ =>
      prec.left(
        EVAL_PREC.comma,
        seq(field('item', $._eval_expression), repeat1(seq(',', field('item', $._eval_expression)))),
      ),

    // Floats exist only inside evaluator brackets: `[1.5 + .5]`.
    float: _ => token(choice(/[0-9]+\.[0-9]*/, /\.[0-9]+/)),

    string: _ =>
      token(seq('"', repeat(choice(/[^"\\\n]/, seq('\\', /[abfnrtv0\\'"]/))), '"')),

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
