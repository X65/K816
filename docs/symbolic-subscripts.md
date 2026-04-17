# Symbolic Subscripts and Symbolic Subscript Field Lists

This document describes the K65 dialect feature for declaring packed symbolic subscript arrays on fixed addresses and accessing their fields with symbolic subscripts.

Symbolic subscript arrays are inspired by the Pawn language's named-field array style.

## Overview

A symbolic subscript array declaration binds a symbol to a base address and defines a field layout on top of that address.

- No bytes are emitted by `var` declarations.
- `data { ... }` remains the mechanism for emitting bytes.
- Field layouts are packed (no padding).

## Declaration Syntax

### Symbolic subscript array form

```k65
var foo[
  .field_w   :word
  .field_w2  :word
  .idx       :byte
  .string[20]:byte
] = 0x1234
```

### Default field width on the symbol

```k65
var baz:byte[
  .a
  .b
  .c
  .len:word
] = 0x2244
```

### Field item grammar

- `.name :byte`
- `.name :word`
- `.name :far`
- `.name[count] :byte`
- `.name[count] :word`
- `.name[count] :far`
- `.name[ ...nested fields... ]`
- `.name` (defaults to `:byte` when omitted)

Separators inside the field list:

- newline
- comma
- trailing separator is allowed

## Access Syntax

Preferred dot form:

```k65
foo.field_w = a = 0xBEEF
foo.idx = a = 7
foo.string[i] = a = 'A'
```

Equivalent symbolic subscript form:

```k65
foo[.field_w] = a = 0xBEEF
foo[.string][i] = a = 'A'
```

## Semantics

Layout is packed and sequential:

- first field offset = `0`
- next field offset = `previous_offset + previous_size`
- field size = `element_width * count` (`count=1` when omitted)
- `:byte` width = 1, `:word` width = 2, `:far` width = 3

For each field access, lowering computes:

- field base = `symbolic_subscript_base + field_offset`
- indexed field element address = `field_base + index * element_width`

`= <address>` on a symbolic subscript array var must be a constant numeric expression.

## Diagnostics

The implementation reports hard errors for:

- missing `= <addr>` in symbolic subscript array declarations
- non-constant symbolic subscript base expressions
- duplicate field names in one symbolic subscript array
- invalid field counts (`<= 0` or out of range)
- unknown field access (with nearest-name suggestion)
- invalid indexing on symbolic subscript arrays (`foo[1]`, `foo[i]`)
- indexing a non-array field

## Implementation Notes

The current implementation is split across the normal frontend stages instead of
using a dedicated lexer mode.

### AST and parser

- `VarDecl` stores symbolic-subscript layouts in `symbolic_subscript_fields`.
- `SymbolicSubscriptFieldDecl` supports:
  - optional width (`:byte`, `:word`, `:far`)
  - optional element count (`.name[count]`)
  - optional nested field lists (`.name[ ... ]`)
- `Expr::Index { base, index }` is used for array-style field indexing such as
  `foo.field[i]`.
- Declaration parsing lives in [crates/core/src/parser/data/vars.rs](../crates/core/src/parser/data/vars.rs).
- Expression suffix handling for `foo[.field]` and `foo.field[i]` lives in
  [crates/core/src/parser/expr.rs](../crates/core/src/parser/expr.rs).

### Eval expansion and semantics

- Field counts are expanded during evaluator expansion in
  [crates/core/src/eval_expand.rs](../crates/core/src/eval_expand.rs).
- Layout computation and validation live in
  [crates/core/src/sema/vars.rs](../crates/core/src/sema/vars.rs).
- Semantic metadata is exposed through:
  - `SymbolicSubscriptFieldMeta`
  - `SymbolicSubscriptMeta`
  - `VarMeta.symbolic_subscript`

### Lowering and formatting

- Lowering resolves symbolic-subscript names and metadata queries in
  [crates/core/src/lower.rs](../crates/core/src/lower.rs).
- Supported lowered forms include:
  - direct field addresses (`foo.field`)
  - symbolic field form (`foo[.field]`)
  - indexed field elements (`foo.field[i]`, `foo[.field][i]`)
- The formatter prints symbolic-subscript declarations and indexed expressions in
  [crates/fmt/src/print_ast.rs](../crates/fmt/src/print_ast.rs).

## Tests

Current coverage is split across parser, semantic, lowering, LSP, and golden
tests:

- parser coverage:
  [crates/core/src/parser/tests/data.rs](../crates/core/src/parser/tests/data.rs)
- semantic layout coverage:
  [crates/core/src/sema/tests.rs](../crates/core/src/sema/tests.rs)
- lowering coverage:
  [crates/core/src/lower.rs](../crates/core/src/lower.rs)
- LSP hover coverage:
  [tests/lsp_cli.rs](../tests/lsp_cli.rs)
- golden fixtures:
  [tests/golden/fixtures/syntax-symbolic-subscripts-forms](../tests/golden/fixtures/syntax-symbolic-subscripts-forms),
  [tests/golden/fixtures/syntax-symbolic-subscripts-nesting](../tests/golden/fixtures/syntax-symbolic-subscripts-nesting),
  [tests/golden/fixtures/syntax-sizeof-offsetof](../tests/golden/fixtures/syntax-sizeof-offsetof)
