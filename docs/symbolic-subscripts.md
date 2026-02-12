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
- `.name[count] :byte`
- `.name[count] :word`
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
- `:byte` width = 1, `:word` width = 2

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

### Lexer (`crates/core/src/lexer.rs`)

- `TokenKind::Eval` now uses `#[token("[", parse_eval)]`.
- `parse_eval` tracks nested bracket depth, so field lists containing `.name[count]` are lexed correctly as one bracket fragment.

### AST (`crates/core/src/ast/mod.rs`)

- `VarDecl` gained `symbolic_subscript_fields: Option<Vec<SymbolicSubscriptFieldDecl>>`.
- New `SymbolicSubscriptFieldDecl { name, data_width, count }`.
- Expressions gained `Expr::Index { base, index }`.

### Parser (`crates/core/src/parser/mod.rs`)

- `var_decl_parser` now parses a bracket payload as either:
  - classic array length expression, or
  - symbolic subscript field list.
- Symbolic subscript field list parsing is implemented by:
  - `parse_var_bracket_payload`
  - `parse_symbolic_subscript_field_list`
  - `parse_symbolic_subscript_field_entry`
- Expression parsing now applies eval suffixes (`[...]`) as postfix operations via `apply_eval_suffixes`:
  - `foo[.field]` becomes identifier form `foo.field`
  - `foo.bar[i]` becomes `Expr::Index { base: foo.bar, index: i }`

### Eval Expansion (`crates/core/src/eval_expand.rs`)

- Symbolic subscript field `count` expressions are expanded.
- `Expr::Index` participates in recursive expansion.

### Semantic Analysis (`crates/core/src/sema/mod.rs`)

- New metadata:
  - `SymbolicSubscriptFieldMeta`
  - `SymbolicSubscriptMeta`
  - `VarMeta.symbolic_subscript: Option<SymbolicSubscriptMeta>`
- `eval_var_layout` handles both traditional vars and symbolic subscript arrays.
- `eval_symbolic_subscript_layout` computes field offsets, sizes, and total layout size, with validation.

### Lowering (`crates/core/src/lower.rs`)

- Added symbolic subscript-aware resolution path:
  - `resolve_symbolic_subscript_name`
  - `eval_index_expr`
  - `eval_index_expr_strict`
- Supports:
  - direct field addresses (`foo.field`)
  - symbolic field form (`foo[.field]`)
  - array field indexing (`foo.field[i]`)
- Emits targeted diagnostics for invalid aggregate indexing and unknown fields (with suggestions).

### Formatter (`crates/fmt/src/print_ast.rs`)

- Supports printing symbolic subscript array declarations and `Expr::Index`.

## Tests

Unit tests were added/updated in:

- `crates/core/src/lexer.rs`
- `crates/core/src/parser/mod.rs`
- `crates/core/src/sema/mod.rs`
- `crates/core/src/lower.rs`

Validation run:

- `cargo fmt`
- `cargo test -p k816-core`
- `cargo test`
