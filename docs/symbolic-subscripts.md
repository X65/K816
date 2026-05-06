# Symbolic Subscripts and Symbolic Subscript Field Lists

This document describes the K65 dialect feature for declaring packed symbolic subscript arrays on fixed addresses and accessing their fields with symbolic subscripts.

Symbolic subscript arrays are inspired by the Pawn language's named-field array style.

## Overview

A symbolic subscript array declaration binds a symbol to a base location and defines a field layout on top of it. As with any other `var`, the base may be either:

- **Fixed** — `var foo[...] = $6000` pins the layout to a compile-time constant address. Use this for hardware register blocks (RIA, CGIA) where the base is dictated by the silicon.
- **Allocated** — `var foo[...]` (no `= ...`) leaves the base to the linker. The struct lands inside the current segment and the linker assigns its final address per the `.ld.ron` config, the same way it places code/data chunks. Field references resolve through per-field section symbols emitted by the lowerer (`foo.field_w`, `foo.idx`, …) so each name is independently relocatable.
- **Abstract** — `abstract var foo[...]` declares the same packed field layout but no storage. It is valid only for metadata queries such as `foo:sizeof` and `foo.field:offsetof`; it emits no bytes or linker symbol.

Both forms produce identical field layouts.

- No fixed-address bytes are emitted by `var = $addr` declarations; Allocated subscript vars emit zero-filled bytes in their segment so the linker reserves the slot.
- `data { ... }` remains the mechanism for emitting initialized bytes.
- Field layouts are packed (no padding).
- A `* count` suffix repeats the whole layout and makes elements addressable as `foo[i]`. Declaration `[N]` reserves a raw span; it does not create repeated aggregate elements.

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

### Abstract layout-only form

```k65
abstract var handler[
  .open  :word
  .close :word
  .init  :word
]
```

`abstract var` reuses the same field item grammar, including nested fields and default field width on the declaration (`abstract var point:byte[...]`). It cannot use an address initializer, storage prefix, allocation count, or array-only bracket form.

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

Repeat element form for declarations with `* count`:

```k65
var foo[
  .field_w   :word
  .string[20]:byte
] * 8 = $4000

lda &&foo[2]               // &&foo + 2*foo:sizeof
lda foo[2].field_w         // element 2, .field_w offset
lda foo[2][.field_w]       // equivalent spelling
lda foo[2].string[3]       // element 2, .string offset, byte 3
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
- repeated aggregate element base = `symbolic_subscript_base + index * symbolic_subscript_base:sizeof`
- repeated field access = `element base + field_offset`

When an `= <address>` initializer is present (Fixed form), it must be a constant numeric expression — the assembler bakes the base into every reference, so the right-hand side has to reduce to a number at semantic-analysis time. The Allocated form (no `= ...`) has no such requirement: the linker assigns the base later, the same way it assigns addresses to code labels.

Repeat indices must be compile-time numeric constants and are bounds checked with `0 <= index < repeat_count`. Runtime indexing should use the CPU's indexed addressing modes or explicit pointer arithmetic instead. `foo:sizeof` is the size of one repeated element, not the total allocation, so `&&foo[i]` is equivalent to `&&foo + i*foo:sizeof`.

## Diagnostics

The implementation reports hard errors for:

- non-constant base expressions in the Fixed form (`var foo[...] = expr` where `expr` is not a compile-time number)
- duplicate field names in one symbolic subscript array
- invalid field counts (`<= 0` or out of range)
- unknown field access (with nearest-name suggestion)
- invalid indexing on symbolic subscript arrays (`foo[1]`, `foo[i]`)
- non-constant, negative, or out-of-range repeat element indices
- indexing a non-array field
- field access on scalar repeat elements (`str[1].field`)
- abstract layout address/index use (`abstract var foo[...]`, then `foo[0]`)

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
  `foo.field[i]` and for repeat element roots such as `foo[i]`.
- `Expr::Member { base, field, .. }` represents field selection after a non-ident
  expression, such as `foo[2].field_w` and `foo[2][.field_w]`. Plain
  `foo.field_w` and `foo[.field_w]` still flatten to the dotted identifier.
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
  - repeated elements and fields (`foo[i]`, `foo[i].field`, `foo[i].field[j]`)
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
