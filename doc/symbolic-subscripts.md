# Symbolic Subscripts and Symbolic Subscript Field Lists

This document describes the K65 dialect feature for declaring packed symbolic subscript arrays on fixed addresses and accessing their fields with symbolic subscripts.

Symbolic subscript arrays are inspired by the Pawn language's named-field array style.

## Overview

A symbolic subscript array declaration binds a symbol to a base location and defines a field layout on top of it. As with any other `var`, the base may be either:

- **Fixed** — `var foo[...] = $6000` pins the layout to a compile-time constant address. Use this for hardware register blocks (RIA, CGIA) where the base is dictated by the silicon.
- **Allocated** — `var foo[...]` (no `= ...`) leaves the base to the linker. The struct lands inside the current segment and the linker assigns its final address, the same way it places code/data chunks. Field references resolve as the base symbol plus a constant offset (`foo+2`), which the assembler carries into the object file and the linker completes after placement — one relocatable name for the declaration, and arithmetic on it for the fields.
- **Abstract** — `abstract var foo[...]` declares the same packed field layout but no storage. It is valid only for metadata queries such as `foo:sizeof` and `foo.field:offsetof`; it emits no bytes or linker symbol.

Both forms produce identical field layouts.

- No fixed-address bytes are emitted by `var = $addr` declarations; Allocated subscript vars reserve their bytes in the current segment (a ca65 `.res`) so the linker sets the slot aside.
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
  .string    :byte [20]
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

`abstract var` reuses the same field item grammar, including nested fields and default field width on the declaration (`abstract var point:byte[...]`). It cannot use an address initializer, storage prefix, allocation count, or array-only bracket form; each of those is `K816-E0136`, the same code a use of the layout as an operand gets.

### Default field width on the symbol

```k65
var baz:byte[
  .a
  .b
  .c
  .len:word
] = 0x2244
```

On a declaration that has fields, the `:` suffix is the *default field width* and nothing else: the datum is the whole layout, so the variable itself claims no register width and a load of it is never checked against one. The default carries into nested lists, and a nested field's own suffix replaces it for its sub-list — in `var v:word[ .a .b:byte[ .c .d ] ]`, `.a` is a word and `.c` and `.d` are bytes.

### Field item grammar

- `.name :byte`
- `.name :word`
- `.name :far`
- `.name :byte [count]`
- `.name:word[count]`
- `.name:far[count]`
- `.name[count]` (shorthand using the var's default field width, or `:byte` when no default is declared)
- `.name[ ...nested fields... ]`
- `.name` (defaults to `:byte` when omitted)

Explicit typed counts put the type before `[count]`, matching top-level
`var NAME:type[count]`. The old `.name[count]:type` order is rejected; write
`.name:type[count]`, or `.name[count]` when the default field width is intended.

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
  .string    :byte [20]
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

| Condition | Code |
| --- | --- |
| A non-constant base in the Fixed form (`var foo[...] = expr`) | `K816-E0108` |
| A field name declared twice in one list | `K816-E0102` |
| An element count of zero, on a field (`.str[0]`) or on an allocation count (`* 0`) | `K816-E0141` |
| An element count of zero on a declaration's raw span (`var buf[0]`) | accepted; it reserves nothing, as K65 does |
| A field the layout does not declare, with a nearest-name suggestion | `K816-E0101` |
| A `.field` selected from something that has no fields — a plain variable, a scalar field, an imported name | `K816-E0115` |
| A repeat or field index that does not fold | `K816-E0134` |
| An index outside the extent, which includes indexing a field that declared no count | `K816-E0135` |
| An `abstract var` used where storage is required, or declaring storage it cannot have | `K816-E0136` |
| `:offsetof` on something that is not a field, or `:sizeof` on something with no size | `K816-E0115` |

Indexing a symbolic subscript array with a number is *not* an error: a variable with no `* count` has no elements, so `foo[1]` is the ordinary byte-offset arithmetic every other countless bracket is, and `foo[.field]` is how a field is selected. Only the field's own bracket and a `* count` variable's are checked.

## Implementation Notes

These notes describe the **original K65 compiler**; the paths below are that
tree's, not this one's. K816 splits the same work differently: the packing
arithmetic is `src/layout.rs`, the declaration pass that drives it is
`src/semantic.rs`, and every access — `foo.field`, `foo[.field]`,
`foo.field[i]`, `foo[i]`, and the two metadata queries — is one walk in
`src/resolve.rs`, so an address, a size, and an offset cannot be derived from
one path by three different rules.

The K65 implementation is split across the normal frontend stages instead of
using a dedicated lexer mode.

### AST and parser

- `VarDecl` stores symbolic-subscript layouts in `symbolic_subscript_fields`.
- `SymbolicSubscriptFieldDecl` supports:
  - optional width (`:byte`, `:word`, `:far`)
  - optional element count (`.name:type[count]` or default-width `.name[count]`)
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
