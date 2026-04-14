# Var Allocation Count (`* count`) Syntax

## Summary

Extend `var` syntax to allow `* count` before the address assignment, multiplying the computed variable size by `count` for address advancement. This enables allocating arrays of compound (symbolic subscript) structures with a single declaration.

## Syntax

```k65
// Compound var with allocation count:
var COMPOUND[
    .one   :byte
    .two   :word
    .three :far
] * count = $4000
// Total allocation: count × (1 + 2 + 3) = count × 6 bytes

// Simple typed var with allocation count:
var simple :byte * 32 = $1000
// Total allocation: 32 × 1 = 32 bytes
```

`count` can be any constant expression known at compile time.

## Implementation

### Phase 1: AST
- Add `alloc_count: Option<Expr>` field to `VarDecl` in `crates/core/src/ast/mod.rs`.
- Add `alloc_count: None` in every existing `VarDecl` construction site.

### Phase 2: Parser
- In `var_decl_parser()` in `crates/core/src/parser/data/vars.rs`, parse optional `* <expr>` between bracket payload and `= <addr>` initializer.

### Phase 3: Eval Expand
- In `expand_var()` in `crates/core/src/eval_expand.rs`, expand the `alloc_count` field the same way `array_len` is expanded.

### Phase 4: Semantic Analysis
- In `eval_var_layout()` in `crates/core/src/sema/vars.rs`, extract base layout computation to `eval_var_base_layout()`, then multiply final size by evaluated `alloc_count` if present. Validate it's positive integer using same error patterns as `array_len`.

### Phase 5: Golden Test
- Bless: `cargo test -p k816-golden-tests --features golden-bless syntax_allocate_multiple`
- Verify: `cargo test -p k816-golden-tests`

## Relevant Files

- `crates/core/src/ast/mod.rs` — `VarDecl` struct
- `crates/core/src/parser/data/vars.rs` — `var_decl_parser()`
- `crates/core/src/parser/items.rs` — image/binary VarDecl fallback
- `crates/core/src/eval_expand.rs` — `expand_var()`
- `crates/core/src/sema/vars.rs` — `eval_var_layout()` / `eval_var_base_layout()`
- `tests/golden/fixtures/syntax-allocate-multiple/` — golden test fixture

## No Changes Needed

- `normalize_hla.rs` — pass-through clone
- `lower.rs` / `emit_object.rs` — work with `VarMeta` from sema
- `fmt/` — text-based formatting
- `lsp/` — no direct VarDecl field access

## Decisions

- `alloc_count` is a top-level multiplier on total var size, distinct from `array_len` (element-level inside brackets). Both can coexist.
- `alloc_count` does NOT create new symbolic subscript entries — only scales total allocation for address advancement.
