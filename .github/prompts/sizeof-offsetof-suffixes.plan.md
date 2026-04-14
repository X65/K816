# Plan: `:offsetof` and `:sizeof` metadata suffixes

## Summary

Add `:offsetof` and `:sizeof` as compile-time metadata suffix operators on variable and field expressions. These join the existing colon-suffix family (`:byte`, `:word`, `:far`, `:abs`) and produce plain numeric values rather than addresses.

Simultaneously, remove the `::` offset operator (`TASKS::state`) — the `:offsetof` suffix supersedes it entirely with clearer, more consistent syntax.

```k65
// Before (confusing — looks almost identical)
lda #TASKS.state          // absolute address
lda #TASKS::state         // byte offset (huh?)

// After (self-documenting)
lda #TASKS.state          // absolute address $4002
ldx #TASKS.state:offsetof // byte offset 2
cpx #TASKS:sizeof          // total struct size 64
```

## Phase 1 — AST and Parser

### 1.1 Add `MetadataQuery` enum to AST

In `crates/core/src/ast/mod.rs`, add:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MetadataQuery {
    SizeOf,
    OffsetOf,
}
```

Add new `Expr` variant:

```rust
MetadataQuery {
    expr: Box<Expr>,
    query: MetadataQuery,
},
```

### 1.2 Add `metadata_query_parser` in parser

In `crates/core/src/parser/data.rs`, alongside `data_width_parser` and `address_hint_parser`.

### 1.3 Extend suffix application in `expr_parser`

In `crates/core/src/parser/expr.rs`:

- Add `ExprSuffix::MetadataQuery(MetadataQuery)` variant
- In the suffix application loop, handle as terminal: error if combined with other suffixes
- Wire parser: `.or(metadata_query_parser().map(ExprSuffix::MetadataQuery))`

## Phase 2 — Eval Expansion

In `crates/core/src/eval_expand.rs`, add `Expr::MetadataQuery` arm in `expand_expr()`: recurse into inner expr, preserve query.

## Phase 3 — Sema Const Evaluation

In `crates/core/src/sema/consts.rs`, `eval_const_expr()`: `Expr::MetadataQuery` cannot resolve at sema time (no `VarMeta` access) — extract identifier and return `ConstExprError::Ident`.

## Phase 4 — Lowering Resolution

- `eval_to_number_strict` / `eval_to_number`: resolve `SizeOf` via `VarMeta.size` / `SymbolicSubscriptFieldMeta.size`; resolve `OffsetOf` via `SymbolicSubscriptFieldMeta.offset`
- `is_immediate_expression`: return `true` (always produces a number)
- `resolve_operand_ident`: produce `OperandOp::Immediate` (not address)

## Phase 5 — Remove `::` offset operator

- Revert lexer `Ident` regex: remove `|::[A-Za-z_]` alternative
- Remove `ResolvedSymbolicSubscriptName::FieldOffset` variant and `split_once("::")` branch
- Remove all `FieldOffset` match arms in lowering
- Re-bless `syntax-invalid-double-colon` golden fixture

## Phase 6 — Golden Tests

- New positive fixture: `syntax-sizeof-offsetof`
- New error fixture: `syntax-sizeof-offsetof-errors`
- Re-bless `syntax-invalid-double-colon`

## Decisions

- `:sizeof` / `:offsetof` chosen over `sizeof()` prefix — matches existing colon-suffix idiom
- `:offsetof` requires a dotted field path — bare `VAR:offsetof` is an error
- `:sizeof` works on both aggregates and fields
- Produce immediates, not addresses — compile-time numeric metadata
- Not valid in sema const contexts — resolved during lowering (same as var addresses)
- Complete `::` removal — no deprecation period; undocumented + untested in positive golden cases
