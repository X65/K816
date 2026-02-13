# Implement Top-Level `const` Keyword With Strict Identifier Diagnostics

## Summary

Add first-class top-level `const` declarations with required initializers and comma-list support, and make undeclared identifiers in numeric/immediate expression evaluation a hard error (no silent fallback to `0`).

## Locked Scope

- `const` is top-level only.
- Initializer is required: `const NAME = expr`.
- Comma lists are supported: `const A = 1, B = 2`.
- Undeclared identifiers in immediate/value numeric evaluation are errors.

## Public Types / Interfaces

- Add `TokenKind::Const` in `crates/core/src/lexer.rs`.
- Add AST `ConstDecl` and `Item::Const(ConstDecl)` in `crates/core/src/ast/mod.rs`.
- Extend semantic model with constants map and metadata in `crates/core/src/sema/mod.rs`.
- Extend LSP semantic cache/symbol handling to include constants in `crates/lsp/src/lib.rs`.

## Implementation Steps

1. Golden tests first (red)

- Add fixture `tests/golden/fixtures/syntax-const-declarations/` for successful `const` use cases.
- Add fixture `tests/golden/fixtures/syntax-const-err-missing-initializer/`.
- Add fixture `tests/golden/fixtures/syntax-const-err-unknown-identifier/`.
- Update evaluator fixtures that currently rely on silent unknown-id fallback.

1. Lexer / parser / AST

- Add `const` token and parser token message.
- Add `const_decl_parser` and wire into top-level `item_parser` only.
- Keep `const` invalid in `stmt_parser`.
- Extend preprocessing comma-splitting to include `const`.
- Add parser unit tests for new `const` grammar and error paths.

1. Semantic analysis

- Add constant collection and duplicate symbol checks across vars/functions/consts.
- Evaluate const expressions with source-order resolution.
- Allow const refs in var initializer, var array length, symbolic field count.
- Add semantic tests for const resolution and duplicate handling.

1. Lowering

- Resolve identifiers through vars/symbolic-subscripts/consts in numeric paths.
- Remove permissive unknown-id `Some(0)` fallback.
- Preserve address/label relocation behavior for true address operands.
- Add lower tests for constant resolution and unknown-id diagnostics.

1. Downstream integration

- Handle `Item::Const` in eval expansion and normalization passes.
- Add AST formatting for `const` in fmt crate.
- Add `const` to LSP directive keywords, hover text, semantic cache, and symbols.

1. Verify

- `cargo test -p k816-core`
- `cargo test -p k816-lsp`
- `cargo test -p k816-golden-tests`
- `cargo clippy -p k816-core -p k816-lsp --tests`

## Acceptance Criteria

- Top-level `const` declarations parse and compile.
- `const` inside blocks is rejected.
- Missing initializer is rejected.
- Undeclared numeric/immediate identifiers fail with diagnostics.
- Golden fixtures and core/lsp tests pass.
