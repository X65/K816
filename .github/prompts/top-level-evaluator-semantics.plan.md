# Implement Top-Level Evaluator Blocks With Full Expression Semantics

## Summary

Implement full evaluator expression semantics for top-level standalone evaluator blocks (`[ ... ]` outside section bodies), and make those assignments usable as compile-time constants across the file.

## Locked Scope

- Implement evaluator execution for top-level evaluator blocks only.
- Do not implement `evaluator [ ... ]` and `for ... eval [ ... ]` data directives in this pass.
- Rebaseline existing evaluator golden fixtures to match new behavior.
- Keep constants as numeric values (int/float capable).
- Require exact integer values in structural integer-only contexts.
- Defer context/side-effect evaluator functions with explicit diagnostics.
- Defer `rnd()` with explicit diagnostic.
- Disallow cross-item reassignment of existing constant names.
- Allow mutation within a single evaluator block (`++`, `--`, compound assign).

## Public Types / Interfaces

- Add top-level AST representation for evaluator blocks in `crates/core/src/ast/mod.rs`.
- Extend evaluator API in `crates/eval` to support context, assignment, and numeric value types.
- Extend semantic constant representation in `crates/core/src/sema/mod.rs` from integer-only to numeric values.

## Implementation Steps

1. Parser and preprocess
- Preserve top-level bracket evaluator lines in `preprocess_source`.
- Parse top-level `TokenKind::Eval(_)` into a dedicated AST item instead of `Stmt::Empty`.

1. AST pass-through updates
- Handle the new evaluator item in `eval_expand`, `normalize_hla`, `lower`, `fmt`, and `lsp`.

1. Evaluator engine
- Implement full expression grammar for top-level evaluator execution:
  - assignment and compound assignment
  - comma sequencing
  - ternary
  - logical/comparison/bitwise/shift
  - prefix/suffix inc/dec
- Add symbol table context for identifier resolution and mutation.

1. Function support policy
- Implement pure math functions in this pass.
- Emit clear diagnostics for deferred functions (`size`, `addbyte`, `print`, `error`, `color`, `index`, `rnd`).

1. Semantic integration
- Evaluate top-level evaluator blocks in source order.
- Merge resulting values into semantic constants for later `const`, `var`, and expression use.
- Reject cross-item constant redefinition.
- Permit in-block mutation while evaluating the same block.

1. Structural coercion
- In structural contexts (`var` address, array sizes, symbolic counts, `align`, `address`, `nocross`), require exact integer-valued results.
- Emit diagnostic if value is non-integer or out of range for target type.

1. Lowering updates
- Read numeric constants from sema and apply context-appropriate coercion.
- Keep relocation/address behavior unchanged for label-based addressing.

1. Diagnostics
- Keep primary errors concise.
- Put remediation in `with_help(...)`.
- Report evaluator errors at bracket block spans.

## Tests

1. `crates/eval` unit tests
- precedence/associativity
- assignment/mutation
- ternary/comma/logical behavior
- function behavior and deferred-function diagnostics

1. `crates/core` parser/sema/lower tests
- top-level evaluator parsing
- source-order constant availability
- cross-item reassignment rejection
- in-block mutation acceptance
- structural exact-integer enforcement

1. Golden tests
- Rebaseline:
  - `tests/golden/fixtures/syntax-evaluator-expressions`
  - `tests/golden/fixtures/syntax-evaluator-index-and-dot-call`
- Keep data-directive evaluator fixtures unchanged for this pass.

## Verification

- `cargo test -p k816-core`
- `cargo test -p k816-eval`
- `cargo test -p k816-golden-tests`
- `cargo clippy -p k816-core -p k816-eval --tests`

## Assumptions

- Top-level evaluator behavior in docs is authoritative for this implementation.
- Deterministic output remains required, so `rnd()` is deferred.
- Data-block evaluator directives are explicitly out of scope for this pass.
