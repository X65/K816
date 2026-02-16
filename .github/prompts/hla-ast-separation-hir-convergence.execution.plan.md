# Enforce Native-vs-HLA Separation in AST (All Sugar), Merge Only in HIR

## Summary
Refactor the frontend so all HLA sugar stays as HLA AST and only collapses to native instruction forms during lowering to HIR.
After this change, `Stmt::Instruction` represents only native mnemonic syntax, while sugar forms like `a!!`, `a=...`, `< goto ...`, `return`, `*`, etc. remain distinct in AST.

## Scope
- AST/API updates in `crates/core/src/ast/mod.rs`
- Parser updates in `crates/core/src/parser/mod.rs`
- Lowering updates in `crates/core/src/lower.rs`
- Formatter updates in `crates/fmt/src/print_ast.rs`
- Pass-through updates in `crates/core/src/eval_expand.rs` and `crates/core/src/normalize_hla.rs`
- Tests and docs updates

## Requirements
1. Distinguish native mnemonics from HLA sugar at AST level.
2. Converge to instruction stream only in lowering/HIR.
3. Cover all current sugar families, including flow and nop shorthand.
4. Keep behavior-equivalent HIR output for existing fixtures.

## Implementation Outline
1. Add explicit HLA variants/enums for sugar families in AST.
2. Convert parser sugar paths to emit `Stmt::Hla(...)` variants.
3. Move assignment-chain decomposition from parser to lowering.
4. Lower all new HLA variants to existing instruction operations.
5. Update formatter to print sugar for HLA variants.
6. Update expansion/normalization to preserve and transform new variants.
7. Update parser/lowering/cli/lsp/golden tests.

## Verification
- `cargo test -p k816-core`
- `cargo test -p k816-fmt`
- `cargo test -p k816-golden-tests`
- `cargo test`
