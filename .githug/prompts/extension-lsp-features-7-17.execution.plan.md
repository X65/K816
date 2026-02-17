# Implement LSP Features 7-17 (Single PR)

## Scope
- Add/finish LSP features 7-17 in `crates/lsp`.
- Keep extension-side changes minimal (`editors/vscode-k816`).
- Use safe symbol-only rename.
- References include declarations.
- Signature help covers evaluator built-ins only.
- Semantic tokens include local/global and resolved/unresolved distinctions.
- No preprocessor `#if/#endif` folding in v1.

## Steps
1. Add capability wiring and request handlers for references, rename, semantic tokens, inlay hints, code lens, folding ranges, and signature help.
2. Add symbol occurrence indexing helpers to support references/rename/code-lens.
3. Implement find references and prepare-rename/rename with canonical symbol matching.
4. Extend completion with register completions + richer docs.
5. Add semantic token legend and full-document token emission.
6. Add inlay hints for resolved addresses, byte sizes, and width context.
7. Add hover metadata for instruction flags/cycles/addressing using static instruction metadata.
8. Add code lenses for reference counts and resolved addresses on labels/functions/segments.
9. Add folding ranges for brace-based constructs.
10. Add signature help for evaluator built-in calls.
11. Tighten diagnostics freshness for workspace-wide requests.
12. Add tests in `crates/lsp/src/lib.rs` and `tests/lsp_cli.rs`.
13. Run `cargo test -p k816-lsp` and `cargo test --test lsp_cli`.

## Acceptance
- New capabilities advertised in initialize response.
- Requests return valid responses for project fixtures.
- Existing tests continue to pass.
