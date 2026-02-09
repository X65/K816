# Chumsky Full Rewrite of Main Grammar Parser (Direct Replace)

## Summary
Replace the handwritten main parser in `crates/core/src/parser/mod.rs` with a full `chumsky 0.12` token-stream parser now, then remove the manual `Parser { tokens, pos }` implementation entirely.  
Chosen direction:
1. **Full rewrite now**
2. **Allow grammar cleanup**
3. **Direct replace** (no feature flag, no dual runtime path)

## Public API / Interface Impact
1. Keep public entrypoints unchanged:
- `parse(source_id, source_text) -> Result<File, Vec<Diagnostic>>`
- `parse_expression_fragment(source_id, source_text) -> Result<Spanned<Expr>, Diagnostic>`

2. Keep AST/HIR boundary unchanged:
- Parser still outputs existing AST types from `crate::ast` so downstream `sema`, `lower`, `emit` continue working without interface changes.

3. Internal parser architecture changes:
- Remove handwritten `Parser` struct and all cursor helpers (`bump`, `expect_*`, `at_kind`, `recover_to_boundary`, etc.).
- Introduce compositional `chumsky` parsers over `logos` tokens via `Stream<(TokenKind, SimpleSpan)>`.
- Introduce a dedicated internal conversion layer from `Rich<TokenKind>` to `Diagnostic`.

## Implementation Plan
1. **Build token input adapter**
- Keep `lex(...)` from `crates/core/src/lexer.rs`.
- Standardize a single adapter in `parser/mod.rs` to convert `Vec<Token>` into `chumsky::input::Stream` with correct EOI span.

2. **Define grammar parsers as composable units**
- Implement parsers for:
  - atoms/expr (`number`, `ident`, `eval`)
  - var declarations
  - data args/commands/blocks
  - statements (`call`, label, instruction, `.byte`, nested `data`, `var`)
  - items (`bank`, `var`, `data`, function/main blocks, top-level stmt)
- Use `recursive(...)` and `choice(...)` idioms from `chumsky 0.12`.

3. **Model separators and block structure idiomatically**
- Encode newline and brace boundaries explicitly instead of manual cursor checks.
- Replace loop-based block parsing with combinators (`repeated`, `delimited_by`, `separated_by`, `or_not`).

4. **Error recovery and diagnostics mapping**
- Use `recover_with(...)` strategies (line/block boundary-aware) to continue after statement-level failures and preserve multi-error behavior.
- Convert `Rich` errors into current `Diagnostic` model with:
  - primary span
  - human-readable message
  - optional labels/hints where useful.

5. **Direct replace and cleanup**
- Delete handwritten parser implementation once combinator parser passes parity checks.
- Keep only combinator-based parser codepath for `parse(...)`.

6. **Grammar cleanup allowance**
- Apply small grammar simplifications where they reduce ambiguity/complexity, provided they do not break core project fixtures or downstream phases.
- Document any intentional syntax/diagnostic behavior changes in parser module comments and PR notes.

## Test Plan
1. **Preserve and extend existing parser tests**
- Keep current tests in `crates/core/src/parser/mod.rs`.
- Add focused tests for every grammar area:
  - block modifiers and ordering
  - labels vs instructions disambiguation
  - `.byte` list parsing
  - `data` command variants and typed bounds (`u16`, `u32`)
  - var array length eval fragments
  - trailing/leading separator handling.

2. **Negative and recovery tests**
- Add tests asserting:
  - multiple diagnostics from one file when recoverable
  - accurate span placement for common syntax failures
  - recovery at newline and `}` boundaries.

3. **Cross-pipeline safety**
- Run full workspace tests (`cargo test`) and ensure:
  - `sema`, `lower`, `emit`, golden tests pass
  - no interface regressions in downstream stages.

4. **Acceptance criteria**
- Handwritten parser removed.
- `parse(...)` and `parse_expression_fragment(...)` signatures unchanged.
- All tests pass.
- Diagnostics remain actionable with correct spans.

## Assumptions and Defaults
1. Keep `logos` as the lexer and parse tokens (not raw chars).
2. Keep AST type definitions unchanged.
3. Diagnostic **wording** may change where `chumsky` error rendering differs, but spans and usability must remain strong.
4. No transitional feature flag: direct replacement in one change set.
