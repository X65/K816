# Simple Whitespace-Only Formatter Rewrite (Execution Plan)

## Goal
Replace `crates/fmt/src/print_ast.rs` with a simpler, whitespace-only formatter that:
- formats only targeted top-level regions,
- enforces recursive multiline delimiter newline/indent rules,
- enforces minimal statement-separator spacing (rule 4) only when AST spans are available,
- keeps non-whitespace bytes unchanged.

## Scope
Targeted top-level regions:
- rooted blocks: `func` / `naked` / `data`
- top-level `var` declarations
- top-level evaluator blocks (`[ ... ]`)
- special care: multiline symbolic var bracket blocks (`var Foo[ ... ] = ...`)

Out of scope:
- semantic rewrites
- literal canonicalization
- keyword/operator canonicalization
- token text rewrites beyond spaces/newlines

## Hard Constraints
- Modify only whitespace layout (`' '`, `'\n'`) and leading tabs converted to 4 spaces.
- Do not change any non-whitespace byte.
- Keep same-line matched delimiters unchanged.
- For multiline matched pairs (`{}`, `[]`, `()`):
  - newline after opener
  - newline before closer
  - recursively indent interior by nesting level
- Indentation changes only leading whitespace of each line.
- Comment-only lines are also indented.
- Default indent width: 4 spaces.

## Rule 4 (statement boundaries)
Applied only when raw AST spans are available:
- mixed assignment spacing (`a= x`, `a =x`) -> normalize to `a = x`
- if spaced style (`a = x`), boundary to next same-line statement must be:
  - newline, or
  - `; `, or
  - at least two spaces
- if compact style (`a=x`), boundary must be:
  - newline, or
  - `;`, or
  - at least one space
- treat these as minimums; keep larger spacing/alignment unchanged

## Parse Failure Behavior
- Best-effort text mode:
  - discover/format regions by textual scan + delimiter matching
  - skip malformed regions without matching delimiters
  - continue formatting remaining regions
- in fallback mode, skip rule 4 entirely

## Top-level blank lines
- ensure at least one empty line between rooted (`func`/`naked`/`data`) blocks
- compact consecutive empty lines to a single empty line

## Integration
- Keep `pretty` crate as composition/render base.
- Add `k816_fmt::format_source(&str) -> String`.
- Keep `format_file(ast, source)` as compatibility wrapper calling `format_source`.
- Add parser API `parse_lenient_raw` (no preprocessor) for accurate raw spans.
- Update CLI `fmt` and LSP formatting to use `format_source` directly.

## Validation
- `cargo test -p k816-fmt`
- `cargo test --test cli fmt_`
- `cargo test --test lsp_cli lsp_completion_symbols_and_formatting_idempotence`
- spot-run `k816 fmt` on affected golden fixtures

