# Segment Keyword Migration Plan

## Goal

Move source-level memory region selection from `bank` to cc65-inspired `segment`.

## Scope

- Make `segment <name>` the canonical directive.
- Keep `bank <name>` as a compatibility alias.
- Propagate naming through AST/HIR/lowering/emit and linker selection rules.
- Update fixtures and formatters to emit `segment`.

## Implementation Steps

1. Parser/Lexer front-end:
   - Add `segment` token.
   - Parse both `segment` and `bank` into a single AST node (`Item::Segment`).
2. Core model update:
   - Rename AST and HIR variants from bank-selection to segment-selection.
   - Update lowering and emit backends to consume the renamed HIR op.
3. Linker config compatibility:
   - Replace `SegmentRule.bank` with `SegmentRule.segment`.
   - Accept legacy `bank` field through serde alias for transition.
4. User-facing output:
   - Update AST/IR formatters to print `segment`.
   - Update docs/fixtures/examples to use `segment`.
5. Validation:
   - Run full test suite.
   - Ensure old `bank` syntax still parses and links.

## Acceptance Criteria

- `segment code` works end-to-end.
- Existing `bank code` sources still work.
- Linker config supports both `segment` and legacy `bank` keys.
- All tests pass.
