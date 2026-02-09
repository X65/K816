# Linker Segment Rule `id` Migration Plan

## Goal

Make linker segment rules unambiguous by separating:

1. Rule identity (for diagnostics and referencing) from
2. Segment selector (the object section name being matched).

Concretely, rename `segments[*].name` to `segments[*].id` in `.k816ld.ron`, keep `segment` as the canonical selector, and remove behavior that makes rule identity look like a selector.

## Summary

Current `SegmentRule` has both:

- `name` (rule label), and
- `segment` (section selector).

This causes confusing configs such as:

- `name: "INFO"` and `segment: Some("INFO")` on the same rule.

The migration introduces `id` for the rule label and keeps selection strictly based on `segment` (or legacy `bank`). This preserves clarity and prevents accidental matching by label.

## Scope

- In scope:
  - `crates/link/src/lib.rs` linker config schema and rule selection.
  - Linker config validation and error messages.
  - CLI/config docs and examples using segment rules.
  - Linker + CLI + golden tests that depend on segment-rule matching.
- Out of scope:
  - Redesign of memory placement algorithm.
  - Any new output file formats.

## Public Interface Changes

### Config schema

- `SegmentRule` field rename:
  - Old: `name: String`
  - New: `id: String`

- `segment` remains the canonical section selector.
- `bank` remains accepted as a legacy alias for `segment`.

### Backward compatibility

- Deserialization accepts both `id` and legacy `name` during transition.
  - Prefer `id` when both are provided.
  - If only `name` is provided, map it to `id`.

- Selection by rule label (`name`/`id`) is removed.
  - Matching uses only:
    1. `segment`/`bank` explicit match.
    2. Fallback default rule with no selector.

## Implementation Plan

1. **Schema update (`crates/link/src/lib.rs`)**
   - Rename `SegmentRule.name` to `SegmentRule.id`.
   - Add serde compatibility for legacy `name`.
   - Keep serialized output/docs/examples on `id`.

2. **Selection logic cleanup**
   - Delete label-based fallback (`matches_rule_name` path).
   - Keep deterministic order:
     - first explicit selector match,
     - then default selector-less rule,
     - otherwise error.

3. **Validation and diagnostics**
   - Add validation for:
     - non-empty `id`,
     - unique `id` values across `segments`.
   - Update all diagnostics to reference `rule id` instead of `rule name`.

4. **Docs and examples**
   - Update `docs/linker-ron-format.md`:
     - field reference from `name` to `id`,
     - selection semantics (no label-based matching).
   - Update linker configs in examples/tests to use `id`.

5. **Tests**
   - Unit tests in `crates/link`:
     - parse config with `id`,
     - parse legacy `name` as compatibility input,
     - explicit selector precedence,
     - default selector fallback,
     - failure when no selector/default rule exists.
   - Regression test:
     - config with label equal to section but without `segment` does not silently match by label.
   - CLI/golden tests:
     - ensure migrated configs still link as expected.

## Acceptance Criteria

1. `.k816ld.ron` examples and docs use `id`, not `name`, for segment rules.
2. Linker no longer matches segments by rule label.
3. Legacy configs using `name` still parse during migration window.
4. Errors/diagnostics consistently reference `rule id`.
5. Full test suite passes after fixture migration.

## Assumptions and Defaults

- `segment` remains the only canonical selector; label matching is intentionally removed.
- Legacy `name` compatibility is retained for at least one migration cycle.
- If both `id` and `name` appear in one rule, `id` wins and `name` is ignored.
