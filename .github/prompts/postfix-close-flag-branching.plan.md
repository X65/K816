# Postfix Close Operators Should Branch on Existing Flags

## Problem
Current lowering emits implicit `CMP #0` for postfix loop-close operators like `} n-?`, producing wrong output for sequences such as:

```k65
{
  a=ready
} n-?
```

Expected: `LDA ready` followed by `BPL` (no compare).
Current: `LDA ready`, `CMP #0`, `BPL`.

## Goals
1. Postfix close operators (`} ==`, `} !=`, `} <`, `} >=`, `} <=`, `} >`, `} n-?`, `} n+?`) branch from existing flags without injecting compare.
2. Explicit compare close (`} a?... OP`) remains compare-driven.
3. Condition-seed pattern (`a?rhs` before `} OP`) remains compare-driven via normalization fusion.
4. Emit parser warnings for inefficient postfix expansions:
   - `} <=` (2 branch instructions)
   - `} >` (3 branch instructions)

## Design
- Introduce dedicated HLA close variants for N-flag forms to disambiguate from `>=` / `<`.
- Preserve `DoCloseWithOp` as postfix-op branch semantics (no implicit compare).
- Keep normalization fusion only for `ConditionSeed + DoCloseWithOp`.
- Stop rewriting standalone `DoCloseWithOp` into `DoClose`.

## Lowering Rules
- `} ==` -> `BEQ target`
- `} !=` -> `BNE target`
- `} <` -> `BCC target`
- `} >=` -> `BCS target`
- `} n-?` -> `BPL target`
- `} n+?` -> `BMI target`
- `} <=` -> `BCC target`; `BEQ target` (warn)
- `} >` -> `BEQ skip`; `BCC skip`; `BRA target`; `skip:` (warn)

## Files to Update
- `crates/core/src/ast/mod.rs`
- `crates/core/src/parser/mod.rs`
- `crates/core/src/normalize_hla.rs`
- `crates/core/src/eval_expand.rs`
- `crates/core/src/lower.rs`
- `crates/fmt/src/print_ast.rs`
- Golden fixtures expected outputs impacted by postfix close semantics.

## Tests
- Parser test for `} n-?` / `} n+?` mapping to dedicated variants.
- Parser warning tests for postfix `} <=` and `} >`.
- Lowering tests for postfix-close no-compare behavior.
- Golden fixtures:
  - `syntax-wait-loop-lda` should have no `CMP`.
  - update any postfix-close fixtures currently depending on implicit compare.
