# Plan: Direct `mem = 0` → STZ syntax

## Context

Gap 2 of the sibling plan (`hla-stack-stz-addr-modes.plan.md`, already implemented) added a **peephole** that rewrites `LDA #0; STA mem` runs into `STZ mem`. The peephole fires on the chain form `mem = a = 0` because that's the only way the current grammar lets a user say "zero this memory cell."

The chain form has two drawbacks:

1. It reads as "also clobber A" — which is *sometimes* the user's intent, so the peephole's `a_dead_at` heuristic only drops `LDA #0` before a handful of obvious A-killers (LDA/PLA/TXA/TYA). Any other op leaves the `LDA` in place, losing the 2-byte savings.
2. It forces users who just want to clear memory to go through A, which is noise at the source level and fragile across contract-bearing functions whose exit mode might include A.

This plan adds a **direct HLA form**: `mem = <const-that-folds-to-0>` lowers straight to `STZ mem`, with no A involvement and no peephole dependency.

## What I verified in the source

1. **Grammar dispatch** — [`parser/stmt.rs:310-338`](crates/core/src/parser/stmt.rs#L310-L338) tries parsers in order; `store_stmt` is the one that matches `<addr-expr> = <reg>`. When `store_stmt` fails (because RHS is `0`, not a register), no other alternative catches the shape `<addr-expr> = <number>`, so the parse fails with "expected something else, found number literal".
2. **Current `store_stmt_parser`** — [`parser/operations.rs:255-278`](crates/core/src/parser/operations.rs#L255-L278) rigidly consumes `operand_expr "=" (a|x|y)` and maps to `HlaStmt::RegisterStore { dest, src }`. The RHS is a `chumsky::select!` over three ident names; no expression branch.
3. **AST shape** — [`ast/mod.rs:354-390`](crates/core/src/ast/mod.rs#L354-L390) has `HlaStmt::RegisterStore { dest: HlaOperandExpr, src: HlaCpuRegister }`. Sibling variants exist for transfers, chains, ALU, etc. Adding a new variant is a standard edit.
4. **Lowering path** — [`lower.rs:3644-3665`](crates/core/src/lower.rs#L3644-L3665) lowers `RegisterStore` into an `Instruction { mnemonic: "sta"/"stx"/"sty", operand: Value { expr, index, addr_mode } }` and then calls `lower_instruction_stmt`. Re-using this pattern for a new `MemStoreZero` variant — but emitting `stz` instead — is trivial.
5. **AST pass-throughs** — [`normalize_hla.rs:157-170`](crates/core/src/normalize_hla.rs#L157-L170) and [`eval_expand.rs:230`](crates/core/src/eval_expand.rs#L230) both enumerate every `HlaStmt` variant and return a clone. New variants must be added here.
6. **Register-effect tracking** — [`lower.rs:225-345`](crates/core/src/lower.rs#L225-L345) `hla_effects` match. `MemStoreZero` reads/modifies no registers, so its entry is `RegEffects::default()`.
7. **Const folding** — [`parser/expr.rs:316`](crates/core/src/parser/expr.rs#L316) `eval_static_expr(&Expr) -> Option<i64>` handles `Expr::Number`, `Expr::EvalText` (via `k816_eval::expand`), `Expr::Index`, and arithmetic. Already used by `address_stmt` and `align_stmt` parsers for exactly this purpose. Handles `0`, `0x00`, `[2-2]`, and evaluator expressions — but **not** `Expr::Ident` pointing at a `const` name (returns `None`). Scope this plan to **parse-time folded zeros only**; let the existing peephole continue to handle `mem = a = CONST` for symbolic constants.
8. **STZ-compatible addressing modes** — from Gap 2 peephole work: STZ accepts `Direct { index: None | Some(X) }` only. Not `Y`, not `S`, not any indirect form. Use the same predicate.

## Implementation

### (a) AST variant

Add to `HlaStmt` in [`crates/core/src/ast/mod.rs`](crates/core/src/ast/mod.rs), next to `RegisterStore`:

```rust
/// `mem = 0` — direct zero-store via STZ. Set at parse time when the RHS
/// expression folds to zero.
MemStoreZero {
    dest: HlaOperandExpr,
},
```

### (b) Parser

In [`crates/core/src/parser/operations.rs`](crates/core/src/parser/operations.rs), extend `store_stmt_parser`. Keep the register-RHS branch as the fast path; add a second branch for an expression RHS that folds to zero:

```rust
let register_store = operand_expr_parser()
    .then_ignore(just(TokenKind::Eq))
    .then(/* a|x|y select */)
    .map(/* existing RegisterStore construction */);

let zero_store = operand_expr_parser()
    .then_ignore(just(TokenKind::Eq))
    .then(expr_parser())
    .try_map(|(dest, rhs), span| {
        match eval_static_expr(&rhs) {
            Some(0) => Ok(Stmt::Hla(HlaStmt::MemStoreZero { dest })),
            Some(_) => Err(Rich::custom(span,
                "non-zero constant stores must go through a register: use 'mem = a = value'")),
            None => Err(Rich::custom(span,
                "store RHS must be a register (a/x/y) or a constant that folds to zero")),
        }
    });

register_store.or(zero_store)
```

### (c) Lowering

In [`crates/core/src/lower.rs`](crates/core/src/lower.rs), next to the existing `RegisterStore` arm:

```rust
HlaStmt::MemStoreZero { dest } => {
    if !stz_compatible_mode(dest.addr_mode, dest.index) {
        diagnostics.push(Diagnostic::error(
            span,
            "'mem = 0' requires zp, zp,X, abs, or abs,X addressing — \
             use 'mem = a = 0' for other modes",
        ));
        return;
    }
    let instruction = Instruction {
        mnemonic: "stz".to_string(),
        operand: Some(Operand::Value {
            expr: dest.expr.clone(),
            force_far: false,
            index: dest.index,
            addr_mode: dest.addr_mode,
        }),
    };
    lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
}
```

Add helper `stz_compatible_mode`: accepts `OperandAddrMode::Direct` with `index` in `{None, Some(X)}`.

Also add `MemStoreZero` arm in `hla_effects` returning `RegEffects::default()`, and any other exhaustive `HlaStmt` match flagged by the compiler.

### (d) AST pass-throughs

In `normalize_hla.rs` and `eval_expand.rs`, add the pass-through clone arm. Let `cargo build -p k816-core` flag any other exhaustive matches (LSP pretty-printing, hover providers, etc.) and fix each.

## Files touched

- `crates/core/src/ast/mod.rs`
- `crates/core/src/parser/operations.rs`
- `crates/core/src/lower.rs`
- `crates/core/src/normalize_hla.rs`
- `crates/core/src/eval_expand.rs`
- `crates/core/src/parser/tests/statements.rs` — parser tests
- `tests/golden/fixtures/syntax-mem-zero-store/` — new golden fixture

## What to reuse

- `eval_static_expr` — [`parser/expr.rs:316`](crates/core/src/parser/expr.rs#L316)
- `lower_instruction_stmt` — lowering entry point used by `RegisterStore`
- STZ mode-compatibility predicate from [`peephole.rs stz_compatible`](crates/core/src/peephole.rs#L120) — factor up or duplicate (6 lines)

## Golden fixture `syntax-mem-zero-store`

```k65
var zp_cell   = 0x10
var abs_cell  = 0x0200

func main {
    zp_cell    = 0          // STZ zp        (64)
    zp_cell,x  = 0          // STZ zp,X      (74)
    abs_cell   = 0          // STZ abs       (9C)
    abs_cell,x = 0          // STZ abs,X     (9E)
    zp_cell    = [0x00]     // STZ zp via evaluator fold
    zp_cell    = [2 - 2]    // STZ zp via arithmetic fold
    return
}
```

Bless with `cargo run -p k816-golden-tests --bin bless -- --case syntax-mem-zero-store`.

## Rejection tests

```rust
#[test]
fn rejects_non_zero_constant_store() {
    let err = parse(SourceId(0), "func main {\n  mem = 1\n}\n").expect_err("must fail");
    assert!(err.iter().any(|e| e.message.contains("folds to zero")));
}
```

Plus an error-fixture for the lower-time unsupported-mode diagnostic (`(ptr),y = 0`).

## Verification

1. `cargo test -p k816-core --lib` — parser tests pass.
2. `cargo test -p k816-golden-tests -- syntax_mem_zero_store` — new fixture passes.
3. `cargo test -p k816-golden-tests -- peephole_stz_zero` — existing peephole fixture still passes (no change to peephole).
4. `cargo test --workspace` — green.
5. Spot-check: compile `zp_cell = 0` and confirm opcode `64` in the listing.

## Scope estimate

~150–200 LOC across 6 files. No ISA/HIR changes. No peephole changes. Breaking-change risk: zero (existing `mem = a = 0` continues to compile unchanged; new syntax is purely additive).
