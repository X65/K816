---
title: "K65 -> 65816 Width-Aware Syntax Extension"
project: k816
area: core (parser/AST) + lowering + diagnostics
status: draft
---

# Goal

Extend K65 syntax so it supports seamless 8/16-bit programming on 65816 with correct instruction sizing and strict mode tracking.

Key requirements:

- Explicit CPU width control for A and X/Y via `@a8/@a16/@i8/@i16`.
- Lexical scoping for mode changes inside `{ ... }` blocks (mode does not leak out).
- Correct PC-relative encoding: immediates and width-dependent opcodes depend on tracked mode.
- Strict errors: no implicit truncation (e.g. `a=0x1234` in `@a8` is an error).
- Typed variables + explicit typed views: `:byte/:word` as the only data width mechanism.
- REP/SEP generation is minimal; consecutive REP or SEP folded before opcode emission.

Non-goals:

- Replacing existing K65 control-flow syntax or register assignment syntax.
- Adding `.b/.w` suffixes (explicitly not used).
- Auto-casting or implicit widening/truncation.

# Summary of Proposed Syntax

## CPU mode (register width)

- Tokens: `@a8`, `@a16`, `@i8`, `@i16`
- Used as statements inside blocks (lexically scoped to the containing `{ ... }`).
- Also allowed in function headers as a "mode contract" (callee requirements + body assembly mode).

## Data width (memory size / view)

- Types: `:byte`, `:word`
- Variables may be declared typed: `var w:word`, `var p:byte`
- Expressions may be reinterpreted via typed view: `expr:byte` or `expr:word`

## AB swap

- Operator: `b><a` => emits `XBA`

# Semantics

## Mode Model

Two independent mode bits:

- A width: `a8` vs `a16` (65816 M flag)
- Index width: `i8` vs `i16` (65816 X flag; affects both X and Y)

Assembler tracks `Mode { a_width, i_width }` at every statement.

## Lexical Scoping for Mode Changes

Every `{ ... }` introduces a "mode frame":

- On entry, record `entry_mode`.
- Inside, `@a*` / `@i*` update `current_mode`.
- On exit, restore to `entry_mode` if different.
- Restore must also occur on early exit that leaves the frame (goto out, break, return, etc.).

Rules:

- Mode changes used inside a block MUST NOT leak outside.
- If the effective mode at block end equals `entry_mode`, emit no restore code.
- Jumping into a deeper frame is illegal (error).

## Instruction Encoding Depends on Tracked Mode

Immediate size and width-dependent instructions encode based on current mode:

- `a=#imm` uses A width: 8-bit immediate in `@a8`, 16-bit immediate in `@a16`.
- `x=#imm` / `y=#imm` use index width: 8/16-bit based on `@i8/@i16`.
- Memory operations that are width-dependent (e.g. `STA`, `LDA`) follow register width at that point.
- Any encoding that depends on width is illegal if mode is unknown.

## Strict Errors (No Implicit Truncation)

- `a=0x1234` in `@a8` is a compilation error.
- `x=0x1234` in `@i8` is a compilation error.
- `a=0x12` in `@a16` is allowed; encodes as `#$0012`.
- Similar rules apply for all immediates and evaluator results.

## Typed Variables and Typed Views (`:byte/:word`)

Declarations:

- `var p:byte = 0x80`
- `var w:word`
- `var tab:word[16]`

Typed views:

- `expr:byte` => treat memory operand as 8-bit value
- `expr:word` => treat memory operand as 16-bit value

Rules:

- Accessing `expr:word` via `a=` requires `@a16`.
- Accessing `expr:word` via `x=`/`y=` requires `@i16`.
- Storing `w:word = a` requires `@a16`.
- Storing `p:byte = a` requires `@a8`.

Assignment to a typed variable without explicit view must match its declared size:

- If `var w:word`, then `w = a` is only legal when `@a16`.
- If `var p:byte`, then `p = a` is only legal when `@a8`.
- Partial updates require explicit view and/or address arithmetic:
  - `w:byte = a` (low byte) in `@a8`
  - `(w+1):byte = a` (high byte) in `@a8`

## Function Mode Contracts ("Colors")

Function headers can specify required modes:

- `func memcpy16 @a16 @i16 { ... }`
- `far func foo @a16 @i8 { ... }`
- `inline bar @a8 @i16 { ... }`

Call-site bridging:

- If caller mode differs from callee required mode, emit minimal `REP/SEP` before call and restore after return.
- If caller already matches, emit nothing.
- Folding pass merges adjacent mode ops.

Tail calls / non-returning transfers:

- For `goto`-like transfers that do not return, do not emit post-restore.
- Any transfer that exits frames must restore those frames first (if needed).

## REP/SEP Generation Model

Do not emit `REP/SEP` immediately during parsing.
Lower mode changes to pseudo-ops:

- `@a16` => `rep(0x20)`
- `@a8`  => `sep(0x20)`
- `@i16` => `rep(0x10)`
- `@i8`  => `sep(0x10)`

Block-exit restore emits a pseudo-op sequence only for bits that differ between `current_mode` and `entry_mode`.

# Folding Pass (AST Post-Processing)

Before IR emission, run a peephole pass to fold consecutive mode ops.

Maintain:

- `want_rep_mask`
- `want_sep_mask`

For each pseudo-op:

- `rep(m)`: `want_rep |= m; want_sep &= ~m`
- `sep(m)`: `want_sep |= m; want_rep &= ~m`

Flush at statement boundary when encountering a non-mode op (or end of block):

- if `want_rep != 0`: emit single `REP #want_rep`
- if `want_sep != 0`: emit single `SEP #want_sep`
- clear both masks

This also cancels redundant toggles and ensures minimal emission.

# Control-Flow + Mode Consistency

Enforce mode consistency at labels:

- All incoming edges to a label must agree on `Mode`.
- If mismatch, compilation error: "mode mismatch at label".

Mode can become unknown after instructions that restore P from runtime:

- `PLP`, `RTI` make mode unknown.
- Any width-dependent encoding in unknown mode is an error until an explicit mode statement establishes mode again.

# Implementation Plan

## Phase 0: Design & Test Scaffolding

- Add golden tests for:
  - immediate sizing correctness in `@a8/@a16` and `@i8/@i16`
  - lexical scoping restores (including nested blocks)
  - early exit restores for `break`, `return`, `goto` out of frame
  - label mode consistency errors
  - typed var assignment errors and required explicit views
  - folding REP/SEP in adjacency cases
- Create fixture set: minimal sources + expected bytes/listings.

## Phase 1: Parser + AST Updates

- Extend grammar:
  - `@a8/@a16/@i8/@i16` as statements and as optional annotations in `main/func/far func/inline/naked` headers.
  - `:byte/:word` in `var` declarations.
  - `:byte/:word` postfix typed-view operator on lvalues/rvalues.
  - `b><a` token/operator.
- AST nodes:
  - `Stmt::ModeSet { a: Option<AWidth>, i: Option<IWidth> }`
  - `Expr::TypedView { base: Expr, view: DataWidth }`
  - `VarDecl { name, addr?, size?, data_width?, ... }`
  - `Stmt::SwapAB`
  - Function header: `FnModeContract { a_width?, i_width? }`

## Phase 2: Semantic Analysis (Mode + Type Checking)

- Implement forward mode tracking through statement list.
- Introduce `ModeFrame` stack:
  - push on `{`
  - pop on `}`
- At block exit:
  - compute delta vs entry and append `Stmt::ModeRestore(delta)` pseudo-op(s) or equivalent.
- Early exits:
  - Detect exiting N frames; inject restores for each exited frame in reverse order (only if needed).
- Label mode unification:
  - Build CFG edges during analysis, record mode at each label entry, unify or error.
- Type rules:
  - Enforce strict immediate fit for target register width.
  - Enforce assignment to typed vars matches size unless explicit typed view used.
  - Enforce typed view usage requires matching CPU width for target register.

## Phase 3: Lowering to IR (Pseudo-Ops)

- Lower `ModeSet` and `ModeRestore` to pseudo-ops `rep(mask)` / `sep(mask)`.
- Lower `b><a` to `XBA` IR op.
- Preserve mapping/spans for diagnostics.

## Phase 4: AST/IR Folding Pass

- Implement the REP/SEP folding pass on the linear stream before final opcode emission.
- Ensure it respects boundaries:
  - do not reorder across non-mode operations
  - fold only adjacent mode ops
- Update golden tests.

## Phase 5: Opcode Emission

- Immediate encoding:
  - A immediates sized by tracked A width
  - X/Y immediates sized by tracked index width
- Width-dependent opcodes emitted using the same mnemonic but correct operand sizing.
- Errors if mode unknown at a width-dependent emission site.

# Diagnostics

Provide explicit and actionable errors:

- "Immediate 0x1234 does not fit in @a8; use @a16 or split into bytes."
- "Store to var w:word requires @a16 or explicit view (w:byte / (w+1):byte)."
- "Mode mismatch at label .loop: incoming edges have different modes (a8/i8 vs a16/i8)."
- "Mode is unknown after PLP/RTI; insert @a8/@a16 and @i8/@i16 before width-dependent instructions."
- "Illegal jump into a deeper block scope."

# Deliverables Checklist

- [ ] Grammar + parser updated
- [ ] AST nodes added for mode sets, typed views, swap operator, function mode contracts
- [ ] Mode-frame tracking with lexical scoping and early-exit restores
- [ ] CFG label mode unification + diagnostics
- [ ] Strict type/width checking for immediates and typed assignments
- [ ] REP/SEP folding pass
- [ ] Opcode emission updated for width-dependent immediates
- [ ] Golden tests covering all key behaviors
