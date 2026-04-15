# Function Contracts for K816 — Synthesized Plan

## Context

K816 currently supports functions (`func`, `far func`, `naked`, `inline`) with
**entry-only** CPU-mode contracts (`@a8/@a16 @i8/@i16`). Call sites are bare
identifiers (`call foo` / `call far foo`) with no visibility into inputs,
outputs, or clobbered registers. The compiler already emits implicit REP/SEP
bridges at call sites ([lower.rs:1198-1219](crates/core/src/lower.rs#L1198-L1219))
and threads mode through lowering ([lower.rs:22-26](crates/core/src/lower.rs#L22-L26)),
so the foundation for richer contracts exists.

We want to make the register contract **visible at every call site** without
violating K816's "no hidden instruction emission" rule, and make `inline`
functions true compile-time macro-generators with typed immediate parameters.

Two prior drafts exist:

- `function-contracts.plan.md` ("Reserved Names And Bare Calls") — solves parser
  ambiguity cleanly but requires the user to hand-declare and echo `clobbers(...)`
  at every call site, and duplicates exit-width info on call sites.
- `function-contract.plan.md` ("K816 Function Contract") — cleaner design
  philosophy (no duplicated widths, auto-computed clobbers, damage tracking,
  empty-output = no arrow) but does not address parser ambiguity between bare
  calls and instruction mnemonics.

This plan merges the **reserved-name + bare-call syntax** from the first draft
with the **width-deduplication, auto-clobber, damage-tracking, and typed
inline-params** ideas from the second, so we get parser determinism *and* a
minimal, non-redundant surface.

## Guiding Principles (hard constraints)

1. **Zero hidden code.** Contracts check and inform mode bridges; they never
   insert moves, pushes, or glue JSRs.
2. **Widths are declared once.** Only `@aX @iX` on the declaration control
   width — call sites never repeat them.
3. **Clobbers are computed, not declared.** The user never types a `clobbers`
   list; the compiler derives it from written registers minus declared outputs,
   and a damage-tracking pass diagnoses live-register collisions.
4. **Contract visibility is at the call site.** The I/O register names appear
   on every contract-bearing call so the programmer can read the data flow.
5. **Parser determinism via reserved names.** User function/inline names may
   not collide with CPU mnemonics or reserved HLA heads, which makes a leading
   identifier unambiguously a bare call when it resolves to a user function.
6. **`call foo` remains the unchecked escape hatch** for legacy code and for
   deliberately opting out of contract verification.

## Final Syntax

### Declaration

```k65
// --- Backward-compatible forms (no contract check) ---
func        legacy_fn                             { ... }  // today's syntax, unchanged
func        legacy2  @a8  @i8                     { ... }  // entry-width only, unchanged

// --- New contract-bearing forms ---
func        add      @a16 @i16 (a, x) -> @a8 @i16 a, y { ... }  // entry+exit width, outputs
far func    lib      @a8  @i16 (a, x) -> a                { ... }  // outputs only
naked       irq      @a8  @i8  (a)                        { ... }  // no outputs, no arrow
inline      mul      @a16 @i16 (a, x, #factor:byte) -> a, y { ... }
inline      scale    @a16 @i16 (a, table) -> a            { ... }  // `table` is a variable alias
func        widen    @a8  ()  -> @a16                     { ... }  // mode transition only
```

**The entire contract clause is optional.** Omitting `(params)` means "no input
check". Omitting `-> ...` means "no output check and no declared exit-mode
transition". Today's contract-less `func name { ... }` continues to parse and
lower exactly as it does now — no breaking changes for existing sources.

**Writing `()` is the explicit opt-in** to zero-input contract checking; a
contract-less function with no inputs is still written with *no* parenthesised
list, matching current syntax.

Arrow rules when a contract clause is present:

- **No arrow** — no register outputs and no post-call mode change.
- `-> @aX @iX` — mode changes, no register outputs.
- `-> reg[, reg...]` — register outputs, no mode change.
- `-> @aX @iX reg[, reg...]` — both.
- `-> ()` is a **syntax error** (empty output list must be omitted).

### Three kinds of parameter

A parameter in the `(...)` list is exactly one of:

| Kind | Syntax in decl | Meaning at call site | Where body sees it |
| ---- | -------------- | -------------------- | ------------------ |
| **Register** | `a`, `x`, `y` | caller must have the value in that register | direct use of the CPU register |
| **Immediate alias** | `#name:byte` \| `#name:word` | caller passes a `#<const-expr>` literal | compile-time constant value substituted everywhere `#name` appears |
| **Variable alias** | `name` (non-register ident) | caller passes a variable or address identifier | local alias bound to the passed address for the duration of the call |

Rules:

- Immediate-alias params (`#name:type`) are **inline-only**. They cannot appear
  on `func`/`far func`/`naked` because their only lowering is textual
  substitution.
- Variable-alias params (non-`#`, non-register identifiers) are **inline-only**
  for the same reason — there is no non-inline lowering that honours the
  "no hidden code" rule for a passed address.
- The three kinds cannot be intermixed in a confused way: each parameter slot
  has exactly one kind, fixed at declaration time; the call site must supply
  an argument of the matching kind (register → register name, `#name:T` →
  literal preceded by `#`, variable alias → bare identifier of a variable).
- Register params are the only kind valid on non-`inline` functions.

### Call site

```k65
add a, data_ptr -> a, y        // bare call with inputs/outputs
mul a, x, #16 -> a, y          // inline expansion, literal for #factor
irq a                          // no outputs → no arrow
widen                          // zero inputs, zero register outputs, exit mode from declaration
old_func                       // legacy bare identifier, only if a contract-less func

call add                       // explicit unchecked escape hatch
call far lib_init
```

Rules:

- Call sites echo **inputs and outputs only**. No widths, no clobbers.
- When the callee declares outputs, the call site **must** repeat them after `->`.
  Mismatch is a hard error (catches reader/caller drift).
- When the callee declares no outputs, `->` must be absent.
- `call foo` bypasses all contract verification.

## Resolution & Parser Rules

1. **Reserved names.** Reject any `func`/`inline`/`naked`/`far func` whose name
   matches a 65816 mnemonic (source: [OPCODE_TABLE in isa65816/src/lib.rs:137-412](crates/isa65816/src/lib.rs#L137-L412),
   looked up via a new `is_mnemonic(&str)` helper built from that table) or
   an HLA statement keyword (`if`, `while`, `loop`, `goto`, `call`, `return`,
   `break`, `continue`, etc.). Emit a dedicated diagnostic on violation.
2. **Leading-identifier disambiguation.** In a statement position, when a
   leading identifier matches a declared user function/inline symbol, parse it
   as a bare call. Otherwise fall through to the existing instruction /
   HLA-statement parser paths. Reserved-name enforcement guarantees this never
   misclassifies an instruction.
3. **Entry-width check** is done against the declared `@aX @iX`; existing REP/SEP
   bridge logic already at [lower.rs:1261](crates/core/src/lower.rs#L1261) stays.
4. **Exit-width propagation.** After a contract-bearing call, the caller's mode
   state is updated from the callee's declared exit annotation (falling back to
   the entry annotation when no `-> @...` exists). Absent a contract, mode is
   assumed unchanged (today's behaviour).

## Inline parameters (`#imm` and variable aliases)

Both immediate aliases (`#name:type`) and variable aliases (non-`#` idents)
are **inline-only** and lexically scoped to the inline expansion and any
inline expansions nested within it.

### Immediate aliases — `#name:byte` | `#name:word`

- Behave as **lexically scoped compile-time constants** during expression
  resolution inside the inline body.
- May shadow module/global `const`s.
- May **not** shadow vars, functions, or labels.
- At the call site, each `#param` slot must receive a `#`-prefixed literal or
  constant expression that folds to the declared type. Registers and bare
  variable names are **not** accepted here.

### Variable aliases — bare identifier (no `#`, not a register name)

- Bind the passed variable/address identifier to the alias name for the
  duration of the inline expansion.
- Inside the body, the alias refers to the same address as the passed argument
  (same addressing modes, same size, same relocation).
- May shadow module/global `const`s *and* vars (the alias is strictly more
  specific within this expansion).
- May **not** shadow functions or labels.
- At the call site, the slot must be a bare identifier resolving to a
  variable or address. Passing a `#`-literal where a variable alias is
  expected, or vice versa, is a hard error — the three parameter kinds
  cannot be substituted for one another.

## Automatic Clobber Computation & Damage Tracking

No `clobbers(...)` syntax exists anywhere. Instead:

### Prerequisite — per-mnemonic register read/write metadata

Damage tracking and clobber computation both need, for every emitted
instruction, the set of registers it **reads** and the set it **modifies**.
Today's [`OpcodeDescriptor` table at isa65816/src/lib.rs:137-412](crates/isa65816/src/lib.rs#L137-L412)
only carries mnemonic + addressing mode. Before Phase F can ship, each entry
must be extended with:

```rust
pub struct OpcodeDescriptor {
    // ... existing fields ...
    pub reads:     RegSet,   // A, X, Y, P flags consumed
    pub modifies:  RegSet,   // A, X, Y, P flags written
}
```

Populate from the WDC 65816 programmer's reference. `RegSet` is a small
bitset covering `{A, X, Y, P_N, P_V, P_Z, P_C, P_I, P_D, P_M, P_X}` (flags
as individual bits so the analysis can be flag-accurate later, but v1 can
collapse flag bits into a single `P` for simplicity).

This ISA extension is a blocking dependency; land it as its own commit
(`isa65816: add reads/modifies metadata to opcode table`) before Phase F.

### Clobber computation

Performed while lowering each function body:

```text
clobbers(f) = ⋃ modifies(op) for op in body  −  declared_outputs(f)
```

Status register P is always considered clobbered in v1. Cache on
`FunctionMeta.clobbers`.

### Damage tracking at call sites

At every contract-bearing call site, run a live-register check on the
caller's current basic block:

```text
live_after_call = liveness_forward(caller_block, call_index)
for reg in live_after_call ∩ clobbers(callee) − declared_outputs(callee):
    error!("register {reg} is live after call but clobbered by {callee}")
```

`liveness_forward` is a linear walk over the remaining statements in the
caller's block: a register is **live** at the call if some later instruction
reads it before any instruction modifies it. Per-opcode `reads`/`modifies`
sets (above) drive the walk; calls to other contract-bearing functions
contribute `reads = callee.inputs` and `modifies = callee.clobbers ∪
callee.outputs`. No full dataflow framework needed for v1.

### Exceptions

- **Naked functions** skip auto-clobber computation because they may violate
  normal calling assumptions by design; callers of `naked` get a warning
  unless the call is via explicit `call foo`.
- **`call foo` escape hatch** skips both clobber computation (for the caller's
  view of the callee) and damage tracking.
- **Contract-less `func name { ... }`** is treated like `call foo` from the
  caller's perspective — no damage check — preserving today's behaviour.

## Implementation Roadmap

### Phase A — AST & parser extensions

Files:

- [crates/core/src/ast/mod.rs](crates/core/src/ast/mod.rs) — extend
  `CodeBlock` (currently at lines 117-125) with:
  - `inputs: Vec<RegName>`
  - `outputs: Vec<RegName>`
  - `exit_contract: Option<ModeContract>` (separate from existing entry
      `mode_contract`)
  - `immediates: Vec<ImmediateParam { name, ty }>` (inline only)
  Extend `CallStmt` (lines 424-427) with:
  - `args: Vec<CallArg>` where `CallArg = Reg(RegName) | Immediate(Expr)`
  - `outputs: Vec<RegName>`
  - `is_bare: bool` (true for bare call, false for explicit `call` keyword)
- [crates/core/src/parser/items.rs](crates/core/src/parser/items.rs) —
  extend `code_block_parser()` at lines 185-292 to accept `(param-list)`
  after mode annotations and an optional `-> [@widths] [outputs]` clause
  before the block.
- [crates/core/src/parser/stmt.rs](crates/core/src/parser/stmt.rs) —
  add a bare-call parser alongside the existing `call_stmt` at lines 105-113.
  Grammar:

  ```
  bare-call ::= ident (arg-list)? ("->" output-list)?
  arg-list  ::= arg ("," arg)*   | "(" arg ("," arg)* ")"
  arg       ::= reg-name | "#" const-expr
  ```

  Parens around args are optional (asm-feel vs Rust-feel, both accepted).
  Bare-call only matches if ident resolves to a known function symbol —
  requires a second pass or lazy resolution at lower time.

### Phase B — Reserved-name enforcement

- New helper `is_reserved_name(&str) -> bool` that checks against
  `OPCODE_TABLE` mnemonics ([isa65816/src/lib.rs:137-412](crates/isa65816/src/lib.rs#L137-L412))
  and a small static list of HLA keywords.
- Wire into `collect_function()` at
  [crates/core/src/sema/functions.rs:3-33](crates/core/src/sema/functions.rs#L3-L33),
  next to the existing duplicate-symbol check on line 10. Emit
  `DiagnosticKind::ReservedFunctionName`.

### Phase C — Semantic model

- Extend `FunctionMeta` at
  [crates/core/src/sema/model.rs:4-10](crates/core/src/sema/model.rs#L4-L10):
  - `inputs: Vec<RegName>`
  - `outputs: Vec<RegName>`
  - `exit_contract: Option<ModeContract>`
  - `immediates: Vec<ImmediateParam>`
  - `clobbers: OnceCell<RegSet>` — populated lazily when body is lowered.

### Phase D — Lowering

- [crates/core/src/lower.rs:1251-1282](crates/core/src/lower.rs#L1251-L1282)
  (`lower_call_with_contract`) grows to:
    1. Verify the bare-call's declared I/O matches `FunctionMeta`.
    2. Reuse existing REP/SEP bridge emission for entry mode.
    3. For `inline`, expand body in-place (see Phase E).
    4. For non-inline, emit `JSR`/`JSL` exactly as today.
    5. Update caller mode state from `exit_contract` if present.
    6. Run damage-tracking check.
- Clobber computation runs once per function body during its own lowering
  pass; cache on `FunctionMeta.clobbers`.

### Phase E — Inline expansion with #args

- [crates/core/src/lower.rs:1284-1328](crates/core/src/lower.rs#L1284-L1328)
  — extend the existing substitution-free inline path.
- Push a new lexical scope with `#name → literal` bindings before expanding
  the body; pop after. Expression resolver already handles constants — route
  `#name` through the same path.
- Validate literal fits declared `:byte` / `:word` type; emit typed error
  otherwise.

### Phase F — ISA metadata + damage tracking

**F.1 — Extend the opcode table.** Add `reads: RegSet` and `modifies: RegSet`
to `OpcodeDescriptor` at [crates/isa65816/src/lib.rs:137-412](crates/isa65816/src/lib.rs#L137-L412)
and populate all 256 entries. Provide a `RegSet` type (bitset over
`{A, X, Y, P}` for v1) in the same crate. Unit-test with a spot check of
representative opcodes (`LDA`, `STA`, `TAX`, `INX`, `JSR`, `REP`, `SEP`).

**F.2 — Damage tracker.** Drop into the lowering pass; runs per basic block:

```rust
fn check_damage(call: &CallStmt, callee: &FunctionMeta, block: &Block, idx: usize) {
    if call.is_legacy_call || !call.has_contract { return; }
    let live_after = forward_live_scan(block, idx + 1);
    let collision = &live_after & &(callee.clobbers() - &callee.outputs);
    for reg in collision {
        diag!(call.span, "register {reg} is live after call to {} but clobbered by it", callee.name);
    }
}
```

`forward_live_scan` walks the remaining statements in the block using
per-opcode `reads`/`modifies` sets; a register is live until the first
modify. Calls to contract-bearing callees expose
`reads = callee.inputs`, `modifies = callee.clobbers ∪ callee.outputs`.

### Phase G — Tests & docs

- Add golden fixtures under
  [tests/golden/fixtures/](tests/golden/fixtures/) mirroring the naming
  of existing fixtures (`syntax-mode-call-contract`, `syntax-inline-func-naked-main`):
  - `syntax-func-contract-bare-call/`
  - `syntax-func-contract-inline-args/`
  - `syntax-func-contract-reserved-name/` (negative)
  - `syntax-func-contract-output-mismatch/` (negative)
  - `syntax-func-contract-live-clobber/` (negative)
  - `syntax-func-contract-exit-width/`
- Add parser unit tests in
  [crates/core/src/parser/tests/statements.rs](crates/core/src/parser/tests/statements.rs).
- Update `syntax-reference.md` and `README.md` with the final grammar and
  at least one complete worked example (declaration + call + generated listing
  showing **identical** output to hand-written `call foo`).

## Critical Files

| Purpose | Path |
| ------- | ---- |
| AST nodes | [crates/core/src/ast/mod.rs](crates/core/src/ast/mod.rs) |
| Function decl parser | [crates/core/src/parser/items.rs:185-292](crates/core/src/parser/items.rs#L185-L292) |
| Call / statement parser | [crates/core/src/parser/stmt.rs:105-113](crates/core/src/parser/stmt.rs#L105-L113) |
| Mode contract parser | [crates/core/src/parser/items.rs:157-183](crates/core/src/parser/items.rs#L157-L183) |
| Function metadata | [crates/core/src/sema/model.rs:4-10](crates/core/src/sema/model.rs#L4-L10) |
| Function symbol collection | [crates/core/src/sema/functions.rs:3-33](crates/core/src/sema/functions.rs#L3-L33) |
| Call lowering | [crates/core/src/lower.rs:1251-1282](crates/core/src/lower.rs#L1251-L1282) |
| Inline expansion | [crates/core/src/lower.rs:1284-1328](crates/core/src/lower.rs#L1284-L1328) |
| Mode bridge emission | [crates/core/src/lower.rs:1198-1219](crates/core/src/lower.rs#L1198-L1219) |
| Mnemonic / opcode table (gains `reads`/`modifies`) | [crates/isa65816/src/lib.rs:137-412](crates/isa65816/src/lib.rs#L137-L412) |
| Golden test harness | [tests/golden/src/harness.rs](tests/golden/src/harness.rs) |

## How This Improves The Original Plan

| Original (plan 1) | Improvement (from plan 2 + user feedback) |
| ----------------- | ----------------------------------------- |
| User echoes `clobbers(p)` at each call site | Clobbers auto-computed; damage-tracking pass replaces manual echoing |
| Exit widths appear on both decl and (implicitly) call site knowledge | Exit widths declared once on the function; callers never repeat them |
| No rule for empty outputs | Empty outputs → no arrow; `-> ()` is a syntax error |
| Inline `#args` untyped, single kind | Three parameter kinds: register, `#name:type` immediate alias, bare variable alias — each with explicit scoping rules |
| Contract clause implicitly required | Contract clause fully optional; contract-less `func name { ... }` is a 100 % backward-compatible escape route |
| No hard "no hidden code" constraint stated | Elevated to a guiding principle that gates every codegen decision |
| No damage-tracking specification | Live-register pass plus ISA-level per-opcode `reads`/`modifies` metadata as a named prerequisite |

What plan 1 contributed that plan 2 lacked and is preserved here:

- Reserved-name rule eliminates bare-call / mnemonic parser ambiguity.
- `call foo` escape hatch preserved verbatim.
- Lexical scoping rules for inline `#args` (may shadow consts; may not shadow
  vars/functions/labels).
- Explicit exit-width annotation on declaration (`-> @a8 @i16 ...`), which
  plan 2 omitted — useful for `widen`-style mode-transition helpers.

## Verification

- `cargo test -p k816-core` — parser and semantic unit tests pass, including
  the new reserved-name and contract-mismatch diagnostics.
- `cargo test -p k816-golden` (or equivalent workspace target running the
  golden harness) — all new fixtures produce the expected `.bin` and `.lst`
  outputs; critically, generated assembly for contract-bearing calls is
  **byte-identical** to the pre-feature `call foo` output except for declared
  REP/SEP bridges and inline expansions.
- Manual smoke on a representative program with `cargo run -- assemble
  examples/<pick-one>.k65 --list` to compare against golden listing.
- Negative fixtures must fail with the exact diagnostic kinds listed in the
  Phase G table.

## Out of Scope for v1

- `preserves reg` keyword (would require caller-side save/restore, reintroducing
  hidden code).
- Whole-program dataflow; the linear block scan is sufficient.
- LSP hover/completion for contracts.
- Bracket-syntax alias `[a x 16] -> [a y]`.
