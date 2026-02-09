---
title: k816 - High-level WDC 65816 assembler
repo_layout: workspace
status: ready
audience: AI coding agent
---

## Goal

Rewrite K65 as **k816**, a high-level assembler targeting **WDC 65816**, using Rust and a small set of workspace crates:

- `core` - lexing, parsing, spans, AST, HIR, diagnostics, semantics, lowering, encoding, emit
- `eval` - compile-time expression evaluator (textual expansion)
- `fmt` - formatter / pretty printer (from `core` AST/IR)
- `assets` - modular data converters used by `data {}` blocks (parsing + placement stays in `core`)
- `tests/golden` - golden test harness + fixtures
- `docs/` - documentation
- `plans/` - agent prompt files

Hard constraints:

- Do not introduce more crates than listed above.
- WDC 65816 encoding must live in `core` (no separate encode crate).

## Non-goals

- No banking/linker-based far-call mechanism.
- No optimizer.
- No attempt to infer operand width by clamping/coercing inside `eval`.

---

## Workspace layout

```

k816/
Cargo.toml                 # workspace
src/main.rs                # optional CLI entry (single binary package)
crates/
core/
src/
  lib.rs
  span.rs              # SourceId, Span, Spanned<T>, line/col mapping
  diag.rs              # ariadne wrappers, Diagnostic types
  lexer.rs             # logos -> Token stream
  parser/              # chumsky grammar -> AST
  ast/                 # AST node types
  hir/                 # HIR node types
  sema/                # symbols, scopes, vars, banks, func metadata
  eval_expand.rs       # integrates eval: textual expansion -> reparse
  data_blocks.rs       # parses/lowers data{} using assets converters
  lower.rs             # AST -> HIR -> encoded ops + fixups
  isa65816/            # opcode tables, addressing modes, encoder
  emit.rs              # per-bank output, fixups, listing
  driver.rs            # compile pipeline over input set
eval/
  src/
  lib.rs               # expression parsing + evaluation -> String
  expr.rs
  functions.rs
fmt/
  src/
  lib.rs               # format_file entrypoint
  doc.rs               # pretty-printer combinators
  print_ast.rs         # AST -> Doc
  print_ir.rs          # optional debug print for HIR
assets/
  src/
  lib.rs               # registry + builtins
  traits.rs
  builtin/
  binary.rs
  charset.rs
  image.rs
tests/
  golden/
    fixtures/
  harness.rs
  golden.rs
docs/
prompts/

````

Dependency rules:

- `core` depends on `eval` and `assets`.
- `fmt` depends on `core` types.
- `eval` and `assets` do not depend on `core`.

---

## Recommended Rust crates (accepted)

Frontend:

- `logos` (lexer)
- `chumsky` (parser)
- `ariadne` (diagnostics)

Support:

- `indexmap` (deterministic iteration)
- `rustc-hash` (fast hashing)
- `thiserror` + `anyhow` (errors)
- `unicode-ident` (optional, identifiers)
- `pretty` (formatter)
- `image` (asset loading)
- `camino` (UTF-8 paths)
- `memmap2` (optional, large binaries)
- `assert_cmd`, `predicates`, `similar-asserts`, `tempfile` (golden tests)
- `clap` (optional root CLI)
- `serde` + `toml` (optional config)

---

## Language semantics to implement (summary)

Use the original K65 docs as source of truth for syntax and constructs:

- Comments: `//` and `/* */`
- `var` declarations (explicit address or implicit auto-increment, arrays)
- `bank <name>` sections
- Labels: global and `.local`
- Compile-time eval blocks: `[ ... ]`
- `data { ... }` blocks with placement constraints and content generators
- Code blocks: `main`, `func`, `naked`, `inline`, plus `far` modifier
- Control constructs (including `else` lowering behavior as in K65)

External reference (source of truth):

- <https://raw.githubusercontent.com/zbyti/k65-mkdocs/refs/heads/master/docs/syntax.md>
- <https://raw.githubusercontent.com/zbyti/k65-mkdocs/refs/heads/master/docs/instructions.md>
- <https://raw.githubusercontent.com/zbyti/k65-mkdocs/refs/heads/master/docs/examples.md>
- <https://github.com/Krzysiek-K/k65/tree/master/src>

Additionally apply the k816 design choices below.

---

## Design choices (locked)

### 1) `eval` is textual insertion (no clamping/coercion)

- `eval` produces a **String**.
- `core` treats it as if the user typed it at that location:
  - re-lex the expansion
  - re-parse in the expected syntactic context
- Any numeric range errors occur during normal parsing/lowering, not in `eval`.

Implication:

- `eval` should not attempt to mask/round to 8/16/24-bit.
- `eval` does not need to know ISA details.

### 2) `far` is a 24-bit addressing operator AND a function ABI modifier

A) **Function declaration modifier**:

- `func name { ... }` auto-appends `RTS` (unless `naked`)
- `far func name { ... }` auto-appends `RTL` (unless `naked`)
- calls to `name`:
  - emit `JSR` for normal funcs
  - emit `JSL` for far funcs
- `naked` disables auto-append (no RTS/RTL)

B) **Operand prefix**:

- `far <addr>` forces a 24-bit addressing mode when applicable:
  - e.g. `LDA far label` selects `absolute long` (or the long form for the mnemonic)
- If no long form exists for that mnemonic/addressing combination, emit a diagnostic error.

No linker-driven bank switching is involved.

---

## `assets` crate contract (modular converters only)

`core` owns:

- parsing `data { ... }`
- placement constraints: `align`, `nocross`, `address`
- emission order and bank integration

`assets` provides only converters. Define a stable trait:

```rust
pub struct ConvertRequest<'a> {
  pub kind: &'a str,          // "binary", "image", "charset", ...
  pub args: &'a [Arg],        // parsed by core (strings, ints, arrays)
  pub span: Span,             // for diagnostics
  pub fs: &'a dyn AssetFS,    // abstracted filesystem access
}

pub trait DataConverter: Send + Sync {
  fn kind(&self) -> &'static str;
  fn convert(&self, req: ConvertRequest) -> Result<Vec<u8>, ConvertError>;
}

pub fn builtin_registry() -> Vec<Box<dyn DataConverter>>;
````

Builtins in `assets`:

- `binary` include
- `charset` converter
- `image` converter (read via `image` crate)

`core::data_blocks` selects converter by `kind`, calls it, then enforces placement and emits bytes.

---

## IR model (core)

### Spans

- `Span { source_id, start, end }`
- `Spanned<T> { node: T, span: Span }`

### AST (high-level)

Must represent:

- file/module items: bank decl, var decl, data block, code blocks
- labels (global + local `.name`)
- statements and expressions
- explicit `far` operand prefix
- eval blocks as AST nodes before expansion

### HIR (normalized)

Lower AST into a reduced set of ops:

- label definitions
- instruction-like ops with a resolved mnemonic + operand shape
- call ops that already chose JSR/JSL based on callee metadata
- branch ops with fixups
- emit-bytes ops for raw/data blocks
- directives for alignment / nocross / absolute placement constraints

---

## Compile pipeline (core)

1. Lexing:

- `logos` -> token stream with spans
- skip comments

1. Parsing:

- `chumsky` -> AST
- keep eval blocks (`[...]`) as nodes with raw text/span

1. Eval expansion (core + eval):

- for each eval node:

  - `eval::expand(text)` -> String
  - re-lex + re-parse into the expected syntactic fragment
  - on errors: report at eval node span with context

1. Semantic analysis:

- symbol table + scopes
- local label scoping (function-level and/or inline-expansion-level)
- var allocator (explicit + implicit, arrays)
- bank tracking and per-bank PC tracking
- function metadata: kind + `return_kind` (RTS/RTL/None) + inline/naked

1. Lowering:

- AST -> HIR
- HIR -> encoded ops + fixups
- apply `far` operand hint to addressing selection

1. Emit + fixups:

- resolve labels and compute addresses
- encode instructions (65816)
- apply relocation/fixups
- output:

  - per-bank `.bin`
  - optional `.lst` listing for golden tests

---

## Formatter (fmt)

- Pretty printer reads `core` AST (and optionally HIR for debug dumps).
- Goals:

  - stable formatting
  - deterministic output
  - no semantic changes
- Provide:

  - `format_file(ast) -> String`

---

## Diagnostics (core)

- Use `ariadne` for all user-facing errors.
- Diagnostics must include:

  - span labels
  - clear message
  - short hint when applicable (e.g. "no long form exists for mnemonic with far operand")

---

## Encoding (core::isa65816)

- Implement opcode + addressing mode model for WDC 65816.
- Encoder API must be pure and deterministic.
- Addressing selection must support:

  - direct page, absolute, long
  - indexed variants
  - immediate widths as required by instruction form
- `far` operand prefix forces a long form when available.

---

## Golden tests (tests/golden)

Structure:

- `tests/golden/fixtures/<name>/input.k816`
- expected outputs:

  - `expected.bin` (or per-bank expected files if multiple banks)
  - `expected.lst` (optional but recommended for readable diffs)

Harness rules:

- compile fixture through `core::driver`
- compare bytes exactly
- compare listing text with readable diff (`similar-asserts`)
- keep outputs deterministic (stable iteration via `indexmap`)

---

## Milestones (implementation order)

M1: Skeleton + spans + diagnostics

- workspace builds
- span mapping and ariadne diagnostics working

M2: Lexer + parser for a minimal subset

- parse labels, a few instructions, var decls, bank decl, code blocks

M3: Eval expansion (textual insertion)

- support `[ ... ]` in expression position
- re-lex/re-parse expansions
- golden tests for expansion

M4: Basic semantics + lowering

- symbol resolution (global + local)
- var allocator (explicit + implicit, arrays)
- function metadata (`func`, `naked`, `inline`, `far func`)

M5: 65816 encoding + emit

- small opcode subset first, then expand
- per-bank output and listing

M6: `data {}` core handling + assets converters

- implement registry and builtins (binary/charset/image)
- placement constraints (align/address/nocross)

M7: `fmt`

- formatting for the constructs implemented so far
- formatter golden tests (text)

---

## Acceptance criteria

- `core` can compile representative K65-like input into exact expected bytes (golden fixtures).
- `eval` expansions behave as if pasted into source:

  - lexical/parse errors reported at the expansion site
  - no evaluator-level clamping/masking
- `far func`:

  - auto-appends `RTL` unless `naked`
  - calls emit `JSL`
- `far <operand>`:

  - forces long addressing when legal
  - errors when illegal
- `assets` contains only converters; `core` owns data block parsing and placement.
- Output is deterministic across runs.

---

## Agent instructions

- Start by reading the original K65 docs in the old repo (syntax + instructions) and implement exactly what is described there, then apply the locked k816 design choices above.
- Do not add new workspace crates.
- Keep every stage testable (unit tests for parser/eval/encoder, golden tests end-to-end).
- Prefer simple, explicit data structures over clever abstractions.
