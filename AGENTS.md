# Project Guidelines

## Project Identity

k816 is a fresh Rust implementation of a high-level assembler for the WDC 65816 microprocessor. It is **not** a port, rewrite, or continuation of any previous compiler. There is no legacy code in this Rust codebase and no need for compatibility layers with prior implementations.

## Code Style

Follow standard Rust conventions with `rustfmt` for formatting and `cargo clippy` for style guidance. Apply clippy suggestions to new and modified code. Example: [crates/core/src/lib.rs](crates/core/src/lib.rs) uses clean module declarations and public exports.

## Reuse First

- Default behavior: search the existing codebase for matching patterns, utilities, and functions before introducing new ones.
- Prefer extending or reusing existing implementations when behavior aligns, instead of creating parallel code paths.
- When touching related code, refactor opportunistically to reduce duplication if it can be done safely and clearly.

## Architecture

Multi-crate workspace for separation of concerns: `core` handles lexing/parsing/AST/HIR/semantics/lowering/encoding/emit (depends on `eval` and `assets`); `eval` for compile-time expression evaluation; `fmt` for pretty-printing; `assets` for data converters; `isa65816` for instruction definitions; `o65` for object format; `link` for RON-based linking. Design emphasizes high-level assembly with C-like syntax, compile-time evaluation, structured blocks, deterministic output, explicit far functions, and 24-bit addressing. No optimizer or linker-based far-calls. Reference: [Cargo.toml](Cargo.toml).

Repository Layout:

- `vendor/` contains the source code of a reference K65 compiler written in C. It is kept for reference purposes only and has no bearing on the Rust implementation.
- `crates/` contains the Rust workspace crates listed above.
- `docs/` contains documentation for k816's syntax and linker configuration.
- `examples/` contains example K65 programs.
- `tests/golden/` contains golden test fixtures for regression testing.

## Methodology

The compiler accepts two source styles that can be mixed in a single file:

- the native 65816 mnemonic syntax (`lda`, `inx`, `pha`, ...)
- the high-level K65-style syntax (`a = ...`, `x++`, `a!!`, ...)

Equivalent native/HLA constructs produce equivalent machine output, but they stay distinct in AST:

- native mnemonics are represented as `Stmt::Instruction`
- HLA sugar is represented as `Stmt::Hla(...)` variants (register assignments/transfers, stack shorthand, flow shorthand, one-letter operators, nop shorthand, chains, etc.)

Convergence happens in lowering (AST -> HIR), not in parsing.

Current frontend/backend flow (see `crates/core/src/driver.rs`):

1. Parse with warnings (`parse_with_warnings`) into AST.
2. Expand evaluator-backed expression fragments (`eval_expand::expand_file`).
3. Normalize HLA constructs into canonical HLA AST forms (`normalize_hla::normalize_file`).
4. Run semantic analysis (`sema::analyze`) to build validated symbol/function/const/var metadata.
5. Lower AST + semantic model into operation-level HIR (`lower::lower`), translating HLA nodes and native instructions through the same instruction/mode validation path.
6. Optimize mode ops with `eliminate_dead_mode_ops` and `fold_mode_ops`.
7. Emit either:
   - flat segment bytes + listing (`emit`), or
   - relocatable o65 object + relocation metadata (`emit_object`).

- AST is syntax-oriented: spans, comments, numeric literal formats, and source-level constructs for diagnostics/formatting are preserved.
- Semantic analysis resolves symbols, const values, variable layouts, and function mode contracts before lowering.
- HIR (`hir::Program`) is operation-oriented: segments, labels, instructions, emitted bytes, mode ops, and relocatable byte fixups.
- Width/mode-sensitive diagnostics (typed loads/stores, `:far` direct access rejection, etc.) are enforced during lowering for both native and HLA forms.
- Emission handles instruction selection/encoding and fixup materialization; linker resolution is handled in the `link` crate from RON config.
- Structured top-level blocks (`func`, `data`, named data blocks) remain the units used by lowering and downstream placement/link flows.

## Build and Test

- Build: `cargo build`
- Test: `cargo test`
- Golden tests: `cargo test -p k816-golden-tests` for reproducible output validation against fixtures. Uses Rust edition 2024 with dependencies like `chumsky` for parsing, `clap` for CLI. Reference: [Cargo.toml](Cargo.toml).
- Golden fixture binaries: Use [tests/golden/link.stub.ld.ron](tests/golden/link.stub.ld.ron) as the linker config (`-T` flag) to generate `expected.bin` files in raw binary format for golden test fixtures.
- Golden bless through `cargo test` (feature-gated):
  - Regenerate all golden outputs: `cargo test -p k816-golden-tests --features golden-bless`
  - Regenerate selected cases via test filter: `cargo test -p k816-golden-tests --features golden-bless syntax_doc_example_dli`
  - Validate after blessing: `cargo test -p k816-golden-tests`
- Golden bless CLI (secondary, script-friendly):
  - Regenerate all golden outputs: `cargo run -p k816-golden-tests --bin bless --`
  - Regenerate only error fixtures (`expected.err`): `cargo run -p k816-golden-tests --bin bless -- --err-only`
  - Regenerate a single fixture: `cargo run -p k816-golden-tests --bin bless -- --case fixture:<name>`
  - List discoverable cases: `cargo run -p k816-golden-tests --bin bless -- --list`

## Testing Approach

- Follow Test-Driven Development (TDD): write or update tests first, then implement behavior to satisfy them.
- Ensure implementations are thoroughly testable through `tests/golden/` fixtures.
- Cover positive desired paths, expected and handled errors, and edge cases in golden fixtures.

## Project Conventions

- No "compat", "legacy", or "deprecated" shims; features are first-class or removed.
- `segment` directive only for output segments; no `bank` alias.
- Error on unknown directives; no silent ignores.
- Syntax: C-style comments, variables with `var`, constants in `[...]`, labels (global `label:`, local `.label:`), numeric literals (decimal, hex `$` or `0x`, binary `%` or `0b`). Example: [examples/hello_uart.k65](examples/hello_uart.k65) and [docs/k65-syntax-reference.md](docs/k65-syntax-reference.md).

## Integration Points

Linker uses RON format for configuration (`.ld.ron` files). `vendor/` contains reference C compiler for syntax validation only; no integration with Rust code. Reference: [docs/linker-ron-format.md](docs/linker-ron-format.md).

## Diagnostics Style

- Keep primary error messages concise and focused on what is wrong.
- Put remediation guidance in Ariadne help text using `Diagnostic::with_help(...)`.
- Avoid embedding "how to fix" details directly in the main error message when help text can carry that context.
