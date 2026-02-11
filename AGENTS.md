# Agent Guidelines for k816

## Project Identity

k816 is a fresh Rust implementation of a high-level assembler for the WDC 65816 microprocessor. It is **not** a port, rewrite, or continuation of any previous compiler. There is no legacy code in this Rust codebase and no need for compatibility layers with prior implementations.

## Key Rules

- Do not introduce "compat", "legacy", or "deprecated" shims. If a feature exists, it is a first-class feature. If it does not belong, remove it.
- The `segment` directive is the only way to select output segments. There is no `bank` alias.
- Do not silently ignore unknown directives. Unknown syntax should produce a parse error.

## Architecture

Workspace with multiple crates:

- `crates/core` — Lexing, parsing, AST, HIR, semantics, lowering, encoding, emit. Depends on `eval` and `assets`.
- `crates/eval` — Compile-time expression evaluator for textual expansion.
- `crates/fmt` — Formatter/pretty printer using `core` types.
- `crates/assets` — Modular data converters for `data {}` blocks.
- `crates/isa65816` — WDC 65816 instruction set definitions and encoding.
- `crates/o65` — O65 relocatable object file format.
- `crates/link` — Linker with RON-based configuration.

Design choices: High-level assembly with C-like syntax, compile-time evaluation, structured code blocks, deterministic output, explicit far functions, and 24-bit addressing. No optimizer or linker-based far-calls.

## Repository Layout

- `vendor/` contains the source code of a reference K65 compiler written in C. It is kept for reference purposes only and has no bearing on the Rust implementation.
- `crates/` contains the Rust workspace crates listed above.
- `docs/` contains documentation for k816's syntax and linker configuration.
- `examples/` contains example K65 programs.
- `tests/golden/` contains golden test fixtures for regression testing.

## Code Style

Follow standard Rust conventions. Use `rustfmt` for formatting. Use `cargo clippy` as code style guidance — apply its suggestions to new and modified code.

## Build and Test

- Build: `cargo build`
- Test: `cargo test`
- Golden tests: `cargo test -p k816-golden-tests` for reproducible output validation against fixtures.
