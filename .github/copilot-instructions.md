# k816 Workspace Instructions

## Code Style

Follow standard Rust conventions and use `rustfmt` for formatting. Reference [src/main.rs](src/main.rs) for basic structure (currently a placeholder). Use `clippy` for linting.

## Architecture

Workspace with multiple crates as outlined in [prompts/init-project.plan.md](prompts/init-project.plan.md):

- `core`: Lexing, parsing, AST, HIR, semantics, lowering, encoding, emit. Depends on `eval` and `assets`.
- `eval`: Compile-time expression evaluator for textual expansion.
- `fmt`: Formatter/pretty printer using `core` types.
- `assets`: Modular data converters for `data {}` blocks.
- `tests/golden`: Golden test harness and fixtures.

Design choices: High-level assembly for WDC 65816 with C-like syntax, compile-time evaluation, structured code blocks, and deterministic output.

## Build and Test

- Build: `cargo build`
- Test: `cargo test`
- Golden tests: Run `cargo test` in `tests/golden` for reproducible output validation against fixtures.

## Project Conventions

- Eval textual insertion: Integrate `eval` for compile-time evaluation and re-parsing.
- Far semantics: Support explicit far functions and 24-bit addressing.
- Data blocks: Parse and lower using `assets` converters.
- No optimizer or linker-based far-calls.

Reference [prompts/init-project.plan.md](prompts/init-project.plan.md) for detailed patterns and constraints.

## Integration Points

None apparent yet.

## Security

None apparent.
