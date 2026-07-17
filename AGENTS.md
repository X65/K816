# Repository Guidelines

## Project Structure & Module Organization

K816 is a Rust 2024 compiler frontend within the cc65 source tree. The current
executable entry point is `src/main.rs`; package metadata and locked dependency
versions live in `Cargo.toml` and `Cargo.lock`. `doc/design.md` is the authority
for the planned architecture: lexer/parser, semantic analysis, lowering to a
common 65816 operation representation, optimization, and ca65 emission.

Keep this as one Cargo package with a library and executable. Add implementation
modules under `src/` and integration tests under `tests/`; place expected-output
fixtures in `tests/golden/`. Do not commit generated files from `target/`.

## Build, Test, and Development Commands

- `cargo build` builds a debug `k816` executable locally.
- `cargo run -- --version` runs the CLI and verifies version handling.
- `cargo test` runs unit, integration, and documentation tests.
- `cargo fmt --all -- --check` checks standard Rust formatting.
- `cargo clippy --all-targets --all-features -- -D warnings` treats lint findings
  as errors before review.
- `make -C .. k816` exercises cc65 integration, producing `../../bin/k816` with
  the repository build ID.

## Coding Style & Naming Conventions

Use `rustfmt` defaults and four-space indentation. Follow Rust naming conventions:
`snake_case` for functions/modules, `UpperCamelCase` for types and traits, and
`SCREAMING_SNAKE_CASE` for constants. Keep parsing, semantic analysis, lowering,
optimization, diagnostics, and emission as distinct module responsibilities.
Represent diagnostics and source spans as structured data; do not pre-render
messages inside compiler logic.

## Testing Guidelines

Use Rust's built-in test framework. Put focused unit tests beside private logic
and public behavior tests in descriptive files such as `tests/lexer.rs` or
`tests/optimizer.rs`. Golden tests should assert exact diagnostics, emitted ca65
assembly, and source mappings where relevant. Add regression tests for every bug
fix. There is currently no numeric coverage requirement.

## Commit & Pull Request Guidelines

Recent history uses short, imperative subjects, for example `Add initial Rust
project structure for k816`. Keep each commit focused and explain non-obvious
design choices in its body. Pull requests should summarize behavior changes,
link relevant issues, list commands run, and call out changes to generated
assembly, diagnostics, or compatibility fixtures. Update `doc/design.md` when a
change alters an architectural boundary or planned language behavior.
