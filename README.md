# K816

K816 is a high-level assembler (HLA) for the WDC 65816 microprocessor.

It is a direct port of **K65**: <https://github.com/Krzysiek-K/k65>

K816 aims to combine a familiar, readable, C-like source format with the control and predictability of hand-written assembly, while providing a small set of high-level conveniences that remove boilerplate and improve code clarity.

## Goals

- Target the WDC 65816 as a first-class CPU, including 24-bit addressing workflows.
- Provide a K65-inspired high-level assembly syntax designed for readability.
- Support compile-time evaluation for generating constants and data programmatically.
- Support structured code blocks (functions, inlining, and explicit far functions).
- Provide deterministic, reproducible output suitable for golden testing.
- Provide a formatter to keep sources consistently styled.
- Keep the toolchain small, fast, and well-suited to modern development workflows.

## Quick Taste

Want to get a feel for [K65-style code](docs/syntax-reference.md)? This is the vibe:

```k65
var SCREEN = $0400

func main {
  x = 0
  {
    a = hello, x
    z-? { SCREEN, x = a x++ }
  } z-?
}

data text_data {
  charset ".ABCDEFGHIJKLMNOPQRSTUVWXYZ..... "
  hello: "HELLO WORLD" $00
}
```

## Register-Width-Aware Syntax

Functions declare their expected register widths with `@a8`/`@a16` and
`@i8`/`@i16`.  The compiler sizes immediate operands correctly, emits
`REP`/`SEP` at call sites automatically, and optimizes away redundant mode
switches:

```k65
func draw @a16 @i16 {
  lda #$1234          // 16-bit immediate -- sized automatically
}

func main {
  call draw           // REP #$30 emitted before JSR
}
```

See [register-width-aware-syntax.md](docs/register-width-aware-syntax.md) for full details.

## CLI Usage

```bash
> k816 help
High-level assembler for the WDC 65816

Usage: k816 [COMMAND] [INPUT]

Commands:
  compile  Compile source file into relocatable o65 object file
  link     Link one or more o65 object files into final binary output
  init     Scaffold a new project directory
  build    Build the current k816 project
  run      Build and run the current k816 project
  clean    Remove project build outputs
  lsp      Run the Language Server Protocol server over stdio
  fmt      Format source files
  help     Print this message or the help of the given subcommand(s)

Arguments:
  [INPUT]  Input source file

Options:
  -T, --config <CONFIG>           Linker config file in RON format
      --output-format <FORMAT>    Output binary format override (`raw` or `xex`) [possible values: raw, xex]
      --listing [<LISTING_FILE>]  Optional listing output path for linked output. When passed without value, writes <output>.lst
  -o, --output <OUTPUT_FILE>      Output file path. In `compile`, this is the object output path. In `link` and shortcut mode, this is the linked artifact path
  -h, --help                      Print help
  -V, --version                   Print version

Examples:
  k816 path/to/input.k65
  k816 -T path/to/link.ld.ron path/to/input.k65
  k816 compile path/to/input.k65
  k816 link path/to/input.o65 -T link.ld.ron
  k816 init hello
  k816 build
  k816 run -- --fast
  k816 clean
  k816 fmt src/*.k65
  k816 fmt --check src/*.k65
  k816 lsp
  k816 --help
```

## Development

Regenerate golden fixtures:

- Via feature-gated test flow: `cargo test -p k816-golden-tests --features golden-bless`
- Via CLI bless tool: `cargo run -p k816-golden-tests --bin bless --`

After regeneration, verify fixtures:

- `cargo test -p k816-golden-tests`

## Visual Studio Code

The [Kk816 Language Support](https://marketplace.visualstudio.com/items?itemName=X65-Project.vscode-k816) extension provides syntax highlighting, formatting, and inline error reporting for K816 source files.

It works by invoking the `k816 lsp` tool, which provides a language server implementation for K816.

## License

K816 is licensed under the 0BSD License. See [LICENSE](LICENSE) for details.
