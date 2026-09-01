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

Want to get a feel for [K65-style code](doc/syntax-reference.md)? This is the vibe:

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

Functions can also declare register contracts that are echoed at bare call
sites, and `inline` functions may take typed immediate aliases:

```k65
func add @a16 @i16 (a, x) -> a, y {
  // body
}

inline scale @a16 (a, #factor:byte) -> a {
  ldx #factor
}

func main {
  add a, x -> a, y
  scale a, #16 -> a
}
```

See [register-width-aware-syntax.md](doc/register-width-aware-syntax.md) for full details.

## Symbolic Subscripts

Declare named-field layouts on a fixed address and access them with dot or
bracket syntax (inspired by Pawn):

```k65
var player[
  .x   :word
  .y   :word
  .hp  :byte
] = $0200

func tick @a16 {
  a = player.x
  player.x = a = a + 1
  lda #player.hp:offsetof   // compile-time field offset
}
```

See [symbolic-subscripts.md](doc/symbolic-subscripts.md).

## Compile-time Evaluator

Square brackets evaluate arbitrary expressions at compile time, including
lookup-table generation with `for ... eval`:

```k65
[ TABLE_SIZE = 256 ]

data SineTable {
  align 256
  for x=0..TABLE_SIZE eval [ sin(x / TABLE_SIZE * pi * 2) * 127 + 128 ]
}
```

## Far Calls

24-bit addressing uses `far func` (emits `JSL`/`RTL`) and `call far` across
compilation units:

```k65
far func lib_init @a8 @i8 {
  nop
}

func main {
  call far lib_init
}
```

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

- Via env-gated test flow: `K816_BLESS=1 cargo test -p k816-golden-tests`
- Via CLI bless tool: `cargo run -p k816-golden-tests --bin bless --`

After regeneration, verify fixtures:

- `cargo test -p k816-golden-tests`

## Editor support

`k816 lsp` runs a language server over stdio. It answers live diagnostics,
hover, go-to-definition, references, document and workspace symbols, semantic
rename, completion, code actions from the compiler's own structured fixes,
document formatting, and semantic tokens.

**It analyzes with the options the build uses**, which is what stops an editor
and a build disagreeing about the same file. State them on the command line —
`k816 lsp --strict-k65 --entry native,a8,i8` — or send them as `strict-k65`
and `entry` in the client's `initializationOptions` or configuration, at the
top level or under a `k816` section. The values are read by the same functions
the flags are, so a spelling the command line refuses is refused in the editor
with the same words.

**Highlighting comes in two formats, and the second is derived from the
first.** Editors that read Tree-sitter queries — Helix, Neovim, Zed, Emacs —
highlight from `tree-sitter-k65/queries/highlights.scm`. Editors that do not
are served `textDocument/semanticTokens`, which the server answers by running
that same query and refining what only the symbol table knows. No TextMate
grammar is shipped (`doc/tree-sitter.md`).

**Not offered**, deliberately: inlay hints, code lens, folding ranges, and
signature help. The [K816 Language
Support](https://marketplace.visualstudio.com/items?itemName=X65-Project.vscode-k816)
extension was written against an older server that had all four, so their
absence is a difference to expect rather than a fault to report.

The program the server analyzes is the set of documents the editor has open.
A name another unit defines resolves only while that unit is open; naming the
units in a project file is future work.

## License

K816 is licensed under the 0BSD License. See [LICENSE](LICENSE) for details.
