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
  call draw           // REP #$20 before JSR -- the accumulator half only,
}                     // because nothing in draw reads the index width
```

Functions can also declare register contracts that are echoed at bare call
sites, and `inline` functions may take typed immediate aliases:

```k65
func add @a16 @i16 (a, x) -> a, y {
  // body
}

inline scale @a16 @i8 (a, #factor:byte) -> a {
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
  a++
  player.x = a
  lda #player.hp:offsetof   // 4 -- folded at compile time
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

K816 compiles one source file to ca65 assembly. It does not assemble, link, or
manage projects: `ca65` assembles what it emits and `ld65` links the result,
which is the rest of the cc65 toolchain doing what it already does.

```text
Usage: k816 <command> [options]

Commands:
  compile [--no-opt] [-o OUT.s] FILE   compile K816 source to ca65 assembly
  emit-ir [--after-opt] [--explain] FILE
                                       dump the lowered operation stream, and
                                       with --explain what the optimizer did
  fmt [--check] FILE...                normalize whitespace: indentation, trailing
                                       space, and the final newline. --check writes
                                       nothing and exits non-zero if a file would
                                       change
  lsp [--strict-k65] [--entry AXES]    run the language server over stdio. The
                                       same options a build states, so an editor
                                       and a build agree; a client may also send
                                       them as `strict-k65` and `entry` in its
                                       initializationOptions or configuration

Options:
  --unit PATH                          a sibling unit whose names this one may
                                       use; repeatable. K816 derives the
                                       imports and exports from them
  --message-format FORMAT              diagnostic output of compile and emit-ir:
                                       human, compact, json
  --strict-k65                         report every construct the original K65
                                       language does not have. Extensions are
                                       errors, and a construct that means
                                       something else there is a warning
  --entry AXES                         the state the machine is in when main
                                       gets control: native or emulation, a8 or
                                       a16, i8 or i16, comma-separated in that
                                       order and each optional. Default
                                       native,a8,i8 - what the startup stub in
                                       examples/fullstack establishes
  -V, --version                        print the version number and exit
```

One unit, through the three tools:

```bash
k816 compile main.k816 -o main.s
ca65 main.s -o main.o
ld65 -C fullstack.cfg -o fullstack.bin startup.o main.o
```

A unit that uses another unit's names. K816 has no `import` syntax — it reads
the siblings you name and derives the imports and exports from them, so every
unit of a program is compiled with the same `--unit` set:

```bash
k816 compile main.k816 --unit lib.k816 -o main.s
k816 compile lib.k816  --unit main.k816 -o lib.s
```

Inspecting a compilation rather than building it:

```bash
k816 emit-ir main.k816                       # the lowered operation stream
k816 emit-ir main.k816 --after-opt --explain # ... and what the optimizer did to it
k816 compile main.k816 --no-opt -o main.s    # skip the optimizer entirely
```

`examples/fullstack/` is a worked build — K816 source, a startup stub, a linker
config, and a `Makefile`.

## Building with cl65

You do not have to run the three tools yourself. `cl65`, the cc65 driver,
knows the pipeline: it compiles a `.k816` or `.k65` file with `k816`,
assembles what `k816` emits with `ca65`, and links the objects with `ld65`.
One installation provides all four, and `cl65` finds them beside itself.

```bash
cl65 -t none --no-target-lib -C fullstack.cfg -o fullstack.bin startup.asm main.k816
```

That is the whole of `examples/fullstack/Makefile`, and it is the command the
test suite runs: `tests/driver.rs` requires the image `cl65` produces to be
byte-identical to the one the three tools produce by hand, for every program
in this repository.

**`-t none --no-target-lib -C <config>` is what a K816 build says.** `cl65`
defaults to the C64, so without `-t none` `ld65` looks for `c64.cfg` and links
`c64.lib` into a program with no C runtime; `-C` replaces the target's
configuration but not its library, and there is no `none.lib`. The layout is
your program's, and you state it. There is no cc65 target for a K816 machine
and `k816` never sees `-t`.

**Every K816 file on one command line is a unit of one program.** K65 has no
`import` syntax, so this is where the unit set comes from: `cl65` compiles each
K816 file with the others as `--unit` siblings, so a name one defines resolves
in another.

```bash
cl65 -t none --no-target-lib -C game.cfg -o game.bin startup.asm main.k816 sprites.k816
```

Two programs that share an include are two `cl65` command lines, not one.

**`-Wk` forwards one option to `k816`, exactly as written.** Unlike `-Wa`,
`-Wc` and `-Wl`, it does not split its argument at commas — a comma is data
inside a K816 option value:

```bash
cl65 -t none --no-target-lib -C game.cfg -Wk --entry=native,a16,i16 -o game.bin main.k816
cl65 -S -Wk --strict-k65 main.k65        # compile only; leaves k816's own main.s
```

`cl65` invents no K816 options of its own: there is one vocabulary, spelled the
way `k816` spells it. `cl65 -S` leaves exactly the assembly `k816 compile`
writes, and `cl65 -g` reaches `ca65` so that a hand-written module linked
beside your K816 one is in the debug database too — K816's own module is there
either way, because it asks for debug info itself.

`doc/cl65.sgml` in the cc65 tree is the driver's reference.

## Development

K816 is one Cargo package. Four gates have to pass:

```bash
cargo fmt --all -- --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
make -C .. k816          # cc65 integration; produces ../../bin/k816
```

`cargo test` needs `ca65` and `ld65` on hand for the end-to-end tests: they are
looked up in the cc65 tree's `bin/` (build them with `make -C ../..`) and can be
pointed elsewhere with the `CA65` and `LD65` environment variables.

Two tracked files are generated, and their tests tell you so when they drift:

- `tree-sitter-k65/generated.lock` — the checksum of the committed parser.
  After editing `grammar.js` and re-running `tree-sitter generate`, refresh it
  with `UPDATE_GENERATED_LOCK=1 cargo test --test corpus`.
- `doc/corpus-census.md` — the counts the prose cites instead of quoting.
  Refresh with `UPDATE_CORPUS_CENSUS=1 cargo test --test live_outs`.

## Formatting

`k816 fmt FILE...` normalizes whitespace and nothing else: the indentation of
each line — four spaces per brace-delimited construct — trailing whitespace,
and the final newline. Whitespace *inside* a line is left exactly as written,
so a column of aligned `=` signs or trailing comments survives, and a file's
line endings are not converted.

`k816 fmt --check FILE...` writes nothing and exits non-zero if a file would
change.

A file whose parse has an error is never formatted: formatting a program the
grammar could not read is guessing, and the diagnostics already say why. The
editor's `textDocument/formatting` calls the same function.

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

The program the server analyzes is the set of documents the editor has open,
and that is a property rather than a limitation waiting on a file. A build
takes its unit set from one `cl65` command line, and there is no project file
anywhere for the editor to read instead — so a name another unit defines
resolves while that unit is open, and open the units of a program and the
editor sees the program a build sees.

## License

K816 is licensed under the 0BSD License. See [LICENSE](LICENSE) for details.
