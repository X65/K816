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
  --create-dep FILE                    write a make dependency file for what
                                       compile read: the source, every unit it
                                       imports, and every file a `binary`
                                       includes. --create-full-dep is the same
                                       list, as K816 has no include path
  --dep-target NAME                    the name on the left of the `:`, when the
                                       file the build tracks is not the -o one
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

A unit that uses another unit's names. Each names the units it may see, and
K816 derives the imports and exports from their symbols — so a compilation
takes one file and reads the rest itself:

```bash
k816 compile main.k816 -o main.s   # reads whatever main.k816 imports
k816 compile lib.k816  -o lib.s    # and whatever lib.k816 imports
```

```
# main.k816
import "lib.k816"

func main @a8 @i8 {
    call helper
}
```

A unit sees what it imports **directly** and nothing further: if `main`
imports `lib` and `lib` imports `hw`, then `main` cannot name `hw`'s
symbols without importing it too. That is what makes a unit mean one thing —
`lib.k816` resolves the same names whether it is the file being compiled or a
file `main.k816` reached — and it is why the closure is still a *dependency*
of the build even where it is not visible: `hw`'s body decides contracts
`lib`'s object was compiled against.

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

**Units name each other; `cl65` compiles what it is given.** A K816 file says
which units it may see, so the driver hands the compiler one file and nothing
about the others — the same thing it does with a C or an assembler file.

```bash
cl65 -t none --no-target-lib -C game.cfg -o game.bin startup.asm main.k816 sprites.k816
```

Both K816 files are compiled and both objects are linked, in the order they
appear. A file that is only imported need not be on the line at all, and an
interface unit — nothing but `extern` declarations — usually is not, since it
produces no object.

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

**`--create-dep` works, and it is `k816` that writes it.** The dependencies of
a K816 object are the source, every unit it imports — transitively — and every
file a `binary` includes, and only the compiler knows the last two. That is
what a build generator needs, and with CMake it is the whole integration:

```cmake
set( CMAKE_ASM_SOURCE_FILE_EXTENSIONS s;asm;k65;k816 )
```

A program of several units needs nothing more. The generator compiles one
source per invocation and has nothing to compute, because the source states
which units it belongs with and the dependency file names them.

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

**The program the server analyzes is the open documents plus what they
import**, transitively, read from disk. So a name another unit defines
resolves whether or not that unit has been opened, and the server answers the
question a build answers rather than a subset of it — because it reads the
same `import` lines. A problem in a file nobody opened is reported against
that file.

An open buffer always wins its own text: it may not be saved yet, and it is
what the author is looking at. Opening a file the server had already read
keeps the identity it had, so nothing that pointed into it moves. There is
still no project file, and there is nothing for one to say.

## License

K816 is licensed under the 0BSD License. See [LICENSE](LICENSE) for details.
