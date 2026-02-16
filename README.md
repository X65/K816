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
  x = #0
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

## Development

Regenerate golden fixtures:

- Via feature-gated test flow: `cargo test -p k816-golden-tests --features golden-bless`
- Via CLI bless tool: `cargo run -p k816-golden-tests --bin bless --`

After regeneration, verify fixtures:

- `cargo test -p k816-golden-tests`
