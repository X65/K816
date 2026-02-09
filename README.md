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
- Keep the toolchain small, fast, and well-suited to modern Rust development workflows.

## Quick Taste

Want to get a feel for K65-style code? This is the vibe:

```k65
var SCREEN = $0400

main {
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
