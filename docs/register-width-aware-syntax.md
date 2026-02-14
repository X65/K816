# Register-Width-Aware Syntax

The 65816 CPU has two width-control flags in its status register:

- **M flag** (bit 5, mask `0x20`) -- accumulator width (8 or 16 bit)
- **X flag** (bit 4, mask `0x10`) -- index register width (8 or 16 bit)

These flags are changed at runtime by `REP` (clear bits = enable 16-bit) and
`SEP` (set bits = enable 8-bit) instructions.  Several instructions have
operands whose byte length depends on the current flag state.  Getting this
wrong silently shifts every subsequent byte -- one of the most common 65816
bugs.

K816 tracks register width statically through mode annotations and
automatically emits `REP`/`SEP` at the right places, sizes immediate operands
correctly, and optimises away redundant mode switches.

## Mode annotations

Four annotations control width:

| Annotation | Meaning               | CPU effect  |
|------------|-----------------------|-------------|
| `@a8`      | 8-bit accumulator     | `SEP #$20`  |
| `@a16`     | 16-bit accumulator    | `REP #$20`  |
| `@i8`      | 8-bit index registers | `SEP #$10`  |
| `@i16`     | 16-bit index registers| `REP #$10`  |

Annotations can be combined freely: `@a16 @i8`, `@a8 @i16`, etc.

## Width-dependent instructions

The following mnemonics use **ImmediateM** addressing -- their immediate
operand is 1 byte when the accumulator is 8-bit, 2 bytes when 16-bit:

    ora  and  eor  adc  bit  lda  cmp  sbc

The following use **ImmediateX** addressing -- their immediate operand width
follows the index register width:

    ldy  ldx  cpy  cpx

Using any of these with an immediate operand in a function that has no width
declaration is a compile-time error:

```
Error: `lda` uses a width-dependent immediate but accumulator width is unknown
   ╭─[ input.k65:7:3 ]
   │
 7 │   lda #$01
   │   ────┬───
   │       ╰───── primary location
   │
   │ Help: add @a8 or @a16 to the enclosing function
───╯
```

## Function-level mode contracts

A function declares its expected register widths with annotations after the
name.  This is the function's **mode contract**:

```k65
func wide_a @a16 {
    lda #1          // 3-byte immediate (16-bit)
}

func narrow @a8 @i8 {
    lda #1          // 2-byte immediate (8-bit)
    ldx #1          // 2-byte immediate (8-bit)
}

func wide_all @a16 @i16 {
    lda #1          // 3-byte immediate
    ldx #1          // 3-byte immediate
}
```

A contract may be partial -- `@a16` alone leaves the index width unspecified.

### Call-site bridging

When calling a function with a mode contract, the compiler automatically emits
`REP`/`SEP` instructions before the `JSR`/`JSL` to satisfy the callee's
contract:

```k65
func main {
    call wide_a         // emits REP #$20 before JSR
    call narrow         // emits SEP #$30 before JSR
    call wide_all       // emits REP #$30 before JSR
    call uncolored      // no REP/SEP needed
}
```

Generated output:

```asm
rep #$20        ; bridge for wide_a @a16
jsr wide_a
sep #$30        ; bridge for narrow @a8 @i8
jsr narrow
rep #$30        ; bridge for wide_all @a16 @i16
jsr wide_all
jsr uncolored   ; no bridge -- uncolored
```

### Label-site bridging

`goto`/branch transfers to labels use label-anchored bridging: when the target
label has a known mode, the compiler emits minimal `REP`/`SEP` at the label
site itself, immediately after the label definition.

```k65
func main @a8 @i8 {
    c+? goto .loop
    @a16
.loop:
    lda #1
}
```

Generated output:

```asm
bcs .loop
rep #$20
lda #$0001
```

This label-anchored `REP`/`SEP` is treated as fixed and is not removed by dead
mode elimination. Redundant mode switches before that fixed label bridge are
still eliminated.

### Entry-point mode setup

The 65816 starts in 8-bit emulation mode after reset.  Regular functions rely
on call-site bridging, but `main` has no caller.  When `main` has a 16-bit
contract, the compiler emits `REP` at the very beginning of the function body:

```k65
@a16 @i16

func main {
    lda #$1234      // entry emits REP #$30 before this
    ldx #$0001
}
```

Generated output:

```asm
000000: C2 30       rep #$30        ; entry-point setup
000002: A9 34 12    lda #$1234
000005: A2 01 00    ldx #$0001
000008: 60          rts
```

Only `REP` (16-bit) is emitted -- `SEP` (8-bit) at entry would be redundant
since the CPU already defaults to 8-bit.

## Block-level mode scoping

Mode can be changed temporarily with a scoped block.  The compiler emits the
mode switch on entry and automatically restores the previous mode on exit:

```k65
func main @a8 @i8 {
    lda #1              // 8-bit

    @a16 {
        lda #1          // 16-bit inside block
    }
    lda #1              // 8-bit -- restored

    @a16 @i16 {
        lda #1          // 16-bit
        ldx #1          // 16-bit
    }
    lda #1              // 8-bit -- restored
}
```

Generated output:

```asm
000000: A9 01       lda #$01        ; 8-bit (function contract)
000002: C2 20       rep #$20        ; enter @a16 block
000004: A9 01 00    lda #$0001      ; 16-bit
000007: E2 20       sep #$20        ; restore @a8
000009: A9 01       lda #$01        ; 8-bit again
00000B: C2 30       rep #$30        ; enter @a16 @i16 block
00000D: A9 01 00    lda #$0001      ; 16-bit
000010: A2 01 00    ldx #$0001      ; 16-bit
000013: E2 20       sep #$20        ; restore @a8 (i8 needs no SEP -- already restored by block exit)
000015: A9 01       lda #$01        ; 8-bit
```

Blocks can be nested.  Each level saves and restores independently.

## Module-level default coloring

Mode annotations placed at the top of a source file (before any `main`/`func`
declarations) set **module-level defaults**.  Every function in the file that
does not declare its own width inherits the module default:

```k65
@a16

func main {
    lda #1          // inherits @a16 -- 3-byte immediate
}

func helper {
    lda #2          // inherits @a16 -- 3-byte immediate
}
```

### Overriding module defaults

A function-level annotation takes precedence over the module default:

```k65
@a16 @i16

func main {
    lda #1          // 16-bit (inherited)
    ldx #1          // 16-bit (inherited)
}

func narrow @a8 @i8 {
    lda #1          // 8-bit (overridden)
    ldx #1          // 8-bit (overridden)
}
```

Generated output:

```asm
; main -- inherits @a16 @i16
000000: C2 30       rep #$30
000002: A9 01 00    lda #$0001
000005: A2 01 00    ldx #$0001
000008: 60          rts

; narrow @a8 @i8 -- overrides module default
000009: A9 01       lda #$01
00000B: A2 01       ldx #$01
00000D: 60          rts
```

### Interaction with dead mode elimination

If a function inherits a module-level mode but its body contains no
width-dependent instructions, the dead mode elimination pass strips the
inherited contract entirely:

```k65
@a16 @i16

func main {
    nop             // not width-dependent
    call helper
}

func helper {
    nop             // not width-dependent
}
```

Output -- all REP/SEP removed, no call-site bridging:

```asm
000000: EA          nop
000001: 20 05 00    jsr $0005
000004: 60          rts

000005: EA          nop
000006: 60          rts
```

## Optimisations

### Dead mode elimination

The first optimisation pass scans each function to determine whether its
`REP`/`SEP` instructions actually affect any subsequent width-dependent
instruction.  If not, the mode-switch op is removed.

**Algorithm:**

1. **Compute effective needs** -- for each function, check whether its body
   (or bodies of functions it calls) contains any ImmediateM or ImmediateX
   instruction.  Propagate transitively through the call graph until stable.

2. **Backward scan** -- walk each function's ops from end to start, tracking
   `need_m` and `need_x` flags (initially false):
   - ImmediateM instruction seen: set `need_m = true`
   - ImmediateX instruction seen: set `need_x = true`
   - JSR/JSL to known function: set flags from target's effective needs
   - `Rep(mask)` / `Sep(mask)`: strip bits for unneeded flags; remove the op
     entirely if mask becomes zero; reset the corresponding need flag

3. **Clear dead contracts** -- if a function's body turns out not to need M
   width at all, clear `a_width` from its contract.  Same for X width.  This
   prevents call-site bridging for contracts that serve no purpose.

This pass runs **before** folding, so it operates on single-bit `Rep`/`Sep`
ops fresh from lowering.

**Pipeline position:** `lower` -> **`eliminate_dead_mode_ops`** -> `fold_mode_ops` -> `emit`

### REP/SEP folding

The second pass merges adjacent `REP` and `SEP` instructions into the minimum
number of machine instructions:

| Source                | Without folding            | Folded         |
|-----------------------|----------------------------|----------------|
| `@a16` then `@i16`    | `REP #$20` + `REP #$10`    | `REP #$30`     |
| `@a8` then `@i8`      | `SEP #$20` + `SEP #$10`    | `SEP #$30`     |
| `@a16` then `@i8`     | `REP #$20` + `SEP #$10`    | `REP #$20` + `SEP #$10` (no merge -- opposite ops) |
| `@a16` then `@a8`     | `REP #$20` + `SEP #$20`    | `SEP #$20` (REP cancelled) |

The algorithm accumulates pending `rep_bits` and `sep_bits` masks.  A new
`Rep(mask)` adds to `rep_bits` and cancels corresponding `sep_bits`; a new
`Sep(mask)` does the opposite.  On any non-mode op, the pending bits are
flushed as one or two machine instructions.

## Compilation pipeline

The full pipeline for register-width processing:

```
parse       -- tokenise @a8/@a16/@i8/@i16 and parse into ModeContract, ModeScopedBlock
            |
expand      -- propagate mode_default through eval/include expansion
            |
normalise   -- propagate mode_default through HLA normalisation
            |
sema        -- merge module defaults into function contracts; build function metadata
            |
lower       -- emit Rep/Sep ops for call-site bridging, block scoping, entry points
            |
eliminate   -- remove Rep/Sep that have no effect on subsequent instructions
            |
fold        -- merge adjacent Rep/Sep into minimal machine instructions
            |
emit        -- encode Rep/Sep as machine bytes; size immediates by tracked width;
               record initial m_wide/x_wide per function for disassembly
            |
link        -- resolve relocations; disassemble with correct initial mode per function
```

## Listing disassembly

The linker's listing output decodes each function's binary back into assembly.
To correctly size width-dependent immediates, each function carries its initial
`m_wide` and `x_wide` state through the object file.  The disassembler starts
with these initial values and updates them as it encounters `REP`/`SEP`
instructions in the stream:

```
[disasm obj#0 default::main]        ; main @a16 -- starts with m_wide=true
000000: C2 20       rep #$20
000002: A9 01 00    lda #$0001      ; 3-byte immediate (m_wide=true)
000005: 60          rts

[disasm obj#0 default::helper]      ; helper inherits @a16 -- m_wide=true
000006: A9 02 00    lda #$0002      ; 3-byte immediate (m_wide=true)
000009: 60          rts
```

Without the initial mode, the disassembler would incorrectly decode `A9 02 00`
as `lda #$02` (2 bytes) followed by a stray `0x00` byte.
