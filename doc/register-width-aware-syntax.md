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
declaration is a compile-time error — with one exception, the entry point,
whose widths come from the environment rather than from an annotation (see
"The entry point" below):

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

### The entry point

Two kinds of section have a caller that is not a call site: `main`, entered by
the startup code, and an **interrupt handler** — any section that returns
through `RTI`, entered by the hardware. Neither can be bridged to, so neither
takes its widths from an annotation the way every other function does. Both
take them from the **entry ABI**, and an unannotated one is not an error.

`RTI` is what marks a handler, not `naked` and not its name: the instruction
exists to pop the status byte an interrupt pushed, so a body containing one is
a body an interrupt vector reaches. A `naked` section without one is an
ordinary function that happens to have no automatic `RTS`, and declares
whatever widths it likes.

The ABI K816 currently targets is the one `examples/fullstack/startup.asm`
establishes: **native mode, 8-bit accumulator, 8-bit index registers**. It is
a contract between the startup code and the compiler, so the value is what the
startup code does rather than what the CPU does at reset — the stub runs
`sei; clc; xce; jsr main`, which leaves emulation mode and widens neither
axis.

An annotation on `main` is therefore an **assertion**, not a request. It may
restate what the environment provides — `func main @a8 @i8` is a true claim,
and repeating a true claim costs nothing — but one the environment does not
satisfy is `K816-E0143`:

```k65
func main @a16 {        // K816-E0143: the environment provides @a8
    lda #$1234
}

naked dli @a16 {        // K816-E0143: the same, for the same reason
    lda #$1234
    return_i
}
```

Every other function's contract is a requirement on its *callers*, which K816
satisfies by bridging at each call site. their callers are not call sites,
so the requirement stays and the bridge goes. No width setup is generated for
either kind: emitting a `REP` to make the annotation true would be K816
choosing an ABI that belongs to the startup code, and to whatever the
interrupt interrupted.

An interrupt is taken at whatever widths the interrupted code was running at,
which is why this is a *stated contract* rather than something K816 works out.
A program whose widths are the environment's — one that widens only inside
scoped blocks — satisfies it; one that leaves the accumulator 16-bit across a
region where interrupts can fire does not, and nothing in the source says so.

Code that needs wider registers in either widens them in the *body*, where the
widening is the body's own and not a claim about the environment:

```k65
func main @a8 {
    @a16 {
        lda #$1234      // 16-bit here, and the block restores @a8 after it
    }
}
```

Selecting a different environment — a target name, a configuration file — is
not implemented; there is exactly one, and it is stated in one place
(`src/abi.rs`) rather than assumed separately by each pass that needs it.

### Call-site bridging

When calling a function with a mode contract, the compiler automatically emits
`REP`/`SEP` instructions before the `JSR`/`JSL` to satisfy the callee's
contract:

```k65
func caller {
    call wide_a         // emits REP #$20 before JSR
    call narrow         // emits SEP #$30 before JSR
    call wide_all       // emits REP #$30 before JSR
    call uncolored      // no REP/SEP needed
}
```

The caller here is an ordinary function, which starts at unknown widths, so
each bridge establishes every axis its callee names. Written as `main` the
listing would be shorter: the entry point starts at the widths the environment
provides, so a bridge only has to change the axes that differ from them.

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

### Unknown exit contracts preserve caller mode

Every function has an outbound width contract — declared (`-> @a16` etc.) or
inferred from the body's reachable returns. The inferred contract has three
shapes:

| Inferred form | Meaning                                              |
|---------------|------------------------------------------------------|
| `Fixed(@aN)`  | All reachable returns exit at the same fixed width.  |
| `Preserve`    | Every return exits at the same width it entered.     |
| `Unknown`     | Cannot be classified as `Fixed` or `Preserve`.       |

**An `Unknown` outbound contract is treated as `Preserve` at the call site.**
That is, the language guarantees that a function whose outbound width cannot
be statically determined is required to leave the caller's tracked widths
unchanged. The caller's tracker therefore stays anchored at its pre-call
state across such a call, and width-dependent immediates that follow remain
sized against the caller's signature.

This is what makes the function signature the single source of truth for
width tracking: a caller with `@a16 @i16` declared can reason about every
`lda #imm` in its body purely from its own contract plus the contracts (or
`Preserve` defaults) of the functions it calls. Cross-unit and
forward-reference callees, whose body has not been inferred yet, fall under
the same rule: `Unknown` ⇒ `Preserve`.

Genuinely *divergent* exit modes — different `Fixed` widths on different
reachable returns — are still rejected as a hard error at the callee site.
The `Unknown ⇒ Preserve` rule never silently masks that.

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

There is none. K816 generates no `REP`/`SEP` for an entry point, whatever it
declared: the environment hands `main` its widths, and an annotation on `main`
is a claim about them rather than a request K816 satisfies. See "The entry
point" above, and widen inside the body if the body needs it.

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

### Leaving a block by a branch

The restore above runs where control *falls out* of the block.  A branch or
jump that leaves the block does not pass it, so the compiler splits that edge
and puts the restore on it: the transfer lands on a generated pad that
restores the block's width and then continues to the original target.

```k65
func f @a8 {
    @a16 {
        bra .out        // leaves the block
    }
.out:
    lda #$12            // 8-bit: the pad restored it before control arrived
}
```

Generated output:

```asm
        rep     #$20            ; enter @a16 block
        bra     :+              ; the branch, redirected to the pad
:                               ; the pad
        sep     #$20            ; restore @a8 on the taken edge
        jmp     @out            ; on to the written target
:
@out:   lda     #$12            ; 8-bit
```

The restore cannot go anywhere else.  Placed *before* the branch it would also
run on the paths that stay in the block; placed *after* it the code is dead.

This applies to a hand-written native branch as well, which is the one place
the compiler does not emit raw native code exactly as written: a block whose
width silently escapes is worse than a branch that grew a landing pad.  A
branch leaving several nested blocks passes one pad per block, innermost
first.

Two consequences follow from the pad being real code.  A `nocross` run
containing such a branch is measured *after* the split, so it can stop fitting
its page and say so; and the extra jump changes branch distances, which is why
the compiler chooses each branch's form after splitting rather than before.

## Module-level default coloring

Mode annotations placed at the top of a source file (before any `main`/`func`
declarations) set **module-level defaults**.  Every function in the file that
does not declare its own width inherits the module default:

```k65
@a16

func work {
    lda #1          // inherits @a16 -- 3-byte immediate
}

func helper {
    lda #2          // inherits @a16 -- 3-byte immediate
}
```

A module default reaches `main` like any other function, and there it is
subject to the entry ABI: a file-wide `@a16` makes `main` claim a width the
environment does not provide, which is `K816-E0143` at `main`'s declaration.
Annotate `main` with what the environment does provide, or widen inside its
body.

### Overriding module defaults

A function-level annotation takes precedence over the module default:

```k65
@a16 @i16

func work {
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
