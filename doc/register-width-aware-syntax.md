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

### Transfers between the two axes

`TAX`, `TAY`, `TXA` and `TYA` move a value from one width axis to the other,
and each moves the **destination's** width: `TAX` moves as many bytes as the
X flag says, `TXA` as many as the M flag says. So when the two axes are known
to be different widths the transfer either carries a high byte the source does
not hold — under `@a8 @i16` a `tax` moves the accumulator's hidden `B` half
into X — or drops the one it does, and the source shows neither:

```
Error: `tax` moves 2 bytes here, and the accumulator holds 1
   ╭─[ input.k65:3:5 ]
   │
 3 │     tax
   │     ─┬─
   │      ╰─── the index register is 2-byte wide here, the accumulator 1
   │
   │ Help: declare both axes the same width around this transfer (@a8 with
   │       @i8, or @a16 with @i16), or leave one width unstated to mean it
   │
   │ Note: the high byte moved is not one the accumulator holds at this width
───╯
```

Only two *known* widths can disagree. An axis whose width is unknown makes no
claim, so a transfer written where one width is deferred is the author's —
which is also how to write one of these on purpose. The transfers that stay on
one axis (`TXY`, `TYX`) have no two widths to disagree, and the ones whose
other end is the stack pointer or the direct-page register (`TXS`, `TSX`,
`TCD`, `TDC`, `TCS`, `TSC`) answer to one flag, or to none.

### Why the rule stops at immediates

An immediate is the only operand a width makes longer or shorter. A memory
operand's size comes from its addressing mode — `lda $80` is two bytes and
`lda $0400` three, whatever `M` says — so in a function with an unknown
width, no instruction's *encoding* depends on the width it runs at. What
depends on it is how many bytes a store writes, and that is the caller's
business: a function that declares no width runs at its caller's, preserves
it (an unknown exit is `Preserve`), and emits neither a mode operation nor an
assumption directive of its own.

**A value the next narrowing makes unreachable is not reported either**, and
for a different reason: it is a construct. `@a16` then `lda`, `@a8` then
`sta`, `xba`, `sta` is how a program splits a sixteen-bit value into two
bytes — the high half is reached by exchanging it into the low one, not by
being read where it was. A rule that called the wide load dead would refuse
the idiom. The legacy K816 reported "accumulator value is dead after
switching to @a8" and K816 declines the check on purpose (decided by
interview, 2026-09-03).

That is what makes an unannotated function legal, and it has to be: the
original K65 targets the 6502 and annotates nothing, so every function of a
K65 program but the entry point is one. The legacy K816 refused a
width-dependent memory access under an unknown width — `stz addr`, `stx addr`
— and K816 deliberately does not (decided by interview, 2026-09-03). The
claims this rests on are checked rather than asserted:
`only_an_immediate_encodes_a_different_length_at_a_different_width` reads the
ISA table, and `an_unannotated_callee_runs_at_its_callers_width` reads the
linked bytes.

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

The ABI is what the build **states**, with `--entry`, and its default is the
one `examples/fullstack/startup.asm` establishes: **native mode, 8-bit
accumulator, 8-bit index registers**. It is a contract between the startup
code and the compiler, so the value is what the startup code does rather than
what the CPU does at reset — the stub runs `sei; clc; xce; jsr main`, which
leaves emulation mode and widens neither axis.

```text
k816 compile --entry native,a8,i8 rom.k816    # the default, spelled out
k816 compile --entry a16 rom.k816             # one axis stated, the rest default
k816 compile --entry emulation rom.k816       # a ROM whose reset vector is main
```

Three axes — `native` or `emulation`, `a8` or `a16`, `i8` or `i16` — written
in that order, each optional. An axis left out keeps the default's value.

`emulation` is not a fourth width choice but a different machine: emulation
mode forces the M and X flags to one (`doc/w65c816s.md`), so both registers
are eight bits wide and stay that way. `--entry emulation,a16` therefore
names no machine and is refused as `K816-E0162` rather than quietly corrected.
Under it a `rep #$20` in the entry point establishes nothing — it is a no-op
on that machine — so K816 goes on sizing immediates against an 8-bit
accumulator instead of believing the widening:

```k65
func main {             // under --entry emulation
    rep #$20            // a no-op: M is forced to one
    lda #$1234          // K816-E0103: here it must lie in -128 … $FF
}
```

Sixteen bits are not merely unproven there, they are **impossible**, so
anything that would have K816 emit a widening `REP` of its own is refused with
`K816-E0163` — a scoped `@a16 { … }` block, a bare `@a16` annotation, and a
managed `call` to a callee whose entry contract asks for one:

```k65
func helper @a16 { … }

func main {             // under --entry emulation
    @a16 {              // K816-E0163: cannot be established here
        lda #$1234
    }
    call helper         // K816-E0163: this call would bridge to @a16
}
```

The callee is not at fault and is not refused — `helper` may be called from
native code elsewhere. It is the *call site* that cannot establish what it
promises.

The refusal lifts as soon as the flag stops being known. After `clc; xce` the
mode is `unknown`, and there `@a16` is the documented assertion for exactly
the `xce`-then-widen pattern:

```k65
func main {             // under --entry emulation
    clc
    xce                 // the flag is unknown from here
    @a16 {              // allowed: the assertion is the escape hatch
        lda #$1234
    }
}
```

An **ordinary function** under `--entry emulation` starts with emulation mode
*unknown* rather than emulation: nothing carries the flag *into* a call, and a
function reached from somewhere K816 cannot see may be entered on either
machine. `@a8`/`@a16` in the body is the escape hatch, as it is after any
`xce`.

**Across a call the compiler works it out.** A callee's inferred exit contract
describes the emulation flag alongside the two widths, so:

| the callee | what the caller believes afterwards |
| --- | --- |
| never executes an `XCE` | whatever it believed before the call |
| exchanges the flag on every return, to one known state | that state |
| exchanges it on some paths, or to a state K816 cannot name | nothing: the flag is unknown |
| is one K816 did not classify — another unit's, or one in a call cycle | native mode survives; emulation mode does not |

The last row is the only asymmetric one, and it is deliberate. Every program
ever written for this compiler assumes a callee returns onto the machine it
was called from, and dropping that assumption for `native` would make the
`rep` after every call unsizeable. Emulation mode is newer and inherits no
such debt: an unseen callee may `clc; xce`, satisfy its declared width
contract, and return onto a machine that is no longer emulating, and sizing an
immediate against a guarantee nobody made writes a wrong byte.

The rule runs in the other direction too. A callee that executes `sec; xce`
puts the machine *into* emulation mode, where a subsequent `rep` is a hardware
no-op — so a caller that went on believing its own `rep` would assemble a
three-byte immediate the CPU reads as two. That is `K816-E0104` now, where the
caller's widening `rep` cannot be believed:

```k65
func flips @a8 @i8 {
    sec
    xce                 // the machine is emulating when this returns
}

func caller @a8 @i8 {
    call flips
    rep #$20            // a no-op there, and K816 no longer believes it
    a = #$1234          // K816-E0104: accumulator width is unknown
}
```

None of this is declarable. There is no `-> @native`: the flag is inferred
from the body and published with the exit contract, the way the exit *widths*
already are.

Target *names* are deliberately absent: a name is a mapping onto these axes,
and the configuration it would have to agree with does not exist yet.

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

### A label's entry width

A width statement written directly in front of a label states the label's
**entry width**, the way a header states a function's. The label is then
entered like a function: every arrival has to arrive at that width, and the
compiler makes each one do so *where it leaves from*, the way it bridges a
call to its callee. The label itself runs nothing.

- The path that falls into the label runs the statement where it is written.
- A branch, `goto` or jump already at the entry width carries nothing.
- One at another width, or at a width K816 does not know, is switched at the
  branch.

The common case is a loop, whose back edge is already at the loop's width:

```k65
var RDY = $FFE0

func poll @a16 @i16 {
    tax
    @a8
.wait:
    bit RDY
    dex
    bne .wait
    @a16
}
```

Generated output:

```asm
tax
sep #$20            ; the fall-through enters .wait at @a8
@wait:
bit a:RDY
dex
bne @wait           ; already at @a8: nothing to switch
rep #$20
```

A branch that arrives at another width is switched in front of it, as a call is
bridged in front of its `jsr`:

```k65
func pick @a8 @i8 {
    lda #$12
    bra .wide
    @a16
.wide:
    lda #$1234
}
```

Generated output:

```asm
lda #$12
rep #$20            ; bridge for .wide
bra @wide
@wide:
lda #$1234
```

A *conditional* branch is switched in front of it too, so both of its paths
continue at the entry width, just as the code after a bridged call continues at
the width the bridge set:

```k65
func main @a8 @i8 {
    bcs .wide
    @a16
.wide:
    lda #1
}
```

Generated output:

```asm
rep #$20            ; bridge for .wide
bcs @wide
@wide:              ; the fall-through is already at @a16
lda #$0001
```

The switch is the caller's because the label cannot know its callers, and the
cost is the caller's too. A caller that does not want to pay for the switch sets
the width itself before it branches, and then nothing is emitted. The one
exception is a branch that also leaves a scoped width block: the block's
restore runs on its taken edge ("Leaving a block by a branch") and would undo a
switch made in front of it, so the switch rides the same landing pad, after the
restore.

The entry width covers the axes the statement names. An axis it leaves unnamed
is the merge of the arrivals, as at any label. An arrival K816 cannot see, such
as an indirect jump through a table, is trusted to arrive at the entry width,
exactly as a function's callers are trusted to meet its header: the label cannot
switch for a caller it does not know. The code after
the label is assembled against that width. A label with no width statement in
front of it has no entry width and is a merge point: arrivals that disagree make
the width unknown there (`K816-E0104` at the first immediate that needs it).

A switch the compiler adds for an entry width is an ordinary mode change to the
optimizer: dead mode elimination removes one that nothing reads, and none is
fixed.

### A transfer K816 cannot name

`jmp (table,x)`, `goto (vec)` and a `call` through a variable land on an
address K816 does not know. Such code arrives from a width nothing established,
so a function holding one treats its labels as reachable from an unknown width
— every global label, and every local label whose address the body took.

A local label whose every use is a branch or a `goto` naming it keeps its
width. An indirect transfer reaches a label only through the label's address,
and a local label is scoped to its function, so if no immediate, data entry
or memory operand in the body names it, no table holds it. A dispatcher, then,
does not make the loops above it unsizeable:

```k65
var table = $0300

func main @a8 @i8 {
.again:
    lda #1              // 8-bit: only `beq .again` arrives here
    beq .again
    jmp (table,x)
}
```

With `ldx #&<.again` in the body, `.again` is reachable from the dispatch, the
accumulator is unknown at the label, and `lda #1` is `K816-E0104`.

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
000013: E2 30       sep #$30        ; restore @a8 @i8 -- both axes, see below
000015: A9 01       lda #$01        ; 8-bit
```

Both axes are restored, including the index one.  Narrowing the index
registers zeroes the high bytes of X and Y, which is a register write and not
only a width change, and every register is live where a function returns -- so
the `SEP`'s X bit is load-bearing even though nothing below it reads the X
flag.  Earlier revisions of this document dropped it.

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

If a function inherits a module-level mode but *nothing in the program* reads
the flag that mode sets, the dead mode elimination pass drops the call-site
bridge that establishes it: the contract it satisfies serves nothing.

```k65
@a16 @i16

func main @a8 @i8 {
    nop             // reads neither M nor X
    call helper
}

func helper {
    nop             // reads neither M nor X
}
```

`main` annotates *both* axes, because the entry point's widths are the
environment's and a module default that contradicts either of them is
`K816-E0143` (see [The entry point](#the-entry-point)).  `helper` still
inherits `@a16 @i16`, so the call site would bridge to it.

Output -- the bridge removed, because no instruction anywhere reads M or X:

```asm
000000: EA          nop
000001: 20 05 00    jsr $0005
000004: 60          rts

000005: EA          nop
000006: 60          rts
```

Add one `lda #1` to either body and the bridge comes back: an axis is
"observed nowhere" only when nothing in the whole program depends on it.

## Optimisations

### Dead mode elimination

The first optimisation pass scans each function to determine whether its
`REP`/`SEP` instructions actually affect anything downstream.  If not, the
mode bits are stripped from the mask, and the op is removed when the mask
empties.

**An axis is *observed* by any instruction that reads its flag**, not only by
one carrying a width-dependent immediate.  The M and X flags size every load,
store, ALU operation, compare, push, pull, index step, and width-dependent
transfer: `rep #$20` before `lda addr` / `sta addr` carries no immediate at
all, and deleting it turns a 16-bit load and store into 8-bit ones.  That is
not a lost optimisation, it is a different program.

**Algorithm:**

1. **Compute effective observation** -- for each function, record which axes
   its body reads the flag of, or reaches code the compiler cannot see
   through.  Propagate transitively through the call graph until stable; a
   recursion cycle observes whatever any member observes.

2. **Backward scan** -- walk each function's basic blocks from end to start,
   unioning over successors, tracking whether each axis is observed:
   - an instruction that reads M or X: that axis is observed
   - `JSR`/`JSL` to a known function: take the axis from the target's
     effective observation, and keep the caller's own answer unless the
     callee's exit contract *fixes* the axis
   - a call or transfer the compiler cannot resolve: both axes are observed
   - `Rep(mask)` / `Sep(mask)`: strip the bits of unobserved axes; remove the
     op entirely if the mask becomes zero; the axes it establishes are no
     longer observed above it

   A `REP`/`SEP` mask is **not** a width operation -- it writes an arbitrary
   set of P bits, and only two of the eight are widths.  `rep #$21` clears M
   *and* the carry, so removing it because M is unobserved breaks the `bcc`
   below it.  Bits outside M and X are ordinary flag writes and belong to the
   passes that prove a flag write dead.

3. **The exit contract is an observer.**  A function's published outbound
   contract is what the rest of the program was compiled against, so the last
   operation establishing it on each path to a return is live *whatever* the
   backward scan says.  That holds for `Fixed(@aN)` and for `Preserve` alike:
   a scoped block's restore is what makes `Preserve` true.  The entry point is
   the exception, and only because nothing can call it.

   Likewise, a **narrowing** bit carries a second obligation.  `SEP` on the
   index axis zeroes the high bytes of X and Y, which is a register write and
   subject to ordinary liveness -- `TXS` reads the X register and no flag, and
   sees the difference.  A mode bit is dead only when the axis is unobserved
   *and* the register write the bit performs is dead.

4. **Contracts that serve nothing** -- an axis observed *nowhere* in the whole
   program is one no instruction depends on, so a call bridge that establishes
   it may go: the contract it satisfies serves nothing.  This is expressed as a
   property of the operation stream rather than by rewriting a contract, and it
   applies to the bridge only.  A published *exit* contract is never dropped on
   this ground, because "nowhere" is a claim about the program and what the
   compiler measured is the workspace it was handed.

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

The cancellation row above is written for masks carrying width bits alone.
Composition is per *bit*: `rep #$21` + `sep #$20` folds to `rep #$01` **and**
`sep #$20`, because the composed clear mask still carries the carry.  And a
narrowing the composed result no longer performs is a register write that has
to be dead first: `sep #$10` + `rep #$10` composes to `rep #$10` only where X
and Y are dead, since the original zeroed their high bytes on the way through.

The rule is the composed status transform, never effect-set equality.
`rep #$01` and `sep #$01` touch exactly the same state and leave the carry at
opposite values.

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
