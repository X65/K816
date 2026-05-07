# K65 Syntax Reference

This reference documents the K65 assembly language syntax. Behavior is cross-checked with the reference compiler grammar in `vendor/src/compiler.inc`.

## Comments

K65 uses C-style comments:

```k65
// this is a line comment

/* this is a
   block comment */
```

## Numeric Literals

- Decimal: `42`
- Hexadecimal: `0x2A` or `$2A`
- Binary: `0b101010` or `%101010`
- Character literals: `'A'`, `'\n'`, `'\0'`

Within evaluator expressions, floating-point literals are also supported (e.g. `[1.5 + .5]`). Values that flow back into K65 integer contexts must resolve exactly to an integer and still pass the target width/range check; they are not rounded or masked automatically.

## Variable Declaration

Variables in K65 are named storage slots. Each declaration belongs to **one of three orthogonal storage classes** that determine where the variable lives and how plain references encode:

| Class              | Syntax                              | Address width | Auto-allocator                              | Initializer required? |
| ------------------ | ----------------------------------- | ------------- | ------------------------------------------- | --------------------- |
| Direct page        | `var dp NAME[:w]`                   | 1 byte        | global 256-byte logical pool, **link-time** | no                    |
| Absolute (default) | `var NAME[:w]` or `var abs NAME[:w]`| 2 bytes       | per-segment cursor                          | no                    |
| Far                | `var far NAME[:w]`                  | 3 bytes       | **none — must have an explicit address**    | yes                   |

Storage class is set by the `dp` / `abs` / `far` prefix before the name. It is **independent** of the data-width suffix (`:byte`, `:word`, `:far`) — `var dp ptr:far` means a 3-byte FAR pointer stored at a 1-byte DP offset. The DP pool reserves three bytes for the variable, while plain references to `ptr` use direct-page address encoding by default. See the "two orthogonal axes" section below.

The final address of a variable is decided at one of two points:

- `var dp foo = $80` / `var foo = $80` / `var far foo = $7E0000` — **fixed at compile time**. The right-hand side must reduce to a constant numeric expression; the assembler bakes the address into every reference. Use this for hardware-mapped I/O (`var VIA = $6000`), pinned DP slots, or any FAR symbol whose bank you control externally.
- `var foo` / `var dp foo` — **assigned by the linker** at link time. ABS-class vars land in the *current segment* (the lexically most recent `segment` directive, defaulting to `default`); DP-class vars share a single 256-byte logical pool that the linker manages **independently of any segment or memory region**.

```k65
var foo = $80           // Fixed ABS: 'foo' lives at $0080
var bar                 // Allocated ABS: linker places it in the default segment
var dp counter          // Allocated DP: linker assigns a slot in the 256-byte DP pool
var dp meter = $42      // Pinned DP: 'meter' lives at DP $42
var far rom_table = $C00000   // Fixed FAR: 24-bit address, no auto-allocation
var arr[10]             // Allocated ABS: 10-byte slot in the current segment
```

The declaration `[N]` suffix reserves a raw storage span: N elements of the declared width, or N bytes when no width is written. It does not create repeat-element metadata and `NAME[i]` on that kind of aggregate remains ordinary byte-offset arithmetic rather than bounds-checked element access. Use the separate `* N` allocation count when you want repeated elements addressable as `NAME[i]`.

A `segment NAME` directive selects which segment subsequent Allocated **ABS** vars land in (DP is segment-orthogonal — `segment` does not affect DP placement):

```k65
segment fixed_lo
var slot_a:word          // Allocated ABS in fixed_lo, offset 0
var slot_b:byte          // Allocated ABS in fixed_lo, offset 2
var dp scratch_a         // Allocated DP, slot 0 in the DP pool — `segment` directive ignored

segment default
var scratch              // Allocated ABS in default
var dp scratch_b         // Allocated DP, next slot in the DP pool
```

### Direct-page storage (`var dp`)

The 65816 direct page is a **runtime-relocatable** 256-byte window addressed with 1-byte offsets relative to the D register (`tcd`/`pld`). It is logically distinct from the program-bank and data-bank address spaces and is not a fixed memory region the assembler can write to. K65 models DP as a **global 256-byte logical pool**, not as bytes inside a segment:

- `var dp NAME` requests a slot of `sizeof(NAME)` bytes from the pool. The linker assigns the final 8-bit DP offset by **first-fit** in declaration order within each compilation unit, and in **link-input order** between units. The assembler does not emit any bytes for DP allocations — runtime is responsible for setting D and providing backing RAM.
- `var dp NAME = $X` pins NAME to a specific DP offset (`$X` must be `≤ $FF`). Pinned slots reserve their byte range so the auto-allocator skips them. **Multiple pinned DP vars at the same offset are allowed by design** — useful for typed views over the same DP slot.
- DP pool overflow (>256 bytes total across all linked units) produces a rich diagnostic at link time pointing at the failing declaration.
- DP storage is independent of `segment` directives and the linker's `memory` / `segments` config. There is no `MemoryKind::DirectPage` and no DP-section concept; the linker only tracks symbol-name → 8-bit DP offset.

### Far storage (`var far`)

`var far` declares a 24-bit-addressable variable. Because the assembler has no notion of which 64K bank should hold an unaddressed FAR symbol, **FAR auto-allocation is rejected** — the user must supply an explicit address:

```k65
var far rom_table = $C00000  // OK: bank $C0, offset $0000
var far missing_addr         // ERROR: FAR variable must have an explicit address
```

### Allocation Count

A `* count` suffix can appear after the variable type/size specification and before the `= address` assignment. It repeats the computed element layout `count` times. For Allocated vars this advances the per-segment cursor by `count × element_size` bytes; for Fixed vars it just sets the reserved slot size at the explicit address.

```k65
var simple :byte * 32           // Allocated: 32 × 1 = 32 bytes in the default segment
var next_var                    // Allocated: next slot, after `simple`

const NUM_SPRITES = 8

var sprites[
    .x    :word
    .y    :word
    .tile :byte
] * NUM_SPRITES                 // Allocated: 8 × (2+2+1) = 40 bytes in the current segment
var after_sprites               // Allocated: next slot, after the 40-byte sprite block
```

`count` can be any constant expression known at compile time, including `const` names and evaluator constants. The allocation count does not create additional symbolic subscript entries; it records a repeat count for bounds-checked element access and scales the total allocation.

### Repeat element accessors

Vars declared with `* count` can be indexed with constant element indices:

```k65
var COMP[
    .one :byte
    .two :word
    .str :byte [5]
] * 10 = $1000

lda &&COMP[1]              // same address as `&&COMP + COMP:sizeof`
lda &&COMP[2]              // same address as `&&COMP + 2*COMP:sizeof`
lda COMP[2].two            // COMP + 2*COMP:sizeof + COMP.two:offsetof
lda COMP[2][.two]          // equivalent field-selection spelling
lda COMP[2].str[3]         // repeated element + field offset + byte element 3
```

Inside a symbolic field list, explicit typed counts use the same order as
top-level typed arrays: `.field :byte [count]` or compact `.field:byte[count]`.
The shorthand `.field[count]` is still valid and uses the var's default field
width, or `:byte` when no default is declared. The old `.field[count]:type`
order is rejected.

`NAME[i]` is compile-time address arithmetic: the index must be a constant numeric expression and must satisfy `0 <= i < repeat_count`. Runtime-variable indices are out of scope for this bracket form; use CPU indexed addressing or explicit pointer arithmetic for those.

`NAME:sizeof` returns the element size, not `element_size * repeat_count`. That keeps the address identity stable:

```k65
&&COMP[i] == &&COMP + i*COMP:sizeof
```

### Abstract symbolic layouts

`abstract var` declares a packed symbolic-subscript layout without declaring storage:

```k65
abstract var DEV_HANDLER[
    .open  :word
    .close :word
    .init  :word
]

lda #DEV_HANDLER:sizeof
lda #DEV_HANDLER.init:offsetof
```

An abstract var emits no bytes, no object symbol, and no linker allocation. It exists only for metadata queries (`:sizeof`, `:offsetof`) and occupies the normal symbol namespace so duplicate names are still rejected. Because it has no address, it cannot be used as an instruction operand or address-of target (`lda DEV_HANDLER`, `lda DEV_HANDLER.open`, and `lda &&DEV_HANDLER` are errors).

The declaration must use a symbolic field list. Declaration-level default field width is allowed (`abstract var POINT:byte[ .x .y ]`), but storage-specific forms are rejected: no `dp`/`abs`/`far` prefix, no initializer, no allocation count, and no array-only form like `abstract var buf[16]`.

### Two orthogonal axes: data width vs. address encoding

K65 separates two unrelated concerns into different syntax slots:

- **Data width / register-width validation** — controlled by suffixes `:byte`, `:word`, `:far`. Belongs to the *value*: how many bytes a variable occupies, what register width is required to load or store it.
- **Address-encoding** — controlled by operand prefixes `dp`, `abs`, `far`. Belongs to the *operand*: which addressing form the assembler emits (DirectPage / Absolute 16-bit / AbsoluteLong 24-bit).

The two axes compose freely: `lda far x:byte` forces AbsoluteLong addressing **and** treats the loaded value as a single byte (valid in `@a8`).

### Typed Width (suffixes)

Variables can have a typed width annotation (`:byte`, `:word`, or `:far`) that controls element size and drives register-width validation:

```k65
var w:word = 0x2000    // 2-byte variable — requires @a16 for load/store
var ptr:far = 0x3000   // 3-byte variable at $3000
                       // direct lda/sta is an error — no register holds 3 bytes
```

The `:far` width marks a 24-bit / 3-byte memory slot. Direct register access of a `:far` variable always errors (no register can hold 3 bytes); use partial views (`ptr:word`, `(ptr+2):byte`) or raw long-indirect mnemonic operands such as `lda [ptr]` instead.

Use-site typed views apply the same widths to one expression:

```k65
lda far x:byte    // AbsoluteLong load + 1-byte data view (valid in @a8)
lda far x:word    // AbsoluteLong load + 2-byte data view (valid in @a16)
lda far x:far     // error: 3-byte data does not fit any register
```

### Address-encoding (prefixes)

The same `dp` / `abs` / `far` keywords appear as **operand prefixes** at use sites and as **declaration prefixes** on `var`. Both forms force the same addressing-mode family:

| Prefix | Forces | Bytes in operand | Notes |
|---|---|---|---|
| `dp` | DirectPage | 1 | Errors if the instruction has no DP form, or if the value does not fit `0x00..=0xFF`. |
| `abs` | Absolute | 2 | Errors if the instruction has no Absolute form. |
| `far` | AbsoluteLong | 3 | Errors if the instruction has no AbsoluteLong form. Also keeps its existing role as the function/data declaration prefix and the data-emission keyword. |

Operand-position prefix:

```k65
var addr = 0x0080

func main {
    lda addr        // auto: page-0 → DirectPage
    lda dp addr     // force DirectPage explicitly
    lda abs addr    // force 16-bit Absolute (emits the high zero byte)
    lda far addr    // force 24-bit AbsoluteLong (emits the bank zero byte)
}
```

Declaration-position prefix on `var` sets the **default** addressing-mode for plain references to that symbol; per-call-site operand prefixes still override it:

```k65
var dp = 0x12              // 1-byte slot at $12, no default
var abs dpa = 0x0012       // dpa: plain refs default to 16-bit Absolute
var far handler = 0x030000 // handler: plain refs default to AbsoluteLong

func main {
    lda dp        // direct page (auto from value range)
    lda dpa       // 16-bit Absolute (var default)
    lda abs dp    // 16-bit Absolute (operand prefix overrides auto)
    lda dp dpa    // direct page (operand prefix overrides var default)
    lda handler   // AbsoluteLong (var default)
}
```

Auto-shrink behavior applies only when the address is known at compile time — numeric literals (`lda $80`) and Fixed vars (`var addr = $0080`). For unprefixed operands in that category, an address that fits in page 0 picks DirectPage encoding if the instruction supports it; otherwise Absolute, with AbsoluteLong reserved for cross-bank values.

Allocated vars (those without an `= addr` initializer) are excluded from auto-shrink: the linker assigns the final address at link time, so the assembler cannot prove DP fit during encoding and defaults to 16-bit Absolute. To put an Allocated var in direct page, opt in with the declaration-position `dp` prefix (`var dp counter:byte`) — the assembler emits a 1-byte DP-width relocation, and the linker rejects it if the resolved address overflows `$00..$FF`. The same applies in reverse: declaration-position `abs` / `far` prefixes bias plain references toward Absolute / AbsoluteLong on Fixed vars.

`dp` and `abs` are recognized **only in the prefix slot** (immediately before an operand expression, or between `var` and the variable name), so they remain available as variable, label, and evaluator-function names (e.g. `var dp = 0` and the `abs(x)` evaluator function still work).

### Metadata Query Suffixes

The `:sizeof` and `:offsetof` expression suffixes return compile-time metadata about variables and symbolic subscript fields. They produce integer constants that can be used in any expression context (immediates, evaluator arithmetic, etc.).

**`:sizeof`** — returns the byte size of a variable element or field:

```k65
var TASKS[
    .sp      :word
    .state   :byte
    .signals :byte
] = $4000

lda #TASKS:sizeof           // 4 (element layout: 2+1+1)
lda #TASKS.sp:sizeof        // 2 (word)
lda #TASKS.state:sizeof     // 1 (byte)
```

Works on plain variables too:

```k65
var counter:word = $80
var buffer[32] = $0100
var tasks[
    .state :byte
] * 8

lda #counter:sizeof         // 2
lda #buffer:sizeof          // 32
lda #tasks:sizeof           // 1, not 8; `* 8` repeats the 1-byte element
```

**`:offsetof`** — returns the byte offset of a field within its parent variable:

```k65
lda #TASKS.sp:offsetof      // 0 (first field)
lda #TASKS.state:offsetof   // 2 (after .sp:word)
lda #TASKS.signals:offsetof // 3
```

Nested fields resolve offsets relative to the top-level variable:

```k65
var REGS[
    .status :byte
    .data[
        .lo :byte
        .hi :byte
    ]
] = $D000

lda #REGS.data.lo:offsetof  // 1 (after .status)
lda #REGS.data.hi:offsetof  // 2
```

Both suffixes bind tightly (at atom level), so they work in arithmetic:

```k65
lda #TASKS:sizeof + 1       // (TASKS:sizeof) + 1
```

K65 also supports symbolic subscript arrays with named `.field` entries inside `var name[...] = <addr>`. This feature is inspired by the Pawn language's named-field array style.

## Constant Declaration

The best way of defining constants is using the evaluator. Evaluator-defined constants are processed in source order and can be mutated within a single evaluator block (`=`, `+=`, `++`, and similar operators). Reassigning the same constant name in a later top-level evaluator block is an error. Evaluator constants can be floating-point values, but K65 instruction operands, variable addresses, data widths, and other integer contexts require an exact integer value that fits the selected encoding width. The compiler does not round evaluator values or mask them to one byte automatically.

```k65
[                       // square brace starts evaluator expression
  MY_CONSTANT = 5,      // define constants
  SOME_NUMBER = 0x13
]                       // end of evaluator expression
```

`const` declarations are also supported and can use enum-like auto-increment in comma-separated groups:

```k65
const A = 0, B, C         // A=0, B=1, C=2
const A, B, C             // A=0, B=1, C=2
const BASE = 10
const X = BASE, Y, Z      // X=10, Y=11, Z=12
const M = 0, N, O = 10, P // P=11
```

The shorthand form only applies to comma-separated groups. A standalone declaration without an initializer is an error:

```k65
const LIMIT               // error
```

## Expression Operators

K65 expressions (used in `const NAME = …`, `var NAME = …`, instruction operands, eval brackets `[…]`, and other compile-time positions) support a C-style operator set. All ops fold at compile time when their operands resolve to integer constants — so flag-style unions, bit-position shifts, and mask inversions all work in `const` declarations and resolve to a single byte/word value at sema time.

### Binary operators

Listed from highest to lowest precedence. Same-row operators are left-associative.

| Precedence | Operators | Description |
|------------|-----------|-------------|
| 1 (highest) | `*` | Multiplication |
| 2 | `+`  `-` | Addition, subtraction |
| 3 | `<<`  `>>` | Left shift, right shift |
| 4 | `&` | Bitwise AND |
| 5 | `^` | Bitwise XOR |
| 6 (lowest) | `\|` | Bitwise OR |

So `A \| B & C` parses as `A \| (B & C)`, `A + B & C` parses as `(A + B) & C`, and `A << 1 \| C` parses as `(A << 1) \| C` — matching standard C precedence.

```k65
const FLAG_NMI = $80
const FLAG_DLI = $40
const FLAG_VBI = $20

const ALL_IRQ      = FLAG_NMI | FLAG_DLI | FLAG_VBI    // $E0
const NMI_NOT_DLI  = FLAG_NMI & ~FLAG_DLI              // $80
const NMI_XOR_DLI  = FLAG_NMI ^ FLAG_DLI               // $C0
const BIT_3        = 1 << 3                            // $08
const HIGH_NIBBLE  = $FF >> 4                          // $0F
```

### Unary operators

| Operator | Description |
|----------|-------------|
| `-` | Numeric negation |
| `~` | Bitwise NOT (one's complement) |

Both repeat in the order written: `-~FOO` is Negate(BitNot(FOO)) and `~-FOO` is BitNot(Negate(FOO)).

### Where compound expressions are accepted

| Position | Compound expressions accepted? |
|----------|-------------------------------|
| `const NAME = expr` | yes |
| `var NAME = expr` | yes |
| Instruction operands (`a = expr`, `lda expr`, ...) | yes |
| Eval brackets `[expr]` | yes (see notes below) |
| Data-block entry value lists (`byte X Y Z`, `word X Y`, `far X`) | **no — atom-only** |

Inside a data-block entry value list, each value must be a single atom: a numeric literal, an identifier (const or label reference), an address-byte operator (`&<`, `&>`, `&&`, `&&&`), or `?`. To use a compound expression in a `byte`/`word`/`far` value list, declare a `const` that pre-folds the expression and reference the const. A standalone `[ … ]` data entry is also available for byte-oriented evaluator output:

```k65
const COMBINED = FLAG_A | FLAG_B    // const-decl path: full expression parser
data SOME {
    byte COMBINED                   // ✓ const reference, single atom
    [FLAG_A | FLAG_B]               // ✓ standalone byte-oriented eval-bracket entry
    byte FLAG_A | FLAG_B            // ✗ atom-list, doesn't accept `|`
}
```

### Evaluator-bracket forms

`[ expr ]` first tries the compile-time evaluator (which handles float math, `sin`/`cos`/`clamp`, evaluator-block-local assignments). If that fails (most commonly because the expression references user-declared k65 consts that the evaluator can't see), the bracketed text is re-parsed as a K65 expression and resolved through the const table. So `[FLAG_A | FLAG_B]` works on user consts because the K65 expression layer handles it after the evaluator declines.

The evaluator side additionally supports `/`, `%`, comparison operators, ternary, and assignment-locals — these are not part of the K65 expression layer and only work on numeric/evaluator-local operands. `[1 / 2]` works; `[CONST_A / CONST_B]` does not (and produces a "could not evaluate eval-bracket expression" diagnostic).

### `&&` gotcha

Because `&&` is the existing 16-bit address-of unary prefix in K65, the expression `FOO && BAR` parses as bitwise-AND of `FOO` with `&&BAR` (the 16-bit address of `BAR`). This is rarely what a user wants if they're thinking of logical AND. K65 does not have a logical-AND operator at this layer; if you really need bitwise-AND with an address-of expression, write it with whitespace and parens to make the intent clear: `FOO & (&&BAR)`.

## Labels

A label can be placed at the beginning of a statement. During assembly, the label is assigned the current value of the active location counter and serves as an instruction operand. There are two types of labels: global and local.

### Global

```k65
var SCREEN=0x400

func main {
  x=0 {
    a=hello,x z-?{ SCREEN,x=a x++ }
  } z-?

  return
}

data text_data {
  charset ".ABCDEFGHIJKLMNOPQRSTUVWXYZ..... "

  hello: "HELLO WORLD" 0
}
```

### Local

Local labels are prefixed with `.` and are scoped to their enclosing function. Two different functions can use the same local label name without collision.

```k65
func one {
  .loop:
  x++
  != goto .loop
}

func two {
  .loop:           // same name, different scope
  y++
  != goto .loop
}
```

Local labels are used heavily for self-modifying code patterns:

```k65
func draw_level {
  .LV_TO_DRAW+1=a=p_current_lv .LV_TO_DRAW+2=a=p_current_lv+1

  .LT+1=.LD+1=a=&<screen_1+224 .RT+1=.RD+1=a=&<screen_1+225
  .LT+2=.RT+2=a=&>screen_1+0x100 .LD+2=.RD+2=a=&>screen_2+0x100

  y=[LV_SIZE] {
    .LV_TO_DRAW: a=levels,y

    x=a .LT: screen_1=x x++ .RT: screen_1+1=x
    a|0x20
    x=a .LD: screen_2=x x++ .RD: screen_2+1=x

    c+ a=.LT+1 a-2 .LT+1=.LD+1=a c-?{ .LT+2-- .RT+2-- .LD+2-- .RD+2-- } x=a x++ .RT+1=.RD+1=x

    y--
  } !=
}
```

## Segment Selection

The 65816 has a linear 16MB address space. The k816 linker splits this memory into versatile, user-defined segments. The active segment can be changed at any point in a file to place functions or data blocks within different segments.

```k65
segment my_segment      // select output segment
```

## Code Sections

Executable code in K65 is specified in sections.

### `func`

User defined function, that can be called from the code. `RTS` is added automatically at the end.

```k65
func inc_x {
  x++        // increments X register and returns
}            // RTS is added automatically
```

Function headers can also carry an optional call contract after the width annotations:

```k65
func add @a16 @i16 (a, x) -> @a8 a, y {
  // body
}
```

- `()` enables checked zero-input calls.
- Register parameters use `a`, `x`, and `y`.
- `inline` functions may additionally declare immediate aliases (`#name`, `#name:byte`, `#name:word`) and bare identifier aliases for variables/addresses.
- `->` can declare output registers, exit-width checks, or both.
- Omitting the entire contract clause keeps the legacy unchecked behavior.

The `@a8`/`@a16` annotations control the accumulator width and `@i8`/`@i16` control index register width; the compiler synthesises [`REP`](#rep-reset-p-bits) or [`SEP`](#sep-set-p-bits) instructions as needed to satisfy these contracts.

The special name `main` designates the program entry point. A `func main` block defaults to 8-bit register widths (matching the 65816's power-on state after XCE) and automatically emits `REP` instructions if 16-bit mode is declared.

```k65
func main {
  a=0        // set accumulator to 0
  {} always  // loop forever
}
```

### `naked`

Just like `func`, but no `RTS` is added automatically at the end.

```k65
naked inc_x_twice {
  x++        // increments X register
  goto inc_x // jump to previously defined inc_x (saves stack)
}            // no RTS here; make sure function never reaches here
```

A `naked` function may carry entry-mode annotations and an input parameter list, but may **not** declare an exit contract — no `-> ...` clause is permitted, because control does not return to the caller implicitly. A bare call to a `naked` function resets the caller's tracked register widths; any code after the call is lowered as if at the start of a new function body and must re-establish whatever mode it needs via `@aX`/`@iX` annotations or explicit bridges.

### `inline`

User defined macro that is inlined in the code when used.

```k65
inline inc_y {
  y++
}
```

Functions and inlines are used simply by specifying their names. A contract-less bare call keeps the legacy behavior:

```k65
func test {
  inc_x      // this will use JSR instruction to call 'inc_x'
  inc_y      // this will inline 'inc_y' - no overhead compared to simple 'y++'
}
```

Contract-bearing functions make the register flow explicit at the call site:

```k65
func add @a16 @i16 (a, x) -> a, y {
  // body
}

inline scale @a16 (a, #factor) -> a {
  lda #factor
}

func test {
  add a, x -> a, y
  scale a, #16 -> a
  call add          // unchecked escape hatch
}
```

Damage tracking for contract-bearing bare calls is **block-local**: after every such call the compiler scans the remaining statements in the same basic block and reports registers that are live past the call but clobbered by the callee. It does not perform inter-procedural or whole-program dataflow — split work into smaller functions if you need the check to see across block boundaries. Use `call foo` to opt out of the check entirely.

Exit-width annotations in a `->` clause are **check-only**. They describe the widths the function body must actually have at every reachable return; they do not force the caller into that mode by declaration alone. If no exit-width annotation is present, a checked bare call adopts the callee's inferred exit mode from its reachable returns. An inferred exit mode of `Unknown` (callee not yet parsed, cross-unit reference, or body variants that are neither uniformly `Fixed` nor uniformly `Preserve`) is treated at the call site as `Preserve` — the caller's tracked widths are left unchanged. See *Unknown exit contracts preserve caller mode* in [register-width-aware-syntax.md](register-width-aware-syntax.md).

### `else`

Creates a jump to a label outside the `else` bracket in branch code.

```k65
inline check_if_key_or_doors {
  a?60 == {gamestate_keys++}
  else {
    a?62 == {
        ptr_doors=a=x
        temp1=a=1
    }
  }
}
```

If-else form:

```k65
=={
  a=1
} else {
  a=2
}
```

### Far Calls

The `far` prefix uses the 65816 CPU's 24-bit addressing. A `far func` generates a `JSL` (Jump to Subroutine Long) call instead of `JSR`, and the function itself ends with `RTL` (Return from subroutine Long) instead of `RTS`. A `far goto label` lowers to `JML` (Jump Long).

```k65
far func long_range_sub {
  x++                   // RTL is added automatically (instead of RTS)
}

func caller {
  far long_range_sub    // generates JSL instead of JSR
}
```

For cross-unit calls (when the callee is defined in a separate compilation unit), use the `call far` syntax:

```k65
// main.k65
func main {
  call far lib_init     // JSL to far function in another unit
}

// lib.k65
far func lib_init @a8 @i8 {
  nop
}
```

The linker validates calling convention consistency across units:

- A near `call` to a `far func` produces a linker error.
- A `call far` to a regular (near) `func` produces a linker error.
- Register width mismatches (caller's A/I width differs from callee's declared `@a8`/`@a16`/`@i8`/`@i16` contract) produce linker errors.

## Data Blocks

Data blocks are defined using the `data` keyword. They support two interchangeable surface forms:

- **Named** — `data NAME { ... }`. The block name is a label at the first emitted byte; the block is addressable as `NAME` (and indexable via `NAME,x`).
- **Anonymous** — `data { ... }`. No leading label is implied; if you want one, write it explicitly inside the block (`NAME: ...`).

The two forms are semantically identical otherwise. Specifically:

```k65
data SOMEBLOCK {
   0 1 2 3 4
   FOO: 5 6 7
}
```

is equivalent to:

```k65
data {
  SOMEBLOCK: 0 1 2 3 4
  FOO: 5 6 7
}
```

Both produce the same bytes, the same labels, and the same HIR. Every entry described in the rest of this section (placement directives, `word`/`far`, strings, `code { }`, `repeat`, `for…eval`, `charset`, address-byte operators, …) is available in both forms — except `code { }`, which is only meaningful at the top level (an anonymous block embedded inside a function body cannot contain `code { }`, since the surrounding scope is already executable).

Anonymous blocks embedded inside a function body emit raw bytes inline:

```k65
var bcol=0xd020

func main {
  data { 0xEA 0xEA 0xEA }
  { bcol++ } always
}

// produces:
//.C:0810  4C 13 08    JMP $0813
//.C:0813  EA          NOP
//.C:0814  EA          NOP
//.C:0815  EA          NOP
//.C:0816  EE 20 D0    INC $D020
//.C:0819  4C 16 08    JMP $0816
```

### `segment` placement rule

A `segment <NAME>` directive inside a data block is scope-local: it switches segments for the block's contents and is restored to the outer segment when the block ends. Because of that, it must appear **at most once** and **as the first entry** of the block — otherwise its placement relative to bytes already emitted before it would be ambiguous, and the compiler reports an error.

```k65
// OK: segment selected before any data is emitted, including the block label.
data VECTORS {
  segment VECTORS
  word reset_handler nmi_handler
}

// Error: `segment` directive must appear as the first entry of a data block.
data BAD {
  1 2 3
  segment OTHER
  4 5 6
}
```

### Placement Directives

```k65
data MyData1 {
  align 16              // align to 16 byte boundary
  1 2 3 4 5 6 7 8
}

data MyData2 {
  align 256             // align to a 256-byte boundary
  1 2 3 4 5 6 7 8
}

data MyData3 {
  nocross               // data will fit completely inside a single page
  1 2 3 4 5 6 7 8
}

data MyData4 {
  address 0x5000        // fixed memory address
  1 2 3 4 5 6 7 8
}
```

### Raw Bytes and Strings

```k65
data bytes {
  0x7F 1 2 3           // raw byte values
  "HELLO" 0            // string data followed by null terminator
}
```

### Labels in Data Blocks

```k65
data text_data {
  hello: "HELLO WORLD" 0
  goodbye: "BYE" 0
}
```

### Address Byte Operators

```k65
var ptr = 0x2345

data bytes {
  0x7F &<ptr &>ptr      // low byte ($45), high byte ($23) of address
  &<ptr+1 &>ptr+1       // with offset arithmetic
  &&ptr                 // full 16-bit address (two bytes, little-endian)
}
```

For emitting full 16-bit addresses in data blocks, consider using the `word` prefix instead (see below).

In code, address byte operators are used to load address parts as immediates:

```k65
a=&<addr               // LDA #<addr (low byte)
a=&>addr               // LDA #>addr (high byte)
```

### `?` (Undefined Byte Placeholder)

A bare `?` inside a data entry reserves a slot whose value is intentionally unspecified — "fill later" / "value doesn't matter". It is accepted in the same value alphabet as numeric literals, both in the implicit-default (bare-numeric) form and inside `byte` / `word` / `far` entries.

```k65
data record {
  byte $7F ? ?           // 1 known byte, 2 placeholder bytes
  word VECTOR ?          // 2 bytes for VECTOR, 2 placeholder bytes
  far ?                  // 3 placeholder bytes (one far slot)
}
```

On the wire, each `?` emits as zero of the surrounding entry's width — `byte ?` is one zero byte, `word ?` is two zero bytes, `far ?` is three (the same widths a literal `0` would produce in the same positions). Pick `?` over an explicit `0` to signal intent: `?` tells a reader "this slot is reserved / will be filled later / the value is irrelevant", a meaning a literal `0` cannot carry. Useful for fields a runtime overwrites in place (cartridge headers, save-state buffers, scratch areas a self-modifying routine populates) and for partially-populated tables where the reader needs to see at a glance which slots are placeholders (`word ? ? ? main_vector ? ? ? ?`).

There is no semantic difference at the binary level — `byte ?` and `byte 0` produce identical bytes. `?` is a source-level affordance only.

### `byte` (8-bit Byte Entries)

The `byte` prefix emits each value as a single 8-bit byte. It is the explicit form of the implicit byte default — `byte 1 2 3` and `1 2 3` produce identical output. Use `byte` when the value list contains an identifier (const or label reference), since bare identifiers are rejected as ambiguous in the implicit-default form (they could be a missing-colon label).

```k65
const FRAME_BYTES = $20

data palette {
  byte FRAME_BYTES         // resolves to $20
  byte FRAME_BYTES 0 1 ?   // mix const, literals, and undef bytes
}
```

Each value on a `byte` line emits 1 byte. Values can be:

- **Numeric literals** (validated to fit in 8 bits): `byte 0 $7F 255`
- **Symbol references** to consts and labels: `byte FRAME_BYTES handler_low`
- **Address byte operators**: `byte &<ptr &>ptr`
- **Undef bytes** (`?`) for placeholder slots: `byte 1 ? 3`

The `byte` keyword is useful both for clarity (signaling intent at the line level) and to lift the bare-identifier restriction. For literal-only byte sequences, the implicit form (no keyword) remains idiomatic.

### `word` (16-bit Word Entries)

The `word` prefix emits each value as a 16-bit little-endian word (2 bytes). This is particularly useful for interrupt vector tables and address lookup tables where every entry is a 16-bit address.

```k65
data VECTORS {
  segment VECTORS
  word 0 0 0 0 0 0 0 0
  word 0 0 0 0 0 0 RESET_HDL 0
}
```

Each value on a `word` line emits 2 bytes in little-endian order. Values can be:

- **Numeric literals** (validated to fit in 16 bits): `word 0 $1234 255`
- **Symbol references** (resolved to full 16-bit addresses): `word main RESET_HDL`
- **Address operators**: `word &&ptr` for a full 16-bit little-endian address
- **Undef words** (`?`) for placeholder slots: `word ?`

This replaces the verbose byte-splitting pattern using `&<` and `&>` in byte entries:

```k65
// Before: manually split addresses into low/high bytes
data VECTORS {
  segment VECTORS
  $00 $00 $00 $00 $00 $00 $00 $00  $00 $00 $00 $00 $00 $00 $00 $00
  $00 $00 $00 $00 $00 $00 $00 $00  $00 $00 $00 $00 &<main &>main $00 $00
}

// After: clean 16-bit word entries
data VECTORS {
  segment VECTORS
  word 0 0 0 0 0 0 0 0
  word 0 0 0 0 0 0 main 0
}
```

Note that since each `word` value is 2 bytes, a line of 8 words produces 16 bytes — the same as 16 individual byte values.

### `far` (24-bit Far Address Entries)

The `far` prefix emits each value as a 24-bit little-endian address (3 bytes). This is used for far (cross-bank) addressing on the 65c816 CPU, where the full 24-bit address space spans 256 banks of 64KB each.

```k65
data FAR_TABLE {
  far handler_a handler_b handler_c
  far $01C000 $02D000
}
```

Each value on a `far` line emits 3 bytes in little-endian order. Values can be:

- **Numeric literals** (validated to fit in 24 bits): `far 0 $01C000 $ABCDEF`
- **Symbol references** (resolved to full 24-bit addresses): `far main RESET_HDL`
- **Address operators**: `far &&&ptr` for a full 24-bit little-endian address
- **Undef far slots** (`?`) for placeholder slots: `far ?`

Note that since each `far` value is 3 bytes, a line of 8 far values produces 24 bytes.

### `code { }` Subblocks

Executable code can be embedded within data blocks:

```k65
data MyData5 {
  0 0 code { a=x }
}
```

The `nocross` modifier can be applied to code subblocks:

```k65
data generated {
  code { a=1 x=2 }
  nocross code { y=3 }
  repeat 3 { 4 }
  ?
}
```

### `repeat`

Repeats data within a data block:

```k65
data repeated {
  repeat 3 { 4 }       // emits: 4 4 4
}
```

### `charset`

Defines character mapping for subsequent string data:

```k65
data text_data {
  charset ".ABCDEFGHIJKLMNOPQRSTUVWXYZ..... "

  hello: "HELLO WORLD" 0
}

data InfoScript {
  charset " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,!'-?:/()acelnosxz&"

  "HELLO" 0xFE
  0xFE 0xF0
}
```

### `for ... eval`

Generates data bytes from evaluator expressions over a range:

```k65
data SineX {
  align 256
  0
  for x=0..213 eval [ (sin(x/212*pi*2)*.499+.499)*130 ]
}

data SineY {
  align 256
  0
  for x=0..255 eval [ (sin(x/256*pi*2)*.499+.499)*180+1 ]
}
```

Reverse ranges are supported:

```k65
[ FACTOR = 3 ]
data table {
  for i=0..4 eval [ i * FACTOR ]    // forward range
  for j=4..0 eval [ j ]             // reverse range
}
```

### `image`

Load pixel data from bitmap images:

```k65
data sprite {
  align 256
  // image <file> <x0> <y0> <byte> <repeat>
  //  <file>    - file name without ".bmp" extension
  //  <x0> <y0> - first pixel to scan
  //  <byte>    - scanning mode for each byte starting with MSB (count+direction)
  //  <repeat>  - scanning mode for consecutive bytes (count+direction)

  image sprites  0 0 8> 16v   // start at (0,0), 8 bits right per byte, 16 rows down
  image sprites 10 0 8> 16v
  image sprites 20 0 8> 16v
}

data fonts {
  address 0x5000
  image "data/font" 0 0  8> 8v tiles 8 0 31
  image "data/font" 0 8  8> 8v tiles 8 0 31
  image "data/font" 0 16 8> 8v tiles 8 0 31
  image "data/font" 0 24 8> 8v tiles 8 0 31
}
```

Additional image modifiers:

```k65
data gfx {
  image "sprites_alt" 8 0 8> 8v inv   // 'inv' inverts pixel values
  tiles 8 0 4                          // tile grid parameters
  colormode 1                          // color mode selector
  imgwave 1 2 "3" 4                    // wave processing
}
```

### `binary`

Embed raw binary files:

```k65
data table {
  address 0x2000
  binary "table.bin"
}
```

### Top-Level `image` and `binary`

Image and binary resources can also be declared at top level (outside data blocks):

```k65
image SpriteSheet = "sprites.bmp"
image "RawSprite" = "raw.bmp"
binary Blob = "payload.bin"
```

## Compile-time Evaluator

The K65 compiler has an embedded evaluator that executes at compile-time. It can perform math operations whose integer results are inlined into K65 integer contexts. The evaluator can also set and use global compiler constants.

In many expression positions where the compiler expects a number, you can invoke the evaluator using square brackets. In data blocks, a standalone eval-bracket entry emits byte-oriented output:

```k65
data NumberFour {
  [2+2]
}
```

The evaluator can also be invoked freely outside any section body to set variables:

```k65
[ FOUR = 4 ]
```

which can be later used as compiler constants:

```k65
data FourFiveSix {
  FOUR             // value of FOUR defined earlier
  [FIVE = FOUR+1]  // defines FIVE and returns its value
  [FIVE+1]         // uses FIVE
}
```

Top-level evaluator blocks execute in source order. A name assigned in one block is available to later code, but assigning that same name again in a different top-level evaluator block is rejected.

### Operators

Available operators and their precedence levels (highest to lowest):

Prec|Assoc|Operators|Description
:---:|:---:|:---|:---
2|L-R|`++` `--` `()` `[]` `.`|suffix increment/decrement, function call, subscript, member access
3|R-L|`++` `--` `+` `-` `!` `~`|prefix increment/decrement, unary plus/minus, logical NOT, bitwise NOT
5|L-R|`*` `/` `%`|multiplication, division, modulo
6|L-R|`+` `-`|addition, subtraction
7|L-R|`<<` `>>`|bitwise left/right shift
8|L-R|`<` `<=` `>` `>=` `?>` `?<`|relational comparisons, select greater/smaller value
9|L-R|`==` `!=`|equality comparisons
10|L-R|`&`|bitwise AND
11|L-R|`^`|bitwise XOR
12|L-R|`\|`|bitwise OR
13|L-R|`&&`|logical AND
14|L-R|`\|\|`|logical OR
15|R-L|`?:`|ternary conditional
16|R-L|`=` `+=` `-=` `*=` `/=` `%=` `<<=` `>>=` `&=` `^=` `\|=`|assignment operators
18|L-R|`,`|expression list (returns value of last)

### Functions

Function|Description
:---|:---
`abs(x)`|Absolute value
`sin(x)`|Sine
`cos(x)`|Cosine
`asin(x)`|Arc sine
`acos(x)`|Arc cosine
`sqrt(x)`|Square root
`pow(x, y)`|Power
`floor(x)`|Round down to nearest integer
`ceil(x)`|Round up to nearest integer
`round(x)`|Round to nearest integer
`frac(x)`|Fractional part (`x - floor(x)`)
`min(a, b)`|Minimum
`max(a, b)`|Maximum
`clamp(x, min, max)`|Clamp x to range
`index(tab, x)`|1-dimensional indexing helper for evaluator array literals
`size(sec)`|Deferred compatibility function; currently rejected in deterministic evaluation
`addbyte(sec, b)`|Deferred compatibility function; currently rejected because section mutation is not supported
`color(r, g, b)`|Deferred compatibility function; currently rejected because palette conversion is not supported
`color(x)`|Deferred compatibility function; currently rejected because palette conversion is not supported
`rnd()`|Deferred compatibility function; currently rejected because randomness is disabled
`print(msg)`|Deferred compatibility function; currently rejected because compile-time side effects are not supported
`error(msg)`|Deferred compatibility function; currently rejected because compile-time side effects are not supported

## Assignment Chaining

Multiple assignments can be chained. The value flows from the rightmost source through each target left-to-right:

```k65
var dst0 = 0x10
var dst1 = 0x11
var src = 0x12

func main {
  dst0=dst1=a=src     // LDA src; STA dst1; STA dst0
  x=a=dst0            // LDA dst0; TAX
  COLPF2=a=VCOUNT     // LDA VCOUNT; STA COLPF2
}
```

Register-to-register chains produce optimal transfer sequences:

```k65
func main {
  y=x=a               // TAX; TXY (transfer A through X to Y)
  d=c=s               // TSC; TCD (transfer S through C to D)
  s=x=y               // TYX; TXS (set stack pointer from Y via X)
  x=a=some_var        // LDA some_var; TAX (load and transfer)
}
```

## Zero-Store Shortcut

`mem = 0` compiles directly to a single `STZ` instruction — no accumulator traffic, no peephole needed:

```k65
var zp_cell   = 0x10
var abs_cell  = 0x0200

func main {
  zp_cell    = 0      // STZ zp
  zp_cell,x  = 0      // STZ zp,X
  abs_cell   = 0      // STZ abs
  abs_cell,x = 0      // STZ abs,X
  zp_cell    = [2-2]  // any expression that folds to zero at compile time
}
```

The LHS must be a variable identifier (possibly with `,x` or `,y` indexing); a bare address literal like `$10 = 0` is not a shortcut for `stz $10`. The RHS must fold to literal zero at parse time — non-zero constants are rejected with *"non-zero constant stores must go through a register: use 'mem = a = value'"*. Addressing modes outside `STZ`'s support (`(mem),y`, `[mem]`, stack-relative, etc.) are rejected at lowering time.

For chains that genuinely do want the accumulator loaded too (e.g. storing the same zero to several cells and leaving `A = 0` for a subsequent operation), keep the traditional `mem1 = mem2 = a = 0` form — the compiler's peephole rewrites each supported `STA` to `STZ` and drops the leading `LDA #0` when `A` is dead afterwards.

## Standard 6502 Instructions

The per-instruction tables in this section document the classical 6502 spellings and the matching K65 HLA shorthand forms. k816 also accepts the 65816 native mnemonic modes listed in [65816 Addressing Modes in K65](#65816-addressing-modes-in-k65), and the exhaustive compiler coverage is kept in `tests/golden/fixtures/syntax-all-256-opcodes/`.

### [Address Modes](https://www.masswerk.at/6502/6502_instruction_set.html)

- `A` accumulator `OPC A` operand is AC (implied single byte instruction)
- `abs` absolute `OPC $LLHH` operand is address `$HHLL` \*
- `abs,X` absolute, X-indexed `OPC $LLHH,X` operand is address; effective address is address incremented by X with carry \*\*
- `abs,Y` absolute, Y-indexed `OPC $LLHH,Y` operand is address; effective address is address incremented by Y with carry \*\*
- `#` immediate `OPC #$BB` operand is byte `BB`
- `impl` implied `OPC` operand implied
- `ind` indirect `OPC ($LLHH)` operand is address; effective address is contents of word at address: `C.w($HHLL)`
- `X,ind` X-indexed, indirect `OPC ($LL,X)` operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: `C.w($00LL + X)`
- `ind,Y` indirect, Y-indexed `OPC ($LL),Y`   operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: `C.w($00LL) + Y`
- `rel` relative `OPC $BB` branch target is PC + signed offset `BB` \*\*\*
- `zpg` zeropage `OPC $LL` operand is zeropage address (hi-byte is zero, address = `$00LL`)
- `zpg,X` zeropage, X-indexed `OPC $LL,X` operand is zeropage address; effective address is address incremented by X without carry \*\*
- `zpg,Y` zeropage, Y-indexed `OPC $LL,Y` operand is zeropage address; effective address is address incremented by Y without carry \*\*

\*   16-bit address words are [little endian](https://en.wikipedia.org/wiki/Endianness), lo(w)-byte first, followed by the hi(gh)-byte. An assembler will use a human readable, big-endian notation as in `$HHLL`.

\*\*  The available 16-bit address space is conceived as consisting of pages of 256 bytes each, with
address hi-bytes represententing the page index. An increment with carry may affect the hi-byte
and may thus result in a crossing of page boundaries, adding an extra cycle to the execution.
Increments without carry do not affect the hi-byte of an address and no page transitions do occur.
Generally, increments of 16-bit addresses include a carry, increments of zeropage addresses don't.
Notably this is not related in any way to the state of the carry bit of the accumulator.

\*\*\* Branch offsets are signed 8-bit values, `-128 ... +127`, negative offsets in two's complement.
Page transitions may occur and add an extra cycle to the exucution.

### 65816 Addressing Modes in K65

In addition to the classical 6502 modes, the K65 operand grammar exposes these 65816 additions:

- `expr,S` — stack-relative (e.g. `lda 4,s`). Written the same for raw mnemonics and HLA forms.
- `(expr,S),Y` — stack-relative indirect, Y-indexed (e.g. `lda (4,s),y`). Usable in both raw-mnemonic and HLA contexts.
- `dp expr` / `abs expr` / `far expr` — operand prefixes that force the address-encoding width to DirectPage / Absolute (16-bit) / AbsoluteLong (24-bit). Composable with the index trailers (`abs expr,x`, `far expr,x`, etc.) and with the parenthesized indirect forms (`jmp (abs expr,x)`, `jmp (abs expr)`, `lda [dp expr]`, `lda [dp expr],y`).
- `[expr]` — indirect-long. The emitted opcode depends on the mnemonic: `lda [zp]` picks `DirectPageIndirectLong`; `jmp [abs]` picks `AbsoluteIndirectLong`. The `[...]` operand form is currently recognised only when it directly follows a raw mnemonic (e.g. `lda [ptr]`, `jmp [vec]`); inside an HLA assignment like `a = [ptr]` the brackets are captured by the compile-time evaluator instead. Use the raw mnemonic form for long-indirect addressing.
- `[expr],Y` — indirect-long, Y-indexed. Raw-mnemonic only for the same reason.
- `&expr` — address-positioned marker for `,X`/`,Y` indexed modes. Lets a `const` (or any const-rooted expression) serve as a small literal byte/word field offset when the struct base is in `X` or `Y`:

  ```k65
  const FIELD_HP = 2
  ldx #&player_struct
  lda &FIELD_HP, x       // lda $02,X — read player_struct + 2
  lda abs &FIELD_HP, x   // forces abs,X encoding (16-bit operand)
  ```

  Without the `&` marker, a bare const in `,X`/`,Y` position is rejected (`const` names a value, not a memory address). The marker is **only** valid in `,X` or `,Y` direct/absolute indexed modes; using it in plain direct/absolute, in stack-relative (`,s`), or inside `#expr` is an error. The marker rhymes with the `&`-led address-of family — `&&label` (16-bit address as immediate), `&&&label` (24-bit), `&<expr` / `&>expr` (low/high byte) — and is the inverse direction: it lets a value play the role of an address-mode operand.

Operand rule for **raw mnemonics**: `#expr` is immediate, while an unprefixed value operand (`lda 0`, `lda addr`, `lda expr`) is an address-mode operand. Bare `const` names and const-rooted expressions are rejected in address position because `const` names values, not memory locations; use `#CONST` for an immediate, declare a `var`/label for memory access, or attach an address-encoding prefix (`lda dp 0`, `lda abs 0`, `lda far 0`) when a numeric literal should force a specific address width. The address-of operators `&&label` and `&&&label` are the intentional exception: they produce immediate relocations even without `#`.

### Registers

- `A` accumulator (8/16 bit)
- `B` high byte of 16bit accumulator (8 bit, can be swapped with A via `b~a`)
- `C` full 16-bit accumulator (16bit, available only to register transfer instructions)
- `X` index register X (8/16 bit)
- `Y` index register Y (8/16 bit)
- `D` direct page register (16 bit)
- `S` stack pointer (16 bit)
- `PC` program counter (16 bit; CPU architectural register, not an HLA assignment target)
- `DBR` data bank register (CPU architectural register, manipulated with native mnemonics such as `phb`/`plb`)
- `PBR` program bank register (CPU architectural register, manipulated through long control flow and `phk`)
- `P` processor status register **NVMXDIZC** (8 bit; stack shorthand target as `p`, `flag`, or an individual flag name)

In K65 HLA syntax, single-letter register names `A`, `B`, `C`, `D`, `S`, `X`, and `Y` are native symbols used by the supported assignment, transfer, and stack shorthand parsers. Not every CPU register is a general-purpose assignment target; unsupported HLA assignments are rejected and must be written with the dedicated native mnemonic. The `b~a` shorthand lowers to the 65816 `XBA` (exchange B and A) instruction.

### SR Flags NVMXDIZC

```
bit 7 to bit 0

N ....  Negative
V ....  Overflow
M ....  Accumulator/Memory width (65816: 0=16-bit, 1=8-bit)
X ....  Index register width (65816: 0=16-bit, 1=8-bit)
D ....  Decimal (use BCD for arithmetics)
I ....  Interrupt (IRQ disable)
Z ....  Zero
C ....  Carry

E ....  Emulation mode (65816, hidden, can be exchanged with Carry flag via XCE instruction)
```

Flag shorthand operations: `c+` (SEC), `c-` (CLC), `d+` (SED), `d-` (CLD), `i+` (SEI), `i-` (CLI), `v-` (CLV).

### Processor Stack

**LIFO**, descending. In emulation mode the stack is page-1 based (`0x0100 - 0x01FF`); in native mode the 65816 uses the 16-bit `S` register.

### Bytes, Words, Addressing

8 bit bytes, 16 bit words in lobyte-hibyte representation (**Little-Endian**).
16 bit address range, operands follow instruction codes.

Signed values are two's complement, sign in bit 7 (most significant bit).

- `%11111111` = `$FF` = `-1`
- `%10000000` = `$80` = `-128`
- `%01111111` = `$7F` = `+127`

---

### `ADC` add with carry

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`a+imm`|`a+mem`|`a+mem,x`|`a+mem,y`|`a+(mem,x)`|`a+(mem),y`

```none
ADC  Add Memory to Accumulator with Carry

     A + M + C -> A, C                N Z C I D V
                                      + + + - - +

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     ADC #oper     69    2     2
     zeropage      ADC oper      65    2     3
     zeropage,X    ADC oper,X    75    2     4
     absolute      ADC oper      6D    3     4
     absolute,X    ADC oper,X    7D    3     4*
     absolute,Y    ADC oper,Y    79    3     4*
     (indirect,X)  ADC (oper,X)  61    2     6
     (indirect),Y  ADC (oper),Y  71    2     5*
```

---

### `AND` and (with accumulator)

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`a&imm`|`a&mem`|`a&mem,x`|`a&mem,y`|`a&(mem,x)`|`a&(mem),y`

```none
AND  AND Memory with Accumulator

     A AND M -> A                     N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     AND #oper     29    2     2
     zeropage      AND oper      25    2     3
     zeropage,X    AND oper,X    35    2     4
     absolute      AND oper      2D    3     4
     absolute,X    AND oper,X    3D    3     4*
     absolute,Y    AND oper,Y    39    3     4*
     (indirect,X)  AND (oper,X)  21    2     6
     (indirect),Y  AND (oper),Y  31    2     5*
```

---

### `ASL` arithmetic shift left

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
`a<<`|||`mem<<`|`mem,x<<`|

```none
ASL  Shift Left One Bit (Memory or Accumulator)

     C <- [76543210] <- 0             N Z C I D V
                                      + + + - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     accumulator   ASL A         0A    1     2
     zeropage      ASL oper      06    2     5
     zeropage,X    ASL oper,X    16    2     6
     absolute      ASL oper      0E    3     6
     absolute,X    ASL oper,X    1E    3     7
```

---

### `BCC` branch on carry clear

- `>={ ... }`
- `< goto label`
- `{ ... } <`
- `c+?{ ... }`
- `c-? goto label`
- `{ ... } c-?`

```none
BCC  Branch on Carry Clear

     branch on C = 0                  N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BCC oper      90    2     2**
```

---

### `BCS` branch on carry set

- `<{ ... }`
- `>= goto label`
- `{ ... } >=`
- `c-?{ ... }`
- `c+? goto label`
- `{ ... } c+?`

```none
BCS  Branch on Carry Set

     branch on C = 1                  N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BCS oper      B0    2     2**
```

---

### `BEQ` branch on equal (zero set)

- `!={ ... }`
- `== goto label`
- `{ ... } ==`
- `z-?{ ... }`
- `z+? goto label`
- `{ ... } z+?`

```none
BEQ  Branch on Result Zero

     branch on Z = 1                  N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BEQ oper      F0    2     2**
```

---

### `BIT` bit test

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`a&?mem`|||||

```none
BIT  Test Bits in Memory with Accumulator

     bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V);
     the zeroflag is set to the result of operand AND accumulator.

     A AND M, M7 -> N, M6 -> V        N Z C I D V
                                     M7 + - - - M6

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      BIT oper      24    2     3
     absolute      BIT oper      2C    3     4
```

---

### `BMI` branch on minus (negative set)

- `>=0{ ... }`
- `<0 goto label`
- `{ ... } <0`
- `n-?{ ... }`
- `n+? goto label`
- `{ ... } n+?`

```none
BMI  Branch on Result Minus

     branch on N = 1                  N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BMI oper      30    2     2**
```

---

### `BNE` branch on not equal (zero clear)

- `=={ ... }`
- `!= goto label`
- `{ ... } !=`
- `z+?{ ... }`
- `z-? goto label`
- `{ ... } z-?`

```none
BNE  Branch on Result not Zero

     branch on Z = 0                  N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BNE oper      D0    2     2**
```

---

### `BPL` branch on plus (negative clear)

- `<0{ ... }`
- `>=0 goto label`
- `{ ... } >=0`
- `n+?{ ... }`
- `n-? goto label`
- `{ ... } n-?`

```none
BPL  Branch on Result Plus

     branch on N = 0                  N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BPL oper      10    2     2**
```

---

### `BRK` interrupt

Raw mnemonic: `brk` or `brk #imm`. The one-character shorthand `%` emits
`brk #0`, which is useful as a compact debug breakpoint marker.

```k65
%        // brk #0
brk #1   // explicit BRK immediate
```

```none
BRK  Force Break

     interrupt,                       N Z C I D V
     push PC+2, push SR               - - - 1 - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       BRK           00    1     7
```

---

### `BVC` branch on overflow clear

- `<<={ ... }`
- `>>= goto label`
- `{ ... } >>=`
- `v+?{ ... }`
- `v- goto label`
- `{ ... } v-`

```none
BVC  Branch on Overflow Clear

     branch on V = 0                  N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BVC oper      50    2     2**
```

---

### `BVS` branch on overflow set

- `>>={ ... }`
- `<<= goto label`
- `{ ... } <<=`
- `v-?{ ... }`
- `v+ goto label`
- `{ ... } v+`

```none
BVS  Branch on Overflow Set

     branch on V = 1                  N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BVS oper      70    2     2**
```

---

### `CLC` clear carry

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`c-`|||||||

```none
CLC  Clear Carry Flag

     0 -> C                           N Z C I D V
                                      - - 0 - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       CLC           18    1     2
```

---

### `CLD` clear decimal

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`d-`|||||||

```none
CLD  Clear Decimal Mode

     0 -> D                           N Z C I D V
                                      - - - - 0 -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       CLD           D8    1     2
```

---

### `CLI` clear interrupt disable

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`i-`|||||||

```none
CLI  Clear Interrupt Disable Bit

     0 -> I                           N Z C I D V
                                      - - - 0 - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       CLI           58    1     2
```

---

### `CLV` clear overflow

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`v-`|||||||

```none
CLV  Clear Overflow Flag

     0 -> V                           N Z C I D V
                                      - - - - - 0

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       CLV           B8    1     2
```

---

### `CMP` compare (with accumulator)

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`a?imm`|`a?mem`|`a?mem,x`|`a?mem,y`|`a?(mem,x)`|`a?(mem),y`

```none
CMP  Compare Memory with Accumulator

     A - M                            N Z C I D V
                                      + + + - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     CMP #oper     C9    2     2
     zeropage      CMP oper      C5    2     3
     zeropage,X    CMP oper,X    D5    2     4
     absolute      CMP oper      CD    3     4
     absolute,X    CMP oper,X    DD    3     4*
     absolute,Y    CMP oper,Y    D9    3     4*
     (indirect,X)  CMP (oper,X)  C1    2     6
     (indirect),Y  CMP (oper),Y  D1    2     5*
```

---

### `CPX` compare with X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`x?imm`|`x?mem`|||||

```none
CPX  Compare Memory and Index X

     X - M                            N Z C I D V
                                      + + + - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     CPX #oper     E0    2     2
     zeropage      CPX oper      E4    2     3
     absolute      CPX oper      EC    3     4
```

---

### `CPY` compare with Y

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`y?imm`|`y?mem`|||||

```none
CPY  Compare Memory and Index Y

     Y - M                            N Z C I D V
                                      + + + - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     CPY #oper     C0    2     2
     zeropage      CPY oper      C4    2     3
     absolute      CPY oper      CC    3     4
```

---

### `DEC` decrement

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`mem--`|`mem,x--`||||

```none
DEC  Decrement Memory by One

     M - 1 -> M                       N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      DEC oper      C6    2     5
     zeropage,X    DEC oper,X    D6    2     6
     absolute      DEC oper      CE    3     6
     absolute,X    DEC oper,X    DE    3     7
```

---

### `DEX` decrement X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`x--`|||||||

```none
DEX  Decrement Index X by One

     X - 1 -> X                       N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       DEC           CA    1     2
```

---

### `DEY` decrement Y

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`y--`|||||||

```none
DEY  Decrement Index Y by One

     Y - 1 -> Y                       N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       DEC           88    1     2
```

---

### `EOR` exclusive or (with accumulator)

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`a^imm`|`a^mem`|`a^mem,x`|`a^mem,y`|`a^(mem,x)`|`a^(mem),y`|

```none
EOR  Exclusive-OR Memory with Accumulator

     A EOR M -> A                     N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     EOR #oper     49    2     2
     zeropage      EOR oper      45    2     3
     zeropage,X    EOR oper,X    55    2     4
     absolute      EOR oper      4D    3     4
     absolute,X    EOR oper,X    5D    3     4*
     absolute,Y    EOR oper,Y    59    3     4*
     (indirect,X)  EOR (oper,X)  41    2     6
     (indirect),Y  EOR (oper),Y  51    2     5*
```

---

### `INC` increment

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`mem++`|`mem,x++`||||

```none
INC  Increment Memory by One

     M + 1 -> M                       N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      INC oper      E6    2     5
     zeropage,X    INC oper,X    F6    2     6
     absolute      INC oper      EE    3     6
     absolute,X    INC oper,X    FE    3     7
```

---

### `INX` increment X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`x++`|||||||

```none
INX  Increment Index X by One

     X + 1 -> X                       N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       INX           E8    1     2
```

---

### `INY` increment Y

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`y++`|||||||

```none
INY  Increment Index Y by One

     Y + 1 -> Y                       N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       INY           C8    1     2
```

---

### `JMP` jump

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`goto mem`|||||`goto (mem)`

```none
JMP  Jump to New Location

     (PC+1) -> PCL                    N Z C I D V
     (PC+2) -> PCH                    - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     absolute      JMP oper      4C    3     3
     indirect      JMP (oper)    6C    3     5
```

---

### `JSR` jump subroutine

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`call mem`|||||

```none
JSR  Jump to New Location Saving Return Address

     push (PC+2),                     N Z C I D V
     (PC+1) -> PCL                    - - - - - -
     (PC+2) -> PCH

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     absolute      JSR oper      20    3     6
```

---

### `LDA` load accumulator

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`a=imm`|`a=mem`|`a=mem,x`|`a=mem,y`|`a=(mem,x)`|`a=(mem),y`|`a=(mem)`

```none
LDA  Load Accumulator with Memory

     M -> A                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     LDA #oper     A9    2     2
     zeropage      LDA oper      A5    2     3
     zeropage,X    LDA oper,X    B5    2     4
     absolute      LDA oper      AD    3     4
     absolute,X    LDA oper,X    BD    3     4*
     absolute,Y    LDA oper,Y    B9    3     4*
     (indirect,X)  LDA (oper,X)  A1    2     6
     (indirect),Y  LDA (oper),Y  B1    2     5*
```

---

### `LDX` load X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`x=imm`|`x=mem`||`x=mem,y`|||

```none
LDX  Load Index X with Memory

     M -> X                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     LDX #oper     A2    2     2
     zeropage      LDX oper      A6    2     3
     zeropage,Y    LDX oper,Y    B6    2     4
     absolute      LDX oper      AE    3     4
     absolute,Y    LDX oper,Y    BE    3     4*
```

---

### `LDY` load Y

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`y=imm`|`y=mem`|`y=mem,x`||||

```none
LDY  Load Index Y with Memory

     M -> Y                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     LDY #oper     A0    2     2
     zeropage      LDY oper      A4    2     3
     zeropage,X    LDY oper,X    B4    2     4
     absolute      LDY oper      AC    3     4
     absolute,X    LDY oper,X    BC    3     4*
```

---

### `LSR` logical shift right

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
`a>>`|||`mem>>`|`mem,x>>`||||

```none
LSR  Shift One Bit Right (Memory or Accumulator)

     0 -> [76543210] -> C             N Z C I D V
                                      0 + + - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     accumulator   LSR A         4A    1     2
     zeropage      LSR oper      46    2     5
     zeropage,X    LSR oper,X    56    2     6
     absolute      LSR oper      4E    3     6
     absolute,X    LSR oper,X    5E    3     7
```

---

### `NOP` no operation

- `*` for single NOP
- `* N` to emit `N` NOPs (where `N` is a constant expression: literal, global `const`, or inline-fn `#param`)
- `<mnemonic> * N` to repeat any implied-addressing instruction `N` times (e.g. `inx * 3`, `clc * 2`, `pha * 4`)

```none
NOP  No Operation

     ---                              N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       NOP           EA    1     2
```

---

### `ORA` or with accumulator

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`a\|imm`|`a\|mem`|`a\|mem,x`|`a\|mem,y`|`a\|(mem,x)`|`a\|(mem),y`|

```none
ORA  OR Memory with Accumulator

     A OR M -> A                      N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     ORA #oper     09    2     2
     zeropage      ORA oper      05    2     3
     zeropage,X    ORA oper,X    15    2     4
     absolute      ORA oper      0D    3     4
     absolute,X    ORA oper,X    1D    3     4*
     absolute,Y    ORA oper,Y    19    3     4*
     (indirect,X)  ORA (oper,X)  01    2     6
     (indirect),Y  ORA (oper),Y  11    2     5*
```

---

### `PHA` push accumulator

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`a!!`|||||||

```none
PHA  Push Accumulator on Stack

     push A                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PHA           48    1     3
```

---

### `PHP` push processor status (SR)

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`p!!` / `flag!!`|||||||

```none
PHP  Push Processor Status on Stack

     push SR                          N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PHP           08    1     3
```

---

### `PLA` pull accumulator

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`a??`|||||||

```none
PLA  Pull Accumulator from Stack

     pull A                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PLA           68    1     4
```

---

### `PLP` pull processor status (SR)

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`p??` / `flag??`|||||||

```none
PLP  Pull Processor Status from Stack

     pull SR                          N Z C I D V
                                      from stack

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PLP           28    1     4
```

---

### `ROL` rotate left

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
`a<<<`|||`mem<<<`|`mem,x<<<`||||

```none
ROL  Rotate One Bit Left (Memory or Accumulator)

     C <- [76543210] <- C             N Z C I D V
                                      + + + - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     accumulator   ROL A         2A    1     2
     zeropage      ROL oper      26    2     5
     zeropage,X    ROL oper,X    36    2     6
     absolute      ROL oper      2E    3     6
     absolute,X    ROL oper,X    3E    3     7
```

---

### `ROR` rotate right

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
`a>>>`|||`mem>>>`|`mem,x>>>`||||

```none
ROR  Rotate One Bit Right (Memory or Accumulator)

     C -> [76543210] -> C             N Z C I D V
                                      + + + - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     accumulator   ROR A         6A    1     2
     zeropage      ROR oper      66    2     5
     zeropage,X    ROR oper,X    76    2     6
     absolute      ROR oper      6E    3     6
     absolute,X    ROR oper,X    7E    3     7
```

---

### `RTI` return from interrupt

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`return_i`|||||||

```none
RTI  Return from Interrupt

     pull SR, pull PC                 N Z C I D V
                                      from stack

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       RTI           40    1     6
```

---

### `RTS` return from subroutine

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`return`|||||||

```none
RTS  Return from Subroutine

     pull PC, PC+1 -> PC              N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       RTS           60    1     6
```

---

### `SBC` subtract with carry

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
||`a-imm`|`a-mem`|`a-mem,x`|`a-mem,y`|`a-(mem,x)`|`a-(mem),y`|

```none
SBC  Subtract Memory from Accumulator with Borrow

     A - M - C -> A                   N Z C I D V
                                      + + + - - +

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immidiate     SBC #oper     E9    2     2
     zeropage      SBC oper      E5    2     3
     zeropage,X    SBC oper,X    F5    2     4
     absolute      SBC oper      ED    3     4
     absolute,X    SBC oper,X    FD    3     4*
     absolute,Y    SBC oper,Y    F9    3     4*
     (indirect,X)  SBC (oper,X)  E1    2     6
     (indirect),Y  SBC (oper),Y  F1    2     5*
```

---

### `SEC` set carry

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`c+`|||||||

```none
SEC  Set Carry Flag

     1 -> C                           N Z C I D V
                                      - - 1 - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       SEC           38    1     2
```

---

### `SED` set decimal

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`d+`|||||||

```none
SED  Set Decimal Flag

     1 -> D                           N Z C I D V
                                      - - - - 1 -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       SED           F8    1     2
```

---

### `SEI` set interrupt disable

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`i+`|||||||

```none
SEI  Set Interrupt Disable Status

     1 -> I                           N Z C I D V
                                      - - - 1 - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       SEI           78    1     2
```

---

### `STA` store accumulator

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`mem=a`|`mem,x=a`|`mem,y=a`|`(mem,x)=a`|`(mem),y=a`|

```none
STA  Store Accumulator in Memory

     A -> M                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      STA oper      85    2     3
     zeropage,X    STA oper,X    95    2     4
     absolute      STA oper      8D    3     4
     absolute,X    STA oper,X    9D    3     5
     absolute,Y    STA oper,Y    99    3     5
     (indirect,X)  STA (oper,X)  81    2     6
     (indirect),Y  STA (oper),Y  91    2     6
```

---

### `STX` store X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`mem=x`||`mem,y=x`|||

```none
STX  Store Index X in Memory

     X -> M                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      STX oper      86    2     3
     zeropage,Y    STX oper,Y    96    2     4
     absolute      STX oper      8E    3     4
```

---

### `STY` store Y

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`mem=y`|`mem,x=y`||||

```none
STY  Store Index Y in Memory

     Y -> M                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      STY oper      84    2     3
     zeropage,X    STY oper,X    94    2     4
     absolute      STY oper      8C    3     4
```

---

### `TAX` transfer accumulator to X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`x=a`|||||||

```none
TAX  Transfer Accumulator to Index X

     A -> X                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TAX           AA    1     2
```

---

### `TAY` transfer accumulator to Y

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`y=a`|||||||

```none
TAY  Transfer Accumulator to Index Y

     A -> Y                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TAY           A8    1     2
```

---

### `TSX` transfer stack pointer to X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`x=s`|||||||

```none
TSX  Transfer Stack Pointer to Index X

     SP -> X                          N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TSX           BA    1     2
```

---

### `TXA` transfer X to accumulator

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`a=x`|||||||

```none
TXA  Transfer Index X to Accumulator

     X -> A                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TXA           8A    1     2
```

---

### `TXS` transfer X to stack pointer

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`s=x`|||||||

```none
TXS  Transfer Index X to Stack Register

     X -> SP                          N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TXS           9A    1     2
```

---

### `TYA` transfer Y to accumulator

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`a=y`|||||||

```none
TYA  Transfer Index Y to Accumulator

     Y -> A                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TYA           98    1     2
```

---

### 65816 Transfer Instructions

The 65816 adds register-to-register transfers for the D (direct page), S (stack pointer), and B (data bank) registers:

#### `TCD` transfer accumulator to direct page

Acc|Implied
:---:|:---:
|`d=c`

```none
TCD  Transfer 16-Bit Accumulator to Direct Page Register

     C -> D                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TCD           5B    1     2
```

---

#### `TCS` transfer accumulator to stack pointer

Acc|Implied
:---:|:---:
|`s=c`

```none
TCS  Transfer 16-Bit Accumulator to Stack Pointer

     C -> S                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TCS           1B    1     2
```

---

#### `TDC` transfer direct page to accumulator

Acc|Implied
:---:|:---:
|`c=d`

```none
TDC  Transfer Direct Page Register to 16-Bit Accumulator

     D -> C                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TDC           7B    1     2
```

---

#### `TSC` transfer stack pointer to accumulator

Acc|Implied
:---:|:---:
|`c=s`

```none
TSC  Transfer Stack Pointer to 16-Bit Accumulator

     S -> C                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TSC           3B    1     2
```

---

#### `TXY` transfer X to Y

Acc|Implied
:---:|:---:
|`y=x`

```none
TXY  Transfer Index Register X to Index Register Y

     X -> Y                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TXY           9B    1     2
```

---

#### `TYX` transfer Y to X

Acc|Implied
:---:|:---:
|`x=y`

```none
TYX  Transfer Index Register Y to Index Register X

     Y -> X                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       TYX           BB    1     2
```

---

#### `XBA` exchange B and A accumulators

Acc|Implied
:---:|:---:
|`b~a` or `a~b`

```none
XBA  Exchange B and A Accumulators

     B <-> A                          N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       XBA           EB    1     3
```

---

### Complete Register Transfer Table

Source \ Dest|A|B|C|D|X|Y|S
:---|:---:|:---:|:---:|:---:|:---:|:---:|:---:
A|-|`b~a`|-|`x=a` TAX|`y=a` TAY|-
B|`b~a`|-|-|-|-|-|-
C|-|-|-|`d=c` TCD|-|-|`s=c` TCS
D|-|-|`c=d` TDC|-|-|-|-
X|`a=x` TXA|-|-|-|-|`y=x` TXY|`s=x` TXS
Y|`a=y` TYA|-|-|-|`x=y` TYX|-|-
S|-|-|`c=s` TSC|-|`x=s` TSX|-|-

Unsupported direct transfers (e.g. `s=y`, `d=x`) produce a compile error with a hint suggesting a two-step chain:

```k65
s=x=y                  // s=y is not supported; use s=x=y instead (TYX + TXS)
d=c=s                  // d=s is not supported; use d=c=s instead (TSC + TCD)
```

---

## 65C02 / 65816 Instruction Extensions

These instructions are unique to the WDC 65C02 and 65816 CPU cores. The K816 ISA recognises every entry listed here. For cycle counts, `n` denotes the number of bytes moved (for block moves) and `m` denotes the accumulator/index width as selected by the `M`/`X` status flags (`@a8`/`@a16`/`@i8`/`@i16`).

### 65C02 Additions

#### `BRA` branch always

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `bra label`. The compiler also emits `BRA` for unconditional `break`/`repeat` in loop constructs.

```none
BRA  Branch Always

     PC + 2 + offset -> PC            N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative      BRA oper      80    2     3
```

---

#### `PHX` push X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`x!!`|||||||

Width follows `@i8`/`@i16` — a 16-bit X pushes 2 bytes.

```none
PHX  Push Index X on Stack

     push X                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PHX           DA    1    3-4
```

---

#### `PHY` push Y

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`y!!`|||||||

Width follows `@i8`/`@i16`.

```none
PHY  Push Index Y on Stack

     push Y                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PHY           5A    1    3-4
```

---

#### `PLX` pull X

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`x??`|||||||

Width follows `@i8`/`@i16`.

```none
PLX  Pull Index X from Stack

     pull X                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PLX           FA    1    4-5
```

---

#### `PLY` pull Y

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`y??`|||||||

Width follows `@i8`/`@i16`.

```none
PLY  Pull Index Y from Stack

     pull Y                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PLY           7A    1    4-5
```

---

#### `STZ` store zero

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|||`mem=0`|`mem,x=0`|||||

Raw mnemonic: `stz zp` / `stz zp,x` / `stz abs` / `stz abs,x`. See the [Zero-Store Shortcut](#zero-store-shortcut) section: `mem = 0` lowers directly to `STZ`, and `mem = a = 0` chains get `STZ` via peephole rewrite.

```none
STZ  Store Zero in Memory

     0 -> M                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      STZ oper      64    2     3
     zeropage,X    STZ oper,X    74    2     4
     absolute      STZ oper      9C    3     4
     absolute,X    STZ oper,X    9E    3     5
```

---

#### `TSB` test and set bits

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `tsb zp` / `tsb abs`. Sets the Z flag from `A & M`, then stores `A | M` back to memory.

```none
TSB  Test and Set Memory Bits with Accumulator

     M | A -> M                       N Z C I D V
     A & M (set Z only)               - + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      TSB oper      04    2     5
     absolute      TSB oper      0C    3     6
```

---

#### `TRB` test and reset bits

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `trb zp` / `trb abs`. Sets the Z flag from `A & M`, then stores `~A & M` back to memory (clears the bits of `M` that are set in `A`).

```none
TRB  Test and Reset Memory Bits with Accumulator

     ~A & M -> M                      N Z C I D V
     A & M (set Z only)               - + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     zeropage      TRB oper      14    2     5
     absolute      TRB oper      1C    3     6
```

---

#### `WAI` wait for interrupt

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `wai`. Halts the CPU until an interrupt is received, minimising interrupt latency.

```none
WAI  Wait for Interrupt

     halt until IRQ/NMI/ABORT/RES     N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       WAI           CB    1     3
```

---

#### `STP` stop

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `stp`. Halts the CPU until a hardware reset is received.

```none
STP  Stop the Processor

     halt until RES                   N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       STP           DB    1     3
```

---

### 65816 Bank Registers and Mode Switch

`TCD`/`TCS`/`TDC`/`TSC`/`TXY`/`TYX`/`XBA` are documented above under [65816 Transfer Instructions](#65816-transfer-instructions). The entries below cover the 65816's bank-register stack ops and the emulation/native mode switch.

#### `PHB` push data bank register

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`b!!`|||||||

See the `example-reset-vector` fixture for real-world PLB use during startup.

```none
PHB  Push Data Bank Register on Stack

     push DBR                         N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PHB           8B    1     3
```

---

#### `PLB` pull data bank register

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`b??`|||||||

```none
PLB  Pull Data Bank Register from Stack

     pull DBR                         N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PLB           AB    1     4
```

---

#### `PHD` push direct page register

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`d!!`|||||||

D is always 16-bit, so PHD always pushes 2 bytes.

```none
PHD  Push Direct Page Register on Stack

     push D                           N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PHD           0B    1     4
```

---

#### `PLD` pull direct page register

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:
|`d??`|||||||

```none
PLD  Pull Direct Page Register from Stack

     pull D                           N Z C I D V
                                      + + - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PLD           2B    1     5
```

---

#### `PHK` push program bank register

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic only — the 65816 has no `PLK` (program bank is only writable via long jump/call/return), so K65 exposes no `k!!` / `k??` HLA shorthand.

```none
PHK  Push Program Bank Register on Stack

     push PBR                         N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       PHK           4B    1     3
```

---

#### `XCE` exchange carry and emulation

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `xce`. Swaps the hidden emulation flag `E` with the carry flag `C`. Executing `clc; xce` transitions the 65816 from emulation mode into native (16-bit) mode; `sec; xce` returns to emulation. See the `example-reset-vector` fixture.

```none
XCE  Exchange Carry and Emulation Flags

     C <-> E                          N Z C I D V
                                      - - + - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       XCE           FB    1     2
```

---

### 65816 Control Flow, Mode and Stack

#### `BRL` branch long

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `brl label`. A 16-bit signed PC-relative branch; reaches anywhere within the current 64 KiB bank.

```none
BRL  Branch Always Long

     PC + 3 + s16-offset -> PC        N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative long BRL oper      82    3     4
```

---

#### `JML` jump long

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `jml addr`. `far goto label` lowers to `JML`.

```none
JML  Jump Long (24-bit Absolute)

     operand -> PC, PBR               N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     absolute long JML oper      5C    4     4
```

---

#### `JSL` jump subroutine long

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `jsl addr`. See [Far Calls](#far-calls): `far func_name` and `call far name` lower to `JSL`, with a matching `RTL` at function exit.

```none
JSL  Jump to Subroutine Long

     push PBR, push PC+3              N Z C I D V
     operand -> PC, PBR               - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     absolute long JSL oper      22    4     8
```

---

#### `RTL` return long

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `rtl`. Emitted automatically at the end of a `far func`; can be written explicitly inside a `naked far` block (see the `syntax-data-fars` fixture).

```none
RTL  Return from Subroutine Long

     pull PC, pull PBR                N Z C I D V
     PC + 1 -> PC                     - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     implied       RTL           6B    1     6
```

---

#### `REP` reset P bits

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `rep #mask`. Each set bit in the immediate mask clears the corresponding bit of the status register. `@a16` synthesises `REP #$20` (clear M, accumulator becomes 16-bit); `@i16` synthesises `REP #$10` (clear X, index registers become 16-bit).

```none
REP  Reset P Bits

     P & ~mask -> P                   N Z C I D V
                                      * * * * * *

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immediate     REP #mask     C2    2     3
```

---

#### `SEP` set P bits

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `sep #mask`. Each set bit in the immediate mask sets the corresponding bit of the status register. `@a8` synthesises `SEP #$20` (set M, accumulator becomes 8-bit); `@i8` synthesises `SEP #$10` (set X, index registers become 8-bit).

```none
SEP  Set P Bits

     P | mask -> P                    N Z C I D V
                                      * * * * * *

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immediate     SEP #mask     E2    2     3
```

---

#### `PEA` push effective absolute

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `pea #addr`. Pushes a 16-bit immediate onto the stack. Despite the `#` syntax, the operand is typically a 16-bit address constant — useful for building stack frames or pushing return targets.

```none
PEA  Push Effective Absolute Address

     push immediate word              N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immediate     PEA #oper     F4    3     5
```

---

#### `PEI` push effective indirect

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `pei (zp)`. Reads a 16-bit word from the direct page and pushes it onto the stack.

```none
PEI  Push Effective Indirect Address

     push word at D+zp                N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     (zeropage)    PEI (oper)    D4    2     6
```

---

#### `PER` push effective PC-relative

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `per label`. Computes `PC + 3 + s16-offset` and pushes the resulting 16-bit address onto the stack — useful for building position-independent code.

```none
PER  Push Effective PC-Relative Address

     push PC + 3 + s16-offset         N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     relative long PER oper      62    3     6
```

---

#### `COP` coprocessor trap

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `cop #imm`. Software interrupt via the coprocessor vector; the immediate signature byte is available to the handler.

```none
COP  Coprocessor Trap

     interrupt via COP vector         N Z C I D V
     push PC+2, push SR               - - - 1 0 -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immediate     COP #oper     02    2     7
```

---

#### `WDM` reserved

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `wdm #imm`. Reserved by WDC for future expansion; acts as a 2-byte no-op on current 65816 cores.

```none
WDM  Reserved for Future Expansion (no-op)

     no effect                        N Z C I D V
                                      - - - - - -

     addressing    assembler    opc  bytes  cyles
     --------------------------------------------
     immediate     WDM #oper     42    2     2
```

---

### 65816 Block Moves

Both instructions copy `C + 1` bytes (where `C` is the full 16-bit accumulator) from source bank `src_bank` to destination bank `dst_bank`. X holds the source address, Y the destination. MVN increments both indices; MVP decrements. Use MVN for non-overlapping or upward-growing copies, MVP when the destination overlaps ahead of the source.

#### `MVN` block move negative

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `mvn src_bank, dst_bank`. The compiler parses two comma-separated operands; fixture `syntax-native-native` shows `mvn 0,0`.

```none
MVN  Block Move Negative (source < dest, copy ascending)

     repeat C+1 times:                N Z C I D V
       M[src_bank:X] -> M[dst_bank:Y] - - - - - -
       X++, Y++, C--

     addressing    assembler     opc  bytes  cyles
     ---------------------------------------------
     block move    MVN src,dst    54    3   7*(C+1)
```

---

#### `MVP` block move positive

Acc|Implied|Imm|Mem|Mem,X|Mem,Y|(Mem,X)|(Mem),Y|(Mem)
:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:

Raw mnemonic: `mvp src_bank, dst_bank`.

```none
MVP  Block Move Positive (source > dest, copy descending)

     repeat C+1 times:                N Z C I D V
       M[src_bank:X] -> M[dst_bank:Y] - - - - - -
       X--, Y--, C--

     addressing    assembler     opc  bytes  cyles
     ---------------------------------------------
     block move    MVP src,dst    44    3   7*(C+1)
```

---

## Branch and Flow Control

### Branch Operator Mapping

Comparison|Flag|Prefix|Goto|Postfix|6502
:---|:---|:---|:---|:---|:---
`<`|`c-?`|`>={ ... }`|`< goto label`|`{ ... } <`|BCC
`>=`|`c+?`|`<{ ... }`|`>= goto label`|`{ ... } >=`|BCS
`==`|`z+?`|`!={ ... }`|`== goto label`|`{ ... } ==`|BEQ
`!=`|`z-?`|`=={ ... }`|`!= goto label`|`{ ... } !=`|BNE
`<0`|`n+?`|`>=0{ ... }`|`<0 goto label`|`{ ... } <0`|BMI
`>=0`|`n-?`|`<0{ ... }`|`>=0 goto label`|`{ ... } >=0`|BPL
`>>=`|`v-` / `v-?`|`<<={ ... }` or `v+?{ ... }`|`>>= goto label` or `v- goto label`|`{ ... } >>=` or `{ ... } v-`|BVC
`<<=`|`v+` / `v+?`|`>>={ ... }` or `v-?{ ... }`|`<<= goto label` or `v+ goto label`|`{ ... } <<=` or `{ ... } v+`|BVS

Note: prefix form inverts the condition (block executes when condition is false, branch skips over it).

### Prefix Form

Prefix form emits a branch *over* the block, so the parser uses the opposite branch mnemonic for the block condition:

```k65
a?10
>={ x++ }             // executes x++ only when A < 10 (BCS skips over)

c-?{ mem++ }          // executes mem++ only when carry is clear
```

### Goto Form

The condition followed by `goto label` branches to the label when true:

```k65
a?10
< goto done           // branch to 'done' when A < 10 (BCC)
c-? goto done         // branch when carry clear
```

### Postfix Form

The condition after `{ ... }` branches back to the start of the block:

```k65
x=10 {
  x--
} !=                   // loop while X != 0 (BNE back to start)

{
  a=ready
} n-?                  // loop until negative flag set (BPL back)
```

### Loops

```k65
{ ... } always         // unconditional loop (JMP back to start)
{ ... } never          // one-shot block (no branch back)
```

### `break` and `repeat`

Within loop blocks:

```k65
{
  x++
  < break              // exit loop on carry clear (BCC forward)
  != repeat            // restart loop on not-zero (BNE back)
  break                // unconditional exit (JMP forward)
} always
```

### If-Else

```k65
=={
  a=1
} else {
  a=2
}
```

### Flow Control Instructions

```k65
goto label             // JMP absolute
goto (vec)             // JMP indirect
call label             // JSR (near call)
call far label         // JSL (far call, 24-bit addressing)
far label              // JSL (far call, alternative syntax)
return                 // RTS
return_i               // RTI
```

### `nocross` Code Wrappers

The `nocross` modifier ensures a code block fits within a single page:

```k65
nocross {
  a=mem
  x++
} !=
```

### Wait-Loop Patterns

Common polling patterns:

```k65
{ a&?READY } n-?       // BIT-based wait (wait for bit 7 clear)
{ a=ready } n-?        // LDA-based wait (poll until positive)
```

### Efficiency Warnings

The `>` and `<=` postfix operators emit efficiency warnings:

```k65
{ x++ } >              // forward branch with efficiency warning
{ x++ } <=             // backward branch with efficiency warning
```

## NOP and Wait Shorthand

```k65
*                       // single NOP
* 5                     // 5 NOPs
* COUNT                 // N NOPs, with N from a global `const`
inx * 3                 // repeat any implied-mode instruction N times
```

## Preprocessor Directives

```k65
#if EXPR
  // conditional code
#elif EXPR
  // alternative
#else
  // fallback
#endif

#error "message"        // emit error and stop compilation
#warn "message"         // emit warning
```

Preprocessor directives work at top-level, inside code sections, and inside data blocks.

## Additional Syntax Notes

### Semicolons

Semicolons are optional statement separators:

```k65
a=0; x=a; y=a;;         // multiple statements per line, empty statements ignored
```

### Mixed-Case Registers and Flags

Register names (A, B, C, D, X, Y, S) and flag names (C, D, I, V, N, Z) are case-insensitive. C and D serve dual roles — register in assignment context, flag with `+`/`-` suffix:

```k65
A=0 X=A Y=x            // uppercase and lowercase registers
d=c s=x                // 65816 register transfers
C+ D- I+ V-            // flag operations (set/clear)
a~b                    // swap A and B accumulators (XBA)
```

### Variable+Offset Addressing

Variables support compile-time address arithmetic:

```k65
var base = 0x200

a=base+3               // LDA base+3 (address 0x203)
a=base-2               // LDA base-2 (address 0x1FE)
```

## Notes

\*  add 1 to cycles if page boundary is crossed

\*\* add 1 to cycles if branch occurs on same page /
   add 2 to cycles if branch occurs to different page

### Legend to Flags

```none
 + .... modified
 - .... not modified
 1 .... set
 0 .... cleared
M6 .... memory bit 6
M7 .... memory bit 7
```
