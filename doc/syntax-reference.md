# K816 Syntax Reference

This reference documents the K816 language, which grew from K65's syntax. Where K816 writes a form its own way — a loop, a flag test, the stack, a condition — a `.k65` source writes K65's spelling of it instead, and each dialect refuses the other's ([`k65-compat.md`](k65-compat.md), "Two dialects, one grammar"); the examples here are K816. Behavior is cross-checked with the reference compiler grammar in `vendor/src/compiler.inc`.

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

Within evaluator expressions, floating-point literals are also supported (e.g. `[1.5 + .5]`). A value that flows back into a K65 integer context is **rounded to the nearest integer, half up** — `2.5` becomes `3` and `-2.5` becomes `-2` — and the target's own width and range check then applies to the result, exactly as it applies to a literal. Nothing is masked: a bracket that produces `300` in a byte entry is refused (`K816-E0109`), just as the literal `300` is.

The arithmetic in between is double precision and both *ends* are single, which is visible in two places. A decimal literal is a `float`, so `[16777217 - 16777216]` is `0`; a `0x` or `0b` literal is not, so `[0x1000001 - 0x1000000]` is `1`. See "Compile-time Evaluator" below.

## Variable Declaration

Variables in K65 are named storage slots. Each declaration belongs to **one of three orthogonal storage classes** that determine where the variable lives and how plain references encode:

| Class              | Syntax                              | Address width | Auto-allocator                              | Initializer required? |
| ------------------ | ----------------------------------- | ------------- | ------------------------------------------- | --------------------- |
| Direct page        | `var dp NAME[:w]`                   | 1 byte        | global 256-byte logical pool, **link-time** | no                    |
| Absolute (default) | `var NAME[:w]` or `var abs NAME[:w]`| 2 bytes       | per-segment cursor                          | no                    |
| Far                | `var far NAME[:w]`                  | 3 bytes       | **none — must have an explicit address**    | yes                   |

Storage class is set by the `dp` / `abs` / `far` prefix before the name. It is **independent** of the data-width suffix (`:byte`, `:word`, `:far`) — `var dp ptr:far` means a 3-byte FAR pointer stored at a 1-byte DP offset. The DP pool reserves three bytes for the variable, while plain references to `ptr` use direct-page address encoding by default. See the "two orthogonal axes" section below.

The final address of a variable is decided at one of two points:

- `var dp foo = $80` / `var foo = $80` / `var far foo = $7E0000` — **fixed at compile time**. The right-hand side must reduce to a constant numeric expression — a pinned variable's own address counts, in this unit or one it imports, so `var chargen = text + $0800` is `$2800` when `text` is `$2000`; the assembler bakes the address into every reference. Use this for hardware-mapped I/O (`var VIA = $6000`), pinned DP slots, or any FAR symbol whose bank you control externally.
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

A bracket that opens with a `.name` is not a span at all but a **symbolic subscript field list**, described in `doc/symbolic-subscripts.md`; one declaration cannot have both, because the bracket opens one or the other. A count of zero is rejected wherever it is written — `var buf[0]`, `var t * 0`, and the field count `.str[0]` are all `K816-E0141`: storage with no elements reserves nothing, and every index into it is out of range the moment it is written.

### What K816 decides, and what the linker decides

K816 decides *size*; ld65 decides *address*. A pinned declaration becomes a ca65 equate and reserves nothing — the address is already in the source, and the bytes belong to whatever put them there. An allocated one becomes a label and a `.res` of exactly the bytes its axes multiply out to, emitted in the segment the declaration sits in, and ld65 places it.

Allocated direct-page variables are the exception, and they are reserved together in a segment named `ZEROPAGE`, declared to ca65 as a zeropage-type segment so that references to them encode in one byte and cross-unit ones export as `.importzp` / `.exportzp`. That segment needs a 256-byte `zp`-type entry in the ld65 configuration:

```
MEMORY   { ZP: start = $0000, size = $0100; }
SEGMENTS { ZEROPAGE: load = ZP, type = zp, optional = yes; }
```

The reservations appear at the end of the emitted unit, after everything else, because the pool is not part of any segment the source selected and must not disturb the one in force.

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
- DP pool overflow is `K816-E0133`, reported by the frontend at the declaration that exceeds the page rather than discovered as a link-time segment overflow. K816 counts what one compilation unit asks for: allocated slots add up, and pinned ones contribute the byte range they cover — counted once where two of them overlap, since overlapping pinned views are allowed on purpose. A pinned slot whose bytes run past `$FF` is the same code with a different message: `var dp p:far = $FE` is three bytes starting two from the end of the page.
- Across units the check is ld65's, and it is the one it already makes: the `ZEROPAGE` segment is 256 bytes in the linker configuration, so two units that each fit on their own but do not fit together overflow the memory area at link time. K816 sees one unit at a time and says what it can about that one.
- DP storage is independent of `segment` directives and the linker's `memory` / `segments` config. There is no `MemoryKind::DirectPage` and no DP-section concept; the linker only tracks symbol-name → 8-bit DP offset.

### Far storage (`var far`)

`var far` declares a 24-bit-addressable variable. Because the assembler has no notion of which 64K bank should hold an unaddressed FAR symbol, **FAR auto-allocation is rejected** — the user must supply an explicit address:

```k65
var far rom_table = $C00000  // OK: bank $C0, offset $0000
var far missing_addr         // K816-E0139: FAR variable must have an explicit address
```

`K816-E0139` is a frontend error because nothing downstream could make the decision either: an address is 24 bits wide because it names a bank, and neither K816 nor ld65 has a rule for which of the 256 banks an unaddressed `far` symbol belongs in.

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

The two multipliers sit on opposite sides of the element boundary, and the difference is visible in the stride:

```k65
var pairs:word[2] * 3      // 3 elements of 2 words: 12 bytes, stride 4
```

`[2]` is *inside* the element — "it repeats the computed element layout", and the raw span is part of that layout — so `pairs[1]` is four bytes along, not two. `* 3` is outside it, and is the only one of the two that makes the bracket element access at all.

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

`NAME[i]` is compile-time address arithmetic: the index must be a constant numeric expression (`K816-E0134` otherwise) and must satisfy `0 <= i < repeat_count` (`K816-E0135`). Runtime-variable indices are out of scope for this bracket form; use CPU indexed addressing or explicit pointer arithmetic for those.

On a variable with no `* count` there is no extent to check and no element to stride, so the bracket steps bytes and neither code applies. That includes the raw span: `var grid:word[4]` reserves four words, and `grid[2]` is two *bytes* along.

A *field's* bracket is the other one. `.str :byte [5]` declares five elements, not a five-byte span, and `foo.str[i]` is `field_base + i * element_width` checked against the five — `doc/symbolic-subscripts.md`, "Semantics". So the same `[N]` means a span on a variable and a count on a field, and a field always has an extent, where a variable has one only when it wrote `* count`:

```k65
var grid:word[4]           // a span: grid[2] is 2 bytes along, unchecked
var pts[ .xs :word [4] ]   // a count: pts.xs[2] is 4 bytes along, checked
```

A field that wrote no bracket is one element, so indexing it is `K816-E0135` rather than a silent stride of zero.

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

Both directions are `K816-E0136`, because they are one complaint from where the author sits — this declares a layout, and a layout has no bytes. The declaration is still recorded when it is refused, so a use of the name reports what is wrong with the *use* rather than reporting the name as undeclared. **A layout crosses a direct `import`**, an `abstract var`'s and a `var`'s alike, because offsets are arithmetic settled where they were written — the same kind of compile-time fact a `const`'s value is. A `var` crosses as a name and an address size, which is what ca65 and ld65 need of it, and its field list travels beside it, so `RIA.op`, `TASKS.state,x`, `&&TASKS[1]` and `TASKS:sizeof` mean in an importing unit what they mean where the variable is declared; the base stays a symbol ld65 resolves, and the offset is a number added to it. An `abstract var` is still not a *definition*: it emits nothing, ld65 never sees it, and two units may declare the same layout without that being a duplicate, with a unit's own declaration winning its own name.

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

#### What the width check covers

A mismatch between a datum's declared width and the register moving it is `K816-E0140`, in both directions: a `:word` read with an 8-bit accumulator reads half of it, and a `:byte` one read with a 16-bit accumulator reads a byte that is not part of it. A `:far` datum is the code's third case and needs no register width to be wrong.

Three things bound the check, and each is a deliberate limit rather than an oversight:

- **A declaration with no `:` suffix makes no claim.** It is not a defaulted `:byte`. Allocation still treats an unwritten width as one byte — `var arr[10]` is ten bytes — but that is a question about *size*, and the absence of a suffix is the absence of an answer about *width*. `var counter` loads in `@a8` and in `@a16` alike.
- **Only accesses that move the datum through a register are checked**, which is what the rule is about. `lda`, `sta`, `ldx`, `cpy`, and the ALU operations qualify; `inc mem` and `stz mem` move their datum through no register, so there is nothing to disagree with them.
- **A pointer is not its target.** `lda [ptr]` names `ptr` and loads what it points at, so the datum check does not apply to it — which is what makes the recommended spelling for a `:far` variable legal.

Where the tracked width is unknown, the check is silent: an unknown width is not a claim either, and `K816-E0104` is the diagnostic for not knowing one.

#### The pointer has a width of its own

The last bullet says the datum check leaves an indirect operand alone. The *pointer* is still checked, against a different question: how many bytes of address the mode reads out of the direct page. `(ptr)`, `(ptr),y` and `(ptr,x)` read **two**; `[ptr]` and `[ptr],y` read **three**. A pointer declared narrower than that is read past its own end, into whatever ld65 placed after it; one declared wider is read short, silently dropping the bank it was declared to hold. Either way the address is wrong and nothing says so, which is why a declared width that disagrees is `K816-E0172`:

```k65
var dp one:byte
var dp near:word
var dp long:far

func main @a8 @i8 {
  lda (near)       // fine: two bytes read, two declared
  lda [long]       // fine: three and three
  lda (one)        // K816-E0172 — reads `one` and the byte after it
  lda (long)       // K816-E0172 — reads two of the three, dropping the bank
  lda [near]       // K816-E0172 — reads a bank byte `near` does not have
}
```

This is the *pointer's* width, not the datum's: how wide the thing it reaches is stays the M flag's business, so `lda (near)` is a one-byte load in `@a8` and a two-byte one in `@a16` with the same two-byte pointer. And the first bullet above still holds — a declaration with no `:` suffix makes no claim, so `var ptr = $80` with `lda (ptr)` is what it always was, which is how every pointer in the original's own sources is written.

`(ptr,s),y` is not in the list. Its operand is an offset into the stack frame rather than a variable that holds a pointer, and `K816-E0168` is what governs it.

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

An offset from a name is at the name's address size. `slot+1` is one byte past `slot`, so it takes `slot`'s encoding rather than falling back to Absolute, and it does so in reads and in stores alike — `a = slot+1` and `slot+1 = a` cannot disagree about how the same expression is encoded. The rule follows the *base* of the offset: one side of a `+` must be rooted in a name and the other must not, and a `-` takes its base from the left only.

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

Each needs something that can answer it, and `K816-E0115` is what says so when there is none. `:sizeof` needs a variable or a field — a `const` is a value with no size, and a function is a place with no size. `:offsetof` needs a *field*, because an offset is a field's distance from the base of its variable: `counter:offsetof` is not zero, it is a question about nothing.

A metadata query is an integer from the moment it folds, so it reads on the value side of an assignment exactly as a `const` does — `a = TASKS:sizeof` loads the number and does not read memory at the address `4`.

Both work on an `abstract var`, which is the whole reason one may be declared. Neither reads the variable's storage, so neither is affected by where it lives.

K65 also supports symbolic subscript arrays with named `.field` entries inside `var name[...] = <addr>`. This feature is inspired by the Pawn language's named-field array style.

## Constant Declaration

The best way of defining constants is using the evaluator. Evaluator-defined constants are processed in source order and can be mutated within a single evaluator block (`=`, `+=`, `++`, and similar operators). Reassigning the same constant name in a later evaluator block is an error (`K816-E0149`), and so is assigning to a name the compiler already owns — a `const`, a `var`, a label. Evaluator constants can be floating-point values; where one reaches an integer context — an instruction operand, a variable address, a data entry — it is rounded to the nearest integer, half up, and must then fit the encoding width the context selected.

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

### A constant's size

A `const` written without a size has none. Each use gives it the size that use needs, the way a widthless `inline` takes the width of the place it is expanded:

```k65
const EUNKNOWN = -1

func fail @a16 @i8 {
    lda #EUNKNOWN         // lda #$FFFF: the accumulator is 16 bits here
    ldx #EUNKNOWN         // ldx #$FF: and the index registers 8
    push EUNKNOWN         // pea $FFFF: PEA always pushes a word
}

data codes {
    EUNKNOWN              // .byte $FF
    word EUNKNOWN         // .word $FFFF
    far EUNKNOWN          // .faraddr $FFFFFF
}
```

A value fits a use *n* bits wide when it lies in −2^(n−1) … 2^n−1, and the use emits its low *n* bits. So −1 and 255 both fit a byte, as `$FF`, and −129 and 256 do not: each is the range error its use reports, `K816-E0103` for an immediate, `K816-E0109` for a data entry and `K816-E0170` for an `inline` argument. A literal is a value too, and `lda #-1` is `lda #$FF` at `@a8`. A constant is never an address — `lda EUNKNOWN` is `K816-E0110` — so the rule has nothing to say about one.

A number is 32 bits of two's complement, so a pattern with its top bit set is the negative it spells: `$FFFFFFFF` and `-1` are one value, and `const ALL = $FFFFFFFF` is `$FF` in a byte as well. The evaluator reads a constant the same way, so `[EUNKNOWN + 1]` is zero.

A unit-level constant is exported, for ca65 code and for an `extern const` elsewhere. One that fits `$0000` … `$FFFF` is exported absolute, `.export LIMIT : abs = $000F`, which is what a ca65 `.import` expects, and one that fits 24 bits far. Any other — a negative one, or one past `$FFFFFF` — has no size ca65 would accept and is exported as the number it is, `.export EUNKNOWN = -1`; ca65 makes it `long`, which no 65816 `.import` can name, so ld65 warns about an import of it. A constant meant for ca65 code states its size.

#### A declared size

A size after the name — `:byte`, `:word` or `:far` — declares the range the value lies in and the size the constant is linked at:

```k65
const ERROR:byte = -1     // .export ERROR : zp = $FF
const PAGE:word = $1234   // .export PAGE : abs = $1234
const BANK:far = $7E0000  // .export BANK : far = $7E0000
const LAST:byte = 255, NEXT:byte  // K816-E0178: NEXT is 256
```

The value must fit the size by the rule a use applies — a `:byte` constant lies in −128 … 255 — and one that does not is `K816-E0178`. It is exported at that size, as its low bits at it, which is the form ca65 and ld65 both accept without a warning: `.export ERROR : zp = -1` would be "long but exported zeropage". ca65 code imports it with `.importzp ERROR` and may then write `lda #ERROR` at `.a8`, which an absolute import refuses.

A size is not a width. A use still gives the constant the width it needs, and the value stays the value, so `lda #ERROR` is `lda #$FFFF` at `@a16`, not `$00FF`. Across the link it is the bits the export carries: ca65 code, or an `extern const ERROR:byte`, that reads it at sixteen bits sees `$00FF`, because ld65 has no negative numbers to extend. The one use a size changes is `push`: a `:byte` constant is a one-byte constant, and `push ERROR` is `K816-E0175` with `ERROR:word` as the fix ([Push and pull](#push-and-pull)).

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

`[ expr ]` is the compile-time evaluator, and only that. There is **one** parse: the interior is the evaluator's own language ("Compile-time Evaluator" below), never the K65 expression layer.

`[FLAG_A | FLAG_B]` works on user constants because the evaluator *sees* them — `const` names, the names earlier brackets assigned, and evaluator variables all live in one namespace it reads — and not because a second parser rescues the bracket after the first declines. So `[CONST_A / CONST_B]` works too, and so does every other operator the evaluator has on any operand it can resolve.

Earlier versions of this compiler described a fallback: the bracketed text re-parsed as a K65 expression when evaluation failed. It existed because *that* evaluator could not read a `const`. The original K65 evaluator can, so there is nothing left for a fallback to rescue and K816 does not have one.

### `&&` gotcha

Because `&&` is the existing 16-bit address-of unary prefix in K65, the expression `FOO && BAR` parses as bitwise-AND of `FOO` with `&&BAR` (the 16-bit address of `BAR`). This is rarely what a user wants if they're thinking of logical AND. K65 does not have a logical-AND operator at this layer; if you really need bitwise-AND with an address-of expression, write it with whitespace and parens to make the intent clear: `FOO & (&&BAR)`.

## Labels

A label can be placed at the beginning of a statement. During assembly, the label is assigned the current value of the active location counter and serves as an instruction operand. There are two types of labels: global and local.

### Global

```k65
var SCREEN=0x400

func main {
  x=0 loop {
    a=hello,x if !z{ SCREEN,x=a x++ }
  } while !z

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
  if != goto .loop
}

func two {
  .loop:           // same name, different scope
  y++
  if != goto .loop
}
```

Local labels are used heavily for self-modifying code patterns:

```k65
func draw_level {
  .LV_TO_DRAW+1=a=p_current_lv .LV_TO_DRAW+2=a=p_current_lv+1

  .LT+1=.LD+1=a=&<screen_1+224 .RT+1=.RD+1=a=&<screen_1+225
  .LT+2=.RT+2=a=&>screen_1+0x100 .LD+2=.RD+2=a=&>screen_2+0x100

  y=[LV_SIZE] loop {
    .LV_TO_DRAW: a=levels,y

    x=a .LT: screen_1=x x++ .RT: screen_1+1=x
    a|0x20
    x=a .LD: screen_2=x x++ .RD: screen_2+1=x

    c+ a=.LT+1 a-2 .LT+1=.LD+1=a if !c{ .LT+2-- .RT+2-- .LD+2-- .RD+2-- } x=a x++ .RT+1=.RD+1=x

    y--
  } while !=
}
```

### Register widths at a label

A width statement written directly in front of a label, such as `@a8` on the
line before `.loop:`, is the label's **entry width**. The label is entered like
a function: every arrival is switched to that width where it leaves from. The
path that falls into the label runs the statement where it is written, and a
branch or `goto` arriving at another width is switched in front of the branch,
as a call is bridged in front of its `jsr`, so the code after a conditional
branch continues at the switched width too. Nothing runs at the label itself, so
a loop whose back edge is already at the width pays nothing per iteration. A label with no width statement in front of it is a merge
of whatever arrives. See `register-width-aware-syntax.md`, "A label's entry
width".

## Segment Selection

The 65816 has a linear 16MB address space. K816 does not divide it: `segment` emits a ca65 `.segment` directive and nothing more, and where a segment lands in memory is decided by the ld65 config the program is linked with. The active segment can be changed at any point in a file to place functions or data blocks within different segments.

```k65
segment my_segment      // select output segment
```

## Imports

A program is more than one file, and each file says which of the others it may see.

```k65
import "sprites.k816"
import "../shared/defs.k65"
```

The path is a string, resolved against the directory of the file that wrote it — the same rule `binary "file"` uses. There is no search path, no include directory and no implicit prelude: a compilation reads the file named, what that file names, and nothing else.

A unit sees what it imports **directly**, and nothing further. If `main` imports `lib` and `lib` imports `hw`, `main` cannot name `hw`'s symbols without importing it too. That is what makes a unit mean one thing: `lib.k816` resolves the same names whether it is the file being compiled or a file `main.k816` reached, and what `main.k816` happens to import is not `lib.k816`'s business.

The whole closure is still *read*, because a callee two imports away decides contracts this compilation is built against — so a change to it can change these bytes, and it is a dependency of the object even where it is not visible. `--create-dep` lists it.

Cycles are legal. Two units that name each other see each other, which is ordinary: a program is not a hierarchy.

Only the file being compiled is emitted. An imported unit contributes names, not bytes; it is compiled in its own right, on its own invocation, and the two objects are linked together.

```bash
k816 compile main.k816 -o main.s   # reads whatever main.k816 imports
k816 compile lib.k816  -o lib.s
```

An `import` may sit inside a `#if` region, and one the preprocessor drops is not a file the build reads — so it is not a dependency of the object either.

**What crosses, and when.** A name an imported unit defines is available wherever a local one would be, and a name this unit declares itself still answers only from the line after its declaration — an `import` adds names, it does not make this file's own order stop mattering. What an imported name *is worth* depends on the kind. A variable, a label, a data label or a function has an address, and ld65 decides it — so a reference to one is an `.import` in the emitted assembly and nothing this compilation can fold. A `const` is the other case: its value was settled where it was written, so it is a compile-time fact here too. It folds in an evaluator bracket, in an initializer of this unit's own declarations, and in an immediate alike, and it leaves no `.import` behind, exactly as a local `const` does. An `abstract var` crosses for the same reason and with the same result — `DEV:sizeof` and `DEV.field:offsetof` answer over an import, and nothing is emitted for it:

```k65
// defs.k65
const REG_Y = 1
const REG_X = REG_Y + 2
const FRAME = REG_X + 2

// main.k65
import "defs.k65"

const NAME_PTR = FRAME + 1      // $06 — folded here, not linked
var   buffer   = FRAME + $100   // an address is a compile-time value too
```

An `extern const` is not this case. It names a value *no* unit of this program defines, so ld65 is the one that knows it and every compile-time position refuses it (`K816-E0167`) — see "Extern declarations".

### What this replaces

The original K65 compiler resolved every name across a project's sources, so a K65 file declares nothing about where a name comes from; K816 reproduced that over whatever the command line named, with a `--unit` option. That option is gone. A build system that compiles one source per invocation cannot produce a sibling list, and a program that says what it is needs no such list — which is why `import` exists and why the option does not.

The cost is that an unmodified multi-unit K65 program does not compile: each of its files needs an `import` line, and `import` is a K816 extension, so a `--strict-k65` program is one file. `doc/k65-compat-manifest.md` records that under `k65-global-namespace`.

## Extern declarations

An `extern` names a symbol **no K816 unit defines**: a function cc65 compiled from C, a routine in a hand-written ca65 module, a variable the linker places, a constant an include file exports. ld65 resolves it, and the declaration is how K816 knows what to ask ld65 for.

```k65
extern func _puts                    // a C function: cc65 mangles `puts` to `_puts`
extern far func rom_entry            // reached by JSL, returning with RTL
extern func setup @a8 @i8 -> @a16    // and what it does to the register widths
extern var _count                    // absolute, the default
extern var dp scratch                // a zero-page cell: `.importzp`
extern var far rom : far             // a 24-bit address, and a 3-byte datum
extern const ROWS                    // a value an include file exports
extern const COLUMNS:byte            // and one exported zeropage: `.importzp`
```

Each form carries exactly what ld65 cannot infer and ca65 needs. The storage class decides the directive — `.importzp`, plain `.import`, or `.import NAME : far` — and ca65 validates addressing modes against it, so a `dp` variable declared absolute is a wrong *instruction* rather than a wrong name. `far func` decides `JSR`/`RTS` against `JSL`/`RTL`, checked at every call site with the same `K816-E0138` a local `far func` gets.

An extern reserves no storage, emits no equate, and produces no `.import` at all until something references it — an interface unit of nothing but declarations costs the objects that read it nothing.

### The contract is declared, not derived

Every other contract in this compiler is worked out from a body: `classify_exit` walks a section's reachable returns and reports what it finds, and a `->` clause on a `func` is *checked* against that (`K816-E0120`). An extern has no body, which is exactly what it is for — so the widths before `->` are the entry a `call` bridges to, the widths after are what the caller may believe, and **nothing verifies either**.

That is the price of naming something in another language, and it is the same price a C prototype for an assembly routine pays. A wrong `extern` is undetectable until the program runs.

Where no clause is written, the declaration says the callee leaves all three axes alone:

```k65
extern func quiet                    // preserves the accumulator, the index
                                     // registers, and the emulation flag
```

This is *stronger* than what a callee K816 cannot see at all gets — that one is assumed to preserve the two widths and may have moved the emulation flag. It is stated as a fact here because it is true of the foreign code that exists: cc65 generates 6502, which has no `REP`, no `SEP` and no `XCE`, so it cannot move any of the three. A foreign routine that does run one has to say so, and today it can only say so about the widths.

### `extern const`: a value the linker decides

ld65 does not distinguish a constant from an address — a symbol is a name, a link-time expression and an address size — so a ca65 include file can `.export ROWS = 25` and any object may `lda #ROWS`. That is what `extern const` names. (A C `#define` cannot: it never reaches the object file, and neither does an `enum` or a `const int`.)

An `extern const` has **no number**. Every use of it stays a symbolic expression the assembler hands to the linker, which is a path K816 already had for relocatable symbols:

```k65
extern const ROWS

func main @a8 @i8 {
    a = #ROWS            // lda #.lobyte(ROWS), and ld65 checks it fits
    rts
}

data table { ROWS }      // .byte .lobyte(ROWS), checked the same way
```

A use gives it the width it needs, as it gives any constant without a size ([A constant's size](#a-constants-size)), and only ld65 can check that the value fits. An `extern const` is the bits of the size it is imported at: two bytes for a plain `.import`, one for `extern const ROWS:byte`, which is an `.importzp`, three for `:far`. A use narrower than that takes the low bits — `.lobyte(ROWS)`, `.loword(ROWS)` — and is followed by an ld65 assertion that they are the whole value: the import's bits, read as a two's-complement pattern, must lie in −2^(n−1) … 2^n−1 for the use's *n* bits. So an imported `$FFFF` is −1 and is `$FF` in a byte, and 300 fails the link with a message that names the K816 line. Without the low byte ca65 refuses `lda #ROWS` at `.a8` against an absolute import, and without the assertion 300 would silently be `$2C`.

A use as wide as the import, or wider, takes the value as ld65 links it, zero-extended: ld65 reads a constant export as an unsigned number, so the sign of a negative one does not cross. A constant ca65 code exports with no size and a negative value is `long`, which no `.import` can name, and ld65 warns about the mismatch; the check reads the import's low bits all the same.

Every position that needs the number *now* refuses it with `K816-E0167`: a `#if` condition, an evaluator bracket, an array extent, an allocation count, a placement, and another `const`'s value. The refusal is its own code rather than the generic "must be known at compile time" a label's address earns, because the two have different answers — a label's address is ld65's and always will be, and a link-time constant is a number somebody could have written here instead.

It is imported absolute (`.import ROWS`), which is what an ordinary `const` in another K816 unit exports as when it fits `$0000` … `$FFFF` (`.export ROWS : abs = $0019`), so the two agree without an ld65 address-size warning. A declared size changes both sides alike: `const ROWS:byte` exports zeropage and `extern const ROWS:byte` imports it.

### Two declarations of one name

Identical ones are ordinary — that is how an interface unit imported by ten units works, and it is the reason to write one rather than repeating the declaration. Two that differ are `K816-E0165`, and the message names the axis: a mismatched address size would be no more than an ld65 *warning*, and a mismatched contract would be a silently wrong byte.

A name that one unit declares `extern` and any unit of the program *defines* is `K816-E0164`. The program contradicts its own interface file, and which of the two is right is not the compiler's to guess.

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
- A `->` clause names registers and the flags `c z n v`; anything it omits is undefined after the call.
- Register parameters use `a`, `x`, and `y`.
- `inline` functions may additionally declare immediate aliases (`#name`, `#name:byte`, `#name:word`) and bare identifier aliases for variables/addresses. See *Why aliases are inline-only* below — they are not available to `func`, and not available at any call site.
- `->` can declare output registers, exit-width checks, or both.
- Omitting the entire contract clause keeps the legacy unchecked behavior.

The `@a8`/`@a16` annotations control the accumulator width and `@i8`/`@i16` control index register width; the compiler synthesises [`REP`](#rep-reset-p-bits) or [`SEP`](#sep-set-p-bits) instructions as needed to satisfy these contracts.

The special name `main` designates the program entry point. Its register widths come from the **entry ABI** — what the startup code hands over, currently native mode with both axes 8-bit — rather than from an annotation: `main`'s caller is the environment, and an environment cannot be bridged. An annotation on `main` is therefore a claim about that ABI, true or `K816-E0143`, and no entry setup is generated for one. See `register-width-aware-syntax.md`, "The entry point".

```k65
func main {
  a=0        // set accumulator to 0
  loop {} always  // loop forever
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

A `naked` function may carry entry-mode annotations and an input parameter list, but may **not** declare an exit contract — no `-> ...` clause is permitted (`K816-E0131`), because control does not return to the caller implicitly. A bare call to a `naked` function resets the caller's tracked register widths; any code after the call is lowered as if at the start of a new function body and must re-establish whatever mode it needs via `@aX`/`@iX` annotations or explicit bridges.

The missing `RTS` is the *whole* of what `naked` means. Its exit widths are inferred from its reachable returns exactly as any other section's, falling off the end included — control does leave there, so that is a return site like any other.

### `inline`

User defined macro that is inlined in the code when used.

```k65
inline inc_y {
  y++
}
```

An `inline` has no address. Nothing is emitted where it is declared, nothing is exported, and a reference to its name anywhere but a use site is an error (`K816-E0115`) — there is no one place for such a reference to mean. For the same reason `far` may not be written on a use site: expansion is not a transfer, so there is nothing for the prefix to lengthen. (In the original the prefix meant a bank switch, and writing it on an `inline` is refused there too.)

An `inline` that expands into itself, directly or through a cycle, is `K816-E0132`. A copy has no base case the way a call does. Recursion through a non-`inline` callee is ordinary recursion and is fine.

**An `inline` is the one section kind that does not link, so it crosses a unit boundary by being copied.** Every other kind crosses as a *name*: `jsr helper` resolves because `helper` has an address for the linker to fill in. An `inline` has none, so a use site in another unit has nothing to link and nothing to expand. K816 therefore copies the declaration into the program of the unit that expands it, after the preprocessor and before any symbol is declared, and everything below that point treats it as an ordinary local `inline`. Three consequences worth knowing:

- **The body keeps its own unit's contract.** A body is classified once, against its section's own contract, and in its own unit that contract includes the file-top `@a8` above it. Expanding it into an `@a16` unit does not resize an immediate on an axis that contract states. An axis the body's own unit leaves unspecified gets no mode bridge at the use site, and the body runs at whatever width the use site has there.
- **Its brackets read its own unit's compile-time values.** A `const` folds, so its value is a fact wherever it was written; the workspace's `const` values are visible to an evaluator expression the way another unit's data sections are, consulted only after this unit's own declarations. A name assigned by a top-level `[ … ]` block or a `#if` condition is a `const` by the time it crosses, so those answer too.
- **A body the parser could not read is never expanded.** Recovery produces a *shorter* body, not a broken one, so expanding it would put the compiler's guess in your object with nothing said. Such a section reports `K816-E0156` where it is used, and the parse errors stay in the unit that wrote it.
- **The names the body reads become the expanding unit's imports**, because that is where the operations land — so the expanding unit has to see each of them, a variable's field list included: `slot.hi` in a borrowed body resolves wherever `slot` does.
- **The body's own mistakes are reported once, by the unit that wrote it.** Repeating them in every unit that expands the section would say one thing many times. What the expanding unit reports instead is `K816-E0156`, naming the section and the unit to go and read; it is an error rather than a note, because otherwise the unit would emit an expansion of a body nothing checked. A complaint that only *this* unit can make — the layout case above — is reported here in full.

Labels inside an expanded body do not keep their names — a second expansion would define them again — so each expansion gets targets of its own. That is also why a `goto` may not name a label of another section (`K816-E0129`): from outside there is nothing to name.

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

A `->` clause may name **flags** as well as registers. The four a callee can hand back are `c`, `z`, `n`, and `v` — the ones a body computes and a branch tests:

```k65
func check @a8 @i8 (a) -> a, c {   // the carry is a result
  a ? #10
}

func caller @a8 @i8 {
  check a -> a, c
  if !c { ... }                   // reading an answer
}
```

`i` and `d` are refused with `K816-E0159`: they are modes the *caller* establishes before the call, so a clause naming one would be a promise nothing could check.

**An output the clause does not list is undefined after the call.** This applies only to a function that *has* a `->` clause — a contract-less function is untouched, which is every K65 function, since K65 has no contracts. Writing the clause is what makes the promise; anything outside it is damage.

Damage tracking for contract-bearing bare calls is **block-local**: after every such call the compiler scans the remaining statements in the same basic block and reports registers and flags that are live past the call but clobbered by the callee. So the same body warns or does not depending on what its clause claims — drop `c` from `check`'s clause above and the `c-?` becomes `K816-W0123`, because a compare writes `N`, `Z` and `C` and the clause promised none of them. It does not perform inter-procedural or whole-program dataflow — split work into smaller functions if you need the check to see across block boundaries. Use `call foo` to opt out of the check entirely.

Exit-width annotations in a `->` clause are **check-only**. They describe the widths the function body must actually have at every reachable return; they do not force the caller into that mode by declaration alone. If no exit-width annotation is present, a checked bare call adopts the callee's inferred exit mode from its reachable returns. An inferred exit mode of `Unknown` (callee not yet parsed, cross-unit reference, or body variants that are neither uniformly `Fixed` nor uniformly `Preserve`) is treated at the call site as `Preserve` — the caller's tracked widths are left unchanged. See *Unknown exit contracts preserve caller mode* in [register-width-aware-syntax.md](register-width-aware-syntax.md).

An `inline` body's labels are renamed at each use site — they become the
enclosing function's unnamed targets — and two rules follow from that:

- **Its labels must be local.** A unit-level label inside an `inline` would be
  a name the whole unit can see with no definition left to point at, and two
  use sites would ask for two of it: `K816-E0102`. The rule covers every
  unit-level name the body can declare, not only a `name:` statement — a
  `data NAME { … }` block and a `label:` entry inside a data block are the
  same name with the same problem, and get the same code. An *anonymous*
  `data { … }` is the shape that works: nothing is named, so nothing is
  exported, and each expansion carries its own copy of the bytes with a jump
  over them.
- **They can be branched to, not taken the address of.** `bra .spot`,
  `goto .spot`, and `lda .spot` all name the label as a *target* and are
  rewritten; `lda #.spot` asks for its address as a number, and there is no
  name left for the expression to mention: `K816-E0115`. Put the label outside
  the `inline`, or make the section a `func`, which has one address.

### Why aliases are inline-only

K65 has no general parameter-passing mechanism. A `func` is reached by `JSR` (or `JSL`), and the only things that survive that jump are the machine's own registers and whatever the caller left in memory. There is no stack frame, no argument area, and no convention for passing a value the compiler makes up. So a `func` contract can name only what the hardware can actually carry across a call: `a`, `x`, and `y`.

An `inline` is different in kind. It is not called at all — its body is expanded at the use site, before any instruction is selected. That expansion is the one place where an arbitrary parameter *can* be injected, because the parameter never has to travel anywhere: the compiler substitutes it into the body as it emits it. That is why, and only why, an `inline` may declare immediate aliases (`#factor`) and bare identifier aliases for variables and addresses:

```k65
inline scale @a16 (a, #factor) -> a {
  lda #factor          // `factor` is substituted during expansion
}

inline store_to (a, target) -> a {
  target = a           // `target` names a variable at the use site
}
```

Two consequences follow, and both are deliberate:

- **`func` may not declare aliases.** There would be nowhere to put the value. A `func` that needs a compile-time constant should take it in a register, read it from a `var`, or be an `inline`.
- **A call site never passes an alias.** Call-site arguments are registers and immediates only — `add a, x -> a, y`, `scale a, #16 -> a`. An alias is bound during expansion, not handed over at a call, so there is no call-site syntax for one. This also keeps two consecutive bare calls unambiguous: with bare identifiers accepted as arguments, `inc_x` followed by `inc_y` would read as one call passing the other's name.

#### How an alias is bound

Aliases bind **positionally**, against the use site's argument list: the alias in the *n*th parameter slot takes the *n*th argument. A register slot wants that register named; an alias slot — either kind — wants an immediate.

```k65
inline scale @a16 (a, #factor) -> a {
  lda #factor
}

inline store_to @a8 (a, target) -> a {
  target = a
}

var cursor = $10
var mem    = $0400

func use @a16 @i8 {
  scale a, #16 -> a         // lda #$0010
}

func place @a8 @i8 {
  store_to a, #mem -> a     // sta a:mem
  store_to a, #cursor -> a  // sta z:cursor
}
```

A parameter's name is an identifier, always. `#3` is not a parameter — a number in that position is refused, because there would be nothing to bind and nothing in the body to substitute for. `#3` is *argument* syntax, and the two positions are what the paragraph below separates.

**The narrowing is a range.** `:byte` and `:word` are the two an immediate parameter takes, and they promise that every argument fits — `0 .. $FF` and `0 .. $FFFF`. The promise is kept at each use site, where the argument is, and a value that overruns it is `K816-E0170` with the caret on the argument rather than the declaration. Only an argument that folds is checked: a link-time symbol has no compile-time value to compare, so `scale a, #<SYMBOL> -> a` passes a narrowed parameter untouched and ld65 decides the number.

There is no third narrowing. `:far` shares the spelling a variable's data width uses, where it does mean something, and on an immediate parameter it is `K816-E0169`: the narrowing is a range an *immediate* must fit, and an immediate is carried by the accumulator — eight or sixteen bits, never twenty-four.

**An alias may be a repetition count.** `lsr * n` in a body is as many `LSR`s as the argument says, and the count is folded at each expansion rather than at the declaration — where there is nothing bound to fold. So an `inline` that names a parameter in a count is not itself an error, and an argument that has no compile-time value is refused at the use site that passed it:

```k65
inline shift (a, #n) -> a {
  lsr * n
}

func work @a8 @i8 {
  a = #$80
  shift a, #3 -> a          // lsr / lsr / lsr
}
```

The `#` in argument position is *argument syntax*, not a claim about what the substituted expression is used as: it is the only way to write a non-register argument, and it is what keeps `inc_x` on one line from reading as an argument to a call on the line above. Whether the alias stands for a value or a place is decided by how it was *declared* — `#factor` is a value and may only be read, `target` is a place and may be stored to.

What follows from binding at expansion rather than at a call:

- **The body is classified once, against the `inline`'s own contract**, on every axis that contract states. `lda #factor` in an `@a16` section is a three-byte instruction at every use site, and the use site is bridged to `@a16` to make that true. **On an axis the `inline` does not state, the body takes the use site's width**, because it is expanded in place: `inline add (#n) { clc adc #n }` is `adc #$04` in an `@a8` function and `adc #$0004` in an `@a16` one, a width block inside it restores to whatever the use site had, and an immediate that cannot be sized is `K816-E0104` at a use site with no width on that axis either, with the body's line as the secondary.
- **An argument's own declaration decides the encoding.** `store_to a, #cursor -> a` narrows to `z:` because `cursor` is a direct-page variable, exactly as a hand-written `cursor = a` would.
- **An argument that does not fit is reported where it was written.** The body is correct on its own terms; the argument is what makes the complaint true, so the caret lands on the use site and the parameter it was bound to is shown as a secondary.

### `else`

Creates a jump to a label outside the `else` bracket in branch code.

```k65
inline check_if_key_or_doors {
  if a?60 == {gamestate_keys++}
  else {
    if a?62 == {
        ptr_doors=a=x
        temp1=a=1
    }
  }
}
```

If-else form:

```k65
if =={
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

K816 validates calling convention consistency, within a unit and across units alike. A foreign section's declaration is read from the units the compiler is given, so a call to one is checked exactly like a call to a section next door:

- A near `call` to a `far func` is `K816-E0138`.
- A `call far` to a regular (near) `func` is `K816-E0138`.
- Register width mismatches (caller's A/I width differs from callee's declared `@a8`/`@a16`/`@i8`/`@i16` contract) are bridged where K816 can bridge them and reported where it cannot; see [Register Width Contracts](register-width-aware-syntax.md).

The check is the frontend's rather than the linker's because that is where the callee's declaration is still in hand: ld65 sees an address and a name, not a convention. The mismatch is worth an error rather than a silent correction because it is a stack-depth bug — `JSR` pushes two bytes and the callee's `RTL` pops three, so control returns to an address that was never a return address.

### Jumping without calling

A `goto` transfers without pushing a return address. Its target may be a label of the same function, a function's own name — the tail jump the [`naked`](#naked) example uses — or, in the indirect form, an address held in memory:

```k65
func dispatch @a8 @i8 {
  goto .direct          // JMP to a label of this function
.direct:
  goto (vec)            // JMP (vec) — the destination is read from memory
  goto far handler      // JML, carrying a bank byte
}
```

The direct form's length follows the target the same way every other address operand does: a `goto` naming a `far func` is a `JML` whether or not `far` is written, and writing `far` forces the long form for a target that would otherwise be near.

**`far goto label` and `goto far label` are the same jump.** The first is the original K65 word order (`Instr -> far goto Arg`) and the second is K816's; both lower to `JML`, and both accept a unit-level label, a local `.name`, or a parenthesized indirect target.

A `goto` may not name a label of *another* function (`K816-E0129`). Arriving in the middle of a function means arriving with register widths that function's contract never promised, which is not a shape the width model can describe — call the function instead.

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
  loop { bcol++ } always
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
```

`nocross` (or the wrapper form `nocross data { ... }`) guards the leading
run of statically-sizeable data entries, so the whole run lands inside one
256-byte page. A run larger than a page is rejected at compile time — it can never fit
however it is placed — and where it lands is checked by ld65, as an assertion
the emitted assembly carries. K816 does not pad before the run to make it
fit: the original's linker did that, and placement belongs to the linker, which
here is ld65 and its configuration. Enforcement covers data runs
(`byte`/`word`/`far` values, strings, `for … eval`); it stops at a `code`,
`address`, `align`, or `charset` entry, and `nocross` around a
`code { ... }` subblock stays advisory (instruction sizes are not known
until placement).

There is a third spelling, which bounds the run with braces instead of
leaving it to the first entry that has no static size:

```k65
data display_list {
  0x70
  nocross { 0x4F 0x00 0x40 }   // these three land in one page
  0x41
}
```

`nocross { ... }` guards exactly the group it wraps. The entries emit where
they stand — the braces move no byte — and the group's guard covers the
leading run of statically-sizeable entries *it* holds, by the same rule the
bare directive uses. Groups nest.

`nocross` takes **no operand**. The older `nocross EXPR` form, which sized
one chunk rather than the run, has been withdrawn: K65 is
newline-insensitive, so `nocross` followed by a value can be read either
way, and the reading that took the value swallowed the first byte of the
run. Nothing in the K65 corpus or examples used the sized form, and
everything that uses `nocross` at all writes it bare. A source that still
carries `nocross 8` now reads as `nocross` followed by a data value `8`,
which emits one extra byte — the two spellings cannot be told apart, so
this is a migration to check by eye rather than one the compiler can flag.

```k65

data MyData4 {
  address 0x5000        // fixed memory address
  1 2 3 4 5 6 7 8
}
```

#### `align N + M` — a boundary and an offset

`align` takes an optional offset, and the `+` is **not** the expression
operator: `align 16 + 4` asks for an address congruent to 4 modulo 16, not
for `align 20`. It lowers to `.align 16` followed by `.res 4`, and the
section's label comes after the pair, so the label names the congruent
address rather than the boundary.

```k65
data offset_table {
  align 16 + 4          // offset_table lands at $..04, $..14, $..24, …
  1 2 3
}
```

The boundary must be a power of two in `1..65536`, and the offset must be
less than the boundary — otherwise the directive names a congruence class no
address is in, and the compiler reports `K816-E0144`.

`.res` emits the offset bytes as fill. The original K65 compiler recorded the
congruence for its own linker and left a gap instead; the bytes are what ld65
fills that region with (`fillval` in your config).

An `.align` inside a segment is a request ld65 checks against the *segment's*
own alignment, not against the address it happens to start at. A config whose
segment says nothing gets a warning even when the start address already
satisfies the request, so a config hosting aligned sections should declare it:

```
SEGMENTS {
    CODE: load = MAIN, type = ro, align = $100;
}
```

#### Placement directives on a code section

`align` and `address` also open a **code** section — a `func`, `naked`, or
`main` — where they place the whole section:

```k65
func handler @a8 @i8 {
  align 256 + 8         // handler lands at $..08, $..08 + $100, …
  a = 1
  return
}
```

There they are the section's **first statement**, and only that: a placement
directive further down would have to move bytes that are already placed, and
the compiler reports `K816-E0111` naming the rule. Data blocks are the
exception — a directive may sit between entries there, and applies from that
point on.

An `inline` may not carry one at all. It has no address of its own and its
body is copied into each use site, so there is no section for a directive to
move; place the section the body expands into instead.

An `address` on a code section puts it in a segment of its own and restores
whatever segment was in force afterwards, so the placement is that section's
and not every section's after it.

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

A typed entry takes **every value that follows it**, not only the ones on its line. K65 is newline-insensitive, so `byte 1 2 3` and `byte 1 2` with `3` on the next line are the same token sequence and emit the same bytes; a line break — or a comment — cannot change what a value means. The entry ends at the first thing that is not a value: another type keyword, a label, a directive, or the block's closing brace.

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

`nocross code { }` asks the page-fit question the statement form asks, about the subblock's own bytes.

A subblock of a **top-level** data block is a body without a function: no entry contract, no caller, and no scope of its own. Two consequences follow, and both are what "no function" means rather than restrictions added on top:

- **Nothing says what the register widths are** where control reaches it, so they start unknown and a width-dependent immediate is `K816-E0104` until the subblock says. A width block is how it says: `code { @a8 { a = 1 } }`.
- **A local label has nowhere to live** (`K816-E0101`), because local labels are scoped to a function. Use a unit-level label, or move the code into a `func`.

A subblock of an **inline** data block — one written inside a function — has neither problem: it is ordinary code of that function, tracked in the same width model as the statements around it, so its immediates are sized by the contract already in force.

### `repeat`

Repeats data within a data block:

```k65
data repeated {
  repeat 3 { 4 }       // emits: 4 4 4
}
```

The body is a data block, so typed entries, strings, and nested `repeat`s all work inside one, and a nested count multiplies rather than replaces. Two kinds of entry are refused there, for the same reason: each copy would need its own answer.

- A **label** — `K816-E0102`. A label names one address, and `repeat 3` would give it three. Put it before the `repeat`, where it names the first copy.
- A **placement directive** — `segment`, `align`, `address`, `nocross`: `K816-E0111`. Placement is a property of where the block's bytes land, and a repetition has one answer to give per copy.

A `code { }` subblock is refused for the label reason: its labels would be defined once per copy.

### `charset`

Defines character mapping for subsequent string data. A charset string is an **index table**: the byte a character emits is its position in that string, so `charset ".ABC"` sends `.` to 0 and `C` to 3, and a character listed twice takes its first slot — which is what the padded form `".ABCDEFGHIJKLMNOPQRSTUVWXYZ..... "` is for.

It is compile-time only: it emits no bytes of its own, has no effect on non-string entries, and there is no runtime translation. A character the table does not list has no byte to emit, and inventing one — the character's own value, say — would put a byte in the output that the declared mapping says nothing about: `K816-E0142`.

The mapping is in force for every string *after* it in the same block. It does not reach the next block, and one written inside a `repeat` body is scoped to that body — each copy starts from the charset in force where the `repeat` is, so a mapping cannot leak from one copy into the next or out past the last.


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

The range is inclusive at both ends, so `0..4` is five values and `7..7` is one. Each value the body folds to is rounded to the nearest integer and emits **one byte**, and it has to fit one — by the rule every use has, −128 … 255 as its low eight bits ([A constant's size](#a-constants-size)), so `[i - 2]` from zero is `$FE $FF $00 …`: a body that leaves the range says so (`K816-E0109`) rather than emitting a byte nobody wrote. Mask or clamp in the expression — `& 255`, `?< 255`.

The body is the whole evaluator language, not a subset of it: floating point, the function library, `pi`, statements, and calls to an `evalfunc` all work, which is what the `sin` examples above depend on. What it sees is the loop variable, every `const`, and every name an earlier evaluator bracket assigned; a name none of those gives a value is `K816-E0101`.

The loop variable is scoped to the loop, and so is anything the body assigns — the body runs against the environment as it stood where the `for` was written, and does not change it. That is one deliberate difference from the original, where a `for … eval` body's assignments outlive the loop; it exists because the bounds are ordinary K65 expressions and are not known where every other bracket is evaluated.

### `image`

**Removed.** `image` in every spelling — inside a data block, at the top
level, and the `tiles` / `colormode` / `imgwave` / `inv` modifiers — reports
`K816-K65-0001` and compiles nothing. Reading a bitmap and scanning it into
tiles is an asset pipeline, and K816 does not own one (`doc/design.md` §1).
Convert the image outside the build and include the bytes with
[`binary`](#binary); no adapter tool ships with the compiler.

The construct is still *recognized* — it is a named node with a diagnostic of
its own rather than a parse error — so a K65 source says which line has to
change. The form the original accepted:

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

The file may also be named with a bare identifier, and then the identifier's
own text *is* the path — nothing is appended, so `binary table` includes a
file called exactly `table`:

```k65
data blob {
  binary table
}
```

The bytes are section bytes like any others: they follow the entries above
them, the section's [`evaluator [ ]`](#section-transforms) transform reaches them,
and [`index` and `size`](#reading-a-sections-bytes) read them.

**Where the file is looked for.** K816 emits `.incbin "file"` with the path
exactly as the source wrote it, and separately checks at compile time that the
file exists **relative to the directory of the source that named it**. A
missing file is `K816-E0153` with the path underlined, rather than an
assembler message with no line. ca65 resolves the same name against its own
search list when it assembles, so a build that runs ca65 from somewhere else
passes `--bin-include-dir`; the driver does that.

K816 normally leaves the file to ca65 and never opens it. It reads the bytes
in exactly two cases, because in both the answer is needed at compile time:
the section has an `evaluator [ ]` transform installed, or an evaluation
indexes the section.

### Section transforms

`evaluator [ code ]` installs a per-byte transform on the **section**. Every
byte the section *generates* below that line is passed through it as `x`, and
the value the bracket answers with replaces it:

```k65
data ramp {
  evaluator [ x * 2 + 1 ]
  for i = 0 .. 3 eval [ i ]      // 01 03 05 07
}
```

Four rules, all of them the original's and all measured
(`tests/compat/k65/evaluator-transform/`):

- **It reaches generated bytes only.** A `for … eval` value, a character of a
  string through [`charset`](#charset), and every byte of a
  [`binary`](#binary) go through it. A *literal* data entry does not — `1 2 3`
  under the transform above is still `01 02 03` — and neither does a plain
  evaluator bracket entry, a `?`, or an address-byte operator.
- **It is positional.** The entries above the line are already emitted and
  nothing goes back for them.
- **A second one replaces the first.** The transforms do not compose; the
  section holds one program.
- **The answer is masked to eight bits by truncation**, not by the rounding
  every other integer boundary in the evaluator uses. `[0 - 1]` through a
  transform is `$FF`, and `[x + 0.6]` over `1` is `1`, not `2`.

The transform runs against the evaluator environment as of its own line, in a
copy per byte, so a name it assigns does not outlive it — the same rule a
[`for … eval`](#for--eval) body runs under, and for the same reason: both run
while a section is being assembled, which happens more than once.

### Reading a section's bytes

`index(sec, i)` answers with byte *i* of a data section. `sec[i]` and
`sec.index(i)` are the same call written differently, and `size(sec)` is the
number of bytes the section holds:

```k65
data ramp { 10 20 30 40 }

data reads {
  [ index(ramp, 2) ]      // 30
  [ ramp[1] ]             // 20
  [ ramp.index(3) ]       // 40
  [ size(ramp) ]          // 4
}
```

The bytes are the ones the section *assembled to*, starting at its own label:
after its [`evaluator [ ]`](#section-transforms) transform, with a `?`
counting as one zero byte, and *without* any padding a leading `align` put in
front of the label. An index outside the section answers zero with no
diagnostic, and the index takes the same truncating conversion a bitwise
operand takes — so `index(ramp, 1.7)` is `ramp[1]`, not `ramp[2]`.

A section can be read from the line after it closes, **and from another unit
of the workspace**: a project is one namespace for section bytes as well as
for section names. What does not cross a unit boundary is an *address* — that
is the linker's, and no evaluation can fold one.

Five shapes have no answer, and `K816-E0154` says which:

| what was named | why there is no answer |
| --- | --- |
| a number or a string | `index` and `size` take the name a `data` block declares |
| a `func` / `inline` / `main` | an instruction's bytes are decided by width analysis, the optimizer and ca65, all of which run after every evaluation |
| a section written below the read | a section's bytes are known from the line after it closes |
| a section holding an address, or an `align` after its label | the value is the linker's, so the section has no compile-time bytes |
| a label written inside a section | only the name on a `data` block names a section |

The two-dimensional `index(sec, x, y)` parses, and on a section it always
reports `K816-E0155`: its width and height came from a top-level `image`,
which is [removed](#image). Index the row arithmetically instead —
`sec[y * WIDTH + x]`. If the base is not a section at all, that is the
complaint you get instead, from the table above.

### Top-Level `image` and `binary`

`binary` can also be declared at top level, outside any data block:

```k65
binary Blob = "payload.bin"
```

The declaration **is** the section it describes: `Blob` is a data label
exactly as `data Blob { binary "payload.bin" }` would declare it, with the
same bytes and the same reads. (The original builds the section too, and then
its linker cannot resolve the label — `binary-top-level` in the compatibility
corpus is that measurement.)

Top-level `image` is removed with the rest of [`image`](#image).

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

Top-level evaluator blocks execute in source order, and so does every other bracket: a `const` group's entries are a sequence, so `const A = 3, B = [A + 1]` reads `A`. A name assigned in one block is available to later code — as an evaluator value *and* as a compiler constant K65 syntax can read — but assigning that same name again in a different block is rejected (`K816-E0149`). Within one block a name may be assigned as often as it likes, which is what makes a counted loop expressible:

```k65
[ n = 0  while n < 5 { n = n + 1 } ]
```

The original K65 compiler allows the reassignment; K816 does not, so a program written as two blocks needs them joined as above. The rule exists so that what a name means does not depend on how far down the file the reader has got, which matters more here than elsewhere because an evaluator name is also a constant the rest of the language reads.

Assigning to a name the compiler already owns — a `const`, a `var`, a label — is the same error.

### Statements

A bracket holds a *run* of statements, not a single expression, and its value is the last one's. Statements are juxtaposed or separated by `;`.

```k65
[ FLAGS = 0  if WIDE FLAGS = FLAGS | 1  if TALL FLAGS = FLAGS | 2 ]
```

| Form | Value |
| :--- | :--- |
| `expr` | the expression's |
| `{ … }` | the last statement's |
| `if C S` | `S`'s when `C` is true, and **0** when it is not |
| `if C S else S2` | the taken branch's |
| `while C S` | the last iteration's, and **0** if the body never ran |

An evaluation is bounded by a step budget; a `while` whose condition never goes false is `K816-E0147` rather than a compiler that does not return.

### `evalfunc`

```k65
evalfunc dbl(q) [ q * 2 ]
evalfunc lerp(a, b, t) { a + (b - a) * t }
```

A function is identified by its **name and argument count** together, so `f(a)` and `f(a, b)` are two functions; redeclaring either, or shadowing a library function at the same count, is an error. Every name in the library is protected, `index` and `size` among them.

A body's **calls are resolved where the body is written**, so a function it uses must be declared above it, and the error lands on the definition rather than on a call site. Its **names** are not: those resolve when the function runs.

A parameter is visible inside the function that declares it and nowhere else. A callee does not see its caller's parameters — there is no scope chain — but global evaluator names and `const`s are visible everywhere.

Recursion works, and is bounded by a call-depth limit (`K816-E0147`) separate from the step budget. The original's `evalfunc` has no frame — its parameters are slots allocated once — so a recursive one there returns a different function's answer or ends the build; no working K65 program contains one.

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

Five notes on what these compute, each of which differs from what the table alone suggests:

- **`%` is a non-negative modulo** — `fmod(fmod(a, b) + b, b)` — so `(0 - 7) % 3` is `2`, not `-1`.
- **`/` is float division.** `[7 / 2]` is `3.5`, which becomes `4` at an integer boundary.
- **`&&` and `\|\|` do not short-circuit.** Both operands are evaluated, and so are all three arms of `?:`.
- **`x op= e` reads `x` before it evaluates `e`.** It expands to `x op e` in that order, so `[X = 1, X += (X = 5), X]` is `6`.
- **The bitwise operators and the shifts truncate their operands toward zero**, which is *not* the rounding an integer boundary does: `[(0 - 2.7) \| 0]` is `-2` while `[0 - 2.7]` in a data entry is `-3`.

`\|\|` is a K816 extension: the original spells both logical operators `&&`, so a source containing `\|\|` does not compile there.

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
`index(sec, x)`|A byte of a data section — see below
`index(sec, x, y)`|A cell of a two-dimensional section — see below
`print(msg)`|Reports `msg` as a compile-time note (`K816-N0145`)
`error(msg)`|Reports `msg` as a compile-time error (`K816-E0146`)
`size(sec)`|How many bytes a data section holds — see below
`addbyte(sec, b)`|Refused: K816 does not let an evaluation change a section's contents
`color(r, g, b)`|Refused: palette conversion is not the compiler's
`color(x)`|Refused: palette conversion is not the compiler's
`rnd()`|Refused: a build whose output differs from itself cannot be compared with itself. Generate the table outside the build and include it with `binary`

`pi` is a value rather than a function, and it is the literal `3.141593` — six decimals, matching the original — not the machine constant. A table generated from a more accurate one has different bytes.

A number splices into a string with `+`, formatted as `%lf`, so `print("n=" + 8)` says `n=8.000000`. Strings support `+`, `==`, `!=`, `<`, `<=`, `>` and `>=`; a string that reaches an integer context is `K816-E0148`, because a string is a value *inside* an evaluation and never the value *of* one.

`abs` is a K816 extension: the original's library does not have it, and calling it there ends the build.

**`index` and `size` read a data section's assembled bytes**, and [Reading a section's bytes](#reading-a-sections-bytes) is where the rules are: `sec[i]` is exactly `index(sec, i)`, out of range is zero, the section has to be written above the read, and five shapes have no answer at all. For a variable or a field, `NAME:sizeof` answers a different size question.

### Member access and method calls

`x.f` is `f(x)` and `x.f(a)` is `f(x, a)`. That is the whole feature: there is no method table and no object model, and the dot must follow the value with no space between, so a call on a literal needs parentheses — `(2.75).frac()`.

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

### A number is a value

In HLA a number is always a value and never a place: `a = $10` loads `#$10`, `a + 1` adds one, and `x?5` compares with five. Memory is reached through a named variable — `a = flags`, `flags,x = a`, `ptr++`, `a = (ptr),y` — so in a `.kk` or `.k816` source a number in any memory shape is refused, `K816-E0176`:

```k65
var flags = $10

func work @a8 @i8 {
  a = flags,x           // a named place
  a = 3,s               // a displacement from S, which no variable can name
  a = &2,x              // an offset from the address in X, said with `&`
  lda $10,x             // a native mnemonic takes an address as written
}
```

`a = $10,x`, `$10 = a`, `$10++` and `a = ($10),y` would use one number as two things a line apart, so each is refused, with the native mnemonic as the fix where the statement is one instruction. A native mnemonic is the escape hatch: it takes an address as written, exactly as ca65 does. A `.k65` source keeps the reading these shapes had before the rule.

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

The LHS must name a declared place: a variable identifier, optionally indexed (`,x` / `,y`) or accessed (`TASKS.state`). Arithmetic over one is not a name — `mem+1 = 0` needs a register, as does a bare address literal like `$10 = 0`, which is not a shortcut for `stz $10`. The RHS must fold to literal zero at parse time — non-zero constants are rejected with *"non-zero constant stores must go through a register: use 'mem = a = value'"*. Addressing modes outside `STZ`'s support (`(mem),y`, `[mem]`, stack-relative, etc.) are rejected at lowering time.

For chains that genuinely do want the accumulator loaded too (e.g. storing the same zero to several cells and leaving `A = 0` for a subsequent operation), keep the traditional `mem1 = mem2 = a = 0` form — the compiler's peephole rewrites each supported `STA` to `STZ` and drops the leading `LDA #0` when `A` is dead afterwards.

The same holds for the index registers, written either way: `ldx #0` then `stx mem`, or `mem = x = 0`, becomes `STZ` where X is known zero, and likewise for Y. An index store has one more condition, because `STZ` stores as many bytes as the accumulator is wide and `STX`/`STY` as many as the index registers are: the two widths have to be known and equal. Under `@a16 @i8` an `stx` stays an `stx`. `STX` has no `STZ` counterpart for `mem,y`, and `sty mem,x` becomes `stz mem,x`, which still reads X — so a zero `X` that indexes a store is not dead, and its load stays.

Both halves are decided per *operation*. `STZ` has four addressing modes and `STA` has fourteen, so a chain that mixes them gets `STZ` where one exists and keeps `STA` where it does not — and the load stays whenever any store still reads the accumulator. The load also **stays at the end of a function**: everything is live where a function returns, the accumulator and the `N`/`Z` it wrote included, and nothing in a call contract could have said otherwise. So the rewrite completes less often at the end of a body than the sentence above suggests, and `mem = 0` on its own — which needs no accumulator at all — is the form to reach for there.

## Standard 6502 Instructions

The per-instruction tables in this section document the classical 6502 spellings and the matching K65 HLA shorthand forms. k816 also accepts the 65816 native mnemonic modes listed in [65816 Addressing Modes in K65](#65816-addressing-modes-in-k65), and the exhaustive compiler coverage is kept in `tests/isa.rs`, which emits every legal instruction-and-mode pair the ISA table claims and assembles the result with ca65 itself.

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

  **Both take a number, not an address.** The operand is one byte counted from the stack pointer — a position in a frame — so a literal or a `const` is what belongs there and the const rule below is inverted: `const nameptr = $12` with `lda nameptr,s` assembles to `A3 12`, while a `var`, a label or an `extern const` in the same place is `K816-E0168` (`K816-E0167` for the extern). The refusal is an error rather than a warning because the alternative is silent: K816 does not choose addresses, so a `var` would reach ca65 as a symbol, ld65 would resolve it, and the frame would be indexed by the low byte of an address nobody picked.
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

Operand rule for **raw mnemonics**: `#expr` is immediate, while an unprefixed value operand (`lda 0`, `lda addr`, `lda expr`) is an address-mode operand. Bare `const` names and const-rooted expressions are rejected in address position because `const` names values, not memory locations — the two stack-relative forms above being the exception, since their operand is an offset rather than an address; use `#CONST` for an immediate, declare a `var`/label for memory access, or attach an address-encoding prefix (`lda dp 0`, `lda abs 0`, `lda far 0`) when a numeric literal should force a specific address width. The address-of operators `&&label` and `&&&label` are the intentional exception: they produce immediate relocations even without `#`.

**A raw instruction ends at the end of its line.** K65 is newline-insensitive and raw mnemonics are K816's, and a mnemonic may stand alone — so with line breaks invisible, `pla` with a bare call on the next line read as `pla` taking the call's name as its operand, and `stz lock` above one read `lock` as an address-encoding prefix. Neither the operand, an encoding prefix's value, nor a repeat's `* N` is taken from the next line; a line comment ends the line as a newline does, and what is written on one line parses as it always did:

```k65
var lock = $10

func epilogue {
    nop
}

func work @a8 @i8 {
    pla
    epilogue        // a call, not an operand of `pla`
    stz lock
    epilogue        // a call, not `stz` under a prefix named `lock`
    nop
    * 3             // three NOPs of its own: this is not `nop`'s count
}
```

A **section's own name** is not an address operand either (`K816-E0115`). `lda target` where `target` is a `func` or a `naked` section is refused, and the reason is the address rather than the load: a body is an operation stream the optimizer rewrites — a mode operation folded, a redundant load coalesced, a dead store dropped — so the offset of its first byte is not one the compiler can promise to keep, and `--no-opt` alone would move it. To read a code byte, put a **label** in the body and load through that; the label is the position the author placed, and it is what survives a rewritten body:

```k65
var slot = $80

func target @a8 @i8 {
patch:
  slot = a
}

func main @a8 @i8 {
  lda patch                 // the byte at the label
  slot = a
}
```

`call target` calls it and `&&target` takes its address as a value; neither reads the byte, which is why the diagnostic names all three. An `inline` name is refused in the same position for a different reason and with its own message — it is copied into each use site and has no one address at all. The original K65 compiler accepts `a = target` as an absolute load, so this is a deliberate narrowing, recorded as `k816-function-name-is-not-a-place` in `doc/k65-compat-manifest.md`.

### Registers

**One rule names them all: a single letter is a flag, and a register that is not `a x y s` is spelled by its datasheet abbreviation.** `a`, `x`, `y`, and `s` are the four the K65 grammar itself names; every other register the 65816 has and the 6502 did not gets a longer name, so no spelling has to be read two ways.

- `A` accumulator (8/16 bit) — one name at both widths. The transfers that move all sixteen bits whatever `M` says (`TCD`, `TDC`, `TCS`, `TSC`) are spelled with it too: what they move is the register.
- `B` high byte of the accumulator (8 bit) — an `a~b` operand and nothing else, because `XBA` swaps one register's halves rather than copying between two. Not a transfer operand, not a contract output, not a stack target.
- `X` index register X (8/16 bit)
- `Y` index register Y (8/16 bit)
- `DPR` direct page register (16 bit) — `dpr = a` is `TCD`, `push dpr` is `PHD`. Spelled `dpr` and not `dp`, because `dp` is the storage-class prefix in `var dp counter`.
- `DBR` data bank register (8 bit) — `push dbr` / `pull dbr` are `PHB` / `PLB`.
- `S` stack pointer (16 bit)
- `PC` program counter (16 bit; CPU architectural register, not an HLA assignment target)
- `PBR` program bank register — **no HLA name.** No instruction loads, stores, or transfers it: only `PHK` names it as a source, and there is no `PLK`. Which bank code runs in is decided by a section's `far` declaration and by the linker, not by a program writing the register, so `phk` is the whole of the surface.
- `P` processor status register **NVMXDIZC** (8 bit; stack shorthand target as `p` — **not** as an individual flag name, see below). One spelling: `flag` was an alias for it and is gone, because a second name for one register is a thing the reader has to know are the same thing.

**A flag letter is not a stack shorthand target.** `z??`, `c!!`, `n??`, `v!!`, `i??`, `b!!` and `d??` are legal K65, and there the letter is decoration: all seven of `z c n v b i d` assemble to plain `PLP`/`PHP`, so every one of the fourteen spellings means the same two instructions. K816 refuses all seven with `K816-K65-0005` and names `pull p` / `push p` (`p??` / `p!!` in a `.k65` source), which is those two instructions without the suggestion that one flag moves on its own.

**Every register and flag spelling is reserved as a name.** A `var`, `const`, `func`, `data`, `inline` or label may not be called `a b x y s p dpr dbr c d i v n z o`, in any case — `K816-E0158`. A name that is also a register would mean two things in one statement, which is the ambiguity the convention exists to remove.

An **`inline` parameter is the exception**, and the only one in the symbol vocabulary: it may be spelled with any of those letters. A parameter is not in a namespace — it is bound to what the use site passed, expanded away, never emitted, and invisible to every other unit and to the rest of its own. So there is nothing for it to collide with, and the ambiguity the rule removes cannot arise: the eight spellings that really are two things (`a b x y s p dpr dbr`) are *tokens*, so a parameter can never be one, and the seven flag letters are flags only in the slots that carry a `+`, `-`, `?` or `??` after them. `inline lsrx (a, #n) { lsr * n }` declares a parameter `n`, and `c+` inside a body whose parameter is `c` is still `SEC`.

A label costs the grammar a little to say that. The eight register spellings are *tokens* — `a = 1` and `x++` need them to be — so a label rule written over identifiers alone cannot see `a:` at all, and it reported `K816-E0001` for two sub-plans. The label's name slot accepts a register spelling now and the reservation refuses it there, which is why all fifteen give one answer and the caret is on the name that has to change.

The compile-time evaluator is **not** covered, and deliberately: the K65 grammar's `AllIdent` rule lets these letters be names again in an evaluator block and as a `for` variable, and `for i = 0 .. 3` is the idiomatic K65 loop. A reserved name is then an evaluator *local* — a later bracket in another item cannot read it, because the bridge is the symbol table and that is what the reservation closes.

**An instruction's name is reserved too, in the same positions** — `K816-E0173`. `func lda`, `var sec`, `data and` and `naked jmp` are refused, whatever the case, and the reason is not the source but the output: K816 emits ca65 assembly, and `lda:` at the start of a line is the instruction there, not a label. The assembler answers "Expected an expression but found `:`" and refuses the file, so a name accepted here would produce a program that cannot be assembled, with nothing said until ca65 spoke about a file the author did not write.

The original K65 has no raw mnemonics — `Instr -> Arg` and the HLA forms are the whole of a statement — so `func lda` compiles there, and this is a narrowing of a program the original accepts. It is recorded as `k816-instruction-named-declaration` in `doc/k65-compat-manifest.md` and measured against the original in `tests/compat/k65/instruction-named-section`. The two exemptions above hold here unchanged: an `inline` parameter may be `#and`, and `[ nop = 3 ]` is an ordinary evaluator assignment, because neither is ever emitted.

```k65
var c = $10                     // E0158
const n = 5                     // E0158
data table {
  for i = 0 .. 3 eval [ i ]     // fine, as in K65
}
```

**`c` and `d` are flags, not registers.** They name the carry and the decimal flag, which is what the K65 grammar spells them (`Flag -> C|c|D|d|I|i|O|o`). Earlier K816 releases gave `c` to the 16-bit accumulator and `d` to the direct page; both spellings are refused now with `K816-E0158`, naming `a` and `dpr`. `b` outside `a~b` is refused the same way.

In K65 HLA syntax the register names above are native symbols used by the assignment, transfer, and stack shorthand parsers. Not every CPU register is a general-purpose assignment target; unsupported HLA assignments are rejected and must be written with the dedicated native mnemonic. The `b~a` shorthand lowers to the 65816 `XBA` (exchange B and A) instruction.

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

Flag shorthand operations: `c+` (SEC), `c-` (CLC), `d+` (SED), `d-` (CLD), `i+` (SEI), `i-` (CLI), `v-` (CLV). A flag something has set is tested by its letter after `if` or `while` — `if c` when it is set, `if !c` when it is clear — for `c`, `z`, `n` and `v` ([Branch and Flow Control](#branch-and-flow-control)).

The overflow flag is `v` in every construct. The K65 grammar is not consistent about it — the set/clear shorthand is `o-` and the branch tests are `v-?` / `v+?`, one flag under two letters — so `o` in the flag slot is refused with `K816-K65-0006` and `v` named. K816 tests it with `if v` / `if !v`. `o` remains an ordinary identifier: `var o` is a declaration. There is no `SEV` on the 65816, so `v+` has no form at all.

### Processor Stack

**LIFO**, descending. In emulation mode the stack is page-1 based (`0x0100 - 0x01FF`); in native mode the 65816 uses the 16-bit `S` register.

### Push and pull

`push` and `pull` move a list through the stack, one instruction per item, in the order written:

```k65
const STATUS = $0012

func frame @a16 @i16 {
  push a, x, y          // PHA PHX PHY
  push STATUS, $0001    // PEA #$0012  PEA #$0001
  pull x, x             // PLX PLX: drop the two words
  pull y, x, a          // PLY PLX PLA
  push p, dbr, dpr      // PHP PHB PHD
  pull dpr, dbr, p      // PLD PLB PLP
  @a16 @i16             // P came back, and the widths with it: say them
}
```

A register is pushed by its own instruction — `a`, `x`, `y`, `p`, `dbr` and `dpr` have one — and how many bytes it moves is its width at that point, which the width model knows. `pull` takes registers only.

**A constant is `PEA`, which pushes two bytes whatever `M` says**, so a constant has to be written as the word it pushes:

| Written | What K816 says |
|---|---|
| `push $0012`, `push 300`, `push LIMIT`, `push LIMIT + 1` | nothing: a word, or a constant that says nothing about its width |
| `push $12`, `push 18`, `push %00010010` | `K816-W0174`, with a fix that writes `$0012` |
| `push &<mem`, `push &>mem`, `push 'a'`, `push LIMIT:byte`, and `push ERROR` where `const ERROR:byte` | `K816-E0175`: one byte by construction, which no 65816 instruction pushes. The fix writes `(&<mem):word` — the zero high byte said out loud — and `ERROR:word` for a `:byte` constant, which pushes its value at sixteen bits: `$FFFF` for −1 |
| `push mem`, `push routine` | `K816-E0177`: a name is the place it names, as in `a = mem`, and `push` takes values. `push #mem` pushes its address |

Like every HLA operand, a constant runs on across a line break — K65 is newline-insensitive, and `;` is what ends a statement — so `push LIMIT` above `* 3` pushes `LIMIT * 3`; `push LIMIT; * 3` is a push and a three-NOP wait. `pei` and `per` stay mnemonics: `pei (ptr)` pushes the word at a direct-page address whatever `M` says, which a `push ptr` would make read like the value at the accumulator's width — which is why `push ptr` is refused rather than given that meaning.

`push` and `pull` are keywords only with an operand after them on their line, so `jsr push` still calls a routine of that name. K65 spells the same instructions `a!!` and `a??`, one register at a time, which a `.k65` source writes instead and a K816 one refuses with `K816-K65-0010`; a `.k65` source refuses `push` and `pull` as `K816-E0180`.

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

- `if >= { ... }`
- `if < goto label`
- `loop { ... } while <`
- `if c { ... }`
- `if !c goto label`
- `loop { ... } while !c`

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

- `if < { ... }`
- `if >= goto label`
- `loop { ... } while >=`
- `if !c { ... }`
- `if c goto label`
- `loop { ... } while c`

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

- `if != { ... }`
- `if == goto label`
- `loop { ... } while ==`
- `if !z { ... }`
- `if z goto label`
- `loop { ... } while z`

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

- `if >=0 { ... }`
- `if <0 goto label`
- `loop { ... } while <0`
- `if !n { ... }`
- `if n goto label`
- `loop { ... } while n`

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

- `if == { ... }`
- `if != goto label`
- `loop { ... } while !=`
- `if z { ... }`
- `if !z goto label`
- `loop { ... } while !z`

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

- `if <0 { ... }`
- `if >=0 goto label`
- `loop { ... } while >=0`
- `if n { ... }`
- `if !n goto label`
- `loop { ... } while !n`

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

- `if v { ... }`
- `if !v goto label`
- `loop { ... } while !v`

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

- `if !v { ... }`
- `if v goto label`
- `loop { ... } while v`

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
- `<mnemonic> * N` to repeat any implied-addressing instruction `N` times (e.g. `inx * 3`, `clc * 2`, `pha * 4`), with the same three spellings of `N`
- inside an `inline`, a count that names a `#param` is folded at each expansion — see "How an alias is bound"

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
|`push a`|||||||

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
|`push p`|||||||

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
|`pull a`|||||||

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
|`pull p`|||||||

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

Source \ Dest|A|B|DPR|X|Y|S
:---|:---:|:---:|:---:|:---:|:---:|:---:
A|-|`b~a`|`dpr=a` TCD|`x=a` TAX|`y=a` TAY|`s=a` TCS
B|`b~a`|-|-|-|-|-
DPR|`a=dpr` TDC|-|-|-|-|-
X|`a=x` TXA|-|-|-|`y=x` TXY|`s=x` TXS
Y|`a=y` TYA|-|-|`x=y` TYX|-|-
S|`a=s` TSC|-|-|`x=s` TSX|-|-

`a=s` and `s=a` are `TSC` and `TCS`, which move all sixteen bits regardless of `M` — the accumulator is one register and `a` names it at either width. `DBR` has no transfer: `PHB`/`PLB` are the only instructions that move it, spelled `push dbr` and `pull dbr`.

`x=a`, `y=a`, `a=x` and `a=y` are the four that cross between the accumulator's width and the index registers', and each moves the **destination's** width: `TAX` moves as many bytes as `@i` says, `TXA` as many as `@a` says. Where both widths are known and differ, the transfer carries a high byte the source does not hold or drops the one it does, and K816 refuses it (`K816-E0171`) — declare both axes the same width around the transfer, or leave one width unstated where the full-width move is what you mean. See `doc/register-width-aware-syntax.md`, "Transfers between the two axes".

Unsupported direct transfers (e.g. `s=y`, `d=x`) produce a compile error with a hint suggesting a two-step chain:

```k65
s=x=y                  // s=y is not supported; use s=x=y instead (TYX + TXS)
dpr=a=s                // dpr=s is not supported; use dpr=a=s instead (TSC + TCD)
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
|`push x`|||||||

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
|`push y`|||||||

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
|`pull x`|||||||

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
|`pull y`|||||||

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

Raw mnemonic: `stz zp` / `stz zp,x` / `stz abs` / `stz abs,x`. See the [Zero-Store Shortcut](#zero-store-shortcut) section: `mem = 0` lowers directly to `STZ`, and `mem = a = 0` chains — and `x`/`y` ones where the widths agree — get `STZ` via peephole rewrite.

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
|`push dbr`|||||||

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
|`pull dbr`|||||||

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
|`push dpr`|||||||

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
|`pull dpr`|||||||

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

Raw mnemonic: `pea #addr`. Pushes a 16-bit immediate onto the stack. Despite the `#` syntax, the operand is typically a 16-bit address constant — useful for building stack frames or pushing return targets. In HLA it is a constant in a `push` list, `push $1234` — see [Push and pull](#push-and-pull), which holds the constant to the word it pushes.

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

Raw mnemonic: `mvn src_bank, dst_bank`. The compiler parses two comma-separated operands; fixture `syntax-native-native` shows `mvn 0,0`. The operands are bank *numbers*, and K816 emits them to ca65 as `mvn #src,#dst`: ca65's bare form reads each operand as an address and encodes its bank byte, so `mvn $01,$02` handed to ca65 as written would move between banks 0 and 0.

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

A condition tests flags something has already set. It is never a comparison of its own: a compare is a statement — `a?10`, `x?mem`, `a&?mem` — and the condition after it reads the flags the compare left. That is why a condition has no operand, and why `if (a < x)` has no spelling: the 65816 cannot compare two registers, and nothing here pretends it can. The compare may be *written* in the `if` — `if a?10 < { … }` — and it is still that statement, run before the condition reads the flags it set ([The Compare in an `if` or a `while`](#the-compare-in-an-if-or-a-while)).

After a compare the condition is a relation — `<`, `>=`, `==`, `!=`, `<0`, `>=0` — which says what the compare found, and the carry convention behind it is the compiler's to remember. For flags something else set — a load, a shift, an addition, a call answering in the carry — the condition is the flag's letter: `c` when it is set and `!c` when it is clear, for `c`, `z`, `n` and `v`. After `if` or `while` the position says a name is a test, so a flag test needs no mark; `if c?` is sub-plan O's spelling, refused with a fix that drops the `?` (`K816-E0182`), and a name that is not one of the four is `K816-E0181`.

### Conditions

Condition|Flag test|Taken by|A block skips with
:---|:---|:---|:---
`<`|`!c`|BCC|BCS
`>=`|`c`|BCS|BCC
`==`|`z`|BEQ|BNE
`!=`|`!z`|BNE|BEQ
`<0`|`n`|BMI|BPL
`>=0`|`!n`|BPL|BMI
—|`v`|BVS|BVC
—|`!v`|BVC|BVS

A condition goes in three places, and in each the thing happens when the condition holds:

- after `if`, before a block — `if >= { x++ }` runs `x++` when A >= 10, and K816 skips the block with the opposite branch, the last column;
- after `if`, before a jump — `if < goto done`, `if < break`, `if != repeat` are taken when it holds;
- after a loop's `while` — `loop { x-- } while !=` repeats while it holds.

`if` says where the test begins. K65 writes the condition alone in front of the block or the jump, which a `.k65` source keeps and a K816 one refuses with a fix that inserts the `if` (`K816-K65-0011`, [K65's Spellings](#k65s-spellings)). The two compile to the same bytes. A hover on `if` names the branch it compiles to.

`<=` and `>` are relations too, and cost two branches each ([Efficiency Warnings](#efficiency-warnings)).

### Conditional Blocks

```k65
var mem = $10

func work @a8 @i8 {
  a?10
  if >= { x++ }         // x++ only when A >= 10 (BCC skips it)
  if !c { mem++ }      // mem++ only when the carry is clear (BCS skips it)
}
```

### Conditional Jumps

```k65
func work @a8 @i8 {
  a?10
  if < goto done        // to `done` when A < 10 (BCC)
  if !c goto done      // to `done` when the carry is clear (BCC)
  x++
done:
}
```

### Loops

A loop says where it begins: `loop`, a block, and a condition after it.

```k65
var ready = $10

func work @a8 @i8 {
  x = 10
  loop {
    x--
  } while !=            // repeat while X != 0 (BNE back to the top)

  loop {
    a = ready
  } while >=0           // repeat while bit 7 is clear (BPL back)

  loop { x++ } always   // forever (JMP back to the top: a BRA when it reaches)
  loop { x++ } never    // once: a block `break` can leave
}
```

`loop` is a keyword only before a `{` on its own line, so `bne loop` still branches to a label of that name.

### `break` and `repeat`

```k65
func work @a8 @i8 {
  loop {
    x++
    if < break          // leave the loop on carry clear (BCC past the end)
    if != repeat        // back to the top on not-zero (BNE)
    break               // leave unconditionally (JMP past the end, or a BRA)
  } always
}
```

`repeat` goes to the top of the loop, never to its `while` test: the test reads flags the body sets, and the body has not run yet. A conditional repeat has a test of its own. A compare written in the `while` is the body's last statement, so a `repeat` skips it too ([The Compare in an `if` or a `while`](#the-compare-in-an-if-or-a-while)).

### If-Else

```k65
func work @a8 @i8 {
  if == {
    a = 1
  } else {
    a = 2
  }
}
```

`else if` chains: the `if` after `else` is the conditional nested in the `else` block, without the braces, and compiles exactly as `else { if … }` does. Each `if` in a chain tests the flags it finds, so a test that needs a compare of its own writes it in its `if`:

```k65
var key = $10
var mode = $11

func classify @a8 @i8 {
  a = key
  if a?'0' < {
    mode = x = 1
  } else if a?':' < {
    mode = x = 2
  } else {
    mode = x = 3
  }
}
```

### The Compare in an `if` or a `while`

The compare that sets a condition's flags may be written between `if` and the condition, and between `while` and a loop's condition. It is the statement it would be on a line of its own, and runs where that statement would: before the conditional in an `if`, and as the last statement of the body in a `while` — so a `repeat`, which goes to the top, skips it, and a `nocross` loop checks it inside its page.

```k65
var count = $10

func drain @a8 @i8 {
  x = count
  loop {
    x--
    if x?4 == repeat     // CPX #4, BEQ to the top
    count = x
  } while x?1 >=         // CPX #1, BCS to the top
}
```

The slot takes a compare and nothing else — `a?`, `x?`, `y?`, or the bit test `a&?` — because those set flags and change nothing else. `if a+1 == { … }` would read as a test of `a+1` while it also stores it, and is `K816-E0179`; write the addition as a statement before the `if`.

### Flow Control Instructions

A jump K816 writes — `goto`, and the ones `always`, `else` and `break` lower to — is a two-byte `BRA` after optimization wherever its target is in the same function and within a branch's reach, and a `JMP` otherwise. A `jmp` written as a mnemonic is always a `JMP`: a native instruction means exactly what it says.

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
var mem = $10

func work @a8 @i8 {
  nocross loop {
    a = mem
    x++
  } while !=
}
```

### Wait-Loop Patterns

Common polling patterns:

```k65
var status = $D000
var ready = $10

func work @a8 @i8 {
  loop { a&?status } while !n   // BIT-based: loops while bit 7 of `status` is clear
  loop { a = ready } while >=0   // LDA-based: loops while `ready` is positive
}
```

### K65's Spellings

A `.k65` source writes K65's own spellings of these forms. A `.kk` or `.k816` source refuses each K65 spelling with a fix that writes the K816 one, and a `.k65` source refuses each K816 spelling as `K816-E0180` with a fix that writes the K65 one ([`k65-compat.md`](k65-compat.md), "Two dialects, one grammar"). Each pair compiles to the same bytes.

K65|K816|Refused in K816 as
:---|:---|:---
`{ x-- } !=`|`loop { x-- } while !=`|`K816-K65-0007`
`{ ... } always`, `{ ... } never`|`loop { ... } always`, `loop { ... } never`|`K816-K65-0007`
`nocross { ... } !=`|`nocross loop { ... } while !=`|`K816-K65-0007`
`c+?`, `c-?`|`c`, `!c` after `if` or `while`|`K816-K65-0008`
`<<=`, `>>=`|`v`, `!v` after `if` or `while`|`K816-K65-0009`
`a!!`, `a??`|`push a`, `pull a`|`K816-K65-0010`
`>= { ... }`, `< goto done`, `== break`|`if >= { ... }`, `if < goto done`, `if == break`|`K816-K65-0011`
`else { == { ... } }`|`else if == { ... }`|`K816-K65-0011`, inside the `else`
`{ x++ x?10 } <`|`loop { x++ } while x?10 <`|`K816-K65-0007`

### Efficiency Warnings

`>` and `<=` are the two branch conditions the 65816 has no single
instruction for. Unsigned `>` is "carry set *and* not equal" and unsigned
`<=` is "carry clear *or* equal", so each one costs two branch instructions
wherever it is written, and K816 warns with `K816-W0157`:

```k65
func work @a8 @i8 {
  loop { x++ } while >   // beq over the branch, then bcs back to the loop
  loop { x++ } while <=  // bcc back to the loop, then beq back to the loop
  if > { x++ }           // bcc past the body, then beq past the body
  if <= { x++ }          // beq over the branch, then bcs past the body
  if > goto done         // beq over the branch, then bcs to `done`
done:
}
```

The warning is about the *expansion*, so it fires at every form and not only
in a loop, and its note names the two instructions that form emits.
Which two they are depends on the form rather than on the operator: a
block branches *past* its body, so it tests the complement, and `<=` and `>`
are each other's complement. There is no form in which either of them is one
instruction.

Both operators are K816 extensions — the original K65 grammar has neither —
and so is the warning. An equality or a signed test (`==`, `!=`, `<`, `>=`,
`<0`, `>=0`) is a single branch, and reordering the comparison often reaches
one.

## NOP and Wait Shorthand

```k65
*                       // single NOP
* 5                     // 5 NOPs
* COUNT                 // N NOPs, with N from a global `const`
inx * 3                 // repeat any implied-mode instruction N times
```

## Preprocessor Directives

```k65
#if COND
  // compiled when COND is true
#elif COND
  // the first true branch wins; the rest are skipped
#else
  // compiled when no branch above was taken
#endif

#error "message"        // an error carrying that message
#warn "message"         // a warning carrying that message
```

There is no text preprocessor. Directives are ordinary syntax, they are decided by a pass over the parsed program, and nothing that follows that pass — name resolution, the width model, lowering — ever sees one. What a directive can do is therefore exactly *select* which parts of the program exist; it cannot paste text, define a macro, or include a file.

### Where a region may sit

A region opens with `#if` and closes with `#endif` in the same element list. There are three such lists, and a region is confined to one of them:

* the file's top-level items,
* the statements of one block — a section body, and equally the body of a `nocross`, a conditional, a loop, or an `@a16 { }`,
* the entries of one data block.

```k65
func main @a8 @i8 {
  #if 1
  return
}
#endif                  // K816-E0152, twice: the `#if` is never closed and
                        // the `#endif` has no `#if` above it
```

`K816-E0152` also covers an `#endif`, `#else` or `#elif` with no `#if` above it, and a second `#else` in one chain. Each report carries both spans — the directive that cannot pair, and the one it should have paired with.

Nesting is unlimited, and a region inside a region that is not taken is not compiled whatever its own condition says.

A directive ends where its line does. `#if` on one line and its condition on the next is `K816-E0001`, and so is `#error` above its message. Only the inside of an evaluator bracket may wrap, because the `]` says where it ends:

```k65
#if [ LEVEL > 1 &&
      !QUIET ]          // fine: the bracket opens on the directive's line
#endif
```

### Conditions

A condition is a number, a name, `!` applied to either, or an evaluator bracket:

```k65
#if 1                   // a literal
#if DEBUG               // a name
#if !DEBUG              // negated
#if [ LEVEL > 1 && !QUIET ]   // anything richer goes in brackets
```

The bracket form is not a stylistic choice. A directive is confined to the line it starts on, and a full evaluator expression is not — `#if 0` above a line beginning with `[` would read the block below as an index into the condition.

A condition may assign, and what it assigns outlives the directive:

```k65
#if [ TARGET = 3 ]
  // ...
#endif
a = #[TARGET]           // TARGET is 3 here
```

A name resolves against the two namespaces an evaluator bracket reads: the unit's `const` values, and the names earlier brackets assigned. A name nothing declares is `K816-E0101`, not a silent zero.

```k65
[ TIER = 2 ]
const FEATURE = 1

#if FEATURE             // a `const`
#if TIER                // a name a bracket assigned
```

The condition is true when its value, taken through the [integer boundary rule](#compile-time-evaluator), is not zero. So `#if [0.6]` is true and `#if [0.4]` is false.

### Order

A directive is decided where it is written, against everything above it. An evaluator bracket earlier in the same block has already run:

```k65
data table {
  [ FLAG = 1 ]
  #if FLAG              // true: the bracket above ran first
  $42
  #endif
}
```

### What a region that is not taken does

Nothing. Its declarations do not exist, its statements are not emitted, its `#error` and `#warn` say nothing, and an evaluator bracket inside it does not run — so a name it would have assigned stays unassigned.

Its *conditions* are not evaluated either, and neither is the condition of an `#elif` whose chain has already taken a branch. `#if 0` therefore comments out whatever is inside it, including a directive naming something that does not exist. This is the one place K816 departs from the original K65 compiler on purpose; `doc/k65-compat-manifest.md` row `pp-dead-conditions` records the departure and the measurement behind it.

### `inline` bodies

A directive inside an `inline` body is decided once, where the body is written, because expansion happens after analysis. Every expansion gets the branch that was chosen there, and an `#error` or `#warn` in the body speaks once.

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
