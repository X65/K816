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

Within evaluator expressions, floating-point literals are also supported (e.g. `[1.5 + .5]`). When used as an instruction operand, the result is rounded to the nearest integer and AND-ed with `0xFF`.

## Variable Declaration

Variables in K65 are names given to chosen memory addresses.

```k65
var foo=0x80            // declares 'foo' at address 0x80
var foo2                // declares 'foo2' at address 0x81 (next after previous var)
var foo3, foo4          // multiple declarations per line allowed
var bar[10], bar2[10]   // [] specifies variable size in bytes (address increment for next var)
var bar3 ?              // adding '?' at the end makes the compiler print var addresses
```

When no address is specified, the variable is placed at the next address following the previous variable. The `[N]` suffix reserves N bytes starting at the variable address, and advances the auto-address counter accordingly.

## Constant Declaration

The best way of defining constants is using the evaluator. Constants defined this way can be changed at any moment during compilation. Constants can be any value of floating point type. When used within a 6502 instruction, they are converted to a single byte by rounding to the nearest integer and AND-ing with `0xFF` (this way negative values are represented in U2 form).

```k65
[                       // square brace starts evaluator expression
  MY_CONSTANT = 5,      // define constants
  SOME_NUMBER = 0x13
]                       // end of evaluator expression
```

## Labels

A label can be placed at the beginning of a statement. During assembly, the label is assigned the current value of the active location counter and serves as an instruction operand. There are two types of labels: global and local.

### Global

```k65
var SCREEN=0x400

main {
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

### `main`

Program entry point.

```k65
main {
  a=0        // set accumulator to 0
  {} always  // loop forever
}
```

### `func`

User defined function, that can be called from the code. `RTS` is added automatically at the end.

```k65
func inc_x {
  x++        // increments X register and returns
}            // RTS is added automatically
```

### `naked`

Just like `func`, but no `RTS` is added automatically at the end.

```k65
naked inc_x_twice {
  x++        // increments X register
  goto inc_x // jump to previously defined inc_x (saves stack)
}            // no RTS here; make sure function never reaches here
```

### `inline`

User defined macro that is inlined in the code when used.

```k65
inline inc_y {
  y++
}
```

Functions and inlines are used simply by specifying their names, which places a `JSR` opcode or inlines the code. Function and inline calls do not pass any parameters. Any potential parameter and return value handling must be handled explicitly by the programmer using registers, stack, or predetermined memory locations.

```k65
func test {
  inc_x      // this will use JSR instruction to call 'inc_x'
  inc_y      // this will inline 'inc_y' - no overhead compared to simple 'y++'
}
```

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

The `far` prefix uses the 65816 CPU's 24-bit addressing. A `far func` generates a `JSL` (Jump to Subroutine Long) call instead of `JSR`, and the function itself ends with `RTL` (Return from subroutine Long) instead of `RTS`.

```k65
far func long_range_sub {
  x++                   // RTL is added automatically (instead of RTS)
}

func caller {
  far long_range_sub    // generates JSL instead of JSR
}
```

## Raw Data

Raw data bytes can be emitted inline in code sections using `data { }`:

```k65
var bcol=0xd020

main {
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

## Data Blocks

Data blocks are defined using the `data` keyword. Defining a data block simultaneously defines a label at its first element, so the block is accessible using simple indexing like `MyData,x`. Data blocks can have optional alignment or no-page-crossing restrictions.

### Placement Directives

```k65
data MyData1 {
  align 16              // align to 16 byte boundary
  1 2 3 4 5 6 7 8
}

data MyData2 {
  align 256 + 8         // align to 8 bytes after page boundary (lower address byte will be 0x08)
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

In code, address byte operators are used to load address parts as immediates:

```k65
a=&<addr               // LDA #<addr (low byte)
a=&>addr               // LDA #>addr (high byte)
```

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

### `?` (Verbose Flag)

Adding `?` at the end of a data block makes the compiler print its address and size:

```k65
data generated {
  code { a=1 }
  ?
}
```

## Compile-time Evaluator

The K65 compiler has an embedded evaluator that executes at compile-time. It can perform arbitrary math operations whose results are inlined in the final code as immediates. The evaluator can also set and use global compiler constants.

In every place where the compiler expects a single number, you can invoke the evaluator using square brackets:

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
`rnd()`|Random value `0 <= x < 1`
`index(tab, x)`|1-dimensional indexing (same as `tab[x]`)
`index(tab, x, y)`|2-dimensional indexing (same as `tab[x, y]`)
`size(sec)`|Current size of section
`addbyte(sec, b)`|Add byte to section
`color(r, g, b)`|Nearest palette index for color (r, g, b)
`color(x)`|Nearest palette index for color `0xRRGGBB`
`print(msg)`|Print message
`error(err)`|Print error and terminate compilation

## Assignment Chaining

Multiple assignments can be chained. The value flows from the rightmost source through each target left-to-right:

```k65
var dst0 = 0x10
var dst1 = 0x11
var src = 0x12

main {
  dst0=dst1=a=src     // LDA src; STA dst1; STA dst0
  x=a=dst0            // LDA dst0; TAX
  y=x=a               // TXA; TAX ... (value propagation)
  COLPF2=a=VCOUNT     // LDA VCOUNT; STA COLPF2
}
```

## Standard 6502 Instructions

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

### Registers

- `PC` program counter (16 bit)
- `AC` accumulator (8 bit)
- `X` X register  (8 bit)
- `Y` Y register  (8 bit)
- `SR` status register **NV-BDIZC** (8 bit)
- `SP` stack pointer (8 bit)

### SR Flags NV-BDIZC

```
bit 7 to bit 0

N ....  Negative
V ....  Overflow
- ....  ignored
B ....  Break
D ....  Decimal (use BCD for arithmetics)
I ....  Interrupt (IRQ disable)
Z ....  Zero
C ....  Carry
```

### Processor Stack

**LIFO**, top down, 8 bit range `0x0100 - 0x01FF`

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

/\* TBD \*/

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
- `v+{ ... }`
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
- `v-{ ... }`
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
|`o-`|||||||

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
- `*<number>` to wait `<number>` of cycles

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
|`flag!!`|||||||

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
|`flag??`|||||||

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
`>>=`|`v-`|`<<={ ... }`|`>>= goto label`|`{ ... } >>=`|BVC
`<<=`|`v+`|`>>={ ... }`|`<<= goto label`|`{ ... } <<=`|BVS

Note: prefix form inverts the condition (block executes when condition is false, branch skips over it).

### Prefix Form

The condition before `{ ... }` causes a branch *over* the block when true:

```k65
a?10
>={ x++ }             // executes x++ only when A < 10 (BCS skips over)

c+?{ mem++ }          // executes mem++ only when carry is clear
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
call label             // JSR
far label              // far call (JSL, 24-bit addressing)
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
*                       // single NOP (2 cycles)
* 5                     // wait 5 cycles
%                       // BRK (debug breakpoint)
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

Register and flag names are case-insensitive:

```k65
A=0 X=A Y=x            // uppercase and lowercase registers
C+ D- I+               // uppercase flags
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
