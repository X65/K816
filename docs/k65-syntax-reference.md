# K65 Syntax Reference (from source docs + C compiler grammar)

This document summarizes K65 syntax as defined by the original project docs and the reference C compiler grammar.

For k816 compatibility: `segment` is the preferred section-selection keyword. Legacy `bank` remains accepted as a deprecated alias and emits a warning.

## Source Material

External source-of-truth URLs (from `prompts/init-project.plan.md`):

- <https://raw.githubusercontent.com/zbyti/k65-mkdocs/refs/heads/master/docs/syntax.md>
- <https://raw.githubusercontent.com/zbyti/k65-mkdocs/refs/heads/master/docs/instructions.md>
- <https://raw.githubusercontent.com/zbyti/k65-mkdocs/refs/heads/master/docs/examples.md>
- <https://github.com/Krzysiek-K/k65/tree/master/src>

Vendored mirror used in this repository (network fetch is unavailable in this environment):

- `vendor/doc/docs/syntax.md`
- `vendor/doc/docs/instructions.md`
- `vendor/doc/docs/examples.md`
- `vendor/doc/docs/evaluator.md`
- `vendor/src/compiler.inc` (authoritative grammar/actions)

## Lexical Elements

- Comments:
  - Line: `// ...`
  - Block: `/* ... */`
- Identifier text: `[a-zA-Z_][a-zA-Z0-9_]*`
- Local label identifier form: `.name` (rewritten internally to function-local names)
- Integer literals:
  - Decimal: `123`
  - Hex: `0x7F`
  - Binary: `0b1010`
- Float literals (evaluator): `1.25`, `.5`
- Strings: `"..."`

## Top-Level Forms

A file is a sequence of top-level elements (`TopElem`), including:

- Empty statement: `;`
- Compile-time evaluator execution:
  - `[ EvalCode ]`
- Variable declarations:
  - `var name`
  - `var name = Number`
  - `var name[Number]`
  - `var name[Number] = Number`
  - Comma-separated lists: `var a, b, c`
  - Verbose marker: trailing `?` on a declaration item
- Constant declarations:
  - `const name`
  - `const name = Number`
- Segment selection (preferred):
  - `segment ident`
- Deprecated alias:
  - `bank ident` (accepted, emits warning)
- Top-level image/binary sections:
  - `image Name = "path"`
  - `image "Name" = "path"`
  - `binary Name = "path"`
- Data section:
  - `data Name { ... }`
- Code sections:
  - `main { ... }`
  - `func Name { ... }` (auto-RTS)
  - `naked Name { ... }` (no auto-RTS)
  - `inline Name { ... }`
- Evaluator function definition:
  - `evalfunc name(args) [ EvalCode ]`
  - `evalfunc name(args) { EvalCode }`
- Preprocessor directives:
  - `#if`, `#elif`, `#else`, `#endif`
  - `#error "..."`, `#warn "..."`

## Section Placement Directives

Supported alignment/address directives where allowed by grammar:

- `align N`
- `align N + OFFSET`
- `address N`
- `nocross` (context-dependent scope or block constraint)

## Data Block Syntax

Inside `data Name { ... }`, `Data` items can include:

- Raw byte-ish arguments: `Arg`
- 16-bit address split shorthand: `&& VarArg`
- Strings: `"TEXT"`
- Labels: `label:`
- Charset declaration for string encoding:
  - `charset "..."`
- Evaluator precompile in data context:
  - `evaluator [ EvalCode ]`
- Loop-generated data:
  - `for ident = Number .. Number eval [ EvalCode ]`
- Nested scoped constraints:
  - `nocross { DataList }`
- Embedded code blocks:
  - `code { Sequence }`
  - `nocross code { Sequence }`
- Repeat data chunk:
  - `repeat Number { DataList }`
- Undefined byte marker:
  - `?`
- Binary include:
  - `binary ident`
  - `binary "path"`
- Image extraction pipeline:
  - `image ident|string x y bit_len bit_dir [bit_seq] img_len row_dir [inv]`
  - `tiles dx dy count`
  - `colormode Number`
  - `imgwave [args...]`

## Code Sequence Syntax

`Sequence` contains `Instr` and optional semicolons.

Instruction families include:

- Function-like call by identifier/argument:
  - `Arg`
  - `far Arg`
- Far jump:
  - `far goto Arg`
- Arithmetic forms:
  - `A ArithOp Arg`
  - `X ArithOp Arg`
  - `Y ArithOp Arg`
- Unary/postfix forms:
  - `Arg ++`, `Arg --`, `Arg <<`, `Arg >>`, `Arg <<<`, `Arg >>>`
- Moves/chains:
  - `Arg = Arg`
  - `Arg = Arg = Arg ...`
- Flag operations:
  - `c+`, `c-`, `d+`, `d-`, `i+`, `i-`, `o+`, `o-`
- Stack shorthand:
  - `a!!`, `a??`
  - `z!!`, `z??`, `c!!`, `c??`, `n!!`, `n??`, `v!!`, `v??`, `b!!`, `b??`, `i!!`, `i??`, `d!!`, `d??`
- Return:
  - `return`, `return_i`
- Goto/call:
  - `goto target`
  - `BranchOp goto target`
  - `goto (target)`
  - `call target`
- Labels:
  - `label:` and `.local:`
- Inline data emission in code:
  - `data { DataList }`
  - `nocross data { DataList }`
- Loop control:
  - `break`
  - `BranchOp break`
  - `repeat`
  - `BranchOp repeat`
- NOP shortcuts:
  - `*`
  - `* Number`
  - `%`

## Branch and Control Syntax

Prefix branch block forms (`Br1`):

- `BranchOp { Sequence }`
- `never { Sequence }`

If/else form:

- `BranchOp { Sequence } else { Sequence }`

Postfix loop forms (`Br2`):

- `{ Sequence } BranchOp`
- `{ Sequence } always`
- `{ Sequence } never`

Branch operators and aliases:

- Symbolic:
  - `>=` (`BCS`), `<` (`BCC`), `==` (`BEQ`), `!=` (`BNE`)
  - `<0` (`BMI`), `>=0` (`BPL`), `>>=` (`BVC`), `<<=` (`BVS`)
- Flag aliases:
  - `c+?`, `c-?`, `z+?`, `z-?`, `n+?`, `n-?`, `v+?`, `v-?`

## Argument Grammar

`Arg` supports:

- Registers: `a`, `x`, `y`, `s` (case-insensitive)
- Immediate number: `Number`
- Variable/address forms (`VarArg`)
- Low/high byte extraction:
  - `&< VarArg`
  - `&> VarArg`
- Indexed:
  - `VarArg, X`
  - `VarArg, Y`
- Indirect:
  - `(VarArg)`
  - `(VarArg, X)`
  - `(VarArg), Y`

`VarArg` supports:

- `ident`
- `ident + ident`
- `ident - ident`
- `ident + Number`
- `ident - Number`

## Evaluator (`[ ... ]`) Syntax

Evaluator code supports:

- Statements and sequencing via `,`
- Grouping with `{ EvalCode }`
- `if` / `if ... else`
- `while`
- Assignments:
  - `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `|=`, `&=`, `^=`, `<<=`, `>>=`
- Ternary: `?:`
- Logic and bitwise operators:
  - `||`, `&&`, `|`, `^`, `&`
- Comparisons:
  - `==`, `!=`, `<`, `>`, `<=`, `>=`
  - `?>` (max), `?<` (min)
- Arithmetic:
  - `+`, `-`, `*`, `/`, `%`, unary `+`, unary `-`
- Unary logical/bitwise:
  - `!`, `~`
- Prefix/postfix increment and decrement on identifiers
- Function calls:
  - `name(args)`
  - method-like `expr.name(args)`
- Indexing:
  - `expr[index]`
  - `expr[x, y]`
- Property access:
  - `expr.name`
- Built-in constant:
  - `pi`

## Preprocessor Scope

`#if/#elif/#else/#endif` applies at multiple grammar levels:

- top-level (`TopElem`)
- code sequence (`Instr`)
- data list (`Data`)

`#if` condition accepts:

- numeric literal (`Number`)
- identifier (resolved via evaluator)
- unary negation (`!`)

## Notable Edge Cases from Grammar/Examples

- Semicolons are optional separators; many statements can be space-separated on one line.
- Local labels (`.name`) are scoped by an internal section-local prefix.
- `for x=a..b eval [...]` in data is inclusive and supports descending ranges.
- `nocross` can apply as a section/data option and as a wrapper around code/data sub-blocks.
- `main`, `func`, `naked`, `inline`, `data`, `image`, `binary` all participate in section creation.
- Branch syntax supports both symbolic (`>=`) and flag aliases (`c+?`) for equivalent operations.
- Legacy grammar/doc examples use `bank`, but k816 prefers `segment`; `bank` is kept for compatibility with deprecation warning.

## Open Notes for k816 Rewrite

- The current k816 parser/encoder may not yet implement all legacy K65 forms listed here.
- Golden fixtures under `tests/golden/fixtures/syntax-*` intentionally cover these forms and may fail until implementation reaches parity.
