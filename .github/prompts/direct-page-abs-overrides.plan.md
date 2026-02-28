# Direct-Page Auto-Shrink with `:abs` Overrides and Variable-Level Defaults

## Summary

Adopt automatic direct-page shrinking for eligible page-0 operands, and add `:abs` as the explicit way to keep 16-bit absolute encoding.

This now applies at two levels:

- use-site override: `lda dp:abs`
- declaration default: `var dp:abs = $0012`

Per the chosen semantics:

- `var dp:abs = $0012` means plain `dp` defaults to absolute 16-bit everywhere
- `:abs` is allowed on top-level `var` declarations
- `:abs` is not allowed on symbolic-subscript field declarations for now

Examples:

```k65
var dp = $0012
var wide_dp:abs = $0012
var other = $1234

lda dp         // auto: may shrink to direct page
lda dp:abs     // force absolute
lda wide_dp    // absolute by declaration default
a = wide_dp    // absolute by declaration default
```

## User-Facing Semantics

### Default Encoding Rule

For direct address operands in native syntax and HLA lowering:

- if the operand resolves to `0x0000..=0x00FF`
- and the instruction has a direct-page encoding for that operand shape
- and there is no active `:abs` preference from declaration or use-site

then use the direct-page encoding.

Otherwise use the absolute-family form (16-bit, or long when required).

### Use-Site `:abs`

`expr:abs` means:

- force 16-bit absolute-family encoding for this operand
- do not use direct-page shrink for this operand
- do not change data width or variable type

Examples:

```k65
lda dp:abs
lda regs.status:byte:abs
lda (ptr:abs),y
a = dp:abs
```

### Declaration `:abs`

`var name:abs = ...` means:

- the symbol carries a default address preference of "force 16-bit absolute"
- every plain reference to that symbol inherits that preference
- a use-site `:abs` on the same symbol is redundant but valid

Examples:

```k65
var dp:abs = $0012

lda dp      // absolute
sta dp      // absolute
a = dp      // absolute
```

This is a symbol default, not a layout/data-width annotation.

### Precedence

Address preference precedence should be:

1. explicit use-site `:abs`
2. declaration default from `var name:abs`
3. auto mode (eligible page-0 references may shrink)

Since there is no opposite marker yet, declaration-level `:abs` is effectively sticky.

## Syntax Changes

## Use-Site Expressions

Extend expression suffix parsing so `:abs` is accepted as an address hint in addition to typed views.

Valid:

```k65
foo:abs
foo:byte:abs
(foo + 1):abs
```

Invalid:

```k65
foo:abs:byte
foo:abs:abs
```

Rules:

- `:byte`, `:word`, `:far` keep their current meaning
- `:abs` is an address-encoding hint, not a typed view
- only one address-encoding hint is allowed
- if present, `:abs` must be last in the suffix chain

## Variable Declarations

Extend top-level `var` declaration syntax to allow `:abs` in the same suffix slot currently used by `:byte/:word/:far`.

Valid:

```k65
var dp:abs = $0012
var table:word:abs = $2000
var ptr:far = $123456
```

Invalid for this iteration:

```k65
var regs[
  .status:byte:abs
] = $2000
```

Symbolic-subscript field declarations remain limited to `:byte/:word/:far`.

## Public AST / Semantic Model Changes

## AST

Current `VarDecl` only stores `data_width` in [mod.rs](/home/smoku/devel/X65/k816/crates/core/src/ast/mod.rs#L83). Extend it with a declaration-level address preference, e.g.:

- `addr_hint: Option<AddressHint>`

Add a reusable enum:

- `AddressHint::ForceAbsolute16`
- optionally structure it for future extension if a long-form hint is ever added

For expressions, do not overload `Expr::TypedView` in [mod.rs](/home/smoku/devel/X65/k816/crates/core/src/ast/mod.rs#L437). Add a dedicated wrapper:

- `Expr::AddressHint { expr: Box<Expr>, hint: AddressHint }`

## Operand Model

Replace the current `force_far: bool` path in operands with a single address-size preference enum. This avoids conflicting booleans once `:abs` exists.

Apply this to:

- `Operand::Value` in [mod.rs](/home/smoku/devel/X65/k816/crates/core/src/ast/mod.rs#L167)
- `OperandShape::Address` in [lib.rs](/home/smoku/devel/X65/k816/crates/isa65816/src/lib.rs#L33)

Recommended enum:

- `AddressSizeHint::Auto`
- `AddressSizeHint::ForceAbsolute16`
- `AddressSizeHint::ForceAbsoluteLong`

## Semantic Model

Current `VarMeta` is populated in [vars.rs](/home/smoku/devel/X65/k816/crates/core/src/sema/vars.rs#L33). Extend it to carry the default address hint from the declaration:

- `addr_hint: Option<AddressHint>`

This lets lowering resolve plain identifiers against symbol defaults without re-reading syntax.

## Parsing Plan

## Expression Parser

In [expr.rs](/home/smoku/devel/X65/k816/crates/core/src/parser/expr.rs#L119):

- replace the current "repeated data widths only" suffix fold with a parser that collects a mixed sequence of:
  - typed views (`byte`, `word`, `far`)
  - address hint (`abs`)
- build nested AST in order
- enforce "`:abs` must be last" and reject duplicates with targeted diagnostics

## Variable Parser

In [vars.rs](/home/smoku/devel/X65/k816/crates/core/src/parser/data/vars.rs#L34):

- replace `.then(data_width_parser().or_not())` with a parser that can accept:
  - optional width only
  - optional `:abs` only
  - optional width followed by `:abs`

For top-level vars, produce:

- `data_width`
- `addr_hint`

For symbolic-subscript fields, keep the existing parser unchanged except for a deliberate diagnostic if `:abs` is attempted, telling the user that declaration-level `:abs` is currently supported only on top-level `var` declarations.

## Lowering and Address Selection

## Identifier Resolution

When lowering a direct operand expression:

- if the expression has an explicit `Expr::AddressHint`, use that
- else if the root symbol is a `var` with declaration `addr_hint`, inherit that
- else use `Auto`

This should apply to both native instructions and HLA-generated loads/stores.

## HLA Immediate Classification

Current immediate detection in [lower.rs](/home/smoku/devel/X65/k816/crates/core/src/lower.rs#L3436) and the parser shortcut in [operations.rs](/home/smoku/devel/X65/k816/crates/core/src/parser/operations.rs#L85) assume plain direct, non-indexed, non-far expressions may be immediates.

Update this so any active address hint (`:abs` at use-site or inherited from a referenced `var`) forces address interpretation, not immediate.

Required behavior:

```k65
var dp:abs = $0012
a = dp      // memory load from $0012, not immediate
```

## Encoder Rules

Update the encoder selection in [lib.rs](/home/smoku/devel/X65/k816/crates/isa65816/src/lib.rs#L496):

### `AddressSizeHint::Auto`

For direct operands:

- if `literal <= 0xFF` and a direct-page opcode exists, prefer direct-page
- otherwise use absolute, then long if required

### `AddressSizeHint::ForceAbsolute16`

For direct operands:

- skip all direct-page candidates
- require the 16-bit absolute-family form for the same operand shape:
  - `Absolute`
  - `AbsoluteX`
  - `AbsoluteY`
  - `AbsoluteIndirect`
  - `AbsoluteIndexedIndirectX`
- if that 16-bit form does not exist, emit a dedicated error

### `AddressSizeHint::ForceAbsoluteLong`

Preserve current `far` behavior semantics.

## Diagnostics

Add targeted diagnostics:

- invalid suffix order:
  - primary: `':abs' must appear after any typed view suffix`
  - help: `write the operand as 'name:byte:abs' or 'name:word:abs'`
- duplicate `:abs`:
  - primary: `duplicate ':abs' suffix`
  - help: `remove the extra address hint`
- unsupported declaration location:
  - primary: `':abs' is only supported on top-level var declarations`
  - help: `move the address preference to the enclosing var or use ':abs' at the use-site`
- impossible forced absolute:
  - primary: `This operand does not support forced absolute addressing`
  - help: `remove :abs or use an operand form with a 16-bit absolute encoding`

Keep remediation in help text, consistent with project diagnostics style.

## Documentation Updates

Update [syntax-reference.md](/home/smoku/devel/X65/k816/docs/syntax-reference.md) in the variable declaration section to distinguish:

- typed width annotations: `:byte`, `:word`, `:far`
- address encoding preference: `:abs`

Add examples:

```k65
var dp = $0012
var dp_full:abs = $0012

lda dp          // may use direct page
lda dp:abs      // force absolute
lda dp_full     // force absolute by declaration
```

Also note explicitly:

- symbolic-subscript fields do not yet support declaration-level `:abs`

## Test Cases

Add or update parser, semantic, and golden tests for:

- `var dp:abs = $0012` parses and stores declaration address hint
- `var table:word:abs = $2000` parses with both width and address hint
- `var dp = $0012; lda dp` emits direct-page
- `var dp = $0012; lda dp:abs` emits absolute
- `var dp:abs = $0012; lda dp` emits absolute
- `var dp:abs = $0012; a = dp` emits an address load, not immediate
- `var dp:abs = $0012; lda dp:byte` still inherits absolute unless explicitly overridden in future
- `var other:abs = $1234; lda other` still emits normal absolute
- invalid order: `foo:abs:byte`
- invalid field declaration: `.field:byte:abs`
- unsupported forced-absolute operand shapes produce the dedicated diagnostic

Update the existing direct-address golden fixture to demonstrate all three cases:

- auto-shrunk page-0
- declaration-forced absolute on page-0
- normal non-page-0 absolute

## Assumptions and Defaults

- Direct-page auto-shrink is the default behavior for eligible page-0 operands.
- `:abs` on a top-level `var` is a default for all plain references to that symbol.
- Use-site `:abs` remains valid and takes highest precedence.
- No "force direct-page" marker is introduced in this change.
- No declaration-level `:abs` support is added for symbolic-subscript fields yet.
- `:word` continues to mean data width only; it must not imply absolute 16-bit addressing.
