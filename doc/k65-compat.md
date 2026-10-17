Keeping `.k65` makes compatibility part of the language contract. K816 should be defined as:

> K816 = original K65 semantics + compatible extensions for the 65816, tooling, and OS development.

“Strict superset” should mean every valid original K65 program remains valid and behaves the same under K816. It does not necessarily require identical object bytes, since ca65/ld65 may encode metadata and layout differently. For documented HLA constructs, however, preserving the expected instruction expansion is valuable because timing and code size can be observable.

I would establish these rules before implementing the new parser:

* Reserve every original K65 keyword, operator, directive, and syntactic form immediately.
* If a legacy feature is recognized but not implemented, issue a specific compatibility diagnostic. Never reinterpret it as a new K816 construct.
* Preserve K65 expression precedence, implicit conversions, scoping, comments, literals, and even inconvenient quirks unless an explicit opt-in mode says otherwise.
* Default legacy code to 6502-compatible 8-bit accumulator and index semantics. Widening must require a K816 construct.
* Lower original K65 HLA syntax into the same native instruction sequence it historically implied, before the common peephole optimizer runs.
* Never pass ambiguous K65 syntax directly through to ca65. The frontend must translate it explicitly so ca65 cannot assign it different semantics.

For example:

```text
error[K816-K65-0001]: K65 `image` is not a K816 construct
  --> example.k65:18:5
   |
18 |     image sprites 0 0 8> 16v
   |     ^^^^^ K816 has no image reader

note: reading a bitmap and scanning it into tiles is an asset pipeline, which
      K816 does not own (`doc/design.md` §1)
help: convert the image outside the build and include the bytes with `binary`
```

The wording is a *removal* rather than a reservation, and that is the shape
every one of these has now: each names the reason it will not change and what
to write instead. There is no "not implemented yet" diagnostic left in the
compiler — `K816-E0201` was retired with its last producer in sub-plan G's
task G6 — because a construct the grammar knows and the compiler does not is
a bug rather than a schedule.

The compatibility work should have its own manifest. It has one:
[`k65-compat-manifest.md`](k65-compat-manifest.md), a row per rule of
the original compiler's grammar and a row per K816 construct that is not one,
each with a status, the fixture that proves it, and — where the status is not
`compatible` — the reason. `tests/compat.rs` reads it.

Its statuses are the master plan's four plus the one this document contributed,
and `Reserved` is gone (`plans/06-compatibility.md` decision 1). The master
plan wins by `plans/implementation.md` §2, so its four stay as it spells them;
`--strict-k65` cannot be implemented without knowing which constructs are
extensions, and that column existed only here.

| Status | Meaning |
| --- | --- |
| `compatible` | Implemented with the original's semantics; the oracle and K816 agree at the bytes, or at the instruction sequence where layout differs. |
| `intentionally-changed` | K816 does something else on purpose, the difference is documented, and `--strict-k65` reports it. |
| `legacy-defective` | The original (or the legacy K816) did something its own documentation contradicts; K816 does what the documentation says, and the row says what the old behaviour was. |
| `backend-different` | Same behaviour, different bytes or layout, because ca65/ld65 own encoding and placement. Compared at the instruction sequence. |
| `K816-extension` | Not in the original grammar. `--strict-k65` rejects it. |

`Reserved` is not carried over, because it names the absence of a decision
rather than a decision. The manifest spells that `open` and names the task that
closes it, and no `open` row survives the sub-plan: sub-plan G's task G10
closed the last four and `tests/compat.rs` asserts there are none.

Two columns sit beside the status and are checked with it. **`Cheap`** is the
per-rule judgment the rule above turns on — was compatibility cheap for K816 —
written down rather than left to be inferred from prose, so that a row claiming
to be compatible with something it never paid for is a test failure. **`Strict`**
is what `--strict-k65` does where the row's construct appears, which the status
alone does not decide: most of the intentional changes are constructs the
compiler already refuses, and a warning there would repeat an error the author
has read.

## Where the superset stops

One rule above says "reserve every original K65 form" and another says
"preserve even inconvenient quirks". Both have a floor, and it is the
instruction set: K816 assembles the **legal** MOS 6502, WDC 65C02 and WDC
65C816 instructions and nothing else ([`design.md`](design.md) §1). Where a
K65 construct can only be expressed with an undocumented encoding, no amount
of compatibility work reaches it — on the 65816 those bytes are a different,
real instruction, so reproducing the K65 behaviour and reproducing the K65
bytes are two incompatible goals and neither is the original's.

Three constructs sit on that floor.

* The five illegal-opcode HLA forms — `a&.imm` (ANC), `a?--mem` (DCP),
  `a-++mem` (ISC), `mem=a&x` (SAX), `x&=a-imm` (AXS). Recognized as named
  nodes and refused; the diagnostic says which 65816 instruction the encoding
  would execute. It does **not** offer a replacement sequence: for two of the
  five there is no clean one, and for the other three, publishing a
  hand-rolled substitute for an encoding this project does not support is
  inviting back what the boundary excludes.
* `* N`, the cycle wait. The original spends an odd cycle through `$04`,
  which is `TSB dp` on the 65816. K816 keeps its own documented meaning — N
  `NOP`s — which is the one place a K65 spelling is given a K816 meaning, and
  `--strict-k65` warns about it.
* `%`, which the original also assembles as `$04` rather than `BRK`. Same
  answer, same warning.

These are `intentionally-changed` rows, not `compatible` ones, and the reason
column is this section.

## Two dialects, one grammar

A `.k65` source is K65 and a `.kk` or `.k816` source is K816. K816 writes
six of K65's forms its own way, and each dialect refuses the other's
spelling of them, with a fix that writes its own:

| K65 spelling | K816 spelling | Refused in K816 as |
|---|---|---|
| `{ x-- } !=`, `{ } always`, `{ } never` | `loop { x-- } while !=`, `loop { } always`, `loop { } never` | `K816-K65-0007` |
| `c+?`, `c-?` (and `z`, `n`, `v`) | `c`, `!c` after `if` or `while` (sub-plan O's `c?` is `K816-E0182` since sub-plan R) | `K816-K65-0008` |
| `<<=`, `>>=` | `v`, `!v` after `if` or `while` | `K816-K65-0009` |
| `a!!`, `a??` | `push a`, `pull a` | `K816-K65-0010` |
| `>= { }`, `< goto done`, `== break`, `else { == { } }` | `if >= { }`, `if < goto done`, `if == break`, `else if == { }` | `K816-K65-0011` |
| `a?10` before `< { }`; `x?10` as a loop body's last statement | `if a?10 < { }`; `loop { } while x?10 <` | — (the first spelling is valid K816 too) |

Each pair compiles to the same bytes, so the refusal is about spelling and
never about meaning: applying the fix changes nothing a program does. The
grammar is one for both dialects, so either spelling parses in either file
and is refused by name rather than reported as a parse error, which is the
rule above for any form a dialect does not take.

**A `.k65` source accepts what K816 adds and nothing K816 respells.** Every
K816 spelling in the table is refused there as `K816-E0180`, with a fix
that writes K65's. What K816 *adds* has no K65 spelling to write instead —
widths, `far`, `import`, `extern`, native mnemonics, typed data, the rest
of the manifest's extension rows — and stays accepted, and `--strict-k65`
reports it. The user, for sub-plan Q (2026-10-16): "the new syntax is only
for k816 and .kk files, not for the .k65 compat", and then, of sub-plan O's
forms, "withdraw them from .k65 too". Until then a `.k65` source accepted
both spellings (`plans/16-if-statement.md` decisions 10 and 11).

Sub-plan S's conditions divide the same way (`plans/18-conditions.md`
decision 7). A chain (`if c | z { }`) and a step in a condition's slot
(`while x--`) are written after `if` or `while`, which a `.k65` refuses
already, so they reach one only inside that refusal — and K65 has no
spelling for either to fix to. `when`, `continue` and `unroll` are
additions: a `.k65` accepts them with the meaning they have in K816 (a
`when` value is a compare's operand, so a bare number is a value in either
dialect, as in `a?4`), and `--strict-k65` reports each (`k816-when`,
`k816-continue`, `k816-unroll`).

K65 compatibility is kept for bootstrapping and as a baseline, and will
eventually be dropped (`plans/14-readable-hla.md` decision 11). The
manifest's `k816-dialect` row is this section.

This document is itself part of the authority chain now
(`plans/implementation.md` §2, rule 4): it is the only one that says what
compatibility *means*, and the manifest is its inventory rather than a rival to
it.

I would also provide `--strict-k65`, but not make it a separate compiler architecture. A `.k65` source accepts K65 and what K816 adds, and a K816 one the same with K816's spelling of the six forms above; strict mode merely rejects K816 extensions so portable K65 programs can be checked.

## `--strict-k65`, and the policy behind it

The flag exists, on `compile` and on `emit-ir` alike, and it is a reporting
mode and nothing else: no width, no lowering and no byte depends on it. What it
adds is `K816-E0160` at each construct the original language does not have and
`K816-W0161` at each construct that compiles in both and means something
different here. A K65 program is silent under it.

**The rule it enforces is the one this document opens with, and it is worth
stating as policy rather than as a switch.** K816 is backwards compatible with
K65 wherever compatibility is *cheap for K816*; where it is not, K816
recognizes the K65 syntax as a named node and refuses it with a diagnostic of
its own — never a generic parse error, and never a K65 spelling quietly given a
K816 meaning. Three consequences follow, and each is checkable rather than
aspirational:

* **A construct K816 refuses needs no flag.** The compiler already said so, by
  name, with what to write instead. That is why most of the
  intentionally-changed rules of the original grammar — sixteen of the
  eighteen — have nothing for `--strict-k65` to add, and why the flag's warning
  set is two rows rather than eighteen.
* **A construct K816 *accepts* and reads differently is the flag's whole
  subject**, and there are two of them in the original's grammar. `* N` and
  `%`, the cycle wait, is decision 11's recorded exception: the original spends
  an odd cycle on an encoding the 65816 gives a different, real meaning, so
  reproducing it is not expensive but impossible. And a section's
  `evaluator [ … ]` transform, where an assignment inside the body does not
  outlive the byte it ran for — narrowed to bodies that *do* assign, because a
  transform that only computes has no difference to report.
* **What counts as an extension is not the flag's opinion.** It is the `Strict`
  column of [`k65-compat-manifest.md`](k65-compat-manifest.md), mirrored in
  `compat::MANIFEST`; `tests/compat.rs` reads the file and checks the two agree
  row for row in both directions, and checks from the other end that every row
  the column marks is reported by something in the corpus. A row marked in the
  document and found by nothing is a promise with no witness, and that is a
  test failure here rather than something a reader has to catch.

Before the C rewrite progresses much further, freeze a reference snapshot of the [original K65 repository](https://github.com/Krzysiek-K/k65) and build a compatibility corpus from:

* Its tests and examples.
* Its documentation.
* Programs compiled by both implementations.
* Negative cases and parser edge cases.
* Real `.k65` sources, including current K816 sources.

That corpus is `tests/compat/`, in the three sets its `README.md` describes,
and the original compiler is its behaviour oracle. What is frozen is the
original's *output*: `tests/compat/oracle.sh` runs it and commits the bytes
beside the fixture with the invocation that produced them, so provenance is
recorded and `cargo test` does not depend on a binary outside the tree.

The test hierarchy should distinguish:

1. Source compatibility: it parses.
2. Semantic compatibility: it behaves identically.
3. Expansion compatibility: documented HLA produces the expected assembly sequence.
4. Binary compatibility: checked where meaningful, but not universally promised.

This also improves the proposed C architecture. The frontend should not begin with “a convenient new grammar.” It should begin with a complete inventory of K65 grammar, including reserved-but-unimplemented nodes. Both original K65 and new K816 syntax then lower into the common assembly-shaped IR, where the effect-aware optimizer operates.

So the revised implementation order should be:

1. Freeze K65 reference, document the compatibility contract, and classify the golden tests.
2. Implement the complete lexical and syntactic namespace, including unsupported-feature nodes.
3. Restore core K65 behavior.
4. Add the common lowered IR and optimizer.
5. Add K816/65816 extensions without consuming any K65 namespace.
6. Emit ca65 source, debug annotations, and diagnostics.
7. Add LSP support over the same frontend.

That makes “K816” an honest continuation of K65 rather than merely another language that happens to use its filename extension.

