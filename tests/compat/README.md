# The K65 compatibility corpus

This is sub-plan G's corpus (`plans/06-compatibility.md`). Every directory
under one of the three sets below is one fixture: sources, a `fixture.txt`
that says what K816 does with them and why, and — where there is one — a
committed golden.

`doc/k65-compat-manifest.md` classifies every rule of the original K65 grammar
and every K816 construct that is not one of them. Each fixture names the row it
answers to; each row names the fixture that proves it. `tests/compat.rs` checks
that the two index each other, and that no line of the original grammar was
skipped in the walk.

## The three sets

### `legacy-k816/`

The legacy K816 compiler's own golden fixtures, in sub-plan G's domain,
imported as **inputs**. Their expectations are re-derived rather than trusted:
where the legacy compiler and the original K65 disagree, the original wins
(decision 2), and the legacy reading becomes a `legacy-defective` row. Two of
the imported fixtures are not oracles for anything at all — the legacy compiler
ignored preprocessor directives with a warning, and compiled a top-level
`image`/`binary` into a label with no bytes behind it.

Each fixture's note says what the legacy compiler produced and whether K816
agrees. Where the bytes differ, the difference is named. The commonest one has
a single cause: the legacy compiler emitted `sep #$30` at the entry of every
`@a8 @i8` function, and K816 does not, because an annotation on a section is an
assertion about the entry ABI rather than a request to set it (`src/abi.rs`,
`K816-E0143`).

### `k65/`

Sources the **original K65 compiler** was asked about, with its answer
committed beside them as `oracle.hex`. These are the fixtures that settle
questions no document of either compiler answers — what `?` after a variable
name emits, whether an evaluator bracket sees a `const`, whether a value past
`$FF` wraps or is refused, what `align 256 + 8` places where.

`cargo test` never runs the original compiler, and a committed golden is
evidence only if it is bound to what produced it. So each one records the
pinned K65 revision from `oracle.lock` and an Adler-32 checksum of every input
the build reads, and `tests/compat.rs` re-checks both without running anything:
an input edited without re-running the script fails, a golden produced against
a compiler the lock does not name fails, an input the golden does not record
fails, and a `k65/` fixture with no golden at all fails.

**A refusal is a result too.** A fixture whose `oracle.conf` says
`expect failure` gets an `oracle.err` instead of an `oracle.hex`, carrying the
exit status and the compiler's own diagnostics. `preprocessor-undefined-name`
and `eval-refusals` are the two: what they prove is that the original *rejects*
those sources, and a build that started succeeding would be as much of a change
as different bytes.

`whole-program/` is the one fixture that is a program rather than a claim — two
units mixing the constructs the others isolate. It is authored here, bitmap
included; it replaced a program copied from the original K65 distribution,
which came with no licence and no redistribution permission.

### `k816/`

Fixtures invented for the decisions sub-plan G takes, one per claim — because
the legacy corpus touches a handful of constructs and coverage comes from
fixtures written at the level the claim lives. Tasks G2 through G11 fill this
directory.

## What the oracle answered

The `k65/` fixtures exist to settle questions, and these are the answers. Each
links to the fixture whose golden is the evidence; the reasoning is in that
fixture's own note.

| Question | Answer | Fixture |
| --- | --- | --- |
| Does the evaluator see a `const`? | **Yes** — and a data label, as a *section*. Only a `var` fails to resolve. | `k65/eval-sees-consts` |
| Is a value outside the context's range wrapped or refused? | **Wrapped, silently.** `[300]` in a byte is `$2C`. | `k65/eval-integer-boundary` |
| How does a value cross an integer boundary? | `i32(floor(f32(v) + 0.5f32))` — round half **up**, over `f64` arithmetic between `f32` ends. Past `int` range the original's conversion is undefined behaviour; this build yields `INT_MIN` for every out-of-range value, `2147483647` included, because `f32` of it is 2³¹. | `k65/eval-integer-boundary` |
| What does `var X ?` emit? | **Nothing.** It prints the variable's address at compile time. | `k65/declaration-comma-lists` |
| Is `align` accepted at the start of a `func`? | **Yes**, and so is `address`, in `func`, `naked`, `main` and `data` alike. | `k65/align-and-address` |
| What does `align 256 + 8` place? | The section at an address **congruent to 8 modulo 256**. | `k65/align-and-address` |
| What does `#if UNDEFINED` do? | **Errors** — "Undeclared eval variable". | `k65/preprocessor-undefined-name` |
| What bytes does `.LT+1 = a` produce? | `sta .LT+1`, absolute, **no diagnostic**. | `k65/label-offset-targets` |
| What is a method call? | **Sugar**: `x.f(a)` is `f(x, a)` and `x.f` is `f(x)`. | `k65/eval-forms` |
| What is a string value? | A value **inside** an evaluation — `+`, `==`, `<`, `print`, `error` — that cannot cross an integer boundary. | `k65/eval-forms` |
| What does an `evaluator [ ]` data entry do? | Installs a **per-byte transform** on the section, over the bytes it *generates* and not over its literal entries. | `k65/eval-forms` |
| Is `evalfunc` recursion bounded? | **No — it is broken.** A self-call resolves to evaluator function zero. | `k65/eval-statements` |
| What does `* N` emit? | **N cycles** of wait, spending an odd one on `NOP $80` — opcode `$04`, which is `TSB dp` on the 65816. | `k65/nop-and-wait` |

Three more things the original does not have, found on the way: no `abs`
(`k65/eval-refusals`), no `||` — both logical operators are spelled `&&` in its
grammar — and `%` is a non-negative modulo.

## `fixture.txt`

`key value` lines up to the first blank line, then prose for a reader:

    row       the manifest row this fixture answers to
    config    the ld65 configuration to link with
    compare   `diagnostics` or `bytes`
    units     the sources, in order. **Every one is compiled**, each with the
              others passed as siblings - which is what `--unit` means on the
              command line. A program's second file reports its own
              diagnostics, and compiling only one of them lets the rest rot
              unnoticed.
    expect    `<unit> <code>`, repeated - every unit is compiled as an
              entry with the others as its siblings, and the expectations are
              in the order the compiler reports them, unit by unit

A fixture that does not compile lists its codes; a fixture that compiles cleanly
commits its linked bytes as `expected.hex`, because "it compiles" is not the
claim — the claim is which bytes.

The prose is deliberately not parsed. What a fixture *proves* is an argument,
and an argument does not belong in an assertion.

## The link configurations

| File | For | Map |
| --- | --- | --- |
| `legacy-stub.cfg` | `legacy-k816/` | The legacy compiler's own link stub: origin 0, `fixed_lo` at 16, `fixed_hi` at 32. An imported fixture's bytes mean the same thing here as they did there. |
| `atarixl.cfg` | `k65/` | The Atari XL map the oracle fixtures were built with, `-lowAddr 0x2000`. |
| `a2600.cfg` | `k65/whole-program/` | The 2600's 4 KiB cartridge at `$F000`, with the entry stub and vector table the system reserves. |

## Re-deriving an oracle golden

```sh
K65_HOME=~/devel/X65/k65 tests/compat/oracle.sh tests/compat/k65/<fixture>
```

`out/k65.exe` must be built in that checkout, the checkout must be at the
revision `oracle.lock` pins and its tree must be clean — the script refuses
otherwise, because a golden produced against an unknown compiler is not
evidence. Moving the pin is deliberate: edit the lock, re-run every fixture,
and review what moved. Note that `bin/compiler.exe` beside `out/k65.exe` is the
*grammar generator*, not the compiler. The script writes the
project file into the fixture directory and runs the compiler from `workdir/`,
because the original resolves both a unit path and an `image`/`binary` file name
against the project file's directory and needs `system_*.nut` on its own
working directory.

## What the suite does not claim

It does not assert that K816's bytes equal the original's. The two lay sections
out differently and the original's linker prepends a startup stub K816 does not
generate, so byte equality would be a false claim dressed as a test. A row's
`compatible` verdict is argued in the manifest, with `oracle.hex` as its
evidence; what the suite guards is that the argued behaviour does not change
unnoticed.
