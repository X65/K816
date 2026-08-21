# The K65 compatibility corpus

This is sub-plan G's corpus (`plans/06-compatibility.md`). Every directory
under one of the three sets below is one fixture: sources, a `fixture.txt`
that says what K816 does with them and why, and — where there is one — a
committed golden.

`plans/compat-manifest.md` classifies every rule of the original K65 grammar
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

`cargo test` never runs the original compiler. `oracle.sh` is how those bytes
were produced and how they are re-produced; the header of each golden carries
the exact invocation, so a reader can check provenance without taking the
committed bytes on faith (decision 2).

`a2600-tutorial-03/` is a whole program vendored from the original
distribution rather than a fixture written here; `PROVENANCE.md` in that
directory says where it came from and why it lives here and not under
`examples/`.

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
| How does a value cross an integer boundary? | `i64(floor(f32(v) + 0.5f32))` — round half **up**, over `f64` arithmetic between `f32` ends. | `k65/eval-integer-boundary` |
| What does `var X ?` emit? | **Nothing.** It prints the variable's address at compile time. | `k65/declaration-comma-lists` |
| Is `align` accepted at the start of a `func`? | **Yes**, and so is `address`, in `func`, `naked`, `main` and `data` alike. | `k65/align-and-address` |
| What does `align 256 + 8` place? | The section at an address **congruent to 8 modulo 256**. | `k65/align-and-address` |
| What does `#if UNDEFINED` do? | **Errors** — "Undeclared eval variable". | measured in G0; a fixture cannot hold a golden for a build that fails |
| What bytes does `.LT+1 = a` produce? | `sta .LT+1`, absolute, **no diagnostic**. | `k65/label-offset-targets` |
| What is a method call? | **Sugar**: `x.f(a)` is `f(x, a)` and `x.f` is `f(x)`. | `k65/eval-forms` |
| What is a string value? | A value **inside** an evaluation — `+`, `==`, `<`, `print`, `error` — that cannot cross an integer boundary. | `k65/eval-forms` |
| What does an `evaluator [ ]` data entry do? | Installs a **per-byte transform** on the section, over the bytes it *generates* and not over its literal entries. | `k65/eval-forms` |
| Is `evalfunc` recursion bounded? | **No — it is broken.** A self-call resolves to evaluator function zero. | `k65/eval-statements` |
| What does `* N` emit? | **N cycles** of wait, spending an odd one on `NOP $80` — opcode `$04`, which is `TSB dp` on the 65816. | `k65/nop-and-wait` |

Three more things the original does not have, found on the way and recorded in
`plans/compat-manifest.md`: no `abs`, no `||` (both logical operators are
spelled `&&` in its grammar), and `%` is a non-negative modulo.

## `fixture.txt`

`key value` lines up to the first blank line, then prose for a reader:

    row       the manifest row this fixture answers to
    config    the ld65 configuration to link with
    compare   `diagnostics` or `bytes`
    units     the sources, in order; the last one is the compiled unit and the
              others are its siblings, which is what `--unit` means on the
              command line
    expect    one diagnostic code, repeated - in the order K816 reports them

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
| `a2600.cfg` | `k65/a2600-tutorial-03/` | The 2600's 4 KiB cartridge at `$F000`, with the entry stub and vector table the system reserves. |

## Re-deriving an oracle golden

```sh
K65_HOME=~/devel/X65/k65 tests/compat/oracle.sh tests/compat/k65/<fixture>
```

`out/k65.exe` must be built in that checkout, and note that `bin/compiler.exe`
beside it is the *grammar generator*, not the compiler. The script writes the
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
