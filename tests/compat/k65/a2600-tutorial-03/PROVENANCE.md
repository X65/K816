# `a2600-tutorial-03` — vendored from the original K65 distribution

`_defs.k65`, `main.k65` and `sprites.bmp` are copied verbatim from
`workdir/a2600-tutorial-03/` of the original K65 compiler's source tree
(<http://devkk.net/wiki/index.php/K65>, authors: KK, DevOps: Traq).  The
checkout this copy was taken from carries no licence file, and the upstream
`Readme.md` states none.

It is here as a **compatibility fixture**, not as a K816 example: the user
ruled explicitly (`plans/06-compatibility.md` decision 2) that it lives under
`tests/compat/k65/` and never under `examples/`, which is the tree's showcase
of K816 programs.

It earns its place because it is the one real K65 program in reach that
exercises `bank`, `var` comma lists, bare-call statement lines, `image` with
`tiles`, `align`, and the evaluator, all at once — the constructs sub-plan G
closes, in a program someone actually wrote rather than in fixtures written to
argue about them.

`oracle.hex` is the original compiler's own output for these sources, produced
with `tests/compat/oracle.sh` against the `A2600` system: a raw 4 KiB
cartridge image of `$F000..$FFFF`, no framing.  It is what "K816 compiles this
the same way" will be measured against.
