#!/bin/sh
#*****************************************************************************
#
#                                 oracle.sh
#
#         Re-derives an oracle golden by running the original K65 compiler
#
#
# Copyright (c) 2026 Tomasz Sterna (smokku)
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
#*****************************************************************************
#
# The original K65 compiler is sub-plan G's behaviour oracle: where a
# document is silent about what a K65 construct means, this script asks the
# compiler and freezes the answer beside the fixture.
#
# `cargo test` never runs this (plans/06-compatibility.md decision 2).  The
# goldens are committed; this script is how they were produced and how they are
# re-produced when a fixture changes.  **A committed golden is evidence only if
# it is bound to what produced it**, so each one records
#
#   - the pinned K65 revision from `oracle.lock`, which this script refuses to
#     run against a checkout that does not match, and
#   - an Adler-32 checksum of every input the build reads.
#
# `tests/compat.rs` re-checks both without running anything: a source edited
# without re-running this script fails, and so does a golden produced against
# a different compiler.
#
# Usage:
#     tests/compat/oracle.sh tests/compat/k65/<fixture>...
#
# Each fixture directory holds its sources and an `oracle.conf` of `key value`
# lines:
#
#     system   AtariXL        # -system: AtariXL, A2600, C64, Plus4, Vic20,
#                             #          AtariXL16Cart
#     lowaddr  0x2000         # -lowAddr, AtariXL only
#     bank     main           # the bank every unit is compiled into
#     units    defs.k65 input.k65
#     expect   failure        # optional: the original is *supposed* to reject
#                             #           this source, and the refusal is the
#                             #           golden
#
# The script writes `oracle.hex` (the program image with its provenance
# header) and `oracle.lst` (the compiler's listing), or `oracle.err` for a
# fixture that expects a refusal.
#
# The K65 checkout is found through $K65_HOME, defaulting to ~/devel/X65/k65.
# `out/k65.exe` must be built and the compiler must run from `workdir/`, so
# that `system_*.nut` resolves (probe 7).

set -e

HERE=$(cd "$(dirname "$0")" && pwd)
LOCK=$HERE/oracle.lock

K65_HOME=${K65_HOME:-$HOME/devel/X65/k65}
K65=$K65_HOME/out/k65.exe
WORKDIR=$K65_HOME/workdir

if [ ! -x "$K65" ]; then
    echo "oracle.sh: $K65 is missing or not executable." >&2
    echo "  Build the original compiler (make -C $K65_HOME) or set K65_HOME." >&2
    echo "  Note that bin/compiler.exe is the *grammar generator*, not the" >&2
    echo "  compiler: out/k65.exe is the compiler." >&2
    exit 1
fi
if [ ! -d "$WORKDIR" ]; then
    echo "oracle.sh: $WORKDIR is missing; the compiler must run from it." >&2
    exit 1
fi

[ $# -gt 0 ] || { echo "usage: oracle.sh <fixture-dir>..." >&2; exit 2; }

# ---------------------------------------------------------------------------
# The pin
#
# A golden is only evidence about a *particular* compiler.  `oracle.lock` names
# which one, and a checkout that is not it — a different revision, or a dirty
# tree — is refused rather than quietly producing goldens nothing can trace.
# Moving the pin is a deliberate act: edit the lock, re-run every fixture, and
# review the goldens that moved.
# ---------------------------------------------------------------------------
PINNED=$(awk '$1=="k65-revision" {print $2}' "$LOCK")
REVISION=$(git -C "$K65_HOME" rev-parse HEAD 2>/dev/null || echo unknown)
if [ -n "$(git -C "$K65_HOME" status --porcelain --untracked-files=no 2>/dev/null)" ]; then
    REVISION="$REVISION-dirty"
fi
if [ "$REVISION" != "$PINNED" ]; then
    echo "oracle.sh: $K65_HOME is at $REVISION;" >&2
    echo "  tests/compat/oracle.lock pins $PINNED." >&2
    echo "  Check out the pinned revision, or move the pin deliberately and" >&2
    echo "  re-run every fixture under tests/compat/k65/." >&2
    exit 1
fi

# Run from `workdir/` even for the banner: the compiler writes a `program.gmap`
# and a `test_raw.sym` into whatever directory it starts in.
VERSION=$( ( cd "$WORKDIR" && "$K65" ) 2>&1 \
    | sed -n 's/^K65 compiler, \(.*\)\.$/\1/p' | head -1 | tr -d '\r')
PINNED_VERSION=$(sed -n 's/^k65-version  *//p' "$LOCK")
if [ "$VERSION" != "$PINNED_VERSION" ]; then
    echo "oracle.sh: the compiler reports '$VERSION';" >&2
    echo "  tests/compat/oracle.lock pins '$PINNED_VERSION'." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Adler-32, as a staleness detector rather than a security primitive — the
# framing `tests/corpus.rs` already uses for the generated-parser fingerprint.
# It is here rather than a stronger hash because both this script and
# `tests/compat.rs` have to compute it, and this one has only awk.
# ---------------------------------------------------------------------------
adler32() {
    od -An -tu1 -v "$1" | awk '
        BEGIN { a = 1; b = 0 }
        { for (i = 1; i <= NF; i++) { a = (a + $i) % 65521; b = (b + a) % 65521 } }
        END { printf "%04X%04X", b, a }'
}

# Files a fixture directory holds that are *not* inputs to the original: this
# script's own output, and K816's side of the fixture.
is_generated() {
    case $1 in
    oracle.hex|oracle.lst|oracle.err|fixture.txt|expected.hex|PROVENANCE.md|*.k65proj)
        return 0 ;;
    *) return 1 ;;
    esac
}

inputs_of() {
    for path in "$1"/*; do
        [ -f "$path" ] || continue
        name=${path##*/}
        is_generated "$name" && continue
        printf '%s\n' "$name"
    done
}

# ---------------------------------------------------------------------------
# byte at offset $1 of file $2, as decimal
# ---------------------------------------------------------------------------
byte_at() {
    od -An -tu1 -j "$1" -N 1 "$2" | tr -d ' \n'
}

# ---------------------------------------------------------------------------
# `origin length skip` for the image inside the compiler's output file.
#
# AtariXL emits an Atari executable: FFFF, then per segment a start word, an
# end word and the bytes between them.  The program is the first segment; the
# trailing six bytes are the RUNAD segment at $02E0, which is the Atari
# loader's, not the program's, and are dropped.
#
# A2600 emits the raw cartridge image of $F000..$FFFF with no framing at all.
# ---------------------------------------------------------------------------
framing() {
    case $1 in
    A2600)
        echo "61440 4096 0"
        ;;
    *)
        _lo=$(byte_at 2 "$2"); _hi=$(byte_at 3 "$2")
        _start=$((_hi * 256 + _lo))
        _lo=$(byte_at 4 "$2"); _hi=$(byte_at 5 "$2")
        _end=$((_hi * 256 + _lo))
        echo "$_start $((_end - _start + 1)) 6"
        ;;
    esac
}

# ---------------------------------------------------------------------------
# The header every golden carries.  Written by `provenance <dir> <name>`.
# ---------------------------------------------------------------------------
provenance() {
    _dir=$1; _name=$2
    echo "# This file is generated by tests/compat/oracle.sh and is committed so"
    echo "# that the suite does not depend on a compiler outside the tree"
    echo "# (plans/06-compatibility.md decision 2).  \`cargo test\` never runs the"
    echo "# original; it re-checks the revision and the checksums below, so a"
    echo "# source edited without re-running the script fails."
    echo "#"
    echo "# fixture   $_name"
    echo "# compiler  $VERSION"
    echo "# revision  $REVISION"
    echo "# command   cd \$K65_HOME/workdir && ../out/k65.exe @<this-directory>/$_name.k65proj"
    inputs_of "$_dir" | while read -r _input; do
        printf '# input     %-16s adler32 %s  %s bytes\n' \
            "$_input" "$(adler32 "$_dir/$_input")" \
            "$(wc -c < "$_dir/$_input" | tr -d ' ')"
    done
    echo "# project"
    # The scratch directory the output goes to is this run's, not the
    # fixture's, so it is rewritten to `./` - a golden that changed because
    # mktemp picked a different name is a golden nobody can trust.
    sed 's|'"$out"'/|./|g; s/^/#     /; s/[ \t]*$//' "$_dir/$_name.k65proj" \
        | sed 's/^#     $/#/'
    echo "#"
}

STAGED=""

for dir in "$@"; do
    dir=$(cd "$dir" && pwd)
    conf=$dir/oracle.conf
    [ -f "$conf" ] || { echo "oracle.sh: $conf is missing" >&2; exit 1; }

    system=$(awk '$1=="system"  {print $2}' "$conf")
    lowaddr=$(awk '$1=="lowaddr" {print $2}' "$conf")
    bank=$(awk '$1=="bank"    {print $2}' "$conf")
    units=$(awk '$1=="units"   {$1=""; print}' "$conf")
    expect=$(awk '$1=="expect"  {print $2}' "$conf")
    : "${system:=AtariXL}" "${bank:=main}" "${expect:=success}"

    name=$(basename "$dir")
    out=$(mktemp -d)
    # The project file goes into the fixture directory and names its units
    # relatively, because the original resolves a unit path against the
    # project file's directory.
    #
    # A `binary`'s file is different, and it was measured rather than assumed:
    # `loadbinary` opens the name as written, so it resolves against the
    # *working* directory — which has to be `workdir/` for `system_*.nut`.  A
    # fixture's data files are therefore staged there for the run and taken
    # away after, and a build left half-way through cannot leave one behind,
    # because the cleanup is a trap.  K816 resolves the same name against the
    # directory of the *source* that wrote it, which is the difference
    # `topelem-binary` records as `backend-different`.
    proj=$dir/$name.k65proj
    # Every input that is not a source and not this script's own output: a
    # `binary`'s data file, an `image`'s bitmap. Matched by exclusion rather
    # than by extension, because `BinLoad -> binary Ident` opens the
    # identifier's own text and a fixture's data file may have no extension
    # at all (`k65/binary-identifier`).
    for asset in "$dir"/*; do
        [ -f "$asset" ] || continue
        asset_name=${asset##*/}
        is_generated "$asset_name" && continue
        case " $units " in *" $asset_name "*) continue ;; esac
        [ "$asset_name" = oracle.conf ] && continue
        if [ -e "$WORKDIR/$asset_name" ]; then
            echo "oracle.sh: $WORKDIR/$asset_name is in the way of $name" >&2
            exit 1
        fi
        cp "$asset" "$WORKDIR/$asset_name"
        STAGED="$STAGED $WORKDIR/$asset_name"
        # shellcheck disable=SC2064
        trap "rm -f$STAGED" EXIT INT TERM
    done

    {
        echo "-system $system"
        [ -n "$lowaddr" ] && echo "-lowAddr $lowaddr"
        echo
        for unit in $units; do echo "$unit   $bank"; done
        echo
        echo "-o $out/$name.bin"
        echo
        echo "-link"
    } > "$proj"

    status=0
    ( cd "$WORKDIR" && "$K65" "@$proj" ) > "$out/log" 2>&1 || status=$?

    if [ "$expect" = failure ]; then
        if [ "$status" -eq 0 ]; then
            echo "oracle.sh: $name says \`expect failure\` but the original accepted it" >&2
            rm -rf "$out" "$proj"
            exit 1
        fi
        {
            provenance "$dir" "$name"
            echo "# The original is *expected* to reject this source: the refusal is what"
            echo "# the fixture is evidence for, and a build that started succeeding would"
            echo "# be as much of a change as different bytes."
            echo "#"
            echo "# Only the compiler's own \`Line N:\` and \`Linker:\` diagnostics are"
            echo "# kept.  It aborts rather than exiting on most of these — the C++"
            echo "# evaluator's error path unwinds through a bad free — and the"
            echo "# allocator's message on the way out is the C library's, not the"
            echo "# compiler's.  A \`Linker:\` line is kept because a refusal that happens"
            echo "# at link time is still a refusal, and \`binary-top-level\` is one."
            echo
            echo "exit $status"
            grep -E '^(Line |Linker:)' "$out/log" || echo "(no diagnostic before the abort)"
        } > "$dir/oracle.err"
        rm -f "$dir/oracle.hex" "$dir/oracle.lst"
        rm -rf "$out" "$proj"
        echo "oracle.sh: $name -> oracle.err (refused, exit $status)"
        continue
    fi

    if [ "$status" -ne 0 ]; then
        echo "oracle.sh: the original compiler rejected $name (exit $status):" >&2
        sed -n '/^Compiling file/,$p' "$out/log" >&2
        tail -3 "$out/log" >&2
        rm -f "$proj"
        exit 1
    fi

    image=$out/$name.bin
    set -- $(framing "$system" "$image")
    origin=$1; length=$2; skip=$3

    hex=$dir/oracle.hex
    {
        echo "# Oracle golden - the bytes the original K65 compiler produced."
        echo "#"
        provenance "$dir" "$name"
        case $system in
        A2600)
            echo "# The A2600 system writes the raw 4 KiB cartridge image of \$F000..\$FFFF"
            echo "# with no framing, so every byte below is the image."
            ;;
        *)
            echo "# The Atari executable framing - \$FFFF, the start and end words, and the"
            echo "# trailing RUNAD segment at \$02E0 - belongs to the Atari loader and is"
            echo "# stripped.  What follows is the program image alone."
            ;;
        esac
        echo "#"
        echo "# Sixteen bytes per line, load address in the left column."
        echo
        printf 'origin %04X\n' "$origin"
        od -An -tx1 -v -j "$skip" -N "$length" "$image" \
            | awk -v org="$origin" '
                { for (i = 1; i <= NF; i++) b[n++] = $i }
                END {
                    for (i = 0; i < n; i += 16) {
                        printf "%04X ", org + i
                        for (j = i; j < i + 16 && j < n; j++) printf " %s", b[j]
                        printf "\n"
                    }
                }' \
            | tr 'a-f' 'A-F'
    } > "$hex"

    if [ -f "$out/$name.lst" ]; then
        {
            echo "; Oracle listing - the original K65 compiler's own .lst for this"
            echo "; fixture, kept for the rows compared at the instruction sequence"
            echo "; rather than at the bytes.  Generated by tests/compat/oracle.sh;"
            echo "; oracle.hex beside it carries the provenance for both."
            echo ";"
            sed 's|'"$dir"'/|./|g' "$out/$name.lst"
        } > "$dir/oracle.lst"
    fi

    rm -f "$dir/oracle.err"
    rm -rf "$out" "$proj"
    echo "oracle.sh: $name -> oracle.hex ($length bytes at $(printf '$%04X' "$origin"))"
done
