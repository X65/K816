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
# goldens are committed bytes; this script is how they were produced and how
# they are re-produced when a fixture changes.
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
#
# The script writes `oracle.hex` (the program image with its provenance
# header) and `oracle.lst` (the compiler's listing) into the fixture
# directory.
#
# The K65 checkout is found through $K65_HOME, defaulting to ~/devel/X65/k65.
# `out/k65.exe` must be built and the compiler must run from `workdir/`, so
# that `system_*.nut` resolves (probe 7).

set -e

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

# Run from `workdir/` even for the banner: the compiler writes a `program.gmap`
# and a `test_raw.sym` into whatever directory it starts in.
VERSION=$( ( cd "$WORKDIR" && "$K65" ) 2>&1 \
    | sed -n 's/^K65 compiler, \(.*\)\.$/\1/p' | head -1 | tr -d '\r')

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

for dir in "$@"; do
    dir=$(cd "$dir" && pwd)
    conf=$dir/oracle.conf
    [ -f "$conf" ] || { echo "oracle.sh: $conf is missing" >&2; exit 1; }

    system=$(awk '$1=="system"  {print $2}' "$conf")
    lowaddr=$(awk '$1=="lowaddr" {print $2}' "$conf")
    bank=$(awk '$1=="bank"    {print $2}' "$conf")
    units=$(awk '$1=="units"   {$1=""; print}' "$conf")
    : "${system:=AtariXL}" "${bank:=main}"

    name=$(basename "$dir")
    out=$(mktemp -d)
    # The project file goes into the fixture directory and names its units
    # relatively, because the original resolves both a unit path and an
    # `image`/`binary` file name against the project file's directory - which
    # is how the a2600 tutorial finds its `sprites.bmp`.
    proj=$dir/$name.k65proj

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

    ( cd "$WORKDIR" && "$K65" "@$proj" ) > "$out/log" 2>&1 || {
        echo "oracle.sh: the original compiler rejected $name:" >&2
        sed -n '/^Compiling file/,$p' "$out/log" >&2
        rm -f "$proj"
        exit 1
    }

    image=$out/$name.bin
    set -- $(framing "$system" "$image")
    origin=$1; length=$2; skip=$3

    hex=$dir/oracle.hex
    {
        echo "# Oracle golden - the bytes the original K65 compiler produced."
        echo "#"
        echo "# This file is generated by tests/compat/oracle.sh and is committed so"
        echo "# that the suite does not depend on a compiler outside the tree"
        echo "# (plans/06-compatibility.md decision 2).  \`cargo test\` never runs the"
        echo "# original; a reader who wants to check provenance re-runs the command"
        echo "# below."
        echo "#"
        echo "# fixture   $name"
        echo "# compiler  $VERSION"
        echo "# command   cd \$K65_HOME/workdir && ../out/k65.exe @<this-directory>/$name.k65proj"
        echo "# project"
        sed 's/^/#     /; s|'"$out"'/|./|g; s/[ \t]*$//' "$proj" \
            | sed 's/^#     $/#/'
        echo "#"
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
            echo "; rather than at the bytes.  Generated by tests/compat/oracle.sh."
            echo ";"
            sed 's|'"$dir"'/|./|g' "$out/$name.lst"
        } > "$dir/oracle.lst"
    fi

    rm -rf "$out" "$proj"
    echo "oracle.sh: $name -> $(basename "$hex") ($length bytes at $(printf '$%04X' "$origin"))"
done
