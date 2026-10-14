#!/usr/bin/env python3
#*****************************************************************************
#
#                               ca65-widths.py
#
#       The register widths a ca65 source believes it runs at, per label
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
"""The register widths a ca65 source believes it runs at.

A 65816 program written for ca65 carries its width assumptions as
``.a8``/``.a16``/``.i8``/``.i16`` directives and ``sep``/``rep`` masks, in
source order, and ca65 checks nothing about them across a ``jsr``.  K816
makes those assumptions a contract on each function and bridges every call
to it, so the first thing a conversion needs to know is what each routine
assumed and what each of its callers assumed -- and where the two disagree,
because that is where K816 will either insert a ``rep``/``sep`` or refuse.

This reads the sources the way ca65 does -- top to bottom, one file at a
time, no control flow -- and reports:

- the state (``m``, ``x``, ``e``) at every label and ``.proc``, as the
  K816 annotation it suggests (``@a16 @i16``);
- the state at every ``jsr``/``jsl``/``jmp``/``brl`` site;
- across all the files given, every target whose callers do not all agree
  with each other or with the state at its definition.

The state is what *ca65 assembled with*, which is what a K816 annotation
has to reproduce for the bytes to come out the same: a directive stays in
force until the next one, through every ``rts`` and label, exactly as ca65
keeps it.  A ``sep``/``rep`` mask moves it too (what ``.smart`` would do),
and a ``sec``/``clc`` followed by ``xce`` sets ``e``; entering emulation
mode forces ``m`` and ``x`` to 8 bits, as the hardware does.  ``?`` is
"nothing stated yet", which is the state at the top of every file.

Usage::

    tools/ca65-widths.py [--calls] [--summary] FILE.asm...

``--calls`` lists call sites too (labels only by default); ``--summary``
prints only the cross-file disagreement table.
"""

import argparse
import re
import sys
from collections import defaultdict

LABEL = re.compile(r"^\s*([A-Za-z_@.][A-Za-z0-9_@.]*):(?![+-])")
PROC = re.compile(r"^\s*\.proc\s+([A-Za-z_][A-Za-z0-9_]*)", re.I)
TRANSFER = re.compile(r"^\s*(jsr|jsl|jmp|jml|brl)\s+([A-Za-z_][A-Za-z0-9_:]*)", re.I)
MASK = re.compile(r"^\s*(sep|rep)\s+#\s*(\$[0-9A-Fa-f]+|%[01]+|[0-9]+)", re.I)
DIRECTIVE = re.compile(r"^\s*\.(a8|a16|i8|i16|smart|p816|setcpu)\b", re.I)
FLAG = re.compile(r"^\s*(sec|clc|xce)\b", re.I)


def number(text):
    if text.startswith("$"):
        return int(text[1:], 16)
    if text.startswith("%"):
        return int(text[1:], 2)
    return int(text)


def strip_comment(line):
    out = []
    quote = None
    for character in line:
        if quote:
            out.append(character)
            if character == quote:
                quote = None
        elif character in "\"'":
            quote = character
            out.append(character)
        elif character == ";":
            break
        else:
            out.append(character)
    return "".join(out)


class State:
    """``m``, ``x`` and ``e`` as 8, 16 or None (unknown); ``e`` as 0, 1 or
    None."""

    def __init__(self):
        self.m = None
        self.x = None
        self.e = None
        self.carry = None  # the last sec/clc, for the next xce

    def annotation(self):
        parts = []
        parts.append("@a%s" % self.m if self.m else "@a?")
        parts.append("@i%s" % self.x if self.x else "@i?")
        if self.e == 1:
            parts.append("emulation")
        return " ".join(parts)

    def key(self):
        return (self.m, self.x, self.e)


def walk(path, lines, callback):
    """Runs ``callback(kind, name, line_number, state)`` for every label
    (``kind == "label"``) and transfer (``kind == "call"``) in source order,
    with the state ca65 would have believed at that line."""
    state = State()
    for number_, raw in enumerate(lines, 1):
        line = strip_comment(raw)
        if not line.strip():
            continue
        match = LABEL.match(line)
        if match:
            name = match.group(1)
            callback("label", name, number_, state)
            line = line[match.end():]
        match = PROC.match(line)
        if match:
            callback("label", match.group(1), number_, state)
            continue
        match = DIRECTIVE.match(line)
        if match:
            which = match.group(1).lower()
            if which == "a8":
                state.m = 8
            elif which == "a16":
                state.m = 16
            elif which == "i8":
                state.x = 8
            elif which == "i16":
                state.x = 16
            continue
        match = MASK.match(line)
        if match:
            mask = number(match.group(2))
            width = 8 if match.group(1).lower() == "sep" else 16
            if mask & 0x20:
                state.m = width
            if mask & 0x10:
                state.x = width
            continue
        match = FLAG.match(line)
        if match:
            which = match.group(1).lower()
            if which == "sec":
                state.carry = 1
            elif which == "clc":
                state.carry = 0
            else:
                state.e = state.carry
                if state.e == 1:
                    state.m = state.x = 8
                state.carry = None
            continue
        match = TRANSFER.match(line)
        if match:
            callback("call", match.group(2), number_, state)
            continue


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("files", nargs="+", metavar="FILE.asm")
    parser.add_argument("--calls", action="store_true", help="list call sites as well as labels")
    parser.add_argument("--summary", action="store_true", help="only the disagreement table")
    args = parser.parse_args(argv)

    definitions = {}  # label -> (file, line, state key)
    calls = defaultdict(list)  # target -> [(file, line, state key)]
    rows = []

    for path in args.files:
        try:
            with open(path, encoding="utf-8", errors="replace") as handle:
                lines = handle.read().splitlines()
        except OSError as error:
            print("%s: %s" % (path, error), file=sys.stderr)
            return 2

        def record(kind, name, line_number, state, path=path):
            if kind == "label":
                if not name.startswith("@") and not name.startswith("."):
                    definitions[name] = (path, line_number, state.key())
                rows.append((path, line_number, "", name, state.annotation()))
            else:
                target = name.split("::")[-1]
                calls[target].append((path, line_number, state.key()))
                rows.append((path, line_number, "call", target, state.annotation()))

        walk(path, lines, record)

    if not args.summary:
        width = max((len("%s:%d" % (row[0], row[1])) for row in rows), default=0)
        for path, line_number, kind, name, annotation in rows:
            if kind == "call" and not args.calls:
                continue
            where = "%s:%d" % (path, line_number)
            shown = "    %s" % name if kind == "call" else "%s:" % name
            print("%-*s  %-28s %s" % (width, where, shown, annotation))
        print()

    # The disagreements: a target reached at a state its definition did not
    # assume, or by callers that assume different states from each other.
    disagreements = []
    for target, sites in sorted(calls.items()):
        states = {site[2] for site in sites}
        defined = definitions.get(target)
        expected = defined[2] if defined else None
        if defined and states <= {expected}:
            continue
        if len(states) == 1 and expected is None and defined is None:
            # An external target, reached consistently: nothing to say.
            continue
        if len(states) == 1 and expected is not None and None in next(iter(states)):
            # The one caller does not know; neither can this.
            pass
        disagreements.append((target, defined, sites))

    if disagreements:
        print("Targets whose callers do not agree with their definition, or with each other:")
        for target, defined, sites in disagreements:
            if defined:
                print("  %s  defined %s:%d as %s" % (target, defined[0], defined[1], describe(defined[2])))
            else:
                print("  %s  (not defined in these files)" % target)
            for path, line_number, key in sites:
                print("      called from %s:%d at %s" % (path, line_number, describe(key)))
        return 1
    print("Every call site agrees with its target's definition.")
    return 0


def describe(key):
    m, x, e = key
    parts = ["@a%s" % m if m else "@a?", "@i%s" % x if x else "@i?"]
    if e == 1:
        parts.append("emulation")
    return " ".join(parts)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
