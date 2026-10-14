#!/usr/bin/env python3
#*****************************************************************************
#
#                               k816-compare.py
#
#        Two linked programs compared symbol by symbol, bytes and code
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
"""Two linked programs compared symbol by symbol.

A program being carried from ca65 to K816 one file at a time has, at every
step, a previous build that worked.  The question after each converted file
is not "does it link" but "which routines changed, and how" -- and the
answer has to be given in instructions, because a changed byte in a width
mask is a different bug from a changed byte in an operand.

This reads the two debug files ld65 wrote (``cl65 -g -Wl --dbgfile,F``),
locates every segment's bytes inside the two output images through the
``oname``/``ooffs`` the debug file records, and compares the bytes of each
symbol -- a label's bytes run to the next label of its segment -- between
the two builds.  Where they differ, both sides are disassembled and shown
side by side, with the first differing instruction marked.

The disassembly is width-aware the only way it can be without the source:
the M and X flags start at ``--widths`` (``a8,i8``, the entry ABI K816
assumes) at the top of each segment and follow every ``sep``/``rep`` in
instruction order through the segment.  Where that assumption is wrong the
listing goes wrong with it, visibly -- the hex beside it is what to trust.

Usage::

    tools/k816-compare.py OLD.dbg NEW.dbg [--widths a16,i16] [--context N]
                          [--old-image FILE] [--new-image FILE]

Exit status is 0 when every shared symbol's bytes are identical, 1 when
any differ, 2 on a bad input.
"""

import argparse
import os
import sys

# ---------------------------------------------------------------------------
# The debug file
# ---------------------------------------------------------------------------


def split_fields(text):
    fields, current, quoted = [], [], False
    for character in text:
        if character == '"':
            quoted = not quoted
            current.append(character)
        elif character == "," and not quoted:
            fields.append("".join(current))
            current = []
        else:
            current.append(character)
    if current:
        fields.append("".join(current))
    return fields


def parse_number(text):
    return int(text, 16) if text.startswith("0x") else int(text)


class Database:
    def __init__(self, path):
        self.path = path
        self.segments = {}  # id -> dict
        self.symbols = []  # dicts with type=lab
        self.typed = set()  # (segment id, offset) of every span ca65 typed: data
        with open(path, encoding="utf-8", errors="replace") as handle:
            for line in handle:
                if "\t" not in line:
                    continue
                kind, rest = line.rstrip("\n").split("\t", 1)
                fields = {}
                for field in split_fields(rest):
                    if "=" in field:
                        key, value = field.split("=", 1)
                        fields[key] = value.strip('"')
                if kind == "seg":
                    self.segments[parse_number(fields["id"])] = fields
                elif kind == "sym" and fields.get("type") == "lab" and "seg" in fields:
                    self.symbols.append(fields)
                elif kind == "span" and "type" in fields:
                    self.typed.add((parse_number(fields["seg"]), parse_number(fields["start"])))
        if not self.symbols:
            raise SystemExit(
                "%s: the debug file names no symbols: assemble with `-g` (`cl65 -g`)" % path
            )

    def image_bytes(self, image_override):
        """Segment id -> bytes, for every segment that reached the image."""
        images = {}
        out = {}
        for sid, segment in self.segments.items():
            if "oname" not in segment:
                continue
            name = segment["oname"]
            if name not in images:
                path = image_override or os.path.join(os.path.dirname(self.path), name)
                try:
                    with open(path, "rb") as handle:
                        images[name] = handle.read()
                except OSError as error:
                    raise SystemExit("%s: cannot read the image %s: %s" % (self.path, path, error))
            start = parse_number(segment["ooffs"])
            size = parse_number(segment["size"])
            out[sid] = images[name][start : start + size]
        return out

    def spans(self):
        """name -> (segment name, address, bytes offset in segment, length)
        for every label; a label's bytes run to the next label in its
        segment, or to the segment's end."""
        by_segment = {}
        for symbol in self.symbols:
            name = symbol["name"]
            if name.startswith("@"):
                continue  # a cheap local: part of the routine above it
            sid = parse_number(symbol["seg"])
            by_segment.setdefault(sid, []).append((parse_number(symbol["val"]), name))
        spans = {}
        for sid, labels in by_segment.items():
            segment = self.segments[sid]
            start = parse_number(segment["start"])
            size = parse_number(segment["size"])
            labels.sort()
            for index, (address, name) in enumerate(labels):
                end = labels[index + 1][0] if index + 1 < len(labels) else start + size
                data = segment.get("type") != "ro" or (sid, address - start) in self.typed
                spans[name] = (segment["name"], sid, address, address - start, max(0, end - address), data)
        return spans


# ---------------------------------------------------------------------------
# A 65816 disassembler, width-aware by assumption
# ---------------------------------------------------------------------------

TABLE = {}


def _fill():
    alu = ["ORA", "AND", "EOR", "ADC", "STA", "LDA", "CMP", "SBC"]
    for index, name in enumerate(alu):
        base = index * 0x20
        for low, mode in [
            (0x01, "(dp,x)"), (0x03, "sr"), (0x05, "dp"), (0x07, "[dp]"), (0x09, "immM"),
            (0x0D, "abs"), (0x0F, "long"), (0x11, "(dp),y"), (0x12, "(dp)"), (0x13, "(sr),y"),
            (0x15, "dp,x"), (0x17, "[dp],y"), (0x19, "abs,y"), (0x1D, "abs,x"), (0x1F, "long,x"),
        ]:
            TABLE[base + low] = (name, mode)
    TABLE[0x89] = ("BIT", "immM")
    for base, name in [(0x00, "ASL"), (0x20, "ROL"), (0x40, "LSR"), (0x60, "ROR")]:
        for low, mode in [(0x06, "dp"), (0x0A, "acc"), (0x0E, "abs"), (0x16, "dp,x"), (0x1E, "abs,x")]:
            TABLE[base + low] = (name, mode)
    singles = {
        0x00: ("BRK", "imm8"), 0x02: ("COP", "imm8"), 0x04: ("TSB", "dp"), 0x08: ("PHP", "imp"),
        0x0C: ("TSB", "abs"), 0x10: ("BPL", "rel"), 0x14: ("TRB", "dp"), 0x18: ("CLC", "imp"),
        0x0B: ("PHD", "imp"), 0x1A: ("INC", "acc"), 0x1B: ("TCS", "imp"), 0x1C: ("TRB", "abs"),
        0x20: ("JSR", "abs"), 0x22: ("JSL", "long"), 0x24: ("BIT", "dp"), 0x28: ("PLP", "imp"),
        0x2B: ("PLD", "imp"), 0x2C: ("BIT", "abs"), 0x30: ("BMI", "rel"), 0x34: ("BIT", "dp,x"), 0x38: ("SEC", "imp"),
        0x3A: ("DEC", "acc"), 0x3B: ("TSC", "imp"), 0x3C: ("BIT", "abs,x"),
        0x40: ("RTI", "imp"), 0x42: ("WDM", "imm8"), 0x44: ("MVP", "move"), 0x48: ("PHA", "imp"),
        0x4B: ("PHK", "imp"), 0x4C: ("JMP", "abs"), 0x50: ("BVC", "rel"), 0x54: ("MVN", "move"),
        0x58: ("CLI", "imp"), 0x5A: ("PHY", "imp"), 0x5B: ("TCD", "imp"), 0x5C: ("JML", "long"),
        0x60: ("RTS", "imp"), 0x62: ("PER", "rell"), 0x64: ("STZ", "dp"), 0x68: ("PLA", "imp"),
        0x6B: ("RTL", "imp"), 0x6C: ("JMP", "(abs)"), 0x70: ("BVS", "rel"), 0x74: ("STZ", "dp,x"),
        0x78: ("SEI", "imp"), 0x7A: ("PLY", "imp"), 0x7B: ("TDC", "imp"), 0x7C: ("JMP", "(abs,x)"),
        0x80: ("BRA", "rel"), 0x82: ("BRL", "rell"), 0x84: ("STY", "dp"), 0x86: ("STX", "dp"),
        0x88: ("DEY", "imp"), 0x8A: ("TXA", "imp"), 0x8B: ("PHB", "imp"), 0x8C: ("STY", "abs"),
        0x8E: ("STX", "abs"), 0x90: ("BCC", "rel"), 0x94: ("STY", "dp,x"), 0x96: ("STX", "dp,y"),
        0x98: ("TYA", "imp"), 0x9A: ("TXS", "imp"), 0x9B: ("TXY", "imp"), 0x9C: ("STZ", "abs"),
        0x9E: ("STZ", "abs,x"),
        0xA0: ("LDY", "immX"), 0xA2: ("LDX", "immX"), 0xA4: ("LDY", "dp"), 0xA6: ("LDX", "dp"),
        0xA8: ("TAY", "imp"), 0xAA: ("TAX", "imp"), 0xAB: ("PLB", "imp"), 0xAC: ("LDY", "abs"),
        0xAE: ("LDX", "abs"), 0xB0: ("BCS", "rel"), 0xB4: ("LDY", "dp,x"), 0xB6: ("LDX", "dp,y"),
        0xB8: ("CLV", "imp"), 0xBA: ("TSX", "imp"), 0xBB: ("TYX", "imp"), 0xBC: ("LDY", "abs,x"),
        0xBE: ("LDX", "abs,y"),
        0xC0: ("CPY", "immX"), 0xC2: ("REP", "imm8"), 0xC4: ("CPY", "dp"), 0xC6: ("DEC", "dp"),
        0xC8: ("INY", "imp"), 0xCA: ("DEX", "imp"), 0xCB: ("WAI", "imp"), 0xCC: ("CPY", "abs"),
        0xCE: ("DEC", "abs"), 0xD0: ("BNE", "rel"), 0xD4: ("PEI", "(dp)"), 0xD6: ("DEC", "dp,x"),
        0xD8: ("CLD", "imp"), 0xDA: ("PHX", "imp"), 0xDB: ("STP", "imp"), 0xDC: ("JML", "[abs]"),
        0xDE: ("DEC", "abs,x"),
        0xE0: ("CPX", "immX"), 0xE2: ("SEP", "imm8"), 0xE4: ("CPX", "dp"), 0xE6: ("INC", "dp"),
        0xE8: ("INX", "imp"), 0xEA: ("NOP", "imp"), 0xEB: ("XBA", "imp"), 0xEC: ("CPX", "abs"),
        0xEE: ("INC", "abs"), 0xF0: ("BEQ", "rel"), 0xF4: ("PEA", "abs"), 0xF6: ("INC", "dp,x"),
        0xF8: ("SED", "imp"), 0xFA: ("PLX", "imp"), 0xFB: ("XCE", "imp"), 0xFC: ("JSR", "(abs,x)"),
        0xFE: ("INC", "abs,x"),
    }
    TABLE.update(singles)
    assert len(TABLE) == 256, len(TABLE)


_fill()

OPERAND_BYTES = {
    "imp": 0, "acc": 0, "imm8": 1, "dp": 1, "dp,x": 1, "dp,y": 1, "(dp,x)": 1, "(dp)": 1,
    "(dp),y": 1, "[dp]": 1, "[dp],y": 1, "sr": 1, "(sr),y": 1, "rel": 1,
    "abs": 2, "abs,x": 2, "abs,y": 2, "(abs)": 2, "(abs,x)": 2, "[abs]": 2, "rell": 2, "move": 2,
    "long": 3, "long,x": 3,
}


class Widths:
    def __init__(self, m, x):
        self.m = m
        self.x = x

    def copy(self):
        return Widths(self.m, self.x)


def parse_widths(text):
    widths = Widths(8, 8)
    for part in text.split(","):
        part = part.strip().lower()
        if part in ("a8", "a16"):
            widths.m = int(part[1:])
        elif part in ("i8", "i16"):
            widths.x = int(part[1:])
        elif part:
            raise SystemExit("--widths: `%s` is not a8, a16, i8 or i16" % part)
    return widths


def format_operand(mode, operand, address, length):
    value = int.from_bytes(operand, "little") if operand else 0
    if mode in ("imp", "acc"):
        return ""
    if mode in ("imm8", "immM", "immX"):
        return "#$%0*X" % (len(operand) * 2, value)
    if mode == "rel":
        target = (address + length + (value - 0x100 if value >= 0x80 else value)) & 0xFFFF
        return "$%04X" % target
    if mode == "rell":
        target = (address + length + (value - 0x10000 if value >= 0x8000 else value)) & 0xFFFF
        return "$%04X" % target
    if mode == "move":
        return "$%02X,$%02X" % (operand[1], operand[0])
    text = "$%0*X" % (len(operand) * 2, value)
    return mode.replace("dp", text).replace("abs", text).replace("long", text).replace("sr", text + ",s")


def disassemble(data, address, widths):
    """Yields (address, bytes, text) over ``data``, tracking ``sep``/``rep``
    in ``widths`` (mutated)."""
    offset = 0
    while offset < len(data):
        opcode = data[offset]
        name, mode = TABLE[opcode]
        if mode == "immM":
            size = 2 if widths.m == 16 else 1
        elif mode == "immX":
            size = 2 if widths.x == 16 else 1
        else:
            size = OPERAND_BYTES[mode]
        raw = data[offset : offset + 1 + size]
        if len(raw) < 1 + size:
            yield (address + offset, raw, "; %d trailing byte(s)" % len(raw))
            return
        operand = raw[1:]
        text = ("%s %s" % (name, format_operand(mode, operand, address + offset, 1 + size))).rstrip()
        if name in ("SEP", "REP") and operand:
            mask = operand[0]
            width = 8 if name == "SEP" else 16
            if mask & 0x20:
                widths.m = width
            if mask & 0x10:
                widths.x = width
        yield (address + offset, raw, text)
        offset += 1 + size


# ---------------------------------------------------------------------------
# The comparison
# ---------------------------------------------------------------------------


def listing(data, address, widths, is_data):
    """Rows of (address, hex, text): instructions for code, eight bytes per
    row and no text for data, which a disassembler would only misread."""
    rows = []
    if is_data:
        for offset in range(0, len(data), 8):
            chunk = data[offset : offset + 8]
            rows.append((address + offset, " ".join("%02X" % byte for byte in chunk), ""))
        return rows
    for at, raw, text in disassemble(data, address, widths):
        rows.append((at, " ".join("%02X" % byte for byte in raw), text))
    return rows


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("old", metavar="OLD.dbg")
    parser.add_argument("new", metavar="NEW.dbg")
    parser.add_argument("--widths", default="a8,i8", help="the M/X state at the top of each segment")
    parser.add_argument("--old-image", help="the old output file, if not beside its debug file")
    parser.add_argument("--new-image", help="the new output file, if not beside its debug file")
    parser.add_argument("--context", type=int, default=3, help="instructions shown around a difference")
    parser.add_argument("--all", action="store_true", help="list identical symbols too")
    args = parser.parse_args(argv)

    old = Database(args.old)
    new = Database(args.new)
    old_bytes = old.image_bytes(args.old_image)
    new_bytes = new.image_bytes(args.new_image)
    old_spans = old.spans()
    new_spans = new.spans()

    # Segments first: a size that moved says where to look.
    print("Segments:")
    old_segments = {s["name"]: s for s in old.segments.values()}
    new_segments = {s["name"]: s for s in new.segments.values()}
    for name in sorted(set(old_segments) | set(new_segments)):
        o = old_segments.get(name)
        n = new_segments.get(name)
        osize = parse_number(o["size"]) if o else None
        nsize = parse_number(n["size"]) if n else None
        if o and n:
            same = old_bytes.get(parse_number(o["id"])) == new_bytes.get(parse_number(n["id"]))
            note = "identical" if same and osize == nsize else ("same size, bytes differ" if osize == nsize else "%+d bytes" % (nsize - osize))
            print("  %-12s %5d -> %5d  %s" % (name, osize, nsize, note))
        else:
            print("  %-12s %s" % (name, "only in old (%d bytes)" % osize if o else "only in new (%d bytes)" % nsize))
    print()

    # Then every symbol. The disassembly state is tracked per segment, in
    # address order, so a routine's listing starts at the widths the
    # routines before it left behind.
    differing = 0
    states_old = {}
    states_new = {}
    rows = []
    for name in sorted(set(old_spans) | set(new_spans), key=lambda n: (old_spans.get(n) or new_spans.get(n))[2]):
        o = old_spans.get(name)
        n = new_spans.get(name)
        if o and not n:
            rows.append((name, "only in old  %s $%04X %d bytes" % (o[0], o[2], o[4]), None))
            continue
        if n and not o:
            rows.append((name, "only in new  %s $%04X %d bytes" % (n[0], n[2], n[4]), None))
            continue
        ob = old_bytes.get(o[1], b"")[o[3] : o[3] + o[4]]
        nb = new_bytes.get(n[1], b"")[n[3] : n[3] + n[4]]
        if ob == nb:
            rows.append((name, "identical    %s $%04X %d bytes" % (o[0], o[2], o[4]), None))
            continue
        differing += 1
        widths_old = states_old.setdefault(o[1], parse_widths(args.widths))
        widths_new = states_new.setdefault(n[1], parse_widths(args.widths))
        rows.append((name, "DIFFERS      %s $%04X %d bytes -> $%04X %d bytes" % (o[0], o[2], len(ob), n[2], len(nb)), (ob, nb, o[2], n[2], widths_old.copy(), widths_new.copy(), o[5] or n[5])))

    for name, summary, detail in rows:
        if detail is None and not args.all and summary.startswith("identical"):
            continue
        print("%-24s %s" % (name, summary))
        if detail is None:
            continue
        ob, nb, oaddr, naddr, wo, wn, is_data = detail
        left = listing(ob, oaddr, wo, is_data)
        right = listing(nb, naddr, wn, is_data)
        first = next((i for i, (a, b) in enumerate(zip(left, right)) if a[1] != b[1]), min(len(left), len(right)))
        lo = max(0, first - args.context)
        hi = max(len(left), len(right)) if args.context < 0 else min(max(len(left), len(right)), first + args.context + 1)
        for index in range(lo, hi):
            l = left[index] if index < len(left) else None
            r = right[index] if index < len(right) else None
            mark = "=>" if index == first else "  "
            lt = ("%04X  %-23s %s" % (l[0], l[1], l[2])).rstrip() if l else ""
            rt = ("%04X  %-23s %s" % (r[0], r[1], r[2])).rstrip() if r else ""
            print("    %s %-48s | %s" % (mark, lt, rt))
        print()

    print()
    shared = len(set(old_spans) & set(new_spans))
    print("%d shared symbols, %d differ; %d only in old, %d only in new" % (
        shared, differing, len(set(old_spans) - set(new_spans)), len(set(new_spans) - set(old_spans))))
    return 1 if differing else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
