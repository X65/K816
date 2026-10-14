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
        self.boundaries = {}  # segment id -> set of offsets a span starts or ends at
        self.all_symbols = []  # every sym record
        self.scopes = {}  # id -> dict
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
                elif kind == "sym":
                    self.all_symbols.append(fields)
                    if fields.get("type") == "lab" and "seg" in fields:
                        self.symbols.append(fields)
                elif kind == "scope":
                    self.scopes[parse_number(fields["id"])] = fields
                elif kind == "span":
                    sid = parse_number(fields["seg"])
                    start = parse_number(fields["start"])
                    edges = self.boundaries.setdefault(sid, set())
                    edges.add(start)
                    edges.add(start + parse_number(fields["size"]))
                    if "type" in fields:
                        self.typed.add((sid, start))
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


def decode(data, address, widths, ends=None, states=None, forget=()):
    """Yields (address, raw bytes, mnemonic, mode, operand bytes) over
    ``data``, tracking ``sep``/``rep`` in ``widths`` (mutated). A last
    instruction cut short by the end of ``data`` comes back with mnemonic
    ``None``. When ``states`` is a list, the (M, X) widths each instruction
    runs at are appended to it, ``None`` where nothing has said.

    ``ends``, when given, is the set of addresses at which ca65 recorded a
    line's bytes to end. A width-dependent immediate is then sized by
    whichever reading ends on one of them, and the tracked flags are only
    the tie-break: ca65 knew the width it assembled with, and the line
    table is where it left the answer."""
    offset = 0
    while offset < len(data):
        if address + offset in forget:
            # Something branches here, from a state this straight-line
            # reading did not follow.
            widths.m = widths.x = None
        opcode = data[offset]
        name, mode = TABLE[opcode]
        if mode in ("immM", "immX"):
            tracked = (widths.m if mode == "immM" else widths.x) == 16
            size = 2 if tracked else 1
            if ends is not None:
                # A reading is ca65's if it ends on a line boundary and no
                # boundary falls inside it: `lda #0` then `asl` ends a line
                # after two bytes *and* after three, and only the first
                # reading keeps the second line whole.
                at = address + offset
                fits = [
                    n
                    for n in (1, 2)
                    if at + 1 + n in ends and not any(at + k in ends for k in range(2, 1 + n))
                ]
                if len(fits) == 1:
                    size = fits[0]
                    # The line table said which width ca65 assembled this
                    # immediate at, so the flag is known from here on.
                    if mode == "immM":
                        widths.m = 8 * size
                    else:
                        widths.x = 8 * size
        else:
            size = OPERAND_BYTES[mode]
        if states is not None:
            states.append((widths.m, widths.x))
        raw = data[offset : offset + 1 + size]
        if len(raw) < 1 + size:
            yield (address + offset, raw, None, None, b"")
            return
        operand = raw[1:]
        if name in ("SEP", "REP") and operand:
            mask = operand[0]
            width = 8 if name == "SEP" else 16
            if mask & 0x20:
                widths.m = width
            if mask & 0x10:
                widths.x = width
        yield (address + offset, raw, name, mode, operand)
        offset += 1 + size


def disassemble(data, address, widths, ends=None):
    """Yields (address, bytes, text) over ``data``; see :func:`decode`."""
    for at, raw, name, mode, operand in decode(data, address, widths, ends):
        if name is None:
            yield (at, raw, "; %d trailing byte(s)" % len(raw))
            return
        yield (at, raw, ("%s %s" % (name, format_operand(mode, operand, at, len(raw)))).rstrip())


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


# ---------------------------------------------------------------------------
# The symbolic comparison: two builds whose layouts differ
# ---------------------------------------------------------------------------
#
# When a program is rewritten rather than edited, every segment moves and a
# byte comparison says only that everything changed. What is still
# comparable is meaning: the same routine, the same instructions, operands
# that name the same thing. An operand names the same thing on both sides
# when it is the same fixed number, or the same shared symbol plus the same
# offset; a branch inside a routine names the same thing when it lands on
# instructions the alignment paired. That is what this mode checks.

BRANCH_MODES = ("rel", "rell")


class Side:
    """One build, read for meaning rather than for bytes."""

    def __init__(self, db, image):
        self.db = db
        self.bytes = db.image_bytes(image)
        self.segs = {}
        for sid, seg in db.segments.items():
            self.segs[sid] = (seg["name"], parse_number(seg["start"]), parse_number(seg["size"]), seg.get("type"))
        self.ends = set()
        for sid, offsets in db.boundaries.items():
            if sid in self.segs:
                base = self.segs[sid][1]
                self.ends.update(base + offset for offset in offsets)
        struct_scopes = {sid for sid, scope in db.scopes.items() if scope.get("type") == "struct"}
        self.labels = {}  # name -> (address, segment id)
        self.equates = {}  # name -> set of values
        for sym in db.all_symbols:
            name = sym["name"]
            if name.startswith("@") or "val" not in sym:
                continue
            value = parse_number(sym["val"])
            if sym.get("type") == "lab" and "seg" in sym:
                self.labels.setdefault(name, (value, parse_number(sym["seg"])))
            elif sym.get("type") == "equ" and parse_number(sym.get("scope", "0")) not in struct_scopes:
                self.equates.setdefault(name, set()).add(value)

    def named(self, common):
        """Two sorted lists of (address, name) over the shared names, labels
        and equates apart: the vocabulary an operand is spelled in. Kept
        apart because a label is what an address *is*, and an equate below
        a number is often a coincidence a reader should not be shown."""
        labels = sorted((address, name) for name, (address, _) in self.labels.items() if name in common)
        equates = sorted(
            (next(iter(values)), name)
            for name, values in self.equates.items()
            if name in common and len(values) == 1
        )
        return (labels, equates)

    def extents(self, shared, data_names=()):
        """name -> (segment id, start, end): a label's bytes run to the next
        label of its segment that both builds have, or that is data here.

        Bounded by shared labels because a label only one side has — a
        branch target the original named globally, a cheap local the
        rewrite made of it — would otherwise cut one side's routine in two
        and leave the other whole."""
        by_segment = {}
        for name, (address, sid) in self.labels.items():
            if name in shared or self.is_data(name) or name in data_names:
                by_segment.setdefault(sid, []).append((address, name))
        out = {}
        for sid, labels in by_segment.items():
            _, start, size, _ = self.segs[sid]
            labels.sort()
            for index, (address, name) in enumerate(labels):
                end = start + size
                for later, _ in labels[index + 1 :]:
                    if later > address:
                        end = later
                        break
                out[name] = (sid, address, end)
        return out

    def is_data(self, name):
        address, sid = self.labels[name]
        _, start, _, kind = self.segs[sid]
        return kind != "ro" or (sid, address - start) in self.db.typed

    def data(self, sid, start, end):
        _, base, _, _ = self.segs[sid]
        return self.bytes.get(sid, b"")[start - base : end - base]


def spell(named, value):
    """The spellings a number has on one side: itself, and the nearest
    shared label and the nearest shared equate at or below it, each plus
    the offset. Two operands mean the same thing when any spelling is
    common to both."""
    return set(spellings(named, value))


def spellings(named, value):
    """``spell``'s set as a list in display order: the label spelling, the
    number, the equate spelling."""
    import bisect

    out = []
    labels, equates = named
    for table in (labels, None, equates):
        if table is None:
            out.append("$%X" % value)
            continue
        index = bisect.bisect_right(table, (value, "\uffff")) - 1
        if index >= 0:
            address, name = table[index]
            if value - address < 0x1000:
                out.append("%s+%d" % (name, value - address) if value != address else name)
    return out


# The instructions whose effect the M flag decides, and those the X flag
# decides. Their encoding is the same at either width; what they move is not.
M_SENSITIVE = set("LDA STA ADC SBC AND ORA EOR CMP BIT INC DEC ASL LSR ROL ROR STZ TSB TRB PHA PLA TXA TYA".split())
X_SENSITIVE = set("LDX LDY STX STY CPX CPY INX INY DEX DEY PHX PHY PLX PLY TAX TAY TSX TXY TYX".split())


class Instruction:
    def __init__(self, at, raw, name, mode, operand, named, start, end, state=(None, None)):
        self.at = at
        self.raw = raw
        self.name = name
        self.mode = mode
        self.target = None
        self.m, self.x = state
        value = int.from_bytes(operand, "little") if operand else None
        if mode in BRANCH_MODES:
            span = 0x100 if mode == "rel" else 0x10000
            delta = value - span if value >= span // 2 else value
            target = (at + len(raw) + delta) & 0xFFFF
            if start <= target < end:
                self.target = target
                order = ["local"]
            else:
                order = spellings(named, target)
        elif mode is None or value is None:
            order = []
        elif mode == "move" or mode == "imm8" or len(operand) == 1 and mode.startswith("imm"):
            order = ["$%X" % value]
        elif mode.startswith("imm"):
            # A 16-bit immediate is a number first; it is an address only
            # when a label is exactly there.
            order = spellings(named, value)
            order.sort(key=lambda text: 0 if text in named_exactly(named, value) else 1 if text.startswith("$") else 2)
        else:
            order = spellings(named, value)
            if mode.endswith((",x", ",y")) and value < 0x100:
                # A small indexed operand is an offset from a base held in
                # the register far more often than it is a zero-page name.
                order.sort(key=lambda text: 0 if text.startswith("$") else 1)
        self.spellings = set(order)
        self.text = ("%s %s" % (name or "?", self.render(mode, order[0] if order else ""))).rstrip()
        width = self.width()
        if width:
            self.text += "  @%s%d" % width

    TEMPLATES = {
        "dp": "{}", "dp,x": "{},x", "dp,y": "{},y", "(dp,x)": "({},x)", "(dp)": "({})",
        "(dp),y": "({}),y", "[dp]": "[{}]", "[dp],y": "[{}],y", "sr": "{},s", "(sr),y": "({},s),y",
        "abs": "{}", "abs,x": "{},x", "abs,y": "{},y", "(abs)": "({})", "(abs,x)": "({},x)",
        "[abs]": "[{}]", "long": "f:{}", "long,x": "f:{},x",
    }

    @staticmethod
    def render(mode, operand):
        if mode in (None, "imp", "acc"):
            return ""
        if mode in ("imm8", "immM", "immX"):
            return "#" + operand
        if mode in BRANCH_MODES or mode == "move":
            return operand
        return Instruction.TEMPLATES[mode].format(operand)

    def width(self):
        """The register width this instruction's effect depends on, as
        ("a", 8) or ("i", 16), or None when it depends on neither or
        nothing has said."""
        if self.name in M_SENSITIVE and self.m:
            return ("a", self.m)
        if self.name in X_SENSITIVE and self.x:
            return ("i", self.x)
        return None

    def matches(self, other):
        if self.name != other.name or self.mode != other.mode:
            return False
        if len(self.raw) != len(other.raw):
            return False  # an immediate assembled at another width
        mine, theirs = self.width(), other.width()
        if mine and theirs and mine != theirs:
            return False  # the same encoding, moving another number of bytes
        if not self.spellings and not other.spellings:
            return True  # implied operands: nothing to disagree about
        return bool(self.spellings & other.spellings)


def named_exactly(named, value):
    labels, _ = named
    return {name for address, name in labels if address == value}


def routine(side, name, extent, named):
    """A routine's instructions. The widths start unknown at the label —
    nothing in a debug file says what a caller left — and become known at
    the first `sep`/`rep` or immediate whose size ca65 recorded."""
    sid, start, end = extent
    rows = []
    data = side.data(sid, start, end)
    # First the branch targets inside the routine, so the widths can be
    # forgotten where control arrives from somewhere a straight reading did
    # not come from.
    targets = set()
    for at, raw, mnemonic, mode, operand in decode(data, start, Widths(None, None), side.ends):
        if mode in BRANCH_MODES and operand:
            value = int.from_bytes(operand, "little")
            span = 0x100 if mode == "rel" else 0x10000
            delta = value - span if value >= span // 2 else value
            target = (at + len(raw) + delta) & 0xFFFF
            if start <= target < end:
                targets.add(target)
    states = []
    decoded = list(decode(data, start, Widths(None, None), side.ends, states, targets))
    for (at, raw, mnemonic, mode, operand), state in zip(decoded, states + [(None, None)]):
        rows.append(Instruction(at, raw, mnemonic, mode, operand, named, start, end, state))
    return rows


def align(left, right):
    """The longest common subsequence under ``Instruction.matches``, as a
    list of (i, j) pairs."""
    n, m = len(left), len(right)
    table = [[0] * (m + 1) for _ in range(n + 1)]
    for i in range(n - 1, -1, -1):
        for j in range(m - 1, -1, -1):
            if left[i].matches(right[j]):
                table[i][j] = table[i + 1][j + 1] + 1
            else:
                table[i][j] = max(table[i + 1][j], table[i][j + 1])
    pairs = []
    i = j = 0
    while i < n and j < m:
        if left[i].matches(right[j]):
            pairs.append((i, j))
            i += 1
            j += 1
        elif table[i + 1][j] >= table[i][j + 1]:
            i += 1
        else:
            j += 1
    return pairs


def compare_routine(left, right):
    """The alignment as a list of rows: ('=', l, r), ('-', l, None),
    ('+', None, r), or ('~', l, r) for a paired instruction whose branch
    lands on instructions the alignment did not pair."""
    pairs = align(left, right)
    paired = dict(pairs)
    index_left = {row.at: k for k, row in enumerate(left)}
    index_right = {row.at: k for k, row in enumerate(right)}
    rows = []
    i = j = 0
    for pi, pj in pairs + [(len(left), len(right))]:
        while i < pi:
            rows.append(("-", left[i], None))
            i += 1
        while j < pj:
            rows.append(("+", None, right[j]))
            j += 1
        if pi < len(left):
            a, b = left[pi], right[pj]
            kind = "="
            if a.target is not None and b.target is not None:
                ti, tj = index_left.get(a.target), index_right.get(b.target)
                if ti is None or tj is None or paired.get(ti) != tj:
                    kind = "~"
            rows.append((kind, a, b))
            i, j = pi + 1, pj + 1
    return rows


def compare_data(left, right, named_left, named_right):
    """The first offset at which two data blocks differ in meaning, or
    None. A word or a far address that spells the same shared name on both
    sides is equal however far the layout moved it."""
    i = 0
    while i < max(len(left), len(right)):
        if i < len(left) and i < len(right) and left[i] == right[i]:
            i += 1
            continue
        moved = False
        for start in (i - 1, i):
            for width in (2, 3):
                if start < 0 or start + width > min(len(left), len(right)):
                    continue
                a = int.from_bytes(left[start : start + width], "little")
                b = int.from_bytes(right[start : start + width], "little")
                symbolic_a = {text for text in spell(named_left, a) if not text.startswith("$")}
                symbolic_b = {text for text in spell(named_right, b) if not text.startswith("$")}
                # Only a label spelling vouches for a moved address: an
                # equate below an arbitrary pair of bytes is a coincidence.
                symbolic_a &= label_spellings(named_left, a)
                symbolic_b &= label_spellings(named_right, b)
                if symbolic_a & symbolic_b:
                    i = start + width
                    moved = True
                    break
            if moved:
                break
        if not moved:
            return i
    return None


def label_spellings(named, value):
    labels, _ = named
    return set(spellings((labels, []), value)) - {"$%X" % value}


def symbolic_main(args):
    old = Side(Database(args.old), args.old_image)
    new = Side(Database(args.new), args.new_image)
    common = (set(old.labels) | set(old.equates)) & (set(new.labels) | set(new.equates))
    named_old = old.named(common)
    named_new = new.named(common)
    shared = sorted(set(old.labels) & set(new.labels), key=lambda name: (old.labels[name][1], old.labels[name][0]))
    extents_old = old.extents(set(shared))
    extents_new = new.extents(set(shared))

    differing = 0
    report = []
    for name in shared:
        # Data when either side says so. ca65 leaves a span untyped for
        # `.asciiz` and for a `.res` in a read-only segment, and a rewrite
        # may reserve a variable in a segment the original kept writable;
        # comparing bytes is right for both, and disassembling a string is
        # wrong for either.
        if old.is_data(name) or new.is_data(name):
            sid, start, end = extents_old[name]
            left = old.data(sid, start, end)
            sid, start, end = extents_new[name]
            right = new.data(sid, start, end)
            if not left and not right:
                continue
            at = compare_data(left, right, named_old, named_new)
            if at is None and len(left) == len(right):
                if args.all:
                    report.append("%-28s same       data, %d bytes" % (name, len(left)))
                continue
            differing += 1
            where = "at byte %d" % at if at is not None else "in length"
            report.append("%-28s DIFFERS    data %s: %d bytes -> %d bytes" % (name, where, len(left), len(right)))
            if at is not None:
                report.append("    old: %s" % " ".join("%02X" % b for b in left[max(0, at - 4) : at + 8]))
                report.append("    new: %s" % " ".join("%02X" % b for b in right[max(0, at - 4) : at + 8]))
            continue
        left = routine(old, name, extents_old[name], named_old)
        right = routine(new, name, extents_new[name], named_new)
        rows = compare_routine(left, right)
        changed = [k for k, row in enumerate(rows) if row[0] != "="]
        if not changed:
            if args.all:
                report.append("%-28s same       %d instructions" % (name, len(left)))
            continue
        differing += 1
        report.append("%-28s DIFFERS    %d -> %d instructions, %d rows differ" % (name, len(left), len(right), len(changed)))
        shown = set()
        for k in changed:
            shown.update(range(max(0, k - args.context), min(len(rows), k + args.context + 1)))
        last = -1
        for k in sorted(shown):
            if last >= 0 and k != last + 1:
                report.append("        ...")
            kind, a, b = rows[k]
            mark = {"=": " ", "-": "-", "+": "+", "~": "~"}[kind]
            report.append("    %s %-34s | %s" % (mark, a.text if a else "", b.text if b else ""))
            last = k
        report.append("")

    only_old = sorted(set(old.labels) - set(new.labels))
    only_new = sorted(set(new.labels) - set(old.labels))
    constants = []
    for name in sorted(set(old.equates) & set(new.equates)):
        if old.equates[name] != new.equates[name]:
            constants.append("  %-28s %s -> %s" % (name,
                ", ".join("$%X" % v for v in sorted(old.equates[name])),
                ", ".join("$%X" % v for v in sorted(new.equates[name]))))

    print("\n".join(report))
    if constants:
        print("Constants whose values differ:")
        print("\n".join(constants))
        print()
    if only_old:
        print("Labels only in the old build (%d): %s" % (len(only_old), " ".join(only_old)))
    if only_new:
        print("Labels only in the new build (%d): %s" % (len(only_new), " ".join(only_new)))
    print()
    print("%d shared labels, %d differ in meaning; %d constants differ" % (len(shared), differing, len(constants)))
    return 1 if differing or constants else 0


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("old", metavar="OLD.dbg")
    parser.add_argument("new", metavar="NEW.dbg")
    parser.add_argument("--widths", default="a8,i8", help="the M/X state at the top of each segment")
    parser.add_argument("--old-image", help="the old output file, if not beside its debug file")
    parser.add_argument("--new-image", help="the new output file, if not beside its debug file")
    parser.add_argument("--context", type=int, default=3, help="instructions shown around a difference")
    parser.add_argument("--all", action="store_true", help="list identical symbols too")
    parser.add_argument(
        "--symbolic",
        action="store_true",
        help="compare meaning rather than bytes: for builds whose layouts differ",
    )
    args = parser.parse_args(argv)
    if args.symbolic:
        return symbolic_main(args)

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
