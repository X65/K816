#*****************************************************************************
#
#                                  Makefile
#
#     Builds the full-stack example: k816 -> ca65 -> ld65 -> 65816 ROM
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

# Tools default to the cc65 tree's bin/ (build them with `make -C ../../..`
# and `make -C ../../.. k816`); override K816/CA65/LD65 to use others.
BIN  := ../../../../bin
K816 ?= $(BIN)/k816
CA65 ?= $(BIN)/ca65
LD65 ?= $(BIN)/ld65

all: fullstack.bin

main.s: main.k816
	$(K816) compile $< -o $@

main.o: main.s
	$(CA65) $< -o $@

startup.o: startup.asm
	$(CA65) $< -o $@

fullstack.bin: startup.o main.o fullstack.cfg
	$(LD65) -C fullstack.cfg -o $@ startup.o main.o

clean:
	rm -f main.s main.o startup.o fullstack.bin

.PHONY: all clean
