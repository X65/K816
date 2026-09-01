#*****************************************************************************
#
#                                  Makefile
#
#          Builds the full-stack example with cl65: a 65816 ROM image
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
# One rule, because cl65 knows the pipeline: it compiles main.k816 with k816,
# assembles what k816 emits and startup.asm with ca65, and links both with
# ld65 against fullstack.cfg. The four hand-written rules this replaced are
# what `cl65 -v` still prints.
#
# `-t none` because cl65's default target is the C64, and ld65 would look for
# c64.cfg and link c64.lib into a program with no C runtime.
# `--no-target-lib` because -C replaces the target's configuration but not its
# library, and there is no none.lib. Both are properties of this command, not
# things cl65 infers from a K816 file being on the line.
#
# startup.asm comes first because it owns the STARTUP segment and the reset
# vector: cl65 adds each object to the link where its source appears.

# Defaults to the cc65 tree's bin/ (build it with `make -C ../../.. bin`);
# override CL65 to use an installed one.
CL65 ?= ../../../../bin/cl65

all: fullstack.bin

fullstack.bin: main.k816 startup.asm fullstack.cfg
	$(CL65) -t none --no-target-lib -C fullstack.cfg -o $@ startup.asm main.k816

clean:
	rm -f fullstack.bin

.PHONY: all clean
