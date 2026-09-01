# Built with the cc65 toolchain: cl65 compiles main.k65 and ../include/defs.k65
# as the two units of one program, assembles what k816 emits, and links it into
# a loadable Atari executable with ../atarixl.cfg.
#
# `-t none` because cl65 defaults to the C64, and `--no-target-lib` because -C
# replaces the target's configuration but not its library, and there is no
# `none.lib`. The layout is the program's, stated with -C, and it is what puts
# `main` in the file's RUNAD segment - there is no startup stub.
#
# linux.k65proj and windows.k65proj are kept beside this file as the record of
# how the original K65 built the same example.

CL65 ?= cl65

all: main.xex

main.xex: main.k65 ../include/defs.k65 ../atarixl.cfg
	$(CL65) -t none --no-target-lib -C ../atarixl.cfg -o $@ main.k65 ../include/defs.k65

clean:
	rm -f main.xex

.PHONY: all clean
