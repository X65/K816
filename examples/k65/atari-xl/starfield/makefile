# The one example in this directory K816 does not compile, and it is a product
# boundary rather than a gap: its `for … eval [ rnd() & 255 ]` tables ask the
# compiler to generate different bytes on every build, and K816 reports
# K816-K65-0002 twice and stops. The fix the diagnostic names is to generate
# the table outside the build and include it with `binary`; doing that would
# make this a different example, so it is left as the record of the construct.
#
# The rule is the same one the other three examples use, so that what fails is
# the compiler saying why rather than a missing build.
#
# linux.k65proj and windows.k65proj are kept beside this file as the record of
# how the original K65 built the same example - it has an `rnd()`.

CL65 ?= cl65

all: main.xex

main.xex: main.k65 ../include/defs.k65 ../atarixl.cfg
	$(CL65) -t none --no-target-lib -C ../atarixl.cfg -o $@ main.k65 ../include/defs.k65

clean:
	rm -f main.xex

.PHONY: all clean
