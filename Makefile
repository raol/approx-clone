# approx: proxy server for Debian archive files
# Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

OCAMLBUILD := ocamlbuild
OCAMLBUILD_OPTS := -classic-display

TARGET := $(shell if [ -x /usr/bin/ocamlopt ]; then echo native; \
		  else echo byte; fi)

all:
	@set -e; for prog in approx gc update; do \
	    echo $(OCAMLBUILD) $(OCAMLBUILD_OPTS) $$prog.$(TARGET); \
	    $(OCAMLBUILD) $(OCAMLBUILD_OPTS) $$prog.$(TARGET); \
	    cp -pv _build/$$prog.$(TARGET) $$prog; \
	done
	@mv -v gc gc_approx
	@mv -v update update_approx

fast:
	@$(MAKE) all OCAMLBUILD_OPTS=

clean:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) -clean
	rm -f approx gc_approx update_approx

.PHONY: tests

test_programs = $(wildcard tests/*.ml)

tests:
	@set -e; for test in $(test_programs:.ml=); do \
	    echo $(OCAMLBUILD) $(OCAMLBUILD_OPTS) $$test.$(TARGET); \
	    $(OCAMLBUILD) $(OCAMLBUILD_OPTS) $$test.$(TARGET); \
	done
