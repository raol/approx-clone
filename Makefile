# approx: proxy server for Debian archive files
# Copyright (C) 2014  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

OCAMLBUILD := ocamlbuild
OCAMLBUILD_OPTS := -classic-display -use-ocamlfind

TARGET := native

programs = approx approx-gc approx-import

all: $(programs)

approx:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) approx.$(TARGET)
	cp -p _build/approx.$(TARGET) $@

approx-gc:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) gc_cache.$(TARGET)
	cp -pv _build/gc_cache.$(TARGET) $@

approx-import:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) import.$(TARGET)
	cp -pv _build/import.$(TARGET) $@

$(programs): $(wildcard *.ml*)

clean:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) -clean
	rm -f $(programs)

.PHONY: tests

tests: $(subst .ml,,$(wildcard tests/*.ml))

%_test:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $@.$(TARGET)

version := $(shell sed -n 's/^let version = "\(.*\)"$$/\1/p' config.ml)
package := approx-$(version)
tarball := $(package).tar.gz

tarball:
	git archive -o $(tarball) --prefix $(package)/ HEAD
