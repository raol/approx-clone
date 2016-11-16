# approx: proxy server for Debian archive files
# Copyright (C) 2016  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

OCAMLBUILD := ocamlbuild
OCAMLBUILD_OPTS := -classic-display -use-ocamlfind

TARGET := native

programs = approx approx-import

all: $(programs)

approx:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) approx.$(TARGET)
	cp -p _build/approx.$(TARGET) $@

approx-import:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) import.$(TARGET)
	cp -pv _build/import.$(TARGET) $@

$(programs): $(wildcard *.ml*)

clean:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) -clean
	rm -f $(programs)

test: tests/runtests
	./$(<F).$(TARGET)

tests/runtests::
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $@.$(TARGET)

version := $(shell sed -n 's/^let version = "\(.*\)"$$/\1/p' config.ml)
package := approx-$(version)
tarball := $(package).tar.gz

tarball:
	git archive -o $(tarball) --prefix $(package)/ HEAD
