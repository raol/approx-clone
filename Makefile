# approx: proxy server for Debian archive files
# Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

OCAMLBUILD := ocamlbuild
OCAMLBUILD_OPTS := -classic-display

TARGET := native

ifeq ($(TARGET),byte)
    OCAMLBUILD_OPTS += -byte-plugin
endif

programs = approx approx-gc approx-update approx-import

all: $(programs)

approx:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) approx.$(TARGET)
	cp -p _build/approx.$(TARGET) $@

approx-%:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(@:approx-%=%).$(TARGET)
	cp -pv _build/$(@:approx-%=%).$(TARGET) $@

$(programs): $(wildcard *.ml*)

clean:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) -clean
	rm -f $(programs)

.PHONY: tests

tests: $(subst .ml,,$(wildcard tests/*.ml))

%_test:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $@.$(TARGET)

version := $(shell sed -n 's/^let version = "\(.*\)"$$/\1/p' config.ml)
tarball := approx_$(version).orig.tar.gz
package := approx-$(version)
excludes := $(tarball) .git _build "*~" "\#*"

tarball:
	tar -czf $(tarball) $(excludes:%=--exclude=%) \
	    --transform "s:^\./:$(package)/:" .
