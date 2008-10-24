# approx: proxy server for Debian archive files
# Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

OCAMLBUILD := ocamlbuild
OCAMLBUILD_OPTS := -classic-display

TARGET := native

ifeq ($(TARGET),byte)
    OCAMLBUILD_OPTS += -byte-plugin
endif

programs = gc update import

all:
	@set -e; for prog in approx $(programs); do \
	    echo $(OCAMLBUILD) $(OCAMLBUILD_OPTS) $$prog.$(TARGET); \
	    $(OCAMLBUILD) $(OCAMLBUILD_OPTS) $$prog.$(TARGET); \
	    cp -pv _build/$$prog.$(TARGET) $$prog; \
	done
	@set -e; for prog in $(programs); do \
	    mv -v $$prog approx-$$prog; \
	done

clean:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) -clean
	rm -f approx $(patsubst %,approx-%,$(programs))

extra-clean: clean
	rm -f *~ \#*

.PHONY: tests

test_programs = $(wildcard tests/*.ml)

tests:
	@set -e; for test in $(test_programs:.ml=); do \
	    echo $(OCAMLBUILD) $(OCAMLBUILD_OPTS) $$test.$(TARGET); \
	    $(OCAMLBUILD) $(OCAMLBUILD_OPTS) $$test.$(TARGET); \
	done

upstream-branch:
	git-status | grep -q "^# On branch upstream"

release: upstream-branch extra-clean
	@set -e; \
	version=$$(sed -n 's/^.*number = "\(.*\)".*$$/\1/p' version.ml); \
	echo Tagging upstream/$$version; \
	git tag -f -m "upstream version $$version" upstream/$$version; \
	tarball=../approx_$$version.orig.tar.gz; \
	echo Creating $$tarball; \
	package=approx-$$version; \
	tar czf $$tarball --transform "s:^\./:$$package/:" --exclude=.git .
