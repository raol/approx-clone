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

clean:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) -clean
	rm -f $(programs)

extra-clean: clean
	rm -f *~ \#* tests/*~ tests/\#*

.PHONY: tests

tests: $(subst .ml,,$(wildcard tests/*.ml))

%_test:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $@.$(TARGET)

upstream-branch:
	git status | grep -q "^# On branch upstream"

release: upstream-branch extra-clean
	@set -e; \
	version=$$(sed -n 's/^.*number = "\(.*\)".*$$/\1/p' version.ml); \
	echo Tagging upstream/$$version; \
	git tag -m "upstream version $$version" upstream/$$version; \
	tarball=../approx_$$version.orig.tar.gz; \
	echo Creating $$tarball; \
	package=approx-$$version; \
	tar czf $$tarball --transform "s:^\./:$$package/:" --exclude=.git .
