# approx: proxy server for Debian archive files
# Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

export OCAMLMAKEFILE = /usr/share/ocaml-tools/OCamlMakefile

export OCAMLFLAGS = -w A

define PROJ_server
    SOURCES = util.ml config.ml default_config.ml url.ml approx.ml
    LIBS = unix pcre syslog netstring http curl
    INCDIRS = +pcre +syslog +netstring +http +curl
    RESULT = approx
endef
export PROJ_server

define PROJ_gc
    SOURCES = util.ml config.ml default_config.ml package.ml gc_approx.ml
    LIBS = unix pcre
    INCDIRS = +pcre
    RESULT = gc_approx
endef
export PROJ_gc

ifndef SUBPROJS
    export SUBPROJS = server gc
endif

all: native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
