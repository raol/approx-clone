# approx: proxy server for Debian archive files
# Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

export OCAMLMAKEFILE = /usr/share/ocaml-tools/OCamlMakefile

export OCAMLFLAGS = -warn-error A

define PROJ_server
    SOURCES = util.ml config.ml default_config.ml log.ml url.ml control_file.ml release.ml ifaddr.c netif.mli server.ml version.ml approx.ml
    INCDIRS = +pcre +syslog +netstring +cgi +nethttpd
    LIBS = unix pcre syslog netstring cgi nethttpd
    RESULT = approx
endef
export PROJ_server

define PROJ_gc
    SOURCES = util.ml config.ml default_config.ml control_file.ml gc_approx.ml
    INCDIRS = +pcre
    LIBS = unix pcre
    RESULT = gc_approx
endef
export PROJ_gc

define PROJ_fsck
    SOURCES = util.ml config.ml default_config.ml control_file.ml release.ml fsck_approx.ml
    INCDIRS = +pcre
    LIBS = unix pcre
    RESULT = fsck_approx
endef
export PROJ_fsck

ifndef SUBPROJS
    export SUBPROJS = server gc fsck
endif

all: native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
