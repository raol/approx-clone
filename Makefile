# approx: proxy server for Debian archive files
# Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

export OCAMLMAKEFILE = /usr/share/ocaml-tools/OCamlMakefile

export OCAMLFLAGS = -warn-error A

define PROJ_server
    SOURCES = util.ml config.ml default_config.ml log.ml url.ml control_file.ml release.ml ifaddr.c internet.mli server.ml version.ml approx.ml
    INCDIRS = +pcre +syslog +netstring +cgi +nethttpd +sha
    LIBS = unix pcre syslog netstring cgi nethttpd sha
    RESULT = approx
endef
export PROJ_server

define PROJ_gc
    SOURCES = util.ml config.ml default_config.ml control_file.ml gc.ml
    INCDIRS = +pcre +sha
    LIBS = unix pcre sha
    RESULT = gc_approx
endef
export PROJ_gc

define PROJ_fsck
    SOURCES = util.ml config.ml default_config.ml control_file.ml release.ml fsck.ml
    INCDIRS = +pcre +sha
    LIBS = unix pcre sha
    RESULT = fsck_approx
endef
export PROJ_fsck

ifndef SUBPROJS
    export SUBPROJS = server gc fsck
endif

all: native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
