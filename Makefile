# approx: proxy server for Debian archive files
# Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

export OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

export OCAMLFLAGS += -warn-error A

define PROJ_server
    SIDE_EFFECT = $(shell ./mkversion > version.ml)
    SOURCES = util.ml version.ml config.ml default_config.ml log.ml url.ml control_file.ml release.ml patch.ml pdiff.ml ifaddr.c internet.mli server.ml approx.ml
    INCDIRS = +pcre +sha +syslog +netsys +netstring +netcgi2 +nethttpd-for-netcgi2
    LIBS = unix pcre sha syslog netsys netstring netcgi nethttpd-for-netcgi2
    RESULT = approx
endef
export PROJ_server

define PROJ_gc
    SOURCES = util.ml version.ml config.ml default_config.ml log.ml url.ml control_file.ml release.ml gc.ml
    INCDIRS = +pcre +sha +syslog +netsys +netstring
    LIBS = unix pcre sha syslog netsys netstring
    RESULT = gc_approx
endef
export PROJ_gc

define PROJ_update
    SOURCES = util.ml version.ml config.ml default_config.ml log.ml url.ml control_file.ml release.ml patch.ml pdiff.ml update.ml
    INCDIRS = +pcre +sha +syslog +netsys +netstring
    LIBS = unix pcre sha syslog netsys netstring
    RESULT = update_approx
endef
export PROJ_update

ifndef SUBPROJS
    export SUBPROJS = server gc update
endif

all: native-code

export TRASH = version.ml

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
