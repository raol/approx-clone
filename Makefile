# approx: proxy server for Debian archive files
# Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

export OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

export OCAMLFLAGS = -warn-error A

define PROJ_server
    SIDE_EFFECT = $(shell ./mkversion > version.ml)
    SOURCES = util.ml config.ml default_config.ml log.ml url.ml control_file.ml release.ml ifaddr.c internet.mli version.ml server.ml approx.ml
    INCDIRS = +pcre +syslog +netsys +netstring +netcgi1 +nethttpd-for-netcgi1 +sha
    LIBS = unix pcre syslog netsys netstring cgi nethttpd-for-netcgi1 sha
# use this after the next release of ocamlnet2
# (a bug in the current release prevents this from working)
#    INCDIRS = +pcre +syslog +netsys +netstring +netcgi2 +nethttpd-for-netcgi2 +sha
#    LIBS = unix pcre syslog netsys netstring netcgi nethttpd-for-netcgi2 sha
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

ifndef SUBPROJS
    export SUBPROJS = server gc
endif

all: native-code

export TRASH = version.ml

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
