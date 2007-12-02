# approx: proxy server for Debian archive files
# Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

export OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

export OCAMLFLAGS += -warn-error A

export VPATH += ..

define PROJ_config
    SOURCES = util.ml config.ml config_test.ml
    INCDIRS = .. +pcre +sha
    LIBS = unix pcre sha
    RESULT = config_test
endef
export PROJ_config

define PROJ_control_file
    SOURCES = util.ml config.ml version.ml default_config.ml log.ml control_file.ml control_file_test.ml
    INCDIRS = .. +pcre +sha +syslog
    LIBS = unix pcre sha syslog
    RESULT = control_file_test
endef
export PROJ_control_file

define PROJ_ifaddr
    SOURCES = ifaddr.c internet.mli ifaddr_test.ml
    INCDIRS = ..
    LIBS = unix
    RESULT = ifaddr_test
endef
export PROJ_ifaddr

define PROJ_metadata
    SOURCES = util.ml config.ml version.ml default_config.ml log.ml control_file.ml release.ml metadata_test.ml
    INCDIRS = .. +pcre +sha +syslog
    LIBS = unix pcre sha syslog
    RESULT = metadata_test
endef
export PROJ_metadata

define PROJ_patch
    SOURCES = util.ml patch.ml patch_test.ml
    INCDIRS = .. +pcre +sha
    LIBS = unix pcre sha
    RESULT = patch_test
endef
export PROJ_patch

define PROJ_sha1
    SOURCES = util.ml sha1_test.ml
    INCDIRS = .. +pcre +sha
    LIBS = unix pcre sha
    RESULT = sha1_test
endef
export PROJ_sha1

ifndef SUBPROJS
    export SUBPROJS = config control_file ifaddr metadata patch sha1
endif

all: native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
