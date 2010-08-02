################################################################################
#  OASIS: architecture for building OCaml libraries and applications           #
#                                                                              #
#  Copyright (C) 2008-2010, OCamlCore SARL                                     #
#                                                                              #
#  This library is free software; you can redistribute it and/or modify it     #
#  under the terms of the GNU Lesser General Public License as published by    #
#  the Free Software Foundation; either version 2.1 of the License, or (at     #
#  your option) any later version, with the OCaml static compilation           #
#  exception.                                                                  #
#                                                                              #
#  This library is distributed in the hope that it will be useful, but         #
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  #
#  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          #
#  details.                                                                    #
#                                                                              #
#  You should have received a copy of the GNU Lesser General Public License    #
#  along with this library; if not, write to the Free Software Foundation,     #
#  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               #
################################################################################


-include Makefile.secret 
export PGDATABASE PGUSER PGPASSWORD PGHOST

LWT_LOG=* -> info
LWT_LOG=* -> debug
export LWT_LOG

CONFIGUREFLAGS += --override ocamlbuildflags -classic-display
#CONFIGUREFLAGS += $(if $(shell ocamlfind query gettext),--enable-gettext,--disable-gettext)

#default: test
default: build
#TESTFLAGS      += -long 
#TESTFLAGS      += -verbose
#TESTFLAGS      += -only-test OASIS:4:Basic:1

# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: 
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

wc:
	find src/ -name "*.ml" | xargs wc -l

headache:
	find ./ -name _darcs -prune -false -o -name _build -prune \
	  -false -o -name ext -prune -false -o -name bindist -prune -false \
	  -o -type f \
	  | xargs headache -h _header -c _headache.config

.PHONY: wc headache

# Source distribution

dist: setup.data
	if ! [ "$$(darcs diff | wc -l)" = 0 ]; then \
	  echo E: Uncommited changes >&2 ; exit 1; \
	fi
	$(MAKE) test
	$(MAKE) dist-step2

-include setup.data
dist-step2:
	darcs dist --dist-name $(pkg_name)-$(pkg_version)
	if ! (darcs query tag | grep "$(pkg_version)" > /dev/null); then \
	  darcs tag "$(pkg_version)"; \
	else \
	  echo W: Version $(pkg_version) already tagged >&2; \
	fi 
	gpg -s -a -b "$(pkg_name)-$(pkg_version).tar.gz"

.PHONY: dist dist-step2

clean-run:
	-$(RM) -r tmp

clean: clean-run

# Run ocsigen
run-ocsigen:
	mkdir tmp || true
	./src/tools/ocsigen-autorestart.sh

# Create DB
db-create:
	./src/sql/install.sh
