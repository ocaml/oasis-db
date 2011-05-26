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


#LWT_LOG=* -> info
LWT_LOG=* -> debug
export LWT_LOG

OCAMLRUNPARAM=b
export OCAMLRUNPARAM

#TESTFLAGS = -only-test OASIS-DB:7:Web -verbose
CONFIGUREFLAGS += --override ocamlbuildflags -classic-display --enable-dev --enable-oasis-db-ocsigen
#CONFIGUREFLAGS += $(if $(shell ocamlfind query gettext),--enable-gettext,--disable-gettext)

#default: build
default: test

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

clean-run:
	-$(RM) -r tmp

clean: clean-run

# Run ocsigen
ocsigen-run:
	mkdir tmp || true
	./src/tools/ocsigen-autorestart.sh

# Create DB
db-create:
	./src/sql/install.sh

# Deploy dev
OCSIGEN_BUNDLER=ocsigen-bundler
deploy-dev:
	$(OCSIGEN_BUNDLER) \
		--verbose \
		--conf etc/ocsigen-dev.conf \
		--target _build/tgt-dev \
		--mkdir data/incoming \
		--mkdir data/dist \
		--mkdir tmp \
		--copy-dir src/web/static static \
		--copy-dir src/web/mkd mkd \
		--port 8080 \
		--copy-file patches/META.cameleon lib/ocaml/METAS \
		--copy-file patches/META.sqlexpr  lib/ocaml/sqlexpr/META \
		--copy-file patches/META.sexplib  lib/ocaml/sexplib/META \
		--deploy-host ssh.ocamlcore.org \
		--deploy-dir /home/groups/oasis/ocsigen/dev/ 

.PHONY: deploy-dev
