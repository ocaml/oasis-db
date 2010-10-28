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

#LWT_LOG=* -> info
LWT_LOG=* -> debug
export LWT_LOG

CONFIGUREFLAGS += --override ocamlbuildflags -classic-display
#CONFIGUREFLAGS += $(if $(shell ocamlfind query gettext),--enable-gettext,--disable-gettext)

default: build
#default: test
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

# Sync dev
DEV_HOST=ssh.ocamlcore.org
DEV_DIR=/home/groups/oasis/oasis-server-dev/
DEV_SYNC_DEST=$(DEV_HOST):$(DEV_DIR)
dev-sync: build
	ssh $(DEV_HOST) "cd $(DEV_DIR) && ./src/tools/oasis-server-stop.sh || true"
	rsync -av Makefile src etc _build/src/web/oasis-server $(DEV_SYNC_DEST)
	ssh $(DEV_HOST) "cd $(DEV_DIR) && ./src/tools/oasis-server-start.sh"

dev-stop: 
	./src/tools/oasis-server-stop.sh || true

dev-restart:
	./src/tools/oasis-server-stop.sh || true
	./src/tools/oasis-server-start.sh
