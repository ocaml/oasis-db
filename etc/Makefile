
GENERATED = ocsigen-local.conf ocsigen-dev.conf 
TOPDIR = `readlink -f $(CURDIR)/..`

all: $(GENERATED)

clean:
	-$(RM) $(GENERATED)

.PHONY: all clean

ocsigen-local.conf: ocsigen.conf.in
	ocaml generate.ml -o $@ -local -set topdir "$(TOPDIR)" $^

ocsigen-dev.conf: ocsigen.conf.in
	ocaml generate.ml -o $@ -dev -set topdir "$(TOPDIR)" $^

