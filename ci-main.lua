bootstrap = require("bootstrap")

bootstrap.init()

ci = require("ci")
godi = require("godi")
oasis = require("oasis")
darcs = require("darcs")

ci.init()
godi.init()

godi.bootstrap("3.12")
godi.update()
godi.upgrade()
godi.build_ocsigen()
godi.build_many(
  {"godi-findlib",
   "godi-ounit",
   "apps-oasis",
   "godi-yojson",
   "godi-sexplib",
   "godi-extlib",
   "godi-pcre",
   "godi-calendar",
   "godi-ocaml-fileutils",
   "godi-inifiles",
   "godi-ocamlnet",
   "godi-curl",
   "godi-ocaml-gettext",
   "godi-ocaml-markdown", 
   "godi-ocaml-inotify", 
   "godi-ocamlcore-api", 
   "godi-ocaml-sqlexpr",
   "godi-ocamlrss",
   "godi-ocaml-xdg-basedir",
   "apps-ocsigen-bundler",
   "apps-ocamlmod",
   "apps-ocamlify"},
   "godi-pgocaml:GODI_PGOCAML_USE_BATTERIES=yes")

oasis.std_process("--enable-dev", "--enable-oasis-db-ocsigen", "--enable-tests")
ci.exec("make", "dist-deploy-dev")
darcs.create_tag(oasis.package_version())
