bootstrap = require("bootstrap")

bootstrap.init()

ci = require("ci")
godi = require("godi")

ci.init()
godi.init()

godi.bootstrap("3.12")
godi.update()
godi.upgrade()
godi.build("godi-findlib")
godi.build("godi-ounit")
godi.build("apps-ocsigen",
  "-option", "godi-lwt:GODI_LWT_GLIB=no", 
  "-option", "apps-ocsigen:CONF_OCSIGEN_USER=$USER", 
  "-option", "apps-ocsigen:CONF_OCSIGEN_GROUP=$USER")
godi.build("apps-oasis")
godi.build("godi-yojson")
godi.build("godi-sexplib")
godi.build("godi-extlib")
godi.build("godi-pcre")
godi.build("godi-calendar")
godi.build("godi-ocaml-fileutils")
godi.build("godi-inifiles")
godi.build("godi-ocamlnet")
godi.build("godi-curl")
godi.build("godi-ocaml-gettext")
godi.build("godi-ocaml-markdown") 
godi.build("godi-inotify") 
godi.build("godi-ocamlcore-api") 
godi.build("godi-ocaml-sqlexpr")
godi.build("godi-ocamlrss")
godi.build("godi-ocaml-xdg-basedir")
godi.build("apps-ocsigen-bundler")

ci.exec("ocaml", "setup.ml", "-configure", "--enable-tests", "--enable-dev", "--enable-oasis-db-ocsigen")
ci.exec("ocaml", "setup.ml", "-build")
ci.exec("ocaml", "setup.ml", "-test")
