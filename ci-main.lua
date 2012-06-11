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
godi.build("godi-curl") -- TODO: patch to avoid pb
godi.build("godi-ocaml-gettext") -- TODO
godi.build("godi-ocaml-markdown") -- TODO
godi.build("godi-inotify") -- TODO
godi.build("godi-ocamlcore-api") -- TODO
godi.build("godi-sqlexpr") -- TODO
godi.build("godi-cameleon-rss") -- TODO (or something better!!!!)
godi.build("godi-ocaml-xdg-basedir") -- TODO 


ci.exec("ocaml", "setup.ml", "-configure", "--enable-tests", "--enable-dev", "--enable-oasis-db-ocsigen")
ci.exec("ocaml", "setup.ml", "-build")
ci.exec("ocaml", "setup.ml", "-test")
