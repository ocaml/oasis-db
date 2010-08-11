#!/bin/sh

echo $(pwd) 
. $(pwd)/Makefile.secret

export PGDATABASE PGUSER PGPASSWORD PGHOST

case $(hostname) in
  ssh)
    CONF=etc/ocsigen-dev.conf
    DAEMON=./oasis-server
    ;;
  *)
    CONF=etc/ocsigen.conf
    DAEMON=_build/src/web/oasis-server
    ;;
esac

nohup $DAEMON --daemon --pidfile tmp/ocsigen.pid -c $CONF
