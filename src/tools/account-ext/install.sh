#!/bin/sh

set -e

#createuser -S -D -R account_ext_admin
#ALTER USER account_ext_admin WITH PASSWORD 'losh9ahJ';
#
#createuser -S -D -R account_ext_3rdparty
#ALTER USER account_ext_3rdparty WITH PASSWORD 'iV9Goo3w';

#createdb -O account_ext_admin --encoding UNICODE account_ext "External account management database"
#
#psql account_ext < account-ext.sql

grant_ro ()
{
	cat <<EOF
GRANT SELECT ON TABLE user_group TO account_ext_3rdparty;
GRANT SELECT ON TABLE users TO account_ext_3rdparty;
EOF
}

grant_ro | psql account_ext


