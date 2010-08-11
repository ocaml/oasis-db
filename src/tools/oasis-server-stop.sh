#!/bin/sh

set -e

read PID < tmp/ocsigen.pid
rm -f tmp/ocsigen.pid

kill $PID
