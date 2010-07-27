#!/bin/sh

set -e

VERBOSE=true

info () {
  if $VERBOSE; then
    echo $*
  fi
}

warning () {
  echo $* >&2
}

#TMP_FILE=`mktemp`
#trap "rm -f '$TMP_FILE'" EXIT
TMP_FILE="test.xml"

echo > "$TMP_FILE"
for PKG in *; do
  info "Checking $PKG"

  WATCH_FILE="$PKG/_watch"
  VERSION_FILE="$PKG/_version"

  if ! test -e "$WATCH_FILE"; then
    warning "No watch file for $PKG"
  fi

  VERSION=
  if test -e "$VERSION_FILE"; then
    VERSION=`cat "$VERSION_FILE"`
  fi

  uscan --package "$PKG" \
        --watchfile "$WATCH_FILE" \
        --upstream-version "$VERSION" \
        --report --dehs >> "$TMP_FILE"
done

cat "$TMP_FILE"
