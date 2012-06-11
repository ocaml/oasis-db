#/bin/sh 

set -e

CURDIR=$(pwd)
BUNDLE_BUILD_DIR="$CURDIR/_build"
BUNDLE_BUILD_BINDIR="$BUNDLE_BUILD_DIR/bin"
BUNDLE_BUILD_LIBDIR="$BUNDLE_BUILD_DIR/lib"
BUNDLE_BUILD_OCAMLLIBDIR="$BUNDLE_BUILD_LIBDIR/ocaml"

OCAMLPATH="$BUNDLE_BUILD_OCAMLLIBDIR:$OCAMLPATH"
OCAMLFIND_DESTDIR="$BUNDLE_BUILD_OCAMLIBDIR"
OCAMLFIND_LDCONF="ignore"
PATH="$BUNDLE_BUILD_BINDIR:$PATH"
LD_LIBRARY_PATH="$BUNDLE_BUILD_LIBDIR:$LD_LIBRARY_PATH"

export BUNDLE_BUILD_DIR
export BUNDLE_BUILD_BINDIR
export BUNDLE_BUILD_LIBDIR
export BUNDLE_BUILD_OCAMLLIBDIR
export OCAMLPATH
export OCAMLFIND_DESTDIR
export OCAMLFIND_LDCONF
export PATH
export LD_LIBRARY_PATH

# Check ocaml 
if ! (ocaml -version > /dev/null); then
  echo "ocaml not found, you must install ocaml in order to compile." >&2
  exit 1
fi

# Check ocamlfind
if ! (ocamlfind printconf > /dev/null); then
  echo "ocamlfind not found, you must install ocamlfind in order to compile." >&2
  exit 1
fi

# Run
ocaml bundle.ml "$@"

