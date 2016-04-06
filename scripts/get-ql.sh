#!/usr/bin/env bash
set -euo pipefail

QL_URL=https://beta.quicklisp.org/quicklisp.lisp
QL_SHA=4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17
QL_TMP=${TMPDIR:-"/tmp"}

if [[ "$(uname -s)" == "Darwin" ]]; then
  SHASUM="shasum -a 256"
else
  SHASUM="sha256sum"
fi

curl -L "$QL_URL" -o "$QL_TMP/quicklisp.lisp"
checksum=$(eval "$SHASUM $QL_TMP/quicklisp.lisp")

if [[ ${checksum//\ */} != "$QL_SHA" ]]; then
  echo "Bad checksum!"
  echo "$checksum"
  exit 1
fi

if [[ ! -d deps ]]; then mkdir deps; fi

mv "$QL_TMP/quicklisp.lisp" deps/quicklisp.lisp
