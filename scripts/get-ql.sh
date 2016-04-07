#!/usr/bin/env bash

# Copyright 2016 Alex Dunn <dunn.alex@gmail.com>

# This file is part of block-party.

# block-party is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.

# block-party is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with read-thing.  If not, see <http://www.gnu.org/licenses/>.

set -euo pipefail

QL_URL=https://beta.quicklisp.org/quicklisp.lisp
QL_SHA=4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17
QL_TMP=${TMPDIR:-"/tmp"}

if [[ "$(uname -s)" == "Darwin" ]]; then
  SHASUM="shasum -a 256"
else
  SHASUM="sha256sum"
fi

curl -L "$QL_URL" -o "$QL_TMP/quicklisp.lisp" >/dev/null 2>&1
checksum=$(eval "$SHASUM $QL_TMP/quicklisp.lisp")

if [[ ${checksum//\ */} != "$QL_SHA" ]]; then
  echo "Bad checksum!"
  echo "$checksum"
  exit 1
fi

if [[ ! -d quicklisp ]]; then mkdir quicklisp; fi

mv "$QL_TMP/quicklisp.lisp" quicklisp/quicklisp.lisp
