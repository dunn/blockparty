#!/usr/bin/env bash

# Copyright 2016 Alex Dunn <dunn.alex@gmail.com>

# This file is part of blockparty.

# blockparty is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.

# blockparty is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with blockparty.  If not, see <http://www.gnu.org/licenses/>.

set -euo pipefail

if [[ -x $(which buildapp) ]]; then
  exit 0
fi

BA_VER=1.5.6
BA_URL="https://github.com/xach/buildapp/archive/release-$BA_VER.tar.gz"
BA_SHA=d77fb6c151605da660b909af058206f7fe7d9faf972e2c30876d42cb03d6a3ed
TMP=${TMPDIR:-"/tmp"}

if [[ "$(uname -s)" == "Darwin" ]]; then
  SHASUM="shasum -a 256"
else
  SHASUM="sha256sum"
fi

curl -sL "$BA_URL" -o "$TMP/buildapp-$BA_VER.tar.gz"
checksum=$(eval "$SHASUM $TMP/buildapp-$BA_VER.tar.gz")

if [[ ${checksum//\ */} != "$BA_SHA" ]]; then
  echo "Bad checksum!"
  echo "$checksum"
  exit 1
fi

tar -C "$TMP" -xzvf "$TMP/buildapp-$BA_VER.tar.gz"
cd "$TMP/buildapp-release-$BA_VER"

make install LISP="$LISP"
