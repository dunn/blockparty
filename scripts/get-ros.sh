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

ROS_VER=0.0.6.64
ROS_URL="https://github.com/roswell/roswell/archive/v$ROS_VER.tar.gz"
ROS_SHA=f9b7a3ada298e62d024b612136196d8564e36650da59b6cc72cfb6c9bdaca3c1
TMP=${TMPDIR:-"/tmp"}

if [[ "$(uname -s)" == "Darwin" ]]; then
  SHASUM="shasum -a 256"
else
  SHASUM="sha256sum"
fi

curl -sL "$ROS_URL" -o "$TMP/roswell-$ROS_VER.tar.gz"
checksum=$(eval "$SHASUM $TMP/roswell-$ROS_VER.tar.gz")

if [[ ${checksum//\ */} != "$ROS_SHA" ]]; then
  echo "Bad checksum!"
  echo "$checksum"
  exit 1
fi

tar -C "$TMP" -xzvf "$TMP/roswell-$ROS_VER.tar.gz"
cd "$TMP/roswell-$ROS_VER" || exit 1

./bootstrap
./configure --disable-dependency-tracking \
            --disable-silent-rules \
            --enable-manual-generation \
            --enable-html-generation \
            --prefix="$HOME/.roswell"
make install
