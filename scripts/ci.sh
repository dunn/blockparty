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

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  bash "$TRAVIS_BUILD_DIR/scripts/get-ros.sh";

  case $LISP in
    "cmucl")
      ros install cmu-bin
      ros use cmu-bin
      ;;
    *)
      ros install "$LISP-bin"
      ros use "$LISP-bin"
  esac
else
  brew update
  brew install redis
  brew services start redis

  case $LISP in
    "ccl")
      brew install clozure-cl
      ;;
    "cmucl")
      brew install --devel dunn/yolo/cmucl
      ;;
    *)
      brew install "$LISP"
  esac
fi
