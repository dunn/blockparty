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

.PHONY: ql server update

quicklisp/dists/distinfo.txt:
	sbcl --load quicklisp/setup.lisp \
	--eval "(ql:update-dist \"quicklisp\")" --quit

update: quicklisp/dists/distinfo.txt

quicklisp/quicklisp.lisp:
	bash scripts/get-ql.sh

quicklisp/setup.lisp: quicklisp/quicklisp.lisp
	sbcl --load quicklisp/quicklisp.lisp \
	--eval "(quicklisp-quickstart:install :path \"${PWD}/quicklisp\")" --quit

ql: quicklisp/setup.lisp

server:
	sbcl --load main.lisp
