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

.PHONY: install-dist ql server update

DIST ?= $(shell cat use-dist | tr -d '\n')
CL_ARGS = --no-sysinit --no-userinit \
          --load quicklisp/setup.lisp

install-dist:
	sbcl ${CL_ARGS} --non-interactive \
       --eval "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/${DIST}/distinfo.txt\" :prompt nil :replace t)" \
       --load blockparty.asd \
       --eval '(ql:quickload "blockparty")'

quicklisp/dists/distinfo.txt:
	sbcl ${CL_ARGS} --non-interactive \
       --eval "(ql:update-dist \"quicklisp\")" --quit

update: quicklisp/dists/distinfo.txt

quicklisp/quicklisp.lisp:
	bash scripts/get-ql.sh

quicklisp/setup.lisp: quicklisp/quicklisp.lisp
	sbcl ${CL_ARGS} \
       --eval "(quicklisp-quickstart:install :path \"${PWD}/quicklisp\")" --quit

ql: quicklisp/setup.lisp

server:
	sbcl ${CL_ARGS} \
       --load blockparty.asd \
       --eval '(ql:quickload "blockparty")' \
       --eval '(blockparty:main)'
