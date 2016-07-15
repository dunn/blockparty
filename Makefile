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

.PHONY: dist-install dist-update ql server tests

# Only SBCL and ABCL are known to work
LISP ?= sbcl

ifeq ($(LISP),$(filter $(LISP),abcl sbcl ccl ccl64))
	LOAD = --load
	EVAL = --eval
else
	LOAD = -load
	EVAL = -eval
endif

ifeq ($(LISP),abcl)
	BATCH = --batch
	EXTRA_ARGS = --noinform --noinit --nosystem
endif
ifeq ($(LISP),$(filter $(LISP),ccl ccl64))
	BATCH = --batch
	EXTRA_ARGS = --no-init
endif
ifeq ($(LISP),ecl)
	EXTRA_ARGS = -norc
endif
ifeq ($(LISP),lisp)
	BATCH = -batch
	EXTRA_ARGS = -nositeinit -noinit
endif
ifeq ($(LISP),sbcl)
	BATCH = --non-interactive
	EXTRA_ARGS = --no-sysinit --no-userinit
endif

CL_ARGS = ${EXTRA_ARGS} ${LOAD} quicklisp/setup.lisp

DIST ?= $(shell cat use-dist | tr -d '\n')

dist-install:
	${LISP} ${CL_ARGS} ${BATCH} \
       ${EVAL} "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/${DIST}/distinfo.txt\" :prompt nil :replace t)" \
       ${LOAD} blockparty.asd \
       ${EVAL} '(ql:quickload "blockparty")' \
			 ${EVAL} '(quit)'

quicklisp/dists/distinfo.txt:
	${LISP} ${CL_ARGS} ${BATCH} \
       ${EVAL} "(ql:update-dist \"quicklisp\")" --quit

dist-update: quicklisp/dists/distinfo.txt

quicklisp/quicklisp.lisp:
	bash scripts/get-ql.sh

quicklisp/setup.lisp: quicklisp/quicklisp.lisp
	${LISP} ${BATCH} ${LOAD} quicklisp/quicklisp.lisp \
       ${EVAL} "(quicklisp-quickstart:install :path \"${PWD}/quicklisp\")" \
			 ${EVAL} '(quit)'

ql: quicklisp/setup.lisp

server:
	${LISP} ${CL_ARGS} \
       ${LOAD} blockparty.asd \
       ${EVAL} '(ql:quickload "blockparty")' \
       ${EVAL} '(blockparty:main)'

tests:
	${LISP} ${BATCH} ${CL_ARGS} \
       ${LOAD} blockparty.asd \
       ${EVAL} '(ql:quickload "blockparty")' \
	   	 ${EVAL} '(redis:connect)' \
			 ${EVAL} '(setq lisp-unit:*print-errors* t)' \
			 ${EVAL} '(setq lisp-unit:*print-failures* t)' \
			 ${EVAL} "(lisp-unit:run-tests :all 'blockparty)" \
	   	 ${EVAL} '(redis:disconnect)' \
			 ${EVAL} '(quit)'
