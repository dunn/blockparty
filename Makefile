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

.PHONY: server tests

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

CL_ARGS = ${EXTRA_ARGS} \
					${LOAD} blockparty.asd \
					${EVAL} '(asdf:load-system :blockparty)'

server:
	${LISP} ${CL_ARGS} ${EVAL} '(blockparty:main nil)'

tests:
	${LISP} ${BATCH} ${CL_ARGS} \
	   	 ${EVAL} '(redis:connect)' \
	     ${EVAL} '(setq lisp-unit:*print-errors* t)' \
	     ${EVAL} '(setq lisp-unit:*print-failures* t)' \
	     ${EVAL} "(lisp-unit:run-tests :all 'blockparty)" \
	     ${EVAL} '(redis:disconnect)' \
	     ${EVAL} '(quit)'
