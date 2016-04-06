.PHONY: deps ql

deps: deps/quicklisp.lisp
	sbcl --load deps/deps.lisp

deps/quicklisp.lisp: scripts/get-ql.sh
	bash scripts/get-ql.sh

ql: deps/quicklisp.lisp
