.PHONY: deps ql server

deps: quicklisp/setup.lisp
	sbcl --load quicklisp/setup.lisp --script scripts/dependencies.lisp

quicklisp/quicklisp.lisp:
	bash scripts/get-ql.sh

quicklisp/setup.lisp: quicklisp/quicklisp.lisp
	sbcl --load quicklisp/quicklisp.lisp \
	--eval "(quicklisp-quickstart:install :path \"${PWD}/quicklisp\")" --quit

ql: quicklisp/setup.lisp

server:
	sbcl --load main.lisp
