.PHONY: ql server

quicklisp/quicklisp.lisp:
	bash scripts/get-ql.sh

quicklisp/setup.lisp: quicklisp/quicklisp.lisp
	sbcl --load quicklisp/quicklisp.lisp \
	--eval "(quicklisp-quickstart:install :path \"${PWD}/quicklisp\")" --quit

ql: quicklisp/setup.lisp

server:
	sbcl --load main.lisp
