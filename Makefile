.PHONY: deps ql

quicklisp/quicklisp.lisp: scripts/get-ql.sh
	bash scripts/get-ql.sh

ql: quicklisp/quicklisp.lisp
	sbcl --load quicklisp/quicklisp.lisp \
	--eval "(quicklisp-quickstart:install :path \"${PWD}/quicklisp\")" --quit
