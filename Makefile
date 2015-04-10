cli:
	sbcl --noinform --disable-debugger --load cli.lisp

install:
	cp erudite /usr/local/bin

tests:
	sbcl --disable-debugger --eval '(asdf:test-system :erudite)' --quit
