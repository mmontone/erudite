cli:
	sbcl --noinform --disable-debugger --load cli.lisp

install:
	cp erudite /usr/local/bin
