SBCL=$(shell which sbcl)
ECL=$(shell which ecl)
CLISP=$(shell which clisp)
CCL=$(shell which ccl)

ifeq ($(LISP),SBCL)		# if compiler is sbcl
  BUILD = $(SBCL) --noinform --disable-debugger --load cli.lisp
  TEST = $(SBCL) --disable-debugger --eval 
endif

ifeq ($(LISP),ECL)		# else if compiler is ecl
  BUILD = $(ECL) -load cli.lisp
  TEST = $(ECL) -q -eval
endif

ifeq ($(LISP),CLISP)		# else if compiler is clisp
  BUILD = $(CLISP) -x '(load "cli.lisp")'
  TEST = $(CLISP) -x 
endif

ifeq ($(LISP),CCL)		# else if compiler is ccl
  BUILD = $(CCL) -l cli.lisp
  TEST = $(CCL) -e
endif

ifndef LISP
  COMMAND = $(SBCL) --noinform --disable-debugger --load cli.lisp
  TEST = $(SBCL) --disable-debugger --eval 
endif

build:
	$(BUILD)

install:
	cp erudite /usr/local/bin

tests:
	$(TEST) '(progn (asdf:test-system :erudite)(uiop/image:quit))'
