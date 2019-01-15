.PHONY: all test clean

SBCL=$(shell which sbcl)
ECL=$(shell which ecl)
CLISP=$(shell which clisp)
CCL=$(shell which ccl)

ifeq ($(LISP),SBCL)		# if compiler is sbcl
  BUILD = $(SBCL) --control-stack-size 100000 --noprint --quit --load cli.lisp
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
  BUILD = $(SBCL) --control-stack-size 100000 --quit --noprint --load cli.lisp
  TEST = $(SBCL) --quit --eval 
endif

erudite: ## Build erudite executable
	$(BUILD)

install: ## Install erudite executable in /usr/local/bin
	cp erudite /usr/local/bin

test:   ## Run tests
	$(TEST) '(progn (asdf:test-system :erudite)(uiop/image:quit))'

clean:  ## Clean
	rm erudite

all: erudite ## Build erudite

help:   ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'
