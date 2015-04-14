.PHONY: all test clean

SBCL=$(shell which sbcl)
ECL=$(shell which ecl)
CLISP=$(shell which clisp)
CCL=$(shell which ccl)

ifeq ($(LISP),SBCL)		# if compiler is sbcl
  TEST = $(SBCL) --disable-debugger --eval 
endif

ifeq ($(LISP),ECL)		# else if compiler is ecl
  TEST = $(ECL) -q -eval
endif

ifeq ($(LISP),CLISP)		# else if compiler is clisp
  TEST = $(CLISP) -x 
endif

ifeq ($(LISP),CCL)		# else if compiler is ccl
  TEST = $(CCL) -e
endif

ifndef LISP
  TEST = $(SBCL) --disable-debugger --eval 
endif

test:
	$(TEST) '(progn (asdf:test-system :erudite)(uiop/image:quit))'

output.tex:
	erudite -o output.tex --output-type=latex output.lisp

output.rst:
	erudite -o output.rst --output-type=sphinx output.lisp

output.md:
	erudite -o output.md --output-type=markdown output.lisp

output.pdf: output.tex
	pdflatex output.tex
	makeindex output.idx
	pdflatex output.tex

all: output.pdf output.md output.rst

clean:
	rm -f output.tex
	rm -f output.md
	rm -f output.rst
	rm -f output.pdf	
