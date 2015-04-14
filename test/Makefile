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

test: ## Run Erudite tests
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

latex: output.tex ## Create Latex output test

pdf: output.pdf ## Create PDF output test

markdown: output.md ## Create Markdown output test

rst: output.rst ## Create Sphinx output test

all: output.pdf output.md output.rst ## Create output test in all formats

clean: ## Remove generated files
	rm -f output.tex
	rm -f output.md
	rm -f output.rst
	rm -f output.pdf

help:   ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'
