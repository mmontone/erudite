.PHONY: all clean sphinx

erudite.pdf: erudite.tex
	pdflatex erudite.tex
	makeindex erudite.idx
	pdflatex erudite.tex

erudite.tex:
	erudite -o erudite.tex --output-type=latex ../erudite.lisp

erudite/index.html: erudite.tex
	latex2html erudite.tex

sphinx: ## Build sphinx literate documentation
	erudite -o sphinx/index.rst --output-type=sphinx ../erudite.lisp

sphinx-pdf: sphinx ## Create Sphinx PDF of literate documentation
	cd sphinx; make latexpdf

sphinx-html: sphinx ## Create Sphinx HTML of literate documentation
	cd sphinx; make html

erudite.md:
	erudite -o erudite.md --output-type=markdown ../erudite.lisp

markdown: erudite.md ## Build markdown literate documentation

pdf: erudite.pdf ## Build PDF literate documentation

latex-html: erudite/index.html ## Generate HTML from Latex

all: erudite.pdf sphinx-pdf erudite.md

clean: ## Remove generated documentation files
	rm -f erudite.tex
	rm -f erudite.md
	rm -f erudite.pdf
	rm -rf erudite

help:   ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'
