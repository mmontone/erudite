.PHONY: all clean sphinx

erudite.pdf: erudite.tex
	pdflatex erudite.tex
	makeindex erudite.idx
	pdflatex erudite.tex

erudite.tex:
	erudite -o erudite.tex --output-type=latex ../erudite.lisp

sphinx:
	erudite -o sphinx/index.rst --output-type=sphinx ../erudite.lisp

sphinx-pdf: sphinx
	cd sphinx; make latexpdf

sphinx-html: sphinx
	cd sphinx; make html

erudite.md:
	erudite -o erudite.md --output-type=markdown ../erudite.lisp

markdown: erudite.md

pdf: erudite.pdf

all: erudite.pdf sphinx-pdf erudite.md

clean:
	rm -f erudite.tex
	rm -f erudite.md
	rm -f erudite.pdf

