latex-pdf: latex
	pdflatex erudite.tex
	makeindex erudite.idx
	pdflatex erudite.tex

latex:
	sbcl --disable-debugger --load make.lisp --eval '(erudite.doc:make-latex)' --quit

_sphinx:
	sbcl --disable-debugger --load make.lisp --eval '(erudite.doc:make-sphinx)' --quit

sphinx-pdf: _sphinx
	cd sphinx; make latexpdf

sphinx-html: _sphinx
	cd sphinx; make html
