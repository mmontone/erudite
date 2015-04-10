pdf: latex
	pdflatex erudite.tex
	makeindex erudite.idx
	pdflatex erudite.tex

latex:
	sbcl --disable-debugger --load make.lisp --eval '(erudite.doc:make)' --quit
