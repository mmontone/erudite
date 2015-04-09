pdf: latex
	pdflatex erudite.tex

latex:
	sbcl --disable-debugger --load make.lisp --eval '(erudite.doc:make)' --quit
