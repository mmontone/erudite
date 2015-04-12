run:
	sbcl --disable-debugger --eval '(asdf:test-system :erudite)' --quit

output:
	erudite -o output.tex output.lisp
	pdflatex output.tex
	makeindex output.idx
	pdflatex output.tex
