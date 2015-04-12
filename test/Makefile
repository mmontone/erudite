run:
	sbcl --disable-debugger --eval '(asdf:test-system :erudite)' --quit

latex:
	erudite -o output.tex --output-type=latex output.lisp

sphinx:
	erudite -o output.rst --output-type=sphinx output.lisp

markdown:
	erudite -o output.md --output-type=markdown output.lisp

pdf: latex
	pdflatex output.tex
	makeindex output.idx
	pdflatex output.tex
