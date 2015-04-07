(defpackage #:erudite
  (:use #:cl #:cl-ppcre)
  (:export #:parse-lisp-source
	   #:file-to-string
	   #:gen-sphinx-doc
	   #:gen-latex-doc))

