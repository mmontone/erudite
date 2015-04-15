(defpackage #:erudite
  (:use #:cl #:cl-ppcre)
  (:export #:erudite
	   #:*short-comments-prefix*
	   #:*syntax*
	   #:*output-type*
	   #:*title*
	   #:*subtitle*
	   #:*catch-errors-p*
	   #:*verbose*
	   #:*debug*
	   #:*code-indexing*
	   #:*erudite-package*
	   #:*implicit-doc*
	   #:*implicit-code*)
  (:documentation "Erudite is a Literate Programming System for Common Lisp"))

