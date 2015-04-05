(ql:quickload :erudite)

(defpackage erudite.doc
  (:use :cl)
  (:export :make))

(in-package :erudite.doc)

(defparameter *files* (list (asdf:system-relative-pathname :erudite "erudite.lisp")))

(defun make ()
  (erudite:gen-latex-doc 
   (asdf:system-relative-pathname :erudite "doc/erudite.tex")
   *files*
   :title "Erudite Developer Manual"
   :author "Mariano Montone"))
