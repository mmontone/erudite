(ql:quickload :erudite)

(defpackage erudite.doc
  (:use :cl)
  (:export :make))

(in-package :erudite.doc)

(defun system-file (filename)
  (asdf:system-relative-pathname :erudite filename))

(defparameter *files* 
  (mapcar #'system-file 
	  (list "erudite.lisp"
		"cli.lisp"
		"commands.lisp"
		"syntax/erudite.lisp"
		"test/test.lisp")))

(defun make ()
  (erudite:erudite
   (asdf:system-relative-pathname :erudite "doc/erudite.tex")
   *files*
   ;:title "Erudite Developer Manual"
   ;:author "Mariano Montone"
   ;:syntax :erudite
   :output-type :latex
   :document-class :book))
