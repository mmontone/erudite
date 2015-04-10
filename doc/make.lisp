(ql:quickload :erudite)

(defpackage erudite.doc
  (:use :cl)
  (:export :make-latex :make-sphinx))

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

(defun make-latex ()
  (erudite:erudite
   (asdf:system-relative-pathname :erudite "doc/erudite.tex")
   *files*
   :output-type :latex
   :document-class :book))

(defun make-sphinx ()
  (erudite:erudite
   (asdf:system-relative-pathname :erudite "doc/sphinx/index.rst")
   *files*
   :output-type :sphinx))
