(ql:quickload :erudite)

(defpackage erudite.doc
  (:use :cl)
  (:export 
   :make-latex 
   :make-sphinx
   :make-markdown))

(in-package :erudite.doc)

(defun system-file (filename)
  (asdf:system-relative-pathname :erudite filename))

(defun make-latex ()
  (erudite:erudite
   (asdf:system-relative-pathname :erudite "doc/erudite.tex")
   (system-file "erudite.lisp")
   :output-type :latex
   :document-class :book
   :debug t
   :catch-errors-p t))

(defun make-sphinx ()
  (erudite:erudite
   (asdf:system-relative-pathname :erudite "doc/sphinx/index.rst")
   (system-file "erudite.lisp")
   :output-type :sphinx))

(defun make-markdown ()
  (erudite:erudite
   (asdf:system-relative-pathname :erudite "doc/erudite.md")
   (system-file "erudite.lisp")
   :output-type :markdown))
