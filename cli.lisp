(ql:quickload :com.dvlsoft.clon)
(ql:quickload :erudite)

(defpackage erudite.cli
  (:use :cl :erudite))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))

(clon:defsynopsis (:postfix "FILES...")
  (text :contents (format nil "Erudite is a Literate Programming System for Common Lisp"))
  (flag :short-name "h" :long-name "help"
        :description "Print this help and exit.")
  (flag :long-name "version"
        :description "Print Erudite version")
  (switch :short-name "v" :long-name "verbose"
          :description "Run in verbose mode"
          :env-var "VERBOSE")
  (switch :short-name "d" :long-name "debug"
          :description "Turn debugging on or off."
          :argument-style :on/off
          :env-var "DEBUG")
  (path :long-name "output"
        :short-name "o"
	:argument-name "OUTPUT"
	:type :file
	:description "The output file. If none is used, result is printed to stdout")
  (enum :long-name "output-type"
	:argument-name "OUTPUT-TYPE"
	:enum (list :latex :sphinx)
	:default-value :latex
	:description "The output type. One of 'latex', 'sphinx'")
  (enum :long-name "syntax"
	:argument-name "SYNTAX"
	:enum (list :erudite :latex :sphinx)
	:default-value :erudite
	:description "The syntax used in source files. One of 'latex', 'sphinx', 'erudite'")
  (stropt :long-name "author"
          :argument-name "AUTHOR"
	  :description "The author to appear in the document")
  (stropt :long-name "title"
          :argument-name "TITLE"
	  :description "The document title"))

(defun stringp* (str)
  (and (stringp str)
       (not (equalp str ""))
       str))

(defun main ()
  (clon:make-context)
  (cond 
    ((or (clon:getopt :short-name "h")
	 (not (clon:cmdline-p)))
     (clon:help))
    ((clon:getopt :long-name "version")
     (print "Erudite Literate Programming System for Common Lisp version 0.0.1"))
    (t
     (let ((title (stringp* (clon:getopt :long-name "title")))
	   (author (stringp* (clon:getopt :long-name "author")))
	   (output-type (clon:getopt :long-name "output-type"))
	   (syntax (clon:getopt :long-name "syntax"))
	   (output (or (clon:getopt :long-name "output")
		       t))
	   (files (mapcar #'pathname (clon:remainder))))
       (erudite:erudite output files 
			:title title
			:author author
			:output-type output-type
			:syntax syntax)))))

(clon:dump "erudite" main)
