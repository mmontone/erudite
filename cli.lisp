#| @section Command line interface

It is possible to invoke @emph{Erudite} from the command line

Run @verb{make} to build @verb{erudite} executable.

This is the command line syntax:

@verbatim
Usage: erudite [-hvd] [+vd] [OPTIONS] FILES...

Erudite is a Literate Programming System for Common Lisp
-h, --help                  Print this help and exit.
--version                   Print Erudite version
-(+)v, --verbose[=yes/no]   Run in verbose mode
Fallback: yes
Default: no
Environment: VERBOSE
-(+)d, --debug[=on/off]     Turn debugging on or off.
Fallback: on
Default: off
Environment: DEBUG
-(+)id, --implicit-doc[=yes/no] Treat all comments as documentation
Fallback: yes
Default: yes
-(+)ic, --implicit-code[=yes/no] Include all code in documentation
Fallback: yes
Default: yes
-o, --output=OUTPUT         The output file. If none is used, result is
printed to stdout
--output-type=OUTPUT-TYPE   The output type. One of 'latex',
'sphinx','markdown', 'org'
Default: latex
--syntax=SYNTAX             The syntax used in source files. One of 'erudite',
'latex', 'sphinx', 'markdown', 'org'
Default: erudite
--author=AUTHOR             The author to appear in the document
--title=TITLE               The document title
@end verbatim

Then run @verb{sudo make install} to install globally in your system

Here is an example usage:
@verbatim
erudite -o erudite.tex erudite.lisp
@end verbatim

@subsection Implementation

The command line is implemented via the @emph{com.dvl.clon} library.

|#

(ql:quickload :net.didierverna.clon)
(ql:quickload :erudite)

(defpackage erudite.cli
  (:use :cl :erudite))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (net.didierverna.clon:nickname-package))

(clon:defsynopsis (:postfix "FILES...")
    (text :contents (format nil "Erudite is a Literate Programming System for Common Lisp"))
  (flag :short-name "h" :long-name "help"
        :description "Print this help and exit.")
  (flag :long-name "version"
        :description "Print Erudite version")
  (switch :short-name "v" :long-name "verbose"
          :description "Run in verbose mode"
          :default-value nil
          :env-var "VERBOSE")
  (switch :short-name "d" :long-name "debug"
          :description "Turn debugging on or off."
          :argument-style :on/off
          :default-value nil
          :env-var "DEBUG")
  (switch :short-name "id" :long-name "implicit-doc"
          :description "Treat all comments as documentation"
          :default-value t)
  (switch :short-name "ic" :long-name "implicit-code"
          :description "Include all code in documentation"
          :default-value t)
  (path :long-name "output"
        :short-name "o"
        :argument-name "OUTPUT"
        :type :file
        :description "The output file. If none is used, result is printed to stdout")
  (enum :long-name "output-type"
        :argument-name "OUTPUT-TYPE"
        :enum (list :latex :sphinx :markdown :org)
        :default-value :latex
        :description "The output type. One of 'latex', 'sphinx','markdown'")
  (enum :long-name "syntax"
        :argument-name "SYNTAX"
        :enum (list :erudite :latex :sphinx :markdown :org)
        :default-value :erudite
        :description "The syntax used in source files. One of 'erudite', 'latex', 'sphinx', 'markdown'")
  (stropt :long-name "short-comments-prefix"
          :argument-name "SC-PREFIX"
          :description "Prefix on short comments")
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
     (format t "Erudite Literate Programming System for Common Lisp version 0.0.1~%"))
    (t
     (let ((title (stringp* (clon:getopt :long-name "title")))
           (author (stringp* (clon:getopt :long-name "author")))
           (short-comments-prefix (stringp* (clon:getopt :long-name "short-comments-prefix")))
           (output-type (clon:getopt :long-name "output-type"))
           (syntax (clon:getopt :long-name "syntax"))
           (output (or (clon:getopt :long-name "output")
                       t))
           (debug (clon:getopt :long-name "debug"))
           (verbose (clon:getopt :long-name "verbose"))
           (implicit-doc (clon:getopt :long-name "implicit-doc"))
           (implicit-code (clon:getopt :long-name "implicit-code"))
           (files (mapcar #'pathname (clon:remainder))))
       (if (null files)
           (format t "Error: provide the files to process~%")
           (erudite:erudite output files
                            :debug debug
                            :verbose verbose
                            :implicit-doc implicit-doc
                            :implicit-code implicit-code
                            :short-comments-prefix short-comments-prefix
                            :title title
                            :author author
                            :output-type output-type
                            :syntax syntax))))))

(clon:dump "erudite" main)
