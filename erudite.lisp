#|

\chapter{Introduction}

\emph{Erudite} is a very simple system for Literate Programming in Common Lisp.

Some of its salient features are:

\begin{itemize}

\item Documentation is written in Common Lisp comments. This is very useful because you can work with your program as if it were not a literate program: you can load it, work from SLIME, etc, directly.

\item There are no chunks weaving or special directives like in original LP systems. This is not so cool, as there's no flexible way of controlling the order of the comments and code, like in other systems. But like Haskell (and its LP support), Lisp code is also pretty easy to sort without too much problems (I think...).

\end{itemize}

\ignore {
|#
(in-package #:erudite)

#|
}

\chapter{Implementation}

Implementation is very ad-hoc at the moment.

First, files with literate code are parsed into \emph{fragments}. Fragments can be of type \textit{documentation} or type \textit{code}. \textit{documentation} is the text that appears in Common Lisp comments. \textit{code} fragments are the rest.
|#

(defvar *commands* (make-hash-table :test #'equalp))

(defun find-command (name &optional (error-p t))
  (let ((command (gethash name *commands*)))
    (when (and error-p (not command))
      (error "Invalid command: ~A" command))
    command))

;;; The parser works like a custom look-ahead parser, with a whole file line
;;; being the slice looked ahead.

(defun parse-file-source (stream)
  "Parses a lisp file source"
  (loop 
     :for line := (read-line stream nil)
     :while line
     :collect
     (parse-line line stream)))

(defun parse-line (line stream)
  (or
   (parse-long-comment line stream)
   (parse-short-comment line stream)
   (parse-code line stream)))

(defun parse-long-comment (line stream)
  "Parse a comment between #| and |#"
  
  ;; TODO: this does not work for long comments in one line
  (when (equalp (search "#|" (string-left-trim (list #\  ) line))
		0)
    ;; We've found a long comment
    ;; Extract the comment source
    (let ((comment
	   (with-output-to-string (s)
	     ;;; First, add the first comment line
	     (register-groups-bind (comment-line) ("\\#\\|\\s*(.+)" line)
	       (write-string comment-line s))
	     ;; While there are lines without |#, add them to the comment source
	     (loop 
		:for line := (read-line stream nil)
		:while (and line (not (search "|#" line)))
		:do 
		(terpri s)
		(write-string line s)
		:finally
		;; Finally, extract the last comment line
		(if line
		  (register-groups-bind (comment-line) ("\\s*(.+)\\|\\#" line)
		    (when comment-line
		      (write-string comment-line s)))
		  (error "EOF: Could not complete comment parsing"))))))
      (list :doc comment))))
      
#|

\chapter{Backends}

\emph{Erudite} support LaTeX and Sphinx generation at the moment.

\section{LaTeX}

The parsed fragments are compiled to latex code. That means embedding the code fragments found between \verb'\begin{code}' and \verb'\end{code}'.
|#

(defun compile-latex-fragments (fragments)
  "Prepare parsed fragments to be compiled to LaTeX"
  (apply #'concatenate 'string
	 (loop for fragment in fragments
	      collect
	      (ecase (first fragment)
		(:code (format nil "\\begin{code}~%~A~%\\end{code}"
			       (string-trim (list #\  #\newline) 
					    (second fragment))))
		(:doc (second fragment))))))

#|

To generate LaTeX, the \emph{gen-latex-doc} function is called:

|#

(defun gen-latex-doc (pathname files &key title author template-pathname)
  "Generates a LaTeX document.

   Args: - pathname: The pathname of the .tex file to generate.
         - files: The list of .lisp files to compile
         - title: Title of the document
         - author: Author of the document
         - template-pathname: A custom LaTeX template file. If none is specified, a default template is used."
  (let ((template (cl-template:compile-template
		   (file-to-string (or template-pathname
				       (asdf:system-relative-pathname 
					:erudite 
					"latex/template.tex")))))
	(fragments
	 (loop for file in files
	    appending
	      (parse-lisp-source (file-to-string file)))))
    (with-open-file (f pathname :direction :output 
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (write-string
       (funcall template (list :title title
			       :author author
			       :body (compile-latex-fragments fragments)))
       f))
    t))

#|

\section{Sphinx}

Sphinx is the other kind of output apart from LaTeX.

Code fragments in Sphinx must appear indented after a \verb'.. code-block::' directive: 

|#

(defun compile-sphinx-fragments (fragments)
  "Prepares parsed fragments for Sphinx generation"
  (apply #'concatenate 'string
	 (loop for fragment in fragments
	    collect
	      (ecase (first fragment)
		(:code (format nil ".. code-block:: common-lisp~%~%     ~A"
			       (indent-code 
				(string-trim (list #\  #\newline) 
					     (second fragment)))))
		(:doc (second fragment))))))

#|

Code blocks in Sphinx are indented. The indent-code function takes care of that:

|#

(defun indent-code (code)
  "Code in sphinx has to be indented"
  (let ((lines (split-sequence:split-sequence #\newline
					      code)))
    (apply #'concatenate 'string
	   (mapcar (lambda (line)
		     (format nil "     ~A~%" line))
		   lines))))

#|

To generate Sphinx code, \emph{gen-sphinx-doc} is called. 

|#

(defun gen-sphinx-doc (pathname files &key prelude postlude)
  "Generates Sphinx document.

   Args: - pathname: Pathname of the .rst file to generate.
         - files: .lisp files to compile.
         - prelude: String (or pathname) to append before the Sphinx document.
         - postlude: String (or pathname) to append after the Sphinx document."
  (let ((fragments
	 (loop for file in files
	    appending
	      (parse-lisp-source (file-to-string file)))))
    (with-open-file (f pathname :direction :output 
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (when prelude
	(write-string 
	 (if (pathnamep prelude)
	     (file-to-string prelude)
	     prelude)
	 f))
      (write-string (compile-sphinx-fragments fragments) f)
      (when postlude
	(write-string (if (pathnamep postlude)
			  (file-to-string postlude)
			  postlude)
		      f)))))
