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

(defvar *short-comments-prefix* ";;")
(defvar *input-type* :erudite)
(defvar *output-type* :latex)
(defvar *current-path* nil)

(defun process-file-to-string (pathname)
  (let ((*current-path* (fad:pathname-directory-pathname pathname)))
    (with-open-file (f pathname)
      (with-output-to-string (s)
        (erudite::process-parts
         (erudite::split-file-source f)
         s)))))

;;; The parser works like a custom look-ahead parser, with a whole file line
;;; being the slice looked ahead. And is implemented in Continuation Passing Style.

(defun split-file-source (stream)
  "Splits a file source in docs and code"
  (append-source-parts
   (loop
      :for line := (read-line stream nil)
      :while line
      :collect
      (parse-line line stream))))

(defun parse-line (line stream)
  (or
   (parse-long-comment line stream)
   (parse-short-comment line stream)
   (parse-code line stream)))

(defun parse-long-comment (line stream)
  "Parse a comment between #| and |#"

  ;; TODO: this does not work for long comments in one line
  (when (equalp (search "#|" (string-left-trim (list #\  #\tab) line))
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

(defun parse-short-comment (line stream)
  (when (search *short-comments-prefix*
                (string-left-trim (list #\  #\tab)
                                  line))
    ;; A short comment was found
    (let* ((comment-regex (format nil "~A\\s*(.+)" *short-comments-prefix*))
           (comment
            (with-output-to-string (s)
              (register-groups-bind (comment-line) (comment-regex line)
                (write-string comment-line s)))))
      (list :doc comment))))

(defun parse-code (line stream)
  (list :code line))

(defun append-to-end (thing list)
  (cond
    ((null list)
     (list thing))
    (t
     (setf (cdr (last list))
           (list thing))
     list)))

(defun append-source-parts (parts)
  "Append docs and code parts"
  (let ((appended-parts nil)
        (current-part (first parts)))
    (loop
       :for part :in (cdr parts)
       :do
       (if (equalp (first part) (first current-part))
           ;; The parts are of the same type. Append them
           (setf (second current-part)
                 (with-output-to-string (s)
                   (write-string (second current-part) s)
                   (terpri s)
                   (write-string (second part) s)))
           ;; else, there's a new kind of part
           (progn
             (setf appended-parts (append-to-end current-part appended-parts))
             (setf current-part part)))
       :finally (append-to-end current-part appended-parts))
    appended-parts))

(defun process-parts (parts output)
  (when parts
    (let ((first-part (first parts)))
      (process-part (first first-part) first-part
                    output
                    (lambda (&key (output output))
                      (process-parts (rest parts) output))))))

(defgeneric process-part (part-type part output cont))

(defmethod process-part ((type (eql :code)) part output cont)
  (write-code (second part) output *output-type*)
  (funcall cont))

(defmethod process-part ((type (eql :doc)) part output cont)
  (with-input-from-string (input (second part))
    (labels ((%process-part (&key (input input) (output output))
               (flet ((process-cont (&key (input input) (output output))
                        (%process-part :input input :output output)))
                 (let ((line (read-line input nil)))
                   (if line
                       (maybe-process-command line input output #'process-cont)
                       (funcall cont :output output))))))
      (%process-part))))

(defun find-matching-command (line)
  (loop
     :for command :in *commands*
     :when (match-command command line)
     :return command))

(defmethod maybe-process-command (line input output cont)
  "Process a top-level command"
  (let ((command (find-matching-command line)))
    (if command
        (process-command command line input output cont)
        (process-doc *input-type* *output-type* line output cont))))


(defmethod process-doc ((input-type (eql :latex)) output-type line stream cont)
  (write-string line stream)
  (terpri stream)
  (funcall cont))

(defmethod process-doc ((input-type (eql :sphinx)) output-type line stream cont)
  (write-string line stream)
  (terpri stream)
  (funcall cont))

(defmethod process-doc ((input-type (eql :erudite)) output-type line stream cont)
  (write-string line stream)
  (terpri stream)
  (funcall cont))

(defmethod write-code (code stream (output-type (eql :latex)))
  (write-string "\\begin{code}" stream)
  (terpri stream)
  (write-string code stream)
  (terpri stream)
  (write-string "\\end{code}" stream)
  (terpri stream))

(defmethod write-chunk-name (chunk-name stream (output-type (eql :latex)))
  (write-string "<<<" stream)
  (write-string chunk-name stream)
  (write-string ">>>" stream)
  (terpri stream))

(defmethod write-chunk (chunk-name chunk stream (output-type (eql :latex)))
  (write-string "<<" stream)
  (write-string chunk-name stream)
  (write-string ">>=" stream)
  (terpri stream)
  (write-string chunk stream))

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

(defmethod write-code (code stream (output-type (eql :sphinx)))
  (write-string ".. code-block:: common-lisp" stream)
  (terpri stream)
  (write-string (indent-code code) stream)
  (terpri stream))

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
