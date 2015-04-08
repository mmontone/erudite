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

(defvar *commands* nil)
(defvar *short-comments-prefix* ";;")
(defvar *input-type* :erudite)
(defvar *output-type* :latex)

(defun process-file-to-string (pathname)
  (with-open-file (f pathname)
    (with-output-to-string (s)
      (erudite::process-parts
       (erudite::split-file-source f)
       s))))

(defun find-command (name &optional (error-p t))
  (let ((command (gethash name *commands*)))
    (when (and error-p (not command))
      (error "Invalid command: ~A" command))
    command))

(defmacro define-command (name &body body)
  (let ((match-function-def (or (find :match body :key #'car)
                                (error "Specify a match function")))
        (process-function-def (or (find :process body :key #'car)
                                  (error "Specify a process function"))))
    `(progn
       ,(destructuring-bind (_ match-args &body match-body) match-function-def
                            `(defmethod match-command ((command (eql ',name))
                                                       ,@match-args)
                               ,@match-body))
       ,(destructuring-bind (_ process-args &body process-body)
                            process-function-def
                            `(defmethod process-command ((command (eql ',name))
                                                         ,@process-args)
                               ,@process-body))
       (pushnew ',name *commands*))))

;;; The parser works like a custom look-ahead parser, with a whole file line
;;; being the slice looked ahead.

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

(define-command input-type
  (:match (line)
    (scan "@input-type\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (input-type) ("@input-type\\s+(.+)" line)
              (setf *input-type* (intern (string-upcase input-type) :keyword)))
            (funcall cont)))

;; \subsection{Chunks}

(defvar *chunks* nil)
(defvar *current-chunk* nil)

(defun find-chunk (chunk-name &key (error-p t))
  (or (assoc chunk-name *chunks* :test #'equalp)
      (error "Chunk not defined: ~A" chunk-name)))

(define-command chunk
  (:match (line)
    (scan "@chunk\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (chunk-name) ("@chunk\\s+(.+)" line)
              ;; Output the chunk name
              (write-chunk-name chunk-name output *output-type*)
              (terpri output)
              ;; Build and register the chunk for later processing
              ;; Redirect the output to the "chunk output"
              (with-output-to-string (chunk-output)
                (let ((*current-chunk* (list :name chunk-name
                                             :output chunk-output
                                             :original-output output)))
                  (funcall cont :output chunk-output)
                  )))))

(define-command end-chunk
  (:match (line)
    (scan "@end chunk" line))
  (:process (line input output cont)
            (push (cons (getf *current-chunk* :name)
                        (getf *current-chunk* :output))
                  *chunks*)
            ;; Restore the output
            (funcall cont :output (getf *current-chunk* :original-output))))

(define-command echo
  (:match (line)
    (scan "@echo\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (chunk-name) ("@echo\\s+(.+)" line)
              ;; Insert the chunk
              (let ((chunk (find-chunk chunk-name)))
                (write-chunk chunk-name
                             (get-output-stream-string (cdr chunk))
                             output
                             *output-type*)
                (funcall cont)))))

;; \subsection{Extraction}

(defvar *extracts* nil)
(defvar *current-extract* nil)

(defun find-extract (extract-name &key (error-p t))
  (or (assoc extract-name *extracts* :test #'equalp)
      (and error-p
           (error "No text extracted with name: ~A" extract-name))))

(define-command extract
  (:match (line)
    (scan "@extract\\s+(.+)" line))
  (:process (line input output cont)
	    (register-groups-bind (extract-name) ("@extract\\s+(.+)" line)
              ;; Build and register the extracted piece for later processing
              ;; Redirect the output to the "extract output"
              (with-output-to-string (extract-output)
                (let ((*current-extract* (list :name extract-name
                                               :output extract-output
                                               :original-output output)))
                  (funcall cont :output extract-output))))))

(define-command end-extract
  (:match (line)
    (scan "@end extract" line))
  (:process (line input output cont)
            (push (cons (getf *current-extract* :name)
                        (getf *current-extract* :output))
                  *extracts*)
            ;; Restore the output
            (funcall cont :output (getf *current-extract* :original-output))))

(define-command insert
  (:match (line)
    (scan "@insert\\s+(.+)" line))
  (:process (line input output cont)
	    (register-groups-bind (extract-name) ("@insert\\s+(.+)" line)
              ;; Insert the extract
              (let ((extract (find-extract extract-name)))
                (write-string (get-output-stream-string (cdr extract))
                              output)
                (funcall cont)))))

;; \subsection{Ignore commmand}

(defvar *ignore* nil)

(define-command ignore
  (:match (line)
    (scan "@ignore" line))
  (:process (line input output cont)
	    (setf *ignore* t)
	    (funcall cont)))

(define-command end-ignore
  (:match (line)
    (scan "@end ignore" line))
  (:process (line input output cont)
	    (setf *ignore* nil)
	    (funcall cont)))

(defmethod process-doc :around (input-type output-type line stream cont)
  (if *ignore*
      (funcall cont)
      (call-next-method)))

(defmethod process-part :around ((type (eql :code)) part output cont)
  (if *ignore*
      (funcall cont)
      (call-next-method)))

(defmethod maybe-process-command :around (line input output cont)
  (if (and *ignore* (not (match-command 'end-ignore line)))
      (funcall cont)
      (call-next-method)))

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
  (write-string "\\end{code}" stream))

(defmethod write-chunk-name (chunk-name stream (output-type (eql :latex)))
  (write-string "<<<" stream)
  (write-string chunk-name stream)
  (write-string ">>>" stream))

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
