#|

@title Erudite
@subtitle Literate Programming System for Common Lisp

@author Mariano Montone
@syntax erudite

@section Introduction

@emph{Erudite} is a system for Literate Programming in Common Lisp.

Some of its salient features are:

@list

@item Documentation is written in Common Lisp comments. This is very useful because you can work with your program as if it were not a literate program: you can load it, work from SLIME, etc, directly.

@item Multiple syntaxes. Multiple type of literate syntax are supported. It is possible to choose from the default Erudite syntax, or use plain Latex or Sphinx syntax, and potentially others.

@item Multiple outputs. Like Latex, Sphinx, Markdown, HTML, etc.

@item Automatic indexing and cross-references.

@item A command line interface.

@item It is portable. You can compile and use in several CL systems.

@end list

@section Literate Programming

@subsection Concept

Literate programming is an approach to programming introduced by Donald Knuth in which a program is given as an explanation of the program logic in a natural language, such as English, interspersed with snippets of macros and traditional source code, from which a compilable source code can be generated.

The literate programming paradigm, as conceived by Knuth, represents a move away from writing programs in the manner and order imposed by the computer, and instead enables programmers to develop programs in the order demanded by the logic and flow of their thoughts. Literate programs are written as an uninterrupted exposition of logic in an ordinary human language, much like the text of an essay, in which macros are included to hide abstractions and traditional source code.

Literate programming tools are used to obtain two representations from a literate source file: one suitable for further compilation or execution by a computer, the "tangled" code, and another for viewing as formatted documentation, which is said to be "woven" from the literate source. While the first generation of literate programming tools were computer language-specific, the later ones are language-agnostic and exist above the programming languages.

@subsection Advantages

According to Knuth, literate programming provides higher-quality programs, since it forces programmers to explicitly state the thoughts behind the program, making poorly thought-out design decisions more obvious. Knuth also claims that literate programming provides a first-rate documentation system, which is not an add-on, but is grown naturally in the process of exposition of one's thoughts during a program's creation. The resulting documentation allows authors to restart their own thought processes at any later time, and allows other programmers to understand the construction of the program more easily. This differs from traditional documentation, in which a programmer is presented with source code that follows a compiler-imposed order, and must decipher the thought process behind the program from the code and its associated comments. The meta-language capabilities of literate programming are also claimed to facilitate thinking, giving a higher "bird's eye view" of the code and increasing the number of concepts the mind can successfully retain and process. Applicability of the concept to programming on a large scale, that of commercial-grade programs, is proven by an edition of TeX code as a literate program.

@subsection Contrast with document generation

Literate programming is very often misunderstood to refer only to formatted documentation produced from a common file with both source code and comments – which is properly called documentation generation – or to voluminous commentaries included with code. This is backwards: well-documented code or documentation extracted from code follows the structure of the code, with documentation embedded in the code; in literate programming code is embedded in documentation, with the code following the structure of the documentation.

This misconception has led to claims that comment-extraction tools, such as the Perl Plain Old Documentation or Java Javadoc systems, are "literate programming tools". However, because these tools do not implement the "web of abstract concepts" hiding behind the system of natural-language macros, or provide an ability to change the order of the source code from a machine-imposed sequence to one convenient to the human mind, they cannot properly be called literate programming tools in the sense intended by Knuth.

@section Other systems

@subsection LP/Lisp

@link{http://mainesail.umcs.maine.edu/software/LPLisp}{LP/Lisp} is an LP system for CL by Roy M. Turner. @emph{Erudite} shares several of its design decisions with it.

Contrary to traditional LP systems, but like @emph{Erudite} extracts text from CL comments. That makes it possible to work with the lisp program interactively; there's no tangling needed.

But unlike @emph{Erudite}:
@list
@item It is not portable. It runs on Allegro Common Lisp only.
@item It is tightly bound to Latex, but in its input and its output.
@item It is not very easily extensible in its current version (an extensible OO model is planned for its version 2).
@end list

@subsection CLWEB

@link{http://www.cs.brandeis.edu/~plotnick/clweb}{CLWEB} is a more traditional LP system for Common Lisp. It is not possible to work with the Lisp program in interpreter mode, as it requires previous code tangling.

@ignore
|#
(in-package #:erudite)

#|
@end ignore

@section Invocation

Erudite is invoked calling @ref{erudite} function.

@insert erudite-function

@section Algorithm

Multiple passes are run on the input files. This is because we want to be able to invoke chunks and extracts from file to file, from top to down and down to top. In a word, from everywhere without restrictions.

@subsection Includes expansion

In the first pass, @emph{include} directives are expanded to be able to process the whole thing from a single stream.
|#

(defvar *include-path* nil)

(defun expand-includes (stream)
  "Expand include directives"
  (with-output-to-string (output)
    (loop
       :for line := (read-line stream nil)
       :while line
       :do
       (cond
         ((scan "@include-path\\s+(.+)" line)
          (log:debug "~A" line)
          (register-groups-bind (path) ("@include-path\\s+(.+)" line)
            (setf *include-path* (pathname path))))
         ((scan "@include\\s+(.+)" line)
          (register-groups-bind (filename-or-path) ("@include\\s+(.+)" line)
            (let ((pathname (cond
                              ((fad:pathname-absolute-p
                                (pathname filename-or-path))
                               filename-or-path)
                              (*include-path*
                               (merge-pathnames filename-or-path
                                                *include-path*))
                              (*current-path*
                               (merge-pathnames filename-or-path
                                                *current-path*))
                              (t (error "No base path for include. This should not have happened")))))
              (log:debug "Including ~A" pathname)
              ;; Expand the included file source into output
              (with-input-from-string (source (file-to-string pathname))
                (write-string (expand-includes source) output))
              )))
         (t
          (write-string line output)
          (terpri output))))))

#|
@subsection Chunks extraction

After includes have been expanded, it is time to extract chunks.

@verb{@chunk} definitions are extracted from the source, and added to the @emph{*chunks*} list for later processing. The chunk name is printed via @emph{write-chunk-name} when a chunk is found.
|#

(defun extract-chunks (string)
  "Splits a file source in docs and code"
  (with-input-from-string (stream string)
    (with-output-to-string (output)
      (loop
         :with current-chunk := nil
         :for line := (read-line stream nil)
         :while line
         :do
         (cond
           ((scan "@chunk\\s+(.+)" line)
            (register-groups-bind (chunk-name) ("@chunk\\s+(.+)" line)
              (setf current-chunk (list :name chunk-name
                                        :output (make-string-output-stream)))
              (write-chunk-name chunk-name output)
              (terpri output)))
           ((scan "@end chunk" line)
            (push (cons (getf current-chunk :name)
                        (getf current-chunk :output))
                  *chunks*)
            (setf current-chunk nil))
           (current-chunk
            (let ((chunk-output (getf current-chunk :output)))
              (write-string line chunk-output)
              (terpri chunk-output)))
           (t
            (write-string line output)
            (terpri output)))))))

#|
Once both includes have been expanded, and chunks have been pre proccessed, the resulting output with literate code is parsed into @emph{fragments}. Fragments can be of type @it{documentation} or type @it{code}. @it{documentation} is the text that appears in Common Lisp comments. @it{code} fragments are the rest. This is done via the @ref{split-file-source} function.
|#

(defvar *parsing-doc* nil)

(defun split-file-source (str)
  "Splits a file source in docs and code"
  (setf *parsing-doc* nil)
  (with-input-from-string (stream str)
    (append-source-fragments
     (loop
        :for line := (read-line stream nil)
        :while line
        :collect
        (parse-line line stream)))))
#|
When splitting the source in fragments, we can parse either a long comment, a short comment, or lisp code:
|#

(defun parse-line (line stream)
  (or
   (parse-long-comment line stream)
   (parse-short-comment line stream)
   (parse-code line stream)))



;; Depending on the value of @ref{*implicit-comments*} we treat the comment as documentation or code

(defun parse-long-comment (line stream)
  "Parse a comment between #| and |#"
  (if *implicit-doc*
      (parse-long-comment-implicit line stream)
      (parse-long-comment-explicit line stream)))

(defun parse-long-comment-implicit (line stream)
  ;; TODO: this does not work for long comments in one line
  (when (equalp (search "#|" (string-left-trim (list #\  #\tab) line))
                0)
    (setf *parsing-doc* t)
    ;; We've found a long comment
    ;; Extract the comment source
    (let ((comment
           (with-output-to-string (s)
             ;;; First, add the first comment line
             (register-groups-bind (comment-line) ("\\#\\|\\s*(.+)" line)
               (write-string comment-line s))
                                        ; While there are lines without |#, add them to the comment source
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

(defun parse-long-comment-explicit (line stream)
  ;; TODO: this does not work for long comments in one line
  (when (scan "^\\s*\\#\\|\\s+@doc" line)
    ;; We've found a long comment explicit comment
    (setf *parsing-doc* t)
    ;; Extract the comment source
    (let ((comment
           (with-output-to-string (s)
;;; First, add the first comment line
             (register-groups-bind (comment-line)
                 ("^\\s*\\#\\|\\s+@doc\\s+(.+)" line)
               (when comment-line
                 (write-string comment-line s)))
                                        ; While there are lines without `|#` or `@end doc`, add them to the comment source
             (loop
                :for line := (read-line stream nil)
                :while (and line (not (or (search "|#" line)
                                          (search "@end doc" line))))
                :do
                (terpri s)
                (write-string line s)
                :finally
                ;; Finally, extract the last comment line
                (if line
                    (when (not (search "@end doc" line))
                      (register-groups-bind (comment-line) ("\\s*(.+)\\|\\#" line)
                        (when comment-line
                          (write-string comment-line s))))
                    (error "EOF: Could not complete comment parsing"))))))
      (list :doc comment))))

(defun parse-short-comment (line stream)
  (if *implicit-doc*
      (parse-short-comment-implicit line stream)
      (parse-short-comment-explicit line stream)))

(defun parse-short-comment-implicit (line stream)
  (when (equalp
         (search *short-comments-prefix*
                 (string-left-trim (list #\space #\tab)
                                   line))
         0)
    ;; A short comment was found
    (setf *parsing-doc* t)
    (let* ((comment-regex (format nil "~A\\s*(.+)" *short-comments-prefix*))
           (comment
            (or
             (register-groups-bind (comment-line) (comment-regex line)
               (string-left-trim (list #\; #\space)
                                 comment-line))
             "")))
      (list :doc comment))))

(defun parse-short-comment-explicit (line stream)
  (let ((regex (format nil "^\\s*~A\\s+@doc\\s*(.*)"
                       *short-comments-prefix*)))
    (cond
      ((and *parsing-doc*
            (search *short-comments-prefix*
                    (string-left-trim (list #\space #\tab)
                                      line)))

       (list :doc (string-left-trim (list #\; #\space)
                                    line)))
      ((ppcre:scan regex line)
       ;; A short comment was found
       (setf *parsing-doc* t)
       (let ((comment
              (or
               (register-groups-bind (comment-line) (regex line)
                 (string-left-trim (list #\; #\space)
                                   comment-line))
               "")))
         (list :doc comment))))))

(defun parse-code (line stream)
  (setf *parsing-doc* nil)
  (list :code line))

(defun append-source-fragments (fragments)
  "Append docs and code fragments"
  (let ((appended-fragments nil)
        (current-fragment (first fragments)))
    (loop
       :for fragment :in (cdr fragments)
       :do
       (if (equalp (first fragment) (first current-fragment))
           ;; The fragments are of the same type. Append them
           (setf (second current-fragment)
                 (with-output-to-string (s)
                   (write-string (second current-fragment) s)
                   (terpri s)
                   (write-string (second fragment) s)))
           ;; else, there's a new kind of fragment
           (progn
             (setf appended-fragments (append-to-end current-fragment appended-fragments))
             (setf current-fragment fragment))))
    (setf appended-fragments (append-to-end current-fragment appended-fragments))
    appended-fragments))

(defun process-fragments (fragments output)
  (when fragments
    (let ((first-fragment (first fragments)))
      (process-fragment (first first-fragment) first-fragment
                        output
                        (lambda (&key (output output))
                          (process-fragments (rest fragments) output))))))

(defgeneric process-fragment (fragment-type fragment output cont))

(defmethod process-fragment ((type (eql :code)) fragment output cont)
  ;; Ensure that this is not an empty code fragment first
  (when (not
         (zerop (length
                 (remove #\  (remove #\newline (second fragment))))))
    ;; Extract and output indexes if it is enabled
    (when *code-indexing*
      (let ((indexes (extract-indexes (second fragment))))
        (write-indexes indexes output *output-type*)))
    ;; Finally write the code fragment to the output
    (write-code (second fragment) output *output-type*))
  ;; Goon with the parsing
  (funcall cont))

(defmethod process-fragment ((type (eql :doc)) fragment output cont)
  (with-input-from-string (input (second fragment))
    (labels ((%process-fragment (&key (input input) (output output))
               (flet ((process-cont (&key (input input) (output output))
                        (%process-fragment :input input :output output)))
                 (let ((line (read-line input nil)))
                   (if line
                       (maybe-process-command line input output #'process-cont)
                       (funcall cont :output output))))))
      (%process-fragment))))

(defmethod maybe-process-command (line input output cont)
  "Process a top-level command"
  (let ((command (find-matching-command line)))
    (if command
        (process-command command line input output cont)
        (process-doc *syntax* *output-type* line output cont))))

(defmethod process-doc ((syntax (eql :latex)) output-type line stream cont)
  (write-string line stream)
  (terpri stream)
  (funcall cont))

(defmethod process-doc ((syntax (eql :sphinx)) output-type line stream cont)
  (write-string line stream)
  (terpri stream)
  (funcall cont))

(defmethod process-doc ((syntax (eql :org)) output-type line stream cont)
  (write-string line stream)
  (terpri stream)
  (funcall cont))

(defmethod process-doc ((syntax (eql :erudite)) output-type line stream cont)
  (let ((formatted-line line))
    (loop
       :for syntax :in *erudite-syntax*
       :while formatted-line
       :when (match-syntax syntax formatted-line)
       :do
       (setf formatted-line (process-syntax syntax formatted-line stream output-type))
       :finally (when formatted-line
                  (write-doc-line formatted-line stream output-type)))
    (terpri stream)
    (funcall cont)))

(defmethod write-doc-line (line stream output-type)
  (write-string line stream))

(defmethod write-code (code stream (output-type (eql :latex)))
  (if *latex-highlight-syntax*
      (progn
        (write-string "\\begin{minted}[fontsize=\\footnotesize]{common-lisp}" stream)
        (terpri stream)
        (write-string code stream)
        (terpri stream)
        (write-string "\\end{minted}" stream)
        (terpri stream))
      (progn
        (write-string "\\begin{code}" stream)
        (terpri stream)
        (write-string code stream)
        (terpri stream)
        (write-string "\\end{code}" stream)
        (terpri stream))))

(defmethod write-code (code stream (output-type (eql :sphinx)))
  (terpri stream)
  (write-string ".. code-block:: common-lisp" stream)
  (terpri stream)
  (terpri stream)
  (write-string (indent-code code) stream)
  (terpri stream)
  (terpri stream))

(defmethod write-code (code stream (output-type (eql :markdown)))
  (terpri stream)
  (write-string "```lisp" stream)
  (terpri stream)
  (write-string code stream)
  (terpri stream)
  (write-string "```" stream)
  (terpri stream))

(defmethod write-code (code stream (output-type (eql :org)))
  (write-string "#+BEGIN_SRC lisp" stream)
  (terpri stream)
  (write-string code stream)
  (terpri stream)
  (write-string " #+END_SRC" stream)
  (terpri stream))

(defmethod write-chunk-name (chunk-name stream)
  (write-string "<<<" stream)
  (write-string chunk-name stream)
  (write-string ">>>" stream))

(defmethod write-chunk (chunk-name chunk stream)
  (write-code (format nil "<<~A>>=~%~A" chunk-name chunk)
              stream *output-type*))

#|
@subsection Chunks and extracts post processing

Once the literate code has been parsed and processed, it is time to resolve the pending chunks and extracts. This is done in @emph{post-process-output} function.

@verb{INSERT_CHUNK} and @verb{INSERT_EXTRACT} are looked for and replaced by entries in @ref{*chunks*} and @ref{*extracts*}, respectively.
|#

(defun post-process-output (str)
  "Resolve chunk inserts and extract inserts after processing"
  (log:debug "Resolving chunks...")
  (with-output-to-string (output)
    (with-input-from-string (s str)
      (loop
         :for line := (read-line s nil)
         :while line
         :do
         (cond
           ((scan "^__INSERT_CHUNK__(.*)$" line)
            (register-groups-bind (chunk-name)
                ("^__INSERT_CHUNK__(.*)$" line)
              ;; Insert the chunk
              (let ((chunk (find-chunk chunk-name)))
                (write-chunk chunk-name
                             (get-output-stream-string (cdr chunk))
                             output))))
           ((scan "^__INSERT_EXTRACT__(.*)$" line)
            (register-groups-bind (extract-name)
                ("^__INSERT_EXTRACT__(.*)$" line)
              ;; Insert the extract
              (let ((extract (find-extract extract-name)))
                (write-string (get-output-stream-string (cdr extract))
                              output))))
           (t
            (write-string line output)
            (terpri output)))))))

#|
@subsection Conclusion

The whole process is invoked from @ref{process-file-to-string} function.

|#

(defmethod process-file-to-string ((pathname pathname))
  (let ((*current-path* (fad:pathname-directory-pathname pathname)))
    (with-open-file (f pathname)
      (post-process-output
       (with-output-to-string (s)
         (process-fragments
          (split-file-source
           (extract-chunks
            (expand-includes f)))
          s))))))

(defmethod process-file-to-string ((files cons))
  (post-process-output
   (with-output-to-string (s)
     (let ((*current-path*
            (fad:pathname-directory-pathname (first files))))
       (process-fragments
        (loop
           :for file :in files
           :appending
           (with-open-file (f file)
             (split-file-source
              (extract-chunks
               (expand-includes f)))))
        s)))))

(defmethod process-file-to-string :before (pathname)
  (setf *chunks* nil
        *extracts* nil))

(defmethod process-file-to-string :after (pathname)
  (setf *chunks* nil
        *extracts* nil))

(defun process-string (string)
  (let ((*chunks* nil)
        (*extracts* nil))
    (post-process-output
     (with-input-from-string (f string)
       (with-output-to-string (s)
         (process-fragments
          (split-file-source
           (extract-chunks
            (expand-includes f)))
          s))))))

#|

@section Source code indexing

|#

(defun parse-definition-type (str)
  (case (intern (string-upcase str))
    (defun :function)
    (defmacro :macro)
    (defclass :class)
    (defvar :variable)
    (defparameter :variable)
    (defmethod :method)
    (defgeneric :generic)
    (otherwise (intern (string-upcase str) :keyword))))

(defun extract-indexes (code)
  (let ((indexes))
    (loop
       :for line :in (split-sequence:split-sequence #\newline code)
       :do
       (do-register-groups (definition-type name)
           ("^\\((def\\S*)\\s+([^\\s(]*)" line)
         (push (list (parse-definition-type definition-type)
                     name)
               indexes)))
    indexes))

(defgeneric write-indexes (indexes output output-type))

(defmethod write-indexes (indexes output (output-type (eql :latex)))
  (when indexes
                                        ; (format output "\\lstset{~{index={~A}~^,~}}"
                                        ;           (mapcar (alexandria:compose #'escape-latex #'second)
                                        ;                   indexes))
    (loop for index in (remove-duplicates indexes :key #'second :test #'equalp)
       do
         (format output "\\index{~A}~%" (escape-latex (second index)))
         (format output "\\label{~A}~%" (latex-label (second index))))
    (terpri output)))

(defmethod write-indexes (indexes output (output-type (eql :sphinx)))
  ;; TODO: implement
  )

(defmethod write-indexes (indexes output (output-type (eql :markdown)))
  ;; TODO: implement
  )

(defmethod write-indexes (indexes output (output-type (eql :org)))
  ;; TODO: implement
  )

(defun escape-latex (str)
  (let ((escaped str))
    (flet ((%replace (thing replacement)
             (setf escaped (regex-replace-all thing escaped replacement))))
      (%replace "\\\\" "\\textbackslash")
      (%replace "\\&" "\\&")
      (%replace "\\%" "\\%")
      (%replace "\\$" "\\$")
      (%replace "\\#" "\\#")
      (%replace "\\_" "\\_")
      (%replace "\\{" "\\{")
      (%replace "\\}" "\\}")
      (%replace "\\~" "\\textasciitilde")
      (%replace "\\^" "\\textasciicircum")
      escaped)))

(defun latex-label (str)
  (let ((escaped str))
    (flet ((%replace (thing replacement)
             (setf escaped (regex-replace-all thing escaped replacement))))
      (%replace "\\\\" "=")
      (%replace "\\&" "=")
      (%replace "\\%" "=")
      (%replace "\\$" "=")
      (%replace "\\#" "=")
      (%replace "\\_" "=")
      (%replace "\\{" "=")
      (%replace "\\}" "=")
      (%replace "\\~" "=")
      (%replace "\\^" "=")
      escaped)))
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

@section Outputs

@emph{Erudite} supports LaTeX, Markdown and Sphinx generation at the moment.

@subsection LaTeX
|#

(defvar *latex-highlight-syntax* nil
  "Highlight syntax using LaTeX minted package: https://ctan.org/pkg/minted")

(defgeneric gen-doc (output-type output files &rest args))

(defmethod gen-doc ((output-type (eql :latex)) output files
                    &key
                      (title *title*)
                      (subtitle *subtitle*)
                      (author *author*)
                      template-pathname
                      (syntax *syntax*)
                      (document-class *latex-document-class*)
                      (highlight-syntax *latex-highlight-syntax*)
                      &allow-other-keys)
  "Generates a LaTeX document.

   Args: - output: The output stream.
         - files: The list of .lisp files to compile
         - title: Document title.
         - subtitle: Document subtitle.
         - author: Author of the document
         - template-pathname: A custom LaTeX template file. If none is specified, a default template is used."
  (let ((*latex-document-class* document-class)
        (*latex-highlight-syntax* highlight-syntax))
    (let ((template (cl-template:compile-template
                     (file-to-string (or template-pathname
                                         (asdf:system-relative-pathname
                                          :erudite
                                          "resource/template.tex")))))
          (body (process-file-to-string files)))
      (write-string
       (funcall template (list :title (or title
                                          *title*
                                          (error "No document title specified"))
                               :subtitle (or subtitle
                                             *subtitle*)
                               :author (or author
                                           *author*
                                           (error "No document author specified"))
                               :body body))
       output))
    t))
#|

@subsection Sphinx

Sphinx is the other kind of output apart from LaTeX.

|#

(defmethod gen-doc ((output-type (eql :sphinx)) output files &key prelude postlude syntax &allow-other-keys)
  "Generates Sphinx document.

   Args: - output: The output stream.
         - files: .lisp files to compile.
         - prelude: String (or pathname) to append before the Sphinx document.
         - postlude: String (or pathname) to append after the Sphinx document."

  (when prelude
    (write-string
     (if (pathnamep prelude)
         (file-to-string prelude)
         prelude)
     output))
  (write-string (process-file-to-string files) output)
  (when postlude
    (write-string (if (pathnamep postlude)
                      (file-to-string postlude)
                      postlude)
                  output)))

#|

@subsection Markdown

Markdown is another output type.

|#

(defmethod gen-doc ((output-type (eql :markdown)) output files &key prelude postlude syntax &allow-other-keys)
  "Generates Markdown document.

   Args: - output: The output stream.
         - files: .lisp files to compile.
         - prelude: String (or pathname) to append before the document.
         - postlude: String (or pathname) to append after the document."

  (when prelude
    (write-string
     (if (pathnamep prelude)
         (file-to-string prelude)
         prelude)
     output))
  (write-string (process-file-to-string files) output)
  (when postlude
    (write-string (if (pathnamep postlude)
                      (file-to-string postlude)
                      postlude)
                  output)))

(defmethod gen-doc ((output-type (eql :org)) output files &key prelude postlude syntax
                                                            (title *title*)
                                                            (subtitle *subtitle*)
                                                            (author *author*)
                                                            &allow-other-keys)
  "Generates Emacs org-mode document.

   Args: - output: The output stream.
         - files: .lisp files to compile.
         - prelude: String (or pathname) to append before the document.
         - postlude: String (or pathname) to append after the document."

  (when prelude
    (write-string
     (if (pathnamep prelude)
         (file-to-string prelude)
         prelude)
     output))

  (let ((title (or title *title*)))
    (when title
      (format output "#+TITLE: ~a~%~%" title)))

  (let ((author (or author *author*)))
    (when author
      (format output "#+AUTHOR: ~A~%~%" author)))

  (write-string (process-file-to-string files) output)

  (when postlude
    (write-string (if (pathnamep postlude)
                      (file-to-string postlude)
                      postlude)
                  output)))

#|

@extract erudite-function

|#

(defun call-with-destination (destination function)
  (cond
    ((null destination)
     (with-output-to-string (output)
       (funcall function output)))
    ((pathnamep destination)
     (with-open-file (f destination :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
       (funcall function f)))
    ((streamp destination)
     (funcall function destination))
    ((eql destination t)
     (funcall function *standard-output*))
    (t (error "Invalid destination: ~A" destination))))

(defun maybe-invoke-debugger (condition)
  "This function is called whenever a
condition CONDITION is signaled in Erudite."
  (if (not *catch-errors-p*)
      (invoke-debugger condition)
      (format t "ERROR: ~A~%" condition)))

(defun call-with-error-handling (catch-errors-p function)
  (setf *catch-errors-p* catch-errors-p)
  (handler-case
      (funcall function)
    (error (e)
      (maybe-invoke-debugger e))))

(defmacro with-destination ((var destination) &body body)
  `(call-with-destination ,destination
                          (lambda (,var) ,@body)))

(defmacro with-error-handling ((&optional (catch-errors-p '*catch-errors-p*))
                               &body body)
  `(call-with-error-handling ,catch-errors-p (lambda () ,@body)))

(defun erudite (destination file-or-files
                &rest args &key
                             (output-type *output-type*)
                             (syntax *syntax*)
                             (debug *debug*)
                             (verbose *verbose*)
                             (catch-errors-p *catch-errors-p*)
                             (code-indexing *code-indexing*)
                             (implicit-doc *implicit-doc*)
                             (implicit-code *implicit-code*)
                             (short-comments-prefix *short-comments-prefix*)
                             &allow-other-keys)
  "Processes literate lisp files and creates a document.

   Args: - destination: If NIL, output is written to a string. If T, output is written to *standard-output*. If a pathname, then a file is created. Otherwise, a stream is expected.
         - files: Literate lisp files to compile
         - args: All sort of options passed to the generation functions
         - output-type: The kind of document to generate.
                        One of :latex, :sphinx
                        Default: :latex
         - syntax: The kind of syntax used in the literate source files.
                       One of: :erudite, :latex, :org, :sphinx.
                       Default: :erudite"
  (with-error-handling (catch-errors-p)
    (with-destination (output destination)
      (let ((*output-type* output-type)
            (*syntax* syntax)
            (*debug* debug)
            (*verbose* verbose)
            (*implicit-doc* implicit-doc)
            (*implicit-code* implicit-code)
            (*short-comments-prefix* short-comments-prefix))
        (log:config :sane :this-console)
        (when *verbose*
          (log:config :info))
        (when *debug*
          (log:config :debug))
        (log:info "Starting. Output type: ~A" output-type)
        (apply #'gen-doc output-type
               output
               (if (listp file-or-files)
                   file-or-files
                   (list file-or-files))
               args)
        (log:config :clear)))))

#|

@end extract

|#

;; @include cli.lisp
;; @include commands.lisp
;; @include syntax/erudite.lisp
;; @include test/test.lisp
