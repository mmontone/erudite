;; @section Tests

(defpackage erudite.test
  (:use :cl :fiveam :erudite)
  (:export :run-tests))

(in-package :erudite.test)

;; Tests are run with @ref{run-tests}

(defun run-tests ()
  (run! 'erudite-tests))

(def-suite erudite-tests)

(in-suite erudite-tests)

;; @extract long-comment-test

(test parse-long-comment-test
  (is (equalp
       (with-input-from-string (s "#| this is
a long comment
|#")
         (erudite::parse-long-comment (read-line s) s))
       '(:DOC "this is
a long comment")))
  (is (equalp
       (with-input-from-string (s "")
         (erudite::parse-long-comment (read-line s nil) s))
       nil))
  (signals error
    (with-input-from-string (s "#| this is
a long comment")
      (erudite::parse-long-comment (read-line s nil) s)))
  (is (equalp
       (with-input-from-string (s " this is not a comment  ")
         (erudite::parse-long-comment (read-line s nil) s))
       nil))
  (is (equalp
       (with-input-from-string (s "   #| this is
a long comment
|# foo")
         (erudite::parse-long-comment (read-line s) s))
       '(:DOC "this is
a long comment")))
  (is (equalp
       (with-input-from-string (s "#| this is long comment in one line |#")
         (erudite::parse-long-comment (read-line s) s))
       '(:DOC "this is a long comment in one line")))
  (is (null
       (with-input-from-string (s "foo #| this is
a long comment
|#")
         (erudite::parse-long-comment (read-line s) s)))))

;; @end extract

;; @extract short-comment-test

(test parse-short-comment-test
  (is (equalp
       (with-input-from-string (s ";; a short comment")
         (erudite::parse-short-comment (read-line s) s))
       '(:doc "a short comment")))
  (is (equalp 
       (with-input-from-string (s ";;; a short comment")
	 (erudite::parse-short-comment (read-line s) s))
       '(:doc "a short comment")))
  (is (null
       (with-input-from-string (s "a short comment")
         (erudite::parse-short-comment (read-line s) s))))
  (is (null
       (with-input-from-string (s "a ;; short comment")
	 (erudite::parse-short-comment (read-line s) s)))))

;; @end extract

(defun test-file (filename)
  (merge-pathnames filename
                   (asdf:system-relative-pathname :erudite "test/")))

(test basic-processing-test
  (is
   (equalp
    (erudite::process-string ";; Hello
(print \"world\")")
    "Hello
\\begin{code}
(print \"world\")
\\end{code}
"))
  (is
   (equalp
    (erudite::process-string "#| Hello
|#
(print \"world\")")
    "Hello
\\begin{code}
(print \"world\")
\\end{code}
")))

;; @extract chunks-test

(test chunks-test
  (is
   (equalp
    (erudite::process-file-to-string (test-file "chunk1.lisp"))
    "This is a good chunk
\\begin{code}
<<<chunk1>>>
\\end{code}
"))
  (is
   (equalp
    (erudite::process-file-to-string (test-file "chunk2.lisp"))
    "This is a good chunk
\\begin{code}
<<<chunk2>>>
\\end{code}
This is the chunk:
\\begin{code}
<<chunk2>>=
(+ 1 1)

\\end{code}
"
))
(signals error
  (erudite::process-file-to-string (test-file "chunk3.lisp")))
(is
 (equalp
  (erudite::process-file-to-string (test-file "chunk4.lisp"))
  "\\begin{code}
<<chunk4>>=
(print \"Start\")

\\end{code}
The end
\\begin{code}
<<<chunk4>>>
\\end{code}
"))
(is (equalp
     (erudite::process-file-to-string (test-file "factorial.lisp"))
     "This is the factorial function:
\\begin{code}
(defun factorial (n)
  (if (<= n 1)
<<<base-case>>>
<<<recursive-case>>>
      ))

\\end{code}
The base case is simple, just check for \\verb|n=1| less:
\\begin{code}
<<base-case>>=
      1

\\end{code}
The recursive step is \\verb|n x n - 1|:
\\begin{code}
<<recursive-case>>=
      (* n (factorial (1- n)))

\\end{code}
")))

;; @end extract

;; @extract extract-test

(test extract-test
  (is
   (equalp
    (erudite::process-file-to-string (test-file "extract1.lisp"))
    "Extract test
This has been extracted
\\begin{code}
(+ 1 2)
\\end{code}
"))
(signals error
  (erudite::process-file-to-string (test-file "extract2.lisp")))
(is
 (equalp
  (erudite::process-file-to-string (test-file "extract3.lisp"))
  "Start
Extract 3
End
")))

;; @end extract

;; @extract ignore-test

(test ignore-test
  (is
   (equalp
    (erudite::process-file-to-string (test-file "ignore1.lisp"))
    "Ignore test
This is not ignored
\\begin{code}
(+ 1 3)
\\end{code}
")))

;; @end extract

;; @extract include-test

(test include-test
  (is
   (equalp
    (erudite::process-file-to-string (test-file "include1.lisp"))
    "Include test
This is includeA
\\begin{code}
(print \"include A\")
(print \"Include\")
\\end{code}
This is includeB
\\begin{code}
(print \"include B\")
\\end{code}
")))

;; @end extract


;; @extract erudite-syntax-tests

;; Erudite syntax tests

(test basic-syntax-test
  (is (equalp
       (erudite::process-string ";; hello world")
       "hello world
")))

(test section-syntax-test
  (is (equalp
       (erudite::process-string ";; @section Section test")
       "\\section{Section test}
")))

(test subsection-syntax-test
  (is (equalp
       (erudite::process-string ";; @subsection Subsection test")
       "\\subsection{Subsection test}
")))

(test subsubsection-syntax-test
  (is (equalp
       (erudite::process-string ";; @subsubsection Subsubsection test")
       "\\subsubsection{Subsubsection test}
")))

(test emphasis-syntax-test
  (is (equalp
       (erudite::process-string ";; This is @emph{emphasized}")
       "This is \\emph{emphasized}
"))
  (is (equalp
       (erudite::process-string ";; @emph{this} is @emph{emphasized}
;; isn't @emph{it}?")
       "\\emph{this} is \\emph{emphasized}
isn't \\emph{it}?
")))

(test bold-syntax-test
  (is (equalp
       (erudite::process-string ";; This is @bold{bold}")
       "This is \\textbf{bold}
"))
  (is (equalp
       (erudite::process-string ";; @bold{this} is @bold{bold}
;; isn't @bold{it}?")
       "\\textbf{this} is \\textbf{bold}
isn't \\textbf{it}?
")))

(test verbatim-syntax-test
  (is (equalp
       (erudite::process-string
        ";; @verbatim
;; This is verbatim
;; @end verbatim")
       "\\begin{verbatim}
This is verbatim
\\end{verbatim}
")))

(test code-syntax-test
  (is (equalp
       (erudite::process-string
        ";; @code
;; This is verbatim
;; @end code")
       "\\begin{code}
This is verbatim
\\end{code}
")))

(test list-syntax-test
  (is (equalp
       (erudite::process-string
        ";; @list
;; @item Item 1
;; @item Item 2
;; @end list")
       "\\begin{itemize}
\\item Item 1
\\item Item 2
\\end{itemize}
")))

;; @end extract

;; @extract when-test

(test when-test
  (is
   (equalp
    (erudite::process-file-to-string (test-file "when.lisp"))
    "When test
This should appear
")))

;; @end extract

;; @extract if-test

(test if-test
  (is
   (equalp
    (let ((erudite::*output-type* :latex))
      (erudite::process-file-to-string (test-file "if.lisp")))
    "This is latex text
"))
  (is
   (equalp
    (let ((erudite::*output-type* :sphinx))
      (erudite::process-file-to-string (test-file "if.lisp")))
    "This is sphinx text
"))
  (is
   (equalp
    (let ((erudite::*output-type* :markdown))
      (erudite::process-file-to-string (test-file "if.lisp")))
    "This is other output text
")))

;; @end extract

(test implicit/explicit-doc-test
  (is (equalp
       (let ((erudite::*implicit-doc* t))
	 (erudite::process-file-to-string (test-file "implicit.lisp")))
       "This is implicit doc
\\begin{code}
(print \"Hello world\")
\\end{code}
End
"))
(is (equalp
     (let ((erudite::*implicit-doc* nil))
       (erudite::process-file-to-string (test-file "implicit.lisp")))
     "\\begin{code}
;; This is implicit doc
(print \"Hello world\")
;; End
\\end{code}
"))
(is (equalp
     (let ((erudite::*implicit-doc* nil)
	   (erudite::*code-indexing* nil))
       (erudite::process-file-to-string (test-file "explicit.lisp")))
     "\\begin{code}
;; This is implicit and does not appear as doc
(print \"Hello world\")
\\end{code}
This is an explicit comment
This appears as doc
\\begin{code}
(defun bye ()
  ;; This comment goes in the code
  (print \"Bye\"))
\\end{code}
")))

(test explicit-code
  (is (equalp
       (let ((erudite:*implicit-code* nil)
	     (erudite:*code-indexing* nil))
	 (erudite::process-file-to-string (test-file "explicit-code.lisp")))
       "Explicit code test
This is the code:
\\begin{code}
(defun hello-world ()
  (print \"hello world\"))
\\end{code}
")))
