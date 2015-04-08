(defpackage erudite.test
  (:use :cl :fiveam :erudite)
  (:export :run-tests))

(in-package :erudite.test)

(defun run-tests ()
  (run! 'erudite-tests))

(def-suite erudite-tests)

(in-suite erudite-tests)

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
       '(:DOC "this is a long comment in one line"))))

(test parse-short-comment-test
  (is (equalp
       (with-input-from-string (s ";; a short comment")
	 (erudite::parse-short-comment (read-line s) s))
       '(:doc "a short comment")))
  (is (null 
       (with-input-from-string (s ";;; a short comment")
	 (erudite::parse-short-comment (read-line s) s))))
  (is (null
       (with-input-from-string (s "a short comment")
	 (erudite::parse-short-comment (read-line s) s)))))

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

(test chunk-test
  (is 
   (equalp
    (erudite::process-file-to-string (test-file "chunk1.lisp"))
    "<<<chunk1>>>
"))
  (is 
   (equalp
    (erudite::process-file-to-string (test-file "chunk2.lisp"))
    "<<<chunk2>>>
This is the chunk:
<<chunk2>>=
This is a good chunk
\\begin{code}

(+ 1 1)

\\end{code}
"
)))

(test extract-test
  (is 
   (equalp
    (erudite::process-file-to-string (test-file "extract1.lisp"))
    "Extract test
This has been extracted
\\begin{code}
(+ 1 2)
\\end{code}
")))

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

(test include-test
  (is 
   (equalp
    (erudite::process-file-to-string (test-file "include1.lisp"))
    "Include test
This is includeA
\\begin{code}
(print \"include A\")
\\end{code}

\\begin{code}
(print \"Include\")
\\end{code}
This is includeB
\\begin{code}
(print \"include B\")
\\end{code}

")))

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
