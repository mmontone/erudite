(defpackage erudite.test
  (:use :cl :fiveam :erudite))

(in-package :erudite.test)

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

(test chunk-test
  (is 
   (equalp
    (erudite::process-file-to-string (test-file "chunk1.lisp"))
    "<<<chunk1>>>"))
  (is 
   (equalp
    (erudite::process-file-to-string (test-file "chunk2.lisp"))
    "<<<chunk2>>")))
