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


