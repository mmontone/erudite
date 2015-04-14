;; Explicit code test
(defun this-code-does-not-appear ()
  (print "error"))
;; This is the code:
;; @code
(defun hello-world ()
  (print "hello world"))
;; @end code
