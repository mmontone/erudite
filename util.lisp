(in-package :erudite)

(defun file-to-string (pathname)
  (with-open-file (stream pathname)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))
