(in-package #:erudite)

(defun file-to-string (pathname)
  (with-open-file (stream pathname)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defun parse-lisp-source (string)
  (loop
     :with fragments = nil
     :with prev-char = nil
     :with mode = :code
     :with fragment = nil
     :for char :across string
     :do
     ;(format t "prevchar: ~A char: ~A  mode: ~A~%" prev-char char mode)
     (cond
       ((and (equalp prev-char #\#)
             (char= char #\|)
             (equalp mode :code))
        ;; Documentation fragment starts
        (setf mode :doc)
        (unless (null fragment)
          (push (list :code (coerce fragment 'string)) fragments)
          (setf fragment nil)
	  (setf char nil)))
       ((and (equalp prev-char #\|)
             (char= char #\#)
             (equalp mode :doc))
        ;; Documentation fragment ends
        (setf mode :code)
        (unless (null fragment)
          (push (list :doc (coerce fragment 'string)) fragments)
          (setf fragment nil)
	  (setf char nil)))
       ((and (equalp mode :code)
	     (equalp prev-char #\#)
	     (not (equalp char #\|)))
	;; False documentation start
	(setf fragment (append fragment (list prev-char char))))
       ((and (equalp mode :doc)
	     (equalp prev-char #\|)
	     (not (equalp char #\#)))
	;; False documentation end
	(setf fragment (append fragment (list prev-char char))))
       ((member char (list #\# #\|) :test #'char=)
	;; Dont output, could be special characters
	)
       (t
        ;; Accumulate char in current fragment
        (setf fragment (append fragment (list char)))))
     (setf prev-char char)
     :finally (unless (null fragment)
                (push (list mode (coerce fragment 'string)) fragments))
     (return (reverse fragments))))

(defun indent-code (code)
  "Code in sphinx has to be indented"
  (let ((lines (split-sequence:split-sequence #\newline
					      code)))
    (apply #'concatenate 'string
	   (mapcar (lambda (line)
		     (format nil "     ~A~%" line))
		   lines))))

(defun compile-sphinx-fragments (fragments)
  (apply #'concatenate 'string
	 (loop for fragment in fragments
	    collect
	      (ecase (first fragment)
		(:code (format nil ".. code-block:: common-lisp~%~%     ~A"
			       (indent-code 
				(string-trim (list #\  #\newline) 
					     (second fragment)))))
		(:doc (second fragment))))))

(defun compile-latex-fragments (fragments)
  (apply #'concatenate 'string
	 (loop for fragment in fragments
	      collect
	      (ecase (first fragment)
		(:code (format nil "\\begin{code}~%~A~%\\end{code}"
			       (string-trim (list #\  #\newline) 
					    (second fragment))))
		(:doc (second fragment))))))

(defun gen-latex-doc (pathname files &key title author)
  (let ((template (cl-template:compile-template
		   (file-to-string (asdf:system-relative-pathname 
				    :erudite 
				    "latex/template.tex"))))
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

(defun gen-sphinx-doc (pathname files &key prelude postlude)
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

      

