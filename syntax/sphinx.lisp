(in-package :erudite)

(defvar *sphinx-indent* nil)

(defmethod write-doc-line :before (line output (output-type (eql :sphinx)))
  (when *sphinx-indent*
    (write-string "    " output)))

;; @subsubsection Sphinx output

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :section))
			   stream
			   syntax)
  (let ((section-name (second syntax)))
    (terpri stream)
    (write-string section-name stream)
    (terpri stream)
    (loop :for char :across section-name
	  :do (write-char #\= stream))
    (terpri stream)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :subsection))
			   stream
			   syntax)
  (let ((subsection-name (second syntax)))
    (terpri stream)
    (write-string subsection-name stream)
    (terpri stream)
    (loop :for char :across subsection-name
	  :do (write-char #\- stream))
    (terpri stream)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :subsubsection))
			   stream
			   syntax)
  (let ((subsubsection-name (second syntax)))
    (terpri stream)
    (write-string subsubsection-name stream)
    (terpri stream)
    (loop :for char :across subsubsection-name
	  :do (write-char #\^ stream))
    (terpri stream)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :begin-verbatim))
			   stream
			   syntax)
  (format stream "::~%")
  (setf *sphinx-indent* t))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :end-verbatim))
			   stream
			   syntax)
  (setf *sphinx-indent* nil))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :inline-verbatim))
			   stream
			   syntax)
  (format stream "``~A``" (second syntax)))


(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :begin-code))
			   stream
			   syntax)
  (terpri stream)
  (format stream ".. code-block:: common-lisp")
  (terpri stream)
  (terpri stream)
  (setf *sphinx-indent* t))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :end-code))
			   stream
			   syntax)
  (terpri stream)
  (setf *sphinx-indent* nil))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :begin-list))
			   stream
			   syntax))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :end-list))
			   stream
			   syntax))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :list-item))
			   stream
			   syntax)
  (format stream "* "))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :emph))
			   stream
			   syntax)
  (format stream "*~A*" (second syntax)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :bold))
			   stream
			   syntax)
  (format stream "**~A**" (second syntax)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :italics))
			   stream
			   syntax)
  (format stream "*~A*" (second syntax)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :link))
			   stream
			   syntax)
  (destructuring-bind (_ target label) syntax
    (format stream "`~A <~A>`_" label target)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :label))
			   stream
			   syntax)
  (format stream ".. _~A:" (second syntax)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :ref))
			   stream
			   syntax)
  (format stream ":ref:`~A`" (second syntax)))

(defmethod %format-syntax ((output-type (eql :sphinx))
			   (selector (eql :index))
			   stream
			   syntax))
