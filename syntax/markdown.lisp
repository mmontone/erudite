(in-package :erudite)

;; @subsubsection Markdown output

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :section))
			   stream
			   syntax)
  (let ((section-name (second syntax)))
    (terpri stream)
    (write-string "# " stream)
    (write-string section-name stream)
    (terpri stream)))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :subsection))
			   stream
			   syntax)
  (let ((subsection-name (second syntax)))
    (terpri stream)
    (write-string "## " stream)
    (write-string subsection-name stream)
    (terpri stream)))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :subsubsection))
			   stream
			   syntax)
  (let ((subsubsection-name (second syntax)))
    (terpri stream)
    (write-string "### " stream)
    (write-string subsubsection-name stream)
    (terpri stream)))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :begin-verbatim))
			   stream
			   syntax)
  (format stream "```"))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :end-verbatim))
			   stream
			   syntax)
  (format stream "```"))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :inline-verbatim))
			   stream
			   syntax)
  (format stream "`~A`" (second syntax)))


(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :begin-code))
			   stream
			   syntax)
  (terpri stream)
  (format stream "```lisp")
  (terpri stream))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :end-code))
			   stream
			   syntax)
  (format stream "```"))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :begin-list))
			   stream
			   syntax))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :end-list))
			   stream
			   syntax))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :list-item))
			   stream
			   syntax)
  (format stream "* "))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :emph))
			   stream
			   syntax)
  (format stream "*~A*" (second syntax)))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :bold))
			   stream
			   syntax)
  (format stream "**~A**" (second syntax)))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :italics))
			   stream
			   syntax)
  (format stream "_~A_" (second syntax)))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :link))
			   stream
			   syntax)
  (destructuring-bind (_ target label) syntax
    (format stream "[~A](~A)" label target)))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :label))
			   stream
			   syntax))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :ref))
			   stream
			   syntax)
  (format stream "~A" (second syntax)))

(defmethod %format-syntax ((output-type (eql :markdown))
			   (selector (eql :index))
			   stream
			   syntax))


