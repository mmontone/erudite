(in-package :erudite)

;; @subsubsection Latex output

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :section))
			   stream
			   syntax)
  (ecase *latex-document-class*
    (:article (format stream "\\section{~A}" (second syntax)))
    (:book (format stream "\\chapter{~A}" (second syntax)))))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :subsection))
			   stream
			   syntax)
  (ecase *latex-document-class*
    (:article (format stream "\\subsection{~A}" (second syntax)))
    (:book (format stream "\\section{~A}" (second syntax)))))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :subsubsection))
			   stream
			   syntax)
  (ecase *latex-document-class*
    (:article (format stream "\\subsubsection{~A}" (second syntax)))
    (:book (format stream "\\subsection{~A}" (second syntax)))))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :begin-verbatim))
			   stream
			   syntax)
  (format stream "\\begin{verbatim}"))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :end-verbatim))
			   stream
			   syntax)
  (format stream "\\end{verbatim}"))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :inline-verbatim))
			   stream
			   syntax)
  (format stream "\\verb|~A|" (second syntax)))


(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :begin-code))
			   stream
			   syntax)
  (format stream "\\begin{code}"))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :end-code))
			   stream
			   syntax)
  (format stream "\\end{code}"))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :begin-list))
			   stream
			   syntax)
  (format stream "\\begin{itemize}"))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :end-list))
			   stream
			   syntax)
  (format stream "\\end{itemize}"))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :list-item))
			   stream
			   syntax)
  (format stream "\\item" (second syntax)))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :emph))
			   stream
			   syntax)
  (format stream "\\emph{~A}" (second syntax)))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :bold))
			   stream
			   syntax)
  (format stream "\\textbf{~A}" (second syntax)))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :italics))
			   stream
			   syntax)
  (format stream "\\textit{~A}" (second syntax)))


(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :link))
			   stream
			   syntax)
  (destructuring-bind (_ target label) syntax
    (format stream "\\href{~A}{~A}"
	    target label)))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :label))
			   stream
			   syntax)
  (format stream "\\label{~A}"
	  (latex-label (second syntax))))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :ref))
			   stream
			   syntax)
  (format stream "\\hyperref[~A]{~A}"
	  (escape-latex (second syntax))
	  (latex-label (second syntax))))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :index))
			   stream
			   syntax)
  (format stream "\\index{~A}"
	  (escape-latex (second syntax))))
