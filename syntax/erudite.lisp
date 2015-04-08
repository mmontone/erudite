;; @section Erudite syntax

;; @ignore
(in-package :erudite)
;; @end ignore

;; Erudite formatting operations are held in @ref{*erudite-syntax*} list
(defvar *erudite-syntax* nil)

(defun find-syntax (name &optional (error-p t))
  (let ((command (gethash name *erudite-syntax*)))
    (when (and error-p (not command))
      (error "Invalid syntax: ~A" command))
    command))

;; @subsection Syntax definition

(defmacro define-erudite-syntax (name &body body)
  (let ((match-function-def (or (find :match body :key #'car)
                                (error "Specify a match function")))
        (process-function-def (or (find :process body :key #'car)
                                  (error "Specify a process function"))))
    `(progn
       ,(destructuring-bind (_ match-args &body match-body) match-function-def
                            `(defmethod match-syntax ((command (eql ',name))
						      ,@match-args)
                               ,@match-body))
       ,(destructuring-bind (_ process-args &body process-body)
                            process-function-def
                            `(defmethod process-syntax ((command (eql ',name))
							,@process-args)
                               ,@process-body))
       (pushnew ',name *erudite-syntax*))))

;; @subsection Commands list

;; @subsubsection Section
(define-erudite-syntax section
  (:match (line)
    (scan "@section" line))
  (:process (line output output-type)
	    (register-groups-bind (title) 
		("@section\\s+(.+)" line)
	      (format-syntax output (list :section title)))
	    nil))

;; @subsubsection Subsection
(define-erudite-syntax subsection
  (:match (line)
    (scan "@subsection" line))
  (:process (line output output-type)
	    (register-groups-bind (title) 
		("@subsection\\s+(.+)" line)
	      (format-syntax output (list :subsection title)))
	    nil))

;; @subsubsection Subsubsection
(define-erudite-syntax subsubsection
  (:match (line)
    (scan "@subsubsection" line))
  (:process (line output output-type)
	    (register-groups-bind (title) 
		("@subsubsection\\s+(.+)" line)
	      (format-syntax output (list :subsubsection title)))
	    nil))

;; @subsubsection Verbatim
(define-erudite-syntax begin-verbatim
  (:match (line)
    (scan "@verbatim" line))
  (:process (line output output-type)
	    (format-syntax output (list :begin-verbatim))
	    nil))

(define-erudite-syntax end-verbatim
  (:match (line)
    (scan "@end verbatim" line))
  (:process (line output output-type)
	    (format-syntax output (list :end-verbatim))
	    nil))

;; @subsubsection Code
(define-erudite-syntax begin-code
  (:match (line)
    (scan "@code" line))
  (:process (line output output-type)
	    (format-syntax output (list :begin-code))
	    nil))

(define-erudite-syntax end-code
  (:match (line)
    (scan "@end code" line))
  (:process (line output output-type)
	    (format-syntax output (list :end-code))
	    nil))

;; @subsubsection Lists
(define-erudite-syntax begin-list
  (:match (line)
    (scan "@list" line))
  (:process (line output output-type)
	    (format-syntax output (list :begin-list))
	    nil))

(define-erudite-syntax end-list
  (:match (line)
    (scan "@end list" line))
  (:process (line output output-type)
	    (format-syntax output (list :end-list))
	    nil))

(define-erudite-syntax list-item
  (:match (line)
    (scan "@item" line))
  (:process (line output output-type)
	    (regex-replace "@item" line
			   (lambda (match)
			     (format-syntax nil (list :list-item)))
			   :simple-calls t)))

;; @subsubsection Emphasis
(define-erudite-syntax emphasis
  (:match (line)
    (scan "@emph{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@emph{(.*?)}" line
			       (lambda (match text)
				 (format-syntax nil (list :emph text)))
			       :simple-calls t)))

;; @subsubsection Bold
(define-erudite-syntax bold
  (:match (line)
    (scan "@bold{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@bold{(.*?)}" line
			       (lambda (match text)
				 (format-syntax nil (list :bold text)))
			       :simple-calls t)))

;; @subsubsection Italics
(define-erudite-syntax italics
  (:match (line)
    (scan "@it{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@it{(.*?)}" line
			       (lambda (match text)
				 (format-syntax nil (list :italics text)))
			       :simple-calls t)))

;; @subsubsection Inline verbatim
(define-erudite-syntax inline-verbatim
  (:match (line)
    (scan "@verb{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@verb{(.*?)}" line
			       (lambda (match text)
				 (format-syntax nil (list :verbatim text)))
			       :simple-calls t)))

;; @subsubsection Reference
(define-erudite-syntax reference
  (:match (line)
    (scan "@ref{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@ref{(.*?)}" line
			       (lambda (match text)
				 (format-syntax nil (list :ref text)))
			       :simple-calls t)))

;; @subsection Syntax formatting

;; @subsubsection Latex output

(defun format-syntax (destination syntax)
  (if (null destination)
      (with-output-to-string (stream)
	(%format-syntax *output-type* (first syntax) stream  syntax))
      (%format-syntax *output-type* (first syntax) destination syntax)))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :section))
			   stream
			   syntax)
  (format stream "\\section{~A}" (second syntax)))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :subsection))
			   stream
			   syntax)
  (format stream "\\subsection{~A}" (second syntax)))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :subsubsection))
			   stream
			   syntax)
  (format stream "\\subsubsection{~A}" (second syntax)))

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
			   (selector (eql :ref))
			   stream
			   syntax)
  (format stream "\\verb#~A#" (second syntax)))

