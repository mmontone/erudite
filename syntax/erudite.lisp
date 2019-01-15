;; @section Erudite syntax

;; @ignore
(in-package :erudite)
;; @end ignore

;; Erudite formatting operations are held in @ref{*erudite-syntax*} list
(defvar *erudite-syntax* nil)
(defvar *latex-document-class* :article)

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

;; @subsection Syntax elements

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
				 (format-syntax nil (list :inline-verbatim text)))
			       :simple-calls t)))

;; @subsubsection Link
(define-erudite-syntax link
  (:match (line)
    (scan "@link{(.*?)}{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@link{(.*?)}{(.*?)}" line
			       (lambda (match target label)
				 (format-syntax nil (list :link target label)))
			       :simple-calls t)))

;; @subsubsection Label
(define-erudite-syntax label
  (:match (line)
    (scan "@label{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@label{(.*?)}" line
			       (lambda (match label)
				 (format-syntax nil (list :label label)))
			       :simple-calls t)))

;; @subsubsection Index
(define-erudite-syntax index
  (:match (line)
    (scan "@index{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@index{(.*?)}" line
			       (lambda (match text)
				 (format-syntax nil (list :index text)))
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

(defun format-syntax (destination syntax)
  (if (null destination)
      (with-output-to-string (stream)
	(%format-syntax *output-type* (first syntax) stream  syntax))
      (%format-syntax *output-type* (first syntax) destination syntax)))
