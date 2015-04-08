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

;; @subsection Syntax formatting

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
