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

;; @subsubsection Emphasis
(define-erudite-syntax emphasis
  (:match (line)
    (scan "@emph{(.*?)}" line))
  (:process (line output output-type)
	    (regex-replace-all "@emph{(.*?)}" line
			       (lambda (match text)
				 (format-syntax nil (list :emph text)))
			       :simple-calls t)))

(defun format-syntax (destination syntax)
  (if (null destination)
      (with-output-to-string (stream)
	(%format-syntax *output-type* (first syntax) stream  syntax))
      (%format-syntax *output-type* (first syntax) destination syntax)))

;; @subsection Syntax formatting

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :section))
			   stream
			   syntax)
  (format stream "\\section{~A}" (second syntax)))

(defmethod %format-syntax ((output-type (eql :latex))
			   (selector (eql :emph))
			   stream
			   syntax)
  (format stream "\\emph{~A}" (second syntax)))
