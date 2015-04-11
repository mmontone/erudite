;; @section Commands

;; @ignore
(in-package :erudite)
;; @end ignore

;; Commands are held in @ref{*commands*} list
(defvar *commands* nil)

(defun find-command (name &optional (error-p t))
  (let ((command (gethash name *commands*)))
    (when (and error-p (not command))
      (error "Invalid command: ~A" command))
    command))

(defun find-matching-command (line)
  (loop
     :for command :in *commands*
     :when (match-command command line)
     :return command))

;; @subsection Commands definition

(defmacro define-command (name &body body)
  (let ((match-function-def (or (find :match body :key #'car)
                                (error "Specify a match function")))
        (process-function-def (or (find :process body :key #'car)
                                  (error "Specify a process function"))))
    `(progn
       ,(destructuring-bind (_ match-args &body match-body) match-function-def
                            `(defmethod match-command ((command (eql ',name))
                                                       ,@match-args)
                               ,@match-body))
       ,(destructuring-bind (_ process-args &body process-body)
                            process-function-def
                            `(defmethod process-command ((command (eql ',name))
                                                         ,@process-args)
                               ,@process-body))
       (pushnew ',name *commands*))))

(defgeneric match-command (command line))

(defgeneric process-command (command line input output cont))

(defmethod process-command :before (command line input output cont)
  (log:debug "Processing `~A`" line))  

;; @subsection Commands list

;; @subsubsection Input type

(define-command syntax
  (:match (line)
    (scan "@syntax\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (syntax) ("@syntax\\s+(.+)" line)
              (setf *syntax* (intern (string-upcase syntax) :keyword)))
            (funcall cont)))

;; @subsubsection Output type
(define-command output-type
  (:match (line)
    (scan "@output-type\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (output-type) ("@output-type\\s+(.+)" line)
              (setf *output-type* (intern (string-upcase output-type) :keyword)))
            (funcall cont)))

;; @subsubsection Title

(define-command title
  (:match (line)
    (scan "@title\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (title) ("@title\\s+(.+)" line)
              (setf *title* title))
            (funcall cont)))

;; @subsubsection Subtitle

(define-command subtitle
  (:match (line)
    (scan "@subtitle\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (subtitle) ("@subtitle\\s+(.+)" line)
              (setf *subtitle* subtitle))
            (funcall cont)))

;; @subsubsection Author

(define-command author
  (:match (line)
    (scan "@author\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (author) ("@author\\s+(.+)" line)
              (setf *author* author))
            (funcall cont)))

;; @subsubsection Chunks

(defun find-chunk (chunk-name &key (error-p t))
  (or (assoc chunk-name *chunks* :test #'equalp)
      (error "Chunk not defined: ~A" chunk-name)))

(define-command insert-chunk
  (:match (line)
    (scan "@insert-chunk\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (chunk-name) ("@insert-chunk\\s+(.+)" line)
	      (format output "__INSERT_CHUNK__~A~%" chunk-name)
	      (funcall cont))))

;; @subsubsection Extraction

(defvar *extracts* nil)
(defvar *current-extract* nil)

(defun find-extract (extract-name &key (error-p t))
  (or (assoc extract-name *extracts* :test #'equalp)
      (and error-p
           (error "No text extracted with name: ~A" extract-name))))

(define-command extract
  (:match (line)
    (scan "@extract\\s+(.+)" line))
  (:process (line input output cont)
	    (register-groups-bind (extract-name) ("@extract\\s+(.+)" line)
              ;; Build and register the extracted piece for later processing
              ;; Redirect the output to the "extract output"
              (let* ((extract-output (make-string-output-stream))
		     (*current-extract* (list :name extract-name
                                               :output extract-output
                                               :original-output output)))
                  (funcall cont :output extract-output)))))

(define-command end-extract
  (:match (line)
    (scan "@end extract" line))
  (:process (line input output cont)
            (push (cons (getf *current-extract* :name)
                        (getf *current-extract* :output))
                  *extracts*)
            ;; Restore the output
            (funcall cont :output (getf *current-extract* :original-output))))

(define-command insert
  (:match (line)
    (scan "@insert\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (extract-name) ("@insert\\s+(.+)" line)
              (format output "__INSERT_EXTRACT__~A~%" extract-name)
	      (funcall cont))))

;; @subsubsection Ignore

(defvar *ignore* nil)

(define-command ignore
  (:match (line)
    (scan "@ignore" line))
  (:process (line input output cont)
            (setf *ignore* t)
            (funcall cont)))

(define-command end-ignore
  (:match (line)
    (scan "@end ignore" line))
  (:process (line input output cont)
            (setf *ignore* nil)
            (funcall cont)))

(defmethod process-doc :around (syntax output-type line stream cont)
  (if *ignore*
      (funcall cont)
      (call-next-method)))

(defmethod process-fragment :around ((type (eql :code)) fragment output cont)
  (if *ignore*
      (funcall cont)
      (call-next-method)))

(defmethod maybe-process-command :around (line input output cont)
  (if (and *ignore* (not (match-command 'end-ignore line)))
      (funcall cont)
      (call-next-method)))
