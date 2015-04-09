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

;; @subsection Commands list

;; @subsubsection Input type

(define-command input-type
  (:match (line)
    (scan "@input-type\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (input-type) ("@input-type\\s+(.+)" line)
              (setf *input-type* (intern (string-upcase input-type) :keyword)))
            (funcall cont)))

;; @subsubsection Chunks

(defun find-chunk (chunk-name &key (error-p t))
  (or (assoc chunk-name *chunks* :test #'equalp)
      (error "Chunk not defined: ~A" chunk-name)))

(define-command echo
  (:match (line)
    (scan "@echo\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (chunk-name) ("@echo\\s+(.+)" line)
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

(defmethod process-doc :around (input-type output-type line stream cont)
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

;; @subsubsection Include

(defvar *include-path* nil)

(define-command include-path
  (:match (line)
    (scan "@include-path\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (path) ("@include-path\\s+(.+)" line)
              (setf *include-path* (pathname path))
              (funcall cont))))

(define-command include
  (:match (line)
    (scan "@include\\s+(.+)" line))
  (:process (line input output cont)
            (register-groups-bind (filename-or-path) ("@include\\s+(.+)" line)
              (let ((pathname (cond
                                ((fad:pathname-absolute-p
                                  (pathname filename-or-path))
                                 filename-or-path)
                                (*include-path*
                                 (merge-pathnames filename-or-path
                                                  *include-path*))
                                (t (merge-pathnames filename-or-path
                                                    *current-path*)))))
                ;; Process and output the included file
                (write-string (process-file-to-string pathname) output)
		(terpri output)
		(funcall cont)))))
