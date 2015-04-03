(asdf:defsystem #:cl-embdoc
  :description "Describe cl-embdoc here"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "embdoc"))
  :depends-on (:cl-fad
	       :cl-ppcre
	       :alexandria
	       :split-sequence))
