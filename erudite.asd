(asdf:defsystem #:erudite
  :description "Literate Programming System for Common Lisp"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
	       (:file "config")
	       (:file "util")
	       (:file "commands")
	       (:module :syntax
			:components
			((:file "erudite")))
	       (:file "erudite"))
  :depends-on (:cl-fad
	       :cl-ppcre
	       :alexandria
	       :split-sequence
	       :cl-template)
  :in-order-to ((asdf:test-op (asdf:test-op :erudite-test))))
