(asdf:defsystem #:erudite
  :description "Poor Lisper's Literate Programming System"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "erudite"))
  :depends-on (:cl-fad
	       :cl-ppcre
	       :alexandria
	       :split-sequence
	       :cl-template)
  :in-order-to ((asdf:test-op (asdf:test-op :erudite-test))))
