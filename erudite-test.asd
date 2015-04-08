(asdf:defsystem #:erudite-test
  :description "Erudite tests"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:module :test
			:components
			((:file "test"))))
  :depends-on (:erudite :fiveam)
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :erudite.test :run-tests)))
