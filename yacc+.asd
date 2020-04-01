(in-package :asdf)

(defsystem :yacc+
  :components 
  ((:file "yacc")
   (:org "yacc+"))
  :depends-on ()
  :in-order-to ((test-op (test-op :yacc+/test)))
  :defsystem-depends-on (lilith))

(defsystem :yacc+/test
  :components 
  ((:file  "yacc-tests"))
  :perform (test-op (op system)
		    (format t "Running original yacc tests...~a~%"
			    (if (funcall (intern "TESTS" "YACC-TESTS"))
				"passed" "failed"))
		    (load (system-relative-pathname :yacc+ "yacc+-tests.lisp")))
  )
