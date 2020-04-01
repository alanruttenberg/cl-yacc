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
  ()
  :serial t
  :perform (test-op (op system)
		    (format t "Running original yacc tests...~a~%"
			    (if (load (system-relative-pathname :yacc+ "yacc-tests.lisp"))
				"passed" "failed"))
		    (load (system-relative-pathname :yacc+ "yacc+-tests.lisp")))
  )
