(in-package :asdf)

(defsystem :yacc+
  :components 
  ((:file "yacc")
   (:org "yacc+"))
  :serial t
  :depends-on ()
  :defsystem-depends-on (lilith))

