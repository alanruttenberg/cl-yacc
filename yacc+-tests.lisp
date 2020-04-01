(in-package :yacc)

(defparameter *yacc+-tests* nil)

(defmacro yacc+test (why grammar &body sequences)
  `(push (list ,why ',grammar ',sequences) *yacc+-tests*))

(defun test-yacc+ ()
  (loop for (why grammar sequences) in (reverse *yacc+-tests*)
	do (format t "Testing ~a (~a) ...~a~%"
		   why
		   (length sequences)
		   (if (test-yacc+-parser grammar sequences)
		       "passed"
		       "failed"))))

(defun test-yacc+-parser (grammar pattern-expected*)
  (let* ((terminals 
	   (set-difference
	    (remove :error (remove nil (remove-duplicates (alexandria.1.0.0:flatten pattern-expected*))))
	    *operators*))
	 (defining-form  `(define-parser test (:start-symbol main)
			    (:terminals ,terminals)
			    ,@(transform-yacc-productions grammar))))
    (multiple-value-bind (res errorp) (ignore-errors (eval defining-form))
      (when errorp
	(pprint defining-form)
	(error "Error defining parser: ~a" errorp)))
    (flet ((list-lexer (list)
	     #'(lambda ()
		 (let ((x (pop list)))
		   (values x x)))))
      (let ((result 
	      (loop for (pattern expected) in pattern-expected*
		    for (parsed errorp)  =  (multiple-value-list
					     (ignore-errors
					      (parse-with-lexer  (list-lexer pattern) test)))
		    for result =  (or (equalp parsed expected)
				      (and (eq expected :error) errorp))
		    unless result 
		      do (format t "~a failed. Expected ~a but got ~a~%" pattern expected (or parsed errorp))
		    always result)))
	(unless result
	  (pprint grammar)
	  (pprint defining-form))
	result))))


(yacc+test "Optionals"
    ((main 
      (a (:? b) (:? c))))
  ((a) (a nil nil))
  ((a b) (a b nil))
  ((a c) (a nil c))
  ((a b c) (a b c)))

(yacc+test "Zero or more repeats"
    ((main 
      (a (:* b))))
  ((a) (a nil))
  ((a b) (a (b)))
  ((a b b) (a (b b)))
  )

(yacc+test "One or more repeats single"
    ((main 
      (a (:+ b))))
  ((a) :error)
  ((a b) (a (b)))
  ((a b b) (a (b b)))
  )

(yacc+test "One or more repeats sequence"
    ((main 
      (a (:+ b c))))
  ((a) :error)
  ((a b c) (a ((b c))))
  ((a b c b c) (a ((b c) (b c))))
  )

(yacc+test "One or more repeats with something following"
    ((main 
      (a (:+ b c) d)))
  ((a) :error)
  ((a b c d) (a ((b c)) d))
  ((a b c b c d) (a ((b c) (b c)) d))
  )

(yacc+test "One or more followed by optional"
    ((main 
      (a (:+ b c) (:? d))))
  ((a) :error)
  ((a b c d) (a ((b c)) d))
  ((a b c b c d) (a ((b c) (b c)) d))
  ((a b c) (a ((b c)) nil ))
  ((a b c b c) (a ((b c) (b c)) nil))
  )

(yacc+test "Zero or more followed by optional"
    ((main 
      (a (:* b c) (:? d))))
  ((a) (a nil nil))
  ((a b c d) (a ((b c)) d))
  ((a b c b c d) (a ((b c) (b c)) d))
  ((a b c) (a ((b c)) nil ))
  ((a b c b c) (a ((b c) (b c)) nil))
  ((a d) (a nil d))
  )

(yacc+test "Alternates"
    ((main
      (a (:or b c d))))
  ((a) :error)
  ((a e) :error)
  ((a b c) :error)
  ((a b) (a b))
  ((a c) (a c))
  ((a d) (a d)))

(yacc+test "Optional alternates"
    ((main
      (a (:? (:or b c d)))))
  ((a) (a nil))
  ((a b) (a b))
  ((a c) (a c))
  ((a d) (a d)))

(yacc+test "One or more alternates"
    ((main
      (a (:+ (:or b c d)))))
  ((a) :error)
  ((a b b c c) (a (b b c c)))
  ((a c) (a (c)))
  ((a d d d) (a (d d d ))))


  ;;  trouble-making. This pattern can't work 
  ;;  (:+ (:* b c d))

  ;; (prove::ok (test-yacc++-parser
  ;; 	      '((main
  ;; 		 (a (:+ (:* b c d)))))
  ;; 	      '(((a) (a (nil)))
  ;; 		((a b c d) (a ((b c d)(b c d)))))))

(yacc+test "Repetition with specified action"
    ((main
      (arg (:* |,| arg (lambda(comma arg) arg))
	   (lambda(arg1 restargs) (cons arg1 restargs)))))
  ((arg) (arg ))
  ((arg |,|  arg) (arg arg))
  ((arg |,|  arg |,| arg) (arg arg arg)))

(test-yacc+)
