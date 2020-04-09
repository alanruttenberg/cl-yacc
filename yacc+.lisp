;;; This file is automatically generated from the lilith file 'yacc+.org'.
;;; It is meant to be loaded by a common lisp directly, without depending on Lilith
;;; This file keeps all text in the original file as lisp comments, except
;;; for the org-mode comments and directives.

;; * Table of Contents                                               :noexport:TOC:
;; - [[#usage][Usage]]
;; - [[#overview][Overview]]
;; - [[#implementation][Implementation]]
;;   - [[#the-operators-and-other-constants][The operators and other constants]]
;;   - [[#naming-productions-to-be-added][Naming productions to be added]]
;;   - [[#keeping-track-of-added-productions][Keeping track of added productions]]
;;   - [[#rewriting-a-sequence][Rewriting a sequence]]
;;   - [[#transforming-a-grammar][Transforming a grammar]]
;;   - [[#constructed-the-added-productions][Constructed the added productions]]
;;   - [[#handling-placeholders][Handling placeholders]]
;; - [[#hooking-cl-yacc][Hooking cl-yacc]]
;; - [[#this-code-written-in-org-mode][This code written in org mode]]
;; - [[#testing][Testing]]
;; * Usage
;; We assume familiarity with [[https://github.com/jech/cl-yacc][cl-yacc]], which this code extends.
;; The extra syntax allows use of four operators.
;; | (:+ ... 'action)  | one or more repetitions                                                         |
;; | (:* ... 'action)  | zero or more repetitions                                                        |
;; | (:? ... 'action)  | optional - can be present or not                                                |
;; | (:or ... 'action) | alternatives. Usual notation is pipe but we can't comfortably use that in lisp. |
;; In the case of :+ and :*, the action is applied to each of the
;; repetitions, which are gathered into a list.  Supplying the actions is
;; optional. The default actions in all cases is 'identity if the clause
;; has a single element and 'list otherwise.
;; * Overview
;; The idea is to take a grammar, which may include the new syntax, and
;; construct a new grammer that cl-yacc can use. It works by replacing
;; productions that use the syntax with modified ones based on them and
;; by adding new productions where necessary.
;; | *:?*  | Creates two productions, copying all but the :? form. One has the optional element one doesn't.                                                                                                                  |
;; | *:+*  | Creates two new productions to handle the repetition. One matches the basic pattern, and the other multiples of it. The name of the latter substituted in place of the :+ form.                                  |
;; | *:**  | As with :+ but makes it optional.                                                                                                                                                                                |
;; | *:or* | Creates a set of productions, each of which has one of the alternatives and a copy of the rest. Originally I made a single new production with the alternatives, but that didn't work well with precedence rule. |
;; * Implementation 

(in-package :yacc)

;; The main rewrite operates on a sequence - the body of one element of a
;; production.  The elements in the sequenced are processed in order. The
;; rewrite is recursive, with the state held in the ~tstate~ structure. As
;; rewriting occurs we may need to make copies of what we already have
;; processed. A head is the translation of the so-far processed
;; elements. ~heads~ accumulates the possibly multiple variants created
;; during processing.  ~tail~ is a list of the remaining elements.

(defstruct (tstate (:conc-name "")
                   (:constructor tstate (heads tail)) )
  heads
  tail
  )

;; ** The operators and other constants
;; I wasn't sure if I wanted to use keywords for the operators or
;; non-keyword symbols. Define the operators as constants. 
;; For an explanation of ~*placeholder-mark*~ see the section <<Handling placeholders>> below.

(defconstant $01 :?)
(defconstant $1+ :+)
(defconstant $0+ :*)
(defconstant $or :or)
(defconstant *operators* (list $01 $1+ $0+ $or))
(defconstant *placeholder-mark* '_)

;; ** Naming productions to be added
;; New productions names are created with make-symbol, so as not to
;; conflict with elements of the original grammar, and numbered
;; sequentially. ~*new-production-counter*~ is the counter. 
;; For debugging purposes you can override this to have the production 
;; names interned in the current package by setting ~*new-productions-in-package*~.

(defvar *new-production-counter*)
(defvar *new-production-prefix* "PRODUCTION-")
(defvar *new-productions-in-package* nil)

;; There are two types of new productions, one for the operated on elements
;; and one that handles multiples of the first. The name of the production that gathers
;; the multiples is the name of the singular one, concatenated with "s".
;; In some cases we record the plural name during the first pass. In that 
;; case the name of the singular production is constructed from it by removing 
;; the "s".

(defun fresh-production-name-s ()
  (let ((name (format nil "PRODUCTION-~aS" (incf *new-production-counter*))))
    (if *new-productions-in-package*
	(intern name)
	(make-symbol name))))

(defun fresh-production-name ()
  (let ((name (format nil "PRODUCTION-~a" (incf *new-production-counter*))))
    (if *new-productions-in-package*
	(intern name)
	(make-symbol name))))

(defun production-name-singular (s-name)
  (let ((name (subseq (string s-name) 0 (- (length (string s-name)) 1))))
    (if *new-productions-in-package*
	(intern name)
	(make-symbol name))))

;; ** Keeping track of added productions
;; A global, ~*aux*~ accumulates the extra productions. 

(defvar *aux*)

;; ~*aux*~ is a list of pairs of a sequence, a name, and marker as to whether it is
;; plural.  For plurals, the name is of the plural production, otherwise
;; the name of the production. Because of the way I do the rewrite, I sometimes
;; have to find the name of an existing production. ~already-in-aux~ takes the
;; basis of the new production and returns it's name.

(defun add-plural-aux (what is-optional)
  (let ((replacements (fresh-production-name-s)))
    (push (list what replacements :multiple is-optional) *aux*)
    replacements))

(defun add-single-aux (what)
  (let ((replacement (fresh-production-name)))
    (push `(,what ,replacement) *aux*)
    replacement))

(defun already-in-aux (what)
  (second (find what *aux* :test 'equalp :key 'car)))

(defun aux-production-is-optional (name)
  (if (consp name) (setq name (car name)))
  (fourth (find name *aux* :key 'second)))

;; ** Rewriting a sequence
;; ~transform-sequence~ is the main recursive function. It calls functions
;; to handle the different cases.
;; *** Rewriting \$? 
;; The simple idea is to create two productions, one with and one without the 
;; element. However, this doesn't work when there is more than one element in the
;; in the clause.
;; (a (:? b c))
;; Our general rule is that each element is one argument to the action. So,
;; when there is more than one element we create a production for the
;; optional part, and recurse on the rest. 
;; Calls to transform sequence will only return when the rest of the
;; sequence is processed at which the only important thing is returning the
;; heads (now complete).  ~merge-heads~ returns a tstate with the two sets
;; of heads appended.
;; For the case where we're removing the clause, we leave a placeholder, 
;; which we'll process out in a later step.

(defun transform-optional (state)
  (let* ((opt (car (tail state)))
	 (token (if (= (length (cdr opt)) 1)
		    (second opt)
		    (add-single-aux (cdr opt)))))
    (merge-heads
     (list
      (transform-sequence (tstate (heads state) (cons token (cdr (tail state))) ))
      (transform-sequence (tstate (heads state) (cons *placeholder-mark* (cdr (tail state))) ))))))

(defun merge-heads (states)
  (tstate (apply 'append (mapcar 'heads states)) nil))


;; *** Rewriting \$+ and \$*
;; Suppose we have
;; (test
;;  (a (:+ b c 'one-rep) 'action))
;; This gets rewritten as
;; (test
;;  (a prod1s 'action))
;; (prod1
;;  (b c 'one-rep))
;; (prod1s 
;;  (prod1 'repetition-action)
;;  (prod1s prod1 'repetition-action))
;; ~transform-+~ handles both cases, begin passed the operator as an argument. 
;; If $* the $+ rewrite the production is marked as being optional. The optionality
;; will manifest in consume-next.
;; Each call to transform-sequence is independently processing each of the
;; accumulated heads, each of which sees the new tail. Without intervention
;; the same pair of productions would be recreated for each. To avoid that
;; we check whether we've already made the production by looking on
;; ~*aux*~, and if we find something we use the name ~already-in-aux~
;; returns.
;; The additional productions prod1 and prod1s are created in a second pass.
;; For now we record just the sequence (b c) and the name of the plural prod1s.

(defun transform-+ (state &optional operator)
  (let* ((what (cdr (car (tail state))))
	 (existing (already-in-aux what)))
    (if existing 
	(transform-sequence (tstate (heads state) (cons existing (cdr (tail state))) ))
	(let ((replacements (add-plural-aux what (eq operator $0+))))
	  (transform-sequence
	   (tstate (heads state)
		   (cons replacements (cdr (tail state)))))))))

;; *** Rewriting alternates
;; We create modified versions of the original, in each case substituting
;; one of the alternatives. As in transform-optional, we merge the heads
;; once we're done. Parentheses are allowed to group elements of an alternate.
;; (main
;;  (a (:or b (c d))))
;; is rewritten as
;; (main 
;;   (a b)
;;   (a prod1)
;; (prod1
;;   (c d))

(defun transform-alternates (state)
  (let* ((alternatives (cdr (car (tail state)))))
    (loop for alt in alternatives
	  collect 
	  (if  (and (consp alt) (not (member (car alt) *operators*)))
	      (transform-sequence (tstate (heads state) (cons (add-single-aux alt) (cdr (tail state))) ))
	      (transform-sequence (tstate (heads state) (cons alt (cdr (tail state))) )))
	    into newstates
	  finally (return (merge-heads newstates)))))

;; *** Consuming the next simple element in the sequence
;; Consuming the next simple element is mostly straightforward - just pop it 
;; off the tail and add it to the end of each head. The exception
;; is when we are consuming a token marked as optional. In that case
;; also include a transformation with a placeholder in place of the 
;; token.
;; (transform-sequence #S(tstate :HEADS nil :TAIL (#:production-1s 'identity))) 
;;    3> Calling (aux-production-is-optional #:production-1s) t 

(defun consume-next (state)
  (let ((new-heads
	  (if (null (heads state))
	      (list* (list (car (tail state)))
		     (if (aux-production-is-optional (car (tail state)))
			 (list (list *placeholder-mark*))))
	      (loop for head in (heads state) 
		    when (aux-production-is-optional (car (tail state)))
		      collect (append head (list *placeholder-mark*))
		    collect (append head (list (car (tail state))))))))
    (transform-sequence (tstate new-heads (cdr (tail state)) ))))

;; *** Main routine
;; Iterates through the sequence dispatching to the appropriate transformer.
;; Returns the accumulated state if tail is nil.

(defun transform-sequence (tstate)
  (if (null (tail tstate))
      tstate
      (let ((heads (heads tstate))
	    (tail (tail tstate)))
	(cond ((atom tail)
		(tstate 
		 (if heads 
		     (mapcar (lambda(head) (append head (list (car tail)))) heads)
		     `(,tail))
		 nil))
	      ((and (consp (car tail)) (eq (caar tail) $or))
	       (transform-alternates tstate))

	      ((and (consp (car tail)) (eq (caar tail) $01))
	       (transform-optional tstate))

	      ((and (consp (car tail)) (eq (caar tail) $1+))
	       (transform-+ tstate $1+))

	      ((and (consp (car tail)) (eq (caar tail) $0+))
	       (transform-+ tstate $0+))

	      ((not (consp (car tail)))
	       (consume-next tstate))

	      ((member (car tail) *operators*) ; sometimes result of other transformations
	       (transform-sequence (tstate heads (list tail) )))

	      ((and (consp (car tail)) ; an action
		    (not (member (car tail) *operators*))
		    (null (cdr tail)))
	       (tstate (mapcar (lambda(head) (append head (list (car tail)))) heads) nil))

	      (t (error "Syntax error in production: ~a" tail))
	      ))))

;; ** Transforming a grammar
;; Takes the set of productions and returns a new set. 
;; Every production is a list of a non-terminal symbol and one or more
;; right hand sides.  Every right hand side is either a symbol, or a list
;; of symbols optionally followed with an action.

(defun transform-yacc-productions (yacc-input)
  (let* ((*new-production-counter* 0)
	 (*aux* nil))
    (append (loop for (lhs . rh-sides) in yacc-input
		  collect
		  (loop for rhs in rh-sides
			for transformed = (transform-sequence (tstate nil rhs))
			append
			(mapcar (lambda(h) (fix-placeholders h))
				(heads transformed)) into rewritten
			finally (return `(,lhs ,@rewritten))))
	    (construct-additional-productions))))

;; ** Constructed the added productions
;; At this point ~*aux*~ has a list of what needs to be implemented in additional
;; productions. The constructed productions are either singular, as is the case for
;; optional sequences, or for a repetition as in $+ and $*. ~construct-additional-productions~
;; iterates over ~*aux*~ and either creates the singular production or calls expand-repetition 
;; to do the work. For the singular cases the production is just (name ,@sequence). 
;; Since the new productions may themselves land up adding to ~*aux*~, we 
;; keep processing it until empty.

(defun construct-additional-productions  ()
  (loop for (body name multiple) = (car *aux*)
	until (null *aux*)
	do (pop *aux*)
	if multiple append (expand-repetition body name)
	  else collect (expand-singular-aux body name)))

;; For repetitions we'll create two productions.
;; Say we have 
;; (main 
;;   (a (:+ b c)))
;; We construct
;; (main
;;   (a prod-1))
;; (prod-1 
;;   (b c))
;; (prod-1s
;;   (prod-1 'repetition-action)
;;   (prod-1s prod-1 'repetition-action))
;; Note that the newly created productions may themselves have need to be
;; transformed, so we do that here, and also fix the placeholders.

(defun sequence-has-action? (tail)
  (and (or (and (consp (car (last tail)))
		(not (member (caar (last tail)) *operators*)))
	   (functionp (car (last tail))))
       (car (last tail))))

(defun expand-repetition (body repeatsname)
  (let ((singlename (production-name-singular repeatsname))
	(has-action (sequence-has-action? body)))
    `((,singlename ,@(loop for head in (heads (transform-sequence (tstate nil body)))
			   for head-with-action = (if has-action
						      head
						      (append head (list (if (> (length head) 1)
									     ''list
									     ''identity))))
			   collect (fix-placeholders head-with-action)))
      (,repeatsname
       (,singlename 'repetition-action )
       (,repeatsname ,singlename 'repetition-action)))))

(defun expand-singular-aux (body name)
  (let ((has-action (sequence-has-action? body)))
    `(,name ,@(loop for head in (heads (transform-sequence (tstate nil body)))
		    for head-with-action = (if has-action
					       head
					       (append head (list (if (> (length head) 1)
								      ''list
								      ''identity))))
		    collect (fix-placeholders head-with-action)))))



;; The repetition action accumulates into a list. If called with one argument it
;; returns a list of it, and if two adds the second to the end of the first.
;; Using the example above, here's the sequence (a b c b c b c) being parsed.
;; | match            | action call                         | action             | result         |
;; | prod-1           | (list a b)                          | list               | '(b c)         |
;; | prod-1s clause 1 | (repetition-action '(b c))          | list               | '((b c))       |
;; | prod-1           | (list a b)                          | list               | '(b c)         |
;; | prod-1s clause 2 | (repetition-action '((b c)) '(b c)) | add element to end | '((b c) (b c)) |

(defun repetition-action (els &optional el)
  (if (null el)
      (list els)
      (append els (list el))))

;; ** Handling placeholders
;; Suppose we have
;; (main
;;   (a (:? b) 'my-action))
;; The sequence has two elements, and so my-action is a function that accepts two arguments. 
;; However, after transformation there are two productions.
;; (main
;;   (a b 'my-action))
;;   (a 'my-action))
;; But then my-action will be called with only one argument if the first
;; sequence matches. Oops.
;; Instead we generate 
;; (main
;;   (a b 'my-action))
;;   (a _ 'my-action))
;; Then, we remove the placeholder and replace 'my action with 
;; (lambda(x) (my-action x nil))
;; Here's a more involved case 
;; (a (:? b) (:? c) d)
;; | Pattern   | w/placeholder | Action call                        | wrapper                       |
;; | (a b c d) | (a b c d)     | (action first second third fourth) | none
;; | (a b d)   | (a b _ d)     | (action first second nil third)    | (lambda(a c d) (action a b nil d))                       |
;; | (a c d)   | (a _ c d)     | (action first nil second third)    | (lambda(a c d) (action a nil c d)) |
;; | (a d)     | (a _ _ d)     | (action first nil nil second)      | (lambda(a d) (action a nil nil d)) |
;; ~fix-placeholders~ does this work. Note that since we /need/ the original
;; action here, if one isn't supplied we default it to 'list.

(defconstant *nothing* nil)

(defun fix-placeholders (sequence)
  (if (atom sequence)
      sequence
      (let ((action (sequence-has-action? sequence)))
	(if action
	    (setq sequence (butlast sequence))
	    (setq action (if (> (length sequence) 1) ''list ''identity)))
	(when (member *placeholder-mark* sequence)
	  (let ((vars (loop repeat (length (remove *placeholder-mark* sequence)) collect (gensym))))
	    (setq action (symbol-function
			  (compile (gensym)
				   `(lambda (,@vars)
				      (funcall ,action 
					       ,@(loop for el in sequence
						       if (eq el *placeholder-mark*) collect *nothing*
							 else collect (pop vars)))))))))
	(append (remove *placeholder-mark* sequence)
		(list action)))))

;; * Hooking cl-yacc
;; In make-grammar, instead of calling make-production on each production while iterating, collect them. Then 
;; transform the collection, then collect make-production on each of transformed productions.
;; * This code written in org mode
;; This code is written in org mode using [[https://github.com/alanruttenberg/lilith][lilith]]. For convenience a lisp version
;; is in the same directory, generated with
;; (lp::tangle-org-file (asdf::system-relative-pathname "yacc" "yacc+.org")
;; 		     :output-file (asdf::system-relative-pathname "yacc" "yacc+.lisp"))
;; The original yacc.asd includes the lisp file.
;; yacc+.asd loads the org file directly, and provides an asdf test op.
;; * Testing
;; If you are using yacc.asd, tests are in yacc-tests.lisp and yacc+-tests.lisp.
;; Loading these run the tests. Rerun the yacc+ tests with (test-yacc+).
;; If you are using yacc+.asd then (asdf:test-system :yacc+)

