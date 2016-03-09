(in-package :omega)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mateja's stuff ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf choose '(  DEFNEXP-M-B  (star  ORIR-M-B ) (disj  (ORIL-M-B REFLEX-M-B)  REFLEX-M-B ) ))

(lea~produce-method choose :method 'choose-m-b :inference 'choose-m :theory 'zmz)

(cri~def-control-rule choose-first
		      (kind methods)
		      (if (always-true))
		      (then
		       (prefer (
				choose-m-b))))


(setf learnt-tryanderror '(DEFNEXP-M-B  (star  (FORALLI-SORT-RESCLASS-M-B EXPAND-IN-M-F) )  (CONVERT-RESCLASS-TO-NUM-M-B OR-E**-M-B)  (star-max  SIMPLIFY-NUMERICAL-EXPR-M-B ) (star-max  REFLEX-M-B ) ))

(lea~produce-method learnt-tryanderror :method 'learnt-tryanderror-m-b :inference 'learnt-tryanderror-m :theory 'zmz)
 
(cri~def-control-rule learnt-tryanderror-first
		      (kind methods)
		      (if (always-true))
		      (then
		       (prefer (
				learnt-tryanderror-m-b))))


(strat~define-strategy-ks
 (name TryAndLearn)
 (refinement-algorithm PPlanner)
 (condition close-goal-p)
 (methods (
	   DefnExp-m-b
	   ForallI-Sort-Resclass-Function-m-b
	   ForallI-Sort-Resclass-m-b
	   ForallI-Sort-Integer-Interval-m-b
	   Convert-Resclass-To-Num-m-b
	   Or-E**-m-b
	   ExistsI-Sort-Resclass-Function-m-b
	   ExistsI-Sort-Resclass-m-b
	   ExistsI-Sort-Integer-Interval-m-b
	   OrIL-m-b OrIR-m-b
	   Expand-Pair-Operation-m-b
	   Expand-in-m-b
	   Expand-not-in-m-b
	   choose-m-b
	   learnt-tryanderror-m-b
	   
	   ;; The following methods are all in the normalization and restriction methods
	   ;; Therefore, we placed them here after the other methods
	   ;; Notice that  Convert-Resclass-To-Num-m-b, Expand-Pair-Operation-m-b could be also in
	   ;; the restriction methods, but:
	   ;; Convert-resclass is VERRY VERRY complex hence, you will not always try to apply it!
	   ;; Expand-Pair-Operation-m-b caused some problems when in the restriction methods (i forgot the exact problem ...)
	   ;; Since Simplify-Resclass-m-b should be tried after Expand-Pair-Operation-m-b it is also not in the restriction methods

	   AndE-M-F PosNat-m-b RewritePowerOfOp-m-b
	   Expand-in-m-f Expand-not-in-m-f
	   PullNeg-m-b AndI-M-B Or2Imp-m-b
	   Reflex-m-b NotReflex-m-b
	   Reduce-Exists-To-Pairs-m-b
	   Reduce-Forall-To-Pairs-m-b
	   DecompNotElementOfCartProd-m-b
	   DecompElementOfCartProd-m-b
	   DecompPairinInequation-m-b
	   DecompPairinEquation-m-b
	   Rewrite-First-Second-Pair-m-b
	   Apply-Def-Function-m-b               ;; replaces SimplifyThat-m-b
	   Simplify-Numerical-Expr-m-b
	   truei-m-b
	   ))
 (normalization-methods (AndE-M-F Expand-in-m-f Expand-not-in-m-f))
 (restriction-methods (truei-m-b
		       PullNeg-m-b AndI-M-B Or2Imp-m-b
		       Reflex-m-b NotReflex-m-b
		       PosNat-m-b RewritePowerOfOp-m-b
		       Reduce-Forall-To-Pairs-m-b
		       Reduce-Exists-To-Pairs-m-b
		       DecompElementOfCartProd-m-b
		       DecompNotElementOfCartProd-m-b
		       DecompPairinEquation-m-b
		       DecompPairinInequation-m-b
		       ;Expand-in-m-b
		       ;Expand-not-in-m-b
		       ;; Expand-Pair-Operation-m-b
		       ;; it was not possible to have Expand-Pair-Operation-m-b in the restrictions (i forgot the reason ...)
		       Rewrite-First-Second-Pair-m-b             ;; VERRY COMPLEX -> at the end of the list
		       Apply-Def-Function-m-b                    ;; ersetzt SimplifyThat-m-b ;; comlpex -> at the end of the list
		       Simplify-Numerical-Expr-m-b               ;; VERRY COMPLEX -> at the end of the list
		       ))
 (control-rules (zmz-learn-standard-select
		 zmz-learn-defnexp-select1
		 zmz-learn-defnexp-select2
		 zmz-prefer-metaors
		 zmz-interrupt-if-insttask
		 zmz-interrupt-for-other-strat
		 zmz-oril-select
		 zmz-orir-select
		 zmz-prefer-simple-parts
		 zmz-learn-reject-methods-in-wrong-order
		 ;zmz-reject-choose-meta
		 choose-first
		 learnt-tryanderror-first
		 )) 
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-further-goal-p)
 (selection waterfall)
 (print "Strategy-KS TryAndLearn: Offer to proof task ~A by trying each possibility"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CRULES OF TryAndLearn STRATEGY

(cri~def-control-rule zmz-learn-defnexp-select1  ;;select defnexp only for defs we have no learned pattern 
		      (kind methods)
		      (if (and (goal-matches ("goal" ("rel" "arg1" "arg2")))
			       (symbol-member "rel" (ORDER-OF-ELEMENT
						     homomorphism isomorphic injective surjective
						     unit inverse-exist divisors-exist
						     distributive))))
		      (then
		       (select ((DefnExp-m-b () ("rel"))))))

(cri~def-control-rule zmz-learn-defnexp-select2  ;;select defnexp only for defs we have no learned pattern 
		      (kind methods)
		      (if (and (goal-matches ("goal" (not ("rel" "arg1" "arg2"))))
			       (symbol-member "rel" (ORDER-OF-ELEMENT
						     homomorphism isomorphic injective surjective
						     unit
						     inverse-exist divisors-exist distributive
						     commutative associative closed-under))))
		      (then
		       (select ((DefnExp-m-b () ("rel"))))))

(cri~def-control-rule zmz-learn-standard-select
		      (kind methods)
		      (if (or-supports ("or-supps")))
		      (then
		       (select (ForallI-Sort-Resclass-Function-m-b
				ForallI-Sort-Resclass-m-b
				Convert-Resclass-To-Num-m-b
				(Or-E**-m-b () ("or-supps"))
				ExistsI-Sort-Resclass-Function-m-b
				ExistsI-Sort-Resclass-m-b
				OrIL-m-b
				OrIR-m-b
				Expand-Pair-Operation-m-b
				Expand-in-m-b
				Expand-not-in-m-b
				learnt-tryanderror-m-b
				choose-m-b
				))))



(cri~def-control-rule zmz-reject-choose-meta
		      (kind methods)
		      (if (goalcontainmetavars))
		      (then
		       (reject (choose-M-B))))

(cri~def-control-rule PREFER-BACKTRACK-CPOOL-FOR-TRYANDLEARN
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; Letzte Strategy gab Fehler 'no method applicable' fuer Task zurueck
				    (strategy-is TRYANDLEARN "strategy"))                  ;; Strategy war TRYANDERROR
			       (and (job-is BACKTRACK-CPOOL-STEP-AFTER-TASK "job1" "task")
				    ;; Es gibt Angebot von BACKTRACK-CPOOL-STEP-AFTER-TASK
				    (job-is BACKTRACK-STEP-TO-TASK "job2" "task")))) ;; Es gibt noch Angebot von BACKTRACK-STEP-TO-TASK
		      (then                                                          ;; -> 
		       (prefer ("job1"))))                                           ;; Ziehe BACKTRACK-CPOOL-STEP-AFTER-TASK vor


(defun less-learn-ordered-method (less-method selected-methods)
  (declare (edited  "04-JUN-2000")
	   (authors Ameier)
	   (input   "A string (which is a meta-variable for a method) and a list of methods.")
	   (effect  "None.")
	   (value   "Computes the set of less-methods to the selected methods list (wrt. the current pplanner strategy)."
		    "Then the input string is bound on each of these less methods and this lists of bindings is returned."))
  (let* ((current-strategy (roc~strategy-ks pplan*roc-state-description))
	 (less-methods (cond ((eq current-strategy (strat~find-strategy-ks 'TryAndlearn))
			      ;;(format t "~%~%~%~%~%~%~~%~%~%~% HERE")
			      (apply #'append (mapcar #'tryandlearn-less-methods-of-method selected-methods))
			      )
			     (t
			      nil))))
    (mapcar #'(lambda (less-meth)
		(list (cons less-method less-meth)))
	    less-methods)))

(cri~def-control-rule zmz-learn-reject-methods-in-wrong-order
		      (kind methods)
		      (if (and (already-applied-methods "applied-methods")
			       (less-learn-ordered-method "less-method" "applied-methods")))
		      (then
		       (reject ("less-method"))))

(defun tryandlearn-less-methods-of-method (method)
  
  ;; Methods which are in the ranking higher forbid to use the methods in the ranking below them
  
  (cond ((or (eq method (infer~find-method 'choose-m))
	     (eq method (meth~find-method 'choose-m-b)))
	 
	 ;; choose-m-b forbids:
	 
	 (list 'choose-m-b))

	((or (eq method (infer~find-method 'Or-E**-m))
	     (eq method (meth~find-method 'Or-E**-m-b)))
	 
	 ;; Or-E**-m-b forbids:
	 
	 (list 'ExistsI-Sort-Resclass-m-b
	       'OrIL-m-b
	       'OrIR-m-b
	       'Expand-Pair-Operation-m-b
	       ))
	
	((or (eq method (infer~find-method 'Convert-Resclass-To-Num-m))
	     (eq method (meth~find-method 'Convert-Resclass-To-Num-m-b)))
	 
	 ;; Convert-Resclass-To-Num forbids:
	 
	 (list 'Or-E**-m-b
	       'ExistsI-Sort-Resclass-m-b
	       'OrIL-m-b
	       'OrIR-m-b
	       'Expand-Pair-Operation-m-b
	       ))
	
	((or (eq method (infer~find-method 'ForallI-Sort-Resclass-m))
	     (eq method (meth~find-method 'ForallI-Sort-Resclass-m-b))
	     )
	 
	 ;; ForallI-Sort-Resclass-m-b forbids:
	 
	 (list  'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'Simplify-Numerical-Expr-m))
	     (eq method (meth~find-method 'Simplify-Numerical-Expr-m-b)))
	 
	 ;; Simplify-Numerical-Expr-m forbids:
	 
	 ;; apply-def-function-m forbids:
	 
	 (list  'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'apply-def-function-m))
	     (eq method (meth~find-method 'apply-def-function-m-b)))
	 
	 ;; apply-def-function-m forbids:
	 
	 (list  'Simplify-Numerical-Expr-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'Rewrite-First-Second-Pair-m))
	     (eq method (meth~find-method 'Rewrite-First-Second-Pair-m-b)))
	 
	 ;; Rewrite-First-Second-Pair-m forbids:
	 
	 (list  'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'DecompPairinInequation-m))
	     (eq method (meth~find-method 'DecompPairinInequation-m-b)))
	 
	 ;; DecompPairinInequation-m forbids:
	 
	 (list  'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'DecompPairinEquation-m))
	     (eq method (meth~find-method 'DecompPairinEquation-m-b)))
	 
	 ;; DecompPairinInequation-m forbids:
	 
	 (list  'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'DecompNotElementOfCartProd-m))
	     (eq method (meth~find-method 'DecompNotElementOfCartProd-m-b)))
	 
	 ;; DecompNotElementOfCartProd-m forbids:
	 
	 (list  'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'DecompElementOfCartProd-m))
	     (eq method (meth~find-method 'DecompElementOfCartProd-m-b)))
	 
	 ;; DecompElementOfCartProd-m-b forbids:
	 
	 (list  'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'Reduce-Exists-To-Pairs-m))
	     (eq method (meth~find-method 'Reduce-Exists-To-Pairs-m-b)))
	 
	 ;; Reduce-Exists-To-Pairs-m-b forbids:
	 
	 (list  'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	
	((or (eq method (infer~find-method 'Reduce-Forall-To-Pairs-m))
	     (eq method (meth~find-method 'Reduce-Forall-To-Pairs-m-b)))
	 
	 ;; Reduce-Forall-To-Pairs-m-b forbids:
	 
	 (list  'Reduce-Exists-To-Pairs-m-b
	        'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'Or2Imp-m))
	     (eq method (meth~find-method 'Or2Imp-m-b)))
	 
	 ;; Or2Imp-m-b forbids:
	 
	 (list  'Reduce-Forall-To-Pairs-m-b
	        'Reduce-Exists-To-Pairs-m-b
	        'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'AndI-M))
	     (eq method (meth~find-method 'AndI-M-B)))
	 
	 ;; AndI-M-B forbids:
	 
	 (list  'Or2Imp-m-b
	        'Reduce-Forall-To-Pairs-m-b
	        'Reduce-Exists-To-Pairs-m-b
	        'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	
	((or (eq method (infer~find-method 'PullNeg-m))
	     (eq method (meth~find-method 'PullNeg-m-b)))
	 
	 ;; PullNeg-m-b forbids:
	 
	 (list  'AndI-M-b
	        'Or2Imp-m-b
	        'Reduce-Forall-To-Pairs-m-b
	        'Reduce-Exists-To-Pairs-m-b
	        'DecompElementOfCartProd-m-b
	        'DecompNotElementOfCartProd-m-b
	        'DecompPairinEquation-m-b
	        'DecompPairinInequation-m-b
	        'Rewrite-First-Second-Pair-m-b
	        'Simplify-Numerical-Expr-m-b
	        'apply-def-function-m-b
	        'ForallI-Sort-Resclass-m-b
	        'Convert-Resclass-To-Num-m-b
	        'Or-E**-m-b
	        'ExistsI-Sort-Resclass-m-b
	        'OrIL-m-b
	        'OrIR-m-b
	        'Expand-Pair-Operation-m-b
		))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETTINGS                                                           ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf sod*current-strategies '(tryandlearn))

(setf sod*current-strategic-control-rules nil)

