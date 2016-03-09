
;;============================================================================
;; The control rules for planning limit-theorems.
;;
;; from 09|10|99
;;
;; NOTE: Look at the set-used-control-rules! entry at the end of this file!!!
;;============================================================================


(cri~def-control-rule Select-Limit-Methods
;;; This rule selects all methods relvant for proof planning limit-theorems:
		      (kind methods)
		      (if (always-true))
		      (then
		       (select (
				;;Initialize-CS-m-b
				PushNeg-m-f
				Elementary-m-b
				;pply-Lim-m-b
				TellCS-m-f
				TellCS-m-b
				AskCS-m-b
				Solve*<-m-b
				Solve*<-<=-m-b
				Solve*<=-<-m-b
				Solve*>-m-b
				Solve*>->=-m-b
				Solve*>=->=-m-b
				ComplexEstimate<-m-b
				ComplexEstimate<=-m-b
				ComplexEstimate>-m-b
				;MP ComplexEstimate>=-m-b
				
				NORMAL-S-B
				SimplifyInequality-m-b
				Simplify-m-b
				FactorialEstimate-m-b
				TrueI-m-b

				OrIL-m-b
				OrIR-m-b

				AlphaUnify-m-b
				Prove-CS-Answer-m-b
				
				ExistsE-m-a
				ForallE-meta-m-f
				SKOLEMIZE-S-B
				
				
				EnvI>-m-b
				
				UNWRAPHYP-S-F
				Ande-m-f

				EnvE<-m-f
				DOMAINCASESPLIT-S-B
				EnvI<-m-b
				indirect-m-b
				))))

;;********************************************************
;;   Initialization of the constraint solver             *
;;********************************************************
(cri~def-control-rule Choose-Initialize-CS
;;; After all quantifiers in the theorem have been eliminated by
;;; the Skolemize-m-b method, the constraint solver is initalized
;;; via Initialize-CS. This methods sends an 'init' message to the
;;; constraint solver CoSIE.
		      (kind methods)
		      (if (last-method SKOLEMIZE-S-B))
		      (then
		       (prefer (Initialize-CS-m-b))))


;;******************************************
;;   Inequality handling                   *
;;******************************************
;;; This following two control rules expresse the general approach to solve inequalities
;;; whose left hand side is the absolute value of an arbitrary term "lhs" 
;;; and whose right hand side is an arbitrary term "rhs":
;;; 1. First of all, the planner should try to apply the TellCS-m-b method,
;;;    which could probably add the inequality to the constraint store.
;;; 2. The method Solve*<-m-b (Solve*>-m-b) tries to unifiy "lhs" with a subformula "term" of
;;;    an assumption "ass" that contains as subformula that is 'similar' to
;;;    the "goal". Solve* decomposes the "goal" to more basic constraints.
;;; 3. ComplexEstimate is applicable, if the "lhs" can be rewritten as a 
;;;    linear combination of an assumption "term".
;;; 4. If none of the previous methods is applicable, the supermethod UNWRAPHYP
;;;    is used to unwrap a 'similar' subformula from a hypothesis if possible.
;;; 5. Last but not least, the inequality could be simplified by Simplify-m-b which
;;;    uses the MAPLE(tm) simplification algorithm.
(cri~def-control-rule Attack-Inequality
		      (kind methods)
		      (if (and (and (goal-matches ("goal" ("rel" "lhs" "rhs")))
				    (symbol-member "rel" (less greater leq geq)))
			       (most-similar-subterm-in-asss-to-goal ("goal"
								      "ass"
								      "pos"))))
		      (then
		       (prefer (
				TellCS-m-b
				AskCS-m-b
				(Solve*<-m-b () ("ass"))
				(Solve*<=-<-m-b () ("ass"))
				(Solve*<-<=-m-b () ("ass"))
				Solve*>-m-b
				(UNWRAPHYP-S-F () ("ass"))
				(ComplexEstimate<-m-b ("ass" "pos") ())
				(ComplexEstimate<=-m-b ("ass" "pos") ())
				(ComplexEstimate>-m-b ("ass" "pos") ())				
				;MP (ComplexEstimate>=-m-b ("ass" "pos") ())				
				SimplifyInequality-m-b
				Simplify-m-b
				(Simplify-m-f () ("ass"))
				))))



;;*********************************************************************
;;   Handling of constraint assumptions                               *
;;*********************************************************************
(cri~def-control-rule Choose-TellCS-F
;;; Assumptions that are legal constraints (wrt. the constraint language of
;;; the constraint solver CoSIE) have to be sent to the constraint solver.
;;; So, in the case of an inequality in an assumption node, the planner should
;;; try to apply the TellCS-m-f method.		      
		      (kind methods)
		      (if (or (assumption-matches ("ass" (in "var" "set")))
			      (and (or (assumption-matches ("ass" (not ("rel" "lhs" "rhs"))))
				       (assumption-matches ("ass" ("rel" "lhs" "rhs"))))
				   (symbol-member "rel" (less leq = geq greater))))
			  )
		      (then
		       (prefer ((TellCS-m-f () ("ass"))
				(SimplifyInequality-m-f () ("ass"))))))


;;*********************************************************************
;;   A local c-rule for the CASE-SPLIT supermethod 
;;*********************************************************************
(cri~def-control-rule Choose-DomainCaseSplit-I
		      (kind methods)
		      (if (and (assumption-matches ("ass" (not (= "lhs" "rhs"))))
			       (not-yet-applied (DomainCaseSplit-m-b)))
			  )
		      (then
		       (prefer (
				(DomainCaseSplit-m-b ("lhs" "rhs") ("ass")))
			       )))


(cri~def-control-rule Definition-Expansion
		      (kind methods)
		      (if (always-true))
		      (then (prefer (
				     (Defn-Exp-m-f ("lim") ())
					;(Defn-Exp-m-f (('fplus)) ())
				     (Defn-Exp-m-b ("lim") ())
					;(Defn-Exp-m-b (('fplus)) ())))))
				     ))))

;;*********************************************************************
;;   Handling of indirect proofs                                      *
;;*********************************************************************
;; We choose an indirect proof, when we want to infer a property of the
;; limit from a property of the function/sequence where the limit and
;; the function are connected by an 'infinite' relation.
;; So we look for a simple (in)eqality, where the limit L and another
;; constant are involved, for the relation R(F,L) and the property
;; P(F) of the function F. In the current domain, any relation 
;; where L and F are involved is an infinite one.

(cri~def-control-rule choose-indirect
                      (kind methods)
                      (if (and (not-yet-applied (indirect-m-b))
			       (infinite-relation "goal" "ass")))
		      (Then
		       (select
			(AskCS-m-b
			(indirect-m-b ("ass") ("goal" "ass"))))))
                               
	
         
    
	
;;****************************************************************
;;; We do not want to use all defined control rules while planning.
;;; To avoid using special control rules (e.g. the ones used in UNWRAPHYP),
;;; we explicitly name the control rules that should be used.
(cri~set-used-control-rules! '(
			       Select-Limit-Methods
			       Definition-Expansion
			       Attack-Inequality
			       Choose-TellCS-F
			       ;Choose-Initialize-CS
			       choose-indirect
			       ))




;;=============================================================================================
;;         old stuff
;;=============================================================================================

#|
(cri~def-control-rule Attack-Inequality>
		      (kind methods)
		      (if (and (goal-matches ("goal" (greater "lhs" "rhs")))
			       (most-similar-subterm-in-asss-to-goal ("goal"
								      "ass"
								      "pos"))))
		      (then
		       (prefer (
				TellCS-m-b
				AskCS-m-b
				Solve*>-m-b 
				(UNWRAPHYP-S-F () ("ass"))
				(ComplexEstimate>-m-b ("ass" "pos") ())
				Simplify-m-b))))


;;***********************************
(cri~def-control-rule Definition-Expansion
		      (kind methods)
		      (if (always-true))
		      (then (prefer (Defn-Exp-m-f ('lim) ())
				    (Defn-Exp-m-f ('fplus) ())
				    (Defn-Exp-m-b ('lim) ())
				    (Defn-Exp-m-b ('fplus) ()))))

(cri~def-control-rule Choose-DomainCaseSplit-II
		      (kind methods)
		      (if (and (assumption-matches ("ass" (or "disj1" "disj2")))
			       (and (sub-of-assumption ("ass" leq))
				    (not-yet-applied (CaseSplit-m-b))))
			  )
		      (then
		       (prefer (
				(CaseSplit-m-b () ("ass")))
			       )))
(cri~def-control-rule Choose-SimplifyInequality-F
;;; Assumptions that are legal constraints (wrt. the constraint language of
;;; the constraint solver CoSIE) have to be sent to the constraint solver.
;;; So, in the case of an inequality in an assumption node, the planner should
;;; try to apply the TellCS-m-f method.		      
		      (kind methods)
		      (if (and (assumption-matches ("ass" (not ("rel" "lhs" "rhs"))))
			       (symbol-member "rel" (less leq = geq greater))
			       )
			  )
		      (then
		       (prefer ((SimplifyInequality-m-f () ("ass"))))))



(cri~def-control-rule Test1
		      (kind methods)
		      (if (and (goal-matches ("goal" (in "elm" "set")))
			       (most-similar-subterm-in-asss-to-goal ("goal"
								      "ass"
								      "pos"))))
		      (then
		       (prefer (
				(UNWRAPHYP-S-F () ("ass"))
				))))




|#

;;;***********************************************************
;;; Old but not necessarily useless stuff.
;;;***********************************************************

#|
(cri~def-control-rule Choose-Notnote-m-f
		      (kind method)
		      (type normal)
		      (if (and (last-method NORMAL-S-B)
			       (last-assumption "ass"))
			  )
		      (then
		       (prefer ((NotnotE-m-f () ("ass"))))))


(cri~def-control-rule Attack-inequality-ii
		      (kind methods)
		      (if (and (goal-matches ("goal" (greater "lhs" "rhs")))
			       (most-similar-subterm-in-asss-to-goal ("goal"
								      "ass"
								      "pos"))))
		      (then
		       (prefer (SOLVE-S-B
				SOLVE*-S-B
				;Simplify-m-b
				;Fraction-m-b
				(UNWRAPHYP-S-F () ("ass"))))))

(cri~def-control-rule attack-unwrapped
		      (kind methods)
		      (if (and (last-method RemoveFocus-m-f)
			       (last-assumption "ass"))
			  )
		      (then
		       (select (;(CaseSplit-m-b () ("ass"))
				(Solve*<-m-b () ("ass"))
				(Solve*>-m-b () ("ass"))
				(ComplexEstimate<-m-b () ("ass"))
				(ComplexEstimate>-m-b () ("ass"))
				Solve-m-b
				Andi-m-b
				(Simplify-m-b () ("ass"))
				))))


(cri~def-control-rule attack-unwrapped
		      (kind methods)
		      (if (and (last-method RemoveFocus-m-f)
			       (last-assumption "ass"))
			  )
		      (then
		       (select ((CaseSplit-m-b () ("ass"))
				(SOLVE*-s-b () ("ass"))
				(ComplexEstimate<-m-b () ("ass"))
				(ComplexEstimate>-m-b () ("ass"))
				(Simplify-m-b () ("ass"))
				))))


;Martin's Hacks

(cri~def-control-rule Limit-reject-Methods
;;; This rule rejects all methods not relvant:
		      (kind methods)
		      (if (always-true))
		      (then
		       (reject (indirect-m-b))))
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following Control-Rules are used for Applying MULTI to solve
;; Limes Problems
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIE Selection CRULES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cri~def-control-rule PREFER-INDIRECT
		      (kind strategic)
		      (if (job-is IndirectProof "job2" "task"))
		      (then
		       (prefer ("job"))))

(cri~def-control-rule EX-ANA-INSERT
		      (kind strategic)
		      (if (source-plan-for-external-analogy-p "ExAnalogy-job"))
		      (then
		       (insert ("ExAnalogy-job"))))

(cri~def-control-rule EX-ANA-REJECT-IF-NOT-ROOT
		      (kind strategic)
		      (if (and (job-is ExternalAnalogy "job" "task")
			       (not-root-p "task")))
		      (then
		       (reject ("job"))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PPLANNER Control-Regeln                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ---------------------------------------------------> Fuer IndirectProof

(cri~def-control-rule MULTI-choose-indirect
                      (kind methods)
                      (if (and (not-yet-applied (indirect-m-b))
			       (infinite-relation "goal" "ass")))
		      (Then
		       (select
			(AskCS-m-b
			 (indirect-m-b ("ass") ("ass"))))))

(cri~def-control-rule interrupt-IndirectProof-after-indirect-step
		      (kind strategy-interruption)
		      (if (goal-results-from-indirect-p "goal"))
		      (then
		       (insert ((nil "goal")))))


;; ---------------------------------------------------> Fuer Normalize Goal

(cri~def-control-rule MYattack-non-atom-goal
		      (kind tasks)
		      (if (non-literal-goal "goal" "meth"))
		      (then
		       (select ("goal"))))

(cri~def-control-rule DEFNEXP-SELECT-GOAL
		      (kind methods)
		      (if (and (or (or (goal-matches ("goal" ("rel" "arg1" "arg2" "arg3")))
				       (goal-matches ("goal" (not ("rel" "arg1" "arg2" "arg3")))))
				   (or (goal-matches ("goal" ("rel" "arg1" "arg2")))
				       (goal-matches ("goal" (not ("rel" "arg1" "arg2"))))))
			       (symbol-member "rel" (lim limseq cont))))
		      (then
		       (select ((DefnExp-m-b () ("rel"))
				))))

(cri~def-control-rule DEFNEXP-SELECT-SUPPORT
		      (kind methods)
		      (if (and (or (or (assumption-matches ("ass" ("rel" "arg1" "arg2" "arg3")))
				       (assumption-matches ("ass" (not ("rel" "arg1" "arg2" "arg3")))))
				   (or (assumption-matches ("ass" ("rel" "arg1" "arg2")))
				       (assumption-matches ("ass" (not ("rel" "arg1" "arg2"))))))
			       (symbol-member "rel" (lim limseq cont))))
		      (then
		       (select ((DefnExp-m-f ("ass") ("rel"))
				))))

;; ------------------------------------------------------------------> UnwrapHyp

#| Version for ImpE-open-m-a, OrEL-open-m-a, and OrER-open-m-a with THM in parameters

  (cri~def-control-rule MYattack-focus
		      (kind tasks)
		      (if (and (focus-goal "goal" "meth" "ass")
			       (imp-ass "goal" "ass" "params"))) 
		      (then
		       (select (("goal" "meth" ("ass") "params"))))) |#

(cri~def-control-rule MYattack-focus
		      (kind tasks)
		      (if (focus-goal "goal" "meth" "ass"))
		      (then
		       (select (("goal" "meth" ("ass")))))) 

;; --------------------------------------------------------------> AttackInequality

;;(cri~def-control-rule MYSelect-DomainCaseSplit
;;		      (kind methods)
;;		      (if (and (assumption-matches ("ass" (not (= "lhs" "rhs"))))
;;			       (not-yet-applied (DomainCaseSplit-m-b))))
;;		      (then
;;		       (select ((DomainCaseSplit-m-b ("ass") ("lhs" "rhs"))))))

(cri~def-control-rule RejectNotCosieTasks
		      (kind tasks)
		      (if (not-cosie-task-p "task"))
		      (then
		       (reject ("task"))))

;;(cri~def-control-rule INTERRUPT-IF-FOCUS
;;		      (kind strategy-interruption)
;;		      (if (focus-p "goal"))
;;		      (then
;;		       (insert ((UnwrapHyp "goal" nil)))))

(cri~def-control-rule INTERRUPT-IF-FOCUS
		      (kind strategy-interruption)
		      (if (focus-p "goal" "ass" "pos"))
		      (then
		       (insert ((UnwrapHyp "goal" ("ass" "pos"))))))


(cri~def-control-rule INTERRUPT-IF-INST
		      (kind strategy-interruption)
		      (if (and (instantiated-in-CS-p "insttask")
			       (strategy-given-p DetFromCs)))
		      (then
		       (insert ((DetfromCS "insttask" nil)))))

(defun strategy-given-p (strat)
  (find strat strat*current-strategy-ks :test #'(lambda (it1 it2)
						  (keim~equal it1 (keim~name it2)))))

(cri~def-control-rule MYAttack-inequality<-non-standard-select
		      (kind methods)
		      (if (and (and (goal-matches ("goal" ("rel" "lhs" "rhs")))
				    (symbol-member "rel" (less greater leq geq)))
			       (MULTImost-similar-subterm-in-asss-to-goal ("goal" "ass" "pos"))))
		      ;;(or (or (not (term-is-not-absval-p "lhs"))
		      ;;	      (not (term-is-not-absval-p "rhs")))
		      ;;	  (not (assumption-matches ("assa" (less (absval "lhsa") "rhsa")))))))
		      (then
		       (select (Solve*<-m-b
				Solve*<-<=-m-b 
				Solve*<=-<-m-b 
				Solve*>-m-b
				Solve*>->=-m-b
				Solve*>=->=-m-b
				FactorialEstimate-m-b
				MComplexEstimate<-m-b
				MComplexEstimate<=-m-b
				MComplexEstimate>-m-b
				MComplexEstimate>=-m-b
				MComplexEstimate>>-m-b
				;;MSumEstimateI<-m-b
				;;MSumEstimateII<-m-b
				;;MSumEstimateI>-m-b
				;;MSumEstimateII>-m-b
				=subst*-m-b
				Simplify-m-b
				SimplifyInequality-m-b
				Simplify-m-f                 ;; OR LATER
				SimplifyInequality-m-f       ;; OR LATER
				(MSET-FOCUS-M-B ("ass") ("pos"))
				EnvI<-m-b
				EnvI>-m-b
				;;Simplify-m-f
				;;SimplifyInequality-m-f
				))))

;;(cri~def-control-rule MYSelect-EnvE<-m-f
;;		      (kind methods)
;;		      (if (and (and (and (goal-matches ("goal" ("rel" "lhs" "rhs")))
;;					 (symbol-member "rel" (less greater leq geq)))
;;				    (MULTImost-similar-subterm-in-asss-to-goal ("goal" "ass" "pos")))
;;			       (and (and (term-is-not-absval-p "lhs")
;;					 (term-is-not-absval-p "rhs"))
;;				    (assumption-matches ("assa" (less (absval "lhsa") "rhsa"))))))
;;		      (then
;;		       (select (Solve*<-m-b
;;				Solve*<-<=-m-b 
;;				Solve*<=-<-m-b 
;;				Solve*>-m-b
;;				Solve*>->=-m-b
;;				Solve*>=->=-m-b
;;				FactorialEstimate-m-b
;;				MComplexEstimate<-m-b
;;				MComplexEstimate<=-m-b
;;				MComplexEstimate>-m-b
;;				MComplexEstimate>>-m-b
;;				Simplify-m-b
;;				SimplifyInequality-m-b
;;				(MSET-FOCUS-M-B ("ass") ("pos"))
;;				(EnvE<-m-f ("assa") ())
;;				Simplify-m-f
;;				SimplifyInequality-m-f
;;				;;(EnvE<-m-f ("assa") ())
;;				))))
;;

(cri~def-control-rule MYSelect-EnvE<-m-f
		      (kind methods)
		      (if (and (and (goal-matches ("goal" ("rel" "lhs" "rhs")))
				    (symbol-member "rel" (less greater leq geq)))
			       (and (and (term-is-not-absval-p "lhs")
					 (term-is-not-absval-p "rhs"))
				    (assumption-matches ("assa" (less (absval "lhsa") "rhsa"))))))
		      (then
		       (insert-end (EnvE<-m-f))))

(cri~def-control-rule MYSelect-EnvE>-m-f
		      (kind methods)
		      (if (and (and (goal-matches ("goal" ("rel" "lhs" "rhs")))
				    (symbol-member "rel" (less greater leq geq)))
			       (and (and (term-is-not-absval-p "lhs")
					 (term-is-not-absval-p "rhs"))
				    (assumption-matches ("assa" (greater (absval "lhsa") "rhsa"))))))
		      (then
		       (insert-end (EnvE>-m-f))))

(cri~def-control-rule MYOrder-Env
		      (kind methods)
		      (if (and (and (goal-matches ("goal" ("rel" "lhs" "rhs")))
				    (symbol-member "rel" (less greater leq geq)))
			       (and (and (term-is-not-absval-p "lhs")
					 (term-is-not-absval-p "rhs"))
				    (assumption-matches ("assa" (less (absval "lhsa") "rhsa"))))))
		      (then
		       (order-before ((EnvE<-m-f Simplify-m-f)))))
;; Die letzte Kontroll-Regel ist etwas Triggy!
;; Im Prinzip ist sie nur reingekommen, da ich sonst LIM-DIV nicht hinbekommen haette ...
;; Eigentlich muss sie spaeter raus!
;; Dann muss das Backtracking fuer LIM-DIV bzw. die Limes Sachen an sich genau analysiert werden
;; und neu gemacht werden. 
;; AMEIER


(cri~def-control-rule MYreject-myset-focus-mmatchings-if-nothing-new
		      (kind mmatchings)
		      (if (or (mmatching-is-myset-focus-but-nothing-new-p "mmatching")
			      (or (mmatching-is-myset-focus-but-at-wrong-position-p "mmatching")
				  (mmatching-is-myset-focus-but-for-node-from-unwraphyp-p "mmatching"))))
		      (then
		       (reject ("mmatching"))))

(cri~def-control-rule MYprefer-non-unwrap-goals
		      (kind tasks)
		      (if (task-is-not-from-unwrap "task"))
		      (then
		       (prefer ("task"))))


(cri~def-control-rule reject-simplify-if-absval-minus-flip
		      (kind mmatchings)
		      (if (mmatching-is-simplify-with-absval-minus-flip "mmatching"))
		      (then
		       (reject ("mmatching"))))


(defun mmatching-is-simplify-with-absval-minus-flip (mmatching)
  ;; mmatchings stehen in cri*current-mmatchings drin ...
  (let* ((mmatchings-with-simplify-m-b (remove-if-not #'(lambda (mmatching)
							  (eq (pplan~matched-method mmatching)
							      (meth~find-method 'simplify-m-b)))
						      cri*current-mmatchings))
	 (mmatchings-with-simplify-m-f (remove-if-not #'(lambda (mmatching)
							  (eq (pplan~matched-method mmatching)
							      (meth~find-method 'simplify-m-f)))
						      cri*current-mmatchings))
	 (mmatchings-with-flip-simplify (remove-if-not #'crihelp=check-simplify-matching-for-absval-minus-flip
						       (append mmatchings-with-simplify-m-b mmatchings-with-simplify-m-f))))
    (mapcar #'(lambda (mm-simplify)
		(list (cons mmatching mm-simplify)))
	    mmatchings-with-flip-simplify)))
							   
	 
	 
(defun crihelp=check-simplify-matching-for-absval-minus-flip (mmatching)
  (let* ((mmapping (pplan~mmatch-mmapp mmatching))
	 (subst (meth~mapp-subst mmapping))
	 (mapp (meth~mapp-mapp mmapping))
	 (assoc-list (append (mapcar #'list (mapp~domain mapp) (mapp~codomain mapp))
			     (mapcar #'list (mapp~domain subst) (mapp~codomain subst))))
	 (avalue (second (assoc 'a assoc-list :test #'(lambda (name obj)
							(string-equal name (keim~name obj))))))
	 (bvalue (second (assoc 'b assoc-list :test #'(lambda (name obj)
							(string-equal name (keim~name obj))))))
	 (cvalue (second (assoc 'c assoc-list :test #'(lambda (name obj)
							(string-equal name (keim~name obj))))))
	 (dvalue (second (assoc 'd assoc-list :test #'(lambda (name obj)
							(string-equal name (keim~name obj))))))
	 )
    (if (or (crihelp=absval-minus-flip-p avalue cvalue)
	    (crihelp=absval-minus-flip-p bvalue dvalue))
	't
      nil)))

(defun crihelp=absval-minus-flip-p (term1 term2)
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (if (and (data~appl-p term1)
	     (keim~equal (data~appl-function term1) (env~lookup-object 'absval env))
	     (data~appl-p (first (data~appl-arguments term1)))
	     (keim~equal (data~appl-function (first (data~appl-arguments term1)))
			 (env~lookup-object 'minus env))
	     (data~appl-p term2)
	     (keim~equal (data~appl-function term2) (env~lookup-object 'absval env))
	     (data~appl-p (first (data~appl-arguments term2)))
	     (keim~equal (data~appl-function (first (data~appl-arguments term2)))
			 (env~lookup-object 'minus env)))
	;; both term1 and term2 are of the form |t-t'|
	(let* ((args1 (data~appl-arguments (first (data~appl-arguments term1))))
	       (args2 (data~appl-arguments (first (data~appl-arguments term2)))))
	  (if (and (keim~equal (first args1) (second args2))
		   (keim~equal (second args1) (first args2)))
	      't
	    nil))
      nil)))
	       
(cri~def-control-rule Apply=Subst*-m-b
		      (kind supports)
		      (if (and (method-is =subst*-m-b)
			       (repair-info-is-available-p "equations" "positions")))
		      (then
		       (select ((() ("equations" "positions"))
				))))

(defun method-is (method)
  (if (stringp method)
      ;; method is unbound
      (if cri*current-method 
	  (list (list (cons method cri*current-method)))
	nil)
    ;; method is bound
    (let* ((me (meth~find-method method)))
      (if (eq me cri*current-method)
	  (list (list (cons T T)))
	nil))))

(defun repair-info-is-available-p (equations positions)
  (let* ((open-node cri*current-task-node)
	 (control (pdsj~control (node~justification open-node)))
	 (broken-matchings (keim::pdsc~broken-matchings control))
	 (matchings-broken-because-of-alphaunify
	  (remove-if-not #'(lambda (broken-matching)
			     (let* ((cause-of-failure (second broken-matching))
				    (cond-function (first cause-of-failure))
				    (cond-args (second cause-of-failure))
				    (cond-mapping (third cause-of-failure)))
			       
			       (if (string-equal 'ALPHAUNIFY cond-function)
				   't
				 nil)))
			 broken-matchings))
	 (suitable-repair-tupels (apply #'append (mapcar #'(lambda (broken-matching)
							     (let* ((rest-tupels (strathelp=repair-unify-by-rests broken-matching)))
							       (if rest-tupels
								   (if (every #'(lambda (rest-tupel)
										  (let* ((node (pdsn~open-node-create (first rest-tupel) (pdsn~hyps open-node) (pds~new-node-name))))
										    (when (pdsn~schematic-p node)
										      (setf (pdsn~current-formula node)
											    (first rest-tupel)))
										    (cosie~test node)))
									      rest-tupels)
								       ;; every rest equation is accepted by cosie
								       (list rest-tupels)
								     nil)
								 nil)))
							 
							 matchings-broken-because-of-alphaunify))))

    (mapcar #'(lambda (repair-tupel)
		(list (list equations (mapcar #'first repair-tupel))
		      (list positions (mapcar #'second repair-tupel))))
	    suitable-repair-tupels)))

	    
;;  (if strathelp*store
;;      (list (list (list equations (mapcar #'first strathelp*store))
;;		  (list positions (mapcar #'second strathelp*store))))
;;    nil))


(defun strathelp=repair-unify-by-rests (broken-matching)
  (declare (edited  "07-SEP-1999")
	   (authors Ameier)
	   (input   "A method matching with a breakpoint entry and the method which was tried to apply.")
	   (effect  "None.")
	   (value   "A list of tupels ..."))
  
  (let* ((method (first broken-matching))
	 (cause-of-failure (second broken-matching))
	 (cond-function (first cause-of-failure))
	 (cond-args (second cause-of-failure))
	 (cond-mapping (third cause-of-failure))
	 (subst (meth~mapp-subst cond-mapping))
	 (mapping (meth~mapp-mapp cond-mapping))
	 (uvar1 (first cond-args))
	 (uvar2 (second cond-args))
	 (uterm1 (subst~apply subst uvar1))
	 (uterm2 (subst~apply subst uvar2)))

    (cond ((or (eq method (meth~find-method 'MYSolve*-leq-m-b))
	       (eq method (meth~find-method 'MYSolve*>-m-b))
	       (eq method (meth~find-method 'MYSolve*<=-<-m-b))
	       (eq method (meth~find-method 'MYSolve*<-<=-m-b))
	       (eq method (meth~find-method 'MYSolve*<-m-b))
	       (eq method (meth~find-method 'Solve*-leq-m-b))
	       (eq method (meth~find-method 'Solve*>-m-b))
	       (eq method (meth~find-method 'Solve*<=-<-m-b))
	       (eq method (meth~find-method 'Solve*<-<=-m-b))
	       (eq method (meth~find-method 'Solve*<-m-b)))
	   
	   ;; in all these methods (alphaunify aa a1 sigma) is called
	   ;; where aa always stems from the assumption (l1) and a1 from the conclusion (l3)
	   ;; both at the position (1) respectively
	   ;; -> choose conclusion (l3) and add position (1)
	   
	   (multiple-value-bind
	       (subst rest-tupels)
	       (strathelp=terms-unifiable-with-rests (list (list uterm2 uterm1)) ;; uterm2 stems from the conclusion!!!
						   (subst~create nil nil)
						   nil
						   (list (list (pos~empty) (pos~empty))) 
						   (pds~environment omega*current-proof-plan))
	     
	     (if subst
		 (mapcar #'(lambda (rest-tupel)
			     (list (term~appl-create (env~lookup-object '= (pds~environment omega*current-proof-plan))
						     (list (first rest-tupel)
							   (second rest-tupel)))
				   (pos~add-front 1 (third rest-tupel))))
			 rest-tupels)
	       nil)))
	  
	  
	  (t
	   ;; further repairs are not yet realized
	   
	   nil))))


(defun strathelp=terms-unifiable-with-rests (pair-list subst rest-tupels position-list env)
  (declare (edited  "05-NOV-2002")
	   (authors Ameier)
	   (input   "A list of pairs that have to be unified (initial a list that contains the pair of terms that should be unified),"
		    "A substitution (initially the empty substitution), a list of rest-tupels (initially nil), a list of positions"
		    "(initially two times the empty position), and an environment.")
	   (effect  "None.")
	   (value   "The list of rest-pairs that remain as rests of the unification of the two terms. Thereby not only the"
		    "the rest pairs are returned but rather rest-4-tupels consisting of the two rest terms and the two positions"
		    "of the terms in the initial terms respectively."))
  (let* ((choosen-pair (first pair-list))
	 (left-side (first choosen-pair))
	 (right-side (second choosen-pair)))

    (cond ((null choosen-pair)
	   ;; no further rest pairs -> done

	   (values subst
		   rest-tupels))
	  
	  ((and (term~constant-p left-side)
		(term~constant-p right-side)
		(data~equal left-side right-side))
	   ;; current rest pair consists of two constants which are equal
	   ;; -> remove rest pair and continue
	   
	   (strathelp=terms-unifiable-with-rests (rest pair-list)
					       subst
					       rest-tupels
					       (rest position-list)
					       env))
	  
;; SINCE WE WANT TO WORK WITH =SUBST ON THE INITIAL GOAL, WE DO NOT PERFOM EQUALITY SUBSTITUTIONS WITHIN THIS PROCEDURE
;;	  
;;	  ((and (term~variable-p left-side)
;;		(null (find left-side (term~free-variables right-side))))
;;
;;	   ;; variable on the left side and no occur clash with the right side -> new subtsition
;;	   ;; update also the rest-tupels!
;;	   
;;	   (let* ((new-subst (subst~create (list left-side) (list right-side)))
;;		  (new-rest-tupels (mapcar #'(lambda (repair-4tupel)
;;						 (cons (subst~apply new-subst (first repair-4tupel))
;;						       (cons (subst~apply new-subst (second repair-4tupel))
;;							     (rest (rest repair-4tupel)))))
;;					     rest-tupels)))
;;
;;	     (strathelp=terms-unifiable-with-rests (mapcar #'(lambda (pair)
;;							       (list (subst~apply new-subst (first pair))
;;								     (subst~apply new-subst (second pair))))
;;							   (rest pair-list))
;;						   (subst~compose-substitution new-subst subst)
;;						   new-rest-tupels
;;						   (rest position-list)
;;						   env)))
;;	  ((and (term~variable-p right-side)
;;		(null (find right-side (term~free-variables left-side))))
;;
;;	   ;; same as previous case with variable on the right
;;	   
;;	   (let* ((new-subst (subst~create (list right-side) (list left-side)))
;;		  (new-rest-tupels (mapcar #'(lambda (repair-4tupel)
;;						 (cons (subst~apply new-subst (first repair-4tupel))
;;						       (cons (subst~apply new-subst (second repair-4tupel))
;;							     (rest (rest repair-4tupel)))))
;;					     rest-tupels)))
;;
;;	     (strathelp=terms-unifiable-with-rests (mapcar #'(lambda (pair)
;;							     (list (subst~apply new-subst (first pair))
;;								   (subst~apply new-subst (second pair))))
;;							 (rest pair-list))
;;						 (subst~compose-substitution new-subst subst)
;;						 new-rest-tupels
;;						 (rest position-list)
;;						 env)))
;;	  
	  ((and (data~appl-p left-side)
		(data~appl-p right-side)
		(data~equal (data~appl-function left-side)
			    (data~appl-function right-side)))

	   ;; left hand side and right hand side are both applications with the same function
	   ;; -> decompose application and continue
	   
	   (let* ((head-positions (first position-list))
		  (left-pos (first head-positions))
		  (right-pos (second head-positions))
		  (new-left-positions (do* ((rest-args (data~appl-arguments left-side) (rest rest-args))
					    (i 1 (+ i 1))
					    (new-poses nil))
					  ((null rest-args)
					   new-poses)
					(setq new-poses (append new-poses (list (pos~add-end i left-pos))))))
		  (new-right-positions (do* ((rest-args (data~appl-arguments right-side) (rest rest-args))
					     (i 1 (+ i 1))
					     (new-poses nil))
					   ((null rest-args)
					    new-poses)
					 (setq new-poses (append new-poses (list (pos~add-end i right-pos))))))
		  (new-pos-pairs (mapcar #'(lambda (pos1 pos2)
					     (list pos1 pos2))
					 new-left-positions
					 new-right-positions)))
	     
	     (strathelp=terms-unifiable-with-rests (append (mapcar #'(lambda (arg1 arg2)
								     (list arg1 arg2))
								 (data~appl-arguments left-side)
								 (data~appl-arguments right-side))
							 (rest pair-list))
						 subst
						 rest-tupels
						 (append new-pos-pairs (rest position-list))
						 env)))
	  

	  (t

	   ;; if none of the previous cases works we mark the remaining pairs of formulas as rest pairs and return them in case:
	   ;; a) they are no abstractions
	   ;; b) there are both of type num
	   ;; otherwise -> nil

	   (let* ((num (env~lookup-object 'num (pds~environment omega*current-proof-plan))))
	     
	     (cond ((or (null (eq (term~type left-side) num))
			(null (eq (term~type right-side) num)))
		    (values nil
			    nil))
		   ((or (data~abstr-p left-side)
			(data~abstr-p right-side))
		    (values nil
			    nil))
		   (t
		    (strathelp=terms-unifiable-with-rests (rest pair-list)
							subst
							(cons (list left-side
								    right-side
								    (first (first position-list))
								    (second (first position-list)))
							      rest-tupels)
							(rest position-list)
							env))))))))













    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PREFER-CASESPLIT-ON-UNWRAPHYP-PREMISE






(cri~def-control-rule PREFER-CASESPLIT-ON-UNWRAPHYP-PREMISE
		      (kind tasks)
		      (if (and (and (and (ROC-OF-LAST-TERMINATED-STRATEGY "roc")
					 (ROC-IS "roc" "task1" backtrack-unwraphy-step))
				    ;; last terminated strategy was backtrack-unwraphy-step
				    (and (TASK-WAS-PREMISE-FROM-UNWRAPHYP-P "task1" "roc")
					 ;; the task on which we performed backtracking was a premise from unwraphyp
					 (TASK-WAS-FOCUSSED-P "task2" "roc")))
			       (and (TASK-FORMULA "task1" "formula")
				    (FORMULA-NOT-IN-HYPS "formula" "task2"))))
		      (then
		       (select (("task2" CaseSplit-m-b () ("formula"))))))

(defun FORMULA-NOT-IN-HYPS (formula task)
  (if (or (stringp formula)
	  (stringp task))
      ;; formula or task unbound
      nil
    ;; formula and task bound
    (let* ((hyps (pdsn~hyps (agenda~task-node task)))
	   (hyps-equal-formula (remove-if-not #'(lambda (hyp)
						  (or (keim~equal formula (node~formula hyp))
						      (keim~equal (term~appl-create (env~lookup-object 'not (th~env 'base))
										    (list formula))
								  (node~formula hyp))))
					      hyps)))
      (if hyps-equal-formula
	  nil
	(list (list (cons T T)))))))

(defun TASK-WAS-FOCUSSED-P (task roc)
  (if (stringp roc)
      ;; roc not bound
      nil
    ;; the roc is supposed to be a backtrack-unwraphy-step roc
    (if (null (roc~backtrack-state-description-p roc))
	(progn
	  (omega~warn "Wrong roc in TASK-WAS-PREMISE-FROM-UNWRAPHYP function")
	  nil)
      (let* ((deleted-steps (roc~backtrack-removed-steps roc))
	     (first-del (first deleted-steps)))
	;; the first deleted step is supposed to be a MSET-FOCUS-M step
	(if (null (and first-del
		       (eq (just~method (pdsc~an-just first-del)) (infer~find-method 'MSET-FOCUS-M))))
	    (progn
	      (omega~warn "Wrong first deleted step in TASK-WAS-PREMISE-FROM-UNWRAPHYP function")
	      nil)
	  (let* ((node (pdsc~an-node first-del))
		 (all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
		 (task-with-node (first (remove-if-not #'(lambda (task)
							   (and (agenda~goal-or-goal-schema-task-p task)
								(eq (agenda~task-node task) node)))
						       all-tasks))))
	    (if task-with-node
		(if (stringp task)
		    ;; task is unbound
		    (list (list (cons task task-with-node)))
		  (if (eq task task-with-node)
		      (list (list (cons T T)))
		    nil))
	      nil)))))))

(defun TASK-WAS-PREMISE-FROM-UNWRAPHYP-P (task roc)
  (if (or (stringp task)
	  (stringp roc))
      ;; the task or the roc are unbound
      nil
    ;; the roc is supposed to be a backtrack-unwraphy-step roc
    (if (null (roc~backtrack-state-description-p roc))
	(progn
	  (omega~warn "Wrong roc in TASK-WAS-PREMISE-FROM-UNWRAPHYP function")
	  nil)
      (let* ((node (agenda~task-node task))
	     (deleted-steps (roc~backtrack-removed-steps roc)))

	(setq crihelp*store-global-deleted-steps deleted-steps)

	(if (crihelp=step-with-node-in-premises-and-impe-p node deleted-steps)
	    ;; node is directly premise of impe-open-m
	    (list (list (cons T T)))
	  (let* ((andi-step (crihelp=step-with-node-in-premises-and-andi-p node deleted-steps)))
	    (if andi-step
		;; node is premise of andi step
		(let* ((andi-node (pdsc~an-node andi-step)))
		  (if (crihelp=step-with-node-in-premises-and-impe-p andi-node deleted-steps)
		      ;; andi-node is premise of impe-open-m
		      (list (list (cons T T)))
		    nil))
	      nil)))))))

(defun crihelp=step-with-node-in-premises-and-andi-p (node steps)
  (first (remove-if-not #'(lambda (step)
			    (if (scon~strategy-step-p step)
				nil
			      (let* ((just (pdsc~an-just step)))
				(if (and (find node (just~premises just))
					 (eq (just~method just) (infer~find-method 'andi-m)))
				    't
				  nil))))
			(crihelp=flatten steps))))


(defun crihelp=step-with-node-in-premises-and-impe-p (node steps)
  (first (remove-if-not #'(lambda (step)
			    (if (scon~strategy-step-p step)
				nil
			      (let* ((just (pdsc~an-just step)))
				(if (and (eq node (first (just~premises just)))
					 (eq (just~method just) (infer~find-method 'ImpE-Open-m)))
				    't
				  nil))))
			(crihelp=flatten steps))))

(defun crihelp=flatten (steps)	
  (apply #'append (mapcar #'(lambda (step)
			      (if (listp step)
				  step
				(list step)))
			  steps)))



(defun TASK-FORMULA (task formula)
  (if (stringp task)
      ;; task is unbound
      nil
    (let* ((task-node (agenda~task-node task))
	   (task-formula (if (pdsn~schematic-p task-node)
			     (pdsn~current-formula task-node)
			   (node~formula task-node))))
      (if (stringp formula)
	  ;; formula is unbound
	  (list (list (cons formula task-formula)))
	(if (keim~equal formula task-formula)
	    (list (list (cons T T)))
	  nil)))))

(defun ROC-OF-LAST-TERMINATED-STRATEGY (roc)
  (let* ((last-terminating-step (crihelp=last-terminating-strategy-step)))
    
    (if (null last-terminating-step)
	;; there is currently no terminating step
	nil
      (let* ((just (scon~strategy-step-just last-terminating-step))
	     (params (pdsj~parameters just))
	     (last-roc (first params)))
	(if (stringp roc)
	    ;; roc is unbound
	    (list (list (cons roc last-roc)))
	  nil)))))

(defun ROC-IS (roc task strategy)
  (if (stringp roc)
      ;; roc is unbound
      nil
    (let* ((start-task (roc~start-task roc))
           (strategy-ks (roc~strategy-ks roc)))
      (cond ((and (stringp strategy)
		  (stringp task))
	     ;; both strategy and task are unbound
	     (list (list (cons strategy strategy-ks)
			 (cons task start-task))))
	    ((and (stringp strategy)
		  (null (stringp task)))
	     ;; strategy is unbound but task is bound
	     (if (eq task start-task)
		 (list (list (cons strategy strategy-ks)))
	       nil))
	    ((and (null (stringp strategy))
		  (stringp task))
	     ;; strategy is bound but task is unbound
	     (if (keim~equal strategy (keim~name strategy-ks))
		 (list (list (cons task start-task)))
	       nil))
	    (t
	     ;; both are bound
	     (if (and (eq task start-task)
		      (keim~equal strategy (keim~name strategy-ks)))
		 (list (list (cons T T)))
	       nil))))))


(defun crihelp=last-terminating-strategy-step ()
  (let* ((current-last-step (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard)))
    (if (null current-last-step)
	;; there is no current last-step
	nil
      (do* ((current-step current-last-step))
	  ((or (null current-step)
	       (crihelp=terminating-step-p current-step))
	   current-step)
	(setf current-step (scon~strategy-step-predecessor current-step))))))
	
(defun crihelp=terminating-step-p (strategy-step)
  (let* ((just (scon~strategy-step-just strategy-step))
	 (method (just~method just)))
    (if (eq method (infer~find-method 'END-STRATEGY-KS-APPLICATION))
	't
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Control Rules for application of =substb and =substf

(cri~def-control-rule =subst-on-goal
		      (kind tasks)
		      (if (and (ordered-equality-support-for-goal "task" "supp" "replaced-term" "replace-term")
			       (positions-of-replaced-term-in-goal "task" "replaced-term" "pos"))) 
			       
		      (then
		       (select (("task" =subst-m-b ("supp") ("pos"))))))

(defun ordered-equality-support-for-goal (task support replaced-term replace-term)
  (if (or (null (stringp task))
	  (null (stringp support))
	  (null (stringp replaced-term))
	  (null (stringp replace-term)))
      ;; something already bound -> not implemented yet
      nil
    ;;(progn
    ;;  (format t "~%CURRENT-TASKS: ~A" cri*current-tasks)
    ;;  (format t "~%ORIG-TASKS: ~A" cri*current-original-alternative-list)
    (let* ((current-tasks cri*current-tasks)
	   (tasks-equation-supports-pairs
	    (apply #'append (mapcar #'(lambda (task)
				       (if (null (agenda~goal-or-goal-schema-task-p task))
					   nil
					 (let* ((supports (pds~node-supports (agenda~task-node task)))
						(equation-supports (remove-if-not #'(lambda (supp)
										      (let* ((formula (node~formula supp)))
											(and (data~appl-p formula)
											     (keim~equal (data~appl-function formula)
													 (data~schema-range (env~lookup-object '= (th~env 'base)))))))
										  supports)))
					   (if equation-supports
					       (list (list task equation-supports))
					     nil))))
				   current-tasks))))
      (apply #'append
	     (mapcar #'(lambda (task-equations-pair)
			 (let* ((curr-task (first task-equations-pair))
				(equations (second task-equations-pair))
				(ordered-triples (apply #'append (mapcar #'(lambda (eq-supp)
									     (let* ((formula (node~formula eq-supp)))
									       (multiple-value-bind
										   (success replaced-side replace-side)
										   (crihelp=ordered-equation-p formula)
										 (if success
										     (list (list eq-supp replaced-side replace-side))
										   nil))))
									 equations))))
			   (if ordered-triples
			       (mapcar #'(lambda (eq-triple)
					   (list (cons task curr-task)
						 (cons support (first eq-triple))
						 (cons replaced-term (second eq-triple))
						 (cons replace-term (third eq-triple))))
				       ordered-triples)
			     nil)))
		     tasks-equation-supports-pairs)))))

(defun crihelp=ordered-equation-p (equation)
  (let* ((args (data~appl-arguments equation))
	 (arg1 (first args))
	 (arg2 (second args)))
    (cond ((and (or (data~appl-p arg1)
		    (data~schema-p arg1))
		(or (term~constant-p arg2)
		    (term~variable-p arg2)))
	   ;; arg1 is a complex term whereas arg2 is a constant or a variable
	   (if (data~substruct-positions arg2 arg1)
	       ;; arg2 is contained in arg1
	       ;; -> replace occurences of the complex term by occurences of the simple term
	       (values t arg1 arg2)
	     ;; arg2 is not containes in arg1
	     ;; -> replace occurences of the simple term by occurences of the complex term
	     ;;    -> may some simplifications are possible 
	     (values t arg2 arg1)))
	  ((and (or (data~appl-p arg2)
		    (data~schema-p arg2))
		(or (term~constant-p arg1)
		    (term~variable-p arg1)))
	   ;; arg2 is a complex term whereas arg1 is a constant or a variable
	   (if (data~substruct-positions arg1 arg2)
	       ;; arg1 is contained in arg2
	       ;; -> replace occurences of the complex term by occurences of the simple term
	       (values t arg2 arg1)
	     ;; arg1 is not containes in arg2
	     ;; -> replace occurences of the simple term by occurences of the complex term
	     ;;    -> may some simplifications are possible 
	     (values t arg1 arg2)))
	  ((and (or (data~appl-p arg2)
		    (data~schema-p arg2))
		(or (data~appl-p arg1)
		    (data~schema-p arg1)))
	   ;; both sides are complex -> nothing 
	   (values nil nil nil))
	  (t
	   ;; both sides are simple!
	   (cond ((and (term~number-p arg1)
		       (null (term~number-p arg2)))
		  ;; arg1 is number, arg2 not
		  ;; replace arg2 by number (may some terms can be simplified)
		  (values (t arg2 arg1)))
		 ((and (term~number-p arg2)
		       (null (term~number-p arg1)))
		  ;; arg2 is number, arg1 not 
		  ;; replace arg1 by number (may some terms can be simplified)
		  (values (t arg1 arg2)))
		 ((and (meta~p arg1)
		       (null (meta~p arg2)))
		  ;; arg1 is meta-variable, arg2 not
		  ;; replace arg2 by meta-variable (may some stuff can be passed to COSIE then)
		  (values t arg2 arg1))
		 ((and (meta~p arg2)
		       (null (meta~p arg1)))
		  ;; arg2 is meta-variable, arg1 not
		  ;; replace arg1 by meta-variable (may some stuff can be passed to COSIE then)
		  (values t arg1 arg2))
		 (t
		  ;; otherwise replace left hand side by right hand side
		  (values t arg1 arg2)))))))


(defun positions-of-replaced-term-in-goal (task term pos)
  (if (or (stringp term)
	  (stringp task))
      ;; term or task is unbund -> not implemented yet!
      nil
    (let* ((formula (node~formula (agenda~task-node task)))
	   (positions (data~substruct-positions term formula)))
      (if (stringp pos)
	  ;; position unbound
	  (mapcar #'(lambda (pos-in-term)
		      (list (cons pos pos-in-term)))
		  positions)
	(if (find pos positions :test #'keim~equal)
	    (list (list (cons T T)))
	  nil)))))
      



