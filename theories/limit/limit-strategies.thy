;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategien zu Analogy    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; External Analogy

(defun ExSourcePlan-p (task)
  (if ana*source-plan
      't
    nil))

(strat~define-strategy-ks
 (name ExternalAnalogy)
 (refinement-algorithm Analogy)
 (condition ExSourcePlan-p)
 (parameter-types (ndline))
 (print "Strategy-KS ExternalAnalogy: Offer to close goal ~A by external analogy"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategien zu InstMeta!  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DetFromCS


(defun det-by-cs-p (task)
  (if (agenda~inst-task-p task)
      (let* ((meta-var (agenda~inst-task-meta-var task))
	     (detvars (cosie~getdetvars))
	     (mv-pair (find meta-var detvars :test #'(lambda (mvar pair)
						       (keim~equal mvar (first pair))))))
	(if mv-pair
	    't
	  nil))
    nil))

(defun compute-from-answer-constraint (meta-var)
  (let* ((detvars (cosie~getdetvars))
	 (mv-pair (find meta-var detvars :test #'(lambda (mvar pair)
						   (keim~equal mvar (first pair))))))

    (if mv-pair
	(cdr mv-pair)
      (omega~error "~%Something wrong in function compute-from-answer-constraint."))))

(strat~define-strategy-ks
 (name DetFromCs)
 (refinement-algorithm InstMeta)
 (condition det-by-CS-p)
 (compute-instantiation-function compute-from-answer-constraint)
 (print "Strategy-KS DetFromCs: Offer to instantiate meta-variable ~A from answer constraint"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SolveFromCS

(defvar strathelp*cosie-solution-store nil)
;; a global variable to store a pair consisting of the current omega*current-proof-plan and the computed solution of the
;; constraint store by CoSie
;; If this information is stored, we have to call CoSie only once!

(defun strathelp=cosie-search ()
  (if (eq (first strathelp*cosie-solution-store) omega*current-proof-plan)
      ;; we searched already and stored the result
      (second strathelp*cosie-solution-store)
    (let* ((subst (cosie~call 'solve)))
      (setf strathelp*cosie-solution-store (list omega*current-proof-plan subst))
      subst)))

(defun no-open-goals-and-solvefromcs-p (task)
  (if (agenda~inst-task-p task)
      (let* ((all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	     (goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p all-tasks)))
	(unless goal-tasks
	  (let* ((subst (strathelp=cosie-search))
		 (meta-var (agenda~inst-task-meta-var task)))
	    (when (find meta-var (subst~domain subst))
	      't))))
    nil))

(defun compute-solvefromcs (meta-var)
  (let* ((subst (strathelp=cosie-search)))
    
    (if (null (find meta-var (subst~domain subst)))
	(omega~error "~%Something wrong in function compute-solvefromcs.")
      (subst~apply subst meta-var))))

(strat~define-strategy-ks
 (name SolveFromCS)
 (refinement-algorithm InstMeta)
 (condition no-open-goals-and-solvefromcs-p)
 (compute-instantiation-function compute-solvefromcs)
 (print "Strategy-KS SolveFromCS: Offer to instantiate meta-variable ~A by search in constraint store"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategien zu PPlanner!  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------> IndirectProof

(defun no-more-goals-p ()
  ;; Agenda steht in pds auf sod*solution-blackboard
  ;; Current ROC steht in pplan*roc-state-description

  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (all-tasks (agenda~all-tasks agenda))
	 (tasks-with-roc (remove-if-not #'(lambda (task)
					    (if (and (agenda~goal-or-goal-schema-task-p task)
						     (find pplan*roc-state-description (agenda~task-rocs task)))
						't
					      nil))
					all-tasks)))
    (if tasks-with-roc
	nil
      't)))

(defun indirect-seems-suitable-p (task)
  ;; currently: indirect seems suitable when there is an infinite relation between limits
  ;; see limit-metaprds.thy -> infinite-relation
  (when (agenda~goal-or-goal-schema-task-p task)
    (let* ((numtype (env~lookup-object 'num (pds~environment omega*current-proof-plan)))
	   (goal (agenda~task-node task))
	   (supports (pds~node-supports goal))
	   (limit-term (node~formula goal))
	   (limit (mapcan #'(lambda (term)
			      (when (and (data~equal (term~type term) numtype)
					 (term~constant-p term)
					 (not (term~number-p term)))
				(list term))) (data~all-substructs (node~formula goal)))))
      (when (and limit
		 (not (term~variables limit-term)))
	;;ass-check
	(let* ((relation (post~read-object '(lam (indf (num num))        ;;maybe add variations 
						 (lam (indx num)
						      (lam (indl num)
							   (lam (inde num) 
								(less (absval (minus (indf indx) indl)) inde)))))
					   (pds~environment omega*current-proof-plan) :existing-term))
	       (limterm (third (data~abstr-n-domain relation)))
	       (functerm (first (data~abstr-n-domain relation)))
	       (relation (data~abstr-n-range relation))
	       (asslist (some #'(lambda (line)            ;;choose every assump that contain the function                  
				  (let ((form (node~formula line)))             
				    (when (not (term~free-variables form))
				      (let ((subst (some #'(lambda (subterm) (term~alpha-match relation subterm))
							 (data~all-substructs form))))
					(when (and subst
						   (some #'(lambda (lim)
							     (data~substruct-positions
							      (subst~apply subst limterm)
							      lim))
							 limit))
					  (let ((fun (subst~apply subst functerm)))
					    (mapcan #'(lambda (support)
							(when (data~substruct-positions fun (node~formula support))
							  (list support)))
						    supports)))))))
			      supports)))
	  (when asslist
	    't))))))
	
(strat~define-strategy-ks
 (name IndirectProof)
 (refinement-algorithm PPlanner)
 (condition indirect-seems-suitable-p)
 (methods (indirect-m-b AskCS-m-b))
 (normalization-methods nil)
 (restriction-methods nil)
 (control-rules (MULTI-choose-indirect
		 interrupt-IndirectProof-after-indirect-step
		 ))
 (loop-detection nil)
 (termination-check no-more-goals-p)
 (selection waterfall)
 (print "Strategy-KS IndirectProof: Offer to prove ~A by an indirect proof"))

;; ------------------------------------------------------------> NORMALIZE-GOAL

(defun no-non-literal-or-defined-concepts-open-goals-p ()
  ;; Agenda steht in pds auf sod*solution-blackboard
  ;; Current ROC steht in pplan*roc-state-description

  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (all-tasks (agenda~all-tasks agenda))
	 (goal-tasks-with-roc (remove-if-not #'(lambda (task)
						 (and (agenda~goal-or-goal-schema-task-p task)
						      (find pplan*roc-state-description (agenda~task-rocs task))))
					     all-tasks))
	 (tasks-with-roc-and-with-goals-with-non-literal-formulas-or-defined-concepts
	  (remove-if-not #'(lambda (task)
			     (let* ((node (agenda~task-node task))
				    (formula (node~formula node)))
			       (if (strathelp=formula-is-not-literal-or-defined-concept-p formula)
				   't
				 nil)))
			 goal-tasks-with-roc)))
    
    (if tasks-with-roc-and-with-goals-with-non-literal-formulas-or-defined-concepts
	nil
      (if goal-tasks-with-roc
	  't
	't))))

(defun non-literal-goal-or-defined-concept-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task))
	     (formula (node~formula node)))
	(if (strathelp=formula-is-not-literal-or-defined-concept-p formula)
	    't
	  nil))
    nil))

(defun strathelp=formula-is-not-literal-or-defined-concept-p (formula)
  (let* ((literal-p (or (logic~atom-p formula)
			(and (logic~negation-p formula)
			     (logic~atom-p (first (data~appl-arguments formula))))))
	 (defined-concept-p (or (and (logic~atom-p formula)
				     (data~appl-p formula)
				     (strathelp=interesting-concept-for-normalize-goal-p (data~appl-function formula)))
				(and (logic~negation-p formula)
				     (logic~atom-p (first (data~appl-arguments formula)))
				     (data~appl-p (first (data~appl-arguments formula)))
				     (strathelp=interesting-concept-for-normalize-goal-p
				      (data~appl-function (first (data~appl-arguments formula))))))))
	(if (or (null literal-p)
		defined-concept-p)
	    't
	  nil)))

(defun strathelp=interesting-concept-for-normalize-goal-p (term)
  ;; in this file we write all defined concepts in whose normalization = expansion we are interested
  ;; in the normalize goal strategy
  
  ;;(or (keim~equal term (env~lookup-object 'lim (pds~environment omega*current-proof-plan)))
   ;;   (keim~equal term (env~lookup-object 'limseq (pds~environment omega*current-proof-plan)))))
  nil)

(strat~define-strategy-ks
 (name NormalizeGoal)
 (refinement-algorithm PPlanner)
 (condition non-literal-goal-or-defined-concept-p)
 (methods (ANDI-M-B IMPI-M-B FORALLI-M-B EXISTSI-META-M-B EquivI-m-b OrIRHYP-m-b OrILHYP-m-b pullneg-m-b));; DefnExp-m-b DefnExp-m-f))
 (normalization-methods nil)
 (restriction-methods nil)
 (control-rules (;;MYATTACK-NON-ATOM-GOAL
		 ;;DEFNEXP-SELECT-SUPPORT
		 ;;DEFNEXP-SELECT-GOAL
		 MYATTACK-NON-ATOM-GOAL
		 ))
 (loop-detection nil)
 (termination-check no-non-literal-or-defined-concepts-open-goals-p)
 (selection waterfall)
 (remark ("The goal<BR><LISP>(verbalize-start-task state-des)</LISP><BR>is normalized to
<BLOCKQUOTE><LISP>(verbalize-outlines state-des)</LISP></BLOCKQUOTE>"
	  "")
	 ("This goal is normalized to
<LISP>(verbalize-number-of-outlines state-des)</LISP> subgoals. These are solved the following way:
<LISP>(increase-depth)</LISP><BLOCKQUOTE><LISP>(verbalize-text-outlines-subproofs state-des)</LISP></BLOCKQUOTE><LISP>(decrease-depth)</LISP>"
	  "This goal is normalized to
<LISP>(verbalize-number-of-outlines state-des)</LISP> subgoals. These are solved the following way:
<LISP>(increase-depth)</LISP><BLOCKQUOTE><LISP>(verbalize-cons-outlines-subproofs state-des)</LISP></BLOCKQUOTE><LISP>(decrease-depth)</LISP>"))
 (print "Strategy-KS NormalizeGoal: Offer to decompose the task ~A"))

;; --------------------------------------------------------> UnwrapHyp

;; (defun demand-for-unwraphyp-p (task)
;;   (if (agenda~goal-or-goal-schema-task-p task)
;;       (let* ((demands-store (black~get-blackboard-object-content 'demands sod*control-blackboard))
;; 	     (demands-for-unwraphyp-and-task (store~elements-with-test demands-store
;; 								       :test #'(lambda (demand)
;; 										 (if (and (demand~strategy-task-demand-p demand)
;; 											  (eq (demand~strategy-task-demand-strategy-ks demand)
;; 											      (strat~find-strategy-ks 'unwraphyp))
;; 											  (eq (demand~strategy-task-demand-task demand)
;; 											      task))
;; 										     't
;; 										   nil)))))
;; 	(if demands-for-unwraphyp-and-task
;; 	    't
;; 	  nil))
;;     nil))

(defun complex-support-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((supports-formula (mapcar #'node~formula (pds~node-supports (agenda~task-node task))))
	     (complex-formulas (remove-if #'(lambda (formula)
					      (or (logic~atom-p formula)
						  (and (logic~negation-p formula)
						       (logic~atom-p (first (data~appl-arguments formula))))))
					  supports-formula)))
	(if complex-formulas
	    ;; the task has supports with complex formula
	    't
	  nil))
    nil))

;; (defun no-focus-p ()
;;   (let* ((focus (black~get-blackboard-object-content 'focus sod*solution-blackboard)))
;;     (if (null focus)
;; 	(let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
;; 	       (agenda (pds~agenda pds))
;; 	       (all-tasks (agenda~all-tasks agenda))
;; 	       (goal-tasks-with-roc (remove-if-not #'(lambda (task)
;; 						       (and (agenda~goal-or-goal-schema-task-p task)
;; 							    (find pplan*roc-state-description (agenda~task-rocs task))))
;; 						   all-tasks)))
;; 	  (if goal-tasks-with-roc
;; 	      't
;; 	    't))
;;        nil)))

(defun no-focus-p ()
  (let* ((focus (black~get-blackboard-object-content 'focus sod*solution-blackboard)))
    (if (null focus)
	(if (and (crihelp=initial-strategy-p)
		 (null (pos~empty-p (second (roc~parameters pplan*roc-state-description)))))
	    ;; if the strategy is in an initial status (nothing done yet)
	    ;; and the position in the parameters is not empty
	    ;; then the decpmpsition is to be done first -> hence: no termination
	    nil
	  't)
      nil)))


;; (strat~define-strategy-ks
;;  (name UnwrapHyp)
;;  (refinement-algorithm PPlanner)
;;  (condition demand-for-unwraphyp-p)
;;  (methods (ANDE-M-F IMPE-OPEN-M-A FORALLE-META-M-F EXISTSE-M-A OrER-open-m-a OrEL-open-m-a)) ;; ORE-M-B))
;;  (normalization-methods nil)
;;  (restriction-methods nil)
;;  (control-rules (MYattack-focus))		 
;;  (loop-detection nil)
;;  (termination-check no-focus-p)
;;  (selection waterfall)
;;  (print "Strategy-KS UnwrapHyp: Offer to decompose assumption ~A"))

(strat~define-strategy-ks
 (name UnwrapHyp)
 (refinement-algorithm PPlanner)
 (condition complex-support-p)
 (methods (ANDE-M-F IMPE-OPEN-M-A FORALLE-META-M-F EXISTSE-M-A OrER-open-m-a OrEL-open-m-a)) ;; ORE-M-B))
 (normalization-methods nil)
 (restriction-methods nil)
 (control-rules (MYattack-focus))		 
 (loop-detection nil)
 (termination-check no-focus-p)
 (selection waterfall)
 (parameter-types (ndline position))
 (remark ("For the  goal <LISP>(verbalize-start-task state-des)</LISP><BR>a hypothesis from <LISP>(verbalize-first-parameter state-des)</LISP> is unwrapped: <LISP>(verbalize-unwrapped state-des)</LISP><BR>
This is used to show the subgoals:<BLOCKQUOTE><LISP>(verbalize-outlines state-des)</LISP></BLOCKQUOTE>"
	  "")
	 ("To show this a hypothesis from <LISP>(verbalize-first-parameter state-des)</LISP> is unwrapped: <LISP>(verbalize-unwrapped state-des)</LISP><BR>
Then the remaining goals are solved with it:<LISP>(increase-depth)</LISP><BLOCKQUOTE><LISP>(verbalize-text-outlines-subproofs state-des)</LISP>
</BLOCKQUOTE><LISP>(decrease-depth)</LISP>"
	  "To show this a hypothesis from <LISP>(verbalize-first-parameter state-des)</LISP> is unwrapped: <LISP>(verbalize-unwrapped state-des)</LISP><BR>
Then the remaining goals are solved with it:<LISP>(increase-depth)</LISP><BLOCKQUOTE><LISP>(verbalize-cons-outlines-subproofs state-des)</LISP>
</BLOCKQUOTE><LISP>(decrease-depth)</LISP>"))
 (print "Strategy-KS UnwrapHyp: Offer to decompose assumption ~A"))

;; ------------------------------------------------------------> AttackInequality

(defun strathelp=inequality-formula-p (formula)
  (and (data~appl-p formula)
       (or (or (keim~equal (data~appl-function formula)
			   (env~lookup-object 'less (pds~environment omega*current-proof-plan)))
	       (keim~equal (data~appl-function formula)
			   (env~lookup-object 'leq (pds~environment omega*current-proof-plan)))
	       (keim~equal (data~appl-function formula)
			   (env~lookup-object 'greater (pds~environment omega*current-proof-plan)))
	       (keim~equal (data~appl-function formula)
			   (env~lookup-object 'geq (pds~environment omega*current-proof-plan))))
	   (and (logic~negation-p formula)
		(data~appl-p (first (data~appl-arguments formula)))
		(or (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				(data~schema-range
				 (env~lookup-object '= (pds~environment omega*current-proof-plan))))
		    (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				(env~lookup-object 'less (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				(env~lookup-object 'leq (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				(env~lookup-object 'greater (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				(env~lookup-object 'geq (pds~environment omega*current-proof-plan))))))))

(defun strathelp=other-goals-for-CoSIE-p (formula)
  (if (data~appl-p formula)
      (let* ((func (data~appl-function formula))
	     (args (data~appl-arguments formula)))
	(if (or (and (keim~equal func (data~schema-range (env~lookup-object 'in (pds~environment omega*current-proof-plan))))
		     (= (length args) 2)
		     (or (keim~equal (second args) (env~lookup-object 'real (pds~environment omega*current-proof-plan)))
			 (keim~equal (second args) (env~lookup-object 'nat (pds~environment omega*current-proof-plan)))))
		(and (keim~equal func (data~schema-range (env~lookup-object '= (pds~environment omega*current-proof-plan))))
		     (= (length args) 2)
		     (or (meta~p (first args))
			 (meta~p (second args)))))
	    't
	  nil))
    nil))


(defun strathelp=goals-reducable-to-inequalities-p (formula)
  (if (logic~negation-p formula)
      (strathelp=goals-reducable-to-inequalities-p (first (data~appl-arguments formula)))
    (if (data~appl-p formula)
	(let* ((func (data~appl-function formula))
	       (args (data~appl-arguments formula))
	       (env (pds~environment omega*current-proof-plan)))
	  (if (or (keim~equal func (env~lookup-object 'lim env))
		  (keim~equal func (env~lookup-object 'cont env))
		  (keim~equal func (env~lookup-object 'limseq env)))
	      't
	    nil))
      nil)))
  
		
(defun strathelp=goal-for-CoSIE-p (node)
  (let* ((formula (node~formula node)))
    (or (strathelp=inequality-formula-p formula)
	(strathelp=goals-reducable-to-inequalities-p formula)
	(strathelp=other-goals-for-CoSIE-p formula)
	(logic~conjunction-p formula))))

(defun goal-for-CoSIE-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task)))
	(strathelp=goal-for-CoSIE-p node))
    nil))

(defun no-goals-for-CoSIE-p ()
  ;; Agenda steht in pds auf sod*solution-blackboard
  ;; Current ROC steht in pplan*roc-state-description

  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (all-tasks (agenda~all-tasks agenda))
	 (tasks-with-roc-and-with-goals-for-CoSIE
	  (remove-if-not #'(lambda (task)
			     (if (agenda~goal-or-goal-schema-task-p task)
				 (let* ((node (agenda~task-node task)))
				   (and (find pplan*roc-state-description (agenda~task-rocs task))
					(strathelp=goal-for-CoSIE-p node)))
			       nil))
			 all-tasks)))
    (if tasks-with-roc-and-with-goals-for-CoSIE
	nil
      't)))

#|
(strat~define-strategy-ks
 (name AttackInequality)
 (refinement-algorithm PPlanner)
 (condition goal-for-CoSIE-p)
 (methods (Solve*<-m-b Solve*<-<=-m-b Solve*<=-<-m-b Solve*>-m-b Solve*>->=-m-b Solve*>=->=-m-b
	   MComplexEstimate<-m-b MComplexEstimate<=-m-b MComplexEstimate>-m-b MComplexEstimate>=-m-b
	   MComplexEstimate>>-m-b FactorialEstimate-m-b
	   SimplifyInequality-m-b SimplifyInequality-m-f Simplify-m-b Simplify-m-f 
	   ;; DomainCaseSplit-m-b
	   ;; EnvI<-m-b EnvI>-m-b
	   EnvE<-m-f
	   MSET-FOCUS-M-B
	   DefnExp-m-b DefnExp-m-f
	   
	   ;; RESTRICTING-METHODS
	   TRUEI-M-B ANDI-M-B AskCS-m-b TellCS-m-b WEAKEN-M-A mflip-absval-inequality-m-b reflexdirect-m-b
	   
	   ;; NORMALIZING-METHODS
	   PushNeg-m-f ANDE-m-F TellCS-m-f mflip-absval-inequality-m-f
	   ))
 (normalization-methods (PushNeg-m-f ANDE-m-F mflip-absval-inequality-m-f
			 TellCS-m-f
			 ;; TellCS-m-f muss ganz nach hinten, da dort die Normalisierung abbricht (keine neue Zeile)
			 ))                       
 (restriction-methods (TRUEI-M-B ANDI-M-B AskCS-m-b
		       TellCS-m-b WEAKEN-M-A mflip-absval-inequality-m-b reflexdirect-m-b
		       ))
 (control-rules (;;MYSelect-DomainCaseSplit
		 ;;MYSelect-EnvE<-m-f ;; this crule is a little bit triggy! see limit-crules.thy
		 MYprefer-non-unwrap-goals
		 RejectNotCosieTasks
		 MYAttack-inequality<-non-standard-select
		 MYSelect-EnvE<-m-f
		 ;; MYOrder-Env ;; this crule is a little bit triggy! see limit-crules.thy
		 Interrupt-If-Focus
		 INTERRUPT-IF-INST
		 MYreject-myset-focus-mmatchings-if-nothing-new
		 DEFNEXP-SELECT-GOAL
		 DEFNEXP-SELECT-SUPPORT
		 ))
 (termination-check no-goals-for-CoSIE-p)
 (loop-detection 15)
 (selection waterfall)
 (remark ("The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is closed by attacking the inequality."
	  "")
	 ("<LISP>(verbalize-start-task state-des)</LISP> is solved by attacking the inequality."
	  "<LISP>(verbalize-start-task state-des)</LISP> is solved by attacking the inequality."))
 (print "Strategy-KS AttackInequality: Offer to tackle inequality-task ~A"))
|#


(strat~define-strategy-ks
 (name AttackInequality)
 (refinement-algorithm PPlanner)
 (condition goal-for-CoSIE-p)
 (methods (;; RESTRICTING-METHODS
	   TRUEI-M-B ANDI-M-B AskCS-m-b TellCS-m-b WEAKEN-M-A mflip-absval-inequality-m-b reflexdirect-m-b
	   
	   ;; NORMALIZING-METHODS
	   PushNeg-m-f ANDE-m-F TellCS-m-f mflip-absval-inequality-m-f

	   Solve*<-m-b Solve*<-<=-m-b Solve*<=-<-m-b Solve*>-m-b Solve*>->=-m-b Solve*>=->=-m-b
	   MComplexEstimate<-m-b MComplexEstimate<=-m-b MComplexEstimate>-m-b MComplexEstimate>=-m-b
	   MComplexEstimate>>-m-b FactorialEstimate-m-b  ;;MSumEstimateI<-m-b MSumEstimateII<-m-b MSumEstimateI>-m-b  MSumEstimateII>-m-b
	   SimplifyInequality-m-b SimplifyInequality-m-f Simplify-m-b Simplify-m-f 
	   	   
	   EnvI<-m-b EnvI>-m-b
	   EnvE<-m-f EnvE>-m-f
	   MSET-FOCUS-M-B
	   DefnExp-m-b DefnExp-m-f
	   CaseSplit-m-b
	   =subst-m-b
	   =subst*-m-b
	   ))
 (normalization-methods (PushNeg-m-f ANDE-m-F mflip-absval-inequality-m-f
			 TellCS-m-f
			 ;; TellCS-m-f muss ganz nach hinten, da dort die Normalisierung abbricht (keine neue Zeile)
			 ))                       
 (restriction-methods (TRUEI-M-B ;; ANDI-M-B
				 AskCS-m-b
		       TellCS-m-b WEAKEN-M-A mflip-absval-inequality-m-b reflexdirect-m-b
		       ))
 (control-rules (MYprefer-non-unwrap-goals
		 RejectNotCosieTasks
		 MYAttack-inequality<-non-standard-select
		 MYSelect-EnvE<-m-f
		 MYSelect-EnvE>-m-f
		 INTERRUPT-IF-COMPLEX-TASK-FROM-DEFNEXP
		 Interrupt-If-Focus
		 INTERRUPT-IF-INST
		 MYreject-myset-focus-mmatchings-if-nothing-new
		 reject-simplify-if-absval-minus-flip
		 DEFNEXP-SELECT-GOAL
		 DEFNEXP-SELECT-SUPPORT
		 PREFER-CASESPLIT-ON-UNWRAPHYP-PREMISE
		 =subst-on-goal
		 Apply=Subst*-m-b
		 ))
 (termination-check no-goals-for-CoSIE-p)
 (loop-detection 15)
 (selection waterfall)
 (remark ("The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is closed by attacking the inequality."
	  "")
	 ("<LISP>(verbalize-start-task state-des)</LISP> is solved by attacking the inequality."
	  "<LISP>(verbalize-start-task state-des)</LISP> is solved by attacking the inequality."))
 (print "Strategy-KS AttackInequality: Offer to tackle inequality-task ~A"))


(cri~def-control-rule INTERRUPT-IF-COMPLEX-TASK-FROM-DEFNEXP
		      (kind strategy-interruption)
		      (if (and (task-from-defnexp-p "task")
			       (not (strategy-failed-already-p "task" Normalizegoal))))
		      (then
		       (insert ((NormalizeGoal "task" nil)))))

(defun task-from-defnexp-p (task)
  (if (null (stringp task))
      ;; already bound
      (if (crihelp=task-from-defnexp-p task)
	  (list (list (cons T T)))
	nil)
    (let* ((all-tasks cri*current-tasks)
	   (all-tasks-from-defnexp-p (remove-if-not #'crihelp=task-from-defnexp-p all-tasks)))
      (mapcar #'(lambda (ta)
		  (list (cons task ta)))
	      all-tasks-from-defnexp-p))))
							
	    
(defun crihelp=task-from-defnexp-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task))
	     (just (node~justification node))
	     (other-reasons (pdsj~other-reasons just))
	     (other-reasons-with-defnexp-m (remove-if-not #'(lambda (step)
							      (let* ((just (pdsc~an-just step))
								     (method (just~method just)))
								(if (eq method (infer~find-method 'defnexp-m))
								    't
								  nil)))
							  other-reasons)))
	(if other-reasons-with-defnexp-m
	    't
	  nil))
    nil))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RepairRestPairs


#| 
(strat~define-strategy-ks
 (name RepairRestPairs)
 (refinement-algorithm PPlanner)
 (condition find-repair-unify-for-open-node-p)
 (methods (=subst*-m-b)
	  )
 (normalization-methods nil)
 (restriction-methods nil)
 (control-rules (Apply=Subst*-m-b)
		)
 (loop-detection nil)
 (termination-check step-done-p)
 (selection waterfall)
 (print "Strategy-KS RepairRestPairs: Offer to repair the broken task ~A"))

(defun step-done-p ()
  (if (roc~pplanner-steps pplan*roc-state-description)
      't
    nil))

(defun strathelp=last-strat-was-ATTACKINEQUALITY-and-terminated-with-no-applicable-method-failure-on-task-p (task)
  (if (and (exmes~failure-message-p sod*execution-message)
	   (string-equal (first (exmes~failure-message-report sod*execution-message)) 'pplanner)	   
	   (string-equal (second (exmes~failure-message-report sod*execution-message)) 'no-applicable-method)
	   (eq task (third (exmes~failure-message-report sod*execution-message)))
	   (eq (strat~find-strategy-ks 'AttackInequality) (roc~strategy-ks (exmes~state-description sod*execution-message))))
      't
    nil))
      
(defun find-repair-unify-for-open-node-p (task)
  (if (and (agenda~goal-or-goal-schema-task-p task)
	   (strathelp=last-strat-was-ATTACKINEQUALITY-and-terminated-with-no-applicable-method-failure-on-task-p task))
      (let* ((open-node (agenda~task-node task))
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

	(if suitable-repair-tupels
	    (progn 
	      (setq strathelp*store (first suitable-repair-tupels))
	      't)
	  nil))
    nil))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                    Strategic Control!!         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cri~def-control-rule PREFER-BACKTRACK-INST-IF-MV-NOT-LONGER-DET
		      (kind strategic)
		      (if (and (determined-meta-variable-not-longer-bound-p "mv")
			       (make-job backtrack-mv-inst "task" ("mv") "job")))
		      (then 
		       (insert ("job"))))

(defun make-job (strategy task params job)
  (if (or (stringp strategy)
	  (some #'stringp params))
      ;; strategy unbound or some of the parameters unbound
      nil
    (let* ((strat (strat~find-strategy-ks strategy)))
      (if (stringp task)
	  ;; task unbound -> choose arbitrary task
	  (let* ((ta (first (agenda~all-tasks (pds~agenda omega*current-proof-plan)))))
	    (list (list (cons task ta)
			(cons job (job~create-strategy-ks-offer strat ta params)))))
	(list (list (cons job (job~create-strategy-ks-offer strat task params))))))))
      

(defun determined-meta-variable-not-longer-bound-p (mv)
  (if (null (stringp mv))
      ;; mv is bound
      (if (crihelp=mv-from-detcs-but-not-longer-bound-p mv)
	  (list (list (cons T T)))
	nil)
    ;; mv is not bound
    (let* ((constraint-pool (pds~constraint-pool omega*current-proof-plan))
	   (bindings (if constraint-pool
			 (pds~cstrpool-bindings constraint-pool)
		       (subst~create () ())))
	   (mvs-inst (subst~domain bindings))
	   (mvs-inst-from-detcs-but-not-longer-bound (remove-if-not #'crihelp=mv-from-detcs-but-not-longer-bound-p mvs-inst)))
      (mapcar #'(lambda (mv-inst)
		  (list (cons mv mv-inst)))
	      mvs-inst-from-detcs-but-not-longer-bound))))
	  

(defun crihelp=mv-from-detcs-but-not-longer-bound-p (mv)
  (let* ((detvars (cosie~getdetvars))
	 (mv-pair (find mv detvars :test #'(lambda (mvar pair)
					     (keim~equal mvar (first pair))))))
    (if mv-pair
	;; -> however mv was bound it is still determined
	nil
      (let* ((all-state-des (store~elements (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))
	     (all-state-des-of-DetFromCs (remove-if-not #'(lambda (state-des)
							    (eq (strat~find-strategy-ks 'detfromcs)
								(roc~strategy-ks state-des)))
							all-state-des))
	     (mvs-bound-by-detfromcs (mapcar #'(lambda (detfromcs-state-des)
						 (let* ((start-task (roc~start-task detfromcs-state-des)))
						   (agenda~inst-task-meta-var start-task)))
					     all-state-des-of-DetFromCs)))
	(if (find mv mvs-bound-by-detfromcs)
	    ;; -> mv was bound by detfromcs but is not longer determined
	    't
	  nil)))))
			    
	

(defun get-goal-task (task)
  (if (stringp task)
      ;; task unbound
      (let* ((all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	     (goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p all-tasks)))
	(if goal-tasks
	    (list (list (cons task (first goal-tasks))))
	  nil))
    ;; task bound
    (if (agenda~goal-or-goal-schema-task-p task)
	(list (list (cons T T)))
      nil)))
	
(cri~def-control-rule PREFER-INDIRECTPROOF-BEFORE-NORMALIZEGOAL
		      (kind strategic)
		      (if (and (job-is INDIRECTPROOF "job1" "task")
			       (job-is NORMALIZEGOAL "job2" "task")))
		      (then
		       (order-before (("job1" "job2")))))

(cri~def-control-rule PREFER-INDIRECTPROOF-BEFORE-ATTACKINEQUALITY
		      (kind strategic)
		      (if (and (job-is INDIRECTPROOF "job1" "task")
			       (job-is ATTACKINEQUALITY "job2" "task")))
		      (then
		       (order-before (("job1" "job2")))))

(cri~def-control-rule REJECT-INDIRECTPROOF-IF-NOT-ROOT-TASK
		      (kind strategic)
		      (if (and (job-is INDIRECTPROOF "job1" "task")
			       (not (task-is-proof-root "task"))))
		      (then
		       (reject ("job1"))))
;; this control rule allows the application of INDIRECTPROOF only to the root of the proof!!

#|(cri~def-control-rule PREFER-RepairRestPairs
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    (strategy-is ATTACKINEQUALITY "strategy"))
			       (job-is RepairRestPairs "job1" "task")))
		      (then
		       (prefer ("job1"))))|#

(cri~def-control-rule PREFER-SIMPLE-ATTACKINEQUALITY-JOBS
		      (kind strategic)
		      (if (and (job-is ATTACKINEQUALITY "job1" "task")
			       (task-is-simple "task")))
		      (then
		       (prefer ("job1"))))


;; a formula t < t' is simple when either t or t' is a meta-variable
(defun task-is-simple (task)
  (if (stringp task)
      ;; task is unbound -> nil
      nil
    ;; task is already bound
    (let* ((formula (node~formula (agenda~task-node task)))
	   (env (pds~environment omega*current-proof-plan)))
      (if (and (data~appl-p formula)
	       (find (data~appl-function formula)
		     (list (env~lookup-object 'less env)
			   (env~lookup-object 'leq env)
			   (env~lookup-object 'greater env)
			   (env~lookup-object 'leq env)))
	       (remove-if-not #'meta~p (data~appl-arguments formula)))
	  (list (list (cons T T)))
	nil))))
      
(defun task-is-proof-root (task)
  (if (stringp task)
      ;; task is unbound -> nil
      nil
    ;; task is already bound
    (let* ((node (agenda~task-node task)))
      (if (eq node (prob~proof-root omega*current-proof-plan))
	  (list (list (cons T T)))
	nil))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategien zu BackTrack  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Backtrack-Cosie-step

;; This strategy backtracks the step that introduced an open goal

(strat~define-strategy-ks
 (name backtrack-Cosie-Step-to-insttask)
 (refinement-algorithm backtrack)
 (condition no-open-goals-and-cosie-fails-p)
 (compute-backtrack-steps-function determine-tellcs-steps)
 (print "Strategy-KS Backtrack-Cosie-Step-to-Insttask: Backtracks a tellcs step since CoSie fails to provide instantiation for insttask ~A"))


(defun no-open-goals-and-cosie-fails-p (task)
  (if (agenda~inst-task-p task)
      (let* ((all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	     (goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p all-tasks)))
	(if (null goal-tasks)
	    (let* ((subst (strathelp=cosie-search))
		   (meta-var (agenda~inst-task-meta-var task)))
	      (unless (find meta-var (subst~domain subst))
		't))
	  nil))
    nil))

(defun determine-tellcs-steps (task)
  (let* ((all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	 (inst-tasks (remove-if-not #'agenda~inst-task-p all-tasks))
	 (subst (strathelp=cosie-search))
	 (unresolved-inst-tasks (remove-if-not #'(lambda (inst-task)
						   (let* ((meta-var (agenda~inst-task-meta-var inst-task)))
						     (unless (find meta-var (subst~domain subst))
						       't))
						   )
					       inst-tasks))
	 (unresolved-mvs (mapcar #'agenda~inst-task-meta-var unresolved-inst-tasks))
	 (plan-steps-with-tellcs-and-unresolved-mvs (strathelp=plan-steps-with-tellcs-and-unresolved-mvs unresolved-mvs))
	 (step (strathelp=choose-interesting-plan-step plan-steps-with-tellcs-and-unresolved-mvs)))
    (values
     (list step)
     nil)))


#|(defun strathelp=choose-interesting-plan-step (plan-steps-with-tellcs-and-unresolved-mvs)
  (let* ((remaining-plan-steps ;; remove-steps where both sides are primitives (e.g. E > 0 etc.)
	  (remove-if #'(lambda (plan-step)
			 (let* ((plan-step-node (pdsc~an-node plan-step))
				(formula (node~formula plan-step-node))
				(args (data~appl-arguments formula))
				(arg1 (first args))
				(arg2 (second args)))
			   (if (and (strathelp=simple-side-p arg1)
				    (strathelp=simple-side-p arg2))
			       't
			     nil)))
		     plan-steps-with-tellcs-and-unresolved-mvs))	 
	 (interesting-steps ;; prefer steps where both sides are complex
	  (remove-if-not #'(lambda (plan-step)
			     (let* ((plan-step-node (pdsc~an-node plan-step))
				    (formula (node~formula plan-step-node))
				    (args (data~appl-arguments formula))
				    (arg1 (first args))
				    (arg2 (second args)))
			       (if (and (null (strathelp=simple-side-p arg1))
					(null (strathelp=simple-side-p arg2)))
				   't
				 nil)))
			 remaining-plan-steps)))
    (if interesting-steps
	;;(first interesting-steps)
	(strathelp=choose-most-complex interesting-steps)
      (if remaining-plan-steps
	  ;;(first remaining-plan-steps)
	  (strathelp=choose-most-complex remaining-plan-steps)
	;;(first plan-steps-with-tellcs-and-unresolved-mvs)
	(strathelp=choose-most-complex plan-steps-with-tellcs-and-unresolved-mvs)))))|#

(defun strathelp=choose-most-complex (plan-steps)
  (first (sort plan-steps #'(lambda (step1 step2)
			      (let* ((plan-step-node1 (pdsc~an-node step1))
				     (formula1 (node~formula plan-step-node1))
				     (plan-step-node2 (pdsc~an-node step2))
				     (formula2 (node~formula plan-step-node2)))
				(> (length (data~all-substructs formula1))
				   (length (data~all-substructs formula2))))))))
  

			      
(defun strathelp=choose-interesting-plan-step (plan-steps-with-tellcs-and-unresolved-mvs)
  (let* ((remaining-plan-steps ;; remove-steps where both sides are primitives (e.g. E > 0 etc.)
	  (remove-if #'(lambda (plan-step)
			 (let* ((plan-step-node (pdsc~an-node plan-step))
				(formula (node~formula plan-step-node))
				(args (data~appl-arguments formula))
				(arg1 (first args))
				(arg2 (second args)))
			   (if (and (strathelp=simple-side-p arg1)
				    (strathelp=simple-side-p arg2))
			       't
			     nil)))
		     plan-steps-with-tellcs-and-unresolved-mvs))
	 (absval-steps ;; steps that contain absval on one side, where the absval is not on a term-primitive just!
	  (remove-if-not #'(lambda (plan-step)
			     (let* ((plan-step-node (pdsc~an-node plan-step))
				    (formula (node~formula plan-step-node))
				    (args (data~appl-arguments formula))
				    (arg1 (first args))
				    (arg2 (second args)))
			       (if (or (strathelp=complex-absval-p arg1)
				       (strathelp=complex-absval-p arg2))
				   't
				 nil)))
			 remaining-plan-steps))
	 (other-interesting-steps ;; steps where both sides are comples
	  (remove-if-not #'(lambda (plan-step)
			     (let* ((plan-step-node (pdsc~an-node plan-step))
				    (formula (node~formula plan-step-node))
				    (args (data~appl-arguments formula))
				    (arg1 (first args))
				    (arg2 (second args)))
			       (if (and (null (strathelp=simple-side-p arg1))
					(null (strathelp=simple-side-p arg2)))
				   't
				 nil)))
			 remaining-plan-steps))
	 (rest-steps (remove-if #'(lambda (plan-step)
				    (if (or (find plan-step absval-steps)
					    (find plan-step other-interesting-steps))
					't
				      nil))
				remaining-plan-steps)))
    (if absval-steps
	(strathelp=choose-most-complex absval-steps)
      (if other-interesting-steps
	  (strathelp=choose-most-complex other-interesting-steps)
	(if rest-steps
	    (strathelp=choose-most-complex rest-steps)
	  (strathelp=choose-most-complex plan-steps-with-tellcs-and-unresolved-mvs))))))


(defun strathelp=complex-absval-p (formula)
  (if (and (data~appl-p formula)
	   (keim~equal (data~appl-function formula) (env~lookup-object 'absval (pds~environment omega*current-proof-plan)))
	   (null (term~primitive-p (first (data~appl-arguments formula)))))
      't
    nil))
	  
(defun strathelp=simple-side-p (formula)
  (if (or (term~primitive-p formula)
	  (and (data~appl-p formula)
	       (keim~equal (data~appl-function formula) (env~lookup-object 'absval (pds~environment omega*current-proof-plan)))
	       (term~number-p (first (data~appl-arguments formula)))))
      't
    nil))

(defun strathelp=plan-steps-with-tellcs-and-unresolved-mvs (unresolved-mvs)
  (remove-if-not #'(lambda (plan-step)
		     (let* ((plan-step-node (pdsc~an-node plan-step))
			    (plan-step-just (pdsc~an-just plan-step))
			    (plan-step-just-method (just~method plan-step-just))
			    (plan-step-just-method-name (keim~name plan-step-just-method)))
		       (if (string-equal plan-step-just-method-name
					 'tellcs-m)
			   (let* ((formula (node~formula plan-step-node)))
			     (if (intersection (data~all-substructs formula) unresolved-mvs)
				 't
			       nil)))))
		 (pds~plan-steps omega*current-proof-plan)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backtrack-unwraphy-step



(strat~define-strategy-ks
 (name backtrack-unwraphy-step)
 (refinement-algorithm backtrack)
 (condition task-from-unwraphyp-p)
 (compute-backtrack-steps-function remove-unwraphyp-strategy-to-task)
 (print "Strategy-KS backtrack-unwraphy-step: Offer to backtrack the last strategy applicated on task ~A"))

(defun task-from-unwraphyp-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task))
	     (andi-node (crihelp=andi-other-reason-p node))
	     (other-reason (if andi-node
			       (first (pdsj~other-reasons (node~justification andi-node)))
			     (first (pdsj~other-reasons (node~justification node)))))
	     (roc-with-step (first (remove-if-not #'(lambda (roc)
						      (find other-reason (roc~pplanner-steps roc)))
						  (remove-if-not #'roc~pplanner-state-description-p
								 (store~elements
								  (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))))))
	(if (null roc-with-step)
	    nil
	  (let* ((start-task (roc~start-task roc-with-step))
		 (start-node (agenda~task-node start-task))
		 (start-other-reason (first (pdsj~other-reasons (node~justification start-node))))
		 (strategy (roc~strategy-ks roc-with-step)))
	    (if (and (eq strategy (strat~find-strategy-ks 'unwraphyp))
		     start-other-reason
		     (eq (just~method (pdsc~an-just start-other-reason)) (infer~find-method 'MSET-FOCUS-M)))
		't
	      nil))))
    nil))

(defun crihelp=andi-other-reason-p (node)
  (let* ((just (node~justification node))
	 (other-reasons (pdsj~other-reasons just))
	 (other-reason-andi (first (remove-if-not #'(lambda (reason)
						      (if (eq (just~method (pdsc~an-just reason)) (infer~find-method 'andi-m))
							  't
							nil))
						  other-reasons)))			    
	 (andi-node (if other-reason-andi
			(pdsc~an-node other-reason-andi)
		      nil)))
    andi-node))  

(defun remove-unwraphyp-strategy-to-task (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task))
	     (andi-node (crihelp=andi-other-reason-p node))
	     (other-reason (if andi-node
			       (first (pdsj~other-reasons (node~justification andi-node)))
			     (first (pdsj~other-reasons (node~justification node)))))
	     (unwraphyproc (first (remove-if-not #'(lambda (roc)
						     (find other-reason (roc~pplanner-steps roc)))
						 (remove-if-not #'roc~pplanner-state-description-p
								(store~elements
								 (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))))))
	(if (null unwraphyproc)
	    (progn
	      (omega~warn "Could not find unwraphyp strategy in function ove-unwraphyp-strategy-to-task")
	      (values nil nil))
	  (let* ((start-task (roc~start-task unwraphyproc))
		 (start-node (agenda~task-node start-task))
		 (start-other-reason (first (pdsj~other-reasons (node~justification start-node))))
		 (attackinequalityroc (first (remove-if-not #'(lambda (roc)
								(find start-other-reason (roc~pplanner-steps roc)))
							    (remove-if-not #'roc~pplanner-state-description-p
									   (store~elements
									    (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))))))
	    (if (null attackinequalityroc)
		(progn
		  (omega~warn "Could not find attackinequality strategy in function ove-unwraphyp-strategy-to-task")
		  (values nil nil))
	      (let* ((all-steps-behind-set-focus (cons start-other-reason
						       (do* ((steps-in-ai (roc~pplanner-steps attackinequalityroc) (rest steps-in-ai))
							     (back-list nil))
							   ((or (null steps-in-ai)
								(eq (first steps-in-ai) start-other-reason))
							    back-list)
							 (setf back-list (append back-list (list (first steps-in-ai))))))))
		(values (append all-steps-behind-set-focus (list (roc~start-step unwraphyproc)))
			nil))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; control backtrack-unwraphy-step


(cri~def-control-rule PREFER-STRATEGY-BACKTRACK-UNWRAPHYP
		      (kind strategic)
		      (if (and (and (LAST-EXMES-IS-NO-METHOD-APPLICABLE-FAILURE "strategy" "task")
				    ;; last strategy said no method applicable for task
				    (strategy-is AttackInequality "strategy"))
			       (and (task-is-from-unwraphyp "task")
				    (job-is backtrack-unwraphy-step "job" "task"))))
		      (then
		       (prefer ("job"))))

(defun task-is-from-unwraphyp (task)
  (if (stringp task)
      (progn 
	(omega~warn "The case of unbound task is not yet implemented in the control rule functeion task-is-from-unwraphyp!")
	nil)
    (let* ((node (agenda~task-node task))
	   (andi-node (crihelp=andi-other-reason-p node))
	   (other-reason (if andi-node
			     (first (pdsj~other-reasons (node~justification andi-node)))
			   (first (pdsj~other-reasons (node~justification node)))))
	   (roc-with-step (first (remove-if-not #'(lambda (roc)
						    (find other-reason (roc~pplanner-steps roc)))
						(remove-if-not #'roc~pplanner-state-description-p
							       (store~elements
								(black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))))))
      (if (null roc-with-step)
	  nil
	(let* ((strategy (roc~strategy-ks roc-with-step)))
	  (if (eq strategy (strat~find-strategy-ks 'unwraphyp))
	      (list (list (cons T T)))
	    nil))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REDUCETOSPECIAL-LIM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Application Condition of ReduceToSpecial Strategy
(defun reducetospecial-lim-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      
      (let* ((node (agenda~task-node task))
	     (formula (node~formula node))
	     (env (pds~environment omega*current-proof-plan))) ;; war: (th~env zmz)   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	(if (or (and (data~appl-p formula)
		     (env~lookup-object 'lim env)
		     (keim~equal (data~appl-function formula)
				 (env~lookup-object 'lim env)))
		
		;;(and (data~appl-p formula)
		;;     (env~lookup-object 'leq env)
		 ;;    (keim~equal (data~appl-function formula)
		;;		 (env~lookup-object 'leq env)))
		;;(and (data~appl-p formula)
		;;     (env~lookup-object 'less env)
		;;    (keim~equal (data~appl-function formula)
		;;		 (env~lookup-object 'less env)))
		;;(and (data~appl-p formula)
		;;     (env~lookup-object 'greater env)
		;;     (keim~equal (data~appl-function formula)
		;;		 (env~lookup-object 'greater env)))
		;;(and (data~appl-p formula)
		;;     (env~lookup-object 'geq env)
		;;     (keim~equal (data~appl-function formula)
		;;		 (env~lookup-object 'geq env)))
		;;
		;;
		)
	    't
	  nil))
    nil))

(strat~define-strategy-ks
 (name ReduceToSpecial-lim)
 (refinement-algorithm PPlanner)
 (condition reducetospecial-lim-p)
 (methods (AskCS-m-b TellCS-m-b
	   MAssertion-m-b
	   ReductionClosed-m-b
	   InResclass-m-b
	   Int-m-b
	   SubsetResclass-m-b
	   ;;IncludeTheorems-m-b -> braucht man nicht mehr -> Theorems kommen uber CRules (Redspec-standard-select) rein
	   ANDI-m-b SimplifyOrder-m-b
	   reflex-m-b NotReflex-m-b
	   ))
 (normalization-methods nil)
 (restriction-methods (AskCS-m-b
		       TellCS-m-b
		       Int-m-b
		       SubsetResclass-m-b
		       InResclass-m-b
		       ANDI-m-b
		       reflex-m-b
		       NotReflex-m-b
		       SimplifyOrder-m-b
		       ))
 (control-rules (;;redspec-include-theorems -> braucht man nicht mehr -> Theorems kommen uber CRules (Redspec-standard-select) rein
		 INTERRUPT-FOR-LIMIT-PROBLEMS-OF-SQUEEZE
		 ;;IGNORE-LIMIT-TASKS-FROM-SQUEEZE
		 INTERRUPT-IF-INST
		 prefer-inequality-tasks
		 redspec-standard-select-lim
		 reject-theorem-application-on-inequality-from-squeeze
		 INTERRUPT-FOR-NORMALIZEGOAL-IF-COMPLEX-PREMISSE-AND-LIMIT
		 )) 
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-further-goal-p)
 (selection waterfall)
 (remark "The goal <LISP>(verbalize-start-task state-des)</LISP><BR>is solved by reducing it with theorem applications to a special case.")
 (print "Strategy-KS ReduceToSpecial: Offer to prove task ~A by reduce it with theorem applications to sepcial cases"))



;;;;;;;;;;;;;;;;;;;;;;;; CRULES OF REDUCETOSPECIAL-LIM

(cri~def-control-rule INTERRUPT-FOR-LIMIT-PROBLEMS-OF-SQUEEZE
		      (kind strategy-interruption)
		      (if (and (no-inequality-tasks-from-squeeze-theorem-p)
			       (task-is-from-normalize-squeeze-theorem-p "task")))
		      (then
		       (insert ((AttackInequality "task" nil)))))

(cri~def-control-rule IGNORE-LIMIT-TASKS-FROM-SQUEEZE
		      (kind tasks)
		      (if (and (no-inequality-tasks-from-squeeze-theorem-p)
			       (task-is-from-normalize-squeeze-theorem-p "task")))
		      (then
		       (reject ("task"))))

(defun no-inequality-tasks-from-squeeze-theorem-p ()
  (let* ((all-goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (tasks-from-squeeze (remove-if-not #'crihelp=task-is-from-normalize-squeeze-theorem-p all-goal-tasks))
	 (inequality-tasks-from-squeeze (remove-if-not #'crihelp=task-is-goal-task-and-formula-is-inequality-p tasks-from-squeeze)))
    (if inequality-tasks-from-squeeze
	nil
      't)))


(cri~def-control-rule reject-theorem-application-on-inequality-from-squeeze
		      (kind mmatchings)
		      (if (and (and (mmatching-with-massertion-m-b-on-task-p "mmatching" "task")
				    (task-goal-is-inequality-p "task"))
			       (and (task-is-from-normalize-squeeze-theorem-p "task")
				    (not (mmatching-binds-function-from-squeeze-p "mmatching")))))
		      (then
		       (reject ("mmatching"))))

(defun mmatching-with-massertion-m-b-on-task-p (mmatching task)
  (if (or (null (stringp mmatching))
	  (null (stringp task)))
      ;; task or mmatching are bound
      nil
    (let* ((current-task cri*current-task)
	   (mmatchings-with-massertion-m-b (remove-if-not #'(lambda (mmatching)
							      (eq (pplan~matched-method mmatching)
								  (meth~find-method 'Massertion-M-B)))
							  cri*current-mmatchings)))
      (mapcar #'(lambda (mm)
		  (list (cons mmatching mm)
			(cons task current-task)))
	      mmatchings-with-massertion-m-b))))

(defun mmatching-binds-function-from-squeeze-p (mmatching)
  (if (stringp mmatching)
      ;; unbound
      nil
    (let* ((mapping (pplan~mmatch-mmapp mmatching))
	   (constraint (meth~mapp-constraint mapping))
	   (bindings (if (pds~constraint-pool-p constraint)
			 (pds~cstrpool-bindings constraint)
		       (subst~create nil nil)))
	   (bound-meta-variables (subst~domain bindings))
	   (all-inst-tasks (remove-if-not #'agenda~inst-task-p (agenda~all-tasks (pds~agenda omega*current-proof-plan))))
	   (inst-tasks (mapcar #'(lambda (mv)
				   (find mv all-inst-tasks :test #'(lambda (mv inst-task)
								     (eq mv (agenda~inst-task-meta-var inst-task)))))
			       bound-meta-variables)))
      (if (remove-if-not #'(lambda (inst-task)
			     (and (crihelp=inst-task-from-squeeze-p inst-task)
				  (crihelp=no-meta-vars-in-inst-p inst-task bindings)
				  (crihelp=no-thm-functions-of-other-side-p inst-task bindings (node~formula (pplan~mmatch-goal mmatching)))))
			 inst-tasks)
	  't
	nil))))

(defun crihelp=no-thm-functions-of-other-side-p (inst-task bindings inequality)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (mv (agenda~inst-task-meta-var inst-task))
	 (new-term (subst~apply bindings mv))
	 (inequality-sides (data~appl-arguments inequality))
	 (lside (first inequality-sides))
	 (rside (second inequality-sides))
	 (other-side (if (and (data~appl-p lside)
			      (keim~equal mv (data~appl-function lside)))
			 rside
		       lside))
	 (all-substructs-other-side (data~all-substructs other-side))
	 (all-substructs-new-term (data~all-substructs new-term))
	 (known-functions (list (env~lookup-object 'plus env)
				(env~lookup-object 'minus env)
				(env~lookup-object 'times env)
				(env~lookup-object 'div env)
				(env~lookup-object 'absval env)
				(env~lookup-object 'power env)))
	 (all-subs-other-side-w (remove-if #'(lambda (sub)
					       (or (null (data~primitive-p sub))
						   (find sub known-functions)))
					   all-substructs-other-side))
	 (all-subs-new-term-w (remove-if #'(lambda (sub)
					     (or (null (data~primitive-p sub))
						 (find sub known-functions)))
					 all-substructs-new-term)))
    (if (intersection all-subs-other-side-w all-subs-new-term-w)
	nil
      't)))
				
				
(defun crihelp=no-meta-vars-in-inst-p (inst-task bindings)
  (let* ((new-term (subst~apply bindings (agenda~inst-task-meta-var inst-task))))
    (if (null (remove-if-not #'meta~p (data~all-substructs new-term)))
	't
      nil)))

(defun crihelp=inst-task-from-squeeze-p (inst-task)      
  (let* ((step (agenda~inst-task-plan-step inst-task))
	 (method-of-step (just~method (pdsc~an-just step)))
	 (goal-of-step (pdsc~an-node step))
	 (step-of-goal (first (pdsj~other-reasons (node~justification goal-of-step))))
	 (method-of-goal-step (just~method (pdsc~an-just step-of-goal))))

    (cond ((and (eq method-of-step (infer~find-method 'existsi-m))
		(eq method-of-goal-step (infer~find-method 'MAssertion-m))
		(eq (first (pdsj~parameters (pdsc~an-just step-of-goal)))
		    (th~find-assumption 'squeeze-theorem (th~find-theory 'limit))))
	   't)
	  ((and (eq method-of-step (infer~find-method 'existsi-m))
		(eq method-of-goal-step (infer~find-method 'existsi-m)))
	   (let* ((goal-of-goal-step (pdsc~an-node step-of-goal))
		  (step-of-goal-goal (first (pdsj~other-reasons (node~justification goal-of-goal-step))))
		  (method-of-goal-goal-step (just~method (pdsc~an-just step-of-goal-goal))))
	     (if (and (eq method-of-goal-goal-step (infer~find-method 'MAssertion-m))
		      (eq (first (pdsj~parameters (pdsc~an-just step-of-goal-goal)))
			  (th~find-assumption 'squeeze-theorem (th~find-theory 'limit))))
		 't
	       nil)))
	  (t
	   nil))))
		
    
    
      




	   

(cri~def-control-rule INTERRUPT-FOR-NORMALIZEGOAL-IF-COMPLEX-PREMISSE-AND-LIMIT
		      (kind strategy-interruption)
		      (if (and (and (current-theory-is Limit)
				    (complex-premisse-from-theorem-p "task"))
			       (not (strategy-failed-already-p "task" Normalizegoal))))
		      (then
		       (insert ((Normalizegoal "task" nil)))))

(defun strategy-failed-already-p (task strategy)
  (if (or (stringp task)
	  (stringp strategy))
      ;; unbound
      nil
    (let* ((strategy (strat~find-strategy-ks strategy))
	   (failed-strategies (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task))))))
      (if (find strategy failed-strategies :test #'(lambda (strat pair)
						     (keim~equal strat (first pair))))
	  't
	nil))))
	   


(defun current-theory-is (theory)
  (let* ((current-theory (prob~theory omega*current-proof-plan)))
    (if (stringp theory)
	;; input unbound
	(list (list (cons theory current-theory)))
      (let* ((set-theory (th~find-theory theory)))
	(if (eq set-theory current-theory)
	    (list (list (cons T T)))
	  nil)))))

(defun complex-premisse-from-theorem-p (task)
  (let* ((current-tasks cri*current-tasks)
	 (goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p current-tasks))
	 (tasks-from-MAssertion-m (remove-if-not #'(lambda (task)
						     (let* ((node (agenda~task-node task))
							    (just (node~justification node))
							    (other-reasons (pdsj~other-reasons just))
							    (other-reasons-with-massertion-m (remove-if-not #'(lambda (step)
														(let* ((just (pdsc~an-just step))
														       (method (just~method just)))
														  (if (eq method (infer~find-method 'MAssertion-m))
														      't
														    nil)))
													    other-reasons)))
						       (if other-reasons-with-massertion-m
							   't
							 nil)))
						 goal-tasks))
	 (complex-tasks-from-MAssertion-m (remove-if-not #'(lambda (task)
							     (non-literal-goal-or-defined-concept-p task))
							 tasks-from-MAssertion-m)))
    (if complex-tasks-from-MAssertion-m
	(mapcar #'(lambda (taski)
		    (list (cons task taski)))
		complex-tasks-from-MAssertion-m)
      nil)))

							 
								    
							 



	 
	   



    

(cri~def-control-rule redspec-standard-select-lim
		      (kind methods)
		      (if (theorem-of-thy-p "theorem"))
		      (then
		       (select ((MAssertion-m-b () ("theorem"))))))


;; Prefer inequalities before other things
;; (a little bit hacky this crule guarantees that problems like sinus and cosinus do work in the limit domain) 
(cri~def-control-rule prefer-inequality-tasks
		      (kind tasks)
		      (if (and (task-goal-is-inequality-p "task")
			       (task-is-from-normalize-squeeze-theorem-p "task")))
		      (then
		       (select ("task"))))

(defun task-is-from-normalize-squeeze-theorem-p (task)
  (if (stringp task)
      (let* ((all-goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	     (all-goal-tasks-from-squeeze (remove-if-not #'crihelp=task-is-from-normalize-squeeze-theorem-p all-goal-tasks)))
	(if all-goal-tasks-from-squeeze
	    (mapcar #'(lambda (tsqu)
			(list (cons task tsqu)))
		    all-goal-tasks-from-squeeze)
	  nil))
    (if (null (agenda~goal-or-goal-schema-task-p task))
	nil
      (if (crihelp=task-is-from-normalize-squeeze-theorem-p task)
	  (list (list (cons T T)))
	nil))))

(defun crihelp=task-is-from-normalize-squeeze-theorem-p (task) 
  (let* ((node (agenda~task-node task))
	 (just (node~justification node))
	 (other-reasons (pdsj~other-reasons just))
	 (other-reason-from-normalize-task (first (remove-if-not #'crihelp=step-in-normalize-task-strategy-p other-reasons))))
    (if other-reason-from-normalize-task
	(let* ((normalize-task-roc (crihelp=step-in-normalize-task-strategy-p other-reason-from-normalize-task))
	       (start-task (roc~start-task normalize-task-roc))
	       (node (agenda~task-node start-task)))
	  (if (crihelp=node-from-massertion-m-with-squeeze-theorem-p node)
	      't
	    nil))
      nil)))

(defun crihelp=node-from-massertion-m-with-squeeze-theorem-p (node)
  (let* ((steps-of-current-roc (roc~pplanner-steps pplan*roc-state-description))
	 (step-with-node-as-premisse-and-massertion-m (first (remove-if-not #'(lambda (step)
										(let* ((just (pdsc~an-just step))
										       (premisses (just~premises just))
										       (method (just~method just)))
										  (if (and (find node premisses)
											   (eq method (infer~find-method 'massertion-m)))
										      't
										    nil)))
									    steps-of-current-roc))))
    (if (null step-with-node-as-premisse-and-massertion-m)
	nil
      (let* ((just (pdsc~an-just step-with-node-as-premisse-and-massertion-m))
	     (theorem (first (pdsj~parameters just))))
	(if (eq theorem (th~find-assumption 'squeeze-theorem (th~find-theory 'limit)))
	    't
	  nil)))))
	     
(defun crihelp=step-in-normalize-task-strategy-p (step)
  (let* ((rocs (store~elements (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))
	 (pplanner-rocs (remove-if-not #'roc~pplanner-state-description-p rocs))
	 (roc-with-step (first (remove-if-not #'(lambda (pplanner-roc)
						  (find step (roc~pplanner-steps pplanner-roc)))
					      pplanner-rocs))))
    (if (null roc-with-step)
	nil
      (let* ((strategy (roc~strategy-ks roc-with-step)))
	(if (eq strategy (strat~find-strategy-ks 'normalizegoal))
	    roc-with-step
	  nil)))))
	 
	
(defun task-goal-is-inequality-p (task)
  (format t "~%CURRENT TASKS: ~A" cri*current-tasks)
  (if (stringp task)
      ;; not bound
      (let* ((current-tasks cri*current-tasks)
	     (inequality-goal-tasks (remove-if-not #'crihelp=task-is-goal-task-and-formula-is-inequality-p current-tasks)))
	(mapcar #'(lambda (intask)
		    (list (cons task intask)))
		inequality-goal-tasks))
    ;; task already bound
    (if (crihelp=task-is-goal-task-and-formula-is-inequality-p task)
	(list (list (cons T T)))
      nil)))

(defun crihelp=task-is-goal-task-and-formula-is-inequality-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((formula (node~formula (agenda~task-node task))))
	(if (data~appl-p formula)
	    (let* ((func (data~appl-function formula))
		   (env (pds~environment omega*current-proof-plan)))
	      (if (find func (list (env~lookup-object 'less env)
				   (env~lookup-object 'leq env)
				   (env~lookup-object 'geq env)
				   (env~lookup-object 'greater env)))
		  't
		nil))
	  nil))
    nil))

(defun theorem-of-thy-p (theorem)
  (let* ((current-th (prob~proof-theory omega*current-proof-plan))
	 (theorems (th~theorems current-th)))
    (mapcar #'(lambda (th)
		(list (cons theorem th)))
	    theorems)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETTINGS                                                           ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf sod*current-strategies '(ExternalAnalogy
			       ;;RepairRestPairs
			       IndirectProof
			       DetFromCS
			       SolveFromCS
			       NormalizeGoal
			       UnwrapHyp
			       AttackInequality
			       backtrack-step-to-task
			       backtrack-Cosie-Step-to-insttask
			       BACKTRACK-LAST-STRATEGY-TO-TASK
			       backtrack-unwraphy-step
			       ;;BackTrack-MV-Inst
			       ))

(setf sod*current-strategic-control-rules '(EX-ANA-INSERT
					    EX-ANA-REJECT-IF-NOT-ROOT
					    PREFER-SIMPLE-ATTACKINEQUALITY-JOBS
					    PREFER-DEMAND-FULFILLING
					    DEFER-OFFERS-FROM-STORE-IF-NOT-SATISFIED-DEMANDS
					    PREFER-OFFERS-FROM-STORE
					    REJECT-ALREADY-APPLIED-STRATEGIES
					    PREFER-BACKTRACK-STEP-IF-NO-METHOD-APPLICABLE-FAILURE
					    PREFER-BACKTRACK-LAST-STRATEGY-IF-NO-METHOD-APPLICABLE-FAILURE-AND-START-TASK
					    PREFER-INDIRECTPROOF-BEFORE-NORMALIZEGOAL
					    PREFER-INDIRECTPROOF-BEFORE-ATTACKINEQUALITY
					    REJECT-INDIRECTPROOF-IF-NOT-ROOT-TASK
					    PREFER-STRATEGY-BACKTRACK-UNWRAPHYP
					    ;;PREFER-RepairRestPairs
					    ;;PREFER-BACKTRACK-INST-IF-MV-NOT-LONGER-DET
					    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; A strategy for interactive planning (replaces attackinequality)    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(strat~define-strategy-ks
 (name lim-inter)
 (refinement-algorithm PPlanner)
 (condition (lambda (&rest x) t))  ;;always
 (methods (Solve*<-m-b Solve*<-<=-m-b Solve*<=-<-m-b Solve*>-m-b Solve*>->=-m-b Solve*>=->=-m-b
	   MComplexEstimate<-m-b MComplexEstimate<=-m-b MComplexEstimate>-m-b MComplexEstimate>=-m-b
	   MComplexEstimate>>-m-b FactorialEstimate-m-b
	   SimplifyInequality-m-b SimplifyInequality-m-f Simplify-m-b Simplify-m-f 
	   ;; DomainCaseSplit-m-b
	   ;; EnvI<-m-b EnvI>-m-b
	   EnvE<-m-f
	   MSET-FOCUS-M-B 
	   
	   ;; RESTRICTING-METHODS
	   TRUEI-M-B ANDI-M-B AskCS-m-b TellCS-m-b WEAKEN-M-A mflip-absval-inequality-m-b reflexdirect-m-b
	   
	   ;; NORMALIZING-METHODS
	   PushNeg-m-f ANDE-m-F TellCS-m-f mflip-absval-inequality-m-f 
	   
	   ))
 (normalization-methods (PushNeg-m-f ANDE-m-F mflip-absval-inequality-m-f
			 TellCS-m-f
			 ;; TellCS-m-f muss ganz nach hinten, da dort die Normalisierung abbricht (keine neue Zeile)
			 ))                       
 (restriction-methods (TRUEI-M-B ANDI-M-B AskCS-m-b
		       TellCS-m-b WEAKEN-M-A mflip-absval-inequality-m-b reflexdirect-m-b
		       ))
 (control-rules (;;MYSelect-DomainCaseSplit
		 ;;MYSelect-EnvE<-m-f
		 ;RejectNotCosieTasks
		 MYAttack-inequality<-non-standard-select
		 MYSelect-EnvE<-m-f
		 MYOrder-Env
		 Interrupt-If-Focus
		 INTERRUPT-IF-INST
		 MYreject-myset-focus-mmatchings-if-nothing-new
		 ))
 (termination-check (lambda (&rest x) nil)) ;never
 (loop-detection 15)
 (selection waterfall)
 (print "Strategy-KS Interactive Limit Planning: Offer to tackle inequality-task ~A"))


