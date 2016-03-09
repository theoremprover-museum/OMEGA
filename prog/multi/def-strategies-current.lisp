(in-package :omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategies for Exp       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pseudo-goal-p (task)
  (agenda~pseudo-goal-p task))

(strat~define-strategy-ks
 (name ExpandUnreliableMethod)
 (refinement-algorithm Exp)
 (condition pseudo-goal-p)
 (print "Strategy-KS ExpandUnreliableMethod: Offer to expand the unreliable Method application in ~A"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategies for InstMeta! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; InstAnswerConstraint

(defun answer-constraint-meta-p (task)
  (declare (edited  "21-JUN-1999")
	   (authors Ameier)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "T if the task is an instantiation task for the meta-variable of the"
		    "answer constraint, nil otherwise."))
  (if (agenda~inst-task-p task)
      (let* ((meta-var (agenda~inst-task-meta-var task))
	     (plan-step (agenda~inst-task-plan-step task))
	     (plan-step-just (pdsc~an-just plan-step))
	     (plan-step-just-method (just~method plan-step-just))
	     (plan-step-just-method-name (keim~name plan-step-just-method)))
	
	(if (stringp plan-step-just-method-name)
	    (if (or (string= plan-step-just-method-name
			     "Initialize-CS-m")
		    (string= plan-step-just-method-name
			     "MYInitialize-CS-m"))
		't
	      nil)
	  (if (or (equal plan-step-just-method-name
			 'Initialize-CS-m)
		  (equal plan-step-just-method-name
			 'MYInitialize-CS-m))
	      't
	    nil)))
    nil))

(defun compute-current-answer-constraint (meta-var)
  (declare (edited  "21-JUN-1999")
	   (authors Ameier)
	   (input   "The meta-variable of the answer-cosntraint.")
	   (effect  "None.")
	   (value   "The current value of this meta-var."))
  ;; (let* ((cs-subst (cosi~reflect-all-cs)))
  ;;
  ;;   (subst~apply cs-subst meta-var)))

  (let* ((met-inst-pair (CoSIE~reflect)))
    (cdr met-inst-pair)))


(strat~define-strategy-ks
 (name InstAnswerConstraint)
 (refinement-algorithm InstMeta)
 (condition answer-constraint-meta-p)
 (compute-instantiation-function compute-current-answer-constraint)
 (print "Strategy-KS InstAnswerConstraint: Offer to instantiate meta-variable ~A"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; InstFromCS


(defun not-answer-constraint-meta-p (task)
  (and (agenda~inst-task-p task)
       (null (answer-constraint-meta-p task))))

#|(defun compute-from-answer-constraint (meta-var)
  (let* ((answer-constraint-formula (cdr (CoSIE~reflect)))
	 (conjunct-list (decompose-conjuncts answer-constraint-formula))
	 (eq-conj-with-meta-var (progn (format t "~%CONJUNCT-LIST: ~A" conjunct-list)
				       (find meta-var conjunct-list
					     :test #'(lambda (mv conj)
						       (and (data~appl-p conj)
							    (progn
							      (format t "~%AN APPL ISSES: ~A" conj)
							      (data~equal (data~appl-function conj) (data~schema-range
												     (env~lookup-object '= (th~env 'base)))))
							    (progn
							      (format t "~%ET BEGINNT ACH MIT =: ~A" conj)
							      (data~equal mv (first (data~appl-arguments conj))))))))))
    
    (if eq-conj-with-meta-var
	(second (data~appl-arguments eq-conj-with-meta-var))
      (omega~error "~%Something wrong in function compute-from-answer-constraint."))))|#

(defun compute-from-answer-constraint (meta-var)
  (let* ((detvars (cosie~getdetvars))
	 (mv-pair (find meta-var detvars :test #'(lambda (mvar pair)
						   (keim~equal mvar (first pair))))))

    (if mv-pair
	(cdr mv-pair)
      (omega~error "~%Something wrong in function compute-from-answer-constraint."))))

(defun decompose-conjuncts (formula)
  (if (and (data~appl-p formula)
	   (data~equal (data~appl-function formula) (env~lookup-object 'and (th~env 'base))))
      (append (decompose-conjuncts (first (data~appl-arguments formula)))
	      (decompose-conjuncts (second (data~appl-arguments formula))))
    (list formula)))

(strat~define-strategy-ks
 (name InstFromCS)
 (refinement-algorithm InstMeta)
 (condition not-answer-constraint-meta-p)
 (compute-instantiation-function compute-from-answer-constraint)
 (print "Strategy-KS InstFromCS: Offer to instantiate meta-variable ~A from answer constraint"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; InstFromParam

(defun compute-instantiation-from-parameters (meta-var)
  (let* ((term (first (roc~parameters instmeta*roc-state-description))))
    term))

(defun instantiation-task-p (task)
  (if (not-answer-constraint-meta-p task)
      t
    nil))

(strat~define-strategy-ks
 (name InstFromParam)
 (refinement-algorithm InstMeta)
 (condition instantiation-task-p)
 (compute-instantiation-function compute-instantiation-from-parameters)
 (parameter-types (term))
 (print "Strategy-KS InstFromParam: Offer to instantiate meta-variable ~A from parameters"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; InstInteractively
;;
;;
;;(defun inst-p (task)
;;  (declare (edited  "12-AUG-1999")
;;	   (authors Ameier)
;;	   (input   "A task.")
;;	   (effect  "None.")
;;	   (value   "T if the task is a instantiation task."))
;;  (agenda~inst-task-p task))
;;
;;(defun compute-instantiation-interactively (meta-var)
;;  (declare (edited  "12-AUG-1999")
;;	   (authors Ameier)
;;	   (input   "A meta-variable.")
;;	   (effect  "None.")
;;	   (value   "Calls the user to give an instantiation for this meta-variable."))
;; (first (inac~interactive-questions (list 'instantiation)
;;				     (list (format nil "Please insert a term to instantiate meta-variable ~A" meta-var))
;;				     (list 'term)
;;				     (list nil))))
;;
;;(strat~define-strategy-ks
;; (name InstInteractively)
;; (refinement-algorithm InstMeta)
;; (condition inst-p)
;; (compute-instantiation-function compute-instantiation-interactively)
;; (print "Strategy-KS InstInteractively: Offer to instantiate meta-variable ~A interactively"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategies for PPlanner! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------> NORMALIZE-GOAL

(defun no-non-literal-open-goals-p ()
  ;; Agenda is in pds on sod*solution-blackboard
  ;; Current ROC is in pplan*roc-state-description

  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (all-tasks (agenda~all-tasks agenda))
	 (goal-tasks-with-roc (remove-if-not #'(lambda (task)
						 (and (agenda~goal-or-goal-schema-task-p task)
						      (find pplan*roc-state-description (agenda~task-rocs task))))
					     all-tasks))
	 (tasks-with-roc-and-with-goals-with-non-literal-formulas
	  (remove-if-not #'(lambda (task)
			     (let* ((node (agenda~task-node task))
				    (formula (node~formula node)))
			       (if (or (logic~atom-p formula)
				       (and (logic~negation-p formula)
					    (logic~atom-p (first (data~appl-arguments formula)))))
				   nil
				 't)))
			 goal-tasks-with-roc)))
    
    (if tasks-with-roc-and-with-goals-with-non-literal-formulas
	nil
      (if goal-tasks-with-roc
	  't
	't))))

(defun non-literal-goal-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      
      (let* ((node (agenda~task-node task))
	     (formula (node~formula node)))
	(if (or (logic~atom-p formula)
		(and (logic~negation-p formula)
		     (logic~atom-p (first (data~appl-arguments formula)))))
	    nil
	  't))
    nil))
      
(strat~define-strategy-ks
 (name NormalizeGoal)
 (refinement-algorithm PPlanner)
 (condition non-literal-goal-p)
 (methods (ANDI-M-B IMPI-M-B FORALLI-M-B EXISTSI-META-M-B EquivI-m-b OrIL-m-b OrIR-m-b))
 (normalization-methods nil)
 (restriction-methods nil)
 (control-rules (MYATTACK-NON-ATOM-GOAL
		 ))
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-non-literal-open-goals-p)
 (print "Strategy-KS NormalizeGoal: Offer to decompose the task ~A"))

;; --------------------------------------------------------> UnwrapHyp

(defun demand-for-unwraphyp-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((demands-store (black~get-blackboard-object-content 'demands sod*control-blackboard))
	     (demands-for-unwraphyp-and-task (store~elements-with-test demands-store
								       :test #'(lambda (demand)
										 (if (and (demand~strategy-task-demand-p demand)
											  (eq (demand~strategy-task-demand-strategy-ks demand)
											      (strat~find-strategy-ks 'unwraphyp))
											  (eq (demand~strategy-task-demand-task demand)
											      task))
										     't
										   nil)))))
	(if demands-for-unwraphyp-and-task
	    't
	  nil))
    nil))

(defun no-focus-p ()
  (let* ((focus (black~get-blackboard-object-content 'focus sod*solution-blackboard)))
    (if (null focus)
	(let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	       (agenda (pds~agenda pds))
	       (all-tasks (agenda~all-tasks agenda))
	       (goal-tasks-with-roc (remove-if-not #'(lambda (task)
						       (and (agenda~goal-or-goal-schema-task-p task)
							    (find pplan*roc-state-description (agenda~task-rocs task))))
						   all-tasks)))
	  (if goal-tasks-with-roc
	      't
	    't))
      nil)))

(strat~define-strategy-ks
 (name UnwrapHyp)
 (refinement-algorithm PPlanner)
 (condition demand-for-unwraphyp-p)
 (methods (ANDE-M-F IMPE-OPEN-M-A FORALLE-META-M-F EXISTSE-M-A))
 (normalization-methods nil)
 (restriction-methods nil)
 (control-rules (MYattack-focus))
 (loop-detection nil)
 (randomization-rules nil)
 (termination-check no-focus-p)
 (print "Strategy-KS UnwrapHyp: Offer to decompose assumption ~A"))

;; ------------------------------------------------------------> AttackInequality

(defun inequality-goal-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((node (agenda~task-node task))
	     (formula (node~formula node)))
	(if (data~appl-p formula)
	    (if (or (keim~equal (data~appl-function formula)
				(env~lookup-object 'less (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function formula)
				(env~lookup-object 'leq (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function formula)
				(env~lookup-object 'greater (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function formula)
				(env~lookup-object 'geq (pds~environment omega*current-proof-plan)))
		    (and (logic~negation-p formula)
			 (data~appl-p (first (data~appl-arguments formula)))
			 (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				     (data~schema-range
				      (env~lookup-object '= (pds~environment omega*current-proof-plan))))))
		't
	      nil)
	  nil))
    nil))

(defun no-inequality-goals-p ()
  ;; Agenda is in pds on sod*solution-blackboard
  ;; Current ROC is in pplan*roc-state-description

  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (all-tasks (agenda~all-tasks agenda))
	 (tasks-with-roc-and-with-goals-with-inequality-formulas
	  (remove-if-not #'(lambda (task)
			     (if (agenda~goal-or-goal-schema-task-p task)
				 (let* ((node (agenda~task-node task))
					(formula (node~formula node))
					(inequality-p (if (data~appl-p formula)
							  (if (or (keim~equal (data~appl-function formula)
									      (env~lookup-object 'less (pds~environment omega*current-proof-plan)))
								  (keim~equal (data~appl-function formula)
									      (env~lookup-object 'leq (pds~environment omega*current-proof-plan)))
								  (keim~equal (data~appl-function formula)
									      (env~lookup-object 'greater (pds~environment omega*current-proof-plan)))
								  (keim~equal (data~appl-function formula)
									      (env~lookup-object 'geq (pds~environment omega*current-proof-plan)))
								  (and (logic~negation-p formula)
								       (data~appl-p (first (data~appl-arguments formula)))
								       (keim~equal (data~appl-function (first (data~appl-arguments formula)))
										   (data~schema-range (env~lookup-object '= (pds~environment omega*current-proof-plan))))))
							      't
							    nil)
							nil)))
				   
				   (and (agenda~goal-or-goal-schema-task-p task)
					(find pplan*roc-state-description (agenda~task-rocs task))
					inequality-p))
			       nil))
			 all-tasks)))
    
    (if tasks-with-roc-and-with-goals-with-inequality-formulas
	nil
      't)))

(strat~define-strategy-ks
 (name AttackInequality)
 (refinement-algorithm PPlanner)
 (condition inequality-goal-p)
 (methods (Solve*<-m-b Solve*<-<=-m-b Solve*<=-<-m-b Solve*>-m-b Solve*>->=-m-b Solve*>=->=-m-b
	   MYComplexEstimate<-m-b MYComplexEstimate>-m-b MYFactorialEstimate-m-b MYComplexEstimate>>-m-b
	   SimplifyInequality-m-b  SimplifyInequality-m-f Simplify-m-b Simplify-m-f 
	   ;; EnvI<-m-b EnvE<-m-f EnvI>-m-b
	   MYSET-FOCUS-M-B 
	   
	   ;; RESTRICTING-METHODS
	   TRUEI-M-B ANDI-M-B AskCS-m-b TellCS-m-b WEAKEN-M-A myflip-absval-inequality-m-b
	   
	   ;; NORMALIZING-METHODS
	   PushNeg-m-f ANDE-m-F TellCS-m-f myflip-absval-inequality-m-f
	   
	   ;; The myflip-... stuff is nessessary, since complexestimate<,> can only take absvals on the left side.
	   ;; The myflip-... stuff always skips Absvals to the left!
	   
	   ;; I DON'T KNOW EXACTLY WHERE YOU NEED: 
	   ;; MYSolve*-leq-m-b,  MYSimplifyInequality-m-b (restrict),
	   ;; MYSimplifyInequality-m-f (normalize),  ExFalsoQuodlibet-m-b
	   ;; MYSimplify-m-f (normalize), MYSimplify-m-b (retrict), MYSimplify-m-f (normalize)
	   ))
 (normalization-methods (PushNeg-m-f ANDE-m-F myflip-absval-inequality-m-f TellCS-m-f
				     ;; TellCS-m-f has to be at the end, since the normalization terminates there (no new line)
				     ))                       
 (restriction-methods (TRUEI-M-B ANDI-M-B AskCS-m-b TellCS-m-b WEAKEN-M-A myflip-absval-inequality-m-b
				 ))
 (control-rules (MYAttack-inequality<-non-standard-select
		 MYAttack-not=-non-standard-select
		 ;;MYselect-domain-split
		 Interrupt-If-Focus
		 INTERRUPT-IF-INST
		 MYreject-myset-focus-mmatchings-if-nothing-new
		 ))
 (termination-check no-inequality-goals-p)
 (loop-detection 15)
 (randomization-rules nil)
 (print "Strategy-KS AttackInequality: Offer to tackle inequality-task ~A"))


;; ------------------------------------------------------------> InitializeConstraintSolver

(defun goal-p (task)
  (declare (edited  "12-AUG-1999")
	   (authors Ameier)
	   (input   "A task.")
	   (effect  "None.")
	   (value   "T if the task is a goal task."))
  (agenda~goal-or-goal-schema-task-p task))

(defun cs-already-initialized-p ()
  (let* ((all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	 (all-inst-tasks (remove-if-not #'agenda~inst-task-p all-tasks))
	 (inst-tasks-with-just-inst (remove-if-not #'(lambda (inst-task)
						       (let* ((plan-step (agenda~inst-task-plan-step inst-task))
							      (plan-step-just (pdsc~an-just plan-step))
							      (plan-step-just-method (just~method plan-step-just))
							      (plan-step-just-method-name (keim~name plan-step-just-method)))
							 (if (stringp plan-step-just-method-name)
							     (if (or (string= plan-step-just-method-name
									      "MYInitialize-CS-m")
								     (string= plan-step-just-method-name
									      "Initialize-CS-m"))
								 't
							       nil)
							   (if (or (equal plan-step-just-method-name
									  'MYInitialize-CS-m)
								   (equal plan-step-just-method-name
									  'Initialize-CS-m))
							       't
							     nil))))
						   all-inst-tasks)))
   
    (if inst-tasks-with-just-inst
	't
      nil)))

(strat~define-strategy-ks
 (name InitializeConstraintSolver)
 (refinement-algorithm PPlanner)
 (condition goal-p)
 (methods (INITIALIZE-CS-M-B))
 (normalization-methods nil)
 (restriction-methods nil)
 (control-rules (MYInitialize-constraint-solver))
 (termination-check cs-already-initialized-p)
 (loop-detection nil)
 (randomization-rules nil)
 (print "Strategy-KS InitializeConstraintSolver: Offer to insert all possible supports of task ~A into COSIE"))

;; ------------------------------------------------------------> CloseConstraintSolver

(defun cs-answer-constraint-p (task)
  (let* ((node (agenda~task-node task)))
    (if (or (null (agenda~goal-schema-p task))
	    (null (meta~p (node~formula node))))
	nil
      (let* ((reasons (pdsj~all-other-reasons (node~justification node)))
	     (reasons-just-method-names (mapcar #'(lambda (reason)
						    (keim~name (just~method (pdsc~an-just reason))))
						reasons))
	     (reason-is-initialize (remove-if-not #'(lambda (plan-step-just-method-name)
						      (if (stringp plan-step-just-method-name)
							  (if (or (string= plan-step-just-method-name
									   "MYInitialize-CS-m")
								  (string= plan-step-just-method-name
									   "Initialize-CS-m"))
							      't
							    nil)
							(if (or (equal plan-step-just-method-name
								       'MYInitialize-CS-m)
								(equal plan-step-just-method-name
								       'Initialize-CS-m))
							    't
							  nil)))
						  reasons-just-method-names)))
	
	(if reason-is-initialize
	    't
	  nil)))))

(defun always-nil ()
  nil)

(strat~define-strategy-ks
 (name CloseConstraintSolver)
 (refinement-algorithm PPlanner)
 (condition cs-answer-constraint-p)
 (methods (Prove-cs-answer-m-b))
 (normalization-methods nil)
 (restriction-methods nil)
 (control-rules )
 (termination-check always-nil)		 
 (loop-detection nil)
 (randomization-rules nil)
 (print "Strategy-KS CloseConstraintSolver: Offer to prove the answer constraint in task ~A of Cosie"))

#|;; -----------------------------------------------------------------> ApplySubstAndEquations

(defun node-task-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      't
    nil))


(defun input-task-closed-p ()
  ;; Agenda is in pds on sod*solution-blackboard
  ;; Current ROC is in pplan*roc-state-description

  (let* ((start-task (roc~start-task pplan*roc-state-description))
	 (node (agenda~task-node start-task)))
    (if (pdsn~open-node-p node)
	nil
      't)))
    
(strat~define-strategy-ks
 (name ApplySubstAndEquations)
 (refinement-algorithm PPlanner)
 (condition node-task-p)
 (methods (MYApplySubstAndEquationsOnTask-m-b
	   MYApplySubstAndEquationsOnSupport-m-b
	   ;; Restricting-methods
	   AskCS-m-b TellCS-m-b))
 (normalization-methods nil)
 (restriction-methods (AskCS-m-b TellCS-m-b))
 (control-rules (MYApplySAEOnTask
		 MYApplySAEOnSupport))
 (termination-check input-task-closed-p)
 (loop-detection nil)
 (parameter-types (substitution ndline term-list position-list))
 (print "Strategy-KS ApplySubstAndEquations: Offer to change task ~A by applying a substitution and a list of equations on it"))
|#


;; -----------------------------------------------------------------> ApplyAndSolveEquations

(defun node-task-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      't
    nil))

(defun only-subst=-task-p ()
  (let* ((start-task (roc~start-task pplan*roc-state-description))
	 (first-just-prem (first (just~premises (node~justification (agenda~task-node start-task))))) 
	 (all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	 (all-tasks-with-current-roc (remove-if-not #'(lambda (task)
							(find pplan*roc-state-description (agenda~task-rocs task)))
						    all-tasks)))

    (if (and (= (length all-tasks-with-current-roc) 1)
	     (keim~equal first-just-prem (agenda~task-node (first all-tasks-with-current-roc))))
	't
      nil)))


(strat~define-strategy-ks
 (name ApplyAndSolveEquations)
 (refinement-algorithm PPlanner)
 (condition node-task-p)
 (methods (MYSubst=param-m-b
	   ;; Restricting-methods
	   AskCS-m-b TellCS-m-b Elementary-m-b MYDecomp-Function-m-b)
	  )
 (normalization-methods nil)
 (restriction-methods (AskCS-m-b TellCS-m-b Elementary-m-b MYDecomp-Function-m-b))
 (control-rules (MYApplyfirstMYSubst=param
		 ))
 (termination-check only-subst=-task-p)
 (loop-detection nil)
 (randomization-rules nil)
 (parameter-types (ndline term position))
 (print "Strategy-KS ApplyAndSolveEquations: Offer to change task ~A by applying a Equation and solving these equation."))


;; ----------------------------------------------------------------> Apply Theorems

(defun limit-goal-p (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      
      (let* ((node (agenda~task-node task))
	     (formula (node~formula node))
	     (substructs (data~all-substructs formula))
	     (lim (env~lookup-object 'lim (pds~environment omega*current-proof-plan))))

	(if (find lim substructs :test #'keim~equal)
	    't
	  nil))))


(defun no-limit-goals-p ()
  ;; Agenda is in pds on sod*solution-blackboard
  ;; Current ROC is in pplan*roc-state-description
  
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (agenda (pds~agenda pds))
	 (all-tasks (agenda~all-tasks agenda))
	 (goal-tasks-with-roc (remove-if-not #'(lambda (task)
						 (if (and (agenda~goal-or-goal-schema-task-p task)
							  (find pplan*roc-state-description (agenda~task-rocs task)))
						     't
						   nil))
					     all-tasks))
	 (goal-tasks-with-roc-and-limit (remove-if-not #'(lambda (task)
							   (let* ((formula (node~formula (agenda~task-node task)))
								  (substructs (data~all-substructs formula))
								  (lim (env~lookup-object 'lim (pds~environment omega*current-proof-plan))))
							     (if (find lim substructs :test #'keim~equal)
								 't
							       nil)))
						       goal-tasks-with-roc)))
    
    (if goal-tasks-with-roc-and-limit
	nil
      't)))


(strat~define-strategy-ks
 (name limittheoremapply)
 (refinement-algorithm PPlanner)
 (condition limit-goal-p)
 (methods (MYAssertion-m-b
	   
	   ;; restricting
	   WEAKEN-M-A ANDI-M-B AskCS-m-b TellCS-m-b ;; MYNat-m-b MYPos-Nat-m-b
	   )
	  )
 (normalization-methods nil)
 (restriction-methods (WEAKEN-M-A ANDI-M-B AskCS-m-b TellCS-m-b))
 (control-rules (APPLY-THEOREM
		 INTERRUPT-IF-INST))
 (termination-check no-limit-goals-p)
 (loop-detection nil)
 (randomization-rules nil)
 (print "Strategy-KS limittheoremapply: Offer to tackle inequality-task ~A"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategies for BackTrack! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ----------------------------------------------------------------> backtrack-step-to-task

(strat~define-strategy-ks
 (name backtrack-step-to-task)
 (refinement-algorithm backtrack)
 (condition backtrack-task-p)
 (compute-backtrack-steps-function steps-to-task)
 (print "Strategy-KS BackTrack-Step-to-Task: Offer to backtrack the step which creates task ~A"))

(defun backtrack-task-p (task)
  (if (and (agenda~goal-or-goal-schema-task-p task)
	   (null (eq (prob~proof-root omega*current-proof-plan) (agenda~task-node task))))
      't
    nil))

(defun steps-to-task (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      ;; Retract reasons to task
      (let* ((node (agenda~task-node task))
	     (just (node~justification node))
	     (reasons (pdsj~reasons just)))
	(values
	 (reverse reasons)
	 nil))
    (values
     nil
     nil)))


;; -------------------------------------------------------------> backtrack-strategy-to-task

(strat~define-strategy-ks
 (name backtrack-last-strategy-to-task)
 (refinement-algorithm backtrack)
 (condition strategy-backtrack-task-p)
 (compute-backtrack-steps-function last-strategy-to-task)
 (print "Strategy-KS BackTrack-Last-Strategy-to-Task: Offer to backtrack the last strategy applied on task ~A"))

(defun strategy-backtrack-task-p (task)
  (if (and (agenda~goal-or-goal-schema-task-p task)
	   (agenda~task-rocs task))
      ;; Task has ROCS entries
      't
    nil))

(defun last-strategy-to-task (task)
  (if (agenda~goal-or-goal-schema-task-p task)
      (let* ((task-rocs (rest (agenda~task-rocs task))) ;; The first one is the new backtrack ROC 
	     (all-rocs (store~elements (black~get-blackboard-object-content 'rocs-list sod*solution-blackboard)))
	     (last-roc (find task-rocs (reverse all-rocs) :test #'(lambda (rocs-list roc)
								    (find roc rocs-list)))))
	
	(values (list (roc~start-step last-roc))
		nil))
    (values nil nil)))
