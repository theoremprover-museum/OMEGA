
(in-package :omega)


(defun limag=theory-lookup (sym)
  (env~lookup-object sym (th~env 'limit)))

(defvar limag*relations (mapcar #'limag=theory-lookup
  (list
  natac*greater 
  natac*geq 
  natac*less
  natac*leq 
  natac*=)))

(defvar limag*arith (mapcar #'limag=theory-lookup
  (list
   natac*plus
   natac*minus
   natac*times
   natac*div 
   natac*power)))


(defun limag=junktor? (obj)
  (and (data~appl-p obj)
       (logic~connective-p (data~appl-function obj))
       (not (logic~universal-quantification-p obj))
       (not (logic~existential-quantification-p obj))))


(defun limag=absval? (obj)
  (let ((absval (env~lookup-object 'absval (th~env 'limit))))
    (some #'(lambda (sub) (data~equal absval sub)) (data~all-substructs obj))))


(defun limag=relation? (obj)
  (when (data~appl-p obj)
    (some #'(lambda (rel) (data~schema-equal (data~appl-function obj) rel)) limag*relations)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand true-is-true
                (argnames goal)
                (argtypes ndline)
                (arghelps "An with Truth")
                (function limag=com-truei-m-b)
                (defaults )
                (frag-cats tactics limit )
                (log-p t))
                

(defun limag=com-truei-m-b (goal)
  (pplan~execute-method 'TrueI-m-b  goal nil nil))

(agent~defmatrix true-is-true
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (logic~truth-constant-p goal)))))
						
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand a=a
                (argnames goal)
                (argtypes ndline)
                (arghelps "An equalitiy with equal term on both sides")
                (function limag=com-reflexdirect-m-b)
                (defaults )
                (frag-cats tactics limit )
                (log-p t))
                

(defun limag=com-reflexdirect-m-b (goal)
  (pplan~execute-method 'reflexdirect-m-b goal nil nil))

(agent~defmatrix a=a
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (and (logic~equality-p goal)
						(apply #'data~equal (data~appl-arguments goal)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand ForallI-m-b
                (argnames goal)
                (argtypes ndline)
                (arghelps "An open line with an universal quantification")
                (function limag=com-ForallI-m-b)
                (defaults )
                (frag-cats tactics limit )
                (log-p t)
                (help "Applies the method ForallI-m-b"))
                

(defun limag=com-ForallI-m-b (goal)
  (pplan~execute-method 'ForallI-m-b goal nil nil))

(agent~defmatrix ForallI-m-b
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (logic~universal-quantification-p goal)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand existsi-meta-m-b
                (argnames goal)
                (argtypes ndline)
                (arghelps "An open line with an existential quantification")
                (function limag=com-existsi-meta-m-b)
                (defaults )
                (frag-cats   tactics limit )
                (log-p t)
                (help "Applies the method existsi-meta-m-b"))
                

(defun limag=com-existsi-meta-m-b (goal)
   (pplan~execute-method 'existsi-meta-m-b goal nil nil))

(agent~defmatrix existsi-meta-m-b
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (logic~existential-quantification-p goal)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand foralle-meta-m-f
		(argnames prem)
		(argtypes ndline)
		(arghelps "An assumption line with an universal quantification")
		(function limag=com-foralle-meta-m-f)
		(defaults )
		(frag-cats   tactics limit )
		(log-p t)
		(help "Applies the method foralle-meta-m-b"))
		

(defun limag=com-foralle-meta-m-f (prem)
   (pplan~execute-method 'foralle-meta-m-f  (foci~focus-line (foci~active-pc))
			(list prem)
			(list (meth=defn-newmetavar 'dummy
						    (term~type (logic~quantification-bound-variable (node~formula prem))))))) ;;just a dummy

(agent~defmatrix foralle-meta-m-f
		 (agents
		  (s-predicate (for prem)
			       (uses )
			       (level 1)
			       (definition (pred1 prem))))
		 (predicates
		  (pred1 (prem)
			 (logic~universal-quantification-p prem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand existse-m-a
		(argnames prem)
		(argtypes ndline)
		(arghelps "An assumption line with an existential quantification")
		(function limag=com-existse-m-a)
		(defaults )
		(frag-cats   tactics limit )
		(log-p t)
		(help "Applies the method existse-m-a"))
		

(defun limag=com-existse-m-a (prem)
 (pplan~execute-method 'existse-m-a (foci~focus-line (foci~active-pc)) (list prem) nil))

(agent~defmatrix existse-m-a
		 (agents
		  (s-predicate (for prem)
			       (uses )
			       (level 1)
			       (definition (pred1 prem))))
		 (predicates
		  (pred1 (prem)
			 (logic~existential-quantification-p prem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand skolemize-s-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An goal with a quantification")
		(function limag=com-skolemize-s-b)
		(defaults )
		(frag-cats   tactics limit )
		(log-p t)
		(help "Applies the method skolemize-s-b"))
		

(defun limag=com-skolemize-s-b (goal)
  (pplan~execute-method 'skolemize-s-b goal nil nil))

(agent~defmatrix skolemize-s-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (or (logic~universal-quantification-p goal)
					       (logic~existential-quantification-p goal))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand unwraphyp
		(argnames prem pos)
		(argtypes ndline position)
		(arghelps "A premise" "The position of the formula to unwrap")
		(function limag=com-unwraphyp-strat)
		(defaults )
		(frag-cats   tactics limit )
		(log-p t)
		(help "Applies the strategy unwraphyp"))
		

(defun limag=com-unwraphyp-strat (prem pos)
   (pplan~execute-strategy 'unwraphyp (foci~focus-line (foci~active-pc)) (list prem pos)))

(agent~defmatrix unwraphyp
		 (agents
		  (s-predicate (for prem)
			       (uses)
			       (level 1)
			       (definition (pred1 prem)))
		  (function    (for pos)
			       (uses prem)
			       (level 1)
			       (definition (pred2 (:node prem)))))
		 (predicates
		  (pred1 (prem)
			 (and (limag=relation? (node~formula (foci~focus-line (foci~active-pc))))
			      (or (logic~universal-quantification-p prem)
				  (logic~existential-quantification-p prem))))
		  (pred2 (prem)
			 (second
			  (crihelp=MULTI-get-most-similar-subterm-of-ass
			   (cri=analyze (node~formula (foci~focus-line (foci~active-pc))) nil)
			   prem)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand split-into-inequalities ;normal-s-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An goal with a formula")
		(function limag=com-normal-s-b)
		(defaults )
		(frag-cats   tactics limit )
		(log-p t)
		(help "Applies the method normal-s-b"))
		

(defun limag=com-normal-s-b (goal)
   (pplan~execute-method 'normal-s-b goal nil nil))

(agent~defmatrix split-into-inequalities ;normal-s-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
 			       (definition (and (limag=junktor? goal)
						(not (logic~equality-p goal)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand tellcs-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An goal with a simple inequality")
		(function limag=com-tellcs-m-b)
		(defaults )
		(frag-cats   tactics limit )
		(log-p t)
		(help "Applies the method tellcs-m-b"))
		

(defun limag=com-tellcs-m-b (goal)
   (pplan~execute-method 'tellcs-m-b  goal nil nil))

(agent~defmatrix tellcs-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 goal))))
		 (predicates
		  (pred1 (goal)
			  (and (limag=relation? goal)
			       (some #'term~variable-p (data~all-substructs goal))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand askcs-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An goal with a simple inequality")
		(function limag=com-askcs-m-b)
		(defaults )
		(frag-cats   tactics limit )
		(log-p t)
		(help "Applies the method askcs-m-b"))
		

(defun limag=com-askcs-m-b (goal)
 (pplan~execute-method 'askcs-m-b goal nil nil))

(agent~defmatrix askcs-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 goal))))
		 (predicates
		  (pred1 (goal)
			  (and (limag=relation? goal)
			       (not (some #'term~variable-p (data~all-substructs goal)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(MP's idea) We could hide more than one method in a command, when the function that applies the method
;;chooses the right method, eg. CE for the cases < and <= ... Don't know if this is useful.
(com~defcommand MComplexEstimate<-m-b
                (argnames goal prem)
                (argtypes ndline ndline)
                (arghelps "An goal with a simple inequation"
                          "A premise containing a factor of the inequation")
                (function limag=com-Mcomplexestimate<-m-b)
                (defaults )
                (frag-cats   tactics limit )
                (log-p t)
                (help "Applies the method Mcomplexestimate<-m-b"))


(defun limag=most-similar (goal ass pos) ;;MP:stupid thing here, I have to run the MetaPred
  (let ((last cri*current-pds))
    (unless last (setf cri*current-pds omega*current-proof-plan))
    (let ((result (most-similar-subterm-in-asss-to-goal (list goal ass pos))))
      (unless last (setf cri*current-pds last))
      result)))

(defun limag=com-Mcomplexestimate<-m-b (goal prem)
   (pplan~execute-method 'Mcomplexestimate<-m-b goal (list prem) nil))

(agent~defmatrix Mcomplexestimate<-m-b
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (pred1 goal)))
                  (s-predicate (for prem)
                               (uses goal)
                               (level 1)
                               (definition (pred2 (:node goal) (:node prem)))))
                 (predicates
                  (pred1 (goal)
                           (and (limag=relation? goal)
                               (data~equal (data~appl-function goal) (env~lookup-object 'less (th~env 'limit)))
                               (limag=absval? goal)))
                  (pred2 (goal prem)
			   (and (limag=relation? (node~formula prem))
				(let ((most-similar (mapcan #'(lambda (premlist)
								(mapcan #'(lambda (prempos)
									    (when (string-equal (car prempos) "ass")
									      (list (rest prempos)))) premlist))
							    (limag=most-similar goal "ass" "pos"))))
				  (omega~trace "goal ~A prem ~A similar ~A" goal prem most-similar)
				  (member prem most-similar))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand solve*<-m-b
                (argnames goal prem)
                (argtypes ndline ndline)
                (arghelps "An goal with an inequation"
                          "A premise with an inequation")
                (function limag=com-solve*<-m-b)
                (defaults )
                (frag-cats   tactics limit )
                (log-p t)
                (help "Applies the method solve*<-m-b"))

(defun limag=com-solve*<-m-b (goal prem)
   (pplan~execute-method 'solve*<-m-b goal (list prem) nil))

(agent~defmatrix solve*<-m-b
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (pred1 goal)))
                  (s-predicate (for prem)
                               (uses goal)
                               (level 1)
                               (definition (pred2 goal  prem))))
                 (predicates
                  (pred1 (goal)
                          (and (limag=relation? goal)
                               (data~equal (data~appl-function goal) (env~lookup-object 'less (th~env 'limit)))))
                  (pred2 (goal prem)
			   (and (limag=relation?  prem)
				(data~equal (data~appl-function goal) (env~lookup-object 'less (th~env 'limit)))
				(let ((arg1 (car (data~appl-arguments prem)))
				      (arg2 (car (data~appl-arguments goal))))
				  (and (not (term~variable-p arg1))
				       (not (term~variable-p arg2))
				       (term~alpha-unify arg1 arg2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand simplify-m-b
                (argnames goal)
                (argtypes ndline )
                (arghelps "An goal with an inequation")
                (function limag=com-simplify-m-b)
                (defaults )
                (frag-cats   tactics limit )
                (log-p t)
                (help "Applies the method simplify-m-b"))

(defun limag=com-simplify-m-b (goal)
   (pplan~execute-method 'simplify-m-b goal nil nil))

(agent~defmatrix simplify-m-b
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (limag=relation? goal)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand solve*>-m-b
                (argnames goal prem)
                (argtypes ndline ndline)
                (arghelps "An goal with an inequation"
                          "A premise with an inequation")
                (function limag=com-solve*>-m-b)
                (defaults )
                (frag-cats   tactics limit )
                (log-p t)
                (help "Applies the method solve*>-m-b"))

(defun limag=com-solve*>-m-b (goal prem)
  (pplan~execute-method 'solve*>-m-b goal (list prem) nil))

(agent~defmatrix solve*>-m-b
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (pred1 goal)))
                  (s-predicate (for prem)
                               (uses goal)
                               (level 1)
                               (definition (pred2 goal  prem))))
                 (predicates
                  (pred1 (goal)
                          (and (limag=relation? goal)
                               (data~equal (data~appl-function goal) (env~lookup-object 'greater (th~env 'limit)))))
                  (pred2 (goal prem)
			   (and (limag=relation?  prem)
				(data~equal (data~appl-function goal) (env~lookup-object 'greater (th~env 'limit)))
				(let ((arg1 (car (data~appl-arguments prem)))
				      (arg2 (car (data~appl-arguments goal))))
				  (and (not (term~variable-p arg1))
				       (not (term~variable-p arg2))
				       (term~alpha-unify arg1 arg2)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand remove-quantifiers
                (argnames goal)
                (argtypes ndline)
                (arghelps "A goal with quantified formula.")
                (function limag=remove-quantifiers)
                (defaults )
                (frag-cats   tactics limit )
                (log-p t)
                (help "Applies the method solve*>-m-b"))

(defun limag=remove-quantifiers (goal)
  (let* ((supps (pds~node-supports goal))
	 (ex (remove-if-not #'logic~existential-quantification-p supps :key #'node~formula))
	 (all (remove-if-not #'logic~universal-quantification-p supps :key #'node~formula)))
    (cond (all
	   (dolist (alli all)
	     (pplan~execute-method
	      'foralle-meta-m-f
	      goal (list alli)
	      (list (meth=defn-newmetavar 'dummy
					  (term~type
					   (logic~quantification-bound-variable (node~formula alli)))))))
	   (limag=remove-quantifiers goal))
	  (ex
	   (limag=remove-quantifiers (agenda~task-node
				      (agenda~first-task (pplan~execute-method 'existse-m-a goal (list (car ex)) nil)))))
	  (T (pplan~execute-method 'skolemize-s-b goal nil nil )))))

	  
(agent~defmatrix  remove-quantifiers
                 (agents
                  (c-predicate (for goal)
                               (uses )
                               (level 1)
                               (definition (pred1 goal))))
                 (predicates
                  (pred1 (goal)
                          (or (logic~universal-quantification-p goal)
                              (logic~existential-quantification-p goal)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand  NormalizeGoal
		(argnames goal)
		(argtypes ndline)
		(arghelps "An goal with a quantification")
		(function limag=com-NormalizeGoal-strat)
		(defaults )
		(frag-cats   tactics limit )
		(log-p t)
		(help "Applies the method skolemize-s-b"))
		

(defun limag=com-NormalizeGoal-strat (goal)
   (pplan~execute-strategy 'NormalizeGoal goal nil))


(agent~defmatrix NormalizeGoal
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (or (logic~universal-quantification-p goal)
					       (logic~existential-quantification-p goal)
					       (limag=junktor? goal))))))
;;;;;;;;;;;;;;;;;;

#|

execute-command
(PLANNING iMULTI
	  (lim-inter)
	  ((lim-inter normalizegoal unwraphyp
	    INSTFROMPARAM BACKTRACK-STEP-TO-TASK DETFROMCS SOLVEFROMCS
	    BACKTRACK-CPOOL-STEP-AFTER-TASK BACKTRACK-LAST-STRATEGY-TO-TASK BACKTRACK-STEP-TO-TASK
	    BackTrack-MV-Inst BackTrack-To-Open InstInteractively))
	  ((PREFER-DEMAND-FULFILLING PREFER-OFFERS-FROM-STORE REJECT-ALREADY-APPLIED-STRATEGIES PREFER-BACKTRACK-STEP-IF-NO-METHOD-APPLICABLE-FAILURE PREFER-BACKTRACK-LAST-STRATEGY-IF-NO-METHOD-APPLICABLE-FAILURE-AND-START-TASK PREFER-INDIRECTPROOF-BEFORE-NORMALIZEGOAL PREFER-INDIRECTPROOF-BEFORE-ATTACKINEQUALITY REJECT-INDIRECTPROOF-IF-NOT-ROOT-TASk))
	  ((normalizegoal unwraphyp DETFROMCS SOLVEFROMCS)))

|#




