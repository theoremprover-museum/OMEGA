(in-package :omega)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suggestion CRULES       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(cri~def-control-rule BACKTRACK-STEP-WHEN-NO-METHOD-APPLICABLE
		      (kind suggestion)
		      (if (backtrack-demand-reaction-for-report-is-no-method-applicable-p "demandreaction"))
		      (then
		       (choose ("demandreaction"))))

(cri~def-control-rule PREFER-SUBST=-FOR-BACKTRACK
		      (kind suggestion)
		      (if (subst-demand-reaction-for-report-no-method-applicable-p "demandreaction"))
		      (then
		       (choose ("demandreaction"))))

(cri~def-control-rule BACKTRACK-STRAT-WHEN-OPEN-CONSTRAINTS
		      (kind suggestion)
		      (if (backtrack-demand-reaction-on-ApplyASE-results-in-open-constraints-p "demandreaction"))
		      (then
		       (choose ("demandreaction"))))






(defun backtrack-demand-reaction-on-ApplyASE-results-in-open-constraints-p (demrec)
  ;; an argument, that has to be instantiated with suited backtrack-demand-reaction

  (if (null suggr*current-execution-message)
      
      nil
    
    (let* ((last-roc (exmes~state-description suggr*current-execution-message))
	   (last-strategy (roc~strategy-ks last-roc)))
      
      (if (or (null (exmes~failure-message-p suggr*current-execution-message))
	      (null (eq last-strategy (strat~find-strategy-ks 'ApplyAndSolveEquations)))
	      (pdsn~open-node-p (agenda~task-node (roc~start-task pplan*roc-state-description))))
	  
	  nil
	
	(let* ((start-task (roc~start-task pplan*roc-state-description))
	       (all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	       (all-tasks-with-current-roc (remove-if-not #'(lambda (task)
							      (find pplan*roc-state-description (agenda~task-rocs task)))
							  all-tasks))
	       (first-task (first all-tasks-with-current-roc))
	       (demand (demand~create-strategy-task-demand (strat~find-strategy-ks 'backtrack-strategy-to-task)
							   first-task
							   nil))
	       (reaction (suggr~create-termination-reaction (list demand)
							    suggr*current-execution-message
							    't)))
	  (list (list (cons demrec reaction))))))))

(defun backtrack-demand-reaction-for-report-is-no-method-applicable-p (demrec)
  ;; an argument, that has to be instantiated with suited backtrack-demand-reaction

  (if (null (exmes~failure-message-p suggr*current-execution-message))
      nil
    (let* ((report (exmes~failure-message-report suggr*current-execution-message)))
      (if (and (string-equal (first report) 'pplanner)
	       (string-equal (second report) 'no-applicable-method))
	  (let* ((task (third report))
		 (backtrack-step-demand-for-task (demand~create-strategy-task-demand (strat~find-strategy-ks 'backtrack-step-to-task)
										     task
										     nil))
		 (reaction (suggr~create-non-termination-reaction (list backtrack-step-demand-for-task)
								  suggr*current-execution-message)))
	    
	    (list (list (cons demrec reaction))))
	nil))))


#|(defun subst-demand-reaction-for-report-no-method-applicable-p (demrec)
  ;; an argument, that has to be instantiated with suited backtrack-demand-reaction

  (if (null (exmes~failure-message-p suggr*current-execution-message))
      nil
    (let* ((report (exmes~failure-message-report suggr*current-execution-message)))
      (if (and (string-equal (first report) 'pplanner)
	       (string-equal (second report) 'no-applicable-method))
	  (let* ((task (third report))
		 ;;(already-applied-strategy-ks-and-parameter-pairs (agenda~task-already-applied-strategy-ks task))
                 ;; already-applied-strategy-ks are now stored at the node directly
                 (already-applied-strategy-ks-and-parameter-pairs
                    (if (agenda~goal-or-goal-schema-task-p task)
                        (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task))))
                      (agenda~task-already-applied-strategy-ks task)))
                 (open-node (agenda~task-node task))
		 (possible-repair-subst= (find-repair-unify-for-open-node open-node))
		 (ApplySubstAndEquations-strat (strat~find-strategy-ks 'ApplySubstAndEquations))
		 
		 ;; TODO AMEIER:
		 ;; gehen wir mal davon aus, dass die Strategie, die diese Reparatur-Dingens letztlich ausfuehrt
		 ;; ApplySubstAndEquations heisst!
		 ;; Dann muss hier noch ein Check hin, ab im Control-Slot des offenen Knoten bereits ein Eintrag uber
		 ;; already applied strategies drinsteht von ApplySubstAndEquations. Wenn dem so ist, dann muss 
		 ;; HIER eigentlich noch eine abfrage rein, welche aus moeglichen Liste bereits probiert wurden und
		 ;; fehlgeschlagen sind!
		 ;; Diese muessen dann aussortiert werden!
		 ;; da ich das noch nicht habe, hier mal einfach nur ubernehmen der kompletten Liste:
		 
		 (allowed-repair-subst= (remove-if #'(lambda (param-list)
						       (find (list ApplySubstAndEquations-strat param-list)
							     already-applied-strategy-ks-and-parameter-pairs
							     :test #'keim~equal))
						   possible-repair-subst=))
		 (choosen-repair-subst= (first allowed-repair-subst=)))
	    
	    (if choosen-repair-subst=
		(let* ((demand (demand~create-strategy-task-demand (strat~find-strategy-ks 'ApplySubstAndEquations)
								   task
								   choosen-repair-subst=))
		       (reaction (suggr~create-non-termination-reaction (list demand)
									suggr*current-execution-message)))
		  
		  (list (list (cons demrec reaction))))
	      nil))
	nil))))|#

(defun subst-demand-reaction-for-report-no-method-applicable-p (demrec)
  ;; an argument, that has to be instantiated with suited backtrack-demand-reaction
  
  (if (null (exmes~failure-message-p suggr*current-execution-message))
      nil
    (let* ((report (exmes~failure-message-report suggr*current-execution-message)))
      (if (and (string-equal (first report) 'pplanner)
	       (string-equal (second report) 'no-applicable-method))
	  (let* ((task (third report))
		 ;;(already-applied-strategy-ks-and-parameter-pairs (agenda~task-already-applied-strategy-ks task))
		 ;; already-applied-strategy-ks are now stored at the node directly
                 (already-applied-strategy-ks-and-parameter-pairs
		  (if (agenda~goal-or-goal-schema-task-p task)
		      (keim::pdsc~already-applied-strategy-ks (pdsj~control (node~justification (agenda~task-node task))))
		    (agenda~task-already-applied-strategy-ks task)))
		 (open-node (agenda~task-node task))
		 (possible-repair-subst= (find-repair-unify-for-open-node open-node))
		 (ApplyAndSolveEquations-strat (strat~find-strategy-ks 'ApplyAndSolveEquations))
		 
		 ;; TODO AMEIER:
		 ;; gehen wir mal davon aus, dass die Strategie, die diese Reparatur-Dingens letztlich ausfuehrt
		 ;; ApplyAndSolveEquations heisst!
		 ;; Dann muss hier noch ein Check hin, ob im Control-Slot des offenen Knoten bereits ein Eintrag uber
		 ;; already applied strategies drinsteht von ApplySubstAndEquations. Wenn dem so ist, dann muss 
		 ;; HIER eigentlich noch eine abfrage rein, welche aus moeglichen Liste bereits probiert wurden und
		 ;; fehlgeschlagen sind!
		 ;; Diese muessen dann aussortiert werden!
		 ;; da ich das noch nicht habe, hier mal einfach nur uebernehmen der kompletten Liste:
		 
		 (allowed-repair-subst= (remove-if #'(lambda (param-list)
						       (find (list ApplyAndSolveEquations-strat param-list)
							     already-applied-strategy-ks-and-parameter-pairs
							     :test #'keim~equal))
						   possible-repair-subst=))
		 (choosen-repair-subst= (first allowed-repair-subst=)))
	    
	    (if choosen-repair-subst=
		(let* ((demand (demand~create-strategy-task-demand (strat~find-strategy-ks 'ApplyAndSolveEquations)
								   task
								   (cons (agenda~task-node task) choosen-repair-subst=)))
		       (reaction (suggr~create-non-termination-reaction (list demand)
									suggr*current-execution-message)))
		  
		  (list (list (cons demrec reaction))))
	      nil))
	nil))))

(defun find-repair-unify-for-open-node (open-node)
  (let* ((control (pdsj~control (node~justification open-node)))
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
			 broken-matchings)))
    
    (apply #'append (mapcar #'(lambda (broken-matching)
				(let* ((back (repair-unify-by-substs= broken-matching)))
				  (if back
				      (list back)
				    back)))
			    matchings-broken-because-of-alphaunify))))


(defun repair-unify-by-substs= (broken-matching)
  (declare (edited  "07-SEP-1999")
	   (authors Ameier)
	   (input   "A method matching with a breakpoint entry and the method which was tried to"
		    "apply.")
	   (effect  "None.")
	   (value   "A list consisting of:"
		    "First: a substitution"
		    "Second: a nd-line"
		    "Third: a list of equations"
		    "Fourth: a list of positions."
		    "Thereby the equations and positions have to be choosen that the"
		    "positions are in the nd-line that there are (after the substitution)"
		    "the left sides of the according equations."))
  
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
	   
	   ;; (alphaunify aa a1 sigma) is called in all these solve methods
	   ;; thereby aa comes form the aussumption (l1) and a1 comes from conclusion (l3), and mainly at positon 1 at each case
	   ;; -> choose conclusion (l3) and add position 1
	   
	   (multiple-value-bind
	       (subst repair-subst=)
	       (terms-unifiable-with-=substs (list (list uterm1 uterm2))
					     (subst~create nil nil)
					     nil
					     (list (list (pos~empty) (pos~empty))) 
					     (pds~environment omega*current-proof-plan))

	     (if subst
		 (list (term~appl-create (env~lookup-object '= (pds~environment omega*current-proof-plan))
					 (list uterm1 uterm2))
		       (pos~list-position '(1)))
	       
		         ;;;       (mapcar #'(lambda (repair-5tupel)
	                 ;;;		   (let* ((left-side (first repair-5tupel))
		         ;;;			  (right-side (second repair-5tupel))
		         ;;;			  (=obj (env~lookup-object '= (th~env 'base))))
		         ;;;		     (term~appl-create =obj
		         ;;;				       (list right-side left-side))))
		         ;;;	       repair-subst=)
		         ;;;       (mapcar #'(lambda (repair-5tupel)
		         ;;;		   (let* ((pos-right (fourth repair-5tupel)))
		         ;;;		     (pos~add-front 1 pos-right)))
		         ;;;	       repair-subst=)))
	       
	       nil)))
	  
	  
	  (t
	   ;; further repairs are not yet implemented!!
	   
	   nil))))


(defun terms-unifiable-with-=substs (pair-list subst repair-subst= position-list env)
  (let* ((choosen-pair (first pair-list))
	 (left-side (first choosen-pair))
	 (right-side (second choosen-pair)))

    (cond ((null choosen-pair)
	   ;; no further pairs to unify -> ready

	   (values subst
		   repair-subst=))
	  
	  ((and (term~constant-p left-side)
		(term~constant-p right-side)
		(data~equal left-side right-side))
	   ;; to equal constants -> remove the pair -> go on

	   (terms-unifiable-with-=substs (rest pair-list)
					subst
					repair-subst=
					(rest position-list)
					env))
	  
	  ((and (term~variable-p left-side)
		(null (find left-side (term~free-variables right-side))))
	   ;; variable at left and occur-check does not fire -> new entry in subst!
	   ;; Update also repair-subst= Tupel !
	   ;; If all repair-subst= Tupel are still Ok -> go on with rest and new subst
	   ;; otherwise -> end
	   
	   (let* ((new-subst (subst~create (list left-side) (list right-side)))
		  (new-repair-subst= (mapcar #'(lambda (repair-5tupel)
						 (cons (subst~apply new-subst (first repair-5tupel))
						       (cons (subst~apply new-subst (second repair-5tupel))
							     (rest (rest repair-5tupel)))))
					     repair-subst=)))

	     (if (every #'still-ok-p new-repair-subst=)
		 
		 (terms-unifiable-with-=substs (mapcar #'(lambda (pair)
							   (list (subst~apply new-subst (first pair))
								 (subst~apply new-subst (second pair))))
						       (rest pair-list))
					       (subst~compose-substitution new-subst subst)
					       new-repair-subst=
					       (rest position-list)
					       env)

	       (values nil nil))))
	  
	  ((and (term~variable-p right-side)
		(null (find right-side (term~free-variables left-side))))
	   ;; variable at right and occur-check does not fire -> new entry in subst!
	   ;; -> go on with rest and new subst
	   
	   (let* ((new-subst (subst~create (list right-side) (list left-side)))
		  (new-repair-subst= (mapcar #'(lambda (repair-5tupel)
						 (cons (subst~apply new-subst (first repair-5tupel))
						       (cons (subst~apply new-subst (second repair-5tupel))
							     (rest (rest repair-5tupel)))))
					     repair-subst=)))

	     (if (every #'still-ok-p new-repair-subst=)

		 (terms-unifiable-with-=substs (mapcar #'(lambda (pair)
							   (list (subst~apply new-subst (first pair))
								 (subst~apply new-subst (second pair))))
						       (rest pair-list))
					       (subst~compose-substitution new-subst subst)
					       new-repair-subst=
					       (rest position-list)
					       env)

	       (values nil nil))))
	  
	  ((and (data~appl-p left-side)
		(data~appl-p right-side)
		(data~equal (data~appl-function left-side)
			    (data~appl-function right-side)))
	   ;; right and left side are applications and the function symbol is equal!
	   ;; -> decompose and go on!

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
	     
	     (terms-unifiable-with-=substs (append (mapcar #'(lambda (arg1 arg2)
							       (list arg1 arg2))
							   (data~appl-arguments left-side)
							   (data~appl-arguments right-side))
						   (rest pair-list))
					   subst
					   repair-subst=
					   (append new-pos-pairs (rest position-list))
					   env)))
	  

	  (t

	   ;; If none of the above cases is true, it fails:
	   ;; 1.) Occur check fails!
	   ;; 2.) appl<-> appl fails because of different function symbols
	   ;; 3.) appl<-> const fails
	   ;; 4.) abstr occurs
	   ;; in the fourth case, you can do nothing -> nil, otherwise -> try to find good =substs

	   ;; THE MOST IMPORTANT HOWEVER IS, THAT only these kind of pairs are permitted, that each have the type num!!!

	   (let* ((num (env~lookup-object 'num (pds~environment omega*current-proof-plan))))
	     
	     (cond ((or (null (eq (term~type left-side) num))
			(null (eq (term~type right-side) num)))
		    (values nil
			    nil))
		   ((or (data~abstr-p left-side)
			(data~abstr-p right-side))
		    (values nil
			    nil))
		   ((and (term~variable-p left-side)
			 (find left-side (term~free-variables right-side)))
		    
		    ;; Occur-check left fires, for example such as X = X + c
		    ;; You can do something, if there is a further meta variable on the right side!
		    ;; if you have X = X + c, the only thing you can do is to show c = 0, but in general that would not be the case!
		    ;; But in the case X = X + Y you can do something!
		    ;; Therefore a constraint for Y results: Y = 0!
		    
		    (let* ((other-meta-vars-on-the-right-side (remove left-side
								      (term~free-variables right-side))))
		      
		      (if other-meta-vars-on-the-right-side
			  (terms-unifiable-with-=substs (rest pair-list)
							subst
							(cons (list left-side
								    right-side
								    (first (first position-list))
								    (second (first position-list))
								    'right-contains-other-mv-as-left)
							      repair-subst=)
							(rest position-list)
							env)
			(values nil nil))))
		   
		   ((and (term~variable-p right-side)
			 (find right-side (term~free-variables left-side)))
		    
		    ;; Occur-check rights fires, analogous to occur-check left!
		    
		    (let* ((other-meta-vars-on-the-left-side (remove right-side
								     (term~free-variables left-side))))
		      
		      (if other-meta-vars-on-the-left-side 
			  (terms-unifiable-with-=substs (rest pair-list)
							subst
							(cons (list left-side
								    right-side
								    (first (first position-list))
								    (second (first position-list))
								    'left-contains-other-mv-as-right)
							      repair-subst=)
							(rest position-list)
							env)
			(values nil nil))))
		   
		   (t
		    ;; appl<->appl or appl<->const fails
		    ;; -> Check whether there are meta variables on the left or right!
		    ;; if so, OK, otherwise unforunately no!
		    
		    (let* ((meta-vars (append (term~free-variables left-side)
					      (term~free-variables right-side))))
		      
		      (if meta-vars
			  (terms-unifiable-with-=substs (rest pair-list)
							subst
							(cons (list left-side
								    right-side
								    (first (first position-list))
								    (second (first position-list))
								    'any-mv)
							      repair-subst=)
							(rest position-list)
							env)
			(values nil nil))))))))))

(defun still-ok-p (repair-5tupel)
  (let* ((left-side (first repair-5tupel))
	 (right-side (second repair-5tupel))
	 (checker (fifth repair-5tupel)))

    (cond ((string-equal checker 'any-mv)
	   (if (append (term~free-variables left-side)
		       (term~free-variables right-side))
	       't
	     nil))
	  ((string-equal checker 'right-contains-other-mv-as-left)
	   (if (remove left-side (term~free-variables right-side))
	       't
	     nil))
	  ((string-equal checker 'left-contains-other-mv-as-right)
	   (if (remove right-side (term~free-variables left-side))
	       't
	     nil)))))
	  
	   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIE Selection CRULES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(cri~def-control-rule PREFER-DEMAND-FULFILLING
		      (kind strategic)
		      (if (job-is-fulfilling-demand-modulo-parameterisation-p ("job" "parameters")))
		      (then
		       (choose (("job" "parameters")))))

(cri~def-control-rule PREFER-OFFERS-FROM-STORE
		      (kind strategic)
		      (if (job-is-from-store-p "job"))
		      (then
		       (choose ("job"))))

(cri~def-control-rule AS-FIRST-INITIALIZE-CS
		      (kind strategic)
		      (if (cs-not-yet-initialized-and-initializeconstraintsolver-p "job"))
		      (then
		       (choose ("job"))))

(cri~def-control-rule REJECT-BACKTRACK-IF-NO-BACKTRACK-DEMAND-AND-OTHER-JOB-OFFERS-FOR-TASK
		      (kind strategic)
		      (if (backtrack-job-but-no-backtrack-demand-and-other-job-offers-for-task-p "job"))
		      (then
		       (reject ("job"))))

(cri~def-control-rule REJECT-INITIALIZECONSTRAINTSOLVER
		      (kind strategic)
		      (if (cs-already-initialized-and-initializeconstraintsolver-p "job"))
		      (then
		       (reject ("job"))))

(cri~def-control-rule DELAY-INST-ANSWER-CONSTRAINT
		      (kind strategic)
		      (if (open-goal-tasks-and-inst-answer-constraint-job-offer-p "job"))
		      (then
		       (reject ("job"))))

(cri~def-control-rule REJECT-InstFromCS
		      (kind strategic)
		      (if (instfromcs-job-p "job"))
		      (then
		       (reject ("job"))))

(cri~def-control-rule Delay-Expand
		      (kind strategic)
		      (if (exp-job-and-open-goals-jobs-exist-p "job"))
		      (then
		       (reject ("job"))))

(cri~def-control-rule Delay-CloseConstraintSolver
		      (kind strategic)
		      (if (close-cs-and-other-open-goals-exist-p "job"))
		      (then
		       (reject ("job"))))

;;
;;(cri~def-control-rule REJECT-InstFromParam
;;		      (kind strategic)
;;		      (if (InstFromParams-job-p "job"))
;;		      (then
;;		       (reject ("job"))))
;;


;;
;;(cri~def-control-rule REJECT-InstInteractively
;;		      (kind strategic)
;;		      (if (InstInteractively-job-p "job"))
;;		      (then
;;		       (reject ("job"))))
;;
;;
;;(defun InstInteractively-job-p (job)
;;  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
;;	 (jobs-to-InstInteractively (remove-if-not #'(lambda (job)
;;						       (if (job~strategy-ks-offer-p job)
;;							   (let* ((strategy-ks (job~strategy-ks job)))
;;							     (if (eq strategy-ks (strat~find-strategy-ks 'InstInteractively))
;;								 't
;;							       nil))))
;;						   all-job-offers)))
;;   
;;    (mapcar #'(lambda (jobi)
;;		(list (cons job jobi)))
;;	    jobs-to-InstInteractively)))
;;

;;(defun InstFromParams-job-p (job)
;;  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
;;	 (jobs-to-InstFromParams (remove-if-not #'(lambda (job)
;;						    (if (job~strategy-ks-offer-p job)
;;							(let* ((strategy-ks (job~strategy-ks job)))
;;							  (if (eq strategy-ks (strat~find-strategy-ks 'InstFromParams))
;;							      't
;;							    nil))))
;;						all-job-offers)))
;;   
;;    (mapcar #'(lambda (jobi)
;;		(list (cons job jobi)))
;;	    jobs-to-InstFromParams)))
;;


(defun exp-job-and-open-goals-jobs-exist-p (job)
  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	 (jobs-to-exp (remove-if-not #'(lambda (job)
					 (if (job~strategy-ks-offer-p job)
					     (let* ((task (job~task job)))
					       (if (agenda~pseudo-goal-p task)
						   't
						 nil))
					   nil))
				     all-job-offers))
	 (jobs-for-open-goals (remove-if-not #'(lambda (job)
						 (if (job~strategy-ks-offer-p job)
						     (let* ((task (job~task job)))
						       (if (agenda~goal-or-goal-schema-task-p task)
							   't
							 nil))
						   nil))
					     all-job-offers)))

    (if jobs-for-open-goals
	(mapcar #'(lambda (jobi)
		    (list (cons job jobi)))
		jobs-to-exp)
      nil)))

(defun close-cs-and-other-open-goals-exist-p (job)
  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	 (jobs-to-close-cs (remove-if-not #'(lambda (job)
					      (if (job~strategy-ks-offer-p job)
						  (let* ((strategy-ks (job~strategy-ks job)))
						    (if (eq strategy-ks (strat~find-strategy-ks 'CloseConstraintSolver))
							't
						      nil))))
					  all-job-offers))
	 (task (if jobs-to-close-cs
		   (job~task (first jobs-to-close-cs))
		 nil))
	 (all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	 (all-goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p all-tasks))
	 (all-other-goal-tasks (remove task all-goal-tasks)))
    
    (if all-other-goal-tasks
	(mapcar #'(lambda (jobi)
		    (list (cons job jobi)))
		jobs-to-close-cs)
      nil)))

(defun instfromcs-job-p (job)
  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	 (jobs-to-instfromcs (remove-if-not #'(lambda (job)
						(if (job~strategy-ks-offer-p job)
						    (let* ((strategy-ks (job~strategy-ks job)))
						      (if (eq strategy-ks (strat~find-strategy-ks 'InstFromCS))
							  't
							nil))))
					    all-job-offers)))
    
    (mapcar #'(lambda (jobi)
		(list (cons job jobi)))
	    jobs-to-instfromcs)))

(defun cs-not-yet-initialized-and-initializeconstraintsolver-p (job)
  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	 (jobs-to-updateconstraintsolver (remove-if-not #'(lambda (job)
							    (if (job~strategy-ks-offer-p job)
								(let* ((strategy-ks (job~strategy-ks job)))
								  (if (eq strategy-ks (strat~find-strategy-ks 'InitializeConstraintSolver))
								      't
								    nil))))
							all-job-offers))
	 (all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	 (all-inst-tasks (remove-if-not #'agenda~inst-task-p all-tasks))
	 (inst-tasks-with-just-inst (remove-if-not #'(lambda (inst-task)
						       (let* ((plan-step (agenda~inst-task-plan-step inst-task))
							      (plan-step-just (pdsc~an-just plan-step))
							      (plan-step-just-method (just~method plan-step-just))
							      (plan-step-just-method-name (keim~name plan-step-just-method)))
							 
							 ;;(format t "~%plan-step-just: ~A" plan-step-just)
							 ;;(format t "~%plan-step-just-method: ~A" plan-step-just-method)
							 
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
    
    (if (and (null inst-tasks-with-just-inst)
	     jobs-to-updateconstraintsolver)
	(list (list (cons job (first jobs-to-updateconstraintsolver))))
      nil)))

(defun cs-already-initialized-and-initializeconstraintsolver-p (job)
  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	 (jobs-to-updateconstraintsolver (remove-if-not #'(lambda (job)
							    (if (job~strategy-ks-offer-p job)
								(let* ((strategy-ks (job~strategy-ks job)))
								  (if (eq strategy-ks (strat~find-strategy-ks 'InitializeConstraintSolver))
								      't
								    nil))))
							all-job-offers))
	 (all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	 (all-inst-tasks (remove-if-not #'agenda~inst-task-p all-tasks))
	 (inst-tasks-with-just-inst (remove-if-not #'(lambda (inst-task)
						       (let* ((plan-step (agenda~inst-task-plan-step inst-task))
							      (plan-step-just (pdsc~an-just plan-step))
							      (plan-step-just-method (just~method plan-step-just))
							      (plan-step-just-method-name (keim~name plan-step-just-method)))
							 
							 ;;(format t "~%plan-step-just: ~A" plan-step-just)
							 ;;(format t "~%plan-step-just-method: ~A" plan-step-just-method)
							 
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
    
    (if (and inst-tasks-with-just-inst
	     jobs-to-updateconstraintsolver)
	
	(mapcar #'(lambda (jobi)
		    (list (cons job jobi)))
		jobs-to-updateconstraintsolver)
      
      nil)))

(defun open-goal-tasks-and-inst-answer-constraint-job-offer-p (job)
  (let* ((all-job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	 (jobs-to-instantiate-instmetavar (remove-if-not #'(lambda (job)
							     (if (job~strategy-ks-offer-p job)
								 (let* ((strategy-ks (job~strategy-ks job)))
								   (if (eq strategy-ks (strat~find-strategy-ks 'InstAnswerConstraint))
								       't
								     nil))))
							 all-job-offers))
	 (all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	 (all-goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p all-tasks)))
    
    (if all-goal-tasks
	(mapcar #'(lambda (jobi)
		    (list (cons job jobi)))
		jobs-to-instantiate-instmetavar)
      nil)))

(defun backtrack-job-but-no-backtrack-demand-and-other-job-offers-for-task-p (job)
  (let* ((demands (store~elements (black~get-blackboard-object-content 'demands sod*control-blackboard)))
	 (backtrack-demands (remove-if-not #'(lambda (demand)
					       (if (and (demand~strategy-task-demand-p demand)
							(eq (refalg~find-refinement-algorithm-object 'backtrack)
							    (strat~strategy-ks-refinement-algorithm
							     (demand~strategy-task-demand-strategy-ks demand))))
						   't
						 nil))
					   demands))
	 (job-offers (store~elements (black~get-blackboard-object-content 'job-offers sod*control-blackboard)))
	 (backtrack-jobs (remove-if-not #'(lambda (job-offer)
					    (let* ((strategy-ks (job~strategy-ks job-offer))
						   (refinement-algo (strat~strategy-ks-refinement-algorithm strategy-ks)))
					      (eq refinement-algo (refalg~find-refinement-algorithm-object 'backtrack))))
					job-offers))
	 (no-backtrack-jobs (remove-if #'(lambda (job-offer)
					   (let* ((strategy-ks (job~strategy-ks job-offer))
						  (refinement-algo (strat~strategy-ks-refinement-algorithm strategy-ks)))
					     (eq refinement-algo (refalg~find-refinement-algorithm-object 'backtrack))))
				       job-offers))
	 (backtrack-jobs-fulfilling-no-backtrack-demands
	  (remove-if #'(lambda (job-offer)
			 (find job-offer backtrack-demands
			       :test #'(lambda (jobi backi)
					 (eq (job~task jobi)
					     (demand~strategy-task-demand-task backi)))))
		     backtrack-jobs))
	 (backtrack-jobs-fulfilling-no-backtrack-demands-and-other-job-offers-for-task
	  (remove-if-not #'(lambda (job-offer)
			     (let* ((task (job~task job-offer)))
			       (find task no-backtrack-jobs
				     :test #'(lambda (taski jobi)
					       (eq taski (job~task jobi))))))
			 backtrack-jobs-fulfilling-no-backtrack-demands)))
    
    (mapcar #'(lambda (jobi)
		(list (cons job jobi)))
	    backtrack-jobs-fulfilling-no-backtrack-demands-and-other-job-offers-for-task)))


(defun job-is-fulfilling-demand-p (job)
  (let* ((demands-store (black~get-blackboard-object-content 'demands sod*control-blackboard))
	 (job-offers-store (black~get-blackboard-object-content 'job-offers sod*control-blackboard))
	 (job-offers (store~elements job-offers-store))
	 (jobs-fulfilling-demands (remove-if-not #'(lambda (jobi)
						     (store~demands-which-will-be-fulfilled-by-job demands-store jobi))
						 job-offers)))
    
    (mapcar #'(lambda (jobi)
		(list (cons job jobi)))
	    jobs-fulfilling-demands)))

(defun job-is-fulfilling-demand-modulo-parameterisation-p (args)
  ;; the first of args has to become job
  ;; second of args demand
  (let* ((job (first args))
	 (parameters (second args))
	 (demands-store (black~get-blackboard-object-content 'demands sod*control-blackboard))
	 (demands (store~elements demands-store))
	 (job-offers-store (black~get-blackboard-object-content 'job-offers sod*control-blackboard))
	 (job-offers (store~elements job-offers-store))
	 (job-offer-demand-pairs (do* ((rest-demands demands (rest rest-demands))
				       (back nil))
				     ((null rest-demands)
				      back)
				   (let* ((head-demand (first rest-demands))
					  (possible-jobs (remove-if-not
							  #'(lambda (job-offer)
							      (demand~demand-will-be-fulfilled-by-job-offer-modulo-parameters-p
							       head-demand
							       job-offer))
							  job-offers)))
				     
				     (when possible-jobs
				       (setf back (append back (mapcar #'(lambda (job)
									   (list job head-demand))
								       possible-jobs))))))))
				       
    (mapcar #'(lambda (pair)
		(let* ((jobi (first pair))
		       (paramsi (if (demand~parameters (second pair))
				    (demand~parameters (second pair))
				  'dummy)))
		  (list (cons job jobi)
			(cons parameters paramsi))))
	    job-offer-demand-pairs)))


(defun job-is-from-store-p (job)
  (let* ((job-offers-store (black~get-blackboard-object-content 'job-offers sod*control-blackboard))
	 (job-offers (store~elements job-offers-store))
	 (job-offers-from-store (remove-if-not #'job~store-offer-p job-offers)))

    (mapcar #'(lambda (jobi)
		(list (cons job jobi)))
	    job-offers-from-store)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PPLANNER Control-Rules                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ---------------------------------------------------> For Normalize Goal



(cri~def-control-rule MYattack-non-atom-goal
		      (kind tasks)
		      (if (non-literal-goal "goal" "meth"))
		      (then
		       (select ("goal"))))


				;; ("goal" "meth")))))



(defun non-literal-goal (goal meth)
  ;; parameter one is the goal to choose
  ;; parameter two are the methods

  (let* ((tasks-with-open-goals-with-non-literal-formulas (remove-if-not #'crihelp=node-is-open-and-formula-is-non-literal-p
									 cri*current-tasks)))

    (apply #'append 
	   (mapcar #'(lambda (task)
		       (let* ((node (agenda~task-node task))
			      (formula (node~formula node))
			      (methlist-to-goal (cond ((logic~universal-quantification-p formula)
						       '(foralli-m-b)
						       )
						      ((logic~existential-quantification-p formula)
						       '(existsi-meta-m-b)
						       )
						      ((logic~conjunction-p formula)
						       '(andi-m-b)
						       )
						      ((logic~disjunction-p formula)
						       '(oril-m-b orir-m-b))
						      ((logic~equivalence-p formula)
						       '(equivi-m-b)
						       )
						      ((logic~negation-p formula)
						       (error "~%In non-literal-goal, Negation case not implemented yet"))
						      ((logic~implication-p formula)
						       '(impi-m-b)
						       ))))
			 (mapcar #'(lambda (meth-to-goal)
				     (list (cons goal task) (cons meth meth-to-goal)))
				 methlist-to-goal)))
		   tasks-with-open-goals-with-non-literal-formulas))))
  

(defun crihelp=node-is-open-and-formula-is-non-literal-p (task)
  (let* ((node (agenda~task-node task))
	 (formula (node~formula node)))
    (if (or (null (pdsn~open-node-p node)) 
	    (logic~atom-p formula)
	    (and (logic~negation-p formula)
		 (logic~atom-p (first (data~appl-arguments formula)))))
	nil
      't)))


;; ----------------------------------------------> InitializeConstraintSolver



(cri~def-control-rule MYInitialize-constraint-solver
		      (kind tasks)
		      (if (cs-not-yet-initialized-p "goal"))
		      (then
		       (choose (("goal" INITIALIZE-CS-M-B)))))




(defun cs-not-yet-initialized-p (goal)
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
	nil
      (let* ((tasks cri*current-tasks))
	(mapcar #'(lambda (task)
		    (list (cons goal task)))
		tasks)))))



;; ------------------------------------------------------------------> UnwrapHyp



(cri~def-control-rule MYattack-focus
		      (kind tasks)
		      (if (focus-goal "goal" "meth" "ass"))
		      (then
		       (choose (("goal" "meth" "ass")))))

(defun focus-goal (goal meth ass)
  (let* ((focus (black~get-blackboard-object-content 'focus sod*solution-blackboard))
	 (focus-goal-node-name (first focus))
	 (focus-ass-node-name (second focus))
	 (pos (third focus))
	 (goal-node (pds~label2node focus-goal-node-name))
	 (ass-node (pds~label2node focus-ass-node-name))
	 (ass-formula (node~formula ass-node))
	 (task-with-goal-node (first (remove-if-not #'(lambda (task)
							(let* ((task-node (agenda~task-node task)))
							  (eq task-node goal-node)))
						    cri*current-tasks))))
    
    (cond ((logic~universal-quantification-p ass-formula)
	   (let* ((new-pos (pos~rest (pos~rest pos)))
		  (new-focussed-assumption-name (crihelp=new-name 1)))
	     
	     (if (null (pos~empty-p new-pos))
		 (black~change-blackboard-object-content! 'focus
							  (list focus-goal-node-name new-focussed-assumption-name new-pos)
							  sod*solution-blackboard)
	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
	     
	     (list (list (cons goal task-with-goal-node) (cons meth 'foralle-meta-m-f) (cons ass (list ass-node))))))
	  
	  ((logic~existential-quantification-p ass-formula)
	   (let* ((new-pos (pos~rest (pos~rest pos)))
		  (new-focussed-assumption-name (crihelp=new-name 1))
		  (new-goal-name (crihelp=new-name 2)))

	     (if (null (pos~empty-p new-pos))
		 (black~change-blackboard-object-content! 'focus
							  (list new-goal-name new-focussed-assumption-name new-pos)
							  sod*solution-blackboard)
	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
	     
	     (list (list (cons goal task-with-goal-node) (cons meth 'existse-m-a) (cons ass (list ass-node))))))
	  
	  ((logic~conjunction-p ass-formula)
	   (let* ((new-pos (pos~rest pos))
		  (new-focussed-assumption-name (if (equal (pos~first pos) 1)
						    (crihelp=new-name 1)
						  (crihelp=new-name 2))))

	     (if (null (pos~empty-p new-pos))
		 (black~change-blackboard-object-content! 'focus
							  (list focus-goal-node-name new-focussed-assumption-name new-pos)
							  sod*solution-blackboard)
	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
	     
	     (list (list (cons goal task-with-goal-node) (cons meth 'ande-m-f) (cons ass (list ass-node))))))
	  
	  ((logic~disjunction-p ass-formula)
	   (let* ((new-pos (pos~rest pos))
		  (new-focussed-assumption-name (if (equal (pos~first pos) 1)
						    (crihelp=new-name 1)
						  (crihelp=new-name 2))))
	     (error "~%In focus-goal, Disjunction case not implemented yet")
	     'mycases-m-a))
	  ((logic~equivalence-p ass-formula)
	   (let* ((new-pos (pos~rest pos))
		  (new-focussed-assumption-name (if (equal (pos~first pos) 1)
						    (crihelp=new-name 1)
						  (crihelp=new-name 2))))
	     (error "ATTENTION: AT POSITIONS AND EQUIVALENCES in focus-goal")
	     'myequive-m-f))
	  ((logic~negation-p ass-formula)
	   (error "~%In focus-goal, Negation case not implemented yet"))
	  ((logic~implication-p ass-formula)
	   (let* ((new-pos (pos~rest pos))
		  (new-focussed-assumption-name (if (equal (pos~first pos) 1)
						    (crihelp=new-name 2)
						  (crihelp=new-name 3)))
		  (new-goal-name (crihelp=new-name 2)))
	     
	     (if (null (pos~empty-p new-pos))
		 (black~change-blackboard-object-content! 'focus
							  (list new-goal-name new-focussed-assumption-name new-pos)
							  sod*solution-blackboard)
	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
	     
	     (list (list (cons goal task-with-goal-node) (cons meth 'impe-open-m-a) (cons ass (list ass-node)))))))))

(defun crihelp=new-name (number)
  (declare (edited  "15-MAR-1999")
	   (authors Ameier)
	   (input   "A number")
	   (effect  "None.")
	   (value   "Returns 'L(label-counter+number)"))
  (let* ((count (+ number (keim::pds=label-counter omega*current-proof-plan))))
    (make-symbol (format nil "L~A" count))))



;; --------------------------------------------------------------> AttackInequality




(cri~def-control-rule INTERRUPT-IF-FOCUS
		      (kind strategy-interruption)
		      (if (focus-p "goal"))
		      (then
		       (demands ((UnwrapHyp "goal" nil)))))

(cri~def-control-rule INTERRUPT-IF-INST
		      (kind strategy-interruption)
		      (if (instantiated-in-CS-p "insttask"))
		      (then
		       (demands ((InstfromCS "insttask" nil)))))

#|(cri~def-control-rule MYAttack-inequality<-non-standard-select
		      (kind tasks)
		      (if (and (goal-is-inequality "goal")
			       (most-similar-subterm-in-asss-to-goal ("goal" "ass" "pos"))))
		      (then
		       (prefer (("goal" Solve*<-m-b nil nil)
				("goal" Solve*<-<=-m-b nil nil)
				("goal" Solve*<=-<-m-b nil nil)
				("goal" Solve*>-m-b nil nil)
				;; Weiss nich wo ich es brauche ("goal" MYSolve*-leq-m-b nil nil)
				("goal" MYFactorialEstimate-m-b nil nil) 
				("goal" MYComplexEstimate<-m-b nil nil)
				("goal" MYComplexEstimate>-m-b nil nil)
				;;("goal" WEAKEN-M-A nil nil)
				("goal" MYSET-FOCUS-M-B ("ass") ("pos"))
				("goal" SimplifyInequality-m-b nil nil)
				("goal" Simplify-m-b nil nil)
				("goal" Simplify-m-f ("ass") nil)
				;;("goal" MYEnvEstimate<-m-b nil nil)
				;;("goal" MYEnvEstimate>-m-b nil nil)
				;;("goal" MYEnvEstimate<-m-f nil nil))
				))))|#

(cri~def-control-rule MYAttack-inequality<-non-standard-select
		      (kind methods)
		      (if (and (and (goal-matches ("goal" ("rel" "lhs" "rhs")))
				    (symbol-member "rel" (less greater leq geq)))
			       ;;(goal-is-inequality "goal")
			       (most-similar-subterm-in-asss-to-goal ("goal" "ass" "pos"))))
		      (then
		       (select (Solve*<-m-b
				Solve*<-<=-m-b 
				Solve*<=-<-m-b 
				Solve*>-m-b
				MYFactorialEstimate-m-b  
				MYComplexEstimate<-m-b 
				MYComplexEstimate>-m-b
				Simplify-m-b
				Simplify-m-f
				;; (Simplify-m-f ("ass") nil)
				SimplifyInequality-m-b
				(SimplifyInequality-m-f ("ass") nil)
				(MYSET-FOCUS-M-B ("ass") ("pos"))
				;;SimplifyInequality-m-b 
				;;Simplify-m-b 
				;;(Simplify-m-f ("ass") nil)
				))))



(cri~def-control-rule MYAttack-not=-non-standard-select
		      (kind tasks)
		      (if (and (goal-is-not= "goal")
			       (most-similar-subterm-in-asss-to-goal ("goal" "ass" "pos"))))
		      (then
		       (select (("goal" NotEqual-m-b nil nil)
				("goal" WEAKEN-M-A nil nil)
				;;("goal" MYSET-FOCUS-M-B ("ass") ("pos"))
				))))
;;
;;(cri~def-control-rule MYselect-domain-split
;;		      (kind tasks)
;;		      (if (and (myassumption-to-goal-matches ("goal" "ass" "lhs" "rhs"))
;;			       (not-yet-applied (DomainCaseSplit-m-b))))
;;		      (then
;;		       (select (("goal" DomainCaseSplit-m-b ("ass") ("lhs" "rhs"))))))
;;

(cri~def-control-rule MYreject-myset-focus-mmatchings-if-nothing-new
		      (kind mmatchings)
		      (if (mmatching-is-myset-focus-but-nothing-new-p "mmatching"))
		      (then
		       (reject ("mmatching"))))


(defun myassumption-to-goal-matches (parameters)
  ;; first parameter: goal
  ;; second parameter: assumption
  ;; third parameter: lhs
  ;; fourth parameter: rhs
  
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (goal (first parameters))
	 (assumption (second parameters))
	 (lhs (third parameters))
	 (rhs (fourth parameters))
	 (back-list (do* ((rest-tasks cri*current-tasks (rest rest-tasks))
			  (back nil))
			((null rest-tasks)
			 back)
		      (let* ((task (first rest-tasks))
			     (task-node (agenda~task-node task))
			     (supports (pds~node-supports task-node pds))
			     (supports-with-not= (remove-if-not #'(lambda (support)
								    (let* ((formula (node~formula support)))
								      (if (and (logic~negation-p formula)
									       (data~appl-p (first (data~appl-arguments formula)))
									       (keim~equal (data~appl-function (first (data~appl-arguments formula)))
											   (env~lookup-object '= (pds~environment omega*current-proof-plan))))
									  't
									nil)))
								supports)))
			(setq back (append back (mapcar #'(lambda (suppnot=)
							    (let* ((args (data~appl-arguments (first (data~appl-arguments (node~formula suppnot=))))))
							      (list task suppnot= (first args) (second args))))
							supports-with-not=)))))))

    (mapcar #'(lambda (quadrupel)
		(list (cons goal (first quadrupel))
		      (cons assumption (second quadrupel))
		      (cons lhs (third quadrupel))
		      (cons rhs (fourth quadrupel))))
	    back-list)))

	 


	 
(defun mmatching-is-myset-focus-but-nothing-new-p (mmatching)
  ;; mmatchings are stored in cri*current-mmatchings ...
  (let* ((mmatchings-with-myset-focus-m-b (remove-if-not #'(lambda (mmatching)
							     (eq (pplan~matched-method mmatching)
								 (meth~find-method 'MYSET-FOCUS-M-B)))
							 cri*current-mmatchings))
	 (mmatchings-with-myset-focus-m-b-but-nothing-new (remove-if-not #'(lambda (mmatching)
									     (nothing-new-with-myset-focus-m-b-p mmatching))
									 mmatchings-with-myset-focus-m-b)))

    (mapcar #'(lambda (matchi)
		(list (cons mmatching matchi)))
	    mmatchings-with-myset-focus-m-b-but-nothing-new)))

(defun nothing-new-with-myset-focus-m-b-p (mmatching)
  (declare (edited  "31-AUG-1999")
	   (authors Ameier)
	   (input   "A mmatching for method myset-focus-m-b.")
	   (effect  "None.")
	   (value   "T if the mmatching will creates nothing new for the task."
		    "This means the respective formula is already pre-unwrapeed"
		    "and compared with the supports of the task. If it is equal to"
		    "one of the supports it is useless to really unwrap it."))
  (let* ((goal (pplan~mmatch-goal mmatching))
	 (pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (goal-supports (pds~node-supports goal pds))
	 (ass (first (pplan~mmatch-exist-prems mmatching)))
	 (pos (first (pplan~mmatch-parameters mmatching))))

    (when (and (pdsn~schematic-p ass)
	       (null (pdsn~up-to-date ass)))
      (setf (pdsn~current-formula ass) (beta~normalize (subst~apply (pds~cstrpool-bindings (pds~constraint-pool pds))
								    (node~formula ass))))
      (setf (pdsn~up-to-date ass) T))
    
    (multiple-value-bind
	(pre-term-at-pos uni-vars)
	(pre-unwraphyp-term-at-pos (if (pdsn~schematic-p ass)
				       (pdsn~current-formula ass)
				     (node~formula ass))
				   pos
				   't)

      (if (find pre-term-at-pos goal-supports
		:test #'(lambda (term node)
			  (term-nothing-new-p term uni-vars node)))
	  't
	nil))))
      

(defun term-nothing-new-p (term uni-vars node)
  (declare (edited  "31-AUG-1999")
	   (authors Ameier)
	   (input   "A term, the universal-variables in the term and a node.")
	   (effect  "None.")
	   (value   "T if the term is term~alpha-equal to the updated current formula"
		    "of the node, with binding the universal-variables on meta-variables."))
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard)))

     (when (and (pdsn~schematic-p node)
	       (null (pdsn~up-to-date node)))
       (setf (pdsn~current-formula node) (beta~normalize (subst~apply (pds~cstrpool-bindings (pds~constraint-pool pds))
								      (node~formula node))))
       (setf (pdsn~up-to-date node) T))

     
     (multiple-value-bind
	 (success subst)
	 (bind~with-bindings
	  ((keim::term=alpha-equal term (if (pdsn~schematic-p node)
					    (pdsn~current-formula node)
					  (node~formula node))
				   uni-vars)))
       
       (let* ((codomain (subst~codomain subst)))
	 (if (and success
		  (every #'meta~p codomain)
		  (= (length codomain)
		     (length (remove-duplicates codomain))))
	     ;; -> Reines Renaming!
	     't
	   nil)))))

	 
(defgeneric pre-unwraphyp-term-at-pos (term pos pol)
  (declare (edited  "31-AUG-1999")
	   (authors Ameier)
	   (input   "A term, a position in the term and a polarity.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: The subterm in the term at the position"
		    "Second: the universal quantified variables with respect to the polarity."))
  (:method ((object-list list) position pol)
	   (if (pos~empty-p position)
	       (values object-list
		       nil)
	     (pre-unwraphyp-term-at-pos (elt object-list (pos~first position))
					(pos~rest position)
					pol)))
  (:method ((term term+appl) position pol)
	   (if (pos~empty-p position)
	       (values term
		       nil)
	     (let* ((number (pos~first position))
		    (substructs (data~substructs term))
		    (new-pol (if (logic~negation-p term)
				 (null pol)
			       pol)))
	       (if (>= number (length substructs))
		   (error "Position ~A not in struct ~A." position term)
		 (multiple-value-bind
		     (pre-unwrap-term uni-vars)
		     (pre-unwraphyp-term-at-pos (elt substructs number)
						(pos~rest position)
						new-pol)
		   (let* ((new-uni-vars (cond ((or (and pol
							(logic~universal-quantification-p term))
						   (and (null pol)
							(logic~existential-quantification-p term)))
					       (cons (logic~quantification-bound-variable term)
						     uni-vars))
					      (t
					       uni-vars))))
		     (values pre-unwrap-term new-uni-vars)))))))
  (:method ((term term+term) position pol)
	   (if (pos~empty-p position)
	       (values term
		       nil)
	     (let ((number (pos~first position))
		   (substructs (data~substructs term)))
	       (if (>= number (length substructs))
		   (error "Position ~A not in struct ~A." position term)
		 (pre-unwraphyp-term-at-pos (elt substructs number)
					    (pos~rest position)
					    pol))))))


		     
(defun focus-p (goal)
  (let* ((focus (black~get-blackboard-object-content 'focus sod*solution-blackboard)))
    (if focus
	(let* ((focus-goal-node-name (first focus))
	       (goal-node (pds~label2node focus-goal-node-name))
	       (task-with-goal-node (first (remove-if-not #'(lambda (task)
							      (let* ((task-node (agenda~task-node task)))
								(eq task-node goal-node)))
							  cri*current-tasks))))
	  (list (list (cons goal task-with-goal-node))))
      nil)))

#|(defun instantiated-in-CS-p (insttask)
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (all-tasks (agenda~all-tasks (pds~agenda pds)))
	 (all-inst-tasks (remove-if-not #'agenda~inst-task-p all-tasks))
	 (relevant-eq-steps
	  ;; berechnet alle steps zurueck bis zum letzten nicht normalizing/restriction STep in diesem ROC, die
	  ;; TellCS-m-b auf eine Gleichung machen  
	  (do* ((current-step (pds~last-plan-step pds) (pdsc~predecessor (pdsj~control (pdsc~an-just current-step))))
		(back-steps nil))
	      ((or (null current-step)
		   (null (find current-step (roc~pplanner-steps pplan*roc-state-description)))
		   (crihelp=not-normalizing-or-restricting-step-p current-step))
	       back-steps)
	    (let* ((just (pdsc~an-just current-step))
		   (method (just~method just)))

	      (when (and (find method (list (infer~find-method 'mytellcs-m)
					    (infer~find-method 'tellcs-m)))
			 ;; tell methode
			 (string= (first (pdsj~outline-pattern just)) "EXISTENT")
			 ;; + rueckwaerts angewand
			 )
		(let* ((formula-send-to-cs (node~formula (pdsc~an-node current-step))))
		  (when (and (data~appl-p formula-send-to-cs)
			     (data~equal (data~appl-function formula-send-to-cs) (data~schema-range
										  (env~lookup-object '= (th~env 'base)))))
		    (setq back-steps (cons current-step  back-steps))))))))
	 (insttasks

	  (progn
	    
	    ;; berechnet die Inst-Tasks zu den steps
	    (do* ((rest-eq-steps relevant-eq-steps (rest rest-eq-steps))
		  (corresponding-inst-tasks nil))
		((null rest-eq-steps)
		 (remove-duplicates corresponding-inst-tasks))
	      (let* ((head-step (first rest-eq-steps))
		     (eq-args (data~appl-arguments (node~formula (pdsc~an-node head-step))))
		     (meta-var (if (meta~p (first eq-args))
				   (first eq-args)
				 nil))
		     (inst-task-to-meta-var (if meta-var
						(let* ((inst-task (find meta-var all-inst-tasks
									:test #'(lambda (var inst-task)
										  (eq var (agenda~inst-task-meta-var inst-task))))))
						  (if inst-task
						      (list inst-task)
						    nil ;; kann passieren wenn Schritt doch bereits abgehandelt ...
						    ))
					      nil)))
		
		
		(setq corresponding-inst-tasks (append corresponding-inst-tasks inst-task-to-meta-var))))))
	 )
    
    ;;(format t "~%~%We have: relevant-eq-steps ~A" relevant-eq-steps)
    ;;(format t "~%~%We have: insttasks ~A" insttasks)
    
    (mapcar #'(lambda (inst-task)
		(list (cons insttask inst-task)))
	    insttasks))) |#


(defun instantiated-in-CS-p (insttask)
  (let* ((pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (all-tasks (agenda~all-tasks (pds~agenda pds)))
	 (all-inst-tasks (remove-if-not #'agenda~inst-task-p all-tasks))
	 (detvars (cosie~getdetvars))
	 (current-bindings (if (pds~constraint-pool pds)
			       (pds~cstrpool-bindings (pds~constraint-pool pds))
			     (subst~create nil nil)))						      
	 (bound-metavars (subst~domain current-bindings))
	 (newdetvars (remove-if #'(lambda (meta-var-assoc-pair)
				    (find (first meta-var-assoc-pair) bound-metavars))
				detvars))
	 (relevant-insttasks (remove-if-not #'(lambda (inst-task)
						(find (agenda~inst-task-meta-var inst-task)
						      newdetvars
						      :test #'(lambda (mvar pair)
								(keim~equal mvar (first pair)))))
					    all-inst-tasks)))
    
    ;;(format t "~%~%We have: relevant-eq-steps ~A" relevant-eq-steps)
    ;;(format t "~%~%We have: insttasks ~A" insttasks)
    
    (mapcar #'(lambda (inst-task)
		(list (cons insttask inst-task)))
	    relevant-insttasks)))


(defun crihelp=not-normalizing-or-restricting-step-p (step)
  (let* ((method (just~method (pdsc~an-just step))))
    
    (if (find method (list (infer~find-method 'MYSolve*<-m)
			   (infer~find-method 'MYSolve*<-<=-m)
			   (infer~find-method 'MYSolve*<=-<-m)
			   (infer~find-method 'MYSolve*>-m)
			   (infer~find-method 'MYSolve*-leq-m)
			   (infer~find-method 'MYComplexEstimate<-m)
			   (infer~find-method 'MYComplexEstimate>-m)
			   (infer~find-method 'MYFactorialEstimate-m)
			   (infer~find-method 'MYSET-FOCUS-M)
			   (infer~find-method 'MYWEAKEN-M)))
	't
      nil)))

(defun goal-is-inequality (goal)
  (let* ((tasks-with-inequalities (remove-if-not #'crihelp=node-is-open-and-formula-is-inequality
						 cri*current-tasks)))
    
    (mapcar #'(lambda (task)
		(list (cons goal task)))
	    tasks-with-inequalities)))

(defun goal-is-not= (goal)
  (let* ((tasks-with-not= (remove-if-not #'crihelp=node-is-open-and-formula-is-not=
					 cri*current-tasks)))
    
    (mapcar #'(lambda (task)
		(list (cons goal task)))
	    tasks-with-not=)))

(defun crihelp=node-is-open-and-formula-is-not= (task)
  (let* ((node (agenda~task-node task))
	 (formula (node~formula node)))
    (if (pdsn~open-node-p node)
	(if (data~appl-p formula)
	    (if (and (logic~negation-p formula)
		     (data~appl-p (first (data~appl-arguments formula)))
		     (keim~equal (data~appl-function (first (data~appl-arguments formula)))
				 (env~lookup-object '= (pds~environment omega*current-proof-plan))))
		't
	      nil)
	  nil)
      nil)))

(defun crihelp=node-is-open-and-formula-is-inequality (task)
  (let* ((node (agenda~task-node task))
	 (formula (node~formula node)))
    (if (pdsn~open-node-p node)
	(if (data~appl-p formula)
	    (if (or (keim~equal (data~appl-function formula)
				(env~lookup-object 'less (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function formula)
				(env~lookup-object 'leq (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function formula)
				(env~lookup-object 'greater (pds~environment omega*current-proof-plan)))
		    (keim~equal (data~appl-function formula)
				(env~lookup-object 'geq (pds~environment omega*current-proof-plan))))
		't
	      nil)
	  nil)
      nil)))


#|;; ---------------------------------------------------> for ApplySubstAndEquations


(cri~def-control-rule MYApplySAEOnTask
		      (kind tasks)
		      (if (goal-in-params ("goal" "subst" "equs" "poses")))
		      (then
		       (choose (("goal" MYApplySubstAndEquationsOnTask-m-b nil ("subst" "equs" "poses"))))))

(cri~def-control-rule MYApplySAEOnSupport
		      (kind tasks)
		      (if (support-to-goal-in-params ("goal" "support" "subst" "equs" "poses")))
		      (then
		       (choose (("goal" MYApplySubstAndEquationsOnSupport-m-b ("support") ("subst" "equs" "poses"))))))

(defun goal-in-params (parameters)
  ;; first von parameters ist goal
  ;; second von parameters ist subst
  ;; third von parameters ist equs
  ;; fourth von parameters ist poses

  (let* ((current-task (first cri*current-tasks))
	 (params (roc~parameters pplan*roc-state-description))
	 (subst (first params))
	 (ndline (second params))
	 (equ-list (third params))
	 (pos-list (fourth params)))
    
    (if (eq (agenda~task-node current-task) ndline)
	(list (list (cons (first parameters) current-task)
		    (cons (second parameters) subst)
		    (cons (third parameters) equ-list)
		    (cons (fourth parameters) pos-list)))
      nil)))


(defun support-to-goal-in-params (parameters)
  ;; first von parameters ist goal
  ;; second von parameters ist support
  ;; third von parameters ist subst
  ;; fourth von parameters ist equs
  ;; fifth von parameters ist poses
  
  (let* ((current-task (first cri*current-tasks))
	 (task-node (agenda~task-node current-task))
	 (pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))
	 (supports (pds~node-supports task-node pds))  
	 (params (roc~parameters pplan*roc-state-description))
	 (subst (first params))
	 (ndline (second params))
	 (equ-list (third params))
	 (pos-list (fourth params)))
    
    (if (find ndline supports)
	(list (list (cons (first parameters) current-task)
		    (cons (second parameters) ndline)
		    (cons (third parameters) subst)
		    (cons (fourth parameters) equ-list)
		    (cons (fifth parameters) pos-list)))
      nil)))
|#


(cri~def-control-rule MYApplyfirstMYSubst=param
		      (kind tasks)
		      (if (goal-in-params ("goal" "equation" "pos")))
		      (then
		       (choose (("goal" MYSubst=param-m-b nil ("equation" "pos"))))))

(defun goal-in-params (parameters)
  ;; first of parameters is goal
  ;; second of parameters is equation
  ;; third of parameters is pos

  (let* ((current-task (first cri*current-tasks))
	 (params (roc~parameters pplan*roc-state-description))
	 (ndline (first params))
	 (equation (second params))
	 (pos (third params)))
    
    (if (eq (agenda~task-node current-task) ndline)
	(list (list (cons (first parameters) current-task)
		    (cons (second parameters) equation)
		    (cons (third parameters) pos)))
      nil)))


;; ------------------------------------------------------> For Limitapplytheorems

(cri~def-control-rule APPLY-THEOREM
		      (kind tasks)
		      (if (apply-lim-theorem-task-p ("goal" "supports")))
		      (then
		       (select (("goal" MYAssertion-m-b "supports" nil)))))

(defun apply-lim-theorem-task-p (args)
  (let* ((goal (first args))
	 (supports (second args))
	 (pds (black~get-blackboard-object-content 'pds sod*solution-blackboard))

	 (all-tasks (agenda~all-tasks (pds~agenda (black~get-blackboard-object-content 'pds sod*solution-blackboard))))
	 (all-inst-tasks (remove-if-not #'agenda~inst-task-p all-tasks))
	 (inst-task-with-cs (first (remove-if-not #'(lambda (inst-task)
						      (let* ((plan-step (agenda~inst-task-plan-step inst-task))
							     (plan-step-just (pdsc~an-just plan-step))
							     (plan-step-just-method (just~method plan-step-just))
							     (plan-step-just-method-name (keim~name plan-step-just-method)))
							
							;;(format t "~%plan-step-just: ~A" plan-step-just)
							;;(format t "~%plan-step-just-method: ~A" plan-step-just-method)
							
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
	 (mv-cs (if inst-task-with-cs
		    (agenda~inst-task-meta-var inst-task-with-cs)
		  nil))

	 (lim-tasks (remove-if-not #'(lambda (task)
				       (let* ((formula (node~formula (agenda~task-node task))))
					 (if (data~appl-p formula)
					     (let* ((function (data~appl-function formula))
						    (lim (env~lookup-object 'lim (pds~environment omega*current-proof-plan))))
					       (if (and lim
							(data~equal lim function))
						   't
						 nil))
					   nil)))
				   cri*current-tasks)))
    
    
    (mapcar #'(lambda (task)
		(let* ((task-node (agenda~task-node task))
		       (node-supports (pds~node-supports task-node pds))
		       (rest-node-supports (remove-if #'(lambda (supp)
							  (keim~equal (node~formula supp) mv-cs))
						      node-supports)))
		  
		  (list (cons goal task)
			(cons supports rest-node-supports))))
	    lim-tasks)))

