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
(in-package "OMEGA")

(defun infinite-relation (open hyp)
;;goal-check
  (let* ((numtype (env~lookup-object 'num (pds~environment omega*current-proof-plan)))
	 (goal (agenda~task-node cri*current-task))
	 (limit-term (node~formula goal))
	 (limit (mapcan #'(lambda (term)
			    (when (and (data~equal (term~type term) numtype)
				       (term~constant-p term)
				       (not (term~number-p term)))
			      (list term))) (data~all-substructs (node~formula goal)))))
    (when (and limit (not (term~variables limit-term)))
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
					    cri*current-supports)))))))
				  cri*current-supports)))
	(print asslist)
  (when asslist  (list (reduce #'(lambda (list ass) (acons hyp ass list))
			       (sort asslist  #'cri=more-complex)
			       :initial-value (acons  open goal nil))))))))
	
(defun cri=more-complex (node1 node2)
  (> (cri=complexity (node~formula node1))
     (cri=complexity (node~formula node2))))
	

(defgeneric cri=complexity (form)
  (:method ((form term+abstr))
	   (+ (cri=complexity (data~abstr-domain form))
	      (cri=complexity (data~abstr-range form))))
  (:method ((form term+appl))
	   (+ (cri=complexity (data~appl-function form))
	      (cri=complexity (data~appl-arguments form))))
  (:method ((form cons))
	   (+ (cri=complexity (Car form))
	      (cri=complexity (cdr form))))
  (:method ((form term+variable))
	   1)
  (:method ((form term+constant))
	   1)
  (:method ((form t))
	   0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following stuff is used for Applying MULTI to solve
;; Limes Problems
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIE Selection CRULES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun not-root-p (task)
  (cond ((stringp task)
	 ;; task is unbound
	 nil)
	((agenda~goal-or-goal-schema-task-p task)
	 (let* ((node (agenda~task-node task)))
	   (unless (eq (prob~proof-root omega*current-proof-plan) node)
	     (list nil)))) ;;(list (cons T T))))))
	((agenda~inst-task-p task)
	 (list nil))
	(t
	 nil)))

(defun source-plan-for-external-analogy-p (exana-job)
  (if ana*source-plan
      (let* ((root-node-in-ana (prob~proof-root ana*source-plan))
	     (root-node-curent (prob~proof-root omega*current-proof-plan))
	     (all-goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p (agenda~all-tasks (pds~agenda omega*current-proof-plan))))
	     (task-to-root (first (remove-if-not #'(lambda (task)
						     (eq (agenda~task-node task) root-node-curent))
						 all-goal-tasks))))
	(when (and root-node-in-ana
		   task-to-root)
	  (list (list (cons exana-job (job~create-strategy-ks-offer (strat~find-strategy-ks 'ExternalAnalogy) task-to-root (list root-node-in-ana)))))))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PPLANNER Control-Regeln                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ---------------------------------------------------> Fuer IndirectProof

(defun goal-results-from-indirect-p (goal)
  (let* ((all-tasks (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
	 (tasks-with-roc (remove-if-not #'(lambda (task)
					    (if (and (agenda~goal-or-goal-schema-task-p task)
						     (find pplan*roc-state-description (agenda~task-rocs task)))
						't
					      nil))
					all-tasks))
	 (goal-nodes (mapcar #'agenda~task-node tasks-with-roc))
	 (goal-nodes-produced-by-indirect
	  (remove-if-not #'(lambda (goal)
			     (let* ((just (node~justification goal))
				    (other-reasons (pdsj~other-reasons just))
				    (other-reasons-indirect
				     (remove-if-not #'(lambda (reason)
							(let* ((method (just~method (pdsc~an-just reason))))
							  (string-equal (keim~name method) 'indirect-m)))
						    other-reasons)))
			       other-reasons-indirect))
			 goal-nodes)))
    (mapcar #'(lambda (goal-by-ind)
		(list (cons goal goal-by-ind)))
	    goal-nodes-produced-by-indirect)))

;; ---------------------------------------------------> Fuer Normalize Goal

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
						       '(pullneg-m-b)
						       )
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


;; ------------------------------------------------------------------> UnwrapHyp

;;(defun focus-goal (goal meth ass)
;;  (let* ((focus (black~get-blackboard-object-content 'focus sod*solution-blackboard))
;;	 (focus-goal-node-name (first focus))
;;	 (focus-ass-node-name (second focus))
;;	 (pos (third focus))
;;	 (goal-node (pds~label2node focus-goal-node-name))
;;	 (ass-node (pds~label2node focus-ass-node-name))
;;	 (ass-formula (node~formula ass-node))
;;	 (task-with-goal-node (first (remove-if-not #'(lambda (task)
;;							(let* ((task-node (agenda~task-node task)))
;;							  (eq task-node goal-node)))
;;						    cri*current-tasks))))
;;   
;;    (cond ((logic~universal-quantification-p ass-formula)
;;	   (let* ((new-pos (pos~rest (pos~rest pos)))
;;		  (new-focussed-assumption-name (crihelp=new-name 1)))
;;	     
;;	     (if (null (pos~empty-p new-pos))
;;		 (black~change-blackboard-object-content! 'focus
;;							  (list focus-goal-node-name new-focussed-assumption-name new-pos)
;;							  sod*solution-blackboard)
;;	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
;;	     
;;	     (list (list (cons goal task-with-goal-node) (cons meth 'foralle-meta-m-f) (cons ass (list ass-node))))))
;;	  ((logic~existential-quantification-p ass-formula)
;;	   (let* ((new-pos (pos~rest (pos~rest pos)))
;;		  (new-focussed-assumption-name (crihelp=new-name 1))
;;		  (new-goal-name (crihelp=new-name 2)))
;;
;;	     (if (null (pos~empty-p new-pos))
;;		 (black~change-blackboard-object-content! 'focus
;;							  (list new-goal-name new-focussed-assumption-name new-pos)
;;							  sod*solution-blackboard)
;;	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
;;	     
;;	     (list (list (cons goal task-with-goal-node) (cons meth 'existse-m-a) (cons ass (list ass-node))))))  
;;	  ((logic~conjunction-p ass-formula)
;;	   (let* ((new-pos (pos~rest pos))
;;		  (new-focussed-assumption-name (if (equal (pos~first pos) 1)
;;						    (crihelp=new-name 1)
;;						  (crihelp=new-name 2))))
;;
;;	     (if (null (pos~empty-p new-pos))
;;		 (black~change-blackboard-object-content! 'focus
;;							  (list focus-goal-node-name new-focussed-assumption-name new-pos)
;;							  sod*solution-blackboard)
;;	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
;;	     
;;	     (list (list (cons goal task-with-goal-node) (cons meth 'ande-m-f) (cons ass (list ass-node))))))
;;	  ((logic~disjunction-p ass-formula)
;;	   ;; VERSION WITH OrEL-open-m-a and OrER-open-m-a with thm as parameter
;;	   ;;(let* ((new-pos (pos~rest pos))
;;	   ;;	  (new-focussed-assumption-name (crihelp=new-name 1))
;;	   ;;	  )
;;	   ;;    
;;	   ;;   (if (null (pos~empty-p new-pos))
;;	   ;;	 (black~change-blackboard-object-content! 'focus
;;	   ;;						  (list focus-goal-node-name new-focussed-assumption-name new-pos)
;;	   ;;						  sod*solution-blackboard)
;;	   ;;      (black~remove-blackboard-object! 'focus sod*solution-blackboard))
;;	   ;;   
;;	   ;;   (list (list (cons goal task-with-goal-node) (if (equal (pos~first pos) 2)
;;	   ;;						     (cons meth 'OrEL-open-m-a)
;;	   ;;						   (cons meth 'OrER-open-m-a)) 
;;	   ;;		 (cons ass (list ass-node)))))
;;	   (let* ((new-pos (pos~rest pos))
;;		  (new-focussed-assumption-name (crihelp=new-name 4))
;;		  (new-goal-name (crihelp=new-name 2)))
;;	     	     
;;	     (if (null (pos~empty-p new-pos))
;;		 (black~change-blackboard-object-content! 'focus
;;							  (list new-goal-name new-focussed-assumption-name new-pos)
;;							  sod*solution-blackboard)
;;	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
;;	     
;;	     (list (list (cons goal task-with-goal-node) (if (equal (pos~first pos) 2)
;;							     (cons meth 'OrEL-open-m-a)
;;							   (cons meth 'OrER-open-m-a)) 
;;			 (cons ass (list ass-node)))))
;;	   )
;;	  ((logic~equivalence-p ass-formula)
;;	   (let* ((new-pos (pos~rest pos))
;;		  (new-focussed-assumption-name (if (equal (pos~first pos) 1)
;;						    (crihelp=new-name 1)
;;						  (crihelp=new-name 2))))
;;	     (error "VORSICHT: BEI POSITIONEN UND EQUIVALENZEN in focus-goal")
;;	     'myequive-m-f))
;;	  ((logic~negation-p ass-formula)
;;	   (error "~%In focus-goal, Negation case not implemented yet"))
;;	  ((logic~implication-p ass-formula)
;;	   (let* ;; VERSION WITH IMPE-OPEN-M-A with THM as PARAMETER
;;	         ;;((new-pos (pos~rest pos))
;;	         ;; (new-focussed-assumption-name (if (equal (pos~first pos) 1)
;;	         ;;				    ;; if pos is 1, we have to choose another method!
;;	         ;;				    ;; One that chooses IMP2OR ??
;;		 ;;				    (crihelp=new-name 1)  ;; 2
;;		 ;;				  (crihelp=new-name 1))) ;; 3
;;		 ;;  ;;(new-goal-name (crihelp=new-name 2))
;;		 ;;  )
;;	       
;;	       ((new-pos (pos~rest pos))
;;		(new-focussed-assumption-name (if (equal (pos~first pos) 1)
;;						  (crihelp=new-name 2)
;;						(crihelp=new-name 3)))
;;		(new-goal-name (crihelp=new-name 2))
;;		)
;;	     
;;	     (if (null (pos~empty-p new-pos))
;;		 (black~change-blackboard-object-content! 'focus
;;						  ;; VERSION WITH IMPE-OPEN-M-A with THM as PARAMETER
;;							  ;; (list focus-goal-node-name new-focussed-assumption-name new-pos)
;;							  (list new-goal-name new-focussed-assumption-name new-pos)
;;							  sod*solution-blackboard)
;;	       (black~remove-blackboard-object! 'focus sod*solution-blackboard))
;;	     
;;	     (list (list (cons goal task-with-goal-node) (cons meth 'impe-open-m-a) (cons ass (list ass-node)))))))))
;;
;;

(defun focus-goal (goal meth ass)
  (let* ((focus (black~get-blackboard-object-content 'focus sod*solution-blackboard)))
    (if focus
	(let* ((focus-goal-node-name (first focus))
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
		 ;; VERSION WITH OrEL-open-m-a and OrER-open-m-a with thm as parameter
		 ;;(let* ((new-pos (pos~rest pos))
		 ;;	  (new-focussed-assumption-name (crihelp=new-name 1))
		 ;;	  )
		 ;;    
		 ;;   (if (null (pos~empty-p new-pos))
		 ;;	 (black~change-blackboard-object-content! 'focus
		 ;;						  (list focus-goal-node-name new-focussed-assumption-name new-pos)
		 ;;						  sod*solution-blackboard)
		 ;;      (black~remove-blackboard-object! 'focus sod*solution-blackboard))
		 ;;   
		 ;;   (list (list (cons goal task-with-goal-node) (if (equal (pos~first pos) 2)
		 ;;						     (cons meth 'OrEL-open-m-a)
		 ;;						   (cons meth 'OrER-open-m-a)) 
		 ;;		 (cons ass (list ass-node)))))
		 (let* ((new-pos (pos~rest pos))
			(new-focussed-assumption-name (crihelp=new-name 4))
			(new-goal-name (crihelp=new-name 2)))
		   
		   (if (null (pos~empty-p new-pos))
		       (black~change-blackboard-object-content! 'focus
								(list new-goal-name new-focussed-assumption-name new-pos)
								sod*solution-blackboard)
		     (black~remove-blackboard-object! 'focus sod*solution-blackboard))
		   
		   (list (list (cons goal task-with-goal-node) (if (equal (pos~first pos) 2)
								   (cons meth 'OrEL-open-m-a)
								 (cons meth 'OrER-open-m-a)) 
			       (cons ass (list ass-node)))))
		 )
		((logic~equivalence-p ass-formula)
		 (let* ((new-pos (pos~rest pos))
			(new-focussed-assumption-name (if (equal (pos~first pos) 1)
							  (crihelp=new-name 1)
							(crihelp=new-name 2))))
		   (error "VORSICHT: BEI POSITIONEN UND EQUIVALENZEN in focus-goal")
		   'myequive-m-f))
		((logic~negation-p ass-formula)
		 (error "~%In focus-goal, Negation case not implemented yet"))
		((logic~implication-p ass-formula)
		 (let* ;; VERSION WITH IMPE-OPEN-M-A with THM as PARAMETER
		     ;;((new-pos (pos~rest pos))
		     ;; (new-focussed-assumption-name (if (equal (pos~first pos) 1)
		     ;;				    ;; if pos is 1, we have to choose another method!
		     ;;				    ;; One that chooses IMP2OR ??
		     ;;				    (crihelp=new-name 1)  ;; 2
		     ;;				  (crihelp=new-name 1))) ;; 3
		     ;;  ;;(new-goal-name (crihelp=new-name 2))
		     ;;  )
		     
		     ((new-pos (pos~rest pos))
		      (new-focussed-assumption-name (if (equal (pos~first pos) 1)
							(crihelp=new-name 2)
						      (crihelp=new-name 3)))
		      (new-goal-name (crihelp=new-name 2))
		      )
		   
		   (if (null (pos~empty-p new-pos))
		       (black~change-blackboard-object-content! 'focus
								;; VERSION WITH IMPE-OPEN-M-A with THM as PARAMETER
								;; (list focus-goal-node-name new-focussed-assumption-name new-pos)
								(list new-goal-name new-focussed-assumption-name new-pos)
								sod*solution-blackboard)
		     (black~remove-blackboard-object! 'focus sod*solution-blackboard))
		   
		   (list (list (cons goal task-with-goal-node) (cons meth 'impe-open-m-a) (cons ass (list ass-node))))))))
      (if (crihelp=initial-strategy-p)
	  (let* ((goal-name (keim~name (agenda~task-node (roc~start-task pplan*roc-state-description))))
		 (params (roc~parameters pplan*roc-state-description))
		 (ass-name (keim~name (first params)))
		 (pos (second params)))
	    (black~add-new-blackboard-object! 'focus (list goal-name ass-name pos) 'dummyt sod*solution-blackboard)
	    (focus-goal goal meth ass)
	    )
	nil))))

(defun imp-ass (goal ass params)
  (if (and (null (stringp goal))
	   (null (stringp ass)))
      ;; goal and ass gebunden
      (let* ((gnode (agenda~task-node goal)))
	(cond ((logic~implication-p (node~formula ass))
	       (list (list (list params (list gnode))))) ;; (pdsn~hyps gnode))))))
	      ((logic~disjunction-p (node~formula ass))
	       (list (list (list params (list gnode))))) ;; (pdsn~hyps gnode))))))
	      (t
	       (list (list (list params 'crinil))))))
    nil))

(defun crihelp=multi-most-similar-node (formula nodes)
  (when (and (listp nodes) (not (null nodes)))
    (let* ((formula-alist (cri=analyze formula nil))
           (pre-result
              (apply #'append
                     (mapcar #'(lambda (node)
                                 (let ((res (cri=get-most-similar-subterm-of-ass formula-alist node)))
                                   (when res
				 (list res))))
                             nodes)))
             (sorted (sort pre-result #'(lambda (match1 match2)
                                          (cond ((> (third match1) (third match2))
					     T)
                                                ((= (third match1) (third match2))
                                                 (let* ((dep1 (logic~depth (node~formula (first match1))))
                                                        (dep2 (logic~depth (node~formula (first match2))))
                                                        )
                                                   (< dep1 dep2)))
					    (T nil))))))
      (if sorted
	  (first (first sorted))
	nil))))

(defun crihelp=new-name (number)
  (declare (edited  "15-MAR-1999")
	   (authors Ameier)
	   (input   "A number")
	   (effect  "None.")
	   (value   "Returns 'L(label-counter+number)"))
  (let* ((count (+ number (keim::pds=label-counter omega*current-proof-plan))))
    (make-symbol (format nil "L~A" count))))

;; --------------------------------------------------------------> AttackInequality

(defun MULTImost-similar-subterm-in-asss-to-goal (args)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  ;; Args: a planning goal, a string for the assumptions and a string for the positions
  ;; the function searches in the assumptions for the most similar subterm for goal
  ;; where the most similar subterm is definied as the term with shares the most
  ;; fct-symbols with the goal
  ;; goal can be bound, ass and subterm not
  ;; NOTE: This metapredicate returns at most two of the most similar assumptions
  ;;       to limit the search space.
  (if (stringp (first args)) ;; goal is not bound
      (mapcan #'(lambda (task)
		  (crihelp=MULTI-most-similar-help (cons (agenda~task-node task)
							 (rest args))))
	      cri*current-tasks)
    (let ((result (crihelp=MULTI-most-similar-help args)))
      ;;(format t "~& final result: ~S ~S" args result)
      result)))

(defun crihelp=MULTI-most-similar-help (args)
  (let* (;;(task (first args))
	 ;;(goal (agenda~task-node task))
	 (goal (first args))
	 (ass-string (second args))
	 (pos-string (third args))
	 (goal-alist (cri=analyze (node~formula goal)
				  nil))
	 (supp (pds~node-supports goal cri*current-pds))
	 (goal-hyps (pdsn~hyps goal))
	 (right-supp (mapcan #'(lambda (node)
				 (when (subsetp (pdsn~hyps node) goal-hyps)
				   (list node)))
			     supp))
	 
	 (pre-result
	  (apply #'append
		 (mapcar #'(lambda (assumption)
			     (let ((res (crihelp=MULTI-get-most-similar-subterm-of-ass goal-alist
										 assumption)))
			       (when res
				 (list res))))
			 right-supp)))
	 (sorted (sort (remove-if #'(lambda (match)
				      (if (null (first match))
					  't
					nil))
				  pre-result)
		       #'(lambda (match1 match2)
			   (cond ((> (third match1) (third match2))
				  T)
				 ((= (third match1) (third match2))
				  (let* ((dep1 (logic~depth (node~formula (first match1))))
					 (dep2 (logic~depth (node~formula (first match2))))
					 )
				    (< dep1 dep2)))
				 (T nil)))))
	 (check-sorted (mapcar #'(lambda (match)
				   (list (first match) (crihelp=in-negation-position (node~formula (first match)) (second match))))
			       sorted))
	 (empty-pos (remove-if-not #'(lambda (match)
				       (pos~empty-p (second match)))
				   check-sorted))
	 (not-empty-pos (remove-if #'(lambda (match)
				       (pos~empty-p (second match)))
				   check-sorted))
	 (sortedii (append not-empty-pos empty-pos))
	 (matchings (mapcar #'(lambda (res)
				(list (cons ass-string (first res))
				      (cons pos-string (second res))))
			    sortedii))
	 )
    (if (> (length matchings) 2)            ;; return at most three alternative matchings
	(list (first matchings) (second matchings) (third matchings))
      matchings)))

(defun crihelp=in-negation-position (formula pos)
  (let* ((pos-length (length (pos~number-list pos)))) 
    (do* ((i pos-length (decf i))
	  (neg-flag nil))
	((or (= i 0)
	     neg-flag)
	 (if neg-flag
	     (pos~butlast pos (+ i 1))
	   pos))
      (let* ((curr-pos (pos~butlast pos i))
	     (term-at-pos (data~struct-at-position formula curr-pos)))
	(when (logic~negation-p term-at-pos)
	  (setf neg-flag 't))))))

(defun crihelp=MULTI-get-most-similar-subterm-of-ass (goal-alist ass)
  (let* ((ass-formula (node~formula ass))
	 (subformulas (crihelp=MULTI-get-positive-subformulas ass-formula))
	 )
    ;;(format t "~% subs: ~S" subformulas)
    (if subformulas
	(let ((positions (mapcar #'(lambda (formula)
				     (first (last (data~substruct-positions formula
									    ass-formula))))
				 subformulas))
	      (result (list nil nil -100)))
	  (dolist (pos positions result)
	    (setf result
		  (cri=extract-and-better? ass
					   goal-alist
					   pos
					   result))
	    )
	  ;;(format t "~% ass result : ~S ~S" ass result)
	  result)
      ;;(when (> (third result) -49) ;; -49 for cont-x^4 example.... :-(
      ;;  result))
      nil)))

(defun crihelp=MULTI-get-positive-subformulas (formula)
  (cond ((null formula)
	 NIL)
	((listp formula)
	 (apply #'append (mapcar #'crihelp=MULTI-get-positive-subformulas formula)))
	((term~abstr-p formula)
	 (crihelp=MULTI-get-positive-subformulas (data~abstr-range formula)))
	((logic~implication-p formula)
	 (crihelp=MULTI-get-positive-subformulas (second (data~appl-arguments formula))))
	((logic~conjunction-p formula)
	 (crihelp=MULTI-get-positive-subformulas (data~appl-arguments formula)))
	((term~appl-p formula)
	 (if (or (data~equal (data~appl-function formula)
			     (post~read-object 'leq (pds~environment
						     omega*current-proof-plan)
					       :existing-term))
		 (data~equal (data~appl-function formula)
			     (post~read-object 'less (pds~environment
						      omega*current-proof-plan)
					       :existing-term)))
	     (list formula)
	   (crihelp=MULTI-get-positive-subformulas (data~appl-arguments formula))))
	(T nil)))

(defun mmatching-is-myset-focus-but-nothing-new-p (mmatching)
  ;; mmatchings stehen in cri*current-mmatchings drin ...
  (let* ((mmatchings-with-myset-focus-m-b (remove-if-not #'(lambda (mmatching)
							     (eq (pplan~matched-method mmatching)
								 (meth~find-method 'MSET-FOCUS-M-B)))
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
	   (input   "A mmatching for method mset-focus-m-b.")
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


(defun mmatching-is-myset-focus-but-at-wrong-position-p (mmatching)
  ;; mmatchings stehen in cri*current-mmatchings drin ...
  (let* ((mmatchings-with-myset-focus-m-b (remove-if-not #'(lambda (mmatching)
							     (eq (pplan~matched-method mmatching)
								 (meth~find-method 'MSET-FOCUS-M-B)))
							 cri*current-mmatchings))
	 (mmatchings-with-myset-focus-m-b-but-at-wrong-position (remove-if-not #'(lambda (mmatching)
										   (myset-focus-in-lim-or-cont-at-forbidden-position-p mmatching))
									       mmatchings-with-myset-focus-m-b)))
    
    (mapcar #'(lambda (matchi)
		(list (cons mmatching matchi)))
	    mmatchings-with-myset-focus-m-b-but-at-wrong-position)))


(defun myset-focus-in-lim-or-cont-at-forbidden-position-p (mmatching)
  (declare (edited  "28-JAN-2003")
	   (authors Ameier)
	   (input   "A mmatching.")
	   (effect  "None.")
	   (value   "T if the matching suggest to unwrap a position in a limes or cont structure"
		    "different from the final position."))
  (let* ((envi (env~create (pds~environment omega*current-proof-plan)))
	 (vars (post~read-object '((var1 o) (var2 o) (var3 o) (var4 o)) envi :variables))
	 (limcont-term1 (post~read-object '(forall (lam (e num)
							(implies (less 0 e)
								 (exists (lam (d num)
									      (and (less 0 d)
										   (forall (lam (x num)
												(implies var1
													 var2)))))))))
					  envi
					  :existing-term
					  ))
	 (limcont-term2 (post~read-object '(forall (lam (e num)
							(exists (lam (d num)
								     (forall (lam (x num)
										  (implies (less 0 e)
											   (and (less 0 d)
												(implies var3
													 var4)))))))))
					  envi
					  :existing-term
					  ))
	 (ass (first (pplan~mmatch-exist-prems mmatching)))
	 (pos (first (pplan~mmatch-parameters mmatching)))
	 (formula (node~formula ass)))

    (cond ((term~alpha-unify formula limcont-term1)
	   ;; if the focus formula is a limit or a cont thing of the first version
	   ;; then check whether the position is (1 0 2 1 0 2 1 0 2)
	   (if (keim~equal pos (pos~list-position '(1 0 2 1 0 2 1 0 2)))
	       ;; allowed position
	       nil
	     ;; forbidden position
	     't))
	  ((term~alpha-unify formula limcont-term1)
	   ;; if the focus formula is a limit or a cont thing of the first version
	   ;; then check whether the position is (1 0 1 0 1 0 2 2 2)
	   (if (keim~equal pos (pos~list-position '(1 0 1 0 1 0 2 2 2)))
	       ;; allowed position
	       nil
	     ;; forbidden position
	     't))
	  (t
	   nil))))



(defun mmatching-is-myset-focus-but-for-node-from-unwraphyp-p (mmatching)
  ;; mmatchings stehen in cri*current-mmatchings drin ...
  (let* ((mmatchings-with-myset-focus-m-b (remove-if-not #'(lambda (mmatching)
							     (eq (pplan~matched-method mmatching)
								 (meth~find-method 'MSET-FOCUS-M-B)))
							 cri*current-mmatchings))
	 (mmatchings-with-myset-focus-m-b-but-for-wrong-node (remove-if-not #'(lambda (mmatching)
										   (crihelp=myset-focus-for-node-from-unwraphyp-p mmatching))
									       mmatchings-with-myset-focus-m-b)))
    
    (mapcar #'(lambda (matchi)
		(list (cons mmatching matchi)))
	    mmatchings-with-myset-focus-m-b-but-for-wrong-node)))


(defun crihelp=myset-focus-for-node-from-unwraphyp-p (mmatching)
  (let* ((ass (first (pplan~mmatch-exist-prems mmatching)))
	 (just (node~justification ass))
	 (method (just~method just)))
    (cond ((or (eq method (infer~find-method 'FORALLE-META-M))
	       (eq method (infer~find-method 'impe)))
	   't)
	  ((eq method (infer~find-method 'hyp))
	   (let* ((other-reasons (pdsj~other-reasons just))
		  (other-reason-with-existse (remove-if-not #'(lambda (reason)
								(let* ((just-of-reason (pdsc~an-just reason))
								       (method-of-reason (just~method just-of-reason)))
								  (if (eq method-of-reason (infer~find-method 'existse-m))
								      't
								    nil)))
							    other-reasons)))
	     (if other-reason-with-existse
		 't
	       nil)))
	  (t
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


		     
;;(defun focus-p (goal)
;;  (let* ((focus (black~get-blackboard-object-content 'focus sod*solution-blackboard)))
;;    (if focus
;;	(let* ((focus-goal-node-name (first focus))
;;	       (goal-node (pds~label2node focus-goal-node-name))
;;	       (task-with-goal-node (first (remove-if-not #'(lambda (task)
;;							      (let* ((task-node (agenda~task-node task)))
;;								(eq task-node goal-node)))
;;							  cri*current-tasks))))
;;	  (if task-with-goal-node
;;	      ;; Usually, focus is set after the MSET-FOCUS Method id applied (in the appl conditions?).
;;	      ;; Then there should be a goal-node and task-with-goal-node
;;	      ;; But during Analogy it can happen that the MSET-FOCUS Method is also applied, but afterwards the
;;	      ;; unwrapping is done directly without removing the focus!
;;	      ;; In this case it can happen that there ist the goal-node but no task associated with this goal-node.
;;	      ;; However, in this case we just return nil, since the unwrapping is done already by analogy!
;;	      (list (list (cons goal task-with-goal-node)))
;;	    nil))
;;      nil)))

(defun focus-p (goal ass pos)
  (let* ((last-plan-step (pds~last-plan-step omega*current-proof-plan)))
    (if (null last-plan-step)
	nil
      (let* ((last-just (pdsc~an-just last-plan-step))
	     (last-method (just~method last-just)))
	(if (eq (infer~find-method 'mset-focus-m) last-method)
	    ;; the last step was an application of mset-focus-m
	    (let* ((goal-node (second (just~premises last-just)))
		   (ass-node (first (just~premises last-just)))
		   (position (first (pdsj~parameters last-just)))
		   (task-with-goal-node (first (remove-if-not #'(lambda (task)
								  (let* ((task-node (agenda~task-node task)))
								    (eq task-node goal-node)))
							      cri*current-tasks))))
	      (list (list (cons goal task-with-goal-node)
			  (cons ass ass-node)
			  (cons pos position))))
	  nil)))))

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

(defun term-is-not-absval-p (term)
  (if (stringp term)
      (progn
	(omega~warning "~%ERROR in metapred term-is-not-absval-p: input argument term has to be bound.")
	nil)
    (let* ((substructs (data~all-substructs term))
	   (absval (env~lookup-object 'absval (pds~environment omega*current-proof-plan))))
      
      (if (find absval substructs :test #'keim~equal)
	  nil
	(list (list (cons T T)))))))

(defun cosie-task-p (task)
  (if (stringp task)
      ;; -> task is unbound
      (let* ((tasks cri*current-tasks)
	     (cosie-tasks (remove-if-not #'(lambda (taski)
					     (let* ((node (agenda~task-node taski)))
					       (strathelp=goal-for-CoSIE-p node)))
					 tasks)))
	(mapcar #'(lambda (cosie-task)
		    (list (cons task cosie-task)))
		cosie-tasks))
    (if (strathelp=goal-for-CoSIE-p (agenda~task-node task))
	(list (list (cons T T)))
      nil)))

(defun not-cosie-task-p (task)
  (if (stringp task)
      ;; -> task is unbound
      (let* ((tasks cri*current-tasks)
	     (not-cosie-tasks (remove-if #'(lambda (taski)
					     (let* ((node (agenda~task-node taski)))
					       (strathelp=goal-for-CoSIE-p node)))
					 tasks)))
	(mapcar #'(lambda (not-cosie-task)
		    (list (cons task not-cosie-task)))
		not-cosie-tasks))
    (if (strathelp=goal-for-CoSIE-p (agenda~task-node task))
	nil
      (list (list (cons T T))))))



(defun task-is-not-from-unwrap (task)
  (if (stringp task)
      ;; -> task is unbound
      (let* ((tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	     (tasks-not-from-unwrap (remove-if #'(lambda (taski)
						   (crihelp=task-from-unwrap-p taski))
					       tasks)))
	(mapcar #'(lambda (not-unwrap-task)
		    (list (cons task not-unwrap-task)))
		tasks-not-from-unwrap))
    
    ;; -> task is bound
    (if (crihelp=task-from-unwrap-p task)
	(list (list (cons T T)))
      nil)))

(defun crihelp=task-from-unwrap-p (task)
  (let* ((node (agenda~task-node task)))
    (crihelp=node-from-unwrap-p node)))

(defun crihelp=node-from-unwrap-p (node)
  (let* ((just (node~justification node))
	 (other-reasons (pdsj~other-reasons just))
	 (other-reasons-from-unwrap (remove-if-not #'(lambda (oreason)
						       (crihelp=reason-from-unwrap-p oreason node))
						   other-reasons)))
    (if other-reasons-from-unwrap
	't
      nil)))
	 
    

(defun crihelp=reason-from-unwrap-p (reason node)
  (let* ((just (pdsc~an-just reason))
	 (method (just~method just))
	 (premises (just~premises just)))
    (cond ((and (or (string-equal (keim~name method) 'ImpE-Open-m)
		    (string-equal (keim~name method) 'ImpE-open-m-a)
		    (string-equal (keim~name method) 'OrEL-open-m)
		    (string-equal (keim~name method) 'OrEL-open-m-a)
		    (string-equal (keim~name method) 'OrER-open-m)
		    (string-equal (keim~name method) 'OrER-open-m-a))
		(eq node (first premises)))
	   't)
	  ((and (or (string-equal (keim~name method) 'Andi-m)
		    (string-equal (keim~name method) 'AndI-m-b))
		(or (eq node (first premises))
		    (eq node (Second premises))))
	   (crihelp=node-from-unwrap-p (pdsc~an-node reason)))
	  (t
	   nil))))

		 
