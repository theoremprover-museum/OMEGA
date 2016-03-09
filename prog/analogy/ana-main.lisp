;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; ana-main.lisp; This file is part of the OMEGA system
;;
;; major updates: 4.10.1999
;; 
;;
;; Authors: Carsten Ullrich
;; email: cullrich@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; the main algorithm for analogy

(in-package "OMEGA")

(mod~defmod ANA 
            :uses (agenda beta cri data env infer just keim lam meth node omega pds pdsc pdsj pdsn plan prob subst)
            :documentation "The analogy algorithm"
            :exports (ana+nodes-correspondance-table
                      ana+step
                      
                      ana~add-corresponding-nodes
                      ana~add-nodes-for-steps
                      ana~advance-planning-step
                      ana~apply-method-on
                      ana~apply-mmatching
                      ana~change-corresponding-nodes
		      ana~choose-match
                      ana~construct-correspondance-table
                      ana~exa
                      ana~exa-strategy
                      ana~first-planning-step
                      ana~get-add-conclusions
                      ana~get-add-hyps
                      ana~get-add-lines-for-operator
                      ana~get-add-premises
                      ana~get-corresponding-nodes
                      ana~get-exist-conclusions
                      ana~get-exist-premises
                      ana~get-methods
                      ana~get-nodes-for-method-nodes
                      ana~get-open-goals
                      ana~get-preconditions-for-operator
                      ana~get-source-conclusion
                      ana~get-source-method
                      ana~get-source-parameters
                      ana~get-source-premises
                      ana~get-target-nodes
                      ana~initialize-correspondance-table
                      ana~just-method
                      ana~match-step
                      ana~match-step-with-node
                      ana~plan-one-bw-step
                      ana~plan-one-fw-step
                      ana~print-table
                      ana~set-step-method!
                      ana~step-goal
                      ana~step-method
                      ana~step-mmatching
                      ana~step-parameters
                      ana~step-premises
                      ana~step-source-goal
                      ana~step-source-parameters
                      ana~step-source-premises
                      ana~step-subst
                      ana~step-target-goal
                      ana~step-target-parameters
                      ana~step-target-premises
                      ana~try-ref
                      ana~update-table

		      ana*skiplimit
                      ana*goal-step
                      ana*ina-steps
                      ana*initial-match-cost
		      ana*interactive-mode
                      ana*internal-analogy
                      ana*max-repair-depth
                      ana*max-subgoal-depth
                      ana*processed-nodes
;                      ana*step-mode
                      ana*table-verbose
		      ana*source-plan
                      ))

;;===============================
;; the main analogy algorithm
;;===============================

(defvar ana*source-plan nil "Needed for Multi: gives the source pds")

(defvar ana*interactive-mode nil)

(defvar ana*internal-analogy nil "Indicates whether we are in internal analogy mode")

(defvar ana*processed-nodes nil)

(defvar ana*ina-steps nil "Saves the source plan for internal analogy.")

(defvar ana*max-subgoal-depth 3
  "How many subgoals should be looked at?")

(defvar ana*goal-step nil "Needed for planning reformulations.")

;(defvar ana*step-mode nil "Wait for <RETURN> after each transferred step.")

(defvar ana*initial-match-cost '(1 5 100) 
  "The maximum allowed costs of the initial repair-match/plan phase.")

(defvar ana*skiplimit 3 "The limit of nodes to skip.")

(defvar ana*current-step nil "The step to be transfered (need for c-rules)")

(defvar ana*max-repair-depth 2 "The number of steps that can be inserted for adaptions.")

;;for experiments:

(defvar ana*no-semantic-choice nil "Toggles whether nodes should be selected by method application.")

(defvar ana*use-node-table t "Toggles whether node-tables should be used.")

(defvar ana*applied-refs 0 "The number of applied reformulations.")

(defclass ana+step ()
  ((method :initform nil :initarg :method
	   :accessor ana~step-method
	   :documentation "The method of this step.")
   (goal :initform nil :initarg :goal
	 :accessor ana~step-goal
	 :documentation "The goal of this step (source and target).")
   (premises :initform nil :initarg :premises
	     :accessor ana~step-premises
	     :documentation "The premises of this step (source and target).")
   (parameters :initform nil :initarg :parameters
	       :accessor ana~step-parameters
	       :documentation "The parameters of this step (source and target.")
   (subst :initform (subst~create nil nil) :initarg :subst
	  :accessor ana~step-subst
	  :documentation "The substitution of this step.")
   (mmatching :initform nil :initarg :mmatching
	      :accessor ana~step-mmatching
	      :documentation "The matching returned from the planner.")
   ))


(defgeneric ana~set-step-method! (thing value)
  (:method ((step ana+step) value)
	   (setf (ana~step-method step) value)))

(defgeneric ana~step-source-goal (thing)
  (:method ((step ana+step))
	   (first (ana~step-goal step))))

(defgeneric ana~step-target-goal (thing)
  (:method ((step ana+step))
	   (second (ana~step-goal step))))

(defgeneric ana~step-source-premises (thing)
  (:method ((step ana+step))
	   (mapcar #'first (ana~step-premises step))))

(defgeneric ana~step-target-premises (thing)
  (:method ((step ana+step))
	   (mapcar #'second (ana~step-premises step))))

(defgeneric ana~step-source-parameters (thing)
  (:method ((step ana+step))
	   (first (ana~step-parameters step))))

(defgeneric ana~step-target-parameters (thing)
  (:method ((step ana+step))
	   (second (ana~step-parameters step))))


(defgeneric ana=copy-step (step)
  (:method ((step ana+step))
	   (make-instance 'ana+step
			  :method (ana~step-method step)
			  :goal (ana~step-goal step)
			  :premises (ana~step-premises step)
			  :parameters (ana~step-parameters step)
			  :subst (ana~step-subst step))))
		 
(defmethod print-object ((object ana+step) (stream stream))
  (format stream "~a" (list "method: " (ana~step-method object)
			    "goal: " (ana~step-goal object)
			    "premises: " (ana~step-premises object)
			    "mmatching: " (ana~step-mmatching object))))

(defun ana~exa-strategy (target-node)
  (declare (edited  "13-FEB-2001")
	   (authors Cullrich)
	   (input   "A target node")
	   (effect  "Analogy as a strategy: Try to solve this problem by analogy")
	   (value   "A (partly) solved pds"))
  (if (ana~get-open-goals omega*current-proof-plan)
      (let ((start-time (get-internal-run-time)))
    (omega~message "~%Starting external analogy!")
    (setf ana*processed-nodes nil)
    (setf ana*matched-nodes 0)
    (setf ana*applied-refs 0)
    (setf plan*already-done nil) ;;needed in order to not instantiate the metavars a second time
    (ana~variabelize-reset)
    (let* ((target omega*current-proof-plan)
	   (open-nodes (pds~open-nodes target)))
      (setf (pds~open-nodes target) (list target-node))
      (let* (;; the source plan is given by this global variable
	   (source-plan ana*source-plan)
	   (user-subst (ana=get-user-propositions source-plan target))
	   (goals (ana=repair-match-or-plan source-plan target user-subst))
	   ;; initialize the correspondance table:
	   (node-table (ana~initialize-correspondance-table
			;; set the source and target goals/assumptions in correspondance:
			(list (list (ana~matcher-source-node goals)
				    (list (ana~matcher-target-node goals)))
			      (list (remove (ana~matcher-source-node goals)
					    (pds~support-nodes source-plan))
				    (union (pds~support-nodes target)
					   (pdsn~just-sponsors (first (pds~open-nodes
								       target))))))))
	   )
      (setf meth*restricting-methods nil)
      (setf omega*current-proof-plan target
	    keim::pds*current-proof-plan target)
      ;; needed because the retrieval changes these variables
      (ana=transfer-step target source-plan (ana~first-planning-step source-plan)
			 (subst~compose-subst-nso user-subst (ana~matcher-subst goals))
			 node-table)
      ;; we have completely transfered the source-plan
      (omega~message "Replay tooked ~F seconds." (/ (- (get-internal-run-time)
						       start-time)
						    internal-time-units-per-second))
      (omega~message "Matched ~a nodes." ana*matched-nodes)
      (omega~message "Matching attemps: ~a~T Applied methods: ~a~%Open nodes: ~a~%"
		     plan*matching-attempts
		     plan*number-of-applied-methods
		     (length (pds~open-nodes omega*current-proof-plan)))
      (omega~message "Semantic :~a~T Table:~a" (not ana*no-semantic-choice)
		     ana*use-node-table)
      (setf (pds~open-nodes target) (remove target-node open-nodes))
      (ana=finished target))))
    (omega~message "There are no open nodes, analogical transfer is not needed.")))

(defun ana~exa (&optional (target omega*current-proof-plan) source)
  (if (ana~get-open-goals omega*current-proof-plan)
      (let ((start-time (get-internal-run-time)))
    (omega~message "~%Starting external analogy!")
    (setf ana*processed-nodes nil)
    (setf ana*matched-nodes 0)
    (setf ana*applied-refs 0)
    (setf plan*already-done nil) ;;needed in order to not instantiate the metavars a second time
    (ana~variabelize-reset)
    (let* (;; retrieve the source plan:
	   (source-plan (ana=retrieve-source target source))
	   (user-subst (ana=get-user-propositions source-plan target))
	   (goals (ana=repair-match-or-plan source-plan target user-subst))
	   ;; initialize the correspondance table:
	   (node-table (ana~initialize-correspondance-table
			;; set the source and target goals/assumptions in correspondance:
			(list (list (ana~matcher-source-node goals)
				    (list (ana~matcher-target-node goals)))
			      (list (remove (ana~matcher-source-node goals)
					    (pds~support-nodes source-plan))
				    (union (pds~support-nodes target)
					   (pdsn~just-sponsors (first (pds~open-nodes
								       target))))))))
	   )
      (setf meth*restricting-methods nil)
      (setf omega*current-proof-plan target
	    keim::pds*current-proof-plan target)
      ;; needed because the retrieval changes these variables
      (ana=transfer-step target source-plan (ana~first-planning-step source-plan)
			 (subst~compose-subst-nso user-subst (ana~matcher-subst goals))
			 node-table)
      ;; we have completely transfered the source-plan
      (omega~message "Replay tooked ~F seconds." (/ (- (get-internal-run-time)
						       start-time)
						    internal-time-units-per-second))
      (omega~message "Matched ~a nodes." ana*matched-nodes)
      (omega~message "Matching attemps: ~a~T Applied methods: ~a~%Open nodes: ~a~%"
		     plan*matching-attempts
		     plan*number-of-applied-methods
		     (length (pds~open-nodes omega*current-proof-plan)))
      (omega~message "Semantic :~a~T Table:~a" (not ana*no-semantic-choice) ana*use-node-table)
      (ana=finished target)))
    (omega~message "There are no open nodes, analogical transfer is not needed.")))

(defun ana=get-user-propositions (source target)
  ;; the user can enter propositions for matchings:
  (if ana*interactive-mode
      (progn
	(omega~message "The source theorem: ~S:~s~%The target theorem: ~S:~s"
		       (prob~proof-root source)(node~formula (prob~proof-root source))
		       (prob~proof-root target)(node~formula (prob~proof-root target)))
	(let* ((user (omega~query "Please enter propositions for the matching (e.g. ((times plus)(a b)) ): "
				  'anything))
	       (domain (when (consp user)
			 (mapcar #'(lambda (sym)
				     (ana~variabelize
				      (env~lookup-object (first sym)
							 (prob~proof-environment source))))
				 user)))
	       (codomain (when (consp user)
			   (mapcar #'(lambda (sym)
				       (env~lookup-object (second sym)
							  (prob~proof-environment target)))
				   user))))
	  (if (and (or (not domain)
		       (every #'data~object-p domain))
		   (or (not codomain)
		       (every #'data~object-p codomain)))
	      (subst~create domain codomain)
	    (progn
	      (omega~warn "Some propositions you entered were not valid terms.")
	      (ana=get-user-propositions source target)))))
    ;; no userinteraction wanted:
    (subst~create nil nil)))
  
(defun ana=repair-match-or-plan (source target subst)
  (progn
;    (omega~message "====================================================")
    (omega~message "Now analyzing the correspondance between the goals:")
    (ana=repair-match-or-plan-h target source
				(ana=remove-non-applicable
				 (list (prob~proof-root source))
				 target (pds~open-nodes target))
				subst 0)
    ))

(defun ana=repair-match-or-plan-h (target source source-subgoals subst depth)
  (let* ((match-result-h
	  (ana~choose-match
	   (ana~match source-subgoals (pds~open-nodes target) subst ana*initial-match-cost)))
	 (match-result (if match-result-h match-result-h
			 (if (and (not match-result-h) source-subgoals)
			     (progn
			       (omega~message "First step is applicable, but no match could be found.")
			       (ana~create-match-problem :source-node (first source-subgoals)
							 :target-node (first (pds~open-nodes target))
							 :subst (subst~create nil nil)))))))
    (if match-result
	(progn
	  (omega~message "Proposing result:~%Source goal ~s with formula ~s~%corresponds to target goal ~s with formula ~s~% with the substitution ~s,term maps ~S and repairs ~s."
			 (ana~matcher-source-node match-result)
			 (node~formula(ana~matcher-source-node match-result))
			 (ana~matcher-target-node match-result)
			 (node~formula(ana~matcher-target-node match-result))
			 (ana~matcher-subst match-result)
			 (ana~matcher-term-maps match-result)
			 (ana~matcher-repairs match-result))
	  (if (or (not ana*interactive-mode)
		  (ana=y-or-n-p "Do you accept these settings? "))
	      match-result
	    (progn
	      (omega~message "The first source goals:")
	      (mapcar #'(lambda (node)
			  (omega~message "Node ~s with formula ~s" (keim~name node)
					 (node~formula node)))
		      source-subgoals)
	      (ana=get-user-correspondings source target subst))))
      (if (< depth ana*max-subgoal-depth)
	  (ana=repair-match-or-plan-h target source
				      (ana=get-goals-of-depth source (1+ depth))
				      subst (1+ depth))
	(progn
	  (omega~message "No correspondance between the goals found... inserting a planning step.")
	  (ana=repair-match-or-plan-h (ana~plan-one-bw-step target)
				      source
				      (ana=remove-non-applicable
				       (list (prob~proof-root source))
				       target (pds~open-nodes target))
				      subst 0))))))

(defun ana=remove-non-applicable (source-goals target goals)
  (remove-if-not #'(lambda (node)
		     (or (not ;;forward methods can sometimes not be applied as premises may be missing
			  (ana=backward-method-p
			   (ana~just-method (node~justification node))))
			 (meth~parameters (ana~just-method (node~justification node)))
			 ;; the old problem with parameters...
			 (ana~method-is-matching
			  (ana~just-method (node~justification node))
			  goals
			  (apply #'append
				 (mapcar #'pdsn~hyps goals))
			  (pdsn~just-parameters node) target)))
		 source-goals))
  
(defun ana=get-user-correspondings(source target subst)
  (let* ((source-label (omega~query "Please enter the label of the source goal:" 'symbol))
	 (target-label (omega~query "Please enter the label of the target goal:" 'symbol))
	 (source-node (pds~label2node source-label source))
	 (target-node (pds~label2node target-label target)))
    (if (and (pdsn~p source-node)(pdsn~p target-node))
	(let ((match-result
	       (ana~choose-match (ana~match (list source-node) (list target-node)
					    subst))))
	  (omega~message "Proposing result:~%Source goal ~s with formula ~s~%corresponds to target goal ~s with formula ~s~% with the substitution ~s, term-maps ~s and repairs ~s."
			 (ana~matcher-source-node match-result)
			 (node~formula(ana~matcher-source-node match-result))
			 (ana~matcher-target-node match-result)
			 (node~formula(ana~matcher-target-node match-result))
			 (ana~matcher-subst match-result)
			 (ana~matcher-term-maps match-result)
			 (ana~matcher-repairs match-result))
	  match-result)
      (progn
	(omega~message "Please enter valid labels.")
	(ana=get-user-correspondings source target subst)))))
      
    
(defun ana=transfer-step (target-plan source-plan planning-step substitution node-table)
  (if (and planning-step
	   (not (agenda~empty-p (pds~agenda target-plan)))
	   (pds~open-nodes target-plan)
	   (not (every #'(lambda (node)
			   (get 'lemma node));;check if all open nodes
		       ;;are inserted nodes
		       (pds~open-nodes target-plan))))
;      (progn
;      (omega~message "Searching for the next planning step to be transferred.")
    ;; if there are still planning steps, then
    (let* (;; first get the informations from the source planning step:
	   (source-method (ana~get-source-method planning-step))
	   (source-conclusion (ana~get-source-conclusion source-method planning-step))
	   (source-premises (ana~get-source-premises source-method planning-step))
	   (source-parameters (ana~get-source-parameters source-method planning-step)))
      (omega~message "Next source-step: Method ~a on conclusion ~a and premises ~a with parameter ~a"
		   source-method source-conclusion source-premises source-parameters)
;      (when ana*step-mode (progn (omega~message "Press <ENTER> to proceed.")(ana=read-line)))
      ;; then calculate all target steps
      ;; and try yo apply one of them
      (ana=apply-step (ana=calculate-all-steps source-method source-conclusion
					       source-premises source-parameters
					       substitution node-table)
		      target-plan source-plan planning-step substitution node-table)
      )
    t))

(defun ana=calculate-all-steps (method conclusion premises parameters subst node-table)
  ;; calculate for the conclusions
  (let* ((dummy (list 
		 (make-instance 'ana+step :method method :parameters
				(list parameters
				      (ana=get-target-parameters parameters
								 subst
								 node-table)))))
	 (conclusion-steps
	  (ana=calc-h dummy
		      conclusion
		      (remove-if-not #'pdsn~open-node-p
				     (ana~get-corresponding-nodes conclusion node-table))
		      #'(lambda (step sg tg)
			  (setf (ana~step-goal step) (list sg tg))))))
    (if conclusion-steps
	(ana=calc-h2 conclusion-steps premises node-table)
      (ana=calc-h2 dummy premises node-table))))

;(defun ana=was-open-node (node method subst)
;  (eq (keim~name node)
;      (keim~name (first (ana=get-specified (meth~exist-conclusions method) subst)))))

(defun ana=calc-h (old obj1 objs2-h slot1)
  (let ((result nil)
	;; if there is a source object (obj1) and no corresponding target object, then
	;; mark it as missing
	(objs2 (or objs2-h (when obj1 (list 'missing))))
	)
    (dolist (obj2 objs2 result)
      (setf result
	    (append
	     (mapcar #'(lambda (old-obj)
			 (let ((step (ana=copy-step old-obj)))
			   (apply slot1 (list step obj1 obj2))
			   step))
		     old)
	     result)))
    result
    ;;if no new steps are calculated return a dummy one (needed for
    ;;example if the conclusions are nil)
    ))

(defun ana=calc-h2 (old nodes table)
  (if nodes
      (ana=calc-h2
       (ana=calc-h old (first nodes)
		   (ana~get-corresponding-nodes (first nodes) table)
		   #'(lambda (step obj1 obj2)
		       (setf (ana~step-premises step)
			     (cons (list obj1 obj2)
				   (ana~step-premises step)))))
       (rest nodes)
       table)
    old))

(defun ana=apply-step (all-steps target-plan source-plan planning-step substitution
				 node-table)
  (progn (omega~message "~s step~:[s are~; is~] calculated." (length all-steps)
		      (= (length all-steps) 1))
  (let ((applicable-steps
	 (ana=select-matching-steps all-steps target-plan)))
    (omega~message "~s step~:[s are~; is~] matching." (length applicable-steps)
		 (= (length applicable-steps) 1))
    (if applicable-steps
	(multiple-value-bind (sorted-steps new-subst)
	    (ana=sort-steps applicable-steps substitution)
	  (if (ana=step-plan (first sorted-steps) target-plan)
	      (ana=transfer-step target-plan source-plan
				 (ana~advance-planning-step planning-step)
				 new-subst
				 (ana~update-table (first sorted-steps)
						   planning-step target-plan
						   node-table))
	    ;; this case is needed only for experiments when the semantic mode is off
	    (ana=repair (first sorted-steps) target-plan source-plan planning-step
			new-subst node-table)
	    ))
      (progn ;(break)
	(omega~message "The first calculated step is ~s." (first all-steps))
	(multiple-value-bind (sorted-steps new-subst)
	    (ana=sort-steps all-steps substitution)
	  (ana=repair (first sorted-steps) target-plan source-plan planning-step
		      new-subst node-table)))))
  ))

(defun ana=sort-steps (steps subst)
  ;; sorts the steps corresponding to the cost of the matches
  (progn
    (when (> (length steps) 1)(omega~message "Sorting the steps."))
    (multiple-value-bind (choice sorted-steps new-subst)
	(ana=choose-by-matching steps #'(lambda (obj) (ana~step-target-goal obj))
				#'(lambda (obj) (list (ana~step-source-goal obj)))
				subst)
      (list sorted-steps) ;;in order to remove the compile message...
      (if (> (length choice) 1)
	  (ana=sort-steps-h 0 choice new-subst)
	(values choice new-subst)))))

(defun ana=sort-steps-h (n steps subst)
  ;; sorts the steps by matching the n'th premise
  (if (nth n (ana~step-premises (first steps))) ;; are there still premises to match?
      (multiple-value-bind (choice sorted-steps new-subst)
	  (ana=choose-by-matching steps #'(lambda (obj)
					    (nth n (ana~step-target-premises obj)))
				  #'(lambda (obj) (list (nth n (ana~step-source-premises obj))))
				  subst)
	(if (> (length choice) 1)
	    (ana=sort-steps-h (1+ n) sorted-steps new-subst)
	  (values choice new-subst)))
    (values steps subst)))

(defun ana=choose-by-matching (steps fct fct2 subst)
  (let ((target-nodes (remove-duplicates (mapcar fct steps))))
    (if (and (> (length target-nodes) 1) ;; then we can select with a match
	     (not (member 'missing target-nodes)))
	(let ((result (ana~choose-match
		       (ana~match (apply fct2 (list (first steps)))
					;(list (ana~step-source-goal (first steps))) ; the source term
				  target-nodes
				  subst))))
	  (if result (ana=select-choosen
		      (ana~matcher-target-node result)
		      steps fct
		      (keim::subst~compose-subst-nso
		       (ana~matcher-subst result)
		       subst))
	    (values steps steps subst)))
      (values steps steps subst))))

(defun ana=select-choosen (choosen objs fct subst)
  (let ((result1 ()) ;; the choosen
	(result2 ())) ;; the rest
    (dolist (obj objs)
      (if (eq (keim~name choosen) (keim~name (apply fct (list obj))))
	  (push obj result1)
	(push obj result2)))
    (values result1 (append result1 result2)
	    subst)))
  
(defun ana=select-matching-steps (steps plan)
  (if ana*no-semantic-choice ;; for experiments: toggle whether nodes should be selected
			     ;; by method application
      steps
    (let ((result ()))
      (dolist (step steps result)
	(let ((match? (ana~match-step step plan)))
	  (when match?
	    (setf (ana~step-mmatching step) (first match?))
	    (push step result)))))))

(defun ana=finished (target)
  (plan~show-data)
  (omega~message "We applied ~s reformulations." ana*applied-refs)
  (if (and (ana~get-open-goals target)
	   (not (every #'(lambda (node)
			   (get 'lemma node));;check if all open nodes
		       ;;are inserted nodes
		       (pds~open-nodes target))))
      (progn
	(omega~message "~%Replay for ~S is finished, but there are still
open nodes:~%~s~%" (keim~name target)(mapcar #'node~formula (ana~get-open-goals target)))
					;	(when (y-or-n-p "Shall I invoke the omega-planner? ")
					;	  (plan~plan target))
	)
    (progn
      (omega~message "~%Replay successfull! Target ~s is solved!"
		     (keim~name target))
      (when (pds~open-nodes target)
	(omega~message "But we had to insert ~:[s some~; a~] lemma."
		       (= (length (pds~open-nodes target)) 1)))
      (unless plan*already-done
	(plan~instantiate-metavars target)))
    )
  )

(defun ana=step-plan (step pds)
  ;; this function applies the planning step.
  (progn 
    (omega~message "Applying step ~S:" step)
    (let ((applied (ana~apply-mmatching (ana~step-target-goal step) step pds))
	)
      (when applied (omega~message "Transfer of planning step successfull."))
      applied
      )))
  
(defun ana=repair (step target-plan source-plan planning-step substitution
			node-table)
  (progn
    (incf ana*applied-refs)
    (omega~message "Step ~s could not be transfered! Repairing!" step)
;    (omega~message "Trying to repair!")
    ;; now checking for the different causes of failure:
    (let ((action (ana=apply-domain-knowledge target-plan source-plan step planning-step
					      substitution node-table)))
      (if action
	  (ana=apply-action action step target-plan source-plan planning-step
			    substitution node-table)
	(let ((node-missing? (ana=node-missing-p step)))
	  (if node-missing?
	      (ana=propose-node node-missing? target-plan source-plan step planning-step
				substitution node-table)
;        (let ((replace-method (ana=replace-applicable step)))
;          (if replace-method
;              (ana=replace step replace-method step target-plan source-plan
;                           planning-step substitution node-table)
	    (let ((skip (ana=skip-applicable step target-plan source-plan node-table substitution)))
	      (if skip
		  (ana=skip-step skip step target-plan source-plan planning-step
				 substitution node-table)
		(let ((ref ;(and (not ana*internal-analogy)
		       (ana=reformulate step target-plan source-plan planning-step
					substitution node-table)))
		  (if ref
		      (ana=transfer-step target-plan source-plan planning-step
					 substitution 
					 ;; set the problem node in correspondance with the just created nodes
					 (ana~add-nodes-for-steps
					  planning-step
					  (pds~last-plan-step target-plan)
					  source-plan
					  target-plan
					  node-table
					  ))
		    (ana=skip-step nil step target-plan source-plan planning-step
				 substitution node-table)))
		))))))))

;;========================================
;; reformulations
;;========================================

(defun ana=apply-domain-knowledge (target-plan source-plan step planning-step
					       substitution node-table)
  (declare (ignore target-plan source-plan planning-step
		   substitution node-table))
  (let* (;(cri*applied-rules nil)
	 (ana*current-step step)
	 (result (cri~call '(nil)
			   :kind 'adaption)))
    ;(and (notcri*applied-rules result)))
    (if (first result) result)))

(defun ana=apply-action (action step target-plan source-plan planning-step
				substitution node-table)
  (case (first action)
;    ('insert_lemma)
    ('replace-current-method_by
     (progn
       (ana~set-step-method! step
			     (meth~find-method (second action)))
       (ana=apply-step (list step) target-plan source-plan planning-step
		       substitution node-table)))
    ('skip-step (ana=transfer-step target-plan source-plan
				   (ana~advance-planning-step planning-step)
				   substitution node-table))
    ('insert-steps
     (let ((table 
	    (mapcar #'(lambda (method)
			(progn (ana=apply-method method target-plan nil nil nil)
			       (ana~add-nodes-for-steps
				planning-step
				nil nil
				target-plan node-table)))
		    (second action))))
       (ana=transfer-step target-plan source-plan planning-step substitution table)
       ))
    (t (omega~error "Undefined adaption action!!"))))

(defun ana=node-missing-p (step)
  ;; we check the premises for missing nodes (only the premises as we don't want to
  ;; propose missing open nodes)
  (let ((result
	 (some #'(lambda (nodes)
		   ;; if there is no target node, return the corresponding source-node
		   (when (eq (second nodes) 'missing) (first nodes)))
	       (ana~step-premises step))))
    (omega~message "Now checking if there is a premises missing...")
    (if result (omega~message "... premises missing!")
      (omega~message "... there is no premises missing."))
    result)
  )

(defun ana=propose-node (missing-node target-plan source-plan step planning-step substitution node-table)
  (let* (;; we need to know which goal we are working for:
	 (goal (first (last (pds~open-nodes target-plan)))) ;actually this is a hack
					;(works with deussen)
	 (formula (lam~eta-contract(beta~normalize
				    (subst~apply substitution 
						 (ana~variabelize missing-node)
						 :test #'data~equal))))
	 (label (ana~create-name missing-node))
	 (lemma-hyps (pdsn~hyps goal))
	 (lemma-supps (pds~node-supports goal target-plan))
	 (node (pdsn~open-node-create formula
				      lemma-hyps
				      label))
	 (new-step step)
	 (premises (ana~step-premises step))
	 (new-premises (mapcar
			#'(lambda (prems)
			    (if (eq (second prems) 'missing) (list (first prems) node)
			      prems
			      ))
			premises))
	 )
    (setf (get 'lemma node) t)
    (setf (ana~step-premises new-step) new-premises)
    (omega~message "Proposing node ~s with formula ~s." node (node~formula node))
    (if (ana~match-step new-step target-plan) ;; the step is applicable with the new node
	(if (or (not ana*interactive-mode)
		(and ana*interactive-mode
		     (ana=y-or-n-p "Do you want to insert this node?")))
	    (progn
	      (omega~message "Now inserting the node in the proof plan.")
	      (pds~add-sponsors node lemma-supps)
	      (pds~add-sponsors goal (list node))
	      (pds~insert-node! node target-plan)
	      (ana=transfer-step target-plan source-plan planning-step
				 substitution
				 (ana~add-corresponding-nodes (list missing-node
								    (list node))
							      node-table)))
	  (progn
	    (omega~message "Node is not inserted.~%Skipping step.")
	    (ana=transfer-step target-plan source-plan (ana~advance-planning-step planning-step)
			       substitution node-table)
	    ))
      (progn
	(omega~message "Inserting the node doesn't help.~%Skipping step.")
	(ana=transfer-step target-plan source-plan (ana~advance-planning-step planning-step)
			   substitution node-table)
	)
      )))

(defun ana=reformulate (step target-plan source-plan planning-step substitution
			     node-table)
  (declare (ignore node-table substitution planning-step source-plan))
  ;; we open a new planning space and plan one step. If the old method is then applicable,
  ;; the reformulation was successfull and is inserted in the target-pds.
  (let ((target-goal (ana~step-target-goal step))
	(target-premises (ana~step-target-premises step))
	(target-parameters (ana~step-target-parameters step))
	(method (ana~step-method step))
	)
    (if (ana=backward-method-p method)
	(progn
	  (setf ana*goal-step step)
	  (omega~message "Trying to add one or more backward planning steps:")
	  (let ((ref-result (ana~apply-method-on (meth~find-method 'analogyref-s-b)
						 target-goal target-premises target-parameters
						 target-plan))
		)
	    (if ref-result
		(progn
		  (omega~message "Some steps could be inserted in order to continue the transfer!")
		  ref-result)
	      (progn (omega~message "No steps could be inserted...") nil))))
                ;;needed as currently omega~message does not return nil when called from loui
      (progn
	  (setf ana*goal-step step)
	  (omega~message "Trying to add one or more forward planning steps:")
	  (let ((ref-result (ana~apply-method-on (meth~find-method 'analogyref-s-f)
						 target-goal target-premises target-parameters
						 target-plan))
		)
	    (if ref-result
		(progn
		  (omega~message "Some steps could be inserted in order to continue the
transfer!")
		  ref-result)
	      (progn (omega~message "No steps could be inserted...") nil))))))
  )


(defun ana=replace (old-method new-method step target-plan source-plan planning-step
			       substitution node-table)
  (progn
    (omega~message "We replace method ~s by ~s because of domain knowledge." old-method new-method)
    ;; we replace the method
    (ana=step-plan (ana~set-step-method! step new-method) target-plan)
    (ana=transfer-step target-plan source-plan (ana~advance-planning-step planning-step)
		       substitution
		       ;; and set the problem node in correspondance with the just created nodes
		       (ana~update-table step planning-step target-plan node-table)
		       )))

(defun ana=skip-applicable (step target-plan source-plan node-table substitution)
  ;; new heuristic: if a daughter/father step is applicable, skip this step
  (progn (omega~message "Now checking if we can skip steps...")
	 (if ;;we want to skip a step if there is a target node missing that corresponds to an
	  ;;'open' source node.
	  (eq 'missing (ana~step-target-goal step))
	  (progn (omega~message "...there is no corresponding target goal...")
		 t)
        (if (ana=backward-method-p (ana~step-method step))
	  ;; check if the method of a daughter is applicable to the current nodes
	  (progn (omega~message "...trying to match with the following target steps...")
	   (let ((result
		  (some
		   #'(lambda (node)
		       (ana~match-step-with-node node step target-plan source-plan
						 node-table substitution))
		   (remove-if
		    #'(lambda (step)
			(or (ana=step-already-transfered (keim~name step))
			    (ana=forward-method-p (ana~just-method (node~justification step))))
			;; fuer bsp 324 auf 326 da sonst unwraphyp!
			    )
		    (remove-duplicates
		      (ana=get-subgoals (pdsn~just-premises (ana~step-source-goal step))
					ana*skiplimit)
		     :from-end t :key #'(lambda (x) (keim~name (ana~just-method
								(node~justification x)))))
		    ))))
	     (if result
		 (progn
		   (omega~message "... a following method will be applicable...")
		   result)
	       (progn (omega~message "..no following method is applicable, so we will not skip this step.")
		      nil))))
	  (progn (omega~message "...trying to match with the preceding target steps...")
		 (let ((result
			(some
			 #'(lambda (node)
			     (ana~match-step-with-node node step target-plan source-plan
						       node-table substitution))
			 (remove-if
			  #'(lambda (step)
			      (ana=step-already-transfered (keim~name step)))
			  (remove-duplicates
			   (ana=get-parents-uptohight
			    (ana~step-source-premises step)
			    ana*skiplimit)
			   :from-end t :key #'(lambda (x) (keim~name (pdsn~just-method x))))
			  ))))
		   (if result
		       (progn
			 (omega~message "... a preceding method will be applicable...")
			 result)
		     (progn (omega~message "..no preceding method is applicable, so we will not skip this
step.") nil))))
	  ))))

(defun ana=skip-step (skip step target-plan source-plan planning-step substitution
			   node-table)
  (declare (ignore step))
  ;; we skip this planning step
  (progn
    (if (consp skip) ;; then we will skip one or more methods and not a whole subproof
	(omega~message "... therefore skipping source step ~s!" planning-step)
      ;; otherwise we will skip the whole subproof as there are no corresponing nodes in the
      ;; target:
      (omega~message "... therefore skipping subproof of ~s." planning-step))
    (ana=transfer-step target-plan source-plan
		       (ana~advance-planning-step planning-step)
		       substitution
		       (if (consp skip) ;;if skip contains new corresponding nodes, then
			   ;;save them
			   (progn
			     (ana~change-corresponding-nodes ;;works only for one pair...
			      skip
			      node-table)
			     (ana~print-table node-table)
			     node-table)
			 node-table)
		       )))



;;=========================================
;; Supporting functions:
;;=========================================

;;------getting the target nodes:

(defun ana~get-target-nodes (source-nodes substitution node-table)
  ;; input: a list of nodes
  ;; returns: a list of matchers for each node
  ;; for each source node we search for the corresponding target node:
  (mapcar #'(lambda (source-node)
              (let ((corresponding-nodes (ana~get-corresponding-nodes
                                          source-node
                                          node-table)))
                (if (not corresponding-nodes) ;;that means there is something missing
                    (progn
                      (omega~message "Ooops, there is no node for ~s." source-node)
                      (list (ana~create-match-problem :source-node
                                                      source-node
                                                      :target-node 'missing-node
                                                      )))
                  (progn
                    (omega~message  "Searching for the corresponding node of ~S in ~S."
                                  source-node corresponding-nodes)
                    (if (= 1 (length corresponding-nodes))
                        ;; if there is only one corresponding node, we choose it without
                        ;; matching:
;                        (progn
;                          (omega~message "As there is only one corresponding node, we choose it
;without matching.")
                          ;; and we return a matcher which is marked 'not-machted
                          (list (ana~create-match-problem :subst 'not-matched :source-node
                                                          source-node
                                                          :target-node (first
                                                                        corresponding-nodes)))
                      ;; else (there are more than one corresponding nodes),
                      ;; we match:
                      (ana~choose-match (ana~match (list source-node) corresponding-nodes substitution)))))))
          source-nodes))

(defun ana~choose-match (matchers)
  ;; here we can add a choice point for choosing between the proposed matches
  (first matchers))

(defun ana=get-target-parameters (source-parameters substitution node-table)
  (if (listp source-parameters)
      (mapcar #'(lambda (parameter)
                  (ana=get-target-parameters parameter substitution node-table))
              source-parameters)
    (ana=get-target-parameters-h source-parameters substitution node-table)))

(defun ana=get-target-parameters-h (source-parameter substitution node-table )
  (let ((target-parameter
         (typecase source-parameter
           (pdsn+node ;(pds~label2node
            (ana~matcher-target-node
             ;; we choose the first matcher (we matched only one node)
	     (let ((result (ana~get-target-nodes (list source-parameter)
						 substitution
						 node-table)))
;	     (caar ;vorher caar ABER lim+/lim+ complexestimate ,,caar fuer dipl-bsp!!
	      ;; ne !!! auf dem laptop anders als an der uni...
	       (if (listp (car result))
		  (caar result)
		(car result)))))
	   (t source-parameter))))
;    (omega~message "We choose ~s for the parameter ~s." target-parameter source-parameter)
    target-parameter))

;;------- subgoal-depth
(defun ana=get-goals-of-depth (pds depth)
  (ana=get-goals-h (list (prob~proof-root pds))
		   0 depth))
			  
(defun ana=get-goals-h (nodes counter depth)
  ;;we get the childs of the rootnode of depth DEPTH
  (if (and nodes ;; no nodes? then stop
	   (< counter depth))
      (ana=get-goals-h ;(append (rest nodes)
       (ana=get-childs-of nodes)
       (1+ counter)
       depth)
    ;; the limit is reached, so we return the result list
    nodes))

(defun ana=get-subgoals (nodes depth)
  ;; if the limit is not reached, we add the current node to the result and add his childs
  ;; to the nodes list and continue
  (when (and nodes ;; no nodes? then stop
	     (< 0 depth))
    (append nodes
	    (ana=get-subgoals (ana=get-childs-of nodes)
			      (1- depth))
    ;; the limit is reached, so we return the result list (reversed to reconstruct the
    ;; order the nodes were added
;    (reverse nodes)))
	    )))

(defun ana=get-childs-of (nodes)
  ;; we take the premises of nodes
  (let ((result ()))
    (dolist (node nodes result)
      (setf result
            (append result
		    (pdsn~just-premises node))))))

;;;========================
;;; retrieval
;;;========================

(defun ana=retrieve-source (target sourcep)
  ;; retrieves the source-pds corresponding to the given target-pds
  (progn
    (omega~message "Retrieving source for ~S:" (keim~name target))
    (let* ((source (prob~find-proof sourcep)))
      (if source (omega~message "Retrievel successfull: Source is ~s" (keim~name source))
	(progn (omega~error "Problem while retrieving!!!") nil)
	)
      source)))


;;--------------
;; repairing
;;--------------

(defun ana~get-methods (fct)
  (sort (let ((result))
	  (dolist (method meth*planning-methods result)
	    (when (and (not (infer~supermethod-p (meth~inference method)))
		       (not (eq 'IMPE-OPEN-M-A (keim~name method)))
		       ;; needed because of a bug with its mapping
		       (> (meth~rating method) 0)
		       (apply fct (list method))
		       )
	      (push method result)))
	  result)
	#'>
	:key #'meth~rating
	))

(defun ana~try-ref (goal support)
  (let* ((old-pds omega*current-proof-plan)
	 (old-agenda (pds~agenda old-pds))
	 (first-task (unless (agenda~empty-p old-agenda)
		       (agenda~first-task old-agenda)))
	 (task (if first-task first-task
		 plan*current-task))
	 (act-goal (if goal goal
		     (agenda~task-node task)))
	 (act-supp (if support (list support)
		     (pds~node-supports act-goal old-pds)))
	 (omega*current-proof-plan (plan~new-pds act-goal
						 act-supp
						 old-pds
						 ))
	 (keim::pds*current-proof-plan omega*current-proof-plan)
	 (cri*used-control-rules-hashtable
	  (cri~set-used-control-rules nil))
	 (new-pds
	  (ana=try-ref (cons (meth~find-method 'ande-m-f)
			     (ana~get-methods #'identity))
		       ana*goal-step
		       (list (ana~step-target-goal ana*goal-step))
		       (ana~step-target-premises ana*goal-step)
		       0 nil nil))
	 )
    (if new-pds
	(let* ((root (prob~proof-root new-pds))
	       (steps (union (list root)
			     (prob~proof-steps new-pds)))
	       (open-prems (pds~open-nodes new-pds))
	       (add-prems (remove root open-prems))
	       (add-hyps (ana=set-difference
			  (ana=union-of-hyps steps)
			  (pdsn~hyps root)))
	       (internal-prems
		(ana=union-of-prems steps))
	       (add-concs
		(remove-duplicates
		 (ana=set-difference
		  (remove-if #'pdsn~open-node-p steps)
		  (cons root
			(union internal-prems
			       (union add-hyps
				      (pdsn~just-unsponsors root)))))))
	       (prec (intersection internal-prems
				   act-supp
				   ))
	       )
	  (values T (list new-pds prec add-hyps
			  (if goal add-prems (remove act-goal add-prems))
			  add-concs root T)))
      (values NIL (list NIL NIL NIL NIL NIL (when goal T))))))

(defun ana=set-difference (set1 set2)
  (declare (edited  "28-JAN-1999")
	   (authors Sorge)
	   (input   "Two list representing sets.")
	   (effect  "None.")
	   (value   "A list containing the members of set1 which are not in set2."
		    "The order of set1 is respected."))
  (remove-if #'(lambda (x) (find x set2)) set1))


(defun ana=try-ref (methods step goals premises depth applicable-methods applied-methods)
  (if methods
      (let ((method (first methods)))
;	(omega~message "Applying ~a" method)
	(multiple-value-bind (applicable new-goals new-prems)
	    (ana=apply-method method omega*current-proof-plan
			      (first (pds~open-nodes omega*current-proof-plan))
			      goals premises)
	  (if applicable
	      (if (or (and (ana=backward-method-p (ana~step-method step))
			   (not (pds~open-nodes omega*current-proof-plan)))
		      ;; if all nodes are closed, then it's fine too if the target method
		      ;; is a backward method
		      (ana~method-is-matching (ana~step-method step)
					      new-goals
					      new-prems
					      (ana~step-target-parameters step)
					      omega*current-proof-plan))
					;(ana~method-is-matching goal-method omega*current-proof-plan)
		  omega*current-proof-plan
		(progn
;		  (omega~message "Backtracking for ~a" method)
		  (ana=backtrack 1)
		  (ana=try-ref (rest methods) step goals premises depth
			       (append applicable-methods
				       (list (append applied-methods
						     (list method))))
			       applied-methods)))
	    (ana=try-ref (rest methods) step goals premises depth applicable-methods
			 applied-methods))))
    (if applied-methods
	(values nil applicable-methods)
					;	nil
					;(ana=try-ref methods goal-method depth applicable-methods nil))
      (when applicable-methods
	(when (< (length (first applicable-methods)) ana*max-repair-depth)
	    (multiple-value-bind (applicable new-goals new-prems)
		(ana=apply-method (first applicable-methods)
				  omega*current-proof-plan
				  (first (pds~open-nodes omega*current-proof-plan))
				  goals premises)
	      (list applicable) 
	      (multiple-value-bind (result new-methods)
		  (ana=try-ref (cons (meth~find-method 'ande-m-f)
				     (ana~get-methods #'identity))
			       step new-goals new-prems
			       (length (first applicable-methods)) (rest applicable-methods)
			       (first applicable-methods))
		(if result result
		  (progn
;		    (omega~message "Backtracking for more methods:~a" applicable-methods)
		    (ana=backtrack (length (first applicable-methods)))
		    (ana=try-ref methods step goals premises depth new-methods nil)))))
	  ;(ana=try-ref nil step goals premises depth (rest applicable-methods) nil)
	    )))))

(defun ana=apply-method (method pds goal goals premises)
  (if (listp method)
      (if (= (length method) 1)
	  (ana=apply-method (first method) pds goal goals premises)
	(multiple-value-bind (applicable new-goals new-prems)
	    (ana=apply-method (first method) pds goal goals premises)
	  (list applicable) ;;just for eliminating the compile message 
	  (ana=apply-method (rest method) pds (first new-goals) new-goals new-prems)))
    (progn
      (ana=update-current-formulas (prob~proof-steps pds) pds)
      (let (;(meth*normalizing-methods nil)
					;(meth*restricting-methods nil)
	    (result (plan~try-planning-step (keim~name method) pds
					    (ana=backward-method-p method))))
					;goal)))
	(if result
	    (values result
		    (ana=new-nodes-for-nodes goals
					     omega*current-proof-plan
					     :kind 'conclusions)
		    (ana=new-nodes-for-nodes premises
					     omega*current-proof-plan
					     :kind 'premises)))))))

	
    

(defun ana~method-is-matching (method goals prems parameters pds)
  (when method
    (progn
      (ana=update-current-formulas (append goals prems) pds)
      (some #'(lambda (open)
		(plan~match-method method open (node~formula open)
				   prems pds :parameters parameters)
					;:invoke-cri nil) ;(when (meth~parameters method) parameters)) 
		)
	    goals))))

(defun ana=update-current-formulas (nodes pds)
  (let ((mvar-subst (if (pds~constraint-pool pds)
			(pds~cstrpool-bindings (pds~constraint-pool pds))
		      (subst~create nil nil))))
    (mapcar #'(lambda (node)
		(when (and mvar-subst
			   (pdsn~schematic-p node)
			   (null (pdsn~up-to-date node)))
		  (setf (pdsn~current-formula node)
			(subst~apply mvar-subst (node~formula node)))
		  (setf (pdsn~up-to-date node) 't)))
	    nodes)
    t
    ))
	
(defun ana=backtrack (steps)
  (dotimes (i steps)
    (when (pds~last-plan-step omega*current-proof-plan)
      (let* ((step (pds~last-plan-step omega*current-proof-plan))
	     (node (pdsc~an-node step)))
;	(pds~delete-node! node))))) ;;doesn't work. how strange (sarcasm off)
	(if (ana=backward-method-p (ana~just-method (pdsc~an-just step)))
	    (progn
	      (pds~open-node! node)
	      (setf (pds~agenda omega*current-proof-plan)
		    (agenda~generate (list
				      (mapcar #'agenda~create-goal
					      (pds~open-nodes omega*current-proof-plan)))))
	      (setf (pds~node-supports node) (pds~support-nodes omega*current-proof-plan))
	      )
	  (progn
	    (pds~open-node! node)
	    (let ((supps (pds~open-nodes omega*current-proof-plan)))
	      (mapcar #'(lambda (open)
			  (setf (pds~node-supports open)
				(pds~support-nodes omega*current-proof-plan)))
		      supps))
	    ))
	))))

(defun ana=new-nodes-for-nodes (nodes pds &key ((:kind kind)))
  ;; replaces those nodes in nodes that were removed in the last planning step by the
  ;; newly created nodes
  (if (pds~last-plan-step pds)
      (let ((just (pdsc~an-just (pds~last-plan-step pds))))
	(if (eq kind 'conclusions)
	    ;;get deleted conclusions, replace them by add-premises
	    (remove nil
		    (append
		     (set-difference
		      nodes
		      (ana~get-minus-conclusions just)
		      :test #'ana~node-equal)
		     (ana~get-add-premises just)))
	  ;;get deleted premises, replace them by add-concs
	  (remove nil
		  (append
		   (set-difference
		    nodes
		    (ana~get-exist-premises just)
		    :test #'ana~node-equal)
		   (ana~get-add-conclusions just))
		  )))
    nodes))

;;--------
(defun ana=union-of-hyps (nodes)
  (when nodes
    (union (pdsn~hyps (first nodes))
	   (ana=union-of-hyps (rest nodes)))))

(defun ana=union-of-prems (nodes)
  (when nodes
    (let* ((node (first nodes))
	   (just (node~justification node))
	   (infer (just~method just))
	   )
      (if (or (infer~dummy-p infer)
	      (pdsn~open-node-p node)
	      (pdsn~hypothesis-node-p node))
	  (ana=union-of-prems (rest nodes))
	(union (just~premises just)
	       (ana=union-of-prems (rest nodes)))))))


;;===============================================
;; the interfaces to the planner and the methods:
;;===============================================

(defun ana=get-parents (nodes)
  (declare (edited  "20-MAY-2000")
	   (authors Cullrich)
	   (input   "a node")
	   (effect  )
	   (value   "The parents (its reasons) of a node."))
  (mapcan #'(lambda (node)
	      (remove-if-not  #'(lambda (parent)
				  (member node (pdsn~just-premises parent)))
			      (mapcar #'pdsc~an-node (pdsj~other-reasons (node~justification node)))
			      ))
	   nodes))

(defun ana=get-parents-uptohight (nodes height)
  (declare (edited  "20-MAY-2000")
	   (authors Cullrich)
	   (input   "a node")
	   (effect  )
	   (value   "The parents (its reasons) of a node up to height HEIGHT."))
  (when (> height 0)
    (append (ana=get-parents nodes)
	    (mapcan #'(lambda (parent)
			(ana=get-parents-uptohight (list parent) (1- height)))
		    (ana=get-parents nodes)))))

(defun ana~first-planning-step (pds)
  (pds~first-plan-step pds))

(defun ana~get-open-goals (pds)
  (pds~open-nodes pds))

(defun ana~get-nodes-for-method-nodes (method-nodes mapping) 
  (ana=get-specified method-nodes mapping))
;  (mapcan #'(lambda (node)
;              (ana=mapp-get-component node mapping))
;          method-nodes))

(defun ana~apply-method-on (method conclusion assumptions params pds)
  (if (consp conclusion)
      (progn
	(ana=update-current-formulas (append conclusion assumptions)
				     pds)
	(some #'(lambda (conc)
		  (plan~step-plan-with method conc assumptions params pds))
	      conclusion))
    (progn
      (ana=update-current-formulas (if conclusion (cons conclusion assumptions)
				     assumptions) pds)
      (plan~step-plan-with method conclusion assumptions params pds))))


(defun ana~match-step (step plan)
  (let* ((agenda (pds~agenda plan))
	 (tasks (reverse (agenda~first-tasks! agenda)))
	 (goal (ana~step-target-goal step))
	 (task (if (and goal (not (eq 'missing goal)))
		   (plan~find-task-for-goal-in-agenda goal agenda)
		 (or
		  (first (member-if-not #'agenda~goal-schema-p tasks))
		  (first tasks))))
	 (target-goal (or goal
			  (agenda~task-node task)))
	 (target-premises (ana~step-target-premises step))
	 (target-parameters (ana~step-target-parameters step))
	 (method (ana~step-method step))
	 (plan*current-task task))
    (when (not (member 'missing (cons goal target-premises)))
      (ana~method-is-matching method (list target-goal) target-premises
			      (when (meth~parameters method) target-parameters) plan)
;(plan~match-method method target-goal (node~formula target-goal) target-premises plan
;			 (when (meth~parameters method) target-parameters)))))
      )))

(defun ana~match-step-with-node (node step plan source-plan node-table substitution)
  (declare (ignore source-plan))
  (let* ((target-goal (or (ana~step-target-goal step)
			  ;; otherwise we choose a random goal:
			  (agenda~task-node
			   (first (agenda~first-tasks! (pds~agenda plan))))))
	 (target-premises
	  (append (ana~step-source-premises step)
		  (apply #'append
			 (mapcar #'(lambda (node)
				     (ana~get-corresponding-nodes node node-table))
				 (pdsn~just-premises node)))))
	             ;; das ist leider nicht gut fuer lim-seq-thm3.2.4 auf 3.2.6... da
		     ;; unwraphyp anwendbar wird!
	 ;; e.g. for lim-plus -> lim-times . There an incorrect node is suggested
	 (target-parameters (ana=get-target-parameters
			     (pdsj~parameters (node~justification node))
			     substitution
			     node-table))
	 (method (ana~just-method (node~justification node)))
	 (result
	  (and method
	       (ana~method-is-matching method (list target-goal) target-premises
				       (when (meth~parameters method)
					 target-parameters)
				       plan))))
;	       (plan~match-method method target-goal (node~formula target-goal)
;				  target-premises plan (when (meth~parameters method)
;							 target-parameters)))))
    (when result ;; then return the corresponding pair which is not as in my thesis the
		 ;; node that was choosen but the nodes the method matched upon!
      (if (ana=backward-method-p method)
	  (list node (ana=get-specified (remove-if-not #'(lambda (line)
							   (meth~minus-node-p line))
						       (meth~conclusions method))
					(plan~mmatch-mmapp (first result))))
	(list node (ana=get-specified (meth~closed-premises method)
				      (plan~mmatch-mmapp (first result))))))))
;	(list node (list target-goal)))))
		     

(defun ana~advance-planning-step (planning-step)
  ;; as some planning steps point to methods that were already applied, we have to exclude
  ;; them
  (if ana*ina-steps (if (eq (first ana*ina-steps) 'ina-end) nil
		      (pop ana*ina-steps)) ;; we look if the source plan is finished
  (when (pdsc~successor (pdsj~control (pdsc~an-just planning-step)))
    (setf ana*processed-nodes
	  (cons (keim~name (pdsc~an-node planning-step)) ana*processed-nodes))
    (when (pdsc~an-just planning-step)
      (let* ((next
	      (pdsc~successor (pdsj~control (pdsc~an-just planning-step))))
	     (node-pointer (remove-if #'not
				      (mapcar #'(lambda (object)
						  (unless (stringp object) object))
					      (pdsj~outline-pattern (pdsc~an-just next))))))
	(if (ana=step-already-transfered node-pointer)
	    (ana~advance-planning-step next)
	  next)))))
  )

(defun ana=step-already-transfered (node-label)
  ;; T if the step correspoding to the node label was already transferred
  (and node-label
       (or (when (consp node-label)
	     (subsetp node-label ana*processed-nodes))
	   (member node-label ana*processed-nodes))))


(defun ana=mapp-get-component (node mapping)
  (meth~mapp-get-component
   (if (meth~meta-node-p node)
       (meth~meta-node-metavar node)
     (list(keim~name node)))
   mapping :all))

(defun ana=get-specified (method-nodes subst)
  (let ((result))
    (dolist (node method-nodes result)
      (let ((cor (ana=mapp-get-component node subst)))
	(cond
	 ((consp cor) (setf result (append cor result)))
	 ((pdsn~p cor) (push cor result)))))
    result))

(defun ana~get-add-conclusions (just)
  (ana=get-specified (meth~add-conclusions (ana~just-method just))(pdsj~subst just)))

(defun ana~get-add-premises (just)
  (ana=get-specified (meth~add-premises (ana~just-method just)) (pdsj~subst just)))

(defun ana~get-add-hyps (just)
  (ana=get-specified (remove-if-not #'meth~hypothesis-node-p
				    (meth~declarative-content (ana~just-method just)))
		     (pdsj~subst just)))

(defun ana~get-exist-conclusions (just)
  (ana=get-specified (meth~exist-conclusions (ana~just-method just))
		     (pdsj~subst just)))

(defun ana~get-minus-conclusions (just)
  (ana=get-specified (remove-if-not #'(lambda (line)
					(meth~minus-node-p line))
				    (meth~conclusions (ana~just-method just)))
		     (pdsj~subst just)))

(defun ana~get-exist-premises (just)
  (ana=get-specified (meth~exist-premises (ana~just-method just))
		     (pdsj~subst just)))

(defun ana~just-method (justification)
;  (if (infer~supermethod-p (just~method justification))
;      (meth~find-method (infer~supermethod (just~method justification)))
  (if (or (infer~supermethod-p (just~method justification))
	  (infer~method-p (just~method justification)))
#|      (if (and (eq(keim~name (just~method justification))
		  'tellcs-m)
	       (= (length (pdsj~outline-pattern justification)) 1)
	       (string= (first (pdsj~outline-pattern justification))
			"CLOSED")) ;;aenderung 17.5.00
	  (meth~find-method 'tellcs-m-f) ; wird in 4.1.10.a gebraucht...|#
	(if (and
	     (eq(keim~name (just~method justification))
		'simplifyinequality-m) ;; wird auch in 4.1.10.a gebraucht
	     (string= (first (pdsj~outline-pattern justification))
		      "EXISTENT"))
	    (meth~find-method 'simplifyinequality-m-b)
	  (pds~inference-application (just~method justification)
				     (mapcar #'(lambda (pattern)
						 (if (stringp pattern) pattern
						   "NONEXISTENT"))
					     (pdsj~outline-pattern justification)
					     )))));)

(defun ana~plan-one-bw-step (plan)
  (let* ((meth*planning-methods (ana~get-methods #'ana=backward-method-p))
	 (cri*used-control-rules-hashtable (cri~set-used-control-rules ())))
    (plan~step-plan plan)
    omega*current-proof-plan))

(defun ana~plan-one-fw-step (plan)
  (let ((meth*planning-methods (ana~get-methods #'ana=forward-method-p)))
    (plan~step-plan plan)
    omega*current-proof-plan))


(defun ana=forward-method-p (method)
  (and method
       (meth~exist-premises method)
       (not (meth~exist-conclusions method))))

(defun ana=backward-method-p (method)
  (and method (meth~exist-conclusions method)
       (not (eq (keim~name method) 'ExistsE-m-a))))


(defun ana=get-operator-for-step (step)
  (ana~just-method (pdsc~an-just step))
  )

(defun ana~get-preconditions-for-operator (operator plan) ; - ()
  (declare (ignore plan))
  (remove nil
	  (mapcan #'(lambda (node)
		      (ana=mapp-get-component node (pdsj~subst (pdsc~an-just operator))))
		  (append
		   (meth~exist-premises (ana=get-operator-for-step operator))
		   (meth~exist-conclusions (ana=get-operator-for-step operator))
		  ))
	  ))


(defun ana~get-add-lines-for-operator (operator plan) ; + hyps
  (declare (ignore plan))
  (let ((just (pdsc~an-just operator)))
    (append (ana~get-add-premises just)
	    (ana~get-add-hyps just)
	    (ana~get-add-conclusions just)))
  )

;;------getting the source nodes:
(defun ana~get-source-method (planning-step)
  (ana~just-method (pdsc~an-just planning-step)))

(defun ana~get-source-conclusion (method planning-step)
  (first
   (ana~get-nodes-for-method-nodes
    (meth~exist-conclusions method)
    (pdsj~subst (pdsc~an-just planning-step))))
  )
(defun ana~get-source-premises (method planning-step)
  (ana~get-nodes-for-method-nodes
   (meth~exist-premises method)
   (pdsj~subst (pdsc~an-just planning-step))))

(defun ana~get-source-parameters (method planning-step)
  (declare (ignore method))
  (pdsj~parameters (pdsc~an-just planning-step)))

;;-------------------

(defun ana~apply-mmatching (conclusion step pds)
  (let ((ac-mmatching
	 (or (ana~step-mmatching step)       ;; for experiments!
      	     (and ana*no-semantic-choice
		  (ana=update-current-formulas (cons conclusion
						     (ana~step-target-premises step))
					       pds
					       )
		  (first (ana~match-step step pds)))))
	)
    (unless (not ac-mmatching);; this case occurs only when semantic-mode is off!
      (setf (pds~agenda pds)
	    (plan~apply-mmatching-with-node conclusion ac-mmatching pds)))))


;;===============================================
;; the nodes correspondance-table structure
;;===============================================

(defvar ana*table-verbose nil "Verbose mode for corresponding-tables.")


(defclass ana+nodes-correspondance-table ()
  ((table :initform (make-hash-table)
	  :accessor ana=table))
  (:documentation "Objects of this class contain information about the correspondance of nodes."))

;; this data-structure is constructed as following:
;; the source-nodes are mapped (by a hashtable) to symbols. These act as pointers through
;; their property list, where the property 'nodes points to the list of target
;; nodes. Confused? For example, by adding a corresponding pair such as ((SA1 SA2 SA3)
;; (TA1 TA2 TA3 TA4)) (ana~add-corresponding-nodes ((SA1 SA2 SA3)(TA1 TA2 TA3 TA4)), we
;; add the information that the target nodes TA1...TA4 correspond to 
;; the source nodes SA1...SA3. When later we find out that SA1 and TA1
;; are the corresponding pair, we change the table (ana~change-correspondance-table (SA1
;; (TA1))).
;; After the first command, the hashtable will have the following form:
;; SA1 -> #G123  ;; where #G123 is a generated symbol
;; SA2 -> #G123
;; SA3 -> #G123
;; and 
;; after the change:
;; SA1 -> #G945  ;; where #G945 is a generated symbol
;; SA2 -> #G123
;; SA3 -> #G123
;; and #G123 property-list contains: 'nodes (TA2 TA3 TA4)
;; and #G945 property-list contains: 'nodes (TA1)
;; that's easy, isn't it?
;; remember, that the corresponding nodes are always a list (of course sometimes with one element)

(defun ana~construct-correspondance-table ()
  (make-instance 'ana+nodes-correspondance-table))

(defgeneric ana~get-corresponding-nodes (node table)
  (:method (node
	    (table ana+nodes-correspondance-table))
;	   (when node
	     (if ana*use-node-table
		 (get 'nodes (gethash node (ana=table table)))
	     ;;for experiments: if we don't use the node table then
	     ;;for an open goal select all open goals
	     ;;for an assumption select all assumptions
;             (if (pdsn~open-node-p
;                  (first (get 'nodes (gethash node (ana=table table)))))
;                 (pds~open-nodes omega*current-proof-plan)
;               (remove-if
;                #'pdsn~open-node-p
	     (if (get 'nodes (gethash node (ana=table table)))
	       (prob~proof-steps omega*current-proof-plan))
	       ));)
  )

(defgeneric ana~add-corresponding-nodes (nodes-pair table)
  ;;nodes pair is a list of pairs, pair is a two elemente list: two list
  (:method ((nodes-pair list) (table ana+nodes-correspondance-table))
	   (when (and (first nodes-pair) ;; do not enter empty pairs
		      (every #'pdsn~p (second nodes-pair));; and only enter nodes (needed
							  ;; as we don't want to have
							  ;; 'missing-node as
							  ;; corresponding node!)
		      )
	     (let* ((new-sym (gensym))
		    (source-nodes (if (listp (first nodes-pair))
				      (first nodes-pair)
				    (list (first nodes-pair))))
		    (target-nodes (second nodes-pair)))
	       (mapcar #'(lambda (source-node)
			   (ana=add-corresponding-node source-node
						       table
						       new-sym))
		       source-nodes)
	       (setf (get 'nodes new-sym) target-nodes)))
	   table)
  )

(defun ana=add-corresponding-node (source-node table new-sym)
    (setf (gethash source-node (ana=table table)) new-sym))

(defgeneric ana~change-corresponding-nodes (node-pair table)
  (:method (node-pair
	    (table ana+nodes-correspondance-table))
	   (let* ((source-node (first node-pair))
		  (target-nodes (second node-pair))
		  (sym (gethash source-node (ana=table table)))
		  (val (get 'nodes sym)))
	     ;; remove the old nodes
	     (setf (get 'nodes sym)
		   (set-difference val target-nodes))
	     ;; insert the new nodes
	     (ana~add-corresponding-nodes (list (list source-node)
						target-nodes)
					  table)
	     )))

(defun ana~initialize-correspondance-table (node-list)
  ;; creates a node-table and adds the nodes in the list
  (let ((new-table (ana~construct-correspondance-table))
	)
;    (omega~message "--------------------------------------")
;    (omega~message "Initializing the correspondance table.")
    (mapcar #'(lambda (nodes)
		(ana~add-corresponding-nodes nodes new-table))
	    node-list)
    (ana~print-table new-table)
    new-table))
    

(defun ana~update-table (step last-source-planning-step target-pds table)
  (progn
    ;; first change the old correspondances
    (ana~change-corresponding-nodes
     (list (ana~step-source-goal step)
	   (list (ana~step-target-goal step)))
     table)
    (dolist (nodes (ana~step-premises step))
      (ana~change-corresponding-nodes
       (list (first nodes)
	     (cdr nodes))
       table))
    ;; then add the new nodes created by the last planning step
    ;; we take the +nodes created by the last planning step and add them
    (ana~add-corresponding-nodes (list
				  (ana~get-add-conclusions (pdsc~an-just
							    last-source-planning-step))
				  (ana~get-add-conclusions (pdsc~an-just
							    (pds~last-plan-step
							     target-pds))))
				 table)
    (ana~add-corresponding-nodes (list
				  (ana~get-add-premises (pdsc~an-just
							 last-source-planning-step))
				  (ana~get-add-premises (pdsc~an-just
							 (pds~last-plan-step
							  target-pds))))
				 table)
    ;; now we search for the introduced hypotheses as they do not appear in the added-node
    ;; lists of the method (very intuitive!)
    (ana~add-corresponding-nodes (list
				  (ana~get-add-hyps (pdsc~an-just
						     last-source-planning-step))
				  (ana~get-add-hyps (pdsc~an-just
						     (pds~last-plan-step
						      target-pds))))
				 table)
    (ana~print-table table)
    table))

(defun ana~add-nodes-for-steps (step1 step2 plan1 plan2 node-table)
  (declare (ignore plan1 step2))
  (let ((table
    (ana~add-corresponding-nodes
     ;; we append the new nodes the old correspondings
     (list (ana~get-exist-conclusions (pdsc~an-just step1))
	   (ana~get-add-premises (pdsc~an-just (pds~last-plan-step plan2))))
     (ana~add-corresponding-nodes
      (list (ana~get-exist-premises (pdsc~an-just step1))
	    (append (ana~get-add-conclusions (pdsc~an-just (pds~last-plan-step plan2)))
		    (mapcan #'(lambda (node)
				(ana~get-corresponding-nodes node node-table))
			    (ana~get-exist-premises (pdsc~an-just step1)))))
      node-table))))
    (ana~print-table table)
    table))


(defun ana~print-table (table)
  (when ana*table-verbose
    (omega~message "New correspondances:")
    (maphash #'(lambda (x y) (omega~message "~S -> ~S" x (get 'nodes y)))
	     (ana=table table))
    table
    ))

;;;;;;;;;;;;;;;;;;;
;; input/output
;;;;;;;;;;;;;;;;;;;

(defun ana=y-or-n-p (text)
  (let ((result (omega~query text 'symbol)))
    (or (not (eq result 'n))(not (eq result 'no)))))

;(defun ana=read-line()
;  (omega~query "press enter" 'anything))

;;;;;;;;;;;;;;;;;;;
;; metapredicates
;;;;;;;;;;;;;;;;;;;

(defun current-source-method (method)
  (declare (edited  "20-MAY-2000")
	   (authors Cullrich)
	   (input   "A method name or a string")
	   (effect  )
	   (value   "if METHOD is a string, returns the source method of the step to be
transfered, otherwise T/nil if METHOD is the source method or not."))
  (if (stringp method)
      (list (list (cons method (ana~step-method ana*current-step))))
    (eq (meth~find-method method) (ana~step-method ana*current-step))))

(defun extended-match-contains (term1 term2)
  (declare (edited  "20-MAY-2000")
	   (authors Cullrich)
	   (input   "Two terms")
	   (effect  )
	   (value   "T if either the substitution or the term-maps contain the pair TERM1 TERM2"))

  (or (cri=search-subst term1 term2 (ana~matcher-subst (ana~step-subst ana*current-step)))
      (cri=search-term-map term1 term2 (ana~matcher-term-maps (ana~step-subst ana*current-step)))))

(defun cri=search-subst (term1 term2 subst)
  (declare (edited  "20-MAY-2000")
	   (authors Cullrich)
	   (input   "Two terms and a substitution.")
	   (effect  )
	   (value   "T if the two terms are a substitution pair."))
  (data~equal (subst~get-component
	       (post~read-object term1 (prob~proof-environment omega*current-proof-plan)
				 :existing-term)
	       subst)
	      (post~read-object term2 (prob~proof-environment omega*current-proof-plan)
				:existing-term)))
   
(defun cri=search-term-map (term1 term2 map)
  (declare (edited  "20-MAY-2000")
	   (authors Cullrich)
	   (input   "Two terms and a term-mapping list.")
	   (effect  )
	   (value   "T if the two terms are a term-map pair."))
  (some #'(lambda (pair)
	      (and
	       (data~equal
		    (post~read-object term1 (prob~proof-environment omega*current-proof-plan)
				      :existing-term)
		    (first pair))
	       (data~equal
		(post~read-object term2 (prob~proof-environment omega*current-proof-plan)
				  :existing-term)
		(second pair))
	       ))
	map))

(defun violated-application-condition (condf)
  (declare (edited  "20-MAY-2000")
	   (authors Cullrich)
	   (input   "a string or a symbol")
	   (effect  )
	   (value   "if CONDF is a string, the cause of method application failure (the name
of the conditional that failed, if any), if CONDF is a symbol T if it is the name of the
conditional that failed, nil otherwise."))
  (if (stringp condf)
      (list (list (cons condf (first meth*cause-of-failure))))
    (eq condf (first meth*cause-of-failure))))


(defun ana=extract-plan (pds)
  (progn
    (setf ana*processed-nodes nil)
    (omega~message "prove ~a" (keim~name (prob~proof-problem omega*current-proof-plan)))
    (ana=extract-plan-h (pds~first-plan-step pds))
    (setf ana*processed-nodes nil)))

(defun ana=extract-plan-h (planning-step)
  (when planning-step
    (let ((source-method (ana~get-source-method planning-step)))
      (if (or
	   (eq (keim~name source-method) 'foralle-meta-m-f)
	   (eq (keim~name source-method) 'EXISTSE-M-A))
	  (omega~message "step-plan")
	(omega~message "step-plan-with ~a ~a ~a ~a"
		       (keim~name source-method)
		       (post~print (keim~name (ana~get-source-conclusion source-method planning-step)) nil)
		       (mapcar #'(lambda (x) (post~print (keim~name x) nil))
			       (ana~get-source-premises source-method planning-step))
		       (mapcar #'(lambda (par)
				   (if (pdsn~p par)
				       (post~print (keim~name par) nil)
				     par))
			       (ana~get-source-parameters source-method
							  planning-step))))
      (ana=extract-plan-h (ana~advance-planning-step planning-step)))))

#|
opr~arrest-listener (&optional (listener opr*lisp-listener))

opr~release-listener (&optional (listener opr*lisp-listener))

|#
