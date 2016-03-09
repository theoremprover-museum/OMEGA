;;; -*- Syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; base-metaprds.thy; This file is part of the OMEGA system
;;
;; major updates: 9.9.1999,
;; 
;;
;; Author: Juergen Zimmer
;; email: jzimmer@ags.uni-sb.de 
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
(in-package "OMEGA")

(defvar cri*symbol-weights
  (list (cons 'minus 1)
	(cons 'plus 1)
	(cons 'times 1)
	(cons 'div 1))
  "The symbol-weights used by the some-meta-predicates.")

;;***********************************************
;;*         always-true
;;***********************************************

(defun always-true ()
  t)

;;***********************************************
;;*         current goal hypotheses
;;***********************************************
(defun current-goal-hyps (hyps)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   "A string LINE for the current goal.")
	   (effect  )
	   (value   "A binding that binds LINE to the current planning goal."))
  (when (stringp hyps)
    (mapcan #'(lambda (open)
		(list (list (cons hyps (pdsn~hyps open)))))
	    (append (list (agenda~task-node cri*current-task))
		    (mapcar #'agenda~task-node cri*current-tasks)))))


;;***********************************************
;;*      goal matches 
;;***********************************************
(defun goal-matches (parameter)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   "A list (LINE META-FORMULA).")
	   (effect  )
	   (value   "A binding of the strings (variables) in META-FORMULA, if"
		    "there's a matching to the formula of the current goal."
		    "NIL, otherwise."))
  (let* ((line (first parameter))
	 (task cri*current-task)
	 (goal (agenda~task-node task))
	 (meta-formula (second parameter))
	 (result ())
	 )
    (when meta-formula
      (if (stringp line)
	  (let* ((mvar-subst (if (pds~constraint-pool omega*current-proof-plan)
				 (pds~cstrpool-bindings (pds~constraint-pool omega*current-proof-plan))
			       (subst~create nil nil)))
		 (bind (cri=string-matching meta-formula
					    (if (pdsn~schematic-p goal)
						(progn
						  (when (or (null (pdsn~up-to-date goal))
							    (null (pdsn~current-formula goal)))
						    (setf (pdsn~current-formula goal)
							  (subst~apply mvar-subst (node~formula goal))))
						  (pdsn~current-formula goal))
					     (node~formula goal))
					    (list (cons T T)))))
	    ;;(omega~message "~& bind: ~S ~S ~S" meta-formula goal bind)
	    (when bind 
	      (list (cons (cons line goal)
			  bind))))
	nil))))  ;; muss noch implementiert werden.

;;***********************************************
;;*       hyps of line
;;***********************************************
(defun hyps-of-line (line hyps)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   "A pdsn+node LINE and a string for the hypotheses of LINE.")
	   (effect  )
	   (value   ))
  ;; gives the hypothesises of the line
  ;; line must be bound, hyps not.
  (list (list (cons hyps (pdsn~hyps line)))))

;;***********************************************
;;*      last method
;;***********************************************
(defun last-method (method)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  (let* ((last-step (pds~last-plan-step cri*current-pds)))
    (when last-step
      (let* ((just (pdsc~an-just last-step))
	     (infer (just~method just))
	     )
	(when (or (infer~method-p infer)
		  (infer~supermethod-p infer))
	  (let* ((node (pdsc~an-node last-step))
		 (outln-pat (cri=actual-outline-pattern node
							cri*current-pds
							just))
		 (last-method (pds~inference-application infer outln-pat))
		 )
	    (when (eql (keim~name last-method)
		       method)
	      (list (list (cons T T))))))))))

;;***********************************************
;;*     not yet applied <method>
;;***********************************************
(defun not-yet-applied (parameters)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  ;; parameters is a list: a method 
  ;; checks if the method has already been applied.
  (let* ((first-step (pds~first-plan-step cri*current-pds))
	 (all-steps (cri=all-plan-steps first-step))
	 (method-name (first parameters))
	 )
    (plan~trace "~% currpds: ~S  " cri*current-pds)
    (plan~trace "~% firststeps: ~S  " first-step)
    (plan~trace "~% allsteps: ~S  " all-steps)
    (plan~trace "~% proof-lines: ~S" (prob~proof-steps cri*current-pds))
    (dolist (step all-steps)
      (let* ((node (pdsc~an-node step))
	     (just (pdsc~an-just step))
	     (infer (just~method just))
	     (outln-pat (cri=actual-outline-pattern node
						    cri*current-pds
						    just))
	     (method (pds~inference-application infer outln-pat)))
	(plan~trace "~% outline: ~S ~S " outln-pat method)
	(when (and method
		   (eql (keim~name method) method-name))
	  (return-from not-yet-applied nil))))
    (list (list (cons T T)))))

;;***********************************************
;;*      last assumption
;;***********************************************
(defun last-assumption (line)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  ;; parameters is a line
  ;; gives the lines which resulted from the last application of the method assumes that line is unbound
  (mapcar #'(lambda (x)
	      (list (cons line x)))
	  (cri=find-latest-assumptions)))



;;***********************************************
;;*  assumption matches
;;***********************************************
(defun assumption-matches (parameter)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  ;; parameter is a list: (line term-that-matches?)
  ;; value: a list of all goals that match term-that-matches?
  (let* ((line (first parameter))
	 (meta-formula (second parameter))
	 (result ())
	 )
    (when meta-formula
      (cond ((stringp line)
	     (mapcan #'(lambda (ass)
			 (let ((bind (cri=string-matching meta-formula
							  (node~formula ass)
							  (list (cons t t)))))
			(when bind
			  (list (append (butlast bind) (list (cons line ass)))))))
		     cri*current-supports))
	    ((listp line)
	     (mapcan #'(lambda (ass)
			 (let ((bind (cri=string-matching meta-formula
							  (node~formula ass)
							  (list (cons t t)))))
			   (when bind
			     (list (butlast bind)))))
		     line))
	    ((pdsn~p line)
	     (mapcan #'(lambda (ass)
			 (let ((bind (cri=string-matching meta-formula
							  (node~formula ass)
							  (list (cons t t)))))
			   (when bind
			     (list (butlast bind) ))))
		     (list line)))
	    (T nil)))))  ;; muss noch implementiert werden.


(defun cri=string-matching (meta formula binding)
  (declare (edited  "27-MAR-1998")
	   (authors Jzimmer)
	   (input   "A 'meta' formula containing strings as meta-variables,"
		    "a formula and a binding (an alist).")
	   (effect  )
	   (value   "The binding of the strings (an alist), iff a matching"
		    "could be found."))
  
  (cond ((null meta) binding)
	((listp meta)
	 (cond ((and (term~appl-p formula)
		     (term~primitive-p (data~appl-function formula)))
		(if (stringp (first meta))
		    (cri=string-matching (rest meta)
					 (data~appl-arguments formula)
					 (cons (cons (first meta)
						     (data~appl-function formula))
					       binding))
		  (let ((func (env~lookup-object (first meta)
						 (pds~environment cri*current-pds)))
			(real-func (env~lookup-object (keim~name (data~appl-function formula))
						      (pds~environment cri*current-pds)))
			)
		    (if func
			(if (data~equal real-func func)
			    (cri=string-matching (rest meta) (data~appl-arguments formula)
						 binding)
			  nil)
		      nil))))
	       ((term~schema-p formula)
		(cri=string-matching meta (data~schema-range) binding))
	       ((consp formula)
		(cond ((symbolp (first meta))
		       (let ((symbol (env~lookup-object (first meta)
							(pds~environment cri*current-pds)))
			     )
			 (if symbol
			     (if (data~equal (first formula)
					     symbol)
				 (cri=string-matching (rest meta)
						      (rest formula)
						      binding)
			       nil)
			   nil)))
		      ((stringp (first meta))
		       (cri=string-matching (rest meta)
					    (rest formula)
					    (cons (cons (first meta)
							(first formula))
						  binding)))
		      (T (let ((matched (cri=string-matching (first meta)
							     (first formula)
							     binding)))
			   (if matched 
			       (append (butlast matched)
				       (cri=string-matching (rest meta)
							    (rest formula)
							    binding))
			     NIL)))))
	       (T NIL)))
	(T NIL)))

;;***********************************************
;;*        last assumption matches
;;***********************************************
(defun last-assumption-matches (parameter)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  ;; parameter is a list: (line term-that-matches?)
  ;; value: a list of all goals that match term-that-matches?
  (let* ((line (first parameter))
	 (meta-formula (second parameter))
	 (result ())
	 )
    (when meta-formula
      (if (stringp line)
	  (apply #'append
		 (mapcar #'(lambda (ass)
			     (let ((bind (cri=string-matching meta-formula
							      (node~formula ass)
							      (list (cons t t)))))
			       (when bind
				 (list (list (cons line ass))))))
			 (cri=find-latest-assumptions)))
	nil))))





(defun cri=all-plan-steps (step)
  (declare (edited  "18-MAY-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  (when step
    (let* ((just (pdsc~an-just step))
	   (next (pdsj~successor just))
	   )
      (cons step (cri=all-plan-steps next)))))


;;***********************************************
;;*  sub of latest assumption
;;***********************************************
(defun sub-of-latest-assumption (parameter)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  ;; parameter is a list, term and line
  ;; term may be bound or not, line must be unbound
  ;; value: all possible combinations of subterms and latest assumptions
  ;; latest assumptions are those lines with no pdsn~successor
  (let ((latest-assumptions (cri=find-latest-assumptions))
	(term (first parameter))
	(ass (second parameter)))
	   ;(format t "~% latest-ass: ~S"
	   ;    (mapcar #'node~formula latest-assumptions))
    (if (stringp term) ;; term is not bound:
	(mapcan #'(lambda (assumption)
		    (cri=combine-ass-terms assumption term ass))
		latest-assumptions)
      ;; term is bound, so it is either a real term or a symbol
      (let ((new-term (if (term~p term) term
			(env~lookup-object term (pds~environment cri*current-pds))))
	    )
	(mapcan #'(lambda (line)
		    (when (data~substruct-positions
			   new-term
			   (node~formula line))
		      (list (list (cons ass line)))))
		latest-assumptions)))))

(defun cri=find-latest-assumptions ()
  (let* ((last-step (pds~last-plan-step cri*current-pds)))
    (when last-step
;      (format t "~% last-step: ~S" last-step)
      (let* ((just (pdsc~an-just last-step))
	     (infer (just~method just))
	     )
	(when (or (infer~method-p infer)
		  (infer~supermethod-p infer))
	  (let* ((node (pdsc~an-node last-step))
		 
		 (outln-pat (cri=actual-outline-pattern node
							cri*current-pds
							just))
		 
		 (method (pds~inference-application infer outln-pat))
		 )
	    (when method
	      (let* ((mmapp (pdsj~subst just))
		     )
		(multiple-value-bind (add-concs prems new-nodes)
		    (meth~complete-outlines (meth~add-conclusions method)
					    nil
					    mmapp
					    nil ;(prob~proof-steps cri*current-pds)) ;;BULLSHIT!!!
					    )
		  add-concs)))))))))

(defun cri=actual-outline-pattern (node &optional (pds omega*current-proof-plan)
					(just (node~justification node)))
  (let* ((outln-pat (pdsj~outline-pattern just))
	 (prems (just~premises just))
	 (concs-l (- (length outln-pat) (length prems))))
    (if (> concs-l 1)
	(multiple-value-bind (pat concs)
	    (cri=adapt-outline-pattern outln-pat node (- concs-l 1) pds)
	  pat)
      outln-pat)
    ))

(defun cri=adapt-outline-pattern (pattern conc concs-l pds)
  (declare (edited  "15-APR-1998" "27-JUN-1997")
	   (authors Jzimmer Lassaad)
	   (input   "An outline-pattern, a node, a positive integer, and a PDS:"
		    "CONC is justified by the same method application together"
		    "with CONCS-L other nodes in PDS.")
	   (effect  "None.")
	   (value   "Two values: the outline pattern that corresponds to the method"
		    "used for justifying NODE and eventually other nodes, a list of"
		    "the nodes that are justified using this method."))
  (if (zerop concs-l)
      (if conc
	  (values pattern (list conc))
	(values pattern nil))
    (let ((first-pat (first pattern)))
      (if (or (infer~existent-pattern-p first-pat)
	      (infer~nonexistent-pattern-p first-pat))
	  ;;;FIRST-PAT is the outline of CONC:
	  (multiple-value-bind (patt concs)
	      (cri=adapt-outline-pattern (rest pattern) nil concs-l pds)
	    (values (cons first-pat patt)
		    (cons conc concs)))
	;;;FIRST-PAT is a label of another conclusion:
	(multiple-value-bind (patt concs)
	    (cri=adapt-outline-pattern (rest pattern) conc (- concs-l 1) pds)
	  (let* ((node (pds~label2node first-pat pds))
		 )
	    (if node
		(let* ((node-just (node~justification node))
		       (node-pat (pdsj~conclusion-outline node-just)))
		  (values (cons node-pat patt)
			  (cons node concs)))
	      pattern)))))))

(defun cri=combine-ass-terms (assumption line-name term-name)
  (mapcar #'(lambda (term)
	      (list (list line-name assumption)
		    (list term-name term)))
	  (cri=get-subterms (node~formula assumption))))
	      
	    
(defun cri=get-subterms (term)
  (cond
   ((listp term) (mapcar #'cri=get-subterms term))
   ((term~appl-p term) (cons term (cri=get-subterms (rest (data~substructs term)))))
   ((or (logic~existential-quantification-p term) (logic~universal-quantification-p term))
    (cons term (cri=get-subterms(logic~quantification-scope term))))
   (t term)))

;;***********************************************
;;*     sub of assumption
;;***********************************************
(defun sub-of-assumption (parameter)
  ;; parameter is a list, term and line
  ;; term may be bound or not, line must be unbound
  ;; value: all possible combinations of subterms and assumptions
  (let* ((assumptions cri*current-supports) 
	(term (post~read-object (first parameter)
				(pds~environment cri*current-pds)
				:existing-term))
	(ass (second parameter))
	(right-ass (mapcan #'(lambda (line)
			       (when (data~substruct-positions
				      term
				      (node~formula line))
				 (list (list (cons ass line)))))
			   assumptions) )
	)
    (if (stringp ass) ;; term is not bound:
	(progn ;(format t "~% assumptions : ~S" right-ass)
	       right-ass)
      (mapcan #'(lambda (assumption)
		  (cri=combine-ass-terms assumption term ass))
	      assumptions)
      )))



;;***********************************************
;;*       most similar nodes to a given formula
;;***********************************************
(defun most-similar-node-to-formula (formula nodes best-node)
  (declare (edited  "13-Feb-2000")
	   (authors Jzimmer)
	   (input   "A FORMULA, the set of nodes to choose from, and a string for the most similar nodes.")
	   (effect  )
	   (value   "A list with node with a formula that is most similar to "
		    "FORMULA, iff one could be found."))
  
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
					    (T nil)))))
	 (matchings (mapcar #'(lambda (res)
				(list (cons best-node (list (first res)))))
			    sorted))
	 
	 )
      ;;(omega~message "~& alist: ~S" formula-alist)
      ;;(omega~message "~& preresult: ~S" pre-result)
      ;;(omega~message "~& sorted: ~S" sorted)
      ;;(omega~message "~& matchings: ~S" matchings)
      (if matchings
          (list (first matchings))
        nil))))
    
  
;;***********************************************
;;* most-similar-subterm-in-asss-to-goal
;;***********************************************
(defun most-similar-subterm-in-asss-to-goal (args)
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
		  (cri=most-similar-help (cons (agenda~task-node task)
					       (rest args))))
	      cri*current-tasks)
    (let ((result (cri=most-similar-help args)))
      ;;(format t "~& final result: ~S ~S" args result)
      result)))

(defun cri=most-similar-help (args)
  (let* ((goal (first args))
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
			     (let ((res (cri=get-most-similar-subterm-of-ass goal-alist
									     assumption)))
			       (when res
				 (list res))))
			 right-supp)))
	 (sorted (sort pre-result #'(lambda (match1 match2)
				      (cond ((> (third match1) (third match2))
					     T)
					    ((= (third match1) (third match2))
					     (let* ((dep1 (logic~depth (node~formula (first match1))))
						    (dep2 (logic~depth (node~formula (first match2))))
						    )
					       (< dep1 dep2)))
					    (T nil)))))
	 (matchings (mapcar #'(lambda (res)
				(list (cons ass-string (first res))
				      (cons pos-string (second res))))
			    sorted))
	 )
    (if (> (length matchings) 4)            ;; return at most four alternative matchings
	(list (first matchings) (second matchings) (third matchings) (fourth matchings))
      matchings)))

(defun cri=get-most-similar-subterm-of-ass (goal-alist ass)
  (let* ((ass-formula (node~formula ass))
	 (subformulas (cri=get-positive-subformulas ass-formula))
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

(defun cri=get-positive-subformulas (formula)
  ;;(format t "~& formula: ~S" formula)
  (cond ((null formula)
	 NIL)
	((listp formula)
	 (apply #'append (mapcar #'cri=get-positive-subformulas formula)))
	((term~abstr-p formula)
	 (cri=get-positive-subformulas (data~abstr-range formula)))
	((logic~implication-p formula)
	 (cri=get-positive-subformulas (second (data~appl-arguments formula))))
	((logic~conjunction-p formula)
	 (cri=get-positive-subformulas (data~appl-arguments formula)))
	((term~appl-p formula)
	 (if (keim~equal (term~type formula) (type~o))
	       (list formula)
	     (cri=get-positive-subformulas (data~appl-arguments formula))))
	(T nil)))

(defun cri=extract-and-better? (ass goal-alist pos result)
  (let* ((term (data~struct-at-position (node~formula ass) pos))
	 (new (cri=compare term
			   goal-alist))
	 (old (third result)))
    ;;       (format t "~% new-value: ~S --> ~S" (data~struct-at-position (node~formula ass) pos) new)
    (if (not (second result))
	(list ass pos new))
    (cond ((< new old) result)
	  ((> new old) (list ass pos new))
	  (t
	   (let ((new-depth (logic~depth (node~formula ass)))
		 (old-depth (logic~depth (node~formula (first result)))))
	     (cond ((> new-depth old-depth) result)
		   ((< new-depth old-depth) (list ass pos new))
		   (T result)))))))


(defun cri=compare (ass goal-alist)
  (let* ((ass-alist (cri=analyze ass nil))
	 (symbols (remove-duplicates
		   (append (mapcar #'car ass-alist)
			   (mapcar #'car goal-alist))
		   :key #'keim~name
		   :test #'eql))
	 (diff 0)
	 )
    (dolist (symbol symbols)
      (let* ((ass-pair (assoc (keim~name symbol) ass-alist
			      :key #'keim~name
			      :test #'eql))
	     (ass-val (if ass-pair (cdr ass-pair) 0))
	     (goal-pair (assoc (keim~name symbol) goal-alist
			       :key #'keim~name
			       :test #'eql))
	     (goal-val (if goal-pair (cdr goal-pair) 0))
	     )
	(cond ((= ass-val goal-val)
	       (setq diff
		     (+ diff
			(* 2
			   (cri=get-weight symbol)))))
	      ((= ass-val 0)
	       (setq diff
		     (- diff
			(* 2 (* goal-val
				(cri=get-weight symbol))))))
	      ((= goal-val 0)
	       (setq diff
		     (- diff
			(* 2 (* ass-val
				(cri=get-weight symbol))))))
	      ((> ass-val goal-val)
	       (setq diff
		     (- diff
			(* (- ass-val goal-val)
			   (cri=get-weight symbol)))))
	      ((< ass-val goal-val)
	       (setq diff
		     (- diff
			(* (* (- goal-val ass-val)
			      (cri=get-weight symbol))))))
	      (T T))))
    diff))

(defun cri=analyze (term alist)
  (cond ((null term) alist)
	((listp term)
	 (let ((new-alist (cri=analyze (first term) alist)))
	   (cri=analyze (rest term) new-alist)))
	((term~appl-p term)
	 (let* ((func (data~appl-function term))
		)
	   (if (term~abstr-p func)
	       (cri=analyze (data~appl-arguments term) alist)
	     (let* ((value (cri=get-value func alist))
		    (new-alist (cri=increment-value func alist))
		    )
	       (cri=analyze (data~appl-arguments term) new-alist)))))
	((term~abstr-p term)
	 (cri=analyze (data~abstr-range term) alist))
	(t alist)))

(defun cri=get-value (symbol alist)
  (let ((value (assoc symbol alist)))
    (if value
	(cdr value)
      0)))

(defun cri=increment-value (symbol alist)
  (let ((value (assoc symbol alist)))
    (if value
	(progn (incf (cdr (assoc symbol alist)))
	       alist)
      (cons (cons symbol 1) alist))))

(defun cri=get-weight (symbol)
  (let ((pair (assoc (keim~name symbol)
		     cri*symbol-weights
		     :key #'symbol-name
		     :test #'eql))
	)
    (if pair
	(cdr pair)
      2)))


;;***********************************************
;;*         no focus in 
;;***********************************************
(defun no-focus-in (line)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  (if (stringp line)
      (let ((result
	     (apply #'append
		    (mapcar #'(lambda (node)
				(unless (data~substruct-positions
					 (env~lookup-object 'focus (pds~environment cri*current-pds))
					 (node~formula node))
				  (list (cons line node))))
			    cri*current-supports)))
	    )
	(list result))
    (unless (data~substruct-positions
	     (env~lookup-object 'focus (pds~environment cri*current-pds))
	     (node~formula line))
      line)))

;;***********************************************
;;*         symbol-member
;;***********************************************
(defun symbol-member (Sym Symbols)
  (declare (edited  "13-AUG-1999")
	   (authors Jzimmer)
	   (input   "A POST SYMBOL and a list of lisp-SYMBOLS.")
	   (effect  )
	   (value   "T, iff the Sym is a TERM+PRIMITIVE and the keim~name of the Sym is a"
		    "member of Symbols."))
  ;;(omega~message "~& member: ~S ~S" (term~primitive-p Sym)(member (intern (keim~name Sym)) Symbols :test #'eql)) 
  (when (and (term~primitive-p Sym)
	     (member (intern (keim~name Sym)) Symbols :test #'eql))
    (list (list (cons T T))))
  )


;;
(defun hyps-different (line hyplist)
  (declare (edited  "03-DEC-2001")
	   (authors Pollet)
	   (input   )
	   (effect  "avoid loops with increasehyps")
	   (value   ))
  (when (and (not (stringp line))(not (stringp hyplist)))
    (when (some #'(lambda (hyp)(not (member hyp (pdsn~hyps line)))) hyplist) (list (list (cons t t))))))
