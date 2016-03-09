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

(cri~def-control-rule perm-by-generators
		      (kind methods)
		      (if (and 	 (not-yet-applied-in-concs (Re-Represent-with-generators-M-b))
				 (and (find-permutation "perm" "position")
				      (find-generators "perm" "generator-set"))))
		      (then
		       (insert ((Re-Represent-with-generators-m-b () ("generator-set" "position"))))))

(defun not-yet-applied-in-concs (parameters)
  (let ((all-nodes (prob~proof-steps  cri*current-pds)))
  (labels ((collect-nodes (node)
			  (let ((concs (remove-if-not #'(lambda (prem)(member node (pdsn~just-premises prem)))
						      all-nodes)))
;			    (omega~trace "collect ~A" concs)
			    (append concs (mapcan #'collect-nodes concs)))))
    (let* ((current-goal cri*current-task-node)
	   (all-steps (collect-nodes current-goal))
	   (method-name (first parameters)))
;      (omega~trace "~A" all-steps)
      (if (some #'(lambda (step)
		    (let* ((just (node~justification step))
			   (infer (just~method just))
			   (outln-pat (cri=actual-outline-pattern step
								  cri*current-pds
								  just))
			   (method (pds~inference-application infer outln-pat)))
;		      (omega~trace "~A ~A" method method-name)
		      (and method (eql (keim~name method) method-name))))
		all-steps)
	  nil
	(list (list (cons T T))))))))
  

;;; Bemerkungen:
;;; 1. Methode muss evtl. mehrfach anwendbar sein. Oder sollen immer gleich alle Permutationen auf einmal Rerepraesentiert werden?
;;;    Evtl. gibt es mehrere Zeilen, die bearbeitet werden muessen.
;;; 2. Warum werden alle Zeilen untersucht? Reichen nicht auch nur alle offenen Zeilen?
;;; 3. Wenn wir geschlossene Zeilen betrachten, dann muss das eine andere Methode machen. Das ist dann ja eine Ableitung, oder?
;;; 4. Rerepraesentation in Supports koennte wichtig sein.
; YES, you are right. I've restricted the test to re-representations in the same proof branch, but obviously some control as:
; 'don't do the re-representation on modified formulas or on its subformulas again' would be nicer. MP


(defun find-permutation (perm pos) 
  (let ((result (pecr=top-permutations (pdsn~current-formula (agenda~task-node cri*current-task)) (pos~empty))))
    (mapcar #'(lambda (x) (acons pos (cdr x)(acons perm (car x) nil))) result)))

(defun find-generators (perm gen-set)
  (let ((all-lines (list (pdsn~current-formula cri*current-task-node))))
    ;(maphash #'(lambda (x y) (push (pdsn~current-formula y) all-lines)) (pds~label-node-hashtable cri*current-pds))
    (let* ((in (env~lookup-object 'in  (pds~environment cri*current-pds)))
	   (generated-set (env~lookup-object 'generated-set (pds~environment cri*current-pds)))
	   (lines-gen-perm (remove-if-not
			    #'(lambda (formula)
				(or  (and (member (pos~list-position '(2))   ;;; was soll das? (G g)? Geht nur (in g G), oder auch P(in g G) <- lieber nicht: P=not?
					     (data~substruct-positions perm  formula)
					     :test #'keim~equal)
					  (data~appl-p formula)
					  (data~schema-equal  (data~appl-function formula) generated-set))
					 (and  (member (pos~list-position '(1))
						       (data~substruct-positions perm  formula)
						       :test #'keim~equal)
					       (data~appl-p formula)
					       (data~schema-equal (data~appl-function formula) in)
					       (data~appl-p (second (data~appl-arguments formula)))
					       (data~schema-equal (data~appl-function (second (data~appl-arguments formula))) generated-set))))
			    all-lines))
;	   (dummy (omega~trace "lines-with-gen-prem ~A" lines-gen-perm))
	   (generated-sets (mapcar #'(lambda (form)
				       (find-if #'(lambda (sub)
						    (when (data~appl-p sub)
						      (data~schema-equal (data~appl-function sub) generated-set)))
						(data~all-substructs form )))
				   lines-gen-perm))
;	   (dummy (omega~trace "gen-sets ~A" generated-sets))
	   ;(new-perms (mapcar #'(lambda (set) (perm~gap-is-in-proof perm set)) generated-sets))
	   )
      (when generated-sets
	  (list (mapcar #'(lambda (generated-s)
			    (cons gen-set generated-s)) generated-sets))))))
      
(defgeneric pecr=top-permutations (term current-pos)
  (:method ((term term+set) (current-pos pos+position))
	   (when (data~equal (term~type term) (post~read-object '(o cyc) (pds~environment omega*current-proof-plan) :existing-type))
	     (acons term current-pos nil)))
  (:method ((term term+primitive) current-pos)
	   (when (eq term perm*identity-perm)
	     (acons term current-pos nil)))
  (:method ((term term+abstr) (current-pos pos+position))
	   (pecr=top-permutations (data~abstr-range term) (pos~concatenate current-pos (pos~list-position '(0)))))
  (:method ((term term+appl) (current-pos pos+position))
	   (if (data~equal (term~type term) (post~read-object '(o cyc) (pds~environment omega*current-proof-plan) :existing-type))
	       (acons term current-pos nil)
	     (let ((subterms (cons (data~appl-function term) (data~appl-arguments term))))
	       (mapcon #'(lambda (subterm)
			   (pecr=top-permutations (car subterm) (pos~concatenate current-pos (pos~list-position (list (- (length subterms)
														     (length subterm)))))))
		       subterms)))))


(defun singleton? (arg)
  (when (and (term~set-p arg)
	     (= (length (term~normalform arg)) 1))
    (list (list (cons T T)))))

(cri~def-control-rule permutation-defnexp-replace
		      (kind methods)
		      (if (or
			   (and (or (goal-matches ("goal" ("rel" "arg1" "arg2")))
				    (goal-matches ("goal" (not ("rel" "arg1" "arg2")))))
				(symbol-member "rel" (in g-orbit g-orbit-representation subsetp stabiliser perm-stab-base)))
			   (and (goal-matches ("goal" ("rel" "arg1")))
				(singleton?  "rel"))))
		      (then (insert-end ((DefnExp-m-b () ("rel"))))))



(cri~def-control-rule prefer-eval
		      (kind methods)
		      (if (always-true))
		      (then (prefer (eval-function-m-b
				     eval-permutation-m-b))))
					
(cri~def-control-rule perm-subst-concrete
		      (kind methods)
		      (if (and (assumption-matches ("assump" (= "lhs" "rhs")))
			       (or (and (concrete-term "lhs")
					(goal-contains-at-pos "goal" "rhs" "pos"))
				   (and (concrete-term "rhs")
					(goal-contains-at-pos "goal" "lhs" "pos")))))
		      (then
		       (prefer ((=subst-m-b ("assump") ("pos"))))))

(defun concrete-term (term)
  (let* ((binding (and (pds~constraint-pool cri*current-pds)
		       (pds~cstrpool-bindings (pds~constraint-pool cri*current-pds))))
	 (trm (if binding (subst~apply binding term) term)))
  (or (term~number-p trm)
      (term~set-p trm)
      (term~cyc-p trm)
      (term~list-p trm))))

;;; control rule that avoids successive application of the same method to the same node,
;;; when expansion is carried out by recursive planning.
(defun above-methods (methods)
  (declare (edited  "10-DEC-2002")
	   (authors Vxs)
	   (input   "A string")
	   (effect  "None.")
	   (value   "A list of methods that are in the above justification of the current goal.")
	   (remark  "This is a planning meta-predicate."))	   
  (let* ((next-node  (when cri*current-task (agenda~task-node cri*current-task)))
	 (above-justs (when next-node (pdsj~above-justs (node~justification next-node))))
	 (applied-methods (mapcar #'(lambda (just)
				      (pds~inference-application
				       (just~method just)
				       (exp~actual-outline-pattern next-node omega*current-proof-plan just)))
				  above-justs)))
    (omega~trace "Already applied methods: ~{~A ~}~%" (mapcar #'keim~name applied-methods))
    (when applied-methods
      (list (list (cons methods (list applied-methods)))))))


(cri~def-control-rule recursive-expansion
		      (kind methods)
		      (if (above-methods "methods"))
		      (then (reject "methods")))
