;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Keim -*-
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

(eval-when (load compile eval)
  (unless (com~find-category 'permutation)
    (com~defcategory permutation
		     (help "Tactics of the theory PERMUTATION.")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some constants to fix the theory signature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant perm*cons (env~lookup-object 'cons (th~env perm*permutation-theory)))
(defconstant perm*nil (env~lookup-object 'cnil (th~env perm*permutation-theory)))
(defconstant perm*first (env~lookup-object 'first (th~env perm*permutation-theory)))
(defconstant perm*last (env~lookup-object 'last (th~env perm*permutation-theory)))
(defconstant perm*identity-cyc (env~lookup-object 'identity-cyc (th~env perm*permutation-theory)))
(defconstant perm*cycle-sort (env~lookup-object 'cycle (th~env perm*permutation-theory)))
(defconstant perm*cycle-set (env~lookup-object 'cycle-set (th~env perm*permutation-theory)))
(defconstant perm*cycle-disjoint (env~lookup-object 'cycle-disjoint (th~env perm*permutation-theory)))
(defconstant perm*permutation-sort (env~lookup-object 'permutation (th~env perm*permutation-theory)))
(defconstant perm*perm-compose (env~lookup-object 'perm-compose (th~env perm*permutation-theory)))
(defconstant perm*perm-exp (env~lookup-object 'perm-exp (th~env perm*permutation-theory)))
(defconstant perm*perm-inverse (env~lookup-object 'perm-inverse (th~env perm*permutation-theory)))
(defconstant perm*perm-apply (env~lookup-object 'perm-apply (th~env perm*permutation-theory)))
(defconstant perm*generated-set (env~lookup-object 'generated-set (th~env perm*permutation-theory)))
(defconstant perm*identity-perm (env~lookup-object 'identity-perm (th~env perm*permutation-theory)))
(defconstant perm*g-orbit (env~lookup-object 'g-orbit (th~env perm*permutation-theory)))
(defconstant perm*g-orbit-representation (env~lookup-object 'g-orbit-representation (th~env perm*permutation-theory)))
(defconstant perm*stabiliser (env~lookup-object 'stabiliser (th~env perm*permutation-theory)))


)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some predicates on the signature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Product-Of-Generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic product-of-generators
		 (outline-mappings (((existent list) product-of-generators-a)
				    ((existent nonexistent) product-of-generators-b)))
		 (expansion-function permtac=expand-product-of-generators)
		 (help "Show that an element of a group is a product of the generators of the group."))


(defun product-of-generators-b (concs prems parameters)
  (declare (ignore prems parameters))
  (multiple-value-bind (element set)
      (permtac=g-in-generated-set-p (car concs))
    (when (and element set)
      (let ((in (env~lookup-object :in (th~env :permutation))))
	(values nil
		(mapcar #'(lambda (elem)
			    (term~appl-create in (list elem set)))
			(perm~decompose-into-permutations element))
		nil)))))


(defun product-of-generators-a (concs prems parameters)
  (declare (ignore parameters))
  ;;; to be completed
  (omega~warn "Not yet implemented!")
  )

(defun permtac=g-in-generated-set-p (formula)
  (declare (edited  "10-AUG-2002")
	   (authors Vxs)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "If the formula is of the form (in g (generated-set sigma)) or ((generated-set sigma) g)"
		    "the function returns g and sigma as two values. Otherwise it returns NIL."))
  (when (data~appl-p formula)
    (let ((func (data~appl-function formula))
	  (args (data~appl-arguments formula)))
      (flet ((generated-set-p (term)
			      (when (data~appl-p term)
				(let ((args (data~appl-arguments term)))
				  (when (and (= (length args) 1)
					     (data~schema-equal (data~appl-function term) perm*generated-set))
				    (car args))))))
	(cond ((= (length args) 1)
	       (let ((set (generated-set-p func)))
		 (when set (values (car args) set))))
	      ((= (length args) 2)
	       (let ((set (generated-set-p (cadr args))))
		 (when (and set (data~schema-equal func (env~lookup-object :in (th~env :permutation))))
		   (values (car args) set)))))))))
	  
(defun permtac=expand-product-of-generators (concs prems parameters)
  (declare (ignore concs prems parameters))
  (omega~warn "Not yet implemented!"))


(com~defcommand product-of-generators
  (argnames product-in-G generators-in-G)
  (argtypes ndline ndline-list)
  (arghelps "Line with g in G" "Lines with g1 in G ... gn in G")
  (function permtac=product-of-generators)
  (frag-cats tactics base)
  (defaults permtac=product-of-generators-defaults)
  (log-p T)
  (level 6)
  (help "Show that an element of a group is a product of the generators of the group."))

(defun permtac=product-of-generators (product-in-G generators-in-G)
  (infer~compute-outline 'product-of-generators (list product-in-G generators-in-G) nil))


(defun permtac=product-of-generators-defaults (product-in-G generators-in-G)
  (cond ((not (com~specified-arg-p product-in-G))
	 (list (pds~find-open-node #'permtac=g-in-generated-set-p) (com~unspecified)))
	((not (com~specified-arg-p generators-in-G))
	 (list product-in-G nil))
	(t (list product-in-G generators-in-G))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-Permutation-Backward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-permutation-backward
		 (outline-mappings (((existent existent) eval-permutation-backward-a)
				    ((existent nonexistent) eval-permutation-backward-b)))
		 (expansion-function permtac=expand-eval-permutation-backward)
		 (parameter-types position)
		 (help "Evaluates a permutation application."))

(com~defcommand eval-permutation-backward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "An open line containing a permutation application"
            "A closed line with the result of the application"
            "The position of the permutation application")
  (function permtac=eval-permutation-backward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Evaluates a permutation application backward."))

(defun permtac=eval-permutation-backward (l1 l2 pos)
  (infer~compute-outline 'eval-permutation-backward (list l1 l2) (list pos)))

(defun permtac=eval-permutation-backward-p (L1 Pos)
  (let ((perm-appl (data~struct-at-position l1 pos)))
    (and (data~appl-p perm-appl)
	 (data~schema-equal (data~appl-function perm-appl) perm*perm-apply)
	 (perm~perm-type-p (car (data~appl-arguments perm-appl)))
	 (not (term~variable-p (car (data~appl-arguments perm-appl)))))))

(defun permtac=eval-permutation-backward-b (L1 Pos)
  (peme=apply-perm L1 Pos))

(tac~deftactic eval-permutation-backward-b eval-permutation-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (permtac=eval-permutation-backward-b (formula L2) POS)))
   (sideconditions (permtac=eval-permutation-backward-p (formula L2) Pos))
   (description "Backward application of Eval-Permutation-Backward."))


(tac~deftactic eval-permutation-backward-a eval-permutation-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-permutation-backward-ap
                    (formula L1) (formula L2) Pos))
   (description "Test application of Eval-Permutation-Backward."))


(defun permtac=eval-permutation-backward-ap (L1 L2 Pos)
  (data~equal (permtac=eval-permutation-backward-b L1 Pos) L2))


(defun permtac=expand-eval-permutation-backward (outline parameters)
  (tacl~init outline)
  (permtac=stepwise-apply-permutation-backward (car outline) (cadr outline) (car parameters))
  (tacl~end)
)

(defun permtac=stepwise-apply-permutation-backward (begin end position &optional step-axiom)
  (declare (edited  "24-DEC-2002")
	   (authors Vxs)
	   (input   "Two lines, a position, and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (let* ((term (data~struct-at-position (node~formula begin) position))
	 (set (car (data~appl-arguments term)))
	 (elements (unless (data~equal set perm*identity-perm)
		     (perm=decompose-set set)))
	 (argument (cadr (data~appl-arguments term))))
  (if (null elements)
      (let ((base-axiom (tacl~insert&return-assumption 'permutation 'apply-perm-base)))
	(tacl~sequence
	 (foralle ('foralle (list nil base-axiom) (list argument)))
	 (dummy ('=subst (list begin end (car foralle)) (list position)))
	))
    (let* ((step-axiom (or step-axiom (tacl~insert&return-assumption 'permutation 'apply-perm-step)))
	   (tyse*emptyset 'identity-perm)
	   (result
	    (tacl~sequence
	     (foralle ('foralle (list nil step-axiom) (list argument)))
	     (foralle-sort ('foralle-sort (list nil (car foralle) nil) (list set)))
	     (dummy0 ('is-permutation (last foralle-sort) nil))
	     (existse-sort ('existse-sort (list begin (car foralle-sort) nil)
					  (list (post~read-object (car elements) (pds~environment omega*current-proof-plan) :existing-term))))
	     (ander ('ander (list nil (fourth existse-sort)) nil))
	     (re-set ('rerepresent-set (list nil (car ander)) (list (pos~list-position '(2 1)))))
	     (eval-cyc ('eval-cycle-forward (list nil (car re-set)) (list (pos~list-position '(2 2)))))
	     (dummy1 ('=subst (list (third existse-sort) nil (car eval-cyc)) (list position))))))
      (permtac=stepwise-apply-permutation-backward (cadr result) end position step-axiom)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-Permutation-Forward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-permutation-forward
		 (outline-mappings (((existent existent) eval-permutation-forward-a)
				    ((nonexistent existent) eval-permutation-forward-f)))
		 (expansion-function permtac=expand-eval-permutation-forward)
		 (parameter-types position)
		 (help "Evaluates a permutation application."))

(com~defcommand eval-permutation-forward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A closed line containing a permutation application"
            "An open line with the result of the application"
            "The position of the permutation application")
  (function permtac=eval-permutation-forward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Evaluates a permutation application forward."))

(defun permtac=eval-permutation-forward (l1 l2 pos)
  (infer~compute-outline 'eval-permutation-forward (list l2 l1) (list pos)))

(tac~deftactic eval-permutation-forward-f eval-permutation-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (permtac=eval-permutation-backward-b (formula L1) POS)))
   (sideconditions (permtac=eval-permutation-backward-p (formula L1) Pos))
   (description "Forward application of Eval-Permutation-Forward."))


(tac~deftactic eval-permutation-forward-a eval-permutation-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-permutation-backward-ap
                    (formula L1) (formula L2) Pos))
   (description "Test application of Eval-Permutation-Forward."))


(defun permtac=expand-eval-permutation-forward (outline parameters)
  (tacl~init outline)
  (permtac=stepwise-apply-permutation-forward (cadr outline) (car outline) (car parameters))
  (tacl~end)
)

(defun permtac=stepwise-apply-permutation-forward (begin end position &optional step-axiom)
  (declare (edited  "24-DEC-2002")
	   (authors Vxs)
	   (input   "Two lines, a position, and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (let* ((term (data~struct-at-position (node~formula begin) position))
	 (set (car (data~appl-arguments term)))
	 (elements (unless (data~equal set perm*identity-perm)
		     (perm=decompose-set set)))
	 (argument (cadr (data~appl-arguments term))))
  (if (null elements)
      (let ((base-axiom (tacl~insert&return-assumption 'permutation 'apply-perm-base)))
	(tacl~sequence
	 (foralle ('foralle (list nil base-axiom) (list argument)))
	 (dummy ('=subst (list end begin (car foralle)) (list position)))
	))
    (let* ((step-axiom (or step-axiom (tacl~insert&return-assumption 'permutation 'apply-perm-step)))
	   (tyse*emptyset 'identity-perm)
	   (existse-sort
	    (tacl~sequence
	     (foralle ('foralle (list nil step-axiom) (list argument)))
	     (foralle-sort ('foralle-sort (list nil (car foralle) nil) (list set)))
	     (dummy0 ('is-permutation (last foralle-sort) nil))
	     (dummy1 ('existse-sort (list end (car foralle-sort) nil)
					  (list (post~read-object (car elements) (pds~environment omega*current-proof-plan) :existing-term))))))
	   (result
	    (tacl~sequence
	     (ander ('ander (list nil (fourth existse-sort)) nil))
	     (re-set ('rerepresent-set (list nil (car ander)) (list (pos~list-position '(2 1)))))
	     (eval-cyc ('eval-cycle-forward (list nil (car re-set)) (list (pos~list-position '(2 2)))))
	     (dummy2 ('=subst (list nil begin (car eval-cyc)) (list position))))))
      (permtac=stepwise-apply-permutation-forward (car result) (third existse-sort) position step-axiom)))))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-Cycle-Backward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-cycle-backward
		 (outline-mappings (((existent existent) eval-cycle-backward-a)
				    ((existent nonexistent) eval-cycle-backward-b)))
		 (expansion-function permtac=expand-eval-cycle-backward)
		 (parameter-types position)
		 (help "Evaluates a cycle application backward."))

(com~defcommand eval-cycle-backward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "An open line containing a cycle application"
            "A closed line with the result of the application"
            "The position of the cycle application")
  (function permtac=eval-cycle-backward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Evaluates a cycle application backward."))

(defun permtac=eval-cycle-backward (l1 l2 pos)
  (infer~compute-outline 'eval-cycle-backward (list l1 l2) (list pos)))

(defun permtac=eval-cycle-backward-p (L1 Pos)
  (let ((perm-appl (data~struct-at-position l1 pos)))
    (and (data~appl-p perm-appl)
	 (data~schema-equal (data~appl-function perm-appl) perm*perm-apply)
	 (perm~cycle-type-p (car (data~appl-arguments perm-appl)))
	 (not (term~variable-p (car (data~appl-arguments perm-appl)))))))

(defun permtac=eval-cycle-backward-b (L1 Pos)
    (let* ((perm-appl (data~struct-at-position l1 pos))
	   (args (data~appl-arguments perm-appl))
	   (perm (car args))
	   (arg (cadr args)))
      (when (and (not (meta~p perm))(not (term~variable-p perm)) (term~number-p arg))
	(if (data~schema-equal perm perm*identity-cyc)
	    (data~replace-at-position l1 pos arg)
	  (let ((result (perm~gap-apply (concatenate 'string "(set " (post~string perm) ")") arg)))
	    (when result
	      (data~replace-at-position l1 pos result)))))))

(tac~deftactic eval-cycle-backward-b eval-cycle-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (permtac=eval-cycle-backward-b (formula L2) POS)))
   (sideconditions (permtac=eval-cycle-backward-p (formula L2) Pos))
   (description "Backward application of Eval-Cycle-Backward."))


(tac~deftactic eval-cycle-backward-a eval-cycle-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-cycle-backward-p (formula L2) Pos)
		   (permtac=eval-cycle-backward-ap (formula L1) (formula L2) Pos))
   (description "Test application of Eval-Cycle-Backward."))


(defun permtac=eval-cycle-backward-ap (L1 L2 Pos)
  (data~equal (permtac=eval-cycle-backward-b L1 Pos) L2))


(defun permtac=get-element-list (list)
  (declare (edited  "25-DEC-2002")
	   (authors Vxs)
	   (input   "A POST term representing a list.")
	   (effect  "None.")
	   (value   "The last element of that list."))
  (when (and (data~appl-p list)
	     (data~schema-equal (data~appl-function list) perm*cons))
    (destructuring-bind (arg1 arg2) (data~appl-arguments list)
      (if (data~schema-equal arg2 perm*nil)
	  (list arg1)
	(cons arg1 (permtac=get-element-list arg2))))))


(defun permtac=expand-eval-cycle-backward (outline parameters)
  (let* ((begin (cadr outline))
	 (end (car outline))
	 (position (car parameters))
	 (cyc-pos (pos~add-end 1 position))
	 (cycle (data~struct-at-position (node~formula end) cyc-pos)))
    (tacl~init outline)
    (when (term~cyc-p cycle)
      (setf end (cadr (tacl~apply 'defni (list end nil)
				  (list cycle (repr~special2term cycle) cyc-pos)))))
    (permtac=stepwise-apply-cycle-backward
     begin end position (permtac=get-element-list (data~struct-at-position (node~formula end) cyc-pos)))
    (tacl~end)
    ))

;;; we might have a problem with cycles of length one. They should be prohibited!
;;; ... can't do that due to base case....

(defun permtac=stepwise-apply-cycle-backward (begin end position elements &optional step-axiom)
  (declare (edited  "25-DEC-2002")
	   (authors Vxs)
	   (input   "Two lines, a position, a list of elements, and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (destructuring-bind (cycle arg)
      (data~appl-arguments (data~struct-at-position (node~formula end) position))
    (cond ((endp elements)
	   (let ((id-axiom (tacl~insert&return-assumption 'permutation 'apply-identity)))
	     (tacl~sequence
	      (foralle ('foralle (list nil id-axiom) (list arg)))
	      (dummy1 ('=subst (list end begin (car foralle)) (list position)))
	      )))
	  ((data~equal arg (car (last elements)))
	   (let ((first-axiom (tacl~insert&return-assumption 'permutation 'apply-cycle-first)))
	     (tacl~sequence
	      (foralle ('foralle (list nil first-axiom) (last elements)))
	      (foralle-sort ('foralle-sort (list nil (car foralle) nil) (list cycle)))
	      (dummy0 ('is-cycle (last foralle-sort) nil))
	      (impe ('impe (list nil nil (car foralle-sort)) nil))
	      (elb ('eval-last-backward (list (second impe) nil) (list (pos~list-position '(1)))))
	      (dummy1 ('=ref (last elb) (last elements)))
	      (eff ('eval-first-forward (list nil (car impe)) (list (pos~list-position '(2)))))
	      (dummy3 ('=subst (list end begin (car eff)) (list position)))))
	   )
	  ((data~equal arg (car elements))
	   (let ((base-axiom (tacl~insert&return-assumption 'permutation 'apply-cycle-base)))
	     (tacl~sequence
	      (foralle1 ('foralle (list nil base-axiom) (list (car elements))))
	      (foralle2 ('foralle (list nil (car foralle1)) (list (cadr elements))))
	      (foralle-sort ('foralle-sort (list nil (car foralle2) nil)
					   (list (cadr (data~appl-arguments (cadr (data~appl-arguments cycle)))))))
	      (beta ('beta-normalize (list (third foralle-sort) nil) nil))
	      (dummy0 ('is-cycle (last beta) nil))
	      (dummy1 ('=subst (list end begin (car foralle-sort)) (list position)))
	      ))
	   )
	  ((endp (rest elements))
	   (let ((id-axiom (tacl~insert&return-assumption 'permutation 'apply-cycle-id)))
	     (tacl~sequence
	      (foralle1 ('foralle (list nil id-axiom) (list (car elements))))
	      (foralle2 ('foralle (list nil (car foralle1)) (list arg)))
	      (impe ('impe (list nil nil (car foralle2)) nil))
	      (dummy0 ('not=int (list (second impe)) nil))
	      (dummy1 ('=subst (list end begin (car impe)) (list position)))
	      )))
	  (t
	   (let* ((foralle1 (or step-axiom
			     (let ((ax (tacl~insert&return-assumption 'permutation 'apply-cycle-step)))
			       (tacl~apply 'foralle (list nil ax) (list arg)))))
		 (result 
		  (tacl~sequence
		   (foralle2 ('foralle (list nil (car foralle1)) (list (car elements))))
		   (foralle-sort ('foralle-sort (list nil (car foralle2) nil)
						(list (cadr (data~appl-arguments cycle)))))
		   (dummy0 ('is-cycle (last foralle-sort) nil))
		   (impe ('impe (list nil nil (car foralle-sort)) nil))
		   (andi ('andi (list (second impe) nil nil) nil))
		   (dummy1 ('not=int (last andi) nil))
		   (elb ('eval-last-backward (list (second andi) nil) (list (pos~list-position '(1 1)))))
		   (dummy2 ('not=int (last elb) nil))
		   (dummy3 ('=subst (list end nil (car impe)) (list position))))))
	     (permtac=stepwise-apply-cycle-backward begin (second result) position (cdr elements) foralle1)
	     ))
	  )

  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-Cycle-Forward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-cycle-forward
		 (outline-mappings (((existent existent) eval-cycle-forward-a)
				    ((nonexistent existent) eval-cycle-forward-f)))
		 (expansion-function permtac=expand-eval-cycle-forward)
		 (parameter-types position)
		 (help "Evaluates a cycle application forward."))

(com~defcommand eval-cycle-forward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A closed line containing a cycle application"
            "An open line with the result of the application"
            "The position of the cycle application")
  (function permtac=eval-cycle-forward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Evaluates a cycle application forward."))

(defun permtac=eval-cycle-forward (l1 l2 pos)
  (infer~compute-outline 'eval-cycle-forward (list l2 l1) (list pos)))

(tac~deftactic eval-cycle-forward-f eval-cycle-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (permtac=eval-cycle-backward-b (formula L1) POS)))
   (sideconditions (permtac=eval-cycle-backward-p (formula L1) Pos))
   (description "Forward application of Eval-Cycle-Forward."))


(tac~deftactic eval-cycle-forward-a eval-cycle-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-cycle-backward-p (formula L1) Pos)
		   (permtac=eval-cycle-backward-ap (formula L2) (formula L1) Pos))
   (description "Test application of Eval-Cycle-Forward."))



(defun permtac=expand-eval-cycle-forward (outline parameters)
  (let* ((begin (car outline))
	 (end (cadr outline))
	 (position (car parameters))
	 (cyc-pos (pos~add-end 1 position))
	 (cycle (data~struct-at-position (node~formula end) cyc-pos)))
    (tacl~init outline)
    (when (term~cyc-p cycle)
      (setf end (car (tacl~apply 'defne (list nil end)
				  (list cycle (repr~special2term cycle) cyc-pos)))))
    (permtac=stepwise-apply-cycle-forward
     begin end position (permtac=get-element-list (data~struct-at-position (node~formula end) cyc-pos)))
    (tacl~end)
    ))

;;; we might have a problem with cycles of length one. They should be prohibited!
;;; ... can't do that due to base case....

(defun permtac=stepwise-apply-cycle-forward (begin end position elements &optional step-axiom)
  (declare (edited  "25-DEC-2002")
	   (authors Vxs)
	   (input   "Two lines, a position, a list of elements, and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (destructuring-bind (cycle arg)
      (data~appl-arguments (data~struct-at-position (node~formula end) position))
    (cond ((endp elements)
	   (let ((id-axiom (tacl~insert&return-assumption 'permutation 'apply-identity)))
	     (tacl~sequence
	      (foralle ('foralle (list nil id-axiom) (list arg)))
	      (dummy1 ('=subst (list begin end (car foralle)) (list position)))
	      )))
	  ((data~equal arg (car (last elements)))
	   (let ((first-axiom (tacl~insert&return-assumption 'permutation 'apply-cycle-first)))
	     (tacl~sequence
	      (foralle ('foralle (list nil first-axiom) (last elements)))
	      (foralle-sort ('foralle-sort (list nil (car foralle) nil) (list cycle)))
	      (dummy0 ('is-cycle (last foralle-sort) nil))
	      (impe ('impe (list nil nil (car foralle-sort)) nil))
	      (elb ('eval-last-backward (list (second impe) nil) (list (pos~list-position '(1)))))
	      (dummy1 ('=ref (last elb) (last elements)))
	      (eff ('eval-first-forward (list nil (car impe)) (list (pos~list-position '(2)))))
	      (dummy3 ('=subst (list begin end (car eff)) (list position)))))
	   )
	  ((data~equal arg (car elements))
	   (let ((base-axiom (tacl~insert&return-assumption 'permutation 'apply-cycle-base)))
	     (tacl~sequence
	      (foralle1 ('foralle (list nil base-axiom) (list (car elements))))
	      (foralle2 ('foralle (list nil (car foralle1)) (list (cadr elements))))
	      (foralle-sort ('foralle-sort (list nil (car foralle2) nil)
					   (list (cadr (data~appl-arguments (cadr (data~appl-arguments cycle)))))))
	      (beta ('beta-normalize (list (third foralle-sort) nil) nil))
	      (dummy0 ('is-cycle (last beta) nil))
	      (dummy1 ('=subst (list begin end (car foralle-sort)) (list position)))
	      ))
	   )
	  ((endp (rest elements))
	   (let ((id-axiom (tacl~insert&return-assumption 'permutation 'apply-cycle-id)))
	     (tacl~sequence
	      (foralle1 ('foralle (list nil id-axiom) (list (car elements))))
	      (foralle2 ('foralle (list nil (car foralle1)) (list arg)))
	      (impe ('impe (list nil nil (car foralle2)) nil))
	      (dummy0 ('not=int (list (second impe)) nil))
	      (dummy1 ('=subst (list begin end (car impe)) (list position)))
	      )))
	  (t
	   (let* ((foralle1 (or step-axiom
			     (let ((ax (tacl~insert&return-assumption 'permutation 'apply-cycle-step)))
			       (tacl~apply 'foralle (list nil ax) (list arg)))))
		 (result 
		  (tacl~sequence
		   (foralle2 ('foralle (list nil (car foralle1)) (list (car elements))))
		   (foralle-sort ('foralle-sort (list nil (car foralle2) nil)
						(list (cadr (data~appl-arguments cycle)))))
		   (dummy0 ('is-cycle (last foralle-sort) nil))
		   (impe ('impe (list nil nil (car foralle-sort)) nil))
		   (andi ('andi (list (second impe) nil nil) nil))
		   (dummy1 ('not=int (last andi) nil))
		   (elb ('eval-last-backward (list (second andi) nil) (list (pos~list-position '(1 1)))))
		   (dummy2 ('not=int (last elb) nil))
		   (dummy3 ('=subst (list nil end (car impe)) (list position))))))
	     (permtac=stepwise-apply-cycle-forward begin (car result) position (cdr elements) foralle1)
	     ))
	  )

  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Is-Permutation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all this could be an extension of well-sorted

(infer~deftactic is-permutation
		 (outline-mappings (((existent) is-permutation-b)))
		 (expansion-function permtac=expand-is-permutation)
		 (help "Closes a line if it contains a valid permutation sort statement."))

(com~defcommand is-permutation
  (argnames line)
  (argtypes ndline)
  (arghelps "An open line with a permutation sort statement.")
  (function permtac=is-permutation)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Closes a line if it contains a valid permutation sort statement."))

(defun permtac=is-permutation (l)
  (infer~compute-outline 'is-permutation (list l) nil))

(defun permtac=permutation-sort-statement-p (formula)
  (and (data~appl-p formula)
       (data~schema-equal (data~appl-function formula) perm*permutation-sort)
       (let ((permutation (car (data~appl-arguments formula))))
	 (and (perm~perm-type-p permutation)
	      (not (term~variable-p permutation))
	      (or (data~schema-equal permutation perm*identity-perm)
		  (and (term~set-p permutation)         ;;was (not (term~constant-p ..))
		       (every #'term~cyc-p (term~normalform permutation))))))))

(defun permtac=valid-permutation-p (formula)
  (perm~gap-is-perm (car (data~appl-arguments formula))))

(tac~deftactic is-permutation-b is-permutation (in permutation)
   (conclusions L)
   (sideconditions
    (permtac=permutation-sort-statement-p (formula L))
    (permtac=valid-permutation-p (formula L)))
   (description "Application of Is-Permutation."))


(defun permtac=expand-is-permutation (outline parameters)
  (declare (ignore parameters))
  (let* ((line (car outline))
	 (permutation (car (data~appl-arguments (node~formula line))))
	 (cycles (keim~name permutation))
	 (perm-def (th~find-assumption "permutation" :permutation))
	 (perm-definiendum (th~definition-constant perm-def))
	 (perm-definiens (data~copy (th~ass-node perm-def) :downto '(term+constant type+primitive)))
	 (env (pds~environment omega*current-proof-plan)))
    (tacl~init outline)
    (if (data~schema-equal permutation perm*identity-perm)
	(let* ((id-def (th~find-assumption "identity-perm" :permutation))
	       (id-definiendum (th~definition-constant id-def))
	       (id-definiens (data~copy (th~ass-node id-def) :downto '(term+constant type+primitive))))
	  (tacl~sequence
	   (defni1 ('defni (list line nil) (list id-definiendum id-definiens (pos~list-position '(1)))))
	   (defni2 ('defni (list (cadr defni1) nil) (list perm-definiendum perm-definiens (pos~list-position '(0)))))
	   (foralli-sort ('foralli-sort* (list (cadr defni2) nil) (list (orules=generate-defaults-foralli (second defni2) env))))
	   (beta ('beta-normalize (list nil (car (third foralli-sort))) nil))
	   (andel ('andel (list nil (car beta)) nil))
	   (dummy ('falsee (list (car (second foralli-sort)) (car andel)) nil))))
      (let* ((foralli-sort
	   (tacl~sequence
	    (defni1 ('defni (list line nil) (list permutation (repr~special2term permutation) (pos~list-position '(1)))))
	    (defni2 ('defni (list (cadr defni1) nil) (list perm-definiendum perm-definiens (pos~list-position '(0)))))
	    (dummy ('foralli-sort* (list (cadr defni2) nil) (list (orules=generate-defaults-foralli (second defni2) env))))))
	  (beta1 (tacl~apply 'beta-normalize (list nil (car (third foralli-sort))) nil))
	  (beta2 (tacl~apply 'beta-normalize (list nil (cadr (third foralli-sort))) nil))
	  (impi (tacl~apply 'impi (list (car (second foralli-sort)) nil) nil))
	  (ore (when (> (length cycles) 1)
		 (tacl~apply 'ore** (list (second impi) (list (car beta1) (car beta2))) nil))))
	(if ore
	    (permtac=close-is-permutation-subgoals (permtac=sort-ore**-result (cddr (second ore)) (third ore)) (third impi))
	  (permtac=close-is-permutation-subgoals (list (list (second impi) (car beta1) (car beta2))) (third impi)))
	))
    (tacl~end)
    ))

(defun permtac=close-is-permutation-subgoals (subgoals not=)
  (declare (edited  "28-DEC-2002")
	   (authors Vxs)
	   (input   "A list of node-lists and a not=-hyp.")
	   (effect  "Closes the given subgoals and introduces some node into the PDS.")
	   (value   "Undefined."))
  (dolist (goal subgoals)
    (let* ((conc (first goal))
	   (equality (car (data~appl-arguments (node~formula not=))))
	   (var1 (first (data~appl-arguments equality)))
	   (hypA (second goal))
	   (hypB (third goal)))
      (multiple-value-bind (hyp1 cyc1 hyp2 cyc2)
	  (let ((argsA (data~appl-arguments (node~formula hypA)))
		(argsB (data~appl-arguments (node~formula hypB))))
	    (if (data~equal var1 (car argsA))
		(values hypA (second argsA) hypB (second argsB))
	      (values hypB (second argsB) hypA (second argsA))))
	(if (data~equal cyc1 cyc2)
	    (tacl~sequence
	     (=subst ('=subst (list nil hyp1 hyp2) (list (pos~list-position '(2)))))
	     (note ('note (list nil (car =subst) not=) nil))
	     (dummy ('falsee (list conc (car note)) nil)))
	  (tacl~sequence
	   (=subst ('=subst** (list conc (list hyp1 hyp2)) (list (list (list (pos~list-position '(1 1 1))
									     (pos~list-position '(2 1)))
								       (list (pos~list-position '(1 2 1))
									     (pos~list-position '(2 2)))))))
	   (andi ('andi* (list (car (last (cadr =subst))) nil) nil))
	   (dummy0 ('is-cycle (list (first (second andi))) nil))
	   (dummy1 ('is-cycle (list (second (second andi))) nil))
	   (dummy2 ('is-cycle-disjoint (list (third (second andi))) nil))
	  ))))))

				


(defun permtac=sort-ore**-result (concs hyps)
  (declare (edited  "28-DEC-2002")
	   (authors Vxs)
	   (input   "A list of conclusions and a list of hypotheses.")
	   (effect  "None.")
	   (value   "A list of list, where each sublist contains a conclusion and the appropriate hypotheses."))
  (when concs
    (let ((conc (car concs)))
      (cons (cons conc (intersection (pdsn~hyps conc) hyps))
	    (permtac=sort-ore**-result (cdr concs) hyps)))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Is-Cycle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all this could be an extension of well-sorted

(infer~deftactic is-cycle
		 (outline-mappings (((existent) is-cycle-b)))
		 (expansion-function permtac=expand-is-cycle)
		 (help "Closes a line if it contains a valid cycle sort statement."))

(com~defcommand is-cycle
  (argnames line)
  (argtypes ndline)
  (arghelps "An open line with a cycle sort statement.")
  (function permtac=is-cycle)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Closes a line if it contains a valid cycle sort statement."))

(defun permtac=is-cycle (l)
  (infer~compute-outline 'is-cycle (list l) nil))

(defun permtac=cycle-sort-statement-p (formula)
  (and (data~appl-p formula)
       (data~schema-equal (data~appl-function formula) perm*cycle-sort)
       (let ((cycle (car (data~appl-arguments formula))))
	 (and (perm~cycle-type-p cycle)
	      (not (term~variable-p cycle))
	      (or (data~schema-equal cycle perm*identity-cyc)
		  (term~cyc-p cycle))))))  ;;was (not (term~constant-p))

(defun permtac=valid-cycle-p (formula)
  (or
   ;;;cycle of length 1.... those are bizarre!!!
    (let* ((cycle (car (data~appl-arguments formula)))
	   (func (data~appl-function cycle))
	   (args (data~appl-arguments cycle)))
      (and (data~schema-equal func perm*cons)
	   (data~schema-equal (second args) perm*nil)))
    (perm~gap-is-cycle (car (data~appl-arguments formula)))))

(tac~deftactic is-cycle-b is-cycle (in permutation)
   (conclusions L)
   (sideconditions
    (permtac=cycle-sort-statement-p (formula L))
    (permtac=valid-cycle-p (formula L)))
   (description "Application of Is-Cycle."))


(defun permtac=expand-is-cycle (outline parameters)
  (declare (ignore parameters))
  (tacl~init outline)
  (permtac=stepwise-apply-cycle (car outline))
  (tacl~end))

(defun permtac=stepwise-apply-cycle (end &optional step-axiom)
  (declare (edited  "26-DEC-2002")
	   (authors Vxs)
	   (input   "A line and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (destructuring-bind (first rest)
      (data~appl-arguments (car (data~appl-arguments (node~formula end))))
    (if (data~schema-equal rest perm*nil)
	;;; step-case
	(let* ((base-axiom (tacl~insert&return-assumption 'permutation 'cycle-base)))
	  (tacl~apply 'foralle (list end base-axiom) (list first)))
	;;; base-case
      (let* ((step-axiom (or step-axiom
			    (tacl~insert&return-assumption 'permutation 'cycle-step)))
	     (result
	      (tacl~sequence
	       (foralle-sort ('foralle-sort (list nil step-axiom nil) (list rest)))
	       (beta ('beta-normalize (list (third foralle-sort) nil) nil))
	       (dummy0 ('not=lists (last beta) nil))
	       (foralle ('foralle (list nil (car foralle-sort)) (list first)))
	       (equiv ('equivsubst (list end nil (car foralle)) (list (pos~empty))))
	       (dummy1 ('andi (list (second equiv) nil nil) nil)))))
	     ;; here goes the cycle-set thingy
	(permtac=not-in-set-expansion-backward
	 (cadr
	  (tacl~apply 'eval-cycle-set-backward (list (second result) nil) (list (pos~list-position '(1 2))))))
	(permtac=stepwise-apply-cycle (third result) step-axiom)))))

;;;; the following two functions could someday be part of the expansion of a more general
;;;; not-in-int-set tactic.

(defun permtac=not-in-set-expansion-backward (line)
  (declare (edited  "27-DEC-2002")
	   (authors Vxs)
	   (input   "An open line containing a statement of the form (not (in a (set b c d....))), where"
		    "a,b,c,d,... are integers.")
	   (effect  "Inserts new proof nodes.")
	   (value   "Undefined."))
  (let* ((set (second (data~appl-arguments (car (data~appl-arguments (node~formula line))))))
	 (in-def (th~find-assumption "in" :permutation))
	 (definiendum (th~definition-constant in-def))
	 (definiens (data~copy (th~ass-node in-def) :downto '(term+constant type+primitive))))
    (permtac=not-in-set-expansion-backward-recursive
     (cadr
      (tacl~sequence
       (defni ('defni (list line nil) (list definiendum definiens (pos~list-position '(1 0)))))
       (dummy ('defni (list (cadr defni) nil) (list set (repr~special2term set) (pos~list-position '(1 0))))))))))

(defun permtac=not-in-set-expansion-backward-recursive (line)
  (let* ((term (node~formula line))
	 (subterm (car (data~appl-arguments term))))
    (cond ((and (logic~negation-p term) (logic~disjunction-p subterm))
	   (let ((conjunction
		  (tacl~sequence
		   (pullneg ('pullneg (list line nil) nil))
		   (dummy0 ('andi (list (cadr pullneg) nil nil) nil)))))
	     (permtac=not-in-set-expansion-backward-recursive (second conjunction))
	     (permtac=not-in-set-expansion-backward-recursive (third conjunction))))
	  ((and (logic~negation-p term) (logic~equality-p subterm))
	   (tacl~apply 'not=int (list line) nil))
	  (t (omega~error "Something unexpected has happend during expansion of a Not In Int-Set statment.")))))
	     
		  

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-Last-Backward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-last-backward
		 (outline-mappings (((existent existent) eval-last-backward-a)
				    ((existent nonexistent) eval-last-backward-b)))
		 (expansion-function permtac=expand-eval-last-backward)
		 (parameter-types position)
		 (help "Evaluates a the last operation on lists backward."))

(com~defcommand eval-last-backward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "An open line containing a last operation on a list"
            "A closed line with the result of the operation"
            "The position of the last operation")
  (function permtac=eval-last-backward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Evaluates a last operation on lists backward."))

(defun permtac=eval-last-backward (l1 l2 pos)
  (infer~compute-outline 'eval-last-backward (list l1 l2) (list pos)))

(defun permtac=eval-last-backward-p (L1 Pos)
  (let ((last-appl (data~struct-at-position l1 pos)))
    (and (data~appl-p last-appl)
	 (data~schema-equal (data~appl-function last-appl) perm*last)
	 (let ((list (car (data~appl-arguments last-appl))))
	   (and (not (data~schema-equal list perm*nil))
		(not (term~variable-p list))
		(not (term~constant-p list)))))))

(defun permtac=eval-last-backward-b (L1 Pos)
  (let* ((last-appl (data~struct-at-position l1 pos))
	 (result (permtac=get-last-list-element (car (data~appl-arguments last-appl)))))
    (when result
	(data~replace-at-position l1 pos result))))

(defun permtac=get-last-list-element (list)
  (declare (edited  "25-DEC-2002")
	   (authors Vxs)
	   (input   "A POST term representing a list.")
	   (effect  "None.")
	   (value   "The last element of that list."))
  (when (data~schema-equal (data~appl-function list) perm*cons)
    (destructuring-bind (arg1 arg2) (data~appl-arguments list)
      (if (data~schema-equal arg2 perm*nil)
	  arg1
	(permtac=get-last-list-element arg2)))))

(tac~deftactic eval-last-backward-b eval-last-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (permtac=eval-last-backward-b (formula L2) POS)))
   (sideconditions (permtac=eval-last-backward-p (formula L2) Pos))
   (description "Backward application of Eval-Last-Backward."))


(tac~deftactic eval-last-backward-a eval-last-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-last-backward-p (formula L2) Pos)
		   (permtac=eval-last-backward-ap (formula L1) (formula L2) Pos))
   (description "Test application of Eval-Last-Backward."))


(defun permtac=eval-last-backward-ap (L1 L2 Pos)
  (data~equal (permtac=eval-last-backward-b L1 Pos) L2))


(defun permtac=expand-eval-last-backward (outline parameters)
  (tacl~init outline)
  (permtac=stepwise-apply-last-backward (cadr outline) (car outline) (car parameters))
  (tacl~end))

(defun permtac=stepwise-apply-last-backward (begin end position &optional step-axiom)
  (declare (edited  "26-DEC-2002")
	   (authors Vxs)
	   (input   "Two lines, a position, and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (let* ((term (data~struct-at-position (node~formula end) position))
	 (type (data~abstr-range (term~type (data~appl-function term))))
	 (list (car (data~appl-arguments term)))
	 (first (car (data~appl-arguments list)))
	 (rest (cadr (data~appl-arguments list))))
    (if (data~schema-equal rest perm*nil)
	;;; step-case
	(let* ((base-axiom (tacl~insert&return-assumption 'permutation 'last-base-cyc)))
	  (tacl~sequence
	   (foralle ('foralle (list nil base-axiom) (list first)))
	   (dummy ('=subst (list end begin (car foralle)) (list position)))))
	;;; base-case
      (let* ((step-axiom (or step-axiom
			    (tacl~insert&return-assumption 'permutation 'last-step-cyc)))
	     (result 
	      (tacl~sequence
	       (kappae ('kappae (list nil step-axiom) (list (list type))))
	       (foralle-sort ('foralle-sort (list nil (car kappae) nil) (list rest)))
	       (beta ('beta-normalize (list (third foralle-sort) nil) nil))
	       (dummy0 ('not=lists (last beta) nil))
	       (foralle ('foralle (list nil (car foralle-sort)) (list first)))
	       (dummy1 ('=subst (list end nil (car foralle)) (list position))))))
	(permtac=stepwise-apply-last-backward begin (second result) position step-axiom)))))

	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-Last-Forward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-last-forward
		 (outline-mappings (((existent existent) eval-last-forward-a)
				    ((nonexistent existent) eval-last-forward-f)))
		 (expansion-function permtac=expand-eval-last-forward)
		 (parameter-types position)
		 (help "Evaluates a the last operation on lists forward."))

(com~defcommand eval-last-forward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A closed line containing a last operation on a list"
            "An open line with the result of the operation"
            "The position of the last operation")
  (function permtac=eval-last-forward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Evaluates a last operation on lists forward."))

(defun permtac=eval-last-forward (l1 l2 pos)
  (infer~compute-outline 'eval-last-forward (list l2 l1) (list pos)))


(tac~deftactic eval-last-forward-f eval-last-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (permtac=eval-last-backward-b (formula L1) POS)))
   (sideconditions (permtac=eval-last-backward-p (formula L1) Pos))
   (description "Forward application of Eval-Last-Forward."))


(tac~deftactic eval-last-forward-a eval-last-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-last-backward-p (formula L1) Pos)
		   (permtac=eval-last-backward-ap (formula L2) (formula L1) Pos))
   (description "Test application of Eval-Last-Forward."))


(defun permtac=expand-eval-last-forward (outline parameters)
  (tacl~init outline)
  (permtac=stepwise-apply-last-forward (car outline) (cadr outline) (car parameters))
  (tacl~end))

(defun permtac=stepwise-apply-last-forward (begin end position &optional step-axiom)
  (declare (edited  "26-DEC-2002")
	   (authors Vxs)
	   (input   "Two lines, a position, and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (let* ((term (data~struct-at-position (node~formula end) position))
	 (type (data~abstr-range (term~type (data~appl-function term))))
	 (list (car (data~appl-arguments term)))
	 (first (car (data~appl-arguments list)))
	 (rest (cadr (data~appl-arguments list))))
    (if (data~schema-equal rest perm*nil)
	;;; step-case
	(let* ((base-axiom (tacl~insert&return-assumption 'permutation 'last-base-cyc)))
	  (tacl~sequence
	   (foralle ('foralle (list nil base-axiom) (list first)))
	   (dummy ('=subst (list begin end (car foralle)) (list position)))))
	;;; base-case
      (let* ((step-axiom (or step-axiom
			    (tacl~insert&return-assumption 'permutation 'last-step-cyc)))
	     (result 
	      (tacl~sequence
	       (kappae ('kappae (list nil step-axiom) (list (list type))))
	       (foralle-sort ('foralle-sort (list nil (car kappae) nil) (list rest)))
	       (beta ('beta-normalize (list (third foralle-sort) nil) nil))
	       (dummy0 ('not=lists (last beta) nil))
	       (foralle ('foralle (list nil (car foralle-sort)) (list first)))
	       (dummy1 ('=subst (list nil end (car foralle)) (list position))))))
	(permtac=stepwise-apply-last-forward begin (first result) position step-axiom)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-First-Backward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-first-backward
		 (outline-mappings (((existent existent) eval-first-backward-a)
				    ((existent nonexistent) eval-first-backward-b)))
		 (expansion-function permtac=expand-eval-first-backward)
		 (parameter-types position)
		 (help "Evaluates a first operation on lists backward."))

(com~defcommand eval-first-backward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "An open line containing a first operation on a list"
            "A closed line with the result of the operation"
            "The position of the first operation")
  (function permtac=eval-first-backward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Evaluates a first operation on lists backward."))

(defun permtac=eval-first-backward (l1 l2 pos)
  (infer~compute-outline 'eval-first-backward (list l1 l2) (list pos)))

(defun permtac=eval-first-backward-p (L1 Pos)
  (let ((first-appl (data~struct-at-position l1 pos)))
    (and (data~appl-p first-appl)
	 (data~schema-equal (data~appl-function first-appl) perm*first)
	 (let ((list (car (data~appl-arguments first-appl))))
	   (and (not (data~schema-equal list perm*nil))
		(not (term~variable-p list))
		(not (term~constant-p list)))))))

(defun permtac=eval-first-backward-b (L1 Pos)
  (let* ((first-appl (data~struct-at-position l1 pos))
	 (result (car (data~appl-arguments (car (data~appl-arguments first-appl))))))
    (when result
	(data~replace-at-position l1 pos result))))


(tac~deftactic eval-first-backward-b eval-first-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (permtac=eval-first-backward-b (formula L2) POS)))
   (sideconditions (permtac=eval-first-backward-p (formula L2) Pos))
   (description "Backward application of Eval-First-Backward."))


(tac~deftactic eval-first-backward-a eval-first-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-first-backward-p (formula L2) Pos)
		   (permtac=eval-first-backward-ap (formula L1) (formula L2) Pos))
   (description "Test application of Eval-First-Backward."))


(defun permtac=eval-first-backward-ap (L1 L2 Pos)
  (data~equal (permtac=eval-first-backward-b L1 Pos) L2))

(defun permtac=expand-eval-first-backward (outline parameters)   ;;; change with Kappae
  (let* ((axiom (tacl~insert&return-assumption 'permutation 'first-defi))
	 (begin (cadr outline))
	 (end (car outline))
	 (position (car parameters))
	 (list (car (data~appl-arguments (data~struct-at-position (node~formula end) position))))
	 (first (car (data~appl-arguments list)))
	 (rest (cadr (data~appl-arguments list))))
  (tacl~init outline)
  (tacl~sequence
   (foralle1 ('foralle (list nil axiom) (list rest)))
   (foralle2 ('foralle (list nil (car foralle1)) (list first)))
   (dummy ('=subst (list end begin (car foralle2)) (list position))))
  (tacl~end)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-First-Forward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-first-forward
		 (outline-mappings (((existent existent) eval-first-forward-a)
				    ((nonexistent existent) eval-first-forward-f)))
		 (expansion-function permtac=expand-eval-first-forward)
		 (parameter-types position)
		 (help "Evaluates a first operation on lists forward."))

(com~defcommand eval-first-forward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A closed line containing a first operation on a list"
            "An open line with the result of the operation"
            "The position of the first operation")
  (function permtac=eval-first-forward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Evaluates a first operation on lists forward."))

(defun permtac=eval-first-forward (l1 l2 pos)
  (infer~compute-outline 'eval-first-forward (list l2 l1) (list pos)))


(tac~deftactic eval-first-forward-f eval-first-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (permtac=eval-first-backward-b (formula L1) POS)))
   (sideconditions (permtac=eval-first-backward-p (formula L1) Pos))
   (description "Forward application of Eval-First-Forward."))


(tac~deftactic eval-first-forward-a eval-first-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-first-backward-p (formula L1) Pos)
		   (permtac=eval-first-backward-ap (formula L2) (formula L1) Pos))
   (description "Test application of Eval-First-Forward."))


(defun permtac=expand-eval-first-forward (outline parameters)   ;;; change with Kappae
  (let* ((axiom (tacl~insert&return-assumption 'permutation 'first-defi))
	 (begin (car outline))
	 (end (cadr outline))
	 (position (car parameters))
	 (list (car (data~appl-arguments (data~struct-at-position (node~formula end) position))))
	 (first (car (data~appl-arguments list)))
	 (rest (cadr (data~appl-arguments list))))
  (tacl~init outline)
  (tacl~sequence
   (foralle1 ('foralle (list nil axiom) (list rest)))
   (foralle2 ('foralle (list nil (car foralle1)) (list first)))
   (dummy ('=subst (list begin end (car foralle2)) (list position))))
  (tacl~end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Not=Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic not=lists
		 (outline-mappings (((existent) not=lists-a)))
		 (expansion-function permtac=expand-not=lists)
		 (help "Justifies that two lists are not equal."))

(tac~deftactic not=lists-a not=lists
	       (in integer)
	       (conclusions L1)
	       (sideconditions (permtac=check-not=lists (formula L1)))
	       (description "Justifies that two lists are not equal."))


(defun permtac=check-not=lists (formula)
  (when (logic~negation-p formula)
    (let* ((arg (first (data~appl-arguments formula))))
      (when (logic~equality-p arg)
	(let* ((a (first (data~appl-arguments arg)))
	       (b (second (data~appl-arguments arg))))
	  (flet ((listp (term)
			(or (and (data~appl-p term)
				 (data~schema-equal (data~appl-function term) perm*cons))
			    (data~schema-equal term perm*nil))))
	    (when (and (listp a) (listp b))
	      (let ((l1 (permtac=get-element-list a))
		    (l2 (permtac=get-element-list b)))
		(or (not (= (length l1) (length l2)))
		    (notevery #'data~equal l1 l2))))))))))

	     

(com~defcommand not=lists
  (argnames line1)
  (argtypes ndline)
  (arghelps "A line containing (not (= List1 List2))")
  (function permtac=not=lists)
  (frag-cats tactics integer)
  (defaults)
  (log-p t)
  (help "Justifies (not (= List1 List2)) for two lists List1 and List2."))

(defun permtac=not=lists (l1)
  (infer~compute-outline 'not=lists (list l1) nil))

(defun permtac=expand-not=lists (outline)
  (omega~warn "Not yet implemented!!!")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-Cycle-Set-Backward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic eval-cycle-set-backward
		 (outline-mappings (((existent existent) eval-cycle-set-backward-a)
				    ((existent nonexistent) eval-cycle-set-backward-b)))
		 (expansion-function permtac=expand-eval-cycle-set-backward)
		 (parameter-types position)
		 (help "Evaluates a cycle-set backwards and inserts the actual set."))

(com~defcommand eval-cycle-set-backward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "An open line containing a cycle-set statement"
            "A closed line with the actual set"
            "The position of the cycle-set statement")
  (function permtac=eval-cycle-set-backward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help  "Evaluates a cycle-set backwards and inserts the actual set."))

(defun permtac=eval-cycle-set-backward (l1 l2 pos)
  (infer~compute-outline 'eval-cycle-set-backward (list l1 l2) (list pos)))

(defun permtac=eval-cycle-set-backward-p (L1 Pos)
  (let ((cycle-set-appl (data~struct-at-position l1 pos)))
    (and (data~appl-p cycle-set-appl)
	 (data~schema-equal (data~appl-function cycle-set-appl) perm*cycle-set)
	 (let ((cycle (car (data~appl-arguments cycle-set-appl))))
	   (and (not (term~variable-p cycle))
		(or (data~schema-equal cycle perm*identity-cyc)
		    (not (term~constant-p cycle))))))))

(defun permtac=eval-cycle-set-backward-b (L1 Pos)
  (let* ((cycle-set-appl (data~struct-at-position l1 pos))
	 (result (permtac=get-element-list (car (data~appl-arguments cycle-set-appl))))
	 (exts (rest (data~appl-arguments cycle-set-appl)))
	 (newset (if result
		     (post~read-object (mapcar #'(lambda (x) (read-from-string (post~string x))) result)
						    (pds~environment omega*current-proof-plan) :set)
		   (env~lookup-object :emptyset (th~env :permutation)))))
    (if exts
	(data~replace-at-position l1 pos  (term~appl-create newset exts))
      (data~replace-at-position l1 pos newset))))

(tac~deftactic eval-cycle-set-backward-b eval-cycle-set-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (permtac=eval-cycle-set-backward-b (formula L2) POS)))
   (sideconditions (permtac=eval-cycle-set-backward-p (formula L2) Pos))
   (description "Backward application of Eval-Cycle-Set-Backward."))


(tac~deftactic eval-cycle-set-backward-a eval-cycle-set-backward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-cycle-set-backward-p (formula L2) Pos)
		   (permtac=eval-cycle-set-backward-ap (formula L1) (formula L2) Pos))
   (description "Test application of Eval-Cycle-Set-Backward."))


(defun permtac=eval-cycle-set-backward-ap (L1 L2 Pos)
  (data~equal (permtac=eval-cycle-set-backward-b L1 Pos) L2))


(defun permtac=expand-eval-cycle-set-backward (outline parameters)
  (let* ((begin (cadr outline))
	 (end (car outline))
	 (position (car parameters))
	 (cycle (car (data~appl-arguments (data~struct-at-position (node~formula end) position)))))
    (tacl~init outline)
    (if (data~schema-equal cycle perm*identity-cyc)
	(let* ((id-axiom (tacl~insert&return-assumption 'permutation 'cycle-set-id)))
	  (tacl~apply '=subst (list end begin id-axiom) (list position)))
      (permtac=stepwise-apply-cycle-set-backward begin end position position))
    (tacl~end)))

(defun permtac=stepwise-apply-cycle-set-backward (begin end position orig-position &optional step-axiom)
  (declare (edited  "29-DEC-2002")
	   (authors Vxs)
	   (input   "Two lines, two positions, and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (let* ((term (data~struct-at-position (node~formula end) position))
	 (list (car (data~appl-arguments term)))
	 (first (car (data~appl-arguments list)))
	 (rest (cadr (data~appl-arguments list))))
    (if (data~schema-equal rest perm*nil)
	;;; step-case
	(let* ((base-axiom (tacl~insert&return-assumption 'permutation 'cycle-set-base)))
	  (tacl~sequence
	   (foralle ('foralle (list nil base-axiom) (list first)))
	   (=subst ('=subst (list end nil (car foralle)) (list position)))
	   (dummy0 ('rerepresent-set (list (second =subst) begin) (list orig-position)))))
	;;; base-case
      (let* ((step-axiom (or step-axiom
			     (tacl~insert&return-assumption 'permutation 'cycle-set-step)))
	     (result 
	      (tacl~sequence
	       (foralle* ('foralle* (list nil step-axiom) (list (list rest first))))
	       (dummy1 ('=subst (list end nil (car foralle*)) (list position))))))
	(permtac=stepwise-apply-cycle-set-backward begin (second result)
						   (pos~add-end 2 position) orig-position step-axiom)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eval-Cycle-Set-Forward
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic eval-cycle-set-forward
		 (outline-mappings (((existent existent) eval-cycle-set-forward-a)
				    ((nonexistent existent) eval-cycle-set-forward-f)))
		 (expansion-function permtac=expand-eval-cycle-set-forward)
		 (parameter-types position)
		 (help "Evaluates a cycle-set forwards and inserts the actual set."))

(com~defcommand eval-cycle-set-forward
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "A closed line containing a cycle-set statement"
            "An open line with the actual set"
            "The position of the cycle-set statement")
  (function permtac=eval-cycle-set-forward)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help  "Evaluates a cycle-set forwards and inserts the actual set."))

(defun permtac=eval-cycle-set-forward (l1 l2 pos)
  (infer~compute-outline 'eval-cycle-set-forward (list l2 l1) (list pos)))

(tac~deftactic eval-cycle-set-forward-f eval-cycle-set-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (permtac=eval-cycle-set-backward-b (formula L1) POS)))
   (sideconditions (permtac=eval-cycle-set-backward-p (formula L1) Pos))
   (description "Forward application of Eval-Cycle-Set-Forward."))


(tac~deftactic eval-cycle-set-forward-a eval-cycle-set-forward (in permutation)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (permtac=eval-cycle-set-backward-p (formula L1) Pos)
		   (permtac=eval-cycle-set-backward-ap (formula L2) (formula L1) Pos))
   (description "Test application of Eval-Cycle-Set-Forward."))


(defun permtac=expand-eval-cycle-set-forward (outline parameters)
  (let* ((begin (car outline))
	 (end (cadr outline))
	 (position (car parameters))
	 (cycle (car (data~appl-arguments (data~struct-at-position (node~formula end) position)))))
    (tacl~init outline)
    (if (data~schema-equal cycle perm*identity-cyc)
	(let* ((id-axiom (tacl~insert&return-assumption 'permutation 'cycle-set-id)))
	  (tacl~apply '=subst (list begin end id-axiom) (list position)))
      (permtac=stepwise-apply-cycle-set-forward begin end position position))
    (tacl~end)))

(defun permtac=stepwise-apply-cycle-set-forward (begin end position orig-position &optional step-axiom)
  (declare (edited  "29-DEC-2002")
	   (authors Vxs)
	   (input   "Two lines, two positions, and optionally an axiom.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (let* ((term (data~struct-at-position (node~formula end) position))
	 (list (car (data~appl-arguments term)))
	 (first (car (data~appl-arguments list)))
	 (rest (cadr (data~appl-arguments list))))
    (if (data~schema-equal rest perm*nil)
	;;; step-case
	(let* ((base-axiom (tacl~insert&return-assumption 'permutation 'cycle-set-base)))
	  (tacl~sequence
	   (foralle ('foralle (list nil base-axiom) (list first)))
	   (=subst ('=subst (list nil end (car foralle)) (list position)))
	   (dummy0 ('rerepresent-set (list begin (first =subst)) (list orig-position)))))
	;;; base-case
      (let* ((step-axiom (or step-axiom
			     (tacl~insert&return-assumption 'permutation 'cycle-set-step)))
	     (result 
	      (tacl~sequence
	       (foralle* ('foralle* (list nil step-axiom) (list (list rest first))))
	       (dummy1 ('=subst (list nil end (car foralle*)) (list position))))))
	(permtac=stepwise-apply-cycle-set-forward begin (first result)
						   (pos~add-end 2 position) orig-position step-axiom)))))


	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Is-Cycle-Disjoint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all this could be an extension of well-sorted

(infer~deftactic is-cycle-disjoint
		 (outline-mappings (((existent) is-cycle-disjoint-b)))
		 (expansion-function permtac=expand-is-cycle-disjoint)
		 (help "Closes a line if it contains a valid cycle-disjoint statement."))

(com~defcommand is-cycle-disjoint
  (argnames line)
  (argtypes ndline)
  (arghelps "An open line with a cycle-disjoint statement.")
  (function permtac=is-cycle-disjoint)
  (frag-cats tactics permutation)
  (defaults)
  (log-p t)
  (help "Closes a line if it contains a valid cycle-disjoint statement."))

(defun permtac=is-cycle-disjoint (l)
  (infer~compute-outline 'is-cycle-disjoint (list l) nil))

(defun permtac=cycle-disjoint-statement-p (formula)
  (and (data~appl-p formula)
       (data~schema-equal (data~appl-function formula) perm*cycle-disjoint)
       (let ((cycle1 (car (data~appl-arguments formula)))
	     (cycle2 (cadr (data~appl-arguments formula))))
	 (and (perm~cycle-type-p cycle1)
	      (perm~cycle-type-p cycle2)
	      (not (term~variable-p cycle1))
	      (not (term~variable-p cycle2))
	      (or (data~schema-equal cycle1 perm*identity-cyc)
		  (not (term~constant-p cycle1)))
	      (or (data~schema-equal cycle2 perm*identity-cyc)
		  (not (term~constant-p cycle2)))))))

(defun permtac=disjoint-cycles-p (formula)
  (let ((cycle1 (car (data~appl-arguments formula)))
	(cycle2 (cadr (data~appl-arguments formula))))
    (null (intersection (permtac=get-element-list cycle1)
			(permtac=get-element-list cycle2)))))

(tac~deftactic is-cycle-disjoint-b is-cycle-disjoint (in permutation)
   (conclusions L)
   (sideconditions
    (permtac=cycle-disjoint-statement-p (formula L))
    (permtac=disjoint-cycles-p (formula L)))
   (description "Application of Is-Cycle-Disjoint."))


(defun permtac=expand-is-cycle-disjoint (outline parameters)
  (declare (ignore parameters))
  (tacl~init outline)
  (let* ((cdj-def (th~find-assumption "cycle-disjoint" :permutation))
	 (cdj-definiendum (th~definition-constant cdj-def))
	 (cdj-definiens (data~copy (th~ass-node cdj-def) :downto '(term+constant type+primitive)))
	 (fixes-def (th~find-assumption "fixes" :permutation))
	 (fixes-definiendum (th~definition-constant fixes-def))
	 (fixes-definiens (data~copy (th~ass-node fixes-def) :downto '(term+constant type+primitive)))
	 (env (pds~environment omega*current-proof-plan))
	 (andi (tacl~sequence
		(defni ('defni (list (car outline) nil) (list cdj-definiendum cdj-definiens (pos~list-position '(0)))))
		(defni* ('defni* (list (second defni) nil) (list fixes-definiendum fixes-definiens
								 (mapcar #'pos~list-position '((2 1 0 2 0)
											       (2 1 0 1 1 0)
											       (1 1 0 2 0)
											       (1 1 0 1 1 0))))))
		(dummy0 ('andi (list (second defni*) nil nil) nil)))))
    (mapc #'(lambda (x) (permtac=expand-is-cycle-disjoint-subproblem (first x) (second x)))
	  (mapcan #'(lambda (conjunct)
		      (let* ((foralli-sort (tacl~apply 'foralli-sort (list conjunct nil)
						       (orules=generate-defaults-foralli conjunct env)))
			     (ecf (tacl~apply 'eval-cycle-set-forward (list nil (third foralli-sort))
					      (list (pos~empty))))
			     (set (data~appl-function (node~formula (car ecf)))))
			(if (data~schema-equal set (env~lookup-object :emptyset (th~env :permutation)))
			    (let* ((es-def (th~find-assumption "emptyset" :permutation))
				   (es-definiendum (th~definition-constant es-def))
				   (es-definiens (data~copy (th~ass-node es-def) :downto '(term+constant type+primitive))))
			      (tacl~sequence 
			       (defne ('defne (list nil (car ecf))
						  (list es-definiendum es-definiens (pos~list-position '(0)))))
			       (andel ('andel (list nil (car defne)) nil))
			       (dummy ('falsee (list (second foralli-sort) (car andel)) nil)))
			      nil)
			  (let* ((defne (tacl~apply 'defne (list nil (car ecf))
						    (list set (repr~special2term set) (pos~list-position '(0)))))
				 (ore* (tacl~apply 'ore* (list (second foralli-sort) (car defne)) nil)))
			    (permtac=sort-ore**-result (cdr (second ore*)) (third ore*))))))
	    (cdr andi))))
  (tacl~end))


(defun permtac=expand-is-cycle-disjoint-subproblem (conc equality)
  (declare (edited  "29-DEC-2002")
	   (authors Vxs)
	   (input   "A conclusion line and an equality line.")
	   (effect  "Inserts nodes into the OMEGA*CURRENT-PROOF-PLAN.")
	   (value   "Undefined."))
  (tacl~sequence
   (=subst ('=subst* (list conc nil equality) (list (mapcar #'pos~list-position '((1 1 1 2) (1 1 2) (2 1 2) (2 2))))))
   (andi ('andi (list (second =subst) nil nil) nil))
   (ecb1 ('eval-cycle-backward (list (second andi) nil) (list (pos~list-position '(1 1)))))
   (dummy0 ('not=int (list (second ecb1)) nil))
   (ecb2 ('eval-cycle-backward (list (third andi) nil) (list (pos~list-position '(1)))))
   (dummy1 ('=ref (list (second ecb2)) (cdr (data~appl-arguments (node~formula (second ecb2))))))))

  
