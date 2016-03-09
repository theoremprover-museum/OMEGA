;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: Keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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
  (unless (com~find-category 'base)
    (com~defcategory base
		     (help "Tactics of the theory BASE.")))
  (unless (com~find-category 'base-equality)
    (com~defcategory base-equality
		     (help "Equality tactics of the theory BASE.")))
  (unless (com~find-category 'base-connective)
    (com~defcategory base-connective
		     (help "Equality tactics of the theory BASE.")))
  (unless (com~find-category 'base-negation)
    (com~defcategory base-negation
		     (help "Equality tactics of the theory BASE.")))
  (unless (com~find-category 'base-quantifier)
    (com~defcategory base-quantifier
		     (help "Equality tactics of the theory BASE."))))

#-logic-new(load "/project/omega/omega-3/theories/base/generic-fix-tactics.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are the common tactics for the theory base.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic ande
		 (outline-mappings (((nonexistent nonexistent existent) ande-f)
				    ((existent nonexistent existent) ande-l)
				    ((nonexistent existent existent) ande-r)
				    ((existent existent nonexistent) ande-b)
				    ((existent existent existent) ande-a)))
		 (expansion-function batac=expand-ande)
		 (help "Two sided And-Elimination."))

(tac~deftactic ande-f ande (in base)
   (premises (L1 "The conjunction"))
   (conclusions (L2 "The left conjunct")
		(L3 "The right conjunct"))
   (computations (L2 (batac=compute-left-conjunct (formula L1)))
                 (L3 (batac=compute-right-conjunct (formula L1))))
   (sideconditions (logic~conjunction-p (formula L1)))
   (description "Forward application of AND-Elimination"))

(tac~deftactic ande-l ande (in base)
   (premises L1)
   (conclusions L2 L3)
   (computations (L3 (batac=compute-right-conjunct (formula L1))))
   (sideconditions (batac=left-conjunct-p (formula L2) (formula L1))
		   (logic~conjunction-p (formula L1)))
   (description "Forward application of AND-Elimination"))

(tac~deftactic ande-r ande (in base)
   (premises L1)
   (conclusions L2 L3)
   (computations (L2 (batac=compute-left-conjunct (formula L1))))
   (sideconditions (batac=right-conjunct-p (formula L3) (formula L1))
		   (logic~conjunction-p (formula L1)))
   (description "Forward application of AND-Elimination"))

(tac~deftactic ande-b ande (in base)
   (premises L1)
   (conclusions L2 L3)
   (computations (L1 (batac=compute-conjunction (formula L2) (formula L3))))
   (sideconditions )
   (description "Backward application of AND-Elimination"))

(tac~deftactic ande-a ande (in base)
   (premises L1)
   (conclusions L2 L3)
   (computations )
   (sideconditions (logic~conjunction-p (formula L1))
		   (batac=left-conjunct-p (formula L2) (formula L1))
		   (batac=right-conjunct-p (formula L3) (formula L1)))
   (description "Application of AND-Elimination to existent lines."))

(defun batac=compute-left-conjunct (conj)
  (car (data~appl-arguments conj)))

(defun batac=compute-right-conjunct (conj)
  (cadr (data~appl-arguments conj)))

(defun batac=compute-conjunction (lc rc)
  (let ((and (env~lookup-object :and (pds~environment omega*current-proof-plan))))
    (term~appl-create and (list lc rc))))

(defun batac=left-conjunct-p (lc conj)
  (data~equal lc (car (data~appl-arguments conj))))

(defun batac=right-conjunct-p (rc conj)
  (data~equal rc (cadr (data~appl-arguments conj))))

(defun batac=expand-ande (outline parameters)
  (tacl~init outline)
  (tacl~sequence 
   (dummy1 ('andel (list (first outline) (third outline)) nil))
   (dummy2 ('ander (list (second outline) (third outline)) nil)))
  (tacl~end))

(com~defcommand ande
  (argnames conjunction lconj rconj)
  (argtypes ndline ndline ndline)
  (arghelps "Conjunction to split" "Left conjunct" "Right conjunct")
  (function batac=ande)
  (defaults batac=ande-defaults)
  (frag-cats tactics base-connective)
  (log-p T)
  (help "Split a conjunction into its two conjuncts."))

(defun batac=ande (con lc rc)
  (infer~compute-outline 'ande (list lc rc con) nil))


(defun batac=ande-defaults (conj lconj rconj)
  (cond ((not (com~specified-arg-p conj))
	 (list (pds~find-support #'logic~conjunction-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p lconj))
	 (list conj
	       (if (and (pdsn~p conj) (logic~conjunction-p (node~formula conj)))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p (car (data~appl-arguments (node~formula conj))))))
		 (oc~default-current-planline))
	       (com~unspecified)))
	((not (com~specified-arg-p rconj))
	 (list conj
	       lconj
	       (if (and (pdsn~p conj) (logic~conjunction-p (node~formula conj)))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p (cadr (data~appl-arguments (node~formula conj))))))
		 (oc~default-current-planline))))
	(t (list conj lconj rconj))))


;;
;; Double Negation elimination
;;

(infer~deftactic notnote 
		 (outline-mappings (((nonexistent existent) notnote-f)
                                    ((existent nonexistent) notnote-b)
				    ((existent existent) notnote-a)))
                 (expansion-function batac=expand-notnote))

                                   
(tac~deftactic notnote-f notnote (in base)
               (premises (L1 "A double negation line."))
               (conclusions L2)
               (computations (L2 (batac=compute-remove-negation (formula L1))))
               (sideconditions (batac=double-negation-p (formula L1)))
               (description "Forward application of double negation elemination."))


(defun batac=compute-remove-negation (neg)
  (first (data~appl-arguments (first (data~appl-arguments neg)))))

(defun batac=double-negation-p (term)
  (and (logic~negation-p term)
       (logic~negation-p (first (data~appl-arguments term)))))


(tac~deftactic notnote-b notnote (in base)
               (premises L1)
               (conclusions L2)
               (computations (L1 (batac=introduce-double-negation (formula L2))))
               (description "Backward application of double negation elemination."))


(defun batac=introduce-double-negation (form)
  (batac=compute-negated-formula (batac=compute-negated-formula form)))

(tac~deftactic notnote-a notnote (in base)
               (premises L1)
               (conclusions L2)
               (computations )
	       (sideconditions (batac=double-negation-p (formula L1))
			       (batac=term-double-neg-term-p (formula L2) (formula L1)))
               (description "Backward application of double negation elemination."))

(defun batac=term-double-neg-term-p (term neg-term)
  (data~equal term (batac=compute-remove-negation neg-term)))

(defun batac=expand-notnote (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (conc-form (node~formula conc))
	 (line (tacl~insert&return-assumption 'base 'tertium-non-datur)))
    (tacl~init outline)
    (tacl~sequence
     (tert-non-dat ('foralle (list nil line) (list conc-form)))   ;;; tertium-non-dat
     (ore-res ('ore (list conc (car tert-non-dat) nil nil) nil))  ;;; A tertium-non-dat A1 A2 hyp1 neghyp2
     (note-res ('note (list nil (sixth ore-res) precond) nil)) ;;; bot neghyp2 precond
     (falsee-res ('falsee (list (fourth ore-res) (first note-res)) nil)) ;;; A2 bot
     (dummy ('weaken (list (third ore-res) (fifth ore-res)) nil))) ;;; a1 hyp1
    (tacl~end)))


(com~defcommand notnote
  (argnames neg pos)
  (argtypes ndline ndline)
  (arghelps "A line with a double negated formula." "The conclusion line.")
  (function batac=notnote)
  (frag-cats tactics base-negation)
  (defaults batac=notnote-defaults)
  (log-p T)
  (help "Eliminate double negations."))

(defun batac=notnote (neg pos)
  (infer~compute-outline 'notnote (list pos neg) nil))


(defun batac=notnote-defaults (neg pos)
  (cond ((not (com~specified-arg-p neg))
	 (list (pds~find-support #'batac=double-negation-p) (com~unspecified)))
	((not (com~specified-arg-p pos))
	 (list neg
	       (if (and (pdsn~p neg) (batac=double-negation-p (node~formula neg)))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p (car (data~appl-arguments (car (data~appl-arguments (node~formula neg))))))))
		 (oc~default-current-planline))))
	(t (list neg pos))))
	 


;;
;; Double Negation introduction
;;

(infer~deftactic notnoti 
		 (outline-mappings (((nonexistent existent) notnoti-f)
                                    ((existent nonexistent) notnoti-b)
				    ((existent existent) notnoti-a)))
                 (expansion-function batac=expand-notnoti))

                                   
(tac~deftactic notnoti-f notnoti (in base)
               (premises L1)
               (conclusions (L2 "A double negation line."))
               (computations (L2 (batac=introduce-double-negation (formula L1))))
               (sideconditions )
               (description "Forward application of double negation introduction."))



(tac~deftactic notnoti-b notnoti (in base)
               (premises L1)
               (conclusions L2)
               (computations (L1 (batac=compute-remove-negation (formula L2))))
	       (sideconditions (batac=double-negation-p (formula L2)))
               (description "Backward application of double negation introduction."))



(tac~deftactic notnoti-a notnoti (in base)
               (premises L1)
               (conclusions L2)
               (computations )
	       (sideconditions (batac=double-negation-p (formula L2))
			       (batac=term-double-neg-term-p (formula L1) (formula L2)))
               (description "Backward application of double negation introduction."))

(defun batac=expand-notnoti (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline)))
    (tacl~init outline)
    (tacl~sequence
     (noti ('noti (list conc nil) nil)) ;; conc bot hyp 
     (note ('note (list (second noti)  precond (third noti)) nil))) ;; bot hyp precond
    (tacl~end)))


(com~defcommand notnoti
  (argnames neg pos)
  (argtypes ndline ndline)
  (arghelps "A double negated formula." "A formula.")
  (function batac=notnoti)
  (frag-cats tactics base-negation)
  (defaults batac=notnoti-defaults)
  (log-p T)
  (help "Introduce double negations."))



(defun batac=notnoti (neg pos)
  (infer~compute-outline 'notnoti (list neg pos) nil))


(defun batac=notnoti-defaults (neg pos)
  (cond ((not (com~specified-arg-p neg))
	 (list (pds~find-open-node #'batac=double-negation-p) (com~unspecified)))
	((not (com~specified-arg-p pos))
	 (list neg
	       (if (and (pdsn~p neg) (batac=double-negation-p (node~formula neg)))
		   (pds~find-node-support
		    neg
		    #'(lambda (p)
			(data~equal p (car (data~appl-arguments (car (data~appl-arguments (node~formula neg))))))))
		 (com~unspecified))))
	(t (list neg pos))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushneg
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic pushneg 
		 (outline-mappings (((nonexistent existent) pushneg-f)
                                    ((existent nonexistent) pushneg-b)
				    ((existent existent) pushneg-a)))
                 (expansion-function batac=expand-pushneg))


(tac~deftactic pushneg-a pushneg (in base)
               (premises L2)
               (conclusions L1)
               (sideconditions (batac=pushneg-a-p (formula L1) (formula L2)))
               (description "Application of pushnegation rule."))

(defun batac=pushneg-a-p (pushed negA)
  (and (logic~negation-p negA)
       (term~alpha-equal (pds~pushneg negA) pushed)))  


(tac~deftactic pushneg-f pushneg (in base)
               (premises L1)
               (conclusions L2)
               (computations (L2 (pds~pushneg (formula L1))))
               (sideconditions (logic~negation-p (formula L1)))
               (description "Forward application of pushnegation rule."))

(tac~deftactic pushneg-b pushneg (in base)
               (premises L1)
               (conclusions L2)
               (computations (L1 (pds~pullneg (formula L2))))
               (description "Backward application of pushnegation rule."))


(defun batac=expand-pushneg (outline parameters)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (conc (first outline))
	 (prem (second outline))
	 (conc-form (node~formula conc))
	 (prem-form (node~formula prem))
	 (the-not (data~appl-function prem-form))
	 (scope (first (data~appl-arguments prem-form))))
    (format t "~% conc ~A prem ~A the-not ~A scope ~A" conc prem the-not scope)
    (tacl~init outline)
    (cond ((logic~negation-p scope)  ;; prem: nnA    conc: A
	   (tacl~sequence
	    (unneg-res ('notnote (list nil prem) nil)) ;; nnA A
	    (weaken-res ('weaken (list conc (first unneg-res)) nil))))
	  ((logic~disjunction-p scope) ;; prem: n(AvB)   conc: nA^nB
	   (tacl~sequence
	    (andi-res ('andi (list conc nil nil) nil)) ;; nAvnB nA nB
	    (noti1-res ('noti (list (second andi-res) nil) nil)) ;; nA bot hypA
	    (noti2-res ('noti (list (third andi-res) nil) nil)) ;; nB bot hypB
	    (note1-res ('note (list (second noti1-res) nil prem) nil))  ;; bot AvB n(AvB)
	    (oril-res ('oril (list (second note1-res) (third noti1-res))
			     (list (second (data~appl-arguments (node~formula (second note1-res))))))) ;; AvB hypA
	    (note2-res ('note (list (second noti2-res) nil prem) nil))  ;; bot AvB n(AvB)
	    (orir-res ('orir (list (second note2-res) (third noti2-res))
			     (list (first (data~appl-arguments (node~formula (second note2-res))))))))) ;; AvB hypB
	  ((logic~conjunction-p scope)   ;; prem: n(A^B)    conc: nAvnB
	   (tacl~sequence
	    (indirect-res ('indirect (list conc nil) nil)) ;; conc bot hyp(n(nAvnB))
	    (pushneg-res ('pushneg (list nil (third indirect-res)) nil))
			                                   ;; nnA^nnB hyp(n(nAvnB))
	    (ande-res ('ande (list nil nil (first pushneg-res)) nil))
	                                                   ;; nnA nnB nnA^nnB
	    (notnote1-res ('notnote (list nil (first ande-res)) nil)) ;; A nnA
	    (notnote2-res ('notnote (list nil (second ande-res)) nil)) ;; B nnB
	    (andi-res ('andi (list nil (first notnote1-res) (first notnote2-res)) nil))
	                                                              ;; A^B A B
	    (note-res ('note (list (second indirect-res) (first andi-res) prem) nil))))
	  ((and (env~lookup-object :exists-sort env)
		(data~schema-equal (data~appl-function scope)       ;; prem n(S-EX x. A So)  conc (S-All x. nA )
				   (env~lookup-object :exists-sort env)))
	   (let* ((bvar (logic~quantification-bound-variable scope))
		  (newconst (batac=generate-new-constant bvar))
		  (ex-def (th~find-assumption "exists-sort" (prob~theory omega*current-proof-plan)))
		  (ex-definiendum (th~definition-constant ex-def))
		  (ex-definiens (data~copy (th~ass-node ex-def) :downto '(term+constant type+primitive)))
		  (al-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
		  (al-definiendum (th~definition-constant al-def))
		  (al-definiens (data~copy (th~ass-node al-def) :downto '(term+constant type+primitive))))
	     (tacl~sequence
	      (defne-res ('defne (list nil prem) (list ex-definiendum ex-definiens (pos~list-position '(1 0)))))
	      (push-res  ('pushneg (list nil (first defne-res)) nil))
	      (foralle-res ('foralle (list nil (first push-res)) (list newconst)))
	      (push-res  ('pushneg (list nil (first foralle-res)) nil))
	      (or2imp-res ('or2imp (list nil (first push-res)) nil))
	      (defni-res ('defni (list conc nil)  (list al-definiendum al-definiens (pos~add-front 0))))
	      (foralli-res ('foralli (list (second defni-res)(first or2imp-res)) (list newconst))))))
	  ((and (env~lookup-object :forall-sort env)
		(data~schema-equal (data~appl-function scope)       ;; prem n(S-All x. nA )  conc (S-EX x. A So)
				   (env~lookup-object :forall-sort env)))
	   (let* ((bvar (logic~quantification-bound-variable scope))
		  (newconst (batac=generate-new-constant bvar))
		  (ex-def (th~find-assumption "exists-sort" (prob~theory omega*current-proof-plan)))
		  (ex-definiendum (th~definition-constant ex-def))
		  (ex-definiens (data~copy (th~ass-node ex-def) :downto '(term+constant type+primitive)))
		  (al-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
		  (al-definiendum (th~definition-constant al-def))
		  (al-definiens (data~copy (th~ass-node al-def) :downto '(term+constant type+primitive))))
	     (tacl~sequence
	      (defne-res ('defne (list nil prem) (list al-definiendum al-definiens (pos~list-position '(1 0)))))
	      (push-res  ('pushneg (list nil (first defne-res)) nil))
	      (existse-res ('existse (list conc (first push-res) nil ) (list newconst)))
	      (push-res  ('pushneg (list nil (fourth existse-res)) nil))
	      (defni-res ('defni (list (third existse-res) nil)  (list ex-definiendum ex-definiens (pos~add-front 0))))
	      (existi-res ('existsi (list (second defni-res)(first push-res))
				    (list newconst
					  (data~substruct-positions newconst (node~formula (first push-res)))))))))
	  ((logic~existential-quantification-p scope)   ;; prem:  n(EXx.A)    conc: ALLx.nA
	   (let* ((bvar (logic~quantification-bound-variable scope))
		  (newconst (batac=generate-new-constant bvar))
		  (poslist (data~substruct-positions bvar (data~abstr-scope (first (data~appl-arguments scope))))))
	     (tacl~sequence
	      (foralli-res ('foralli (list conc nil) (list newconst))) ;; ALLx.nA nA, wobei x const
	      (noti-res ('noti (list (second foralli-res) nil) nil))    ;; nA bot hypA
	      (note-res ('note (list (second noti-res) nil prem) nil)) ;; bot EXx.A n(EXx.A)
	      (existsi ('existsi (list (second note-res) (third noti-res)) (list
									    (data~struct-at-position (node~formula
												      (third noti-res))
												     (car poslist)) 
									    poslist))))))
	  ((logic~universal-quantification-p scope)   ;; prem: n(ALLx.A)  conc:  EXx.nA
	   (let* ((bvar (logic~quantification-bound-variable scope))
		  (newconst (batac=generate-new-constant bvar)))
	     (tacl~sequence
	      (indirect-res ('indirect (list conc nil) nil)) ;; EXx.nA bot hyp(nEXx.nA)
	      (pushneg-res ('pushneg (list nil (third indirect-res)) nil))
	                                                     ;; ALLx.nnA hyp(nEXx.nA) 
	      (foralle-res ('foralle (list nil (first pushneg-res)) (list newconst)))
	                                                     ;; nnA  ALLx.nnA
	      (notnote-res ('notnote (list nil (first foralle-res)) nil)) ;; A nnA
	      (note-res ('note (list (second indirect-res) nil prem) nil))
	                                                     ;; bot ALLx.A nALLx.A
	      (foralli-res ('foralli (list (second note-res) (first notnote-res)) (list
										   newconst)))))) ;; ALLx.A A, wobei x const
	  ((logic~implication-p scope)    ;; prem: n(A=>B)    conc: A^nB
	   (let ((Aterm (batac=compute-negated-formula (car (data~appl-arguments (node~formula conc)))))
		 (Bterm (car (data~appl-arguments (cadr (data~appl-arguments (node~formula conc)))))))
	     (tacl~sequence
	      (indirect-res ('indirect (list conc nil) nil)) ;; A^nB bot hyp(n(A^nB))
	      (pushneg-res ('pushneg (list nil (third indirect-res)) nil)) ;; nAvnnB hyp(n(A^nB))
	      (note-res ('note (list (second indirect-res) nil prem) nil))
	                                                                    ;; bot A=>B n(A=>B)
	      (ore-res ('ore (list (second note-res) (first pushneg-res) nil nil) nil))
	                                                                    ;; A=>B nAvnnB A=>B A=>B hypnA hypnnB
	      (oril-res ('oril (list nil (fifth ore-res)) (list Bterm))) ;; nAvB hypnA
	      (notnote-res ('notnote (list nil (sixth ore-res)) nil)) ;; B nnB
	      (orir-res ('orir (list nil (first notnote-res)) (list Aterm))) ;; nAvB B
	      (or2imp-res1 ('or2imp (list (third ore-res) (first oril-res)) nil)) ;; A=>B nAvB
	      (or2imp-res2 ('or2imp (list (fourth ore-res) (first orir-res)) nil))))) ;; A=>B nAvB
	  ((logic~equivalence-p scope)    ;; prem: n(A<=>B)  conc: (n(A=>B)vn(B=>A))
	   (tacl~sequence
	    (indirect-res ('indirect (list conc nil) nil)) ;;  (n(A=>B)vn(B=>A)) bot n(n(A=>B)vn(B=>A))
	    (pushneg-res ('pushneg (list nil (third indirect-res)) nil)) ;; nn(A=>B)vnn(B=>A)
	    (ande-res ('ande (list nil nil (first pushneg-res)) nil))
	                                                                   ;; nn(A=>B) nn(B=>A) n(n(A=>B)vn(B=>A))
	    (notnote-res1 ('notnote (list nil (first ande-res)) nil)) ;; (A=>B) nn(A=>B)
	    (notnote-res2 ('notnote (list nil (second ande-res)) nil)) ;; (B=>A) nn(B=>A)
	    (equivi-res ('equivi (list nil (first notnote-res1) (first notnote-res2)) nil))
	                                            ;; (A<=>B) (A=>B) (B=>A)
	    (note-res ('note (list (second indirect-res) (first equivi-res) prem) nil))))
	                                            ;; bot (A<=>B) n(A<=>B)
	  (t (warn "pushneg is not yet implemented for ~A" conc-form)))
    (tacl~end)))


(com~defcommand pushneg
  (argnames neg pos)
  (argtypes ndline ndline)
  (arghelps "A negated formula" "A formula with pushed negation")
  (function batac=pushneg)
  (frag-cats tactics base-negation)
  (defaults batac=pushneg-defaults)
  (log-p T)
  (help "Push a negation."))

(defun batac=pushneg (neg pos)
  (infer~compute-outline 'pushneg (list pos neg) nil))    


(defun batac=pushneg-defaults (neg pos)
  (cond ((not (com~specified-arg-p neg))
	 (list (pds~find-support
		#'(lambda (p) (and (logic~negation-p p)
				   (or (data~appl-p (car (data~appl-arguments p)))
				       (logic~existential-quantification-p (car (data~appl-arguments p)))
				       (logic~universal-quantification-p (car (data~appl-arguments p)))))))
	       (com~unspecified)))
	((not (com~specified-arg-p pos))
	 (list neg
	       (if (and (pdsn~p neg) (logic~negation-p (node~formula neg)))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p (pds~pushneg (node~formula neg)))))
		 (oc~default-current-planline))))
	(t (list neg pos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pullneg
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	    
(infer~deftactic pullneg 
		 (outline-mappings (((nonexistent existent) pullneg-f)
                                    ((existent nonexistent) pullneg-b)
				    ((existent existent) pullneg-a)))
                 (expansion-function batac=expand-pullneg))


(tac~deftactic pullneg-f pullneg (in base)
               (premises L1)
               (conclusions L2)
               (computations (L2 (pds~pullneg (formula L1))))
               (description "Forward application of pullnegation rule."))

(tac~deftactic pullneg-b pullneg (in base)
               (premises L1)
               (conclusions L2)
               (computations (L1 (pds~pushneg (formula L2))))
               (sideconditions (logic~negation-p (formula L2)))
               (description "Backward application of pullnegation rule."))


(tac~deftactic pullneg-a pullneg (in base)
               (premises L1)
               (conclusions L2)
               (computations)
               (sideconditions (batac=pullneg-a-p (formula L1) (formula L2)))
               (description "Backward application of pullnegation rule."))

(defun batac=pullneg-a-p (A pulled)
  (and (logic~negation-p pulled)
       (or (term~alpha-equal (pds~pullneg A) pulled)
	   (term~alpha-equal (pds~pushneg pulled) A) ;necessary since (not (equiv a b)) is not
						     ;equal to (pullneg (pushneg (not (equiv a b))))
	   ))) 


(defun batac=expand-pullneg (outline parameters)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (conc (first outline))
	 (prem (second outline))
	 (conc-form (node~formula conc))
	 (prem-form (node~formula prem))
	 (the-not (data~appl-function conc-form))
	 (scope (first (data~appl-arguments conc-form))))
    (tacl~init outline)
    (cond ((logic~disjunction-p scope)
	   (tacl~sequence
	    ((conj1 conj2) ('ande (list nil nil prem) nil))     ;;;conj1 conj2 prem
	    (noti-res ('noti (list conc nil) nil))              ;;;conc bot hyp
	    (ore-res ('ore (list (second noti-res) (third noti-res) nil nil) nil)) ;;;bot hyp bot1 bot2 hyp1 hyp2
	    (note1-res ('note (list (third ore-res) (fifth ore-res) conj1) nil))  ;;;bot1 hyp1 conj1
	    (note2-res ('note (list (fourth ore-res) (sixth ore-res) conj2) nil))  ;;;bot2 hyp2 conj2
	    ))
	  ((logic~conjunction-p scope)
	   (tacl~sequence
	    (noti-res ('noti (list conc nil) nil))                                 ;;;conc bot hyp
	    ((conj1 conj2) ('ande (list nil nil (third noti-res)) nil))            ;;;conj1 conj2 hyp
	    (ore-res ('ore (list (second noti-res) prem nil nil) nil))             ;;;bot prem bot1 bot2 hyp1 hyp2
	    (note1-res ('note (list (third ore-res) conj1 (fifth ore-res)) nil))   ;;;bot1 conj1 hyp1
	    (note2-res ('note (list (fourth ore-res) conj2 (sixth ore-res)) nil))  ;;;bot2 conj2 hyp2
	    ))
	  ((and (env~lookup-object :forall-sort env)
		(data~schema-equal (data~appl-function scope)       ;; conc n(S-All x. nA )  prem (S-EX x. A So)
				   (env~lookup-object :forall-sort env)))
	   (let* ((bvar (logic~quantification-bound-variable scope))
		  (newconst (batac=generate-new-constant bvar))
		  (ex-def (th~find-assumption "exists-sort" (prob~theory omega*current-proof-plan)))
		  (ex-definiendum (th~definition-constant ex-def))
		  (ex-definiens (data~copy (th~ass-node ex-def) :downto '(term+constant type+primitive)))
		  (al-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
		  (al-definiendum (th~definition-constant al-def))
		  (al-definiens (data~copy (th~ass-node al-def) :downto '(term+constant type+primitive))))
	     (tacl~sequence
	      (defne-res ('defne (list nil prem) (list ex-definiendum ex-definiens (pos~add-front  0))))
	      (existse-res ('existse (list conc (first defne-res) nil ) (list newconst)))
	      (defne-res ('defni (list (third existse-res) nil)  (list al-definiendum al-definiens (pos~list-position '(1 0)))))
	      (pull-res ('pullneg (list (second defne-res) nil) nil))
	      (existsi-res ('existsi (list (second pull-res) nil)
				     (list newconst
					   (data~substruct-positions
					    (logic~quantification-bound-variable (node~formula (second pull-res)))
					    (logic~quantification-scope (node~formula (second pull-res)))))))
	      (pull-res ('pullneg (list (second existsi-res) (fourth existse-res)) nil)))))
	  ((and (env~lookup-object :exists-sort env)
		(data~schema-equal (data~appl-function scope)       ;; conc n(S-EX x. A So)  prem (S-All x. nA )
				   (env~lookup-object :exists-sort env)))
	   (let* ((bvar (logic~quantification-bound-variable scope))
		  (newconst (batac=generate-new-constant bvar))
		  (ex-def (th~find-assumption "exists-sort" (prob~theory omega*current-proof-plan)))
		  (ex-definiendum (th~definition-constant ex-def))
		  (ex-definiens (data~copy (th~ass-node ex-def) :downto '(term+constant type+primitive)))
		  (al-def (th~find-assumption "forall-sort" (prob~theory omega*current-proof-plan)))
		  (al-definiendum (th~definition-constant al-def))
		  (al-definiens (data~copy (th~ass-node al-def) :downto '(term+constant type+primitive))))
	     (tacl~sequence
	      (defni-res ('defni (list conc nil) (list ex-definiendum ex-definiens (pos~list-position '(1 0)))))
	      (pull-res  ('pullneg (list (second defni-res) nil) nil))
	      (foralli-res ('foralli (list (second pull-res) nil) (list newconst)))
	      (pull-res  ('pullneg (list (second foralli-res) nil) nil))
	      (imp2or-res ('imp2or (list (second pull-res) nil) nil))
	      (defne-res ('defne (list nil prem)  (list al-definiendum al-definiens (pos~add-front 0))))
	      (foralle-res ('foralle (list (second imp2or-res)(first defne-res)) (list newconst))))))
	  ((logic~existential-quantification-p scope)
	   (let* ((bvar (logic~quantification-bound-variable scope))
		  (newconst (batac=generate-new-constant bvar)))
	     (tacl~sequence
	      (noti-res ('noti (list conc nil) nil))                                                 ;;;conc bot1 hyp1
	      (existse-res ('existse (list (second noti-res) (third noti-res) nil) (list newconst))) ;;;bot1 hyp1 bot2 hyp2
	      ((not-hyp2) ('foralle (list nil prem) (list newconst)))                                ;;;not-hyp2 prem
	      (note-res ('note (list (third existse-res) (fourth existse-res) not-hyp2) nil))        ;;;bot2 hyp2 not-hyp2
	      )))
	  ((logic~universal-quantification-p scope)
	   (let* ((bvar (logic~quantification-bound-variable scope))
		  (newconst (batac=generate-new-constant bvar)))
	     (tacl~sequence
	      (noti-res ('noti (list conc nil) nil))                                          ;;;conc bot1 hyp1
	      (existse-res ('existse (list (second noti-res) prem nil) (list newconst)))      ;;;bot1 prem bot2 hyp2
	      ((not-hyp2) ('foralle (list nil (third noti-res)) (list newconst)))             ;;;not-hyp2 hyp1
	      (note-res ('note (list (third existse-res) not-hyp2 (fourth existse-res)) nil)) ;;;bot2 hyp2 not-hyp2
	      )))
	  ((logic~implication-p scope)
	   (let ((Atermconc (car (data~appl-arguments (car (data~appl-arguments (node~formula conc))))))
		 (Atermprem (car (data~appl-arguments (node~formula prem)))))
	     (if (data~equal Atermconc Atermprem)
		 (tacl~sequence
		  (noti-res ('noti (list conc nil) nil))                 ;;;conc bot hyp
		  ((conj1 conj2) ('ande (list nil nil prem) nil))        ;;;conj1 conj2 prem
		  ((succ) ('impe (list nil conj1 (third noti-res)) nil)) ;;;succ conj1 hyp
		  (note-res ('note (list (second noti-res) succ conj2) nil))) ;;;bot succ conj2
	       (tacl~sequence
		(noti-res ('noti (list conc nil) nil))                 ;;;conc bot hyp
		((conj1 conj2) ('ande (list nil nil prem) nil))        ;;;conj1 conj2 prem
		((succ) ('impe (list nil conj2 (third noti-res)) nil)) ;;;succ conj1 hyp
		(note-res ('note (list (second noti-res) succ conj1) nil))))))
	  ((logic~equivalence-p scope)
	   (let ((arg1 (car (data~appl-arguments prem-form)))
		 (arg2 (cadr (data~appl-arguments prem-form))))
	     (cond
	      ((logic~disjunction-p prem-form)  ;;; n(A => B) \/ n(B => A) --> not(A<=>B)
	       (let* ((equiv-def (th~find-assumption "equiv" (prob~theory omega*current-proof-plan)))
 		      (equiv-definiendum (th~definition-constant equiv-def))
		      (equiv-definiens (data~copy (th~ass-node equiv-def) :downto '(term+constant type+primitive))))
		  (tacl~sequence
		   (pulled ('pullneg (list nil prem) nil))
		   (defni-res ('defni (list conc (car pulled))
				(list equiv-definiendum equiv-definiens (pos~list-position '(1 0))))))))
	      ((logic~negation-p arg1)  ;;; ((notA)<=>B) --> not(A<=>B) 
	       (tacl~sequence
		(noti-res ('noti (list conc nil) nil)) ;;; not(A<=>B) false {A<=>B}
		(equivsubst-res ('equivsubst (list nil prem (third noti-res)) (list (pos~add-front 2)))) ;;; nA<=>A
		(equive-res ('equive (list nil nil (car equivsubst-res)) nil)) ;;; nA=>A A=>nA
		(imp2or-res1 ('imp2or (list nil (car equive-res)) nil)) ;;; AvA
		(imp2or-res2 ('imp2or (list nil (cadr equive-res)) nil)) ;;; nAvnA
		(ore-res1 ('ore (list (cadr noti-res) (car imp2or-res1) nil nil) nil)) ;;; false AvA false false {A} {A}
		(ore-res2 ('ore (list (third ore-res1) (car imp2or-res2) nil nil) nil)) ;;; false nAvnA false false {nA} {nA}
		(ore-res3 ('ore (list (fourth  ore-res1) (car imp2or-res2) nil nil) nil)) ;;; false nAvnA false false {nA} {nA}
		(note-res1 ('note (list (third ore-res2) (fifth ore-res1) (fifth ore-res2)) nil)) ;;; false
		(note-res2 ('note (list (fourth ore-res2) (fifth ore-res1) (sixth ore-res2)) nil)) ;;; false
		(note-res1 ('note (list (third ore-res3) (sixth ore-res1) (fifth ore-res3)) nil)) ;;; false
		(note-res1 ('note (list (fourth ore-res3) (sixth ore-res1) (sixth ore-res3)) nil)))) ;;; false
	      (T                       ;;; (A<=>nB) --> n(A<=>B)
	       (tacl~sequence  
		(noti-res ('noti (list conc nil) nil)) ;;; n(A<=>B) false {A<=>B}
		(equivsubst-res ('equivsubst (list nil prem (third noti-res)) (list (pos~add-front 1)))) ;;; B<=>nB
		(equive-res ('equive (list nil nil (car equivsubst-res)) nil)) ;;; B=>nB nB=>B
		(imp2or-res1 ('imp2or (list nil (car equive-res)) nil)) ;;; nBvnB
		(imp2or-res2 ('imp2or (list nil (cadr equive-res)) nil)) ;;; BvB
		(ore-res1 ('ore (list (cadr noti-res) (car imp2or-res1) nil nil) nil)) ;;; false nBvnB false false {nB} {nB}
		(ore-res2 ('ore (list (third ore-res1) (car imp2or-res2) nil nil) nil)) ;;; false BvB false false {B} {B}
		(ore-res3 ('ore (list (fourth  ore-res1) (car imp2or-res2) nil nil) nil)) ;;; false BvB false false {B} {B}
		(note-res1 ('note (list (third ore-res2) (fifth ore-res2) (fifth ore-res1)) nil)) ;;; false
		(note-res2 ('note (list (fourth ore-res2) (sixth ore-res2) (fifth ore-res1)) nil)) ;;; false
		(note-res1 ('note (list (third ore-res3) (fifth ore-res3) (sixth ore-res1)) nil)) ;;; false
		(note-res1 ('note (list (fourth ore-res3) (sixth ore-res3) (sixth ore-res1)) nil)) ;;; false
		)))))
	  (t (tacl~apply 'notnoti (list conc prem) nil)))
    (tacl~end)))
  

(defun batac=generate-new-constant (var &optional (env (pds~environment
							 omega*current-proof-plan)))
  (term~generate-term-primitive-with-new-name
   (keim~name var)
   (term~type var)
   'term+constant
   env))


(com~defcommand pullneg
  (argnames neg pos)
  (argtypes ndline ndline)
  (arghelps "The formula with its negation pulled." "A formula with a negation inside.")
  (function batac=pullneg)
  (frag-cats tactics base-negation)
  (defaults batac=pullneg-defaults)
  (log-p T)
  (help "Pull a negation."))

(defun batac=pullneg (neg pos)
  (infer~compute-outline 'pullneg (list neg pos) nil))


(defun batac=pullneg-defaults (neg pos )
  (cond ((not (com~specified-arg-p neg))
	 (list (pds~find-open-node
		#'(lambda (p) (and (logic~negation-p p)
				   (or (logic~implication-p (car (data~appl-arguments p)))
				       (logic~disjunction-p (car (data~appl-arguments p)))
				       (logic~conjunction-p (car (data~appl-arguments p)))
				       (logic~existential-quantification-p (car (data~appl-arguments p)))
				       (logic~universal-quantification-p (car (data~appl-arguments p)))))))
	       (com~unspecified)))
	((not (com~specified-arg-p pos))
	 (list neg
	       (if (and (pdsn~p neg) (logic~negation-p (node~formula neg)))
		   (pds~find-node-support
		    neg
		    #'(lambda (p)
			(data~equal p (pds~pushneg (node~formula neg)))))
		 (com~unspecified))))
	(t (list neg pos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Or-MP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic ormp
		 (outline-mappings (((nonexistent existent existent) ormp-f)
				    ((existent existent nonexistent) ormp-l)
				    ((existent nonexistent existent) ormp-r)
				    ((existent existent existent) ormp-a)))
                 (expansion-function batac=expand-ormp)
		 (help "Modus ponens for disjunctions"))

(tac~deftactic ormp-f ormp (in base)
               (premises (L2 "A disjunction (or A B).")
			 (L3 "A negated formula (not B)."))
               (conclusions (L1 "A formula A."))
               (computations (L1 (batac=ormp-f (formula L2) (formula L3))))
               (sideconditions (batac=ormp-f-sidecond (formula L2) (formula L3)))
               (description "Forward application of ormp rule."))

(defun batac=ormp-f (disj neg)
  (let ((args (data~appl-arguments disj))
        (neg-arg (first (data~appl-arguments neg))))
    (cond ((data~equal neg-arg (first args))
           (second args))
          ((data~equal neg-arg (second args))
           (first args))
          (second (data~appl-arguments disj)))))

(defun batac=ormp-f-sidecond (disj neg)
  (let ((args (data~appl-arguments disj))
        (neg-arg (first (data~appl-arguments neg))))
    (and (logic~disjunction-p disj) (logic~negation-p neg)
         (or (data~equal neg-arg (first args))
             (data~equal neg-arg (second args))))))


(tac~deftactic ormp-a ormp (in base)
               (premises L2 L3)
               (conclusions L1)
               (computations )
               (sideconditions (batac=ormp-a-sidecond (formula L1) (formula L2) (formula L3)))
               (description "Application of ormp rule."))


(defun batac=ormp-a-sidecond (conc disj neg)
  (let ((args (data~appl-arguments disj))
        (neg-arg (first (data~appl-arguments neg))))
    (and (logic~disjunction-p disj) (logic~negation-p neg)
         (or (data~equal neg-arg (first args))
             (data~equal neg-arg (second args)))
	 (data~equal (batac=ormp-f disj neg)
		     conc))))

(tac~deftactic ormp-l ormp (in base)
               (premises L2 L3)
               (conclusions L1)
               (computations (L3 (batac=ormp-l (formula L2) (formula L1))))
               (sideconditions (batac=ormp-l-sidecond (formula L2) (formula L1)))
               (description "Backward application of ormp rule."))

(defun batac=ormp-l (disj con)
  (let ((args (data~appl-arguments disj)))
    (cond ((data~equal con (first args))
           (logic~negate (second args)))
          ((data~equal con (second args))
           (logic~negate (first args))))))

(defun batac=ormp-l-sidecond (disj con)
  (let ((args (data~appl-arguments disj)))  
    (and (logic~disjunction-p disj)
         (or (data~equal con (first args))
             (data~equal con (second args))))))

(tac~deftactic ormp-r ormp (in base)
               (premises L2 (L3  "A negated formula (not B)."))
               (conclusions L1)
               (computations (L2 (batac=ormp-r (formula L3) (formula L1))))
               (sideconditions (logic~negation-p (formula L3)))
               (description "Backward application of ormp rule."))

(defun batac=ormp-r (neg con)
  (term~appl-create (env~lookup-object :or (pds~environment omega*current-proof-plan))
               (list (first (data~appl-arguments neg)) con))) ;REIHENFOLGE?

(defun batac=expand-ormp (outline parameters)
  (let* ((conc (car outline))   ;; conc
	 (disj (cadr outline)) ;; disjunction
	 (neg (caddr outline))) ;; negated formula
    (tacl~init outline)
    (cond ((data~equal (node~formula conc) (cadr (data~appl-arguments (node~formula disj))))
	   ;; conc: B
	   ;; disj: AvB
	   ;; neg: nA
	   (tacl~sequence
	    (ore-res ('ore (list conc disj nil nil) nil)) ;; B AvB B B hypA hypB
	    (weaken-res ('weaken (list (fourth ore-res) (sixth ore-res)) nil)) ;;; B hypB
	    (note-res ('note (list nil (fifth ore-res) neg) nil)) ;;; bot hypA nA
	    (falsee-res ('falsee (list (third ore-res) (first note-res)) nil))) ;;; B bot
	   (tacl~end))
	  ((data~equal (node~formula conc) (car (data~appl-arguments (node~formula disj))))
	   ;; conc: A
	   ;; disj: AvB
	   ;; neg: nB
	   (tacl~sequence
	    (ore-res ('ore (list conc disj nil nil) nil)) ;; A AvB A A hypA hypB
	    (weaken-res ('weaken (list (third ore-res) (fifth ore-res)) nil)) ;;; B hypB
	    (note-res ('note (list nil (sixth ore-res) neg) nil)) ;;; bot hypB nB
	    (falsee-res ('falsee (list (fourth ore-res) (first note-res)) nil))) ;;; B bot
	   (tacl~end)))))

(com~defcommand ormp
  (argnames conc disj neg)
  (argtypes ndline ndline ndline)
  (arghelps "A formula B" "A disjunction (OR A B)" "A negated formula (not A)")
  (function batac=ormp)
  (frag-cats tactics base-connective)
  (defaults batac=defaults-ormp)
  (log-p T)
  (help "Eliminate disjunction."))


(defun batac=ormp (conc disj neg)
  (infer~compute-outline 'ormp (list conc disj neg) nil))



(defun batac=defaults-ormp (conc disj neg)
  (cond ((not (com~specified-arg-p conc))
	 (list (oc~default-current-planline) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p disj))
	 (list conc
	       (if (pdsn~p conc)
		   (pds~find-node-support
		    conc
		    #'(lambda (p)
			(and (logic~disjunction-p p)
			     (or (data~equal (node~formula conc) (cadr (data~appl-arguments  p)))
				 (data~equal (node~formula conc) (car (data~appl-arguments  p)))))))
		 (pds~find-support #'logic~disjunction-p))
	       (com~unspecified)))
	((not (com~specified-arg-p neg))
	 (list conc
	       disj
	       (cond ((and (pdsn~p conc) (pdsn~p disj) (logic~disjunction-p (node~formula disj)))
		      (cond ((data~equal (node~formula conc) (car (data~appl-arguments (node~formula disj))))
			     (pds~find-node-support
			      conc 
			      #'(lambda (p)
				  (and (logic~negation-p p)
				       (data~equal (cadr (data~appl-arguments (node~formula disj)))
						   (car (data~appl-arguments p)))))))
			    ((data~equal (node~formula conc) (cadr (data~appl-arguments (node~formula disj))))
			     (pds~find-node-support
			      conc 
			      #'(lambda (p)
				  (and (logic~negation-p p)
				       (data~equal (car (data~appl-arguments (node~formula disj)))
						   (car (data~appl-arguments p)))))))
			    (t (com~unspecified))))
		     ((and (pdsn~p disj) (logic~disjunction-p (node~formula disj)))
		      (pds~find-node-support
		       conc
		       #'(lambda (p)
			   (and (logic~negation-p p)
				(or (data~equal (car (data~appl-arguments p))
						(car (data~appl-arguments (node~formula disj))))
				    (data~equal (car (data~appl-arguments p))
						(cadr (data~appl-arguments (node~formula disj)))))))))
		     (t (pds~find-support #'logic~negation-p)))))
	(t (list conc disj neg))))



;;; Equivalences
;;;LC: Fehlen noch die Outline und Expansion Functions
(infer~deftactic "EquivEL"
		 (outline-mappings (((nonexistent existent) equivel-f)
				    ((existent nonexistent) equivel-b)
				    ((existent existent) equivel-a)))
		 (expansion-function batac=expand-equivel)
		 (help "Left EQUIV-Elimination."))

(tac~deftactic equivel-f equivel (in base)
   (premises (L2 "An equivalence."))
   (conclusions L1)
   (computations (L1 (batac=equivel-f-create (formula L2))))
   (sideconditions (batac=equivel-f-p (formula L2)))
   (description "Forward application of EQUIV-Elimination-LEFT."))

(defun batac=equivel-f-create (equivalence)
  (term~appl-create (env~lookup-object :implies (pds~environment omega*current-proof-plan))
		    (data~appl-arguments equivalence)))

(defun batac=equivel-f-p (term)
  (logic~equivalence-p term))

(tac~deftactic equivel-b equivel (in base)
   (premises (L2 "An equivalence."))
   (conclusions (L1 "An Implication."))
   (computations (L2 (batac=equivel-b-create (formula L1))))
   (sideconditions (batac=equivel-b-p (formula L1)))
   (description "Backward application of EQUIV-Elimination-LEFT."))

(defun batac=equivel-b-create (implication)
  (term~appl-create (env~lookup-object :equiv (pds~environment omega*current-proof-plan))
		    (data~appl-arguments implication)))

(defun batac=equivel-b-p (term)
  (logic~implication-p term))

(tac~deftactic equivel-a equivel (in base)
   (premises L2)
   (conclusions L1)
   (sideconditions (batac=equivel-a-p (formula L1) (formula L2)))
   (description "Backward application of EQUIV-Elimination-LEFT."))

(defun batac=equivel-a-p (term1 term2)
  (and (logic~implication-p term1)
       (logic~equivalence-p term2)
       (data~equal (first (data~appl-arguments term1)) (first (data~appl-arguments term2)))
       (data~equal (second (data~appl-arguments term1)) (second (data~appl-arguments term2)))))
       
(defun batac=expand-equivel (outline parameters)
  (let* ((LtR (first outline))
	 (equiv (second outline))
	 (equiv-def (th~find-assumption "equiv" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant equiv-def))
	 (definiens (data~copy (th~ass-node equiv-def) :downto '(term+constant type+primitive))))
    
    (tacl~init outline)
    (tacl~sequence
     ((conj) ('defne (list nil equiv) (list definiendum definiens (pos~add-front 0)))) ;;; conj equiv
     (ande-res ('andel (list LtR conj) nil))                      ;;; RtL conj
     ))
    (tacl~end))


(com~defcommand equivel
  (argnames equiv limp)
  (argtypes ndline ndline)
  (arghelps "Equivalence to split" "Left-to-right implication of an equivalence")
  (function batac=equivel)
  (frag-cats tactics base)
  (defaults batac=equivel-defaults)
  (log-p T)
  (help "Compute the Left-to-right implication of an equivalence"))

(defun batac=equivel (equiv limp)
  (infer~compute-outline 'equivel (list limp equiv) nil))

(infer~deftactic "EquivER"
		 (outline-mappings (((nonexistent existent) equiver-f)
				    ((existent nonexistent) equiver-b)
				    ((existent existent) equiver-a)))
		 (expansion-function batac=expand-equiver)
		 (help "Right EQUIV-Elimination."))


(defun batac=equivel-defaults (equiv left)
  (cond ((not (com~specified-arg-p equiv))
	 (list (pds~find-support #'logic~equivalence-p) (com~unspecified)))
	((not (com~specified-arg-p left))
	 (list equiv
	       (if (and (pdsn~p equiv) (logic~equivalence-p (node~formula equiv)))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p
				    (term~appl-create
				     (env~lookup-object :implies (pds~environment omega*current-proof-plan))
				     (list (car (data~appl-arguments (node~formula equiv)))
					   (cadr (data~appl-arguments (node~formula equiv))))))))
		 (oc~default-current-planline))))
	(t (list equiv left))))


(tac~deftactic equiver-f equiver (in base)
   (premises L2)
   (conclusions L1)
   (computations (L1 (batac=equiver-f-create (formula L2))))
   (sideconditions (batac=equiver-f-p (formula L2)))
   (description "Forward application of EQUIV-Elimination-LEFT."))

(defun batac=equiver-f-create (equivalence)
  (term~appl-create (env~lookup-object :implies (pds~environment omega*current-proof-plan))
		    (reverse (data~appl-arguments equivalence))))

(defun batac=equiver-f-p (term)
  (logic~equivalence-p term))

(tac~deftactic equiver-b equiver (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (batac=equiver-b-create (formula L1))))
   (sideconditions (batac=equiver-b-p (formula L1)))
   (description "Backward application of EQUIV-Elimination-LEFT."))

(defun batac=equiver-b-create (implication)
  (term~appl-create (env~lookup-object :equiv (pds~environment omega*current-proof-plan))
		    (reverse (data~appl-arguments implication))))

(defun batac=equiver-b-p (term)
  (logic~implication-p term))

(tac~deftactic equiver-a equiver (in base)
   (premises (L2 "An equivalence."))
   (conclusions (L1 "An implication"))
   (sideconditions (batac=equiver-a-p (formula L1) (formula L2)))
   (description "Backward application of EQUIV-Elimination-LEFT."))

(defun batac=equiver-a-p (term1 term2)
  (and (logic~implication-p term1)
       (logic~equivalence-p term2)
       (data~equal (first (data~appl-arguments term1)) (second (data~appl-arguments term2)))
       (data~equal (second (data~appl-arguments term1)) (first (data~appl-arguments term2)))))
       
(defun batac=expand-equiver (outline parameters)
  (let* ((RtL (first outline))
	 (equiv (second outline))
	 (=def (th~find-assumption "equiv" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant =def))
	 (definiens (data~copy (th~ass-node =def) :downto '(term+constant type+primitive))))
    
    (tacl~init outline)
    (tacl~sequence
     ((conj) ('defne (list nil equiv) (list definiendum definiens (pos~add-front 0)))) ;;; conj equiv
     (ande-res ('ander (list RtL conj) nil))                      ;;; RtL conj
     )
    (tacl~end)))

(com~defcommand equiver
  (argnames equiv rimp)
  (argtypes ndline ndline)
  (arghelps "Equivalence to split"
	    "Right-to-left implication of an equivalence")
  (function batac=equiver)
  (frag-cats tactics base)
  (defaults batac=equiver-defaults)
  (log-p T)
  (help "Compute the Right-toleft implication of an equivalence"))

(defun batac=equiver (equiv rimp)
  (infer~compute-outline 'equiver (list rimp equiv) nil))



(defun batac=equiver-defaults (equiv right)
  (cond ((not (com~specified-arg-p equiv))
	 (list (pds~find-support #'logic~equivalence-p) (com~unspecified)))
	((not (com~specified-arg-p right))
	 (list equiv
	       (if (and (pdsn~p equiv) (logic~equivalence-p (node~formula equiv)))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p
				    (term~appl-create
				     (env~lookup-object :implies (pds~environment omega*current-proof-plan))
				     (list (cadr (data~appl-arguments (node~formula equiv)))
					   (car (data~appl-arguments (node~formula equiv))))))))
		 (oc~default-current-planline))))
	(t (list equiv right))))


(infer~deftactic equive
		 (outline-mappings (((nonexistent nonexistent existent) equive-f)
				    ((existent nonexistent existent) equive-l)
				    ((nonexistent existent existent) equive-r)
				    ((existent existent nonexistent) equive-b)
				    ((existent existent existent) equive-a)))
		 (expansion-function batac=expand-equive)
		 (help "Two sided EQUIV-Elimination."))

(tac~deftactic equive-a equive (in base)
   (premises (L3 "An equivalence."))
   (conclusions (L1 "The left-to-right implication.") (L2 "The right-to-left implication."))
   (computations )
   (sideconditions (batac=equive-a-p (formula L1) (formula L2) (formula L3)))
   (description "Forward application of EQUIV-Elimination."))


(defun batac=equive-a-p (L1 L2 L3)
  (and (logic~equivalence-p L3) (logic~implication-p L1) (logic~implication-p L2)
       (term~alpha-equal (car (data~appl-arguments L1)) (car (data~appl-arguments L3)))
       (term~alpha-equal (cadr (data~appl-arguments L1)) (cadr (data~appl-arguments L3)))
       (term~alpha-equal (cadr (data~appl-arguments L2)) (car (data~appl-arguments L3)))
       (term~alpha-equal (car (data~appl-arguments L2)) (cadr (data~appl-arguments L3)))))


(tac~deftactic equive-f equive (in base)
   (premises L1)
   (conclusions L2 L3)
   (computations (L2 (batac=compute-left-implies (formula L1)))
                 (L3 (batac=compute-right-implies (formula L1))))
   (sideconditions (logic~equivalence-p (formula L1)))
   (description "Forward application of EQUIV-Elimination."))

(tac~deftactic equive-l equive (in base)
   (premises L1)
   (conclusions L2 L3)
   (computations (L3 (batac=compute-right-implies (formula L1))))
   (sideconditions (logic~equivalence-p (formula L1))
		   (logic~implication-p (formula L2))
		   (batac=left-implies-p (formula L2) (formula L1)))
   (description "Forward application of EQUIV-Elimination."))

(tac~deftactic equive-r equive (in base)
   (premises L1)
   (conclusions L2 L3)
   (computations (L2 (batac=compute-left-implies (formula L1))))
   (sideconditions (logic~equivalence-p (formula L1))
		   (logic~implication-p (formula l3))
		   (batac=right-implies-p (formula L3) (formula L1)))
   (description "Forward application of EQUIV-Elimination."))

(tac~deftactic equive-b equive (in base)
   (premises L1)
   (conclusions L2 L3)
   (computations (L1 (batac=compute-equivalence (formula L2))))
   (sideconditions (logic~implication-p (formula L2))
		   (logic~implication-p (formula L3))
		   (batac=reversed-implications-p (formula L2) (formula L3)))
   (description "Backward application of EQUIV-Elimination."))

(defun batac=compute-left-implies (equiv)
  (term~appl-create (env~lookup-object :implies (pds~environment omega*current-proof-plan))
	       (data~appl-arguments equiv)))

(defun batac=compute-right-implies (equiv)
  (term~appl-create (env~lookup-object :implies (pds~environment omega*current-proof-plan))
	       (reverse (data~appl-arguments equiv))))

(defun batac=left-implies-p (limp equiv)
  (let ((equiv-args (data~appl-arguments equiv))
	(limp-args (data~appl-arguments limp)))
    (and (data~equal (car equiv-args) (car limp-args))
	 (data~equal (cadr equiv-args) (cadr limp-args)))))

(defun batac=right-implies-p (rimp equiv)
  (let ((equiv-args (data~appl-arguments equiv))
	(rimp-args (data~appl-arguments rimp)))
    (and (data~equal (car equiv-args) (cadr rimp-args))
	 (data~equal (cadr equiv-args) (car rimp-args)))))

(defun batac=compute-equivalence (imp)
  (term~appl-create (env~lookup-object :equiv (pds~environment omega*current-proof-plan))
	       (data~appl-arguments imp)))

(defun batac=reversed-implications-p (limp rimp)
  (let ((limp-args (data~appl-arguments limp))
	(rimp-args (data~appl-arguments rimp)))
    (and (data~equal (car limp-args) (cadr rimp-args))
	 (data~equal (cadr limp-args) (car rimp-args)))))

(defun batac=expand-equive (outline parameters)
  (let* ((LtR (first outline))
	 (RtL (second outline))
	 (equiv (third outline))
	 (=def (th~find-assumption "equiv" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant =def))
	 (definiens (data~copy (th~ass-node =def) :downto '(term+constant type+primitive))))

    (tacl~init outline)
    (tacl~sequence
     ((conj) ('defne (list nil equiv) (list definiendum definiens (pos~add-front 0)))) ;;; conj equiv
     (ande-res ('ande (list LtR RtL conj) nil))                      ;;; LtR RtL conj
     )
    (tacl~end)))

(com~defcommand equive
  (argnames equiv limp rimp)
  (argtypes ndline ndline ndline)
  (arghelps  "Equivalence to split" "Left-to-right implication of an equivalence" "Right-to-left implication of an equivalence"
	    "Equivalence to split")
  (function batac=equive)
  (frag-cats tactics base)
  (defaults batac=equive-defaults)
  (log-p T)
  (help "Split an equivalence into its two implications."))

(defun batac=equive (equiv limp rimp)
  (infer~compute-outline 'equive (list limp rimp equiv) nil))


(defun batac=equive-defaults (equiv left right)
  (cond ((not (com~specified-arg-p equiv))
	 (list (pds~find-support #'logic~equivalence-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p left))
	 (list equiv
	       (if (and (pdsn~p equiv) (logic~equivalence-p (node~formula equiv)))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p
				    (term~appl-create
				     (env~lookup-object :implies (pds~environment omega*current-proof-plan))
				     (list (car (data~appl-arguments (node~formula equiv)))
					   (cadr (data~appl-arguments (node~formula equiv))))))))
		 (oc~default-current-planline))
	       (com~unspecified)))
	((not (com~specified-arg-p right))
	 (list equiv
	       left
	       (if (and (pdsn~p equiv) (logic~equivalence-p (node~formula equiv)))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p
				    (term~appl-create
				     (env~lookup-object :implies (pds~environment omega*current-proof-plan))
				     (list (cadr (data~appl-arguments (node~formula equiv)))
					   (car (data~appl-arguments (node~formula equiv))))))))
		 (oc~default-current-planline))))
	(t (list equiv left right))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equiv introduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic equivi
		 (outline-mappings (((nonexistent existent existent) equivi-f)
				    ((existent nonexistent nonexistent) equivi-b)
				    ((existent nonexistent existent) equivi-l)
				    ((existent existent nonexistent) equivi-r)
				    ((existent existent existent) equivi-a)))
		 (expansion-function batac=expand-equivi)
		 (help "Two sided EQUIV-Introduction."))


(tac~deftactic equivi-f equivi (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=compute-equivalence (formula L1))))
   (sideconditions (batac=reversed-implications-p (formula L1) (formula L2)))
   (description "Forward application of EQUIV-Introduction."))

(tac~deftactic equivi-b equivi (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=compute-left-implies (formula L3)))
                 (L2 (batac=compute-right-implies (formula L3))))
   (sideconditions (logic~equivalence-p (formula L3)))
   (description "Backward application of EQUIV-Elimination."))

(tac~deftactic equivi-l equivi (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=compute-left-implies (formula L3))))
   (sideconditions (logic~equivalence-p (formula L3))
		   (logic~implication-p (formula l2))
		   (batac=right-implies-p (formula L2) (formula L3)))
   (description "Backward application of EQUIV-Elimination."))

(tac~deftactic equivi-r equivi (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=compute-right-implies (formula L3))))
   (sideconditions (logic~equivalence-p (formula L3))
		   (logic~implication-p (formula l1))
		   (batac=left-implies-p (formula L1) (formula L3)))
   (description "Backward application of EQUIV-Elimination."))

(tac~deftactic equivi-a equivi (in base)
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (batac=equive-a-p (formula L2) (formula L3) (formula L1)))
   (description "Backward application of EQUIV-Elimination."))


(defun batac=expand-equivi (outline parameters)
  (let* ((LtR (second outline))
	 (RtL (third outline))
	 (equiv (first outline))
	 (=def (th~find-assumption "equiv" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant =def))
	 (definiens (data~copy (th~ass-node =def) :downto '(term+constant type+primitive))))
    
    (tacl~init outline)
    (let ((defni-res (tacl~apply 'defni (list equiv nil) (list definiendum definiens (pos~add-front 0)))))
      (if (and (term~alpha-equal (car (data~appl-arguments (node~formula (second defni-res))))
				 (node~formula LtR))
	       (term~alpha-equal (cadr (data~appl-arguments (node~formula (second defni-res))))
				 (node~formula RtL)))
	  (tacl~apply 'andi (list (second defni-res) LtR RtL) nil)
	(tacl~sequence	   
	 (defne1-res ('defne (list nil LtR) (list definiendum definiens (pos~add-front 0))))  ;; kann weg sobald
									;; defni&defne mit
									;; positionen  ---
									;; --> keim-3 
     
	 (defne2-res ('defne (list nil RtL) (list definiendum definiens (pos~add-front 0))))
	 (andi-res ('andi (list (second defni-res) (car defne1-res) (car defne2-res)) nil)))))      ;;; conj LtR RtL
    (tacl~end)))

(com~defcommand equivi
  (argnames equiv limp rimp)
  (argtypes ndline ndline ndline)
  (arghelps "Equivalence to split" "Left-to-right implication of an equivalence" "Right-to-left implication of an equivalence")
  (function batac=equivi)
  (frag-cats tactics base)
  (defaults batac=equivi-defaults)
  (log-p T)
  (help "Split an equivalence into its two implications."))

(defun batac=equivi (equiv limp rimp)
  (infer~compute-outline 'equivi (list equiv limp rimp) nil))



(defun batac=equivi-defaults (equiv left right)
  (cond ((not (com~specified-arg-p equiv))
	 (list (pds~find-open-node #'logic~equivalence-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p left))
	 (list equiv
	       (if (pdsn~p equiv)
		   (pds~find-node-support
		    equiv
		    #'(lambda (p)
			(data~equal p
				    (term~appl-create
				     (env~lookup-object :implies (pds~environment omega*current-proof-plan))
				     (list (car (data~appl-arguments (node~formula equiv)))
					   (cadr (data~appl-arguments (node~formula equiv))))))))
		 (pds~find-support #'logic~implication-p))
	       (com~unspecified)))
	((not (com~specified-arg-p right))
	 (list equiv
	       left
	       (cond ((pdsn~p equiv)
		      (pds~find-node-support
		       equiv
		       #'(lambda (p)
			   (data~equal p
				       (term~appl-create
					(env~lookup-object :implies (pds~environment omega*current-proof-plan))
					(list (cadr (data~appl-arguments (node~formula equiv)))
					      (car (data~appl-arguments (node~formula equiv)))))))))
		     ((pdsn~p left)
		      (pds~find-support
		       #'(lambda (p)
			   (data~equal p
				       (term~appl-create
					(env~lookup-object :implies (pds~environment omega*current-proof-plan))
					(list (cadr (data~appl-arguments (node~formula left)))
					      (car (data~appl-arguments (node~formula left)))))))))
		     (t (pds~find-support #'logic~implication-p)))))
	(t (list equiv left right))))

;;; Forall-E*

(infer~deftactic foralle*
		 (outline-mappings (((nonexistent existent) foralle*-f)
                                    ((existent existent) foralle*-a)
				    ))
		 (parameter-types term-list)
		 (expansion-function batac=expand-foralle*)
		 (help "Iterated FORALL-Elimination."))

(tac~deftactic foralle*-f foralle* (in base)
   (parameters (TL cons "A list of terms."))
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=compute-iterated-foralle (formula L1) TL)))
   (sideconditions (batac=forall-p (formula L1)))
   (description "Forward application of iterated FORALL-Elimination."))

(tac~deftactic foralle*-a foralle* (in base)
   (parameters (TL cons "A list of terms."))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac=forall-p (formula L1))
		   (batac=foralle*-a-p (formula L1) (formula L2) TL))
   (description "Forward application of iterated FORALL-Elimination."))

(defun batac=foralle*-a-p (l1 l2 tl)
  (data~equal (batac=compute-iterated-foralle l1 tl) l2))

(defun batac=forall-p (formula)
  (and
   (term~appl-p formula)
   (data~schema-equal (data~appl-function formula)
	       (env~lookup-object :forall (pds~environment omega*current-proof-plan)))))

(defun batac=compute-iterated-foralle (formula term-list)
  (let ((new-term formula))
    (dolist (x term-list)
      (setq new-term (beta~contract
                      (term~appl-create (car (data~appl-arguments new-term))
                                   (list x)))))
    new-term))

(defun batac=expand-foralle* (outline parameters)
  (let ((newline (cdr outline))
	(term-list (car parameters)))
    (tacl~init outline)
    (do* ((rest term-list (cdr rest))
	  (term (car rest) (car rest)))
	((null rest) t)
      (setf newline
	    (if (null (rest rest))
		(tacl~apply 'foralle (list (car outline) (car newline)) (list term))
	      (tacl~apply 'foralle (list nil (car newline)) (list term)))))
    (tacl~end)))
  
(com~defcommand foralle*
  (argnames univline elimline terms)
  (argtypes ndline ndline term-list)
  (arghelps "A universally quanitified line" "The reduced line" "A list of terms")
  (function batac=foralle*)
  (frag-cats tactics base-quantifier)
  (defaults batac=foralle*-defaults)
  (log-p T)
  (help "Apply a series of Forall-Eliminations."))

(defun batac=foralle*-defaults (univ elim termlist)
  (labels ((strip-quantors (term)
			   (if (logic~universal-quantification-p term)
			       (strip-quantors (logic~quantification-scope term))
			     term)))
    (cond ((not (com~specified-arg-p univ))
	   (list (pds~find-support #'logic~universal-quantification-p) (com~unspecified) (com~unspecified)))
	  ((not (com~specified-arg-p elim))
	   (list univ
		 (if (logic~universal-quantification-p (node~formula univ))
		     (let ((scope (strip-quantors (node~formula univ))))
		       (pds~find-open-node #'(lambda (x) (term~alpha-match scope x))))
		   (oc~nil-argument))
		 (com~unspecified)))
	  ((not (com~specified-arg-p termlist))
	   (list univ
		 elim
		 (if (and (logic~universal-quantification-p (node~formula univ))
			  elim)
		     (let* ((subst (term~alpha-match (strip-quantors (node~formula univ))
						     (node~formula elim))))
		       (if subst
			   (subst~codomain subst)     ;;; hier noch was zum Ordnen der Substitutionen einbauen  VS.
			 (com~unspecified)))
		   (com~unspecified))))
	  (t (list univ elim termlist)))))

(defun batac=foralle* (P C terms)
  (infer~compute-outline 'foralle* (list C P) (list terms)))



;;; foralli*

(infer~deftactic foralli*
		 (outline-mappings (((existent nonexistent) foralli*-b)
				    ((nonexistent existent) foralli*-f)))
		 (parameter-types termsym-list)
		 (expansion-function batac=expand-foralli*)
		 (help "Iterated FORALL-Introduction."))

(tac~deftactic foralli*-b foralli* (in base)
   (parameters (SL cons "A list of new constants."))
   (premises P)
   (conclusions C)
   (computations (P (batac=compute-iterated-foralli (formula C) SL)))
   (sideconditions (batac=forall-p (formula C))
		   (pds~constants-not-free-p SL C))
   (description "Backward application of iterated FORALL-Introduction."))

(tac~deftactic foralli*-f foralli* (in base)
   (parameters (clist cons "A list of constants."))
   (premises P)
   (conclusions C)
   (computations (C (batac=compute-iterated-foralli-f (formula p) clist)))
   (sideconditions (batac=constants-not-in-hyps clist P))
   (description "Forward application of iterated FORALL-Introduction."))

(defun batac=compute-iterated-foralli-f (term clist)
  (do* ((rest-clist (reverse clist) (rest rest-clist))
	(current-formula term))
      ((null rest-clist)
       current-formula)
    (setf current-formula (orules=substitute-and-quantify current-formula (first rest-clist)))))

(defun batac=constants-not-in-hyps (clist node)
  (every #'(lambda (const)
	     (orules=not-free-in-hyps-p const node))
	 clist))
  

(defun batac=compute-iterated-foralli (formula const-list)
  (let ((new-term formula))
    (dolist (const const-list)
      (setq new-term (beta~contract
                      (term~appl-create (car (data~appl-arguments new-term))
                                   (list const)))))
    new-term))

(defun batac=expand-foralli* (outline parameters)
  (let* ((forall-line (first outline))
	 (const-list (first parameters))
	 (last-const (first (last const-list))))
    (tacl~init outline)
    (when (rest const-list)
      (do ((rest-consts const-list (rest rest-consts)))
	  ((null (rest rest-consts)) t)
	(let* ((head-const (first rest-consts)))
	  (setq forall-line 
		(second (tacl~apply 'foralli (list forall-line nil) (list head-const)))))))
    (tacl~apply 'foralli (list forall-line (second outline)) (list last-const))
    (tacl~end)))
  
(com~defcommand foralli*
  (argnames univline line newconsts)
  (argtypes ndline ndline termsym-list)
  (arghelps "A universally quanitified line" "A line."  "A list of new constants wrt. the first line.")
  (function batac=foralli*)
  (frag-cats tactics base-quantifier)
  (defaults orules=foralli*-defaults)
  (log-p T)
  (help "Apply a series of Forall-Eliminations."))

(defun batac=foralli* (C P consts)
  (infer~compute-outline 'foralli* (list C P) (list consts)))

(defun orules=foralli*-defaults (planline line consts)
    (cond ((not (com~specified-arg-p planline))
	   (list (pds~find-open-node #'logic~universal-quantification-p) (com~unspecified) (com~unspecified)))
	  ((not (com~specified-arg-p line))
	   (list planline nil (com~unspecified)))
	  ((not (com~specified-arg-p consts))
	   (list planline line
		 (if planline
		     (orules=generate-defaults-foralli planline
						       (pds~environment omega*current-proof-plan))
		   nil)))
	  (t (list planline line consts))))

(defun orules=generate-defaults-foralli (line env)
  (let ((form (node~formula line)))
    (do ((form form (if (data~abstr-p (car (data~appl-arguments
					    form)))
			(logic~quantification-scope form)
		      form))
	 (res nil))
	((not (logic~universal-quantification-p form))
	 (nreverse res))
      (let ((var (if (data~abstr-p (car (data~appl-arguments form)))
		     (logic~quantification-bound-variable form))))
	(if var
	    (let ((var-name (concatenate 'string (string (keim~name var))
					 (princ-to-string 1))))
	      (if (env~lookup-object var-name env)
		  (let ((n 2))
		    (loop
		     (setq var-name (concatenate 'string (string (keim~name var))
						 (princ-to-string n)))
		     (when (not (env~lookup-object var-name env))
		       (return))
		     (incf n))))
	      (term~generate-term-primitive-with-new-name
	       (keim~name var)
	       (term~type var)
	       'term+constant
	       env)
	      (push (env~lookup-object var-name env) res))
	  (setq form nil)))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflexivity of equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic =ref
		 (outline-mappings (((nonexistent) =ref-f)
                                    ((existent) =ref-a)
				    ))
		 (parameter-types term)
		 (expansion-function batac=expand-=ref)
		 (help "Reflexivity of equality."))


(tac~deftactic =ref-f =ref (in base)
   (parameters (Term term+term "A term."))
   (premises)
   (conclusions L1)
   (computations (L1 (batac=equality-ref-create Term)))
   (sideconditions)
   (description "Forward application equality-reflexivity."))


(tac~deftactic =ref-a =ref (in base)
   (parameters (Term term+term "A term."))
   (premises)
   (conclusions L1)
   (computations)
   (sideconditions (batac=equality-ref-p (formula L1) Term))
   (description "Closing equality-reflexivity."))

(defun batac=equality-ref-create (Term)
  (term~appl-create
   (env~lookup-object := (pds~environment omega*current-proof-plan))
   (list Term Term)))

(defun batac=equality-ref-p (formula Term)
  (when (logic~equality-p formula)
    (and (data~equal (car (data~appl-arguments formula)) (cadr (data~appl-arguments formula)))
	 (data~equal (car (data~appl-arguments formula)) Term))))

   

(defun batac=expand-=ref (outline parameters)
  (let* ((=def (th~find-assumption "=" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant =def))
	 (definiens (data~copy (th~ass-node =def) :downto '(term+constant type+primitive))))
    
    (tacl~init outline)
    (let* ((result
	    (tacl~sequence
	     (defni-res ('defni (list (car outline) nil) (list  definiendum definiens (pos~add-front 0)))))) ;;; a=a leibaa
	   (pvar (logic~quantification-bound-variable (node~formula (second result))))  ;;; var(leibab)
	   (newconst (batac=generate-new-constant pvar)))
      (tacl~sequence
       (foralli-res ('foralli (list (second result) nil)  (list newconst)))    ;;; leibaa
									       ;;; (pa
									       ;;; =>pa) ,
									       ;;; wobei p
									       ;;; constante
       (impi-res ('impi (list (cadr foralli-res) nil) nil))                    ;;; (pa => pa) pa hyp
       (dummy ('weaken (list (second impi-res) (third impi-res)) nil))))
    (tacl~end)))



(com~defcommand =ref
  (argnames equality-line term)
  (argtypes ndline term)
  (arghelps "A line with equality" "A term.")
  (function batac==ref)
  (frag-cats tactics base-equality)
  (defaults batac==ref-defaults)
  (log-p T)
  (help "Equality-reflexity."))

(defun batac==ref (P term)
  (infer~compute-outline '=ref (list P) (list term)))


(defun batac==ref-defaults (equality-line term)
    (cond ((not (com~specified-arg-p equality-line))
	   (list (pds~find-open-node #'logic~equality-p)
		 (com~unspecified)))
	  ((not (com~specified-arg-p term))
	   (if (and equality-line (logic~equality-p (node~formula equality-line)))
	       (list equality-line
		     (car (data~appl-arguments (node~formula equality-line))))
	     (list (com~unspecified) (com~unspecified))))
	  (t (list equality-line term))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Symmetric Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic =sym
		 (outline-mappings (((nonexistent existent) =sym-f)
                                    ((existent existent) =sym-a)
                                    ((existent nonexistent) =sym-b)
				    ))
		 (parameter-types)
		 (expansion-function batac=expand-=sym)
		 (help "Commutativity of equality."))


(tac~deftactic =sym-f =sym (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=equality-sym-create (formula L1))))
   (sideconditions (logic~equality-p (formula L1)))
   (description "Forward application equality-symmetry."))


(tac~deftactic =sym-a =sym (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac=equality-sym-p (formula L1) (formula L2)))
   (description "Closing equality."))

(defun batac=equality-sym-create (term)
  (term~appl-create
   (env~lookup-object := (pds~environment omega*current-proof-plan))
   (list (second (data~appl-arguments term)) (first (data~appl-arguments term)))))


(tac~deftactic =sym-b =sym (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=equality-sym-create (formula L2))))
   (sideconditions (logic~equality-p (formula L2)))
   (description "Closing equality."))


(defun batac=equality-sym-p (term1 term2)
  (and (logic~equality-p term1)
       (logic~equality-p term2)
       (let ((args1 (data~appl-arguments term1))
             (args2 (data~appl-arguments term2)))
         (and (data~equal (first args1) (second args2))
              (data~equal (second args1) (first args2))))))

#| ;;; CB: see below, expansion by using =subst is much simpler
(defun batac=expand-=sym (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (=def (th~find-assumption "=" (prob~theory omega*current-proof-plan)))
	 (definiendum (car (data~appl-arguments (th~ass-formula =def))))
         (definiens (cadr (data~appl-arguments (th~ass-formula =def))))
	 (type (term~type (car (data~appl-arguments (node~formula conc)))))
	 (qvar (sym~variable-create 'Q (type~abstract type (type~o))))
	 (newconst (batac=generate-new-constant qvar))
	 (xvar (sym~variable-create 'x type))
	 (the-not (env~lookup-object :not (pds~environment omega*current-proof-plan)))    
	 (form (term~abstr-create xvar (term~appl-create the-not (list (term~appl-create qvar (list xvar)))))))
    
    (tacl~init outline)
    (tacl~sequence
     (defne-res ('defne (list nil precond) (list definiendum definiens))) ;;; leibab a=b ...
     (foralle-res ('foralle (list nil (car defne-res)) (list form))) ;;; (lamnotPa => lamnotPb) leibab form
        ;;; ab hier geht's nun rueckwarts
     (defni-res ('defni (list conc nil) (list definiendum definiens))) ;;; b=a leibba ...
     (foralli-res ('foralli (list (second defni-res) nil) (list newconst))) ;;; leibba (Pb => Pa)
     (contra-res ('contrapos (list (second foralli-res) nil) nil)) ;;; (Pb => Pa) (notPa => notPb)
     (lambda-res ('lambda (list (second contra-res) (car foralle-res)) nil)))
    (tacl~end)))
|#

(defun batac=expand-=sym (outline parameters)
  (let* ((conc (car outline))
	 (precond (cadr outline))
	 (refterm (car (data~appl-arguments (node~formula precond)))))
    (tacl~init outline)
    (tacl~sequence
     (ref-res ('=ref (list nil) (list refterm)))  ;;; a=a
     (subst-res ('=subst (list conc (car ref-res) precond) (list (pos~add-front 1)))))
    (tacl~end)))


(com~defcommand =sym
  (argnames equality-line1 equality-line2)
  (argtypes ndline ndline)
  (arghelps "A line with the conclusion equality."
	    "Another line with the premise equality.")
  (function batac==sym)
  (frag-cats tactics base-equality)
  (defaults batac==sym-defaults)
  (log-p T)
  (help "Equality-symmetry."))

(defun batac==sym (P P2)
  (infer~compute-outline '=sym (list P P2) nil))

(defun batac==sym-defaults (equality-line1 equality-line2)
    (cond ((not (com~specified-arg-p equality-line1))
	   (list (pds~find-open-node #'logic~equality-p) (com~unspecified)))
	  ((not (com~specified-arg-p equality-line2))
	   (list equality-line1
		 (if (and (pdsn~p equality-line1) (logic~equality-p (node~formula equality-line1)))
		     (pds~find-node-support
		      equality-line1
		      #'(lambda (p) (data~equal p (batac=equality-sym-create (node~formula equality-line1)))))
		   (pds~find-support #'logic~equality-p))))
	  (t (list equality-line1 equality-line2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transitivity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic =trans
		 (outline-mappings (((nonexistent existent existent) =trans-f)
                                    ((existent existent existent) =trans-a)
                                    ((existent existent nonexistent) =trans-l)
				    ((existent nonexistent existent) =trans-r)
                                    ))
		 (parameter-types)
		 (expansion-function batac=expand-=trans)
		 (help "Transitivity of equality."))


(tac~deftactic =trans-f =trans (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=equality-trans-create
                      (formula L1) (formula L2))))
   (sideconditions (batac=equality-trans-create
                    (formula L1) (formula L2)))
   (description "Forward application equality-transitivity."))


(tac~deftactic =trans-a =trans (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations)
   (sideconditions (batac=equality-trans-p (formula L3) (formula L1) (formula L2)))
   (description "Closing transitivity of equality."))

(defun batac=equality-trans-p (conclusion term1 term2)
  (let ((trans  (batac=equality-trans-create term1 term2)))
    (and trans
         (or (data~equal trans conclusion)
             (batac=equality-sym-p trans conclusion)))))


(defun batac=equality-trans-create (term1 term2)
  (let ((t11 (first (data~appl-arguments term1)))
        (t12 (second (data~appl-arguments term1)))
        (t21 (first (data~appl-arguments term2)))
        (t22 (second (data~appl-arguments term2))))
    (cond
     ((data~equal t11 t21)
      (term~appl-create
       (env~lookup-object := (pds~environment omega*current-proof-plan))
       (list t12 t22)))
     ((data~equal t12 t21)
      (term~appl-create
       (env~lookup-object := (pds~environment omega*current-proof-plan))
       (list t11 t22)))
     ((data~equal t11 t22)
      (term~appl-create
       (env~lookup-object := (pds~environment omega*current-proof-plan))
       (list t12 t21)))
     ((data~equal t12 t22)
      (term~appl-create
       (env~lookup-object := (pds~environment omega*current-proof-plan))
       (list t21 t11)))
     (T nil))))


(tac~deftactic =trans-l =trans (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=equality-trans-create (formula L1) (formula L3))))
   (sideconditions (batac=equality-trans-create (formula L1) (formula L3)))
   (description "Sideward application of transitivity."))


(tac~deftactic =trans-r =trans (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=equality-trans-create (formula L2) (formula L3))))
   (sideconditions (batac=equality-trans-create (formula L2) (formula L3)))
   (description "Sideward application of transitivity."))


(defun batac=expand-=trans (outline parameters)
  (let* ((conc (first outline))
	 (precond1 (second outline))
	 (precond2 (third outline)))
    (tacl~init outline)
    (let ((new-out
	   (if (find (car (data~appl-arguments  (node~formula precond1)))
		     (data~appl-arguments (node~formula precond2)) :test #'data~equal)
	       (tacl~apply '=subst (list nil precond1 precond2) (list (pos~add-front 1)))
	     (tacl~apply '=subst (list nil precond1 precond2) (list (pos~add-front 2))))))
      (if (data~equal (node~formula (car new-out)) (node~formula conc))
	  (tacl~apply 'weaken (list conc (car new-out)) nil)
	(tacl~apply '=sym (list conc (car new-out)) nil)))	   
    (tacl~end)))


(com~defcommand =trans
  (argnames equality-line1 equality-line2 equality-line3)
  (argtypes ndline ndline ndline)
  (arghelps "The conclusion line (a=c)" "Line with first equation (a=b)"
            "Line with first equation (b=c)")
  (function batac==trans)
  (frag-cats tactics base-equality)
  (defaults batac==trans-defaults)
  (log-p T)
  (help "Equality-transitivity."))

(defun batac==trans (P P2 P3)
  (infer~compute-outline '=trans (list P P2 P3) nil))

(defun batac==trans-defaults (equality-line1 equality-line2 equality-line3)
    (cond ((not (com~specified-arg-p equality-line1))
	   (list (pds~find-open-node #'logic~equality-p) (com~unspecified) (com~unspecified)))
	  ((not (com~specified-arg-p equality-line2))
	   (list equality-line1
		 (if (and (pdsn~p equality-line1) (logic~equality-p (node~formula equality-line1)))
		     (pds~find-node-support
		      equality-line1
		      #'(lambda (p) (and (logic~equality-p p)
					 (data~equal (car (data~appl-arguments p))
						     (car (data~appl-arguments (node~formula equality-line1)))))))
		   (pds~find-support #'logic~equality-p))
		 (oc~nil-argument)))
	  ((not (com~specified-arg-p equality-line3))
	   (list equality-line1
		 equality-line2
		 (cond ((and (pdsn~p equality-line1) (logic~equality-p (node~formula equality-line1))
			     (pdsn~p equality-line2) (logic~equality-p (node~formula equality-line2)))
			(pds~find-node-support
			 equality-line1
			 #'(lambda (p)
			     (and (logic~equality-p p)
				  (data~equal (cadr (data~appl-arguments p))
					      (cadr (data~appl-arguments (node~formula equality-line1))))
				  (data~equal (car (data~appl-arguments p))
					      (cadr (data~appl-arguments (node~formula equality-line2))))))))
		       ((and (pdsn~p equality-line2) (logic~equality-p (node~formula equality-line2)))
			(pds~find-support
			 #'(lambda (p)
			     (and (logic~equality-p p)
				  (data~equal (car (data~appl-arguments p))
					      (cadr (data~appl-arguments (node~formula equality-line2))))))))
		       ((and (pdsn~p equality-line1) (logic~equality-p (node~formula equality-line1)))
			(pds~find-node-support
			 equality-line1
			 #'(lambda (p)
			     (and (logic~equality-p p)
				  (data~equal (cadr (data~appl-arguments p))
					      (cadr (data~appl-arguments (node~formula equality-line1))))))))
		       (t (pds~find-support #'logic~equality-p)))))
	  (t (list equality-line1 equality-line2 equality-line3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Equality Substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; =subst

(infer~deftactic =subst
		 (outline-mappings (((nonexistent existent existent) =subst-f)
                                    ((existent existent existent) =subst-a)
                                    ((existent existent nonexistent) =subst-l)
				    ((existent nonexistent existent) =subst-r)
                                    ))
		 (parameter-types position)
		 (expansion-function batac=expand-=subst)
		 (help "Replacement property of equality."))


(tac~deftactic =subst-f =subst (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=equality-subst-create-f
                      (formula L1) (formula L2) position)))
   (sideconditions (batac=equality-subst-f-p
		    (formula L1) (formula L2) position))
   (description "Forward application equality-substitution."))


(tac~deftactic =subst-a =subst (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations)
   (sideconditions (batac=equality-subst-a-p
                    (formula L3) (formula L1) (formula L2) position))
   (description "Closing equality substitution."))

(tac~deftactic =subst-l =subst (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=equality-subst-create-l (formula L3) (formula L1) position))) 
   (sideconditions (batac=equality-subst-l-p (formula L3) (formula L1) position))
   (description "Creating equation for a substitution."))

(tac~deftactic =subst-r =subst (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=equality-subst-create-f (formula L3) (formula L2) position))) 
   (sideconditions (batac=equality-subst-f-p
                    (formula L3) (formula L2) position))
   (description "Creating equation for a substitution."))

(defun batac=equality-subst-f-p (term equal-term position)
  (when (logic~equality-p equal-term)
    (let* ((arg1 (first (data~appl-arguments equal-term)))
	   (arg2 (second (data~appl-arguments equal-term)))
	   (positions-of-arg1 (data~substruct-positions arg1 term :test 'data~equal))
	   (positions-of-arg2 (data~substruct-positions arg2 term :test 'data~equal)))
      (or (find position positions-of-arg1 :test 'keim~equal)
	  (find position positions-of-arg2 :test 'keim~equal)))))


(defun batac=equality-subst-create-f (term equal-term position)
  (let* ((term-at-position (data~struct-at-position term position))
	 (args (data~appl-arguments equal-term)))
    (cond ((data~equal term-at-position (first args))
	   (data~replace-at-position term position (second args)))
	  (t
	   (data~replace-at-position term position (first args))))))
	  

(defun batac=equality-subst-a-p (conclusion term equal-term position)
  (and (batac=equality-subst-f-p term equal-term position)
       (term~alpha-equal (batac=equality-subst-create-f term equal-term position) conclusion)))

(defun batac=equality-subst-l-p (conclusion term position)
  (let ((positions-of-conc (data~positions conclusion #'(lambda (arg) 't)))
	(positions-of-term (data~positions term #'(lambda (arg) 't))))
    (when (and (find position positions-of-conc :test 'keim~equal)
	       (find position positions-of-term :test 'keim~equal))
      (term~alpha-equal (data~replace-at-position conclusion
						  position
						  (data~struct-at-position term position))
			term))))
	     
(defun batac=equality-subst-create-l (conclusion term position)
  (term~appl-create (env~lookup-object := (pds~environment omega*current-proof-plan))
		    (list (data~struct-at-position conclusion position)
			  (data~struct-at-position term position))))



(defun batac=expand-=subst (outline parameters &optional (rec nil))
  (let* ((precond (second outline))
	 (equation (third outline))
	 (conc (first outline))
	 (=def (th~find-assumption "=" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant =def))
	 (definiens (data~copy (th~ass-node =def) :downto '(term+constant type+primitive)))
	 (pos (first parameters))
	 (inst1 (ignore-errors (data~appl-function (beta~expand (node~formula precond) (list pos)))))
	 (term-at-pos (ignore-errors (data~struct-at-position (node~formula precond) pos)))
	 (term-at-pos2 (ignore-errors (data~struct-at-position (node~formula conc) pos)))
	 (left-eq (car (data~appl-arguments (node~formula equation))))
	 (right-eq (cadr (data~appl-arguments (node~formula equation)))))
    (cond ((and term-at-pos term-at-pos2
		(data~equal term-at-pos left-eq);; precond: Ca  equation: a=b  conc: Cb
		(data~equal term-at-pos2 right-eq))
	   (tacl~init outline)
	   (tacl~sequence
	    (defne-res ('defne (list nil equation) (list definiendum definiens
							 (pos~add-front 0))))      ;;; leibab equation ...
	    (foralle-res ('foralle (list nil (car defne-res)) (list inst1)))  ;;; ((lamC)a => (lamC)b) leibab
                      ;;; nun rueckwaerts
	    (impe-res ('impe (list conc  precond nil) nil))    ;;; Cb Ca (Ca => Cb)
	    (lambda-res ('lambda (list (third impe-res) (first foralle-res)) nil)))		     
	   (tacl~end))
	  ((and term-at-pos term-at-pos2
		(data~equal term-at-pos right-eq);; precond: Ca  equation: b=a  conc: Cb
		(data~equal term-at-pos2 left-eq))
	   (tacl~init outline)   ;;;; das hier direct expandieren
	   (tacl~sequence
	    (=sym-res ('=sym (list nil equation) nil))
	    (=subst-res ('=subst (list conc precond (car =sym-res)) parameters)))
	   (tacl~end))
	  ((not rec)               ;;;;;;;;; Hack for non-compatible numerical functions!!!!!!!!!   VS
	   (batac=expand-=subst (mapcar #'natac~num2func-line outline) parameters 1))
	  ((= rec 1)               ;;;;;;;;; Hack for non-compatible numerical functions!!!!!!!!!   VS
	   (batac=expand-=subst (mapcar #'natac~func2num-line outline) parameters t))
	  (t (omega~error "Application of =-Subst cannot be expanded. The problematic terms are:~% ~A   <--->   ~A~% OR~% ~A   <--->   ~A~%In case numbers are involved try to establish the same representation everywhere!" term-at-pos left-eq term-at-pos right-eq)))))

	    

(com~defcommand =subst
  (argnames line1 line2 equality-line position)
  (argtypes ndline ndline ndline position)
  (arghelps "The substituted line" "The unsubstituted line"
            "The equation to be applied." "A position.")
  (function batac==subst)
  (frag-cats tactics base-equality)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (level 1)
  (log-p T)
  (help "Equality-Substitution."))

(defun batac==subst (P P2 P3 position)
  (infer~compute-outline '=subst (list P P2 P3) (list position)))



;;; =subst*

(infer~deftactic =subst*
		 (outline-mappings (((nonexistent existent existent) =subst*-f)
                                    ((existent existent existent) =subst*-a)
                                    ((existent existent nonexistent) =subst*-l)
				    ((existent nonexistent existent) =subst*-r)
                                    ))
		 (parameter-types position-list)
		 (expansion-function batac=expand-=subst*)
		 (help "Simultaneous replacement of several subterms with one equality."))


(tac~deftactic =subst*-f =subst* (in base)
   (parameters (positions cons "A list of positions."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=equality-subst*-create-f
                      (formula L1) (formula L2) positions)))
   (sideconditions (batac=equality-subst*-f-p
		    (formula L1) (formula L2) positions))
   (description "Forward application of simultaneous equality-substitution."))


(tac~deftactic =subst*-a =subst* (in base)
   (parameters (positions cons "A list of positions."))
   (premises L1 L2)
   (conclusions L3)
   (computations)
   (sideconditions (batac=equality-subst*-a-p
                    (formula L3) (formula L1) (formula L2) positions))
   (description "Closing simultaneous equality substitution."))

(tac~deftactic =subst*-l =subst* (in base)
   (parameters (positions cons "A list of positions."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=equality-subst*-create-l (formula L3) (formula L1) positions))) 
   (sideconditions (batac=equality-subst*-l-p (formula L3) (formula L1) positions))
   (description "Creating equation for a simultaneous substitution."))

(tac~deftactic =subst*-r =subst* (in base)
   (parameters (positions cons "A list of positions."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=equality-subst*-create-f (formula L3) (formula L2) positions))) 
   (sideconditions (batac=equality-subst*-f-p
                    (formula L3) (formula L2) positions))
   (description "Backward application of simultaneous equality-substitution."))

(defun batac=equality-subst*-f-p (term equal-term positions)
  (when (logic~equality-p equal-term)
    (let* ((arg1 (first (data~appl-arguments equal-term)))
	   (arg2 (second (data~appl-arguments equal-term)))
	   (positions-of-arg1 (data~substruct-positions arg1 term :test 'data~equal))
	   (positions-of-arg2 (data~substruct-positions arg2 term :test 'data~equal)))
      (every #'(lambda (position) (or (find position positions-of-arg1 :test 'keim~equal)
				      (find position positions-of-arg2 :test 'keim~equal)))
	     positions))))


(defun batac=equality-subst*-create-f (term equal-term positions)
  (let* ((terms-at-position (mapcar #'(lambda (position) (data~struct-at-position term position)) positions))
	 (args (data~appl-arguments equal-term))
	 (farg (car args))
	 (sarg (cadr args)))
    (labels ((recursive-subst (term repterms positions)
			      (if (and repterms positions)
				  (let ((fterm (car repterms))
					(fpos (car positions)))
				    (if (data~equal fterm farg)
					(recursive-subst
					 (data~replace-at-position term fpos sarg)
					 (cdr repterms) (cdr positions))
				      (recursive-subst
				       (data~replace-at-position term fpos farg)
				       (cdr repterms) (cdr positions))))
				term)))
      (recursive-subst term terms-at-position positions))))
	  

(defun batac=equality-subst*-a-p (conclusion term equal-term positions)
  (and (batac=equality-subst*-f-p term equal-term positions)
       (term~alpha-equal (batac=equality-subst*-create-f term equal-term positions) conclusion)))

(defun batac=equality-subst*-l-p (conclusion term positions)
  (let ((positions-of-conc (data~positions conclusion #'(lambda (arg) 't)))
	(positions-of-term (data~positions term #'(lambda (arg) 't))))
    (when (every #'(lambda (position) (and (find position positions-of-conc :test 'keim~equal)
					   (find position positions-of-term :test 'keim~equal)))
		 positions)
      (let* ((term-list (mapcar #'(lambda (pos) (data~struct-at-position term pos)) positions))
	     (conc-list (mapcar #'(lambda (pos) (data~struct-at-position conclusion pos)) positions))
	     (members (remove-duplicates (append term-list conc-list) :test #'data~equal)))
	(and (or (= (length members) 1)
		 (and
		  (= (length members) 2)
		  (let ((fm (first members))
			(sm (second members)))
		    (every #'(lambda (t1 t2)
			       (or (and (data~equal t1 fm) (data~equal t2 sm))
				   (and (data~equal t2 fm) (data~equal t1 sm))))
			   term-list conc-list))))
	     (labels ((recursive-subst (conc positions terms)
				       (if (and positions terms)
					   (recursive-subst
					    (data~replace-at-position conc
								      (car positions)
								      (car terms))
					    (cdr positions) (cdr terms))
					 conc)))
	       (term~alpha-equal (recursive-subst conclusion positions term-list) term)))))))
	     
(defun batac=equality-subst*-create-l (conclusion term positions)
  (term~appl-create (env~lookup-object := (pds~environment omega*current-proof-plan))
		    (list (data~struct-at-position conclusion (car positions))
			  (data~struct-at-position term (car positions)))))

(defun batac=expand-=subst* (outline parameters)
  (let* ((precond (second outline))
	 (equation (third outline))
	 (conc (first outline))
	 (pos-list (first parameters)))
    (tacl~init outline)
    (dolist (pos (butlast pos-list))
      (setf conc (second (tacl~apply '=subst (list conc nil equation) (list pos)))))
    (tacl~apply '=subst (list conc precond equation) (last pos-list))
    (tacl~end)))
	  

(com~defcommand =subst*
  (argnames line1 line2 equality-line position)
  (argtypes ndline ndline ndline position-list)
  (arghelps "The substituted line" "The unsubstituted line"
            "The equation to be applied" "A list of positions")
  (function batac==subst*)
  (frag-cats tactics base-equality)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (level 1)
  (log-p T)
  (help "Equality-Substitution in several sub-terms."))

(defun batac==subst* (P P2 P3 positions)
  (infer~compute-outline '=subst* (list P P2 P3) (list positions)))


;;; =subst**

(infer~defwild-tactic =subst**
		 (outline-mappings (((nonexistent list) =subst**-f)
                                    ((existent list) =subst**-b)
                                    ))  ;;; The a-direction is subsumed by the b-direction!
		 (parameter-types position-list-list)
		 (expansion-function batac=expand-=subst**)
		 (help "Simultaneous replacement of several subterms with respect to a list of equalities."))


(defun =subst**-f (concs prems parameters)
  (declare (ignore concs))
  (let ((prem (car prems))
	(equations (cdr prems))
	(positions (car parameters)))
    (when (and (= (length equations) (length positions))
	       (batac=equality-subst**-f-p prem equations positions))
      (values (list (batac=equality-subst**-create-f prem equations positions)) nil nil))))

(defun batac=equality-subst**-f-p (term equations positions)
  (every #'(lambda (eq pos) (batac=equality-subst*-f-p term eq pos))
	 equations positions))

(defun batac=equality-subst**-create-f (term equations positions)
  (let ((new-term term))
    (mapc #'(lambda (eq pos)
	      (setf new-term (batac=equality-subst*-create-f new-term eq pos)))
	  equations positions)
    new-term))

(defun =subst**-b (concs prems parameters)
  (let ((conc (car concs))
	(positions (car parameters)))
    (multiple-value-bind (prem equations)
	(batac==subst**-prems&equations prems (length positions))
      (cond ((and prem equations)                                 ;;; the a-direction
	     (when (batac=equality-subst**-a-p conc prem equations positions)
	       t))
	    (equations                                      ;;; the actual b-direction
	     (when (batac=equality-subst**-f-p conc equations positions)
	       (values nil (list (batac=equality-subst**-create-f conc equations positions)) nil)))
  ))))

(defun batac==subst**-prems&equations (list n)
  (declare (edited  "09-MAR-2000")
	   (authors Sorge)
	   (input   "A list and a number.")
	   (effect  "None.")
	   (value   "Two values: the car and the cdr of LIST if the length of LIST is N+1,"
		    "NIL and LIST if the length of LIST is equal to N, otherwise NIL."))
  (cond ((= (length list) (1+ n)) (values (car list) (cdr list)))
	((= (length list) n) (values nil list))))
  

(defun batac=equality-subst**-a-p (conclusion term equations positions)
  (and (batac=equality-subst**-f-p term equations positions)
       (term~alpha-equal (batac=equality-subst**-create-f term equations positions) conclusion)))

(defun batac=expand-=subst** (conclusions premises parameters)
  (let* ((precond (first premises))
	 (equations (rest premises))
	 (conc (first conclusions))
	 (positions (first parameters)))
    (tacl~init (append conclusions premises))
    (mapc #'(lambda (eq pos)
	      (setf conc (second (tacl~apply '=subst* (list conc nil eq) (list pos)))))
	  (butlast equations) (butlast positions))
    (tacl~apply '=subst* (list conc precond (car (last equations))) (last positions))
    (tacl~end)))
	  

(com~defcommand =subst**
  (argnames line1 line2 equality-lines position-lists)
  (argtypes ndline ndline ndline-list position-list-list)
  (arghelps "The substituted line" "The unsubstituted line"
            "A list of equations to be applied" "A list of position-lists")
  (function batac==subst**)
  (frag-cats tactics base-equality)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (level 1)
  (log-p T)
  (help "Equality-Substitution in several sub-terms with respect to a list of equations."))

(defun batac==subst** (P P2 P3 positions)
  (infer~compute-outline '=subst** (list P (if p2 (cons P2 P3) p3)) (list positions)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; neg equal introduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

P<a>  -P<b>
----------- neg=i 
  -a=b

Position always means the position in P<a> |#



(infer~deftactic neg=i
		 (outline-mappings (((nonexistent existent existent) neg=i-f)
                                    ((existent existent existent) neg=i-a)
                                    ((existent existent nonexistent) neg=i-l)
				    ((existent nonexistent existent) neg=i-r)
                                    ))
		 (parameter-types position)
		 (expansion-function batac=expand-neg=i)
		 (help "Replacement property of equality."))


(tac~deftactic neg=i-f neg=i (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=neg=i-create-f
                      (formula L1) (formula L2) position)))
   (sideconditions (batac=neg=i-f-p
		    (formula L1) (formula L2) position))
   (description "Forward application neg-equality introduction."))


(tac~deftactic neg=i-a neg=i (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations)
   (sideconditions (batac=neg=i-a-p
                    (formula L3) (formula L1) (formula L2) position))
   (description "Closing neg equality introduction."))

(tac~deftactic neg=i-l neg=i (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=neg=i-create-l (formula L3) (formula L1) position))) 
   (sideconditions (batac=neg=i-l-p
                    (formula L3) (formula L1) position))
   (description "Creating negation for neg-equality introduction."))

(tac~deftactic neg=i-r neg=i (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=neg=i-create-r (formula L3) (formula L2) position))) 
   (sideconditions (batac=neg=i-r-p
                    (formula L3) (formula L2) position))
   (description "Creating left side for neg-equality introduction."))

(defun batac=neg=i-f-p (term neg-term position)
  (if (logic~negation-p neg-term)
      (let* ((rest-term (first (data~appl-arguments neg-term))))
	(batac=equality-subst-l-p term rest-term position))
    nil))

(defun batac=neg=i-create-f (term neg-term position)
  (let* ((term1 (data~struct-at-position term position))
	 (term2 (data~struct-at-position (first (data~appl-arguments neg-term)) position)))
    (term~appl-create (env~lookup-object :not (pds~environment omega*current-proof-plan))
		      (list (term~appl-create (env~lookup-object := (pds~environment omega*current-proof-plan))
					      (list term1 term2))))))

(defun batac=neg=i-a-p (conclusion term neg-term position)
  (if (null (and (batac=neg=i-f-p term neg-term position)
		 (logic~negation-p conclusion)))
      nil
    (let* ((neg-equation (batac=neg=i-create-f term neg-term position))
	   (equation (first (data~appl-arguments neg-equation)))
	   (rest-conclusion (first (data~appl-arguments conclusion))))
      (or (data~equal equation rest-conclusion)
	  (batac=equality-sym-p equation rest-conclusion)))))
      

      
(defun batac=neg=i-l-p (conclusion term position)
  (and (logic~negation-p conclusion)
       (batac=equality-subst-f-p term (first (data~appl-arguments conclusion)) position)))

(defun batac=neg=i-create-l (conclusion term position)
  (let* ((new-term (batac=equality-subst-create-f term (first (data~appl-arguments conclusion)) position)))
    (term~appl-create (env~lookup-object :not (pds~environment omega*current-proof-plan))
		      (list new-term))))


(defun batac=neg=i-r-p (conclusion neg-term position)
  (and (logic~negation-p conclusion)
       (logic~negation-p neg-term)
       (batac=equality-subst-f-p (first (data~appl-arguments neg-term))
				 (first (data~appl-arguments conclusion)) position)))

(defun batac=neg=i-create-r (conclusion neg-term position)
  (batac=equality-subst-create-f (first (data~appl-arguments neg-term))
				 (first (data~appl-arguments conclusion)) position))



(defun batac=expand-neg=i (outline parameters)
  (let* ((conc (car outline))   ;;; (not a=b)
	 (precond1 (cadr outline)) ;;; P<a>
	 (precond2 (caddr outline)) ;; nP<b>
	 (pos (car parameters))) ;;; pos of a and b 
    (tacl~init outline)
    (tacl~sequence
     (noti-res ('noti (list conc nil) nil)) ;;; (not a=b) bot (a=b)
     (subst-res ('=subst (list nil precond1 (third noti-res)) (list pos))) ;;; P<b> P<a> (a=b)
     (note-res ('note (list (second noti-res) (first subst-res) precond2) nil)))
    (tacl~end)))



(com~defcommand neg=i
  (argnames unequation line1 line2 position)
  (argtypes ndline ndline ndline position)
  (arghelps "An unequation" "A line." "A negated line." "A position.")
  (function batac=neg=i)
  (frag-cats tactics base-equality)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Negated equality Introduction."))

(defun batac=neg=i (P P2 P3 position)
  (infer~compute-outline 'neg=i (list P P2 P3) (list position)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Symmetric Inequality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic neg=sym
		 (outline-mappings (((nonexistent existent) neg=sym-f)
                                    ((existent existent) neg=sym-a)
                                    ((existent nonexistent) neg=sym-b)
				    ))
		 (parameter-types)
		 (expansion-function batac=expand-neg=sym)
		 (help "Commutativity of inequality."))


(tac~deftactic neg=sym-f neg=sym (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=inequality-sym-create-f (formula L1))))
   (sideconditions (batac=inequality-sym-f-p (formula L1)))
   (description "Forward application inequality-symmetry."))


(tac~deftactic neg=sym-a neg=sym (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac=inequality-sym-a-p (formula L1) (formula L2)))
   (description "Closing inequality."))

(defun batac=inequality-sym-f-p (unequation)
  (and (logic~negation-p unequation)
       (logic~equality-p (first (data~appl-arguments unequation)))))

(defun batac=inequality-sym-create-f (term)
  (term~appl-create
   (env~lookup-object :not (pds~environment omega*current-proof-plan))
   (list (batac=equality-sym-create (first (data~appl-arguments term))))))

(defun batac=inequality-sym-a-p (term1 term2)
  (and (logic~negation-p term1)
       (logic~negation-p term2)
       (batac=equality-sym-p (first (data~appl-arguments term1))
			     (first (data~appl-arguments term2)))))

(tac~deftactic neg=sym-b neg=sym (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=inequality-sym-create-f (formula L2))))
   (sideconditions (batac=inequality-sym-f-p (formula L2)))
   (description "Closing inequality."))

(defun batac=expand-neg=sym (outline parameters)
  (let* ((conc (car outline))   ;;; (not b=a)
	 (precond (cadr outline)) ;;; (not a=b)
	 (refterm (cadr (data~appl-arguments (car (data~appl-arguments (node~formula conc)))))))  ;;; a
    (tacl~init outline)
    (tacl~sequence
     (noti-res ('noti (list conc nil) nil)) ;;; (not b=a) bot (b=a)
     (ref-res  ('=ref (list nil) (list refterm))) ;;; (a=a)
     (subst-res ('=subst (list nil precond (third noti-res))
			 (list (pos~add-front 1 (pos~add-front 2)))))        ;;; (not a=a)  (not a=b) (b=a) 
     (note-res ('note (list (second noti-res) (first ref-res) (first subst-res)) nil)))
    (tacl~end)))

(com~defcommand neg=sym
  (argnames inequality-line1 inequality-line2)
  (argtypes ndline ndline)
  (arghelps "A line with inequality" "Another line with inequality.")
  (function batac=neg=sym)
  (frag-cats tactics base-equality)
  (defaults batac=neg=sym-defaults)
  (log-p T)
  (help "Inequality-symmetry."))

(defun batac=neg=sym (P P2)
  (infer~compute-outline 'neg=sym (list P P2) nil))


(defun batac=neg=sym-defaults (inequality-line1 inequality-line2)
    (cond ((not (com~specified-arg-p inequality-line1))
	   (list (pds~find-open-node #'(lambda (p)
					 (and (logic~negation-p p)
					      (data~appl-p (car (data~appl-arguments p)))
					      (logic~equality-p (car (data~appl-arguments p))))))
		 (com~unspecified)))
	  ((not (com~specified-arg-p inequality-line2))
	   (list inequality-line1
		 (if (and (pdsn~p inequality-line1)
			  (logic~negation-p (node~formula inequality-line1))
			  (data~appl-p (car (data~appl-arguments (node~formula inequality-line1))))
			  (logic~equality-p (car (data~appl-arguments (node~formula inequality-line1)))))
		     (pds~find-node-support
		      inequality-line1
		      #'(lambda (p) (data~equal p (batac=inequality-sym-create-f (node~formula inequality-line1)))))
		   (pds~find-support #'(lambda (p)
					 (and (logic~negation-p p)
					      (data~appl-p (car (data~appl-arguments p)))
					      (logic~equality-p (car (data~appl-arguments p)))))))))
	  (t (list inequality-line1 inequality-line2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implies rewrite as or
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic imp2or
		 (outline-mappings (((nonexistent existent) imp2or-f)
                                    ((existent existent) imp2or-a)
                                    ((existent nonexistent) imp2or-b)
				    ))
		 (parameter-types)
		 (expansion-function batac=expand-imp2or)
		 (help "Rewriting implies as or."))

(tac~deftactic imp2or-f imp2or (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=imp2or-create-f (formula L1))))
   (sideconditions (batac=imp2or-f-p (formula L1)))
   (description "Forward application of imp2or"))

(defun batac=imp2or-f-p (implication-term)
  (logic~implication-p implication-term))

(defun batac=imp2or-create-f (implication-term)
  (term~appl-create (env~lookup-object :or (pds~environment omega*current-proof-plan))
		    (list (logic~negate (first (data~appl-arguments implication-term)))
			  (second (data~appl-arguments implication-term)))))


(tac~deftactic imp2or-a imp2or (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac=imp2or-a-p (formula L1) (formula L2)))
   (description "Closing imp2or."))

(defun batac=imp2or-a-p (implication-term or-term)
  (and (logic~implication-p implication-term)
       (logic~disjunction-p or-term)
       (or (data~equal (batac=imp2or-create-f implication-term) or-term)
	   (data~equal (batac=imp2or-create-b or-term) implication-term))))

(tac~deftactic imp2or-b imp2or (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=imp2or-create-b (formula L2))))
   (sideconditions (batac=imp2or-b-p (formula L2)))
   (description "imp2or backward."))

(defun batac=imp2or-b-p (or-term)
  (logic~disjunction-p or-term))

(defun batac=imp2or-create-b (or-term)
  (term~appl-create (env~lookup-object :implies  (pds~environment omega*current-proof-plan))
		    (list (logic~negate (first (data~appl-arguments or-term)))
			  (second (data~appl-arguments or-term)))))


(defun batac=expand-imp2or (outline parameters)
  (let ((conc (car outline)) 
	(precond (cadr outline)))
    (tacl~init outline)
    (if (term~alpha-equal (term~appl-create (env~lookup-object :not (pds~environment omega*current-proof-plan))
					      (list (car (data~appl-arguments (node~formula conc)))))
			    (car (data~appl-arguments (node~formula precond))))
	  ;(logic~negation-p (car (data~appl-arguments (node~formula precond))))
	  ;; precond: nA=>B          or    precond: nA=>nB
	  ;; conc:    AvB                  conc:    AvnB
	  ;MP:  that's not enough, think of  precond:    nnA=>B
	  ;                                  conc:        nAvB               
	(let ((tertium-arg (car (data~appl-arguments (node~formula conc))))
	      (line (tacl~insert&return-assumption 'base 'tertium-non-datur)))
	  (tacl~sequence
	   (tert-non-dat ('foralle (list nil line) (list tertium-arg)))   ;;; tertium-non-dat(AvnA) ...
	   (ore-res ('ore (list conc (car tert-non-dat) nil nil) nil)) ;;; AvB (AvnA) AvB AvB hypA hypnA
	   (oril-res ('oril (list (third ore-res) (fifth ore-res))
			    (list (second (data~appl-arguments (node~formula (third ore-res))))))) ;;; AvB hypA
	   (impe-res ('impe (list nil (sixth ore-res) precond) nil)) ;; B hypnA nA=>B
	   (orir-res ('orir (list (fourth ore-res) (first impe-res))
			    (list (first (data~appl-arguments (node~formula (fourth ore-res)))))))) ;; AvB B
	  (tacl~end))
	      ;; precond: A=>B          or    precond: A=>nB
	      ;; conc:    nAvB                conc:  nAvnB
      (let ((tertium-arg (car (data~appl-arguments (node~formula precond))))
		 (line (tacl~insert&return-assumption 'base 'tertium-non-datur)))
	(tacl~sequence
	 (tert-non-dat ('foralle (list nil line) (list tertium-arg)))   ;;; tertium-non-dat(AvnA) ...
	 (ore-res ('ore (list conc (car tert-non-dat) nil nil) nil)) ;;; nAvB (AvnA) nAvB nAvB hypA hypnA
	 (oril-res ('oril (list (fourth ore-res) (sixth ore-res))
			  (list (second (data~appl-arguments (node~formula (fourth ore-res))))))) ;;; nAvB hypnA
	 (impe-res ('impe (list nil (fifth ore-res) precond) nil)) ;; B hypA A=>B
	 (orir-res ('orir (list (third ore-res) (first impe-res))
			       (list (first (data~appl-arguments (node~formula (third ore-res)))))))) ;; nAvB B
	(tacl~end)))))


(com~defcommand imp2or
  (argnames implication-line or-line)
  (argtypes ndline ndline)
  (arghelps "A line with an implication" "A line with a disjunction.")
  (function batac=imp2or)
  (frag-cats tactics base-connective)
  (defaults batac=imp2or-defaults)
  (log-p T)
  (help "Rewriting implies as or"))

(defun batac=imp2or (P P2)
  (infer~compute-outline 'imp2or (list P2 P) nil))


(defun batac=imp2or-defaults (imp or)
    (cond ((not (com~specified-arg-p imp))
	   (list (pds~find-support #'logic~implication-p) (com~unspecified)))
	  ((not (com~specified-arg-p or))
	   (list imp
		 (if (and (pdsn~p imp) (logic~implication-p (node~formula imp)))
		     (pds~find-open-node
		      #'(lambda (p)
			  (and (logic~disjunction-p p)
			       (data~equal (batac=imp2or-create-f (node~formula imp)) p))))
		   (com~unspecified))))
		 
	  (t (list imp or))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  indirect  - not elimination alternative
;;
;;   L1 |- not A
;;   Ass,L1 |- false
;; ------------------
;;   Ass |- A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic indirect
		 (outline-mappings (((existent existent) indirect-a)
				    ((existent nonexistent) indirect-b)))
		 (expansion-function batac=expand-indirect)
		 (help "Alternative not-elimination"))


(tac~deftactic indirect-b indirect (in base)
   (hypotheses ((H L2) "H is a hypothesis for L2"))
   (premises L2)
   (conclusions L1)
   (computations (H (batac=compute-negated-formula (formula L1)))
		 (L2 (batac=compute-false )))
   (description "Backward application of alternative not-elimination"))


;;; LC: Volker wird def~tactic um ein Slot hypotheses erweitern
;;; Volker hat tactics um einen slot erweitert!! VS
(tac~deftactic indirect-a indirect (in base)
   (premises P) 
   (conclusions C)
   (hypotheses ((H P) "H is a hypothesis for P"))		
   (sideconditions (batac=negated-formula-hyp-p (hyps P) (formula C)))
   (description "Application of alternative not-elimination"))


(defun batac=compute-negated-formula (form)
  (let ((not (env~lookup-object :not (pds~environment omega*current-proof-plan))))
    (term~appl-create not (list form))))

(defun batac=compute-false ()
  (env~lookup-object :false (pds~environment omega*current-proof-plan)))

(defun batac=negated-formula-hyp-p (hyp-list formula)
  (let ((neg-form (batac=compute-negated-formula formula)))
    (some #'(lambda (hyp-node)
	      (data~equal (node~formula hyp-node)
			  neg-form))
	  hyp-list)))


(defun batac=expand-indirect (outline parameters)
  (let* ((conc (car outline))     ;; A
	 (false (cadr outline))   ;; bot
	 (hyp   (car
		 (set-difference (pdsn~hyps false)
				 (pdsn~hyps conc))))) ;; hypnA
    (tacl~init outline)
    (let ((result
	   (tacl~sequence
	    (notnote-res ('notnote (list conc nil) nil)) ;; A nnA
	    (noti-res ('noti (list (second notnote-res) nil) nil)) ;; nnA bot hypnA1
	    )))
      (tac~forget&destroy-hyp (list (second result)) hyp (third result))
      (tacl~apply 'weaken (list (second result) false) nil))
    (tacl~end)))


(com~defcommand indirect
  (argnames form false)
  (argtypes ndline ndline)
  (arghelps "Conclusion line" "Line with False")
  (function batac=indirect)
  (frag-cats tactics base)
  (defaults batac=indirect-defaults)
  (log-p T)
  (level 6)
  (help "Alternative not-elimination."))

(defun batac=indirect (form false)
  (infer~compute-outline 'indirect (list form false) nil))


(defun batac=indirect-defaults (conc fals)
  (cond ((not (com~specified-arg-p conc))
	 (list (oc~default-current-planline) (com~unspecified)))
	((not (com~specified-arg-p fals))
	 (list conc (pds~find-node-support conc #'oc~falsity-p)))
	(t (list conc fals))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modus Tollens
;;;
;;;
;;;  A => B     not B
;;; ------------------
;;;      not A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(infer~deftactic modtoll
		 (outline-mappings (((nonexistent existent existent) modtoll-f)
                                    ((existent existent existent) modtoll-a)
                                    ((existent existent nonexistent) modtoll-l)
				    ((existent nonexistent existent) modtoll-r)
                                    ))
		 (parameter-types)
		 (expansion-function batac=expand-modtoll)
		 (help "Modus Tollens."))


(tac~deftactic modtoll-f modtoll (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=modtoll-compute-f (formula L1))))
   (sideconditions (batac=modtoll-f-p (formula L1) (formula L2)))
   (description "Forward application of modus tollens."))


(tac~deftactic modtoll-a modtoll (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations)
   (sideconditions (batac=modtoll-a-p (formula L3) (formula L1) (formula L2)))
   (description "Closing modus tollens."))


(defun batac=modtoll-compute-f (neg)
  (when (logic~implication-p neg)
    (batac=compute-negated-formula (car (data~appl-arguments neg)))))


(defun batac=remove-negation (neg)
  (first (data~appl-arguments neg)))

(defun batac=modtoll-f-p (aimpb notb)
  (and (logic~implication-p aimpb)
       (logic~negation-p notb)
       (data~equal (cadr (data~appl-arguments aimpb))
		   (batac=remove-negation notb))))

(defun batac=modtoll-a-p (nota aimpb notb)
  (and (logic~implication-p aimpb)
       (logic~negation-p notb)
       (logic~negation-p nota)
       (data~equal (car (data~appl-arguments aimpb))
		   (batac=remove-negation nota))
       (data~equal (cadr (data~appl-arguments aimpb))
		   (batac=remove-negation notb))))


(tac~deftactic modtoll-l modtoll (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=modtoll-l-create (formula L1))))
   (sideconditions (batac=modtoll-l-p (formula L1) (formula L3)))
   (description "Sideward application (left) of modus tollens."))


(defun batac=modtoll-l-create (aimpb)
  (batac=compute-negated-formula (cadr (data~appl-arguments aimpb))))

(defun batac=modtoll-l-p (aimpb nota)
  (and (logic~implication-p aimpb)
       (logic~negation-p nota)
       (data~equal (car (data~appl-arguments aimpb))
		   (batac=remove-negation nota))))

(tac~deftactic modtoll-r modtoll (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=modtoll-r-create (formula L3) (formula L2))))
   (sideconditions (batac=modtoll-r-p (formula L3) (formula L2)))
   (description "Sideward application (right) of modus tollens."))

(defun batac=modtoll-r-create (nota notb)
  (term~appl-create (env~lookup-object :implies (pds~environment omega*current-proof-plan))
	       (list (batac=remove-negation nota)
		     (batac=remove-negation notb))))

(defun batac=modtoll-r-p (nota notb)
  (and (logic~negation-p nota)
       (logic~negation-p notb)))



(defun batac=expand-modtoll (outline parameters)
  (let ((precond1 (second outline))  ;;  (A=>B) 
	(precond2 (third outline))   ;;  nB
	(conc (first outline)))     ;;   nA
    (tacl~init outline)
    (let* ((contra-res (tacl~apply 'contrapos (list nil precond1) nil))    ;;; (nB=>nA) (A=>B)
	   (left (first (data~appl-arguments (node~formula (first contra-res)))))
	   (right (second (data~appl-arguments (node~formula (first contra-res))))))
      (cond ((and (keim~equal right (node~formula conc))
		  (keim~equal left (node~formula precond2)))
	    (tacl~sequence
	     (impe-res ('impe (list conc precond2 (car contra-res)) nil))))  ;;; nA nB
									   ;;; (nB=>nA)
	   ((and (keim~equal right (node~formula conc))
		(not (keim~equal left (node~formula precond2))))
	    (tacl~sequence
	     (notnote-res ('notnote (list nil precond2) nil))
	     (impe-res ('impe (list conc (car notnote-res) (car contra-res)) nil))))
	   ((and (not (keim~equal right (node~formula conc)))
		 (keim~equal left (node~formula precond2)))
	    (tacl~sequence
	     (notnoti-res ('notnoti (list conc nil) nil))
	     (impe-res ('impe (list (cadr notnoti-res) precond2 (car contra-res)) nil))))
	   (t
	    (tacl~sequence
	     (notnote-res ('notnote (list nil precond2) nil))
	     (notnoti-res ('notnoti (list conc nil) nil))
	     (impe-res ('impe (list (cadr notnoti-res) (car notnote-res) (car contra-res)) nil))))))
    (tacl~end)))



(com~defcommand modtoll
  (argnames aimpb negb nega)
  (argtypes ndline ndline ndline)
  (arghelps "A line with implication" "A line with the negated succedent."
            "A line with the negated antecedent (conclusion).")
  (function batac=modtoll)
  (frag-cats tactics base)
  (defaults batac=modtoll-defaults)
  (log-p T)
  (help "Modus Tollens."))

(defun batac=modtoll-defaults (aimpb negb nega)
    (cond ((not (com~specified-arg-p aimpb))
	   (list (pds~find-support #'logic~implication-p) (com~unspecified) (com~unspecified)))
	  ((not (com~specified-arg-p negb))
	   (list aimpb
		 (if (and (pdsn~p aimpb) (logic~implication-p (node~formula aimpb)))
		     (pds~find-support
		      #'(lambda (p) (and (logic~negation-p p)
					 (data~equal (car (data~appl-arguments p))
						     (cadr (data~appl-arguments (node~formula aimpb)))))))
		   (pds~find-support #'logic~negation-p))
		 (oc~nil-argument)))
	  ((not (com~specified-arg-p nega))
	   (list aimpb
		 negb
		 (if (and (pdsn~p aimpb) (logic~implication-p (node~formula aimpb)))
		     (pds~find-open-node
		      #'(lambda (p)
			  (and (logic~negation-p p)
			       (data~equal (car (data~appl-arguments p))
					   (car (data~appl-arguments (node~formula aimpb)))))))
		   (pds~find-open-node #'logic~negation-p))))
	  (t (list aimpb negb nega))))

(defun batac=modtoll (aimpb negb nega)
  (infer~compute-outline 'modtoll (list nega aimpb negb) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta-expandsion and reduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic beta-normalize
		 (outline-mappings (((nonexistent existent) beta-normalize-f)
                                    ((existent nonexistent) beta-normalize-b)
				    ((existent existent) beta-normalize-a)))
                 (expansion-function batac=expand-beta-normalize)
		 (help "Beta-Reduction"))


(tac~deftactic beta-normalize-f beta-normalize (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (beta~normalize (formula L1))))
   (sideconditions)
   (description "Good old beta-normalization."))

(tac~deftactic beta-normalize-b beta-normalize (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (beta~normalize (formula L2))))
   (sideconditions)
   (description "Good old beta-normalization."))

(tac~deftactic beta-normalize-a beta-normalize (in base)
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (term~alpha-equal (beta~normalize  (formula L1))
				     (beta~normalize  (formula L2))))
   (description "Good old beta-normalization."))


(defun batac=expand-beta-normalize (outline parameters)
  (let ((precond (second outline))  ;;  (A=>B) 
	(conc (first outline)))     ;;   nA
    (tacl~init outline)
    (tacl~sequence
     (lambda-res ('lambda (list conc precond) nil)))     ;;; 
    (tacl~end)))


(com~defcommand beta-normalize
  (argnames line1 line2)
  (argtypes ndline ndline)
  (arghelps "A conclusion line" "A premise line")
  (function batac=beta-normalize)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (level 5)
  (help "Beta-normalizes a line"))

(defun batac=beta-normalize (line1 line2)
  (infer~compute-outline 'beta-normalize (list line1 line2) nil))



(infer~deftactic beta-expand
		 (outline-mappings (((nonexistent existent) beta-expand-f)
                                    ((existent existent) beta-expand-a)))
                 (parameter-types position-list)
                 (expansion-function batac=expand-beta-expand)
		 (help "Beta-Expansion"))


(tac~deftactic beta-expand-f beta-expand (in base)
   (parameters (Poslist cons "A list of positions"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (beta~expand (formula L1) Poslist)))
   (sideconditions)
   (description "Good old beta-expansion."))


(tac~deftactic beta-expand-a beta-expand (in base)
   (parameters (Poslist cons "A list of positions"))
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (lam~equal-p  (formula L1) (formula L2)))
   (description "Good old beta-expansion."))


(defun batac=expand-beta-expand (outline parameters)
  (let ((precond (second outline))  ;;  (A=>B) 
	(conc (first outline)))     ;;   nA
    (tacl~init outline)
    (tacl~sequence
     (lambda-res ('lambda (list conc precond) nil)))     ;;; 
    (tacl~end)))


(com~defcommand beta-expand
  (argnames line1 line2 positions)
  (argtypes ndline ndline position-list)
  (arghelps "A conclusion line" "A premise line" "A list of positions")
  (function batac=beta-expand)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
  (log-p T)
  (level 5)
  (help "Beta-expands a line"))

(defun batac=beta-expand (line1 line2 positions)
  (infer~compute-outline 'beta-expand (list line1 line2) (list positions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contrapos
;;
;;
;;  not A => not B
;; ----------------
;;      B => A
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic contrapos
		 (outline-mappings (((existent existent) contrapos-a)
                                    ((nonexistent existent) contrapos-f)
                                    ((existent nonexistent) contrapos-b)))
                 (expansion-function batac=expand-contrapos)
		 (help "Contrapositum."))

(defun batac=contrapos (line1 line2)
  (infer~compute-outline 'contrapos (list line1 line2) nil))

(com~defcommand contrapos
  (argnames conc line2)
  (argtypes ndline ndline)
  (arghelps "The contrapositum" "the antecedent line")
  (function batac=contrapos)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Contrapositum: A => B |- -B => -A."))


(tac~deftactic contrapos-f contrapos (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=contrapos-f (formula L1))))
   (sideconditions (logic~implication-p (formula L1)))
   (description "Contrapositum in forward direction."))


(defun batac=contrapos-f (l1)
  (let* ((Opp (data~appl-function l1))
         (Env (pds~environment omega*current-proof-plan))
         (Aterm (logic~negate (first (data~appl-arguments l1))))
         (Bterm (logic~negate (second (data~appl-arguments l1)))))
    (term~appl-create Opp (list Bterm Aterm))))

(tac~deftactic contrapos-b contrapos (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=contrapos-f (formula L2))))
   (sideconditions (logic~implication-p (formula L2)))
   (description "Contrapositum in backward direction."))


(tac~deftactic contrapos-a contrapos (in base)
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (batac=contrapos-ap (formula L1) (formula L2)))
   (description "Contrapositum in test direction."))

(defun batac=contrapos-ap (l1 l2)
  (data~equal l1 (batac=contrapos-f l2)))



(defun batac=expand-contrapos (outline parameters)
  (let ((precond (second outline))  ;;  nA => nB
	(conc (first outline)))     ;;   B => A
    (tacl~init outline)
    (cond ((and (logic~negation-p (car (data~appl-arguments (node~formula precond))))
	       (logic~negation-p (cadr (data~appl-arguments (node~formula precond)))))
	   ;; precond :  nA => nB    
	   ;; conc    :   B => A     
	   (tacl~sequence
	    (impi-res ('impi (list conc nil) nil))                     ;;; (B=>A) A hypB
	    (notnote-res  ('notnote (list (second impi-res) nil) nil))     ;;; A nnA
	    (noti-res ('noti (list (second notnote-res) nil) nil))           ;;; nnA bot HypnA
	    (impe-res ('impe (list nil (third noti-res) precond) nil))  ;;; nB HypnA (nA=>nB)
	    (note-res ('note (list  (second noti-res) (third impi-res) (first impe-res)) nil))) ;;; bot hypB nB
	   (tacl~end))
	  ((and (not (logic~negation-p (car (data~appl-arguments (node~formula precond)))))
		(not (logic~negation-p (cadr (data~appl-arguments (node~formula precond))))))
	   ;; precond :   A => B      
	   ;; conc    :  nB => nA     
	   (tacl~sequence
	    (impi-res ('impi (list conc nil) nil))                     ;;; (nB=>nA) nA hypnB
	    (noti-res ('noti (list (second impi-res) nil) nil))           ;;; nA bot HypA
	    (impe-res ('impe (list nil (third noti-res) precond) nil))  ;;; B HypA (A=>B)
	    (note-res ('note (list  (second noti-res) (first impe-res) (third impi-res)) nil))) ;;; bot B hypnB 
	   (tacl~end))
	  ((and (logic~negation-p (car (data~appl-arguments (node~formula precond))))
	        (not (logic~negation-p (cadr (data~appl-arguments (node~formula precond))))))
	   ;;   precond : nA => B
	   ;;   conc    : nB => A
	   (tacl~sequence
	    (impi-res ('impi (list conc nil) nil))                     ;;; (nB=>A) A hypnB
	    (notnote-res  ('notnote (list (second impi-res) nil) nil))     ;;; A nnA
	    (noti-res ('noti (list (second notnote-res) nil) nil))           ;;; nnA bot HypnA
	    (impe-res ('impe (list nil (third noti-res) precond) nil))  ;;; B HypnA (nA=>B)
	    (note-res ('note (list  (second noti-res) (first impe-res) (third impi-res)) nil))) ;;; bot B hypnB
	   (tacl~end))
	  ((and (not (logic~negation-p (car (data~appl-arguments (node~formula precond)))))
		(logic~negation-p (cadr (data~appl-arguments (node~formula precond)))))
	   ;;      precond : A => nB
	   ;;      conc    : B => nA
	   (tacl~sequence
	    (impi-res ('impi (list conc nil) nil))                     ;;; (B=>nA) nA hypB
	    (noti-res ('noti (list (second impi-res) nil) nil))           ;;; nA bot HypA
	    (impe-res ('impe (list nil (third noti-res) precond) nil))  ;;; nB HypA (A=>nB)
	    (note-res ('note (list  (second noti-res) (third impi-res) (first impe-res)) nil))) ;;; bot hypB nB
	   (tacl~end)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; Transitivity of implication: Modus Barbara
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(infer~deftactic modbarbara
		 (outline-mappings (((existent existent existent) modbarbara-a)
                                    ((nonexistent existent existent) modbarbara-f)
				    ((existent nonexistent existent) modbarbara-l)
				    ((existent existent nonexistent) modbarbara-r)))

                 (expansion-function batac=expand-modbarbara)
		 (help "Transitivity of Implication."))


(tac~deftactic modbarbara-f modbarbara (in base)
   (premises L1 L2)
   (conclusions C)
   (computations (C (batac=modbarbara-f (formula L1) (formula L2))))
   (sideconditions (batac=modbarbara-f-p (formula L1) (formula L2)))
   (description "Transitivity of implication in forward direction."))

(defun batac=modbarbara-f-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (data~equal (second (data~appl-arguments form1))
		   (first (data~appl-arguments form2)))))


(defun batac=modbarbara-f (l1 l2)
  (let ((Opp (data~appl-function l1))
        (Aterm (first (data~appl-arguments l1)))
        (Bterm (second (data~appl-arguments l2))))
    ;; removing double negation
    (term~appl-create Opp (list Aterm Bterm))))

(tac~deftactic modbarbara-a modbarbara (in base)
   (premises L1 L2)
   (conclusions C)
   (computations )
   (sideconditions (batac=modbarbara-a-p (formula C) (formula L1) (formula L2)))
   (description "Transitivity of implication."))


(defun batac=modbarbara-a-p (form1 form2 form3)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (logic~implication-p form3)
       (data~equal (second (data~appl-arguments form2))
		   (first (data~appl-arguments form3)))
       (data~equal (batac=modbarbara-f form2 form3)
		   form1)))

(tac~deftactic modbarbara-l modbarbara (in base)
   (premises L1 L2)
   (conclusions C)
   (computations (L1 (batac=modbarbara-l (formula L2) (formula C))))
   (sideconditions (batac=modbarbara-l-p (formula L2) (formula C)))
   (description "Transitivity of implication in left direction."))

(defun batac=modbarbara-l-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (data~equal (second (data~appl-arguments form1))
		   (second (data~appl-arguments form2)))))


(defun batac=modbarbara-l (l2 conc)
  (let ((Opp (data~appl-function l2))
        (Aterm (first (data~appl-arguments conc)))
        (Bterm (first (data~appl-arguments l2))))
      (term~appl-create Opp (list Aterm Bterm))))

(tac~deftactic modbarbara-r modbarbara (in base)
   (premises L1 L2)
   (conclusions C)
   (computations (L2 (batac=modbarbara-r (formula L1) (formula C))))
   (sideconditions (batac=modbarbara-r-p (formula L1) (formula C)))
   (description "Transitivity of implication in right direction."))

(defun batac=modbarbara-r-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (data~equal (first (data~appl-arguments form1))
		   (first (data~appl-arguments form2)))))


(defun batac=modbarbara-r (l1 conc)
  (let ((Opp (data~appl-function l1))
        (Aterm (second (data~appl-arguments l1)))
        (Bterm (second (data~appl-arguments conc))))
      (term~appl-create Opp (list Aterm Bterm))))


(defun batac=expand-modbarbara (outline parameters)
  (let ((p1 (second outline))
	(p2 (third outline))
	(conc (first outline)))
    (tacl~init outline)
    (tacl~sequence
     (impi-res ('impi (list conc nil) nil))                ;;; conc  C hypA
     (BB ('impe (list nil (third impi-res) p1) nil))      ;;; B p1 hypA
     (CC ('impe (list (second impi-res) (car BB) p2) nil))) ;;; C p2 B
    (tacl~end)))

(defun batac=modbarbara (line1 line2 line3)
  (infer~compute-outline 'modbarbara (list line1 line2 line3) nil))

(com~defcommand modbarbara
  (argnames conc line2 line3)
  (argtypes ndline ndline ndline)
  (arghelps "An implication line"
	    "Another one with the same antecedent"
	    "One with the same succedent as the first")
  (function batac=modbarbara)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Transitivity of implication: modus barbara."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; or rewrite as implies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic or2imp
		 (outline-mappings (((existent nonexistent) or2imp-b)
				    ((nonexistent existent) or2imp-f)
				    ((existent existent) or2imp-a)))
		 (parameter-types)
		 (expansion-function batac=expand-or2imp)
		 (help "Rewriting or as implies."))

(tac~deftactic or2imp-a or2imp (in base)
   (premises (L2 "A implication node"))
   (conclusions (L1 "A disjunction node."))
   (computations )
   (sideconditions (batac=or2imp-a-p (formula L1) (formula L2)))
   (description "or2imp application."))

(defun batac=or2imp-a-p (imp-term or-term)
  (and (logic~implication-p imp-term)
       (logic~disjunction-p or-term)
       (term~alpha-equal (cadr (data~appl-arguments or-term)) (cadr (data~appl-arguments
								     imp-term)))
       (term~alpha-equal (logic~negate (car (data~appl-arguments or-term)))
			 (car (data~appl-arguments imp-term)))))


(tac~deftactic or2imp-b or2imp (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=or2imp-create-b (formula L2))))
   (sideconditions (batac=or2imp-b-p (formula L2)))
   (description "or2imp backward."))

(defun batac=or2imp-b-p (imp-term)
  (logic~implication-p imp-term))

(defun batac=or2imp-create-b (imp-term)
  (term~appl-create
   (env~lookup-object :or (pds~environment omega*current-proof-plan))
   (list (logic~negate
          (first (data~appl-arguments imp-term)))
         (second (data~appl-arguments imp-term)))))


(tac~deftactic or2imp-f or2imp (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=or2imp-create-f (formula L1))))
   (sideconditions (batac=or2imp-f-p (formula L1)))
   (description "or2imp forward."))


(defun batac=or2imp-create-f (or-term)
  (term~appl-create
   (env~lookup-object :implies (pds~environment omega*current-proof-plan))
   (list (logic~negate
          (first (data~appl-arguments or-term)))
         (second (data~appl-arguments or-term)))))

(defun batac=or2imp-f-p (or-term)
  (logic~disjunction-p or-term))


(defun batac=expand-or2imp (outline parameters)
  (let* ((conc (car outline)) 
	 (precond (cadr outline))) 
    (tacl~init outline)
    (if (term~alpha-equal (term~appl-create (env~lookup-object :not (pds~environment omega*current-proof-plan))
					    (list (car (data~appl-arguments (node~formula conc)))))
			  (car (data~appl-arguments (node~formula precond))))
	  ;(logic~negation-p (car (data~appl-arguments (node~formula precond)))) MP
	  ;; precond: nAvB          or    precond: nAvnB
	  ;; conc:    A=>B                conc:    A=>nB
	(progn
	  (tacl~sequence
	   (impi-res ('impi (list conc nil) nil)) ;; A=>B B hypA
	   (ore-res  ('ore (list (second impi-res) precond nil nil) nil)) ;; B nAvB B B hypnA hypB
	   (weaken-res ('weaken (list (fourth ore-res) (sixth ore-res)) nil)) ;; B hypB
	   (note-res ('note (list nil (third impi-res) (fifth ore-res)) nil)) ;; bot hypA hypnA
	   (falsee-res ('falsee (list (third ore-res) (first note-res)) nil))) ;; B bot
	  (tacl~end))
      	  ;; precond: AvB          or    precond: AvnB
	  ;; conc:    nA=>B                conc:  nA=>nB
      (progn
	(tacl~sequence
	 (impi-res ('impi (list conc nil) nil)) ;; nA=>B B hypnA
	 (ore-res  ('ore (list (second impi-res) precond nil nil) nil)) ;; B AvB B B hypA hypB
	 (weaken-res ('weaken (list (fourth ore-res) (sixth ore-res)) nil)) ;; B hypB
	 (note-res ('note (list nil (fifth ore-res) (third impi-res)) nil)) ;; bot hypA hypnA
	 (falsee-res ('falsee (list (third ore-res) (first note-res)) nil)));; B bot
	(tacl~end)))))

(com~defcommand or2imp
  (argnames or-line implication-line)
  (argtypes ndline ndline)
  (arghelps "A line with a disjunction." "Another line with an implication")
  (function batac=or2imp)
  (frag-cats tactics base-connective)
  (defaults ((com~unspecified) (oc~default-current-planline))) 
  (log-p T)
  (help "Rewriting or as implies"))

(defun batac=or2imp (P P2)
  (infer~compute-outline 'or2imp (list P2 P) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     Tactics for Extensionality and Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Equivalence Substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic equivsubst
		 (outline-mappings (((nonexistent existent existent) equivsubst-f)
                                    ((existent existent existent) equivsubst-a)
				    ((existent existent nonexistent) equivsubst-l)
				    ((existent nonexistent existent) equivsubst-r)))
		 (parameter-types position)
		 (expansion-function batac=expand-equivsubst)
		 (help "Replacement property of equivalence."))


(tac~deftactic equivsubst-f equivsubst (in base)
   (parameters (position pos+position "A position."))
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=equivsubst-create-f
                      (formula L1) (formula L2) position)))
   (sideconditions (batac=equivsubst-f-p
		    (formula L1) (formula L2) position))
   (description "Forward application equivsubstitution."))


(tac~deftactic equivsubst-a equivsubst (in base)
   (parameters (position pos+position "A position"))
   (premises L1 L2)
   (conclusions L3)
   (computations)
   (sideconditions (batac=equivsubst-a-p
                    (formula L3) (formula L1) (formula L2) position))
   (description "Closing equivalence substitution."))


(defun batac=equivsubst-f-p (term equiv-term position)
  (and (logic~equivalence-p equiv-term)
       (type~o-p (term~type (data~struct-at-position term position)))
       (let* ((arg1 (first (data~appl-arguments equiv-term)))
	      (arg2 (second (data~appl-arguments equiv-term))))
	 (let* ((positions-of-arg1 (data~substruct-positions arg1 term :test 'data~equal))
		(positions-of-arg2 (data~substruct-positions arg2 term :test 'data~equal)))
	   (or (member position positions-of-arg1 :test 'keim~equal)
	       (member position positions-of-arg2 :test 'keim~equal))))))


(defun batac=equivsubst-create-f (term equiv-term position)
  (let* ((term-at-position (data~struct-at-position term position))
	 (args (data~appl-arguments equiv-term)))
    (cond ((data~equal term-at-position (first args))
	   (data~replace-at-position term position (second args)))
	  (t
	   (data~replace-at-position term position (first args))))))
	  

(defun batac=equivsubst-a-p (conclusion term equiv-term position)
  (and (batac=equivsubst-f-p term equiv-term position)
       (data~equal (batac=equivsubst-create-f term equiv-term position) conclusion)))

(tac~deftactic equivsubst-l equivsubst (in base)
   (parameters (position pos+position "A position"))
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=equivsubst-create-l (formula L3) (formula L1) position))) 
   (sideconditions (batac=equivsubst-l-p (formula L3) (formula L1) position))
   (description "Creating equivalence for a substitution."))



(tac~deftactic equivsubst-r equivsubst (in base)
   (parameters (position pos+position "A position"))
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=equivsubst-create-f (formula L3) (formula L2) position))) 
   (sideconditions (batac=equivsubst-f-p
                    (formula L3) (formula L2) position))
   (description "Creating premise for a substitution."))


(defun batac=equivsubst-l-p (conclusion term position)
  (let ((positions-of-conc (data~positions conclusion #'(lambda (arg) 't)))
	(positions-of-term (data~positions term #'(lambda (arg) 't))))
    (when (and (find position positions-of-conc :test 'keim~equal)
	       (find position positions-of-term :test 'keim~equal))
      (data~equal-p (data~replace-at-position conclusion
					     position
					     (data~struct-at-position term position))
		    term))))

(defun batac=equivsubst-create-l (conclusion term position)
  (term~appl-create (env~lookup-object :equiv (pds~environment omega*current-proof-plan))
	       (list (data~struct-at-position conclusion position)
		     (data~struct-at-position term position))))


(defun batac=expand-equivsubst (outline parameters)
    (let* ((precond (second outline))
	   (equivalence (third outline))
	   (conc (first outline))
	   (pos (first parameters)))
      (tacl~init outline)
      (tacl~sequence
       (equiv2=-res ('equiv2= (list nil equivalence) (list (pos~empty)))) ;;; a=b a<=>b
       (=subst-res ('=subst (list conc precond (first equiv2=-res)) (list pos))))
      (tacl~end)))


(com~defcommand equivsubst
  (argnames line1 line2 equivalence-line position)
  (argtypes ndline ndline ndline position)
  (arghelps "The substituted line" "The unsubstituted line"
            "The equivalence to be applied." "A position.")
  (function batac=equivsubst)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Equivalence-Subsitution."))

(defun batac=equivsubst (P P2 P3 position)
  (infer~compute-outline 'equivsubst (list P P2 P3) (list position)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; =2equiv
;;;
;;;   P[a=b]
;;;   ---------
;;;   P[a<=>b]                     P refers to a position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic =2equiv
		 (outline-mappings (((nonexistent existent) =2equiv-f)
				    ((existent nonexistent) =2equiv-b)
				    ((existent existent) =2equiv-a)))
		 (parameter-types position)
		 (expansion-function batac=expand-=2equiv)
		 (help "Equivalence of equality and equivalence on truth values."))

(tac~deftactic =2equiv-f =2equiv (in base)
   (parameters (pos pos+position "position of the equation"))	       
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=equality-2-equivalence (formula L1) pos)))
   (sideconditions (batac=equality-at-pos-p (formula L1) pos))
   (description "Forward application of Equality to Equivalence"))

(defun batac=equality-at-pos-p (term pos)
   (logic~equality-p (data~struct-at-position term pos)))

(tac~deftactic =2equiv-b =2equiv (in base)
   (parameters (pos pos+position "position of equation"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=equivalence-2-equality (formula L2) pos)))
   (sideconditions (batac=equivalence-at-pos-p (formula L2) pos))
   (description "Backward application of Equality to Equivalence"))

(defun batac=equivalence-at-pos-p (term pos)
   (logic~equivalence-p (data~struct-at-position term pos)))

(tac~deftactic =2equiv-a =2equiv (in base)
   (parameters (pos pos+position "position of equation")) 	       
   (premises (L1 "An equivalence"))
   (conclusions (L2 "An equality"))
   (computations )
   (sideconditions (batac=equivalence-at-pos-p (formula L2) pos)
		   (batac=equality-at-pos-p (formula L1) pos)
		   (batac==2equiv-applicable-p (formula L1) (formula L2) pos))
   (description "Backward application of Equality to Equivalence"))



(defun batac==2equiv-applicable-p (F1 F2 pos)
  (term~alpha-equal F2 (data~replace-at-position F1 pos (DATA~STRUCT-AT-POSITION F2 pos))))


(defun batac=equivalence-2-equality (F pos)
  (let* ((args (data~appl-arguments (data~struct-at-position F pos)))
	 (equality (term~appl-create (env~lookup-object := (pds~environment omega*current-proof-plan))
				     args)))
    (data~replace-at-position F pos equality)))


(defun batac=equality-2-equivalence (F pos)
  (let* ((args (data~appl-arguments (DATA~STRUCT-AT-POSITION F pos)))
	 (equivalence (term~appl-create (env~lookup-object :equiv (pds~environment omega*current-proof-plan))
				   args)))
    (data~replace-at-position F pos equivalence)))

	
(defun batac=expand-=2equiv (outline parameters)
  (if (or (null (car parameters))
	  (pos~empty-p (car parameters)));; top position-case
      (let* ((=def (th~find-assumption "=" (prob~theory omega*current-proof-plan)))
	     (definiendum (th~definition-constant =def))
	     (definiens (data~copy (th~ass-node =def) :downto '(term+constant
								type+primitive)))
	     (conc (first outline))
	     (prem (second outline))
	     (pos (third parameters))	 
	     (lam-x (post~read-object '(lam (x o) x) (pds~environment omega*current-proof-plan) :existing-term))
	     (not-lam-x (post~read-object '(lam (x o) (not x)) (pds~environment omega*current-proof-plan) :existing-term))
	     )
	(tacl~init outline)
	(tacl~sequence
	 ((leibn) ('defne (list nil prem) (list definiendum definiens (pos~add-end 0 pos))))
	 (norm ('foralle (list nil leibn) (list lam-x)))
	 (norm2 ('beta-normalize (list nil (car norm)) nil))
	 (neg ('foralle (list nil leibn) (list not-lam-x)))
	 (neg2 ('beta-normalize (list nil (car neg)) nil))
	 (contr-res ('contrapos (list nil (car neg2)) nil))
	 (end ('equivi (list conc (car norm2) (car contr-res)) nil))
	 )
	(tacl~end))
    (progn
      (tacl~init outline)
      (tacl~sequence
       (=subst-res ('=subst (list (car outline) (cadr outline) nil) parameters)) ;P(a=b) P(a<=>b) (a=b = a<=>b)
       (equiv2=-res ('equiv2= (list (third =subst-res) nil) (list (pos~empty)))) ;(a=b = a<=>b) (a=b <=> a<=>b)
       (equivi-res ('equivi (list (second equiv2=-res) nil nil) nil)) ; (a=b <=> a<=>b)  (a=b => a<=>b) (a<=>b => a=b)
       (impi1-res ('impi (list (third equivi-res) nil) nil)) ; (a=b => a<=>b) (a<=>b) [a=b]
       (=2equiv-res ('=2equiv (list (second impi1-res) (third impi1-res)) (list (pos~empty)))) ; (a<=>b) [a=b]
       (impi2-res ('impi (list (second equivi-res) nil) nil)) ; (a<=>b => a=b)  (a=b) [a<=>b]
       (equiv2=-res ('equiv2= (list (second impi2-res) (third impi2-res)) (list (pos~empty))))) ; (a=b) [a<=>b] 
      (tacl~end))))


(com~defcommand =2equiv
  (argnames equality equivalence position)
  (argtypes ndline ndline position)
  (arghelps "A line with equality" "A line with equivalence"
	    "The position of the embedded equality or equivalence")
  (function batac==2equiv)
  (frag-cats tactics base)
  (log-p T)
  (help "Transforms equality on truth-values into equivalence of truth-values."))

(defun batac==2equiv (equality-line equivalence-line position)
  (infer~compute-outline '=2equiv (list equivalence-line equality-line) (list position)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; equiv2=
;;;
;;;    P(a<=>b)
;;;   --------- equiv2=
;;;     P(a=b)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic equiv2=
		 (outline-mappings (((nonexistent existent) equiv2=-f)
				    ((existent nonexistent) equiv2=-b)
				    ((existent existent) equiv2=-a)))
		 (parameter-types position)
		 (expansion-function batac=expand-equiv2=)
		 (help "Equivalence of equality and equivalence on truth values."))

(tac~deftactic equiv2=-f equiv2= (in base)
   (parameters (pos pos+position "position of equivalence"))	       
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=equivalence-2-equality (formula L1) pos)))
   (sideconditions (batac=equivalence-at-pos-p (formula L1) pos))
   (description "Forward application of Equality to Equivalence"))

(tac~deftactic equiv2=-b equiv2= (in base)
   (parameters (pos pos+position "position of equivalence"))	       
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=equality-2-equivalence (formula L2) pos)))
   (sideconditions (batac=equality-at-pos-p (formula L2) pos))
   (description "Backward application of Equality to Equivalence"))

(tac~deftactic equiv2=-a equiv2= (in base)
   (parameters (pos pos+position "position of equivalence"))	       
   (premises (L1 "An equivalence"))
   (conclusions (L2 "An equality"))
   (computations )
   (sideconditions (batac=equivalence-at-pos-p (formula L1) pos)
		   (batac=equality-at-pos-p (formula L2) pos)
		   (batac=equiv2=-applicable-p (formula L2) (formula L1) pos))
   (description "Backward application of Equality to Equivalence"))

(defun batac=equiv2=-applicable-p (F1 F2 pos)
  (term~alpha-equal F2 (data~replace-at-position F1 pos (DATA~STRUCT-AT-POSITION F2 pos))))
	
(defun batac=expand-equiv2= (outline parameters)
  (if (or (null (car parameters))
	  (pos~empty-p (car parameters)))    ;; top position-case
      (let* ((line (tacl~insert&return-assumption 'base 'ext-bool))
	     (prem (cadr outline))
	     (conc (car outline))
	     (term1 (car (data~appl-arguments (node~formula prem))))
	     (term2 (cadr (data~appl-arguments (node~formula prem)))))
	(tacl~init outline)
	(tacl~sequence
	 (foralle*-res ('foralle* (list nil line) (list  (list term1 term2))))
	 (impe-res ('impe (list conc prem (car foralle*-res)) nil)))
	(tacl~end))
    (progn
      (tacl~init outline)
      (tacl~sequence
       (=subst-res ('=subst (list (car outline) (cadr outline) nil) parameters)) ;P(a<=>b) P(a=b) (a<=>b = a=b)
       (equiv2=-res ('equiv2= (list (third =subst-res) nil) (list (pos~empty)))) ;(a<=>b = a=b) (a<=>b <=> a=b)
       (equivi-res ('equivi (list (second equiv2=-res) nil nil) nil)) ;(a<=>b <=> a=b)   (a<=>b => a=b) (a=b => a<=>b) 
       (impi1-res ('impi (list (second equivi-res) nil) nil)) ; (a=b => a<=>b) (a<=>b) [a=b]
       (=2equiv-res ('=2equiv (list (second impi1-res) (third impi1-res)) (list (pos~empty)))) ; (a<=>b) [a=b]
       (impi2-res ('impi (list (third equivi-res) nil) nil)) ; (a<=>b => a=b)  (a=b) [a<=>b]
       (equiv2=-res ('equiv2= (list (second impi2-res) (third impi2-res)) (list (pos~empty))))) ; (a=b) [a<=>b] 
      (tacl~end))))
    

(com~defcommand equiv2=
  (argnames equivalence equality pos)
  (argtypes  ndline ndline position)
  (arghelps  "A line with equivalence" "A line with equality" "position of equivalence")
  (function batac=equiv2=)
  (frag-cats tactics base)
  (log-p T)
  (help "Transforms equality on truth-values into equivalence of truth-values."))

(defun batac=equiv2= (equality-line equivalence-line pos)
  (infer~compute-outline 'equiv2= (list equivalence-line equality-line) (list pos)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LR2equiv
;;;
;;;   a
;;;   b
;;; ------
;;;  a<=>b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(infer~deftactic LR2equiv
		 (outline-mappings (((nonexistent existent existent) LR2equiv-f)
				    ((existent nonexistent nonexistent) LR2equiv-b)
				    ((existent existent nonexistent) LR2equiv-l)
				    ((existent nonexistent existent) LR2equiv-r)
				    ((existent existent existent) LR2equiv-a)))
		 (expansion-function batac=expand-LR2equiv)
		 (help "Two formulae are valid if their truth valuse are equivalent."))


(tac~deftactic LR2equiv-f LR2equiv (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=compute-LR2equiv (formula L1) (formula L2))))
   (sideconditions )
   (description "Forward application of LR to Equivalence"))

(tac~deftactic LR2equiv-l LR2equiv (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=compute-equiv2R (formula L3))))
   (sideconditions (batac=left-equiv-term-p (formula L3) (formula L1)))
   (description "Forward application of LR to Equivalence"))

(tac~deftactic LR2equiv-r LR2equiv (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=compute-equiv2L (formula L3))))
   (sideconditions (batac=right-equiv-term-p (formula L3) (formula L2)))
   (description "Forward application of LR to Equivalence"))

(tac~deftactic LR2equiv-b LR2equiv (in base)
   (premises L1 L2)
   (conclusions L3)
   (computations (L1 (batac=compute-equiv2L (formula L3)))
		 (L2 (batac=compute-equiv2R (formula L3))))
   (sideconditions (logic~equivalence-p (formula L3)))
   (description "Backward application of LR to Equivalence"))

(tac~deftactic LR2equiv-a LR2equiv (in base)
   (premises (L1 "The left subterm") (L2 "The right subterm"))
   (conclusions (L3 "An equivalence"))
   (computations )
   (sideconditions (logic~equivalence-p (formula L3))
		   (batac=LR2equiv-applicable-p (formula L3) (formula L1) (formula L2)))
   (description "Backward application of Equality to Equivalence"))

(defun batac=LR2equiv-applicable-p (F0 F1 F2)
  (and (data~equal (car (data~appl-arguments F0)) F1)
       (data~equal (cadr (data~appl-arguments F0)) F2)))

(defun batac=right-equiv-term-p (Equ Rt)
  (data~equal (cadr (data~appl-arguments Equ)) Rt))

(defun batac=left-equiv-term-p (Equ Lt)
  (data~equal (car (data~appl-arguments Equ)) Lt))

(defun batac=compute-equiv2L (F)
  (car (data~appl-arguments F)))

(defun batac=compute-equiv2R (F)
  (cadr (data~appl-arguments F)))

(defun batac=compute-LR2equiv (LT RT)
  (term~appl-create (env~lookup-object :equiv (pds~environment omega*current-proof-plan))
	       (list LT RT)))
	
(defun batac=expand-LR2equiv (outline parameters)
  (declare (ignore parameters))
  (let* ((lterm (node~formula (second outline)))
	 (rterm (node~formula (third outline)))
	 (neg-lterm (logic~negate lterm))
	 (neg-rterm (logic~negate rterm)))
    (tacl~init outline)
    (tacl~sequence
     (left-disj ('orir (list nil (second outline)) (list neg-rterm)))
     (right-disj ('orir (list nil (third outline)) (list neg-lterm)))
     (left-impl ('or2imp (list nil (car left-disj)) nil))
     (right-impl ('or2imp (list nil (car right-disj)) nil))
     (equiv-res ('equivi (list (car outline) (car right-impl) (car left-impl)) nil)))
    (tacl~end)))
    

(com~defcommand LR2equiv
  (argnames Leftterm rightterm equivalence)
  (argtypes ndline ndline ndline)
  (arghelps "A line with leftside of equivalence" "A line with rightside of equivalence"
	    "A line with equivalence")
  (function batac=LR2equiv)
  (frag-cats tactics base)
  (log-p T)
  (help "Transforms equality on truth-values into equivalence of truth-values."))

(defun batac=LR2equiv (L R E)
  (infer~compute-outline 'LR2equiv (list E L R) nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ti introduction of true
;;;
;;;    
;;;   ------ trueI
;;;      t
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic trueI
		 (outline-mappings (((nonexistent) trueI-f)
                                    ((existent) trueI-a)
				    ))
		 (parameter-types)
     		 (expansion-function batac=expand-trueI)
		 (help "True introduction."))


(tac~deftactic trueI-f trueI (in base)
   (conclusions L1)
   (computations (L1 (batac=trueI-create)))
   (description "Forward application True introduction."))


(tac~deftactic trueI-a trueI (in base)
   (conclusions L1)
   (sideconditions (batac=trueI-a-p (formula L1)))
   (description "Closing True."))

(defun batac=trueI-create ()
  (env~lookup-object :true (pds~environment omega*current-proof-plan)))

(defun batac=trueI-a-p (term)
  (data~equal term (batac=trueI-create)))

(defun batac=expand-trueI (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (car outline))
	 (true (node~formula conc))
	 (line1 (tacl~insert&return-assumption 'base 'tertium-non-datur))
	 (ft-def (th~find-assumption "true" (prob~theory omega*current-proof-plan)))
	 (definiendum (th~definition-constant ft-def))
	 (definiens (data~copy (th~ass-node ft-def) :downto '(term+constant type+primitive)))
	 )

    (tacl~init outline)
    (tacl~sequence
     (forall-res ('foralle (list nil line1) (list true)))
     ((res1 res2 res3 res4 res5 res6) ('ore (list conc (car forall-res) nil nil) nil))
     (weaken1 ('weaken (list res3 res5) nil))
     (defn-res ('defne (list nil res6) (list definiendum definiens (pos~list-position '(1)))))
     (notnote-res ('notnote (list nil (car defn-res)) nil))
     (false-res ('falsee (list res4 (car notnote-res)) nil)))
     (tacl~end)))


(com~defcommand trueI
  (argnames trueline)
  (argtypes ndline)
  (arghelps "An open line containing True.")
  (function batac=trueI)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline)))
  (log-p T)
  (level 7)
  (help "True introduction"))

(defun batac=trueI (C)
  (infer~compute-outline 'trueI (list C) nil))




;; Idempotence of OR

(infer~deftactic idemor
		 (outline-mappings (((nonexistent existent) idemor-f)
				    ((existent nonexistent) idemor-b)
				    ((existent existent) idemor-a)))
		 (expansion-function batac=expand-idemor)
		 (help "Merge a formula (a or a) to (a)."))

(defun batac=expand-idemor (outline parameters)
  (tacl~init outline)
  (tacl~sequence 
   ((merged-formula false-line hyp-line) ('indirect (list (first outline) nil) nil))
   ((false-line or-line false1-line false2-line case1-line case2-line) ('ore (list false-line (second outline) nil nil) nil))
   (dummy1 ('note (list false1-line case1-line hyp-line) nil))
   (dummy2 ('note (list false2-line case2-line hyp-line) nil)))
  (tacl~end))




(tac~deftactic idemor-f idemor (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=compute-idemor-f (formula L1))))
   (sideconditions (batac=idemor-f-p (formula L1)))
   (description "Merge a formula (a or a) to (a)."))

(tac~deftactic idemor-b idemor (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=compute-idemor-b (formula L2))))
   (sideconditions )
   (description "From a formula a to (a or a)"))

(tac~deftactic idemor-a idemor (in base)
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (batac=idemor-a-p (formula L2) (formula L1)))
   (description "From a formula a to (a or a)"))

(defun batac=idemor-f-p (formula)
  (declare (edited  "10-SEP-1997")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T If formula is a disjunction and the both disjunction sides are data~equal."))
  (and (logic~disjunction-p formula)
       (data~equal (first (data~appl-arguments formula)) (second (data~appl-arguments formula)))))

(defun batac=idemor-a-p (formula1 formula2)
  (declare (edited  "10-SEP-1997")
	   (authors Ameier)
	   (input   "Two formulas.")
	   (effect  "None.")
	   (value   "T if the first formula is the result of a idemor on the second formula."))
  (and (batac=idemor-f-p formula2)
       (data~equal formula1 (batac=compute-idemor-f formula2))))

(defun batac=compute-idemor-f (formula)
  (declare (edited  "10-SEP-1997")
	   (authors Ameier)
	   (input   "A formula of the form (or a a)")
	   (effect  "None.")
	   (value   "A formula of the form a."))
  (car (data~appl-arguments formula)))

(defun batac=compute-idemor-b (formula)
  (declare (edited  "10-SEP-1997")
	   (authors Ameier)
	   (input   "A formula a.")
	   (effect  "None.")
	   (value   "A formula of the form (or a a)"))
  (let ((or (env~lookup-object :or (pds~environment omega*current-proof-plan))))
    (term~appl-create or (list formula (data~copy formula)))))

(com~defcommand idemor
  (argnames disjunction merge)
  (argtypes ndline ndline)
  (arghelps "Disjunction to merge" "Merged formula")
  (function batac=idemor)
  (defaults batac=idemor-defaults)
  (frag-cats tactics base)
  (log-p T)
  (help "Merge a disjunction of the form (or a a) to (a)."))

(defun batac=idemor (disjunction merge)
  (infer~compute-outline 'idemor (list merge disjunction) nil))

(defun batac=idemor-defaults (disj merge)
  (cond ((not (com~specified-arg-p disj))
	 (list (pds~find-support #'logic~disjunction-p) (com~unspecified)))
	((not (com~specified-arg-p merge))
	 (list disj
	       (if (and (pdsn~p disj)
			(logic~disjunction-p (node~formula disj))
			(data~equal (first (data~appl-arguments (node~formula disj)))
				    (second (data~appl-arguments (node~formula disj)))))
		   (pds~find-open-node
		    #'(lambda (p)
			(data~equal p (car (data~appl-arguments (node~formula disj))))))
		 (oc~default-current-planline))))
	(t
	 (list disj merge))))



;;
;; Simplifiers
;;

(infer~deftactic "Simplify"
		 (outline-mappings (((nonexistent existent) simplify-f)
                                    ((existent existent) simplify-a)))
		 (expansion-function batac=expand-simplify)
		 (help "Elemination of Multiple Definitions"))

(tac~deftactic simplify-f Simplify (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=simplify-term (formula L1))))
   (sideconditions)
   (description "Forward application of definition elemination."))

(tac~deftactic simplify-a Simplify (in base)
   (premises (L2 "a term with defined symbols"))
   (conclusions (L1 "a term with all definitions removed"))
   (computations) 
   (sideconditions (batac=simplified-p (formula L1) (formula L2)))
   (description "Check definition expansion."))


(com~defcommand simplify
  (argnames concl line2)
  (argtypes ndline ndline)
  (arghelps  "Simplified line" "Line with simplifiable artefacts")
  (function batac=simplify)
  (frag-cats tactics base)
  (defaults batac=simplify-defaults)
  (log-p T)
  (level 5)
  (help "Simplifies."))

(defun batac=simplify-defaults (conc prec)
  (cond ((not (com~specified-arg-p conc))
	 (list nil (pds~find-support #'(lambda (x) x))))
	((not (com~specified-arg-p prec))
	 (if (null conc)
	     (list conc (pds~find-support #'(lambda (x) x)))
	   (list conc
		 (pds~find-node-support
		  conc
		  #'(lambda (p)
		      (batac=simplified-p (node~formula conc)
					  p))))))
	 (t (list conc prec))))


(defun batac=simplify (concl line2)
  (infer~compute-outline 'simplify (list concl line2) nil))

(defun batac=expand-simplify (outline parameters)
  (declare (ignore parameters))
  (let ((conc (first outline))
	(prec-line (cdr outline))
	(prec (second outline)))
    (if (term~alpha-equal (node~formula conc) (node~formula prec))
	
	(progn
	  (tacl~init outline)
	  (tacl~apply 'weaken (list conc prec) nil)
	  (tacl~end))
      
      (multiple-value-bind
	  (conc-eq tlist)
	  (batac=simplify-term (node~formula prec))
	(declare (ignore conq-eq))
	(tacl~init outline)
	(do* ((restlist tlist (cdr restlist))
	      (simplifier (caar restlist) (caar restlist))
	      (pos (cdar restlist) (cdar restlist)))
	    ((null restlist) t)
	  (let* ((assuname (keim~name (th~simplifier-assumption simplifier)))
		 (theory (th~ass-theory
			  (th~simplifier-assumption simplifier)))
		 (assuline (tacl~insert&return-assumption theory assuname))
		 (simplantecedent (th~simplifier-antecedent simplifier)))

	    (let* ((subst (term~alpha-match simplantecedent
					    (data~struct-at-position (node~formula (car prec-line)) pos)))
		   (bindable-type-vars (keim::th~simplifier-bindable-type-vars simplifier))
		   (substed-bindable-type-vars (mapcar #'(lambda (var)
							   (subst~apply subst var))
						       bindable-type-vars))
		   (grounded-assuline (if bindable-type-vars
					  (first (tacl~apply 'kappae (list nil assuline) (list substed-bindable-type-vars)))
					assuline))
		   (grounded-assuformula (node~formula grounded-assuline)))
	      
	      (multiple-value-bind
		  (term assuboundvars) 
		  (logic~remove-leading-universal-quantors grounded-assuformula)
		(declare (ignore term))
		(let* ((assuinstances (mapcar #'(lambda (var)
						  (subst~apply subst var))
					      assuboundvars)))
		  (setf prec-line
			(if (null (rest restlist))
			    (let ((eqline (if assuboundvars
					      (first (tacl~apply 'foralle* (list nil grounded-assuline) (list assuinstances)))
					    grounded-assuline)))
			      (tacl~apply '=subst (list conc (car prec-line) eqline) (list pos)))
			  (let ((eqline (if assuboundvars
					    (first (tacl~apply 'foralle* (list nil grounded-assuline) (list assuinstances)))
					  grounded-assuline)))
			    (tacl~apply '=subst (list nil (car prec-line) eqline) (list pos)))))))))
	  (tacl~end))))))


(defun batac=simplify-term (f)
  (let* ((defs (th~simplifiers-recursed
                (prob~theory omega*current-proof-plan))))
    (multiple-value-bind (fnew positions)
        (rewrite~preorder
         f
         ;; predicates
         (mapcar #'(lambda (simpi)
                     #'(lambda (term)
			 (let* ((subst (term~alpha-match (th~simplifier-antecedent simpi) term)))
			   subst)))
		 defs)
         ;; names (we use definitions themselves)
         defs
         ;; transformations
         (mapcar #'(lambda (simpi)
                     #'(lambda (term)
			 (let* ((subst (term~alpha-match (th~simplifier-antecedent simpi) term)))
			   (subst~apply subst (th~simplifier-consequent simpi)))))
                 defs))
      (values (beta~normalize fnew) positions))))


(defun batac=simplified-p (f1 f2)
  (data~equal f1 (batac=simplify-term f2)))

(infer~deftactic "simplify-goal"
		 (outline-mappings (((existent nonexistent) simplify-goal-b)
                                    ((existent existent) simplify-goal-a)))
		 (expansion-function batac=expand-sImplify-goal)
		 (help "Elemination of Simplifiables"))


(com~defcommand simplify-goal
  (argnames concl line2)
  (argtypes ndline ndline)
  (arghelps  "Line with simplififables" "Simplified line")
  (function batac=simplify-goal)
  (frag-cats tactics base)
  (defaults batac=simplify-goal-defaults)
  (log-p T)
  (level 5)
  (help "Simplifies the conclusion line."))



(defun batac=simplify-goal-defaults (conc prec)
  (cond ((not (com~specified-arg-p conc))
	 (list (oc~default-current-planline) nil))
	((not (com~specified-arg-p prec))
	 (if (null conc)
	     (list conc nil)
	   (list conc
		 (pds~find-node-support
		  conc
		  #'(lambda (p)
		      (batac=simplified-p p (node~formula conc)))))))
	 (t (list conc prec))))

(defun batac=simplify-goal (concl line2)
  (infer~compute-outline 'simplify-goal (list concl line2) nil))


(tac~deftactic simplify-goal-a simplify-goal (in base)
   (premises (L2 "a term with simplifiables"))
   (conclusions (L1 "a simplified term"))
   (computations) 
   (sideconditions (batac=simplified-p (formula L2) (formula L1)))
   (description "Check definition expansion."))

(tac~deftactic simplify-goal-b simplify-goal (in base)
   (premises L2)
   (conclusions L1)
   (computations (L2 (batac=simplify-term (formula L1))))
   (sideconditions)
   (description "Application of simplification on the conclusion line."))


(defun batac=expand-simplify-goal (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (conc-line (list nil conc))
	 (prec (second outline)))
    (if (term~alpha-equal (node~formula conc) (node~formula prec))
	
	(progn
	  (tacl~init outline)
	  (tacl~apply 'weaken (list conc prec) nil)
	  (tacl~end))
      
      (multiple-value-bind
	  (prec-eq tlist) (batac=simplify-term (node~formula conc))
	(declare (ignore prec-eq))
	(tacl~init outline)
	(do* ((restlist tlist (cdr restlist))
	      (simplifier (caar restlist) (caar restlist))
	      (pos (cdar restlist) (cdar restlist)))
          ((null restlist) t)
	  (let* ((assuname (keim~name (th~simplifier-assumption simplifier)))
		 (theory (th~ass-theory
			  (th~simplifier-assumption simplifier)))
		 (assuline (tacl~insert&return-assumption theory assuname))
		 (simplantecedent (th~simplifier-antecedent simplifier)))

	    (let* ((subst (term~alpha-match simplantecedent
					    (data~struct-at-position (node~formula (cadr conc-line)) pos))))
	      
	      (multiple-value-bind
		  (term assuboundvars)
		  (logic~remove-leading-universal-quantors (node~formula assuline))
		(declare (ignore term))
		(let ((assuinstances (mapcar #'(lambda (var) (subst~apply subst var))
					     assuboundvars)))
		  (setf conc-line
			(if (null (rest restlist))
			    (let ((eqline (tacl~apply 'foralle* (list nil assuline) (list assuinstances))))
			      (tacl~apply '=subst (list (cadr conc-line) prec (car eqline)) (list pos)))
			  (let ((eqline (tacl~apply 'foralle* (list nil assuline) (list assuinstances)))) 
			    (tacl~apply '=subst (list (cadr conc-line) nil (car eqline)) (list pos)))))))))
	  (tacl~end))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushneg-special ;; wird spaeter durch allgemeine Regel fuer subtermpos ersetzt
;; fuer Armin
;;
;; --(AvB)
;; -------
;; -(-AnB)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic pushneg-special
		 (outline-mappings (((nonexistent existent) pushneg-special-f)
                                    ((existent nonexistent) pushneg-special-b)
				    ((existent existent) pushneg-special-a)))
                 (expansion-function batac=expand-pushneg-special))


(tac~deftactic pushneg-special-a pushneg-special (in base)
               (premises L2)
               (conclusions L1)
               (sideconditions (batac=pushneg-special-a-p (formula L1) (formula L2)))
               (description "Application of special pushnegation rule."))

(defun batac=pushneg-special-a-p (pushed negA)
  (and (batac=double-negation-p negA)
       (logic~negation-p pushed)
       (term~alpha-equal
	(pds~pushneg (car (data~appl-arguments negA)))
	(car (data~appl-arguments pushed)))))


(tac~deftactic pushneg-special-f pushneg-special (in base)
               (premises L1)
               (conclusions L2)
               (computations (L2 (batac=pushneg-special-f (formula L1))))
               (sideconditions (batac=double-negation-p (formula L1)))
               (description "Forward application of pushnegation rule."))

(defun batac=pushneg-special-f (L)
  (batac=compute-negated-formula
   (pds~pushneg (car (data~appl-arguments L)))))

(tac~deftactic pushneg-special-b pushneg-special (in base)
               (premises L1)
               (conclusions L2)
               (computations (L1 (batac=pullneg-special-f (formula L2))))
	       (sideconditions (logic~negation-p (formula L2)))
               (description "Backward application of pushnegation rule."))

(defun batac=pullneg-special-f (L)
  (batac=compute-negated-formula
   (pds~pullneg (car (data~appl-arguments L)))))

(defun batac=expand-pushneg-special (outline parameters)
  (progn
    (tacl~init outline)
    (tacl~sequence
     (=subst-res ('=subst (list (car outline) (cadr outline) nil) (list (pos~add-front 1))))
				    ;-(-An-B) --(AvB) -(AvB)=(-An-B)
     (equiv2=-res ('equiv2= (list (third =subst-res) nil) (list (pos~empty)))) 
     (equivi-res ('equivi (list (second equiv2=-res) nil nil) nil))
     (impi1-res ('impi (list (third equivi-res) nil) nil))  
     (dummy1 ('pushneg (list (second impi1-res) (third impi1-res)) nil))
     (impi2-res ('impi (list (second equivi-res) nil) nil)) 
     (dummy2 ('pullneg (list (second impi2-res) (third impi2-res)) nil)))
    (tacl~end)))



(com~defcommand pushneg-special
  (argnames neg pos)
  (argtypes ndline ndline)
  (arghelps "--(AvB)" "-(-An-B)")
  (function batac=pushneg-special)
  (frag-cats tactics base-negation)
  (defaults ((com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Push a negation."))


(defun batac=pushneg-special (neg pos)
  (infer~compute-outline 'pushneg-special (list pos neg) nil))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pullneg-special ;; wird spaeter durch allgemeine Regel fuer subtermpos ersetzt
;; fuer Armin
;;
;; -(-AnB)
;; -------
;; --(AvB)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic pullneg-special
		 (outline-mappings (((nonexistent existent) pullneg-special-f)
                                    ((existent nonexistent) pullneg-special-b)
				    ((existent existent) pullneg-special-a)))
                 (expansion-function batac=expand-pullneg-special))


(tac~deftactic pullneg-special-a pullneg-special (in base)
               (premises L2)
               (conclusions L1)
               (sideconditions (batac=pushneg-special-a-p (formula L2) (formula L1)))
               (description "Application of special pullnegation rule."))


(tac~deftactic pullneg-special-f pullneg-special (in base)
               (premises L1)
               (conclusions L2)
               (computations (L2 (batac=pullneg-special-f (formula L1))))
               (sideconditions (logic~negation-p (formula L1)))
               (description "Forward application of pullnegation rule."))

(tac~deftactic pullneg-special-b pullneg-special (in base)
               (premises L1)
               (conclusions L2)
               (computations (L1 (batac=pushneg-special-f (formula L2))))
	       (sideconditions (batac=double-negation-p (formula L2)))
               (description "Backward application of pullnegation rule."))


(defun batac=expand-pullneg-special (outline parameters)
  (progn
    (tacl~init outline)
    (tacl~sequence
     (=subst-res ('=subst (list (car outline) (cadr outline) nil) (list (pos~add-front 1))))
				    ;-(-An-B) --(AvB) -(AvB)=(-An-B)
     (equiv2=-res ('equiv2= (list (third =subst-res) nil) (list (pos~empty)))) 
     (equivi-res ('equivi (list (second equiv2=-res) nil nil) nil))
     (impi1-res ('impi (list (third equivi-res) nil) nil))  
     (dummy1 ('pullneg (list (second impi1-res) (third impi1-res)) nil))
     (impi2-res ('impi (list (second equivi-res) nil) nil)) 
     (dummy2 ('pushneg (list (second impi2-res) (third impi2-res)) nil)))
    (tacl~end)))



(com~defcommand pullneg-special
  (argnames neg pos)
  (argtypes ndline ndline)
  (arghelps "--(AvB)" "-(-An-B)")
  (function batac=pullneg-special)
  (frag-cats tactics base-negation)
  (defaults ((com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Push a negation."))


(defun batac=pullneg-special (neg pos)
  (infer~compute-outline 'pullneg-special (list neg pos) nil))  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NotImp2NotOr ;; wird spaeter durch allgemeine Regel fuer subtermpos ersetzt
;; von und fuer Armin
;;
;; -(A=>B)
;; -------
;; -(-AvB)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic notimp2notor
		 (outline-mappings (((nonexistent existent) notimp2notor-f)
                                    ((existent nonexistent) notimp2notor-b)
				    ((existent existent) notimp2notor-a)))
                 (expansion-function batac=expand-notimp2notor))


(tac~deftactic notimp2notor-a notimp2notor (in base)
               (premises L1)
               (conclusions L2)
               (sideconditions (batac=notimp-equiv-notor-p (formula L1) (formula L2)))
               (description "Application of NotImp2NotOr rule."))

(defun batac=negated-implication-p (wff)
  (and (logic~negation-p wff) (logic~implication-p (car (data~appl-arguments wff)))))

(defun batac=negated-disjunction-p (wff)
  (and (logic~negation-p wff) (logic~disjunction-p (car (data~appl-arguments wff)))))

(defun batac=notimp-equiv-notor-p (notimp notor)
  (and (batac=negated-implication-p notimp)
       (batac=negated-disjunction-p notor)
       (let ((impargs (data~appl-arguments (car (data~appl-arguments notimp))))
	     (orargs (data~appl-arguments (car (data~appl-arguments notor)))))
	 (and (term~alpha-equal (car impargs) (logic~negate (car orargs)))
	      (term~alpha-equal (cadr impargs) (cadr orargs))))))


(tac~deftactic notimp2notor-f notimp2notor (in base)
               (premises L1)
               (conclusions L2)
               (computations (L2 (batac=notimp2notor-f (formula L1))))
               (sideconditions (batac=negated-implication-p (formula L1)))
               (description "Forward application of NotImp2NotOr rule."))

(defun batac=compute-disjunction (a b)
  (let ((or (env~lookup-object :or (pds~environment omega*current-proof-plan))))
    (term~appl-create or (list a b))))

(defun batac=notimp2notor-f (notimp)
  (let ((impargs (data~appl-arguments (car (data~appl-arguments notimp)))))
    (logic~negate (batac=compute-disjunction (logic~negate (car impargs))
					     (cadr impargs)))))

(tac~deftactic notimp2notor-b notimp2notor (in base)
               (premises L1)
               (conclusions L2)
               (computations (L1 (batac=notor2notimp-f (formula L2))))
	       (sideconditions (batac=negated-disjunction-p (formula L2)))
               (description "Backward application of NotImp2NotOr rule."))

(defun batac=compute-implication (a b)
  (let ((impl (env~lookup-object :implies (pds~environment omega*current-proof-plan))))
    (term~appl-create impl (list a b))))

(defun batac=notor2notimp-f (notor)
  (let ((disargs (data~appl-arguments (car (data~appl-arguments notor)))))
    (logic~negate (batac=compute-implication (logic~negate (car disargs))
					     (cadr disargs)))))

(defun batac=expand-notimp2notor (outline parameters)
  (progn
    (tacl~init outline)
    (tacl~sequence
     (indirect-res ('indirect (list (car outline) nil) nil))
     (notnote-res ('notnote (list nil (third indirect-res)) nil))
     (or2imp-res ('or2imp (list nil (first notnote-res)) nil))
     (dummy ('note (list (second indirect-res) (first or2imp-res) (cadr outline)) nil))
     )
    (tacl~end)))



(com~defcommand notimp2notor
  (argnames notimp notor)
  (argtypes ndline ndline)
  (arghelps "-(A=>B)" "-(-AvB)")
  (function batac=notimp2notor)
  (frag-cats tactics base-negation)
  (defaults ((com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Apply Imp2Or on the argument of a negation."))


(defun batac=notimp2notor (notimp notor)
  (infer~compute-outline 'notimp2notor (list notor notimp) nil))    






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NotOr2NotImp ;; wird spaeter durch allgemeine Regel fuer subtermpos ersetzt
;; von und fuer Armin
;;
;; -(-AvB)
;; -------
;; -(A=>B)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic notor2notimp
		 (outline-mappings (((nonexistent existent) notor2notimp-f)
                                    ((existent nonexistent) notor2notimp-b)
				    ((existent existent) notor2notimp-a)))
                 (expansion-function batac=expand-notor2notimp))


(tac~deftactic notor2notimp-a notor2notimp (in base)
               (premises L1)
               (conclusions L2)
               (sideconditions (batac=notimp-equiv-notor-p (formula L2) (formula L1)))
               (description "Application of NotOr2NotImp rule."))

(tac~deftactic notor2notimp-f notor2notimp (in base)
               (premises L1)
               (conclusions L2)
               (computations (L2 (batac=notor2notimp-f (formula L1))))
               (sideconditions (batac=negated-disjunction-p (formula L1)))
               (description "Forward application of Notor2notimp rule."))

(tac~deftactic notor2notimp-b notor2notimp (in base)
               (premises L1)
               (conclusions L2)
               (computations (L1 (batac=notimp2notor-f (formula L2))))
	       (sideconditions (batac=negated-disjunction-p (formula L2)))
               (description "Backward application of NotOr2NotImp rule."))

(defun batac=expand-notor2notimp (outline parameters)
  (progn
    (tacl~init outline)
    (tacl~sequence
     (indirect-res ('indirect (list (car outline) nil) nil))
     (notnote-res ('notnote (list nil (third indirect-res)) nil))
     (imp2or-res ('imp2or (list nil (first notnote-res)) nil))
     (dummy ('note (list (second indirect-res) (first imp2or-res) (cadr outline)) nil))
     )
    (tacl~end)))



(com~defcommand notor2notimp
  (argnames notor notimp)
  (argtypes ndline ndline)
  (arghelps "-(-AvB)" "-(A=>B)")
  (function batac=notor2notimp)
  (frag-cats tactics base-negation)
  (defaults ((com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Apply Or2Imp on the argument of a negation."))


(defun batac=notor2notimp (notor notimp)
  (infer~compute-outline 'notor2notimp (list notimp notor) nil))    



;; (load "/project/omega/omega-3/theories/base/base-rules-agents.thy")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sforalli   
;; sforalle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;now in POST

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUSH (PULL) - QUANTIFIER Tactic
;; Pushes quantifiers from the outside of a formula as close as
;; possible to atomic formulas, i.e. it builds some kind of
;; Anti-Prenex-Normalform.
;; So far it works for the following quantifier pairs:
;; forall            --  exists
;; forall-sort       --  exists-sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic push-quant
		 (outline-mappings (((nonexistent existent) push-quant-f)
				    ((existent existent) push-quant-a)))
	         (expansion-function batac=expand-pull-quant)
		 (help "Pushing of quantifiers through the formula."))

(tac~deftactic push-quant-f push-quant (in base)
  (conclusions C)
  (premises P )
  (sideconditions )
  (computations 
   (C (batac=push-quantifiers (formula P))))
  (description "Pushing quantifiers through the formula."))

(tac~deftactic push-quant-a push-quant (in base)
  (conclusions C)
  (premises P )
  (sideconditions 
   (batac=push-quant-applicable-p (formula P) (formula C)))
  (computations )
  (description "Pushing quantifiers through the formula."))

(com~defcommand push-quant
  (argnames line pushed)
  (argtypes ndline ndline)
  (arghelps "An arbitrary line" "A line where all quantifiers are pushed inside the formula")
  (function batac=push-quant)
  (frag-cats tactics base-quantifier) 
  (log-p T)
  (help "All quantifiers are pushed inside the formula to the innermost possible position."))

(defun batac=push-quant (line pushed)
  (infer~compute-outline 'push-quant (list pushed line) nil))

(infer~deftactic pull-quant
		 (outline-mappings (((existent nonexistent) pull-quant-b)
				    ((existent existent) pull-quant-a)))
	         (expansion-function batac=expand-pull-quant)
		 (help "Pushing of quantifiers through the formula in an open line."))

(tac~deftactic pull-quant-b pull-quant (in base)
  (conclusions C)
  (premises P )
  (sideconditions )
  (computations 
   (P (batac=push-quantifiers (formula C))))
  (description "Backwards pushing of quantifiers through the formula."))

(tac~deftactic pull-quant-a pull-quant (in base)
  (conclusions C)
  (premises P )
  (sideconditions 
   (batac=push-quant-applicable-p (formula C) (formula P)))
  (computations )
  (description "Pushing quantifiers through the formula."))

(defun batac=expand-pull-quant (outline parameters)
  (declare (ignore parameters))
    (tacl~init outline)
    (let* (;(equality (th~find-assumption '= 'base))
	   ;(new-conc (tacl~apply 'defsi (list (car outline) nil) (list (list equality))))
	   ;(new-prem (tacl~apply 'defse (list nil (cadr outline)) (list (list equality))))
	   ;(node (cadr new-conc))
	   ;(premises (list (car new-prem))))
	   (node (car outline))
	   (premises (cdr outline)))
      (setf (just~method (node~justification node)) (infer~find-method 'otter))
      (setf (just~premises (node~justification node)) premises)
      (setf (pdsj~parameters (node~justification node)) (list t))
      (tacl~end)
      (setf (pdsj~status (node~justification node)) "untested")))

(com~defcommand pull-quant
  (argnames line pulled)
  (argtypes ndline ndline)
  (arghelps "An open line" "A line where all quantifiers are pushed inside the formula")
  (function batac=pull-quant)
  (frag-cats tactics base-quantifier) 
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "All quantifiers are pushed inside the formula to the innermost possible position."))

(defun batac=pull-quant (line pulled)
  (infer~compute-outline 'pull-quant (list line pulled) nil))

(defun batac=push-quant-applicable-p (P C)
  (term~alpha-equal (batac=push-quantifiers P) C))

(defgeneric batac=push-quantifiers (formula &optional quantifier-list)
  (declare (edited  "13-NOV-1998")
	   (authors Sorge)
	   (input   "A term and a list of quantifiers together with bound variables and additional arguments.")
	   (effect  )
	   (value   "A formula where the logical quantifiers are pushed to the innermost"
		    "formulae (as far as possible at least)."))
  (:method (formula &optional quantifier-list)
	   (declare (ignore quantifier-list))
	   (error "~A is not a term." formula))
  (:method ((formula cons) &optional quantifier-list)
	   (mapcar #'(lambda (x) (batac=push-quantifiers x quantifier-list)) formula))
  (:method ((formula term+term) &optional quantifier-list)
	   (if quantifier-list
	       (batac=create-quantification-from-list quantifier-list formula)
	     formula))
  (:method ((formula term+appl) &optional quantifier-list)
	   (let ((function (data~appl-function formula))
		 (arguments (data~appl-arguments formula)))
	     (cond ((or (logic~universal-quantor-p function) (logic~existential-quantor-p function))
		    (let ((var (data~abstr-binder (car arguments)))
			  (scope (data~abstr-range (car arguments))))
		      (batac=push-quantifiers scope (cons (list function var (cdr arguments)) quantifier-list))))
		   (quantifier-list
		    (do* ((quantifiers quantifier-list (cdr quantifiers))
			  (quantifier (car quantifiers) (car quantifiers))
			  (new-term (batac=push-quantifier-rule quantifier formula)
				    (batac=push-quantifier-rule quantifier new-term)))
			((or (null (cdr quantifiers))
			     (logic~universal-quantification-p new-term)
			     (logic~existential-quantification-p new-term))
			 (if (null quantifiers)
			     new-term
			   (batac=create-quantification-from-list (cdr quantifiers) new-term)))))
		   (t (term~appl-create function
					(mapcar #'batac=push-quantifiers arguments)))))))

(defun batac=create-quantification-from-list (quantifier-list formula)
  (declare (edited  "14-NOV-1998 20:48")
	   (authors SORGE)
	   (input   "A list of quantifier-triple and a formula.")
	   (effect  "None.")
	   (value   "The formula quantified with the given list of quantifiers."))
  (if quantifier-list
      (let* ((quant (car quantifier-list))
	     (quantifier (first quant))
	     (bound-var (second quant))
	     (add-args (third quant)))
	(batac=create-quantification-from-list (cdr quantifier-list)
					       (logic~quantification-create quantifier bound-var formula add-args)))
    formula))


(defun batac=push-quantifier-rule (quantifier-triple appl)
  (declare (edited  "14-NOV-1998 20:33")
	   (authors SORGE)
	   (input   "A quantifier-triple and an application.")
	   (effect  "None.")
	   (value   "A term where the quantifier is pushed inside as far as possible."))
  (let ((quantifier (first quantifier-triple))
	(bound-var (second quantifier-triple))
	(add-args (third quantifier-triple))
	(function (data~appl-function appl))
	(arguments (data~appl-arguments appl)))
    (cond ((logic~negation-p appl)          ;;;  NOT Rule
	   (term~appl-create function
			      (batac=push-quantifiers arguments
						      (list (list (logic~dual-quantifier quantifier) bound-var add-args)))))
	  ((logic~disjunction-p appl)        ;;;  OR Rules
	   (let ((farg (car arguments))
		 (sarg (cadr arguments)))
	     (cond ((batac=var-not-occurs-in-term bound-var arguments)              ;;; OR Rule 1a
		    (term~appl-create function (list (batac=push-quantifiers arguments))))
		   ((batac=var-not-occurs-in-term bound-var farg)                   ;;; OR Rule 1b
		    (term~appl-create function
				      (list (batac=push-quantifiers farg)
					    (batac=push-quantifiers sarg (list quantifier-triple)))))
		   ((batac=var-not-occurs-in-term bound-var sarg)                   ;;; OR Rule 1c
		    (term~appl-create function
				      (list (batac=push-quantifiers farg (list quantifier-triple))
					    (batac=push-quantifiers sarg))))
		   ((logic~existential-quantor-p quantifier)                           ;;; OR Rule 2
		    (term~appl-create function
				      (list (batac=push-quantifiers farg (list quantifier-triple))
					    (batac=push-quantifiers sarg (list quantifier-triple)))))
		   (t (logic~quantification-create quantifier bound-var appl add-args)))))  ;;; failed
	  ((logic~conjunction-p appl)        ;;;  AND Rules
	   (let ((farg (car arguments))
		 (sarg (cadr arguments)))
	     (cond ((batac=var-not-occurs-in-term bound-var arguments)              ;;; AND Rule 1a
		    (term~appl-create function (list (batac=push-quantifiers arguments))))
		   ((batac=var-not-occurs-in-term bound-var farg)                   ;;; AND Rule 1b
		    (term~appl-create function
				      (list (batac=push-quantifiers farg)
					    (batac=push-quantifiers sarg (list quantifier-triple)))))
		   ((batac=var-not-occurs-in-term bound-var sarg)                   ;;; AND Rule 1c
		    (term~appl-create function
				      (list (batac=push-quantifiers farg (list quantifier-triple))
					    (batac=push-quantifiers sarg))))
		   ((logic~universal-quantor-p quantifier)                           ;;; AND Rule 2
		    (term~appl-create function
				      (list (batac=push-quantifiers farg (list quantifier-triple))
					    (batac=push-quantifiers sarg (list quantifier-triple)))))
		   (t (logic~quantification-create quantifier bound-var appl add-args)))))  ;;; failed
	  ((logic~implication-p appl)        ;;;  IMPLIES Rules
	   (let ((farg (car arguments))
		 (sarg (cadr arguments)))
	     (cond ((batac=var-not-occurs-in-term bound-var arguments)              ;;; IMPLIES Rule 1a
		    (term~appl-create function (list (batac=push-quantifiers arguments))))
		   ((batac=var-not-occurs-in-term bound-var farg)                   ;;; IMPLIES Rule 1b
		    (term~appl-create function
				      (list (batac=push-quantifiers farg)
					    (batac=push-quantifiers sarg (list quantifier-triple)))))
		   ((batac=var-not-occurs-in-term bound-var sarg)                   ;;; IMPLIES Rule 1c
		    (term~appl-create function
				      (list (batac=push-quantifiers
					     farg
					     (list (list (logic~dual-quantifier quantifier) bound-var add-args)))
					    (batac=push-quantifiers sarg))))
		   ((logic~existential-quantor-p quantifier)                           ;;; IMPLIES Rule 2
		    (term~appl-create function
				      (list (batac=push-quantifiers
					     farg
					     (list (list (logic~dual-quantifier quantifier) bound-var add-args)))
					    (batac=push-quantifiers sarg (list quantifier-triple)))))
		   (t (logic~quantification-create quantifier bound-var appl add-args)))))  ;;; failed
	  (t (logic~quantification-create quantifier bound-var appl add-args)))))
	  


(defgeneric batac=var-not-occurs-in-term (var term)
  (declare (edited  "14-NOV-1998 22:25")
	   (authors SORGE)
	   (input   "A variable or a list of variables and a term or a list of terms.")
	   (effect  "None.")
	   (value   "NIL if one of the variables occurs in one of the terms. T o/w."))
  (:method ((var cons) term)
	   (every #'(lambda (x) (batac=var-not-occurs-in-term x term)) var))
  (:method (var (term cons)) 
	   (every #'(lambda (x) (batac=var-not-occurs-in-term var x)) term))
  (:method ((var term+variable) (term term+term))
	   (not (find var (data~free-variables term) :test #'keim~equal))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; foralle-sort  
;; foralli-sort
;; existsi-sort  
;; existse-sort  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;now in POST

;;; existsi*
(infer~deftactic existsi*
		 (outline-mappings (((existent nonexistent) existsi*-b)))
		 (parameter-types termsym-list)
		 (expansion-function batac=expand-existsi*)
		 (help "Iterated FORALL-Introduction."))

(tac~deftactic existsi*-b existsi* (in base)
   (parameters (TL cons "A list of new constants."))
   (premises P)
   (conclusions C)
   (computations (P (batac=compute-iterated-exists (formula C) TL)))
   (sideconditions (batac=exists-p (formula C)))
   (description "Backward application of iterated FORALL-Introduction."))

(defun batac=exists-p (formula)
  (and
   (term~appl-p formula)
   (data~schema-equal (data~appl-function formula)
	       (env~lookup-object :exists (pds~environment omega*current-proof-plan)))))

(defun batac=compute-iterated-exists (formula const-list)
  (let ((new-term formula))
    (dolist (const const-list)
      (setq new-term (beta~contract
                      (term~appl-create (car (data~appl-arguments new-term))
                                   (list const)))))
    new-term))

(defun batac=expand-existsi* (outline parameters)
  (let* ((exists-line (first outline))
	 (const-list (first parameters))
	 (last-const (first (last const-list))))
    (tacl~init outline)
    (when (rest const-list)
      (do ((rest-consts const-list (rest rest-consts)))
	  ((null (rest rest-consts)) t)
	(let* ((head-const (first rest-consts)))
	  (setq exists-line 
		(second (tacl~apply 'existsi
				    (list exists-line nil)
				    (list head-const
					  (data~substruct-positions
					   (logic~quantification-bound-variable (node~formula exists-line))
					   (logic~quantification-scope (node~formula exists-line))))))))))
    (tacl~apply 'existsi
		(list exists-line (second outline))
		(list last-const
		      (data~substruct-positions
					   (logic~quantification-bound-variable (node~formula exists-line))
					   (logic~quantification-scope (node~formula exists-line)))))
    (tacl~end)))

(com~defcommand existsi*
  (argnames exline line witness)
  (argtypes ndline ndline term-list)
  (arghelps "An existentially quanitified line" "A line." "A list of witness terms.")
  (function batac=existsi*)
  (frag-cats tactics base-quantifier)
  (defaults batac=existsi*-defaults)
  (log-p T)
  (help "Apply a series of Existsi-Introductions."))

(defun batac=existsi* (C P wit)
  (infer~compute-outline 'existsi* (list C P) (list wit)))

(defun batac=existsi*-defaults (planline line consts)
    (cond ((not (com~specified-arg-p planline))
	   (list (pds~find-open-node #'logic~existential-quantification-p) (com~unspecified) (com~unspecified)))
	  ((not (com~specified-arg-p line))
	   (list planline nil (com~unspecified)))
	  ((not (com~specified-arg-p consts))
	   (list planline line (com~unspecified)))
	   (t (list planline line consts))))



;;; existse*

(infer~deftactic existse*
		 (outline-mappings (((existent existent nonexistent) existse*-b)))
		 (parameter-types termsym-list)
		 (expansion-function batac=expand-existse*)
		 (help "Iterated EXISTS-Elimination."))

(tac~deftactic existse*-b existse* (in base)
   (parameters (SL cons "A list of new constants."))
   (conclusions C)
   (premises Ex P)
   (hypotheses (H P))
   (sideconditions 
    (batac=exists-p (formula Ex))
    (batac=not-free-in-nodes-or-hyps-p SL C Ex))
   (computations
    (P (batac=compute-existse-sort (formula C)))
    (H (batac=compute-iterated-exists (formula Ex) SL)))
   (description "Backward application of iterated EXISTS-Elimination."))

(defun batac=compute-existse-sort (ex)
        ex)

(defun batac=not-free-in-nodes-or-hyps-p (list C E)
  (and (= (length (remove-duplicates list))(length list))
       (cond ((null list) T)
	     ((atom list) (pds~not-free-in-nodes-or-hyps-p list C E))
	      (T (and (pds~not-free-in-nodes-or-hyps-p (first list) C E)
		      (batac=not-free-in-nodes-or-hyps-p (rest list) C E))))))

 
(defun batac=expand-existse* (outline parameters)
  (let* ((conc-line (car outline))
	 (exi-line (cadr outline))
	 (prem-line (caddr outline))
	 (const-list (first parameters))
	 (last-const (first (last const-list)))
	 (hyp (car (set-difference (pdsn~hyps prem-line) (pdsn~hyps conc-line)))))
    (tacl~init outline)
    (when (rest const-list)
      (do ((rest-consts const-list (rest rest-consts)))
	  ((null (rest rest-consts)) t)
	(let* ((head-const (first rest-consts))
	       (new-outline (tacl~apply 'existse (list conc-line exi-line nil) (list head-const))))
	  (setq conc-line (third new-outline)
		exi-line (fourth new-outline)))))
    (let ((result (tacl~apply 'existse (list conc-line exi-line nil) (list last-const))))
      (tac~forget&destroy-hyp (list (third result)) hyp (fourth result))
      (tacl~apply 'weaken (list (third result) prem-line) nil)
      (tacl~end)
      (batac=change-hyp-to-hyps-in hyp
				   (set-difference (pdsn~hyps (third result))(pdsn~hyps prem-line))
				   prem-line))))

(defun batac=change-hyp-to-hyps-in (hyp hyp-list line)
  (let ((node-list (pds~ordered-premises line)))
    (dolist (node node-list)
      (let  ((line-hyps (pdsn~hyps node)))
	(when (member hyp line-hyps)
	  (setf (pdsn~hyps node) (union hyp-list line-hyps)))))))


(com~defcommand existse*
  (argnames exline line parameter subgoal)
  (argtypes ndline ndline termsym-list ndline)
  (arghelps "An existentially quanitified line" "A line." "A list of new constants." "The line with the subgoal.")
  (function batac=existse*)
  (frag-cats tactics base-quantifier)
  (defaults batac=existse*-defaults)
  (log-p T)
  (help "Apply a series of Exists-Elmination."))

(defun batac=existse* (exline C param P)
  (infer~compute-outline 'existse* (list C exline P) (list param)))

(defun batac=existse*-defaults (ex-line line consts prem)
  (cond ((not (com~specified-arg-p ex-line))
	   (list  (pds~find-support #'logic~existential-quantification-p)
                  (com~unspecified)
		  (com~unspecified)
		  (com~unspecified)))
	((not (com~specified-arg-p line))
	 (list ex-line
	       (oc~default-current-planline)
	       (com~unspecified)
	       (com~unspecified)))
	((not (com~specified-arg-p consts))
	 (list ex-line
	       line
	       (batac=generate-defaults-existse* ex-line)
	       (com~unspecified))) 
	((not (com~specified-arg-p prem))
	 (list ex-line
	       line
	       consts
	       (oc~nil-argument))) 
	(t (list ex-line line consts prem))))


(defun batac=generate-defaults-existse* (line)
  (let ((form (node~formula line))
	(env (pds~environment omega*current-proof-plan)))
    (do ((form form (if (data~abstr-p (car (data~appl-arguments
					    form)))
			(logic~quantification-scope form)
		      form))
	 (res nil))
	((not (logic~existential-quantification-p form))
	 (nreverse res))
      (let ((var (if (data~abstr-p (car (data~appl-arguments
				    form)))
		     (logic~quantification-bound-variable form))))
	(if var
	    (let ((var-name (concatenate 'string (string (keim~name var))
					 (princ-to-string 1))))
	      (if (env~lookup-object var-name env)
		  (let ((n 2))
		    (loop
		     (setq var-name (concatenate 'string (string (keim~name var))
						 (princ-to-string n)))
		     (when (not (env~lookup-object var-name env))
		       (return))
		     (incf n))))
	      (term~generate-term-primitive-with-new-name
	       (keim~name var)
	       (term~type var)
	       'term+constant
	       env)
	      (push (env~lookup-object var-name env) res))
	  (setq form nil)))))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ded Tactic
;;
;; The ded tactic performs impi forwards with arbitrarily many hypotheses
;;                             Hyp_1, ..., Hyp_n, Hypm1, ... |- F
;; -----------------------------------------------------------------------------------
;; Hypm1, .... |- Formula(Hyp_1) => (Formula(Hyp_2) => (..... Formula(Hyp_n) => F)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(infer~deftactic ded
		 (outline-mappings (((nonexistent existent) ded-f)))
		 (parameter-types ndline-list)
		 (expansion-function batac=expand-ded)
		 (help "Performs ImpI Forwads."))

(tac~deftactic ded-f ded (in base)
   (parameters (hyps cons "A list of hypotheses."))
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=implication-of-hyps-f L1 hyps)))
   (sideconditions (batac=hyps-in-hyps-of-premise l1 hyps))
   (description "Forward application of simultaneous ImpI."))

(defun batac=hyps-in-hyps-of-premise (node hyps)
  (if (null hyps)
      nil
    (let* ((hyps-of-node (pdsn~hyps node)))
      (every #'(lambda (hyp)
		 (find hyp hyps-of-node))
	     hyps))))
  
(defun batac=implication-of-hyps-f (node hyps)
  (let* ((impl-obj (env~lookup-object 'implies (th~env 'base)))
	 (new-formula (do* ((current-formula (node~formula node))
			    (rest-hyps hyps (rest rest-hyps)))
			  ((null rest-hyps)
			   current-formula)
			(setf current-formula (term~appl-create impl-obj (list (node~formula (first rest-hyps)) current-formula))))))
    new-formula))
	 
(defun batac=expand-ded (outline parameters)
   (tacl~init outline)

  (let* ((conc (first outline))
	 (prem (second outline))
	 (hyps (first parameters))
	 (new-prem (do* ((rest-hyps (reverse hyps) (rest rest-hyps))
			 (current-conc conc))
		       ((null rest-hyps)
			current-conc)
		     (let* ((result (tacl~apply 'impi (list current-conc nil) nil))
			    (new-conc (second result))
			    (new-hyp (third result)))
		       (tac~forget&destroy-hyp (list new-conc) (first rest-hyps) new-hyp)
		       (setf current-conc new-conc)))))
    (tacl~apply 'weaken (list new-prem prem) nil)
    (tacl~end)
    ))
    
			

  
(com~defcommand ded
  (argnames line hyps)
  (argtypes ndline ndline-list)
  (arghelps "A line" "Hyps of this line")
  (function batac=ded)
  (frag-cats tactics base)
  (defaults )
  (log-p T)
  (help "Applies the ded tactic."))

(defun batac=ded (line hyps)
  (let* ((conc (first (infer~compute-outline 'ded (list nil line) (list hyps)))))
    (setf (pdsn~hyps conc)
	  (remove-if #'(lambda (hyp)
			 (find hyp hyps))
		     (pdsn~hyps conc)))))
;; This is the worst of all possible HACKS!
;; Currently, we do not have the possibility to handle hypotheses in the needed way, that is, to decrease the set of
;; hypotheses by an tactic step ... AMEIER



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following are all wild tactics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; AndI*

(infer~defwild-tactic andi*
		      (outline-mappings (((nonexistent list) andi*-f)
					 ((existent nonexistent) andi*-b)
					 ((existent list) andi*-a)))
		      (parameter-types )
		      (expansion-function batac=expand-andi*)
		      (help "Simplification of a conjunctive goal."))

(defun andi*-f (concs prems parameters)
  (declare (ignore concs parameters))
  (values (list (batac=assemble-conjunction prems)) nil))

(defun andi*-b (concs prems parameters)
  (declare (ignore prems parameters))
  (values nil (batac=split-on-and (car concs))))

(defun andi*-a (concs prems parameters)
  (declare (ignore parameters))
  (let* ((formulas (batac=split-on-and (car concs)))
	 (subset (batac=formula-list-subsetp prems formulas)))
    (cond ((null subset) nil)
	  ((listp subset) (values nil subset))
	  (t t))))

(defun batac=formula-list-equal (list1 list2)
  (declare (edited  "02-FEB-1999 16:39")
	   (authors SORGE)
	   (input   "Two lists of formulas.")
	   (value   "T if both lists contain the same formulas (in a data~~equal sense)."))
  (cond ((and (null list1) (null list2)) t)
	((or
	  (and list1 (null list2))
	  (and (null list1) list2)) nil)
	(t
	 (let* ((formula1 (car list1))
		(formula2 (find formula1 list2 :test #'data~equal)))
	   (when formula2
	     (batac=formula-list-equal (cdr list1) (remove formula2 list2 :count 1)))))))

(defun batac=formula-list-subsetp (list1 list2)
  (declare (edited  "02-FEB-1999 16:39")
	   (authors SORGE)
	   (input   "Two lists of formulas.")
	   (value   "T if both lists contain the same formulas, if list1 is"
		    "a proper subset of list2 the elements of list2 not occuring in"
		    "list1, and NIL if an element of list1 does not occur in list2"
		    "(all in a data~~equal sense)."))
  (cond ((and (null list1) (null list2)) t)
	((and (null list1) list2) list2)
	((and list1 (null list2)) nil)
	(t
	 (let* ((formula1 (car list1))
		(formula2 (find formula1 list2 :test #'data~equal)))
	   (when formula2
	     (batac=formula-list-subsetp (cdr list1) (remove formula2 list2 :count 1)))))))

(defun batac=split-on-and (formula)
  (declare (edited  "27-JAN-1999 00:07")
	   (authors SORGE)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "A conjunction is split into a list of formulas."))
  (if (logic~conjunction-p formula)
      (append (batac=split-on-and (car (data~appl-arguments formula)))
	      (batac=split-on-and (cadr (data~appl-arguments formula))))
    (list formula)))

(defun batac=assemble-conjunction (formulas &optional (and (logic~conjunction-constant :name :and)))
  (declare (edited  "02-FEB-1999 15:44")
	   (authors SORGE)
	   (input   "A list of formulas and an conjunction junctor.")
	   (effect  "None.")
	   (value   "A conjunction made of the given formulas."))
  (cond ((null formulas) formulas)
	((rest formulas)
	 (term~appl-create and
			   (list (car formulas)
				 (batac=assemble-conjunction (cdr formulas) and))))
	(t (car formulas))))

(defun batac=expand-andi* (conclusions premises parameters)
  (declare (ignore parameters))
  (tacl~init (append conclusions premises))
  (labels ((apply-andi (line prems)
		       (let* ((formula (node~formula line))
			      (farg? (find-if #'(lambda (x)
						 (data~equal (car (data~appl-arguments formula))
							     (node~formula x)))
					     prems))
			      (sarg? (find-if #'(lambda (x)
						  (data~equal (cadr (data~appl-arguments formula))
							      (node~formula x)))
					      prems)))
			 (cond ((and farg? sarg?) (tacl~apply 'andi (list line farg? sarg?) nil))
			       (farg? (apply-andi
				       (third (tacl~apply 'andi (list line farg? nil) nil))
				       (remove farg? prems)))
			       (sarg? (apply-andi
				       (second (tacl~apply 'andi (list line nil sarg?) nil))
				       (remove sarg? prems)))
			      (t (let ((new-outline (tacl~apply 'andi (list line nil nil) nil)))
				   (apply-andi (second new-outline) prems)
				   (apply-andi (third new-outline) prems)))))))
    (apply-andi (car conclusions) premises)
    (tacl~end)))

(com~defcommand andi*
  (argnames conjunction conjunct-list)
  (argtypes ndline ndline-list)
  (arghelps "Conjunction to split" "List of conjuncts")
  (function batac=andi*)
  (frag-cats tactics base-connective)
  (log-p T)
  (help "Split a conjunction into its two conjuncts."))

(defun batac=andi* (conj prems)
  (infer~compute-outline 'andi* (list conj prems) nil))

(defun batac=andi*-defaults (conj list-of-conjuncts)
  (cond ((not (com~specified-arg-p conj))
	 (list (pds~find-support #'logic~conjunction-p) (com~unspecified) (com~unspecified)))
	(t (list conj list-of-conjuncts))))

;;; AndE*

(infer~defwild-tactic ande*
		      (outline-mappings (((list nonexistent) ande*-b)
					 ((nonexistent existent) ande*-f)
					 ((list existent) ande*-a)))
		      (parameter-types )
		      (expansion-function batac=expand-ande*)
		      (help "Simplification of a conjunctive goal."))

(defun ande*-b (concs prems parameters)
  (declare (ignore prems parameters))
  (values nil (list (batac=assemble-conjunction concs))))

(defun ande*-f (concs prems parameters)
  (declare (ignore concs parameters))
  (values (batac=split-on-and (car prems)) nil))

(defun ande*-a (concs prems parameters)
  (declare (ignore parameters))
  (let* ((formulas (batac=split-on-and (car prems)))
	 (subset (batac=formula-list-subsetp concs formulas)))
    (cond ((null subset) nil)
	  ((listp subset) (values subset nil))
	  (t t))))

(defun batac=expand-ande* (conclusions premises parameters)
  (declare (ignore parameters))
  (tacl~init (append conclusions premises))
  (labels ((apply-ande (line concs)
		       (let* ((formula (node~formula line))
			      (farg? (find-if #'(lambda (x)
						 (data~equal (car (data~appl-arguments formula))
							     (node~formula x)))
					     concs))
			     (sarg? (find-if #'(lambda (x)
						 (data~equal (cadr (data~appl-arguments formula))
							     (node~formula x)))
					     concs)))
			(cond ((and farg? sarg?) (tacl~apply 'ande (list farg? sarg? line) nil))
			      (farg? (apply-ande
				      (second (tacl~apply 'ande (list farg? nil line) nil))
				      (remove farg? concs)))
			      (sarg? (apply-ande
				      (first (tacl~apply 'ande (list nil sarg? line) nil))
				      (remove sarg? concs)))
			      (t (let ((new-outline (tacl~apply 'ande (list nil nil line) nil)))
				   (apply-ande (car new-outline) concs)
				   (apply-ande (cadr new-outline) concs)))))))
    (apply-ande (car premises) conclusions)
    (tacl~end)))

(com~defcommand ande*
  (argnames conjunction conjunct-list)
  (argtypes ndline ndline-list)
  (arghelps "Premises to split" "List of conjuncts")
  (function batac=ande*)
;;;  (defaults batac=ande-defaults)
  (frag-cats tactics base-connective)
  (log-p T)
  (help "Split a conjunction into its two conjuncts."))

(defun batac=ande* (prems conj)
  (infer~compute-outline 'ande* (list conj prems) nil))




;;;;;;;;;;;;;;;;; hypweaken

(infer~deftactic hypweaken
		 (outline-mappings (((existent existent) hypweaken-a)))
		 (expansion-function batac=expand-hypweaken)
		 (help "Weaken and insert all hyps."))

(tac~deftactic hypweaken-a hypweaken (in base)
   (premises L1)
   (conclusions L2)
   (computations )
   (sideconditions (batac=hypweaken-check l2 (formula L2) (hyps L2) (formula l1) (hyps L1)))
   (description "Weaken with the insertion of hyps."))

(defun batac=hypweaken-check  (openline openterm openhyps closedterm closedhyps)
  (let ((newhyps (set-difference closedhyps openhyps)))
    (if (term~alpha-equal closedterm openterm)
	(if newhyps (batac=hypweaken-add-hyps openline newhyps) T)
      nil)))

(defun batac=hypweaken-add-hyps (line hyps)
  (let* ((lines nil)
	 (line-hyps (pdsn~hyps line))
	 (line-supp (pds~node-supports line)))
    (dolist (hyp hyps)
      (when (not (member hyp line-hyps))
	(setf line-hyps (cons hyp line-hyps))
	(when (pdsn~open-node-p line)
	  (setf line-supp (cons hyp line-supp)))))
    (setf (pdsn~hyps line) line-hyps)
    (setf (pds~node-supports line) line-supp)
    (maphash #'(lambda (key node)
			    (when (and
				   (member line (pdsn~just-premises node))
				   (not (subsetp hyps (pdsn~hyps node))))
			      (setf lines (cons node lines))))
			(pds~label-node-hashtable omega*current-proof-plan))
    (format t "lines ~A line ~A" lines line)
    (dolist (node lines)
      (batac=hypweaken-add-hyps node hyps)))
  t)
  
(defun batac=expand-hypweaken (outline parameters)
  (tacl~init outline)
  (tacl~apply 'weaken (list (first outline) (second outline)) nil)
  (tacl~end))

(com~defcommand hypweaken
  (argnames open closed)
  (argtypes ndline ndline)
  (arghelps "An open line" "A closed line")
  (function batac=hypweaken)
  (defaults batac=hypweaken-defaults)
  (frag-cats tactics base)
  (log-p T)
  (help "Close an open line that is alpha-equal to an already proven
         line and insert the hyps of the closed line to all necessary lines."))

(defun batac=hypweaken (open closed)
  (infer~compute-outline 'hypweaken (list open closed) nil))


(defun batac=hypweaken-defaults (lower upper)
  (if (and lower (com~specified-arg-p lower))
      (list lower
	    (pds~find-node-support lower
				   #'(lambda (p) (data~equal-p (node~formula lower) p))))
    (list (oc~default-current-planline)
	  (com~unspecified))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PULL-QUANTIFICATION TACTIC
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A quantification is pulled at the top of a formula
;;
;;
;;           t1 QUANTORx.F[X] t2
;;           ----------------------
;;            QUANTOR'x't1F[x']t2
;;
;;




(infer~deftactic pull-quantification
		 (outline-mappings (((nonexistent existent) pull-quantification-f)))
		 (parameter-types position)
		 (expansion-function batac=expand-pull-quantification)
		 (help "Pulls a quantor to the front of a formula."))

(tac~deftactic pull-quantification-f pull-quantification (in base)
	       (parameters (position pos+position "Position of a universal or existential quantification."))
	       (premises L1)
	       (conclusions L2)
	       (sideconditions (batac=pull-quantification-quantification-at-position-p (formula L1) position))
	       (computations (L2 (batac=pull-quantification-create-f
				  (formula L1) position)))
	       (description "Forward application of pull-quantification"))

(defun batac=pull-quantification-quantification-at-position-p (formula position)
  (declare (edited  "14-JAN-1999")
	   (authors Ameier)
	   (input   "A formula and a position.")
	   (effect  "None.")
	   (value   "T if the subformula in the formula at position is a quantification and if"
		    "it is not in the 'scope' of an equivalece."))
  (let* ((subformula (data~struct-at-position formula position)))
    (if (null (or (logic~universal-quantification-p subformula)
		  (logic~existential-quantification-p subformula)))
	(progn
	  (omega~message "~%Substruct ~A at position ~A is not a quantification!" subformula position)
	  nil)

      (do* ((rest-pos-list (reverse (pos~number-list position)) (rest rest-pos-list))
	    (last-pos position current-position)
	    (current-position (pos~list-position (reverse rest-pos-list)) (pos~list-position (reverse rest-pos-list)))
	    (equiv-flag nil))
	  ((or (null rest-pos-list)
	       equiv-flag)
	   (cond (equiv-flag
		  (omega~message "~%The quantification at position ~A is in a equivalence at position ~A" position last-pos)
		  nil)
		 ((logic~equivalence-p formula)
		  (omega~message "~%The quantification at position ~A is in a equivalence at position ~A" position (pos~empty))
		  nil)
		 (t
		  't)))
	
	(let* ((curr-subformula (data~struct-at-position formula current-position)))
	  ;; (format t "~%~%CURR-POS: ~A" current-position)
	  ;; (format t "~%LAST POS: ~A" last-pos)
	  ;; (format t "~%CURR-SUBFORM: ~A" curr-subformula)
	  
	  (when (logic~equivalence-p curr-subformula)
	    (setq equiv-flag 't)))))))


(defun batac=pull-quantification-create-f (formula position)
  (declare (edited  "14-JAN-1999")
	   (authors Ameier)
	   (input   "A formula and a position.")
	   (effect  "None.")
	   (value   "A formula in that the quantification at position is pulled in front of the formula."))

  (multiple-value-bind
      (new-formula quantor-to-pull)
      (batac=pull-quantor-recursive formula position 't nil nil)

    (let* ((quantor-to-bound (first quantor-to-pull))
	   (var-to-bound (second quantor-to-pull))
	   (new-abstr (term~abstr-create (list var-to-bound)
					 new-formula))
	   (new-formula (term~appl-create quantor-to-bound
					  (list new-abstr))))

      ;; einige Subformulas koennen doppelt drin sein -> kopieren
      (data~copy new-formula :downto '(data+primitive)))))


(defun batac=pull-quantor-recursive (term position pol in-scope-of-forall-quantifications in-scope-of-exists-quantifications)
  (declare (edited  "15-JAN-1999")
	   (authors Ameier)
	   (input   "A term, a position in that term, the current polarity under that the term is regarded, the"
		    "list of all forall-variables in whose scope the term is and the list of all exists-variables"
		    "in whose scope the term is.")
	   (effect  "None.")
	   (value   "The quantification at position is cutted and pulled in front of the complete term."
		    "Thereby also renamings of bound variables can be happened."
		    "Multiple-value:"
		    "First: The changed term"
		    "Second: The cutted quantor inside the input term (a list of the needed quantor (forall/exists) and the"
		    "        bound variable (possibly renamed)."
		    "Third: A list of all forall-quantified variabled that are in the changed term (needed to handle possible"
		    "       renamings of bound variables"))
  
  (if (pos~empty-p position)
      
      (let* ((quantor (data~appl-function term))
	     (bound-var (logic~quantification-bound-variable term))
	     (scope (logic~quantification-scope term))
	     (possibly-conflicting-name-strings (mapcar #'(lambda (scope-var)
							    (let* ((name (keim~name scope-var)))
							      (if (stringp name)
								  name
								(string name))))
							(append in-scope-of-forall-quantifications
								in-scope-of-exists-quantifications)))
	     (bound-var-name (keim~name bound-var))
	     (bound-var-name (if (stringp bound-var-name)
				 bound-var-name
			       (string bound-var-name))))
	
	
	
	(cond ((or (and (logic~universal-quantification-p term)
			pol)
		   (and (logic~existential-quantification-p term)
			(null pol)))
	       ;; -> universal quantification

	       (multiple-value-bind
		   (conflict new-name)		   
		   (batac=compute-unconflicting-name bound-var-name possibly-conflicting-name-strings)

		 (if conflict

		     ;; conflict des Namens der bound-var mit anderen Namen -> umbenamsen durch neue Variable
		     (let* ((new-bound-var (term~variable-create new-name (term~type bound-var)))
			    (new-scope (data~replace-struct scope bound-var new-bound-var
							    :downto '(data+primitive)
							    :replacers-downto '(data+primitive)
							    :test #'eq)))
		       (values new-scope
			       (list (env~lookup-object 'forall (pds~environment omega*current-proof-plan))
				     new-bound-var)
			       nil))
		   
		   ;;sonst: Namen + Var einfach beibehalten 
		   (values scope
			   (list (env~lookup-object 'forall (pds~environment omega*current-proof-plan))
				 bound-var)
			   nil))))
	      
	      ((or (and (logic~universal-quantification-p term)
			(null pol))
		   (and (logic~existential-quantification-p term)
			pol))
	       ;; existential quantification
	       
	       (if in-scope-of-forall-quantifications

		   ;; existential quantification in scope of forall quantifications -> Functions symbol erzeugen !
		   (multiple-value-bind
		       (conflict function-name-string)		   
		       (batac=compute-unconflicting-name (format nil "F-~A" (keim~name bound-var))
							 possibly-conflicting-name-strings)
		     
		     (let* ((function-name (intern (string-upcase function-name-string) (find-package :omega)))
			    (function-type (type~func-create (mapcar #'term~type in-scope-of-forall-quantifications)
							     (term~type bound-var)))
			    (new-function-variable (term~variable-create function-name function-type))
			    (new-func-term (term~appl-create new-function-variable
							     in-scope-of-forall-quantifications))
			    (new-scope (data~replace-struct scope bound-var new-func-term
							    :downto '(data+primitive)
							    :replacers-downto '(data+primitive)
							    :test #'eq)))
		       
		       (values new-scope
			       (list (env~lookup-object 'exists (pds~environment omega*current-proof-plan))
				     new-function-variable)
			       nil)))
		 

		 ;; sonst: Versuchen Variable beizubehalten
		 (multiple-value-bind
		     (conflict new-name)		   
		     (batac=compute-unconflicting-name bound-var-name possibly-conflicting-name-strings)
		   
		   (if conflict
		       
		       ;; conflict des Namens der bound-var mit anderen Namen -> umbenamsen mit neuer Variable
		       (let* ((new-bound-var (term~variable-create new-name (term~type bound-var)))
			      (new-scope (data~replace-struct scope bound-var new-bound-var
							      :downto '(data+primitive)
							      :replacers-downto '(data+primitive)
							      :test #'eq)))
			 (values new-scope
				 (list (env~lookup-object 'exists (pds~environment omega*current-proof-plan))
				       new-bound-var)
				 nil))
		     
		     ;; sonst: Name + Var beibehalten
		     (values scope
			     (list (env~lookup-object 'exists (pds~environment omega*current-proof-plan))
				   bound-var)
			     nil)))))))
    
    (cond ((or (logic~universal-quantification-p term)
	       (logic~existential-quantification-p term))
	   
	   (let* ((quantor (data~appl-function term))
		  (bound-var (logic~quantification-bound-variable term))
		  (scope (logic~quantification-scope term))
		  (pos-rest (pos~rest (pos~rest position)))
		  (bound-var-name (keim~name bound-var)))
	     
	     (multiple-value-bind
		 (new-scope quantor-to-pull forall-quant-in-scope)
		 (if (or (and (logic~universal-quantification-p term)
			      pol)
			 (and (logic~existential-quantification-p term)
			      (null pol)))
		     ;; universal-quantification -> add bound-var to the in-scope-of-forall-quantifications list
		     (batac=pull-quantor-recursive scope pos-rest pol
						   (append in-scope-of-forall-quantifications (list bound-var))
						   in-scope-of-exists-quantifications)
		   (batac=pull-quantor-recursive scope pos-rest pol
						 in-scope-of-forall-quantifications
						 (append in-scope-of-exists-quantifications (list bound-var))))
	       
	       ;; es kann folgender Fall eintreten: wir haben hier ein forall-quantification und die gepullte quantification ist eine
	       ;; exists. Ausserdem haben wir innerhalb der forall-quant-in-scope einen Namens Konflikt
	       ;; z.B. (forall (lam (x i)
	       ;;              (forall (lam (x i)
               ;;                      (exists (y i)
	       ;;                              ....
	       ;; Das exists y wird nach aussen gepullt, dabei entsteht eine Funktionsanwendung (F-Y X X) auf die beiden forall-Variablen
	       ;; -> Eine der Forall-Variablen muss umbenamest werden, damit (F-Y X1 X)

	       (let* ((possibly-conflicting-names (mapcar #'(lambda (scope-var)
							      (let* ((name (keim~name scope-var)))
								(if (stringp name)
								    name
								  (string name))))
							  forall-quant-in-scope)))

		 (multiple-value-bind
		     (conflict new-name)
		     (batac=compute-unconflicting-name (if (stringp bound-var-name)
							   bound-var-name
							 (string bound-var-name))
						       possibly-conflicting-names)
		   
		 
		   (if (and
			;; dies hier ist universal-quantification
			(or (and (logic~universal-quantification-p term)
				 pol)
			    (and (logic~existential-quantification-p term)
				 (null pol)))
			;; gepullter ist exists
			(keim~equal (first quantor-to-pull)
				    (env~lookup-object 'exists (pds~environment omega*current-proof-plan)))
			;; Namens conflict vorhanden 
			conflict)
		       
		       (let* ((new-bound-var (term~variable-create new-name (term~type bound-var)))
			      (new-new-scope (data~replace-struct new-scope bound-var new-bound-var
								  :downto '(data+primitive)
								  :replacers-downto '(data+primitive)
								  :test #'eq)))
			 
			 (values (term~appl-create quantor
						   (list (term~abstr-create (list new-bound-var)
									    new-new-scope)))
				 quantor-to-pull
				 (cons new-bound-var forall-quant-in-scope)))

		     (values (term~appl-create quantor
					       (list (term~abstr-create (list bound-var)
									new-scope)))
			     quantor-to-pull
			     (if (or (and (logic~universal-quantification-p term)
					  pol)
				     (and (logic~existential-quantification-p term)
					  (null pol)))
				 ;; universal-quantification -> Zurueckgeben
				 (cons bound-var  forall-quant-in-scope)))))))))
	  
	  ((or (logic~conjunction-p term)
	       (logic~disjunction-p term)
	       (and (logic~implication-p term)
		    (= (pos~first position) 2)))
	   
	   (let* ((connector (data~appl-function term))
		  (args (data~appl-arguments term))
		  (pos-first (pos~first position))
		  (pos-rest (pos~rest position)))

	     (when (null (or (= pos-first 1) (= pos-first 2)))
	       (error "~% Ei oppala in Function batac=pull-quantor-recursive OR/AND/IMPLIES2 Case"))
	     
	     (multiple-value-bind
		 (new-arg-at-pos quantor-to-pull forall-quant-in-scope)
		 (batac=pull-quantor-recursive (if (= pos-first 1)
						   (first args)
						 (second args))
					       pos-rest
					       pol
					       in-scope-of-forall-quantifications
					       in-scope-of-exists-quantifications)
	       
	       (values (term~appl-create connector (if (= pos-first 1)
						       (list new-arg-at-pos (second args))
						     (list (first args) new-arg-at-pos)))
		       quantor-to-pull
		       forall-quant-in-scope))))
	  
	  ((logic~negation-p term)

	   (let* ((connector (data~appl-function term))
		  (args (data~appl-arguments term))
		  (pos-first (pos~first position))
		  (pos-rest (pos~rest position)))
	     
	     (when (null (= pos-first 1))
	       (error "~% Ei oppala in Function batac=pull-quantor-recursive NOT Case"))
	     
	     (multiple-value-bind
		 (new-arg quantor-to-pull forall-quant-in-scope)
		 (batac=pull-quantor-recursive (first args)
					       pos-rest
					       (null pol)
					       in-scope-of-forall-quantifications
					       in-scope-of-exists-quantifications)
	       
	       (values (term~appl-create connector (list new-arg))
		       quantor-to-pull
		       forall-quant-in-scope))))
	  
	  ((logic~implication-p term)
	   ;; erster Teil in implication da zweiter Teil bereits oben abgefangen

	   (let* ((connector (data~appl-function term))
		  (args (data~appl-arguments term))
		  (pos-first (pos~first position))
		  (pos-rest (pos~rest position)))
	     
	     (when (null (= pos-first 1))
	       (error "~% Ei oppala in Function batac=pull-quantor-recursive IMPLIES Case"))

	     (multiple-value-bind
		 (new-arg quantor-to-pull forall-quant-in-scope)
		 (batac=pull-quantor-recursive (first args)
					       pos-rest
					       (null pol)
					       in-scope-of-forall-quantifications
					       in-scope-of-exists-quantifications)
	       
	       (values (term~appl-create connector (list new-arg (second args)))
		       quantor-to-pull
		       forall-quant-in-scope))))
	  
	  ((logic~equivalence-p term)
	   (error "~%STILL TO DO !!! EQUIVALENCE CASE OF batac=pull-quantor-recursive")))))


(defun batac=compute-unconflicting-name (in-string list-of-strings)
  (declare (edited  "15-JAN-1999")
	   (authors Ameier)
	   (input   "A string and a list-of-strings.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: T if the string is in the list-of-strings, nil otherwise."
		    "Second: If first is t a string that consists of the in-string and a number such that"
		    "        this new sting is not in the list-of-strings, otherwise the in-string itself."))

  (if (null (find in-string list-of-strings :test #'string=))
      (values nil in-string)
    (do* ((i 1 (+ i 1))
	  (new-string (intern (string-upcase (format nil "~A~A" in-string i))
			      (find-package :omega))
		      (intern (string-upcase (format nil "~A~A" in-string i))
			      (find-package :omega))))
	((null (find new-string list-of-strings :test #'string=))
	 (values 't
		 new-string)))))


(com~defcommand pull-quantification
		(argnames pulled-line to-pull-line position)
		(argtypes ndline ndline position)
		(arghelps "The line with the pulled quantor"
			  "The line with a quantor in to pull"
			  "The position of the unpulled quantor")
		(function batac==pull-quantification)
		(frag-cats tactics base-quantifier)
		(defaults ((com~unspecified) (com~unspecified) (com~unspecified)))
		(level 1)
		(log-p t)
		(help "Pulling a quantor to the top of a formula."))

(defun batac==pull-quantification (l1 l2 position)
  (infer~compute-outline 'pull-quantification (list l1 l2) (list position)))

(defun batac=expand-pull-quantification (outline parameters)
  (declare (ignore parameters))
    (tacl~init outline)
    ; in which case do you need the expansion? 
    (let* (;(equality (th~find-assumption '= 'base))   
	   ;(new-conc (tacl~apply 'defsi (list (car outline) nil) (list (list equality))))
	   ;(new-prem (tacl~apply 'defse (list nil (cadr outline)) (list (list equality))))
	   ;(node (cadr new-conc))
	   ;(premises (list (car new-prem)))
	   (node (car outline))
	   (premises (cdr outline)))

      ;; DANGER: OTTER EXPANSION TAKES AS DEFAULT LEMMAS: CONSTANTS
      ;; THIS CAN (THEORETICALLY) AGAIN CAUSE PULL-QUANTIFICATIONS -> CIRCLE !!
      ;; KEEP AN EYE ON IT!!
      ;; IF EXISTS QUANTIFICATIONS ARE PULLED OVER FORALL-JUSTIFICATIONS AND VICE VERSA THE
      ;; EXPANSION CAN'T BE DONE BY OTTER AT ALL!
      ;; IT WOULD (THEORETICALLY) HAVE TO BE DONE BY TPS!

      (setf (pds~node-supports node) premises)
      
      (setf (just~method (node~justification node)) (infer~find-method 'otter))
      (setf (just~premises (node~justification node)) premises)
      (setf (pdsj~parameters (node~justification node)) (list t))
      (tacl~end)
      
      (setf (pdsj~status (node~justification node)) "untested")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wellsorted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;now in POST

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OrE*  Wild-Tactic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic ore*
		      (outline-mappings (((existent existent) ore*-b)))
		      (parameter-types )
		      (expansion-function batac=expand-ore*)
		      (help "Elimination of a disjunctive premise."))

(defun ore*-b (concs prems parameters)
  (declare (ignore parameters))
  (when (logic~disjunction-p (car prems))
    (let ((disj (batac=split-on-or (car prems)))
	  (conc (car concs)))
      (values nil
	      (mapcar #'(lambda (hyp) (list conc hyp))
		      disj)))))

(defun batac=split-on-or (formula)
  (declare (edited  "01-MAR-2000" "27-JAN-1999 00:07")
	   (authors Sorge SORGE)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "A disjunction is split into a list of formulas."))
  (if (logic~disjunction-p formula)
      (append (batac=split-on-or (car (data~appl-arguments formula)))
	      (batac=split-on-or (cadr (data~appl-arguments formula))))
    (list formula)))

(defun batac=expand-ore* (conclusions premises parameters)
  (declare (ignore parameters))
  (let* ((fprem (car premises))
	 (conc (car conclusions))
	 (prems (cdr premises))
	 (hyps (apply #'append
		     (mapcar #'(lambda (prem)
				 (batac=stable-set-difference (pdsn~hyps prem) (pdsn~hyps conc)))
			     prems))))
    (labels ((apply-ore (conc fprem)
			(let* ((result (tacl~apply 'ore (list conc fprem nil nil) nil))
			       (sprem (third result))
			       (tprem (fourth result))
			       (shyp (fifth result))
			       (thyp (sixth result)))
			  (cond ((and (logic~disjunction-p (node~formula shyp)) (logic~disjunction-p (node~formula thyp)))
				 (apply-ore sprem shyp)
				 (apply-ore tprem thyp))
				((logic~disjunction-p (node~formula shyp))
				 (apply-ore sprem shyp)
				 (tac~forget&destroy-hyp (list tprem) (pop hyps) thyp)
				 (tacl~apply 'weaken (list tprem (pop prems)) nil))
				((logic~disjunction-p (node~formula thyp))
				 (tac~forget&destroy-hyp (list sprem) (pop hyps) shyp)
				 (tacl~apply 'weaken (list sprem (pop prems)) nil)
				 (apply-ore tprem thyp))
				(t
				 (tac~forget&destroy-hyp (list sprem) (pop hyps) shyp)
				 (tacl~apply 'weaken (list sprem (pop prems)) nil)
				 (tac~forget&destroy-hyp (list tprem) (pop hyps) thyp)
				 (tacl~apply 'weaken (list tprem (pop prems)) nil))))))
      (tacl~init (append conclusions premises))
      (apply-ore conc fprem)
      (tacl~end))))

(com~defcommand ore*
  (argnames goal disjuntion)
  (argtypes ndline ndline)
  (arghelps "A goal to prove" "A disjunction")
  (function batac=ore*)
  (defaults orules=ore-defaults)
  (frag-cats tactics base-connective)
  (log-p T)
  (help "Make a case split on a disjunction."))

(defun batac=ore* (goal disj)
  (infer~compute-outline 'ore* (list goal disj) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ore**  Wild-Tactic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defwild-tactic ore**
		      (outline-mappings (((existent list) ore**-b)))
		      (parameter-types )
		      (expansion-function batac=expand-ore**)
		      (help "Elimination of a list of disjunctive premises."))

(let ((indicator 0))

  (defun ore**-b (concs prems parameters)
    (declare (ignore parameters))
    (when (every #'logic~disjunction-p prems)
      (setf indicator 0)
      (let* ((disjs (mapcar #'batac=split-on-or prems))
	     (conc (car concs))
	     (hyps (batac=assemble-hypotheses-lists (mapcar #'list (first disjs)) (rest disjs))))
	(values nil
		(mapcar #'(lambda (hyp) (cons conc hyp))
			hyps)))))

  (defun batac=mark-hyps (hyps)
    (declare (edited  "03-MAR-2000")
	     (authors Sorge)
	     (input   "A list of list of hypotheses.")
	     (effect  "Increases the value of the locally bound indicator variable.")
	     (value   "A list where recurring hypotheses are marked."))
    (mapcar #'(lambda (hyp-list)
		(incf indicator)
		(append (butlast hyp-list)
			(list (append (last hyp-list) (list indicator)))))
	    hyps))
  )


(defun batac=assemble-hypotheses-lists (fhyps rhyps)
  (declare (edited  "02-MAR-2000")
	   (authors Sorge)
	   (input   "Two list of lists of hypotheses.")
	   (effect  "None.")
	   (value   "A list of lists resembling the cartesian product of the input lists."))
  (if rhyps
      (batac=assemble-hypotheses-lists
       (batac=assemble-hypotheses-list (batac=mark-hyps fhyps) (car rhyps))
       (cdr rhyps))
    fhyps))

(defun batac=assemble-hypotheses-list (old-hyps new-hyps)
  (declare (edited  "02-MAR-2000")
	   (authors Sorge)
	   (input   "A list of list of hypotheses and a list of hypotheses.")
	   (effect  "Copys each of the terms in NEW-HYPS as often as the length of OLD-HYPS indicates.")
	   (value   "The cartesian product between the lists of OLD-HYPS and NEW-HYPS."))
  (when old-hyps
    (append (mapcar #'(lambda (new-hyp)
			(append (car old-hyps) (list new-hyp)))
		    new-hyps)
	    (batac=assemble-hypotheses-list (cdr old-hyps) new-hyps))))
    

;; Expansion

(defun batac=expand-ore** (conclusions premises parameters)
  (declare (ignore parameters))
  (let* ((conc-hyps (pdsn~hyps (car conclusions)))
	 (disjs (batac=get-original-premises conc-hyps premises))
	 (old-prems (batac=stable-set-difference premises disjs))
	 (hyp-list-list (batac=relate-hyps2prems disjs old-prems conc-hyps)))
    (tacl~init (append conclusions premises))
    (batac=weaken-old-new-premises
     old-prems
     (batac=apply-ore*-successively conclusions disjs hyp-list-list))
    (tacl~end)))

(defun batac=weaken-old-new-premises (old-prems new-prems)
  (when (and old-prems new-prems)
    (tacl~apply 'weaken (list (car new-prems) (car old-prems)) nil)
    (batac=weaken-old-new-premises (cdr old-prems) (cdr new-prems))))

(defun batac=apply-ore*-successively (conclusions prem-list hyps-list)
  (if (and prem-list hyps-list)
      (let* ((prem (car prem-list))
	     (result (batac=apply-ore*-once conclusions prem (car hyps-list))))
	(batac=apply-ore*-successively result (cdr prem-list) (cdr hyps-list)))
    conclusions))

(defun batac=apply-ore*-once (conclusions premise hyps)
  (when conclusions
      (let* ((real-hyps (car hyps))
	     (result (tacl~apply 'ore* (list (car conclusions) premise) nil))
	     (res-prems (cdr (second result))))
	(mapc #'tac~forget&destroy-hyp res-prems real-hyps (third result))
	(append res-prems (batac=apply-ore*-once (cdr conclusions) premise (cdr hyps))))))
	
(defun batac=relate-hyps2prems (old-prems new-prems conc-hyps)
  (declare (edited  "06-MAR-2000")
	   (authors Sorge)
	   (input   "Three list of lines.")
	   (effect  "None.")
	   (value   "A list of list of lines where each individual list corresponds to the hypotheses"
		    "introduced by splitting the premises."))
  (let* ((pre-hyps (mapcar #'(lambda (prem) (batac=stable-set-difference (pdsn~hyps prem) conc-hyps)) new-prems))
	 (ordering (batac=determine-ordering pre-hyps))
	 (hyps (mapcar #'(lambda (hyp) (batac=order-hyp-set hyp ordering)) pre-hyps)))
    (do* ((rest-prems old-prems (cdr rest-prems))
	  (counter 0 (1+ counter))
	  (divide 1 (* divide (length (car (nth (1- counter) result)))))
	  (result nil))
	((null rest-prems) result)
      (let ((rest-hyps (remove-duplicates (mapcar #'(lambda (hl) (nth counter hl)) hyps))))
	(setf result (append result (list (batac=divide-list-by-length rest-hyps divide))))))))

(defun batac=order-hyp-set (set ordering)
  (when ordering
    (let ((member (intersection set (car ordering))))
      (append member
	      (batac=order-hyp-set (remove member set) (cdr ordering))))))

(defun batac=determine-ordering (sets)
  (declare (edited  "16-MAY-2000")
	   (authors Vxs)
	   (input   "A list of sets of hypotheses.")
	   (effect  "None.")
	   (value   "A list of sets of hypotheses ordered according to their number of occurences in SETS."
		    "I.e., those hypotheses occuring most often are in front."))
  (let ((list (apply #'append sets))
	alist rlist)
    (dolist (x list)
      (let ((element (assoc x alist)))
	(if element
	    (setf (cdr element) (1+ (cdr element)))
	  (push (cons x 1) alist))))
    (setf alist (sort alist #'> :key #'cdr))
    (dolist (x alist)
      (if (and rlist (= (cdr x) (caar rlist)))
	  (setf (first rlist) (append (car rlist) (list (car x))))
	(push (list (cdr x) (car x)) rlist)))
    (mapcar #'cdr (reverse rlist))))
	
    

(defun batac=divide-list-by-length (list n)
  (declare (edited  "06-MAR-2000")
	   (authors Sorge)
	   (input   "A list and a positive integer.")
	   (effect  "None.")
	   (value   "A list of lists where the original list is split into N sub-lists."))
  (let ((length (floor (length list) n)))
    (do* ((rest-list list (subseq rest-list length))
	  (result (list (subseq rest-list 0 length))
		  (append result (list (subseq rest-list 0 length)))))
	((null rest-list) (butlast result)))))
    
(defun batac=stable-set-difference (set1 set2 &optional (test #'eq))
  (declare (edited  "03-MAR-2000")
	   (authors Sorge)
	   (input   "Two sets and a equivalence test.")
	   (effect  "None.")
	   (value   "The set-difference between SET1 and SET2 where the ordering of SET1 is preserved."))
  (when set1
    (if (find (car set1) set2 :test test)
	(batac=stable-set-difference (cdr set1) set2 test)
      (cons (car set1) (batac=stable-set-difference (cdr set1) set2 test)))))


(defun batac=get-original-premises (hyps prems)
  (declare (edited  "03-MAR-2000")
	   (authors Sorge)
	   (input   "Two list of nodes.")
	   (effect  "None.")
 	   (value   "A list containing those nodes in PREMS whose hypotheses is a subset of HYPS."))
  (cond ((null prems) nil)
	((subsetp (pdsn~hyps (car prems)) hyps)
	 (cons (car prems) (batac=get-original-premises hyps (cdr prems))))
	(t (batac=get-original-premises hyps (cdr prems)))))

;; End Expansion

(com~defcommand ore**
  (argnames goal disjuntions)
  (argtypes ndline ndline-list)
  (arghelps "A goal to prove" "A list of disjunctions")
  (function batac=ore**)
  (defaults batac=ore**-defaults)
  (frag-cats tactics base-connective)
  (log-p T)
  (help "Make a case split on a list of disjunctions."))

(defun batac=ore** (goal disj)
  (infer~compute-outline 'ore** (list goal disj) nil))

(defun batac=ore**-defaults (goal disj)
  (cond ((not (com~specified-arg-p goal))
	 (list (oc~default-current-planline) (com~unspecified)))
	((not (com~specified-arg-p disj))
	 (list goal
	       (mapcan #'(lambda (x)
			   (when (logic~disjunction-p (node~formula x))
			     (list x)))
		       (pds~node-supports goal))))
	(t (list goal disj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Island Wild Tactic
;;
;; P1 ... Pn
;; ---------
;;    c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defwild-tactic island-tactic
		      (outline-mappings (((nonexistent list) island-tactic-f)
					 ((existent list) island-tactic-a)))
		      (parameter-types term)
		      (expansion-function batac=expand-island-tactic)
		      (help "Place Island Step."))


(defun island-tactic-f (concs prems parameters)
  (declare (ignore concs))
  (values (list (first parameters)) nil))

(defun island-tactic-a (concs prems parameters)
  (declare (ignore concs prems parameters))
  t)

(com~defcommand island-tactic
  (argnames conc prems param)
  (argtypes ndline ndline-list term)
  (arghelps "Conclusion of step" "Premises of step" "Formula of Conclusion")
  (function batac=island-tactic)
  (frag-cats tactics base)
  (log-p T)
  (help "Inserts an Island Step."))

(defun batac=island-tactic (conc prems param)
  (cond ((not (null conc))
	 (infer~compute-outline 'island-tactic (list conc prems) (list param)))
	((not (null param))
	 (infer~compute-outline 'island-tactic (list nil prems) (list param)))
	(t
	 (omega~error "Something wrong in batac=island"))))

(defun batac=expand-island-tactic (conclusions premises parameters)
  (let* ((conc (first conclusions))
	 (prems (just~premises (node~justification conc))))
    (tacl~init (append conclusions premises))
    (pds~add-sponsors conc prems)
    (setf (pds~open-nodes omega*current-proof-plan)
	  (cons conc (pds~open-nodes omega*current-proof-plan)))
    (setf (pds~agenda omega*current-proof-plan)
	  (agenda~create (agenda~create-goal conc) nil nil (pds~agenda omega*current-proof-plan)))
    (tacl~end)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; That introduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(infer~deftactic thatI 
		 (outline-mappings (((existent nonexistent nonexistent) thatI-b)
				    ((existent existent existent) thatI-a)
				    ))
		 (parameter-types)
		 (expansion-function batac=expand-thatI)
		 (help "Elimination of the first description operator THAT."))

(tac~deftactic thatI-b thatI (in base)
	       (premises P1 P2)
	       (conclusions C)
	       (computations (P1 (batac=that-eu-b (formula C)))
			     (P2 (batac=that-imp-b (formula C))))
	       (sideconditions (batac=that-position (formula C)))
	       (description "Elimination of the first description operator."))

(tac~deftactic thatI-a thatI (in base)
	       (premises P1 P2)
	       (conclusions C)
	       (sideconditions (batac=that-i-check (formula C) (formula P1) (formula P2)))
	       (description "Elimination of the first description operator."))

(defun batac=that-i-check (that-term exists-term implication)
  (when (batac=that-position that-term)
    (and (lam~equal-p exists-term (batac=that-eu-b that-term))
	 (lam~equal-p implication (batac=that-imp-b that-term)))))

(defun batac=that-position (that-term)
  (let* ((the-that (data~schema-range (env~lookup-object :that (pds~environment omega*current-proof-plan))))
	 (positions (data~positions
		    that-term
		    #'(lambda (x) (and (data~appl-p x) (data~equal the-that (data~appl-function x)))))))
    (when positions
      (first positions))))

(defun batac=that-eu-b (form)
  (let* ((exp (beta~expand form (list (batac=that-position form))))
	 (set (first (data~appl-arguments (first (data~appl-arguments exp))))))
    (term~appl-create (env~lookup-object :exists-unique (pds~environment omega*current-proof-plan))
		 (list set))))

(defun batac=that-imp-b (form)
  (let* ((exp (beta~expand form (list (batac=that-position form))))
	 (context (data~alpha-copy (data~appl-function exp) nil))
	 (set (first (data~appl-arguments (first (data~appl-arguments exp)))))
	 (var (data~abstr-bound-var context)))
    (beta~normalize
     (term~appl-create (env~lookup-object :forall (pds~environment omega*current-proof-plan))
		       (list (term~abstr-create
			      (list var) 
			      (term~appl-create (env~lookup-object :implies (pds~environment omega*current-proof-plan))
						(list (term~appl-create set (list var))
						      (data~abstr-scope context)))))))))

(com~defcommand thati
  (argnames thati-line)
  (argtypes ndline)
  (arghelps "A line with a description operator.")
  (function batac=thati)
  (frag-cats tactics base)
  (defaults batac=thati-defaults) 
  (log-p T)
  (help "Eliminating the first description operator THAT"))

(defun batac=thati (P)
  (infer~compute-outline 'thati (list P nil nil) nil))

(defun batac=thati-defaults (line)
  (if (not (com~specified-arg-p line))
      (let ((that (env~lookup-object :that (th~env 'base))))
	(list (pds~find-open-node #'(lambda (x) (data~substruct-p x that #'data~schema-equal)))))
    (list line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; That Elimination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(infer~deftactic thatE
		 (outline-mappings (((nonexistent nonexistent existent) thatE-f)
				    ((existent existent existent) thatE-a)
				    ))
		 (parameter-types)
		 (expansion-function batac=expand-thatE)
		 (help "Elimination of the first description operator THAT."))

(tac~deftactic thatE-f thatE (in base)
	       (premises P1 P)
	       (conclusions C2)
	       (hypotheses (H P1))
	       (computations (H (batac=that-hyp-b (formula P)))
			     (P1 (batac=that-all-b (formula P)))
			     (C2 (batac=that-imp-b (formula P))))
	       (sideconditions (batac=that-position (formula P)))
	       (description "Elimination of the first description operator."))

(tac~deftactic thatE-a thatE (in base)
	       (premises P1 P)
	       (conclusions C2)
	       (hypotheses (H P1))
	       (sideconditions (batac=that-e-check (formula P) (formula P1) (formula C2) (hyps P1)))
	       (description "Elimination of the first description operator."))


(let ((new-constant nil)) ;; to store the new constant for both the hypotheses and the new premises

  (defun batac=that-hyp-b (form)
    (let* ((exp (beta~expand form (list (batac=that-position form))))
	   (context (first (data~appl-arguments (first (data~appl-arguments exp)))))
	   (env (pds~environment omega*current-proof-plan))
	   (var (data~abstr-bound-var context))
	   (new-cons (term~generate-term-primitive-with-new-name 'c (term~type var) 'term+constant env)))
      (setf new-constant new-cons)
      (beta~normalize (term~appl-create context (list new-cons)))))

  (defun batac=that-all-b (form)
    (let* ((exp (beta~expand form (list (batac=that-position form))))
	   (context (first (data~appl-arguments (first (data~appl-arguments exp)))))
	   (env (pds~environment omega*current-proof-plan))
	   (var (data~abstr-bound-var context))
	   (new-var (term~generate-term-primitive-with-new-name (keim~name var) (term~type var) 'term+variable env))
	   (equality (term~appl-create (env~lookup-object := env) (list new-var new-constant)))
	   (pred2 (term~appl-create (env~lookup-object :implies env)
				    (list (beta~normalize (term~appl-create context (list new-var))) equality))))
      (term~appl-create (env~lookup-object :forall env) (list (term~abstr-create (list new-var) pred2)))))

  )

#+old(defun batac=that-ex-b (form)
  (let* ((exp (beta~expand form (list (batac=that-position form))))
	 (context (first (data~appl-arguments (first (data~appl-arguments exp)))))
	 (env (pds~environment omega*current-proof-plan))
	 (var (data~abstr-bound-var context))
	 (set (data~abstr-scope context))
	 (new-var (term~generate-term-primitive-with-new-name (keim~name var) (term~type var) 'term+variable env))
	 (equality (term~appl-create (env~lookup-object := env) (list new-var var)))
	 (pred2 (term~appl-create (env~lookup-object :implies env)
				  (list (beta~normalize (term~appl-create context (list new-var))) equality)))
	 (all-term (term~appl-create (env~lookup-object :forall env) (list (term~abstr-create (list new-var) pred2))))
	 (pred1 (term~appl-create (env~lookup-object :implies env) (list set all-term))))
    (term~appl-create (env~lookup-object :exists env) (list (term~abstr-create (list var) pred1)))))

(defun batac=that-e-check (that-term forall-term implication hyp-list)
  (let ((hyp (potac=that-hyp-b that-term)))
    (when (batac=that-position that-term)
      (and (lam~equal-p forall-term (batac=that-all-b that-term))
	   (lam~equal-p implication (batac=that-imp-b that-term))
	   (some #'(lambda (h) (term~alpha-equal hyp (node~formula h))) hyp-list)))))

(com~defcommand thatE 
  (argnames thatE-line)
  (argtypes ndline)
  (arghelps "A line with a description operator.")
  (function batac=thatE)
  (frag-cats tactics base)
  (defaults batac=thate-defaults) 
  (log-p T)
  (help "Eliminating the first description operator THAT"))

(defun batac=thatE (P)
  (infer~compute-outline 'thate (list nil nil P) nil))


(defun batac=thate-defaults (line)
  (if (not (com~specified-arg-p line))
      (let ((that (env~lookup-object :that (th~env 'base))))
	(list (pds~find-support #'(lambda (x) (data~substruct-p x that #'data~schema-equal)))))
    (list line)))


;; assert -> now in post-tactics.thy (because it uses sorts)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import-definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic import-definition
		 (outline-mappings (((nonexistent) import-definition-f)))
		 (parameter-types term)
		 (expansion-function batac=expand-import-definition))


(tac~deftactic import-definition-f import-definition
	       (in base)
	       (parameters (c term+term "a constant"))
               (premises )
               (conclusions L1)
	       (sideconditions (batac=definied-constant-p c))
               (computations (L1 (batac=compute-def-equivalence c))) 
               (description "Application of pushnegation rule."))

(defun batac=definied-constant-p (term)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (const (cond ((term~constant-p term)
		       term)
		      ((and (term~schema-p term)
			    (term~constant-p (data~schema-range term)))
		       (data~schema-range term))
		      (t
		       nil))))
    (if const
	(let* ((current-definitions
		(th~definitions-recursed (prob~proof-theory omega*current-proof-plan)))
	       (matching-definition
		(first (remove-if-not #'(lambda (def)
					  (let* ((def-const (th~definition-constant def)))
					    (if (or (and (term~constant-p def-const)
							 (keim~equal def-const const))
						    (and (term~schema-p def-const)
							 (term~constant-p (data~schema-range def-const))
							 (keim~equal (data~schema-range def-const) const)))
						't
					      nil)))
				      current-definitions))))
	  (if matching-definition
	      't
	    nil))
      nil)))

(defun batac=compute-def-equivalence (term)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (const (cond ((term~constant-p term)
		       term)
		      ((and (term~schema-p term)
			    (term~constant-p (data~schema-range term)))
		       (data~schema-range term))
		      (t
		       nil)))
	 (current-definitions
	  (th~definitions-recursed (prob~proof-theory omega*current-proof-plan)))
	 (matching-definition
	  (first (remove-if-not #'(lambda (def)
				    (let* ((def-const (th~definition-constant def)))
				      (if (or (and (term~constant-p def-const)
						   (keim~equal def-const const))
					      (and (term~schema-p def-const)
						   (term~constant-p (data~schema-range def-const))
						   (keim~equal (data~schema-range def-const) const)))
					  't
					nil)))
				current-definitions))))
    	  
    (if matching-definition
	(let* ((def-node (th~ass-node matching-definition))
	       (def-const (th~definition-constant matching-definition))
	       (copy-of-node (keim~copy def-node))
	       (kappas (if (term~schema-p copy-of-node)
			   (data~schema-domain copy-of-node)
			 nil))
	       (kappa-range (if (data~schema-p copy-of-node)
				(data~schema-range copy-of-node)
			      copy-of-node))
	       (lambda-vars (if (term~abstr-p kappa-range)
				(data~abstr-domain kappa-range)
			      nil))
	       (lambda-range (if (term~abstr-p kappa-range)
				 (data~abstr-range kappa-range)
			       kappa-range))
	       (equivalence (term~appl-create (env~lookup-object 'equiv env)
					      (list (if lambda-vars
							(term~appl-create def-const lambda-vars)
						      const)
						    lambda-range)))
	       (forall? (if lambda-vars
			    (do* ((current-formula equivalence)
				  (rest-vars (reverse lambda-vars) (rest rest-vars)))
				((null rest-vars)
				 current-formula)
			      (let* ((head-var (first rest-vars)))
				(setf current-formula
				      (term~appl-create (env~lookup-object 'forall env)
							(list (term~abstr-create (list head-var)
										 current-formula))))))
			  equivalence))
	       (kappa? (if kappas
			   (term~schema-create forall? :kappas kappas)
			 forall?)))
	  kappa?)
      (omega~error "There shoudl be a matching definition in function  batac=compute-def-equivalence!"))))


(com~defcommand import-definition
  (argnames definition)
  (argtypes term)
  (arghelps "A constant that is defined.")
  (function batac=import-definition)
  (frag-cats tactics base)
  (defaults )
  (log-p T)
  (help "Import of an definition."))

(defun batac=import-definition (P)
  (infer~compute-outline 'import-definition (list nil) (list P)))

						    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import-definition-expansion-direction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic import-definition-expansion-direction
		 (outline-mappings (((nonexistent) import-definition-expansion-direction-f)))
		 (parameter-types term)
		 (expansion-function batac=expand-import-definition-expansion-direction))


(tac~deftactic import-definition-expansion-direction-f import-definition-expansion-direction
	       (in base)
	       (parameters (c term+term "a constant"))
               (premises )
               (conclusions L1)
	       (sideconditions (batac=definied-constant-p c))
               (computations (L1 (batac=compute-def-implies-expansion c))) 
               (description "Application of pushnegation rule."))
						  

(defun batac=compute-def-implies-expansion (term)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (const (cond ((term~constant-p term)
		       term)
		      ((and (term~schema-p term)
			    (term~constant-p (data~schema-range term)))
		       (data~schema-range term))
		      (t
		       nil)))
	 (current-definitions
	  (th~definitions-recursed (prob~proof-theory omega*current-proof-plan)))
	 (matching-definition
	  (first (remove-if-not #'(lambda (def)
				    (let* ((def-const (th~definition-constant def)))
				      (if (or (and (term~constant-p def-const)
						   (keim~equal def-const const))
					      (and (term~schema-p def-const)
						   (term~constant-p (data~schema-range def-const))
						   (keim~equal (data~schema-range def-const) const)))
					  't
					nil)))
				current-definitions))))
    	  
    (if matching-definition
	(let* ((def-node (th~ass-node matching-definition))
	       (def-const (th~definition-constant matching-definition))
	       (copy-of-node (keim~copy def-node))
	       (kappas (if (term~schema-p copy-of-node)
			   (data~schema-domain copy-of-node)
			 nil))
	       (kappa-range (if (data~schema-p copy-of-node)
				(data~schema-range copy-of-node)
			      copy-of-node))
	       (lambda-vars (if (term~abstr-p kappa-range)
				(data~abstr-domain kappa-range)
			      nil))
	       (lambda-range (if (term~abstr-p kappa-range)
				 (data~abstr-range kappa-range)
			       kappa-range))
	       (equivalence (term~appl-create (env~lookup-object 'implies env)
					      (list (if lambda-vars
							(term~appl-create def-const lambda-vars)
						      const)
						    lambda-range)))
	       (forall? (if lambda-vars
			    (do* ((current-formula equivalence)
				  (rest-vars (reverse lambda-vars) (rest rest-vars)))
				((null rest-vars)
				 current-formula)
			      (let* ((head-var (first rest-vars)))
				(setf current-formula
				      (term~appl-create (env~lookup-object 'forall env)
							(list (term~abstr-create (list head-var)
										 current-formula))))))
			  equivalence))
	       (kappa? (if kappas
			   (term~schema-create forall? :kappas kappas)
			 forall?)))
	  kappa?)
      (omega~error "There shoudl be a matching definition in function  batac=compute-def-equivalence!"))))


(com~defcommand import-definition-expansion-direction
  (argnames definition)
  (argtypes term)
  (arghelps "A constant that is defined.")
  (function batac=import-definition-expansion-direction)
  (frag-cats tactics base)
  (defaults )
  (log-p T)
  (help "Import of an definition."))

(defun batac=import-definition-expansion-direction (P)
  (infer~compute-outline 'import-definition-expansion-direction (list nil) (list P)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normalize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar batac*counter 0)
;; yes I know that's stupid, but I've just no Lisp manual at hand to read and check how
;; external variables are defined ...


(infer~defwild-tactic normalize
		      (outline-mappings (((existent) normalize-b)))
		      (parameter-types )
		      (expansion-function batac=normalize-b)
		      (help "Normalization of complex formula."))

(defun normalize-b (concs prems parameters)
  (declare (ignore parameters))
  (setq batac*counter 0)
  (let* ((conc (first concs))
	 (conc-hyps-pairs (batac=do-normalize conc)))
    (values nil
	    (mapcar #'(lambda (conc-hyps-pair)
			(cons (first conc-hyps-pair)
			      (second conc-hyps-pair)))
		    conc-hyps-pairs))))

(defun batac=do-normalize (formula)
  (let* ((env (th~env 'base)))
    (cond ((logic~conjunction-p formula)
	   (append (batac=do-normalize (first (data~appl-arguments formula)))
		   (batac=do-normalize (second (data~appl-arguments formula))))
	   )
	  ((logic~implication-p formula)
	   (let* ((conc-hyps-pairs (batac=do-normalize (second (data~appl-arguments formula))))
		  (counter (incf batac*counter)))
	     (mapcar #'(lambda (conc-hyp-pair)
			 (list (first conc-hyp-pair)
			       (cons (list (first (data~appl-arguments formula)) counter)
				     (second conc-hyp-pair))))
		     conc-hyps-pairs))
	   )
	  ((logic~equivalence-p formula)
	   (append (batac=do-normalize (term~appl-create (env~lookup-object 'implies env)
						      (data~appl-arguments formula)))
		   (batac=do-normalize (term~appl-create (env~lookup-object 'implies env)
						      (reverse (data~appl-arguments formula))))))
	  ((logic~universal-quantification-p formula)
	   (let* ((quantifier (data~appl-function formula))
		  (bound-var (logic~quantification-bound-variable formula))
		  (scope (logic~quantification-scope formula))
		  (const-sym (read-from-string (format nil "C~A" (keim~name bound-var))))
		  (new-const (post~read-object (list const-sym
						     (read-from-string (post~print (term~type bound-var) nil)))
					       (pds~environment omega*current-proof-plan)
					       :constant))
		  (scope-replace (data~replace-struct scope bound-var new-const))
		  (conc-hyps-pairs (batac=do-normalize scope-replace)))
	     
	     (if (string-equal (keim~name quantifier) 'forall-exists)
		 (let* ((sort (second (data~appl-arguments formula)))
			(hyp (term~appl-create sort (list new-const)))
			(counter (incf batac*counter)))
		   (mapcar #'(lambda (conc-hyp-pair)
			       (list (first conc-hyp-pair) counter)
			       (cons hyp (second conc-hyp-pair)))
			   conc-hyps-pairs))
	       conc-hyps-pairs)))
	  (t
	   (list (list formula nil))))))

(com~defcommand normalize
  (argnames goal)
  (argtypes ndline)
  (arghelps "A complex goal to prove")
  (function batac=normalize)
  (defaults )
  (frag-cats tactics base-connective)
  (log-p T)
  (help "Normalizes a complex goal."))

(defun batac=normalize (goal)
  (infer~compute-outline 'normalize (list goal) nil))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  Prenex Normalform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun batac~prenex-form (formula &optional (env (pds~environment omega*current-proof-plan)))
  (batac~pren-form (batac~prenex-expand-defs formula) env))


(defun batac~pren-form (formula &optional (env (pds~environment omega*current-proof-plan)))
  (declare (edited  "08.07.2006")
	   (authors Chris)
	   (input   "A formula, an environment")
	   (effect  "none")
	   (value   ""))
  (cond ((logic~atom-p formula) formula)
	((or (logic~universal-quantification-p formula) (logic~existential-quantification-p formula))
	 (term~appl-create
	  (data~appl-function formula)
	  (list 
	   (term~abstr-create
	    (list (logic~quantification-bound-variable formula))
	    (batac~pren-form (logic~quantification-scope formula) env)))))
	((logic~negation-p formula)
	 (if (or (logic~universal-quantification-p (first (data~appl-arguments formula)))
		 (logic~existential-quantification-p (first (data~appl-arguments formula))))
	     (batac~pren-form (pds~pushneg formula) env)
	   formula))
	((logic~falsity-constant-p formula) formula)
	((logic~truth-constant-p formula) formula)
	((logic~logical-p formula)
	 (if (or (logic~universal-quantification-p (first (data~appl-arguments formula)))
		 (logic~existential-quantification-p (first (data~appl-arguments formula)))
		 (logic~universal-quantification-p (second (data~appl-arguments formula)))
		 (logic~existential-quantification-p (second (data~appl-arguments formula))))
	     (batac~pren-form (batac=prenex-form (data~appl-function formula) (data~appl-arguments formula) env) env)
	   (let ((func (data~appl-function formula))
		 (pren-args (mapcar #'(lambda (X) (batac~pren-form X env)) (data~appl-arguments formula))))
	     (cond ((= (length pren-args) 2)
		    (if (and (term~alpha-equal (first pren-args) (first (data~appl-arguments formula)))
			     (term~alpha-equal (second pren-args) (second (data~appl-arguments formula))))
			formula
		      (batac~pren-form (term~appl-create func pren-args))))
		   ((= (length pren-args) 1)
		    (if (term~alpha-equal (first pren-args) (first (data~appl-arguments formula)))
			formula
		      (batac~pren-form (term~appl-create func pren-args))))
		   (t formula)))))

	   ;; formula))
	(t formula)))



(defun batac=prenex-form (head prenexargs env)
  (declare (edited  "08.07.2006")
	   (authors Chris)
	   (input   "a logical connectives, a list of terms in prenexform,  an environment")
	   (effect  "none")
	   (value   ""))
  (let ((firstarg (batac~pren-form (first prenexargs)))
	(secondarg (batac~pren-form (second prenexargs))))
    (cond ((logic~universal-quantification-p firstarg)
	   (let* ((firstarg-c (term~alpha-copy firstarg nil))
		  (firstarg-c-head (data~appl-function firstarg-c))
		  (firstarg-c-bound-var (data~abstr-bound-var (first (data~appl-arguments firstarg-c))))  
		  (firstarg-c-scope (data~abstr-scope (first (data~appl-arguments firstarg-c)))))
	     (cond ((or (logic~disjunction-connective-p head) (logic~conjunction-connective-p head))
		    (term~appl-create (env~lookup-object :forall env)
				      (list (term~abstr-create (list firstarg-c-bound-var)
							 (term~appl-create head (list firstarg-c-scope secondarg))))))
		   ((logic~implication-connective-p head)
		    (term~appl-create (env~lookup-object :exists env)
				      (list (term~abstr-create (list firstarg-c-bound-var)
							       (term~appl-create head (list firstarg-c-scope secondarg)))))))))
	  ((logic~existential-quantification-p firstarg)
	   (let* ((firstarg-c (term~alpha-copy firstarg nil))
		  (firstarg-c-head (data~appl-function firstarg-c))
		  (firstarg-c-bound-var (data~abstr-bound-var (first (data~appl-arguments firstarg-c))))  
		  (firstarg-c-scope (data~abstr-scope (first (data~appl-arguments firstarg-c)))))
	     (cond ((or (logic~disjunction-connective-p head) (logic~conjunction-connective-p head))
		    (term~appl-create (env~lookup-object :exists env)
				      (list (term~abstr-create (list firstarg-c-bound-var)
							       (term~appl-create head (list firstarg-c-scope secondarg))))))
		   ((logic~implication-connective-p head)
		    (term~appl-create (env~lookup-object :forall env)
				      (list (term~abstr-create (list firstarg-c-bound-var)
							       (term~appl-create head (list firstarg-c-scope secondarg)))))))))
	  ((logic~universal-quantification-p secondarg)
	   (let* ((secondarg-c (term~alpha-copy secondarg nil))
		  (secondarg-c-head (data~appl-function secondarg-c))
		  (secondarg-c-bound-var (data~abstr-bound-var (first (data~appl-arguments secondarg-c))))  
		  (secondarg-c-scope (data~abstr-scope (first (data~appl-arguments secondarg-c)))))
	     (cond ((or (logic~disjunction-connective-p head) (logic~conjunction-connective-p head))
		    (term~appl-create (env~lookup-object :forall env)
				      (list (term~abstr-create (list secondarg-c-bound-var)
							       (term~appl-create head (list firstarg secondarg-c-scope))))))
		   ((logic~implication-connective-p head)
		    (term~appl-create (env~lookup-object :forall env)
				      (list (term~abstr-create (list secondarg-c-bound-var)
							       (term~appl-create head (list firstarg secondarg-c-scope)))))))))
	  ((logic~existential-quantification-p secondarg)
	   (let* ((secondarg-c (term~alpha-copy secondarg nil))
		  (secondarg-c-head (data~appl-function secondarg-c))
		  (secondarg-c-bound-var (data~abstr-bound-var (first (data~appl-arguments secondarg-c))))  
		  (secondarg-c-scope (data~abstr-scope (first (data~appl-arguments secondarg-c)))))
	     (cond ((or (logic~disjunction-connective-p head) (logic~conjunction-connective-p head))
		    (term~appl-create (env~lookup-object :exists env)
				      (list (term~abstr-create (list secondarg-c-bound-var)
							       (term~appl-create head (list firstarg secondarg-c-scope))))))
		   ((logic~implication-connective-p head)
		    (term~appl-create (env~lookup-object :exists env)
				      (list (term~abstr-create (list secondarg-c-bound-var)
							       (term~appl-create head (list firstarg secondarg-c-scope))))))))))))









(defun batac~prenex-expand-defs (form &optional (defs (list (th~find-assumption 'equiv (prob~theory
									     omega*current-proof-plan)))))
  (batac=prenex-expand-defs form defs))
				      


(defgeneric  batac=prenex-expand-defs (obj defs)
  (:method ((obj term+term) (def th+def))
	   (batac=prenex-expand-def obj def))
  (:method ((obj term+term) (defs list))
	   (if defs 
	       (batac=prenex-expand-defs (batac=prenex-expand-defs obj (car defs)) (cdr defs))
	     obj)))
	   
   

(defun batac=prenex-expand-def (formula def)
  (let* (
	 (definiendum (th~definition-constant def))
	 (definiens (th~ass-node def))
	 (poslist (data~positions formula
				  #'(lambda (X) (data~schema-equal X definiendum)))))
    (let* ((expanded-form
	    (let ((form formula))
	      (dolist (pos (reverse poslist) form)
		(setq form (pds~replace-and-contract-at-position definiendum definiens form pos))))))
      expanded-form)))



