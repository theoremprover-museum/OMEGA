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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FORALLI-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ForallI-Sort-m"
		 (outline-mappings (((existent nonexistent) ForallI-Sort-m-b)))
		 (help "The method for FORALL-sort introduction"))


(meth~defmethod ForallI-Sort-m-b ForallI-Sort-m
		(in post)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o term)
		  (phi1 o term)
		  (sort1 (o aa) term)
		  (c aa const)
		  )
		 )
		(premises (+ l2))
		(conclusions (- l3))

		(outline-computations
		 (c (type-newconst aa))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist c)) phi))
		 )
		
		(decl-content
		 (l1 ()   (sort1 c)                                        ("Hyp" () ()))
		 (l2 (l1) phi1                                             ("Open" () ()))
		 (l3 ()   (forall-sort (lam (x aa var) phi) sort1)         ("ForallI-Sort" (c) (l2)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This method has the same effect as the backward application of the tactic ForallI-sort."))
		(remark ;local short, long
		("<U>ForallI-Sort:</U><BR>
We have to prove<BR>
<TAB><TERM>forall</TERM><TERM>(in x sort1)</TERM>><BR>
<TAB><TERM>phi</TERM>.<BR>
So we take an arbitrary <TERM>c</TERM> and have to show<BR>
<TAB><TERM>phi1</TERM><BR>
under the assumption <TERM>(in c sort1)</TERM>."
	       "")
	      ;global text, constr
		("We have to prove<BR>
<TAB><TERM>forall</TERM><TERM>(in x sort1)</TERM><BR>
<TAB><TERM>phi</TERM>.<BR>
So we take an arbitrary <TERM>c</TERM> and prove<BR>
<TAB><TERM>phi1</TERM><BR>
under the assumption <TERM>(in c sort1)</TERM>.<BR>
<LISP>(verbalize-text-next l2)</LISP>"
		 "We have to prove<BR>
<TAB><TERM>forall</TERM><TERM>(in x sort1)</TERM>><BR>
<TAB><TERM>phi</TERM>.<BR>
So we take an arbitrary <TERM>c</TERM> and prove<BR>
<TAB><TERM>phi1</TERM><BR>
under the assumption <TERM>(in c sort1)</TERM>.<BR>
<LISP>(verbalize-cons-next l2)</LISP>")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;ExistsI-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ExistsI-Sort-m"
		 (outline-mappings (((existent nonexistent nonexistent) ExistsI-Sort-m-b)))
		 (help "The method for Exists-sort introduction"))

(meth~defmethod ExistsI-Sort-m-b ExistsI-Sort-m
		(in post)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o)
		  (pos o poslist)
		  (phi1 o)
		  (sort1 (o aa))
		  (mv aa metavar)
		  )
		 )
		
		(premises (+ l1) (+ l2))
		(conclusions (- l3))
		
		(application-condition
		 (termoccs x phi pos))
		(outline-computations
		 (mv (newmetavar (:symbol :M) aa))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist mv)) phi))
		 )
		
		(decl-content
		 (l1 ()   (sort1 mv)                                       ("Open" () ()))   
		 (l2 ()   phi1                                             ("Open" () ()))
		 (l3 ()   (exists-sort (lam (x aa var) phi) sort1)    ("ExistsI-Sort" (mv pos) (l1 l2)))
		 )

		;;(outline-orderings (before l2 l1))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-sort"))
		(remark ;local short, long
		("<U>ExistsI-Sort:</U><BR>
We have to prove<BR>
<TAB><TERM>exists</TERM><TERM>(in x sort1)</TERM><BR>
<TAB><TERM>phi</TERM>.<BR>
So we have to find a term <TERM>mv</TERM> such that<BR>
<TAB><TERM>phi1</TERM><BR>
and <TERM>(in mv sort1)</TERM> holds."
	       "")
	      ;global text, constr
		("For <TERM>mv</TERM> we show that<BR>
<TAB><TERM>phi1</TERM><BR>
and <TERM>(in mv sort1)</TERM> holds.<BR>
<LISP>(verbalize-text-next l2)</LISP>"
		 "For <TERM>mv</TERM> we show that<BR>
<TAB><TERM>phi1</TERM><BR>
and <TERM>(in mv sort1)</TERM> holds.<BR>
<LISP>(verbalize-text-next l2)</LISP>")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FORALLE-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Foralle-Sort-m"
		 (outline-mappings (((nonexistent existent nonexistent) Foralle-Sort-m-f)))
		 (help "The method for FORALL-sort introduction"))


(meth~defmethod Foralle-Sort-m-f Foralle-Sort-m
		(in post)
		(rating 0)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o term)
		  (phi1 o term)
		  (sort1 (o aa) term)
		  (mv aa metavar)
		  )
		 )
		(premises  (- l2)(+ l1))
		(conclusions (+ l3))

		(outline-computations
		 (mv (newmetavar (:symbol :M) aa))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist mv)) phi))
		 )
		
		(decl-content
		 (l1 ()   (sort1 mv)                                        ("open" () ()))
 		 (l2 ()   (forall-sort (lam (x aa var) phi) sort1))
		 (l3 ()   phi1                                              ("Foralle-Sort" (mv) (l2 l1)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic Foralle-sort."))
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EXISTSE-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "Existse-Sort-m"
		 (outline-mappings (((existent existent nonexistent) Existse-Sort-m-f)))
		 (help "The method for EXISTS-sort eleminiation"))


(meth~defmethod Existse-Sort-m-f Existse-Sort-m
		(in post)
		(rating 0)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o term)
		  (psi o term)
		  (phi1 o term)
		  (sort1 (o aa) term)
		  (c aa const)
		  )
		 )
		(premises (- l10) (+ l40))
						     
		(conclusions (- l50))

		(outline-computations
		 (c (type-newconst aa))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist c)) phi))
		 )
		
		(decl-content
		 (l00 ()   (and (sort1 c) phi1)                             ("Hyp" () ()))
		 (l10 ()   (exists-sort (lam (x aa var) phi) sort1))        
		 (l40 (l00) psi                                             ("open"  ()()))    
		 (l50 ()    psi                                             ("Existse-Sort" (c) (l10 l40)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the forward application
                                       of the tactic Existse-sort."))
		)


;;*******************************************************
;;*    Methods for definition expansion                 *
;;*******************************************************

;;;;DefnI-m
;(infer~defmethod "DefnI-m"
;                 (outline-mappings (((existent nonexistent) DefnI-m)))
;                 (help "Reduction of an atom by substituting the predicate symbol by its the theory definition."))
;
;;;;was def-intro
;(meth~defmethod DefnI-m
;                DefnI-m
;                (in post)
;                (rating 1)
;                (reasoning :planning  :middle-out)
;                (declarations
;                 ;;(type-variables aa)
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (phi o) (tL list termlist) (thing (o list) const)
;                  )
;                 )
;                (premises (+ l1))
;                (application-condition
;                 (th-restricted-definition (:symbol :post) thing thing-def))
;;;;                 (th-definition thing thing-def))  ;;; too general especially for definitions in post-theory
;                (outline-computations
;                 (phi (beta-normalize (:term (thing-def tL))))
;                 )
;                (conclusions (- l2))
;                (decl-content
;                 (l1 () phi            ("Open" () ()))
;                 (l2 () (thing tL)     ("DefnI" (thing thing-def) (l1)))
;                 )
;                (proc-content schema-interpreter)
;                (remark "");Backward application of a theory definition.)
;                )
;;
;;;;;;DefnE-m
;(infer~defmethod "DefnE-m"
;                 (outline-mappings (((nonexistent closed) DefnE-m)))
;                 (help "Unfolding an atom by applying the theory definition of its predicate symbol."))
;
;;;;was def-intro
;(meth~defmethod DefnE-m
;                DefnE-m
;                (in post)
;                (rating 0)
;                (reasoning :planning :middle-out)
;                (declarations
;                 ;;(type-variables aa)
;                 (sorted-meta-variables
;                  ;;(meta-var typ sort)
;                  (phi o) (tL list termlist) (thing (o list) const)
;                  )
;                 )
;                (premises (- l1))
;                (application-condition
;                 (th-restricted-definition (:symbol :post) thing thing-def))
;;;;                 (th-definition thing thing-def))  ;;; to general especially for definitions in post-theory
;                (outline-computations
;                 (phi (beta-normalize (:term (thing-def tL))))
;                 )
;                (conclusions (+ l2))
;                (decl-content
;                 (l1 () (thing tL)         )
;                 (l2 () phi            ("DefnE" (thing thing-def) (l1)))
;                 
;                 )
;                (proc-content schema-interpreter)
;                (remark "");Forward application of a theory definition.)
;                )
;
;
;
;
;
;
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EXISTSE-SORT*-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defmethod "Existse-Sort*-m"
		 (outline-mappings (((existent existent nonexistent) Existse-Sort*-m-b)))
		 (parameter-types term-list)
		 (help "The method for EXISTS-sort* eleminiation"))


(meth~defmethod Existse-Sort*-m-b Existse-Sort*-m
		(in post)
		(rating 0)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (exsort-formula o term) (psi o term)
		  (new-consts list termlist)
		  (hyps o prlnlist)
		  (andes o prlnlist)
		  )
		 )
		
		(parameters new-consts)
		
		(premises (- l1) (+ l11))						     
		(conclusions (- l12))
		
		(application-condition
		 (mexsort-formula-p exsort-formula))
		
		(outline-computations
		 (hyps (mcompute-hyps-for-exsort exsort-formula new-consts))
		 (andes (mcompute-andes-for-exsort hyps))
		 (sponsors (mcompute-sponsors-for-exsort andes))
		 )
		
		(outline-actions 
		 (l11 (sponsor sponsors)))
		
		(decl-content
		 (l1  ()      exsort-formula          )
		 (l11 (hyps)  psi                     ("Open" () ()))
		 (l12 ()      psi                     ("Existse-Sort*" (new-consts) (l1 l11)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the existse-sort* tactic."))
		)

(com~defcommand mexistse-sort*
  (argnames concline exline subgoal parameter)
  (argtypes ndline ndline ndline termsym-list)
  (arghelps "Conclusion Line." "An existentially quanitified line" "Subgoal Line." "Termsym List.")
  (function potac=mexistse-sort*)
  (frag-cats tactics post)
  (defaults potac=mexistse-sort*-defaults)
  (log-p T)
  (help "Apply a series of Exists-Sort-Elminations."))

(defun potac=mexistse-sort* (C exline P param)
  (plan~step-plan-with (meth~find-method 'Existse-Sort*-m-b)
		       C
		       (list exline)
		       (list param)))

(defun potac=mexistse-sort*-defaults (conc-line ex-line subgoal-line consts)
  (cond ((not (com~specified-arg-p conc-line))
	 (list (oc~default-current-planline)
	       (com~unspecified)
	       (com~unspecified)
	       (com~unspecified)))
	((not (com~specified-arg-p ex-line))
	 (list conc-line
	       (pds~find-support #'potac=exists-sort-formula?)
	       (com~unspecified)
	       (com~unspecified)))
	((not (com~specified-arg-p subgoal-line))
	 (list conc-line 
	       ex-line
	       (oc~nil-argument)
	       (com~unspecified)))
	((not (com~specified-arg-p consts))
	 (list conc-line 
	       ex-line
	       subgoal-line
	       (batac=generate-defaults-existse* ex-line)))
	(t
	 (list conc-line ex-line subgoal-line consts))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;wellsorted-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond wellsorted-premparam (args cmapp)
	      (declare)
	      (let* ((goal (car args))
		     (prems (second args))
		     (newprems (third args))
		     (thms (fourth args))
		     (result (potac=wellsorted-check-and-return-theorems goal prems)))
		(cond ((and (car result)(cadr result))
		       (meth~mapp-extend-mapp cmapp newprems (car result))
		       (meth~mapp-extend-mapp cmapp thms (cadr result)))
		      ((car result)
		       (meth~mapp-extend-mapp cmapp newprems (car result)))
		      ((cadr result)
		       (meth~mapp-extend-mapp cmapp thms (cadr result)))
		      (T (meth~mapp-new-constraint cmapp NIL)))))

(meth~new-relational-function 'wellsorted-premparam)

(meth~deffun wellsorted-prepara (goal prems)
	      (declare)
	      (potac=wellsorted-check-and-return-theorems goal prems))

(infer~defmethod "wellsorted-m"
		 (outline-mappings (((existent) wellsorted-m-c))) ;;other cases!
		 (help "The method sort-computation"))


(meth~defmethod wellsorted-m-c wellsorted-m
		(in post)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (psi o term)
		  (new-consts list termlist)
		  (prems o prlnlist)
		  (thms o)
		  )
		 )
		
		(premises )
		(conclusions (- l11))
		
		(application-condition
		 (wellsorted-premparam l11 (mnil) prems thms))
		
		(decl-content
		 (l11 ()  psi                     ("wellsorted" (thms) ()))
		 )
		
		(proc-content schema-interpreter)
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;eval annotated constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond an-contain? (args cmapp)
	      (declare)
	      (let* ((goal (car args)))
		(meth~mapp-new-constraint cmapp (if (some #'(lambda (term)
							      (or (term~special-p term)
								  (term~annotated-term-p term)))
							  (data~all-substructs goal)) t nil))))


(meth~defcond an-eval-formula (args cmapp)
	      (declare)
	      (let* ((goal (car args))
		     (line (second args))
		     (prems (third args))
		     (result (repr~check goal)))
		  (if (and (= (length result) 1)(term~alpha-equal goal (car result)))
		      (meth~mapp-new-constraint cmapp NIL)
		    (progn
                      (meth~mapp-extend-mapp cmapp prems result)
		      (meth~mapp-new-constraint cmapp T)))))

(meth~deffun an-eval-prems (goal line premises)
	     (declare )
	     (mapcar #'(lambda (form)
			 (pdsn~open-node-create form
						(pdsn~hyps line)
						(pds~new-node-name omega*current-proof-plan)))
				(remove-duplicates premises :test #'term~alpha-equal)))
	     

(meth~new-relational-function 'an-eval-formula)

(infer~defmethod "an-eval-m"
		 (outline-mappings (((existent nonexistent) an-eval-m-b))) 
		 (help ""))

(meth~defmethod an-eval-m-b an-eval-m
		(in post)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (psi o term)
		  (prems o prlnlist)
		  (premformulas o termlist)
		  )
		 )
		
		(premises (+ prems))
		(conclusions (- l11))
		
		(application-condition
		 (mand
		  ;(mnot (existential-p psi))(mnot (universal-p psi))
		  (an-contain? psi)
		  (an-eval-formula psi l11 termlist)))

		(outline-computations
		 (prems (an-eval-prems psi l11 termlist)))
		
		(decl-content
		 (l11 ()  psi                     ("an-eval" () prems))
		 )
		
		(proc-content schema-interpreter)
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;check properties for annotated constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond an-check-formula (args cmapp)
	      (declare)
	      (let* ((goal (car args)))
		(meth~mapp-new-constraint cmapp (if (repr~check goal) t nil))))

(infer~defmethod "an-check-m"
		 (outline-mappings (((existent) an-check-m-c))) 
		 (help ""))

(meth~defmethod an-check-m-c an-check-m
		(in post)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (psi o term)
		  )
		 )
		
		(conclusions (- l11))
		
		(application-condition
		 (mand 		  (mnot (existential-p psi))(mnot (universal-p psi))
				  (an-contain? psi)
				  (an-check-formula psi)))
		
		(decl-content
		 (l11 ()  psi                     ("an-check" () ()))
		 )
		
		(proc-content schema-interpreter)
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;check negated properties for annotated constants in premises (false implies everything)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond an-check-not-formula (args cmapp)
	      (declare)
	      (let* ((goal (car args)))
		(meth~mapp-new-constraint cmapp (if (repr~check-not goal) t nil))))

(infer~defmethod "an-check-not-prem-m"
		 (outline-mappings (((existent existent) an-check-not-prem-m-c))) 
		 (help ""))

(meth~defmethod an-check-not-prem-m-c an-check-not-prem-m
		(in post)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o term)
		  (psi o term)
		  )
		 )

		(premises l10)
		(conclusions (- l11))
		
		(application-condition
		 (mand 		  (mnot (existential-p psi))(mnot (universal-p psi))
				  (an-contain? psi)
				  (an-check-not-formula psi)))
		
		(decl-content
		 (l10 ()  psi)
		 (l11 ()  phi                     ("an-check-not-prem" () (l10)))
		 )
		
		(proc-content schema-interpreter)
		)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eval-function-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond eval-function-application (args cmapp)
  (declare (edited  "24-JUL-2002")
	   (authors Pollet)
	   (input   "A term, a position, an unbound var for a new mv and an unbound var for the result.")
	   (effect  "-")
	   (value   "Returns true if the term contains the application of a function to an element"
		    "that can be evaluated, the vars will be bound to the evaluated term."))
  (let* ((term (first args))
	 (ifthen (env~lookup-object 'ifthen (th~env 'post)))
	 (positions  (remove-if #'(lambda (pos)
				    (or (data~abstr-p (data~struct-at-position term (pos~butlast (pos~butlast pos))))
					(and (data~appl-p (data~struct-at-position term (pos~butlast (pos~butlast pos))))
					     (data~schema-equal ifthen (data~appl-function (data~struct-at-position
											    term
											    (pos~butlast (pos~butlast pos))))))))
		      (data~substruct-positions ifthen 
					       term :test #'data~schema-equal)))
	 (new (third args)))
;    (omega~trace "pos ~A" positions)
    (if positions
       (let ((result (reduce #'(lambda (trm pos)
				  (let ((value (peme=apply-func (data~struct-at-position trm (pos~butlast pos)))))
				    (if value
					(data~replace-at-position trm (pos~butlast pos) value)
				      trm)))
			      positions :initial-value term)))
	 (if (data~equal result term)
	     (meth~mapp-new-constraint cmapp nil)
	   (meth~mapping-create (meth~mapp-subst cmapp)
				(meth~mapp-mapp cmapp)
				(mapp~create (list new)(list result))
				T)))
     (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'eval-function-application)

(defun peme=apply-func (term)
  (unless (or (data~all-variables term)
	      (if (crihelp=applfunc-equals 'ifthen term)
		  (not (every #'term~number-p (data~appl-arguments (first (data~appl-arguments term)))))
		 nil))
    (if (crihelp=applfunc-equals 'ifthen term)
	(if (natac=ensure-arith-correctness (first (data~appl-arguments term)))
	    (second (data~appl-arguments term))
	  (peme=apply-func (third (data~appl-arguments term))))
      term)))
	  
(infer~defmethod eval-function-m
		 (outline-mappings (((existent nonexistent) eval-function-m-b)))
;		 (parameter-types position)
		 (help "Eval application of functions in terms."))

(meth~defmethod eval-function-m-b eval-function-m
		(in post)
		(rating 10)
		(reasoning :planning)

		(declarations
		 (sorted-meta-variables
		  (phi o term)
		  (phiprime o term)
		  (position o pos)))
		
		(premises (+ l0))

		(application-condition
		 (eval-function-application phi position phiprime))
		  
		(outline-computations)
		 
		(conclusions (- l1))

		(decl-content
		 (l0 () phiprime  ("open" () ()))
		 (l1 () phi  ("eval-function" () (l0)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods evaluates function applications."))
		)

