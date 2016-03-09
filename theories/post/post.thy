;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

(th~deftheory POST
	      (uses nic)
	      (constants )
	      (help "POST sorted higher order logic with partial"))


;; Definition of defined

(th~defconstant undefined
  (in post)
  (type (all-types aa aa))
  (help "The undefined element, which is an undefined constant in each type.")
  )



(th~defdef defined 
  (in post)
  (type-variables aa)
  (definition
    (lam (x aa) (not (= x undefined))))
  ;;;(sort)
  (help "The sort of welldefined terms.")
  )


(th~defaxiom not-defined-undefined
	     (in post)
	     (formula (not (defined undefined)))
	     (help "The error value is undefined"))


;; The sorted quantifiers

(th~defdef forall-defined
	   (in post)
	   (type-variables aa)
	   #+logic-new(connective-type universal-quantor)
           (definition
	     (lam (U (O aa))
		  (forall (lam (x aa) (implies (defined x) (U x))))))
	   (help "The sorted universal quantifier.
                  (forall-defined (lam (X aa) A)) is an abbreviation for (forall (lam (X aa) (implies (defined X) A)))"))

(th~defdef exists-defined
	   (in post)
	   (type-variables aa)
	   #+logic-new(connective-type existential-quantor)
           (definition
	     (lam (T (o aa))
		  (exists (lam (X aa)
			       (and (defined X) (T X))))))
	   (help "The sorted existential quantifier.
                  (exists-defined (lam (X aa) A)) is an abbreviation for (exists (lam (X aa) (and (defined X) A)))"))

(th~defdef forall-sort
	   (in post)
	   (type-variables aa)
	   #+logic-new(connective-type universal-quantor)
           (definition
             (lam (V (o aa))
                  (lam (U (O aa))
                       (forall (lam (x aa) (implies (U x) (V x)))))))
	   (help "The sorted universal quantifier.
                  (forall-sort (lam (X aa) A) S) is an abbreviation for (forall (lam (X aa) (implies S X) A))"))

(th~defdef exists-sort
	   (in post)
	   (type-variables aa)
	   #+logic-new(connective-type existential-quantor)
           (definition
	     (lam (T (o aa))
		  (lam (S (o aa))
		       (exists (lam (X aa)
				(and (S X) (T X)))))))
	   (help "The sorted existential quantifier.
                  (exists-sort (lam (X aa) A) S) is an abbreviation for (exists (lam (X aa) (and (S X) A)))"))

(th~defdef exists-unique-sort
           (in post)
	   (type-variables aa)
           (definition
	     (lam (T (o aa))
		  (lam (S (o aa))
		       (exists-unique (lam (X aa)
					   (and (S X) (T X)))))))
	   (help "The sorted quantifier for unique existence.
	          (exists-unique-sort (lam (X aa) A) S)
                  is an abbreviation for
                  (exists-unique (lam (X aa) (and (S X) A)))"))
                 
(th~defdef exists-sort-unique
           (in post)
	   (type-variables aa)
           (definition
	     (lam (T (o aa))
		  (lam (S (o aa))
		       (exists-sort (lam (X aa)
					 (and (T X)
					      (forall-sort (lam (Y aa) (implies (T Y) (= Y X))) S)))
				    S))))
	   (help "The sorted quantifier for unique existence defined via sorted versions of the exists and forall quantifier."))

(th~defdef fun-sort
  (in post)
  (type-variables cc bb)
  (definition
    (lam (Dom (o cc))
	 (lam (Im (o bb))
	      (lam (F (bb cc))
			  (forall-sort (lam (x cc) (Im (F x))) Dom))))) 
  (help "The functional sort with domain Dom and range Im.")
  )

;; A sort is a subsort of the set of defined terms

(th~defdef defined-subsort
  (in post)
  (definition (all-types aa
			 (lam (P (o aa))
			      (forall (lam (x aa) (implies (P x) (defined x)))))))
  (sort)
  (help "The sort of welldefined sorts."))

;; The correct tertium non datur

(th~defaxiom tnd
	     (in post)
	     (formula 
	      (forall (lam (X O)
			   (equiv (defined x) (or X (not X))))))
	     (help "The Axiom of the excluded middle."))


(th~defdef ifthen
	   (in post)
	   (type-variables bb)
           (definition
             (lam (cond o)
                  (lam (x bb)
                       (lam (y bb)
                            (that (lam (z bb)
                                       (or (and cond (= z x))
                                           (and (not cond) (= z y)))))))))
	   (help "The ifthen conditional defined by the that operator."))

(th~defdef when
	   (in post)
	   (type-variables bb)
           (definition
             (lam (cond o)
                  (lam (x bb)
		       (that (lam (z bb)
				  (and cond (= z x)))))))
	   (help "The ifthen conditional defined by the that operator."))


;; a sorted strict = 

(th~defdef equal
	   (in post)
	   (type-variables bb)
           (definition
             (lam (X bb)
                  (lam (Y bb)
		       (when (and (defined X)(defined Y))
			 (forall-sort (lam (P (o bb))
					   (implies (P X) (P Y))) defined)))))
		;  #+logic-new(connective-type equality)
	   (help "The equality constant defined by the Leibniz definition."))

;;; Axiomatisation of a three-valued logic

;truth-values

;; (th~defaxiom undefined-true
;;              (in post)
;;              (formula (not (= true undefined))))

;; (th~defaxiom undefined-false
;;              (in post)
;;              (formula (not (= false undefined))))

;; (th~defdef truth
;; 	   (in post)
;; 	   (sort)
;;            (definition
;;              (lam (X o)
;; 		  (= X true))))

;; (th~deftheorem subsort-truth-defined
;; 	       (in post)
;; 	       (termdecl)
;; 	       (conclusion
;; 		(forall-sort (lam (x o) (defined x)) truth)))

;; (th~deftheorem defined-true
;; 	       (in post)
;; 	       (termdecl)
;; 	       (conclusion
;; 		(defined true)))

;; (th~defdef falsity
;; 	   (in post)
;; 	   (sort)
;;            (definition
;;              (lam (X o)
;; 		  (= X false))))

;; (th~deftheorem subsort-falsity-defined
;; 	       (in post)
;; 	       (termdecl)
;; 	       (conclusion
;; 		(forall-sort (lam (x o) (defined x)) falsity)))

;; (th~deftheorem defined-false
;; 	       (in post)
;; 	       (termdecl)
;; 	       (conclusion
;; 		(defined false)))

;; ;not

;; (th~defaxiom undefined-not
;;              (in post)
;;              (formula (= (not undefined) undefined)))

;; (th~deftheorem defined-not
;; 	     (in post)
;; 	     (conclusion 
;; 	      (forall-sort (lam (x o)
;; 				(defined (not x))) defined))
;; 	     (termdecl)
;; 	     (help "Falsity is defined."))

;; ;and

;; (th~defaxiom undefined-and1
;;              (in post)
;;              (formula (= (and true undefined) undefined)))

;; (th~defaxiom undefined-and2
;;              (in post)
;;              (formula (= (and undefined true) undefined)))

;; (th~defaxiom undefined-and3
;;              (in post)
;;              (formula (= (and undefined undefined) undefined)))

;; (th~deftheorem defined-and
;; 	     (in post)
;; 	     (conclusion 
;; 	      (forall-sort (lam (x o)
;; 				(forall-sort (lam (y o)
;; 						  (defined (and x y))) defined)) defined))
;; 	     (termdecl)
;; 	     (help "And is defined."))

;; ;or

;; (th~defaxiom undefined-or1
;;              (in post)
;;              (formula (= (or false undefined) undefined)))

;; (th~defaxiom undefined-or2
;;              (in post)
;;              (formula (= (or undefined false) undefined)))

;; (th~defaxiom undefined-or3
;;              (in post)
;;              (formula (= (or undefined undefined) undefined)))


;; (th~deftheorem defined-or
;; 	     (in post)
;; 	     (conclusion 
;; 	      (forall-sort (lam (x o)
;; 				(forall-sort (lam (y o)
;; 						  (defined (or x y))) defined)) defined))
;; 	     (termdecl)
;; 	     (help "Or is defined."))

;; ;implies

;; (th~defaxiom undefined-implies1
;;              (in post)
;;              (formula (= (implies undefined false) undefined)))

;; (th~defaxiom undefined-implies2
;;              (in post)
;;              (formula (= (implies undefined undefined) undefined)))

;; (th~defaxiom undefined-implies3
;;              (in post)
;;              (formula (= (or true undefined) undefined)))

;; (th~deftheorem defined-implies
;; 	     (in post)
;; 	     (conclusion 
;; 	      (forall-sort (lam (x o)
;; 				(forall-sort (lam (y o)
;; 						  (defined (implies x y))) defined)) defined))
;; 	     (termdecl)
;; 	     (help "Implies is defined."))






