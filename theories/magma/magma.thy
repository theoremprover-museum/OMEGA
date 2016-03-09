;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

(th~deftheory magma
              (uses integer morphism)
	      (help "Magma (groupoid) theory"))



;; Concept of order! Defined in the same way as cardinality 

(th~defdef order
	   (in magma)
	   (type-variables bb)
           (definition
             (lam (G (o bb))
                  (that (lam (x num)
                             (and (in x Nat)
                                  (exists (lam (F (num bb))
                                               (bijective G (first-n-nats x) F))))))))
           (help "Definition of the finite order of sets."))

;;; Formalisation of a^n with respect to an operation

(th~defconstant power-of-operation
  (in magma)
  (type (all-types AA (aa aa num (aa aa aa))))
  (help "The power-operation induced by an operation."))


(th~defaxiom the-power-of-an-operation-1
	     (in magma)
	     (formula (all-types aa
			  (forall (lam (op (aa aa aa))
			  (forall (lam (x aa)
				       (and
					(= (power-of-operation op 1 x) x)
					(forall-sort (lam (n num)
							  (= (power-of-operation op (s n) x)
							     (power-of-operation op n (op x x)))) pos-nat))))))))
	     (help "Inductive axiomatic definition of power-of-operation."))

(th~defaxiom the-power-of-an-operation-2
	     (in magma)
	     (formula (all-types aa
			     (forall (lam (op (aa aa aa))
					  (and
					   (= (power-of-operation op 1) (lam (x aa) x))
					   (forall-sort (lam (n num)
							     (= (power-of-operation op (s n))
								(lam (x aa) (op x (power-of-operation op n x))))) pos-nat))))))
	     (help "Inductive axiomatic definition of power-of-operation."))

(th~deftheorem the-power-op-of-plus
	     (in magma)
	     (conclusion
	      (forall-sort (lam (n num)
				(= (power-of-operation plus (s n))
				   (lam (x num) (times n x))))
			   pos-nat))
	     (help "power-operation of plus is times."))

(th~deftheorem the-power-op-of-times
	     (in magma)
	     (conclusion
	      (forall-sort (lam (n num)
				(= (power-of-operation times (s n))
				   (lam (x num) (power x n))))
			   pos-nat))
	     (help "power-operation of times is power."))



(th~defdef order-of-element ; now in magma
  (in magma)
  ;(type (all-types AA (o num aa (aa aa aa) (o  aa) )))
  (help "Relation that a given element x of a structure (set, op) has order n.")
  (type-variables aa)
  (definition
    (lam (S (o aa))
    (lam (op (aa aa aa))
    (lam (x aa)
    (lam (n num)
	 (and (pos-nat n)       ;; n in  Nat\{0}               
	      (exists-sort (lam (e aa)    ;;x^n=e
				(and (and (closed-under S op)
					  (unit S Op e))
				     (and (= (power-of-operation op n x) e)
					  (forall-sort (lam (m num) ;; n is smallest such number
							    (implies (= (power-of-operation op m x) e)
								     (= m n)))
						       (integer-intervall 1 n)))))
			   S))))))))




;;Formalisation of trace 

(th~defdef trace-of-element 
  (in magma)
  (definition
    (all-types AA
	       (lam (op (aa aa aa))
			(lam (x aa)
			     (lam (y aa)
				  (exists-sort (lam (n num)
						    (= y (power-of-operation op n x)))
				   pos-nat))))))
  (help "The set that is the trace of an element with respect to an operation.")
  )


;;;
;;; Definition of MAGMA
;;;

(th~defdef magma
	   (in magma)
	   (type-variables aa)
	   (definition
	     (lam (M (o aa))
		  (lam (op (aa aa aa))
		       (and (not-empty M)
			    (closed-under M op)))))
	   (help "Definition of a Magma."))





