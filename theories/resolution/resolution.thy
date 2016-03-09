;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(th~deftheory resolution
              (uses propositional-logic)
	      (constants (all-clause-sets  (o (o (o form)))) )
	      (help "A test theory for completeness proofs."))


;; ===================================================================================
;;     U N I V E R S E L L E     R E S O L U T I O N 
;; ====================================================================================
(th~defdef clause
	   (in resolution)
	   (definition 
	     (lam (cl (o form))
		  (forall (lam (li form) (implies (cl li) (literal li))))))
	   (help "A clause is a set of literals."))
           ;; theorem: (implies (clause cl) (clause-form (osy cl))))
           ;; theorem: (clause (empty-cl))

(th~defdef clause-set
	   (in resolution)
	   (definition
	     (lam (cs (o (o form)))
		  (forall (lam (cl (o form))
			       (implies (in cl cs) (clause cl))))))
	   (help "clause-set: every element is a clause."))

(th~defaxiom set-of-clause-sets
           (in resolution)
           (formula (forall (lam (cls (o (o form))) (equiv (clause-set cls)
						           (in cls all-clause-sets)))))
           (help "A constant for the set of all clause-sets."))

(th~defdef clause-pendant
	   (in resolution)
	   (definition
	     (lam (f form) (lam (cl (o form))
				(or (and (setunit cl) (= f (that (lam (li form) (cl li)))))    	  
				    (and (not (setunit cl)) (= f (osy cl)))))))
	   ;; 3. Fall Empty-clause fehlt noch.
	   (help "Corresponds a formula to a clause (literal-set)?"))

(th~defdef clause-set-pendant
	   (in resolution)
	   (definition
	     (lam (f form) (lam (cs (o (o form)))
			 (and (and (clause-set cs) (conjunction f))
			      (and
			       (forall (lam (cl (o form)) 
				 (implies (cs cl)
					  (exists (lam (sf form)
					    (and (in sf (subform-set f))
						 (clause-pendant sf cl)))))))
			       (forall (lam (tf form)
				 (implies (in tf (subform-set f))
					  (exists (lam (cl (o form))
					    (and (cs cl) (clause-pendant tf cl))))))))))))
	   (help "Corresponds a formula to a clause-set? ."))


(th~defdef unsat-cl
	   (in resolution)
	   (definition
	     (lam (cl (o form))
		  (exists (lam (f form)
			       (and (clause-pendant f cl)
				    (unsat-form f))))))
	   (help "A clause is unsatisfiable, if its corresponding formula is."))
           ;; theorem: (unsat-cl empty-cl)

(th~defdef unsat-cl-set
	   (in resolution)
	   (definition
	     (lam (cs (o (o form)))
		  (exists (lam (f form)
			       (and (clause-set-pendant f cs)
				    (unsat-form f))))))
	   (help "A clause-set is unsatisfiable, if its corresponding formula is."))
           ;;theorem: (equiv (unsat-cl-set cls)
           ;;                (exists (lam (cl (o form)) (implies (cls cl) (unsat-cl cl)))))


(th~defdef empty-cl
	   (in resolution)
	   (definition (lam (f form) FALSE))
	   (help "Empty-clause by definition, says always no."))

;; ============ Resolution ==============================



(th~defdef resolvable-b
	   (in resolution)
	   (definition
	     (lam (tco1 (o form)) (lam (tco2 (o form))
	     (lam (li1 form) (lam (li2 form)				  
				  (and (and (tco1 li1) (tco2 li2))
					    (com-pair li1 li2)) )))))
	   (help "Are 2 clauses with 2 literals resolvable in a calculus?"))

(th~defdef resolvable-s
	   (in resolution)
	   (definition
	     (lam (tco1 (o form)) (lam (tco2 (o form))
		  (exists (lam (li1 form) (exists (lam (li2 form)				  
			(and (and (tco1 li1) (tco2 li2))
				  (com-pair li1 li2)))))))))
	   (help "Are 2 clauses resolvable in a calculus?"))
;; Die Klauslen muessen nicht explicit als verschieden angenommen werden, da
;; Resolvieren auf einer Tautologie (Klausel die ein komplementaeres Paar enthaelt)
;; nach meiner Definition wieder sich selbst erzeigt, also keine Wirkung erzielt.

(th~defdef resolvent-s
	   (in resolution)
	   (definition
	     (lam (tco1 (o form)) (lam (tco2 (o form)) (lam (tco3 (o form))
	       (exists (lam (li1 form) (exists (lam (li2 form)
		       (and (resolvable-b tco1 tco2 li1 li2)
			    (set= (setminus (setminus (union tco1 tco2)
						      (singleton li1))	
					    (singleton li2))	   	
				  tco3))))))))))
	   (help "Is a clause possibly the resolvent of 2 other clauses."))

(th~defdef resolvent-b
           (in resolution)
           (definition
	     (lam (tco1 (o form)) (lam (tco2 (o form)) (lam (tco3 (o form))
             (lam (li1 form) (lam (li2 form)
		  (and (resolvable-b tco1 tco2 li1 li2)
		       (set= (setminus (setminus (union tco1 tco2)
						 (singleton li1))	
				       (singleton li2))	   	
			     tco3))))))))
	   (help "Is a clause the resolvent of two other clauses respectively 2 literals."))   

(th~defdef resolvent-of
	   (in resolution)
	   (definition
	     (lam (kl1 (o form)) (lam (kl2 (o form)) (lam (li1 form) (lam (li2 form)
		  (that (lam (rk (o form))
			     (set= rk
				   (setminus (setminus (union kl1 kl2)
						       (singleton li1))	
					     (singleton li2))))))))))
	   (help "Function, determing the resolvent of 2 clauses and literals ."))


