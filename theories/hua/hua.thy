;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                         ;;
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

(in-package :keim)
(th~deftheory hua
	      
              (uses semigroup)
	      (help "Halbgruppen und Automaten"))
	      )

;5.3.
(th~defdef leftcongruence
	   (in hua)
	   (type-variables bb)
	   (definition
	     (lam (SG (struct bb))
		  (lam (rel (o bb bb))
		       (and (semigroup (struct-set SG)(struct-op SG))
			    (forall (lam (x1 bb)
			    (forall (lam (x2 bb)
			    (forall (lam (g bb)
					 ;; wenn (semigroup SG hier stehen wuerde, waere
					 ;; es schwieriger...		    
					 (implies (rel x1 x2)
						  (rel (struct-op SG g x1) (struct-op SG g x2))
						  )))))))))))

	   (help "The definition of leftcongruence."))
;4.11
(th~defdef induzierte_eqrel_abb
	   (in hua)
	   (type-variables aa bb)
	   (definition
	     (lam (rel (o bb bb))
		  (lam (f (aa bb))
		       (forall (lam (x bb)
				    (forall (lam (y bb)
						 (equiv (rel x y)
							(= (f x)
							   (f y))))))))))
	   (help "Definition einer induzierten Equivalenzrelation einer Abbildung")
	   )

;5.7
(th~defdef induzierte_eqrel_hom
	   (in hua)
	   (type-variables bb aa)
	   (definition
	     (lam (rel (o bb bb))
		  (lam (H (morphism aa bb))
		       (implies
			(homomorphism
			 (struct-set (morphism-domain H))
			 (struct-op (morphism-domain H))
			 (struct-set (morphism-codomain H))
			 (struct-op (morphism-codomain H))
			 (morphism-function H))
			(forall (lam (x bb)
				     (forall (lam (y bb)
						  (equiv (rel x y)
							 (= (morphism-function H x)
							    (morphism-function H y)))))))))))
	   (help "Die Definition einer induzierten Equivalenzrelation eines Homomorphismus
einer Halbgruppe."))
