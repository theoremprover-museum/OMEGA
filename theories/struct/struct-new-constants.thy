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

(in-package :omega)

(th~deftheory struct
              (uses function)
	      (type-constants struct  morphism)
	      (help "Theory stub for general algebraic structures."))

(th~defdef struct-set
	   (in struct)
	   (type-variables bb)
	   (constants (struct-set (o bb (struct bb))))
	   (formula true)
	   (help "The base set of an algebraic structure"))


(th~defdef morphism-function
	   (in struct)
	   (type-variables bb cc)
	   (constants (morphism-function (bb cc morphism)))
	   (formula true)
	   (help "The function of a morphism."))


(th~defdef morphism-domain
	   (in struct)
	   (constants (morphism-domain (struct morphism)))
	   (formula true)
	   (help "The domain structure of a morphism."))


(th~defdef morphism-codomain
	   (in struct)
	   (constants (morphism-codomain (struct morphism)))
	   (formula true)
	   (help "The codomain of a morphism."))

(th~defdef monomorphism
	   (in struct)
	   (constants (monomorphism (o morphism)))
	   (formula 
	    (= monomorphism
	       (lam (F morphism)
		    (injective (morphism-function F)
			       (struct-set (morphism-domain F))))))
	   (help "A morphism is a nonomorphism, iff it is injective."))

(th~defdef epimorphism
	   (in struct)
	   (constants (epimorphism (o morphism)))
	   (formula 
	    (= epimorphism
	       (lam (F morphism)
		    (surjective (morphism-function F)
				(struct-set (morphism-domain F))
				(struct-set (morphism-codomain F))))))
	   (help "A morphism is an epimorphism, iff it is surjective."))

(th~defdef isomorphism
	   (in struct)
	   (constants (isomorphism (o morphism)))
	   (formula 
	    (= isomorphism
	       (lam (F morphism)
		    (bijective (morphism-function F)
			       (struct-set (morphism-domain F))
			       (struct-set (morphism-codomain F))))))
	   (help "A morphism is an isomorphism, iff it is bijective."))
