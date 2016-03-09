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

(th~deftheory group6
	      (uses morphism integer)
	      (help "Group theory"))


;;;
;;; Definition of GROUP
;;;

(th~defdef group
	   (in group6)
	   (type-variables aa)
	   (definition
	     (lam (G (o aa))
		  (lam (op (aa aa aa))
		       (and (not-empty G)
			    (and (closed-under G op)
				 (and (associative G op)
				      (and (exists-sort (lam (e aa) (right-unit G op e)) G)
					   (right-inverse-exist G op (struct-right-unit G op)))))))))
	   (help "Definition of a Group."))

(th~defdef abelian-group
	   (in group6)
	   (type-variables aa)
	   (definition
	     (lam (G (o aa))
		  (lam (op (aa aa aa))
		       (and (group G op)
			    (commutative G op)))))
	   (help "Definition of an Abelian Group."))

(th~defdef group-unit 
	   (in group6)
	   (type-variables bb)
           (definition
             (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (struct-unit G op))))
	   (help "(unit n G op) holds if of all y in G (op y n) = y = (op n y)."))

(th~defdef group-inverse
	   (in group6)
	   (type-variables bb)
           (definition
             (lam (G (o bb))
		  (lam (op (bb bb bb))
		       (lam (elem bb)
			    (that (lam (inv bb)
				       (and (G inv)
					    (and (= (op elem inv) (group-unit g op))
						 (= (op inv elem) (group-unit g op)))))))))))

(th~defdef  subgroup  
           (in group6)
           (type-variables se)
           (definition
             (lam (M (o se))
		  (lam (op (se se se))
             (lam (M1 (o se))
		  (lam (op1 (se se se))
		       (and  (and (= op op1) ;restriction!
				  (subset M M1))
			     (and (group M op)
				  (group M1 op1)))))))))

