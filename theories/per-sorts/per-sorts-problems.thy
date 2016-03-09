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
(in-package :omega)

; cebrown 30/7/01:  An example of a theorem motivating the
; use of per sorts.

(th~defproblem
 contravariant-per-sort-problem
 (in per-sorts)
 (conclusion a-bad-functional-is-not-in-the-per-sort
	     (forall
	      (lam (zero i)
		   (forall
		    (lam (one i)
			 (implies (not (= zero one))
				  (forall
				   (lam (zerop (o i i)) ; a per sort with only zero in its domain
					; the other per sort is equality
					(implies (and (per zerop)
						      (and (in-per-dom zero zerop)
							   (forall
							    (lam (n i)
								 (implies (in-per-dom n zerop)
									  (= n zero))))))
						 (not (in-per-dom
						       (lam (f (i i))
							    (f one))
						       (fun-per-sort
							(fun-per-sort zerop eq-per)
							eq-per))))))))))))
 (help "Why Chad is always saying interpreting sorts as pers is better than
interpreting them as sets."))

(th~defproblem
 top-per-is-singleton
 (in per-sorts)
 (conclusion top-per
	     (all-types
	      aa
	      (exists-unique-per-sort
	       (lam (x aa) true)
	       top-per)))
 (help "There is only one equiv class given by the per that relates everything.
(So the per sort should be thought of as a singleton.)"))

