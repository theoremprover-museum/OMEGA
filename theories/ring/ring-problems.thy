;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

(th~deftheorem mul-zero
	       (in ring)
	       (conclusion
		(all-types aa
		(forall (lam (S (struct aa))
			     (implies (ring (struct-set S)(struct-op S)(ring-times S))
				      (forall (lam (x aa)
						   (implies (in x (struct-set S))
							    (and (= (struct-neut S)
								    (struct-op S (struct-neut S) x))
								 (= (struct-neut S)
								    (struct-op S x (struct-neut S)))
								 )))))))))
	       (help "In Rings, x * 0 = 0* x = 0."))




;(th~defproblem pfunc-ring
;               (in ring)
;               (conclusion 
;                (all-types aa
;                 (forall (lam (S (struct aa))
;                             (implies (ring S) (ring (pfunc-struct S (struct-set S))))))))
;               (help "The structure of pointwise functions of a ring is again one."))



