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

(in-package :keim)

; cauchy-sequence needs bistruct
;(th~defproblem cauchy-convergent
;               (in sequences)
;               (conclusion cauchy-convergent
;                           (all-types aa
;                                      (forall (lam (S1 (struct aa))
;                                                   (implies (complete-poset S1)
;                                                            (forall (lam (seq (aa num))
;                                                                         (implies (cauchy-sequence S1 seq)
;                                                                                  (convergent S1 seq)))))))))
;               (help "All cauchy sequences converge in complete 
;                      ordered structures."))

;Head reduction is blocked!
;
(th~defproblem limit-unique
               (in sequences)
               (conclusion limit-unique
			   (all-types aa bb
			    (forall (lam (S (bistruct bb aa))
					 (forall (lam (seq (aa num))
						      (forall (lam (x aa)
								   (forall (lam (y aa)
										(implies (and (is-limit-of S seq x)
											      (is-limit-of S seq y))
											 (= x y))))))))))))
	       (help "The limit of a sequence is unique, if it exists."))

