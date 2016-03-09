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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;cr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cri~def-control-rule homo-defnexp-select
		      (kind methods)
		      (if (and (and (goal-matches ("goal" ("rel" "args")))
				    (theory-definition "rel"))
			       (not-defined-in-theories "rel" (generic base post))))
		      (then
		       (select ((DefnExp-m-b () ("rel"))
				))))

(cri~def-control-rule group-closed-only-once
                      (kind methods)
		      (if (last-method group-closed-m-b))
		      (then
		       (reject (
			group-closed-m-b
			))))

(cri~def-control-rule rewrite-to-unit
                      (kind methods)
		      (if (and (or (assumption-matches ("ass" (= "term" (group-unit "grp" "op"))))
				   (assumption-matches ("ass" (= (group-unit "grp" "op") "term"))))
			       (goal-contains-at-pos "goal" "term" "pos")))
		      (then
		       (prefer (
				(equal-subst-m-b ("ass") ("pos"))))))

(cri~def-control-rule homo1
                      (kind methods)
		      (if (reason-applied? homomorphism-on-domain-m))
		      (then
		       (reject (
			homomorphism-m-b
			))))

(cri~def-control-rule homo2
                      (kind methods)
		      (if (reason-applied? homomorphism-m))
		      (then
		       (reject (
			homomorphism-on-domain-m-b
			))))

(cri~def-control-rule prefer-equation-tasks
                      (kind tasks)
		      (if (equation? "task"))
		      (then
		       (select ("task"))))






;(cri~def-control-rule forward-step
;                      (kind methods)
;                      (if (and (goalcontainmetavars)
;                               (or (assumption-matches ("ass" (image-of-domain "fun" "set" "const")))
;                                   (assumption-matches ("ass" (exists-sort "scope" "sort"))))))
;                      (then
;                       (prefer (
;                                (existse-sort-m-f () ("ass"))
;                                (defne-m () ("ass"))
;                                ))))


;(cri~def-control-rule reject-defni
;                     (kind methods)
;                     (if (or (goal-matches ("goal" (exists-sort "term" "sort")))
;                             (goal-matches ("goal" (forall-sort "term" "sort")))))
;                     (then
;                      (reject (
;                               (defni-m () ("goal"))
;                               ))))
;
;(cri~def-control-rule reject-defne
;                      (kind methods)
;                      (if (or (assumption-matches ("ass" (exists-sort "term" "sort")))
;                              (assumption-matches ("ass" (forall-sort "term" "sort")))))
;                      (then
;                       (reject (
;                                (defne-m () ("ass"))
;                                ))))

;(cri~def-control-rule reject-meth
;                      (kind methods)
;                     (if (always-true))
;                     (then
;                      (reject (
;                               ALPHAUNIFY-M-B
;
;                               ANALOGYCLOSE-M-B
;                               Internal-Analogy-S-B
;                               AnalogyRef-S-b
;                               AnalogyRef-S-F
;                               ))))


(cri~set-used-control-rules! '(

			       prefer-equation-tasks
			       group-closed-only-once
			       homo1
			       homo2
                              ))

