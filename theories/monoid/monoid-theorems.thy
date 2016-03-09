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
(in-package :omega)

;(th~deftheorem zf-apply-function 
;               (in zermelo-fraenkel)
;               (author "http://www.ags.uni-sb.de/~kohlhase")
;               (date 18091998)
;               (conclusion
;                (forall (lam (F zf)
;                             (forall (lam (arg zf)
;                                          (implies (and (zf-function F)
;                                                        (in arg (zf-domain F)))
;                                                   (defined (zf-apply F arg))))))))
;               (help "The function application of F to A  is defined whenever F is a function and A is in the domain of F."))


;(th~deftheorem monoid-neut-thm
;               (in monoid)
;               (conclusion
;                (all-types aa (forall (lam (S (struct aa))
;                                      (implies (monoid S)
;                                               (and (in (struct-neut S)
;                                                        (struct-set S))
;                                                    (neutral-in (struct-set S)
;                                                                (struct-op S)
;                                                                (struct-neut S))))))))
;               (help "The struct-neutral of a monoid is welldefined."))
