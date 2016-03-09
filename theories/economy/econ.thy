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

(th~deftheory economy
	      (uses size calculus)
	      (constants (dm unit)
			 (kwh unit)
			 (liter unit)
			 (prod unit))
	      (help "Theory of elementary economy exam questions."))

(th~defaxiom optimum
	   (in economy)
	   (constants (optimum  (size (o size) (size size))))
	   (formula
	    (forall (lam (s size)
	     (forall (lam (u unit)
              (forall (lam (F (size size))
               (forall (lam (G (o num))
                (forall (lam (H (o size))
	         (implies (= (in s H)
	                    (and (in (sz-num s) g)
	                         (= (sz-unit s) u)))
		  (= (optimum F H)
		     (that (lam (r size)
                      (and (total-minimum-at (sz-num r) (sf-fun F) G) (in r H)))))))))))))))))
	   (help "Axiom for optima of size functions"))
