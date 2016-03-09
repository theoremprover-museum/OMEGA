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

(th~deftheory economy
	      (uses size calculus)
	      (constants (dm num)
			 (kwh num)
			 (cbm num)
			 (liter num)
			 (prod num)
			 (opt (o (o num) (num num))))
	      (help "Theory of elementary economy exam questions."))

(th~defaxiom dm-unit (in economy) (formula (sizeunit dm)))
(th~defaxiom kwh-unit (in economy) (formula (sizeunit kwh)))
(th~defaxiom cbm-unit (in economy) (formula (sizeunit cbm)))
(th~defaxiom liter-unit (in economy) (formula (sizeunit liter)))
(th~defaxiom prod-unit (in economy) (formula (sizeunit prod)))

(th~defaxiom cbm-conversion
	     (in economy)
	     (formula
	      (sizeunit-conversion (size-function (lam (x num) (times x 1000)) cbm liter)))
	     (help "One cbm is 1000 liters."))

#|
(th~defdef local-optimum
	   (in economy)
	   (constants (local-optimum  (o (num num) num)))
	   (formula
	    (=def local-optimum
	       (lam (s num)
		(lam (F (num num))
		      (local-minimum (size-num s) (sf-fun F))))))
	   (help "Axiom for global optima."))

(th~defdef global-optimum
	   (in economy)
	   (constants (global-optimum  (o (o num) (num num) num)))
	   (formula
	    (=def global-optimum
	       (lam (s num)
		(lam (F (num num))
		 (lam (G (o num))
		      (total-minimum (size-num s)
				     (compose-functions size-num F)
				     (ss-set G)))))))
	   (help "Axiom for optima of size functions on a set."))
|#

(th~defaxiom opt
	   (in economy)
;	   (constants (opt (o (o num) (num num))))
	   (formula
	    (forall (lam (F (num num))
	     (forall (lam (G (o num))
	      (forall (lam (u num)
	       (forall (lam (v num)
		(equiv (opt (size-function F U V) (size-set G U))
		       (exists (lam (min num)
				  (total-minimum min F G)))))))))))))
	   (help "Axiom for optima of size functions on a set."))
