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

(th~defproblem solving-equations
	       (in field)
;               (reference "Lueneburg: p. 13")
	       (conclusion
		(all-types aa
		(forall (lam (S (struct aa))
	         (implies (field (struct-set S)(struct-op S)(ring-times S))
                  (forall (lam (x aa)
	           (forall (lam (y aa)
		    (implies (and (not (= x (ring-zero S)))
				  (in x (struct-set S)))
			     (and (exists-unique (lam (z aa)
						      (and (in z (struct-set S))
							   (= (ring-times S x z) y))))
				  (= (ring-times S x (field-div S y x)) y))))))))))))
	       (help "In Fields, there is exactly one solution z 
                       for the equation x*z=y, it is z=y/x."))

						    



;(th~defproblem pfunc-field
;               (in field)
;               (conclusion 
;                (all-types aa
;                           (forall (lam (S (struct aa))
;                             (implies (field S) (field (pfunc-struct S (struct-set S))))))))
;               (help "The structure of pointwise functions of a field is again one."))




