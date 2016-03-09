;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

(th~deftheory ordered-ring
              (uses field poset)
	      (help "Theory of Ordered rings and fields."))

(th~defdef ordered-ring
	   (in ordered-ring)
           (type-variables aa)
	   (definition
             (lam (S (struct aa))
                  (and (and (poset S)
                            (ring (struct-set S)(struct-op S)(ring-times S)))
                       (and (forall (lam (x aa)
                                         (monotonic (struct-ordering S)
                                                    (struct-op S x)
                                                    (struct-set S))))
                            (forall (lam (x aa)
                                         (monotonic (struct-ordering S)
                                                    (ring-times S x)
                                                    (struct-set S))))))))
	   (help "Definition of the predicate for ordered rings,
                  i.e. rings with a strict partial ordering that is monotonic
                       for addition and multiplication."))

(th~defdef positive
	   (in ordered-ring)
           (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (struct-ordering S (ring-zero S))))
	   (help "x is positive in an ordered ring, iff it is greater than zero."))

(th~defdef strictly-positive
	   (in ordered-ring)
           (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (poset-strict-ordering S (ring-zero S))))
	   (help "x is positive in an ordered ring, iff it is greater than zero."))


(th~defdef struct-absval
	   (in ordered-ring)
	   (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (lam (x aa)
                       (ifthen (positive S x)
                               x
                               (ring-negative S x)))))
	   (help "The absolute value function of an ordered ring."))

(th~defdef ordered-field
	   (in ordered-ring)
           (type-variables aa)
           (definition 
             (lam (S (struct aa))
                  (and (ordered-ring S)
                       (field (struct-set S)(struct-op S)(ring-times S)))))
	   (help "Definition of the predicate for ordered fields, 
                  i.e. the ordered rings that are also fields."))




