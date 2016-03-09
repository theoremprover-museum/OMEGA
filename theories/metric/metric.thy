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

(th~deftheory metric
              (uses ordered-ring)
	      (constants (struct-measure-field 
                          (all-types aa bb ((struct aa) (bistruct aa bb))))
			 (struct-base
			  (all-types aa bb ((struct bb) (bistruct aa bb))))
			 (struct-metric (all-types aa bb (aa bb bb (bistruct aa bb)))))
	      (help "Theory of metric spaces."))

#| Metric spaces are sets together with a distance function, called a metric,
   The metric is binary function into an ordered field, the measure-field.
   
|#

(th~defdef triangle-eq
	   (in metric)
           (type-variables aa bb)
	   (definition
             (lam (S (bistruct aa bb))
                  (forall (lam (x bb)
                   (forall (lam (y bb)
                    (forall (lam (z bb)
                   (struct-ordering (struct-measure-field S)
		    (struct-metric S x y)
		     (struct-op (struct-measure-field S)
		      (struct-metric S x z)
		      (struct-metric S z y)))))))))))
	   (help "The triangle equality of a metric space."))


(th~defdef distance-function
	   (in metric)
           (type-variables aa bb)
	   (definition
             (lam (S (bistruct aa bb))
              (lam (D (aa bb bb))
	       (and (and (symmetric-function D)
			 (forall (lam (x bb)
		          (= (ring-zero (struct-measure-field S)) (D x x)))))
		    (forall (lam (x bb)
                     (forall (lam (y bb)
		      (implies (not (= x y))
			       (strictly-positive (struct-measure-field S) (D x y)))))))))))
	   (help "A binary function D is a distance function, iff it is symmetric and
                  non-degenerate, i.e. if D(X,Y)=0 iff X ne Y."))

(th~defdef metric-space
	   (in metric)
           (type-variables aa bb cc)
	   (definition
             (lam (S (bistruct aa bb))
		    (and (and (ordered-field (struct-measure-field S))
			      (function-domain2-range
			       (struct-metric S)
			       (struct-set (struct-base S))
			       (struct-set (struct-base S))
			       (struct-set (struct-measure-field S))))
			 (distance-function S (struct-metric S)))))
	   (help "The predicate for structures that are metric spaces."))


