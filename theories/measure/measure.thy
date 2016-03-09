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

(th~deftheory measure
              (uses metric)
	      (constants (struct-measure (all-types aa (aa aa (struct aa)))))
	      (help "Measured fields theory"))


#+rethink(th~defdef measure-space
	   (in measure)
	   (type-variables aa bb)
	   (definition
             (lam (S (struct aa))
                  (and (and (exists (lam (A (struct aa))
			       (and (= A (struct-measure-field S))
				    (ordered-field A))))
			      (exists (lam (M bb bb)
			       (and (= M (struct-measure S))
				    (forall-sort  (lam (x bb)
				     (in (struct-measure S x)
					 (struct-set (struct-measure-field S)))) (struct-set S)))))
			      (and (and (forall-sort  (lam (x bb)
					 (strictly-positive S (struct-measure S x))) (ring-set-star S))
					(ring-homomorphism (struct-measure S) S))
				   (exists-sort  (lam (x bb)
				    (not (= (struct-measure S) (ring-one S)))) (ring-set-star S)))))))
	   (help "The predicate for measure spaces."))
