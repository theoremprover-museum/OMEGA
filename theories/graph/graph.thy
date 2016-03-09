;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

(th~deftheory GRAPH
	      (uses integer)
	      (help "Graph theory."))

;; basics

(th~defdef graph
	   (in graph)
	   (type-variables aa)
           (definition
	     (lam (vertices (o aa))
		  (lam (edges (o (o aa)))
		       (forall-sort (lam (edge (o aa))
					 (and (subset edge vertices)
					      (= (cardinality edge) 2)))
				    edges)))))


(th~defdef graph-homomorphism
           (in graph)
           (type-variables aa bb)
           (definition
             (lam (vertices1 (o aa))
             (lam (edges1 (o (o aa)))
             (lam (vertices2 (o bb))
             (lam (edges2 (o (o bb)))
             (lam (f (bb aa))
                          (forall-sort (lam (vert1 aa)
                          (forall-sort (lam (vert2 aa)
                                            (implies (in (union (singleton vert1)(singleton vert2))
                                                         edges1)
                                                     (in (union (singleton (f vert1))(singleton (f vert2)))
                                                         edges2)))
                                       vertices1))
                                       vertices1))))))))

;; idea for expansion of (set var1 var2): this represents the function constant 
;; (lam x1, x2 . (set x1 x2)) 
;(th~defdef graph-homomorphism
;           (in graph)
;           (type-variables aa bb)
;           (definition
;             (lam (vertices1 (o aa))
;             (lam (edges1 (o (o aa)))
;             (lam (vertices2 (o bb))
;             (lam (edges2 (o (o bb)))
;             (lam (f (bb aa))
;                          (forall-sort (lam (vert1 aa)
;                          (forall-sort (lam (vert2 aa)
;                                            (implies (in ((lam (x aa) (lam (y aa) (set x y))) vert1 vert2)
;                                                         edges1)
;                                                     (in ((lam (x bb) (lam (y bb) (set x y))) (f vert1)(f vert2))
;                                                         edges2)))
;                                       vertices1))
;                                       vertices1))))))))


(th~defdef graph-isomorphism
	   (in graph)
	   (type-variables aa bb)
           (definition
	     (lam (vertices1 (o aa))
	     (lam (edges1 (o (o aa)))
	     (lam (vertices2 (o bb))
	     (lam (edges2 (o (o bb)))
	     (lam (iso (bb aa))
		  (and (bijective vertices1 vertices2 iso)
		       (and (graph-homomorphism vertices1 edges1 vertices2 edges2 iso)
			    (graph-homomorphism vertices2 edges2 vertices1 edges1 (invert-function iso)))))))))))


;; or |v1|=|v2|:

(th~deftheorem graph-isomorphism-by-cardinality
	       (in graph)
	       (conclusion (all-types aa bb 
		(forall (lam (vertices1 (o aa))
		(forall (lam (edges1 (o (o aa)))
	        (forall (lam (vertices2 (o bb))
	        (forall (lam (edges2 (o (o bb)))
	        (forall	(lam (iso (bb aa))
			     (implies 
			      (and (bijective vertices1 vertices2 iso)
				   (and (graph-homomorphism vertices1 edges1 vertices2 edges2 iso)
					(= (cardinality vertices1)(cardinality vertices2))))
			      (graph-isomorphism vertices1 edges1 vertices2 edges2 iso)))))))))))))))



