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


(th~defproblem is-it-graph?
	       (in graph)
	       (conclusion
		(graph (set 1 2 3)
		       (set (set 1 2)(set 1 3)))))

(th~defproblem is-it-not-graph
	       (in graph)
	       (conclusion
		(graph (set 1 2 3)
		       (set (set 1 4)(set 1 3)))))


(th~defproblem graph-bijective
	       (in graph)
	       (conclusion
		(bijective (set 1 2 3)
			   (set 4 5 6)
			   (lam (x num)(ifthen (= x 1) 4 (ifthen (= x 2) 5 6))))))

(th~defproblem graph-exists
	       (in graph)
	       (conclusion
		(exists-sort (lam (x num)(= x 2))( set  1 2 3))))

(th~defproblem graph-cardinality
	       (in graph)
	       (conclusion
		(= (cardinality (set 1 2))
		   (cardinality (set (set 1 4)(set 1 3))))))

(th~defproblem are-the-graphs-isomorph
	       (in graph)
	       (conclusion
		(exists (lam (f (num num))
			     (graph-isomorphism (set 1 2 3)
						(set (set 1 2)(set 1 3))
						(set 1 3 2)
						(set (set 3 2)(set 1 3))
						f)))))

(th~defproblem are-the-graphs-homomorph
	       (in graph)
	       (conclusion
		(exists (lam (f (num num))
			     (graph-homomorphism (set 1 2 3)
						 (set (set 1 2))
						 (set 1 3 2)
						 (set (set 3 2)(set 1 3))
						 f)))))

