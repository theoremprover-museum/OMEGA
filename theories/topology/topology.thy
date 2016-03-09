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

(th~deftheory topology
              (uses struct typed-set)
	      (help "Basic, set-theoretic topology."))

(th~defconstant struct-topology
		(in topology)
		(type (all-types aa (o (o aa) (struct aa))))
		(help "The the feature for the topology of a structure"))


(th~defdef topology
	   (in topology)
	   (type-variables aa)
           (definition
             (lam (top (o (o aa)))
                  (lam (M (o aa))
                       (and (and (in (lam (x aa) false) top)
                                 (in  M top))
                            (and (forall (lam (II (o (o aa)))
                                              (implies (subset II top)
                                                       (in (union-over-collection II) top))))
				   (forall (lam (S (o aa))
				    (forall (lam (T (o aa))
				     (implies (and (in S top) (in T top))
					      (in (intersection S T) top)))))))))))
	   (help "The predicates for topologies. 
                  (topology O M) is true, iff O is a topology over M."))

(th~defdef topological-space
	   (in topology)
	   (type-variables bb)
           (definition
             (lam (S (struct bb))
                  (topology (struct-topology S) (struct-set S))))
	   (help "The predicate for structures that are topological  spaces."))



(th~defdef open
	   (in topology)
	   (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (lam (M (o aa))
                       (in M (struct-topology S)))))
	   (help "The predicate for openness in a topological space."))

(th~defdef closed
	   (in topology)
	   (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (lam (M (o aa))
                       (open S (set-complement (struct-set S) M)))))
	   (help "The predicate for closedness in a topological space."))

(th~defdef environment
	   (in topology)
	   (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (lam (M (o aa))
                       (lam (x aa)
                            (and (open S M)
                                 (in x M))))))
	   (help "The predicate for openness in a topological space."))


