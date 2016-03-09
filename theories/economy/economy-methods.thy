;;; -*- syntax: common-lisp; package: omega; base: 10; mode: keim -*-
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
  
(in-package "OMEGA")

; Methods

(infer~defmethod optimize
		 (outline-mappings (((existent nonexistent) optimize)))
		 (help "Optimization."))


(meth~defmethod optimize optimize
		(in economy)
		(rating 2)
		(declarations
		 (sorted-meta-variables
		  (F (num num)) (G (o num)) (U num term) (V num term)
		  ))
		(premises (+ p1))
		(expansion-computations
		 (optax (th-ass "opt")))
		(conclusions (- conc))
		(decl-content
		 (p1 () (exists (lam (min num) (total-minimum min F G))) ("Open" ()()))
		 (conc () (opt (size-function F U V) (size-set G U))     ("Assertion" () (optax p1))))
		(proc-content schema-interpreter)
		(remark "The method is the application of the optimization axiom.")
		)

