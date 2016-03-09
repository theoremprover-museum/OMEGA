;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: THEORY -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 2000 by Armin Fiedler and AG Siekmann,                   ;;
;;   Fachbereich Informatik, Universitaet des Saarlandes,                   ;;
;;   Saarbruecken, Germany.                                                 ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, see the P.rex home page at:        ;;
;;     http://www.ags.uni-sb.de/~prex                                       ;;
;;   or write to:                                                           ;;
;;     P.rex Project                                                        ;;
;;     AG Siekmann/FR Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 151150                                                      ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: prex@ags.uni-sb.de                                    ;;
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

(th~deftheory prex
	      (uses typed-set)
	      (help "Extra fuer ein Beispiel in einem Paper."))

(th~deftype set
	    (in prex)
;	    (expansion (o i))
	    (expansion (all-types aa (o aa)))
	    (help "A definition for set"))

(infer~deftactic union-lemma
		 (outline-mappings (((existent existent) union-lemma-a)))
		 (expansion-function oc=expand-union-lemma)
		 )

(defun oc=expand-union-lemma (outline parameters)
  (let ((disj (cadr outline))
	(union (car outline)))
    (format t "~&disj: ~S~&union: ~S" disj union)
    (tacl~init outline)
    (tacl~sequence
     (ore-res ('ore (list union disj nil nil) nil))
;     (dummy1 ('assertion (list (list (first ore-res)) 'union (second ore-res)) nil))
;     (dummy2 ('assertion (list (list (third ore-res)) 'union (fourth ore-res)) nil))
     )
    (tacl~end)))

(tac~deftactic union-lemma-a union-lemma (in prex)
	       (premises (L1 "The disjunction"))
	       (conclusions (L2 "The union"))
	       )

(com~defcommand union-lemma
		(argnames disj union)
		(argtypes ndline ndline)
		(arghelps "The disjunction" "The union")
		(function oc=union-lemma)
		(defaults )
		(frag-cats tactics)
		(log-p t)
		)

(defun oc=union-lemma (d u)
  (infer~compute-outline 'union-lemma (list u d) nil))
