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
(eval-when (load compile eval)
  (unless (com~find-category 'typed-set)
    (com~defcategory typed-set
		     (help "Tactics of the typed set theory."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Rerepresent-set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic rerepresent-set
		 (outline-mappings (((existent existent) rerepresent-set-a)
				    ((nonexistent existent) rerepresent-set-f)
				    ((existent nonexistent) rerepresent-set-b)))
		 (expansion-function tyse=expand-rerepresent-set)
		 (parameter-types position)
		 (help "Rerepresent a set."))

(com~defcommand rerepresent-set
  (argnames line1 line2 position)
  (argtypes ndline ndline position)
  (arghelps "An open line containing a set"
            "A line containing the same set with different representation"
            "The position of the set")
  (function tyse=rerepresent-set)
  (frag-cats tactics typed-set)
  (defaults)
  (log-p t)
  (help "Rerepresent a set."))

(defun tyse=rerepresent-set (l1 l2 pos)
  (infer~compute-outline 'rerepresent-set (list l1 l2) (list pos)))

;;
;; Rerepresent-Set Directions
;;

(tac~deftactic rerepresent-set-f rerepresent-set (in typed-set)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L2 (tyse=rerepresent-set-f (formula L1) POS)))
   (sideconditions (tyse=rerepresent-set-fp (formula L1) Pos))
   (description "Forward application of Rerepresent-Set."))

(defun tyse=rerepresent-set-fp (L1 Pos)
  (tyse=composed-set2elements
   (data~struct-at-position L1 Pos)))

(defun tyse=rerepresent-set-f (L1 Pos)
  (multiple-value-bind (flag set)
      (tyse=composed-set2elements
       (data~struct-at-position L1 Pos))
    (declare (ignore flag))
    (data~replace-at-position L1 Pos (tyse=element-list2set Set))))

(tac~deftactic rerepresent-set-a rerepresent-set (in typed-set)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations)
   (sideconditions (tyse=rerepresent-set-ap
                    (formula L1) (formula L2) Pos)) 
   (description "Test application of Rerepresent-Set."))


(defun tyse=rerepresent-set-ap (L1 L2 Pos)
  (or ;;(and (not (term~set-p (data~struct-at-position L1 pos)))
   (data~equal (tyse=rerepresent-set-f L1 Pos) L2)
   ;;(and (not (term~set-p (data~struct-at-position L2 pos)))
   (data~equal (tyse=rerepresent-set-f L2 Pos) L1)))


;;
;; Backward
;;

(tac~deftactic rerepresent-set-b rerepresent-set (in typed-set)
   (parameters (POS pos+position "A position"))
   (premises L1)
   (conclusions L2)
   (computations (L1 (tyse=rerepresent-set-f (formula L2) POS)))
   (sideconditions (tyse=rerepresent-set-fp (formula L2) Pos))
   (description "Backward application of Rerepresent-Set."))


(defun tyse=expand-rerepresent-set (outline parameters)
  (let ((begin (car outline))
	(end (cadr outline)))
    (remove-duplicates
     (remove-if-not #'(lambda (x) (member x tyse*set-functions :test #'data~schema-equal))
		    (data~all-substructs (node~formula begin))))
    ;;;(omega~warn "Not yet implemented!!!")
    ))



