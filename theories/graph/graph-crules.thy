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


(cri~def-control-rule graph-defnexp-replace
		      (kind methods)
		      (if (and (or (goal-matches ("goal" ("rel" "arg1" "arg2")))
				   (goal-matches ("goal" (not ("rel" "arg1" "arg2")))))
			       (defined-in-theories "rel" (function graph))))
		      (then (insert-end ((DefnExp-m-b () ("rel"))))))

(defun defined-in-theories (obj thylist)
  (some #'(lambda (thy) (and (th~find-assumption (keim~name  obj) thy)
			     (notany #'(lambda (th) (th~find-assumption (keim~name  obj) th))
				     (th~imports thy))))
	thylist))




(cri~def-control-rule graph-prefer
		      (kind methods)
		      (if (always-true))
		      (then (prefer (eval-function-m-b))))
					


(defun theorem-of-theory (theories theorem)
  (let ((theorems (mapcan  #'th~theorems  theories)))
    (mapcar #'(lambda (th)
		(list (cons theorem th)))
	    theorems)))


(cri~def-control-rule graph-theorem-select
		      (kind methods)
		      (if (theorem-of-theory (graph) "theorem")) 
		      (then (insert-end ((assertion-m-b () ("theorem"))))))
