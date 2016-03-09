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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;metaprds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goal-contains-at-pos (goal term position)
  (cond ((and (stringp goal) (stringp position)(and (term~p term)))
	 (let* ((task cri*current-task)
		(goal (agenda~task-node task))
		(poslist (data~substruct-positions term (node~formula goal))))
	   (when poslist
	     (mapcar #'(lambda (pos) (mapp~create (list position)(list  pos))) poslist))))
	(T nil)))

(defun reason-applied? (method)
  (let* ((node cri*current-task-node)
	 (justs (mapcan #'(lambda (node)
			  (let ((just (node~justification node)))
			    (when (or (infer~method-p (just~method just))
				      (infer~supermethod-p (just~method just)))
			      (list just))))
			(prob~proof-steps cri*current-pds))))
	    (when (some #'(lambda (just)
			    (string=
			     (string-upcase (keim~name (just~method just)))
			     (string method))) justs)
	      (list (list (cons T T))))))



(defun equation? (thetask)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (equation-tasks (remove-if-not #'(lambda (task)
					       (let* ((node (agenda~task-node task))
						      (formula (node~formula node)))
						 (logic~equality-p formula)))
					   goal-tasks)))
    (mapcar #'(lambda (equation)
		(list (cons thetask equation)))
	    equation-tasks)))

(defun not-mv? (thetask)
  (let* ((goal-tasks (remove-if-not #'agenda~goal-or-goal-schema-task-p cri*current-tasks))
	 (equation-tasks (remove-if #'(lambda (task)
					       (some #'meta~p
						     (term~free-variables
						      (node~formula
						       (agenda~task-node task)))))
					   goal-tasks)))
    (mapcar #'(lambda (equation)
		(list (cons thetask equation)))
	    equation-tasks)))



(defun goalcontainmetavars ()
  (let* ((task cri*current-task)
	 (goalformula (node~formula (agenda~task-node task))))
    (some #'meta~p (data~all-substructs goalformula))))

(defun not-defined-in-theories (obj thylist)
    (notany #'(lambda (thy) (th~find-assumption (keim~name  obj) thy)) thylist)) 

(defun theory-definition (obj)
    (th~find-assumption (keim~name obj)  (prob~theory omega*current-proof-plan)))
