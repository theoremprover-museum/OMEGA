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

;used by graph and permutation
;nearly the same as instbycas, can be unified

(cri~def-control-rule gap-interrupt-if-insttask
		      (kind strategy-interruption)
		      (if (p-information-for-inst-task-p "insttask" "params"))
		      (then
		       (insert ((Instfromgap "insttask")))))

(defun p-information-for-inst-task-p (insttask params) 
  (let* ((tasks cri*current-tasks) ;; (agenda~all-tasks (pds~agenda omega*current-proof-plan)))
;	 (dummy (progn (omega~trace "~A" tasks)))
	 (inst-tasks (remove-if-not #'agenda~inst-task-p tasks))
	 (start-task (roc~start-task pplan*roc-state-description)))
    (apply #'append (mapcar #'(lambda (inst-task)
				(let ((param-to-inst-task  (crihelp=param-to-inst-task-mp inst-task)))
				  (if (and param-to-inst-task
					   (null (already-tried-p inst-task param-to-inst-task)))
				      (list (list (cons insttask inst-task)
						  (cons params (list param-to-inst-task))))
				    nil)))
			    inst-tasks))))

(defun compute-instantiation-with-gap (meta-var)
  (let ((inst-task (roc~start-task instmeta*roc-state-description)))
    (crihelp=param-to-inst-task-mp inst-task)))

(strat~define-strategy-ks
 (name Instfromgap)
 (refinement-algorithm InstMeta)
 (condition instantiation-task-p)
 (compute-instantiation-function compute-instantiation-with-gap)
 (parameter-types )
 (print "Strategy-KS InstfromGAP: Offer to instantiate meta-variable ~A with the GAP"))


