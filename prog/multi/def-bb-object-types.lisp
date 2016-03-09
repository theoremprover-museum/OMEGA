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

#| ------------------------------------------------------- Blackboard Object Types -------------------------------------------------- |#

(black~define-blackboard-object-type
 (name symbol)
 (check symbolp)
 (save (lambda (symbol stream)
	 (format stream "~A" symbol)))
 (load (lambda (symbol)
	 symbol)))

(black~define-blackboard-object-type
 (name symbollist)
 (check (lambda (item)
	    (and (listp item)
		 (every #'symbolp item))))
 (save (lambda (symbollist stream)
	 (format stream "(")
	 ( mapcar #'(lambda (element)
		      (format stream " ~A" element))
		  symbollist)
	 (format stream ")")))
 (load (lambda (symbollist)
	 symbollist)))

(defun dummy ()
  )

;;(black~define-blackboard-object-type
;; (name stack)
;; (check stack~stack-p)
;; (save dummy)
;; (load dummy))

(black~define-blackboard-object-type
 (name pds)
 (check pds~proof-plan-p)
 (save dummy)
 (load dummy))

(defun focusp (item)
  't)

(black~define-blackboard-object-type
 (name focus)
 (check focusp)
 (save dummy)
 (load dummy))

(defun bindenv-p (item)
  't)

(black~define-blackboard-object-type
 (name binding-environment)
 (check bindenv-p)
 (save dummy)
 (load dummy))



(defun dummyt (item)
  t)

(black~define-blackboard-object-type
 (name dummyt)
 (check dummyt)
 (save dummy)
 (load dummy))
