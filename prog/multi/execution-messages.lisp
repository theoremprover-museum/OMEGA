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



(mod~defmod EXMES 
            :uses (keim roc)
            :documentation "execution messages"
            :exports (
                      exmes+exmes
                      exmes+interruption-message
                      exmes+termination-message
                      
                      exmes~create-interruption-message
                      exmes~create-termination-message
                      exmes~interruption-message-demands
                      exmes~interruption-message-p
                      exmes~interruption-message-state-description
                      exmes~termination-message-p
                      exmes~termination-message-strategy-ks
                      exmes~termination-message-task
                      ))

;; If the execution of a strategy-ks ends (if proper termination or interruption, or wahat ever) a message has to be send back to the
;; main algorithm. This module contains the structures for this messages. These messages are important for the main-system, where different
;; messages can have different effects.

#| ---------------------------------------------------------- Execution message ------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass exmes+exmes (keim+object)
    ((state-description :initform nil
			:initarg :state-description
			:accessor exmes~state-description))))


#| -------------------------------------------------------------- Start Message ------------------------------------------------------- |#

;; One message is, that the system has just been turned on.

(eval-when (load compile eval)
  (defclass exmes+start-message (exmes+exmes)
    ()))

(defun exmes~start-message-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a exmes+termination-message, nil otherwise."))
  (typep obj 'exmes+start-message))

(defmethod print-object ((message exmes+start-message) stream)
  (format stream "START-Message"))

(defun exmes~create-start-message ()
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "A new start message."))
  (make-instance 'exmes+start-message
		 :state-description nil))

#| --------------------------------------------------------- Termination Message ------------------------------------------------------ |#

;; One possible message is, that it was cleanly terminated. In this case it is interesting which strategy-ks has been terminated and
;; on which goal it had been invoked on. This message is created, if a strategy-ks execution terminates cleanly.

(eval-when (load compile eval)
  (defclass exmes+termination-message (exmes+exmes)
    ((demands :initform nil
	      :initarg :demands
	      :accessor exmes~termination-message-demands))))

(defun exmes~termination-message-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a exmes+termination-message, nil otherwise."))
  (typep obj 'exmes+termination-message))

(defmethod print-object ((message exmes+termination-message) stream)
  (format stream "Termination-Message: Execution of strategy-ks ~A on task ~A terminated with demands ~A."
	  (keim~name (roc~strategy-ks (exmes~state-description message)))
	  (roc~start-task (exmes~state-description message))
	  (exmes~termination-message-demands message)))

(defun exmes~create-termination-message (state-des demands)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A state-description and a demands list.")
	   (effect  "None.")
	   (value   "A new termination message."))
  (make-instance 'exmes+termination-message
		 :state-description state-des
		 :demands demands
		 ))

#| ------------------------------------------------------- Interruption Message ------------------------------------------------------ |#

;; A further possible messge is, that the execution has been interrupted, that demands and a state description have been created, that
;; have been entried in the demands respectively the store.

(eval-when (load compile eval)
  (defclass exmes+interruption-message (exmes+exmes)
    ((demands :initform nil
	      :initarg :demands
	      :accessor exmes~interruption-message-demands)
     (applied-crules :initform nil
		     :initarg :applied-crules
		     :accessor exmes~interruption-message-applied-crules))))

(defun exmes~interruption-message-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a exmes+interruption-message, nil otherwise."))
  (typep obj 'exmes+interruption-message))

(defmethod print-object ((message exmes+interruption-message) stream)
  (let* ((state-des (exmes~state-description message)))
    (format stream "Intrruption-Message: Execution of strategy-ks ~A on task ~A interrupted bu rules ~A with demands ~A."
	    (keim~name (roc~strategy-ks state-des))
	    (roc~start-task state-des)
	    (exmes~interruption-message-applied-crules message)
	    (exmes~interruption-message-demands message))))

(defun exmes~create-interruption-message (state-description demands applied-crules)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A list of demands and a state-description.")
	   (effect  "None.")
	   (value   "A new interruption message."))
  (make-instance 'exmes+interruption-message
		 :demands demands
		 :state-description state-description
		 :applied-crules applied-crules))

#| ----------------------------------------------------------- Failure Message ------------------------------------------------------ |#

;; A further message is, that the execution has been interrupted, since there had been no possibily to go on!

(eval-when (load compile eval)
  (defclass exmes+failure-message (exmes+exmes)
    ((report :initform nil
	     :initarg :report
	     :accessor exmes~failure-message-report))))

(defun exmes~failure-message-p (obj)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if input is a exmes+failure-message, nil otherwise."))
  (typep obj 'exmes+failure-message))

(defmethod print-object ((message exmes+failure-message) stream)
  (let* ((state-des (exmes~state-description message)))
    (format stream "Failure Message: Execution of strategy-ks ~A on task ~A failed because of ~A"
	    (keim~name (roc~strategy-ks state-des))
	    (roc~start-task state-des)	    
	    (exmes~failure-message-report message))))

(defun exmes~create-failure-message (state-description report)
  (declare (edited  "07-JUN-1999")
	   (authors Ameier)
	   (input   "A state-description and a report.")
	   (effect  "None.")
	   (value   "A new failure message."))
  (make-instance 'exmes+failure-message
		 :state-description state-description
		 :report report))




