;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
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


(mod~defmod SCON 
            :uses (black keim sod store)
            :documentation "Stuff and Data-Structures for strategic control steps"
            :exports (
                      scon+strategy-step
                      
                      scon~add-step-behind-step!
                      scon~create-scon-step
                      scon~delete-step!
                      scon~initialize-with-step!
                      scon~replace-step!
                      scon~strategy-steo-predecessor
                      scon~strategy-step-just
                      scon~strategy-step-p
                      scon~strategy-step-predecessor
                      scon~strategy-step-successor
                      ))



;; This module contains the data structures for the control of the sod system.

(eval-when (load compile eval)
  (defclass scon+strategy-step (keim+object)
    ((predecessor :initarg :predecessor
		  :initform nil
		  :accessor scon~strategy-step-predecessor
		  :documentation "The predecessor step of a strategy-step.")
     (successor :initarg :successor
		:initform nil
		:accessor scon~strategy-step-successor
		:documentation "The successor step of a strategy-step.")
     (just :initarg :just
	   :initform nil
	   :accessor scon~strategy-step-just
	   :documentation "The justification of this strategy-step."))))

(defun scon~strategy-step-p (obj)
  (declare (edited  "22-JUN-1999")
	   (authors Ameier)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is of type scon+strategy-step."))
  (typep obj 'scon+strategy-step))

(defmethod print-object ((scon-step scon+strategy-step) stream)
  (format stream "!Scon-step with just: ~A" (scon~strategy-step-just scon-step)))

(defun scon~create-scon-step (pred-step succ-step just)
  (declare (edited  "22-JUN-1999")
	   (authors Ameier)
	   (input   "A predecessor-step, a succ-step, a justification.")
	   (effect  "None.")
	   (value   "A new scon step."))
  (make-instance 'scon+strategy-step
		 :predecessor pred-step
		 :successor succ-step
		 :just just))

#| ---------------------------------------------------------- Changes ------------------------------------------------------------ |#

;; Each changing effects also the ssteps-list store on the sod*solution-blackboard!

(defun scon~initialize-with-step! (new-step)
  (declare (edited  "26-MAR-2000")
	   (authors Ameier)
	   (input   "A new step.")
	   (effect  "Initializes the 'ssteps-list entries with this step.")
	   (value   "Undefined."))

  (store~add-element! (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)
		      new-step))
  
(defun scon~add-step-behind-step! (new-step old-step)
  (declare (edited  "22-JUN-1999")
	   (authors Ameier)
	   (input   "A new step and an old-step.")
	   (effect  "The new step is added behind the old-step. This means, the pred-slot of new step"
		    "is set to the old-step, while its succ-slot is set to the succ-slot of the old-step."
		    "If the succ-slot of the old-step is not nil, let's say a step S, then the pred-slot of S"
		    "is set to the new step. The succ-slot of the old-step  is set to the new step.")
	   (value   "Undefined."))
  (let* ((old-step-succ (scon~strategy-step-successor old-step)))
    
    (when old-step-succ
      (setf (scon~strategy-step-successor new-step) old-step-succ)
      (setf (scon~strategy-steo-predecessor old-step-succ) new-step)
      )

    
    (setf (scon~strategy-step-successor old-step) new-step)
    (setf (scon~strategy-step-predecessor new-step) old-step)
    
    (store~add-element! (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)
			new-step)
    ))


(defun scon~replace-step! (new-step old-step)
  (declare (edited  "24-JUN-1999")
	   (authors Ameier)
	   (input   "A new step and an old-step.")
	   (effect  "The old step is replaced by the new step (changes the successor/predecessor entries"
		    "in the new-step and in the steps connected until now with the old-step).")
	   (value   "Undefined."))
  (let* ((pred (scon~strategy-step-predecessor old-step))
	 (succ (scon~strategy-step-successor old-step))
	 (ssteps-list (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)))

    (setf (scon~strategy-step-successor new-step) succ)
    (setf (scon~strategy-step-predecessor new-step) pred)

    (when pred
      (setf (scon~strategy-step-successor pred) new-step))

    (when succ
      (setf (scon~strategy-step-predecessor succ) new-step))

    (when (eq old-step (black~get-blackboard-object-content 'first-strategy-ks-step sod*control-blackboard))
      (black~change-blackboard-object-content! 'first-strategy-ks-step new-step sod*control-blackboard))
    (when (eq old-step (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard))
      (black~change-blackboard-object-content! 'last-strategy-ks-step new-step sod*control-blackboard))
    
    (store~add-element! ssteps-list new-step)
    (store~remove-element! ssteps-list old-step)))

(defun scon~delete-step! (step)
  (declare (edited  "24-JUN-1999")
	   (authors Ameier)
	   (input   "A step.")
	   (effect  "The step is deleted (this changes the predecessor and successor entries"
		    "of the steps connected with it).")
	   (value   "Undefined."))
  (let* ((pred (scon~strategy-step-predecessor step))
	 (succ (scon~strategy-step-successor step)))
    
    (when pred
      (setf (scon~strategy-step-successor pred) succ))

    (when succ
      (setf (scon~strategy-step-predecessor succ) pred))

    (when (eq step (black~get-blackboard-object-content 'first-strategy-ks-step sod*control-blackboard))
      (black~change-blackboard-object-content! 'first-strategy-ks-step succ sod*control-blackboard))
    (when (eq step (black~get-blackboard-object-content 'last-strategy-ks-step sod*control-blackboard))
      (black~change-blackboard-object-content! 'last-strategy-ks-step pred sod*control-blackboard))
    
    (store~remove-element! (black~get-blackboard-object-content 'ssteps-list sod*solution-blackboard)
			   step)
    ))

(defun scon~all-steps-after (step)
  (declare (edited  "29-AUG-2000")
	   (authors Ameier)
	   (input   "A step.")
	   (effect  "None.")
	   (value   "A list of all steps after this step."))
  (let* ((succ (scon~strategy-step-successor step)))
    (if succ
	(cons succ (scon~all-steps-after succ))
      nil)))

(defun scon~all-steps-before (step)
  (declare (edited  "29-AUG-2000")
	   (authors Ameier)
	   (input   "A step.")
	   (effect  "None.")
	   (value   "A list of all steps after this step."))
  (let* ((pred (scon~strategy-step-predecessor step)))
    (if pred
	(append (scon~all-steps-before pred) (list pred))
      nil)))

(defun scon~all-steps ()
  (declare (edited  "29-AUG-2000")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "NOne.")
	   (value   "A list of all current steps."))
  (let* ((first-step (black~get-blackboard-object-content 'first-strategy-ks-step sod*control-blackboard)))
    (if (null first-step)
	nil
      (cons first-step (scon~all-steps-after first-step)))))
