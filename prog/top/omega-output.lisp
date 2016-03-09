;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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
 

(mod~defmod OOUT
            :uses (asi comint inter)
            :documentation "OMEGA output functions."
            :exports (
		      omega~error
                      omega~message
                      omega~output
                      omega~query
                      omega~trace
                      omega~warn
                      ))

;;
;; Output for Omega
;;

(defmacro omega=output (stream)
  (declare (edited  "03-MAR-1998")
	   (authors Sorge Konrad)
	   (input   "A function specifying the interface-stream.")
	   (effect  "Prints arguments to the specified stream of the OMEGA interface.")
	   (value   "NIL"))
  `(let ((interface (if (and (boundp 'comint*current-comint) comint*current-comint)
			(comint~interface comint*current-comint)
		      (asi~create))))
     (,stream interface (apply #'format nil args))
     nil))

(defun omega~output (&rest args)
  (omega=output inter~output-object))

(defun omega~message (&rest args)
  (omega=output inter~print-message))
  
(defun omega~warn (&rest args)
  (omega=output inter~print-warning))
		         

(defun omega~error (&rest args)
  (let ((interface (if (and (boundp 'comint*current-comint) comint*current-comint)
		       (comint~interface comint*current-comint)
		     (asi~create))))
    (cond ((typep (car args) 'condition)
	   (inter~print-error interface (car args)))
	  ((= (length args) 1) 
	   (inter~print-error interface (string (car args))))
	  (T (inter~print-error interface (apply #'format nil args))))
    nil))

(defun omega~trace (&rest args)
  (omega=output inter~print-trace))

(defun omega~query (&rest args)
  (declare (edited  "26-APR-2000")
	   (authors Pollet)
	   (input   "Same as in omega-processes.lisp.")
	   (effect  "Same as in omega-processes.lisp, but without LOUI-Dialog.")
	   (value   "Same as in omega-processes.lisp."))
  (let ((queries (if (listp (car args)) args (list args)))
	(interface (if (and (not (boundp 'comint*current-comint))  comint*current-comint)
		       (comint~interface comint*current-comint)
		     (asi~create)))
	(result (mapcar #'(lambda (arg)
			    (inter~input-query interface (car arg)(second arg)))
			queries)))
    (if (listp (car args)) result (car result))))


(defun omega~query-choice (text choices &optional defaultnumber)
  (declare (edited  "08-MAY-2000" )
	   (authors Pollet)
	   (input   "Same as in omega-processes.lisp.")
	   (effect  "Same as in omega-processes.lisp, but without LOUI-Dialog.")
	   (value   "Same as in omega-processes.lisp."))
  (omega~message text)
  (omega~message "~%")
  (do ((choice choices (rest choice))
       (number 1 (1+ number)))
      ((null choice))
    (omega~message "(~A): ~A~%"
		   number (car choice)))
  (let* ((interface (if (and (not (boundp 'comint*current-comint))  comint*current-comint)
		       (comint~interface comint*current-comint)
			    (asi~create)))
	(number (1- (if defaultnumber 
			(inter~prompt-for-input-with-default
			 interface "Enter a number: " 'posinteger (1+ defaultnumber))
		      (inter~input-query
		       interface "Enter a number: " 'posinteger)))))
	  number))



;(defun omega~query (prompt argtyps)
;  (let ((interface (if (not (boundp 'comint*current-comint))
;                       (comint~interface comint*current-comint)
;                     (asi~create))))
;    (inter~input-query interface prompt argtype)))
