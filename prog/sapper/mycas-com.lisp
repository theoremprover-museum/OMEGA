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
 
(mod~defmod cas :uses (mod sys com omega keim help
			  asi arg prob post env)
	    :documentation "Definition of COMPALG commands."
	    :exports ())


(defvar cas*current-object nil)
(defvar cas*current-symbol-stack nil)

;;; CREATE-POLY creates a polynomial from a string and binds it to a symbol

(defun cas=create-poly (symb str)
  (inter~output-object (comint~interface comint*current-comint)
		       (setf (symbol-value symb) (ca~create-polynomial str)))
  (cas=push symb))

;;; The command ADD-POLY adds up two polynomials

(defun cas=add-poly (poly1 poly2)
  (setf cas*current-object (ca~polynomial-addition (eval poly1) (eval poly2)))
  (inter~output-object (comint~interface comint*current-comint) cas*current-object))

;;; The command SUB-POLY deducts poly2 from poly2

(defun cas=sub-poly (poly1 poly2)
  (setf cas*current-object (ca~polynomial-subtraction (eval poly1) (eval poly2)))
  (inter~output-object (comint~interface comint*current-comint) cas*current-object))

;;; The command MULT-POLY multiplies two polynomials

(defun cas=mult-poly (poly1 poly2 key)
  (setf cas*current-object (ca~polynomial-multiplication (eval poly1) (eval poly2) :scheme key))
  (inter~output-object (comint~interface comint*current-comint) cas*current-object))

;;; The command SCALAR-MULT multiplies a polynomial with a scalar

(defun cas=scalar-mult (poly scalar)
  (setf cas*current-object (ca~scalar-mult-on-poly (eval poly) scalar))
  (inter~output-object (comint~interface comint*current-comint) cas*current-object))

;;; DIFF-POLY differentiates a polynomial

(defun cas=diff-poly (poly variable)
  (setf cas*current-object (ca~differentiate-polynomial (eval poly) variable))
  (inter~output-object (comint~interface comint*current-comint) cas*current-object))


;;; INTEG-POLY integrates a polynomial

(defun cas=integ-poly (poly variable constant)
  (setf cas*current-object (ca~integrate-polynomial (eval poly) variable constant))
  (inter~output-object (comint~interface comint*current-comint) cas*current-object))


;;; ROOTS computes the roots of a polynomial

(defun cas=roots (poly)
  (setf cas*current-object (ca~polynomial-roots (eval poly)))
  (inter~output-object (comint~interface comint*current-comint) cas*current-object))


;;; The command SHOW-POLYS shows all polynomials bound to members of CAS*CURRENT-SYMBOL-STACK

(defun cas=show-polys ()
  (mapcar
   #'(lambda (x) (inter~output-object (comint~interface comint*current-comint) (format nil "~A: ~A~%" x (eval x))))
   cas*current-symbol-stack))

;;; The command SHOW-POLY shows a specified polynomial bound to members of CAS*CURRENT-SYMBOL-STACK

(defun cas=show-poly (poly)
  (when (member poly cas*current-symbol-stack)
     (inter~output-object (comint~interface comint*current-comint) (format nil "~A: ~A~%" poly (eval poly)))))

(defun cas=push (symbol)
  (declare (edited  "04-JUL-1995 20:42")
	   (authors SORGE)
	   (input   "A symbol")
	   (effect  "Pushes a new symbol on CAS*CURRENT-SYMBOL-STACK")
	   (value   "Undefined"))
  (pushnew symbol cas*current-symbol-stack))

;;; STORE-LAST-POLY pushes the last computes polynomial on the stack

(defun cas=store-last-poly (symbol)
  (when (ca~polynomial-p cas*current-object)
    (setf (symbol-value symbol) cas*current-object)    
    (cas=push symbol)))

(defun cas=copy-poly (poly symbol)
  (when (member poly cas*current-symbol-stack)
    (setf (symbol-value symbol) (ca~copy-polynomial (eval poly)))
    (cas=push symbol)))

(defun cas=delete-poly (poly)
  (when (member poly cas*current-symbol-stack)
     (delete poly cas*current-symbol-stack)))


; ;;; Finally we create the command-interpreter MYCAS  <== commented due to multiprocessing VS!

;;; Command interpreter in OMEGA
;; (com~defcommand mycas
;;   (function cas=mycas)
;;   (argnames)
;;   (argtypes)
;;   (arghelps)
;;   (frag-cats omega change-comint no-gui)
;;   (defaults)
;;   (help "Enters the command interpreter for myCAS."))

;; (defun cas=mycas ()
;;   (comint~top (comint~create :myCAS
;; 			    (list (com~find-fragment 'comint) (com~find-fragment 'mycas))
;; 			    (comint~interface comint*current-comint)
;; 			    (comint~logfile comint*current-comint))))




