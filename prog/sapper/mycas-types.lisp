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
 
(mod~defmod cat :uses (mod sys com arg term prob omega keim type inter 
			  asi pds rule)
	    :documentation "Definition of COMPALG argument types."
	    :exports ())

;;;begin{latex}
;;;\chapter{Compalg argument types}
;;; We define various argument types for the Compalg commands. 
;;;end{latex}

;;; The POLY argtype is the class of all polynomials.

(eval-when (load compile eval)
(arg~deftype poly
 (predicate polynomial~p)
 (read-function cat~read-poly)
 (help "a polynomial"))
)

(defun cat=read-poly (obj type)
  (sys~handler-case 
   (term~read obj (prob~environment omega*current-proof-plan))
   (error () (arg~signal-wrong-type type obj))))

(defmethod cat~read-poly ((obj ca+polynomial) &rest others)
  (declare (ignore others))
  obj)

(defmethod cat~read-poly ((obj t) &rest others)
  (declare (ignore others))
  (cat=read-poly obj 'poly))

;;; The FIELD argtype consists of all field identifiers for polynomials.

(eval-when (load compile eval)
(arg~deftype field
 (predicate cat~field-p)
 (read-function cat~read-field)
 (help "a field identifier"))
)

(defconstant cat*id-list '(#\n #\z #\q #\r #\c))

(defun cat~field-p (obj)
  (declare (edited  "22-JUN-1995 17:41")
	   (authors SORGE)
	   (input   "A lisp object")
	   (value   "T if obj is a character and a valid field identifier"))
  (and (characterp obj)
       (member obj cat*id-list)))

(defmethod cat~read-field (obj &rest others)
  (let ((newobj (ot~read-term obj)))
    (if (ot~formula-p newobj)
	newobj
	(arg~signal-wrong-type 'field obj))))

;;; The VALUE argtype is some number.

(eval-when (load compile eval)
(arg~deftype value
 (predicate numberp)
 (read-function cat~read-value)
 (help "a value"))
)

(defmethod cat~read-value (obj &rest others)
  (let ((newobj (ot~read-term obj)))
    (if (numberp newobj)
	newobj
	(arg~signal-wrong-type 'value obj))))

;;; The FLAG argtype is a boolean.

(eval-when (load compile eval)
(arg~deftype flag
 (predicate cat~flag-p)
 (read-function cat~read-flag)
 (help "a term variable"))
)

(defun cat~flag-p (obj)
  (declare (edited  "22-JUN-1995 18:00")
	   (authors SORGE)
	   (input   "A lisp object")
	   (value   "T if object is either t or nil"))
  (or (null obj)
      (equal obj t)))

(defmethod cat~read-flag (obj &rest others)
  (let ((newobj (ot~read-term obj)))
    (if (cat~flag-p newobj)
	newobj
	(arg~signal-wrong-type 'flag obj))))

;; And the all the stuff as lists as well

(arg~deflisttype poly-list poly)

(arg~deflisttype value-list value)

(arg~deflisttype field-list field)

(arg~deflisttype flag-list flag)

