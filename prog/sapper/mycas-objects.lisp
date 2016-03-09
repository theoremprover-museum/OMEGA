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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the file with the object definitions for myCAS
;; i.e. here are some extensions of the methods:
;; CA~BUILD-OBJECT
;; CA~REBUILD-OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We always expect to receive completely expanded terms.
;; Otherwise some of the algorithms below might just return nonsense.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; polynomials

(defconstant symnum*add-function :plus)
(defconstant symnum*mult-function :times)
(defconstant symnum*pow-function :power)
(defconstant symnum*number-type :num)


(defmethod ca~build-object (term (object (eql :realpoly)) (system (eql :mycas)))
  (ca=build-mycas-poly #\r term))

(defmethod ca~build-object (term (object (eql :ratpoly)) (system (eql :mycas)))
  (ca=build-mycas-poly #\q term))

(defmethod ca~build-object (term (object (eql :intpoly)) (system (eql :mycas)))
  (ca=build-mycas-poly #\z term))

(defun ca=build-mycas-poly (field term)
  (let* ((vars (data~abstr-binder term))
	 (scope (data~struct-at-position term (pos~list-position '(0)))))
    (if (and (term~number-p scope) (= (keim~name scope) 0))
	(ca~create-polynomial nil :var (length vars) :field field)
      (ca~create-polynomial
       (mapcar #'(lambda (x)
		   (remove-if-not #'numberp (mapcar #'keim~name x)))
	       (mapcar #'(lambda (x) (ca=dec-on-symb x symnum*pow-function))
		       (mapcar #'(lambda (x) (ca=dec-on-symb x symnum*mult-function))
			       (ca=dec-on-symb scope symnum*add-function))))
       :var (length vars)
       :field field))))

(defun ca=dec-on-symb (term symbol)
  (declare (edited  "16-JUL-1995 22:13")
	   (authors SORGE)
	   (input   "A term representing the POST form of a polynomial and a logical symbol.")
	   (value   "A list of monomials in POST syntax."))
  (cond ((data~appl-p term)
	 (let ((func (data~appl-function term))
	       (args (data~appl-arguments term)))
	   (if (string-equal (keim~name func) symbol)
	       (append (ca=dec-on-symb (car args) symbol)
		       (ca=dec-on-symb (cdr args) symbol))
	     (list term))))
	((consp term) (append (ca=dec-on-symb (car term) symbol)
			      (ca=dec-on-symb (cdr term) symbol)))
	((null term) term)
	(t (list term))))

;;; numbers

(defmethod ca~build-object (term (object (eql :nat)) (system (eql :mycas)))
  (let ((number (ca=build-mycas-number term)))
    (unless (typep number '(integer 1))
      (error "~A is not a positive integer" number))
    number))

(defmethod ca~build-object (term (object (eql :real)) (system (eql :mycas)))
  (let ((number (ca=build-mycas-number term)))
    (unless (realp number)
      (error "~A is not a positive integer" number))
    number))

(defun ca=build-mycas-number (term)
  (keim~name term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rebuilding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod ca~rebuild-object ((term ca+polynomial) (object (eql :realpoly)) (system (eql :mycas)))
  (ca=rebuild-mycas-poly term))

(defmethod ca~rebuild-object ((term ca+polynomial) (object (eql :ratpoly)) (system (eql :mycas)))
  (ca=rebuild-mycas-poly term))

(defmethod ca~rebuild-object ((term ca+polynomial) (object (eql :intpoly)) (system (eql :mycas)))
  (ca=rebuild-mycas-poly term))

(defun ca=rebuild-mycas-poly (term)
  (declare (edited  "30-NOV-1995 16:27")
	   (authors SORGE)
	   (input   "A polynomial in myCAS-syntax.")
	   (effect  "Creates a logical object in OMEGA-syntax")
	   (value   "The created object"))
  (let* ((data (ca~poly-data term))
	 (type (env~lookup-object symnum*number-type ca*global-environment))
	 (mult (env~lookup-object symnum*mult-function ca*global-environment))
	 (plus (env~lookup-object symnum*add-function ca*global-environment))
	 (pow (env~lookup-object symnum*pow-function ca*global-environment))
	 (vars (do ((i (ca~poly-var-number term) (1- i))
		    (list nil (append list (list (term~generate-term-primitive-with-new-name
						  "X" type 'term+variable ca*global-environment)))))
		   ((= i 0) list))))
    (if data
	(term~abstr-create
	 vars
	 (ca=comp-w-symb
	  plus
	  (mapcar #'(lambda (x)
		      (ca=comp-w-symb
		       mult
		       (append (list (ca=create-term (car x)))
			       (map 'list
				    #'(lambda (y z)
					(ca=pow-create pow y (ca=create-term z)))
				    vars
				    (cdr x)))))
		  data)))
      (term~abstr-create vars (ca=create-term 0)))))

(defun ca=pow-create (pow var exp)
  (declare (edited  "28-NOV-1995 14:35")
	   (authors SORGE)
	   (input   "A power-function, a variable and an exponent.")
	   (effect  "Creates the application (POW VAR EXP)." )
	   (value   "The new application."))
	   (data~appl-create pow
			(list var exp)))


(defun ca=comp-w-symb (symb args)
  (declare (edited  "28-NOV-1995 15:06")
	   (authors SORGE)
	   (input   "A logical function and a list of arguments")
	   (effect  )
	   (value   ))
  (labels ((ca=cws-inline (symb args)
	       (if (null (cdr args)) args
		 (list (data~appl-create symb
				    (append
				     (list (car args))
				     (ca=cws-inline symb (cdr args))))))))
    (car (ca=cws-inline symb args))))


(defgeneric ca=create-term (term)
  (declare (edited  "28-NOV-1995 15:41")
	   (authors SORGE)
	   (input   "A term in CA-Syntax.")
	   (effect  )
	   (value   "Creates the corresponding KEIM-term."))
  (:method (term)
	   (error "~A is an invalid argument." term))
  (:method ((term term+term))
	    term)
  (:method ((term number))
	   (term~constant-create term
				(env~lookup-object symnum*number-type ca*global-environment)))
  (:method ((term symbol))
	   (env~lookup-object term ca*global-environment)))
	   

(defmethod ca~rebuild-object ((term real) (object (eql :real)) (system (eql :mycas)))
  (ca=rebuild-mycas-number term))

(defmethod ca~rebuild-object ((term integer) (object (eql :nat)) (system (eql :mycas)))
  (unless (typep term '(integer 0))
    (error "~A is not a natural number." term))
  (ca=rebuild-mycas-number term))

(defun ca=rebuild-mycas-number (number)
  (term~constant-create number (env~lookup-object symnum*number-type ca*global-environment)))


