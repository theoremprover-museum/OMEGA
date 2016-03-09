;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
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

(in-package :keim)




(mod~defmod SKSYM 
            :uses (data env post term type)
            :documentation "Skolem Constants and Extensionality Skolem Constants"
            :exports (
                      sksym+constant
                      sksym+ext-constant
                      sksym+sk-constant
                      
                      sksym~arity
                      sksym~check
                      sksym~create
                      sksym~env-enter
		      sksym~env-enter-ext
                      sksym~ext-create
                      sksym~ext-p
                      sksym~p
                      sksym~sk-p
                      ))






(defclass sksym+constant (term+constant)
;;        ==============
  ((arity :initarg :arity :accessor sksym~arity))
  (:documentation "Skolem constants"))



(defclass sksym+sk-constant (sksym+constant)
;;        ==============
  ()
  (:documentation "A symbol which has a determined arity, to be used as "
		  "a Skolem constant."))


(defclass sksym+ext-constant (sksym+constant)
;;        ==============
  ()
  (:documentation "extensionality skolem symbols"))


(defun sksym~p (object)
  (typep object 'sksym+constant))

(defun sksym~sk-p (object)
;;     =======
  (declare (edited  "14-NOV-1996")
	   (authors Gebhard)
	   (input   "An object")
	   (effect  "None")
	   (value   "True if object is of class sksym+sk-constant, else nil"))
 (typep object 'sksym+sk-constant))


(defun sksym~ext-p (object)
;;     =======
  (declare (edited  "14-NOV-1996")
	   (authors Gebhard)
	   (input   "An object")
	   (effect  "None")
	   (value   "True if object is of class sksym+ext-constant, else nil"))
 (typep object 'sksym+ext-constant))

(defun sksym~env-enter (symbol type arity env)
;;     ===============
  (declare (edited  "14-NOV-1996")
	   (authors Gebhard)
	   (input   "A symbol for th name of the Skolemconstant, her type "
		    "and arity, and an environment")
	   (effect  "A skolemconstant with the key 'symbol' enters into env" )
	   (value   "A value existing of the created sk-constant and env if"
		    "successful, else error"))
  (when (not (symbolp symbol))  (error "~S is not a symbol." symbol))
  (when (not (env~p env))       (error "~S is not an environment." env))
  (when (not (type~p type))     (error "~S is not a type." type))
  (let ((thing (env~lookup-object symbol env)))
    (cond ((null thing)
	   (let ((skolsym (sksym~create symbol type arity)))
	         (env~enter symbol skolsym env)
;	         (format t "Neues Element in env eingefuegt!")
	         (values skolsym env)))
	  ((not (sksym~p thing))
	         (post~error "Can't declare ~A as a skolem constant: ~
                              it already exists in the environment as a ~A." 
		               symbol (class-name (class-of thing))))
	  (t (if (and (data~equal type (data~annotation thing))
		      (= arity (sksym~arity thing)))
		 (values thing env)
	       (post~error "Can't declare ~A as a skolemconstant because ~A ~
                            already exists in the environment but with ~
                            type ~A and arity ~A instead of type ~A and ~
                            arity ~A" symbol thing (data~annotation thing)
			    (sksym~arity thing) type arity))))))



(defun sksym~env-enter-ext (symbol type arity env)
;;     ===============
  (declare (edited  "14-NOV-1996")
	   (authors Gebhard)
	   (input   "A symbol for th name of the Ext-Skolemconstant, her type "
		    "and arity, and an environment")
	   (effect  "A ext-skolemconstant with the key 'symbol' enters into env" )
	   (value   "A value existing of the created ext-sk-constant and env if"
		    "successful, else error"))
  (when (not (symbolp symbol))  (error "~S is not a symbol." symbol))
  (when (not (env~p env))       (error "~S is not an environment." env))
  (when (not (type~p type))     (error "~S is not a type." type))
  (let ((thing (env~lookup-object symbol env)))
    (cond ((null thing)
	   (let ((skolsym (sksym~ext-create symbol type arity)))
	         (env~enter symbol skolsym env)
;	         (format t "Neues Element in env eingefuegt!")
	         (values skolsym env)))
	  ((not (sksym~p thing))
	         (post~error "Can't declare ~A as a skolem constant: ~
                              it already exists in the environment as a ~A." 
		               symbol (class-name (class-of thing))))
	  (t (if (and (data~equal type (data~annotation thing))
		      (= arity (sksym~arity thing)))
		 (values thing env)
	       (post~error "Can't declare ~A as a skolemconstant because ~A ~
                            already exists in the environment but with ~
                            type ~A and arity ~A instead of type ~A and ~
                            arity ~A" symbol thing (data~annotation thing)
			    (sksym~arity thing) type arity))))))
                       


(defgeneric sksym~create (thing type arity)
;;          ============
  (declare (edited  "14-NOV-1996")
	   (authors Gebhard)
	   (input   "A name, a type and an integer.")
	   (effect  "None.")
	   (value   "If Input is valid, the corresponding skolemconstant,"
		    "else error."))
 (:method (thing type (arity integer)) ;; &optional refsym
	   (if (<= 0 arity (length (data~n-domain type)))
	       (let ((new-const (make-instance 'sksym+sk-constant
					       :name thing
					       :annotation type
					       :arity arity)))
		 (setf (data~constant-origin new-const) new-const)
		 new-const)
	     (post~error "The arity ~A is not possible for a skolem ~
                            constant of type ~A" arity type))))




(defgeneric sksym~ext-create (thing type arity)
;;          ============
  (declare (edited  "14-NOV-1996")
	   (authors Gebhard)
	   (input   "A name, a type and an integer.")
	   (effect  "None.")
	   (value   "If Input is valid, the corresponding skolemconstant,"
		    "else error."))
 (:method (thing type (arity integer)) ;; &optional refsym
	   (if (<= 0 arity (length (data~n-domain type)))
	       (let ((new-const (make-instance 'sksym+ext-constant
					       :name thing
					       :annotation type
					       :arity arity)))
		 (setf (data~constant-origin new-const) new-const)
		 new-const)
	     (post~error "The arity ~A is not possible for a ext-skolem ~
                            constant of type ~A" arity type))))






;;;;;;;;;;;;;;;; check for correct higher order terms with skolem functions ;;;;;;;;;

;; A checking function to test whether all occurences of skolemsymbols in an
;; object are correctly used.


(defun sksym~check (object)
  (declare (edited  "14-NOV-1996")
	   (authors Gebhard)
	   (input   "An object")
	   (effect  "None")
	   (value   "The object if every condition is ok, else nil"))
     (if (sksym=check object nil)
	 object
         nil))








(defgeneric sksym=var-test (object varlist)
  (declare (edited  "20-NOV-1996")
	   (authors Gebhard)
	   (input   "An object and a list of variables")
	   (effect  "None.")
	   (value   "True if object contains no wrong occurence of an element of varlist,"
		    "else nil"))
  (:method ((object term+appl) varlist)
	   ;struktureller Abstieg: Test wird fuer beide Komponenten durchgefuehrt
	   (and (sksym=var-test (data~appl-function object) varlist)
		(sksym=var-test (data~appl-arguments object) varlist)))
  (:method ((object term+abstr) varlist)
	   ;Die durch die Abstraktion gebundenen Variablen sind in der domain erlaubt
	   ;und muessen aus varlist geloescht werden.
	   ;Struktureller Abstieg in die Domain mit der (evt.) verkleinerten varlist.
	   (sksym=var-test (data~abstr-range object)
			   (set-difference varlist (data~abstr-domain object))))
  (:method ((object term+primitive) varlist)
	   ; Primitive duerfen nicht in der  varlist enthalten sein.
	   (if (member object varlist)
	       nil
	     t))
  (:method ((object list) varlist)
	   ; in einer Liste muessen alle elemente die Bedingungen erfuellen
	   (if (every #'(lambda (x) (sksym=var-test x varlist)) object)
	       t
	     nil))
  (:method ((object sksym+sk-constant) varlist)
	   ;Auch ein skolemsymbol isr nur erlaubt wenn es nicht in der verbotenen Liste ist.
	   (if (member object varlist)
	       nil
	     t)))

 




