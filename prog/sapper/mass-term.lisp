;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: KEIM -*-
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


(mod~defmod MASS 
            :uses (ca data env keim omega pds pos term)
            :documentation "The terms of the mu algebraic support system."
            :exports (
                      mass+monatom
                      mass+monom
                      mass+term
                      mass+termatom
                      
                      mass~append
                      mass~atom
                      mass~create
                      mass~div
                      mass~eql
                      mass~equal
                      mass~exp-base-list
                      mass~first-monatom
                      mass~first-termatom
                      mass~get-args
                      mass~get-base
                      mass~get-coeff
                      mass~get-function
                      mass~get-lambda-list
                      mass~get-monom
                      mass~get-term
                      mass~inverse
                      mass~minus
                      mass~monom-list
                      mass~monom-rest
                      mass~number-p
                      mass~number-value
                      mass~one
                      mass~pderiv
                      mass~plus
                      mass~pop
                      mass~power
                      mass~pplus
                      mass~prove-eql
                      mass~prove-eql*
                      mass~read
                      mass~read-args
                      mass~rebuild
                      mass~remove-if-contained
                      mass~set-args
                      mass~set-coeff
                      mass~set-function
                      mass~set-monom
                      mass~set-term
                      mass~sinkable
                      mass~stimes
                      mass~term-rest
                      mass~times
                      mass~zero
                      
                      mass*div
                      mass*equality
                      mass*expt
                      mass*mult
                      mass*num
                      mass*pderiv
                      mass*plus
                      mass*power
                      mass*mod
                      mass*pplus
                      mass*stimes
                      mass*sub
                      mass*unknown))


(defclass mass+term ()
  ((term          :initarg     :term
		  :initform    nil
		  :reader      mass~get-term
		  :writer      mass~set-term))
  (:documentation "mass+term represents a term using a list of
                   mass+termatoms. The meaning of NIL in the
                   TERM slot is zero, a value of a list is the
                   sum of its elements."))

(defclass mass+termatom ()
  ((coeff         :initarg     :coeff
		  :initform    1
		  :reader      mass~get-coeff
		  :writer      mass~set-coeff)
   (monom         :initarg     :monom
		  :initform    (make-instance 'mass+monom)
		  :reader      mass~get-monom
		  :writer      mass~set-monom))
  (:documentation "mass+termatom represents a monom and a coefficient,
                   which is a number. Its value is COEFF * MONOM."))

(defclass mass+monom ()
  ((monom         :initarg     :monom
		  :initform    nil
		  :reader      mass~get-monom
		  :writer      mass~set-monom))
  (:documentation "mass+monom represents a monom which is a list of
                   monatoms. Its value is the product of its members.
                   An empty list means 1."))

(defclass mass+monatom ()
  ((func          :initarg     :func
		  :initform    mass*unknown
		  :reader      mass~get-function
		  :writer      mass~set-function)
   (args          :initarg     :args
		  :initform    nil
		  :reader      mass~get-args
		  :writer      mass~set-args))
  (:documentation "mass+monatom represents a piece of a term that is
                   not simplifiable any more (at least not using this
                   CAS). Its value ist the application of FUNC to
                   ARGS, the type of elements contained in ARGS
                   depends on FUNC."))


;; The following predicates and functions should be defined for
;; diverse combinations of POST term, MASS term, MASS termatom and
;; MASS monatom.

(defgeneric mass~eql   (a b))

(defgeneric mass~plus  (a b &optional pos))

(defgeneric mass~minus (a b &optional pos))

(defgeneric mass~times (a b &optional pos))

(defgeneric mass~div   (a b &optional pos))

(defgeneric mass~sqrt  (a &optional pos))

(defgeneric mass~power (a b &optional pos))

(defgeneric mass~mod   (a b &optional pos))

(defgeneric mass~inverse     (a &optional pos))

(defgeneric mass~number-p     (a))

(defgeneric mass~number-value (a))

(defgeneric mass~zero        (a))

(defgeneric mass~one         (a))

(defgeneric mass~read        (a &optional (pos (pos~empty))))

(defgeneric mass~create      (a))

(defgeneric mass~remove-if-contained (a b))

(defgeneric mass~append (a b))

(defgeneric mass~monom-list (a))

(defgeneric mass~exp-base-list (a))

(defgeneric mass~get-base (a))

(defgeneric mass~sinkable (a b))

(defgeneric mass~mod-reducible (a n))

(defgeneric mass~mod-argument (a))

(defgeneric mass~is-mod-term (a n))

(defgeneric mass~is-mod-term* (a n))

;; Proof construction:

(defgeneric mass~prove-eql (a b &optional pos))

(defgeneric mass~prove-eql* (a b n &optional pos))

(defgeneric mass~pop (a b &optional pos))


;; Creating an MASS term:

(defmethod mass~create ((a mass+term))
  a)

(defmethod mass~create ((a mass+termatom))
  (cond ((mass~zero a)
	 (mass~create 0))
	(t
         (make-instance 'mass+term :term (list a)))))

(defmethod mass~create ((a mass+monom))
  (mass~create (make-instance 'mass+termatom :monom a)))

(defmethod mass~create ((a mass+monatom))
  (mass~create (make-instance 'mass+monom :monom (list a))))

(defmethod mass~create ((a number))
  (cond ((= 0 a)
	 (make-instance 'mass+term))
	(t
         (mass~create (make-instance 'mass+termatom :coeff a)))))

(defmethod mass~create ((a term+variable))
  (mass~create (make-instance 'mass+monatom
			     :func mass*expt
			     :args (list a
					 (mass~create 1)))))

(defmethod mass~create ((a term+constant))
  (mass~create (make-instance 'mass+monatom
			     :func mass*expt
			     :args (list a
					 (mass~create 1)))))

   
;; Accessing the data in MASS term structures:

(defmethod mass~atom ((a mass+term))
  (mass~zero (mass~term-rest a)))

(defmethod mass~atom ((a mass+monom))
  (mass~one (mass~monom-rest a)))


(defmethod mass~first-termatom (a)
  (car (mass~get-term a)))


(defmethod mass~term-rest      (a)
  (make-instance 'mass+term :term (cdr (mass~get-term a))))


(defmethod mass~first-monatom  (a)
  (car (mass~get-monom a)))


(defmethod mass~monom-rest     (a)
  (make-instance 'mass+monom :monom (cdr (mass~get-monom a))))



(defmethod mass~remove-if-contained ((a mass+monatom) (b mass+monom))
  (cond ((mass~one b)
	 (list nil (make-instance 'mass+monom)))
	((mass~eql a (mass~first-monatom b))
	 (list t (mass~monom-rest b)))
	(t
	 (let ((RIC (mass~remove-if-contained a (mass~monom-rest b))))
	   (list (first RIC)
                 (mass~append (mass~first-monatom b)
			     (second RIC)))))))

(defmethod mass~remove-if-contained ((a mass+termatom) (b mass+term))
  (cond ((mass~zero b)
	 (list nil (make-instance 'mass+term)))
	((mass~eql a (mass~first-termatom b))
	 (list t (mass~term-rest b)))
	(t
	 (let ((RIC (mass~remove-if-contained a (mass~term-rest b))))
	   (list (first RIC)
                 (mass~append (mass~first-termatom b)
			     (second RIC)))))))



(defmethod mass~append ((a mass+termatom) (b mass+term))
  (make-instance 'mass+term :term (cons a
				       (mass~get-term b))))

(defmethod mass~append ((a mass+monatom) (b mass+monom))
  (make-instance 'mass+monom :monom (cons a
					 (mass~get-monom b))))

(defmethod mass~monom-list ((a mass+term))
  (cond ((mass~zero a)
	 nil)
	(t
	 (cons (mass~get-monom (mass~first-termatom a))
	       (mass~monom-list (mass~term-rest a))))))

(defmethod mass~exp-base-list ((a mass+monom))
  (cond ((mass~one a)
	 nil)
	(t
	 (cons (mass~get-base (mass~first-monatom a))
	       (mass~exp-base-list (mass~monom-rest a))))))

(defmethod mass~get-base ((a mass+monatom))
  (let ((func (mass~get-function a))
	(args (mass~get-args a)))
    (cond ((eql func mass*expt)
	   (mass~create (first args)))
	  ((eql func mass*power)
	   (first args))
	  (t
	   (mass~create a)))))

(defmethod mass~sinkable ((a mass+monom) (b cons))
  (or (mass~eql a (first b))
      (mass~sinkable a (cdr b))))

(defmethod mass~sinkable ((a mass+term) (b cons))
  (or (mass~eql a (first b))
      (mass~sinkable a (cdr b))))

(defmethod mass~sinkable (a (b null))
  nil)



;; Building an MASS term from POST syntax:

(defmethod mass~read-args ((args cons) n &optional (pos (pos~empty)))
  (cons (mass~read (car args) (pos~add-end n pos))
	(mass~read-args (cdr args) (1+ n) pos)))

(defmethod mass~read-args ((args null) n &optional (pos (pos~empty)))
  nil)
 
(defmethod mass~read ((term term+appl) &optional (pos (pos~empty)))
  (let ((plus   (env~lookup-object mass*plus     ca*global-environment))
        (times  (env~lookup-object mass*mult     ca*global-environment))
        (minus  (env~lookup-object mass*sub      ca*global-environment))
        (div    (env~lookup-object mass*div      ca*global-environment))
        (sqrt   (env~lookup-object mass*sqrt     ca*global-environment))
        (power  (env~lookup-object mass*power    ca*global-environment))
        (mod    (env~lookup-object mass*mod      ca*global-environment))
	(stimes (env~lookup-object mass*stimes   ca*global-environment))
	(pplus  (env~lookup-object mass*pplus    ca*global-environment))
	(pderiv (env~lookup-object mass*pderiv   ca*global-environment))
	(num    (env~lookup-object mass*num      ca*global-environment))
	(equal  (env~lookup-object mass*equality ca*global-environment))
	(func   (data~appl-function term))
	(args   (mass~read-args (data~appl-arguments term) 1 pos)))
	   (cond ((eql func plus)
		  (mass~plus  (first args) (second args) pos))
		 ((eql func times)
		  (mass~times (first args) (second args) pos))
		 ((eql func minus)
		  (mass~minus (first args) (second args) pos))
		 ((eql func div)
		  (mass~div   (first args) (second args) pos))
		 ((eql func sqrt)
		  (mass~sqrt  (first args) pos))
		 ((eql func power)
		  (mass~power (first args) (second args) pos))
		 ((eql func mod)
		  (mass~mod   (first args) (second args) pos))
		 ((eql func stimes)
		  (mass~stimes (first args) (second args)))
		 ((eql func pplus)
		  (mass~pplus  (first args) (second args)))
		 ((eql func pderiv)
		  (let ((f (first args)))
		    (mass~pderiv f (first (mass~get-lambda-list f)) (mass~read (second args)))))
		 ((eql func equal)
		  (mass~equal (first args) (second args) pos))
		 (t
		  (mass~create (make-instance 'mass+monatom
					     :args (cons func args)))))))

(defmethod mass~read ((term term+number) &optional (pos (pos~empty)))
  (mass~create (keim~name term)))

(defmethod mass~read ((term term+constant) &optional (pos (pos~empty)))
  (mass~create term))

(defmethod mass~read ((term term+variable) &optional (pos (pos~empty)))
  (mass~create term))


;; Predicates on MASS terms:

(defmethod mass~zero ((a mass+term)) (null (mass~get-term a)))

(defmethod mass~zero ((a mass+termatom)) (= 0 (mass~get-coeff a)))



(defmethod mass~one ((a mass+monatom))
  (cond ((eql (mass~get-function a) mass*expt)
	 (mass~zero (second (mass~get-args a))))
	((eql (mass~get-function a) mass*power)
	 (mass~zero (second (mass~get-args a))))
	(t
	 nil)))

(defmethod mass~one ((a mass+monom)) (null (mass~get-monom a)))

(defmethod mass~one ((a mass+termatom))
  (and (= 1 (mass~get-coeff a))
       (mass~one (mass~get-monom a))))

(defmethod mass~one ((a mass+term))
  (cond ((null (mass~get-term a))
	 nil)
	(t
	 (and t (mass~one (mass~first-termatom a))
	      (mass~zero (mass~term-rest a))))))
  


(defmethod mass~eql ((a mass+term) (b mass+term))
  (cond ((mass~zero a)
         (mass~zero b))
	(t
	 (let ((RIC (mass~remove-if-contained (mass~first-termatom a) b)))
           (and (first RIC) (mass~eql (mass~term-rest a) (second RIC)))))))
			      
(defmethod mass~eql ((a mass+termatom) (b mass+termatom))
  (and (= (mass~get-coeff a) (mass~get-coeff b))
       (mass~eql (mass~get-monom a) (mass~get-monom b))))

(defmethod mass~eql ((a mass+monom) (b mass+monom))
  (cond ((mass~one a)
	 (mass~one b))
	(t
         (let ((RIC (mass~remove-if-contained (mass~first-monatom a) b)))
           (and (first RIC) (mass~eql (mass~monom-rest a) (second RIC)))))))

(defmethod mass~eql ((a mass+monatom) (b mass+monatom))
  (let ((funca (mass~get-function a))
	(argsa (mass~get-args a))
	(funcb (mass~get-function b))
	(argsb (mass~get-args b)))
    (cond ((and (eql funca mass*expt) (eql funcb mass*expt))
	   (and (eql (first argsa) (first argsb))
		(mass~eql (second argsa) (second argsb))))
	  ((and (eql funca mass*unknown) (eql funcb mass*unknown))
	   (and (eql (first argsa) (first argsb))
		(mass~eql (cdr argsa) (cdr argsb))))
	  ((eql funca funcb)
	   (mass~eql argsa argsb))
          (t
	   nil))))

(defmethod mass~eql ((a cons) (b cons))
  (and (mass~eql (car a) (car b))
       (mass~eql (cdr a) (cdr b))))

(defmethod mass~eql ((a null) (b null))
  t)

(defmethod mass~eql ((a null) (b cons))
  nil)

(defmethod mass~eql ((a cons) (b null))
  nil)



(defmethod mass~number-p ((a mass+term))
  (or (null (mass~get-term a))
      (and (= (list-length (mass~get-term a)) 1)
	   (mass~number-p (mass~first-termatom a)))))

(defmethod mass~number-p ((a mass+termatom))
  (mass~one (mass~get-monom a)))

(defmethod mass~number-value ((a mass+term))
  (cond ((mass~zero a)
	 0)
	(t
	 (mass~number-value (mass~first-termatom a)))))

(defmethod mass~number-value ((a mass+termatom))
  (mass~get-coeff a))

(defmethod mass~number-value ((a number))
  a)


	 
;; Functions on MASS terms:

(defmethod mass~inverse ((a mass+term) &optional (pos (pos~empty)))
  (cond ((mass~zero a)
	 a)
	((mass~atom a)
	 (mass~create (mass~inverse (mass~first-termatom a))))
	(t
	 (mass~append (mass~inverse (mass~first-termatom a))
		     (mass~inverse (mass~term-rest a))))))

(defmethod mass~inverse ((a mass+termatom) &optional (pos (pos~empty)))
  (make-instance 'mass+termatom
		 :coeff (- (mass~get-coeff a))
		 :monom (mass~get-monom a)))



(defmethod mass~plus ((a mass+term) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~zero a)
	 (ca~output-method '((:forward . 0+e) (:backward . 0+i-b)) (list pos))
	 b)
	((mass~zero b)
	 (ca~output-method '((:forward . +0e) (:backward . +0i-b)) (list pos))
	 a)
	((mass~atom a)
	 (mass~plus (mass~first-termatom a) b pos))
	((mass~atom b)
	 (ca~output-method '((:forward . c-plus)
			     (:backward . c-plus-b)) (list pos))
	 (mass~plus (mass~first-termatom b) a pos))
	((mass~sinkable (mass~get-monom (mass~first-termatom a))
		       (mass~monom-list b))
	 (let ((b-new (mass~pop (mass~get-monom (mass~first-termatom a)) b (pos~add-end 2 pos))))
	   (ca~output-method '((:forward . cross-swap-plus)
		  	       (:backward . cross-swap-plus-b)) (list pos))
	   (let ((sum (mass~plus (mass~first-termatom a)
				(mass~first-termatom b-new)
				(pos~add-end 1 pos))))
	     (cond ((mass~zero sum)
		    (ca~output-method '((forward . 0+e) (:backward . 0+i-b)) (list pos))
		    (mass~plus (mass~term-rest a)
			      (mass~term-rest b-new)
			      pos))
		   (t
	            (mass~append sum
				(mass~plus (mass~term-rest a)
				          (mass~term-rest b-new)
				          (pos~add-end 2 pos))))))))
	(t
	 (ca~output-method '((:forward . a-plus-right)
			     (:backward . a-plus-right-b)) (list pos))
	 (let ((sum (mass~plus (mass~term-rest a) b (pos~add-end 2 pos))))
	   (when (mass~zero sum) (ca~output-method '((:forward . +0e)
						    (:backward . +0i-b))
						  (list pos)))
	   (mass~append (mass~first-termatom a) sum)))))

(defmethod mass~plus ((a mass+termatom) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~zero b)
	 (ca~output-method '((:forward . +0e) (:backward . +0i-b)) (list pos))
	 (mass~create a))
	((mass~atom b)
	 (mass~create (mass~plus a (mass~first-termatom b) pos)))
	((mass~sinkable (mass~get-monom a)
		       (mass~monom-list b))
	 (let ((b-new (mass~pop (mass~get-monom a) b (pos~add-end 2 pos))))
	   (ca~output-method '((:forward . a-plus-left)
			       (:backward . a-plus-left-b)) (list pos))
	   (let ((sum (mass~plus a
				(mass~first-termatom b-new)
				(pos~add-end 1 pos))))
	     (cond ((mass~zero sum)
		    (ca~output-method '((forward . 0+e) (:backward . 0+i-b)) (list pos))
		    (mass~term-rest b-new))
		   (t
	            (mass~append sum
				(mass~term-rest b-new)))))))
	(t
	 (mass~append a b))))

(defmethod mass~plus ((a mass+termatom) (b mass+termatom) &optional (pos (pos~empty)))
  (let* ((new-coeff (+ (mass~get-coeff a) (mass~get-coeff b)))
	 (pos1  (if (= 1 (mass~get-coeff a))
		    (pos~add-end 1 pos)
		  (pos~add-end 2 (pos~add-end 1 pos))))
	 (pos2  (if (= 1 (mass~get-coeff b))
		    (pos~add-end 2 pos)
		  (pos~add-end 2 (pos~add-end 2 pos)))))
  (cond ((mass~eql (mass~get-monom a) (mass~get-monom b))	 
	 (mass~prove-eql (mass~get-monom a)
			(mass~get-monom b)
			pos2)
	 (ca~output-method '((:forward . add-monomials)
			     (:backward . split-monomials-plus-b))
			   (list pos
			         (mass~rebuild (mass~get-coeff a))
			         (mass~rebuild (mass~get-coeff b))))
	 (when (and (= 0 new-coeff)
		    (not (mass~one (mass~get-monom a))))
	       (ca~output-method '((:forward . 0*e)
			  	   (:backward . 0*i-b))
				 (list pos
				       (mass~rebuild (mass~get-monom a)))))
	 (make-instance 'mass+termatom
			:coeff new-coeff
			:monom (mass~get-monom a)))
	(t
	 (make-instance 'mass+term
			:term (list a b))))))



(defmethod mass~minus ((a mass+term) (b mass+term) &optional (pos (pos~empty)))
		      (ca~output-method '((:forward . minus2plus)
					  (:backward . plus2minus))
				        (list pos))
  (mass~plus a (mass~times (mass~create -1) b (pos~add-end 2 pos)) pos))



(defmethod mass~sqrt ((a mass+term) &optional (pos (pos~empty)))
		      (ca~output-method '((:forward . sqrt2power)
					  (:backward . power2sqrt))
				        (list pos))
  (mass~power a (mass~create 1/2) pos))



(defmethod mass~times ((a mass+term) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~zero a)
	 (ca~output-method '((:forward . 0*e)
			     (:backward . 0*i-b))
			   (list pos
				 (mass~rebuild b)))
	 a)
	((mass~one a)
	 (ca~output-method '((:forward . 1*e)
			     (:backward . 1*i-b))
			   (list pos))
	 b)
        ((mass~zero b)
	 (ca~output-method '((:forward . *0e)
			     (:backward . *0i-b))
			   (list pos
				 (mass~rebuild a)))
	 b)
	((mass~one b)
	 (ca~output-method '((:forward . *1e)
			     (:backward . *1i-b))
			   (list pos))
	 a)
	((mass~atom a)
	 (mass~times (mass~first-termatom a) b pos))
	((mass~atom b)
	 (ca~output-method '((:forward . c-times) (:backward . c-times-b)) (list pos))
	 (mass~times (mass~first-termatom b) a pos))
	(t
	 (ca~output-method '((:forward . distribute-r)
			     (:backward . cummulate-r-b))
			   (list pos))
	 (mass~plus (mass~times (mass~first-termatom a) b (pos~add-end 1 pos))
		   (mass~times (mass~term-rest a) b (pos~add-end 2 pos))
		   pos))))

(defmethod mass~times ((a mass+termatom) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~zero b)
	 (ca~output-method '((:forward . *0e)
			     (:backward . 0*i-b))
			   (list pos
				 (mass~rebuild a)))
	 b)
	((mass~atom b)
	 (mass~create (mass~times a (mass~first-termatom b) pos)))
	(t
	 (ca~output-method '((:forward . distribute-l)
			     (:backward . cummulate-l-b))
			   (list pos))
	 (mass~plus (mass~times a (mass~first-termatom b) (pos~add-end 1 pos))
		   (mass~times a (mass~term-rest b) (pos~add-end 2 pos))
		   pos))))

(defmethod mass~times ((a mass+termatom) (b mass+termatom) &optional (pos (pos~empty)))
  (let* ((coeff-a (mass~get-coeff a))
	 (coeff-b (mass~get-coeff b))
	 (monom-a (mass~get-monom a))
	 (monom-b (mass~get-monom b))
	 (new-coeff (* coeff-a coeff-b)))
  (cond ((mass~one a)
	 (ca~output-method '((:forward . 1*e)
			     (:backward . 1*i-b))
			   (list pos))
	 b)
	((mass~one b)
	 (ca~output-method '((:forward . *1e)
			     (:backward . *1i-b))
			   (list pos))
	 a)
	((and (mass~one monom-a)
	      (= 1 coeff-b))
	 (make-instance 'mass+termatom
			:coeff coeff-a
			:monom monom-b))
	((and (= 1 coeff-a)
	      (= 1 coeff-b))
	 (make-instance 'mass+termatom
			:monom (mass~times monom-a
					  monom-b
					  pos)))
	(t
         (ca~output-method '((:forward . mult-monomials)
			     (:backward . split-monomials-times-b))
			   (list pos
                                 (mass~rebuild (mass~get-coeff a))
				 (mass~rebuild (mass~get-coeff b))))
	 (let* ((pos2 (if (= 1 new-coeff)
			  pos
		        (pos~add-end 2 pos)))
		(new-monom (mass~times monom-a monom-b pos2)))
	   (when (and (mass~one new-monom)
                      (not (mass~one monom-a)))
	     (ca~output-method '((:forward . *1e)
			         (:backward . *1i-b))
			       (list pos)))
           (make-instance 'mass+termatom
		          :coeff new-coeff
		          :monom new-monom))))))
	 

(defmethod mass~times ((a mass+monom) (b mass+monom) &optional (pos (pos~empty)))
  (cond ((mass~one a)
	 b)
	((mass~one b)
	 a)
	((mass~atom a)
	 (mass~times (mass~first-monatom a) b pos))
	((mass~atom b)
	 (ca~output-method '((:forward . c-times)
			     (:backward . c-times-b)) (list pos))
	 (mass~times (mass~first-monatom b) a pos))
	((mass~sinkable (mass~get-base (mass~first-monatom a))
		       (mass~exp-base-list b))
	 (let ((b-new (mass~pop (mass~get-base (mass~first-monatom a)) b (pos~add-end 2 pos))))
	   (ca~output-method '((:forward . cross-swap-times)
		  	       (:backward . cross-swap-times-b)) (list pos))
	   (let ((prod (mass~times (mass~first-monatom a)
				  (mass~first-monatom b-new)
				  (pos~add-end 1 pos))))
	     (cond ((mass~one prod)
		    (ca~output-method '((forward . 1*e) (:backward . 1*i-b)) (list pos))
		    (mass~times (mass~monom-rest a)
			       (mass~monom-rest b-new)
			       pos))
		   (t
	            (mass~append prod
				(mass~times (mass~monom-rest a)
				           (mass~monom-rest b-new)
				           (pos~add-end 2 pos))))))))
	(t
	 (ca~output-method '((:forward . a-times-right)
			     (:backward . a-times-right-b)) (list pos))
	 (let ((prod (mass~times (mass~monom-rest a) b (pos~add-end 2 pos))))
	   (when (mass~one prod) (ca~output-method '((:forward . *1e)
						    (:backward . *1i-b))
						  (list pos)))
	   (mass~append (mass~first-monatom a) prod)))))

(defmethod mass~times ((a mass+monatom) (b mass+monom) &optional (pos (pos~empty)))
  (cond ((mass~one b)
	 (make-instance 'mass+monom
			:monom (list a)))
	((mass~atom b)
	 (mass~times a (mass~first-monatom b) pos))
	((mass~sinkable (mass~get-base a)
		       (mass~exp-base-list b))
	 (let ((b-new (mass~pop (mass~get-base a) b (pos~add-end 2 pos))))
	   (ca~output-method '((:forward . a-times-left)
		  	       (:backward . a-times-left-b)) (list pos))
	   (let ((prod (mass~times a
				  (mass~first-monatom b-new)
				  (pos~add-end 1 pos))))
	     (cond ((mass~one prod)
		    (ca~output-method '((forward . 1*e)
					(:backward . 1*i-b))
				      (list pos))
		    (mass~monom-rest b-new))
		   (t
	            (mass~append (mass~first-monatom prod)
				(mass~monom-rest b-new)))))))
	(t
	 (mass~append a b))))

(defmethod mass~times ((a mass+monatom) (b mass+monatom) &optional (pos (pos~empty)))
  (cond ((not (mass~eql (mass~get-base a) (mass~get-base b)))
	 (make-instance 'mass+monom
			:monom (list a b)))
	((eql (mass~get-function a) mass*expt)
	 (when (mass~one (second (mass~get-args a)))
	   (ca~output-method '((:forward . ^1i)
			       (:backward . ^1e-b))
			     (list (pos~add-end 1 pos))))
	 (when (mass~one (second (mass~get-args b)))
	   (ca~output-method '((:forward . ^1i)
			       (:backward . ^1e-b))
			     (list (pos~add-end 2 pos))))
	 (ca~output-method '((:forward . mult-power)
			     (:backward . split-power-b))
			   (list pos))
	 (let ((sum (mass~plus (second (mass~get-args a))
			      (second (mass~get-args b))
			      (pos~add-end 2 pos))))
	   (cond ((mass~one sum)
	          (ca~output-method '((:forward . ^1e)
			              (:backward . ^1i-b))
			            (list pos))
		  (let ((c (make-instance 'mass+monatom
				          :func mass*expt
				          :args (list (first (mass~get-args a))
					              sum))))
		    (make-instance 'mass+monom
				   :monom (list c))))			   
		 ((mass~zero sum)
	          (ca~output-method '((:forward . ^0e)
			              (:backward . ^0i-b))
			            (list pos (first (mass~get-args a))))
		  (make-instance 'mass+monom))
		 (t
		  (let ((c (make-instance 'mass+monatom
				          :func mass*expt
				          :args (list (first (mass~get-args a))
					              sum))))
		    (make-instance 'mass+monom
				   :monom (list c)))))))
	(t
	 (let ((base1 (if (eql (mass~get-function a) mass*power)
			 (first (mass~get-args a))
		       (mass~create a)))
	       (base2 (if (eql (mass~get-function b) mass*power)
			 (first (mass~get-args b))
		       (mass~create b)))
	       (exp1 (if (eql (mass~get-function a) mass*power)
			 (second (mass~get-args a))
		       (mass~create 1)))
	       (exp2 (if (eql (mass~get-function b) mass*power)
			 (second (mass~get-args b))
		       (mass~create 1))))
	   (when (mass~one exp1)
	     (ca~output-method '((:forward . ^1i)
			         (:backward . ^1e-b))
			       (list (pos~add-end 1 pos))))
	   (when (mass~one exp2)
	     (ca~output-method '((:forward . ^1i)
			         (:backward . ^1e-b))
			       (list (pos~add-end 2 pos))))
	   (mass~prove-eql base1 base2 (pos~add-end 2 (pos~add-end 2 pos))) 
	   (ca~output-method '((:forward . mult-power)
			       (:backward . split-power-b))
			     (list pos))
	   (let* ((exp-new (mass~plus exp1 exp2 (pos~add-end 2 pos)))
		  (c (make-instance 'mass+monatom
			 	    :func mass*power
				    :args (list base1 exp-new))))
	     (make-instance 'mass+monom
			    :monom (list c)))))))
	   
	   
	       
		  
	  
				    
(defmethod mass~div ((a mass+term) (b mass+term) &optional (pos (pos~empty)))
  (ca~output-method '((:forward . div2times)
                      (:backward . times2div-b))
                    (list pos))
  (cond ((mass~number-p b)
	 (mass~times a (mass~create (/ 1 (mass~number-value b))) pos))
        (t
	 (mass~create (make-instance 'mass+monatom
				    :func mass*div
				    :args (list a b))))))
         
	


(defun posint (a)
  (and (= (floor a) a) (> a 0)))

(defmethod mass~power ((a mass+term) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~zero b)
	 (ca~output-method '((:forward . ^0e)
			     (:backward . ^0i-b))
			   (list pos (mass~rebuild a)))
	 (mass~create 1))
	((mass~one b)
	 (ca~output-method '((:forward . ^1e)
			     (:backward . ^1i-b))
			   (list pos))
	 a)
	((mass~one a)
	 (ca~output-method '((:forward . 1^e)
			     (:backward . 1^i-b))
			   (list pos))	 
	 (mass~create 1))
	((mass~atom a)
	 (mass~create (mass~power (mass~first-termatom a) b pos)))
	((and (mass~number-p b)
	      (posint (mass~number-value b)))
	 (ca~output-method '((:forward . inflate-power)
			     (:backward . deflate-power-b))
			   (list pos))
	 (let ((b-new (mass~number-value b)))
	   (if (> b-new 2)
	       (mass~times a (mass~power a (mass~create (- b-new 1)) (pos~add-end 2 pos)) pos)
	     (mass~times a a pos))))
	(t
	 (mass~create (make-instance 'mass+monatom
				    :func mass*power
				    :args (list a b))))))

(defmethod mass~power ((a mass+termatom) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~one a)
	 (ca~output-method '((:forward . 1^e)
			     (:backward . 1^i-b))
			   (list pos))	 
	 (mass~create 1))
	((= 1 (mass~get-coeff a))
	 (mass~power (mass~get-monom a) b pos))
        ((and (mass~number-p a)
              (mass~number-p b)
              (posint (mass~number-value b)))
         (ca~output-method '((:forward . simplify-num)
                             (:backward . expand-num-b))
                           (list pos
                                 (mass~rebuild
                                  (make-instance 'mass+monatom
                                                 :func mass*power
                                                 :args (list a b)))))
         (mass~create (expt (mass~number-value a) (mass~number-value b))))
	((mass~one (mass~get-monom a))
;	 (mass~power (mass~create (mass~get-coeff a)) b pos))
	 (mass~create (make-instance 'mass+monatom
				    :func mass*power
				    :args (list (mass~create a) b))))
	(t
	 (ca~output-method '((:forward . split-base)
			     (:backward . mult-base-b))
			   (list pos))	 
	 (mass~times (mass~power (mass~create (mass~get-coeff a)) b (pos~add-end 1 pos))
;		     (make-instance 'mass+monatom
;				    :func mass*power
;				    :args (list (mass~create (mass~get-coeff a))
;						b)))
		    (mass~create
		     (mass~power (mass~get-monom a) b (pos~add-end 2 pos))) pos))))

(defmethod mass~power ((a mass+monom) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~one a)
	 (ca~output-method '((:forward . 1^e)
			     (:backward . 1^i-b))
			   (list pos))	 
	 (mass~create a))
	((mass~atom a)
	 (mass~power (mass~first-monatom a) b pos))
	(t
	 (ca~output-method '((:forward . split-base)
			     (:backward . mult-base-b))
			   (list pos))	 
	 (mass~times (mass~power (mass~first-monatom a) b (pos~add-end 1 pos))
	            (mass~power (mass~monom-rest a) b (pos~add-end 2 pos))
		    pos))))

(defmethod mass~power ((a mass+monatom) (b mass+term) &optional (pos (pos~empty)))
  (let ((func (mass~get-function a))
	(args (mass~get-args a)))
    (cond ((eql func mass*expt)
	   (cond ((mass~one (second args))
		  (mass~create
	           (make-instance 'mass+monatom
			          :func mass*expt
			          :args (list (first args)
				              b))))
		 (t
	          (ca~output-method '((:forward . mult-exp)
			              (:backward . split-exp-b))
			            (list pos))	 
		  (mass~create
	           (make-instance 'mass+monatom
			          :func mass*expt
			          :args (list (first args)
				              (mass~times
					       (second args)
					       b
					       (pos~add-end 2 pos))))))))
	  ((eql func mass*power)
	   (ca~output-method '((:forward . mult-exp)
			       (:backward . split-exp-b))
			     (list pos))
	   (let ((ex (mass~times (second args) b (pos~add-end 2 pos))))
	     (mass~power (first args) ex pos)))
	  (t
	   (mass~create
	    (make-instance 'mass+monatom
			   :func mass*power
			   :args (list (mass~create a)
				       b)))))))



(defmethod mass~is-mod-term ((a mass+term) n)
  (cond ((mass~zero a) nil)
        ((mass~one a) nil)
        ((mass~atom a)
         (mass~is-mod-term (mass~first-termatom a) n))
        (t nil)))

(defmethod mass~is-mod-term ((a mass+termatom) n)
  (and (= 1 (mass~get-coeff a))
       (mass~is-mod-term (mass~get-monom a) n)))

(defmethod mass~is-mod-term ((a mass+monom) n)
  (and (not (mass~one a))
       (mass~atom a)
       (mass~is-mod-term (mass~first-monatom a) n)))

(defmethod mass~is-mod-term ((a mass+monatom) n)
  (and (eql (mass~get-function a) mass*mod)
       (let ((arg (cadr (mass~get-args a))))
          (and (mass~number-p arg)
               (let ((m (mass~number-value arg)))
                 (= m n))))))



(defmethod mass~is-mod-term* ((a mass+term) n)
  (cond ((mass~zero a) nil)
        ((mass~one a) nil)
        ((mass~atom a)
         (mass~is-mod-term* (mass~first-termatom a) n))
        (t nil)))

(defmethod mass~is-mod-term* ((a mass+termatom) n)
  (and (= 1 (mass~get-coeff a))
       (mass~is-mod-term* (mass~get-monom a) n)))

(defmethod mass~is-mod-term* ((a mass+monom) n)
  (and (not (mass~one a))
       (mass~atom a)
       (mass~is-mod-term* (mass~first-monatom a) n)))

(defmethod mass~is-mod-term* ((a mass+monatom) n)
  (and (eql (mass~get-function a) mass*mod)
       (let ((arg (cadr (mass~get-args a))))
          (and (mass~number-p arg)
               (let ((m (mass~number-value arg)))
                 (= (/ m n) (floor (/ m n))))))))



(defmethod mass~mod-reducible ((a mass+term) n)
  (cond ((mass~zero a) nil)
        ((mass~one a) nil)
        ((mass~is-mod-term* a n) t)
        (t
         (or (mass~mod-reducible (mass~first-termatom a) n)
             (mass~mod-reducible (mass~term-rest a) n)))))

(defmethod mass~mod-reducible ((a mass+termatom) n)
  (cond ((mass~one a) nil)
        ((mass~mod-reducible (mass~get-coeff a) n)
         t)
        (t (mass~mod-reducible (mass~get-monom a) n))))

(defmethod mass~mod-reducible ((a mass+monom) n)
  (cond ((mass~one a) nil)
        (t
         (or (mass~mod-reducible (mass~first-monatom a) n)
             (mass~mod-reducible (mass~monom-rest a) n)))))

(defmethod mass~mod-reducible ((a mass+monatom) n)
  (or (mass~is-mod-term* a n)
      (and (eql (mass~get-function a) mass*power)
           (mass~mod-reducible (car (mass~get-args a)) n))))

(defmethod mass~mod-reducible ((a number) n)
  (>= a n))



(defmethod mass~mod-argument ((a mass+term))
  (mass~mod-argument (mass~first-termatom a)))

(defmethod mass~mod-argument ((a mass+termatom))
  (mass~mod-argument (mass~get-monom a)))

(defmethod mass~mod-argument ((a mass+monom))
  (mass~mod-argument (mass~first-monatom a)))

(defmethod mass~mod-argument ((a mass+monatom))
  (car (mass~get-args a)))
 



(defmethod mass~mod ((a mass+term) (b mass+term) &optional (pos (pos~empty)))
  (if (and (mass~number-p b)
           (or (mass~mod-reducible a (mass~number-value b))
               (mass~number-p a)))
      (let ((n (mass~number-value b)))
        (cond ((mass~number-p a)
               (mass~create (mod (mass~number-value a) n)))
              ((mass~is-mod-term a n)
               (print "tactic: simplify-mod-mod")
               (mass~mod-argument a))
              ((mass~is-mod-term* a n)
               (print "tactic: simplify-mod-mod")
               (mass~mod (mass~mod-argument a) b pos))
              ((mass~atom a)
               (mass~mod (mass~first-termatom a) b pos))
              (t
               (let ((a-left (mass~first-termatom a))
                     (a-right (mass~term-rest a)))
                 (when (and (not (mass~is-mod-term a-left n))
                            (mass~mod-reducible a-left n))
                   (print "tactic: push-mod-left")
                   (when (mass~is-mod-term* a-left n)
                     (print "tactic: simplify-mod-mod")
                     t))
                 (when (and (not (mass~is-mod-term a-right n))
                            (mass~mod-reducible a-right n))
                   (print "tactic: push-mod-right")
                   (when (mass~is-mod-term* a-right n)
                     (print "tactic: simplify-mod-mod")
                     t))
                 (let ((a2-left
                        (cond ((mass~is-mod-term a-left n)
                               (mass~mod-argument a-left))
                              ((mass~is-mod-term* a-left n)
                               (mass~mod
                                (mass~mod-argument a-left) b (pos~add-end 1 pos)))
                              ((mass~mod-reducible a-left n)
                               (mass~mod a-left b (pos~add-end 1 pos)))
                              (t
                               a-left)))
                       (a2-right
                        (cond ((mass~is-mod-term a-right n)
                               (mass~mod-argument a-right))
                              ((mass~is-mod-term* a-right n)
                               (mass~mod
                                (mass~mod-argument a-right) b (pos~add-end 1 pos)))
                              ((mass~mod-reducible a-right n)
                               (mass~mod a-right b (pos~add-end 1 pos)))
                              (t
                               a-right))))
                   (when (or (mass~is-mod-term a2-left n)
                             (mass~is-mod-term a-left n))
                     (print "tactic pull-mod-left")
                     t)
                   (when (or (mass~is-mod-term a2-right n)
                             (mass~is-mod-term a-right n))
                     (print "tactic pull-mod-right")
                     t)
                   (let* ((a3-left (mass~create
                                    (if (mass~is-mod-term a2-left n)
                                        (mass~mod-argument a2-left)
                                      a2-left)))
                          (a3-right (mass~create
                                     (if (mass~is-mod-term a2-right n)
                                         (mass~mod-argument a2-right)
                                       a2-right)))
                          (result (mass~plus a3-left a3-right (pos~add-end 1 pos))))
                     (mass~mod result b pos)))))))
    (mass~create
     (make-instance 'mass+monatom
		    :func mass*mod
		    :args (list a b)))))


(defmethod mass~mod ((a mass+termatom) (b mass+term) &optional (pos (pos~empty)))
  (if (and (mass~number-p b) (mass~mod-reducible a (mass~number-value b)))
      (let ((n (mass~number-value b)))
        (cond ((mass~is-mod-term a n)
               (print "tactic: simplify-mod-mod")
               (mass~mod-argument a))
              ((mass~is-mod-term* a n)
               (print "tactic: simplify-mod-mod")
               (mass~mod (mass~mod-argument a) b pos))
              ((= 1 (mass~get-coeff a))
               (mass~mod (mass~get-monom a) b pos))
              ((mass~one (mass~get-monom a))
               (print "tactic: simplify-num")
               (mass~create (mod (mass~get-coeff a) n)))
              (t
               (let ((coeff (mass~get-coeff a))
                     (monom (mass~get-monom a)))
                 (when (mass~mod-reducible coeff n)
                   (print "tactic: push-mod-left")
                   (print "tactic: simplify-num")
                   t)
                 (when (and (not (mass~is-mod-term monom n))
                            (mass~mod-reducible monom n))
                   (print "tactic: push-mod-right")
                   (when (mass~is-mod-term* monom n)
                     (print "tactic: simplify-mod-mod")
                     t))
                 (let ((new-coeff
                        (if (mass~mod-reducible coeff n)
                            (mod coeff n)
                           coeff))
                       (monom2
                        (cond ((mass~is-mod-term monom n)
                               (mass~mod-argument monom))
                              ((mass~is-mod-term* monom n)
                               (mass~mod
                                (mass~mod-argument monom) b (pos~add-end 1 pos)))
                              ((mass~mod-reducible monom n)
                               (mass~mod monom b (pos~add-end 1 pos)))
                              (t
                               monom))))
                   (when (mass~is-mod-term monom2 n)
                     (print "tactic pull-mod-right")
                     t)
                   (let* ((new-monom (mass~create
                                      (if (mass~is-mod-term monom2 n)
                                          (mass~mod-argument monom2)
                                        monom2)))
                          (result (mass~times
                                   (mass~create new-coeff)
                                   new-monom
                                   (pos~add-end 1 pos))))
                     (mass~mod result b pos)))))))
    (mass~create
     (make-instance 'mass+monatom
		    :func mass*mod
		    :args (list (mass~create a) b)))))



(defmethod mass~mod ((a mass+monom) (b mass+term) &optional (pos (pos~empty)))
  (if (and (mass~number-p b) (mass~mod-reducible a (mass~number-value b)))
      (let ((n (mass~number-value b)))
        (cond ((mass~is-mod-term a n)
               (print "tactic: simplify-mod-mod")
               (mass~mod-argument a))
              ((mass~is-mod-term* a n)
               (print "tactic: simplify-mod-mod")
               (mass~mod (mass~mod-argument a) b pos))
              ((mass~atom a)
               (mass~mod (mass~first-monatom a) b pos))
              (t
               (let ((a-left (mass~first-monatom a))
                     (a-right (mass~monom-rest a)))
                 (when (and (not (mass~is-mod-term a-left n))
                            (mass~mod-reducible a-left n))
                   (print "tactic: push-mod-left")
                   (when (mass~is-mod-term* a-left n)
                     (print "tactic: simplify-mod-mod")
                     t))
                 (when (and (not (mass~is-mod-term a-right n))
                            (mass~mod-reducible a-right n))
                   (print "tactic: push-mod-right")
                   (when (mass~is-mod-term* a-right n)
                     (print "tactic: simplify-mod-mod")
                     t))
                 (let ((a2-left
                        (cond ((mass~is-mod-term a-left n)
                               (mass~mod-argument a-left))
                              ((mass~is-mod-term* a-left n)
                               (mass~mod
                                (mass~mod-argument a-left) b (pos~add-end 1 pos)))
                              ((mass~mod-reducible a-left n)
                               (mass~mod a-left b (pos~add-end 1 pos)))
                              (t
                               a-left)))
                       (a2-right
                        (cond ((mass~is-mod-term a-right n)
                               (mass~mod-argument a-right))
                              ((mass~is-mod-term* a-right n)
                               (mass~mod
                                (mass~mod-argument a-right) b (pos~add-end 1 pos)))
                              ((mass~mod-reducible a-right n)
                               (mass~mod a-right b (pos~add-end 1 pos)))
                              (t
                               a-right))))
                   (when (mass~is-mod-term a2-left n)
                     (print "tactic pull-mod-left")
                     t)
                   (when (mass~is-mod-term a2-right n)
                     (print "tactic pull-mod-right")
                     t)
                   (let* ((a3-left (mass~create
                                    (if (mass~is-mod-term a2-left n)
                                        (mass~mod-argument a2-left)
                                      a2-left)))
                          (a3-right (mass~create
                                     (if (mass~is-mod-term a2-right n)
                                         (mass~mod-argument a2-right)
                                       a2-right)))
                          (result (mass~times a3-left a3-right (pos~add-end 1 pos))))
                     (mass~mod result b pos)))))))
    (mass~create
     (make-instance 'mass+monatom
		    :func mass*mod
		    :args (list a b)))))

(defmethod mass~mod ((a mass+monatom) (b mass+term) &optional (pos (pos~empty)))
  (if (and (mass~number-p b) (mass~mod-reducible a (mass~number-value b)))
      (let ((func (mass~get-function a))
	    (args (mass~get-args a))
            (n    (mass~number-value b)))
        (cond ((eql func mass*mod)
               (print "tactic: simplify-mod-mod")
               (if (mass~is-mod-term a n)
                   a
                 (mass~mod (mass~mod-argument a) b pos)))
	      ((eql func mass*power)
               (print "tactic: push-mod")
	       (let ((new-base (mass~mod (first args) b (pos~add-end 1 pos))))
	         (mass~power new-base (second-args) pos)))
	      (t
	       (mass~create
	        (make-instance 'mass+monatom
			       :func mass*mod
			       :args (list (mass~create a)
				           b))))))
    (mass~create
     (make-instance 'mass+monatom
		    :func mass*mod
		    :args (list a b)))))



;; Proof construction:

(defmethod mass~prove-eql ((a mass+term) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~zero a)
	 (mass~zero b))
	((mass~atom a)
         (mass~prove-eql (mass~first-termatom a)
		        (mass~first-termatom b)
		        pos))
	(t
	 (let ((b-new (mass~pop (mass~first-termatom a) b pos)))	   
           (mass~prove-eql (mass~first-termatom a)
		          (mass~first-termatom b-new)
		          (pos~add-end 1 pos))
           (mass~prove-eql (mass~term-rest a)
			  (mass~term-rest b-new)
			  (pos~add-end 2 pos))))))

(defmethod mass~prove-eql ((a mass+termatom)
			  (b mass+termatom) 
                          &optional (pos (pos~empty)))
  (cond ((mass~one (mass~get-monom a))
	 t)
	((= 1 (mass~get-coeff a))
	 (mass~prove-eql (mass~get-monom a)
			(mass~get-monom b)
			pos))
	(t
	 (mass~prove-eql (mass~get-monom a)
			(mass~get-monom b)
			(pos~add-end 2 pos)))))

(defmethod mass~prove-eql ((a mass+monom) (b mass+monom) &optional (pos (pos~empty)))
  (cond ((mass~one a)
	 (mass~one b))
	((mass~atom a)
         (mass~prove-eql (mass~first-monatom a)
		        (mass~first-monatom b)
		        pos))
	(t 
         (let ((b-new (mass~pop (mass~first-monatom a) b pos)))
           (mass~prove-eql (mass~first-monatom a)
		          (mass~first-monatom b-new)
		          (pos~add-end 1 pos))
           (mass~prove-eql (mass~monom-rest a)
			  (mass~monom-rest b-new)
			  (pos~add-end 2 pos))))))

(defmethod mass~prove-eql ((a mass+monatom) (b mass+monatom) &optional (pos (pos~empty)))
  (let ((func  (mass~get-function a))
	(argsa (mass~get-args a))
	(argsb (mass~get-args b)))
    (cond ((eql func mass*expt)
	   (mass~prove-eql (second argsa) (second argsb) (pos~add-end 2 pos)))
	  ((eql func mass*power)
	   (mass~prove-eql* argsa argsb 1 pos))
	  ((eql func mass*unknown)
	   (mass~prove-eql* (cdr argsa) (cdr argsb) 1 pos)))))

(defmethod mass~prove-eql* ((a cons) (b cons) n &optional (pos (pos~empty)))
  (mass~prove-eql (first a) (first b) (pos~add-end n pos))
  (mass~prove-eql* (cdr a) (cdr b) (+ n 1) pos))

(defmethod mass~prove-eql* (a b n &optional (pos (pos~empty)))
  t)

	 
(defmethod mass~pop ((a mass+termatom) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~eql a (mass~first-termatom b))
	 b)
	((mass~atom b)
	 (error "mass~pop: Sorry, no appropriate mass+termatom to pop."))
	(t
	 (let ((b-new (mass~pop a (mass~term-rest b) (pos~add-end 2 pos))))
	   (cond ((mass~atom (mass~term-rest b))
	          (ca~output-method '((:forward . c-plus)
				      (:backward . c-plus-b))
				    (list pos)))
	         (t
	          (ca~output-method '((:forward . pop-plus)
				      (:backward . pop-plus-b))
				    (list pos))))	       
	   (mass~append a
		       (mass~append (mass~first-termatom b)
				   (mass~term-rest b-new)))))))
		       
	 
(defmethod mass~pop ((a mass+monom) (b mass+term) &optional (pos (pos~empty)))
  (cond ((mass~eql a (mass~get-monom (mass~first-termatom b)))
	 b)
	((mass~atom b)
	 (error "mass~pop: Sorry, no appropriate mass+termatom to pop."))
	(t
	 (let ((b-new (mass~pop a (mass~term-rest b) (pos~add-end 2 pos))))
	   (cond ((mass~atom (mass~term-rest b))
	          (ca~output-method '((:forward . c-plus)
				      (:backward . c-plus-b))
				    (list pos)))
	         (t
	          (ca~output-method '((:forward . pop-plus)
				      (:backward . pop-plus-b))
				    (list pos))))	       
	   (mass~append (mass~first-termatom b-new)
		       (mass~append (mass~first-termatom b)
				   (mass~term-rest b-new)))))))
		       
			       
(defmethod mass~pop ((a mass+monatom) (b mass+monom) &optional (pos (pos~empty)))
  (cond ((mass~eql a (mass~first-monatom b))
	 b)
	((mass~atom b)
	 (error "mass~pop: Sorry, no appropriate mass+monatom to pop."))
	(t
	 (let ((b-new (mass~pop a (mass~monom-rest b) (pos~add-end 2 pos))))
	   (cond ((mass~atom (mass~monom-rest b))
	          (ca~output-method '((:forward . c-times)
				      (:backward . c-times-b))
				    (list pos)))
	         (t
	          (ca~output-method '((:forward . pop-times)
				      (:backward . pop-times-b))
				    (list pos))))	       
	   (mass~append a
		       (mass~append (mass~first-monatom b)
				   (mass~monom-rest b-new)))))))
		
(defmethod mass~pop ((a mass+term) (b mass+monom) &optional (pos (pos~empty)))
  (cond ((mass~eql a (mass~get-base (mass~first-monatom b)))
	 b)
	((mass~atom b)
	 (error "mass~pop: Sorry, no appropriate mass+monatom to pop."))
	(t
	 (let ((b-new (mass~pop a (mass~monom-rest b) (pos~add-end 2 pos))))
	   (cond ((mass~atom (mass~monom-rest b))
	          (ca~output-method '((:forward . c-times)
				      (:backward . c-times-b))
				    (list pos)))
	         (t
	          (ca~output-method '((:forward . pop-times)
				      (:backward . pop-times-b))
				    (list pos))))	       
	   (mass~append (mass~first-monatom b-new)
		       (mass~append (mass~first-monatom b)
				   (mass~monom-rest b-new)))))))
		       
			       
(defmethod mass~equal ((a mass+term) (b mass+term) &optional (pos (pos~empty)))
  (if (mass~eql a b)
      (let ((result (mass~prove-eql a b (pos~add-end 2 pos))))
         (ca~output-method '((:backward . =ref-b)) (list (mass~rebuild a)))
         (mass~create (make-instance 'mass+monatom
			             :func mass*equality
			             :args (list a a)))
      )
    (mass~create (make-instance 'mass+monatom
			        :func mass*equality
			        :args (list a b)))))

;; Building a POST term from MASS syntax:

(defgeneric mass~rebuild (term))

(defmethod mass~rebuild ((term number))
  (let ((num    (env~lookup-object mass*num   ca*global-environment)))
    (term~constant-create term num)))

(defmethod mass~rebuild ((term mass+term))
  (let ((plus   (env~lookup-object mass*plus  ca*global-environment))
	(num    (env~lookup-object mass*num   ca*global-environment))
	(sumnum (list-length (mass~get-term term))))
    (cond ((= 0 sumnum)
	   (term~constant-create 0 num))
	  ((= 1 sumnum)
	   (mass~rebuild (mass~first-termatom term)))
	  ((> sumnum 1)
	   (term~appl-create plus (list (mass~rebuild (mass~first-termatom term))
				        (mass~rebuild (mass~term-rest      term))))))))

(defmethod mass~rebuild ((term mass+termatom))
  (let ((times  (env~lookup-object mass*mult  ca*global-environment))
	(num    (env~lookup-object mass*num   ca*global-environment))
	(coeff  (mass~get-coeff term))
	(monom  (mass~get-monom term)))
    (cond ((= 1 coeff)
	   (mass~rebuild (mass~get-monom term)))
	  ((mass~one monom)
	   (term~constant-create coeff num))
	  (t
	   (term~appl-create times (list (term~constant-create coeff num)
					 (mass~rebuild monom)))))))

(defmethod mass~rebuild ((term mass+monom))
  (let ((times  (env~lookup-object mass*mult  ca*global-environment))
	(num    (env~lookup-object mass*num   ca*global-environment))
	(facnum (list-length (mass~get-monom term))))
    (cond ((= 0 facnum)
	   (term~constant-create 1 num))
	  ((= 1 facnum)
	   (mass~rebuild (mass~first-monatom term)))
	  ((> facnum 1)
	   (term~appl-create times (list (mass~rebuild (mass~first-monatom term))
					 (mass~rebuild (mass~monom-rest term))))))))

(defmethod mass~rebuild ((term mass+monatom))
  (let ((power  (env~lookup-object mass*power ca*global-environment))
        (mod    (env~lookup-object mass*mod   ca*global-environment))
	(num    (env~lookup-object mass*num   ca*global-environment))
	(equal  (env~lookup-object mass*equality ca*global-environment))
	(func   (mass~get-function term))
	(args   (mass~get-args term)))
    (cond ((eql func mass*expt)
	   (cond ((mass~one (second args))
		  (first args))
		 (t
	          (term~appl-create power (list (first args)
					        (mass~rebuild (second args)))))))
	  ((eql func mass*power)
	   (term~appl-create power (list (mass~rebuild (first args))
					 (mass~rebuild (second args)))))
	  ((eql func mass*mod)
	   (term~appl-create mod (list (mass~rebuild (first args))
					 (mass~rebuild (second args)))))
	  ((eql func mass*unknown)
	   (term~appl-create (car args)
			     (mapcar #'mass~rebuild (cdr args))))
	  ((eql func mass*equality)
	   (term~appl-create equal (list (mass~rebuild (first args))
					 (mass~rebuild (second args)))))
	  (t
	   (error "mass~rebuild: Sorry, can't handle function ~A." func)))))

(defmethod mass~rebuild ((term t))
  (env~lookup-object :true (pds~environment omega*current-proof-plan)))

(defmethod mass~rebuild ((term null))
  (env~lookup-object :false (pds~environment omega*current-proof-plan)))




;; Test area:


(defmethod print-object ((obj mass+term) stream)
  (format stream "~%Mass-Term:~%")
  (print-object (mass~rebuild obj) stream)
  (format stream "~%"))

(defmethod print-object ((obj mass+termatom) stream)
  (format stream "~%Mass-Termatom:~%")
  (print-object (mass~rebuild obj) stream)
  (format stream "~%"))

(defmethod print-object ((obj mass+monom) stream)
  (format stream "~%Mass-Monom:~%")
  (print-object (mass~rebuild obj) stream)
  (format stream "~%"))

(defmethod print-object ((obj mass+monatom) stream)
  (format stream "~%Mass-Monatom:~%")
  (print-object (mass~rebuild obj) stream)
  (format stream "~%"))
