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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                   ;;
;;  Well howdy folks!                                                ;;
;;  This is going to be my very own, freshly brewed, hot and spicy,  ;;
;;  yet delicious COMPUTER ALGEBRA SYSTEM!                           ;;
;;  You shouldn't wonder if you can't do anything practical with it, ;;
;;  cos it's only there to manipulate polynomials. Nothing fancy,    ;;
;;  nothing new and espacially nothing useful.....                   ;;
;;                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Der Mensch ist das Mass aller Dinge,
;; wofern sie sind, dafuer dass sie sind,
;; und wofern sie nicht sind, dafuer dass sie nicht sind.
;;
;;                           Prothagoras (someday B.C.)


(in-package "KEIM")


(mod~defmod ca
	    :uses ()
	    :documentation "Computer Algebra Functions"
	    :exports (ca+polynomial
		      ca+nat-polynomial
		      ca+int-polynomial
		      ca+rat-polynomial
		      ca+real-polynomial
		      ca+complex-polynomial
		      ca~polynomial-p
		      ca~get-poly-field
		      ca~set-poly-data!
		      ca~poly-var-number
		      ca~poly-data
		      ca~compare-coeff-fields
		      ca~compare-dimension
		      ca~compare-polynomials
		      ca~create-polynomial
		      ca~copy-polynomial
		      ca~polynomial-addition
		      ca~polynomial-subtraction
		      ca~scalar-mult-on-poly
		      ca~polynomial-multiplication
		      ca~differentiate-polynomial
		      ca~integrate-polynomial
		      ca~minimum
		      ca~polynomial-degree
		      ca~polynomial-let
		      ca~polynomial-roots
		      ca~read-polynomial
		      ca~write-polynomial
		      ca~computed-methods
		      ca~set-verbose
		      ca~reset

		      ca~output-method
		      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datastructures for polynomials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ca+polynomial ()
  ((variate :initarg :variate
	    :initform 1
	    :reader ca=get-poly-var-number)
   (data :initarg :data
	 :initform :nil
	 :reader ca=get-poly-data
	 :writer ca=set-poly-data!))
  (:documentation "variate - number of used variables, i.e. the polynomial-ring (K[x,y,z] etc.)
data - a list that stores coefficients and exponents"))

(defclass ca+nat-polynomial (ca+polynomial)
  ()
  (:documentation "A polynomial over the naturals (positive integers)"))

(defclass ca+int-polynomial (ca+polynomial)
  ()
  (:documentation "A polynomial over the integers"))

(defclass ca+rat-polynomial (ca+polynomial)
  ()
  (:documentation "A polynomial over the rationals"))

(defclass ca+real-polynomial (ca+polynomial)
  ()
  (:documentation "A polynomial over the reals"))

(defclass ca+complex-polynomial (ca+polynomial)
  ()
  (:documentation "A polynomial over the complex numbers"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Constants for the field identifier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant ca*natural-character #\n)
(defconstant ca*integer-character #\z)
(defconstant ca*rational-character #\q)
(defconstant ca*real-character #\r)
(defconstant ca*complex-character #\c)

					
(defvar ca*computed-methods nil)        ; each external-function that manipulates a polynomial
					; should have a before-method to reset this variable
					; or this should be done in the interface that returns the methods...
					; last is better I guess, as functions might be calling each other

(defvar ca*verbose nil)                 ; a variable to switch on and of verbose mode, i.e. if a method should
					; be added or not.
					; Some methods, as for instance the dc method for multiplication
					; might prefer to write their methods seperatly with no connection
					; to the actual computing being done.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  The main functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ca~reset ()
  (declare (edited  "30-MAR-1995")
	   (authors Sorge)
	   (effect  "Resets the internal variables"))
  (setf ca*computed-methods nil))

(defun ca~set-verbose (&optional (symbol t))
  (setf ca*verbose symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates and constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; something to query


(defun ca~polynomial-p (obj)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "An object")
	   (effect  "None")
	   (value   "True if object is a polynomial"))
  (typep obj 'ca+polynomial))


(defun ca~poly-var-number (poly)
  (declare (edited  "27-NOV-1995 16:10")
	   (authors SORGE)
	   (input   "A polynomial")
	   (value   "The variable-number of the polynomial"))
  (ca=get-poly-var-number poly))


(defun ca~poly-data (poly)
  (declare (edited  "27-NOV-1995 16:11")
	   (authors SORGE)
	   (input   "A polynomial")
	   (value   "The data-list of the polynomial"))
  (ca=get-poly-data poly))


(defun ca~set-poly-data! (poly data)
  (declare (edited  "27-NOV-1995 16:12")
	   (authors SORGE)
	   (input   "A polynomial and an appropriate data-list")
	   (effect  "Sets the data of the polynomial to DATA")
	   (value   "The updated polynomial"))
  (ca=set-poly-data! data poly))


(defgeneric ca~get-poly-field (poly)
  (declare (edited  "30-MAR-1995")
	   (authors Sorge)
	   (input   "A polynomial")
	   (effect  "None")
	   (value   "An character naming the field of coefficients of poly"))
  (:method (poly)
	   (error "~A is not a polynomial" poly))
  (:method ((poly ca+nat-polynomial))
	   (values ca*natural-character (ca=get-poly-var-number poly)))
  (:method ((poly ca+int-polynomial))
	   (values ca*integer-character (ca=get-poly-var-number poly)))
  (:method ((poly ca+rat-polynomial))
	   (values ca*rational-character (ca=get-poly-var-number poly)))
  (:method ((poly ca+real-polynomial))
	   (values ca*real-character (ca=get-poly-var-number poly)))
  (:method ((poly ca+complex-polynomial))
	   (values ca*complex-character (ca=get-poly-var-number poly))))

(defgeneric ca~compare-coeff-fields (poly1 poly2)
  (declare (edited  "29-MAR-1995")
	   (authors Sorge)
	   (input   "Two polynomials")
	   (effect  "None")
	   (value   "T if polynomials are having the same field of coefficient"))
  (:method (poly1 poly2)
	   (error "both ~A and ~A must be polynomials" poly1 poly2))
  (:method ((poly1 ca+polynomial) (poly2 ca+polynomial))
	   (equal (type-of poly1) (type-of poly2))))


(defgeneric ca~compare-dimension (poly1 poly2)
  (declare (edited  "30-MAR-1995")
	   (authors Sorge)
	   (input   "Two polynomials")
	   (effect  "None")
	   (value   "T if both polynomials are having the same dimension, i.e. number of variables"))
  (:method (poly1 poly2)
	   (error "both ~A and ~A must be polynomials" poly1 poly2))
  (:method ((poly1 ca+polynomial) (poly2 ca+polynomial))
	   (= (ca=get-poly-var-number poly1) (ca=get-poly-var-number poly2))))


(defgeneric ca~compare-polynomials (poly1 poly2)
  (declare (edited  "30-MAR-1995")
	   (authors Sorge)
	   (input   "Two polynomials")
	   (effect  "None")
	   (value   "T if both polynomials are the same (in sense of equal)"))
  (:method (poly1 poly2)
	   (error "both ~A and ~A must be polynomials" poly1 poly2))
  (:method ((poly1 ca+polynomial) (poly2 ca+polynomial))
	   (when (and (ca~compare-coeff-fields poly1 poly2) (ca~compare-dimension poly1 poly2))
	     (equal (ca=get-poly-data poly1) (ca=get-poly-data poly2)))))

(defgeneric ca~polynomial-degree (poly)
  (declare (edited  "03-JUL-1997 21:36")
	   (authors SORGE)
	   (input   "A polynomial.")
	   (value   "The degree of the polynomial."))
  (:method (poly)
	   (error "~A must be a polynomial" poly))
  (:method ((poly ca+polynomial))
	   (let ((lc (first (ca=get-poly-data poly))))
	     (apply #'+ (ca=exps lc)))))
  
;; the creative part

(defgeneric ca~create-polynomial (poly &key field var)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A polynomial (in a certain form) and maybe a key for a field-character and/or a number")
	   (effect  "None")
	   (value   "An instance of a natural polynomial"))
  (:method :before (poly &key (field ca*real-character) (var 1))
	   (declare (ignore poly))
	   (when (not (characterp field))
	     (error "~A is not a valid field-identifier" field))
	   (when (not (typep var '(integer 0)))
	     (error "The number must be an positive integer not ~D" var)))
  (:method (poly &key (field ca*real-character) (var 1))
	   (declare (ignore field var))
	   (error "~A is not the expected form for a polynomial" poly))
  (:method ((poly list) &key (field ca*real-character) (var 1))
	   (if (= var 0) (ca=simplify-non-poly poly)
	     (ca=simplify-on-zeros!
	      (ca=simplify-on-matching-exponents!
	       (ca=sort-poly! (ca=create-polynomial field poly var))))))
	   
;; at this point we include a most skillful piece of programming, called:
;; mycas-input-intfc (some might call it shit, some call it ganja, some
;;                    call it marijuhana...legalize it)

  (:method ((poly string) &key (field ca*real-character) (var 1))
	   (multiple-value-setq (poly field var) (ca=read-from-console poly))
	   (ca~create-polynomial poly :field field :var var)))

(defgeneric ca=create-polynomial (field data var)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A field-identifier and a list of polynomial-data")
	   (effect  "None")
	   (value   "Returns an instance of poly in ring field[x1,...xvar]"))
  (:method (field data var)
	   (declare (ignore var data))
	   (error "~A is not a valid field-identifier" field))
  (:method ((field (eql ca*natural-character)) data var)
	   (make-instance 'ca+nat-polynomial
			  :variate var
			  :data (ca=check-poly-validity data var #'(lambda (x) (typep x '(integer 0))))))
  (:method ((field (eql ca*integer-character)) data var)
	   (make-instance 'ca+int-polynomial
			  :variate var
			  :data (ca=check-poly-validity data var #'integerp)))
  (:method ((field (eql ca*rational-character)) data var)
	   (make-instance 'ca+rat-polynomial
			  :variate var
			  :data (ca=check-poly-validity data var #'rationalp)))
  (:method ((field (eql ca*real-character)) data var)
	   (make-instance 'ca+real-polynomial
			  :variate var
			  :data (ca=check-poly-validity data var #'realp)))
  (:method ((field (eql ca*complex-character)) data var)
	   (make-instance 'ca+complex-polynomial
			  :variate var
			  :data (ca=check-poly-validity data var #'(lambda (x) (or (complexp x) (realp x))))))
  )


(defun ca~copy-polynomial (poly)
  (declare (edited  "30-MAR-1995")
	   (authors Sorge)
	   (input   "A Polynomial")
	   (effect  "None")
	   (value   "A copy of the polynomial"))
  (ca~create-polynomial (ca=get-poly-data poly)
			:field (ca~get-poly-field poly)
			:var (ca=get-poly-var-number poly)))


;; avoid some major errors

(defun ca=check-poly-validity (data number test)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A list of polynomial data, the number of valid variables and a test predicate")
	   (effect  "Signals an error if invalid data as input")
	   (value   "The polynomial data"))
  ;; check the validity of the exponents
  (mapcar #'(lambda (x)
	      (when (not (every #'(lambda (y)
				    (typep y '(integer 0)))
				(ca=exps x)))
		(error "~A is an invalid list of exponents for a polynomial" (ca=exps x)))
	      x)
	  ;; testing the coefficient type
	  (mapcar #'(lambda (x)
		      (when (not (apply test (list (ca=coeff x))))
			(error "~A is not of the expected coefficient type" (ca=coeff x)))
		      x)
		  ;; trimming the monomial list 
		  (mapcar #'(lambda (x)
			      (if (consp x)
				  (if (> (1+ number) (length x))
				      (append x (make-list (- (1+ number) (length x)) :initial-element 0))
				    (butlast x (- (length x) (1+ number))))
				(values)))
			  ;; removing rubbish
			  (remove-if-not #'consp data)))))

(defun ca=sort-poly! (poly)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A Polynomial")
	   (effect  "Sorts the polynomial data list")
	   (value   "The sorted polynomial"))
  (let ((poly-data (copy-list (ca=get-poly-data poly))))
    (when poly-data
      (do ((i (ca=get-poly-var-number poly) (1- i)))
	  ((= i 0))
	(ca=set-poly-data! (stable-sort (ca=get-poly-data poly) #'(lambda (x y) (> (nth i x) (nth i y)))) poly)))
    (when ca*verbose (ca=update-methods-after-sort (ca=get-poly-data poly) poly-data))
    poly)
  )

;;; we need some additional functions to output adequate methods after sorting!

(defun ca=update-methods-after-sort (list1 list2)
  (declare (edited  "10-DEC-1995 19:35")
	   (authors SORGE)
	   (input   "Two polynomial data lists.")
	   (effect  "Outputs the necessary logical methods, i.e. we take a"
		    "look at what has happened during the sorting by"
		    "comparing the old and the new lists and make sure that all"
		    "necessary logical methods are derived.")
	   (value   "Undefined."))
  (let ((pol-length (1- (length list1))))
    (labels ((ca=umas-with-counter (l1 l2 count)
                                   (when l1
                                     (let ((pos2 (+ count (position (car l1) l2 :test #'equal)))
                                           (pos1 count))
                                       (when (> pos2 pos1)
					 (ca~output-method '((:forward . :poly-sort-forw) (:backward . :poly-sort-back))
							   (list pos2 pos1 pol-length)))
                                       (ca=umas-with-counter (cdr l1) (remove (car l1) l2 :test #'equal :count 1) (1+ count))))))
    (ca=umas-with-counter list1 list2 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplification functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric ca=simplify-on-zeros! (poly)
  (declare (edited  "29-MAR-1995")
	   (authors Sorge)
	   (input   "A polynomial")
	   (effect  "Erases every monomial with coefficient zero")
	   (value   "The simplified polynomial"))
  (:method ((poly ca+polynomial))
	   (ca=set-poly-data!
	    (ca=simplify-on-zeros!(ca=get-poly-data poly))
	    poly)
	   poly)
  (:method ((data list))
	   (let ((pol-length (1- (length data)))             ;;; this again is needed for the method-output
		 (count -1))
	     (when (consp data)
	       (remove-if #'(lambda (x) (and (setf count (1+ count))
					     (= (ca=coeff x) 0)
					     (not (ca~output-method '((:backward . expand-0)
								      (:forward . reduce-0))
								    (list count pol-length)))
					     (setf count (1- count))
					     (setf pol-length (1- pol-length))))
			  data)))))

;; One fine day I was testing whether the interior function or
;; a recursive one would be faster on a list.
;; It was a close finish, but remove-if was victorious
;;(defun timetest (data)
;;  (cond ((null data) data)
;;	((= 0 (caar data))
;;      	 (ca~output-method "reduction on zero coefficient~%")
;;	 (ca=simplify-on-zeros! (cdr data)))
;;	(t (append (list (car data)) (ca=simplify-on-zeros! (cdr data))))))


(defun ca=simplify-on-matching-exponents! (poly)
  (declare (edited  "30-MAR-1995")
	   (authors Sorge)
	   (input   "A polynomial")
	   (effect  "Sums monomials with the same exponent-lists")
	   (value   "The simplified polynomial"))
  (ca=set-poly-data!
   (ca=simplify-on-matching-exponents-rec (ca=get-poly-data poly))
   poly)
  poly)


(defun ca=simplify-on-matching-exponents-rec (data)
  (declare (edited  "29-MAR-1995")
	   (authors Sorge)
	   (input   "A polynomial data-list")
	   (effect  "None")
	   (value   "A data-list which has the monomials with the same exponent-list summed"))
  (labels ((ca=somer-with-count (dat count)                    ;;; this inline function is just for method-output
         	(cond ((null dat) dat)
		      ((equal (ca=exps (first dat)) (ca=exps (second dat)))
		       (ca~output-method '((:backward . expand-sum) (:forward . reduce-sum))
					 (list count (+ count (1- (length dat)))))
		       (ca=somer-with-count
			(append
			 (list (append (list (+ (ca=coeff (first dat)) (ca=coeff (second dat))))
				       (ca=exps (second dat))))
			 (cddr dat))
			count))
		      (t (append (list (car dat))
				 (ca=somer-with-count (cdr dat) (1+ count)))))))
    (ca=somer-with-count data 0)))


(defun ca=simplify-non-poly (data)
  (declare (edited  "19-APR-1995")
	   (authors Sorge)
	   (input   "A pseudo-polynomial-data-list")
	   (effect  "Reduces the list on addition")
	   (value   "A number")
	   (example "((1) (2) (3) (4)) -> 10; (1 2 3 4) -> 10; (((1) (2)) ((3) (4))) -> 10"))
  (do ((hdata data (reduce #'append hdata)))
      ((notany #'consp hdata)
       (if (every #'numberp hdata) (reduce #'+ hdata)
	 (error "There is some rubbish in ~A" data)))))


;; dedicated to M. Kerber


(defun ca=coeff (monomial)
  (declare (edited  "18-APR-1995")
	   (authors Sorge)
	   (input   "A monomial data structure")
	   (effect  "None")
	   (value   "The coefficient of that monomial"))
  (when (consp monomial) (car monomial)))


(defun ca=exps (monomial)
  (declare (edited  "18-APR-1995")
	   (authors Sorge)
	   (input   "A monomial data structure")
	   (effect  "None")
	   (value   "The exponentlist of that monomial"))
  (when (consp monomial) (cdr monomial)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polynomial arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric ca~polynomial-addition (poly1 poly2)
  (declare (edited  "30-MAR-1995")
	   (authors Sorge)
	   (input   "Two polynomials")
	   (effect  "None")
	   (value   "A polynomial containing the sum of the polynomials"))
  (:method :before ((poly1 ca+polynomial) (poly2 ca+polynomial))
	   (unless (and (ca~compare-coeff-fields poly1 poly2)
			(ca~compare-dimension poly1 poly2))
	     (error "both ~A and ~A must have coefficients of the same field" poly1 poly2)))
  (:method (poly1 poly2)
	   (error "both ~A and ~A must be polynomials" poly1 poly2))
  (:method ((poly1 ca+polynomial) (poly2 ca+polynomial))
	   (ca~output-method '((:backward . :poly-split-plus) (:forward . :poly-plus)))
	   (ca~create-polynomial (ca=polynomial-addition-rec (ca=get-poly-data poly1)
							     (ca=get-poly-data poly2))
				 :field (ca~get-poly-field poly1)
				 :var (ca=get-poly-var-number poly1))))


(defun ca=polynomial-addition-rec (data1 data2)
  (declare (edited  "30-MAR-1995")
	   (authors Sorge)
	   (input   "Two lists of polynomial data")
	   (effect  "None")
	   (value   "A list containing the sum of the input lists"))
  (cond ((null data1) data2)
	((null data2) data1)
	((equal (ca=exps (car data1)) (ca=exps (car data2)))
	 (ca~output-method '((:backward . :mon-split-plus) (:forward . :mon-plus)))
	 (append (list
		  (append (list (+ (ca=coeff (car data1)) (ca=coeff (car data2))))
			  (ca=exps (car data1))))
		 (ca=polynomial-addition-rec (cdr data1) (cdr data2))))
	((ca=list-greater-query
	  (ca=exps (car data1))
	  (ca=exps (car data2)))        
	 (ca~output-method '((:backward . :push-first) (:forward . :pop-first)))
	 (append (list (car data1))          
		 (ca=polynomial-addition-rec (cdr data1) data2)))
	(t (ca~output-method '((:backward . :push-second) (:forward . :pop-second)))
	   (append (list (car data2))
		   (ca=polynomial-addition-rec data1 (cdr data2))))))

(defun ca=list-greater-query (list1 list2)
  (declare (edited  "09-DEC-1995 22:24")
	   (authors SORGE)
	   (input   "Two lists of numbers.")
	   (value   "LIST1 is greater than LIST2 if at least on number is greater than the equivalent in LIST2"
		    "and there is no number prior to that in LIST1 one that is smaller than its equivalent"
		    "in LIST2.")
	  (example  "'(1 2 1) is list-greater than '(1 1 2)"
		    "'(1 2 1) is not list-greater than '(2 1 0)"))
  (cond ((null list1) nil)
	((null list2) nil)
	((= (car list1) (car list2)) (ca=list-greater-query (cdr list1) (cdr list2)))
	((> (car list1) (car list2)) t)
	(t nil)))

(defgeneric ca~polynomial-subtraction (poly1 poly2)
  (declare (edited  "07-APR-1995")
	   (authors Sorge)
	   (input   "Two polynomials")
	   (effect  "None")
	   (value   "A polynomial containing poly1-poly2"))
  (:method (poly1 poly2)
	   (error "both ~A and ~A must be polynomials" poly1 poly2))
  (:method :before ((poly1 ca+polynomial) (poly2 ca+polynomial))
	   (unless (and (ca~compare-coeff-fields poly1 poly2)
			(ca~compare-dimension poly1 poly2))
	     (error "both ~A and ~A must have coefficients of the same field" poly1 poly2)))
  (:method ((poly1 ca+nat-polynomial) (poly2 ca+nat-polynomial))
	   (error "This operation can't be performed on natural polynomials"))
  (:method ((poly1 ca+polynomial) (poly2 ca+polynomial))
	   (ca~create-polynomial
	    (ca=polynomial-subtraction-rec
	     (ca=get-poly-data poly1)
	     (ca=get-poly-data poly2))
	    :field (ca~get-poly-field poly1)
	    :var (ca=get-poly-var-number poly1))))


(defun ca==polynomial-subtraction (data1 data2)
  (declare (edited  "07-APR-1995")
	   (authors Sorge)
	   (input   "Two polynomial data-lists")
	   (effect  "None")
	   (value   "A list containing the difference of the input-lists"))
  (ca~output-method "polynomial subtraction~%")
  (ca=polynomial-addition-rec
   data1
   (ca=scalar-mult-on-poly data2 -1)))


(defun ca=polynomial-subtraction-rec (data1 data2)
  (declare (edited  "19-APR-1995")
	   (authors Sorge)
	   (input   "Two polynomial data-lists")
	   (effect  "None")
	   (value   "A list containing the difference of the input-lists"))
  (cond ((null data1) data2)
	((null data2) data1)
	((equal (ca=exps (car data1)) (ca=exps (car data2)))
	 (ca~output-method "monomial subtraction~%")
	 (append (list
		  (append (list (- (ca=coeff (car data1)) (ca=coeff (car data2))))
			  (ca=exps (car data1))))
		 (ca=polynomial-subtraction-rec (cdr data1) (cdr data2))))
	((some #'>                           
	       (ca=exps (car data1))         
	       (ca=exps (car data2)))        
	 (append (list (car data1))          
		 (ca=polynomial-subtraction-rec (cdr data1) data2)))
	(t (append (list
		    (append (list (* -1 (ca=coeff (car data2))))
			    (ca=exps (car data2))))
		   (ca=polynomial-subtraction-rec data1 (cdr data2))))))

  

(defgeneric ca~scalar-mult-on-poly (poly scalar)
  (declare (edited  "31-MAR-1995")
	   (authors Sorge)
	   (input   "A polynomial and a scalar")
	   (effect  "None")
	   (value   "A polynomial multiplied with the scalar"))
  (:method (poly scalar)
	   (declare (ignore poly scalar))
	   (error "not the right input objects for a scalar-multiplication"))
  (:method ((poly ca+polynomial) scalar)
	   (error "~A is not of the right number type for ~A" scalar poly))
  (:method ((poly ca+nat-polynomial) (scalar number))
	   (if (typep scalar '(integer 0))
	       (ca~create-polynomial 
		(ca=scalar-mult-on-poly (ca=get-poly-data poly) scalar)
		:field ca*natural-character    
		:var (ca=get-poly-var-number poly))
	     (error "~D is not a natural number" scalar)))
  (:method ((poly ca+int-polynomial) (scalar integer))
	   (ca~create-polynomial 
	    (ca=scalar-mult-on-poly (ca=get-poly-data poly) scalar)
	    :field ca*integer-character
	    :var (ca=get-poly-var-number poly)))
  (:method ((poly ca+rat-polynomial) (scalar rational))
	   (ca~create-polynomial 
	    (ca=scalar-mult-on-poly (ca=get-poly-data poly) scalar)
	    :field ca*rational-character
	    :var (ca=get-poly-var-number poly)))
  (:method ((poly ca+real-polynomial) (scalar real))
	       (ca~create-polynomial 
		(ca=scalar-mult-on-poly (ca=get-poly-data poly) scalar)
		:field (ca~get-poly-field poly)
		:var (ca=get-poly-var-number poly)))
  (:method ((poly ca+complex-polynomial) (scalar number))
	   (if (typep scalar '(or real complex))
	       (ca~create-polynomial 
		(ca=scalar-mult-on-poly (ca=get-poly-data poly) scalar)
		:field (ca~get-poly-field poly)
		:var (ca=get-poly-var-number poly))
	     (error "~D is not a complex number" scalar))))
	   

(defun ca=scalar-mult-on-poly (data scalar)
  (declare (edited  "07-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial data-list and a scalar")
	   (effect  "None")
	   (value   "A list containing the input list multiplied with the scalar"))
  (ca~output-method '((:backward . :split-stimes-poly) (:forward . :stimes-poly)))
  (let ((last (car (last data))))
    (mapcar #'(lambda (x)
		(unless (every #'= x last)
		  (ca~output-method '((:backward . :cummulate-left) (:forward . :distribute-left))))
		(ca~output-method '((:backward . :mon-split-stimes) (:forward . :mon-stimes)))
		(substitute (* scalar (ca=coeff x)) (ca=coeff x) x :count 1))
	    data)))


(defgeneric ca~polynomial-multiplication (poly1 poly2 &key scheme)
  (declare (edited  "06-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial and a flag to switch on the devide-and-conquer algorithm")
	   (effect  "None")
	   (value   "A polynomial containing the result of poly1 * poly2"))
  (:documentation "The primitive multiplication algorithm:
It only multiplies the first element of data1 with all the elements of data2, then the second element of data1 and so on. The pure algorithm is in the categories of O(n^2).
The divide-and-conquer multiplication algorithm:
The algorithm is based on an idea of Karatsuba and Ofman.")
  (:method (poly1 poly2 &key (scheme nil))
	   (declare (ignore scheme))
	   (error "both ~A and ~A must be polynomials" poly1 poly2))
  (:method :before ((poly1 ca+polynomial) (poly2 ca+polynomial) &key (scheme nil))
	   (declare (ignore scheme))
	   (unless (and (ca~compare-coeff-fields poly1 poly2)
			(ca~compare-dimension poly1 poly2))
	     (error "both ~A and ~A must have the same field" poly1 poly2)))
  (:method ((poly1 ca+polynomial) (poly2 ca+polynomial) &key (scheme nil))
	   (ca~output-method '((:backward . :poly-split-times) (:forward . :poly-times)))
	   (ca~create-polynomial
	    (ca=polynomial-multiplication (ca=get-poly-data poly1)
					  (ca=get-poly-data poly2)
					  scheme)
	    :field (ca~get-poly-field poly1)
	    :var (ca=get-poly-var-number poly1))))


;; This f..... polynomial multiplication isn't as easy as it seemed at first!
;; In terms of logic we have to take care of zero multiplication seperately,
;; so this adds up to some more work (and computation time?).

(defgeneric ca=polynomial-multiplication (data1 data2 scheme)
  (declare (edited  "04-MAY-1995")
	   (authors Sorge)
	   (input   "Two polynomial data-lists and a flag")
	   (effect  "None")
	   (value   "A polynomial data-list, containing the result of the multiplication"))
  (:method ((data1 (eql nil)) (data2 (eql nil)) scheme)
	   (declare (ignore scheme))
	   (ca~output-method '((:backward . omega::catac=mult-0-left)))
	   nil)
  (:method ((data1 (eql nil)) (data2 cons) scheme)
	   (declare (ignore scheme))
	   (ca~output-method 'omega::catac=mult-0-left)
	   nil)
  (:method ((data1 cons) (data2 (eql nil)) scheme)
	   (declare (ignore scheme))
	   (ca~output-method 'omega::catac=mult-0-right)
	   nil)
  (:method ((data1 cons) (data2 cons) (scheme (eql nil)))
	   (let ((result (ca=polynomial-multiplication-rec data1 data2)))
	     (ca=remove-output-method '((:backward . :cummulate-right) (:forward . :distribute-right)))
	     (ca=remove-output-method '((:backward . :pop-last) (:forward . :push-last)))
	     (ca~output-method '((:backward . :reset-poly-mult) (:forward . :reset-poly-mult)))
	     result))
  (:method ((data1 cons) (data2 cons) (scheme (eql :rec)))
	   (let ((result (ca=polynomial-multiplication-rec1 data1 data2)))
	     (ca=remove-output-method '((:backward . :cummulate-right) (:forward . :distribute-right)))
	     (ca=remove-output-method '((:backward . :pop-last) (:forward . :push-last)))
	     (ca~output-method '((:backward . :reset-poly-mult) (:forward . :reset-poly-mult)))
	     result))
  (:method ((data1 cons) (data2 cons) (scheme (eql :it)))
	   (let ((result (ca=polynomial-multiplication-it data1 data2)))
	     (ca=remove-output-method '((:backward . :cummulate-right) (:forward . :distribute-right)))
	     (ca=remove-output-method '((:backward . :pop-last) (:forward . :push-last)))
	     (ca~output-method '((:backward . :reset-poly-mult) (:forward . :reset-poly-mult)))
	     result))
  (:method ((data1 cons) (data2 cons) (scheme (eql :dc)))
	   (if (= 1 (length (ca=exps (car data1))))
	       (let ((ca*verbose nil))
		 (ca=polynomial-multiplication-dc data1 data2))
	     (error "multiplication with devide-and-conquer works only for univariate polynomials"))))
	       
	   
(defun ca=polynomial-multiplication-rec (data1 data2)
  (declare (edited  "06-APR-1995")
	   (authors Sorge)
	   (input   "Two lists of polynomial data")
	   (effect  "None")
	   (value   "A list containing the multiplication of the input lists"))
  (when data1
    (ca~output-method '((:backward . :cummulate-right) (:forward . :distribute-right)))
    (append
     (mapcar #'(lambda (x)
		 (ca~output-method '((:backward . :mon-split-times) (:forward . :mon-times)))
		 (cons (* (ca=coeff (car data1)) (ca=coeff x))
		       (map 'list #'+ (ca=exps (car data1)) (ca=exps x))))
	     data2)
     (ca~output-method '((:backward . :pop-last) (:forward . :push-last)))
     (ca=polynomial-multiplication-rec (cdr data1) data2))
    ))

(defun ca=polynomial-multiplication-it (data1 data2)
  (declare (edited  "28-FEB-1996 20:14")
	   (authors SORGE)
	   (input   "Two lists of polynomial data")
	   (value   "A list containing the multiplication of the input lists"))
  (apply #'append
	 (mapcar #'(lambda (x)
		     (ca~output-method '((:backward . :cummulate-right) (:forward . :distribute-right)))
		     (mapcar #'(lambda (y)
				 (ca~output-method '((:backward . :mon-split-times) (:forward . :mon-times)))
				 (cons (* (ca=coeff x) (ca=coeff y))
				       (map 'list #'+ (ca=exps x) (ca=exps y))))
			     data2)
		     (ca~output-method '((:backward . :pop-last) (:forward . :push-last))))
		 data1)))

(defun ca=polynomial-multiplication-rec1 (data1 data2)
  (declare (edited  "06-APR-1995")
	   (authors Sorge)
	   (input   "Two lists of polynomial data")
	   (effect  "None")
	   (value   "A list containing the multiplication of the input lists"))
  (when data1
    (ca~output-method '((:backward . :cummulate-right) (:forward . :distribute-right)))
    (append
     (ca=polynomial-multiplication-rec2 (car data1) data2)
     (ca~output-method '((:backward . :pop-last) (:forward . :push-last)))
     (ca=polynomial-multiplication-rec1 (cdr data1) data2))
    ))

(defun ca=polynomial-multiplication-rec2 (monomial data2)
  (declare (edited  "06-APR-1995")
	   (authors Sorge)
	   (input   "A Monomial and a list of polynomial data.")
	   (effect  "None.")
	   (value   "A list containing the multiplication of the monomial and the list."))
  (when data2
    (ca~output-method '((:backward . :mon-split-times) (:forward . :mon-times)))
    (append
     (list (cons (* (ca=coeff monomial) (ca=coeff (car data2)))
	   (map 'list #'+ (ca=exps monomial) (ca=exps (car data2)))))
     (ca=polynomial-multiplication-rec2 monomial (cdr data2)))))

(defun ca=polynomial-multiplication-dc (data1 data2)
  (declare (edited  "06-APR-1995")
	   (authors Sorge)
	   (input   "Two lists of polynomial data")
	   (effect  "None")
	   (value   "A list containing the multiplication of the input lists"))

  ;; Let F := data1 and G := data2. Then F = f0 x^m + f1 and G = g0 x^m + g1
  ;; The whole function works with the formula: f0 g0 x^2m + [(f1 + f0)(g1 + g0) - f0 g0 - f1 g1] x^m + f1 g1
  ;; If f1 is zero then f0 must be a single monomial and F is actually multiplied with G.
  ;; In all other cases the formula is computed which means that this very function is called recursively.

  (let* ((m (ca=split-poly-at-half data1))
	 (f0 (ca=split-poly-at-exponent data1 m))
	 (f1 (ca=trim-poly-list data1 (length f0)))
	 (g0 (ca=split-poly-at-exponent data2 m))
	 (g1 (ca=trim-poly-list data2 (length g0))))
    (cond ((null f1) (mapcar #'(lambda (x)
				 (ca~output-method "I'm multiplying something~%" nil :force t)
				 (append (list (* (ca=coeff x) (ca=coeff (car f0))))
					 (list (+ (car (ca=exps x)) m))))
			     data2))
	  ((null g0) (let ((y (ca=polynomial-multiplication-dc f1 g1))
			   (z (ca=polynomial-multiplication-dc
			       (ca=polynomial-addition-rec f1 f0)
			       g1)))
		       ;; [(f1 + f0) g1 - f1 g1] x^m + f1 g1
		       (ca=polynomial-addition-rec
			(ca=add-exponent (ca=polynomial-subtraction-rec z y) m)
			y)))
	  ((null g1) (let ((x (ca=polynomial-multiplication-dc f0 g0))
			   (z (ca=polynomial-multiplication-dc
			       (ca=polynomial-addition-rec f1 f0)
			       g0)))
		       ;; f0 g0 x^2m + [(f1 + f0) g0 - f0 g0] x^m
		       (ca=polynomial-addition-rec
			(ca=add-exponent x (* 2 m))
			(ca=add-exponent (ca=polynomial-subtraction-rec z x) m))))
	  (t (let ((x (ca=polynomial-multiplication-dc f0 g0))
		   (y (ca=polynomial-multiplication-dc f1 g1))
		   (z (ca=polynomial-multiplication-dc
		       (ca=polynomial-addition-rec f1 f0)
		       (ca=polynomial-addition-rec g1 g0))))
	       ;; f0 g0 x^2m + [(f1 + f0)(g1 + g0) - f0 g0 - f1 g1] x^m + f1 g1
	       (ca=polynomial-addition-rec
		(ca=polynomial-addition-rec
		 (ca=add-exponent x (* 2 m))
		 (ca=add-exponent
		  (ca=polynomial-subtraction-rec z
					     (ca=polynomial-addition-rec x y))
		  m))
		y))))))
		 

(defun ca=trim-poly-list (list number)
  (declare (edited  "07-APR-1995")
	   (authors Sorge)
	   (input   "A list and a number")
	   (effect  "None")
	   (value   "The list with n (=number) elements cut of"))
  (if number (reverse (butlast (reverse list) number))
    list))


(defun ca=split-poly-at-half (data)
  (declare (edited  "07-APR-1995")
	   (authors Sorge)
	   (input   "A list fo polynomial data")
	   (effect  "None")
	   (value   "Determines the exponent of the middlemost monomial. If data has an even number of monomials, the middlemost is (n/2)+1 (if you know what I mean)."))
  (car (ca=exps (nth (- (length data)
			(1+ (floor (/ (length data) 2))))
		  data))))


(defun ca=split-poly-at-exponent (data exp)
  (declare (edited  "07-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial data list and an exponent number")
	   (effect  "None")
	   (value   "The list from the monomial with biggest exponent till the monomial with exponent number devided by x^exp"))
  (mapcar
   #'(lambda (x) (append (list (ca=coeff x))
			 (list (- (car (ca=exps x)) exp))))
   (subseq data
	   0
	   (position exp
		     data
		     :test #'(lambda (x y) (> x (car (ca=exps y))))))))


(defun ca=add-exponent (data exp)
  (declare (edited  "07-APR-1995")
	   (authors Sorge)
	   (input   "A list of polynomial data and a exponent number")
	   (effect  "None")
	   (value   "The list multiplied with x^exp"))
  (mapcar #'(lambda (x)
	      (append (list (car x))
		      (list (+ (cadr x) exp))))
	  data))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Differentiation and integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ca~differentiate-polynomial (poly arg)
  (declare (edited  "31-MAR-1995")
	   (authors Sorge)
	   (input   "A polynomial and a variable-number")
	   (effect  "None")
	   (value   "The first derivative of the polynomial with the respect to the specified variable"))
  (:method (poly arg)
	   (error "~A must be a polynomial and ~A a positive integer" poly arg))
  (:method :before ((poly ca+polynomial) (arg number))
	   (unless (typep arg '(integer 1))
	     (error "~D must be a positive integer" arg))
	   (when (> arg (ca=get-poly-var-number poly))
	     (error "~A has less then ~D variables" poly arg)))
  (:method ((poly ca+polynomial) (arg number))
	   (when (null (ca=get-poly-data poly))
	     (ca~output-method "derivative of zero~%"))
	   (let ((new-poly (ca~create-polynomial
			    (ca=differentiate-polynomial-rec (ca=get-poly-data poly) arg)
			    :field (ca~get-poly-field poly)
			    :var (ca=get-poly-var-number poly))))
	     (ca=remove-output-method '((:forward . :add-mon) (:backward . :separate-mon)))
	     new-poly)))


(defun ca=differentiate-polynomial-rec (data arg)
  (declare (edited  "31-MAR-1995")
	   (authors Sorge)
	   (input   "A polynomial-data list and a number")
	   (effect  "None")
	   (value   "A list with the differentiated data"))
  (cond ((null data) data)
	((= 0 (nth arg (car data)))
	 (ca~output-method '((:forward . :const-diff) (:backward . :const-integ)))
	 (ca=differentiate-polynomial-rec (cdr data) arg)             ;;; this line is actually computation!
	 (ca~output-method '((:forward . :add-mon) (:backward . :separate-mon))))
	(t
	 (ca~output-method '((:forward . :mon-diff) (:backward . :mon-integ)))
	 (let ((result 
		(append                                                      ;;; that here is the computation!
		 (list (append                                               ;;;
			(list (* (caar data) (nth arg (car data))))          ;;;
			(butlast (cdar data) (- (length (car data)) arg))    ;;;
			(list (1- (nth arg (car data))))                     ;;;  
			(nthcdr (1+ arg) (car data))))                       ;;;
		 (ca=differentiate-polynomial-rec (cdr data) arg))))           ;;; until here.
	   (ca~output-method '((:forward . :add-mon) (:backward . :separate-mon)))
	   result))))

(defgeneric ca~integrate-polynomial (poly arg &optional constant)
  (declare (edited  "19-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial, an argument number and an integration constant (defaults to 0)")
	   (effect  "None")
	   (value   "The polynomial, integrated on the variable specified by arg"))
  (:method (poly arg &optional (constant 0))
	   (declare (ignore constant))
	   (error "~A must be a polynomial and ~A a positive integer" poly arg))
  (:method :before ((poly ca+polynomial) (arg number) &optional (constant 0))
	   (declare (ignore constant))
	   (unless (typep arg '(integer 1))
	     (error "~D must be a positive integer" arg))
	   (when (> arg (ca=get-poly-var-number poly))
	     (error "~A has less then ~D variables" poly arg)))
  (:method :before ((poly ca+nat-polynomial) arg &optional (constant 0))
	   (declare (ignore arg))
	   (unless (typep constant '(integer 0))
	     (error "~A must be a positive integer" constant)))
  (:method :before ((poly ca+int-polynomial) arg &optional (constant 0))
	   (declare (ignore arg))
	   (unless (typep constant 'integer)
	     (error "~A must be an integer" constant)))
  (:method :before ((poly ca+rat-polynomial) arg &optional (constant 0))
	   (declare (ignore arg))
	   (unless (typep constant 'rational)
	     (error "~A must be a rational" constant)))
  (:method :before ((poly ca+real-polynomial) arg &optional (constant 0))
	   (declare (ignore arg))
	   (unless (typep constant 'real)
	     (error "~A must be a real number" constant)))
  (:method :before ((poly ca+complex-polynomial) arg &optional (constant 0))
	   (declare (ignore arg))
	   (unless (typep constant '(or complex real))
	     (error "~A must be a complex number" constant)))
  (:method ((poly ca+polynomial) (arg number) &optional (constant 0))
	   (when (> (length (ca=get-poly-data poly)) 1)
	     (ca~output-method "integration of a sum~%"))
	   (when (null (ca=get-poly-data poly))
	     (ca~output-method "integration of zero~%"))
	   (ca~create-polynomial
	    (ca=integrate-polynomial-rec (ca=get-poly-data poly) arg constant)
	    :field (ca~get-poly-field poly)
	    :var (ca=get-poly-var-number poly))))


(defun ca=integrate-polynomial-rec (data arg constant)
  (declare (edited  "19-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial data-list, an argument number and a constant")
	   (effect  "None")
	   (value   "A list containing the 'integrated data'"))
  (cond ((null data) (when (not (= constant 0))
		       (list (list constant))))
	(t (ca~output-method "integration of a monomial~%")
	   (append
	    (list (append
		   (list (/ (caar data) (1+ (nth arg (car data)))))
		   (butlast (cdar data) (- (length (car data)) arg))
		   (list (1+ (nth arg (car data))))
		   (nthcdr (1+ arg) (car data))))
	    (ca=integrate-polynomial-rec (cdr data) arg constant)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation and substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defgeneric ca~polynomial-let (poly values &optional variables)
  (declare (edited  "11-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial, a list of values or a single value and optional a list of variables")
	   (effect  "None")
	   (value   "A polynomial with the values substituted for the variables"))
  (:documentation "This function allows you to set variables in the polynomial to some value. The entered values should match the field of the polynomial. You can enter both a list of values and a list of variables. If only a list of values is entered, each value is bound to consecutive variables, beginning with the first. The list of variables specifies the "position" of each variable in the polynomial-ring (for example enter 2 to bind x2 in R[x1,x2,x3]). Thus the first value is bound to the first specified position and so on. If the value-list is larger as the variable-list the remaining values are bound in successive order after the last specified position.")
  (:method (poly values &optional (variables 1))
	   (declare (ignore values variables))
	   (error "~A must be a polynomial" poly))

  (:method ((poly ca+polynomial) values &optional (variables 1))
	   (declare (ignore variables))
	   (error "~A is not of the right type" values))
  
  (:method ((poly ca+polynomial) (values cons) &optional (variables 1))
	   (cond ((numberp variables) (ca~polynomial-let poly values (list variables)))
		 (t
		  (when (> (length values) (length variables))
		    (do ((i (1+ (car (last variables))) (1+ i)))
			((or (> i (ca=get-poly-var-number poly))
			     (= (length variables) (length values))))
		      (nconc variables (list i))))
		  (do ((i 0 (1+ i)))
		      ((or (null (nth i variables))
			   (null (nth i values))))
		    (setf poly (ca~polynomial-let poly (nth i values) (nth i variables)))
		    (setf variables (mapcar #'(lambda (z)
						(if (> z (nth i variables))
						    (1- z)
						  z))
					    variables)))
		  poly)))

  (:method :around ((poly ca+polynomial) (values number) &optional (variables 1))
	   (if (consp variables)
	       (call-next-method poly values (car variables))
	     (call-next-method poly values variables)))

  (:method :before ((poly ca+polynomial) values &optional (variables 1))
	   (declare (ignore values))
	   (when (and (consp variables)
		      (notevery #'(lambda (x) (funcall #'typep x (list 'integer '1 (ca=get-poly-var-number poly))))
				variables))
	     (error "In ~A are illegal variable numbers for ~A" variables poly))
	   (when (and (numberp variables)
		      (not (funcall #'typep variables (list 'integer '1 (ca=get-poly-var-number poly)))))
	     (error "~D is an illegal variable number for ~A" variables poly)))
  
  (:method ((poly ca+nat-polynomial) (values number) &optional (variables 1))
	   (if (typep values '(integer 0))
	       (ca~create-polynomial
		(ca=polynomial-let-rec (ca=get-poly-data poly) values variables)
		:field ca*natural-character
		:var (1- (ca=get-poly-var-number poly)))
	     (error "~D is not a positive integer" values)))
  (:method ((poly ca+int-polynomial) (values integer) &optional (variables 1))
	   (ca~create-polynomial
	    (ca=polynomial-let-rec (ca=get-poly-data poly) values variables)
	    :field ca*integer-character
	    :var (1- (ca=get-poly-var-number poly))))
  (:method ((poly ca+rat-polynomial) (values rational) &optional (variables 1))
	   (ca~create-polynomial
	    (ca=polynomial-let-rec (ca=get-poly-data poly) values variables)
	    :field ca*rational-character
	    :var (1- (ca=get-poly-var-number poly))))
  (:method ((poly ca+real-polynomial) (values real) &optional (variables 1))
	   (ca~create-polynomial
	    (ca=polynomial-let-rec (ca=get-poly-data poly) values variables)
	    :field ca*real-character
	    :var (1- (ca=get-poly-var-number poly))))
  (:method ((poly ca+complex-polynomial) (values number) &optional (variables 1))
	   (if (typep values '(or real complex))
	       (ca~create-polynomial
		(ca=polynomial-let-rec (ca=get-poly-data poly) values variables)
		:field ca*complex-character
		:var (1- (ca=get-poly-var-number poly)))
	     (error "~D is not a complex number" values))))


(defun ca=polynomial-let-rec (data value variable)
  (declare (edited  "12-APR-1995")
	   (authors Sorge)
	   (input   "A list of polynomial data, a value and a variable number to substitute")
	   (effect  "None")
	   (value   "The modified polynomial-data-list"))
  (when data (append
	      (list (append
		     (list (* (caar data) (expt value (nth variable (car data)))))
		     (butlast (cdar data) (- (length (car data)) variable))
		     (nthcdr (1+ variable) (car data))))
	      (ca=polynomial-let-rec (cdr data) value variable))))




;; ------ CAUTION! CONSTRUCTION AHEAD ------

(defgeneric ca~polynomial-roots (poly)
  (declare (edited  "20-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial")
	   (effect  "None")
	   (value   "A list of the primitive roots of thata polynomial"))
  (:method (poly)
	   (declare (ignore poly))
	   (error "~A must be a polynomial"))
  (:method ((poly ca+polynomial))
	   (when (> (ca=get-poly-var-number poly) 1)
	     (error "I was too lazy to implement this for more than one variables"))
	   (ca=get-roots (ca=get-poly-data poly))))

(defun ca=get-roots (data)
  (declare (edited  "20-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial data-list")
	   (effect  "None")
	   (value   "A list of roots"))
  (cond ((= (length data) 0) nil)
	((= (car (ca=exps (car data))) 0) nil)
	((= (car (ca=exps (car data))) 1)
	 (if (> (length data) 1) (list (/ (* -1 (ca=coeff (cadr data))) (ca=coeff (car data))))
	   (list 0)))
	((= (length data) 1)
	 (make-list (car (ca=exps (car data))) :initial-element 0))
	((= (car (ca=exps (car data))) 2)
	 (ca=compute-quadratic-formula data))
	((not (= (car (ca=exps (car (last data)))) 0))
	 (append (list 0)
		 (ca=get-roots (ca=divide-by-root data 0))))
	(t (let* ((h1 (ca=compute-newton-iteration data))
		  (h2 (ca=divide-by-root data h1)))
	     (append (list h1)
		     (ca=get-roots h2))))))


(defun ca=compute-quadratic-formula (data)
  (declare (edited  "20-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial data-list of a quadratic polynomial")
	   (effect  "None")
	   (value   "A list of two roots"))
  (let* ((a (ca=coeff (first data)))
	 (b (if (= (car (ca=exps (second data))) 1)
		(ca=coeff (second data))
	      0))
	 (ch (if (= b 0) (ca=coeff (second data))
		  (ca=coeff (third data))))
	 (c (if ch ch
	      0)))
    (append
     (list
      (/ (+ (* -1 b)
	    (sqrt (- (* b b) (* 4 a c))))
	 (* 2 a)))
     (list
      (/ (- (* -1 b)
	    (sqrt (- (* b b) (* 4 a c))))
	 (* 2 a)))
     )))


(defun ca=monomial-by-x (monomial)
  (declare (edited  "21-APR-1995")
	   (authors Sorge)
	   (input   "A monomial over x")
	   (effect  "None")
	   (value   "The monomial divided once by x"))
   (append
    (list (ca=coeff monomial))
    (list (1- (car (ca=exps monomial))))))
  


(defun ca=divide-by-root (data root)
  (declare (edited  "20-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial data-list and a root of that polynomial")
	   (effect  "None")
	   (value   "The polynomial data-list divided by (x - root)"))
  (cond ((null data) data)
	((= root 0) (mapcar #'ca=monomial-by-x data))
	((<= (length data) 1) nil)
	(t (append
	    (list (ca=monomial-by-x (car data)))
	    (ca=divide-by-root                           ;divide the rest of
	     (ca=simplify-on-zeros!
	      (ca=polynomial-subtraction-rec              ; data - ((car data)/x) * (x - root)
	       data
	       (ca=polynomial-multiplication-rec
		(append '((1 1))
			(list
			 (append (list (* -1 root))
				 '(0))))
		(list (ca=monomial-by-x (car data))))))
	     root)))))

								     
	 
(defun ca=compute-newton-iteration (data)
  (declare (edited  "20-APR-1995")
	   (authors Sorge)
	   (input   "A polynomial data-list")
	   (effect  "None")
	   (value   "A root of that polynomial"))
  (let ((prime (ca=differentiate-polynomial-rec data 1))
	(x (ca=coeff (car (last data)))))
    (loop (let ((a (ca=simplify-non-poly (ca=polynomial-let-rec data x 1)))
		(b (ca=simplify-non-poly (ca=polynomial-let-rec prime x 1))))
	    (cond ((= a 0) (return x))                                             ; YAAHOOO! We've got a root
		  ((not (= b 0))                                                   ; no root yet...
		   (let ((y (- x (/ a b))))				
		     (cond ((< (abs (- x y)) 1d-16)                                ; we're pretty close
			    (return y))
			   ((complexp y)                                           ; we're in the complex
			    (format t "complex  ")
			    (setf x
				  (complex (ca=manipulate-root (coerce (realpart y) 'double-float))
					   (ca=manipulate-root (coerce (imagpart y) 'double-float)))))
			   (t                                                      ; a real number
			    (format t "real ~D  " y)
			    (setf x (ca=manipulate-root (/ (coerce (numerator y) 'double-float)
							   (coerce (denominator y) 'double-float))))))))
		  (t (setf x (1+ x))))))))                                         ; parachute into a new iteration


(defun ca=manipulate-root (y)
  (declare (edited  "21-APR-1995")
	   (authors Sorge)
	   (input   "An iterated 'root'")
	   (effect  "None")
	   (value   "A number that might be closer to the actual root"))
  (cond ((< (abs (- (round y) y)) 1e-7)                         ; we're close to some integer
	 (round y))
	((= (- y (rationalize y)) 0)                            ; we're close to some rational
	 (rationalize y))
	(t y)))
;; ------ END OF CONSTRUCTION ------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following stuff should be more elaborated....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ca~minimum (poly)
  (declare (edited  "03-JUL-1997 22:11")
	   (authors SORGE)
	   (input   "A real polynomial")
	   (effect  "None.")
	   (value   "Computes the minima of the polynomial."))
  (:method (poly)
	   (error "~A must be a polynomial" poly))
  (:method ((poly ca+polynomial))
	   (error "Cannot compute the minima of a polynomial with coefficients in " (ca~get-poly-field poly)))
  (:method ((poly ca+real-polynomial))
	   (let* ((first-deriv (ca~differentiate-polynomial poly 1))
		  (second-deriv (ca~differentiate-polynomial first-deriv 1))
		  (first-roots (ca~polynomial-roots first-deriv)))
	     (remove-if #'null
			(mapcar #'(lambda (x)
				    (when (> (ca~polynomial-let second-deriv x) 0) x))
				first-roots)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Input/Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ca~read-polynomial (file)
  (declare (edited  "04-MAY-1995")
	   (authors Sorge)
	   (input   "A filename")
	   (effect  "Reads a list of polynomials from file")
	   (value   "A list of polynomials"))
  (when (open file :if-does-not-exist nil)
    (with-open-file (in file :direction :input)
		    (ca=read-polynomial in))))


(defun ca=read-polynomial (stream)
  (declare (edited  "05-MAY-1995")
	   (authors Sorge)
	   (input   "An open file stream")
	   (effect  "Creates polynomials read from stream")
	   (value   "A list of the created polynomials"))
  (let ((char (read stream nil))
	(var  (read stream nil))
	(data (read stream nil)))
    (when (and char var)
      (append (list (ca~create-polynomial data :var var :field char))
	      (ca=read-polynomial stream)))))


(defgeneric ca~write-polynomial (file poly &key supersede)
  (declare (edited  "04-MAY-1995")
	   (authors Sorge)
	   (input   "A filename, a polynomial and a flag")
	   (effect  "Creates file if it doesn't exist, supersedes file if the flag is set otherwise appends the data")
	   (value   "T if nothing strange happened o/w nil"))
  (:method (file poly &key supersede)
	   (declare (ignore supersede))
	   (error "filename ~A must be a string and ~A a polynomial" file poly))
  (:method (file (poly ca+polynomial) &key supersede)
	   (declare (ignore supersede))
	   (error "filename ~A must be a string" file))
  (:method ((file string) poly &key supersede)
	   (declare (ignore supersede))
	   (error "~A must be a polynomial" poly))
  (:method ((file string) (poly ca+polynomial) &key (supersede nil))
	   (when (probe-file (directory-namestring file))
	     (when supersede
	       (open file :direction :output :if-exists :supersede :if-does-not-exist  :create))
	     (ca=write-polynomial file poly))))


(defgeneric ca=write-polynomial (file poly)
  (declare (edited  "05-MAY-1995")
	   (authors Sorge)
	   (input   "A filename and a polynomial")
	   (effect  "Writes the polynomial to the file")
	   (value   "None"))
  (:method (file (poly ca+nat-polynomial))
	   (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create)
			   (write ca*natural-character :stream out)
			   (format out "~%")
			   (ca=write-poly-to-file out poly)))
  (:method (file (poly ca+int-polynomial))
	   (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create)
			   (write ca*integer-character :stream out)
			   (format out "~%")
			   (ca=write-poly-to-file out poly)))
  (:method (file (poly ca+rat-polynomial))
	   (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create)
			   (write ca*rational-character :stream out)
			   (format out "~%")
			   (ca=write-poly-to-file out poly)))
  (:method (file (poly ca+real-polynomial))
	   (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create)
			   (write ca*real-character :stream out)
			   (format out "~%")
			   (ca=write-poly-to-file out poly)))
  (:method (file (poly ca+complex-polynomial))
	   (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create)
			   (write ca*complex-character :stream out)
			   (format out "~%")
			   (ca=write-poly-to-file out poly))))


(defun ca=write-poly-to-file (stream poly)
  (declare (edited  "05-MAY-1995")
	   (authors Sorge)
	   (input   "An open file and a polynomial")
	   (effect  "Writes the var-number and the data-list of poly to the stream")
	   (value   "T"))
  (write (ca=get-poly-var-number poly) :stream stream)
  (format stream "~%")
  (write (ca=get-poly-data poly) :stream stream)
  (format stream "~%")
  t)
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Everything for the beauty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod print-object :before ((obj ca+nat-polynomial) stream)
  (ca=print-with-indexed-variables obj stream "N"))
  
(defmethod print-object :before ((obj ca+int-polynomial) stream)
  (ca=print-with-indexed-variables obj stream "Z"))
  
(defmethod print-object :before ((obj ca+rat-polynomial) stream)
  (ca=print-with-indexed-variables obj stream "Q"))
  
(defmethod print-object :before ((obj ca+real-polynomial) stream)
  (ca=print-with-indexed-variables obj stream "R"))
  
(defmethod print-object :before ((obj ca+complex-polynomial) stream)
  (ca=print-with-indexed-variables obj stream "C"))
  
(defmethod print-object ((obj ca+polynomial) stream)
  (ca=print-polynomial (ca=get-poly-data obj) stream))

(defun ca=print-polynomial (list stream)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A list of polynomial-data")
	   (effect  "Prints the data in 'nice' form")
	   (value   "None"))
  (cond (list
	 (ca=print-monomial (car list) stream)
	 (mapcar #'(lambda (x)
		     (format stream "+ ")
		     (ca=print-monomial x stream))
		 (cdr list)))
	(t (format stream "0"))))

(defun ca=print-monomial (list stream)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A list of monomial-data")
	   (effect  "Prints the data in 'nice' form")
	   (value   "None"))
  (when (or (not (= (car list) 1)) (every #'(lambda (x) (= x 0)) (cdr list)))
    (format stream "~D " (car list)))
  (do ((i 1 (1+ i))
       (x (cdr list) (cdr x)))
      ((null x))
    (cond ((= (car x) 1) (format stream "x~D " i))
	  ((= (car x) 0) )
	  (t (format stream "x~D^~D " i (car x))))))
    
(defun ca=print-with-indexed-variables (obj stream char)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A number")
	   (effect  "Prints variables (x) indexed from 1 to number, separated by kommata")
	   (value   "None"))
  (format stream "Polynomial in ~A[" char)
  (do ((i 1 (1+ i)))
      ((> i (ca=get-poly-var-number obj)))
    (if (= 1 i) (format stream "x~D" i)
      (format stream ",x~D" i)))
  (format stream "]:  "))


(eval-when (load)
    (format t "~%Welcome to myCAS~%")
    (format t "================~%"))


(defun ca~output-method (string &optional args &key (force nil)) 
  (declare (edited  "29-MAR-1995")
	   (authors Sorge)
	   (input   "A string and a switch to force output of methods")
	   (effect  "Tells me when a logical method should be induced")
	   (value   "None"))
  (when (or ca*verbose force)
    (if args
	(push (mapcar #'(lambda (assoc)
			  (cons (car assoc)
				(cons (cdr assoc) args)))
		      string)
	      ca*computed-methods)
      (push string ca*computed-methods))
    (values)))

(defun ca=replace-output-method (old-tac new-tac)
  (nsubstitute new-tac old-tac ca*computed-methods :test #'equal :count 1))

(defun ca=remove-output-method (old-tac)
  (setq ca*computed-methods (remove old-tac ca*computed-methods :test #'equal :count 1)))

(defun ca~computed-methods ()
  (declare (edited  "02-JUL-1997 19:35")
	   (authors SORGE)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The list of computed methods."))
  (if (or (eq ca*verbose t)
	  (null ca*verbose))
      (reverse ca*computed-methods)
    (mapcar #'(lambda (x)
		(cdr (assoc ca*verbose x :test #'string-equal)))
	    (reverse ca*computed-methods))))
