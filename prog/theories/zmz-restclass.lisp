;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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

(mod~defmod RCL 
            :uses (atptop data env gap keim maple ohlp omega opr pds post serv tac term)
            :documentation "Functions for restclass sets"
            :exports (rcl+multiplication-table
                      rcl+multiplication-table
                      rcl+product-multiplication-table
                      
                      rcl~call-gap
                      rcl~call-maple
                      rcl~check-associativity
                      rcl~check-closed-homomorphism
                      rcl~check-closure
                      rcl~check-commutativity
                      rcl~check-distributivity
                      rcl~check-divisors
                      rcl~check-inverses
                      rcl~check-isomorphism
                      rcl~check-non-injectivity
                      rcl~check-non-isomorphism
                      rcl~check-non-isomorphism-special
                      rcl~check-unit-element
                      rcl~element-orders
                      rcl~elements
                      rcl~gap-associative?
                      rcl~gap-commutative?
                      rcl~gap-element-order
                      rcl~gap-element-power
                      rcl~gap-generated-substructure
                      rcl~gap-inverse-element
                      rcl~gap-neutral-element?
                      rcl~generated-substructures
                      rcl~get-gap-table
                      rcl~maple2post
                      rcl~modulo
                      rcl~multiplication-table
                      rcl~operation
                      rcl~post2maple
                      rcl~print-multiplication-table
                      rcl~product-multiplication-table
                      rcl~single-tables
                      rcl~solve-equation-with-maple
                      rcl~table
                      rcl~table-pair2divisors
                      rcl~table-pair2result
                      rcl~table2gap
                      rcl~translate-object
                      rcl~zero
                      
                      rcl*check-line
                      rcl*gap-objects))

(defvar rcl*check-line nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rcl+multiplication-table (keim+object)
  ((elements :accessor rcl~elements
	     :initarg :elements
	     :initform nil
	     :documentation "The elements of the set.")
   (zero :accessor rcl~zero
	 :initarg :zero
	 :initform nil
	 :documentation "Does there exist a zero element in the set.")
   (modulo :accessor rcl~modulo
	   :initarg :modulo
	   :initform nil
	   :documentation "The number modulo which the operation is executed.")
   (table :accessor rcl~table
	  :initarg :table
	  :initform nil
	  :documentation "The actual multiplcation table represented as a list of lists. Each component list corresponds to row in the multiplication table.")
   (operation :accessor rcl~operation
	      :initarg :operation
	      :initform nil
	      :documentation "The operation on the set."))
  (:documentation "The class of multiplcation tables of residue classes."))
   
(defclass rcl+product-multiplication-table (rcl+multiplication-table)
  ((single-tables :accessor rcl~single-tables
		  :initarg :single-tables
		  :initform nil
		  :documentation "A list of individual multiplication tables of the sets composing the cartesian product."))
  (:documentation "The class of multiplicaiton tables of cartesian products of residue classes."))


(defclass rcl+scalar-multiplication-table (keim+object)
  ((scalar-elements :accessor rcl~scalar-elements
		    :initarg :scalar-elements
		    :initform nil
		    :documentation "The scalar elements of the table.")
   (elements :accessor rcl~elements
	     :initarg :elements
	     :initform nil
	     :documentation "The elements of the table.")
   (modulo :accessor rcl~modulo
	   :initarg :modulo
	   :initform nil
	   :documentation "The number modulo which the operation is executed.")
   (table :accessor rcl~table
	  :initarg :table
	  :initform nil
	  :documentation "The actual multiplcation table represented as a list of lists. Each component list corresponds to row in the multiplication table.")
   (operation :accessor rcl~operation
	      :initarg :operation
	      :initform nil
	      :documentation "The operation between the scalars and the elements."))
  (:documentation "The class of scalar multiplcation tables of residue classes."))

(defclass rcl+product-scalar-multiplication-table (rcl+scalar-multiplication-table)
  ((single-tables :accessor rcl~single-tables
		  :initarg :single-tables
		  :initform nil
		  :documentation "A list of individual multiplication tables of the sets composing the cartesian product."))
  (:documentation "The class of scalar multiplicaiton tables of cartesian products of residue classes."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rcl~multiplication-table (elements modulo &key (operation #'+) (zero nil))
  (declare (edited  "09-MAR-2000")
	   (authors Sorge)
	   (input   "A number or a set of numbers, a number as modulo factor for the operation,"
		    "and optionally keys indicating whether there is a zero element in the set"
		    "and a binary operation.")
	   (effect  "Creates and instance of RCL+MULTIPLICATION-TABLE.")
	   (value   "The newly created object."))
  (:method ((numbers list) (modulo term+number) &key (operation #'+) (zero nil))
	   (rcl~multiplication-table numbers (rcl~translate-object modulo) operation zero))
  (:method ((numbers list) (modulo integer) &key (operation #'+) (zero nil))
	   (declare (ignore zero))
	   (let* ((elements (sort (remove-duplicates (mapcar #'rcl~translate-object numbers)) #'<))
		  (zero (find 0 elements :test #'=))
		  (operation (rcl~translate-object operation)))
	     (make-instance 'rcl+multiplication-table
			    :elements elements
			    :zero zero
			    :modulo (abs modulo)
			    :table (rcl=make-table modulo elements operation)
			    :operation operation)))
  (:method ((number term+number) (modulo term+number) &key (operation #'+) (zero nil))
	   (rcl~multiplication-table (rcl~translate-object number) (rcl~translate-object modulo)
				     operation zero))
  (:method ((number integer) (modulo term+number) &key (operation #'+) (zero nil))
	   (rcl~multiplication-table number (rcl~translate-object modulo) operation zero))
  (:method ((number term+number) (modulo integer) &key (operation #'+) (zero nil))
	   (rcl~multiplication-table (rcl~translate-object number) modulo operation zero))
  (:method ((number integer) (modulo integer) &key (operation #'+) (zero nil))
	   (let ((elements (rcl=make-element-list number zero))
		 (operation (rcl~translate-object operation)))
	     (make-instance 'rcl+multiplication-table
			    :elements elements
			    :zero zero
			    :modulo (abs modulo)
			    :table (rcl=make-table modulo elements operation)
			    :operation operation)))
  (:method ((table rcl+multiplication-table) modulo &key operation zero)
	   (declare (ignore modulo operation zero))
	   table)
  )


(defgeneric rcl~scalar-multiplication-table (scalar-elements elements modulo &key (operation #'+))
  (declare (edited  "03-JUNE-2003")
	   (authors AMeier)
	   (input   "A number or a set of numbers, a number or a set of numbers, a number as modulo"
		    "factor for the operation, and optionally a key indicating a binary operation.")
	   (effect  "Creates and instance of RCL+SCALAR-MULTIPLICATION-TABLE.")
	   (value   "The newly created object."))
  (:method ((scalar-numbers list) (numbers list) (modulo term+number) &key (operation #'+))
	   (rcl~scalar-multiplication-table scalar-numbers numbers (rcl~translate-object modulo) operation))
  (:method ((scalar-numbers list) (numbers list) (modulo integer) &key (operation #'+))
	   (let* ((scalar-elements (sort (remove-duplicates (mapcar #'rcl~translate-object scalar-numbers)) #'<))
		  (elements (sort (remove-duplicates (mapcar #'rcl~translate-object numbers)) #'<))
		  (operation (rcl~translate-object operation)))
	     (make-instance 'rcl+scalar-multiplication-table
			    :scalar-elements scalar-elements
			    :elements elements
			    :modulo (abs modulo)
			    :table (rcl=make-scalar-table modulo scalar-elements elements operation)
			    :operation operation)))
  (:method ((scalar-number term+number) (number term+number) (modulo term+number) &key (operation #'+))
	   (rcl~scalar-multiplication-table (rcl~translate-object scalar-number) (rcl~translate-object number)
					    (rcl~translate-object modulo) :operation operation))
  (:method ((scalar-number integer) (number integer) (modulo term+number) &key (operation #'+))
	   (rcl~scalar-multiplication-table scalar-number number (rcl~translate-object modulo)
					    :operation operation))
  (:method ((scalar-number integer) (number integer) (modulo integer) &key (operation #'+))
	   (rcl~scalar-multiplication-table (rcl=make-element-list scalar-number T)
					    (rcl=make-element-list number T)
					    modulo
					    :operation operation))
  (:method ((scalar-numbers list) (number integer) (modulo integer) &key (operation #'+))
	   (rcl~scalar-multiplication-table scalar-numbers
					    (rcl=make-element-list number T)
					    modulo
					    :operation operation)) 
  )



(defun rcl~product-multiplication-table (elements &key
						  (modulo (make-list (length elements) :initial-element 1))
						  (operation (make-list (length elements) :initial-element #'+))
						  (zero (make-list (length elements) :initial-element t)))
  (declare (edited  "18-JUL-2000")
	   (authors Sorge)
	   (input   "Four lists that can individually form residue class sets.")
	   (effect  "An object of type RCL+PRODUCT-MULTIPLICATION-TABLE.")
	   (value   "The newly created object."))
  (let* ((tables (mapcar #'(lambda (el mod op z) (rcl~multiplication-table el mod :operation op :zero z))
			 elements modulo operation zero))
	 (elements (rcl=make-product-elements (mapcar #'rcl~elements tables)))
	 (operation (mapcar #'rcl~operation tables))
	 (zeros (mapcar #'rcl~zero tables))
	 (modulo (mapcar #'rcl~modulo tables))
	 (table (rcl=make-product-table elements operation modulo)))
    (make-instance 'rcl+product-multiplication-table
		   :elements elements
		   :zero zeros
		   :modulo modulo
		   :table table
		   :operation operation
		   :single-tables tables)))



(defgeneric rcl~product-scalar-multiplication-table (scalar-elements
						     elements
						     &key
						     (modulo (make-list (length elements) :initial-element 1))
						     (operation (make-list (length elements) :initial-element #'+)))
  (declare (edited  "02-JUNE-2003")
	   (authors AMeier)
	   (input   "The scalar-elemenst and four lists that can individually form residue class sets.")
	   (effect  "An object of type RCL+PRODUCT-SCALAR-MULTIPLICATION-TABLE.")
	   (value   "The newly created object."))
  (:method ((scalar-elements list) elements &key (modulo (make-list (length elements) :initial-element 1))
	                                         (operation (make-list (length elements) :initial-element #'+)))
	   (let* ((tables (mapcar #'(lambda (el mod op)
				      (rcl~scalar-multiplication-table scalar-elements el mod :operation op))
				  elements modulo operation))
		  (elements (rcl=make-product-elements (mapcar #'rcl~elements tables)))
		  (operation (mapcar #'rcl~operation tables))
		  (modulo (mapcar #'rcl~modulo tables))
		  (table (rcl=make-scalar-product-table scalar-elements elements operation modulo)))
	     (make-instance 'rcl+product-scalar-multiplication-table
			    :scalar-elements (rcl~scalar-elements (first tables))
			    :elements elements
			    :modulo modulo
			    :table table
			    :operation operation
			    :single-tables tables)))
  (:method ((scalar-number integer) elements &key (modulo (make-list (length elements) :initial-element 1))
	                                          (operation (make-list (length elements) :initial-element #'+)))
	   (rcl~product-scalar-multiplication-table (rcl=make-element-list scalar-number T)
						    elements
						    :modulo modulo
						    :operation operation)))




;;; Making tables

(defun rcl=make-product-table (elements operation modulo)
  (declare (edited  "18-JUL-2000")
	   (authors Sorge)
	   (input   "A list of element tuples, a list of operations, and a list of modulo factors.")
	   (effect  "None.")
	   (value   "A multiplication table for these elements with respect to the given operations."))
  (mapcar #'(lambda (element)
	      (mapcar #'(lambda (x)
			  (rcl=apply-operation (list element x) operation modulo))
		      elements))
	  elements))


(defun rcl=make-scalar-product-table (scalar-elements elements operation modulo)
  (mapcar #'(lambda (scalar-element)
	      (mapcar #'(lambda (x)
			  (rcl=apply-scalar-operation scalar-element x operation modulo))
		      elements))
	  scalar-elements))


(defun rcl=make-table (number elements operation)
  (mapcar #'(lambda (element)
	      (mapcar #'(lambda (x) (mod (funcall operation element x) (abs number)))
		      elements))
	  elements))

(defun rcl=make-scalar-table (number scalar-elements elements operation)
  (mapcar #'(lambda (scalar-element)
	      (mapcar #'(lambda (x)
			  (mod (funcall operation scalar-element x) (abs number)))
		      elements))
	  scalar-elements))

;;; Making elements

(defgeneric rcl=apply-operation (pair operation &optional modulo)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "A pair of elements, an operation, and optionally an integer considered as modulo factor.")
	   (effect  "None.")
	   (value   "The result of applying the given operation to the pair of elements."))
  (:method ((pair list) (operation list) &optional modulo)
	   (mapcar #'(lambda (a1 a2 a3 a4)
		       (rcl=apply-operation (list a1 a2) a3 a4))
		   (first pair) (second pair) operation modulo))
  (:method ((pair list) operation &optional modulo)
	   (if modulo
	       (mod (apply operation pair) modulo)
	     (apply operation pair))))

(defgeneric rcl=apply-scalar-operation (scalar-element element operation modulo)
  (:method (scalar-element (pair list) (operation list) modulo)
	   (mapcar #'(lambda (elem op mod)
		       (mod (funcall op scalar-element elem) (abs mod)))
		   pair operation modulo)))
		   

(defun rcl=make-product-elements (elements)
  (declare (edited  "18-JUL-2000")
	   (authors Sorge)
	   (input   "A list of element lists.")
	   (effect  "None.")
	   (value   "A list of elements of the cartesian product of the given sets of elements."))
  (if (> (length elements) 1)
      (labels ((permute-elements (elem1 elem2)
				 (when elem1
				   (append (mapcar #'(lambda (x) (cons (car elem1) x)) elem2)
					   (permute-elements (cdr elem1) elem2))))
	       (permute-element-lists (elem-list)
				      (cond ((cdr elem-list)
					     (permute-elements (car elem-list)
							       (permute-element-lists (cdr elem-list))))
					    (elem-list (mapcar #'list (car elem-list))))))
	(permute-element-lists elements))
    (mapcar #'list (car elements))))

(defun rcl=make-element-list (number &optional (zero nil))
  (declare (edited  "18-FEB-2000")
	   (authors Sorge)
	   (input   "A number and a boolean.")
	   (effect  "None.")
	   (value   "A list containing all elements of an integer residue class."
		    "If zero is non-nil the residue class is considered to include 0."))
  (flet ((make-element-list (from to)
			    (do* ((i from (1+ i))
				  (list (cons i nil) (cons i list)))
				((>= i to) (reverse list)))))
    (cond ((= number 0)                               nil)
	  ((and (or (= number -1) (= number 1)) zero) '(0))
	  ((or (= number 1) (= number -1))            nil)
	  ((and zero (< number 0))                   (make-element-list 0 (1- (abs number))))
	  ((< number 0)                              (make-element-list 1 (1- (abs number))))
	  (zero   	                             (make-element-list 0 (1- number)))
	  (t                    	             (make-element-list 1 (1- number))))))

;;; Creating pairs

(defun rcl=permute2pairs (elements)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "A list of elements.")
	   (effect  "None.")
	   (value   "A list of all possible distinct ordered pairs of the elements."))
  (mapcan #'(lambda (x)
	      (mapcar #'(lambda (y) (list x y)) elements))
	  elements))

;;; Stuff for tuples

(defun rcl=make-tuple-from-elements (tuple elements)
  (declare (edited  "18-JUL-2000")
	   (authors Sorge)
	   (input   "A list containing numbers or NIL and a list of elements.")
	   (effect  "None.")
	   (value   "A completed tuple."))
  (mapcar #'(lambda (el list) (if el el (car list)))
	  tuple elements))

(defun rcl=make-tuples-from-elements (tuples elements)
  (declare (edited  "18-JUL-2000")
	   (authors Sorge)
	   (input   "A list of (possibly incomplete) lists that are to be arranged as tuples"
		    "and a list of elements.")
	   (effect  "None.")
	   (value   "A list of completed tuples."))
  (let ((length (apply #'max (mapcar #'length tuples))))
    (do* ((count length (1- count))
	  (tuple (mapcar #'car tuples) (mapcar #'car rest-tuples))
	  (rest-tuples (mapcar #'cdr tuples) (mapcar #'cdr rest-tuples))
	  (result (list (rcl=make-tuple-from-elements tuple elements))
		  (append result (list (rcl=make-tuple-from-elements tuple elements)))))
	((= count 1) result))))
    
(defun rcl=make-tuple-lists-from-elements (tuple-lists elements)
  (declare (edited  "18-JUL-2000")
	   (authors Sorge)
	   (input   "A list of (possibly incomplete) lists that are to be arranged as tuple-lists"
		    "and a list of elements.")
	   (effect  "None.")
	   (value   "A list of completed tuple-lists."))
  (let* ((tuple-lists (mapcar #'(lambda (tl) (when (listp tl) tl)) tuple-lists))
	 (length (apply #'max (mapcar #'length tuple-lists))))
    (do* ((count length (1- count))
	  (tuple (mapcar #'car tuple-lists) (mapcar #'car rest-tuples))
	  (rest-tuples (mapcar #'cdr tuple-lists) (mapcar #'cdr rest-tuples))
	  (result (list (rcl=make-tuples-from-elements tuple elements))
		  (append result (list (rcl=make-tuples-from-elements tuple elements)))))
	((= count 1) result))))
    
(defun rcl=lists2tuples (&rest lists)
  (declare (edited  "19-JUL-2000")
	   (authors Sorge)
	   (input   "Lists of equal length.")
	   (effect  "None.")
	   (value   "A list of tuples consisting of elements of the different lists."))
  (when (notany #'null lists)
    (cons
     (mapcar #'car lists)
     (apply #'rcl=lists2tuples (mapcar #'cdr lists)))))
	 
(defun rcl=lists2tuple-lists (lists)
  (declare (edited  "19-JUL-2000")
	   (authors Sorge)
	   (input   "Lists of lists containing lists of pairs.")
	   (effect  "None.")
	   (value   "A list of tuples consisting of elements of the different lists."))
  (if (cdr lists)
      (append (rcl=pair2tuples-pairs (car lists) (rcl=lists2tuple-lists (cdr lists))))
    (mapcar #'(lambda (x) (list (list (car x)) (list (cadr x)))) (car lists))))

(defun rcl=pair2tuples-pairs (list rest)
  (when list
    (append (mapcar #'(lambda (x) (list (cons (caar list) (car x))
					(cons (cadar list) (cadr x))))
		    rest)
	    (rcl=pair2tuples-pairs (cdr list) rest))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some special accessor functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rcl~table-pair2result (table pair)
  (declare (edited  "10-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table and a pair of elements.")
	   (effect  "None.")
	   (value   "The result of multiplying the elements read from the table. In case one of the elements"
		    "not in the set, NIL is returned."))
  (:method ((table rcl+multiplication-table) (pair cons))
	   (rcl=element-pair2result-in-table
	    (rcl~elements table) (rcl~table table) (first pair) (second pair)))
  (:method ((table rcl+product-multiplication-table) (pair cons))
	   (rcl=element-pair2result-in-table
	    (rcl~elements table) (rcl~table table) (first pair) (second pair))))
  
(defun rcl=element-pair2result-in-table (elements table first-element second-element)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "A list of elements, a list of lists representing a multiplication table,"
		    "and two single elements.")
	   (effect  "None.")
	   (value   "The result of multiplying the two elements as given in the table. In case one"
		    "of the elements not in the set, NIL is returned."))
  (let ((pos1 (position first-element elements))
	(pos2 (position second-element elements)))
    (when (and pos1 pos2)
      (nth pos2 (nth pos1 table)))))
 

(defgeneric rcl~table-pair2divisors (table pair)
  (declare (edited  "10-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table and a pair of elements a and b.")
	   (effect  "None.")
	   (value   "The divisors for the two elements, i.e., x and y such that a*x=b and y*a=b."
		    "If no divisors exist, NIL is returned."))
  (:method ((table rcl+multiplication-table) (pair cons))
	   (let* ((elements (rcl~elements table))
		  (rows (rcl~table table))
		  (columns (rcl=rows2columns rows))
		  (fpos (position (car pair) elements))
		  (selem (cadr pair))
		  (posA (when fpos (position selem (nth fpos rows))))
		  (posB (when fpos (position selem (nth fpos columns)))))
	     (cond ((and posA posB)
		    (list (nth posA elements) (nth posB elements)))
		   (posA (list (nth posA elements) nil))
		   (posB (list nil (nth posB elements)))
		   (t (list nil nil)))))
  (:method ((table rcl+product-multiplication-table) (pair cons))
	   (let* ((elements (rcl~elements table))
		  (rows (rcl~table table))
		  (columns (rcl=rows2columns rows))
		  (fpos (position (car pair) elements :test #'equal))
		  (selem (cadr pair))
		  (posA (when fpos (position selem (nth fpos rows) :test #'equal)))
		  (posB (when fpos (position selem (nth fpos columns) :test #'equal))))
	     (cond ((and posA posB)
		    (list (nth posA elements) (nth posB elements)))
		   (posA (list (nth posA elements) nil))
		   (posB (list nil (nth posB elements)))
		   (t (list nil nil))))))
		    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some stuff to do translation of objects and operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Translate keim-objects into lisp entities

(defgeneric rcl~translate-object (operation)
  (declare (edited  "09-MAR-2000")
	   (authors Sorge)
	   (input   "An operation on residue classes.")
	   (effect  "None.")
	   (value   "The operation as a funcallable expression."))
  (:method (anything)
	   (omega~error ";;;RCL~~TRANSLATE-OBJECT: ~A is not a suitable operation!" anything))
  (:method ((function function))
	   function)
  (:method ((operation term+abstr))
	   (if (= (length (data~abstr-binder operation)) 2)
	       (let ((operation (rcl=translate-operation operation)))
		 (or operation
		     (omega~error ";;;RCL~~TRANSLATE-OBJECT: ~A is not a legal operation!" operation)))
	     (omega~error ";;;RCL~~TRANSLATE-OBJECT: ~A is not a legal operation in two variables!" operation)))
  (:method ((number term+number))
	   (keim~name number))
  (:method ((number number))
	   number)
  (:method ((operation term+term))
	   (omega~error ";;;RCL~~TRANSLATE-OBJECT: Don't know how to translate ~A as operation as it is not an abstraction!" operation)))
	   

(defun rcl=translate-operation (operation)
  (declare (edited  "10-MAR-2000")
	   (authors Sorge)
	   (input   "An abstraction representing an operation.")
	   (effect  "None.")
	   (value   "A funcallable lisp function corresponding to the operation."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (functions (mapcan
		     #'(lambda (symbol arith)
			 (let ((func (env~lookup-object symbol env)))
			   (when func (list (cons func arith)))))
		     '(plus minus times div mod power)
		     '(+ - * / mod expt)))
	 (vars (data~abstr-binder operation))
	 (var-list (list (read-from-string (symbol-name (gensym "X")))
			 (read-from-string (symbol-name (gensym "Y")))))
	 (variables (pairlis vars var-list))
	 (new-term (rcl=translate-term (data~abstr-range operation) functions variables)))
    (when new-term
      (eval (list 'lambda var-list new-term)))))
		     
(defgeneric rcl=translate-term (term functions variables)
  (:method ((appl term+appl) functions variables)
	   (let* ((func (data~appl-function appl))
		  (args (data~appl-arguments appl))
		  (real-func (assoc func functions :test #'data~equal)))
	     (if real-func 
		 (let ((farg (rcl=translate-term (first args) functions variables))
		       (sarg (rcl=translate-term (second args) functions variables)))
		   (when (and farg sarg)
		     (list (cdr real-func) farg sarg)))
	       (omega~error "Illegal function ~A in operation!" func))))
  (:method ((number term+number) functions variables)
	   (declare (ignore functions variables))
	   (keim~name number))
  (:method ((var term+variable) functions variables)
	   (declare (ignore functions))
	   (let* ((real-var (assoc var variables :test #'data~equal)))
	     (or (cdr real-var)
		 (omega~error "Illegal variable ~A in operation!" var))))
  (:method ((term term+term) functions variables)
	   (declare (ignore functions variables))
	   (omega~error "Illegal term ~A in operation!" term)))


;;; Translate lisp operations into Post syntax
		 
(defun rcl=operation2post (operation &optional alist)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "An operation and an association list.")
	   (effect  "None.")
	   (value   "The operation in post syntax, where elements in the original operation are"
		    "replaced according to ALIST."))
  (cond ((null operation) operation)
	((consp operation) (mapcar #'(lambda (x)
				       (rcl=operation2post x alist))
				   operation))
	(t (or (cdr (assoc operation
			   (append
			    '((+ . plus) (- . minus) (* . times) (/ . div) (mod . mod) (expt . power))
			    alist)))
	       operation))))
  
(defgeneric rcl=post2operation (operation &optional alist)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "An operation in post syntax and an association list.")
	   (effect  "None.")
	   (value   "Two values: The operation in lisp syntax,where elements in the original operation are"
		    "replaced according to ALIST, and a list of occuring variable-terms."))
  (:method ((operation string) &optional alist)
	   (multiple-value-bind (op vars)
	       (rcl=post2operation (read-from-string operation) alist)
	     (values op vars)))
  (:method (operation &optional alist)
	   (let (var-terms)
	     (labels ((p2op (op)
			    (cond ((null op) op)
				  ((consp op) (mapcar #'(lambda (x) (p2op x)) op))
				  (t (let ((op-term
					    (cdr (assoc op
							(append '((plus . +) (minus . -) (times . *) (div . /) (mod . mod) (power . expt))
								alist)))))
				       (if op-term op-term
					 (progn
					   (pushnew op var-terms)
					   op)))))))
	       (values (p2op operation) (remove-if #'numberp var-terms))))))
	      
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inquiries via GAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rcl*gap-objects (make-hash-table)
  "A hash-table mapping RCL+MULTIPLICATION-TABLE objects to GAP variables.")

(defun rcl~get-gap-table (table)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Can enter new objects into the gap objects hash-table and create"
		    "create objects in the GAP service.")
	   (value   "A variable representing a GAP variable for a MAGMA in GAP."))
  (let ((gap-var (gethash table rcl*gap-objects)))
    (or gap-var
	(let ((new-var (symbol-name (gensym "M")))
	      (gap-table (rcl=gap-magma-table table)))
	  (when gap-table
	    (rcl~call-gap (concatenate 'string new-var " := " gap-table))
	    (setf (gethash table rcl*gap-objects) new-var)
	    new-var)))))

(defun rcl~gap-commutative? (table)
  (declare (edited  "05-JUN-2000" "11-MAR-2000")
	   (authors Sorge Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls GAP.")
	   (value   "T if the table is commutative. Otherwise NIL."))
  (let ((gap-table (rcl~get-gap-table table)))
    (when gap-table
      (let* ((result (rcl~call-gap (concatenate 'string "IsCommutative(" gap-table ")"))))
	(cond ((string-equal result "true") t)
	      ((string-equal result "false") nil)
	      (t (omega~error "GAP could not process the variable: ~A" gap-table)))))))

(defun rcl~gap-associative? (table)
  (declare (edited  "11-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls GAP.")
	   (value   "T if the table is associative. Otherwise NIL."))
  (let ((gap-table (rcl~get-gap-table table)))
    (when gap-table
      (let* ((result (rcl~call-gap (concatenate 'string "IsAssociative(" gap-table ")"))))
	(cond ((string-equal result "true") t)
	      ((string-equal result "false") nil)
	      (t (omega~error "GAP could not process the variable: ~A" gap-table)))))))

(defun rcl~gap-neutral-element? (table)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls GAP.")
	   (value   "The unit element of the table if one exists. Otherwise NIL."))
  (let ((gap-table (rcl~get-gap-table table)))
    (when gap-table
      (let* ((result (rcl~call-gap (concatenate 'string "MultiplicativeNeutralElement(" gap-table ")"))))
	(cond ((string-equal result "fail") nil)
	      (result (nth (rcl=gap-element2number result) (rcl~elements table)))
	      (t (omega~error "GAP could not process the variable: ~A" gap-table)))))))

(defun rcl~gap-inverse-element (table n) 
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table and a number.")
	   (effect  "Calls GAP.")
	   (value   "The inverse element for the nth table elements if one exists. Otherwise NIL."))
  (let ((gap-table (rcl~get-gap-table table)))
    (when gap-table
      (let* ((result (rcl~call-gap
		      (concatenate 'string
				   "Inverse(Elements(" gap-table ")[" (write-to-string n) "])"))))
	(cond ((string-equal result "fail") nil)
	      (result (nth (rcl=gap-element2number result) (rcl~elements table)))
	      (t (omega~error "GAP could not process the variable: ~A" gap-table)))))))

;;; Isomorphisms on groups

(defun rcl=gap-check-isomorphism (table1 table2)
  (declare (edited  "06-JUN-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables.")
	   (effect  "Calls GAP.")
	   (value   "The GAP representation of the isomorphism if both tables for groups and"
		    "an isomorphism exists, otherwise NIL."))
  (when (rcl=check-isomorphism-prerequisites table1 table2)
    (let ((res1 (gap~call-gap (concatenate 'string "GroupByMultiplicationTable(" (rcl~table2gap table1) ")")))
	  (res2 (gap~call-gap (concatenate 'string "GroupByMultiplicationTable(" (rcl~table2gap table1) ")"))))
      (unless (or (string-equal res1 "fail") (string-equal res2 "fail"))
	(gap~call-gap "IsomorphismGroups(last2,last)")))))

;;; Order

(defun rcl~gap-element-order (table n)
  (declare (edited  "12-SEP-2000")
	   (authors Sorge)
	   (input   "A closed multiplication table and an integer.")
	   (effect  "Calls GAP.")
	   (value   "The order of the nth element of the table if it has one. O/w NIL."))
  (let ((gap-table (rcl~get-gap-table table)))
    (when gap-table
      (let* ((elem-str (concatenate 'string "Elements(" gap-table ")[" (write-to-string n) "]"))
	     (result (rcl~call-gap (concatenate 'string "Inverse(" elem-str ")"))))
	(cond ((string-equal result "fail") nil)
	      (result 
	       (read-from-string (rcl~call-gap (concatenate 'string "Order(" elem-str ")"))))
	      (t (omega~error "GAP could not process the variable: ~A" gap-table)))))))

(defun rcl~gap-generated-substructure (table n)
  (declare (edited  "12-SEP-2000")
	   (authors Sorge)
	   (input   "A closed multiplication table and an integer.")
	   (effect  "Calls GAP.")
	   (value   "The substructure generated by the nth element of the table."))
  (let ((gap-table (rcl~get-gap-table table)))
    (when gap-table
      (let ((elements (rcl~elements table))
	    (result (rcl~call-gap
		     (concatenate 'string "Elements(Submagma(" gap-table ",[Elements("
				  gap-table ")[" (write-to-string n) "]]))"))))
	(cond ((string-equal result "fail")
	       (omega~warn "Gap could not find a generated substructure!")
	       nil)
	      (result
	       (mapcar #'(lambda (n) (nth n elements))
		       (rcl=gap-element-list2number-list result)))
	      (t (omega~error "GAP could not process the variable: ~A" gap-table)))))))

(defun rcl~gap-element-power (table n m)
  (declare (edited  "13-SEP-2000")
	   (authors Sorge)
	   (input   "A closed multiplication table and two integers.")
	   (effect  "Calls GAP.")
	   (value   "The mth power of the nth element in the table."))
  (let ((gap-table (rcl~get-gap-table table)))
    (when gap-table
      (let ((elements (rcl~elements table))
	    (result (rcl~call-gap (concatenate 'string "Elements(" gap-table ")["
						(write-to-string n) "]^" (write-to-string m)))))
	(cond ((or (string-equal result "fail")
		   (string-equal (read-from-string result) "Error"))
	       (omega~warn "Gap could not compute the ~:R power of ~A." m (nth n elements))
	       nil)
	      (result (nth (rcl=gap-element2number result) elements))
	      (t (omega~error "GAP could not process the variable: ~A" gap-table)))))))

      
;;; Auxiliary functions for working with GAP

(defun rcl=gap-element-list2number-list (list)
  (declare (edited  "12-SEP-2000")
	   (authors Sorge)
	   (input   "A string representing a list of elements in GAP syntax.")
	   (effect  "None.")
	   (value   "A list of numbers corresponding to the GAP list."))
  (let ((str-list (rcl=gap-list2list list)))
      (mapcar #'rcl=gap-element2number str-list)))

(defun rcl=gap-list2list (list)
  (declare (edited  "12-SEP-2000")
	   (authors Sorge)
	   (input   "A string representing a list in GAP syntax.")
	   (effect  "None.")
	   (value   "A list corresponding to the GAP list."))
  (atptop~divide-string (string-trim '(#\{ #\} #\[ #\] #\SPACE) list) #\, :ignore-char-list '(#\SPACE)))

(defun rcl=gap-element2number (element)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A GAP element of a magma.")
	   (effect  "None.")
	   (value   "The number it corresponds to."))
  (1- (read-from-string (string-left-trim '(#\m) element))))
	  
(defun rcl=gap-magma-table (table)
  (declare (edited  "11-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "None.")
	   (value   "An expression for GAP's MagmaByMultiplicationTable command."))
  (when (rcl~check-closure table)
    (concatenate 'string "MagmaByMultiplicationTable(" (rcl~table2gap table) ");")))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Output Interface 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GAP Interface

(defgeneric rcl~table2gap (table)
  (declare (edited  "21-FEB-2000")
	   (authors Sorge)
	   (input   "A multiplication table. (The table should be closed under the operation!)")
	   (effect  "None.")
	   (value   "The multiplication table in GAP format."))
  (:method ((table rcl+multiplication-table))
	   (let* ((elements (rcl~elements table))
		  (gapelems (rcl=make-element-list (1+ (length elements))))
		  (elemassoc (pairlis elements gapelems))
		  (table (mapcar #'(lambda (row)
				     (mapcar #'(lambda (col)
						 (cdr (assoc col elemassoc :test #'equal)))
					     row))
				 (rcl~table table)))
		  (rows (mapcar #'(lambda (row)
				    (format nil "[~{~A~^, ~}]" row))
				table)))
	     (format nil "[~{~A~^, ~}]" rows))))
			
;;; Print Object Method

(defmethod print-object ((table rcl+multiplication-table)  stream)
  (if (> (length (rcl~elements table)) 50)
      (omega~warn "Printing size of table expired.")
    (if (rcl~operation table)
	(multiple-value-bind (a1 a2 a3)
	    (function-lambda-expression (rcl~operation table))
	  (declare (ignore a2))
	  (let ((elements (rcl~elements table))
		(operation (format nil "~A" (if a1 a1 a3)))
		(table (rcl~table table)))
	    (if (> (length operation) 3)
		(format stream "~A~%   |~{~3:<~A~>~}" operation elements)
	      (format stream "~3:<~A~>|~{~3:<~A~>~}" operation elements))
	    (format stream "~%----")
	    (dotimes (x (length elements))
	      (format stream "---"))
	    (mapc #'(lambda (x y)
		      (format stream "~%~3:<~A~>|~{~3:<~A~>~}" x y))
		  elements
		  table)))
      (let ((elements (rcl~elements table))
	    (table (rcl~table table)))
	(format stream "~%   |~{~3:<~A~>~}" elements)
	(format stream "~%----")
	(dotimes (x (length elements))
	  (format stream "---"))
	(mapc #'(lambda (x y)
		  (format stream "~%~3:<~A~>|~{~3:<~A~>~}" x y))
	      elements
	      table)))))
      

(defmethod print-object ((table rcl+scalar-multiplication-table)  stream)
  (if (or (> (length (rcl~elements table)) 50)
	  (> (length (rcl~scalar-elements table)) 50))
      (omega~warn "Printing size of table expired.")
    (multiple-value-bind
	(a1 a2 a3)
	(function-lambda-expression (rcl~operation table))
      (declare (ignore a2))
      
      (let ((scalar-elements (rcl~scalar-elements table))
	    (elements (rcl~elements table))
	    (operation (format nil "~A" (if a1 a1 a3)))
	    (table (rcl~table table)))
	(if (> (length operation) 3)
	    (format stream "~%~A~%   |~{~3:<~A~>~}" operation elements)
	  (format stream "~%~3:<~A~>|~{~3:<~A~>~}" operation elements))
	(format stream "~%----")
	(dotimes (x (length elements))
	  (format stream "---"))
	(mapc #'(lambda (x y)
		  (format stream "~%~3:<~A~>|~{~3:<~A~>~}" x y))
	      scalar-elements
	      table)))))

(defmethod print-object ((table rcl+product-multiplication-table)  stream)
  (if (> (length (rcl~elements table)) 30)
      (omega~warn "Printing size of table expired.")
    (let* ((operations (mapcar #'(lambda (op) (multiple-value-list (function-lambda-expression op)))
			       (rcl~operation table)))
	   (operation (format nil "(~{~A ~})"
			      (mapcar #'(lambda (op) (if (first op) (first op) (third op))) operations)))
	   (elements (rcl~elements table))
	   (table (rcl~table table)))
      (if (> (length operation) 8)
	  (format stream "~%~A~%       |~{~8:<~A~>~}" operation elements)
	(format stream "~%~8:<~A~>|~{~8:<~A~>~}" operation elements))
      (format stream "~%--------")
      (dotimes (x (length elements))
	(format stream "--------"))
      (mapc #'(lambda (x y)
		(format stream "~%~8:<~A~>|~{~8:<~A~>~}" x y))
	    elements
	    table))))

(defmethod print-object ((table rcl+product-scalar-multiplication-table)  stream)
  (if (or (> (length (rcl~elements table)) 30)
	  (> (length (rcl~scalar-elements table)) 30))
      (omega~warn "Printing size of table expired.")
    (let* ((operations (mapcar #'(lambda (op)
				   (multiple-value-list (function-lambda-expression op)))
			       (rcl~operation table)))
	   (operation (format nil "(~{~A ~})"
			      (mapcar #'(lambda (op) (if (first op) (first op) (third op))) operations)))
	   (scalar-elements (rcl~scalar-elements table))
	   (elements (rcl~elements table))
	   (table (rcl~table table)))
      (if (> (length operation) 8)
	  (format stream "~%~A~%       |~{~8:<~A~>~}" operation elements)
	(format stream "~%~8:<~A~>|~{~8:<~A~>~}" operation elements))
      (format stream "~%--------")
      (dotimes (x (length elements))
	(format stream "--------"))
      (mapc #'(lambda (x y)
		(format stream "~%~8:<~A~>|~{~8:<~A~>~}" x y))
	    scalar-elements
	    table))))

;; Printing functions

(defun rcl~print-multiplication-table (table &optional (stream t))
  (declare (edited  "30-JUL-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Prints some information on the table.")
	   (value   "NIL."))
  (let* ((operations (mapcar #'(lambda (op) (multiple-value-list (function-lambda-expression op)))
			     (rcl~operation table)))
	 (operation (format nil "(~{~A ~})"
			    (mapcar #'(lambda (op) (if (first op) (first op) (third op))) operations)))
	 (elements (format nil "~{ ~A~}" (mapcar #'(lambda (x) (format nil "~A" x)) (rcl~elements table)))))
    (format stream "~%Elements: ~A" elements)
    (format stream "~%Operation: ~A" operation)
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking Properties of Multiplication Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Closed under the operation?
;;

(defgeneric rcl~check-closure (table)
  (declare (edited  "09-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplcation table.")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the table is closed under the operation, otherwise NIL."
		    "Second value: The pair of values for whose result is not in the set if the table is not closed."))
  (:method ((table rcl+multiplication-table))
	   (let* ((elements (rcl~elements table))
		  (table (rcl~table table))
		  (values (rcl=check-rows-for-closure elements table elements)))
	     (values (not values) values)))	
  (:method ((table rcl+product-multiplication-table))
	   (let* ((tables (rcl~single-tables table))
		  (values (mapcar #'(lambda (tab) (multiple-value-list (rcl~check-closure tab))) tables)))
	     (if (every #'car values)
		 (values t nil)
	       (values nil
		       (rcl=make-tuples-from-elements (mapcar #'cadr values) (mapcar #'rcl~elements tables))
	       ))))
  )

(defun rcl=check-rows-for-closure (column rows elements)
  (declare (edited  "19-FEB-2000")
	   (authors Sorge)
	   (input   "A list corresponding to the first column of a multiplication table,"
		    "a list of lists representing the multiplication table, and another list"
		    "containing all the elements in the set.")
	   (effect  "None.")
	   (value   "A list containing a column and a row element if for those two elements"
		    "the operation is not closed. NIL if the table is closed." ))
  (when (and column rows)
    (let ((col (rcl=check-row-for-closure elements (car rows) elements)))
      (if col
	  (list (car column) col)
	(rcl=check-rows-for-closure (cdr column) (cdr rows) elements)))))

(defun rcl=check-row-for-closure (row-elements row elements)
  (declare (edited  "19-FEB-2000")
	   (authors Sorge)
	   (input   "A list corresponding to the first row of a multiplication table,"
		    "a list one row of a multiplication table, and another list"
		    "containing all the elements in the set.")
	   (effect  "None.")
	   (value   "The row-element in whose column a none-closure has been found."
		    "NIL if the row is closed." ))
  (when (and row-elements row)
    (if (find (car row) elements)
	(rcl=check-row-for-closure (cdr row-elements) (cdr row) elements)
      (car row-elements))))

;;
;; Is the operation associative?
;;

(defgeneric rcl~check-associativity (table)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the table is associative wrt. the operation, otherwise NIL."
		    "Second value: An equation in post syntax, hinting at the proof of associativity"
		    "or a triple of values that refutes associativity."
		    "(The equation is currently omitted.)"))
  (:method ((table rcl+multiplication-table))
	   (let* ((associative? (rcl~gap-associative? table))
		  (operation (rcl~operation table))
		  (rhs (rcl=right-associative-operation operation))
		  (lhs (rcl=left-associative-operation operation))
		  )
	     (if associative?
		 (values
		  t
		  #+weg(concatenate 'string
				    "(= "
				    (rcl~call-maple (list "simplify" lhs) :syntax 'post2post)
				    " "
				    (rcl~call-maple (list "simplify" rhs) :syntax 'post2post)
				    ")"))
	       (values nil
		       (rcl=refute-associativity
			table
			(rcl~call-maple (list "solve" (concatenate 'string "(= " lhs " " rhs ")")) :syntax 'post2maple))))))
  (:method ((table rcl+product-multiplication-table))
	   (let* ((tables (rcl~single-tables table))
		  (values (mapcar #'(lambda (tab) (multiple-value-list (rcl~check-associativity tab))) tables)))
	     (if (every #'car values)
		 (values t nil)
	       (values nil
		       (rcl=make-tuples-from-elements (mapcar #'cadr values) (mapcar #'rcl~elements tables))
		       ))))
  )

(defun rcl=refute-associativity (table hint)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication-table and a string representing some hint in MAPLE syntax.")
	   (effect  "None.")
	   (value   "A triple of elements for which associativity does not hold."))
  (let* ((hint-list (rcl=remove-redundant-hints (rcl=dissect-maple-hint hint)))
	 (modulo (rcl~modulo table))
	 (operation (rcl~operation table))
	 (triples (rcl=permute-with-hint (rcl~elements table) hint-list modulo)))
    (find-if-not
     #'(lambda (x) (= (mod (rcl=compute-left-assoc x operation) modulo)
		      (mod (rcl=compute-right-assoc x operation) modulo)))
     triples)))

(defun rcl=permute-with-hint (elements hints modulo)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list of elements, some hints, and a number.")
	   (effect  "None.")
	   (value   "A list of triples of elements, that are valid wrt. the hint."))
  (rcl=remove-with-hint (mapcan #'(lambda (x)
				    (mapcan #'(lambda (y)
						(mapcar #'(lambda (z)
							    (list x y z))
							elements))
					    elements))
				elements)
			hints modulo))

(defun rcl=remove-with-hint (lists hints modulo)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list of lists and some hints.")
	   (effect  "None.")
	   (value   "A list of lists where elements are removed wrt. to the hint."))
  (if hints
      (flet ((rewrite-number (n)
			     (let ((res (etypecase n
					  (number n)
					  (symbol (read-from-string (symbol-name n)))
					  (string (read-from-string n)))))
			       (if (numberp res)
				   (mod res modulo)
				 res))))
	(let* ((fhint (caar hints))
	       (shint (cadar hints))
	       (num? (rewrite-number shint)))
	  (cond ((not (or (find shint '(a b c "-a" "-b" "-c") :test #'string-equal)
			  (numberp num?)))
		 (rcl=remove-with-hint lists (cdr hints) modulo))
		((or (and (string-equal fhint :a) (string-equal shint :b))
		     (and (string-equal fhint :b) (string-equal shint :a)))
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (first x) (second x))) lists) (cdr hints) modulo))
		((or (and (string-equal fhint :a) (string-equal shint :c))
		     (and (string-equal fhint :c) (string-equal shint :a)))
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (first x) (third x))) lists) (cdr hints) modulo))
		((or (and (string-equal fhint :b) (string-equal shint :c))
		     (and (string-equal fhint :c) (string-equal shint :b)))
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (second x) (third x))) lists) (cdr hints) modulo))
		((or (and (string-equal fhint :a) (string-equal shint "-b"))
		     (and (string-equal fhint :b) (string-equal shint "-a")))
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (first x) (mod (* -1 (second x)) modulo))) lists) (cdr hints) modulo))
		((or (and (string-equal fhint :a) (string-equal shint "-c"))
		     (and (string-equal fhint :c) (string-equal shint "-a")))
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (first x) (mod (* -1 (third x)) modulo))) lists) (cdr hints) modulo))
		((or (and (string-equal fhint :b) (string-equal shint "-c"))
		     (and (string-equal fhint :c) (string-equal shint "-b")))
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (second x) (mod (* -1 (third x)) modulo))) lists) (cdr hints) modulo))
		((string-equal fhint :a)
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (first x) num?)) lists) (cdr hints) modulo))
		((string-equal fhint :b)
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (second x) num?)) lists) (cdr hints) modulo))
		((string-equal fhint :c)
		 (rcl=remove-with-hint (remove-if #'(lambda (x) (= (third x) num?)) lists) (cdr hints) modulo))
		)))
    lists))

(defun rcl=compute-left-assoc (triple operation)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list with three values and an operation.")
	   (effect  "None.")
	   (value   "The result of applying the operation *: (a * b) * c."))
  (let ((a (first triple))
	(b (second triple))
	(c (third triple)))
    (funcall operation (funcall operation a b) c)))

(defun rcl=compute-right-assoc (triple operation)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list with three values and an operation.")
	   (effect  "None.")
	   (value   "The result of applying the operation *: a * (b * c)."))
  (let ((a (first triple))
	(b (second triple))
	(c (third triple)))
    (funcall operation a (funcall operation b c))))

(defun rcl=remove-redundant-hints (hint-list)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list of list, where each element list contains exactly two string arguments.")
	   (effect  "None.")
	   (value   "A list where all the element lists containing two equal strings are removed."))
  (cond ((and hint-list (string-equal (first (car hint-list)) (second (car hint-list))))
	 (rcl=remove-redundant-hints (cdr hint-list)))
	(hint-list (cons (car hint-list) (rcl=remove-redundant-hints (cdr hint-list))))))

(defun rcl=dissect-maple-hint (hint)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A string representing some hint for solving an equation in MAPLE syntax.")
	   (effect  "None.")
	   (value   "A list of relating variables to their substitution."))
  (mapcar #'(lambda (string)
	      (atptop~divide-string string #\=))
	  (atptop~divide-string (string-trim '(#\{ #\} #\[ #\] #\SPACE) hint) #\, :ignore-char-list '(#\SPACE))))

(defun rcl=left-associative-operation (operation)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A lambda expression.")
	   (effect  "None.")
	   (value   "A string containing the lefthand-side of associativity in post syntax."))
  (multiple-value-bind (a1 a2 a3)
      (function-lambda-expression operation)
    (declare (ignore a2))
    (if a1
	(let* ((var-list (second a1))
	       (op (third a1))
	       (new-op1 (rcl=operation2post op (pairlis var-list '(a b))))
	       (new-op2 (rcl=operation2post op (pairlis var-list (list new-op1 'c)))))
	  (string-downcase (write-to-string new-op2)))
      (let ((new-operation (rcl=operation2post a3)))
	(string-downcase (write-to-string (list new-operation (list new-operation 'a 'b) 'c)))))))

(defun rcl=right-associative-operation (operation)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A lambda expression.")
	   (effect  "None.")
	   (value   "A string containing the righthand-side of associativity in post syntax."))
  (multiple-value-bind (a1 a2 a3)
      (function-lambda-expression operation)
    (declare (ignore a2))
    (if a1
	(let* ((var-list (second a1))
	       (op (third a1))
	       (new-op1 (rcl=operation2post op (pairlis var-list '(b c))))
	       (new-op2 (rcl=operation2post op (pairlis var-list (list 'a new-op1)))))
	  (string-downcase (write-to-string new-op2)))
      (let ((new-operation (rcl=operation2post a3)))
	(string-downcase (write-to-string (list new-operation 'a (list new-operation 'b 'c))))))))

  
;;
;; Does there exist a neutral element?
;;

(defgeneric rcl~check-unit-element (table)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplcation table.")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the table contains an neutral element wrt. its operation, otherwise NIL."
		    "Second value: The unit element if it exists, otherwise a list of pairs where each"
		    "element of the set is paired with the element that it changes under the operation."))
  (:method ((table rcl+multiplication-table))
	   (let ((unit (rcl~gap-neutral-element? table)))
	     (if unit
		 (values t unit)
	       (values nil (rcl=refute-unit-element table)))))
  (:method ((table rcl+product-multiplication-table))
	   (let* ((tables (rcl~single-tables table))
		  (values (mapcar #'(lambda (tab) (multiple-value-list (rcl~check-unit-element tab))) tables))
		  (elements (mapcar #'rcl~elements tables)))
	     (if (every #'car values)
		 (values t
			 (rcl=make-tuple-from-elements (mapcar #'cadr values) elements))
	       (values nil
		       (rcl=refute-unit-element table)
		       ))))
  )

(defun rcl=refute-unit-element (table)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table that does not have a unit.")
	   (effect  "None.")
	   (value   "A list of pairs suitable for refuting the proposition of a unit element."))
  (labels ((disagree (elem l1 l2)
		     (cond ((or (null l1) (null l2)) nil)
			   ((equal (car l1) (car l2)) (disagree elem (cdr l1) (cdr l2)))
			   (t (list elem (car l2))))))
    (let* ((elements (rcl~elements table))
	   (rows (rcl~table table))
	   (columns (rcl=rows2columns rows))
	   (first-list (mapcar #'(lambda (element row) (disagree element row elements)) elements rows))
	   (second-list (unless (notany #'null first-list)
			  (mapcar #'(lambda (element column) (disagree element column elements)) elements columns))))
      (or second-list first-list))))


;;
;; Does every element have an inverse?
;;

(defgeneric rcl~check-inverses (table)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplcation table.")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the table contains inverses for every element, otherwise NIL."
		    "Second value: A list of pairs consisting of elements and their inverses, if one"
		    "exists for every element, otherwise an element for which no inverse exists."))
  (:method ((table rcl+multiplication-table))
	   (labels ((check-inverse (elements n)
				   (when elements
				     (cons (list (car elements) (rcl~gap-inverse-element table n)) 
					   (check-inverse (cdr elements) (1+ n))))))
	     (let* ((result (check-inverse (rcl~elements table) 1))
		    (no-success (car (find-if #'null result :key #'second))))
	       (if no-success (values nil no-success)
		 (values t result)))))
  (:method ((table rcl+product-multiplication-table))
	   (let* ((tables (rcl~single-tables table))
		  (values (mapcar #'(lambda (tab) (multiple-value-list (rcl~check-inverses tab))) tables)))
	     (if (every #'car values)
		 (values t (rcl=lists2tuple-lists (mapcar #'cadr values)))
	       (values nil
		       (rcl=make-tuple-from-elements
			(mapcar #'(lambda (x) (unless (listp (cadr x)) (cadr x))) values)
			(mapcar #'rcl~elements tables))
	       ))))
  )

;;
;; Do divisors exist for every two elements?
;;

(defgeneric rcl~check-divisors (table)
  (declare (edited  "15-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplcation table.")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if divisors exist for every two elements, otherwise NIL."
		    "Second value: If first value is NIL, a pair of elements for which no divisors exist."))
  (:method ((table rcl+multiplication-table))
	   (let* ((rows (rcl~table table))
		  (columns (rcl=rows2columns rows))
		  (elements (rcl~elements table))
		  (divisors (or (rcl=check-for-divisors rows elements elements)
				(rcl=check-for-divisors columns elements elements))))
	     (values (unless divisors t) divisors)))
  (:method ((table rcl+product-multiplication-table))
	   (let* ((tables (rcl~single-tables table))
		  (values (mapcar #'(lambda (tab) (multiple-value-list (rcl~check-divisors tab))) tables)))
	     (if (every #'car values)
		 (values t nil)
	       (values nil
		       (rcl=make-tuples-from-elements (mapcar #'cadr values) (mapcar #'rcl~elements tables))
		       ))))
  )

(defun rcl=check-for-divisors (list1 list2 elements)
  (declare (edited  "15-MAR-2000")
	   (authors Sorge)
	   (input   "Three lists.")
	   (effect  "None.")
	   (value   "A pair of elements for which no divisors exist, NIL if all elements have divisors."))
  (when list1
    (let ((difference (set-difference list2 (car list1))))
      (if difference
	  (list (car elements) (car difference))
	(rcl=check-for-divisors (cdr list1) list2 (cdr elements))))))
  
  
(defun rcl=rows2columns (table)
  (declare (edited  "15-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table given as a list of rows.")
	   (effect  "None.")
	   (value   "A list of columns."))
  (unless (every #'null table)
    (cons (mapcar #'car table)
	  (rcl=rows2columns (mapcar #'cdr table)))))


;;
;; Is the operation commutative?
;;

(defgeneric rcl~check-commutativity (table)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the table is commutative wrt. the operation, otherwise NIL."
		    "Second value: An equation in post syntax, hinting at the proof of commutativity"
		    "or a pair of values that refutes commutativity."
		    "(The equation is currently omitted.)"))
  (:method ((table rcl+multiplication-table))
	   (let* ((commutative? (rcl~gap-commutative? table)))
	     (if commutative? t
	       (values nil
		       (rcl=check-for-non-commuting-elements table)))))
  (:method ((table rcl+product-multiplication-table))
	   (let* ((tables (rcl~single-tables table))
		  (values (mapcar #'(lambda (tab) (multiple-value-list (rcl~check-commutativity tab))) tables)))
	     (if (every #'car values)
		 (values t nil)
	       (values nil
		       (rcl=make-tuples-from-elements (mapcar #'cadr values) (mapcar #'rcl~elements tables))
		       ))))
  )

(defun rcl=check-for-non-commuting-elements (table)
  (declare (edited  "05-JUN-2000")
	   (authors Sorge)
	   (input   "A multiplication-table.")
	   (effect  "None.")
	   (value   "A pair of elements that are not commutative."))
  (let* ((elements (rcl~elements table))
	 (rows (rcl~table table))
	 (columns (rcl=rows2columns rows)))
    (labels ((check-rows (rows columns counts)
			 (when (and rows columns counts)
			   (let ((result (check-columns (car rows) (car columns) elements)))
			     (if result
				 (list (car counts) result)
			       (check-rows (cdr rows) (cdr columns) (cdr counts))))))
	     (check-columns (rows columns counts)
			    (when (and rows columns counts)
			      (if (= (car rows) (car columns))
				  (check-columns (cdr rows) (cdr columns) (cdr counts))
				(car counts)))))
      (check-rows rows columns elements))))


;;
;; Is the operation distributive?
;;

(defgeneric rcl~check-distributivity (table1 table2)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables.")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if distributivity holds between the operation of the two tables, otherwise NIL."
		    "Second value: An equation in post syntax, hinting at the proof of distributivity left"
		    "              or a triple of values that refutes distributivity."
		    "Third value: An equation in post syntax, hinting at the proof of distributivity right."
		    "(The equations are currently omitted."))
  (:method ((table1 rcl+multiplication-table) (table2 rcl+multiplication-table))
	   (when (and (= (rcl~modulo table1) (rcl~modulo table2))
		      (tree-equal (rcl~elements table1) (rcl~elements table2)))
	     (let* ((op1 (rcl~operation table1))
		    (op2 (rcl~operation table2))
		    (leq (rcl=left-distributivity-equation op1 op2))
		    (req (rcl=right-distributivity-equation op1 op2))
		    (hint (rcl=check-for-distributivity leq req)))
		  
	       (if hint
		   (values nil (rcl=refute-distributivity table1 table2 hint))
		 t
		 #+weg(let* ((hleq (read-from-string leq))
			(leql (string-downcase (write-to-string (second hleq))))
			(leqr (string-downcase (write-to-string (third hleq))))
			(hreq (read-from-string req))
			(reql (string-downcase (write-to-string (second hreq))))
			(reqr (string-downcase (write-to-string (third hreq)))))
		   (values
		    t                                               ;;; If the additional values are not needed they should be omitted,
		    (concatenate 'string                            ;;; since they really slow things down....  VS
				 "(= "
				 (rcl~call-maple (list "simplify" leql) :syntax 'post2post)
				 " "
				 (rcl~call-maple (list "simplify" leqr) :syntax 'post2post)
				 ")")
		    (concatenate 'string
				 "(= "
				 (rcl~call-maple (list "simplify" reql) :syntax 'post2post)
				 " "
				 (rcl~call-maple (list "simplify" reqr) :syntax 'post2post)
				 ")")))))))
  (:method ((table1 rcl+product-multiplication-table) (table2 rcl+product-multiplication-table))
	   (let* ((tables1 (rcl~single-tables table1))
		  (tables2 (rcl~single-tables table2))
		  (values (mapcar #'(lambda (tab1 tab2)
				      (multiple-value-list (rcl~check-distributivity tab1 tab2)))
				  tables1 tables2)))
	     (if (every #'car values)
		 (values t nil)
	       (values nil
		       (rcl=make-tuples-from-elements (mapcar #'cadr values) (mapcar #'rcl~elements tables1))
		       ))))
    )

(defun rcl=check-for-distributivity (leq req)
  (declare (edited  "06-JUN-2000")
	   (authors Sorge)
	   (input   "Two strings containing the distributivity left and right equation.")
	   (effect  "Calls Maple.")
	   (value   "A set of equations as the result of Maples solution for the equations."))
  (let ((solution (rcl~call-maple (list "solve"
					(concatenate 'string "{" leq "," req "}")) :syntax 'post2maple)))
    (rcl=remove-redundant-hints (rcl=dissect-maple-hint solution))))

(defun rcl=refute-distributivity (table1 table2 hint)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "Two multiplication-tables and a string representing some hint in MAPLE syntax.")
	   (effect  "None.")
	   (value   "A triple of elements for which distributivity does not hold."))
  (let* ((modulo (rcl~modulo table1))
	 (op1 (rcl~operation table1))
	 (op2 (rcl~operation table2))
	 (triples (rcl=permute-with-hint (rcl~elements table1) hint modulo)))
    (find-if-not
     #'(lambda (x) (and (= (mod (rcl=compute-left-distrib-lhs x op1 op2) modulo)
			   (mod (rcl=compute-left-distrib-rhs x op1 op2) modulo))
			(= (mod (rcl=compute-right-distrib-lhs x op1 op2) modulo)
			   (mod (rcl=compute-right-distrib-rhs x op1 op2) modulo))))
     triples)))
     
(defun rcl=compute-left-distrib-lhs (triple op1 op2)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list with three values and two operations.")
	   (effect  "None.")
	   (value   "The result of applying the operation +,*: (a * (b + c))."))
  (let ((a (first triple))
	(b (second triple))
	(c (third triple)))
    (funcall op2 a (funcall op1 b c))))

(defun rcl=compute-left-distrib-rhs (triple op1 op2)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list with three values and two operations.")
	   (effect  "None.")
	   (value   "The result of applying the operation +,*: ((a * b) + (a * c))."))
  (let ((a (first triple))
	(b (second triple))
	(c (third triple)))
    (funcall op1 (funcall op2 a b) (funcall op2 a c))))


(defun rcl=compute-right-distrib-lhs (triple op1 op2)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list with three values and two operations.")
	   (effect  "None.")
	   (value   "The result of applying the operation +,*: ((a + b) * c)."))
  (let ((a (first triple))
	(b (second triple))
	(c (third triple)))
    (funcall op2 (funcall op1 a b) c)))

(defun rcl=compute-right-distrib-rhs (triple op1 op2)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "A list with three values and two operations.")
	   (effect  "None.")
	   (value   "The result of applying the operation +,*: ((a * c) + (b * c))."))
  (let ((a (first triple))
	(b (second triple))
	(c (third triple)))
    (funcall op1 (funcall op2 a c) (funcall op2 b c))))


(defun rcl=left-distributivity-equation (op1 op2)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "Two lambda expressions.")
	   (effect  "None.")
	   (value   "A string containing the equation for distributivity left in post syntax."))
  (multiple-value-bind (a1 a2 a3)
      (function-lambda-expression op1)
    (declare (ignore a2))
  (multiple-value-bind (b1 b2 b3)
      (function-lambda-expression op2)
    (declare (ignore b2))
    (cond ((and a1 b1)
	   (let* ((var-list1 (second a1)) (var-list2 (second b1))
		  (hop1 (third a1))       (hop2 (third b1))
		  (lop1 (rcl=operation2post hop1 (pairlis var-list1 '(b c))))
		  (lop2 (rcl=operation2post hop2 (pairlis var-list2 (list 'a lop1))))
		  (rop1a (rcl=operation2post hop2 (pairlis var-list2 '(a b))))
		  (rop1b (rcl=operation2post hop2 (pairlis var-list2 '(a c))))
		  (rop2 (rcl=operation2post hop1 (pairlis var-list1 (list rop1a rop1b)))))
	     (concatenate 'string "(= "
			  (string-downcase (write-to-string lop2)) " "
			  (string-downcase (write-to-string rop2)) ")")))
	  (a1
	   (let* ((var-list1 (second a1)) 
		  (hop1 (third a1))       (hop2 (rcl=operation2post b3))
		  (lop  (list hop2 'a (rcl=operation2post hop1 (pairlis var-list1 '(b c)))))
		  (rop (rcl=operation2post hop1 (pairlis var-list1 (list (list hop2 'a 'b) (list hop2 'a 'c))))))
	     (concatenate 'string "(= "
			  (string-downcase (write-to-string lop)) " "
			  (string-downcase (write-to-string rop)) ")")))
	  (b1 
	   (let* ((var-list2 (second b1)) 
		  (hop2 (third b1))       (hop1 (rcl=operation2post a3))
		  (lop  (rcl=operation2post hop2 (pairlis var-list2 (list 'a (list hop1 'b 'c)))))
		  (rop1a (rcl=operation2post hop2 (pairlis var-list2 '(a b))))
		  (rop1b (rcl=operation2post hop2 (pairlis var-list2 '(a c))))
		  (rop2  (list hop1 rop1a rop1b)))
	     (concatenate 'string "(= "
			  (string-downcase (write-to-string lop)) " "
			  (string-downcase (write-to-string rop2)) ")")))
	  (t (let* ((hop1 (rcl=operation2post a3))       (hop2 (rcl=operation2post b3))
		    (lop  (list hop2 'a (list hop1 'b 'c)))
		    (rop  (list hop1 (list hop2 'a 'b) (list hop2 'a 'c))))
	       (concatenate 'string "(= "
			    (string-downcase (write-to-string lop)) " "
			    (string-downcase (write-to-string rop)) ")")))
	  ))))

(defun rcl=right-distributivity-equation (op1 op2)
  (declare (edited  "14-MAR-2000")
	   (authors Sorge)
	   (input   "Two lambda expressions.")
	   (effect  "None.")
	   (value   "A string containing the equation for distributivity right in post syntax."))
  (multiple-value-bind (a1 a2 a3)
      (function-lambda-expression op1)
    (declare (ignore a2))
  (multiple-value-bind (b1 b2 b3)
      (function-lambda-expression op2)
    (declare (ignore b2))
    (cond ((and a1 b1)
	   (let* ((var-list1 (second a1)) (var-list2 (second b1))
		  (hop1 (third a1))       (hop2 (third b1))
		  (lop1 (rcl=operation2post hop1 (pairlis var-list1 '(a b))))
		  (lop2 (rcl=operation2post hop2 (pairlis var-list2 (list lop1 'c))))
		  (rop1a (rcl=operation2post hop2 (pairlis var-list2 '(a c))))
		  (rop1b (rcl=operation2post hop2 (pairlis var-list2 '(b c))))
		  (rop2 (rcl=operation2post hop1 (pairlis var-list1 (list rop1a rop1b)))))
	     (concatenate 'string "(= "
			  (string-downcase (write-to-string lop2)) " "
			  (string-downcase (write-to-string rop2)) ")")))
	  (a1
	   (let* ((var-list1 (second a1)) 
		  (hop1 (third a1))       (hop2 (rcl=operation2post b3))
		  (lop  (list hop2 (rcl=operation2post hop1 (pairlis var-list1 '(a b))) 'c))
		  (rop  (rcl=operation2post hop1 (pairlis var-list1 (list (list hop2 'a 'c) (list hop2 'b 'c))))))
	     (concatenate 'string "(= "
			  (string-downcase (write-to-string lop)) " "
			  (string-downcase (write-to-string rop)) ")")))
	  (b1 
	   (let* ((var-list2 (second b1)) 
		  (hop2 (third b1))       (hop1 (rcl=operation2post a3))
		  (lop  (rcl=operation2post hop2 (pairlis var-list2 (list (list hop1 'a 'b) 'c))))
		  (rop1a (rcl=operation2post hop2 (pairlis var-list2 '(a c))))
		  (rop1b (rcl=operation2post hop2 (pairlis var-list2 '(b c))))
		  (rop2  (list hop1 rop1a rop1b)))
	     (concatenate 'string "(= "
			  (string-downcase (write-to-string lop)) " "
			  (string-downcase (write-to-string rop2)) ")")))
	  (t (let* ((hop1 (rcl=operation2post a3))       (hop2 (rcl=operation2post b3))
		    (lop  (list hop2 (list hop1 'a 'b) 'c))
		    (rop  (list hop1 (list hop2 'a 'c) (list hop2 'b 'c))))
	       (concatenate 'string "(= "
			    (string-downcase (write-to-string lop)) " "
			    (string-downcase (write-to-string rop)) ")")))
	  ))))


;;; For later:
;;;
;;; Quasi-Groups:
;;; (rcl~multiplication-table 5 5 :operation #'(lambda (x y) (+ (+ x x) (+ y y))) :zero t) 
;;; (rcl~multiplication-table 5 5 :operation #'(lambda (x y) (+ x (* y 2))) :zero t)
;;; (rcl~multiplication-table '(0 2 4) 6 :operation #'(lambda (x y) (+ (+ x x) (+ y y))) :zero t)
;;; (rcl~multiplication-table 4 4  :operation #'(lambda (x y) (mod (+ (mod (* x x) 4) y) 4)) :zero t)
;;;
;;; Monoids:
;;; (rcl~multiplication-table 5 5 :operation #'* :zero t)
;;; (rcl~multiplication-table 6 6 :operation #'(lambda (x y) (+ (+ x y) 1)) :zero t)
;;;
;;; Semi-Groups:
;;; (rcl~multiplication-table 5 5 :operation #'(lambda (x y) (+ (* (+ x y) 5) 1)) :zero t)
;;; (rcl~multiplication-table 4 4 :operation #'(lambda (x y) (* (* x y) 2)) :zero t)
;;; (rcl~multiplication-table 6 6 :operation #'(lambda (x y) (* (* x y) 3)) :zero t)
;;;
;;; Non-Abelian:
;;; (rcl~multiplication-table 5 5 :operation #'- :zero t)
;;;
;;; Testing products:
#|
(setq m11 (rcl~product-multiplication-table (list (rcl~multiplication-table 2 2 :zero 0)
					    (rcl~multiplication-table 2 2 :zero 0))))
(setq m12 (rcl~product-multiplication-table (list (rcl~multiplication-table 2 2 :zero 0)
					    (rcl~multiplication-table 3 3))))
(setq m13 (rcl~product-multiplication-table (list (rcl~multiplication-table 2 2 :zero 0)
					    (rcl~multiplication-table 5 5 :operation #'(lambda (x y) (+ (+ x x) (+ y y))) :zero t))))
(setq m14 (rcl~product-multiplication-table (list (rcl~multiplication-table 2 2 :zero 0)
					    (rcl~multiplication-table 5 5 :operation #'(lambda (x y) (+ (* (+ x y) 5) 1)) :zero t))))
(setq m15 (rcl~product-multiplication-table (list (rcl~multiplication-table 2 2 :zero 0)
					    (rcl~multiplication-table 5 5 :operation #'* :zero t))))
(setq m16 (rcl~product-multiplication-table (list (rcl~multiplication-table 2 2 :zero 0)
					    (rcl~multiplication-table 5 5 :operation #'- :zero t))))
|#
;;;
#|
Testing all types of things:

(setq m0 (rcl~multiplication-table 3 3 :operation #'+ :zero nil))
(setq m1 (rcl~multiplication-table 2 2 :operation #'(lambda (x y) (+ (* x y) 1)) :zero t))
(setq m2 (rcl~multiplication-table 5 5 :operation #'(lambda (x y) (+ (+ x x) (+ y y))) :zero t))
(setq m3 (rcl~multiplication-table 6 6 :operation #'(lambda (x y) (* (* x y) 3)) :zero t))
(setq m4 (rcl~multiplication-table 5 5 :operation #'* :zero t))
(setq m5 (rcl~multiplication-table 5 5 :operation #'+ :zero t))
(setq m6 (rcl~multiplication-table 5 5 :operation #'- :zero t))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for isomorphisms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rcl=check-isomorphism-prerequisites (table1 table2)
  (declare (edited  "06-JUN-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables.")
	   (effect  "None.")
	   (value   "Checks some preliminary prerequisites for the two tables to be isomorphic."))
  (and (rcl~check-closure table1)
       (rcl~check-closure table2)
       (= (length (rcl~elements table1)) (length (rcl~elements table2)))))

(defgeneric rcl~check-isomorphism (table1 table2)
  (declare (edited  "06-JUN-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the two tables are isomorphic, otherwise NIL."
		    "Second value: A list of pairs mapping the element of the first table to those of the second."
		    "              satisfying isomorphism."))
  (:method ((table1 rcl+multiplication-table) (table2 rcl+multiplication-table))
	   (when (rcl=check-isomorphism-prerequisites table1 table2)
	     (multiple-value-bind (solution variables)
		 (rcl=solve-equation-system table1 table2)
	       (when solution
		 (let* ((vars (mapcar #'cdr variables))
			(results (rcl=split2single-solutions (rcl=dissect-maple-hint solution)))
			(elements2 (rcl~elements table2))
			(useful (find-if-not #'(lambda (x)
						 (rcl=check-for-illegal-mappings x elements2 vars))
					     results))
			(mapping (when useful
				   (rcl=produce-isomorphism-mapping useful elements2 (rcl~modulo table2) variables))))
		   (values (if mapping t nil) mapping))))))
  (:method ((table1 rcl+multiplication-table) (table2 rcl+product-multiplication-table))
	   (when (rcl=check-isomorphism-prerequisites table1 table2)
	   (let* ((tables2 (rcl~single-tables table2))
		  (solutions 
		   (mapcar #'(lambda (tab2) (multiple-value-list (rcl=solve-equation-system table1 tab2)))
			   tables2))
		  (variables (mapcar #'second solutions))
		  (results (mapcar #'(lambda (sol)
				       (rcl=split2single-solutions (rcl=dissect-maple-hint (car sol))))
				   solutions))
		  (elements (mapcar #'rcl~elements (rcl~single-tables table2)))
		  (elements2 (mapcar #'rcl=occuring-elements (rcl~single-tables table2)))
		  (useful (mapcar #'(lambda (x y z)
				      (mapcan #'(lambda (sol)
						  (unless (rcl=check-for-illegal-pair-mappings sol y z)
						    (list sol)))
					      x))
				  results elements2 variables))
		  (modulo (rcl~modulo table2))
                  (final-results (when (notany #'null useful)
				   (mapcar #'rcl=complete-pair-mappings
					   (mapcar #'rcl=produce-isomorphism-mapping2pairs-rec
						   useful elements2 modulo variables)
					   (mapcar #'(lambda (list) (mapcar #'car list)) variables)
					   elements)))
		  (mapping (when (and final-results (notany #'null final-results))
			     (rcl=test-tuple-mappings final-results (rcl~elements table2)))))
	     (values (if mapping t nil) mapping)))))

(defun rcl=complete-pair-mappings (mappings domain codomain)
  (declare (edited  "24-JUL-2000")
	   (authors Sorge)
	   (input   "List of lists mappings between elements, a modulo factor, and the domain and codomain for mappings.")
	   (effect  "None.")
	   (value   "A list of mappings between elements, where the mappings are completed with respect to all"
		    "possible variable mappings."))
  (when mappings
    (append
     (rcl=complete-single-mappings (car mappings) domain codomain)
     (rcl=complete-pair-mappings (cdr mappings) domain codomain))))


(defun rcl=complete-single-mappings (mapping domain codomain)
  (declare (edited  "24-JUL-2000")
	   (authors Sorge)
	   (input   "A list of mappings between elements, a modulo factor, and the domain and codomain for the mapping.")
	   (effect  "None.")
	   (value   "A list of mappings between elements, where the mappings are completed with respect to all"
		    "possible variable mappings."))
  (let* ((number (/ (length domain) (length codomain)))
	 (existing-domain (mapcar #'car mapping))
	 (rest-domain (set-difference domain existing-domain))
	 (existing-codomain (mapcar #'cadr mapping))
	 (codomain-occur (mapcar #'(lambda (x) (cons x (count x existing-codomain))) codomain))
	 (rest-codomain (mapcar #'(lambda (x) (cons (car x) (- number (cdr x)))) codomain-occur))
	 )
    (mapcar #'(lambda (new-codom)
		(append mapping (mapcar #'list rest-domain new-codom)))
	    (rcl=permute-codomain-elements rest-codomain))))
  

(defun rcl=permute-codomain-elements (alist)
  (declare (edited  "25-SEP-2000")
	   (authors Sorge)
	   (input   "An association list of codomain elements and number of occurrences.")
	   (effect  "None.")
	   (value   "A list of lists permuting the codomain elements wrt. the number of occurrences.")
	   (example "Input:  '((0 . 1) (1 . 2))"
		    "Output: '((0 1 1) (1 0 1) (1 1 0))"))
  (let ((first-element (mapcan #'(lambda (elem)
				   (make-list (cdr elem) :initial-element (car elem)))
			       alist)))
    (remove-duplicates (rcl=permute-elements-in-list first-element) :test #'equal)))

(defun rcl=permute-elements-in-list (list)
  (declare (edited  "25-SEP-2000")
	   (authors Sorge)
	   (input   "A list.")
	   (effect  "None.")
	   (value   "A list containing all possible permutations of the elements of the given list.")
	   (example "Input:  '((0 1 1))"
		    "Output: '((0 1 1) (1 0 1) (1 1 0))"))
  (cond ((and (cdr list) (every #'(lambda (x) (equal (car list) x)) list))
	 (list list))
	((cdr list)
	 (do* ((pos 0 (1+ pos))
	       (restlist list (cdr restlist))
	       (flist (car list) (car restlist))
	       (result))
	     ((null restlist) result)
	   (setf result
		 (remove-duplicates
		  (append result
			  (mapcar #'(lambda (x) (cons flist x))
				  (rcl=permute-elements-in-list (append (subseq list 0 pos)
									(subseq list (1+ pos))))))
		  :test #'equal))))
	(t (list list))))
  

(defun rcl=occuring-elements (table)
  (declare (edited  "22-SEP-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "None.")
	   (value   "All elements of the multiplication table that actually occur in the table."))
  (remove-duplicates (apply #'append (rcl~table table))))

(defun rcl=test-tuple-mappings (mappings elements)
  (declare (edited  "26-JUL-2000")
	   (authors Sorge)
	   (input   "A list of lists of mappings and a list of elements.")
	   (effect  "None.")
	   (value   "An isomorphism mapping from the members of mappings to elements if one exits. O/w NIL."))
  (labels ((recurse-mappings (maps acc)
			     (if maps
				 (do* ((single-maps (car maps) (cdr single-maps))
				       (single-map (car single-maps) (car single-maps))
				       (result))
				     ((or (null single-map) result) result)
				   (setf result (recurse-mappings (cdr maps)
								  (append acc (list single-map)))))
			       (let ((tuple-mappings (rcl=construct-tuple-mappings (car acc) (cdr acc))))
				 (when (rcl=check-mapping-validity tuple-mappings elements)
				   tuple-mappings)))))
    (recurse-mappings mappings nil)))

(defun rcl=check-mapping-validity (mapping elements)
  (declare (edited  "26-JUL-2000")
	   (authors Sorge)
	   (input   "A list of tuple-mappings and a list of tuples.")
	   (effect  "None.")
	   (value   "T if the mapping is valid, i.e. all members of elements are in the codomain of mappings."))
  (cond ((and mapping (not elements)) nil)
	((and elements (not mapping)) nil)
	((and (null mapping) (null elements)) t)
	((find (car elements) mapping :test #'equal :key #'second)
	 (rcl=check-mapping-validity (remove (car elements) mapping
					     :test #'equal :key #'second)
				     (cdr elements)))))

(defun rcl=produce-isomorphism-mapping2pairs-rec (results elements mod2 mappings)
  (declare (edited  "24-JUL-2000")
	   (authors Sorge)
	   (input   "List of lists containing results, elements, modulo factors, and variable mappings.")
	   (effect  "None.")
	   (value   "A list containing lists of mapping from the elements of mappings to the"
		    "given elements as determined by results."))
  (when results
    (append
     (rcl=produce-isomorphism-mapping2pairs (car results) elements mod2 mappings)
     (rcl=produce-isomorphism-mapping2pairs-rec (cdr results) elements mod2 mappings))))

(defun rcl=produce-isomorphism-mapping2pairs (results elements mod2 mappings)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "A list of results, a list of elements, a list of modulo factods,"
		    "a list of variable mappings, and a list of old mappings.")
	   (effect  "None.")
	   (value   "A list containing lists of mappings from the elements of mappings to the"
		    "given elements as determined by results."))
  (let* ((variables (mapcar #'cdr mappings)))
    (multiple-value-bind (map1 res1)
	(rcl=find-variables2numerals results)
      (if res1
	  (let* ((new-res1 (rcl=transform-result-functions res1 map1 variables))
		 (new-res2 (unless (rcl=parse-error-p new-res1)
			     (rcl=extract-implicit-assignments new-res1 map1 variables))))
	    (when new-res2
	      (mapcar #'(lambda (rest) (rcl=map-elements2elements-pair mappings rest))
		      (remove-duplicates (rcl=compute-rest-elements2pairs map1 mod2 new-res2 elements)
					 :test #'equal))))
	(list (rcl=map-elements2elements-pair mappings map1))))))

(defun rcl=compute-rest-elements2pairs (elem-map modulo func-maps elements2)
  (declare (edited  "26-JUN-2000")
	   (authors Sorge)
	   (input   "A list mapping variables to integers, an integer modulo factor,"
		    "a list mappings varibles to functions, a list of integers.")
	   (effect  "None.")
	   (value   "A list mapping variables to integers. The list is the completion of the input element"
		    "list with element mappings that take are computed from the function mappings."
		    "Thereby a mapping is a distribution for a possible isomorphism mapping on pairs."))
  (let* ((old-numbers (mapcar #'cdr elem-map))
	 (variables (tac~set-manage-list #'union (mapcar #'third func-maps)))
	 (permutations (rcl=combine-lists-permute-all variables (rcl=make-element-list modulo t)))
	 )
    (flet ((apply-function (func-map perm)
			   (let ((arguments (mapcar #'(lambda (x)
							(or (cdr (assoc x perm :test #'string-equal))
							    (cdr (assoc x elem-map :test #'string-equal))))
						    (third func-map)))
				 (function (second func-map)))
			     (apply function arguments)))
	   (legal-test (result)
		       (let* ((numbers (mapcar #'cdr result))
			      (all-numbers (append old-numbers numbers))
			      (divisor (/ (length all-numbers) (length elements2))))
			 (and 
			  (every #'(lambda (x) (find x elements2)) numbers)
			  (every #'(lambda (x) (= divisor (count x all-numbers))) elements2)))))
      (mapcan #'(lambda (perm)
		  (let ((result (mapcar #'(lambda (fm)
					    (cons (car fm)
						  (mod (apply-function fm perm) modulo)))
					func-maps)))
		    (when (legal-test result) (list (append elem-map result)))))
	      permutations))))

(defun rcl=construct-tuple-mappings (map mappings)
  (declare (edited  "23-JUL-2000")
	   (authors Sorge)
	   (input   "A list mappings elements to single tuple elements, and a list of such mappings.")
	   (effect  "None.")
	   (value   "A mapping that maps elements to tuples.")
	   (example "INPUT:  '(((1 2) 1) ((1 1) 2) ((1 0) 0) ((0 2) 1) ((0 1) 2) ((0 0) 0))"
		    "        '((((1 2) 1) ((1 1) 1) ((1 0) 1) ((0 2) 0) ((0 1) 0) ((0 0) 0))))"
		    "OUTPUT: (((1 2) (1 1)) ((1 1) (2 1)) ((1 0) (0 1)) ((0 2) (1 0)) ((0 1) (2 0)) ((0 0) (0 0)))"))
  (when map
    (let* ((key (caar map))
	   (felem (cadar map))
	   (tuple-list (mapcar #'(lambda (mapping)
				   (find key mapping :test #'equal :key #'car))
			       mappings))
	   (rest-lists (mapcar #'(lambda (mapping)
				   (remove key mapping :test #'equal :key #'car))
			       mappings)))
      (cons (list key (cons felem (mapcar #'second tuple-list)))
	    (rcl=construct-tuple-mappings (cdr map) rest-lists)))))

(defun rcl=extract-implicit-assignments (function-a numeral-a variables)
  (declare (edited  "23-JUL-2000")
	   (authors Sorge)
	   (input   "A list assigning variables to functions, an assoc-list assigning variables to numerals,"
		    "and a list of occuring variables.")
	   (effect  "Can call MAPLE if necessary.")
	   (value   "An assignment of variables to functions that were implicitely given in the input assignment.")
	   (example "INPUT:  '((\"x01\" (LAMBDA (X10 X11) (+ X10 X11)) (X10 X11) (X10 X11)))"
		    "        '((\"x00\" . 0))"
		    "        '(\"x11\" \"x10\" \"x01\" \"x00\")"
		    "OUTPUT: '((\"x11\" (LAMBDA (X10 X01) (- X01 X10)) (X10 X01) (X10 X01))"
		    "          (\"x10\" (LAMBDA (X11 X01) (- X01 X11)) (X10 X11) (X10 X11)))"))
  (let ((diff (set-difference variables (mapcar #'car (append function-a numeral-a)) :test #'string-equal)))
    (if diff
      (labels ((eq-system1 (functions variables)
			  (when (and functions variables)
			    (if (car functions)
				(cons (format nil "~((= ~A ~A)~)" (caar functions)
					      (rcl=operation2post (third (second (car functions)))))
				      (eq-system1 (cdr functions) (cdr variables)))
			      (omega~warn ";;; RCL=EXTRACT-IMPLICIT-ASSIGNMENTS: The variable ~A could not be assigned." (car diff)))))
	       (eq-system2 (functions)
			   (when functions
			     (cons (format nil "~((= ~A ~A)~)" (caar functions)
					   (rcl=operation2post (third (second (car functions)))))
				   (eq-system2 (cdr functions))))))
	(let* ((functions (mapcar #'(lambda (d)
				      (find-if
				       #'(lambda (x) (find d (third x) :test #'string-equal))
				       function-a))
				  diff))
	       (others (set-difference function-a functions :test #'equal))
	       (equations (append (eq-system1 functions diff) (eq-system2 functions)))
	       (vars (append diff (mapcar #'car functions)))
	       (result1 (rcl~call-maple
			 (list "solve"
			       (format nil "{~{~A~^, ~}}" equations)
			       (format nil "{~{~A~^, ~}}" vars))
			 :syntax :post2maple))
	       (repl-assoc)
	       (result2 (mapcar #'(lambda (x) (if (string-equal (car x) (cadr x))
						  (let ((old-name (car x))
							(new-name (format nil "~A" (gensym "_NN"))))
						    (pushnew (cons (read-from-string old-name)
								   (read-from-string new-name)) repl-assoc)
						    (list old-name new-name))
						x))
				(rcl=dissect-maple-hint result1))))
	(rcl=substitute-tree (append others (rcl=transform-result-functions result2 numeral-a variables)) repl-assoc)))
      function-a)))

(defun rcl=substitute-tree (tree alist)
  (declare (edited  "24-JUL-2000")
	   (authors Sorge)
	   (input   "A nested list and an association list.")
	   (effect  "None.")
	   (value   "Substitutes elements in the tree with the corresponding elements in the assoc-list."))
  (cond ((null tree) nil)
	((atom tree) (let ((elem (assoc tree alist :test #'equal)))
		       (if elem (cdr elem) tree)))
	(t (cons (rcl=substitute-tree (car tree) alist)
		 (rcl=substitute-tree (cdr tree) alist)))))


;;; Stuff for isomorphism between simple residue class sets

(defun rcl=produce-isomorphism-mapping (results elements mod2 mappings)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "A list of results, a list of elements of the codomain, a list of variable mappings.")
	   (effect  "None.")
	   (value   "A list containing a mapping from the elements of mappings to the"
		    "given elements as determined by results."))
  (let* ((variables (mapcar #'cdr mappings)))
    (multiple-value-bind (map1 res1)
	(rcl=find-variables2numerals results)
      (if res1
	  (let ((new-res1 (rcl=transform-result-functions res1 map1 variables)))
	    (when new-res1
	      (rcl=map-elements2elements
	       mappings
	       (rcl=compute-rest-elements map1 mod2 new-res1 elements)
	       elements)))
	(rcl=map-elements2elements mappings map1 elements)))))

(defun rcl=compute-rest-elements (elem-map modulo func-maps elements2)
  (declare (edited  "26-JUN-2000")
	   (authors Sorge)
	   (input   "A list mapping variables to integers, an integer modulo factor,"
		    "a list mappings varibles to functions, and one lists of integers representing the codomain.")
	   (effect  "None.")
	   (value   "A list mapping variables to integers."))
  (let* ((old-numbers (mapcar #'cdr elem-map))
	 (new-numbers (set-difference elements2 old-numbers))
	 (variables (tac~set-manage-list #'union (mapcar #'third func-maps)))
	 (permutations (rcl=combine-lists-permute variables (rcl=make-element-list modulo t)))
	 )
    (flet ((apply-function (func-map perm)
			   (let ((arguments (mapcar #'(lambda (x)
							(or (cdr (assoc x perm :test #'string-equal))
							    (cdr (assoc x elem-map :test #'string-equal))))
						    (third func-map)))
				 (function (second func-map)))
			     (apply function arguments)))
	   (legal-test (result)
		       (let ((numbers (mapcar #'cdr result)))
			 (and (= (length numbers) (length (remove-duplicates numbers)))
			      (every #'(lambda (x) (find x new-numbers)) numbers)
			      (notany #'(lambda (x) (find x old-numbers)) numbers)))))
      (dolist (perm permutations)
	(let ((result (mapcar #'(lambda (fm)
				  (cons (car fm)
					(mod (apply-function fm perm) modulo)))
			      func-maps)))
	  (when (legal-test result)
	    (return (append elem-map result)))))
    )))

(defun rcl=combine-lists-bc (list1 list2)
  (declare (edited  "27-JUN-2000")
	   (authors Sorge)
	   (input   "Two lists.")
	   (effect  "None.")
	   (value   "A list of association lists representing the all possible pairs distributed wrt. binomial coefficient."
		    "Example: (a b) (1 2 3) becomes (((a . 1) (b . 2)) ((a . 1) (b . 3)) ((a . 2) (b . 3)))"))
  (cond ((> (length list1) (length list2)) nil)
	((null list1) nil)
	((null (cdr list1)) (mapcar #'(lambda (x) (list (cons (car list1) x))) list2))
	(t (append
	    (mapcar #'(lambda (x) (acons (car list1) (car list2) x))
		    (rcl=combine-lists-bc (cdr list1) (cdr list2)))
	    (rcl=combine-lists-bc list1 (cdr list2))))))

(defun rcl=combine-lists-permute (list1 list2)
  (declare (edited  "27-JUN-2000")
	   (authors Sorge)
	   (input   "Two lists.")
	   (effect  "None.")
	   (value   "A list of association lists representing the all possible pair permutations."
		    "Example: (a b) (1 2 3) becomes (((a . 1) (b . 2)) ((a . 1) (b . 3))"
		    "((a . 2) (b . 1)) ((a . 2) (b . 3)) ((a . 3) (b . 1)) ((a . 3) (b . 2)))"))
  (labels ((permute-list (list lists)
			 (when list
			   (append (mapcan #'(lambda (x) (unless (find (car list) x :test #'equal)
							   (list (cons (car list) x))))
					   lists)
				   (permute-list (cdr list) lists))))
	   (permute-elements (list n)
			     (if (and list (> n 1))
				 (let ((result (permute-elements list (1- n))))
				   (permute-list list result))
			       (mapcar #'list list))))
    (unless (> (length list1) (length list2))
      (mapcar #'(lambda (perm)
		  (mapcar #'cons list1 perm))
	      (permute-elements list2 (length list1))))))
	    
(defun rcl=combine-lists-permute-all (list1 list2)
  (declare (edited  "27-JUN-2000")
	   (authors Sorge)
	   (input   "Two lists.")
	   (effect  "None.")
	   (value   "A list of association lists representing the all possible pairs."
		    "Example: (a b) (1 2) becomes (((a . 1) (b . 1)) ((a . 1) (b . 2))"
		    "((a . 2) (b . 1)) ((a . 2) (b . 2)))"))
    (mapcar #'(lambda (perm)
		(mapcar #'cons list1 perm))
	    (rcl=make-product-elements (make-list (length list1) :initial-element list2))))
	    
(defun rcl=map-elements2elements (map1 map2 &optional freedom degree)
  (if (and map1 map2)
      (let* ((fem (car map1))
	     (sem (assoc (cdr fem) map2 :test #'string-equal)))
	(if sem
	    (cons (list (car fem) (cdr sem))
		  (rcl=map-elements2elements (cdr map1) map2 (remove (cdr sem) freedom) degree))
	  (rcl=map-elements2elements (cdr map1) map2 freedom (cons (car fem) degree))))
    (if (= (length freedom) (length degree))
	(mapcar #'(lambda (x y) (list x y)) degree freedom)
      (omega~warn ";;;RCL=MAP-ELEMENTS2ELEMENTS: Something went wrong with the degree ~A of freedom ~A when mapping elements."
		  degree freedom))))

;(defun rcl=map-elements2elements-pair (map1 map2 &optional freedom degree)
;  (if (and map1 map2)
;      (let* ((fem (car map1))
;             (sem (assoc (cdr fem) map2 :test #'string-equal)))
;        (if sem
;            (cons (list (car fem) (cdr sem))
;                  (rcl=map-elements2elements-pair (cdr map1) map2 (remove (cdr sem) freedom) degree))
;          (rcl=map-elements2elements-pair (cdr map1) map2 freedom (cons (car fem) degree))))
;    (format t "~%Freedom: ~A   Degree: ~A" freedom degree)
;    ))

(defun rcl=map-elements2elements-pair (map1 map2)
  (when (and map1 map2)
    (let* ((fem (car map1))
	   (sem (assoc (cdr fem) map2 :test #'string-equal)))
      (if sem
	  (cons (list (car fem) (cdr sem))
		(rcl=map-elements2elements-pair (cdr map1) map2))
	(rcl=map-elements2elements-pair (cdr map1) map2)))))

#+old(defun rcl=split2single-solutions2 (solutions number)
  (declare (edited  "26-JUN-2000")
	   (authors Sorge)
	   (input   "A list of solutions and an integers.")
	   (effect  "None.")
	   (value   "A list of lists of solutions where each single list is of length NUMBER."))
  (flet ((clear-solution (sol)
			 (list
			  (string-trim '(#\{ #\} #\SPACE) (car sol))
			  (string-trim '(#\{ #\} #\SPACE) (cadr sol)))))
    (when solutions
      (cons (mapcar #'clear-solution (subseq solutions 0 number)) 
	    (rcl=split2single-solutions (subseq solutions number) number)))))

(defun rcl=split2single-solutions (solutions)
  (declare (edited  "26-JUN-2000")
	   (authors Sorge)
	   (input   "A list of solutions.")
	   (effect  "None.")
	   (value   "A list of lists of solutions where each single list is of length NUMBER."))
  (flet ((clear-solution (sol)
			 (list
			  (string-trim '(#\{ #\} #\SPACE) (car sol))
			  (string-trim '(#\{ #\} #\SPACE) (cadr sol)))))
    (when solutions
      (let ((number (position-if #'(lambda (x) (not (string-equal x (string-right-trim '(#\}) x))))
				 solutions :key #'second)))
	(if number
	    (cons (mapcar #'clear-solution (subseq solutions 0 (1+ number)))
		  (rcl=split2single-solutions (subseq solutions (1+ number))))
	  (list (mapcar #'clear-solution solutions)))))))

(defun rcl=transform-result-functions (result mapping variables)
  (declare (edited  "08-JUN-2000")
	   (authors Sorge)
	   (input   "A list of results, a mapping of variables to numerals, and a list of variables.")
	   (effect  "Calls the Maple2POST parser.")
	   (value   "A mapping for the resultants."))
  (when result
    (let* ((fres (first result))
	   (operation (rcl~maple2post (cadr fres))))
      (if (rcl=parse-error-p operation) operation
	(multiple-value-bind (op vars)
	    (rcl=post2operation operation mapping)
	  (multiple-value-bind (var1 var2)
	      (rcl=separate-variables vars variables)
	    (let ((new-results (rcl=transform-result-functions (cdr result) mapping variables)))
	      (if (rcl=parse-error-p new-results) new-results
		(cons (list (car fres)
			    (rcl=construct-lambda-expr op (append var1 var2))
			    (append var1 var2)
			    var2)
		      new-results)))))))))

(defun rcl=parse-error-p (string)
  (and (stringp string)
       (string-equal "Error: parse error" string)))

(defgeneric rcl=construct-lambda-expr (operation variables)
  (declare (edited  "09-JUN-2000")
	   (authors Sorge)
	   (input   "An operation and a list of variables.")
	   (effect  "None.")
	   (value   "A funcallable lambda expression."))
  (:method ((operation string) (variables list))
	   (rcl=construct-lambda-expr (read-from-string operation) variables))
  (:method (operation (variables list))
	   (let ((real-vars (mapcar #'(lambda (var)
					(etypecase var
					  (symbol var)
					  (string (read-from-string var))))
				    variables)))
	     (list 'lambda real-vars operation))))

(defun rcl=separate-variables (list1 list2)
  (declare (edited  "09-JUN-2000")
	   (authors Sorge)
	   (input   "Two lists of strings.")
	   (effect  "None.")
	   (value   "Two values: a list of elements of list1 that do not occur in list2"
		    "and list of elements that do occur in list1."))
  (values
   (set-difference list1 list2 :test #'string-equal)
   (intersection list1 list2 :test #'string-equal)))

(defun rcl=find-variables2numerals (result)
  (declare (edited  "08-JUN-2000")
	   (authors Sorge)
	   (input   "A list of pairs, consisting of variables and results.")
	   (effect  "None.")
	   (value   "Two values:"
		    "1. A list of pairs mapping variables to numbers."
		    "2. The remaining results."))
  (when result
    (multiple-value-bind (map res)
	(rcl=find-variables2numerals (cdr result))
      (let ((resultant (read-from-string (cadar result))))
	(if (numberp resultant)
	    (values (acons (caar result) resultant map) res)
	  (values map (cons (list (caar result) resultant) res))
  )))))

(defun rcl=check-for-illegal-pair-mappings (reslist elements variables &optional occuring)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "A list of results, a list of elements, a list of variable names,"
		    "and a list of elements occuring in the results.")
	   (effect  "None.")
	   (value   "NIL if all the result is useful to produce a pair isomorphism, the disturbing element otherwise."))
  (cond (reslist
	 (let* ((res (cadar reslist))
		(real-res (read-from-string res)))
	   (cond ;;((find res occuring :test #'string-equal) res)
	    ((and (numberp real-res) (not (find real-res elements :test #'=))) res)
	    ((find res occuring :test #'string-equal)
	     (rcl=check-for-illegal-pair-mappings (cdr reslist) elements variables occuring))
	    (t (rcl=check-for-illegal-pair-mappings (cdr reslist) elements variables (cons res occuring))))))
	((not (>= (length occuring) (length elements)))
	 (set-difference elements (mapcar #'read-from-string occuring) :test #'equal))))
  
(defun rcl=check-for-illegal-mappings (reslist elements variables &optional occuring)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "A list of results, a list of elements, a list of variable names,"
		    "and a list of elements occuring in the results.")
	   (effect  "None.")
	   (value   "NIL if all the result is useful to produce an isomorphism, the disturbing element otherwise."))
  (when reslist
    (let* ((res (cadar reslist))
	   (real-res (read-from-string res)))
      (cond ((find res occuring :test #'string-equal) res)
	    ((and (numberp real-res) (not (find real-res elements :test #'=))) res)
	    (t (rcl=check-for-illegal-mappings (cdr reslist) elements variables
					       (if (or (numberp real-res) (find res variables :test #'string-equal))
						   (cons res occuring)
						 occuring)))))))
  
(defgeneric rcl=construct-variable (element &optional (var-name "x"))
  (declare (edited  "21-JUL-2000")
	   (authors Sorge)
	   (input   "An element of a residue class set, i.e. a number or a tuple.")
	   (effect  "None.")
	   (value   "A string containing an appropriate variable name."))
  (:method ((element number) &optional (var-name "x"))
	   (format nil "~A~A" var-name element))
  (:method (element &optional (var-name "x"))
	   (format nil "~A~A" var-name element))
  (:method ((element cons) &optional (var-name "x"))
	   (format nil "~A~{~A~}" var-name element)))

  
(defun rcl=solve-equation-system (table1 table2)
  (declare (edited  "06-JUN-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables.")
	   (effect  "Calls Maple.")
	   (value   "Two values:"
		    "1 -- A string containing the result of solving the isomorphism equation system with MAPLE."
		    "2 -- An association list mapping the elements of the first table to variables in the solution."))
  (let* ((elem1 (rcl~elements table1))
	 (variables (pairlis elem1 (mapcar #'rcl=construct-variable elem1)))
	 (factor (rcl~modulo table2))
	 (equations (rcl=construct-equations table1 table2 variables)))
    (values
     (rcl~call-maple (list "msolve" equations (write-to-string factor))
		     :syntax :maple2mapleallsolutions)
     variables)))

(defun rcl=construct-equations (table1 table2 variables)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables and an assoc list.")
	   (effect  "Calls the POST2Maple parser.")
	   (value   "A list of equations in Maple syntax."))
  (let* ((pairs (rcl=permute2pairs (rcl~elements table1)))
	 (lhs (rcl=construct-equation-lhs (rcl~operation table1) pairs variables (rcl~modulo table1)))
	 (rhs (rcl=construct-equation-rhs (rcl~operation table2) pairs variables))
	 (post-equations (mapcar #'(lambda (l r) (concatenate 'string "(= " l " " r ")")) lhs rhs)))
    (rcl~post2maple (format nil "{~{~A~^, ~}}" post-equations))))

(defun rcl=construct-equation-rhs (operation pairs variables)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "An operation, a list of pairs, and an assoc list.")
	   (effect  "None.")
	   (value   "A list of right-hand-sides for the isomorphism-equations."))
  (flet ((get-vars (list)
		   (mapcar #'(lambda (x) (read-from-string (cdr (assoc x variables :test #'equal)))) list)))
    (multiple-value-bind (a1 a2 a3)
	(function-lambda-expression operation)
      (declare (ignore a2))
      (if a1
	  (let* ((var-list (second a1))
		 (op (third a1)))
	    (mapcar #'(lambda (pair)
			(string-downcase
			 (write-to-string
			  (rcl=operation2post op (pairlis var-list (get-vars pair))))))
		    pairs))
	(let ((new-operation (rcl=operation2post a3)))
	  (mapcar #'(lambda (pair)
		      (string-downcase (write-to-string (cons new-operation (get-vars pair)))))
		  pairs))))))

(defun rcl=construct-equation-lhs (operation pairs variables &optional modulo)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "An operation, a list of pairs, an assoc list, and optionally an integer.")
	   (effect  "None.")
	   (value   "A list of left-hand-sides for the isomorphism-equations."))
  (let ((results (mapcar #'(lambda (pair) (rcl=apply-operation pair operation modulo)) pairs)))
    (mapcar #'(lambda (element)
		(cdr (assoc element variables :test #'equal)))
	    results)))

;;; currently unused begin
	       
(defun rcl=find-position-in-size-list (position size-list)
  (declare (edited  "24-JUL-2000")
	   (authors Sorge)
	   (input   "An integer and a nested integer list.")
	   (effect  "None.")
	   (value   "A list containing the previous positions in the size list."))
  (let ((pos-list (find-if (lambda (x) (find position x)) size-list)))
    (subseq pos-list 0 (position position pos-list))))

(defun rcl=group-same-sized-tables (tables)
  (declare (edited  "24-JUL-2000")
	   (authors Sorge)
	   (input   "A list of multiplication tables.")
	   (effect  "None.")
	   (value   "A list of integers indicating positions of tables in the list having the same size."))
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((insert-new-table (table position)
			       (let* ((len (length (rcl~elements table)))
				      (key (gethash len hash)))
				 (if key
				     (setf (gethash len hash) (push position key))
				   (setf (gethash len hash) (list position)))))
	     (group-tables (tables position)
			   (when tables
			     (insert-new-table (car tables) position)
			     (group-tables (cdr tables) (1+ position)))))
      (group-tables tables 0))
    (mapcar #'reverse (ohlp~hash2list hash t))))

(defun rcl=check-isomorphism (table1 table2)
  (declare (edited  "30-JUN-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables")
	   (effect  "None.")
	   (value   "A list of pairs mapping the element of the first table to those of the second"
		    "if an isomorphism between the two tables exists."))
  (multiple-value-bind (solution variables)
      (rcl=solve-equation-system table1 table2)
    (when solution
      (let* ((vars (mapcar #'cdr variables))
	     (results (rcl=split2single-solutions (rcl=dissect-maple-hint solution)))
	     (elements2 (rcl~elements table2))
	     (useful (find-if-not #'(lambda (x)
				      (rcl=check-for-illegal-mappings x elements2 vars))
				  results)))
	(when useful
	  (rcl=produce-isomorphism-mapping useful elements2 (rcl~modulo table2) variables))))))

;;; unused end  

;;
;; Testing isomorphisms:
;;
;; (setq m1 (rcl~multiplication-table 3 3 :operation #'+ :zero t))
;; (setq m2 (rcl~multiplication-table 3 3 :operation #'(lambda (x y) (+ (+ x y) 1)) :zero t))
;;
;; (setq m3 (rcl~multiplication-table 2 2 :operation #'+ :zero t))
;; (setq m4 (rcl~multiplication-table 2 2 :operation #'(lambda (x y) (+ (+ x y) 1)) :zero t))
;; (setq m5 (rcl~multiplication-table '(0 2) 4 :operation #'+ :zero t))
;;
;; (setq m6 (rcl~multiplication-table 4 4 :operation #'+ :zero t))
;; (setq m7 (rcl~multiplication-table 5 5 :operation #'* ))
;;
;; (setq m8 (rcl~product-multiplication-table '(2 3) :operation (list #'+ #'+) :zero '(t t) :modulo '(2 3)))
;; (setq m9 (rcl~product-multiplication-table '(3 2) :operation (list #'+ #'+) :zero '(t t) :modulo '(3 2)))
;; (setq m10 (rcl~multiplication-table 6 6 :zero t))
;; (setq m11 (rcl~product-multiplication-table '((0 2) 3) :operation (list #'+ #'+) :zero '(t t) :modulo '(4 3)))
;;
;; (setq m12 (rcl~multiplication-table 4 4 :zero t))
;; (setq m13 (rcl~product-multiplication-table '(2 2) :operation (list #'+ #'+) :zero '(t t) :modulo '(2 2)))
;; (setq m14 (rcl~product-multiplication-table '((0 2) 2) :operation (list #'+ #'+) :zero '(t t) :modulo '(4 2)))
;; (setq m15 (rcl~product-multiplication-table '(2 (0 2)) :operation (list #'+ #'+) :zero '(t t) :modulo '(2 4)))
;;
;; (setq m16 (rcl~product-multiplication-table '(4 3) :operation (list #'+ #'+) :zero '(t t) :modulo '(4 3)))
;; (setq m17 (rcl~product-multiplication-table '(5 3) :operation (list #'* #'(lambda (x y) (+ (+ x y) 1))) :zero '(nil t) :modulo '(5 3)))
;; (setq m18 (rcl~product-multiplication-table '(3 4) :operation (list #'+ #'+) :zero '(t t) :modulo '(3 4)))
;;
;; (setq m19 (rcl~product-multiplication-table '(5 3 4) :operation (list #'* #'+ #'+) :zero '(nil t t) :modulo '(5 3 4)))
;; (setq m20 (rcl~product-multiplication-table '(3 4 5) :operation (list #'+ #'+ #'*) :zero '(t t nil) :modulo '(3 4 5)))
;; (setq m19a (rcl~product-multiplication-table '(5 3 2) :operation (list #'* #'+ #'+) :zero '(nil t t) :modulo '(5 3 2)))
;; (setq m20a (rcl~product-multiplication-table '(3 2 5) :operation (list #'+ #'+ #'*) :zero '(t t nil) :modulo '(3 2 5)))
;;; m19, m20, m19a, m20a don't work due to maple....
;;
;; (setq m21 (rcl~product-multiplication-table '(2 3 2) :operation (list #'+ #'* #'(lambda (x y) (+ (+ x y) 1))) :zero '(t nil t) :modulo '(2 3 2)))
;; (setq m22 (rcl~product-multiplication-table '(2 2 3) :operation (list #'(lambda (x y) (+ (+ x y) 1)) #'+ #'*) :zero '(t t nil) :modulo '(2 2 3)))
;;
;; (setq m23 (rcl~product-multiplication-table '(4 3 2) :operation (list #'+ #'* #'(lambda (x y) (+ (+ x y) 1))) :zero '(t nil t) :modulo '(4 3 2)))
;; (setq m24 (rcl~product-multiplication-table '(2 4 3) :operation (list #'(lambda (x y) (+ (+ x y) 1)) #'+ #'*) :zero '(t t nil) :modulo '(2 4 3)))
;;
;; (setq m25 (rcl~product-multiplication-table '(4 4 2) :operation (list #'+ #'+ #'+) :zero '(t t t) :modulo '(4 4 2)))
;; (setq m26 (rcl~product-multiplication-table '(2 4 4) :operation (list #'+ #'+ #'+) :zero '(t t t) :modulo '(2 4 4)))
;;
;; (setq m27 (rcl~product-multiplication-table '(5 4 2) :operation (list #'+ #'+ #'+) :zero '(t t t) :modulo '(5 4 2)))
;; (setq m28 (rcl~product-multiplication-table '(2 4 5) :operation (list #'+ #'+ #'+) :zero '(t t t) :modulo '(2 4 5)))
;; 
;; (setq m29 (rcl~product-multiplication-table '(6 4 2) :operation (list #'+ #'+ #'+) :zero '(t t t) :modulo '(6 4 2)))
;; (setq m30 (rcl~product-multiplication-table '(2 4 6) :operation (list #'+ #'+ #'+) :zero '(t t t) :modulo '(2 4 6)))
;;
;; (setq m31 (rcl~product-multiplication-table '(6 4) :operation (list #'(lambda (x y) 1) #'+) :zero '(t t) :modulo '(6 4)))
;; (setq m32 (rcl~product-multiplication-table '(4 6) :operation (list #'+ #'(lambda (x y) 0)) :zero '(t t) :modulo '(4 6)))
;; (setq m33 (rcl~product-multiplication-table '(2 2) :operation (list #'(lambda (x y) 1) #'+) :zero '(t t) :modulo '(2 2)))
;; (setq m34 (rcl~product-multiplication-table '(2 2) :operation (list #'+ #'(lambda (x y) 0)) :zero '(t t) :modulo '(2 2)))
;; (setq m35 (rcl~product-multiplication-table '(2 2) :operation (list #'+ #'(lambda (x y) 1)) :zero '(t t) :modulo '(2 2)))
;; (setq m36 (rcl~product-multiplication-table '(2 2) :operation (list #'(lambda (x y) 0) #'+) :zero '(t t) :modulo '(2 2)))
;; ---> (((0 0) (0 1)) ((0 1) (1 1)) ((1 0) (0 0)) ((1 1) (1 0)))
;; (setq m37 (rcl~product-multiplication-table '(2 2) :operation (list #'(lambda (x y) 1) #'(lambda (x y) 1)) :zero '(t t) :modulo '(2 2)))
;; (setq m38 (rcl~product-multiplication-table '(2 2) :operation (list #'(lambda (x y) 0) #'(lambda (x y) 0)) :zero '(t t) :modulo '(2 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff to compute closed form homomorphism mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rcl~check-closed-homomorphism (table1 table2 mapping)
  (declare (edited  "18-AUG-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables and a homomorphism mapping.")
	   (effect  "None.")
	   (value   "A list of functions that are compact representations for the homomorphism mapping"
		    "from table1 into table2."))
  (:argument-precedence-order table2 table1 mapping)
  (:method ((table1 rcl+multiplication-table) (table2 rcl+multiplication-table) (mapping list))
	   (let ((domain (mapcar #'first mapping))
		 (codomain (mapcar #'second mapping))
		 (max-order (rcl~modulo table1))
		 (modulo (rcl~modulo table2)))
	     (dotimes (order max-order)
	       (let ((function (rcl=construct-homomorphism order domain codomain modulo)))
		 (when function (return (list function)))))))
  (:method ((table1 rcl+multiplication-table) (table2 rcl+product-multiplication-table) (mapping list))
	   (let* ((single-codomains (rcl=split2single-mappings (mapcar #'second mapping)))
		  (single-domain (mapcar #'first mapping))
		  (single-mappings (mapcar #'(lambda (cod)
					       (mapcar #'(lambda (x y) (list x y))
						       single-domain cod))
					   single-codomains))
		  )
	     (mapcan #'(lambda (table map) (rcl~check-closed-homomorphism table1 table map))
		     (rcl~single-tables table2) single-mappings)))
;;;                  (functions (mapcar #'(lambda (table map) (rcl~check-closed-homomorphism table1 table map))
;;;                                     (rcl~single-tables table2) single-mappings)))
;;;             (unless (some #'null functions)
;;;               (mapcar #'car functions))))
  (:method ((table1 rcl+product-multiplication-table) (table2 rcl+multiplication-table) (mapping list))
	   (let ((domain (mapcar #'first mapping))
		 (codomain (mapcar #'second mapping))
		 (max-order (mapcar #'(lambda (perm) (mapcar #'1+ perm))
				    (rcl=construct-permutations-of-length (rcl~modulo table1))))
		 (modulo (rcl~modulo table2)))
	     (dolist (order max-order)
	       (let ((function (rcl=construct-homomorphism order domain codomain modulo)))
		 (when function (return (list function))))))))


(defun rcl=construct-permutations-of-length (length)
  (declare (edited  "19-AUG-2000")
	   (authors Sorge)
	   (input   "A list indicating the length of each permutation element.")
	   (effect  "None.")
	   (value   "A list containing permutation lists.")
	   (example "Input: '(2 3 2)"
		    "Output: '((0 0 0) (1 0 0) (0 1 0) (1 1 0) (0 2 0) (1 2 0)"
		    "          (0 0 1) (1 0 1) (0 1 1) (1 1 1) (0 2 1) (1 2 1))"))
  (if (cdr length)
      (mapcan #'(lambda (res)
		  (mapcar #'(lambda (perm)
			      (cons perm res))
			  (rcl=make-element-list (car length) t)))
	      (rcl=construct-permutations-of-length (cdr length)))
    (mapcar #'list (rcl=make-element-list (car length) t))))

(defun rcl=split2single-mappings (mappings)
  (if (cdr mappings)
      (mapcar #'cons (car mappings) (rcl=split2single-mappings (cdr mappings)))
    (mapcar #'list (car mappings))))
  

(defun rcl=construct-homomorphism (order domain codomain modulo)
  (declare (edited  "18-AUG-2000")
	   (authors Sorge)
	   (input   "An integer and two lists of integers and an integer.")
	   (effect  "None.")
	   (value   "A function representation of a homomorphism if one exists. O/w NIL."))
  (multiple-value-bind (poly vars coeffs)
      (rcl=construct-polynomial-of-order order)
    (declare (ignore coeffs))
    (let* ((maple-solutions (rcl=solve-homomorphism-equations poly (reverse vars) domain codomain modulo))
	   (real-solutions (rcl=split2single-solutions (rcl=dissect-maple-hint maple-solutions)))
	   (useful-solutions (remove-if-not #'(lambda (x)                                        ;;; currently still naive!!!
						(every #'(lambda (y) (numberp (read-from-string (second y)))) x))
					    real-solutions)))
      (when useful-solutions
	(let* ((subst-poly (rcl=substitute-tree
			    (read-from-string (rcl~maple2post poly))
			    (mapcar #'(lambda (x) (cons (read-from-string (first x))
							(read-from-string (second x))))
				    (car useful-solutions))))
	       (simpl-poly (rcl~call-maple (list "simplify" (format nil "~(~A~)" subst-poly)) :syntax 'post2post)))
	  (read-from-string
	   (if vars
	       (format nil "~{(lam (~A num) ~}(mod ~A ~A )~A"
		       (reverse vars) simpl-poly modulo (make-string (length vars) :initial-element #\)))
	     (format nil "(mod ~A ~A)" simpl-poly modulo))))))))

(defun rcl=solve-homomorphism-equations (poly vars domain codomain modulo)
  (declare (edited  "18-AUG-2000")
	   (authors Sorge)
	   (input   "A polynomial, a list of its variabls, and two lists of elements and an integer.")
	   (effect  "None.")
	   (value   "The solutions for the homomorphism equations."))
  (if vars
      (let* ((inst (mapcar #'(lambda (x) (rcl=construct-variable-instantiation vars x)) domain))
	     ;;  so sollte es aussehen, wenn mathweb mitmachte...
              (evals (mapcar #'(lambda (x) (format nil "eval( ~(~A~) , ~(~A~) )" poly x)) inst))
              (eval-polys (rcl=dissect-maple-hint
                           (rcl~call-maple (list (format nil "[~{ ~A~^ ,~}]" evals))
                                           :syntax 'maple2maple)))
	      ;; folgendes ist temporaer
;;;             (evals (mapcar #'(lambda (x) (list "eval" (string-downcase poly) (string-downcase x))) inst))
;;;             (eval-polys (mapcar #'(lambda (x) (rcl~call-maple x :syntax 'maple2maple)) evals))
	     (equations (mapcar #'(lambda (x y) (format nil "~A = ~A" x y)) eval-polys codomain))
	     (maple-equs (format nil "{~{ ~A~^ ,~}}" equations)))
	(rcl~call-maple (list "msolve" maple-equs (format nil "~D" modulo)) :syntax 'maple2mapleallsolutions))
    (let* ((equations (mapcar #'(lambda (y) (format nil "~A = ~A" poly y)) codomain))
	   (maple-equs (format nil "{~{ ~A~^ ,~}}" equations)))
      (rcl~call-maple (list "msolve" maple-equs (format nil "~D" modulo)) :syntax 'maple2mapleallsolutions)
      )))

(defgeneric rcl=construct-variable-instantiation (variables domain)
  (declare (edited  "18-AUG-2000")
	   (authors Sorge)
	   (input   "A list of variables and its instantiations.")
	   (effect  "None.")
	   (value   "The instantiation for the variables in Maple syntax."))
  (:method ((variables list) (domain list))
	   (let ((inst-list (mapcar #'(lambda (x y) (format nil "~A=~A" x y))
				    variables domain)))
	     (format nil "{~{ ~A~^ ,~}}" inst-list)))
  (:method ((variables list) domain)
	   (format nil "{ ~A=~A }" (car variables) domain)))

(defgeneric rcl=construct-polynomial-of-order (order)
  (declare (edited  "17-AUG-2000")
	   (authors Sorge)
	   (input   "A non-negative integer.")
	   (effect  "None.")
	   (value   "Three values: the polynomial, a list of its variables, and a list of coefficients."))
  (:method ((order cons))
	   (let* ((variables (rcl=make-variable-list 'x (length order)))
		  (exp-list (rcl=construct-permutations-of-length order))
		  (coeff-list (mapcar #'(lambda (mon) (rcl=construct-variable mon "a")) exp-list))
		  (poly-list (mapcar #'(lambda (mon coeff) (cons coeff mon)) exp-list coeff-list)))
	     (values
	      (with-output-to-string (str)
				     (rcl=print-polynomial poly-list str)
				     str)
	      variables (mapcar #'read-from-string coeff-list))))
  (:method ((order integer))
	   (cond ((< order 0) (omega~error "Negative of order of polynomials is not allowed."))
		 ((= order 0) (values "a0" nil (list 'a0)))
		 (t 
		  (multiple-value-bind (pol vars coeffs)
		      (rcl=construct-polynomial-of-order (1- order))
		    (declare (ignore vars))
		    (let ((coeff (format nil "a~D" order)))
		      (values
		       (concatenate 'string coeff (format nil " * x^~D + " order) pol)
		       (list 'X)
		       (cons (read-from-string coeff) coeffs))))))))

#|
	     (labels ((poly-flat (order varname &optional (coeff-number 0) (final t))
			       (cond ((< order 0) (omega~error "Negative of order of polynomials is not allowed."))
				     ((and final (= order 0))
				      (let ((var (format nil "a~D0" coeff-number)))
					(values var (list (read-from-string var)))))
				     (t 
				      (multiple-value-bind (pol coeffs)
					  (poly-flat (1- order) varname coeff-number final)
					(let ((coeff (format nil "a~D~D" coeff-number order)))
					  (values
					   (concatenate 'string coeff (format nil " * ~A + " varname) pol)
					   (cons (read-from-string coeff) coeffs)))))))
			       
		    (poly-mixed ()
				))
	     
	       (if flat
		   (do* ((ord (car order) (car rest-order))
		       (count 0 (1+ 0))
		       (rest-order (cdr order) (cdr rest-order))
		       (poly (poly-flat ord (format nil "x~D" count) count)
			     (concatenate 'string poly " + " (poly-flat ord (format nil "x~D" count) count nil))))
		     ((null rest-order) poly))
	     )
	   )))
|#
	   
(defun rcl=print-polynomial (list stream)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A list of polynomial-data")
	   (effect  "Prints the data in 'nice' form")
	   (value   "None"))
  (cond (list
	 (rcl=print-monomial (car list) stream)
	 (mapcar #'(lambda (x)
		     (format stream "+ ")
		     (rcl=print-monomial x stream))
		 (cdr list)))
	(t (format stream "0"))))

(defun rcl=print-monomial (list stream)
  (declare (edited  "28-MAR-1995")
	   (authors Sorge)
	   (input   "A list of monomial-data")
	   (effect  "Prints the data in 'nice' form")
	   (value   "None"))
  (format stream "~A " (car list))
  (do ((i 1 (1+ i))
       (x (cdr list) (cdr x)))
      ((null x))
    (cond ((= (car x) 1) (format stream "* x~D " i))
	  ((= (car x) 0) )
	  (t (format stream "* x~D^~D " i (car x))))))
    
(defgeneric rcl=make-variable-list (name number)
  (:method (name (number integer))
	   (when (> number 0) 
	     (cons (read-from-string (format nil "~A~D" name number))
		   (rcl=make-variable-list name (1- number)))))
  (:method (name (number list))
	   (mapcar #'(lambda (num) (read-from-string (format nil "~A~D" name num))) number)))
			  

#| Testing some stuff

(multiple-value-setq (dum iso1) (rcl~check-isomorphism m19a m20a))
(rcl~check-closed-homomorphism m19a m20a iso1)

result:
'((LAM (X1 NUM) (LAM (X2 NUM) (LAM (X3 NUM) (MOD (TIMES 2 X2) 3))))
  (LAM (X1 NUM) (LAM (X2 NUM) (LAM (X3 NUM) (MOD X3 2))))
  (LAM (X1 NUM)
       (LAM (X2 NUM)
	    (LAM (X3 NUM) (MOD (PLUS X1 (TIMES (TIMES 3 X1) X3)) 5)))))

(multiple-value-setq (dum iso2) (rcl~check-isomorphism m8 m10))
(rcl~check-closed-homomorphism m8 m10 iso2)

result:
'((LAM (X1 NUM) (LAM (X2 NUM) (MOD (PLUS (TIMES 3 X1) (TIMES 2 X2)) 6))))

(multiple-value-setq (dum iso3) (rcl~check-isomorphism m10 m8))
(rcl~check-closed-homomorphism m8 m10 iso3)

result:
'((LAM (X NUM) (MOD X 2))
  (LAM (X NUM) (MOD (TIMES 2 X) 3)))
|#


(defgeneric rcl~check-non-injectivity (table1 table2 function)
  (declare (edited  "16-SEP-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables and a homomorphism function.")
	   (effect  "None.")
	   (value   "T and a pair of elements from table1 for which the funciton is not injectiv."
		    "NIL if no such pair exists."))
  (:method ((table1 rcl+multiplication-table) (table2 rcl+multiplication-table) (function list))
	   (multiple-value-bind (op vars)
	       (rcl=post2operation (rcl=strip-lambda-bindings function))
	     (let ((closure (eval (list 'lambda (reverse vars) op))))
	       (mapcar #'(lambda (elem)
			   (if (listp elem)
			       (apply closure elem)
			     (funcall closure elem)))
		       (rcl~elements table1)))))
  (:method (table1 table2 (function term+term))
           (rcl~check-non-injectivity table1 table2 (read-from-string (post~print function nil))))
  (:method (table1 table2 (function term+abstr))
           (rcl~check-non-injectivity table1 table2
                                      (read-from-string (post~print (data~abstr-range function) nil))))
  )

(defun rcl=strip-lambda-bindings (operation)
  (declare (edited  "16-SEP-2000")
	   (authors Sorge)
	   (input   "A list representing an operation in POST syntax.")
	   (effect  "None.")
	   (value   "The operation devoid of its lambda binders."))
  (if (string-equal (car operation) :lam)
      (rcl=strip-lambda-bindings (car (last operation)))
    operation))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff to compute Non-Isomorphism reasons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rcl~check-non-isomorphism (table1 table2)
  (declare (edited  "06-JUN-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables")
	   (effect  "None.")
	   (value   "NIL, if they structures are not not isomorphic"
		    "Two values (T and a tupel), if there exists a"
		    "pair that violates against injectivity for all"
		    "homomorphisms between the two structs"))
  (:method ((table1 rcl+multiplication-table) (table2 rcl+multiplication-table))
	   (if (rcl=check-isomorphism-prerequisites table1 table2)
	       (multiple-value-bind (solution variables)
		   (rcl=solve-equation-system table1 table2)
	       (when solution
		 (let* ((vars (mapcar #'cdr variables))
			(results (rcl=split2single-solutions (rcl=dissect-maple-hint solution)))
			(elements2 (rcl~elements table2))
			(useful (every #'(lambda (x)
					   (rcl=check-for-illegal-mappings x elements2 vars))
				       results))
                        (pair (when useful (rcl=produce-non-isomorphism-pair results))))
		   (when pair
		     (values t
			     (mapcar #'(lambda (x) (car (rassoc x variables :test #'string-equal))) pair))))))))
  )


(defun rcl=produce-non-isomorphism-pair (results)
  (declare (edited  "26-SEP-2000")
	   (authors Sorge)
	   (input   "A set of results of a solution of homomorphism equation.")
	   (effect  "None.")
	   (value   "A pair of elements for which the homomorphism is never injective if one exists."
		    "Otherwise NIL."))
  (multiple-value-bind
   (sorting-fun testing-fun)
   (etypecase (caaar results)
	       (string (values #'string< #'string-equal))
	       (real (values #'< #'=))
	       (symbol (values #'(lambda (x y)(string< (string x)(string y))) #'eq)))
   (let	 ((res (rcl=find-elements-in-recursive-intersection
		(mapcar #'(lambda (res)
			    (rcl=split-pair-list-wrt-second-element
			     (sort res sorting-fun :key #'cadr)
			     :sorting sorting-fun))
			results)
		     :test testing-fun)))
     (when res (subseq res 0 2)))))


(defun rcl=find-elements-in-recursive-intersection (list-lists &key (test #'string-equal))
  (declare (edited  "26-SEP-2000")
	   (authors Sorge)
	   (input   "A list of lists containing lists of strings.")
	   (effect  "None.")
	   (value   "Performs a recursive intersection operation on the input lists."
		    "It returns a list of elements that are together in all lists of sublists.")
	   (example "Input: '(((\"x3\" \"x0\" \"x2\") (\"x4\" \"x1\")) ((\"x3\" \"x2\") (\"x0\" \"x4\" \"x1\")))"
		    "Output: '(\"x3\" \"x2\")"))
  (if (cdr list-lists)
      (let ((flist (car list-lists))
	    (rest-lists (cdr list-lists)))
	(do ((fl (car flist) (car rl))
	     (rl (cdr flist) (cdr rl))
	     result)
	    ((or result (null fl)) result)
	  (let ((intsct (remove-if #'(lambda (x) (< (length x) 2))
				   (mapcar #'(lambda (x) (intersection fl x :test test))
					   (car rest-lists)))))
	    (when intsct
	      (setf result
		    (rcl=find-elements-in-recursive-intersection
		     (cons intsct (cdr rest-lists)) :test test))))))
    (caar list-lists)))

(defgeneric rcl~check-non-isomorphism-special (table1 table2)
  (declare (edited  "06-JUN-2000")
	   (authors Sorge)
	   (input   "Two multiplication tables")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the two tables are isomorphic, otherwise NIL."
		    "Second value: A list of pairs mapping the element of the first table to those of the second."
		    "              satisfying isomorphism."))
  (:method ((table1 rcl+multiplication-table) (table2 rcl+multiplication-table))
	   (if (rcl=check-isomorphism-prerequisites table1 table2)
	       (multiple-value-bind (solution variables)
		   (rcl=solve-equation-system table1 table2)
	       (when solution
		 (let* ((vars (mapcar #'cdr variables))
			(results (rcl=split2single-solutions (rcl=dissect-maple-hint solution)))
			(elements2 (rcl~elements table2))
			(useful (every #'(lambda (x)
					   (rcl=check-for-illegal-mappings x elements2 vars))
				       results))
                        (list (when useful (rcl=produce-non-isomorphism-list results)))
			(res (mapcar #'(lambda (x) (car (rassoc x variables :test #'string-equal))) list)))
		   (when list
		     (values t
			     (subseq res 0 2)
			     res)))))))
  )


(defun rcl=produce-non-isomorphism-list (results)
  (declare (edited  "26-SEP-2000")
	   (authors Sorge)
	   (input   "A set of results of a solution of homomorphism equation.")
	   (effect  "None.")
	   (value   "A pair of elements for which the homomorphism is never injective if one exists."
		    "Otherwise NIL."))
  (let ((res (rcl=find-elements-in-recursive-intersection
	      (mapcar #'(lambda (res)
			  (rcl=split-pair-list-wrt-second-element
			   (sort res #'string< :key #'cadr)))
		      results))))
    res))


;;
;;(defun rcl=find-elements-in-recursive-intersection (list-lists)
;;  (if (null list-lists)
;;      nil
;;    (do* ((still-possible-pairs (apply #'append (mapcar #'rcl=make-pair-list (first list-lists))))
;;          ;; still-possible-pairs initialized with all pairs that can be constructred of the first solution
;;          (rest-lists (rest list-lists) (rest rest-lists)))
;;        ((or (null still-possible-pairs)
;;             (null rest-lists))
;;         (progn
;;           ;;(format t "ALL POSSIBLE PAIRS: ~A" still-possible-pairs) 
;;           (first still-possible-pairs)))
;;      (let* ((next-lists-to-check (first rest-lists))
;;             (possible-pairs-of-this-list (apply #'append (mapcar #'rcl=make-pair-list next-lists-to-check)))
;;             ;; holds the possible pairs of the next solution
;;             )
;;        (setf still-possible-pairs
;;              (remove-if-not #'(lambda (pair)
;;                                 (find pair possible-pairs-of-this-list :test #'equal))
;;                             still-possible-pairs))
;;        ;; remove all pairs which are not in the possible pairs of the current solution
;;        ))))
;;
;;
;;    
;;(defun rcl=make-pair-list (list)
;;  (declare (edited  "26-SEP-2000")
;;           (authors AMeier)
;;           (input   "A list.")
;;           (effect  "None.")
;;           (value   "A list of all pairs of members of the input list."
;;                    "Example: (a b c) -> ((A B) (A C) (B C))"))
;;  (do* ((rest-list list (rest rest-list))
;;        (back-pairs nil))
;;      ((null rest-list)
;;       back-pairs)
;;    (let* ((head-item (first rest-list))
;;           (rest-rest (rest rest-list))
;;           (new-pairs (mapcar #'(lambda (rest-item)
;;                                  (list head-item rest-item))
;;                              rest-rest)))
;;      (setf back-pairs (append back-pairs new-pairs)))))
;;

(defun rcl=split-pair-list-wrt-second-element (pair-list &key (sorting #'string<))
  (declare (edited  "26-SEP-2000")
	   (authors Sorge)
	   (input   "A list of pairs sorted wrt. the second elements.")
	   (effect  "None.")
	   (value   "A list of lists where each list contains the first elements of the pairs"
		    "and the lists grouped wrt. the equal second elements.")
	   (example "Input: '(((\"x3\" \"0\") (\"x0\" \"0\") (\"x2\" \"0\") (\"x4\" \"1\") (\"x1\" \"1\")))"
		    "Output: '((\"x3\" \"x0\" \"x2\") (\"x4\" \"x1\"))"))
  (when pair-list
    (let* ((sfel (cadar pair-list))
	   (pos (position sfel pair-list :test sorting :key #'cadr)))
      (if pos
	  (cons
	   (mapcar #'first (subseq pair-list 0 pos))
	   (rcl=split-pair-list-wrt-second-element (subseq pair-list pos) :sorting sorting))
	(list (mapcar #'first pair-list))))))

(defun rcl=produce-non-isomorphism-mapping (results elements mod2 mappings)
  (declare (edited  "07-JUN-2000")
	   (authors Sorge)
	   (input   "A list of results, a list of elements, a list of variable mappings.")
	   (effect  "None.")
	   (value   "A list containing a mapping from the elements of mappings to the"
		    "given elements as determined by results."))
  (let* ((variables (mapcar #'cdr mappings)))
    (multiple-value-bind (map1 res1)
	(rcl=find-variables2numerals results)
      (print map1)
      (print res1)

      (if res1
	  (let ((new-res1 (rcl=transform-result-functions res1 map1 variables)))
	    (when new-res1
	      (rcl=map-elements2elements
	       mappings
	       (rcl=compute-rest-elements map1 mod2 new-res1 elements))))
	(rcl=map-elements2elements mappings map1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Orders of Elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rcl~element-orders (table)
  (declare (edited  "12-SEP-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "None.")
	   (value   "If the table is closed a list of pairs each containing an element and its order."
		    "NIL if the table is not closed."))
  (:method ((table rcl+multiplication-table))
	   (let ((elements (rcl~elements table))
		 result)
	     (dotimes (x (length elements))
	       (push
		(list (nth x elements)
		      (rcl~gap-element-order table (1+ x)))
		result))
	     (reverse result))))

(defun rcl=compare-element-orders (order-list1 order-list2)
  (declare (edited  "13-SEP-2000")
	   (authors Sorge)
	   (input   "Two lists with element orders.")
	   (effect  "None.")
	   (value   "An element of the first list where the order cannot be found in the second list."))
  (find-if #'(lambda (order)
	       (notany #'(lambda (y) (= order (second y))) order-list2))
	   order-list1
	   :key #'second))

(defgeneric rcl~generated-substructures (table &key (sorted nil))
  (declare (edited  "12-SEP-2000")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "None.")
	   (value   "If the table is closed a list pairing the elements of the table"
		    "with the substructures they generate. NIL if the table is not closed."))
  (:method ((table rcl+multiplication-table) &key (sorted nil))
	   (let ((elements (rcl~elements table))
		 result)
	     (dotimes (x (length (rcl~elements table)))
	       (push
		(list (nth x elements)
		      (if sorted
			  (rcl=trace-sort-substructure-elements
			   table (1+ x) (length (rcl~gap-generated-substructure table (1+ x))))
			(rcl~gap-generated-substructure table (1+ x))))
		result))
	     (reverse result)))
  )

(defun rcl=compare-generated-substructures (struct-list1 struct-list2)
  (declare (edited  "13-SEP-2000")
	   (authors Sorge)
	   (input   "Two lists with generated substructures.")
	   (effect  "None.")
	   (value   "An element of the first list where a substructure of that order cannot be found in the second list."))
  (find-if #'(lambda (x)
	       (let ((order (length x)))
		 (notany #'(lambda (y) (= order (length (second y)))) struct-list2)))
	   struct-list1
	   :key #'second))

(defun rcl=trace-sort-substructure-elements (table n m)
  (declare (edited  "13-SEP-2000")
	   (authors Sorge)
	   (input   "A multiplication table and two integers.")
	   (effect  "None.")
	   (value   "The trace of length m of the nth element in the table."))
  (let (result)
    (dotimes (x m)
      (push (rcl~gap-element-power table n (1+ x)) result))
    (reverse result)))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other stuff that might be moved some day...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rcl~solve-equation-with-maple (equation meta-var)
  (declare (edited  "15-MAR-2000")
	   (authors Sorge)
	   (input   "Two terms, one representing an equation, one a meta-var.")
	   (effect  "Calls Maple.")
	   (value   "A term representing the solution of the equation wrt. to the meta-var."))
  (let ((result (rcl~call-maple
		 (list "solve"
		       (string-downcase (post~string equation))
		       (string-downcase (post~string meta-var)))
		 :syntax :post2post)))
    (if (string-equal result "Error")
	nil
      (post~read-object (read-from-string result) (pds~environment omega*current-proof-plan) :existing-term))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special calls to Maple and GAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rcl~call-maple (expression &key syntax)
  (opr~arrest-listener opr*service-listener)
  (let* ((maple-result (maple~call-maple expression :syntax syntax)))
    (maple~leave)
    (opr~release-listener opr*service-listener)
    (setq rcl*check-line maple-result)
    maple-result))

(defun rcl~call-gap (expression)
  (opr~arrest-listener opr*service-listener)
  (prog1 (gap~call-gap expression)
    (opr~release-listener opr*service-listener)))

(defun rcl~maple2post (expression)
  (opr~arrest-listener opr*service-listener)
  (serv~enter 'mapletrans)
  (prog1 (serv~apply 'mapletrans (format nil "maple2post('~A')" expression))
    (opr~release-listener opr*service-listener)))

(defun rcl~post2maple (expression)
  (opr~arrest-listener opr*service-listener)
  (serv~enter 'mapletrans)
  (prog1 (serv~apply 'mapletrans (format nil "post2maple('~A')" expression))
    (opr~release-listener opr*service-listener)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for the FINDER Interface 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| until finder works properly
(defun rcl~table2finder (table &key (number 0) (only-elements t))
  (declare (edited  "02-JUL-2001")
	   (authors Sorge)
	   (input   "A multiplication table, a non-negative integer, and a boolean."
		    "The ONLY-ELEMENTS flag indicates whether only the elements of the domain"
		    "are to be taken as enumeration sort or all elements"
		    "(e.g., in case a table is not closed).")
	   (effect  "None.")
	   (value   "Five strings and an association list. In detail:"
		    "1. a sort name for FINDER."
		    "2. a FINDER sort declaration."
		    "3. a function name for FINDER."
		    "4. a FINDER function declaration."
		    "5. an association list containing a mapping of elements to their"
		    "   respective FINDER representation."
		    "6. the multiplication table in FINDER format."))
  (let* ((elements (rcl~elements table))
	 (all-elements (remove-duplicates (append elements (apply #'append (rcl~table table))) :test #'equal))
	 (mapping (sort (rcl=map-finder-elements all-elements number)
			#'(lambda (x y) (string<  (cdr x)(cdr  y)))))
	 (finder-elems (mapcar #'cdr mapping))
	 (sort (format nil "elem~A" number))
	 (sort-string
	  (if only-elements
	      (let ((enum-elems (mapcar #'(lambda (x) (cdr (assoc x mapping :test #'equal))) elements)))
		(format nil "~A  enum: ~{ ~A~^ ,~}." sort enum-elems))
	    (format nil "~A  enum: ~{ ~A~^ ,~}." sort finder-elems)))
	 (function (format nil "op~A" number))
	 (function-string
	  (format nil "~A:  ~A,~A -> ~A." function sort sort sort))
	 )
    (values
     sort sort-string
     function function-string
     mapping
     (format nil "~{~% ~A~}"
	     (mapcan #'(lambda (y row)
			 (mapcar #'(lambda (x result)
				     (format nil "~A ~A ~A = ~A." 
					     (cdr (assoc y mapping :test #'equal)) function
					     (cdr (assoc x mapping :test #'equal))
					     (cdr (assoc result mapping :test #'equal))))
				 elements row))
		     elements (rcl~table table))))))
  
(defun rcl~finder-isomorphism (table1 table2)
  (declare (edited  "12-SEP-2001")
	   (authors Pollet)
	   (input   "Two multiplication tables")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the two tables are isomorphic, otherwise NIL."
		    "Second value: A list of pairs mapping the element of the first table to those of the second."
		    "              satisfying isomorphism."))
  (when (rcl=check-isomorphism-prerequisites table1 table2)
    (multiple-value-bind (domsort domsort-decl domfunction domfunction-decl domelements domclauses)
	(rcl~table2finder table1 :number 0)
      (multiple-value-bind (ransort ransort-decl ranfunction ranfunction-decl ranelements ranclauses)
	  (rcl~table2finder table2 :number 1)
	(let* ((homofunction-decl (format nil "hom : ~A -> ~A {bijective}" domsort ransort))
	       (homo-property (format nil "hom(X) ~A hom(Y)= hom(X ~A Y)." ranfunction domfunction))
	       (input (finder~finder-input
		       :sorts (list domsort-decl ransort-decl)
		       :constants nil
		       :functions (list domfunction-decl ranfunction-decl homofunction-decl)
		       :clauses (list  ranclauses domclauses   homo-property);;ranclauses before domclauses!!!
		       :solutions 1))
	       (result (finder~translate-models input (finder~call-finder input))))
	  (if (equal result "")
	      (values nil nil)
	    (values T (rcl=associate-finder-function 'hom
						     (rcl=model-mapping (car (rcl=read-all-finder-models result)))
						     (append domelements ranelements)))))))))

  

(defun rcl~finder-closure (table)
  (declare (edited  "06-JUL-2001")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "T if the table is closed, o/w NIL and a pair of elements serving as counter example."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2finder table :only-elements nil)
    (let* ((table-elements (rcl~elements table))
	   (set-elements (mapcar #'(lambda (x) (cdr (assoc x elements :test #'equal))) table-elements))
	   (inequalities (mapcar #'(lambda (x) (format nil "X ~A Y = ~A -> false." function x)) set-elements))
	   (prohibited-values (mapcar #'(lambda (x) (format nil "X = ~A -> false.~%      Y = ~A -> false." x x))
				      (mapcan #'(lambda (y) (unless (find (car y) table-elements :test #'equal)
							      (list (cdr y))))
					      elements)))
	   (input (finder~finder-input
		   :sorts (list sort-decl)
		   :constants (mapcar #'(lambda (var) (rcl=finder-declaration var sort)) '(X Y))
		   :functions (list function-decl)
		   :clauses (cons clauses (append inequalities prohibited-values))
		   :solutions 1))
	   (result (finder~translate-models input (finder~call-finder input)))
	   (mapping (rcl=model-mapping (car (rcl=read-all-finder-models result))))
	   (pair (list (rcl=associate-finder-element 'X mapping elements)
		       (rcl=associate-finder-element 'Y mapping elements))))
      (if (equal result "")
	  (values T NIL)
	(values NIL pair)))))
     
(defun rcl~finder-associativity (table)
  (declare (edited  "06-JUL-2001")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "T if the table is associative, o/w NIL and a triple of elements serving as counter example."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2finder table)
    (declare (ignore elements sort))
    (let* ((input (finder~finder-input
		  :sorts (list sort-decl)
		  :constants nil
		  :functions (list function-decl)
		  :clauses (list clauses (format nil "(X ~A Y) ~A Z = X ~A (Y ~A Z)." function function function function))))
	   (result (finder~translate-models input (finder~call-finder input))))
      (if (equal result "")
	  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	      (rcl~table2finder table :only-elements nil)
	    (let* ((input (finder~finder-input
			   :sorts (list sort-decl)
			   :constants (mapcar #'(lambda (var) (rcl=finder-declaration var sort)) '(X Y Z))
			   :functions (list function-decl)
			   :clauses (list clauses
					  (format nil "(X ~A Y) ~A Z = X ~A (Y ~A Z) -> false." function function function function))
			   :solutions 1))
		   (result (finder~translate-models input (finder~call-finder input)))
		   (mapping (rcl=model-mapping (car (rcl=read-all-finder-models result))))
		   (triple (mapcar #'(lambda (var) (rcl=associate-finder-element var mapping elements)) '(X Y Z))))
	      (values NIL triple)))
	(values T NIL)))))
     
(defun rcl~finder-commutativity (table)
  (declare (edited  "06-JUL-2001")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "T if the table is commutative, o/w NIL and a pair of elements serving as counter example."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2finder table)
    (declare (ignore elements sort function))
    (let* ((input (finder~finder-input
		  :sorts (list sort-decl)
		  :constants nil
		  :functions (list (concatenate 'string (string-right-trim "." function-decl) " {commutative}"))
		  :clauses (list clauses )))
	   (result (finder~translate-models input (finder~call-finder input))))
      (if (equal result "")
	  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	      (rcl~table2finder table :only-elements nil)
	    (let* ((input (finder~finder-input
			   :sorts (list sort-decl)
			   :constants (mapcar #'(lambda (var) (rcl=finder-declaration var sort)) '(X Y))
			   :functions (list function-decl)
			   :clauses (list clauses
					  (format nil "X ~A Y = Y ~A X -> false." function function))
			   :solutions 1))
		   (result (finder~translate-models input (finder~call-finder input)))
		   (mapping (rcl=model-mapping (car (rcl=read-all-finder-models result))))
		   (pair (mapcar #'(lambda (var) (rcl=associate-finder-element var mapping elements)) '(X Y))))
	      (values NIL pair)))
	(values T NIL)))))

(defun rcl~finder-unit-element (table)
  (declare (edited  "06-JUL-2001")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "T and the unit element of the table if it has one."
		    "O/w NIL and a list of pairs of elements serving as counter example."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2finder table)
    (let* ((input (finder~finder-input
		   :sorts (list sort-decl)
		   :constants (list (rcl=finder-declaration 'UNIT sort)) ; (rcl=finder-declaration 'X sort)) has to hold forall X, don't declare it
		   :functions (list function-decl)
		   :clauses (list clauses
				  (format nil "UNIT ~A X = X." function))))
	   (result (finder~translate-models input (finder~call-finder input))))
      (if (equal result "")
	  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	      (rcl~table2finder table :only-elements nil)
	    (let* ((table-elements (rcl~elements table))
		   (input (finder~finder-input
			   :sorts (list sort-decl)
			   :constants (mapcar #'(lambda (var) (rcl=finder-declaration var sort)) '(X UNIT))
			   :functions (list function-decl)
			   :clauses (list clauses
					  (format nil "UNIT ~A X = X, X ~A UNIT = X -> false." function function)
					  ;;;(format nil " -> false." function)
					  )
			   :solutions (* (length table-elements) (length table-elements))))
		   (result (finder~translate-models input (finder~call-finder input)))
		   (mappings (mapcar #'rcl=model-mapping (rcl=read-all-finder-models result)))
		   (pairs (mapcar #'(lambda (mapping)
				      (list (rcl=associate-finder-element 'X mapping elements)
					    (rcl=associate-finder-element 'UNIT mapping elements)))
				  mappings)))
	      (values NIL (mapcar #'(lambda (elem) (assoc elem pairs :test #'equal)) table-elements))))
	(values T (rcl=associate-finder-element 'unit
						(rcl=model-mapping (car (rcl=read-all-finder-models result)))
						elements ))))))

(defun rcl~finder-inverses (table)
  (declare (edited  "14-SEP-2001")
	   (authors Pollet)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value    "First value: T if the table contains inverses for every element, otherwise NIL."
		     "Second value: A list of pairs consisting of elements and their inverses, if one"
		     "exists for every element, otherwise an element for which no inverse exists."))
  (multiple-value-bind (goon? neut) (rcl~finder-neutral-element? table)
    (when goon?
      (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	  (rcl~table2finder table)
	(let* ((findneut (cdr (assoc neut elements :test #'equal)))
	       (input (finder~finder-input
		       :solutions 1
		       :sorts (list sort-decl)
		       :constants nil
		       :functions (list function-decl (format nil "inv: ~A -> ~A {bijective}" sort sort))
		       :clauses (list clauses
				      (format nil "~% X ~A inv(X) = ~A.~% X ~A inv(X) = ~A."
					      function findneut function findneut))))
	       (result (finder~translate-models input (finder~call-finder input))))
	  (if (equal result "")
	      (multiple-value-bind (sort sort-decl function function-decl elements clauses)
		  (rcl~table2finder table)
		(let* ((input (finder~finder-input
			       :sorts (list sort-decl)
			       :constants (list (rcl=finder-declaration 'X sort))
			       :functions (list function-decl)
			       :clauses (list clauses
					      (format nil "X ~A Y = ~A -> false." function findneut)
					      )
			       :solutions 1))
		       (result (finder~translate-models input (finder~call-finder input))))
		  (when result ""
			(setq input (finder~finder-input
				     :sorts (list sort-decl)
				     :constants (list (rcl=finder-declaration 'X sort))
				     :functions (list function-decl)
				     :clauses (list clauses
						    (format nil "Y ~A X = ~A -> false." function findneut)
						    )
				     :solutions 1))
			(setq result (finder~translate-models input (finder~call-finder input))))
		  (let* ((mapping (rcl=model-mapping (car (rcl=read-all-finder-models result))))
			 (notinv (rcl=associate-finder-element 'X mapping elements)))
		    (values NIL notinv))))
	    (values T (rcl=associate-finder-function 'inv
						     (rcl=model-mapping (car (rcl=read-all-finder-models result)))
						     elements))))))))

(defun rcl~finder-divisors (table)
  (declare (edited  "14-SEP-2001")
	   (authors Pollet)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "Two values: "
		    "First value: T if divisors exist for every two elements, otherwise NIL."
		    "Second value: If first value is NIL, a pair of elements for which no divisors exist."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2finder table)
    (declare (ignore elements))
    (let* ((input (finder~finder-input
		   :solutions 1
		   :sorts (list sort-decl)
		   :constants nil
		   :functions (list function-decl
				    (format nil "leftdiv: ~A,~A -> ~A ." sort sort sort)
				    (format nil "rightdiv: ~A,~A -> ~A ." sort sort sort))
		   :clauses (list(format nil "~% X ~A leftdiv(X,Y) = Y.~% rightdiv(X,Y) ~A X = Y."
					 function function )
				 clauses)))
	   (result (finder~translate-models input (finder~call-finder input))))
      (if (equal result "")
	  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	      (rcl~table2finder table)
	    (let* ((input (finder~finder-input
			   :sorts (list sort-decl)
			   :constants (mapcar #'(lambda (var) (rcl=finder-declaration var sort)) '(A B))
			   :functions (list function-decl)
			   :clauses (list clauses
					  (format nil "A ~A X = B -> false." function)
					  )
			   :solutions 1))
		   (result (finder~translate-models input (finder~call-finder input))))
	      (when result ""
		    (setq input (finder~finder-input
				 :sorts (list sort-decl)
				 :constants (mapcar #'(lambda (var) (rcl=finder-declaration var sort)) '(A B))
				 :functions (list function-decl)
				 :clauses (list clauses
						(format nil "X ~A A = B -> false." function)
						)
				 :solutions 1))
		    (setq result (finder~translate-models input (finder~call-finder input))))
	      (let* ((mapping (rcl=model-mapping (car (rcl=read-all-finder-models result))))
		     (notdiv (list (rcl=associate-finder-element 'A mapping elements)
				   (rcl=associate-finder-element 'B mapping elements))))
		(values NIL notdiv))))
	(values T NIL)))))
|#

(defun rcl=map-finder-elements (elements number &optional (var 'c))
  (declare (edited  "02-JUL-2001")
	   (authors Sorge)
	   (input   "A list of integers, an integer, and a symbol.")
	   (effect  "None.")
	   (value   "A association list mapping elements to their FINDER representation."))
  (let ((prefix (format nil "~A~A"  (string-downcase var) number)))
    (mapcar #'(lambda (elem)
		(cons elem (format nil "~A~A" prefix
				   (if (consp elem) (format nil "~{x~A~}" elem) elem))))
	    elements)))
  

(defun rcl=associate-finder-element (variable finder-mapping element-mapping)
  (declare (edited  "10-JUL-2001")
	   (authors Sorge)
	   (input   "A symbol, a list of FINDER mappings associating variables with values,"
		    "and an association list mapping table elements to their FINDER representation.")
	   (effect  "None.")
	   (value   "The table element that the FINDER variable is mapped to."))
  (car (rassoc (caadr (assoc variable finder-mapping)) element-mapping :test 'string-equal)))

(defun rcl=finder-declaration (var sort)
  (declare (edited  "06-JUL-2001")
	   (authors Sorge)
	   (input   "A variable name and a sort name.")
	   (effect  "None.")
	   (value   "A string containing a valid finder declaration."))
  (format nil "~A: ~A." var sort))
			
(defun rcl=read-all-finder-models (model-string)
  (declare (edited  "10-JUL-2001")
	   (authors Sorge)
	   (input   "A string containing translated FINDER models.")
	   (effect  "None.")
	   (value   "A list with all models."))
  (read-from-string
   (concatenate 'string "(" model-string ")")))

(defun rcl=model-domain (model)
  (declare (edited  "10-JUL-2001")
	   (authors Sorge)
	   (input   "A list representing a FINDER model.")
	   (effect  "None.")
	   (value   "A list representing the domain of a FINDER model."))
  (second (assoc 'domain (third model))))

(defun rcl=model-mapping (model)
  (declare (edited  "10-JUL-2001")
           (authors Sorge)
           (input   "A list representing a FINDER model.")
           (effect  "None.")
           (value   "A list representing the mapping of elements of a FINDER model."))
  (second (third model)))

(defun rcl=associate-finder-function (function mapping elements)
  (mapcar #'(lambda (map) (list (car (rassoc (caar map) elements  :test #'string-equal))
				(car (rassoc (cadr map) elements  :test #'string-equal))))
		  (second (assoc function mapping))))



;;;;;;SEM quick and dirty implementation

(defun rcl~table2sem (table &key (number 0) (only-elements t))
  (declare (edited  "02-OCT-2001")
	   (authors Pollet)
	   (input   "A multiplication table, a non-negative integer, and a boolean."
		    "The ONLY-ELEMENTS flag indicates whether only the elements of the domain"
		    "are to be taken as enumeration sort or all elements"
		    "(e.g., in case a table is not closed).")
	   (effect  "None.")
	   (value   "Five strings and an association list. In detail:"
		    "1. a sort name for SEM."
		    "2. a SEM sort declaration."
		    "3. a function name for SEM."
		    "4. a SEM function declaration."
		    "5. an association list containing a mapping of elements to their"
		    "   respective SEM representation."
		    "6. the multiplication table in SEM format."))
  (let* ((elements (rcl~elements table))
	 (all-elements (remove-duplicates (append elements (apply #'append (rcl~table table))) :test #'equal))
	 (mapping (sort (rcl=map-finder-elements all-elements number)
			#'(lambda (x y) (string<  (cdr x)(cdr  y)))))
	 (finder-elems (mapcar #'cdr mapping))
	 (sort (format nil "elem~A" number))
	 (sort-string
	  (if only-elements
	      (let ((enum-elems (mapcar #'(lambda (x) (cdr (assoc x mapping :test #'equal))) elements)))
		(format nil "~A  : ~{ ~A~^ ,~}" sort enum-elems))
	    (format nil "~A : ~{ ~A~^ ,~}" sort finder-elems)))
	 (function (format nil "op~A" number))
	 (function-string
	  (format nil "~A:  ~A ~A -> ~A" function sort sort sort))
	 )
    (values
     sort sort-string
     function function-string
     mapping
     (mapcan #'(lambda (y row)
  			 (mapcar #'(lambda (x result)
				     (format nil "~A(~A,~A) = ~A" 
					     function (cdr (assoc y mapping :test #'equal))
					     (cdr (assoc x mapping :test #'equal))
					     (cdr (assoc result mapping :test #'equal))))
				 elements row))
		     elements (rcl~table table)))))






(defun rcl~scalar-table2sem (scalar-table &key (number 0) (ring-number 0) (only-elements t))
  (declare (edited  "03-JUNE-2003")
	   (authors AMeier)
	   (input   "A multiplication table, two non-negative integers, and a boolean."
		    "The ONLY-ELEMENTS flag indicates whether only the elements of the domain"
		    "are to be taken as enumeration sort or all elements"
		    "(e.g., in case a table is not closed).")
	   (effect  "None.")
	   (value   "Five strings and an association list. In detail:"
		    "1. a sort name for the module elements for SEM."
		    "2. a SEM sort declaration for the module elements."
		    "3. a sort name for the ring elements for SEM."
		    "4. a SEM sort declaration for the ring elements."
		    "5. an association list containing a mapping of module elements to their"
		    "   respective SEM representation."
		    "6. an association list containing a mapping of ring elements to their"
		    "   respective SEM representation."
		    "7. a function name for SEM for the scalar operation ring X module -> module."
		    "8. a SEM function declaration."
		    "9. the multiplication table in SEM format."))
  (let* ((elements (rcl~elements scalar-table))
	 (all-elements (remove-duplicates (append elements (apply #'append (rcl~table scalar-table))) :test #'equal))
	 (mapping (sort (rcl=map-finder-elements all-elements number)
			#'(lambda (x y) (string<  (cdr x)(cdr  y)))))
	 (finder-elems (mapcar #'cdr mapping))
	 (sort (format nil "elem~A" number))
	 (sort-string
	  (if only-elements
	      (let ((enum-elems (mapcar #'(lambda (x) (cdr (assoc x mapping :test #'equal))) elements)))
		(format nil "~A  : ~{ ~A~^ ,~}" sort enum-elems))
	    (format nil "~A : ~{ ~A~^ ,~}" sort finder-elems)))
	 
	 (scalar-elements (rcl~scalar-elements scalar-table))
	 (scalar-mapping (sort (rcl=map-finder-elements scalar-elements ring-number)
			#'(lambda (x y) (string<  (cdr x)(cdr  y)))))
	 (scalar-finder-elems (mapcar #'cdr scalar-mapping))
	 (scalar-sort (format nil "relem~A" ring-number))
	 (scalar-sort-string (format nil "~A : ~{ ~A~^ ,~}" scalar-sort scalar-finder-elems))

	 (function (format nil "scalarop~A" number))
	 (function-string (format nil "~A:  ~A ~A -> ~A" function scalar-sort sort sort))
	 )
    (values
     sort sort-string
     scalar-sort scalar-sort-string
     mapping scalar-mapping
     function function-string
     (mapcan #'(lambda (y row)
  			 (mapcar #'(lambda (x result)
				     (format nil "~A(~A,~A) = ~A" 
					     function (cdr (assoc y scalar-mapping :test #'equal))
					     (cdr (assoc x mapping :test #'equal))
					     (cdr (assoc result mapping :test #'equal))))
				 elements row))
		     scalar-elements (rcl~table scalar-table)))))

(defun rcl~table2sem-input (table &optional (number 0))
  (declare (edited  "08-NOV-2002")
	   (authors Vxs)
	   (input   "A multiplication table.")
	   (effect  "None.")
	   (value   "A string containing the declarations for SEM input file.")
	   (comment "This function is just a shortcut for translating a multiplication table directly into SEM input."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2sem table :number number)
    (sem~sem-input
     :sorts (list sort-decl)
     :variables nil
     :functions (list function-decl)
     :clauses clauses)
    ))


(defun rcl~sem-isomorphism (table1 table2)
  (declare (edited  "12-OCT-2001")
	   (authors Pollet)
	   (input   "Two multiplication tables")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the two tables are isomorphic, otherwise NIL."
		    "Second value: A list of pairs mapping the element of the first table to those of the second."
		    "              satisfying isomorphism."))
  (when (rcl=check-isomorphism-prerequisites table1 table2)
    (multiple-value-bind (domsort domsort-decl domfunction domfunction-decl domelements domclauses)
	(rcl~table2sem table1 :number 0)
      (multiple-value-bind (ransort ransort-decl ranfunction ranfunction-decl ranelements ranclauses)
	  (rcl~table2sem table2 :number 1)
	(let* ((input (sem~sem-input
		       :sorts (list domsort-decl ransort-decl)
		       :variables (sem~vardecl '(X Y)  domsort)
		       :functions (cons domfunction-decl
					(cons ranfunction-decl (sem~fundecl "hom" ransort domsort :special "(BIJ)")))
		       :clauses (cons (format nil "~A(hom(X),hom(Y)) = hom(~A(X,Y))" ranfunction domfunction)
				      (append ranclauses domclauses))))
	       (result (sem~translate-models input (sem~call-sem input))))
	  (if (null result)
	      (values nil nil)
	    (values T (rcl=associate-finder-function 'hom
						     (rcl=model-mapping (car result))
						     (append domelements ranelements)))))))))



(defun rcl~sem-module-isomorphism (module1-table module2-table ring-module1-table ring-module2-table)
  (declare (edited  "12-OCT-2001")
	   (authors Pollet)
	   (input   "Four multiplication tables")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the first two tables are isomorphic as modules with respect to the"
		    "             ring multiplication in the third and the fourth multiplication table,"
		    "             otherwise NIL."
		    "Second value: A list of pairs mapping the element of the first table to those of the second."
		    "              satisfying module isomorphism."))
  (when (rcl=check-isomorphism-prerequisites module1-table module2-table)
    (multiple-value-bind
	(domsort domsort-decl domfunction domfunction-decl domelements domclauses)
	(rcl~table2sem module1-table :number 0)
      (multiple-value-bind
	  (ransort ransort-decl ranfunction ranfunction-decl ranelements ranclauses)
	  (rcl~table2sem module2-table :number 1)
	(multiple-value-bind
	    (domsort domsort-decl ringsort ringsort-decl domelements ringelements domscalarfunction domscalarfunction-decl domscalarclauses)
	    (rcl~scalar-table2sem ring-module1-table :number 0 :ring-number 2)
	  (multiple-value-bind
	      (ransort ransort-decl ringsort ringsort-decl ranelements ringelements ranscalarfunction ranscalarfunction-decl ranscalarclauses)
	      (rcl~scalar-table2sem ring-module2-table :number 1 :ring-number 2)
	    
	    (let* ((input (sem~sem-input
			   :sorts (list domsort-decl ransort-decl ringsort-decl)
			   :variables (append (sem~vardecl '(X Y Z)  domsort)
					      (sem~vardecl '(R)  ringsort))
			   :functions (cons domscalarfunction-decl
					    (cons ranscalarfunction-decl
						  (cons domfunction-decl
							(cons ranfunction-decl (sem~fundecl "hom" ransort domsort :special "(BIJ)")))))
			   :clauses (cons (format nil "hom(~A(R,Z)) = ~A(R,hom(Z))" domscalarfunction ranscalarfunction)
					  
					  (cons (format nil "~A(hom(X),hom(Y)) = hom(~A(X,Y))" ranfunction domfunction)
						(append ranclauses domclauses domscalarclauses ranscalarclauses)))))
		   (result (sem~translate-models input (sem~call-sem input))))
	      (if (null result)
		  (values nil nil)
		(values T (rcl=associate-finder-function 'hom
							 (rcl=model-mapping (car result))
							 (append domelements ranelements)))))))))))




(defun rcl~sem-non-isomorphism (table1 table2)
  (declare (edited  "12-OCT-2001")
	   (authors Pollet)
	   (input   "Two multiplication tables")
	   (effect  "None.")
	   (value   "Two values: "
		    "First value: T if the two tables are isomorphic, otherwise NIL."
		    "Second value: A list of pairs mapping the element of the first table to those of the second."
		    "              satisfying isomorphism."))
  (multiple-value-bind (domsort domsort-decl domfunction domfunction-decl domelements domclauses)
      (rcl~table2sem table1 :number 0)
    (multiple-value-bind (ransort ransort-decl ranfunction ranfunction-decl ranelements ranclauses)
	(rcl~table2sem table2 :number 1)
      (let* ((countinput (sem~sem-input
			  :sorts (list domsort-decl ransort-decl)
			  :variables (sem~vardecl '(X Y)  domsort)
			  :functions (cons domfunction-decl (cons ranfunction-decl (sem~fundecl "hom" ransort domsort)))
			  :clauses (cons (format nil "~A(hom(X),hom(Y)) = hom(~A(X,Y))" ranfunction domfunction)
					 (append ranclauses domclauses))
			  :solutions 0))
	     (homs (mapcar #'(lambda (hom)
			       (rcl=associate-finder-function 'hom
						     (rcl=model-mapping hom)
						     (append domelements ranelements)))
				 (sem~translate-models countinput (sem~call-sem countinput))))
	     (pair (rcl=produce-non-isomorphism-pair homs)))
	(when pair
	  (values t pair))))))


#| this work only for less than 4 homos, use rcl~sem-non-isomorphism
(defun rcl=sem-not-iso (table1 table2)
  (multiple-value-bind (domsort domsort-decl domfunction domfunction-decl domelements domclauses)
      (rcl~table2sem table1 :number 0)
    (multiple-value-bind (ransort ransort-decl ranfunction ranfunction-decl ranelements ranclauses)
	(rcl~table2sem table2 :number 1)
      (let* ((countinput (sem~sem-input
			  :sorts (list domsort-decl ransort-decl)
			  :variables (sem~vardecl '(X Y)  domsort)
			  :functions (cons domfunction-decl (cons ranfunction-decl (sem~fundecl "hom" ransort domsort)))
			  :clauses (cons (format nil "~A(hom(X),hom(Y)) = hom(~A(X,Y))" ranfunction domfunction)
					 (append ranclauses domclauses))
			  :solutions 0))
	     (homonames (maplist #'(lambda (thing) (format nil "hom~A" (length thing)))
				 (sem~translate-models countinput (sem~call-sem countinput))))
	     (homodecls (sem~fundecl homonames ransort domsort)) 
	     (homoprops (mapcar #'(lambda (hom) (format nil "~A(~A(X),~A(Y)) = ~A(~A(X,Y))"
							ranfunction hom hom hom domfunction))
				homonames))
	     (notinj (cons "I != J" (mapcar #'(lambda (hom) (format nil "~A(I) = ~A(J)" hom hom)) homonames)))
	     (different (mapcon #'(lambda (homs)
				    (mapcar #'(lambda (hom)
						(format nil "~A(elem~A~A) != ~A(elem~A~A)"
							(car homs)(car homs) hom hom (car homs) hom))
					    (rest homs)))
				homonames))		      
	     (constnames (mapcon #'(lambda (homs)
				     (mapcar #'(lambda (hom)
						 (format nil "elem~A~A" (car homs) hom))
					     (rest homs)))
				 homonames))
	     (notinput (sem~sem-input
			:sorts (list domsort-decl ransort-decl)
			:variables (sem~vardecl '(X Y)  domsort)
			:functions (cons  domfunction-decl
					  (cons ranfunction-decl
						(append homodecls (sem~fundecl (cons 'I (cons 'J constnames)) domsort))))
			:clauses (append notinj homoprops ranclauses domclauses different)))
	     (notresult (car (sem~translate-models notinput (sem~call-sem notinput)))))
	(mapcar #'(lambda (elem)
		    (rcl=associate-finder-element elem (rcl=model-mapping notresult) (append domelements ranelements)))
		'(I J))))))  
|#

(defun rcl~sem-closure (table)
  (declare (edited  "06-OCT-2001")
	   (authors Pollet)
	   (input   "A multiplication table.")
	   (effect  "Calls SEM.")
	   (value   "T if the table is closed, o/w NIL and a pair of elements serving as counter example."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2sem table :only-elements nil)
    (let* ((table-elements (rcl~elements table))
	   (set-elements (mapcar #'(lambda (x) (cdr (assoc x elements :test #'equal))) table-elements))
	   (inequalities (mapcar #'(lambda (x) (format nil "~A(X,Y) != ~A" function x)) set-elements))
	   (prohibited-values (mapcan #'(lambda (x)
					  (list (format nil "X != ~A" x)
						(format nil "Y != ~A" x)))
				      (mapcan #'(lambda (y) (unless (find (car y) table-elements :test #'equal)
							      (list (cdr y))))
					      elements)))
	   (input (sem~sem-input
		   :sorts (list sort-decl)
		   :variables nil
		   :functions (cons function-decl
				    (sem~fundecl '(X Y) sort))
		   :clauses (append clauses inequalities prohibited-values)))
	   (result (sem~translate-models input (sem~call-sem input)))
	   (mapping (rcl=model-mapping (car result)))
	   (pair (list (rcl=associate-finder-element 'X mapping elements)
		       (rcl=associate-finder-element 'Y mapping elements))))
      (if (null result)
	  (values T NIL)
	(values NIL pair)))))

(defun rcl~sem-associativity (table)
  (declare (edited  "06-JUL-2001")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "T if the table is associative, o/w NIL and a triple of elements serving as counter example."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2sem table)
    (declare (ignore elements ))
    (let* ((input (sem~sem-input
		  :sorts (list sort-decl)
		  :variables (sem~vardecl '(X Y Z) sort)
		  :functions  (list function-decl)
		  :clauses (cons (format nil "~A(~A(X,Y),Z) = ~A(X,~A(Y,Z))" function function function function) clauses)))
	   (result (sem~translate-models input (sem~call-sem input))))
      (if (null result)
	  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	      (rcl~table2sem table :only-elements nil)
	    (let* ((input (sem~sem-input
			   :sorts (list sort-decl)
			   :variables nil
			   :functions (cons function-decl (sem~fundecl '(X Y Z) sort))
			   :clauses (cons
					  (format nil "~A(~A(X,Y),Z) != ~A(X,~A(Y,Z))" function function function function)
					   clauses)))
		   (result (sem~translate-models input (sem~call-sem input)))
		   (mapping (rcl=model-mapping (car  result)))
		   (triple (mapcar #'(lambda (var) (rcl=associate-finder-element var mapping elements)) '(X Y Z))))
	      (values NIL triple)))
	(values T NIL)))))
     
(defun rcl~sem-commutativity (table)
  (declare (edited  "06-JUL-2001")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "T if the table is commutative, o/w NIL and a pair of elements serving as counter example."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2sem table)
    (declare (ignore elements function))
    (let* ((input (sem~sem-input
		   :sorts (list sort-decl)
		   :variables (sem~vardecl '(X Y) sort)
		   :functions (list (concatenate 'string function-decl " (COMM)"))
		   :clauses clauses))
	   (result (sem~translate-models input (sem~call-sem input))))
      (if (null result)
	  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	      (rcl~table2sem table :only-elements nil)
	    (let* ((input (sem~sem-input
			   :sorts (list sort-decl)
			   :variables nil
			   :functions (cons  function-decl
					     (sem~fundecl '(X Y) sort))
			   :clauses (cons (format nil "~A(X,Y) != ~A(Y,X)" function function)
					  clauses)))
		   (result (sem~translate-models input (sem~call-sem input)))
		   (mapping (rcl=model-mapping (car result)))
		   (pair (mapcar #'(lambda (var) (rcl=associate-finder-element var mapping elements)) '(X Y))))
	      (values NIL pair)))
	(values T NIL)))))

(defun rcl~sem-unit-element (table)
  (declare (edited  "06-JUL-2001")
	   (authors Sorge)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "T and the unit element of the table if it has one."
		    "O/w NIL and a list of pairs of elements serving as counter example."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2sem table)
    (let* ((input (sem~sem-input
		   :sorts (list sort-decl)
		   :variables (sem~vardecl 'X sort)
		   :functions (cons function-decl (sem~fundecl "UNIT" sort))
		   :clauses (cons (format nil "~A(UNIT,X) = X" function)
				  (cons (format nil "~A(X,UNIT) = X" function)
					clauses))))
	   (result (sem~translate-models input (sem~call-sem input))))
      (if (null result)
	  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	      (rcl~table2sem table :only-elements nil)
	    (let* ((input (sem~sem-input
			   :sorts (list sort-decl)
			   :variables (sem~vardecl 'UNIT sort)
			   :functions (cons function-decl
					    (sem~fundecl 'X sort sort))
			   :clauses (cons (format nil "~A(UNIT,X(UNIT)) != X(UNIT) | ~A(X(UNIT),UNIT) != X(UNIT)"
						  function function)
					  clauses )))
		   (result (sem~translate-models input (sem~call-sem input)))
		   (mapping (rcl=model-mapping (car  result)))
		   (notunits (rcl=associate-finder-function 'X mapping elements)))
	      (values NIL notunits)))
	(values T (rcl=associate-finder-element 'unit
						(rcl=model-mapping (car  result))
						elements ))))))

(defun rcl~sem-inverses (table)
  (declare (edited  "14-SEP-2001")
	   (authors Pollet)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value    "First value: T if the table contains inverses for every element, otherwise NIL."
		     "Second value: A list of pairs consisting of elements and their inverses, if one"
		     "exists for every element, otherwise an element for which no inverse exists."))
  (multiple-value-bind (goon? neut) (rcl~sem-unit-element table)
    (when goon?
      (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	  (rcl~table2sem table)
	(let* ((findneut (cdr (assoc neut elements :test #'equal)))
	       (input (sem~sem-input
		       :sorts (list sort-decl)
		       :variables (sem~vardecl '(X) sort)
		       :functions (cons function-decl (sem~fundecl "inv" sort sort :special "(BIJ)"))
		       :clauses (cons (format nil "~A(X ,inv(X)) = ~A" function findneut)
				      (cons (format nil "~A(inv(X),X) = ~A" function findneut)
					    clauses))))
	       (result (sem~translate-models input (sem~call-sem input))))
	  (if (null result)
	      (multiple-value-bind (sort sort-decl function function-decl elements clauses)
		  (rcl~table2sem table)
		(let* ((input (sem~sem-input
			       :sorts (list sort-decl)
			       :variables (sem~vardecl 'X sort)
			       :functions (cons function-decl
						(sem~fundecl 'Y sort))
			       :clauses (cons (format nil "~A(X,Y) != ~A | ~A(Y,X) != ~A"
						      function findneut function findneut)
					      clauses)))
		       (result (sem~translate-models input (sem~call-sem input)))
		       (mapping (rcl=model-mapping (car  result)))
		       (notinv (rcl=associate-finder-element 'Y mapping elements)))
		  (values NIL notinv)))
	    (values T (rcl=associate-finder-function 'inv
						     (rcl=model-mapping (car  result))
						     elements))))))))

(defun rcl~sem-divisors (table)
  (declare (edited  "14-SEP-2001")
	   (authors Pollet)
	   (input   "A multiplication table.")
	   (effect  "Calls FINDER.")
	   (value   "Two values: "
		    "First value: T if divisors exist for every two elements, otherwise NIL."
		    "Second value: If first value is NIL, a pair of elements for which no divisors exist."))
  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
      (rcl~table2sem table)
    (declare (ignore elements))
    (let* ((input (sem~sem-input
		   :sorts (list sort-decl)
		   :variables (sem~vardecl '(X Y) sort)
		   :functions (cons function-decl
				    (sem~fundecl (list "leftdiv" "rightdiv")
						 sort (format nil "~A ~A" sort sort)))
		   :clauses (cons (format nil "~A(X,leftdiv(X,Y)) = Y" function)
				  (cons (format nil "~A(rightdiv(X,Y),X) = Y" function)
					clauses))))
	   (result (sem~translate-models input (sem~call-sem input))))
      (if (null result)
	  (multiple-value-bind (sort sort-decl function function-decl elements clauses)
	      (rcl~table2sem table)
	    (let* ((input (sem~sem-input
			   :sorts (list sort-decl)
			   :variables (sem~vardecl 'X sort)
			   :functions (cons function-decl (sem~fundecl '(A B) sort))
			   :clauses (cons (format nil "~A(A,X) != B | ~A(X,A) != B" function function)
					  clauses)))
		   (result (sem~translate-models input (sem~call-sem input)))
		   (mapping (rcl=model-mapping (car  result)))
		   (notdiv (list (rcl=associate-finder-element 'A mapping elements)
				 (rcl=associate-finder-element 'B mapping elements))))
	      (values NIL notdiv)))
	(values T NIL)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for computing generators of arbitrary magmas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rcl~get-generators&factorization (elements operation model)
  (declare (edited  "25-NOV-2003")
	   (authors Vxs)
	   (input   "A list of constants representing the elements of a structure,"
		    "an operation on it, and a model. The model is a list of lists that"
		    "relates pairs of elements to their result under the operation,"
		    "i.e. it is of the form (((X Y) Z) ((U V) W)...).")
	   (effect  "None.")
	   (value   "A list of constants representating the generators of the structure and"
		    "a list of factorization of the form ((A (op B B)) (B B) (C (op (op B B) B))...)."))
  (multiple-value-bind  (generators factorisation)
      (rcl~generators elements model)
    (flet ((make-factorisation (constant multiplicity)
			       (let ((result constant))
				 (dotimes (i (1- multiplicity) result)
				   (setf result (term~appl-create operation (list result constant)))))))
      (values generators
	      (append (mapcar #'list generators generators)
		      (mapcar #'(lambda (fact) (list (first fact)
						     (make-factorisation (second fact) (third fact))))
			      factorisation))))))

(defun rcl~generators (elements table)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "A list of elements (arbitrary) and a multiplication table given as a list of lists.")
	   (effect  "None.")
	   (value   "A set of generators and a list of generating equations."))
  (let* ((traces (rcl=compute-all-traces elements table))
	 (generators (rcl=compute-generators elements (pairlis elements traces))))
    (values 
     generators
     (rcl=compute-factorisations (set-difference elements generators)
				 (mapcar #'(lambda (x) (find x traces :test #'keim~equal :key #'car))
					 generators)))))


(defun rcl=compute-factorisations (elements traces)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "A list of elements for which factorisations have to be computed."
		    "And a list of traces of the generators.")
	   (effect  "None.")
	   (value   "A list of factorisations."))
;;  (print elements)
;;  (print traces)
  (when elements
    (let* ((felem (car elements))
	   (trace (find-if #'(lambda (x) (find felem x :test #'keim~equal)) traces))
	   (multiplicity (1+ (position felem trace :test #'keim~equal))))
      (cons (list felem (car trace) multiplicity)
            (rcl=compute-factorisations (cdr elements) traces)))))
      

  
(defun rcl=compute-generators (elements traces)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "A list of elements and a list of traces for each element of the form (element . trace).")
	   (effect  "None.")
	   (value   "A list of generators sufficient to generate the structure. NOTE: This is not"
		    "necessarily a minimal list of generators, since we currently only consider"
		    "traces of single generators, thereby neglecting possible interactions between generators."))
  ;; pick longest elements in trace
  (let* ((longest (car (sort (copy-list traces) #'(lambda (x y) (> (length x) (length y))) :key #'cdr)))
	 (longest-trace (cdr longest))
	 (new-elements (set-difference elements longest-trace :test #'keim~equal)))
    (if (null new-elements)
	(list (car longest))
      (cons (car longest)
	    ;; remove elements from the traces
	    (rcl=compute-generators new-elements
				    (mapcar #'(lambda (trace)
						(cons (car trace)
						      (set-difference (cdr trace) longest-trace :test #'keim~equal)))
					    traces))))))
  

(defun rcl~operations-list2table (elements table)
  (declare (edited  "07-DEC-2003")
	   (authors Vxs)
	   (input   "A list of elements and a list representing a multiplication table by single binary operations.")
	   (effect  "None.")
	   (value   "A list representing the corresponding multiplication table. If the operations are insufficient"
		    "to determine the full table, NIL is returned."))
  (labels ((all-operations-left (element &optional (table table))
				(cond ((null table) table)
				      ((keim~equal element (caaar table))
				       (cons (car table) (all-operations-left element (cdr table))))
				      (t (all-operations-left element (cdr table))))))
    (let ((row-list (mapcar #'all-operations-left elements))
	  (element-number (length elements)))
      (when (every #'(lambda (row) (= (length row) element-number)) row-list)
	(do* ((row row-list (cdr row))
	      (operation
	       (mapcar #'(lambda (element)
			   (find element (car row) :test #'keim~equal :key #'cadar))
		       elements)
	       (mapcar #'(lambda (element)
			   (find element (car row) :test #'keim~equal :key #'cadar))
		       elements))
	      (result (list (mapcar #'second operation))
		      (cons (mapcar #'second operation) result)))
	    ((or (null (cdr row)) (null operation))
	     (and operation
		  (every #'(lambda (x) (notany #'null x)) result)
		  (reverse result))))
	))))

(defun rcl~table2operations-list (elements table)
  (declare (edited  "07-DEC-2003")
	   (authors Vxs)
	   (input   "A list of elements and a list representing a multiplication table.")
	   (effect  "None.")
	   (value   "A list representing the single binary operations determined by the table."))
  (mapcan #'(lambda (first-element row)
	      (mapcar #'(lambda (second-element column)
			  (list (list first-element second-element) column))
		      elements row))
	  elements table))

(defun rcl=compute-all-traces (elements table)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "A list of elements and list of lists representing a multiplication table.")
	   (effect  "None.")
	   (value   "A list containing the traces of all elements under the operation determined"
		    "by the multiplication table."))
  (mapcar #'(lambda (element) (rcl=compute-trace element elements table)) elements))


(defun rcl=compute-trace (element elements table &key (key :operations))
  (declare (edited  "07-DEC-2003" "24-NOV-2003")
	   (authors Vxs Vxs)
	   (input   "An element, a list of elements and list of lists representing a multiplication table."
                    "given in terms of single binary operations, i.e. (((a b) c) ((a c) d) \ldots) .")
	   (effect  "None.")
	   (value   "A list containing the trace of the element under the operation determined"
		    "by the multiplication table."))
  (when (find element elements)
    (do* ((trace nil (cons trace-element trace))
	  (trace-element element
			 (if (equal key :operations)
			     (rcl=element-pair2result-in-operations-list table trace-element element)
			   (rcl=element-pair2result-in-table elements table trace-element element)
			   ;; this is for lists that represent a multiplication table
			   )))
	((find trace-element trace) (reverse trace)))))

(defun rcl=element-pair2result-in-operations-list (table first-element second-element)
  (declare (edited  "07-DEC-2003" )
	   (authors Vxs)
	   (input   "A list of lists representing binary operations and their results of the form"
		    "((a b) c), and two single elements.")
	   (effect  "None.")
	   (value   "The result of multiplying the two elements as given in the table. In case"
		    "the binary operation is not defined for the two elements NIL is returned."))
  (cadr
   (find-if #'(lambda (x)
		(and (keim~equal (first x) first-element)
		     (keim~equal (second x) second-element)))
	    table
	    :key #'car)))

;;(defun rcl=compute-trace2 (element elements operations-list)
;;  (declare (edited  "07-DEC-2003")
;;           (authors Vxs)
;;           (input   "An element, a list of elements and list of lists representing a multiplication table"
;;           (effect  "None.")
;;           (value   "A list containing the trace of the element under the operation determined"
;;                    "by the multiplication table."))
;;  (when (find element elements)
;;    (do* ((trace nil (cons trace-element trace))
;;          (trace-element element 
;;
;;  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following is currently not in use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rcl=elements2equations (elements table)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "A list of elements (arbitrary) and a multiplication table given as a list of lists.")
	   (effect  "None.")
	   (value   "A list of all equations specified by the table."))
  (mapcan #'(lambda (left lhss)
	      (mapcar #'(lambda (right lhs)
			  (list lhs left right))
		      elements lhss))
	  elements table))

(defun rcl=sort-wrt-lhs (elements equations)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "A list of elements and a list of triples representing equations.")
	   (effect  "None.")
	   (value   "A list of lists sorted with respect to the lefthand sides of the equations."))
  (mapcar #'(lambda (element)
	      (remove-if-not #'(lambda (x) (equal x element))
			     equations :key #'car))
	  elements))

(defun rcl=substitute-in-equation (equ1 equ2)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "Two triples representing equations.")
	   (effect  "None.")
	   (value   "The modified EQU1 where all occurrences of the lefthand side of EQU2 are replaced"
		    "by the righthand side of EQU2"))
  (let ((rhs (cdr equ1))
	(to-replace (car equ2))
	(replace-by (cdr equ2)))
    (cons (car equ1)
	  (rcl=substitute-recursively rhs to-replace replace-by))))

(defun rcl=substitute-recursively (list a b)
  (declare (edited  "24-NOV-2003")
	   (authors Vxs)
	   (input   "A list, an atom and an arbitrary structure.")
	   (effect  "None.")
	   (value   "The LIST where all occurrences of A are replaced by B"))
  (cond ((null list) list)
	((and (atom list) (equal list a) b))
	((listp list) (list (rcl=substitute-recursively (car list) a b)
			    (rcl=substitute-recursively (cadr list) a b)))
	(t list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The previous is currently not in use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

