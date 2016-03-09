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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions for ZmZ Theory files
;; in particular predicates and functions for zmz-tactics.thy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The following is a little hack to elegantly deal with all the multiple value constructs below.
;; (Ameier seems to love those.)

(defmacro multiple-value-or (&body forms)
  (declare (edited  "08-JUN-2003")
	   (authors Vxs)
	   (input   "Some forms.")
	   (effect  "The macro behaves similar to the regular or on single value forms."
		    "I.e. the forms are evaluated sequentially. If a form is non-nil"
		    "(or its first return value is non-nil) all its values are returned."
		    "Otherwise the next form is evaluated.")
	   (value   "The values of the first form that evaluates to non-nil. NIL if there is no such form."))
  (when forms
     `(let ((list (multiple-value-list ,(car `,forms))))
       (if (car list) (values-list list)
	 (multiple-value-or ,@(rest forms))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun zmztac=prod-of-resclass-sets-p (formula)
  (declare (edited  "03-AUG-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T of the formula represents a direct product of sets of resclasses."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (do* ((rest-list (list formula))
	  (flag 't))
	((or (null rest-list)
	     (null flag))
	 flag)
      (let* ((head (first rest-list)))
	
	(cond ((and (data~appl-p head)
		    (keim~equal (data~appl-function head)
				(data~schema-range (env~lookup-object 'cartesian-product env))))
	       (setf rest-list (append (data~appl-arguments head) (rest rest-list))))
	      ((zmztac=resclass-set-p head)
	       (setf rest-list (rest rest-list)))
	      (t
	       (setf flag nil)))))))
				

;; Es kann verschiedene Repraesentationen geben fuer Mengen die Resclassen
;; 1. (resclass-set n)
;; 2.1. (setminus (resclass-set n) (singleton (resclass n m))) 
;; 2.2. (setminus (resclass-set n) (lam (x (o num)) (or (= x (resclass n m)) ... )))
;; 3. (or (= x (resclass n m1)) (= x (resclass n m2)) ...
;;
;;


(defun zmztac=resclass-set-p (formula)
  (declare (edited  "08-JUN-2003" "25-FEB-2000")
	   (authors Vxs Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple values:"
		    "First: T if the formula represents a residue class set.."
		    "Second: If first is t then the class-factor n."
		    "Third: If first is t then a list of elements m1, m2, ... as numbers."))
  (multiple-value-or
   ;; 1.) (resclass-set n)
   ;;     Formula is an application, starts with resclass-set, and has type o <- (o <- num)
   (multiple-value-bind (flag class-factor)
       (zmz=resclass-set-formula-p formula)
     (when flag
       (values t class-factor (zmztac=count-first-n-nats class-factor))))
   ;; 2.) 2.1.: (setminus (resclass-set n) (singleton (resclass n m))) or
   ;;     2.2.: (setminus (resclass-set n) (set (resclass n m) .... ))
   ;;     2.3.: (setminus (resclass-set n) (lam (x (o num)) (or (= x (resclass n m)) .... )))
   ;;     Formula is an application, starts with setminus, has two arguments, the first argument satisfies the conditions of
   ;;     the first case.
   (when (and (zmztac=setminus-appl-p formula)
	      (= (length (data~appl-arguments formula)) 2))
     (multiple-value-bind (flag1 class-factor1)
	 (zmz=resclass-set-formula-p (first (data~appl-arguments formula)))
       (when flag1 
	 (let ((minus-set (second (data~appl-arguments formula))))
	   (multiple-value-bind (flag2 class-factor2 elements)
	       (multiple-value-or
		(zmz=resclass-singleton-p minus-set) ;; 2.1.: (setminus (resclass-set n) (singleton (resclass n m)))
		(zmz=set-of-resclass-p minus-set)    ;; 2.2.: (setminus (resclass-set n) (set (resclass n m) .... ))
		(zmz=or-set-of-resclass-p minus-set) ;; 2.3.: (setminus (resclass-set n) (lam (x (o num)) (or (= x (resclass n m)) .... )))
		)
	     (when (and flag2 (keim~equal class-factor1 class-factor2))
	       (values t class-factor2
		       (set-difference (zmztac=count-first-n-nats class-factor1)
				       (if (listp elements) elements (list elements))
				       :test #'keim~equal))))))))
   ;; 3.) (set (resclass n m1) (resclass n m2) .... )
   (zmz=set-of-resclass-p formula)
   ;; 4.) (lam (x (o num)) (or (= x (resclass n m1)) (= x (resclass n m2)) ... ))
   (zmz=or-set-of-resclass-p formula)
   ))

(defun zmz=set-of-resclass-p (term)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the formula has the form '(set (resclass n m1) (resclass n m2) ...)'."
		    "Second: If first is t then n."
		    "Third: If first is t then list m1, m2, ..."))
  (when (term~set-p term)
    (let ((result (mapcar #'(lambda (x)
			      (multiple-value-list (zmz=resclass-formula-p x)))
			  (keim~name term))))
      (when (notany #'(lambda (x) (null (car x)))  result)
	(let* ((ns (mapcar #'second result))
	       (ms (mapcar #'third result))
	       (fn (car ns)))
	  (when (every #'(lambda (x) (keim~equal fn x)) (cdr ns))
	    (values t fn ms)))))))
    
(defun zmz=or-set-of-resclass-p (formula)
  (declare (edited  "29-FEB-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the formula has the form '(lam (x (o num) (or (= x (resclass n m1)) (= x (resclass n m2)) ...))'."
		    "Second: If first is t then n."
		    "Third: If first is t then list m1, m2, ..."))
  (when (data~abstr-p formula)
    (let* ((domain (data~abstr-domain formula))
	   (range (data~abstr-range formula))
	   (env (pds~environment omega*current-proof-plan))
	   (var (first domain)))
      (unless (null (and (= (length domain) 1)
		     (keim~equal (term~type var)
				 (post~read-object '(o num) env :existing-type))))
	(do* ((rest-formulas (list range))
	      (class-factor nil)
	      (residues nil)
	      (flag 't))
	    ((or (null rest-formulas)
		 (null flag))
	     (if (null flag)
		 (values nil nil nil)
	       (values 't class-factor residues)))
	  (let* ((head-rest-formula (first rest-formulas)))

	    (cond (;; head-rest-formula = (= X (resclass n m1))
		   (and (zmztac=equal-appl-p head-rest-formula)
			(= (length (data~appl-arguments head-rest-formula)) 2))

		   (setf rest-formulas (rest rest-formulas))
		   
		   (let* ((first-arg (first (data~appl-arguments head-rest-formula)))
			  (second-arg (second (data~appl-arguments head-rest-formula))))
		     (cond ((keim~equal first-arg var)
			    (multiple-value-bind
				(curr-flag curr-class-factor curr-residuum)
				(zmz=resclass-formula-p second-arg)
			      (if curr-flag
				  (cond ((null class-factor)
					 (setf class-factor curr-class-factor)
					 (setf residues (remove-duplicates (append residues (list curr-residuum)))))
					((keim~equal class-factor curr-class-factor)
					 (setf residues (remove-duplicates (append residues (list curr-residuum)))))
					(t
					 (setf flag nil)))
				(setf flag nil))))
			   ((keim~equal second-arg var)
			    (multiple-value-bind
				(curr-flag curr-class-factor curr-residuum)
				(zmz=resclass-formula-p first-arg)
			      (if curr-flag
				  (cond ((null class-factor)
					 (setf class-factor curr-class-factor)
					 (setf residues (remove-duplicates (append residues (list curr-residuum)))))
					((keim~equal class-factor curr-class-factor)
					 (setf residues (remove-duplicates (append residues (list curr-residuum)))))
					(t
					 (setf flag nil)))
				(setf flag nil))))
			   (t
			    (setf flag nil)))))
		  (;; head-rest-formula = (or S1 S2)
		   (logic~disjunction-p head-rest-formula)

		   (setf rest-formulas (append (rest rest-formulas) (data~appl-arguments head-rest-formula))))
		  (t
		   (setf flag nil)))))))))
	      
(defun zmz=resclass-formula-p (formula)
  (declare (edited  "29-FEB-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: T if the formula has the form '(resclass n m)'."
		    "Second: If first is true, then n."
		    "Third: If first is true, then m."))
  (cond ((and (zmztac=resclass-appl-p formula)               ;;;;; first case is the old and simple case
	      (= (length (data~appl-arguments formula))) 2)
	 (values 't
		 (first (data~appl-arguments formula))
		 (second (data~appl-arguments formula))))
	((and (term~tuple-p formula)                    ;;;; this is the tuple case which could cause many problems!!!!!!!!  VS
	     (every #'zmz=resclass-formula-p (keim~name formula)))
	(values t
		(mapcar #'(lambda (x) (first (data~appl-arguments x))) (keim~name formula))
		(mapcar #'(lambda (x) (second (data~appl-arguments x))) (keim~name formula))))
	))
	 
	 
(defun zmz=resclass-singleton-p (formula)
  (declare (edited  "29-FEB-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: T if the formula has the form '(singleton (resclass n m))', nil otherwise."
		    "Second: If first is t, then n otherwise nil."
		    "Third: If first is t, then m otherwise nil."))
  (if (and (data~appl-p formula)
	   (keim~equal (data~appl-function formula)
		       (data~schema-range (env~lookup-object :singleton (pds~environment omega*current-proof-plan))))
	   (= (length (data~appl-arguments formula)) 1))
      (let* ((subformula (first (data~appl-arguments formula))))
	(multiple-value-bind
	    (flag class-factor residuum)
	    (zmz=resclass-formula-p subformula)
	  (values flag class-factor residuum)))
    (values nil nil nil)))

(defun zmz=resclass-set-formula-p (formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: T if the formula has the form '(resclass-set n)', nil otherwise."
		    "Second: If first is t, then n otheriwse nil."))
  (if (and (data~appl-p formula)
           (keim~equal (data~appl-function formula)
                       (env~lookup-object :resclass-set (pds~environment omega*current-proof-plan)))
	   (keim~equal (term~type formula)
		       (post~read-object '(o (o num)) (pds~environment omega*current-proof-plan) :existing-type)))
      (values 't
	      (first (data~appl-arguments formula)))
    (values nil nil)))
	            

(defun zmztac=operator-is-binary-operator-on-resclasses-p (op)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if the term has the type (o num) <- (o num) (o num)."))
  (let* ((term-type (term~type op)))
    (keim~equal term-type (post~read-object '(o num (o num) (o num))
					    (pds~environment omega*current-proof-plan)
					    :existing-type))))
  

(defun zmztac=resclass-in-resclass-set-p (formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula has the form '(RESCLASS-SET R)'."))
  (if (data~appl-p formula)
      (let* ((func (data~appl-function formula))
	     (args (data~appl-arguments formula)))
	(cond ((> (length args) 1)
	       (let* ((resclass-set-term (term~appl-create func (butlast args)))
		      (resclass-term (first (last args))))
		 (if (zmztac=resclass-set-p resclass-set-term)
		     't
		   nil)))
	      (t
	       (if (zmztac=resclass-set-p func)
		   't
		 nil))))
    nil))

(defun zmztac=resclasses-of-same-resclass-set-p (formula1 formula2)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "Two formulas representing resclasses.")
	   (effect  "None.")
	   (value   "T if the resclasses are with respect to the same resclass-set."))

  (let* ((func1 (data~appl-function formula1))
	 (func2 (data~appl-function formula2))
	 (args1 (data~appl-arguments formula1))
	 (args2 (data~appl-arguments formula2)))

    (term~alpha-equal (term~appl-create func1 (butlast args1))
		      (term~appl-create func2 (butlast args2)))))


(defun zmztac=corresponding-equation-p (eq formula1 formula2)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "Three formulas, such that the second and the third formula are of the form"
		    "'(RESCLASS-SET R1)', '(RESCLASS-SET R2)'.")
	   (effect  "None.")
	   (value   "T if the first formula has the form '(R1 = R2)'."))
  (let* ((resclass1 (first (last (data~appl-arguments formula1))))
	 (resclass2 (first (last (data~appl-arguments formula2)))))
    (keim~equal eq
		(term~appl-create (env~lookup-object '= (pds~environment omega*current-proof-plan))
				  (list resclass1 resclass2)))))


(defun zmztac=resclass-equation-at-pos-p (form pos)
  (declare (edited  "02-MAR-2000")
	   (authors Ameier)
	   (input   "A formula and a position.")
	   (effect  "None.")
	   (value   "T if the formula has at the position the form '(= R1 R2)', where R1 and R2 have the form:"
		    "'(resclass n m1,2)' or a terms composed by plus-resclass, minus-resclass, times-resclass."))
  (let* ((formula (data~struct-at-position form pos)))
    
    (when (and (zmztac=equal-appl-p formula)
	       (= (length (data~appl-arguments formula)) 2))
      (let* ((args (data~appl-arguments formula))
	     (class-factor1 (or (zmztac=composed-resclass-p (first args))
				(zmztac=composed-resclass-tuple-p (first args))))
	     (class-factor2 (or (zmztac=composed-resclass-p (second args))
				(zmztac=composed-resclass-tuple-p (second args)))))
	    (when (and class-factor1 class-factor2 (keim~equal class-factor1 class-factor2))
		't)))))
  
(defun zmztac=composed-resclass-p (formula)
  (declare (edited  "20-JUN-2003" "02-MAR-2000")
	   (authors Vxs Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "If the formula has the form '(resclass n m)' or is composed of such formulas and"
		    "the operations plus/minus/times-resclass where all subformulas of the form '(resclass n m)'"
		    "have the same class-factor n, n is returned. Otherwise NIL."))
  (do* ((rest-formulas (list formula))
	(class-factor nil)
	(flag 't))
      ((or (null flag)
	   (null rest-formulas))
       (when flag class-factor))
    (let* ((head-formula (first rest-formulas)))
      (cond ((and (zmztac=resclass-appl-p head-formula)
		  (= (length (data~appl-arguments head-formula)) 2))
	     ;; head-formula has form '(resclass n m)'
	     (if (null class-factor)
		 (setf class-factor (first (data~appl-arguments head-formula))
		       rest-formulas (rest rest-formulas))
	       (if (keim~equal (first (data~appl-arguments head-formula)) class-factor)
		   (setf rest-formulas (rest rest-formulas))
		 (setf flag nil))))
	    ((and (data~appl-p head-formula)
		  (zmztac=convertable-operator-on-resclasses-p (data~appl-function head-formula))
		  (= (length (data~appl-arguments head-formula)) 2))
	     ;; head-formula has form '(plus/minus/times-resclass r1 r2)
	     (setf rest-formulas (append (rest rest-formulas) (data~appl-arguments head-formula))))
	    (t
	     (setf flag nil))))))
		      
(defun zmztac=composed-resclass-tuple-p (term)
  (declare (edited  "20-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "If the term is a tuple containing resclass terms of the form '(resclass n m)' or"
		    "is composed of such tuples and pair operations containing"
		    "the operations plus/minus/times-resclass where all subterms are tuples"
		    "with respect to the same class-factor (n1,n2,\\ldots), (n1,n2,\\ldots) is returned."
		    "Otherwise NIL."
		    "Tuples can be either special tuple terms or composed with the PAIR operator."))
  
  )


(defun zmztac=resclass-equality-p (formula)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula is an equality involving a term of type (o num)."))
  (and (zmztac=equal-appl-p formula)
       (keim~equal (term~type (first (data~appl-arguments formula)))
		   (post~read-object '(o num) 
				     (pds~environment omega*current-proof-plan)
				     :existing-type))))



(defun zmztac=residue-class-equation-p (formula)
  (declare (edited  "06-MAR-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula is an equation on residue classes."))
  (when (zmztac=equal-appl-p formula)
    (let* ((args (data~appl-arguments formula))
	   (farg (first args))
	   (sarg (second args)))
      (when (and (or (zmztac=composed-resclass-p farg)
		     (and (zmztac=resclass-appl-p farg)
			  (= (length (data~appl-arguments farg)) 2)))
		 (or (zmztac=composed-resclass-p sarg)
		     (and (zmztac=resclass-appl-p sarg)
			  (= (length (data~appl-arguments sarg)) 2))))
	't))))


(defun zmztac=resclass-in-resclass-set-at-pos-p (formula pos)
  (declare (edited  "03-MAR-2000")
	   (authors Ameier)
	   (input   "A formula and a position.")
	   (effect  "None.")
	   (value   "T if the subformula at position has the form '(Resclass-Set resclass)'."))
  (let* ((subformula (data~struct-at-position formula pos)))
    (zmztac=resclass-in-resclass-set-p subformula)))


(defun zmztac=convertable-operator-on-resclasses-p (op)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "NOne.")
	   (value   "T if the term is a binary operation on resclasses that can be converted into a"
		    "operation on numbers."))
  (and (zmztac=operator-is-binary-operator-on-resclasses-p op)
       (or (keim~equal op (env~lookup-object 'plus-resclass (pds~environment omega*current-proof-plan)))
	   (keim~equal op (env~lookup-object 'minus-resclass (pds~environment omega*current-proof-plan)))
	   (keim~equal op (env~lookup-object 'times-resclass (pds~environment omega*current-proof-plan))))
       ;; -> kann man noch erweitern!
       ))

(defun zmztac=class-residue-operator-and-args-at-position-p (conc-formula pos op)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula, a position, and a binary operation on resclasses.")
	   (effect  "None.")
	   (value   "T if the formula has at position the form '(class-residue (op R1 R2) N)'."))
  (let* ((struct-at-pos (data~struct-at-position conc-formula pos)))

    (if (and (data~appl-p struct-at-pos)
	     (keim~equal (data~appl-function struct-at-pos) (env~lookup-object :class-residue (pds~environment omega*current-proof-plan)))
	     (= (length (data~appl-arguments struct-at-pos)) 2))
	(let* ((op-arg (first (data~appl-arguments struct-at-pos))))
	  (if (and (data~appl-p op-arg)
		   (= (length (data~appl-arguments op-arg)) 2)
		   (keim~equal (data~appl-function op-arg) op))
	      't
	    nil))
      nil)))

(defun zmztac=class-resdue-resclass-appl-p (term)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if the term is of the form (class-residue (resclass \\ldots))."))
  (and (zmztac=class-residue-appl-p term)
       (data~appl-p (first (data~appl-arguments term)))
       (not (zmztac=resclass-appl-p (first (data~appl-arguments term))))))

(defun zmztac=class-residue-at-position-p (formula pos)
  (declare (edited  "03-MAR-2000")
	   (authors Ameier)
	   (input   "A formula and a position.")
	   (effect  "None.")
	   (value   "T if the subformula at position has the form '(class-residue ... n)'."))
  (let* ((struct-at-pos (data~struct-at-position formula pos)))
    (and (data~appl-p struct-at-pos)
	 (keim~equal (data~appl-function struct-at-pos) (env~lookup-object :class-residue (pds~environment omega*current-proof-plan)))
	 (= (length (data~appl-arguments struct-at-pos)) 2))))

(defun zmztac=pushable-class-residue-positions-p (form)
  (declare (edited  "03-MAR-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "A list of all positions of subterms of the formula of the form:"
		    "'(class-residue (op R1 R2) n)' where op is a pushable operator."))
  (data~positions form #'(lambda (formula)
			   (if (and (zmztac=class-residue-appl-p formula)
				    (= (length (data~appl-arguments formula)) 2))
			       (let* ((op-arg (first (data~appl-arguments formula))))
				 (if (and (data~appl-p op-arg)
					  (= (length (data~appl-arguments op-arg)) 2)
					  (zmztac=convertable-operator-on-resclasses-p (data~appl-function op-arg)))
				     't
				   nil))
			     nil))))

(defun zmztac=replace-class-residues-terms-at-positions-list-p (formula pos-list)
  (declare (edited  "28-FEB-2000")
	   (authors Ameier)
	   (input   "A formula and a list of position.")
	   (effect  "None.")
	   (value   "T if each subformula at a position has the form '(class-residue (resclass N C) N)'."))
  (every #'(lambda (pos)
	     (zmztac=replaceable-class-residue-p (data~struct-at-position formula pos)))
	 pos-list))

(defun zmztac=replaceable-class-residue-p (formula)
  (declare (edited  "07-JUN-2003" "06-MAR-2000")
	   (authors Vxs Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula has the form '(class-residue (resclass n m) n)'."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (num-type (env~lookup-object 'num env))
	 (nvar (term~variable-create 'n num-type))
	 (cvar (term~variable-create 'c num-type))
	 (match-term (term~appl-create (env~lookup-object 'class-residue env)
				       (list (term~appl-create (env~lookup-object 'resclass env)
							       (list nvar cvar))
					     nvar)))
	 (match (bind~with-bindings ((keim::term=alpha-equal match-term formula (list nvar cvar))))))
    (when match 't)))

	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Predicates to simplify Mr Meier's Horror-Code !!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun zmztac=resclass-appl-p (term)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if the term is a functional application of the RESCLASS constant."))
  (and (data~appl-p term)
       (keim~equal (data~appl-function term)
		   (env~lookup-object 'resclass (pds~environment omega*current-proof-plan)))))

(defun zmztac=equal-appl-p (term)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if the term is a functional application of the equality constant =."))
  (and (data~appl-p term)
       (keim~equal (data~appl-function term)
		   (data~schema-range
		    (env~lookup-object '= (pds~environment omega*current-proof-plan))))))

(defun zmztac=class-residue-appl-p (term)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if the term is a functional application of the CLASS-RESIDUE constant."))
  (and (data~appl-p term)
       (keim~equal (data~appl-function term) 
		   (env~lookup-object 'class-residue (pds~environment omega*current-proof-plan)))))
       
(defun zmztac=forall-sort-appl-p (term)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if the term is a functional application of the FORALL-SORT constant."))
  (and (data~appl-p term)
       (keim~equal (data~appl-function term) 
		   (data~schema-range (env~lookup-object :forall-sort (pds~environment omega*current-proof-plan))))))

(defun zmztac=exists-sort-appl-p (term)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if the term is a functional application of the EXISTS-SORT constant."))
  (and (data~appl-p term)
       (keim~equal (data~appl-function term) 
		   (data~schema-range (env~lookup-object :exists-sort (pds~environment omega*current-proof-plan))))))

(defun zmztac=setminus-appl-p (term)
  (declare (edited  "07-JUN-2003")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if the term is a functional application of the SETMINUS constant."))
  (and (data~appl-p term)
       (keim~equal (data~appl-function term) 
		   (data~schema-range (env~lookup-object :setminus (pds~environment omega*current-proof-plan))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun zmztac=class-factor (formula)
  (declare (edited  "08-JUN-2003" "25-FEB-2000")
	   (authors Vxs Ameier)
	   (input   "A formula, representing a set of resclasses with a common class factor.")
	   (effect  "None.")
	   (value   "The common class factor of this set of resclasses."))
  (multiple-value-bind (flag class-factor)
      (multiple-value-or
       (zmztac=resclass-set-p formula)
       (zmztac=resclass-tuple-set-p formula))
    (when flag class-factor)))


(defun zmztac=class-factor-of-resclass (term)
  (declare (edited  "28-FEB-2000")
	   (authors Ameier)
	   (input   "A term representing a resclass.")
	   (effect  "None.")
	   (value   "The class factor of the resclass."))
  (cond ((and (data~appl-p term)
	      (zmztac=convertable-operator-on-resclasses-p (data~appl-function term)))
	 (zmztac=class-factor-of-resclass (first (data~appl-arguments term))))
	((zmztac=resclass-appl-p term)
	 (first (data~appl-arguments term)))
	(t
	 (omega~error "~%Not implemented yet, zmztac=class-factor-of-resclass."))))
	      


(defun zmztac=compute-corresponding-class-residue-equation-at-pos (formula pos)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula and a position such that the subterm in the formula has the form '(= R1 R2)'.")
	   (effect  "None.")
	   (value   "A formula of the form: '(= (class-residue r1) (class-residue r2))'."))
  
  (let* ((eq (data~struct-at-position formula pos))
	 (class-residue (env~lookup-object 'class-residue (pds~environment omega*current-proof-plan)))
	 (resclass1 (first (data~appl-arguments eq)))
	 (resclass2 (second (data~appl-arguments eq)))
	 (class-factor (zmztac=class-factor-of-resclass resclass1))
	 (new-eq (term~appl-create (env~lookup-object '= (pds~environment omega*current-proof-plan))
				   (list (term~appl-create class-residue (list resclass1 class-factor))
					 (term~appl-create class-residue (list resclass2 class-factor))))))

    (data~replace-at-position formula pos new-eq)))    


(defun zmztac=compute-class-residue-in-number-set-at-pos (form pos)
  (declare (edited  "02-MAR-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(Resclass-Set resclass)'.")
	   (effect  "None.")
	   (value   "A formula of the form '(Number-Set (class-residue resclass))' where number-set is the"
		    "set of natural numbers corresponding to Resclass-Set."))
  (let* ((formula (data~struct-at-position form pos))
	 (func (data~appl-function formula))
	 (args (data~appl-arguments formula))
	 (resclass-set (if (term~set-p func) func (term~appl-create func (butlast args))))
	 (number-set (zmztac=number-set-to-resclass-set resclass-set))
	 (class-factor (zmztac=class-factor resclass-set))
	 (new-subformula (term~appl-create number-set
					   (list (term~appl-create (env~lookup-object :class-residue (pds~environment omega*current-proof-plan))
								   (list (first (last args))
									 class-factor))))))
    
    (data~replace-at-position form pos new-subformula)))


(defun zmz=resclass-set (formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(RESCLASS-SET R)'.")
	   (effect  "None.")
	   (value   "RESCLASS-SET"))
  (let* ((func (data~appl-function formula))
	 (args (data~appl-arguments formula)))
    
    (cond ((= (length args) 1)
	   func)
	  (t
	   (term~appl-create func (butlast args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun zmztac=residue-class-set-to-number-list (formula)
  (declare (edited  "08-JUN-2003" "25-FEB-2000")
	   (authors Vxs Ameier)
	   (input   "A formula, representing a set of resclasses with a common class factor.")
	   (effect  "None.")
	   (value   "A list of numbers."))
  (multiple-value-bind (flag class-factor number-list)
      (zmztac=resclass-set-p formula)
    (declare (ignore class-factor))
    (when flag number-list)))


(defun zmztac=number-set-to-resclass-set (formula)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula, representing a set of resclasses with a common class factor.")
	   (effect  "None.")
	   (value   "A formula, representing the corresponding set of natural numbers."))
  (multiple-value-bind (flag class-factor number-list)
      (zmztac=resclass-set-p formula)
    (declare (ignore class-factor))
    (when flag (zmztac=produce-number-set number-list))))


(defun zmztac=convert-resclass-operation-to-num-operation (operation class-factor)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "An opertion on resclasses and the class-factor of the rest-classes.")
	   (effect  "None.")
	   (value   "The corresponding operation on nums."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (num-type (env~lookup-object 'num env))
	 (var1 (term~variable-create 'n1 num-type))
	 (var2 (term~variable-create 'n2 num-type)))
  (cond ((or (keim~equal operation (env~lookup-object 'plus-resclass env))
	     (keim~equal operation (env~lookup-object 'plusgen-resclass env)))
	 (term~abstr-create (list var1 var2)
			    (term~appl-create (env~lookup-object 'mod env)
					      (list (term~appl-create (env~lookup-object 'plus env)
								      (list var1 var2))
						    class-factor))))
	((or (keim~equal operation (env~lookup-object 'minus-resclass env))
	     (keim~equal operation (env~lookup-object 'minusgen-resclass env)))
	 (term~abstr-create (list var1 var2)
			    (term~appl-create (env~lookup-object 'mod env)
					      (list (term~appl-create (env~lookup-object 'minus env)
								      (list var1 var2))
						    class-factor))))
	((or (keim~equal operation (env~lookup-object 'times-resclass env))
	     (keim~equal operation (env~lookup-object 'timesgen-resclass env)))
	 (term~abstr-create (list var1 var2)
			    (term~appl-create (env~lookup-object 'mod env)
					      (list (term~appl-create (env~lookup-object 'times env)
								      (list var1 var2))
						    class-factor))))
	(t
	 (omega~error "~%Error in function zmztac=convert-resclass-operation-to-num-operation, convertion for operation ~A not implemented yet!"  operation)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun zmztac=count-first-n-nats (number)
  (declare (edited  "29-FEB-2000")
	   (authors Ameier)
	   (input   "An integer.")
	   (effect  "None.")
	   (value   "A list with the first n natural numbers: (0 ... n-1)."))
  (when (term~number-p number)
    (let* ((env (pds~environment omega*current-proof-plan)))
      (do* ((rest-number (abs (if (numberp number)
				  number
				(keim~name number)))
			 (decf rest-number))
	  (number-list nil (cons (post~read-object rest-number env :existing-term) number-list)))
	  ((= rest-number 0)
	   number-list)))))

	   
(defun zmztac=produce-number-set (number-list)
  (declare (edited  "29-FEB-2000")
	   (authors Ameier)
	   (input   "A list of natural numbers '(n1:num n2:num ... nm:num)'"
		    "or a list of tuples of natural numbers.")
	   (effect  "None.")
	   (value   "A formula of the form: '(lam (x num) (or (= x n1) ... (= x nm),"
		    "respectively '(lam (x (num num ...)) (or (= x (n11 n12...))...)."))
  (when (null number-list)
    (omega~warn "~%In function zmztac=produce-number-set: number-list should not be nil!"))
  
  (let* ((env (pds~environment omega*current-proof-plan))
	 (num (env~lookup-object :num env))
	 (or (env~lookup-object :or env))
	 (equality (env~lookup-object := env))
	 (var (term~variable-create 'x num))
	 (range (do* ((rest-number-list (reverse number-list) (rest rest-number-list))
		      (formula nil))
		    ((null rest-number-list)
		     formula)
		  (let* ((head-number (first rest-number-list))
			 (equation (term~appl-create equality (list var head-number))))
		    (cond ((null formula)
			   (setf formula equation))
			  (t
			   (setf formula (term~appl-create or
							   (list equation formula)))))))))
    (term~abstr-create (list var) range)))
				 

(defun zmztac=compute-pushed-line (formula pos op)
  (declare (edited  "25-FEB-2000")
	   (authors Ameier)
	   (input   "A formula, a position, and an operator op such that the formula has at the position the"
		    "form '(class-residue (op r1 r2)', and op can be converted into a corresponding operator op'"
		    "on nums.") 
	   (effect  "None.")
	   (value   "A formula that has at position the term '(op' (class-residue r1) (class-residue r2)'."))
  
  (let* ((struct-at-pos (data~struct-at-position formula pos))
	 (resclass-args (data~appl-arguments (first (data~appl-arguments struct-at-pos))))
	 (resclass1 (first resclass-args))
	 (resclass2 (second resclass-args))
	 (class-factor (zmztac=class-factor-of-resclass resclass1))
	 (class-residue (env~lookup-object 'class-residue (pds~environment omega*current-proof-plan)))
	 (converted-operation (zmztac=convert-resclass-operation-to-num-operation op class-factor))
	 (new-subterm (term~appl-create converted-operation
					(list (term~appl-create class-residue
								(list resclass1 class-factor))
					      (term~appl-create class-residue
								(list resclass2 class-factor))))))
    
    (data~replace-at-position formula pos new-subterm)))

