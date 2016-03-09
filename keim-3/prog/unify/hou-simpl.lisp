;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

;; Revisions: 10.Jan. added data~defgenerics (kk)
;;
;;

(in-package "KEIM")

(mod~defmod hou-simpl :uses (uni-main) 
	    :documentation "Abstract interface to higher order simplification
                            includes algorithms and the unification-problem
                            datastructure."
	    :exports (hou+nullvariable
                      hou~generate-nullvariable
                      hou~nullvariable-p
                      hou~bind
                      hou~alpha-eta
                      hou~decompose
		      hou~flex-p
                      hou~simplify
                      hou~simplify-aux
                      )
	    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hou nullvariables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval) 
  (defclass hou+nullvariable (;term+variable ; kleiner bugfix -kk-
                              term+constant)
    ()
    (:documentation
     "Nullvariables are variables for HOU which can't unify
      with anything except themselves. They are a mixture of variables
      and constants.")))

(defmethod hou~generate-nullvariable ((ref term+variable))
  (declare (edited  "1.4.96 5.10.00")
	   (authors kk cullrich)
	   (input   "a reference variable")
	   (value   "")
	   (effect "Nullvariables are variables for HOU which can't unify
                 with anything except themselves. Ref is a reference variable
                 with the same type."))
  (let ((nullvariable (change-class
                  (data~variable-create ref :name (gensym "?Z"))
                  'hou+nullvariable)))
    (setf (data~annotation nullvariable) (data~annotation ref))
    ;; we save the reference variable in order to be able to retrieve it
    ;; needed for term mappings in analogy
    (keim~set-plist! nullvariable
		     (acons 'reference-variable
			    ref
			    (keim~plist nullvariable)))
    nullvariable))

  
(defmethod hou~nullvariable-p ((variable hou+nullvariable))
  (declare (edited  "1.12.96")
           (authors kk)
           (input   "a variable")
           (effect  "tests whether object is a nullvar or not.")
           (value   "truth value"))
  T)

(defmethod hou~nullvariable-p ((variable term+term))
  (declare (edited  "1.12.96")
           (authors kk)
           (input   "a variable")
           (effect  "tests whether object is a nullvar or not.")
           (value   "truth value"))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hou binding of variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod uni~bind ((variable term+variable)(term data+object)(uni uni+hou))
  (declare (edited  "11.12.96")
           (authors kk)
           (input   "variable and term")
           (effect  "Sets binding of variable to term.")
           (value   "nil when failure or, term if success."))
  (unless (or (hou~nullvariable-p variable)
              (hou~occur-p variable term uni)
              (hou~contains-nullvariable-p term uni))
    (bind~bind variable term)
    term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Occur- and nullvar-Check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric hou~occur-p (variable term uni)
  (declare (edited  "11.12.96")
	   (authors kk)
	   (input   "an variable and a term")
	   (effect  "None.")
	   (value   "T if variable is not assignable to term."))
  (:method (variable (term term+constant) (uni uni+hou))
           (declare (ignore variable))
	   nil)
  (:method (variable (term term+variable) (uni uni+hou))
           (or (hou~trivial-p variable term uni) 
               (when (bind~binding term)
                 (hou~occur-p variable (bind~binding term) uni))))
  (:method (variable (term term+appl) (uni uni+hou))
      ;;; eigentlich will man fragen ob head(term) =/= Var, denn sonst darf man binden
      ;;; aber es tritt einFehler auf in verschiedenen Bsp.
      ;;; (and (not (data~variable-p  (data~appl-function term)))
		(or (hou~occur-p
		     variable (data~appl-function term) uni)
		    (dolist (subterm (data~appl-arguments term) nil)
		      (when (hou~occur-p variable subterm uni)
			(return T)))))
      ;;; )
  (:method (variable (term term+abstr) (uni uni+hou))
           (if (find variable (data~abstr-domain term) :test
                     #'(lambda (x y) (hou~trivial-p x y uni)))
               nil
             (hou~occur-p variable (data~abstr-range term) uni))))

;; old code
;;  (:method (variable (term term+abstr) (uni uni+hou))
;;           (if (find variable (data~abstr-domain term) :test
;;                     #'(lambda (x y) (hou~trivial-p x y uni)))
;;               (not (hou~contains-nullvariable-p (data~abstr-range term) uni))
;;             (hou~occur-p variable (data~abstr-range term) uni))))


(defgeneric hou~contains-nullvariable-p (term uni)
  (declare (edited  "21.4.96")
	   (authors kk)
	   (input   "a term")
	   (effect  "None.")
	   (value   "T if term contains a null-variable.
                     Note: This function takes bindings
                     into account."))
  (:method ((term hou+nullvariable)(uni uni+hou))
	   (declare (ignore variable))
	   t)
  (:method ((term term+constant)(uni uni+hou))
           (declare (ignore variable))
	   nil)
  (:method ((term term+variable)(uni uni+hou))
           (or (hou~nullvariable-p term)
               ;; probably not necessary -> bound variables cant have nullvars
               (when (bind~binding term)
                 (hou~contains-nullvariable-p (bind~binding term) uni))))
  (:method ((term term+appl)(uni uni+hou))
	   (or (hou~contains-nullvariable-p (data~appl-function term) uni)
	       (dolist (subterm (data~appl-arguments term) nil)
		 (when (hou~contains-nullvariable-p subterm uni)
		   (return T)))))
  (:method ((term term+abstr)(uni uni+hou))
	   (hou~contains-nullvariable-p (data~abstr-range term) uni)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The simple part of the Unification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alpha-eta-reduction on two terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod hou~alpha-eta ((term1 term+term) (term2 term+term) (uni uni+hou))
  (declare (edited  "16.4.96")
           (authors kk)
           (input   "term1 term2 (one of them an abstraction)
                     and unfictation structure uni.")
           (effect  "Executes ALPHA-eta-reduction to term1 and term2.")
           (value "two reduced terms"))
  (let* ((bind1 (and (data~abstr-p term1) (data~abstr-domain term1)))
         (bind2 (and (data~abstr-p term2) (data~abstr-domain term2)))        
         (lb1 (length (the list bind1)))
         (lb2 (length (the list bind2))))
    (if (> lb2 lb1)
        (hou~alpha-eta term2 term1 uni) ;; turn parameters
      ;; else
      (let* ((shared-store
              (mapcar #'(lambda (x)
                          (pop bind1) ;; throw away excess binder
                          (hou~generate-nullvariable x)) bind2)) 
             (xtra-binder (mapcar #'hou~generate-nullvariable bind1)))
        ;; extra parameters of term1
        (if (and xtra-binder shared-store)
            (list (term~appl-create term1 (append shared-store xtra-binder))
                  (term~appl-create term2 (append shared-store xtra-binder)))
          ;; else
          (if xtra-binder
              (list (term~appl-create term1 xtra-binder)
                    (term~appl-create term2 xtra-binder))
            ;; else
            (list (term~appl-create term1 shared-store)
                  (term~appl-create term2 shared-store))))))))

(defmethod hou~trivial-p ((term1 term+term) (term2 term+term) (uni uni+hou))
  (declare (edited  "16.4.96")
           (authors kk)
           (input   "term1 term2 and a unification structure.")
           (effect  "Returns T if both terms are low-level equal.")
           (value "truth value"))
  (and (eq (keim~name term1) (keim~name term2))
       (data~equal (term~type term1) (term~type term2))))
  
  ;; this is necessary as long as the post~read-object function in keim-2 generates
  ;; objects of class term+constant and the log~-functions create objects of class
  ;; log+connective
;;  (data~eq term1 term2))

   

;;(declare (inline hou~trivial))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decomposition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod hou~rigid-head-p (head (uni uni+hou))
  (declare (edited  "16.4.96")
           (authors kk)
           (input   "a head")
           (effect  "Tests whether a head is rigid, constant or nullvariabe.
                     Need to think about this, because of variable bindings.")
           (value "truth value"))
  (data~constant-p head))

(defmethod hou~flex-p (term (uni uni+hou))
   (and (data~appl-p term)
	(data~variable-p (data~appl-function
			  term))
	(not (hou~nullvariable-p (data~appl-function
			     term)))))

(defmethod hou~decompose ((term1 term+appl)
                          (term2 term+appl)
                          (uni uni+hou))
  (declare (edited  "16.4.96")
           (authors "kk,chris")
           (input   "two terms and their unification structure")
           (effect   "Returns a decomposed termlist
                      or nil in cas of a clash or :flex when flexible heads.")
           (value "a termlist or nil or :flex"))
  (let* ((head1 (data~appl-function term1))
         (head2 (data~appl-function term2))
         (flex1 (not (hou~rigid-head-p (data~appl-function term1) uni)))
         (flex2 (not (hou~rigid-head-p (data~appl-function term2) uni))))
    (if (or flex1 flex2) :flex
      (let* ((args1 (data~appl-arguments term1))
             (args2 (data~appl-arguments term2))
             (termlist nil))
        ;; building the termlist
	(when (= (length args1) (length args2))
	  (dolist (i args1 (cons (list head1 head2)
				 (nreverse termlist)))
	    (push (list i (pop args2)) termlist)))))))


(defmethod hou~simplify-next (unsolved flex-stack (uni uni+hou))
  (if unsolved
      (let* ((preterm1 (first (first unsolved)))
             (preterm2 (second (first unsolved)))
             ;; we don't normalize unless necessary!
             (term1 (if (data~appl-p preterm1)
                        (beta~head-normalize preterm1)
                      preterm1))
             (term2 (if (data~appl-p preterm2)
                        (beta~head-normalize preterm2)
                      preterm2)))
        ;; (format T "~% ~S ~S" term1 term2)
        (hou~simplify-aux term1 term2 (rest unsolved) flex-stack uni))
    ;; else
    (if flex-stack
        (multiple-value-bind
            (bound flex-rigid flex-flex)
            (hou~split flex-stack uni)
          (if bound
              (hou~simplify-next bound (append flex-rigid flex-flex) uni)
            (values flex-rigid flex-flex uni)))
      ;; else
      (values nil nil uni))))

;;
;; main function
;;

(defmethod hou~simplify! ((uni uni+hou))
  (declare (edited  "18-NOV-1996")
	   (authors Konrad)
	   (input  "A unification structure.")
	   (effect )
	   (value  "a rest problem or nil for failure."))
  (multiple-value-bind (flex-rigid flex-flex new-uni)
      (hou~simplify-next (uni~terms uni)
                         (append (uni~flex-rigid uni)
                                 (uni~flex-flex uni))
                         uni)
    (when new-uni ;; unification structure found
      (setf (uni~flex-rigid new-uni) flex-rigid)
      (setf (uni~flex-flex new-uni) flex-flex)
      (setf (uni~terms new-uni) nil) ;; there are no standard terms any more.
      new-uni)))
    
(defun hou~split (flex-stack uni)
  (let ((bound nil)
        (flex-flex nil)
	(flex-rigid nil))
    (dolist (pair flex-stack (values bound flex-rigid flex-flex))
      (let* ((term1 (first pair))
             (term2 (second pair))
             (head1 (or (and (data~appl-p term1)
                             (data~appl-function term1))
                        term1))
             (head2 (or (and (data~appl-p term2)
                             (data~appl-function term2))
                        term2)))
        (cond
         ((and (data~variable-p term1)
               (data~variable-p term2)
               (or (bind~binding term1)
                   (bind~binding term2)))
          (push pair bound))                         ;; bound var+term
         ((and
           (not (data~abstr-p head1))
           (not (data~abstr-p head2))
           (not (hou~rigid-head-p head1 uni))
           (not (and (data~variable-p head1) (bind~binding head1)))
           (not (hou~rigid-head-p head2 uni))    
           (not (and (data~variable-p head2) (bind~binding head2))))
          ;; flex-flex
          (push pair flex-flex))
         ((and (or (and (data~variable-p head1)
                        (bind~binding head1))
                   (hou~rigid-head-p head1 uni)
                   (data~abstr-p head1)
                   (not (data~variable-p head1)))
               (or (and (data~variable-p head2)
                        (bind~binding head2))
                   (hou~rigid-head-p head2 uni)
                   (data~abstr-p head2)
                   (not (data~variable-p head2)))) ;; rigid-rigid
          (push pair bound))
         (T
          (push pair flex-rigid)))))))

(defgeneric hou~simplify-aux (term1 term2 unsolved flex-stack uni)
  (declare (edited  "12-Dec-1996")
	   (authors KK Chris)
	   (input   "two terms a list of unsolved termpairs and a list"
		    "of flex-stack-pairs")
	   (effect  "may bind some variables in the actual binding"
		    "environment")
	   (value   "a list of flex-stack-pairs or :fail"))
  (:method ((term1 term+constant) (term2 term+constant)
	    unsolved flex-stack (uni uni+hou))
	   (when (hou~trivial-p term1 term2 uni)
             (hou~simplify-next unsolved flex-stack uni)))
  (:method ((term1 hou+nullvariable)(term2 hou+nullvariable)
            unsolved flex-stack (uni uni+hou))
           (when (hou~trivial-p term1 term2 uni)
             (hou~simplify-next unsolved flex-stack uni)))
  (:method ((term1 hou+nullvariable)(term2 T)
            unsolved flex-stack (uni uni+hou))
           (when (hou~flex-p term2 uni)
             (hou~simplify-next unsolved (cons (list term1 term2) flex-stack)
                                uni)))
  (:method ((term1 T)(term2 hou+nullvariable)
           unsolved flex-stack (uni uni+hou))
           (hou~simplify-aux term2 term1 unsolved flex-stack uni))
  (:method ((term1 term+variable) (term2 term+variable)
	    unsolved flex-stack (uni uni+hou))
           (if (hou~trivial-p term1 term2 uni)
	       (hou~simplify-next unsolved flex-stack uni)
             ;; else
             (if (bind~binding term1)
                 (hou~simplify-aux term2 (bind~binding term1)
                                   unsolved flex-stack uni)
               ;; else
               (if (bind~binding term2)
                   (hou~simplify-aux term1 (bind~binding term2)
                                     unsolved flex-stack uni)
                 ;; else
                 (hou~simplify-next unsolved (cons (list term1 term2)
                                                   flex-stack) uni)))))
  (:method ((term1 term+variable) (term2 term+constant)
	    unsolved flex-stack (uni uni+hou))
           (if (bind~binding term1)
               (hou~simplify-aux (bind~binding term1) term2
                                 unsolved flex-stack uni)
             ;; else
             (let ((bound (uni~bind term1 term2 uni)))
               (when bound
                 (hou~simplify-next unsolved flex-stack uni)))))
  (:method ((term1 term+constant) (term2 term+variable) 
	    unsolved flex-stack (uni uni+hou))
           (hou~simplify-aux term2 term1 unsolved flex-stack uni))
  (:method ((term1 term+variable)(term2 term+abstr)
	    unsolved flex-stack (uni uni+hou))
           (if (bind~binding term1)
               (hou~simplify-aux (bind~binding term1) term2
                                 unsolved flex-stack uni)
             ;; else
             (let ((bound (uni~bind term1 term2 uni)))
               (if bound
		   (hou~simplify-next unsolved flex-stack uni)
		 (let ((termpair (hou~alpha-eta term1 term2 uni)))
		   (hou~simplify-next (cons termpair unsolved)
				      flex-stack uni))))))
  (:method ((term1 term+abstr) (term2 term+variable)
	    unsolved flex-stack (uni uni+hou))
           (hou~simplify-aux term2 term1 unsolved flex-stack uni))
  (:method ((term1 term+variable) (term2 term+appl)
	    unsolved flex-stack uni)
            (if (bind~binding term1)
                (hou~simplify-aux (bind~binding term1) term2
                                  unsolved flex-stack uni)
              ;; else
              (if (hou~flex-p term2 uni) ;; flex-flex
                  (hou~simplify-next unsolved
                                     (cons (list term1 term2) flex-stack)
                                     uni)
                ;; else
                (let ((bound (uni~bind term1 term2 uni))) ;; chris fragen!
                  (if bound
		      (hou~simplify-next unsolved flex-stack uni)
		    (hou~simplify-next unsolved
                                     (cons (list term1 term2) flex-stack)
                                     uni))))))
  (:method ((term1 term+appl) (term2 term+variable)
	    unsolved flex-stack (uni uni+hou))
	   (hou~simplify-aux term2 term1 unsolved flex-stack uni))
  (:method ((term1 term+constant) (term2 term+appl)
	    unsolved flex-stack (uni uni+hou))
           (when (hou~flex-p term2 uni)
             (hou~simplify-next unsolved
                                (cons (list term2 term1)
                                      flex-stack) uni)))
  (:method ((term1 term+appl) (term2 term+constant)
	    unsolved flex-stack (uni uni+hou))
	   (hou~simplify-aux term2 term1 unsolved flex-stack uni))
  (:method ((term1 term+appl) (term2 term+appl)
            unsolved flex-stack (uni uni+hou))
           (let ((decompose (hou~decompose term1 term2 uni)))
             (if decompose
		 (if (equal decompose :flex)
		     ;; (flex-something)
		     (hou~simplify-next unsolved
					(cons (list term1 term2) flex-stack) uni)
		   (hou~simplify-next (append decompose unsolved)
				      flex-stack uni))
               :fail)))
  (:method (term1 term2 unsolved flex-stack (uni uni+hou))
           (let ((termpair (hou~alpha-eta term1 term2 uni)))
               (hou~simplify-next (cons termpair unsolved)
                                  flex-stack uni))))



