;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     LEO Project                                                          ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: leo@cs.uni-sb.de                                      ;;
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


(in-package "KEIM")

(mod~defmod GB :uses ()
	    :documentation ""
	    :exports (gb~construct
		      gb~helpvariable-create
		      gb~helpvariable-p
		      gb~imitation
		      gb~projections)

)

;;
;; Helpvariables
;;

(eval-when (load compile eval) 
  (defclass gb+helpvariable (term+variable)
    ()
    (:documentation
     "Helpvariables are generated variables for unification. They may"
     "ask for a different behavior")))

(defmethod gb~helpvariable-create (ref annotation)
  (declare (edited  "1.4.96")
	   (authors kk)
	   (input   "a reference variable or class name  and an annotation.")
	   (value   "a new H-Variable.")
	   (effect "none."))
  (let ((helpvar (change-class
                  (term~variable-create  (gensym "?H") annotation)
                  'gb+helpvariable)))
    helpvar))

(defmethod gb~helpvariable-p ((var gb+helpvariable))
  (declare (edited  "1.12.96")
           (authors kk)
           (input   "a variable")
           (effect  "tests whether object is a nullvar or not.")
           (value   "truth value"))
  T)

(defmethod gb~helpvariable-p ((var term+term))
  (declare (edited  "1.12.96")
           (authors kk)
           (input   "a variable")
           (effect  "tests whether object is a nullvar or not.")
           (value   "truth value"))
  nil) 

;;

(defgeneric gb~construct (type const)
  (declare (edited  "12-FEB-1997")
	   (authors Chris)
	   (input   "a type and a (function-)constant")
	   (effect  "none")
	   (value   "the general bindings wrt. TYPE and CONTS"))
;  (:method ((type type+prim) const)
;	   (list const))
  (:method ((type type+type) const)
	   (append (gb~projections type const)
		   (unless (hou~nullvariable-p const)  ;; eingefuegt am 2.3.99
		     ;; zur Vermeidung des Absturz beim unifizieren in Churchnumerals
		     ;; von (* 1 X) = 1
		     (list (gb~imitation type const))))))



(defgeneric gb~imitation (type const)
  (declare (edited  "13-FEB-1997")
	   (authors Chris)
	   (input   "a type and a constant-symbol")
	   (effect  "none")
	   (value   "1. the imitation binding wrt. to the input arguments"
		    "2. the list of the new free variables"))
  (:method (type (const term+primitive))  ;;; Bsp. buf1 lief sonst in Fehler, hier stand
					  ;;; term+constant aber in keim-2 steht da
					  ;;; term+primitive , chris
	   (let* ((binder-vars-types (data~n-domain type))
		  (new-vars-goal-types (data~n-domain (term~type const)))
		  (new-vars-types
		   (gb=new-vars-types binder-vars-types new-vars-goal-types))
		  (binder-vars (gb=vars-create binder-vars-types)) 
		  (new-vars (gb=vars-create new-vars-types))
		  (arg-terms
		   (if binder-vars
		       (mapcar #'(lambda (nvar)
				   (term~appl-create nvar binder-vars))
			       new-vars)
		     new-vars)))
	     (values (gb=gb-create binder-vars const arg-terms)
		     new-vars)))
  (:method (type (const sksym+sk-constant))
	   (let* ((binder-vars-types (data~n-domain type))
		  (new-vars-goal-types (data~n-domain (term~type const)))
		  (new-vars-types
		   (gb=new-vars-types-skolem binder-vars-types
					     new-vars-goal-types
					     (sksym~arity const)))
		  (binder-vars (gb=vars-create binder-vars-types)) 
		  (new-vars (gb=vars-create new-vars-types))
		  (arg-terms
		   (append (subseq  new-vars 0 (sksym~arity const))
			   (if binder-vars
			       (mapcar #'(lambda (nvar)
					   (term~appl-create nvar binder-vars))
				       (subseq  new-vars (sksym~arity const)))
			     (subseq  new-vars (sksym~arity const))))))
	     (values 
	      (gb=gb-create binder-vars const arg-terms)
	      new-vars))))



(defun gb=gb-create (binder-vars head arg-terms)
  (declare (edited  "14-FEB-1997")
	   (authors Chris)
	   (input   "a list of variables for the binder, a head-symbol"
		    "and a list of argument-types")
	   (effect  "none")
	   (value   "the gb-term wrt. the given arguments"))
  (if binder-vars
      (term~abstr-create binder-vars
			 (if arg-terms
			     (term~appl-create head arg-terms)
			   head))
     (if arg-terms
	 (term~appl-create head arg-terms)
       head)))
	   

(defun gb=new-vars-types (argtypes goaltypes)
  (declare (edited  "12-FEB-1997")
	   (authors Chris)
	   (input   "the argumentypes and the goaltypes of the variables, to"
		    "be created")
	   (effect  "none")
	   (value   "a list of #goaltypes new variables. The nth new variable"
		    "has as argumenttypes ARGTYPES and the nth type of"
		    "GOALTYPES as goaltype"))
  (if argtypes
      (mapcar #'(lambda (goaltype) (type~func-create argtypes goaltype))
	      goaltypes)
    goaltypes))


(defun gb=new-vars-types-skolem (argtypes goaltypes int)
  (declare (edited  "12-FEB-1997")
	   (authors Chris)
	   (input   "the argumentypes and the goaltypes of the variables, to"
		    "be created and an integer")
	   (effect  "none")
	   (value   "a list of #goaltypes new variables. The first INT"
		    "variables have the types of the first INT types in"
		    "GOALTYPES. For all other variables holds: The nth new"
		    "variable has as argumenttypes ARGTYPES and the nth type"
		    "of GOALTYPES as goaltype and the"))
  (append (subseq goaltypes 0 int)
	  (gb=new-vars-types argtypes (subseq goaltypes int))))



(defun gb=vars-create (typelist)
  (declare (edited  "13-FEB-1997")
	   (authors Chris)
	   (input   "a list of types")
	   (effect  "none")
	   (value   "a list of fresh variables of the specified types"))
  (mapcar #'(lambda (tp) (gb~helpvariable-create 'term+variable tp))
	  typelist))



(defgeneric gb~projections (type const)
  (declare (edited  "13-FEB-1997")
	   (authors Chris)
	   (input   "a type and a constant (the imithead)")
	   (effect  "none")
	   (value   "1. a list with all possible projection-bindings wrt. the "
		    "   given arguments"
		    "2. a list of list of the new free variables in the"
		    "   projection-bindings"))
  (:method (type const)
	   (let* ((binder-vars-types (data~n-domain type))
		  (pos-type-pairs
		   (gb=proj-pos-type-pairs const binder-vars-types))
		  (projectionslist nil)
		  (newvarslist nil))
	     (mapcar
	      #'(lambda (pos-type-pair)
		  (let* ((proj-pos (first pos-type-pair))
			 (proj-type (second pos-type-pair))
			 (new-vars-goal-types (data~n-domain proj-type))
			 (new-vars-types
			  (gb=new-vars-types binder-vars-types
					     new-vars-goal-types))
			 (binder-vars (gb=vars-create binder-vars-types)) 
			 (new-vars (gb=vars-create new-vars-types))
			 (proj-var (nth (1- proj-pos) binder-vars))
			 (arg-terms
			  (mapcar #'(lambda (nvar)
				      (term~appl-create nvar binder-vars))
				  new-vars)))
		    (push (gb=gb-create binder-vars proj-var arg-terms)
			  projectionslist)
		    (push new-vars newvarslist)))
	      pos-type-pairs)
	     (values projectionslist newvarslist))))
	     
		  
 
(defgeneric gb=proj-pos-type-pairs (const types)
  (declare (edited  "13-FEB-1997")
	   (authors Chris)
	   (input   "a symbol (constant or skolemconstant) and a list of"
		    "types (binder-vars-types)")
	   (effect  "none")
	   (value   "A list of pairs of positions (integers) and types."
		    "Each Pair specifies the position of a type in the"
		    "typelist and the type at this position"))
  (:method (const types)
	   (let ((count 0)
		 (type (term~type const)))
	     (mapcan #'(lambda (tp)
			 (progn (incf count)
				(when (data~equal (data~n-range type)
						  (data~n-range tp))
				  (list (list count tp)))))
		     types))))




;;;;;;;;;;;;;;;;;;;;;;;;  Memoizer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric gb=memo-construct (type var)
  (declare (edited  "17-FEB-1997")
	   (authors Chris)
	   (input   "a type and a constant-symbol")
	   (effect  "none")
	   (value   "This function is used in connection with the memoizer"
		    "for general binders. For memoization we don't compute"
		    "in imitation case a gb like"
		    "lam X1...Xn . CONST (H1 X1...Xn)...(Hk X1...Xn)"
		    "but lam Y . lam X1...Xn . Y (H1 X1...Xn)...(Hk X1...Xn)."
		    "This term can then be put into the memoizer and has to"
		    "be applied on the given rigid-head of an actual problem"
		    "to obtain the wanted imitation binder."
		    "Note: Projections don't cause any problems"
		    "THEREFORE the RESULT of this function is:"
		    "a list of general-binders, whereas the first element in"
		    "this list is the special imitation-binding which has to"
		    "be applied on the actual rigid-head to obtain a correct"
		    "imitation binding"))
  (:method ((type type+type) (var data+variable))
	   (multiple-value-bind (special-imitation new-vars)
	       (gb~imitation type var)
	     (multiple-value-bind (projections new-vars-list)
		 (gb~projections type var)
	        
	   ;;; here we put the imitation-gb in front position;
    	   ;;; later - maybe it will be better to
	   ;;; put it at the end of the projections-list
	   ;;; (Example: (H (f a)) (f (H a)) would otherwise produce no
	   ;;; unifier in endless searchspace.
	   ;;; If you want to change this order, don't do this here but
	   ;;; in gb~memo-construct

	       (list  (cons (term~abstr-create (list var) special-imitation)
			   projections)
		      (cons new-vars new-vars-list))))))
	       
	       

(defun gb=memo-fresh-vars (listoflistofvars)
  (mapcar #'(lambda (varlist)
	      (when varlist
		(mapcar #'(lambda (var) (gb~helpvariable-create
					 'term+variable
					 (term~type var)))
			varlist)))
	  listoflistofvars))


(defun gb=memo-fresh-gbs (memo-gb &key not-rename)
  (let* ((oldvarslist (second memo-gb))
	 (newvarslist
	  (if not-rename oldvarslist
	    (gb=memo-fresh-vars oldvarslist))))
    (mapcar #'(lambda (gb oldvars newvars)
		(if oldvars (term~appl-create (term~abstr-create oldvars gb)
					      newvars)
		  gb))
	    (first memo-gb) oldvarslist newvarslist)))
			     
			     
  

(let ((memo (make-hash-table :test #'equal)))
  (defun gb~memo-construct1 (type-flex-head imithead)
    (declare (edited  "16-FEB-1997")
	     (authors Chris)
	     (input   "the type of the flexible head and the imithead")
	     (effect  "the general bindings of the given arguments may be"
		      "stored in the hashtable memo, so that they don't have"
		      "to be computed again")
	     (value   "the general bindings of TYPE-FLEX-HEAD and IMITHEAD"))
    (let* ((type-rigid-head (term~type imithead))
	   (gb (gethash (gb=key-transform type-flex-head type-rigid-head)
			memo)))
      (if gb (let* ((fresh-gbs (gb=memo-fresh-gbs gb))
		    (imit-special (first fresh-gbs))
		    (projections (cdr fresh-gbs))
		    (imitation
		     (term~appl-create imit-special (list imithead))))
	       ;;; imitation last ???
	       (append projections (list imitation)))
	(let* ((new-var (gb~helpvariable-create 'term+variable
                                           type-rigid-head))
	       (new-gb-memo (gb=memo-construct type-flex-head new-var))
	       (new-gb (gb=memo-fresh-gbs new-gb-memo :not-rename t))
	       (imit-special (first new-gb))
	       (projections  (cdr new-gb))
	       (imitation
		(term~appl-create imit-special (list imithead))))
	  (progn
	    (setf (gethash
		   (gb=key-transform type-flex-head type-rigid-head)
		   memo)
		  new-gb-memo)
	    ;;; imitation last ???
	    (append projections (list imitation))))))))


(let ((memo (make-hash-table :test #'equal)))
  (defun gb~memo-construct2 (type-flex-head imithead)
    (declare (edited  "16-FEB-1997")
	     (authors Chris)
	     (input   "the type of the flexible head and the imithead")
	     (effect  "the general bindings of the given arguments may be"
		      "stored in the hashtable memo, so that they don't have"
		      "to be computed again")
	     (value   "the general bindings of TYPE-FLEX-HEAD and IMITHEAD"))
    (let* ((type-rigid-head (term~type imithead))
	   (gb (gethash (gb=key-transform type-flex-head type-rigid-head)
			memo)))
      (if gb (let* ((fresh-gbs (mapcar #'data~rename gb))
		    (imit-special (first fresh-gbs))
		    (projections (cdr fresh-gbs))
		    (imitation
		     (term~appl-create imit-special (list imithead))))
	       ;;; imitation last ???
	       (append projections (list imitation)))
	(let* ((new-var (gb~helpvariable-create 'term+variable
                                             type-rigid-head))
	       (new-gb (first (gb=memo-construct type-flex-head new-var)))
	       (imit-special (first new-gb))
	       (projections  (cdr new-gb))
	       (imitation
		(term~appl-create imit-special (list imithead))))
	  (progn
	    (setf (gethash
		   (gb=key-transform type-flex-head type-rigid-head)
		   memo)
		  new-gb)
	    ;;; imitation last ???
	    (append projections (list imitation))))))))




(defun gb=key-transform (type1 type2)
  (declare (edited  "18-FEB-1997")
	   (authors Konrad)
	   (input   "two types.")
	   (effect  "none.")
	   (value   "returns a key for a equal-hashtable."))
  (cons (sxhash type1) (sxhash type2)))
  
  
;;;; Zum testen



(defun hou~gb-construct (flex-term term &key only-imitation short-flag)
 (gb~memo-construct1 (term~type (data~top flex-term)) (data~top term)))

;;(defun hou~gb-construct (flex-term term &key only-imitation short-flag)
;;  (let ((gb
;;	 (gb~memo-construct2 (term~type (data~top flex-term)) (data~top term))))
;   (mapcar #'(lambda (gb) (format t "~% ~A : ~A" gb (term~type gb))) gb)
;;    gb))
