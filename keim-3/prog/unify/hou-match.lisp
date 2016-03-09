;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-
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

;;
;; this file contains the modifications for higher order matching
;; 
;;

(in-package :keim)

;;
;; The matching I propose here works by restriction of bindings.
;; A variable becomes a matching variable by inluding it into the
;; match-only slot of the matching unification problem. Before
;; unification starts, the function match~create-uni creates a
;; hashtable for these variables; the test whether a variable
;; is restricted should only be O(1).
;;
;; The code below restricts the unification by preventing these
;; matching variables to get any binding. Instead, they are treated
;; similar to constants. (Alternative implementation:
;; replacing all variables on one side by constants.)
;;
;; Beware: To get a proper matching problem, you must ensure that
;; all variables on one side are restricted and that none of these
;; variables appear on the other side of the equations. Otherwise,
;; you get (interesting?) HO problems with a somewhat unclear
;; semantics.
;;
;;


(mod~defmod match :uses (uni-main) 
	    :documentation "Higher Order Matching Modifications, including
                            those for simplification."
	    :exports (
		      uni+match
		      
		      match~create-uni
                      match~match-only-p
		      match~hash-match-only
		      uni~match-only
		      uni~match-only-table
                      uni~syntactic-matcher
                      uni~syntactic-matchers
		      hou~simplify-aux
		      hou~simplify-next
                      
                      )
	    )

;;
;; Higher Order Matching
;;

(eval-when (load compile eval) 
  (defclass uni+match (uni+hou)
    ((match-only :accessor uni~match-only :initarg :match-only
                 :initform nil)
     (match-only-table :accessor uni~match-only-table
                       :initarg :match-only-table
                       :initform (make-hash-table :test #'eq)))
    (:documentation 
     "Uni+match are HOU problems with restricted variables that can't
      get bindings.")))

(defmethod print-object ((object uni+match) (stream stream))
  (format stream "~a" (list "MATCH"
                            "terms:" (uni~terms object)
                            "subst:" (uni~substitution object)
                            "flex-flex:" (uni~flex-flex object)
                            "flex-rigid:" (uni~flex-rigid object)
                            "match-only:" (uni~match-only object)
			    "bill:" (uni~bill object))))


(defmethod keim~copy ((object uni+match)
                      &key (explode :all-classes) (share :all-classes)
                      preserve downto)
  (let ((newobj (uni~create object
			 :terms (uni~terms object))))
    (setf (uni~flex-rigid newobj)  (uni~flex-rigid object))
    (setf (uni~flex-flex newobj)  (uni~flex-flex object))
    (setf (uni~substitution newobj) (uni~substitution object))
    (setf (uni~match-only newobj)  (uni~match-only object))
    (setf (uni~match-only-table newobj)  (uni~match-only-table object))
    ;; match-only information is shared
    (setf (uni~bill newobj) (uni~bill object))
    (setf (uni~link newobj) (uni~link object))
    newobj))

;;
;; creating match uni problems
;;

(defun match~hash-match-only (var table)
  (setf (gethash var table) T))

(defun match~match-only-p (var uni)
  (gethash var (uni~match-only-table uni)))

;;(defun match~create-uni (termlist &key
;;                                 (match-only nil))
;;  (let* ((uni (uni~create nil :class 'uni+match :terms termlist))
;;         (table (uni~match-only-table uni)))
;;    (dolist (i match-only)
;;      (match~hash-match-only i table))
;;    (setf (uni~match-only uni) match-only)
;;    uni))

;;
;; restrict variable bindings to non-match variables in simplify
;;

(defmethod hou~simplify-next (unsolved flex-stack (uni uni+match))
  (if unsolved
      (let* ((preterm1 (first (first unsolved)))
             (preterm2 (second (first unsolved)))
             ;; we don't normalize unless necessary!
             (term1 (if (and (data~appl-p preterm1)
                             (or (data~constant-p preterm2)
                                 (data~appl-p preterm2)
                                 (match~match-only-p preterm2 uni)))
                        (beta~head-normalize preterm1)
                      preterm1))
             (term2 (if (and (data~appl-p preterm2)
                             (or (data~constant-p term1)
                                 (data~appl-p term1)
                                 (match~match-only-p term1 uni)))
                        (beta~head-normalize preterm2)
                      preterm2)))
        ;; (format T "~% ~S = ~S" preterm1 preterm2)
        ;; (format T "~% ~S ~S" (type-of term1) (type-of term2))
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

(defmethod hou~simplify-aux ((term1 term+variable)(term2 term+variable)
                             unsolved flex-stack (uni uni+match))
  (declare (edited  "12-Dec-1996")
	   (authors KK)
	   (input   "two terms a list of unsolved termpairs and a list"
		    "of flex-stack-pairs")
	   (effect  "may bind some variables in the actual binding"
		    "environment")
	   (value   "a list of flex-stack-pairs or :fail"))
  (if (hou~trivial-p term1 term2 uni)
      (hou~simplify-next unsolved flex-stack uni)
    ;; else
    (if (match~match-only-p term1 uni)
        (unless (match~match-only-p term2 uni)
          (let ((bound (uni~bind term2 term1 uni)))
            (when bound
              (hou~simplify-next unsolved flex-stack uni))))
      ;; else
      (if (match~match-only-p term2 uni)
          (let ((bound (uni~bind term1 term2 uni)))
            (when bound
              (hou~simplify-next unsolved flex-stack uni)))
        ;; else (standard case)
        (hou~simplify-next unsolved (cons (list term1 term2)
                                          flex-stack) uni)))))


(defmethod hou~simplify-aux ((term1 term+variable) (term2 term+constant)
                             unsolved flex-stack (uni uni+match))
  (unless (match~match-only-p term1 uni)
    (if (bind~binding term1)
	(hou~simplify-aux (bind~binding term1) term2
			  unsolved flex-stack uni)
      ;; else
      (let ((bound (uni~bind term1 term2 uni)))
	(when bound
	  (hou~simplify-next unsolved flex-stack uni))))))


(defmethod hou~simplify-aux ((term1 term+constant) (term2 term+variable)
                             unsolved flex-stack (uni uni+match))
  (unless (match~match-only-p term2 uni)
    (if (bind~binding term2)
	(hou~simplify-aux term1 (bind~binding term2) 
			  unsolved flex-stack uni)
      ;; else
      (let ((bound (uni~bind term2 term1 uni)))
	(when bound
	  (hou~simplify-next unsolved flex-stack uni))))))


(defmethod hou~simplify-aux ((term1 term+variable) (term2 term+abstr)
                             unsolved flex-stack (uni uni+match))
  (unless (match~match-only-p term1 uni)
    (let ((bound (uni~bind term1 term2 uni)))
      (when bound
        (hou~simplify-next unsolved flex-stack uni)))))

(defmethod hou~simplify-aux ((term1 term+variable) (term2 term+appl)
                             unsolved flex-stack (uni uni+match))
  (if (match~match-only-p term1 uni)
      (when (hou~flex-p term2 uni)
        (hou~simplify-next unsolved
                           (cons (list term2 term1) flex-stack)
                           uni))
    ;; else
    (if (hou~flex-p term2 uni) ;; flex-flex
        (hou~simplify-next unsolved
                           (cons (list term1 term2) flex-stack)
                           uni)
      ;; else
      (let ((bound (uni~bind term1 term2 uni))) 
        (when bound
          (hou~simplify-next unsolved flex-stack uni))))))


;;
;; rigid/flex stuff
;;


(defmethod hou~rigid-head-p (head (uni uni+match))
  (declare (edited  "16.4.96")
           (authors kk)
           (input   "a head")
           (effect  "Tests whether a head is rigid.")
           (value "truth value"))
  (or (data~constant-p head) (hou~nullvariable-p head)
      (match~match-only-p head uni)))

(defmethod hou~flex-p (term (uni uni+match))
  (and (data~appl-p term)
       (data~variable-p (data~appl-function
                         term))
       (not (or (hou~nullvariable-p (data~appl-function
                                term))
                (match~match-only-p (data~appl-function
                                     term) uni)))))

;;
;; prevent general bindings for match-only variable
;; -- not necessary, but would be nice...
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic Matching (According to VS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uni~syntactic-matcher (var-term const-term)
  (declare (edited  "06-APR-1998 15:37")
	   (authors SORGE)
	   (input   "A term that is matched against CONST-TERM. Any free variables in"
		    "CONST-TERM are treated as constants.")
	   (effect  "None.")
	   (value   "A substitution corresponding to the first available"
		    "syntactic matcher, if there is any. O/w NIL."))
  (unless (or (and (term~appl-p var-term) (term~abstr-p const-term))      ;;;; some bug in unifier or deeper!!!
	      (and (term~abstr-p var-term) (term~appl-p const-term)))     ;;;; see polynomial diff for an example
    (multiple-value-bind (term dom codom)
	(data~replace-free-variables-and-rename var-term nil nil)
      (let ((matcher (car (uni~unify (list (list  term const-term))
				     :class 'uni+match
				     :match-only (term~free-variables const-term)))))
	(when matcher	  
	  (subst~idem (subst~disjoint-compose-substitution (uni~substitution matcher) (subst~create dom codom))))))))

(defun uni~syntactic-matchers (var-term const-term)
  (declare (edited  "06-APR-1998 15:37")
	   (authors SORGE)
	   (input   "A term that is matched against CONST-TERM. Any free variables in"
		    "CONST-TERM are treated as constants.")
	   (effect  "None.")
	   (value   "A list of substitutions corresponding to the"
		    "syntactic matchers, if there is any. O/w NIL."))
  (multiple-value-bind (term dom codom)
      (data~replace-free-variables-and-rename var-term nil nil)
    (let ((matchers (uni~unify (list (list  term const-term))
			       :class 'uni+match
			       :match-only (term~free-variables const-term))))
      (when matchers
	(mapcar #'(lambda (x)
		    (subst~disjoint-compose-substitution
		     (uni~substitution x)
		     (subst~create dom codom)))
		matchers)))))
