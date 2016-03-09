;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;
;; Bug, n.:
;;         An aspect of a computer program which exists because the
;; programmer was thinking about Jumbo Jacks or stock options when s/he
;; wrote the program.
;;
;; Fortunately, the second-to-last bug has just been fixed.
;;                -- Ray Simard
;;

(in-package "KEIM")

(mod~defmod uni-main :uses (uni-data)
	    :documentation "Main functions for generic unification."
	    :exports  (uni~unify
		       uni~unify-problems
		       uni~unify-terms
                       uni~simplify-terms
                       uni~bind
		       uni~create
                       uni~bill-increase!
		       uni~check
                       uni~bill-pay!
                       uni~bill-empty-p
		       uni~reduce!
                       hs*cost-limit
                       hs*cost-function-hook
                       hs*heuristic-function-hook
		       uni~unify-two-terms
		       )
	    )


(defvar uni*global-resource nil
  "The global resource uni*global-resource may contain a structure that is
   uses as a blackboard for available resources. E.g., the global resource
   may store the number of currently available problem structures (= memory
   restriction). The cost function used for hou-astar may rule the cost of
   a node x to be higher than the current cost limit if a node will exceed
   the available global resources.")


(defvar hs*heuristic-function-hook #'(lambda (problem path-cost)
                                        (declare (ignore problem))
                                        (+ path-cost 1))
  "The variable hs*cost-function-hook is a hook for the heuristic cost
   function. It takes a node structure and the current path
   costs and returns a number which estimates the cost for the next goal.
   By using a (let ...) form, it can be reassigned any cost function, e.g.
   for breadth first search, it must return the current search depth+1.")

(defvar hs*cost-function-hook #'(lambda (problem) (declare (ignore problem)) 0)
  "The variable hs*cost-function-hook is a hook for the actual cost
   function. It takes a node structure and returns the resources
   already spend for it, e.g. the recursion depth.")

(defvar hs*cost-limit :infinite
  "The variable hs*cost-limit defines the maximum cost of a node according to
   a cost function. When a node is cheaper than this limit, it will be
   extracted from the node heap by heap~extract. If the cost is too high,
   then the node will stay in the heap and heap~node-accessible-p will return T.
   A cost limit of :infinite will cause no cost-limit.")



(defgeneric uni~unify-problem (object &key destructive solutions))

;; (defmethod uni~unify-problem ((object uni+data) &key destructive solutions))

;;
;; the usable high-level functions
;;

;;
;; Creating unification problems
;;

(defgeneric uni~create-instance (ref
			&key class terms))


(defmethod uni~create-instance ((ref T)
		       &key (class nil)(terms nil))
  (if class
      (make-instance class :terms terms)
    ;; else
    (if ref
        (make-instance (class-of ref) :terms terms)
      ;; else
      (make-instance 'uni+hou :terms terms))))
 
(defmethod uni~create ((ref T) &key
                       (class nil)
                       (terms nil)
                       (bill 0)
                       (match-only nil)
                       (constraints nil))
  (let* ((uniprob (uni~create-instance ref
                                       :class class
                                       :terms terms)))
    (when (slot-exists-p uniprob 'bill)
      (setf (uni~bill uniprob) bill))
    (when (slot-exists-p uniprob 'match-only)
      (let ((table (uni~match-only-table uniprob)))
        (dolist (i match-only)
          (match~hash-match-only i table))
        (setf (uni~match-only uniprob) match-only)))
                                        ;(when (slot-exists-p uniprob 'constraints)
                                        ;  (setf (uni~constraints uniprob) constraints))
    uniprob))

(defmethod uni~unify ((terms list) &key
                      (class 'uni+hou)
                      (destructive T)
                      (solutions 'all)
                      (cost-limit :infinite)
                      (global-resource uni*global-resource)
                      (bill 0)
                      (match-only nil)
                      (constraints nil)
                      ;(heuristic-fn hs*heuristic-function-hook)
                      (cost-fn
                       #'(lambda (uni)
                           (uni~bill uni)))
                      ;(sort-fn ds*sort-function-hook)
                      )
  (declare (edited  "10-JAN-1997")
	   (authors Konrad)
	   (input   "a list of terms and additional parameters for uni~unify")
	   (effect  "unifies terms via class.")
	   (value   "unification problems and possibly a rest heap."))
  (let* ((uniprob (uni~create nil
                              :class class
                              :terms terms
                              :bill bill
                              :match-only match-only
                              :constraints constraints)))
    (let (;(hs*heuristic-function-hook heuristic-fn)
          ;(hs*cost-function-hook cost-fn)
          (hs*cost-limit cost-limit)
          ;(ds*sort-function-hook sort-fn)
          (uni*global-resource global-resource))
      (uni~unify-problem uniprob
                 :destructive destructive
                 :solutions solutions))))

(defmethod uni~unify ((problem uni+terms) &key
                      (destructive T)
                      (solutions 'all)
                      (reset T)
                      (global-resource uni*global-resource)
                      (cost-limit :infinite)
                      (bill 0)
                      (match-only nil)
                      (constraints nil)
                      ;(heuristic-fn hs*heuristic-function-hook)
                      (cost-fn #'(lambda (uni)
                                   (uni~bill uni)))
                      ;(sort-fn ds*sort-function-hook)
                      )
  (declare (edited  "06-FEB-1997")
	   (authors Konrad)
	   (input   "a unification problem and some"
                    "parameters that control unification.")
 	   (effect  "unifies the problems.")                    
	   (value   "a list of solutions.")
           (ignore reset match-only constraints bill))
  (let (;(hs*heuristic-function-hook heuristic-fn)
        ;(hs*cost-function-hook cost-fn) ;; hs-fn reduntant here (?)
        (hs*cost-limit cost-limit)
        ;(ds*sort-function-hook sort-fn)
        (uni*global-resource global-resource))
    (uni~unify-problem problem
                       :destructive destructive
                       :solutions solutions)))

;;
;; Bind variables
;;
  
(defgeneric uni~bind (var term uni)
  (declare (edited  "13-JAN-1997")
	   (authors Konrad)
	   (input   "variable, term and unification structure.")
	   (effect  "binds variable and has other possible side effects.")
	   (value   "term if success, nil otherwise.")))



;;
;; cost manipulation in uni~bill
;;


(defun heap=leq (key1 key2) ;; this comes from the old heap-search
  (declare (edited  "06-FEB-1997")
	   (authors Konrad)
	   (input   "two keys, numerical or :inifinite.")
	   (effect  "none.")
	   (value   "T iff key1<=key2, with x<=:infinite for all numbers x."))
  (or (eq key2 :infinite)
      (and (numberp key2) (numberp key1)  (<= key1 key2))))

(defmethod uni~bill-empty-p ((uni-prob uni+hou))
  (declare (edited  "27-DEC-1996")
	   (authors Chris KK)
	   (input   "a hou unification problem")
	   (effect  "none")
	   (value   "t if there are no resources left for UNI-PROB"
		    "Note: the hou bills are integers for the current"
		    "search-depth"
		    "e.g.: 10"))
  (heap=leq hs*cost-limit (uni~bill uni-prob)))


(defmethod uni~bill-pay! ((uni-prob uni+hou))
  (declare (edited  "27-DEC-1996")
	   (authors Chris)
	   (input   "a hou unification problem")
	   (effect  "destructively changes the UNI-PROB")
	   (value   "the changed UNI-PROB (the bill"
		    "information is reduced"))
  (setf (uni~bill uni-prob) (1+ (uni~bill uni-prob))))


(defmethod uni~bill-increase! ((uni-prob uni+hou) n)
  (declare (edited  "27-DEC-1996")
	   (authors Chris KK)
	   (input   "a hou unification problem")
	   (effect  "destructively changes the UNI-PROB")
	   (value   "the changed UNI-PROB (the bill"
		    "information is inreased by n"))
  (setf (uni~bill uni-prob) (+ n (uni~bill uni-prob))))


;;
;; Beautifying solutions
;;

(defun data~collect-free-variables (problem) 
  (reduce #'append
          (mapcar #'(lambda (x)
                      (append (data~free-variables (first x))
                              (data~free-variables (second x))))
                  problem)))


;;(defun uni~reduce-subst (subst)
;;  (let ((idem (subst~idem subst)))
;;    (mapcan  #'(lambda (x) (beta~normalize (subst~apply subst x)))
;;            (subst~codomain idem))))

(defun uni~reduce! (problem solutions &key (subst-only T))
  (let ((vars (data~collect-free-variables problem)))
    (dolist (i solutions solutions)
      (setf (uni~substitution i)
            (first (subst~reduce (list (uni~substitution i))
                                 :restrict-vars vars))))
    (if subst-only
      (mapcar #'uni~substitution solutions)
      solutions)))

;;
;; checking solutions
;;


(defun uni~check (solutions problem)
  (when solutions
    (format t "~%Uni-struct:~% ~s~%" (first solutions))
    (uni~check-unifier (first solutions) problem)
    (uni~check (cdr solutions) problem)))

(defun uni~check-unifier (subst constraints)
  (when constraints
    (format t "~%~a = ~b~%"
	    (data~beta-normalize
	     (subst~apply subst (first (first constraints))))
	    (data~beta-normalize
	     (subst~apply subst (second (first constraints)))))
    (uni~check-unifier subst (cdr constraints))))



(defun uni~unify-two-terms (term1 term2 &key subst bindables)
  (declare (edited  "22-APR-2000")
	   (authors Pollet)
	   (input   "Two terms, SUBST is a pre-executed substituion,"
		    "BINDABLES is a list of variables that may be instantiated,"
		    "by default all variables are bindable.")
	   (effect  "None.")
	   (value   "A subst if the terms are matchable, else nil"))
  (let ((subst
	 (bind~with-bindings
	  ((when (subst~p subst) (subst~bind-substitution subst))
	   (let ((uni (car (uni~unify (list (list term1 term2)) :match-only bindables))))
	     (when uni (uni~substitution uni)))))))
	(when subst
	  (subst~reduce subst :restrict-vars bindables))))


