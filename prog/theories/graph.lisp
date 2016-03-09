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

; needs permutation.lisp

(defconstant graph*graph-theory 'graph)

(defconstant graph*gap-file (concatenate 'string  (sys~getenv 'home) "/omega/omega-3/prog/sapper/graphs.g"))
;  "/home/staff/vxs/science/eindhoven/arjeh/graphgps2.g")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translating graphutations to GAP syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric graph~graph2gap (v e)
  (declare (edited  "7-JUL-2004")
	   (authors pollet)
	   (input   "A terms representing a graph.")
	   (effect  "None.")
	   (value   "A string containing the GAP represenation of the graph"))
  (:method ((v term+set)(e term+set))
	   (if (graph=graph-type-p v e)
	       (graph=graph2gap v e)
	     (omega~error "GRAPH~~GRAPH2GAP: Formulas ~A|~A is not of graph type." v e)))
  (:method (v e)
	   (omega~error "GRAPH~~GRAPH2GAP: ~A|~A is an illegal graph object." v e)))

(defun graph=graph2gap (vert edge)
  (declare (edited  "7-JUL-2004")
	   (authors Pollet)
	   (input   "Two lists representing a graph.")
	   (effect  "None.")
	   (value   "Two values: strings containing a GAP representation of the graph."))
  (let* ((v (term~normalform vert))
	 (e (mapcar #'term~normalform (term~normalform edge)))
	 (vnumbers (reverse (maplist #'(lambda (x) (length x)) v)))
	 (enumbers (mapcar #'(lambda (tupel)
			       (mapcar #'(lambda (tu)
					   (1+ (position tu v :test #'data~equal)))
					   tupel))
			       e))
	 (estrings (mapcar #'(lambda (tupel) (format nil "[~A,~A]" (car tupel)(cadr tupel))) enumbers)))
	(values (format nil "[~A~{,~A~}]" (car vnumbers)(rest vnumbers))
		(format nil "[~A~{,~A~}]" (car estrings)(rest estrings)))))


(defun graph=pointwise2term (termtupel)
  (let ((var (term~variable-create 'x (term~type (caar termtupel)))))
    (term~abstr-create
	  (list var)
	  (graph=points2term termtupel var))))
    
(defun graph=points2term (termtupel var)
 (if (rest termtupel)
     (term~appl-create (graph=get-term 'ifthen)
		       (list (term~appl-create (graph=get-term '=)
					       (list var (caar termtupel)))
			     (cadar termtupel)
			     (graph=points2term (rest termtupel) var)))
   (cadar termtupel)))
   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Stuff to show that p is in G

(defun graph~gap-isomorphism (vert1 edge1 vert2 edge2)
  (declare (edited  "7-JUL-2004")
	   (authors pollet)
	   (input   "A graph and a set of generators as POST terms.")
	   (effect  "None.")
	   (value   "Returns a pointwise function for the isomorphism or NIL."))
  (when (and (perm~load-gap-file graph*gap-file)
	     (graph=graph-type-p vert1 edge1)
	     (graph=graph-type-p vert2 edge2))
    (multiple-value-bind (v1 e1)
	(graph~graph2gap vert1 edge1)
    (multiple-value-bind (v2 e2)
	(graph~graph2gap vert2 edge2)
      (let ((result (and v1 e1 v2 e2 (rcl~call-gap (format nil "PIsomorphism(~A,~A,~A,~A)"
							   v1 e1 v2 e2)))))
      (when (and result (not (string-equal result "fail")))
	(let* ((pointwisenumbers (read-from-string  (read-from-string result)))
	       (pointwiseterms	 (mapcar #'(lambda (tupel)
					     (list (nth (1- (first tupel)) (term~normalform vert1))
						   (nth (1- (second tupel)) (term~normalform vert2))))
					 pointwisenumbers))
	       (pointwisefct (graph=pointwise2term pointwiseterms)))
	  pointwisefct)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun graph=graph-type-p (vert edge)
  (declare (edited  "22-JUL-2004")
	   (authors Sorge)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if terms are of graph type, o/w NIL."))
  (let ((e (term~type edge))
	(v (term~type vert)))
    (and
     (type~func-p e)(type~func-p v)
     (data~equal (type~o) (data~abstr-range e))
     (data~equal (type~o) (data~abstr-range v))
     (data~equal v
		 (car (data~abstr-domain e))))))



(defun graph=get-term (symbol)
  (env~lookup-object symbol (th~env graph*graph-theory)))

#|
Tests
(setf vert (post~read-object '(set zero (s zero) (s (s zero))) (th~env graph*graph-theory) :existing-term))

(setf edge (post~read-object '(set (set zero (s zero))(set zero (s (s zero)))) (th~env graph*graph-theory) :existing-term))
      
(graph~gap-isomorphism
(post~read-object '(set  1 2 3) (th~env graph*graph-theory) :existing-term)
(post~read-object '(set (set  2 3)(set 2 1)) (th~env graph*graph-theory) :existing-term)
(post~read-object '(set 3 4 5) (th~env graph*graph-theory) :existing-term)
(post~read-object '(set (set  4 3)(set 4 5)) (th~env graph*graph-theory) :existing-term))


|#
