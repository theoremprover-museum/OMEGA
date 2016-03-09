;;; -*- Syntax: Common-Lisp; Package: KEIM; Base: 10; Mode: LISP -*-
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(IN-PACKAGE "KEIM")

(mod~defmod lab :uses (keim mod)
	    :documentation "A labelling for clauses, assumptions and conclusions."
	    :exports (
		      lab+mark
                      lab~mark
		      lab~get-node-from-label
		      lab~new-label
		      lab~compute-mark
                      lab~print-formulas
		      )
            )

#{\section{Labels}
\label {mod:lab}

This module provides the basic functions for handling marks of assumptions, conclusions and clauses. The class {\vb lab+mark} is a superclass for {\vb assum+assumption}, {\vb conc+conclusion} and {\vb cl+clause} with the slot mark. The module has the functions for computing the DNF of the marks and a print-function. The formulas are stored in the property-list of the marks, which are symbols.#}


(defvar lab*label-hash-table (make-hash-table)) ;; a hashtable for labels and initial nodes

(defun lab~new-label (node)
  (declare (edited  "10-DEC-1997")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "A new label."))
  (let ((label (gensym (format nil "Node-"))))
    (setf (gethash label lab*label-hash-table)
	  node)
    (list (list label))))

(defun lab~get-node-from-label (label)
  (declare (edited  "10-DEC-1997")
	   (authors Ameier)
	   (input   "A initial label.")
	   (effect  "None.")
	   (value   "corresponding node."))
  (gethash label lab*label-hash-table))

(eval-when (load compile eval)
(defclass lab+mark ()
  ((mark :initarg :mark :accessor lab~mark))
  (:documentation "An abstract class for marks in DNF of clauses, assumptions and conclusions.")))


(defun lab~compute-mark (node-list)
  (declare (edited  "10-DEC-1997")
	   (authors Ameier)
	   (input   "A list of nodes (parents of a given node).")
	   (effect  "None.")
	   (value   "The label of the child-node (dnf of the conjunction of"
		    "the labels of the input nodes)."))
  (lab=compute-dnf-and node-list))

(defun lab=compute-dnf-and (clause-list)
  (declare (edited "5-Mar-97")
	   (authors naumann)
	   (input  "A list of clauses with marks in DNF")
	   (value  "Computes the DNF from the conjunction of the marks from the clause-list-elements.")
           (effect "None."))
  (cond ((null clause-list) nil)
	((equal (length clause-list) 1) (lab~mark (first clause-list)))
	((equal (length clause-list) 2) (lab=compute-dnf-two-and (lab~mark (first clause-list)) (lab~mark (second clause-list))))
	(t (lab=compute-dnf-two-and (lab~mark (first clause-list)) (lab=compute-dnf-and (cdr clause-list))))))

(defun lab=compute-dnf-two-and (mark1 mark2 &optional (solution nil))
  (declare (edited "25-Feb-97")
	   (authors naumann)
	   (input  "Two marks in DNF")
	   (value  "Computes the DNF of 'mark1 and mark2'.")
           (effect "None."))
  (cond ((null mark2) (lab=subsume-dnf (lab=dnf-sort solution)))
	(t (remove-duplicates (lab=compute-dnf-two-and mark1 (cdr mark2) (append solution (lab=combine-lists mark1 (car mark2)))) :test #'equal))))

(defun lab=combine-lists (mark1 mark2 &optional (solution nil))
  (declare (edited "25-Feb-97")
	   (authors naumann)
	   (input  "Two marks.")
	   (value  "Combines mark1 and mark 2.")
           (effect "None."))
  (cond ((null mark1) solution)
	(t (lab=combine-lists (cdr mark1) mark2 (cons (remove-duplicates (append (car mark1) mark2) :test #'equal) solution)))))

(defun lab=compute-dnf-or (clause-list)
   (declare (edited "5-Mar-97")
	   (authors naumann)
	   (input  "A list of clauses with marks in DNF")
	   (value  "Computes the DNF from the disjunction of the marks from the clause-list-elements.")
           (effect "None."))
  (cond ((null clause-list) nil)
	((equal (length clause-list) 1) (lab~mark (first clause-list)))
	((equal (length clause-list) 2) (lab=compute-dnf-two-or (lab~mark (first clause-list)) (lab~mark (second clause-list))))
	(t (lab=compute-dnf-two-or (lab~mark (first clause-list)) (lab=compute-dnf-or (cdr clause-list))))))


(defun lab=compute-dnf-two-or (mark1 mark2)
  (declare (edited "25-Feb-97")
	   (authors naumann)
	   (input  "Two marks in DNF")
	   (value  "Computes the DNF of 'mark1 or mark2'.")
           (effect "None."))
  (lab=subsume-dnf (remove-duplicates (append mark1 mark2) :test 'equal)))
  
(defun lab=dnf-sort (mark &optional (solution nil))
  (declare (edited "27-Feb-97")
	   (authors naumann)
	   (input  "A marks in DNF")
	   (value  "The lexicographic sortet mark in DNF.")
           (effect "None."))
  (cond ((null mark) solution)
	(t (lab=dnf-sort (cdr mark) (cons (sort (car mark) #'string<) solution)))))
	 
(defun lab=subsume-dnf (dnf &optional (solution dnf) (current-list dnf))
  (declare (edited "10-Mar-97")
	   (authors naumann)
	   (input "A sorted mark in DNF")
	   (value "The minimal DNF using subsumtion.")
	   (effect "None."))
  (cond ((null current-list) solution)
	(t (if (lab=dnf-subsume-p (car current-list) dnf)
	       (lab=subsume-dnf dnf (remove (car current-list) solution :test 'equal) (cdr current-list))
	       (lab=subsume-dnf dnf solution (cdr current-list))))))

(defun lab=dnf-subsume-p (element dnf-list)
  (declare (edited "10-Mar-97")
	   (authors naumann)
	   (input "An element and an dnf-list")
	   (value "t if element really subsumes one element of the dnf-list, else nil.")
	   (effect "None."))
  (cond ((null dnf-list) nil)
	((and (> (length element)  (length (car dnf-list))) (lab=subsume-two-elements-p element (car dnf-list))) t)
        (t (lab=dnf-subsume-p element (cdr dnf-list)))))

(defun lab=subsume-two-elements-p (element1 element2)
  (declare (edited "10-Mar-97")
	   (authors naumann)
	   (input "Two sortet list")
	   (value "t if element1 subsumes element2, else nil.")
	   (effect "None."))
  (cond ((null element2) t)
        ((null element1) nil)
	((equal (car element1) (car element2)) (lab=subsume-two-elements-p (cdr element1) (cdr element2)))
	(t (lab=subsume-two-elements-p (cdr element1) element2))))

(defun lab~print-formulas (formula stream)
  (declare (edited "10-Mar-97")
	   (authors naumann)
	   (input "An formula, i.e. an clause, an assumption or an conclusion and a stream.")
	   (value "Prints the marks of the formula.")
	   (effect "None."))
 (format stream "{")
  (do ((mark-list (lab~mark formula) (cdr mark-list)))
      ((null mark-list) (format stream "}"))
      (let ((mark-conjunction-list (lab=list-marks (car mark-list))))
        (format stream "{")
	 (do ((element-list mark-conjunction-list (cdr element-list)))
	     ((null element-list) (format stream "}"))
	     (format stream "~A " (car element-list))))))

(defun lab=list-marks (mark-conjunction)
   (declare (edited "10-Mar-97")
	   (authors naumann)
	   (input "A conjuction of a mark")
	   (value "The formulas behind the mark.")
	   (effect "None."))
   (do ((mark mark-conjunction (cdr mark))
        (solution nil (append solution (list (get (car mark) 'formula)))))
       ((null mark) solution)
       ()))
