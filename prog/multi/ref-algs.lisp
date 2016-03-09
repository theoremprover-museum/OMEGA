;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(in-package :omega)


(mod~defmod REFALG 
            :uses (keim omega)
            :documentation "Stuff and data-structures for refinement algorithms"
            :exports (
                      refalg+refinement-algorithm
                      
                      refalg~create-new-refinement-algorithm
                      refalg~define-refinement-algorithm
                      refalg~find-refinement-algorithm-object
                      refalg~invokation-function
                      refalg~parameter-list
                      refalg~refinement-algorithm-p
                      refalg~reinvokation-function
                      
                      refalg*refalg-hash-table))





;; This module defines the refinement-algorithm objects
;; A refinement algorithm objects consists of the specification of a invokation function and the specifications of the
;; parameter objects. The invokation function can be used to invoke a strategy for this refinement algorithm.
;;
;; The arguments of the invokation function have to be:
;; the strategy itself 
;; and an rest argument to give also other additional arguments (for example for PPlanner task ...) not specified in the strategy itself

#| --------------------------------------------------------- global vars ------------------------------------------------------------ |#

(defvar refalg*refalg-hash-table (make-hash-table))
;; A hash table to store all refinement algorithms in

#| --------------------------------------------------- refalg+refinement-algorithm -------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass refalg+refinement-algorithm (keim+name keim+object)
    ((invokation-function :initform nil
			  :initarg :invokation-function
			  :accessor refalg~invokation-function)
     (reinvokation-function :initform nil
			    :initarg :reinvokation-function
			    :accessor refalg~reinvokation-function)
     (parameter-list :initform nil
		     :initarg :parameter-list
		     :accessor refalg~parameter-list))))

(defmethod print-object ((refalg refalg+refinement-algorithm) stream)
  (format stream "<REFALG: ~A with parameters: ~A"
	  (keim~name refalg)
	  (refalg~parameter-list refalg)))

(defun refalg~create-new-refinement-algorithm (name invokation-function reinvokation-function parameter-list)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A name, a invokation-function, a reinvokation-function and a parameter-list for"
		    "a refinement-algorithm.")
	   (effect  "A new refinement algorithm object is created and a new entry is made in"
		    "refalg*refalg-hash-table (if there already is an entry with this key a"
		    "the olds one is overwritten with the new one.")
	   (value   "The new refinement algorithm object."))
  (let* ((new-ref-alg-obj (make-instance 'refalg+refinement-algorithm
					 :name name
					 :invokation-function invokation-function
					 :reinvokation-function reinvokation-function
					 :parameter-list parameter-list))
	 (old-value (gethash name refalg*refalg-hash-table)))
    
    (when old-value
      (omega~message "The old value for ~A in refalg*refalg-hash-table is replaced by a new definition." name))
    
    (setf (gethash name refalg*refalg-hash-table) new-ref-alg-obj)
    
    new-ref-alg-obj))

(defmacro refalg~define-refinement-algorithm (&rest attribs)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "The written representation of a refinement algorithm.")
	   (effect  "Creates the new refinement algorithm and add it to the refalg*refalg-hash-table.")
	   (value   "The new refinement-algorithm."))
  
  (do* ((rest-attribs attribs (rest rest-attribs))
	(found-name nil)
	(found-parameter-list nil)
	(found-invokation-function nil)
	(found-reinvokation-function nil)
	)
      ((null rest-attribs)
       (if (and found-name
		;;found-parameter-list
		found-reinvokation-function
		found-invokation-function)
	   `(refalg~create-new-refinement-algorithm ',found-name
						    #',found-invokation-function
						    #',found-reinvokation-function
						    ',found-parameter-list)
	 (omega~error "Wrong specification of refinement algorithm. At least name, invokation function, and reinvokation function have to be specified.")))
    (let* ((head-attrib (first rest-attribs)))
      (if (null (listp head-attrib))
	  (omega~error "Each attribute of the refinement algorithm definition has to be a pair, ~A is unfortunately not.")

	(let* ((car-attrib (first head-attrib))
	       (second-attrib (second head-attrib)))
	  
	  (cond ((string-equal car-attrib 'name)
		 (setq found-name second-attrib))
		((string-equal car-attrib 'parameter-list)
		 (setq found-parameter-list second-attrib))
		((string-equal car-attrib 'invokation-function)
		 (setq found-invokation-function second-attrib))
		((string-equal car-attrib 'reinvokation-function)
		 (setq found-reinvokation-function second-attrib))
		(t
		 (omega~error "Don't know how to read attribute ~A in refinement algorithm definition." head-attrib))))))))

(defun refalg~find-refinement-algorithm-object (name)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A name.")
	   (effect  "None.")
	   (value   "Looks-up refalg*refalg-hash-table for a entry with name as key and returns it,"
		    "nil if no one exists."))
  (gethash name refalg*refalg-hash-table))

(defun refalg~refinement-algorithm-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "Something." )
	   (effect  "NOne.")
	   (value   "T if input is a refalg+refinement-algorithm, nil otherwise."))
  (typep obj 'refalg+refinement-algorithm))



