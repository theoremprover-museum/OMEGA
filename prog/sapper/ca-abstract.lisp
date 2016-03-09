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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains the machinery to handle CAS's abstractly 
;; All extensions for the functions generically provided in this
;; module should be done in different files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)

(mod~defmod caab
	    :uses ()
	    :documentation "Abstract Computer Algebraic Engines"
	    :exports (ca+system
		      ca~system-create
		      ca~system-trans-table
		      ca~insert-trans-table
		      ca~system-call
		      ca~sys-table-lookup
		      ca~find-system
		      ca~defsystem
		      ca~read-system-file
		      ca~get-all-systems
		      ca~translation
		      ca~build-object
		      ca~rebuild-object
		      ))

(defclass ca+system (help+help)
  ((trans-table :initarg :trans-table
		:initform nil
		:accessor ca~system-trans-table
		:documentation "An association list containing the translation table")
   (call :initarg :call
	 :initform nil
	 :accessor ca~system-call
	 :documentation "A function which calls the System"))
  (:documentation "The abstract class of all Computer Algebra Systems"))

(defvar ca*system-hash-table (make-hash-table :test #'equal)
  "A hash table, that keeps track of all the existing abstract CAS at any time.")


;;; unfortunately we need one variable, that is totally global!

(defvar ca*global-environment nil
  (documentation "This variable contains the environment in which an OMEGA proof is living."))

(defun ca~system-create (name trans-table call help)
  (declare (edited  "22-NOV-1995 19:23")
	   (authors SORGE)
	   (input   "A name, an association list containing a valid translation table for"
		    "the CAS, a calling function and a help-string")
	   (effect  "An instance of CA+SYSTEM is created")
	   (value   "The new instance"))
  (make-instance 'ca+system
		 :name name
		 :trans-table trans-table
		 :call call
		 :help help))

(defgeneric ca~insert-trans-table (sys key value)
  (declare (edited  "22-NOV-1995 19:16")
	   (authors SORGE)
	   (input   "A CAS, a KEY and a VALUE (or a list of KEYs and a list od VALUEs.")
	   (effect  "The KEYs and VALUEs are entered in the translatin table.")
	   (value   "The new translation table."))
  (:method (sys key value)
	   (declare (ignore key value))
	   (error "~A must be of type CA+SYSTEM" sys))
  (:method ((sys ca+system) key value)
	   (setf (ca~system-trans-table sys)
		 (acons key value (ca~system-trans-table sys))))
  (:method ((sys ca+system) (key cons) (value cons))
	   (setf (ca~system-trans-table sys)
		 (append (ca~system-trans-table sys)
			 (pairlis key value)))))

(defun ca~sys-table-lookup (sys function)
  (declare (edited  "22-NOV-1995 21:02")
	   (authors SORGE)
	   (input   "A CAS and a function in Omega-syntax")
	   (value   "The corresponding function in CAS-syntax if it exists, NIL if not"))
  (cdr (assoc function (ca~system-trans-table sys) :test #'string-equal)))


(defgeneric ca~find-system (name)
  (declare (edited  "22-NOV-1995 19:47")
	   (authors SORGE)
	   (input   "A name of a CAS.")
	   (value   "The CAS if it exists otherwise NIL."))
  (:method (name)
	   (error "~A is not a valid specifier of a CAS." name))
  (:method ((name symbol))
	   (gethash (symbol-name name) ca*system-hash-table))
  (:method ((name string))
	   (gethash (string-upcase name) ca*system-hash-table))    
  (:method ((sys ca+system))
	   (gethash (keim~name sys) ca*system-hash-table)))
	   
  

(defmacro ca~defsystem (name &rest attribs)
  (declare (edited  "22-NOV-1995 19:41")
	   (authors SORGE)
           (input   "A written representation of an abstract ca+system."
		    "Here is an example of the syntax:  
                     \\begin{codebox} 
                     \\vspace{.1cm}
 (ca~defsystem myCAS
	   (help "Polynomials and nothing but polynomials")
	   (translations (p1+ (ca~polynomial-addition (poly poly)))
			 (p2+ (ca~polynomial-addition (poly poly)))
			 (p1* (ca~polynomial-multiplication (poly poly)))
			 (dp1 (ca~differentiate-polynomial (poly nat))))
	   (call eval)) 
 \\end{codebox}")
           (effect  "Read the system, construct a ca+system object and register it.")
           (value   "None."))
  `(block :defsystem
     (let ((help "") (name ',name) (attribs ',attribs)
	   (trans-l1) (trans-l2) (call))
       (do ((attribs (cdr attribs) (cdr attribs))
	    (attrib (car attribs) (car attribs)))
	   ((and (null attrib) (null attribs)))
	 (if (consp attrib)
	     (let ((carattrib (if (symbolp (car attrib)) 
				  (symbol-name (car attrib)) 
				(car attrib))))
	       (cond ((string-equal carattrib :help) (setq help (cadr attrib)))
		     ((string-equal carattrib :translations)
		      (mapcar #'(lambda (x)
				  (setq trans-l1 (append trans-l1 (list (car x))))
				  (setq trans-l2 (append trans-l2 (cdr x))))
			      (cdr attrib)))
		     ((string-equal carattrib :call) (setq call (cadr attrib)))
		     (t (return-from :defsystem
			  (omega~error ";;;ca~~defsystem ~A: Not expecting ~S" name attrib)))))
	   (return-from :defsystem
	     (omega~error ";;;ca~~defsystem ~A: ~A is not a valid attribute specification" name attrib))))
       (when (ca~find-system name)
	 (omega~warn ";;; Redefining abstract CAS ~A!" name))
       (let ((new-cas (make-instance 'ca+system
				     :name name
				     :trans-table (pairlis trans-l1 trans-l2)
				     :call call
				     :help help)))
	 (setf (gethash (etypecase name
			  (symbol (symbol-name name))
			  (string (string-upcase name)))
			ca*system-hash-table)
	       new-cas)))))

(defun ca~read-system-file (file)
  (declare (edited  "22-NOV-1995 20:07")
	   (authors SORGE)
	   (input   "A filename.")
	   (effect  "Reads and creates abstract CAS from a file.")
	   (value   "T if everything went allright."))
  (when (open file :if-does-not-exist nil)
    (with-open-file (in file :direction :input)
		    (do ((x (read in nil) (read in nil)))
			((null x) t)
		      (eval x)))))

(defun ca~get-all-systems ()
  (declare (edited  "26-FEB-1996 15:42")
	   (authors SORGE)
	   (input   "None.")
	   (value   "A list of all existing abstract CAS."))
  (let ((sys-list))
    (maphash #'(lambda (x y)
		 (declare (ignore x))
		 (setf sys-list (append sys-list (list y))))
	     ca*system-hash-table)
    sys-list))


;;;;;;; An attempt on a translator
;;; This is the generic translation unit
;;; It provides function for building and rebuilding objects and calling a CAS


(defun ca~translation (term sys)
  (declare (edited  "22-NOV-1995 20:56")
	   (authors SORGE)
	   (input   "A POST-term and a CAS")
	   (effect  "Translates the term into CA-Syntax and does the necessary calls")
	   (value   "A POST-term with the result"))
  (let* ((func (data~struct-at-position term (pos~list-position '(0))))
	 (cafunction (ca~sys-table-lookup sys (keim~name func))))
    (if cafunction
	(let* ((args (data~appl-arguments term))
	       (ca-func (car cafunction))
	       (ca-args (map 'list
			     #'(lambda (x y)
				 (ca~build-object x y (keim~name sys)))
			     args (reverse (cdadr cafunction))))
	       (ca-result (caadr cafunction))
	       (call (ca~system-call sys)))
	  (ca~rebuild-object (funcall (if (functionp call) call
					(symbol-function call))
				      (cons ca-func ca-args)) ca-result (keim~name sys)))
      (error "~A is not a valid term" term))))
    

(defgeneric ca~build-object (term object system)
  (declare (edited  "22-NOV-1995 22:14")
	   (authors SORGE)
	   (input   "A term, a symbol indicating the object and one indicating the system.")
	   (effect  "Builds a CAS object from TERM. For each CAS and each of its objects"
		    "a corresponding method should exist. There is one default method provided."
		    "(See CA~REBUILD-OBJECT)")
	   (value   "The build TERM"))
  (:method (term object system)
	   (declare (ignore term object system))
	   (error "You really must be more specific")))


(defgeneric ca~rebuild-object (term object system)
  (declare (edited  "27-NOV-1995 14:55")
	   (authors SORGE)
	   (input   "A term in a CAS-syntax, an object-type and a CA-system.")
	   (effect  "A term in OMEGA-syntax of type object is constructed from term."
		    "For each CAS and each of its objects a corresponding method should exist."
		    "There is one default method provided.(See CA~REBUILD-OBJECT)")
	   (value   "The new OMEGA-term"))
  (:method (term object system)
	   (declare (ignore term object system))
	   (error "Could you be more specific, PLEASE?")))

;(defmethod make-load-form ((obj ca+system) &optional env)
;  (declare (ignore env))
;  (make-load-form-saving-slots obj :slots '(keim::name keim::help omega::trans-table omega::call)))
