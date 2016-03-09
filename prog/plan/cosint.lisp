;;; -*- Mode: KEIM; Base: 10; Syntax: Common-lisp; Package: OMEGA -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
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

(mod~defmod COSINT 
            :uses (help keim omega)
            :documentation "The constraint solver interface."
            :exports (cosint+constraint-solver
                      
                      cosint~new-cs
                      cosint~cs-call-function
                      cosint~cs-meta-var
		      
                      cosint~solve-all-cs
                      cosint~reset-all-cs
		      cosint~reflect-all-cs
		      
                      cosint*constraint-solvers))


(defvar cosint*constraint-solvers (make-hash-table :test #'eql)
  "The hashtable with all known constraint-solvers")

(eval-when (load compile eval)
  (defclass cosint+constraint-solver (help+help)
    ((call-function :initarg :call-function
		    :initform nil
		    :accessor cosint~cs-call-function
		    :documentation "")
     )
    (:documentation ""))
  )


(defun cosint~new-cs (name call-fun)
  (declare (edited  "16-SEP-1998")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  (let ((new-cs (make-instance 'cosint+constraint-solver
			       :name name
			       :call-function call-fun
			       ))
	)
    (setf (gethash name cosint*constraint-solvers) new-cs)
    new-cs))


(defun cosint~call-cs (csname command &rest args)
  (let ((cs (gethash csname cosint*constraint-solvers))
	)
    (if cs
	(let ((result (apply (cosint~cs-call-function cs) (cons command args))))
	  result)
      (progn
	(omega~trace "The constraint-solver ~S doesn't exist." csname)
	(values nil nil)))))


(defun cosint~reflect-all-cs (&rest args)
  (let* ((metavars nil)
	 (formulae nil))
    (maphash #'(lambda (name cs)
		 (if cs
		     (let* ((pair (apply (cosint~cs-call-function cs) (cons 'reflect args)))
			    (meta (car pair))
			    (formula (cdr pair)))
		       (when meta
			 (push meta metavars)
			 (push formula formulae)))
		   (omega~error "There is something wrong in the cs-hash-table...")))
	     cosint*constraint-solvers)
    (if (null metavars)
	(subst~create nil nil)
      (subst~create metavars formulae))))


(defun cosint~solve-all-cs (&rest args)
  (let* ((subst (first args))
	 (domain (subst~domain subst))
	 (codomain (subst~codomain subst))
	 )
    (maphash #'(lambda (name cs)
		 (if cs
		     (let* ((cs-subst (apply (cosint~cs-call-function cs) (cons 'findSolution args)))
			    )
		       (setf subst (subst~compose-substitution subst cs-subst))
		       )
		   (omega~error "There is something wrong in the cs-hash-table...")))
	     cosint*constraint-solvers)
    subst))

(defun cosint~reset-all-cs ()
  (maphash #'(lambda (name cs)
	       (if cs
		   (apply (cosint~cs-call-function cs) (list 'reset))
		 (omega~error "There is something wrong in the cs-hash-table...")))
	   cosint*constraint-solvers)
  )


