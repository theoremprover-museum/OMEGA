;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


(in-package "KEIM")

(mod~defmod sctx :uses (sort)
            :documentation "Fundamental data structures."
            :exports (
		     sctx+var-sctx
		     sctx~domain
		     sctx~bindings
		     sctx~var-sctx-create
		     sctx~add-pair
		     sctx~add
		     sctx~var-sctx-rem-pair
		     sctx~empty-var-sctx
		     sctx~var-sctx-get-component
		     sctx+sctx
		     sctx~create
		     sctx~domain
		     sctx~in-domain-p
		     sctx~in-codomain-p
		     )
	    )


#{ \section{contexts}\label{mod:sctx}

\subsection{variable contexts}\label{mod:sctx:var-sctx}

variable contexts are assignments of variables to annotations like types or sorts.
For instance the variable context $[ X : i ],[ F : (i i)\rightarrow i ]$ means that the type i
is assigned to the variable $X$, and the type $(i i)\rightarrow i$ is assigned to the variable $F$.

#}

(eval-when (load compile eval)
  (defclass sctx+var-sctx (data+object)
    ((domain :initarg :domain
	     :accessor sctx~domain)
     (bindings :initarg :bindings
	       :accessor sctx~bindings))
    (:documentation "This is the class of variable-contexts")))

(defun sctx~var-sctx-create (domain bindings)
  (make-instance 'sctx+var-sctx
		 :domain domain
		 :bindings bindings))

(defun sctx~add-pair (pair var-sctx)
  (sctx~var-sctx-create (cons (first pair) (sctx~domain var-sctx))
		      (cons (second pair) (sctx~bindings var-sctx))))

(defgeneric sctx~add (sctx1 sctx2)
  (declare (edited  "04-MAR-1996 15:09")
	   (authors GKLEIN)
	   (input   "Two contexts SCTX1 and SCTX2.")
	   (effect  "None.")
	   (value   "The concatenation of the two contexts.")))

(defmethod sctx~add ((var-sctx1 sctx+var-sctx) (var-sctx2 sctx+var-sctx))
  (sctx~var-sctx-create (append (sctx~domain var-sctx2) (sctx~domain var-sctx1))
		      (append (sctx~bindings var-sctx2) (sctx~bindings var-sctx1))))

(defmethod sctx~add ((var-sctxs list) (var-sctx2 sctx+var-sctx))
  (if var-sctxs
      (sctx~add (car var-sctxs) (sctx~add (cdr var-sctxs) var-sctx2))
    var-sctx2))

(defun sctx~list-add (sctx-list)
  (declare (edited  "05-MAR-1996 12:09")
	   (authors GKLEIN)
	   (input   "A list of variable contexts SCTX-LIST.")
	   (effect  "None.")
	   (value   "A new variable context containing all contexts in SCTX-LIST."))
  (let ((domain (mapcan #'sctx~domain sctx-list))
	(bindings (mapcan #'sctx~bindings sctx-list)))
    (sctx~var-sctx-create domain bindings)))

(defun sctx~var-sctx-rem-pair (variable context)
  (sctx~var-sctx-rem-pair! variable (sctx=copy context)))

(defun sctx~var-sctx-rem-pair! (variable context)
  (labels ((sctx=sctx-delete (domain bindings)
			   (cond ((null domain) (values nil nil))
				 ((data~equal variable (car domain))
				  (values (cdr domain) (cdr bindings)))
				 (t (multiple-value-bind (domain-tail bindings-tail)
					(sctx=sctx-delete  (cdr domain) (cdr bindings))
				      (setf (cdr domain) domain-tail
					    (cdr bindings) bindings-tail)
				      (values domain bindings))))))
    (multiple-value-bind (domain-tail bindings-tail)
	(sctx=sctx-delete (sctx~domain context) (sctx~bindings context))
      (setf (sctx~domain context) domain-tail)
      (setf (sctx~bindings context) bindings-tail)))
  context)

(defmethod sctx=copy ((context sctx+var-sctx) &key downto)
  (declare (ignore downto))
  (sctx~var-sctx-create (copy-list (sctx~domain context)) (copy-list (sctx~bindings context))))


;;; The empty variable context

(defvar sctx*empty-var-sctx (sctx~var-sctx-create nil nil))

(defun sctx~empty-var-sctx ()
  (declare (special sctx*empty-var-sctx))
  sctx*empty-var-sctx)

(defun sctx~var-sctx-get-component (variable context)
  (some #'(lambda (var sort)
	    (if (eq var variable) sort nil))
	(sctx~domain context)
	(sctx~bindings context)))


#{\subsection{complex contexts}\label{mod:sctx:com-sctx}

In the domain of a calculus for a higher order logic different kinds of variables may occur:
positive variables which can be instantiated by some term, and locally bounded variables
which are not allowed to occur free within
a instantiation term for any positive variable.\\
Furthermore there are some restrictions usually, called variable conditions.
variable conditions are pairs $(X^+,Y^-)$ of variables where the first variable is positive and the second is negative,
with the meaning that in a instantiation term for the positive variable $X^+$ an occurance of the negative
variable $Y^-$ must not occur.
#}

(eval-when (load compile eval)
  (defclass sctx+sctx (data+object)
    ((sov :initarg :sov
	  :accessor sctx~sov)
     (loc :initarg :loc
	  :accessor sctx~loc))
    (:documentation "This is the class of contexts")))
  
(defun sctx~create (sov loc)
  (make-instance 'sctx+sctx
		 :sov sov
		 :loc loc))

;;;
;;; print-object
;;;

(defmethod print-object ((sctx sctx+var-sctx) stream)
  (format stream "[")
  (format stream "~:{ ~S :: ~S ~}" (mapcar #'(lambda (var sort) (list var sort))
					  (sctx~domain sctx)
					  (sctx~bindings sctx)))
  (format stream "]"))

(defmethod print-object ((sctx sctx+sctx) stream)
  (format stream "sctx:")
  (format stream "~%sov: ~A" (sctx~sov sctx))
  (format stream "~%loc: ~A" (sctx~loc sctx))
  )




    

