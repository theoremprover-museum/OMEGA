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


(mod~defmod THTAC 
            :uses (data keim mod omega pds prob term th)
            :documentation "Functions for tactics in theories"
            :exports (
		      thtac~subsumed                      
                      thtac~substitute-defis
                      ))

(defgeneric thtac~substitute-defis (f prohibited)
  (declare (edited  "09-FEB-2001")
	   (authors Pollet)
	   (input   "A term, a list of prohibitied definitions.")
	   (value   "1. The term, where each defined constant"
		    "(not in prohibitied list) is substituted"
		    "by it's definition."
		    "2. A list of definitions, that were used."))
  (:method ((f term+abstr) prohibited)
	   (multiple-value-bind (expandedf changed?)
	       (thtac~substitute-defis (data~abstr-range f) prohibited)
	     (values (if changed?
			 (term~abstr-create (data~abstr-domain f) expandedf)
		       f)
		     changed?)))
  (:method ((f term+appl) prohibited)
	   (multiple-value-bind (expandedf changedf?)
	       (thtac~substitute-defis (data~appl-function f) prohibited)
	     (multiple-value-bind (expandedargs changedargs?)
		 (thtac~substitute-defis (data~appl-arguments f) prohibited)
	       (let ((result (if (or changedf? changedargs?)
			     (term~appl-create expandedf
					       expandedargs)
			     f)))
	       (values
		result
		(append changedf? changedargs?))))))
  (:method ((f term+constant) prohibited)
	   (let ((defi (repr~find-definition (keim~name f) (prob~theory omega*current-proof-plan))))
	     (if (and defi
		      (notany #'(lambda (x) (eq x defi)) prohibited))
		 (let* ((look-pre (th~ass-node defi))
			(look (pds~ground-definiens look-pre f)))
		   (multiple-value-bind (newf changed?)
		       (thtac~substitute-defis look prohibited)
		     (values newf (cons defi changed?))))
	       (values f nil))))
  (:method ((f term+variable) prohibited)
	   (declare (ignore prohibited))
	   (values f nil))
  (:method ((f list) prohibited)
	   (let (changedlist)
	     (values
	      (mapcar #'(lambda (term)
			  (multiple-value-bind (newterm changed?)
			      (thtac~substitute-defis term prohibited)
			    (setf changedlist (append changed? changedlist))
			    newterm)) f)
	      changedlist)))
  (:method (f  prohibited)
	   (declare (ignore prohibited))
	   (omega~error "~A is not a term" f)))

;; for special terms

(defmethod thtac~substitute-defis ((f term+annotated-constant) prohibited)
	   (let ((defi (repr~definition f)))
	     (if (notany #'(lambda (x) (data~equal (th~definition-constant x)
						   (th~definition-constant defi))) prohibited)
		 (let* ((look-pre (th~ass-node defi))
			(look (pds~ground-definiens look-pre f)))
		   (multiple-value-bind (newf changed?)
		       (thtac~substitute-defis look prohibited)
		     (values newf (cons defi changed?))))
	       (values f nil))))

#| The following should be obsolete
(defmethod thtac~substitute-defis ((f term+number) prohibited)
	   (let ((defi (repr~definition f)))
	     (if (notany #'(lambda (x) (data~equal (th~definition-constant x)
						   (th~definition-constant defi))) prohibited)
		 (let* ((look-pre (th~ass-node defi))
			(look (pds~ground-definiens look-pre f)))
		   (multiple-value-bind (newf changed?)
		       (thtac~substitute-defis look prohibited)
		     (values newf (cons defi changed?))))
	       (values f nil))))

(defmethod thtac~substitute-defis ((f term+set) prohibited)
	   (let ((defi (repr~definition f)))
	     (if (notany #'(lambda (x) (data~equal (th~definition-constant x)
						   (th~definition-constant defi))) prohibited)
		 (let* ((look-pre (th~ass-node defi))
			(look (pds~ground-definiens look-pre f)))
		   (multiple-value-bind (newf changed?)
		       (thtac~substitute-defis look prohibited)
		     (values newf (cons defi changed?))))
	       (values f nil))))

(defmethod thtac~substitute-defis ((f term+list) prohibited)
	   (let ((defi (repr~definition f)))
	     (if (notany #'(lambda (x) (data~equal (th~definition-constant x)
						   (th~definition-constant defi))) prohibited)
		 (let* ((look-pre (th~ass-node defi))
			(look (pds~ground-definiens look-pre f)))
		   (multiple-value-bind (newf changed?)
		       (thtac~substitute-defis look prohibited)
		     (values newf (cons defi changed?))))
	       (values f nil))))

(defmethod thtac~substitute-defis ((f term+cyc) prohibited)
	   (let ((defi (repr~definition f)))
	     (if (notany #'(lambda (x) (data~equal (th~definition-constant x)
						   (th~definition-constant defi))) prohibited)
		 (let* ((look-pre (th~ass-node defi))
			(look (pds~ground-definiens look-pre f)))
		   (multiple-value-bind (newf changed?)
		       (thtac~substitute-defis look prohibited)
		     (values newf (cons defi changed?))))
	       (values f nil))))
|#


;;; test for supsumption 

(defun thtac~subsumed (goal supports)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (clauses
	  (append (hocnf~normalize (logic~negate (node~formula goal))env)
		  (mapcan #'(lambda (form)(hocnf~normalize (node~formula form)env)) supports))))
    (thtac=simple-prover clauses)))
    

(defun thtac=simple-prover (clauses)
  (cond ((null clauses) nil)
	((some #'cl~empty-p clauses) T)
	(T (let* ((unit (find-if #'cl~unit-p clauses)))
	     (unless unit
	       (setf unit (nth (random (length clauses)) clauses)))
	     (do* ((cl clauses (rest cl))
		   (resolv (res~binary-resolution (car cl) unit :eliminate-tautologies t)
			   (res~binary-resolution (car cl) unit :eliminate-tautologies t)))
		 ((or resolv (null (rest cl)))
		  (if resolv
		      (thtac=simple-prover (append (remove (car cl) (remove unit clauses)) resolv))
		    (thtac=simple-prover (remove unit clauses)))))))))

	       
