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
;;   such use.                                                               ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


(in-package "KEIM")

(mod~defmod hou :uses (bind uni-main)
	    :documentation "Abstract interface to higher order pre-unification
                            includes algorithms and the unification-problem
                            datastructure."
	    :exports (hou~unify!
		      hou~solved
		      hou~gb
		      uni~bill-empty-p
		      uni~bill-pay!)
	    )


(defmethod hou~solved ((uni-prob uni+hou))
  (declare (edited  "27-DEC-1996")
	   (authors Chris)
	   (input   "a hou unification problem")
	   (effect  "none")
	   (value   "t if the UNI-PROB is solved, nil otherwise"))
  (null (uni~flex-rigid uni-prob)))

(defmethod hou~gb ((uni-prob uni+hou))
  (declare (edited  "27-DEC-1996")
	   (authors Chris)
	   (input   "a hou unification problem")
	   (effect  "none")
	   (value   "a list of new unification problems, generated"
		    "by applying general binding construction rule"
		    "on UNI-PROB"))
  (when (and (not (hou~solved uni-prob))
	     (not (uni~bill-empty-p uni-prob)))
    (let* ((flex-term (if (or (hou~flex-p (caar (uni~flex-rigid uni-prob)) uni-prob)
			      (data~variable-p (caar (uni~flex-rigid uni-prob))))
			  (caar (uni~flex-rigid uni-prob))
			(cadar (uni~flex-rigid uni-prob))))
	   (rigid-term (if (or (hou~flex-p (caar (uni~flex-rigid uni-prob)) uni-prob)
			       (data~variable-p (caar (uni~flex-rigid uni-prob))))
			  (cadar (uni~flex-rigid uni-prob))
			(caar (uni~flex-rigid uni-prob))))
	   (flex-head (data~top flex-term))
	   (rigid-head (data~top rigid-term))
	   ;; (general-bindings (hou~gb-construct flex-term rigid-term))
	   (general-bindings (gb~construct (term~type flex-head) rigid-head)))
      (mapcar #'(lambda (gb)
		  (let ((new-prob (keim~copy uni-prob)))
		    (setf (uni~terms new-prob) (cons (list flex-head gb)
						     (uni~terms uni-prob)))
		    ;;(setf (uni~flex-rigid new-prob)
                    ;;      (cdr (uni~flex-rigid uni-prob))) ;; cdr deleted -kk- 
		    (setf (uni~link new-prob) uni-prob)
                    ;; establishing link to daughters (optional)
                    (setf (uni~daughters uni-prob)
                          (cons new-prob (uni~daughters uni-prob)))
		    (uni~bill-pay! new-prob)
		    new-prob))
	      general-bindings))))

 
(defmethod hou~unify! ((uni-prob uni+hou))
  (declare (edited  "27-DEC-1996")
	   (authors Chris)
	   (input   "a hou unification problem")
	   (effect  "destructively changes the UNI-PROB")
	   (value   "a list of solved unification-problems respecting"
		    "the given resources for UNI-PROB"))
  (when     
      (every #'(lambda (prob)
		 (every #'(lambda (x)
			    (data~equal (term~type (car prob));;the terms to unify have to be of the same type
					(term~type x)))       ;;to restrictive for type-vars?
			(rest prob)))      
	     (uni~terms uni-prob))
    (bind~with-bindings ((hou=unify! uni-prob)) :btp 'btp0 :insert nil)))



(defmethod hou=unify! ((uni-prob uni+hou))
  (declare (edited  "27-DEC-1996")
	   (authors Chris KK)
	   (input   "a hou unification problem")
	   (effect  "destructively changes the UNI-PROB")
	   (value   "a list of solved unification-problems respecting"
		    "the given resources for UNI-PROB"))
  (let ((simplified-prob (hou~simplify! uni-prob)))
    (when simplified-prob
      (if (hou~solved simplified-prob)
	  (progn (setf (uni~substitution simplified-prob)
		       (bind~global-substitution 'btp0))
		 (list simplified-prob))
	;; else
	(when (not (uni~bill-empty-p simplified-prob))
	  (let ((general-binded-probs (hou~gb simplified-prob)))
	    (mapcan
	     #'(lambda (x)
		 (block new-choice;; return without building substitution
		   (bind~with-bindings
		    ((return-from new-choice
		       (hou=unify! x))) :btp 'btp1 :insert nil)))
	     general-binded-probs)))))))

;; now the unification hook

(defmethod uni~unify-problem((uni uni+hou)
                             &key destructive
                             (solutions 'all))
  (declare (edited  "10-JAN-1997")
	   (authors Konrad)
	   (input   "a higher-order unification problem.")
	   (effect  )
	   (value   "a list of solved problems."))
  (unless destructive (setq uni (keim~copy uni)))
  (let ((solutionprobs (hou~unify! uni)))
    (when (numberp solutions)
      (setq solutionprobs (subseq 0 solutions solutionprobs)))
    solutionprobs))

;; the main function for leo

(defun hou~pre-unify (termlist &key (solutions 'all) (search-depth 10))
  (let* ((uniprob (uni~create nil
			      :class 'uni+hou
			      :terms termlist)))
    (setf (uni~bill uniprob) search-depth)
    (let ((solutionprobs (hou~unify! uniprob)))
      (when (numberp solutions)
	(setf solutionprobs (subseq 0 solutions solutionprobs)))
      (mapcar #'uni~substitution solutionprobs))))






