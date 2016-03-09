;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1994 by AG Siekmann, Fachbereich Informatik,             ;;
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

(in-package :keim)



(mod~defmod SUBS 
            :uses (cl data lit res term)
            :documentation "Procedures for testing whether one clause subsumes another."
            :exports (
                      
                      subs~subsumes-p
                      ))




#{
\section{Subsumption}
This module contains functions for testing whether one clause subsumes
another.
#}

(defun subs~subsumes-p (clause1 clause2)
  (declare
   (authors nesmith)
   (input "Two clauses")
   (value "T if CLAUSE1 subsumes CLAUSE2, otherwise nil")
   (effect "None"))
  ;; clause1 can subsume clause2 only if it is at least as short
  (unless (> (length (cl~literals clause1))
	     (length (cl~literals clause2)))
    ;; The idea is, CLAUSE1 subsumes CLAUSE2 if:
    ;;   we replace CLAUSE2's variables by constants, then
    ;;   we make a set of unit clauses, each being the negation of a
    ;;   literal in CLAUSE2, then we try to resolve away CLAUSE1 
    ;;   using these unit clauses. If we get the empty clause, we win.
    (let ((neg-clauses (subs=make-ground (cl~literals clause2)))
	  (clause-set (list (cl~literals clause1))))
      ;; neg-clauses is a list of lists of literals,
      ;; as is clause-set 
      (loop
       (when (member nil clause-set) ; found empty clause!
	 (return-from subs~subsumes-p t))
       ;; make all resolvents between current clause-set and neg-clauses
       (setq clause-set (subs=get-all-resolvents clause-set neg-clauses))
       (when (null clause-set) ; can't make any more resolvents
	 (return-from subs~subsumes-p nil))))))

(defmethod cl~literals ((cl list))
  cl)

#|
(defmethod cl~neg-literals ((cl list))
  (let ((neg-lits nil))
    (dolist (lit cl (nreverse neg-lits))
      (unless(lit~positive-p lit)
	(push lit neg-lits)))))


(defmethod cl~pos-literals ((cl list))
  (let ((pos-lits nil))
    (dolist (lit cl (nreverse pos-lits))
      (when (lit~positive-p lit)
	(push lit pos-lits))))
  )
|# ;; testen ob notwendig AMEIER


;; Return a list of lists, each containing a literal.
;; The literals are the negated literals from the clause, with
;; new constants replacing all the free variables.

(defun subs=make-ground (lits)
  (declare
   (authors nesmith)
   (input "List of literals")
   (value "A list of lists of literals.  This list is constructed by 
replacing all free variables in the literals by constants, then negating each
literal.  Each of the negated literals is put into its own list and the list
of these is returned.")
   (effect "None"))
  (let* ((vars (data~free-variables lits))
	 ;; make brand-new constants
	 (consts 
	  (mapcar #'(lambda (var) 
		      (term~constant-create (gensym "CON")
					    (term~type var)))
		  vars))
	 ;; subst~apply should give us *new* literals, not
	 ;; destructively changed ones
	 (new-lits (subst~apply (subst~create vars consts) lits))
	 (result nil))
    (dolist (new-lit new-lits (nreverse result))
      ;; change the literals' polarities, return list 
      (setf (lit~polarity new-lit)
	    (not (lit~positive-p new-lit)))
      (push (list new-lit) result))))


(defun subs=get-all-resolvents (clause-set neg-clauses)
  (declare
   (authors nesmith)
   (input "Two lists of clauses (where clauses are lists of literals)")
   (value "A list of all resolvents between clauses from CLAUSE-SET and
NEG-CLAUSES")
   (effect "None"))
  (let ((result nil)
	(result1 nil))
    (dolist (clause clause-set result)
      (setq result
	    (nconc result
		   (progn (setq result1 nil)
			  ;; inner loop returns a list of resolvents
			  ;; between a single clause in CLAUSE-SET and
			  ;; all clauses in NEG-CLAUSES
			  (dolist (neg-clause neg-clauses result1)
			    (setq result1 
				  (nconc result1
					 (mapcar #'cl~literals
						 (res~binary-resolution 
						  (cl~create clause)
						  (cl~create neg-clause))))))))))))



