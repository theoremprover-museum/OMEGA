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

(in-package :omega)
 
(mod~defmod tacl
	    :uses (rule view comint asi com tac rule)
	    :documentation "Tactical Functions"
	    :exports (tacl~init
		      tacl~apply
		      tacl~end
		      tacl~sequence
		      tacl~insert&return-assumption
		      ))

(eval-when (load compile eval)
  (let ((reasons nil)
	(conclusions nil)
	(premises nil)
	(successful t))
    
    (defun tacl~init (outline)
      (let ((prem-list (tacl=get-all-premises outline)))
	(setf premises (remove-if #'null
				  (mapcar #'(lambda (x)
					      (find x prem-list))
					  outline)))
	(setf conclusions (subseq outline 0 (- (length outline) (length premises))))
	(dolist (node conclusions)
	  (setf (node~justification node)
		(pdsj~insert-just-below
		 (node~justification node)
		 (pdsj~open-just-create))))
	(let ((test-reasons (mapcar #'(lambda (node) (or (pdsj~own-reason (node~justification node))
						      (pdsj~above-own-reason (node~justification node))))
				    conclusions)))
	  (if (car test-reasons) (setf reasons test-reasons)
	    (setf reasons
		  (remove-duplicates
		   (apply #'append
			  (mapcar #'(lambda (node)
				      (pdsj~all-other-reasons (node~justification node)))
				  conclusions)))))
	  )))
    
    (defun tacl=get-all-premises (node-list)
      (declare (edited  "19-JUN-1997 18:39")
	       (authors SORGE)
	       (input   "A list of nodes.")
	       (value   "A list containing all the premises of the nodes."))
      (when node-list
	(union (node~just-premises (car node-list))
	       (tacl=get-all-premises (cdr node-list)))))
    
    
    (defun tacl~apply (name outline parameters)
      (let* ((outline-pattern (infer~compute-outline-pattern outline)) 
	     (object (infer~outline-pattern2application name outline-pattern))
	     (rule? (rule~find-rule object))
	     (tactic? (tac~find-tactic object))
	     (wtac (infer~find-method name))
	     (wtac? (when (infer~wild-tactic-p wtac)
		      (if object object (infer~outline-function wtac)))))
	(cond (rule? (multiple-value-bind (success new-outline)
			 (rule~apply object outline parameters rule*verbose :purpose :expand)
		       (if success
			   (progn
			     (setf (pdsj~outline-pattern (node~justification (car new-outline))) outline-pattern)
			     new-outline)
			 (setf successful nil))))
	      (tactic? (multiple-value-bind (success new-outline concl)
			   (tac~apply object outline parameters tac*verbose :purpose :expand)
			 (if success
			     (progn
			       (tac~rewrite-outline-pattern outline-pattern concl)
			       new-outline)
 			   (setf successful nil))))
	      (wtac? (multiple-value-bind (conclusions premises)
			 (wtac~preprocess-outline outline outline-pattern)
		       (multiple-value-bind (success new-conclusions new-premises hypotheses)
			   (wtac~apply wtac object conclusions premises parameters wtac*verbose
				       :purpose :expand :passkey (infer~passkey wtac))
			 (if success
			     (let* ((new-pattern (append
						  (make-list (length conclusions) :initial-element "EXISTENT")
						  (make-list (- (length new-conclusions)
								(length conclusions))
							     :initial-element "NONEXISTENT")
						  (make-list (length premises) :initial-element "EXISTENT")
						  (make-list (- (length new-premises)
								(length premises))
							     :initial-element "NONEXISTENT"))))
			       (tac~rewrite-outline-pattern new-pattern new-conclusions)
			       (list new-conclusions new-premises hypotheses))
 			   (setf successful nil)))))
	      (t (setf successful nil)
		 (omega~warn ";;;TACL~~APPLY: Something went wrong....")
		 ))))
    

    (defun tacl=construct-method (name direction)
      (declare (edited  "26-JUN-1997 10:33")
	       (authors SORGE)
	       (input   "A name of a tactic and a string inidicating the direction.")
	       (effect  "None.")
	   (value   "The method-name in a special direction.")) 
      (etypecase name
	(symbol (concatenate 'string (symbol-name name) (string-upcase direction)))
	(string (concatenate 'string (string-upcase name) (string-upcase direction)))))

    (defun tacl~end (&key ((:force force) nil))
      (if (and (not force) successful)
	  (mapc #'(lambda (conc reason)
		    (setf (pdsj~status (pdsj~above (node~justification conc)))
			  "expanded")  ;;; set the old justification-status
		    (unless (infer~rule-p (just~method (node~justification conc)))
		      (setf (pdsj~status (node~justification conc)) "unexpanded"))
		    (tacl=insert-reasons-upward (node~just-premises conc) reason))
		conclusions reasons)
	(mapc #'(lambda (conc)
		  (setf (node~justification conc)
			(pdsj~above (node~justification conc)))
		  (setf (pdsj~below (node~justification conc)) nil))
	      conclusions))
      (setf successful t))

    (defun tacl=insert-reasons-upward (node-list reason)
      (when node-list
	(dolist (node node-list)
	  (unless (find node premises)
	    (pdsn~insert-reason! node reason)
	    (tacl=insert-reasons-upward (node~just-premises node) reason)))))
    
    ))

(defun tacl=line-bindings (vars lines &optional (number 0))
  (cond ((null vars) nil)
	((symbolp vars) (list (list vars lines)))
	((listp vars) (cons (list (car vars) (list 'elt lines number))
			    (tacl=line-bindings (cdr vars) lines (1+ number))))))

(defmacro tacl=sequence (&body tactics)
  (let ((lines (gentemp "lines-")))
    `(let ((,lines ,(if (null (caar tactics)) nil `(tacl~apply ,@(cadar tactics)))))
       (if ,lines
	   (let ,(tacl=line-bindings (caar tactics) lines)
	     ,(if (rest tactics)
		  `(tacl=sequence ,@(rest tactics))
		`(progn (unless hiding-status ;(or (mixin~activep) hiding-status)
			  (mapc #'(lambda (line)
				    (if (listp line)
					(mapc #'view~unhide-line line)
				      (view~unhide-line line)))
				,lines))
			,lines)))
	 (progn (warn "Tactic ~A failed" (quote ,(cadar tactics)))
		nil)))))

(defmacro tacl~sequence (&body clauses)
  (declare (edited  "11-FEB-1994 10:38")
	   (authors SORGE RICHTS)
	   (input   "Some lists of the form (VAR FORM) where VAR is a symbol or a list of symbols and FORM is"
		    "an expressions calling a rule or tactic, i.e. returning two values:"
		    "1. True if the rule/tactic was applied successfully;"
		    "2. A list of new lines.")
	   (effect  "The forms are evaluated sequently: If a form was evaluated"
		    "successfully (i.e. its first value is true), VAR is bound to the"
		    "second value (the new lines) and the next form is evaluated."
		    "If a form returns NIL, the sequence is stopped and NIL is returned."
		    "The binding of VAR to the returned lines happens as follows: If VAR"
		    "is a symbol, it is bound to the list of lines; if VAR is a list"
		    "symbols, the first symbol in VAR is bound to the first line in the"
		    "list, the second symbol to the second line, and so on. An error occurs"
		    "if there are more symbols in VAR than lines in the second result of"
		    "the evaluated form.")
	   (value   "Multiple value:"
		    "1. True iff all tactics were applied successfully."
		    "2. The second value of the last tactic, i.e. a list of lines,"
		    "   if all tactics were applied successfully; NIL else.")
	   (remark  "If you want to return another result than that of the last tactic,"
		    "you have to add a dummy tactic that simply returns T"
		    "and the desired lines (See example)."
		    "Since the variable of the last clause is never"
		    "needed (its scope ends with the TAC~SEQUENCE), you better leave it"
		    "out in order to prevent the compiler from signaling a 'variable never used'"
		    "warning (See example)."
		    "There should not be too many clauses (more than 10) in a TACL~SEQUENCE since else the"
		    "compiler will run very long. This happens because each clause will be"
		    "expanded to several lines by this macro; so the function becomes very"
		    "long and the compiler seems not to be linear in the length of"
		    "functions. Therefore it is better to divide a long TACL~SEQUENCE into two or more functions.")
	   (example "\\begin{codebox}"
		    "(tacl~sequence"
		    "\\  \\ ((conjunct1 conjunct2)"
		    "\\ \\ \\ (rule~apply 'and-i (list conjunction)))"
		    "\\  \\ (lines1 (rule~apply 'implies-i (list conjunct1)))"
		    "\\  \\ (lines2 (rule~apply 'implies-i (list conjunct2)))"
		    "\\  \\ (() (values t (append lines1 lines2))))))"
		    "\\end{codebox} applies AND-I to LINE and then IMPLIES-I to both"
		    "conjuncts. If this can be done successfully it returns T and"
		    "the four lines resulting from the two IMPLIES-I calls."))
  `(let ((hiding-status (view~hiding-state)))
     (unwind-protect
	 (progn
	   (view~set-hiding-state (tacl=hiding-state))
	   (tacl=sequence ,@clauses))
       (view~set-hiding-state hiding-status))))

(defun tacl=hiding-state ()
  t)

(defgeneric tacl~insert&return-assumption (theory assumption)
  (declare (edited  "03-JUL-1997 18:18")
	   (authors SORGE)
	   (input   "A theory and an assumption.")
	   (effect  "Inserts the assumption into the PDS.")
	   (value   "The assumption."))
  (:method (theory assumption)
	   (declare (ignore assumption))
	   (warn ";;;TACL~~INSERT&RETURN-ASSUMPTION: wrong theory: ~A" theory))
  (:method ((theory symbol) assumption)
	   (if theory
	       (tacl~insert&return-assumption (th~find-theory theory) assumption)
	     (warn ";;;TACL~~INSERT&RETURN-ASSUMPTION: wrong theory: ~A" theory)))
  (:method ((theory string) assumption)
	   (tacl~insert&return-assumption (th~find-theory theory) assumption))
  (:method ((theory th+theory) assumption)
	   (warn ";;;TACL~~INSERT&RETURN-ASSUMPTION: wrong assumption: ~A" assumption))
  (:method ((theory th+theory) (assumption string))
	   (tacl~insert&return-assumption theory (th~find-assumption assumption theory)))
  (:method ((theory th+theory) (assumption symbol))
	   (if assumption
	       (tacl~insert&return-assumption theory (th~find-assumption assumption theory))
	   (warn ";;;TACL~~INSERT&RETURN-ASSUMPTION: wrong assumption: ~A" assumption)))
  (:method ((theory th+theory) (assumption th+assumption))
	   (pds~add-thy-assertion assumption omega*current-proof-plan)
	   (pds~label2node (keim~name assumption) omega*current-proof-plan))
  (:method ((theory th+theory) (assumption prob+problem))
	   (if (prob~proven-p assumption)
	       (progn
		 (pds~add-thy-assertion assumption omega*current-proof-plan)
		 (pds~label2node (keim~name assumption) omega*current-proof-plan)
		 )
	     (warn "An open problem cannot be used as a new assumption."))))

