;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
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

(in-package :keim)



(mod~defmod SIMPL 
            :uses (data env keim logic pos post term type)
            :documentation "Simplification of formulas of type o."
            :exports (
                      simpl+body
                      simpl+normal-line
                      simpl+prot
                      simpl+prot-line
                      simpl+quantification-line
                      simpl+recursion-line
                      
                      simpl~get-line-contense
                      simpl~get-prot-body
                      simpl~get-prot-header
                      simpl~get-prot-lines
                      simpl~get-rec-protocol
                      simpl~get-rec-protocols
                      simpl~get-rule-name
                      simpl~get-variable
                      simpl~simplify
                      
                      simpl*absorption-on
                      simpl*and
                      simpl*env
                      simpl*equiv
                      simpl*equiv-elimination
                      simpl*exists
                      simpl*forall
                      simpl*input
                      simpl*or
                      simpl*prop
                      simpl*protocol-on
                      simpl*quantor))







#{\section{Simplification}\label{mod:simpl}
This module cares for simplifications of formulas of type o. The simplification itself is
done by one interface function named simpl~simplify, it is described in more detail below.
For users, who don't believe that this simplification works correctly
a recording of the activities of this program is possible. The rest of interface functions
are related to these recordings.
#}

(defvar simpl*equiv-elimination ())
;; If simpl*equivalence-on is true, equivalences are eliminated, else
;; not.

(defvar simpl*absorption-on ())
;; switch for absorption

(defvar simpl*protocol-on ())
;; switch for recording


(defvar simpl*env ())
;; This global variable is bound in interfaces. It then contains the
;; environment all functions refer to.

#{\subsection{Simplification itself}
The interface function named simpl~simplify is starting the simplification algorithm.
It is working on the datastructures provided by \keim.
Through parameter switches you can activate/deactivate absorption,
equivalence elimination and the recording of simplification.

The algorithm is descending the term recursively. The control lies by the
function simpl=simplify, that calls, depending of the input, the functions
simpl=atom-treatment, simpl=and-treatment, simpl=or-treatment,
simpl=equiv-treatment and simpl=quantification-treatment. These functions,
except the first one, are calling again simpl=simplify. Implications are elminated.

For sake of efficiency, the 'range' of operators like 'and are extended as
far as possible, before simplification is done on that level. That means,
logical junctions are seen as n-ary operators, in the end, they are
expressed in binary junctors, associated to the left. 
While descending, the arguments are checked on truthvalues. On coming up,
the arguments of junctors are sorted, duplicates removed, absorption is 
applied, truthvalues handled. If a quantification is built up, the quantor
is moved in the term as far as possible. 

#}

(defun simpl~simplify (formula simpl*env &key (absorption t) (protocol nil) (eliminate-equiv nil))
  (declare (edited  "23-FEB-1993 09:58")
	   (authors SCHEJA)
	   (input   "A formula of type o that is in beta--eta--longform, if it is not a first
order formula, an env, where the term comes from, and
optionally three switches.")
	   (effect  "None.")
	   (value   "A new version of formula in some ACI-form, unshared. That means, on each level,
arguments of operators are sorted following some internal order, the law of idempotentcy
 is applied, quantors are moved in as far as possible, implications are expressed in terms of
conjunctions and disjunctions, negations are pushed down on atomar level. If eliminate-equiv is set
on 't, the equivalences are expressed in terms of 'and resp. 'or. Furthermore, the rule of absorption
 (treatment of terms like (a and (a or b))) is applied, if not deactivated by setting absorption on nil.
If protocol is set on 't, a recording of the simplification is given as a second value. "))
  (setq simpl*absorption-on absorption)
  (setq simpl*protocol-on protocol)
  (setq simpl*equiv-elimination eliminate-equiv)
  (if simpl*protocol-on
      (let ((header (simpl=make-prot-header formula))
	    (result ())
	    (body ()))
	(multiple-value-setq (result body) (simpl=simplify formula nil))
	(when  (data~equal result formula)
	  (simpl=change-body nil body))
	(values result
		(simpl=make-protocol header body)))
      (simpl=simplify formula nil)))
	




(defun simpl=simplify (formula negation)
  (declare (edited  "11-JUN-1992 15:27" )
	   (authors SCHEA )
	   (input   "A formula of type o and 'nil (rsp. 't)")
	   (effect  "None." )
	   (value   "If formula is a wff of type o, a simplfied version of  the
aci-normalform of the formula (rsp. of the negated formula).
simpl*protocol-on is a switch for recording the simplification.
The protocol is given as a second value if recording is on,
if negation is nil, the protocol of the simplification of formula,
if negation is t, the same for the negated formula, where negation is pushed in one
step, if possible.
simpl*absorption-on is a switch for absorption" ))
  (unless (type~o-p (term~type formula))
    (error  "The term ~a is no formula of type o." formula))
  
  (cond ((logic~atom-p formula) (simpl=atom-treatment formula negation))
	((simpl=quantification-p formula) (simpl=quantification-treatment formula negation))
					;	((simpl=term~pi-term
					;	formula) (if negation
					;	(list
					;	(env~lookup-object
					;	'not) formula)
					;	formula))
	(t (let ((operator (data~top formula))
		 (arguments (data~appl-arguments formula)))
	     (cond  ((simpl=equiv-p operator)
		     (simpl=equiv-treatment (simpl=equiv-parts formula negation)))
		    ((simpl=or-p operator)
		     (if negation
			 (simpl=and-treatment (simpl=conjunctive-parts formula negation))
			 (simpl=or-treatment (simpl=disjunctive-parts formula negation))))
		    ((simpl=and-p operator)
		     (if negation
			 (simpl=or-treatment (simpl=disjunctive-parts formula negation))
			 (simpl=and-treatment (simpl=conjunctive-parts formula negation))))
		    ((simpl=implies-p operator)
		     (if negation
			 (simpl=and-treatment (append (simpl=conjunctive-parts (first arguments) (not negation))
						      (simpl=conjunctive-parts (second arguments) negation)))
			 (simpl=or-treatment (append (simpl=disjunctive-parts (first arguments) (not negation))
						     (simpl=disjunctive-parts (second arguments) negation)))))
		    ((simpl=not-p operator)
		     (simpl=simplify (first arguments) (not negation)))
		    (t (error "The subterm ~a is no formula of type o in c-normalform." formula)))))))
          
          

; ---------------------------------- Treatment of Atoms -------------------------------------------

(defun simpl=atom-treatment (formula negation) 
  (declare (edited  "02-FEB-1993 16:17")
	   (authors SCHEJA)
	   (input   "An atom.")
	   (effect  "None.")
	   (value   "A copy of atom if negation is nil, a copy of negated atom else.  If simpl*protocol-on
is t, the same value as a second value."))
  (let ((result (if negation
		    (simpl=negate (data~copy formula))
		    (data~copy formula))))
    (if simpl*protocol-on
	(values  result
		 (simpl=init-prot-body (simpl=make-prot-line 'simpl*prop result)))
	result)))
	       

  
; --------------------------------- Treatment of Quantifications -----------------------------------------------

(defun simpl=quantification-treatment (quantification negation)
  (declare (edited  "11-JUN-1992 15:27" )
	   (authors SCHEJA )
	   (input    "A quantification, and 'nil (rsp. 't).")
	   (effect   "None.")
	   (value    "A simplified version of quantification,
where the quantor is moved in the fomula as far as possible.
If simpl*protocol is on, the body of the protocol that describes
this simplification as a second value."))
  
  (let* ((quantor (if  negation
		       (simpl=dual-quantor (data~top quantification))
		       (data~top quantification)))
	 (scope (logic~quantification-scope quantification))
	 (variable (logic~quantification-bound-variable quantification))
	 (result1 (multiple-value-list (simpl=simplify scope negation))))
    (if simpl*protocol-on
	(let ((body (if (and (not negation)
			     (data~equal (first result1) scope))
			;;ie. no deeper simplification has been done and quantor isn't changed
			(simpl=make-prot-body)
			(simpl=init-prot-body
			 (simpl=make-quantification-line
			  (if (logic~universal-quantor-p quantor)
			      'simpl*forall
			      'simpl*exists )
			  variable
			  (simpl=make-protocol (simpl=make-prot-header (if negation
									   (simpl=negate scope)
									   scope))
					       (if (data~equal (first result1) (if negation
										   (simpl=negate scope)
										   scope))
						   ;; ie no deeper simplification has been done.
						   (simpl=make-prot-body)
						   (second result1)))))))
	  
	      (result2 (simpl=move-quantor-in (logic~quantification-create quantor variable (first result1)))))
	  (unless (data~equal result2 (logic~quantification-create quantor variable (first result1)))
	    (simpl=add-prot-line (simpl=make-prot-line 'simpl*quantor result2) body))
	  (values result2
		  body))
	  
	(simpl=move-quantor-in (logic~quantification-create quantor
							   variable  (first result1))))))


; ------------------------------------- Treatment of Equivalences ------------------------------------------------


(defun simpl=equiv-treatment (arglist)
  (declare (edited  "11-AUG-1992 15:27" )
	   (authors SCHEJA )
	   (input    "A list of formulas.")
	   (effect   "None.")
	   (value    "ACI-form of (equiv a1 .. an).
If the switch simpl*equiv-elimination is on, the internal representation of the
disjunctive normal form, ie. equivalences are erased.
If simpl*protocol-on is t, a protocol is given as a second value."))
  (if simpl*protocol-on
      (let ((body ())
	    (result ()))
	(let ((sub-prot ())
	      (no-simplification t))
	  (multiple-value-setq (result sub-prot no-simplification) (simpl=recursion-with-protocol arglist))
	  (setq body  (if  no-simplification
			   (simpl=make-prot-body)
			   (simpl=init-prot-body (simpl=make-recursion-line 'simpl*equiv sub-prot)))))
	(setq result (delete-if #'simpl=true-p result))
	(setq result (simpl=equiv-treatment-help! (sort result #'simpl=formula-less-p)))
	(setq result (simpl=equiv-value result))
	(if (data~equal result (simpl=n-ary-form (env~lookup-object 'equiv simpl*env)
						 arglist))
	    (setq body (simpl=init-prot-body (simpl=make-prot-line 'simpl*prop  result)))
	    ;; recording of subformulas is not necessary, because
	    ;; nothing has been simplified
	    (simpl=add-prot-line (simpl=make-prot-line 'simpl*prop result) body))
	(values result
		body))
      
      (let ((result (mapcar #'(lambda (x) (simpl=simplify x nil)) arglist)))
	(setq result (delete-if #'simpl=true-p result))
	(setq result (simpl=equiv-treatment-help! (sort result #'simpl=formula-less-p)))
	(simpl=equiv-value result))))


(defun simpl=equiv-value (arguments)
  (declare (edited  "19-JAN-1993 16:55")
	   (authors SCHEJA)
	   (input   "A list of terms.")
	   (effect  "None.")
	   (value   "If simpl*equiv-elimination is nil, the equivalence of the
terms, else this equivalence is expressed in disjunctive normal form."))

  (cond ((null arguments) (env~lookup-object 'true simpl*env))
	((= 1 (length arguments)) (first arguments))
	(t     (if simpl*equiv-elimination
		   (let* ((list (simpl=odd-and-even-sublists arguments))
			  (even-list (second list))
			  (complement-list (simpl=complements-of-sublists even-list arguments)))
		     ;; Now, equivalence is rewriten by the disjunctive normal form.
		     (simpl=n-ary-form (env~lookup-object 'or simpl*env)
				       (mapcar #'(lambda (x y)
						   (simpl=n-ary-form (env~lookup-object 'and simpl*env)
								     (append (mapcar #'simpl=negate x)
									     y)))
					       even-list complement-list)))
		   (simpl=n-ary-form (env~lookup-object 'equiv simpl*env) arguments)))))
			   


(defun simpl=equiv-treatment-help!  (arglist)
  (declare (edited  "11-AUG-1992 16:11")
	   (authors SCHEJA)
	   (input   "A list of formulas.")
	   (effect  "Arglist may be modified.")
	   (value   "(false, a,rest) --> (-a,rest), (a, a, rest) --> (rest),
 (a,-a,b,rest) --> (aciform(not b),rest) are applied."))
  (cond ((null arglist) ())
	((eq (length arglist) 1) arglist)
	(t (cond ((data~equal  (first arglist) (second arglist))
		  (simpl=equiv-treatment-help! (cddr arglist)))
		 ((simpl=false-p (first arglist))
		  (cons (simpl=negate (second arglist))
			(simpl=equiv-treatment-help! (cddr arglist))))
		 ((some #'(lambda (x) (simpl=negated-equal x (first arglist))) (rest arglist))
		  (setq arglist (delete-if #'(lambda (x) (simpl=negated-equal x (first arglist))) (rest arglist)  :count 1))
		  (if ( < (length arglist) 2)
		      (env~lookup-object 'false simpl*env)
		      (cons (simpl=negate (second arglist)) (cddr arglist))))
		 (t (cons (first arglist) (simpl=equiv-treatment-help! (rest arglist))))))))


(defun simpl=equiv-parts (formula negation)
  (declare (edited  "11-AUG-1992 15:50")
	   (authors SCHEJA)
	   (input   "A formula an a flag negation.")
	   (effect  "None.")
	   (value   "A list (x1, ... , xn) of subterms of formula, where the
expression (equiv x1 (equiv ... xn)) is equivalent to formula (resp. (not formula)),
and the arity is expanded as far as possible.
Example: (a equiv (not (b equiv c))) , true --> ((not a) (not b) c)."))
  (let ((formula-as-list (data~substructs formula)))
    (if (logic~negation-p formula)
	(simpl=equiv-parts (second formula-as-list) (not negation))
	(if (logic~equivalence-p formula)
	    (append (simpl=equiv-parts (second formula-as-list) negation)
		    (simpl=equiv-parts (third formula-as-list) nil))
	    (if negation
		(list (data~appl-create (env~lookup-object 'not simpl*env) (list formula)))
		(list formula))))))


; --------------------------------------- Treatment of Conjunctions ---------------------------------------------------------


(defun simpl=and-treatment (arglist)
  (declare (edited  "11-AUG-1992 14:51")
	   (authors SCHEJA)
	   (input   "A conjunction and nil, or a disjunction and t.")
	   (effect  "None.")
	   (value   "A multiple value:
1.: left associated aci form of formula, truthvalues erased; 
2.: The prot-body, description of this simplification."))

  (let ((result arglist))
    (if simpl*protocol-on
	(let ((body ()))
	  (if (some #'simpl=false-p arglist)
	      (multiple-value-setq (result body) (simpl=simple-result (env~lookup-object 'false simpl*env)))
	      (progn
		(let ((sub-prot nil)
		      (no-simplification t))
		  (multiple-value-setq (result sub-prot no-simplification) (simpl=recursion-with-protocol arglist))
		 
		  (setq body  (if  no-simplification
				   (simpl=make-prot-body)
				   (simpl=init-prot-body (simpl=make-recursion-line 'simpl*and sub-prot)))))
				   
		(if (some #'simpl=false-p result)
		    (progn
		      (simpl=add-prot-line (simpl=make-prot-line 'simpl*prop (env~lookup-object 'true simpl*env)) body)
		      (setq result (env~lookup-object 'false simpl*env)))
		    (progn
		      (setq result (delete-if #'simpl=true-p  result))
		      (setq result (delete-duplicates result :test #'data~equal ))
		      (cond ((null result)
			     (multiple-value-setq (result body) (simpl=simple-result (env~lookup-object 'true simpl*env))))
			    (t (setq result (sort result #'simpl=formula-less-p ))
			       (setq result (simpl=n-ary-form (env~lookup-object 'and simpl*env)
							      (if (and simpl*absorption-on
								       (> (length result) 1))
								  (simpl=absorption-super! (env~lookup-object 'and simpl*env) result)
								  result)))
			       (simpl=add-prot-line (simpl=make-prot-line 'simpl*prop result) body)))))))
	  (values result body))


	(progn
	  ;;now without recording
	  
	  (when (some #'simpl=false-p result)
	    (setq result (list (env~lookup-object 'false simpl*env))))
	  (setq result (delete-if #'simpl=true-p result))
	  (setq result (mapcar #'(lambda (x) (simpl=simplify x nil)) result))
	  (when (some #'simpl=false-p result)
	    (setq result (list (env~lookup-object 'false simpl*env))))
	  (setq result (delete-if #'simpl=true-p result))
	  (setq result (delete-duplicates result :test #'data~equal))
	  (cond ((null result)
		 (env~lookup-object 'true simpl*env))
		(t (setq result (sort result #'simpl=formula-less-p))
		   (simpl=n-ary-form (env~lookup-object 'and simpl*env)
				     (if (and simpl*absorption-on
					      (> (length result) 1))
					 (simpl=absorption-super!
					  (env~lookup-object 'and simpl*env) result)
					 result))))))))


(defun simpl=conjunctive-parts (formula negation)
  (declare (edited  "07-AUG-1992 16:40")
	   (authors SCHEJA)
	   (input   "A formula in n normalform and a flag  negation.")
	   (effect  "None.")
	   (value   "A list (x1, ... ,xn) of subterms of formula, where
the expression (and x1 (and ... xn)) is equivalent to formula (resp. (not formula))
and the arity of operator is expanded as far as possible,
where implications are eliminated and negations are pushed into the subformulas.
Examples: (a and (not (b implies c))) ,nil --> (a b (not c))
          further examples see at the dual function simpl=disjunctive-parts."))
  (let ((formula-as-list (data~substructs formula)))
    (if (logic~negation-p formula)
	(simpl=conjunctive-parts (second formula-as-list) (not negation))
	(if negation
	    (if (logic~disjunction-p formula)
		(append (simpl=conjunctive-parts (second formula-as-list) negation)
			(simpl=conjunctive-parts (third formula-as-list) negation))
		(if (logic~implication-p formula)
		    (append (simpl=conjunctive-parts (second formula-as-list) (not negation))
			    (simpl=conjunctive-parts (third formula-as-list) negation))
		    (list (data~appl-create (env~lookup-object 'not simpl*env) (list formula)))))
	    (if (logic~conjunction-p formula)
		(append (simpl=conjunctive-parts (second formula-as-list) negation)
			(simpl=conjunctive-parts (third formula-as-list) negation))
		(list formula))))))


(defun simpl=recursion-with-protocol (arglist)
  (declare (edited  "10-FEB-1993 20:21")
	   (authors SCHEJA)
	   (input   "A list of formulas.")
	   (effect  "None.")
	   (value  "The first value is the list of elements in arglist in ACI-form,
the second is a list of protocols of the corresponding recordings,
the third is a flag, whether anything has been simplified:
  t, if nothing has been simplified,
  nil else."))
  (let* ((prot-list ())
	 (no-simplification t)
	 (result (mapcar #'(lambda (x) (let ((help (multiple-value-list (simpl=simplify x nil))))
					 (if (data~equal (first help) x)
					     (setq prot-list (cons
							      (simpl=make-protocol (simpl=make-prot-header x)
										   (simpl=make-prot-body))
							      prot-list))
					     ;; nothing has been simplified.
					     (progn
					       (setq prot-list (cons
								(simpl=make-protocol (simpl=make-prot-header x)
										     (second help))
								prot-list))
					       (setq no-simplification nil)))
					 (first help)))
			 arglist)))
    (values result (nreverse prot-list) no-simplification)))


; ----------------------------------------- Treatment of Disjunctions ----------------------------


(defun simpl=or-treatment (arglist)
  (declare (edited  "11-AUG-1992 14:51")
	   (authors SCHEJA)
	   (input   "A list (x1 ... xn) of formulas, with length greater then two.")
	   (effect  "Arglist is eventually destructed.")
	   (value   "(or y1 ... ym), the internal aci-form of the `ored' input.
If simpl*protocol is on, a protocol is given as a second value."))

  (let ((result arglist))
    (if simpl*protocol-on
	(let ((body ()))
	  (if (some #'simpl=true-p arglist)
	      (multiple-value-setq (result body) (simpl=simple-result (env~lookup-object 'true simpl*env)))
	      (progn
		(let ((sub-prot nil)
		      (no-simplification t))
		  (multiple-value-setq (result sub-prot no-simplification) (simpl=recursion-with-protocol arglist))
		  (setq body  (if  no-simplification
				   (simpl=make-prot-body)
				   (simpl=init-prot-body (simpl=make-recursion-line 'simpl*or sub-prot)))))
				
		(if (some #'simpl=true-p result)
		    (progn
		      (simpl=add-prot-line (simpl=make-prot-line 'simpl*prop (env~lookup-object 'true simpl*env)) body)
		      (setq result (env~lookup-object 'true simpl*env)))
		    (progn
		      (setq result (delete-if #'simpl=false-p  result))
		      (setq result (delete-duplicates result :test #'data~equal ))
		      (cond ((null result)
			     (multiple-value-setq (result body) (simpl=simple-result (env~lookup-object 'false simpl*env))))
			    (t (setq result (sort result #'simpl=formula-less-p ))
			       (setq result (simpl=n-ary-form (env~lookup-object 'or simpl*env)
							      (if (and simpl*absorption-on
								       (> (length result) 1))
								  (simpl=absorption-super! (env~lookup-object 'or simpl*env) result)
								  result)))
			       (simpl=add-prot-line (simpl=make-prot-line 'simpl*prop result)
							       body)
			       result))))))
	  (values result body))
			       
	(if (some #'simpl=true-p arglist)
	    (env~lookup-object 'true simpl*env)
	    (progn
	      (setq result (delete-if  #'simpl=false-p arglist))
	      (setq result (mapcar #'(lambda (x) (simpl=simplify x nil)) arglist))
	      (if (some #'simpl=true-p result)
		  (env~lookup-object 'true simpl*env)
		  (progn
		    (setq result (delete-if #'simpl=false-p  result))
		    (setq result (delete-duplicates result :test #'data~equal ))
		    (cond ((null result) (env~lookup-object 'false simpl*env))
			  (t (simpl=n-ary-form (env~lookup-object 'or simpl*env)
					       (if (and (> (length result) 1) simpl*absorption-on)
						   (simpl=absorption-super! (env~lookup-object 'or simpl*env)
									    (sort result #'simpl=formula-less-p ))
						   (sort result #'simpl=formula-less-p ))))))))))))



(defun simpl=disjunctive-parts (formula negation)
  (declare (edited  "07-AUG-1992 16:40")
	   (authors SCHEJA)
	   (input   "A formula in n normalform and a flag for the parity of surrounding negations.")
	   (effect  "None.")
	   (value   "A list (x1, ... ,xn) of subterms of formula, where
the expression (or x1 (or ... xn)) is equivalent to formula (resp. (not formula))
and the arity of operator is expanded as far as possible, and
where implications are eliminated and negations are pushed into the subformulas.
Examples: (a and b) ,nil --> (a , b)
          (((not (a and b)) or c)),nil --> ((not a) (not b) c))
          (a impl (b or c)), nil  --> ((not a) b c)
          (a and b), true --> ((not a) (not b))." ))
  (let ((formula-as-list (data~substructs formula)))
    (if (logic~negation-p formula)
	(simpl=disjunctive-parts (second formula-as-list) (not negation))
	(if negation
	    (if (logic~conjunction-p formula)
		(append (simpl=disjunctive-parts (second formula-as-list) negation)
			(simpl=disjunctive-parts (third formula-as-list) negation))
		(list (data~appl-create (env~lookup-object 'not simpl*env) (list formula))))
	    (if (logic~disjunction-p formula)
		(append (simpl=disjunctive-parts (second formula-as-list) negation)
			(simpl=disjunctive-parts (third formula-as-list) negation))
		(if (logic~implication-p formula)
		    (append (simpl=disjunctive-parts (second formula-as-list) (not negation))
			    (simpl=disjunctive-parts (third formula-as-list) negation))
		    (list formula)))))))

  
(defun simpl=formula-less-p (formula1 formula2)
  (simpl=term-greater-p formula2 formula1))


;;; -----------------------------------------------------------------------------------------
;;;                                  Absorption
;;; -----------------------------------------------------------------------------------------


(defun simpl=absorption-super! ( operator arglist)
  (declare (edited  " 7-JUL-1992 12:10" )
	   (authors SCHEJA )
	   (input   "An 'and or an 'or, and a list of formulas.")
	   (effect  "Formula is destructed." )
	   (value   "The simplified list of formulas, after absorption-super has
been applied." ))

  (let* ((truthvalue (if (simpl=and-p operator)
			 (env~lookup-object 'true simpl*env)
			 (env~lookup-object 'false simpl*env)))
	 (dual-truthvalue (simpl=dual-truthvalue truthvalue))
	 (result (do ((element (first arglist) (first args1))
		      (args1 (rest arglist) (rest args1))
		      (args2 () (cons element args2)))
		     ((null element) (nreverse args2))
		   (unless (simpl=truthvalue-p element)
		     (let ((neg-positions1 (mapcar #'(lambda (x) (data~positions x #'(lambda (y) (and (type~o-p y)
												      (simpl=negated-equal element y)))))
						   args1))
			   (neg-positions2 (mapcar #'(lambda (x) (data~positions x #'(lambda (y) (and (type~o-p y)
												      (simpl=negated-equal element y)))))
						   args2))
			   ;; negative occurrences of element in args1 resp. args2
	      
			   (pos-positions1 (mapcar #'(lambda (x) (data~positions x #'(lambda (y) (data~equal y element))))
						   args1))
			   (pos-positions2 (mapcar #'(lambda (x) (data~positions x #'(lambda (y) (data~equal y element))))
						   args2))
			   ;; positive occurrences of element in args1 resp. args2
			   )
		       (unless (null args1)
			 (setq args1 (simpl=with-positions-simplify
				      (simpl=with-positions-simplify args1 neg-positions1 dual-truthvalue)
				      pos-positions1 truthvalue)))
		       (unless (null args2)
			 (setq args2  (simpl=with-positions-simplify
				       (simpl=with-positions-simplify args2 neg-positions2 dual-truthvalue)
				       pos-positions2 truthvalue))))))))
    (if (some #'(lambda (x) (data~equal dual-truthvalue x)) result)
	dual-truthvalue
	(delete-if #'(lambda (x) (data~equal truthvalue x)) result))))


(defun simpl=with-positions-simplify (formula-list positions-list truthvalue)
  (declare (edited  "12-FEB-1993 14:44")
	   (authors SCHEJA)
	   (input   "A list of formulas_o, a list of list of positions and a truthvalue, where the lists have the same length.")
	   (effect  "Formulas are destructed.")
	   (value   "In formula-n the subterms at the list-n of posiions are replaced
by truthvalue, the formula is then simplified. The value is the list of simplified formulas."))
  (if (every #'null positions-list)
      formula-list
      ;; nothing to be done.
      (mapcar #'(lambda (formula positions) (simpl=erase-truthvalues-with-positions
					     (do ((help positions (rest help))
						  (result formula (data~replace-at-position result (first help) truthvalue
											    :destructive 't)))
						 ((null help) result))
					     positions))			
	      formula-list positions-list)))


(defun simpl=erase-truthvalues-with-positions (formula positions)
  (declare (edited  "12-FEB-1993 15:10")
	   (authors SCHEJA)
	   (input   "A formula (without 'implies),which quantors are eta--expanded, and a list of positions,
where at each position in formula is a truthvalue.")
	   (effect  "formula is destoyed.")
	   (value   "The simplified formula."))
  (do ((result formula (if (simpl=truthvalue-p (data~struct-at-position result (first pos)))
			   (simpl=erase-one-truthvalue result (first pos))
			   result
			   ;; ie. this truthvalue has been simplified by a former step
			   ;; of this routine.
			   ))
       (pos positions (rest positions)))
      ((null pos) result)))



(defun simpl=erase-one-truthvalue (formula position)
  (declare (edited  "22-DEC-1992 15:44")
	   (authors SCHEJA)
	   (input  "A beta-reduced formula,which quantors are eta--expanded, and a (non empty) position, the subterm
at position being a truthvalue." )
	   (effect "Formula is destroyed.")
	   (value  "The simplified formula."))
  (if (pos~empty-p (pos~butlast position))
      ;; ie. simplification on top-level necessary
      (simpl=simplified-formula formula position)
      (do ((sequence (pos~butlast position) (pos~butlast sequence))
	   (nextposition (pos~last position) (pos~last sequence))
	   (help ())
	   (testterm ()))
	  ((pos~empty-p sequence) formula)
	(setq help (data~struct-at-position formula sequence))
	(setq testterm (data~struct-at-position help nextposition))
	(cond ((data~abstr-p help) ())
	      ((data~appl-p help)
	       (if (or (logic~equivalence-p help)
		       (logic~conjunction-p help)
		       (logic~disjunction-p help))
		   (progn (data~replace-at-position formula sequence (simpl=simplified-formula help nextposition)
						    :destructive 't)
			  (return formula))
		   ;; no further simplification possible
		   (if (logic~negation-p help)
		       (data~replace-at-position formula sequence (simpl=simplified-formula help nextposition)
						 :destructive 't)
		     (error "~a doesn't match the input of this routine." formula))))
	      (t (error "~a, ~a doesn't match the input of this routine." formula position))))))


(defun simpl=simplified-formula (formula position)
  (declare (edited  "23-DEC-1992 15:54")
	   (authors SCHEJA)
	   (input   "A conjunction, disjunction, equivalence or negation,
where the argument denoted by position is a truthvalue.")
	   (effect  "None.")
	   (value   "The simplified formula."))
  (let ((testterm (data~struct-at-position formula position)))
    (cond
      ((logic~negation-p formula)
       (simpl=dual-truthvalue testterm))
      ((or (and (logic~conjunction-p formula) (simpl=false-p testterm))
	   (and (logic~disjunction-p formula) (simpl=true-p testterm)))
       testterm)
      ((or (and (logic~conjunction-p formula) (simpl=true-p testterm))
	   (and (logic~disjunction-p formula) (simpl=false-p testterm)))
       (data~struct-at-position formula (pos~list-position (list (- 3 (pos~first position))))))
      ((logic~equivalence-p formula)
       (if (simpl=true-p testterm)
	   (data~struct-at-position formula (pos~list-position (list (- 3 (pos~first position)))))
	   (simpl=negate  (data~struct-at-position formula (pos~list-position (list (- 3 (pos~first position))))))))
      (t (error "~a doesn't match the input of this routine.")))))


#{\subsection{Protocols}

A protocol has a head and a body. In head, the input is recorded. 
Body may consist of some protocol--lines. If body is nil,
 no simplification was applicable. 
Each line has a rule--name.

Lines of class simpl+normal-line contain just a set of \post--expressions.
Rule--names: simpl*input for a protocol head; simpl*prop for a propositional rule
on this level (ie. a commuting of arguments in a conjunction, an application of
an absorption by a term on a higher level, elimination of implications );
simpl*quantor for a moving of quantors
into the list of arguments (ie. a 'forall may be moved through conjunctions).


Lines of class simpl+quantification-line contain a (quantified) variable and
a \post--expression. Rule--names: simpl*forall (resp. simpl*exists) for the recursive simplification
of the scope of a universal (resp. existential) quantification.

Lines of class simpl+recursion-line contain the protocols of the recursive simplification
of the arguments of the given junktor. Rule--names: simpl*and, simpl*or, simpl*not.

We record only some of the simplification recordings, gaps like absorption, or which simplification rule
has been applied, has to be filled for instance by a propositional tableau, that is
capable to treat easy quantor rule like moving a universal quantor through
conjunctions. The successors to recordings are selfcommenting.
#}

(eval-when (load compile eval)
(defclass simpl+prot ()
  ((header :reader simpl=get-prot-header :initarg :header)
   (body   :reader simpl=get-prot-body   :initform nil :initarg :body )))

(defclass simpl+body ()
  ((line-list :reader simpl=get-prot-lines
	      :initform nil
	      :initarg :line-list :writer simpl=change-body  )))

(defclass simpl+prot-line ()
  ((rule :reader simpl=get-rule-name :initarg :rule)))

(defclass simpl+normal-line (simpl+prot-line)
  ((contense :reader simpl=get-line-contense :initarg :formula)))

(defclass simpl+quantification-line (simpl+prot-line)
  ((variable :reader simpl=get-variable :initarg :variable)
   (protocol  :reader simpl=get-prot :initarg :protocol)))

(defclass simpl+recursion-line (simpl+prot-line)
  ((protocol-list :reader simpl=get-prot-list 
		  :initarg :protocol-list)))
)


(defun simpl=make-protocol (header body)
  (declare (edited  "16-FEB-1993 14:34")
	   (authors SCHEJA)
	   (input   "A protocol header and a protocol-body.")
	   (effect  "None.")
	   (value   "The protocol, consisting of header and body."))

  (make-instance 'simpl+prot :header header :body body))

(defun simpl~get-prot-header (protocol)
  (declare (edited  "11-AUG-1993 16:51")
	   (authors SCHEJA)
	   (input   "A protocol.")
	   (effect "None." )
	   (value  "The header of a protocol." ))
(simpl=get-prot-header protocol))

(defun simpl~get-prot-body (protocol)
  (declare (edited  "11-AUG-1993 16:51")
	   (authors SCHEJA)
	   (input   "A protocol.")
	   (effect "None." )
	   (value  "The body of a protocol." ))
(simpl=get-prot-body protocol))



(defun simpl=make-prot-body ()
  (declare (edited  "16-FEB-1993 14:31")
	   (authors SCHEJA)
	   (input   "Nothing.")
	   (effect  "None.")
	   (Value   "An empty protocol body."))
  (make-instance 'simpl+body ))

(defun simpl~get-prot-lines (body)
  
  (declare (edited  "11-AUG-1993 16:54")
	   (authors SCHEJA)
	   (input   "A protocol body.")
	   (effect  "None.")
	   (value   "A list of all lines of protocol, in natural order."))
  (simpl=get-prot-lines body))

(defun simpl=init-prot-body (line)
  (declare (edited  "16-FEB-1993 14:31")
	   (authors SCHEJA)
	   (input   "A protocol line.")
	   (effect  "None.")
	   (Value   "A new protocol body, initialized with the list '(line) ."))
  (make-instance 'simpl+body :line-list (list line)))


(defun simpl=make-prot-header (formula)
  (declare (edited  "16-FEB-1993 16:22")
	   (authors SCHEJA)
	   (input  "A formula." )
	   (effect  "None.")
	   (value   "The header of a protocol for the simplification of formula."))
  (make-instance 'simpl+normal-line  :rule 'simpl*input :formula  formula))



(defun simpl=make-prot-line (rule argument)
  (declare (edited  "16-FEB-1993 14:37")
	   (authors SCHEJA)
	   (input   "A symbol, that appoints a simplification and a formula." )
	   (effect  "None.")
	   (value   "A protocol-line, consisting of rule and formula." ))
  (make-instance 'simpl+normal-line :rule rule :formula argument))

(defgeneric simpl~get-rule-name (line) 
  (declare (edited  "11-AUG-1993 16:57")
	   (authors SCHEJA)
	   (input   "A protocol line.")
	   (effect  "None.")
	   (value   "The symbol that names the represented rule."))
  (:method ((line simpl+prot-line)) (simpl=get-rule-name line)))

(defun simpl~get-line-contense (line)
  (declare (edited  "11-AUG-1993 17:02")
	   (authors SCHEJA)
	   (input  "A normal protocol line" )
	   (effect "None." )
	   (value  "The post-expression that describes the formula after
 the application of the rule line stands for."))
  (simpl=get-line-contense line))


(defun simpl=make-recursion-line (prot-operator arguments)
  (declare (edited  "16-FEB-1993 16:01")
	   (authors SCHEJA)
	   (input   "A recording rule and a list of recordings.")
	   (effect "None." )
	   (value  "The protocol-line consisting of prot-operator and the recording of the application
of operator on arguments."))
  (make-instance 'simpl+recursion-line :rule prot-operator :protocol-list arguments))

(defgeneric simpl~get-rec-protocols (line)
  (declare (edited  "11-AUG-1993 17:08")
	   (authors SCHEJA)
	   (input  "A recursive protocol line." )
	   (effect "None." )
	   (value  "A list of the recordings of the simplifications of the arguments." ))
  (:method ((line simpl+recursion-line )) (simpl=get-prot-list line)))

(defun simpl=make-quantification-line (rule-quantor variable expression)
  (declare (edited  "16-FEB-1993 17:22")
	   (authors SCHEJA)
	   (input  "A recording quantor, a variable and a post-representation of a formula or the
recording of the simplification of a formula." )
	   (effect "None.")
	   (value  "The protocol-line, consisting of the recording of the quantificaton of the formula
mentioned above."))
  (make-instance 'simpl+quantification-line :rule rule-quantor :variable variable :protocol expression))

(defgeneric simpl~get-rec-protocol (line)
  (declare (edited  "11-AUG-1993 17:08")
	   (authors SCHEJA)
	   (input  "A quantificational protocol line." )
	   (effect "None." )
	   (value  "A recording of the simplifications of the scope." ))
  (:method ((line simpl+quantification-line)) (simpl=get-prot line)))

(defgeneric simpl~get-variable (line)
    (declare (edited  "11-AUG-1993 17:08")
	   (authors SCHEJA)
	   (input  "A quantificational protocol line." )
	   (effect "None." )
	   (value  "The variable, that is quantified over." ))
  (:method ((line simpl+quantification-line)) (simpl=get-variable line)))

(defun simpl=add-prot-line (line body)
  (declare (edited  "16-FEB-1993 14:52")
	   (authors SCHEJA)
	   (input  "A protocol-line, and body.")
	   (effect "None." )
	   (value   "The line is added to the body."))
  (simpl=change-body (reverse (cons line (simpl=get-prot-lines body))) body))


(defun simpl=simple-result (term)
  (declare (edited  "16-FEB-1993 13:57")
	   (authors SCHEJA)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "The first value is term itself. The second is a protocol-body
consisting of a reconrding of simpl*prop and term."))
  (values term
	  (simpl=init-prot-body (simpl=make-prot-line 'simpl*prop term))))

(defmethod print-object ((object simpl+prot) stream)
  (simpl=post-print  object stream 0))

(defgeneric simpl=post-print (object stream indent)
  (:method ((object simpl+prot) stream indent)
	   (let ((line (simpl~get-prot-header object)))
	     (format stream (format nil "~~%~~~a,1@T(" indent))
	     (format stream "~a " (simpl~get-rule-name line))
	     (post~print (simpl~get-line-contense line) stream)
	     (format stream ")"))
	   (simpl=post-print (simpl~get-prot-body object) stream (1+ indent))
	   (format stream ")"))
  (:method ((object simpl+body) stream indent)
	   (mapcar #'(lambda (x) (format stream "~%")
			     (simpl=post-print x stream indent))
		   (simpl~get-prot-lines object))
	   nil)
  (:method ((object simpl+quantification-line) stream indent)
	   (format stream (format nil "~~~a,1@T(~~a ~~a" indent) (simpl~get-rule-name object)
		   (simpl~get-variable object))
	   (simpl=post-print (simpl~get-rec-protocol object) stream (1+ indent))
	   (format stream ")"))
  (:method ((object simpl+recursion-line) stream indent)
	   (format stream (format nil "~~~a,1@T(~~a" indent) (simpl~get-rule-name object))
	   (mapcar #'(lambda (x) (simpl=post-print x stream (1+ indent)))
		   (simpl~get-rec-protocols object))
	   (format stream ")"))
  (:method ((object simpl+normal-line) stream indent)
	   (format stream (format nil "~~~a,1@T(~~a " indent) (simpl~get-rule-name object))
	   (post~print (simpl~get-line-contense object ) stream)
	   (format stream ")"))
  )
  

; -------------     Service Routines      -----------------------------------------------------------------

(defun simpl=dual-quantor (quantor)
  (declare (edited  "26-JUN-1992 16:02" )
	   (authors SCHEJA )
	   (input  "A quantor."  )
	   (effect "None." )
	   (value  "The dual quantor."  ))
  (if (logic~existential-quantor-p quantor)
      (env~lookup-object 'forall simpl*env )
      (env~lookup-object 'exists simpl*env )))

(defun simpl=quantor-p (quantor)
  (declare (edited  "15-JUL-1992 15:43")
	   (authors SCHEJA)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True, iff quantor is a quantor."))
  (or (logic~existential-quantor-p quantor)
      (logic~universal-quantor-p quantor)))

(defun simpl=quantification-p (formula)
  (declare (edited  "08-JAN-1993 16:25")
	   (authors SCHEJA)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True, iff formula is a quantification."))
  (or (logic~existential-quantification-p formula)
      (logic~universal-quantification-p formula)))



(defun simpl=dual-operator (operator)
  (declare (edited  " 2-JUL-1992 12:43" )
	   (authors SCHEJA )
	   (input   "An operator." )
	   (effect  "None." )
	   (value   "Iff operator is 'and then 'or, and viceversa." ))
  (if (simpl=and-p operator)
      (env~lookup-object 'or simpl*env)
      (env~lookup-object 'and simpl*env)))


(defun simpl=and-p (operator)
  (declare (edited  "27-JUN-1992 10:17" )
	   (authors SCHEJA )
	   (input   "A logical operator." )
	   (effect  "None." )
	   (value   "True, iff operator is data~equal to 'and." ))
  (data~equal operator (env~lookup-object 'and simpl*env)))

(defun simpl=or-p (operator)
  (declare (edited  "27-JUN-1992 10:17" )
	   (authors SCHEJA )
	   (input   "A logical operator." )
	   (effect  "None." )
	   (value   "True, iff operator is data~equal to 'or." ))
  (data~equal operator (env~lookup-object 'or simpl*env)))

(defun simpl=implies-p (operator)
  (declare (edited  "27-JUN-1992 10:17" )
	   (authors SCHEJA )
	   (input   "A logical operator." )
	   (effect  "None." )
	   (value   "True, iff operator is data~equal to 'implies." ))
  (data~equal operator (env~lookup-object 'implies simpl*env)))

(defun simpl=equiv-p (operator)
  (declare (edited  "27-JUN-1992 10:17" )
	   (authors SCHEJA )
	   (input   "A logical operator." )
	   (effect  "None." )
	   (value   "True, iff operator is data~equal to 'equiv." ))
  (data~equal operator (env~lookup-object 'equiv simpl*env)))

(defun simpl=not-p (operator)
  (declare (edited  "27-JUN-1992 10:17" )
	   (authors SCHEJA )
	   (input   "A logical operator." )
	   (effect  "None." )
	   (value   "True, iff operator is data~equal to 'not." ))
  (data~equal operator (env~lookup-object 'not simpl*env)))

(defun simpl=truthvalue-p (formula)
  (declare (edited  "27-JUN-1992 11:08" )
	   (authors SCHEJA )
	   (input   "An object." )
	   (effect  "None." )
	   (value   "True, iff formula is a truthvalue." ))
  (or (data~equal formula (env~lookup-object 'true simpl*env)) (data~equal formula (env~lookup-object 'false simpl*env))))

(defun simpl=dual-truthvalue (formula)
  (declare (edited  "27-JUN-1992 11:12" )
	   (authors SCHEJA )
	   (input   "A truthvalue." )
	   (effect  "Error, if formula is no truthvalue." )
	   (value   "The negated truthvalue." ))
  (cond ((data~equal formula (env~lookup-object 'true simpl*env)) (env~lookup-object 'false simpl*env))
	((data~equal formula (env~lookup-object 'false simpl*env)) (env~lookup-object 'true simpl*env))
	(t (error "~a is no truthvalue."))))



(defun simpl=true-p (arg)
  (declare (edited  "30-JUN-1992 13:31" )
	   (authors SCHEJA )
	   (input  "A formula."  )
	   (effect "None." )
	   (value  "True, iff arg is the constant true." ))
  (data~equal (env~lookup-object 'true simpl*env) arg))

(defun simpl=false-p (arg)
  (declare (edited  "30-JUN-1992 13:31" )
	   (authors SCHEJA )
	   (input  "A formula."  )
	   (effect "None." )
	   (value  "True, iff arg is the constant false." ))
  (data~equal (env~lookup-object 'false simpl*env) arg))

(defun simpl=negate (term)
  (declare (edited  "19-JAN-1993 17:17")
	   (authors SCHEJA)
	   (input   "A formula of type o.")
	   (effect  "None.")
	   (value   "The negated formula."))
  (data~appl-create (env~lookup-object 'not simpl*env) (list term)))

(defun simpl=literal-p (formula)
  (declare (edited  "25-JAN-1993 18:06")
	   (authors SCHEJA)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T, iff formula is an atom or negated atom."))
  (or (logic~atom-p formula)
      (when (logic~negation-p formula)
	(logic~atom-p (first (data~appl-arguments formula)))))) 


(defun simpl=n-ary-form (op arguments)
  (declare (edited  "19-JAN-1993 12:40")
	   (authors SCHEJA)
	   (input   "A logical junctor and a non-empty list of terms (a1 .. an).")
	   (effect  "None.")
	   (value   "(op a1 (op a2 ...))."))

  (if (= (length arguments) 1)
      (first arguments)
      (data~appl-create op (list (first arguments)
					(simpl=n-ary-form op (rest arguments))))))

(defun simpl=odd-and-even-sublists (list)
  (declare (edited  "07-OCT-1992 13:35")
	   (authors SCHEJA)
	   (input   "A list of objects, that are not eq.")
	   (effect  "None.")
	   (value   "A List, consisting of one list of all sublists  with odd length and one of all aublists with even length."))
  (let ((odd-list ())
        (even-list ()))
    (do* ((step list (rest step))
          (help-list (mapcar #'list list) (simpl=cons-new-elements step help-list)))
         ((null step) (list odd-list (cons nil even-list)))
      (if (oddp (length (first help-list)))
          (setq odd-list (append odd-list help-list))
          (setq even-list (append even-list help-list))))))
          


(defun simpl=cons-new-elements (elements target-list)
  (declare (edited  "07-OCT-1992 13:40")
	   (authors SCHEJA)
	   (input   "A list of elements, and a list of lists.")
	   (effect  "None.")
	   (value   "Each element of elements is consed at the elements of target-list,until
the first element of an element of target-list is eq to element."))
  (do* ((result ())
	(help-list elements (rest help-list))
	(element (first help-list) (first help-list)))
       ((null help-list) result)
    (do ((run-list target-list (rest run-list)))
	((eq (first (first run-list)) element) ())
      (setq result (append result (list (cons element (first run-list))))))))



(defun simpl=complements-of-sublists (sublists list)
  (declare (edited  "07-OCT-1992 17:58")
	   (authors SCHEJA)
	   (input   "A list of sublists of list and a list.")
	   (effect  "None.")
	   (value   "A list of the complements of sublists in list." ))
  (mapcar #'(lambda (x)
	      (block :do-loop
		(do* ((help-list x (rest help-list))
		    (element (first help-list) (first help-list))
		    (result (if (null help-list)
				(return-from :do-loop list)
				(remove-if #'(lambda (y) (keim~equal element y)) list :count 1))
			    (remove-if #'(lambda (y) (keim~equal element y)) result :count 1)))
		   ((= 1 (length help-list)) result))))
	  sublists))






; Ordnung auf Termen -------------------------------------------------------------------------------

(defun simpl=negated-equal (formula1 formula2)
  (declare (edited  "15-JUL-1992 13:45")
	   (authors SCHEJA)
	   (input   "Two formulas-o.")
	   (effect  "None.")
	   (value   "True, iff formula1 is the aci-form of ('not formula2)."))
  (cond ((logic~atom-p formula1)
	 (or (and (logic~negation-p formula2) (data~top formula2)
		  (data~equal formula1 (first (data~appl-arguments  formula2))))
	     ;;  atom  and (not atom) ?
	     (and (simpl=truthvalue-p formula1) (simpl=truthvalue-p formula2)
		  (data~equal formula1 (simpl=dual-truthvalue formula2)))))
        ((logic~atom-p formula2)
	 (and (logic~negation-p formula1)
	      (data~equal formula2 (first (data~appl-arguments formula1)))))
        (t
	 (let ((operator1 (data~top formula1))
	       (operator2 (data~top formula2))
	       (arglist1 (data~appl-arguments formula1))
	       (arglist2 (data~appl-arguments formula2)))
	   (cond ((or (simpl=and-p operator1)
		      (simpl=or-p operator1))
		  (and (data~equal (simpl=dual-operator operator1) operator2)
		       (every #'simpl=negated-equal arglist1 arglist2)))
		 ((simpl=quantor-p operator1)
		  (and (data~equal (simpl=dual-quantor operator1) operator2)
		       (simpl=negated-equal (logic~quantification-scope formula1)
					    (logic~quantification-scope formula2))))
		 ((and (simpl=equiv-p operator1)
		       (simpl=equiv-p operator2))
		  (and (simpl=negated-equal (first arglist1) (first arglist2))
		       (data~equal (second arglist1) (second arglist2))))
		 (t nil))))))
                  
     

(defun simpl=termlist-greater-p (list1 list2)
  (declare (edited  "24-JUN-1992 11:36" )
	   (authors SCHEJA )
	   (input   "Two lists of terms." )
	   (effect  "None." )
	   (value   "True, iff list1 is greater then list2, corresponding to an internal
order." ))
  (if (= (length list1) (length list2))
      (do ()
	  ((null list1) 'nil)
        (cond ((data~equal (first list1) (first list2))
	       (setq list1 (rest list1))
	       (setq list2 (rest list2)))
	      (t (return (simpl=term-greater-p (first list1) (first list2))))))
      (> (length list1) (length list2))))



(defun simpl=term-greater-p (term1 term2)
  (declare (edited  "24-JUN-1992 11:39" )
	   (authors SCHEJA )
	   (input   "Two terms." )
	   (effect  "None." )
	   (value   "True, iff term1 is greater then term2, corresponding to an internal order." ))
  (let ((test1 (simpl=shallow-term-order term1))
	(test2 (simpl=shallow-term-order term2)))
    (if (= test1 test2)
	(case test1
	  (1 (string-greaterp (string (keim~name term1))
			      (string (keim~name term2))))
	  (2 (if (simpl=term-greater-p (data~appl-function term1) (data~appl-function term2))
		 't
		 (if (data~equal (data~appl-function term1) (data~appl-function term2))
		     (simpl=termlist-greater-p (data~appl-arguments term1) (data~appl-arguments term2))
		     'nil)))
	  (3 (if (string-greaterp (string (keim~name (data~abstr-domain term1)))
				  (String (keim~name (data~abstr-domain term2))))
		 't
	         (if (data~equal (data~abstr-domain term1) (data~abstr-domain term2))
		     (simpl=term-greater-p (data~abstr-range term1) (data~abstr-range term2))
		     'nil))))
	(> test1 test2))))

(defun simpl=shallow-term-order (term)
  (declare (edited  "24-JUN-1992 13:20" )
	   (authors SCHEJA )
	   (input    )
	   (effect   )
	   (value    ))
  (cond ((or (term~variable-p term) (term~constant-p term)) 1)
	((data~appl-p term) 2)
	((data~abstr-p term) 3)))


; --------------------------------------------------------------------------------------
;                        Moving of Quantors
; --------------------------------------------------------------------------------------



(defun simpl=move-quantor-in (quantification)
  (declare (edited  " 6-JUL-1992 16:15" )
	   (authors SCHEJA )
	   (input   "A quantification.")
	   (effect  "None." )
	   (value   "A version of quantification, where the quantor at the head is moved in
as far as possible."))
  (unless (simpl=quantification-p quantification)
    (error "~a should be a quantification." quantification))
  (let ((scope (logic~quantification-scope quantification))
	(variable (logic~quantification-bound-variable quantification))
	(easy-op (if (logic~universal-quantification-p quantification)
		     (env~lookup-object 'and simpl*env)
		     (env~lookup-object 'or simpl*env))))

    (cond ((simpl=literal-p scope)
	   (if (simpl=var-free-p variable scope)
	       quantification
	       scope))
   
	  (t (let ((args (data~appl-arguments scope))
		   (operator (data~top scope))
		   (quantor (data~top quantification)))
	       (cond ((data~equal easy-op operator)
		      (data~appl-create operator (mapcar #'(lambda (x) (simpl=move-quantor-in
									       (logic~quantification-create quantor
													   variable
													   x)))
								args)))
		     ((data~equal operator (simpl=dual-operator easy-op))
		      (do ((arglist (if (data~equal operator (env~lookup-object 'and simpl*env))
					(simpl=conjunctive-parts scope nil)
					(simpl=disjunctive-parts scope nil)) (rest arglist))
			   (free-list ())
			   (notfree-list ()))
			  ((null arglist)
			   (cond ((null free-list) quantification)
				 ((null notfree-list)
				  (logic~quantification-create quantor
							      variable
							      (simpl=n-ary-form operator free-list)))
				 (t (data~appl-create operator
							     (list (simpl=n-ary-form operator notfree-list)
								   (logic~quantification-create quantor variable
											       (simpl=n-ary-form operator free-list)))))))
			(if (simpl=var-free-p variable (first arglist))
			    (setq free-list (cons (first arglist) free-list))
			    (setq notfree-list (cons (first arglist) notfree-list)))))
		     (t (if (simpl=var-free-p variable scope)
			    quantification
			    scope)
			;; kann noch verbessert werden.
			)))))))

		       
(defun simpl=var-free-p (variable formula)
  (declare (edited  " 6-JUL-1992 13:07" )
	   (authors SCHEJA )
	   (input   "A variable and a formula." )
	   (effect  "None." )
	   (value   "True, iff variable is free in formula." ))
  (if (simpl=literal-p formula)
      (member variable (data~free-variables formula) :test #'data~equal)
      (let ((operator (data~top formula)))
	(cond ((or (simpl=and-p operator)
		   (simpl=or-p operator)
		   (simpl=not-p operator)
		   (simpl=equiv-p operator))
	       (some #'(lambda (x) (simpl=var-free-p variable x)) (data~appl-arguments formula)))
	      ((simpl=quantor-p operator)
	       (if (data~equal variable (logic~quantification-bound-variable formula))
		   nil
		   (simpl=var-free-p variable (logic~quantification-scope formula))))
	      (t (error "~a doesn't match the innput of this routine." formula))))))



 














