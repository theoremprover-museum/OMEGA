;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(pushnew :logic-new *features*)

(mod~defmod LOGIC 
            :uses (data keim term th type)
            :documentation "Built-in (fo-) logical connectives."
            :exports (
                      
                      logic~all-bound-variables
                      logic~atom-p
                      logic~conjunction-constant
                      logic~conjunction-p
		      logic~conjunction-connective-p
		      logic~disjunction-connective-p
		      logic~connective-p
		      logic~connective-type
		      logic~connective-type-list
		      logic~depth
                      logic~disjunction-constant
                      logic~disjunction-p
		      logic~dual-quantifier
		      logic~equality-constant
		      logic~equality-connective-p
                      logic~equality-p
                      logic~equivalence-constant
                      logic~equivalence-connective-p
                      logic~equivalence-p
                      logic~existential-quantification-p
                      logic~existential-quantor
                      logic~existential-quantor-p
                      logic~falsity-constant
                      logic~falsity-constant-p
                      logic~fo-application-create
                      logic~fo-application-p
                      logic~fo-arguments
                      logic~fo-arity
                      logic~fo-atom-p
                      logic~fo-constant-create
                      logic~fo-constant-p
                      logic~fo-formula-p
                      logic~fo-function-create
                      logic~fo-function-p
                      logic~fo-predicate-counter
                      logic~fo-predicate-create
                      logic~fo-predicate-p
                      logic~fo-quantification-bound-variable
                      logic~fo-quantification-create
                      logic~fo-quantification-p
                      logic~fo-quantification-scope
                      logic~fo-set-arguments
                      logic~fo-term-p
                      logic~fo-variable-create
                      logic~fo-variable-list-p
                      logic~fo-variable-p
                      logic~implication-constant
                      logic~implication-connective-p
                      logic~implication-p
                      logic~logical-p
                      logic~negate
                      logic~negation-constant
                      logic~negation-connective-p
                      logic~negation-p
		      logic~pl-formula-p
                      logic~quantification-bound-variable
                      logic~quantification-create
                      logic~quantification-scope
                      logic~quantifiers
                      logic~remove-leading-universal-quantors
                      logic~truth-constant
                      logic~truth-constant-p
                      logic~universal-quantification-p
                      logic~universal-quantor
                      logic~universal-quantor-p
                      
                      logic*constant-counter
                      logic*fo-constant-counter
                      logic*fo-function-counter
                      logic*fo-predicate-counter
                      logic*fo-variable-counter
		      logic*current-theory
		      ))



(defvar logic*current-theory nil)

;  termc~compound-term-p          --> data~complex-p
;  termc~quantification-create    --> logic~quantification-create
;  termc~quantification-bound-variable  --> logic~quantification-bound-variable
;  termc~quantification-scope     --> logic~quantification-scope
;  termc~universal-quantor-p      --> logic~universal-quantor-p

; usw.
		      
;                      termc~existential-quantor-p
;                      termc~universal-quantification-p
;                      termc~existential-quantification-p
;                      termc~remove-leading-universal-quantors
;                      termc~read-atom
;                      termc~atom-p
;                      termc~negation-p
;                      termc~disjunction-p
;                      termc~conjunction-p
;                      termc~equivalence-p
;                      termc~equality-p
;                      termc~implication-p
;                      termc~all-bound-variables
;                      termc~depth
;                      termc~negate

; BIS AUF

; termc~ground-p       --> data~ground-p



;;
;;(eval-when (compile load eval)
;;  (if (find-package "OMEGA")	
;;      (use-package (cons (find-package "KEIM")
;;			 (package-use-list (find-package "KEIM")))
;;		   (find-package "OMEGA"))
;;    (make-package "OMEGA" 
;;		  :use (cons (find-package "KEIM")
;;			     (package-use-list (find-package "KEIM")))))
;;)
;;



#{\section{Compound Terms}\label{mod:termc}
In this section the basic functionality for manipulating built in logical connectives is provided.
Quantors are general, i.e. they consist of an abstraction together with several additional arguments.
However, the abstraction has to be the very first argument of a quantifier.
#}

(defun logic~connective-type-list ()
  '("UNIVERSAL-QUANTOR" "EXISTENTIAL-QUANTOR" "NEGATION" "TRUTH" "FALSITY"
    "DISJUNCTION" "CONJUNCTION" "EQUIVALENCE" "IMPLICATION" "EQUALITY"))

(defun logic~connective-p (term &key (theory logic*current-theory))
  (some #'(lambda (type) (logic=term-is-connective-p term type :theory theory)) (logic~connective-type-list)))

(defun logic~connective-type (term &key (theory logic*current-theory))
  (some #'(lambda (type) (when (logic=term-is-connective-p term type :theory theory) type)) (logic~connective-type-list)))

(defun logic=term-is-connective-p (term type &key (theory logic*current-theory))
  (declare (edited  "13-AUG-1998")
	   (authors Ameier)
	   (input   "A term, a type and as keyword the theory (default: logic*current-theory)")
	   (effect  "None.")
	   (value   "T if term is a constant and is in the theory a connective of type type."))
  (if (term~constant-p term)
      (let* ((connectives (th~connectives theory))
	     (term-pair (find term connectives :test #'(lambda (term pair)
							 (cond ((or (and (term~schema-p term)
									 (term~schema-p (first pair)))
								    (and (null (term~schema-p term))
									 (null (term~schema-p (first pair)))))
								(keim~equal term (first pair)))
							       (t
								(let* ((term-domain (if (term~schema-p term)
											(data~schema-range term)
										      term))
								       (pair-domain (if (term~schema-p (first pair))
											(data~schema-range (first pair))
										      (first pair))))
								  (keim~equal term-domain pair-domain))))))))
	(and term-pair  ;; is ueberhaupt connective
	     (string= type (second term-pair))))
    nil))

(defun logic=convert2string (name)
  (if (stringp name)
      name
    (string name)))

(sys~define-condition logic+no-connective (sys+error)
    ((theory nil) (type nil) (name nil))
    (lambda (condition stream)
      (format stream "In theory ~A a connective of type ~A and name ~A is searched, but not found."
	      (logic+no-connective-theory condition)
	      (logic+no-connective-type condition)
	      (logic+no-connective-name condition))))

(sys~define-condition logic+several-connectives (sys+error)
    ((theory nil) (type nil))
    (lambda (condition stream)
      (format stream "In theory ~A a connective of type ~A is searched, but there exists more than one and no name was specified."
	      (logic+several-connectives-theory condition)
	      (logic+several-connectives-type condition))))

(defun logic=connective-of-type-and-name (type &key (theory logic*current-theory) (name nil))
  (declare (edited  "13-AUG-1998")
	   (authors Ameier)
	   (input   "The type (a string!) of connective, as keywords the theory and the name of the connective.")
	   (effect  "None.")
	   (value   "The original (i.e. polymorphic) connective of the theory if ONE exists."
		    "Exists none nil is returned. Exists more than one the one with the name is returned if"
		    "name is not nil."))
  (let* ((connectives (th~connectives theory))
	 (connectives-of-type (mapcar #'first (remove-if-not #'(lambda (con-pair)
								 (string= (second con-pair) type))
							     connectives)))
	 (string-name (if (null name)
			  nil
			(logic=convert2string name))))
    (cond ((= (length connectives-of-type) 1)
	   (let* ((connectiv-of-type (first connectives-of-type))
		  (connectiv-name (if (data~schema-p connectiv-of-type)
				      (keim~name (data~schema-range connectiv-of-type))
				    (keim~name connectiv-of-type))))
	     (if (or (null name)
		     (string= string-name (logic=convert2string connectiv-name)))
		 connectiv-of-type
	       (error (sys~make-condition 'logic+no-connective
					       :theory theory
					       :type type
					       :name name)))))
	  ((= (length connectives-of-type) 0)
	   nil)
	  (t ;; mehr als einer
	   (if (not (null name))
	       (let* ((connectiv-of-type (find string-name connectives-of-type
					       :test #'(lambda (it1 it2)
							 (let* ((name2 (keim~name
									(if (data~schema-p it2)
									    (data~schema-range it2)))))
							   (string= it1 (logic=convert2string name2)))))))
		 (if connectiv-of-type
		     connectiv-of-type
		   (error (sys~make-condition 'logic+no-connective
						   :theory theory
						   :type type
						   :name name))))
	     (error (sys~make-condition 'logic+several-connectives
					     :theory theory
					     :type type)))))))


#{\subsection{Quantifications and semantically-predefined symbols} #}


;;(eval-when (load compile eval)
;;  (defconstant logic*env (th~env (th~require-only 'base)))
;;  (defconstant logic*quantifiers (list (env~lookup-object 'forall logic*env)
;;				       (env~lookup-object 'exists logic*env)))
;;  )

;;(proclaim '(special logic*env logic*quantifiers))

(defun logic~quantifiers (&key (theory logic*current-theory))
  (declare (edited  "12-AUG-1998")
	   (authors Ameier)
	   (input   "As key a theory (default: logic*current-theory).")
	   (effect  "None.")
	   (value   "All quantififers of the theory."))
  (let* ((connectives (th~connectives theory)))
    (mapcar #'first (remove-if-not #'(lambda (con-pair)
				       (or (string= (second con-pair) "UNIVERSAL-QUANTOR")
					   (string= (second con-pair) "EXISTENTIAL-QUANTOR")))
				   connectives))))

(defun logic~quantification-create (quantor bound-var scope &optional (add-args nil))
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer KOHLHASE Sorge)
	   (input   "A quantor, a variable, amd a term {\vb SCOPE} of type o. Maybe some additional arguments"
		    "if the qunator has additional conditions.")
	   (effect  "If {\vb QUANTOR} has a polymorphic type the type variables can be instantiated"
		    "so that the the shared symbol will be changed.")
	   (value   "If {\vb SCOPE} is of type o, {\vb BOUND-VAR} of type A and quantor of type ((A -> o) -> o)"
		    "Then the term (quantor [bound-var] scope)."
		    "Else Error.")
	   (example "Let FORALL be of type ((A) -> o) -> o"
		    "FORALL     X     (P X)  --> FORALL [X].(P X)"))
  (if (type~o-p (term~type scope))
      (if (consp bound-var)
	  (term~appl-create quantor (cons (term~abstr-create bound-var scope) add-args))
	(term~appl-create quantor (cons (term~abstr-create (list bound-var) scope) add-args)))
      (error "~A is not a formula of type O, that can be quantified over"
	     scope)))


(defun logic~quantification-bound-variable (term)
  (declare (edited  "12-FEB-1998") 
	   (authors Fehrer KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "If TERM is a quantification, then the bound variable, else error.")
	   (example "(FORALL [X].(P X))  -->  X"))
  (if (and (term~appl-p term)
	   (type~o-p (term~type term)))
      (let* ((arguments (data~appl-arguments term))
	     (farg (car arguments)))
	(if (and ;;;(= (length arguments) 1)   Generalization of quantifiers...  VS + AM 
		 (term~abstr-p farg)
		 (type~o-p (term~type (data~abstr-range farg))))
	    (data~abstr-bound-var (car (data~appl-arguments term)))
	  (error "term ~A is not a quantification" term)))
    (error "term ~A is not a quantification" term)))

(defun logic~quantification-scope (term)
  (declare (edited  "12-FEB-1998") 
	   (authors Fehrer KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "If TERM is a quantification, then the scope, else error.")
	   (example "(FORALL [X].(P X))  -->  (P X)"))
  (if (and (term~appl-p term)
	   (type~o-p (term~type term)))
      (let* ((arguments (data~appl-arguments term))
	     (farg (car arguments)))
	(if (and ;;;(= (length arguments) 1)   Generalization of quantifiers...  VS + AM + HG
		 (term~abstr-p farg)
		 (type~o-p (term~type (data~abstr-range farg))))
	    (data~abstr-scope (car (data~appl-arguments term)))
	  (error "term ~A is not a quantification 1" term)))
    (error "term ~A is not a quantification 2" term)))


(defun logic~universal-quantor-p (term &key (theory logic*current-theory))
  (declare (edited  "12-FEB-1998") 
	   (authors AMEIER Fehrer KOHLHASE )
	   (input   "A term and a (optional) theory.")
	   (effect  "None." )
	   (value   "True, if TERM is an universal quantor in the theory.")
	   (example "FORALL BASE--> T"
		    "EXISTS BASE --> NIL"
		    "X --> NIL"))
  (logic=term-is-connective-p term "UNIVERSAL-QUANTOR" :theory theory))


(defun logic~remove-leading-universal-quantors (struct &key vars (theory logic*current-theory))
  (declare (edited  "05-DEC-1997")
	   (authors Konrad)
	   (input   "a formula")
	   (effect  "none")
	   (value   "returns the formula with leading all-quantors removed."))
  (if (logic~universal-quantification-p struct :theory theory)
      (let ((qscope (logic~quantification-scope struct)))
        (logic~remove-leading-universal-quantors qscope
                                                 :vars (cons
                                                        (logic~quantification-bound-variable struct)
							vars) :theory theory))
    ;; else
    (values struct (nreverse vars))))

(defun logic~existential-quantor-p (term &key (theory logic*current-theory))
  (declare (edited  "16-JUN-1992 16:15" ) 
	   (authors AMEIER KOHLHASE )
	   (input   "A term and as key the theory (default: logic*current-theory)." )
	   (effect  "None." )
	   (value   "True, if TERM is an existential quantor in the theory.")
	   (example "EXISTS BASE --> T"
		    "FORALL BASE --> NIL"
		    "X      --> NIL"))
  (logic=term-is-connective-p term "EXISTENTIAL-QUANTOR" :theory theory))

(defun logic=convert2string (name)
  (if (stringp name)
      name
    (string name)))

(defun logic~existential-quantor (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors Fehrer AMEIER)
	   (input   "As keys the theory (default logic*current-theory) and an name (default nil).")
	   (effect  "none")
	   (value   "the original (i.e. polymorphic) existential quantor of the theory if ONE exists."
		    "Exists none nil is returned. Exists more than one the one with the name is returned if"
		    "name is not nil."))
  (logic=connective-of-type-and-name "EXISTENTIAL-QUANTOR" :theory theory :name name))

(defun logic~universal-quantor (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors Fehrer AMEIER)
	   (input   "As keys the theory (default logic*current-theory) and an name (default nil).")
	   (effect  "none")
	   (value   "the original (i.e. polymorphic) universal quantor of the theory if ONE exists."
		    "Exists none nil is returned. Exists more than one the one with the name is returned if"
		    "name is not nil."))
  (logic=connective-of-type-and-name "UNIVERSAL-QUANTOR" :theory theory :name name))

(defun logic~universal-quantification-p (term &key (theory logic*current-theory))
  (logic=universal-quantification-p term theory))

(data~defgeneric logic=universal-quantification-p ((term) theory)
  (declare (edited  "12-FEB-1998") 
	   (authors AMEIER Fehrer KOHLHASE )
	   (input   "A term and as key a theory." )
	   (effect  "None." )
	   (value   "True, if TERM is a universal quantification, i.e. a term of the form (forall [X] A).")
	   (example "(FORALL [X].(P X)) --> T"
		    "([X].[Y].(G X Y) X D) --> NIL"))
  (:method ((term term+term) theory)
	   nil)
  (:method ((term term+appl) theory)
	   (and (logic~universal-quantor-p (data~appl-function term) :theory theory)
		(data~abstr-p (car (data~appl-arguments term))))))

(defun logic~existential-quantification-p (term &key (theory logic*current-theory))
  (logic=existential-quantification-p term theory))

(data~defgeneric logic=existential-quantification-p ((term) theory)
  (declare (edited  "12-FEB-1998") 
	   (authors AMEIER Fehrer KOHLHASE )
	   (input   "A term and as key a theory." )
	   (effect  "None." )
	   (value   "True, if TERM is a existential quantification, i.e. a term of the"
		    "form (exists [X] A).")
	   (example "(EXISTS [X].(P X)) --> T"
		    "([X].[Y].(G X Y) X D) --> NIL"))
  (:method ((term term+term) theory)
	   nil)
  (:method ((term term+appl) theory)
	   (and (logic~existential-quantor-p (data~appl-function term) :theory theory)
		(data~abstr-p (car (data~appl-arguments term))))))

(defun logic~atom-p (term &key (theory logic*current-theory))
  (logic=atom-p term theory))

(data~defgeneric logic=atom-p ((term) theory)
  (declare (edited  "12-FEB-1998")
	   (authors AMEIER Fehrer nesmith )
	   (input   "A term and as key a theory.")
	   (effect  "None.")
	   (value   "True if {\vb TERM} is an atom, else NIL. For our purposes, an"
		    "atom is what you think of in the first-order sense of atom, i.e., no lambda"
		    "bindings, no quantifiers, no connectives.  So an atom is a term with type O,"
		    "such that either it is a symbol, or it is an application whose function is"
		    "a predicate and is not a quantification, negation, conjunction, disjunction,"
		    "implication or equivalence.")
	   (example "(P X Y) --> T"
		    "(F X)   --> NIL"
		    "([X].[Y].(G X Y) X D) --> NIL"
		    "(EXISTS [X].(Q X))  --> NIL"
		    "(CON (EXISTS [X].(Q X))) --> T"))
  (:method ((term term+primitive) theory)
      (type~o-p (term~type term)))
  (:method ((term term+abstr) theory)
      nil)
  (:method ((term term+appl) theory)
      (and (type~o-p (term~type term))
	   (not (logic~universal-quantification-p term :theory theory))
	   (not (logic~existential-quantification-p term :theory theory))
	   (not (logic~negation-p term :theory theory))
	   (not (logic~disjunction-p term :theory theory))
	   (not (logic~conjunction-p term :theory theory))
	   (not (logic~equivalence-p term :theory theory))
	   (not (logic~implication-p term :theory theory)))))

(defun logic~negation-p (term &key (theory logic*current-theory))
  (logic=negation-p term theory))

(data~defgeneric logic=negation-p ((term) theory)
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is a negation.")
	   (example "(NOT (EXISTS [X].(Q X))) --> T"))
  (:method ((term term+term) theory)
	   nil)
  (:method ((term term+appl) theory)
      (logic=term-is-connective-p (data~appl-function term) "NEGATION" :theory theory)))

(defun logic~truth-constant-p (obj &key (theory logic*current-theory))
  (declare (edited  "03-MAR-1998")
	   (authors CHRIS)
	   (input   "An object")
	   (effect  "none")
	   (value   "t, iff obj is truth constant."))
  (logic=term-is-connective-p obj "TRUTH" :theory theory))

(defun logic~falsity-constant-p (obj &key (theory logic*current-theory))
  (declare (edited  "03-MAR-1998")
	   (authors CHRIS)
	   (input   "An object")
	   (effect  "none")
	   (value   "t, iff obj is falsity constant."))
    (logic=term-is-connective-p obj "FALSITY" :theory theory))

(defun logic~disjunction-p (term &key (theory logic*current-theory))
  (logic=disjunction-p term theory))

(data~defgeneric logic=disjunction-p ((term) theory)
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is a disjunction.")
	   (example "(OR (Q A) (NOT (Q A)))  --> T"
		    "(AND (Q A) (NOT (Q A)))) --> NIL"))
  (:method ((term term+term) theory)
	   nil)
  (:method ((term term+appl) theory)
      (logic=term-is-connective-p (data~appl-function term) "DISJUNCTION" :theory theory)))

(defun logic~conjunction-p (term &key (theory logic*current-theory))
  (logic=conjunction-p term theory))

(data~defgeneric logic=conjunction-p ((term) theory)
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is a conjunction.")
	   (example "(AND (Q A) (NOT (Q A)))  --> T"
		    "(OR (Q A) (NOT (Q A)))) --> NIL"))
  (:method ((term term+term) theory)
	   nil)
  (:method ((term term+appl) theory)
      (logic=term-is-connective-p (data~appl-function term) "CONJUNCTION" :theory theory)))

(defun logic~equivalence-p (term &key (theory logic*current-theory))
  (logic=equivalence-p term theory))

(data~defgeneric logic=equivalence-p ((term) theory)
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is an equivalence.")
	   (example "(EQUIV (Q A) (NOT (Q A)))  --> T"
		    "(OR (Q A) (NOT (Q A)))) --> NIL"))
  (:method ((term term+term) theory)
	   nil)
  (:method ((term term+appl) theory)
	   (logic=term-is-connective-p (data~appl-function term) "EQUIVALENCE" :theory theory)))

(defun logic~implication-p (term &key (theory logic*current-theory))
  (logic=implication-p term theory))

(data~defgeneric logic=implication-p ((term) theory)
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is an implication.")
	   (example "(IMPLIES (Q A) (NOT (Q A)))  --> T"
		    "(OR (Q A) (NOT (Q A)))) --> NIL"))
  (:method ((term term+term) theory)
	   nil)
  (:method ((term term+appl) theory)
      (logic=term-is-connective-p (data~appl-function term) "IMPLICATION" :theory theory)))


(defun logic~equality-p (term &key (theory logic*current-theory))
  (logic=equality-p term theory))


(data~defgeneric logic=equality-p ((term) theory)
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer KK AMEIER)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is an equation.")
	   (example "(= A A)  --> T"))
  (:method ((term term+term) theory)
	   nil)
  (:method ((term term+appl) theory)
	   (logic=term-is-connective-p (data~appl-function term) "EQUALITY" :theory theory)))




(defun logic~negation-constant (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors AMEIER Fehrer)
	   (input   "none")
	   (effect  "none")
	   (value   "the negation constant"))
  (logic=connective-of-type-and-name "NEGATION" :theory theory :name name))


(defun logic~truth-constant (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors AMEIER CHRIS)
	   (input   "none")
	   (effect  "none")
	   (value   "the truth"))
  (logic=connective-of-type-and-name "TRUTH" :theory theory :name name))

(defun logic~falsity-constant (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors AMEIER Fehrer)
	   (input   "none")
	   (effect  "none")	   (value   "the falsum"))
  (logic=connective-of-type-and-name "FALSITY" :theory theory :name name))

(defun logic~disjunction-constant (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors AMEIER Fehrer)
	   (input   "none")
	   (effect  "none")
	   (value   "the original disjunction constant"))
  (logic=connective-of-type-and-name "DISJUNCTION" :theory theory :name name))


(defun logic~conjunction-constant (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors AMEIER Fehrer)
	   (input   "none")
	   (effect  "none")
	   (value   "the original conjunction constant"))
  (logic=connective-of-type-and-name "CONJUNCTION" :theory theory :name name))

(defun logic~equivalence-constant (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors AMEIER Fehrer)
	   (input   "none")
	   (effect  "none")
	   (value   "the original equivalence constant"))
  (logic=connective-of-type-and-name "EQUIVALENCE" :theory theory :name name))


(defun logic~implication-constant (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors AMEIER Fehrer)
	   (input   "none")
	   (effect  "none")
	   (value   "the original implication constant"))
  (logic=connective-of-type-and-name "IMPLICATION" :theory theory :name name))

(defun logic~equality-constant (&key (theory logic*current-theory) (name nil))
  (declare (edited  "03-MAR-1998")
	   (authors AMEIER Fehrer)
	   (input   "none")
	   (effect  "none")
	   (value   "the original (i.e. polymorphic) equality constant"))
  (logic=connective-of-type-and-name "EQUALITY" :theory theory :name name))


(defun logic~negation-connective-p (object &key (theory logic*current-theory))
  (declare (edited  "21-DEC-1998")
	   (authors Lassaad)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "true, iff {\vb OBJECT} connects two terms to a conjunction."))
  (logic=term-is-connective-p object "NEGATION" :theory theory))

(defun logic~conjunction-connective-p (object &key (theory logic*current-theory))
  (declare (edited  "21-DEC-1998")
	   (authors Lassaad)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "true, iff {\vb OBJECT} connects two terms to a conjunction."))
  (logic=term-is-connective-p object "CONJUNCTION" :theory theory))

(defun logic~disjunction-connective-p (object &key (theory logic*current-theory))
  (declare (edited  "21-DEC-1998")
	   (authors Lassaad)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "true, iff {\vb OBJECT} connects two terms to a conjunction."))
  (logic=term-is-connective-p object "DISJUNCTION" :theory theory))

(defun logic~implication-connective-p (object &key (theory logic*current-theory))
  (declare (edited  "21-DEC-1998")
	   (authors Lassaad)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "true, iff {\vb OBJECT} connects two terms to a conjunction."))
  (logic=term-is-connective-p object "IMPLICATION" :theory theory))

(defun logic~equivalence-connective-p (object &key (theory logic*current-theory))
  (declare (edited  "21-DEC-1998")
	   (authors Lassaad)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "true, iff {\vb OBJECT} connects two terms to a conjunction."))
  (logic=term-is-connective-p object "EQUIVALENCE" :theory theory))

(defun logic~equality-connective-p (object &key (theory logic*current-theory))
  (declare (edited  "21-DEC-1998")
	   (authors Lassaad)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "true, iff {\vb OBJECT} connects two terms to a conjunction."))
  (logic=term-is-connective-p object "EQUALITY" :theory theory))




#{\subsection{Simple operations on terms} #}


(data~defgeneric logic~all-bound-variables ((term))
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer richts)
	   (input   "A term or termlist.")
	   (effect  "None.")
	   (value   "The list of all variables bound by any abstraction inside of {\vb TERM}.")
	   (example "(FORALL [A].(FORALL [B].(EQUIV (P B) (FORALL [X].(IMPLIES (A X) (B X)))))) --> (A B X)"))
  (:method ((termlist list))
	   (mapcan #'logic~all-bound-variables termlist))
  (:method ((variable term+variable))
	   nil)
  (:method ((constant term+constant))
	   nil)
  (:method ((application term+appl))
	   (mapcan #'logic~all-bound-variables (data~substructs application)))
  (:method ((abstraction term+abstr))
	   (append (data~abstr-domain abstraction) 
		   (logic~all-bound-variables (data~abstr-scope abstraction))))
  (:method ((schema term+schema))
	   (append (data~schema-domain schema)
		   (logic~all-bound-variables (data~schema-range schema)))))


(defun logic~depth (term)
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer richts)
	   (input   "A term or termlist.")
	   (effect  "None.")
	   (value   "The depth of TERM.")
	   (example "x --> 1"
		    "(G X Y) --> 2"
		    "(P (F C) C) --> 3"
		    "([X].(Q X) C) --> 4"
		    "(FORALL [X].(PPP (F X) X)) --> 5"))
  (if (listp term)
      (apply #'max (mapcar #'logic~depth term))
      (1+ (apply #'max 0 (mapcar #'logic~depth (data~substructs term))))))



(defun logic~negate (formula &key (theory logic*current-theory) (name nil))
  (declare (edited  "12-FEB-1998")
	   (authors Fehrer RICHTS)
	   (input   "A formula and as keyword the theory (default: logic*current-theory).")
	   (effect  "None.")
	   (value   "The negated formula, i.e. if FORMULA starts with a NOT the argument"
		    "of this application; else an application of the symbol NOT found in"
		    "ENV and FORMULA"))
  (if (logic~negation-p formula :theory theory)
      (first (data~appl-arguments formula))
    (term~appl-create (logic~negation-constant :theory theory :name name)
		      (list formula))))





;; ==================================================================================
;; 
;;             U E B E R B L E I B S E L    V O M     F O - M O D  U L
;;
;; ==================================================================================

#{
\section{Some special function as interface for first order terms}\label{mod:fo}
\subsection{Introduction}

In order to facilitate the use of \keim\ for users who only want to
use first order predicate logic, \keim\ features a special interface.
The functions in the module {\vb fo} are abbreviations for functions
in {\vb term} that screen out the use of types (first order terms
always have type I).  {\vb fo} does not support all \keim\
functionality for that the {\vb term} functions can be used.


%\begin{figure}
%\input{fo-fig}
%\end{figure}
#}


#{The special instances are present in the names of the interface
functions, and therefore only a mnemonic aid for the programmer.
They include:

\begin{tabular}{lp{11.9cm}}
{\vb VARIABLE}        &  {\vb TERM+VARIABLE}s of type $\iota$\\
{\vb FUNCTION}        &  {\vb TERM+CONSTANT}s of type $\iota \ldots \iota \rightarrow \iota$\\
{\vb CONSTANT}        &  {\vb TERM+CONSTANT}s of type $\iota$\\
{\vb PREDICATE}       &  {\vb TERM+CONSTANT}s of type $\iota \ldots \iota \rightarrow o$\\
{\vb APPLICATION}     &  The same as {\vb TERM+APPL}\\
{\vb QUANTIFICATION}  &  {\vb APPL}ications with a unitary topfunction and an {\vb TERM+AB\-ST}raction as argument
                  $(Q [x].t)$, where $Q \in  \{ALL, EX, \ldots \}$
\end{tabular}
#}

#{First order terms are not defined to be \clos\ subclasses of the {\vb
term}-classes, even though conceptually the set of first-order
\keim\-terms is a subset of the set of \keim-terms. Otherwise it would
not be possible to use terms of the two classes mixedly. The property
of a term to be first order can only be checked by a complex parsing
process and can change over the lifespan of a term (e.g. by
substitution). Thus a classifier would constantly have to check
whether the terms that were created as \keim-terms are now first-order
\keim-terms and accordingly change the class of the term.
#}


#{\subsection{Variables}
The following procedures manipulating variables are prefixed by logic~fo-variable-.
#}


(defvar logic*fo-variable-counter 0)
;A counter for uniquely numerating new variables. Increased by 1 each
;time, when fo~variable-create is called.

(defun logic~fo-variable-create (&optional name)
  (declare (edited  "01-APR-1998" "20-JAN-1998" "25-JUL-1991 13:03")
	   (authors Gebhard Fehrer huang RICHTS)
	   (input   "name: a symbol.")
	   (effect  "None.")
	   (value   "The new variable (if NAME is nil its name will be \"x-<number>\").")
	   (example "(fo~variable-create) --> x-2"
		    "(fo~variable-create) --> x-3"
		    "(term~type (fo~variable-create)) --> i"))
  (apply #'term~variable-create
	 (list (if name name (intern (format nil "x-~A" (incf logic*fo-variable-counter))))
	       (type~i))))

(defun logic~fo-variable-p (object)
  (declare (edited  "01-APR-1998" "25-JUL-1991 13:13")
	   (authors Gebhard RICHTS )
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a first order variable.")
	   (example "x --> t"
		    "c --> nil"))
  (and (term~variable-p object) (type~i-p (term~type object))))


(defun logic~fo-variable-list-p (termlist)
  (declare (edited  "01-APR-1998" "08-AUG-1991 13:30")
	   (authors Gebhard RICHTS)
	   (input   "A list of terms.")
	   (effect  "None.")
	   (value   "True iff TERMLIST is a non-empty list of first order variables.")
	   (example "(x y) --> t"
		    "(x c) --> nil"))
  (and termlist
       (every 'logic~fo-variable-p termlist))) 


#{\subsection{Constants}
The following procedures manipulating constants are prefixed by logic~fo-constant-.
#}

(defvar logic*constant-counter 0)
;A counter for uniquely numerating new constant symbols. Increased by 1 each
;time, when fo~constant-create is called.

(defun logic~fo-constant-create (&optional name)
  (declare (edited  "01-APR-1998" "20-JAN-1998" "25-JUL-1991 13:03")
	   (authors Gebhard Fehrer huang RICHTS)
	   (input   "name: a symbol"
		    "and a reference term")
	   (effect  "None.")
	   (value   "The new constant (if name is nil its name will be \"c-<number>\").")
	   (example "(fo~constant-create 'c)--> C"
		    "(fo~constant-create) --> c-3"
		    "(term~type (fo~constant-create)) --> i"))
  (apply #'term~constant-create
	 (list (if name name (intern (format nil "c-~A" (incf logic*fo-constant-counter))))
	       (type~i))))

(defun logic~fo-constant-p (object)
  (declare (edited  "01-APR-1998" "20-JAN-1998" "02-AUG-1991 21:23")
	   (authors Gebhard Fehrer RICHTS)
	   (input   "A object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a first order constant.")
	   (example "c --> t"
		    "f --> nil"
		    "x --> nil"))
  (and (term~constant-p object) (type~i-p (term~type object))))


#{\subsection{Functions}
The following procedures manipulating functions are prefixed by logic~fo-function-.
#}

(defvar logic*fo-function-counter 0)
;A counter for uniquely numerating new function symbols. Increased by 1 each
;time, when fo~function-create is called.


(defun logic~fo-function-create (arity &optional name &rest keyword-pairs);???
  (declare (edited  "01-APR-1998" "25-JUL-1991 13:03")
	   (authors Gebhard RICHTS)
	   (input   "A type(????), something that can be coerced into a string, a value and a sort.")
	   (effect  "None.")
	   (value   "The new function  (if NAME is nil its name will be \"f-<number>\")."))
  (apply #'term~constant-create
	 (list (if name (string name) (format nil "f-~A" (incf logic*fo-function-counter)))
	       (type~function-create arity))))

(defun logic~fo-function-p (object)
  (declare (edited  "01-APR-1998" "08-AUG-1991 13:38")
	   (authors Gebhard RICHTS)
	   (input   "A object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a first order function.")
	   (example "f --> t"
		    "c --> nil"))
  (and (term~constant-p object)
       (type~i-p (type~n-range (term~type object)))
       (every 'type~i-p (type~n-domain (term~type object)))))

(defun logic~fo-arity (term)
  (declare (edited  "01-APR-1998" "02-AUG-1991 20:59")
	   (authors Gebhard RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "The arity of the term (computed from its type).")
	   (example "Let f be a binary function symbol and P a binary predicate symbol"
		    "f --> 2"
		    "P --> 2"
		    "(P a b) --> 0"))
  (length (type~n-domain (term~type term))))


#{\subsection{Predicates}
The following procedures manipulating predicate symbols are prefixed by logic~fo-predicate-.
#}

(defvar logic~fo-predicate-counter 0)

(defun logic~fo-predicate-create (arity &optional name &rest keyword-pairs);???
  (declare (edited  "20-JAN-1998" "25-JUL-1991 13:03")
           (authors Fehrer RICHTS)
           (input   "A number, something that can be coerced into a string, a value and a sort.")
           (effect  "None.")
           (value   "The new predicate (if name is nil its name will be \"P-<number>\").")
           (example "???"))
  (apply #'term~constant-create
         (list (if name (string name) (format nil "P-~A" (incf logic*fo-predicate-counter)))
	       (type~predicate-create arity))))

(defun logic~fo-predicate-p (object)
  (declare (edited  "01-APR-1998" "20-JAN-1998" "08-AUG-1991 13:38")
	   (authors Gebhard Fehrer RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a first order predicate.")
	   (example "P --> t"
		    "f --> nil"))
  (and (term~constant-p object)
       (type~o-p (type~n-range (term~type object)))
       (every 'type~i-p (type~n-domain (term~type object)))))


#{\subsection{Applications}
The following procedures manipulating application are prefixed by logic~fo-application-.
#}

(defun logic~fo-application-create (function arguments &rest keyword-pairs)
  (declare (edited  "01-APR-1998" "15-AUG-1991 13:41")
	   (authors Gebhard RICHTS)
	   (input   "A term with arity n and a list of terms with length = n.")
	   (effect  "None.")
	   (value   "The term (function . arguments).")
	   (example "f       (c1 c2) --> (f c1 c2)"))
  (if (and (logic~fo-function-p function) ;; Neu H.G.
	   (= (length (type~n-domain (term~type function))) (length arguments)))
      (apply #'term~appl-create (list function arguments))
    (error "Wrong number of arguments (~A) for function ~A (with arity ~A) to create a first order application."
	   (length arguments)  function (logic~fo-arity function))))

(defun logic~fo-application-p (object)
  (declare (edited  "01-APR-1998" "12-SEP-1991 14:25")
	   (authors Gebhard RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is an application,"
		    "i.e. an instance of TERM+APPL which is not a quantification.")
	   (example "(f c1 c2) --> t"
		    "f --> nil"))
  (and (term~appl-p object)
       (logic~fo-function-p (data~appl-function object)) ;; Neu H.G.
       (not (logic~fo-quantification-p object))))

#{\subsection{Quantification}
The following procedures manipulating quantification symbols are prefixed by fo~quantification-.
#}

(defun logic~fo-quantification-create (quantor variable scope)
  (declare (edited  "01-APR-1998" "15-AUG-1991 13:46")
           (authors Gebhard RICHTS)
           (input   "A Quantor, a variable and a term.")
           (effect  "None.")
           (value   "The new term (quantor [variable]-scope).")
           (example "(forall x (P x)) --> (forall [x].(P x))"))
  (logic~quantification-create quantor variable scope))
   
(defun logic~fo-quantification-p (object &key (theory logic*current-theory))
  (declare (edited  "15-AUG-1991 13:45")
           (authors RICHTS)
           (input   "A term.")
           (effect  "None.")
           (value   "True iff the term is a quantification"
                    "(i.e. it's an application with one argument which is an abstraction).")
           (example "(forall [x].(P x)) --> t"
                    "(P x) --> nil)"))
  (or (logic~universal-quantification-p object :theory theory)
      (logic~existential-quantification-p object :theory theory)))

(defun logic~fo-quantification-bound-variable (quantification)
  (declare (edited  "01-APR-1998" "15-AUG-1991 13:48")
           (authors Gebhard RICHTS)
           (input   "A quantification.")
           (effect  "None.")
           (value   "The bound variable of the quantification.")
           (example "(forall [x].(P x)) --> x"))
  (logic~quantification-bound-variable quantification))

(defun (setf logic~fo-quantification-bound-variable) (new-bound-variable quantifiaction)
  (declare (edited  "15-AUG-1991 13:50")
           (authors RICHTS)
           (input   "A quantification and a variable.")
           (effect  "The bound variable of the quantification is replaced by the new variable.")
           (value   "The new bound variable.")
           (example "(forall [x].(P x))     y --> (forall [y].(P y))"))
  (if (logic~fo-quantification-p quantification)
      (setf (first (data~abstr-domain (first (data~appl-arguments quantification)))) new-bound-variable)
    (error "Please apply (setf logic~~foquantification-bound-variabe) only on quantifiactions.")))

(defun logic~fo-quantification-scope (quantification)
  (declare (edited  "15-AUG-1991 13:49")
           (authors RICHTS)
           (input   "A quantification.")
           (effect  "None.")
           (value   "The scope of the quantification.")
           (example "(forall [x].(P x)) --> (P x)"))
  (if (logic~fo-quantification-p quantification)
      (data~abstr-scope (car (data~appl-arguments quantification)))
    (error "please apply fo~~quantification-scope only on fo-quantifications")))

(defun (setf logic~fo-quantification-scope) (new-scope quantification)
  (declare (edited  "15-AUG-1991 13:50")
           (authors RICHTS)
           (input   "A quantification and a term.")
           (effect  "The scope of QUANTIFICATION is replaced by NEW-SCOPE.")
           (value   "The new scope.")
           (example "(forall [x].(P x))     (Q x x) --> (forall [x].(Q x x))"))
  (if (logic~fo-quantification-p quantification)
      (setf (data~abstr-scope (car (data~appl-arguments quantification)))
	    new-scope)
    (error "please apply fo~~quantification-scope only on fo-quantifications")))

#{\subsection{First order test procedures.}
The following procedures are special tests for first order objects.#}


(defun logic~fo-term-p (term)
  (declare (edited  "20-JAN-1998" "07-jul-1993 19:20")
	   (authors Fehrer nesmith)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff TERM is a first order logic term."))
  (and (type~i-p (term~type term))
       (or (term~primitive-p term)
	   (and (term~appl-p term)
		(term~constant-p (data~appl-function term))
		(every #'logic~fo-term-p (data~appl-arguments term))))))



(defun logic~fo-atom-p (term)
  (declare (edited  "20-JAN-1998" "05-oct-1990 19:20")
	   (authors Fehrer richts)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff TERM is a first order atom."))
  (and (type~o-p (term~type term))
       (or (term~constant-p term)
	   (and (term~appl-p term)
		(every #'logic~fo-term-p (data~appl-arguments term))))))


(defun logic~fo-formula-p (term &key (theory logic*current-theory))
  (declare (edited  "01-APR-1998" "07-jul-1993 19:20")
           (authors Gebhard nesmith)
           (input   "A term.")
           (effect  "None.")
           (value   "True iff TERM is a first order logic formula."))
  (cond ((logic~fo-atom-p term) t)
	((not (term~appl-p term)) nil)
        ((or (logic~universal-quantification-p term :theory theory)
             (logic~existential-quantification-p term :theory theory))
         (and (type~i-p (logic~quantification-bound-variable term))
              (logic~fo-formula-p (logic~quantification-scope term))))
        ((or (logic~negation-p term :theory theory)
             (logic~disjunction-p term :theory theory)
             (logic~conjunction-p term :theory theory)
             (logic~equivalence-p term :theory theory)
	     (logic~implication-p term :theory theory)) ;; evt noch implication mit aufnehemn
         (every #'logic~fo-formula-p (data~appl-arguments term)))
        (t nil)))


(data~defgeneric logic~fo-arguments ((term))
  (declare (edited  "01-APR-1998" "20-JAN-1998" "02-AUG-1991 20:46")
	   (authors Gebhard Fehrer RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "Nil if term is a constant or variable,"
		    "a list containing the scope if it's a quantification,"
		    "a list containing the arguments of the application otherwise.")
	   (example "Let c be an object constant, x an object variable and P a binary predicate constant"
		    "c --> nil"
		    "x --> nil"
		    "(forall (lam (y I) (P y c))) --> ((P y c))"
		    "(P x c) --> (x c)"))
  (:method ((variable term+variable))
	   nil)
  (:method ((constant term+constant))
	   nil)
  (:method ((application term+appl))
	   (if (logic~fo-quantification-p application)
	       (list (logic~fo-quantification-scope application))
	     (data~appl-arguments application)))
  (:method ((abstraction term+abstr))
	   (error "This function is not defined for abstractions.")))

(data~defgeneric (setf logic~fo-set-arguments) (new-arguments (term))
  (declare (edited  "26-AUG-1992 13:45")
	   (authors RICHTS)
	   (input   "A term and the new list of arguments.")
	   (effect  "The argument-list of term is replaced by new-arguments which must be of equal length.")
	   (value   "New-arguments.")
	   (example "(P x c)       (x1 c1) --> (x1 c1), the original term is changed to (P x1 c1)"))
  (:method ((convar term+primitive) new-arguments)
   (declare (ignore new-arguments))
   (error "~A is not an application and so no arguments can be set." variable))
  (:method ((application term+appl) new-arguments)
	   (if (and (= (length new-arguments) (length (logic~fo-arguments application)))
		    (every #'(lambda (arg1 arg2)
			       ;; Das keim~equal ist wohl zu streng und muss evt noch
			       ;; durch type~alpha-equal oder wharscheinlich das richtige
			       ;; type~alpha-unify ersetzt werden. Dann muss die substitution vor
			       ;; dem neu setzt noch auf die Typen der neuen argumnte azngewandt
			       ;; werden.
			       (data~equal (term~type arg1) (term~type arg2)))
			   new-arguments (logic~fo-arguments application)))
	       (if (logic~fo-quantification-p application)
		   (setf (data~abstr-scope (car (data~appl-arguments application))) (car new-arguments))
		 (setf (data~appl-arguments application) new-arguments))
	     (error "The new arguments ~A do not fit the arguments in ~A." new-arguments application)))
  (:method ((abstraction term+abstr) new-arguments)
   (declare (ignore new-arguments))
   (error "~A is not an application and so no arguments can be set." abstraction)))



(defun logic~logical-p (object &key (theory logic*current-theory))
  (declare (edited  "15-JUN-1998")
	   (authors Chris)
	   (input   "an object (term)")
	   (effect  "None")
	   (value   "t iff object is a formula "))
  (or (logic~existential-quantification-p object :theory theory)
      (logic~universal-quantification-p object :theory theory)
      (logic~equivalence-p object :theory theory)
      (logic~implication-p object :theory theory)
      (logic~conjunction-p object :theory theory)
      (logic~disjunction-p object :theory theory )
      (logic~negation-p object :theory theory)
      (logic~equality-p object :theory theory)
      (logic~connective-p object :theory theory)))


(defun logic~dual-quantifier (quantifier)
  (declare (edited  "14-NOV-1998 19:52")
	   (authors SORGE)
	   (input   "A logical quantifier.")
	   (effect  "None.")
	   (value   "The dual to this quantifier.")
	   (example "FORALL maps to EXISTS and vice versa."))
  (let ((quantifier-name (keim~name quantifier)))
    (cond ((string-equal quantifier-name :forall) (logic~existential-quantor :name :exists))
	  ((string-equal quantifier-name :exists) (logic~universal-quantor :name :forall))
	  ((string-equal quantifier-name :forall-sort) (logic~existential-quantor :name :exists-sort))
	  ((string-equal quantifier-name :exists-sort) (logic~universal-quantor :name :forall-sort))
	  )))


(defun logic~pl-formula-p (term &key (theory logic*current-theory))
  (declare (edited  "01-APR-1998" "07-jul-1993 19:20")
           (authors chris)
           (input   "A term.")
           (effect  "None.")
           (value   "True iff TERM is a first order logic formula."))
  (cond ((and (type~o-p (term~type term))
	      (term~constant-p term))
	 t)
	((not (term~appl-p term)) nil)
        ((or (logic~negation-p term :theory theory)
             (logic~disjunction-p term :theory theory)
             (logic~conjunction-p term :theory theory)
             (logic~equivalence-p term :theory theory)
	     (logic~implication-p term :theory theory)) ;; evt noch implication mit aufnehemn
         (every #'logic~pl-formula-p (data~appl-arguments term)))
        (t nil)))
