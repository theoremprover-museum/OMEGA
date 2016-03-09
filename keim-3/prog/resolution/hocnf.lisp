;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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
;;     D-6604ecken                                                          ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                      ;;
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


(mod~defmod HOCNF 
            :uses (beta cl data delta env infer keim lit logic node pos res sksym subst term termix toset type)
            :documentation "Clause normalization for general HO-formulas."
            :exports (
                      
                      hocnf~normalize
                      hocnf~normalize-res-proof!
                      hocnf~normform-p
                      hocnf~skolemize
                      hocnf~transform-in-formula
                      hocnf~new-skolem-term-create
                      hocnf*skolem-constants))


(defvar hocnf*skolem-constants nil)


(defun hocnf~normalize-res-proof! (res-proof &key (tautology-elim 't) (ordered-clauses nil))
  (declare (edited  "20-JUN-1997")
	   (authors Ameier)
	   (input   "A resolution-proof.")
	   (effect  "The resolution proof is destructively changed:" 
		    "The clauses slot is set to the clause normal form of the assumptions and the"
		    "conclusion of the resolution proof and the delta relation slot is set to an"
		    "according new delta-relation. Also the skolem-functions slot is set to the"
		    "set of new skolem-functions."
		    "The new skolem-constants and the clauses are inserted into the environment"
		    "of the resolution-proof.")
	   (value   "The changed resolution proof."))
  (let* ((delta-relation (res~proof-delta-relation res-proof))
	 (proof-environment (res~proof-environment res-proof)))
    
    ;; settings of the global variables
    (setq hocnf*skolem-constants nil)

    ;; removing current settings
    (setf (delta~relation-pairs delta-relation) nil)
    (setf (res~proof-clauses res-proof) nil)
    (mapcar #'(lambda (skolem-function)
		(env~remove (keim~name skolem-function) proof-environment))
	    (res~proof-skolem-functions res-proof))
    (setf (res~proof-skolem-functions res-proof) nil)
    
    ;; normalization of the conclusion and the assumptions
    (let* ((clauses (apply 'append
			   (cons (hocnf=normalize-named-formula (res~proof-conclusion res-proof)
								proof-environment
								:delta-relation delta-relation
								:pol nil
								:tautology-elim tautology-elim)
				 (mapcar #'(lambda (assumption)
					     (hocnf=normalize-named-formula assumption
									    proof-environment
									    :delta-relation delta-relation
									    :pol t
									    :tautology-elim tautology-elim
									    :ordered-clauses ordered-clauses))
					 (res~proof-assumptions res-proof))))))
      
      ;; each new created clause gets an initial justification
      (mapcar #'(lambda (clause)
		  (let* ((new-initial (res~initial-create)))
		    (setf (node~justification clause) new-initial)))
	      clauses)
      
      ;; changing the resolution proof
      (setf (res~proof-clauses res-proof) clauses)
      (setf (res~proof-skolem-functions res-proof) hocnf*skolem-constants)))
  res-proof)

(defun hocnf=normalize-named-formula (named-term env &key (delta-relation nil) (pol nil) (tautology-elim 't) (ordered-clauses nil))
  (declare (edited  "09-MAR-1998")
	   (authors Ameier)
	   (input   "A named term, an environment and as keywords a delta-relation and"
		    "the polarity of the named-term.")
	   (effect  "The term is clause normalized, mayby new created skolem-constants are"
		    "inserted into the environment and the delta-relation is updated.")
	   (value   "A list of clauses, the CNF of the named-term."))
  (let* ((term (termix~term named-term))
	 (list-of-lit+pos-lists (hocnf=normalize term pol nil env nil
						 :remove-equiv-pos nil
						 :remove-leibniz-pos nil
						 :remove-equiv-neg nil
						 :remove-leibniz-neg nil)))
    (apply 'append (mapcar #'(lambda (lit+pos-list)
			       (let* ((literals (mapcar #'first lit+pos-list))
				      (clause (if ordered-clauses
						  (cl~create-ordered literals)
						(cl~create literals))))
				 
				 (if (and tautology-elim (cl~tautology-p clause))
				     nil
				   ;; update-delta-relation
				   (progn
				     (mapcar #'(lambda (lit+pos-pair)
						 (let* ((lit (first lit+pos-pair))
							(pos-in-formula (pos~list-position (second lit+pos-pair)))
							(pos-in-clause (first (data~substruct-positions lit clause :test #'eq))))
						   (delta~add-pair! delta-relation named-term pos-in-formula clause pos-in-clause)))
					     lit+pos-list)
				     (list (hocnf=copy-clause-and-update-delta-relation! clause delta-relation))))))
			   list-of-lit+pos-lists))))

(setf (symbol-function 'hocnf=copy-new-name)
      (data~name-generator 0 :prefix 'tv))

(defun hocnf=copy-clause-and-update-delta-relation! (clause delta-relation)
  (declare (edited  "12-MAR-1998")
	   (authors Ameier)
	   (input   "A clause and a delta-relation.")
	   (effect  "A copy of the clause is made (with downto=nil). The delta-relation is updated"
		    "by replacing all occurrences of the input clause by the new copied clause.")
	   (value   "The new copied clause."))

    ;; since all terms in the literals are shared, even over the literal structures and the clause structures it seems
  ;; to be a good idea to make a copy of each clause with complete new structures. AMEIER
  (let* ((new-clause (data~copy clause :downto nil))
	 (all-type-vars (remove-duplicates (apply #'append (mapcar #'(lambda (lit)
								       (term~free-type-variables (lit~atom lit)))
								   (cl~literals new-clause)))))
	 (new-type-vars (mapcar #'(lambda (tv)
				    (type~variable-create (hocnf=copy-new-name)))
				all-type-vars))
	 (new-clauseii (if all-type-vars
			   (subst~apply (subst~create all-type-vars new-type-vars) new-clause)
			 new-clause))
	 (pairs (delta~relation-pairs delta-relation)))
    (mapcar #'(lambda (pair)
		(when (eq (delta~delta-clause pair) clause)
		  (setf (delta~delta-clause pair) new-clauseii)))
	    pairs)
    new-clauseii))



	     

(defun hocnf~normform-p (clause)
  (every #'(lambda (lit) (logic~atom-p (data~top (lit~atom lit))))
       (cl~literals clause)))



(defgeneric hocnf~normalize (obj env
				 &key
				 (tautology-elim t)
				 (remove-equiv-pos t)
				 (remove-leibniz-pos t)
				 (remove-equiv-neg t)
				 (remove-leibniz-neg t))
  (declare (edited  "30-NOV-1996")
	   (authors Chris)
	   (input   "a formula or a clause and a key signalling if trivial tautologies
shall be elimanted"
		    "Furthermore some flags signalling if equivalences and/or equations"
		    "shall be removed.")
	   (effect  "create's new skolem constants and enters them into"
		    "the current environment")
	   (value   "a list containing only normalized clauses"))
  (:method ((obj cl+clause) env &key (tautology-elim t) (remove-equiv-pos t) (remove-leibniz-pos t) (remove-equiv-neg t) (remove-leibniz-neg t))
	   (if (hocnf~normform-p obj) (list obj)
	     (let ((result nil))
	       (mapcar #'(lambda (litlist)
			   (let ((clause (cl~create
					; (data~replace-free-variables-and-rename
					; litlist nil nil)
					  (mapcar #'first litlist)
					  :justification (infer~method-create "CNF"
									      :NIL
									      (list
									       obj)
									      "Clause normalization"))))
			     (if (and tautology-elim
				      (cl~tautology-p (cl~beta-reduce! clause)))
				 result
			       (push clause result))))
		       (hocnf=normalize (hocnf~transform-in-formula obj)
					t nil env nil
					:remove-equiv-pos  remove-equiv-pos
					:remove-leibniz-pos remove-leibniz-pos
					:remove-equiv-neg remove-equiv-neg
					:remove-leibniz-neg remove-leibniz-neg))
	       result)))
  (:method ((obj term+term) env &key (tautology-elim t) (remove-equiv-pos t) (remove-leibniz-pos t) (remove-equiv-neg t) (remove-leibniz-neg t))
	   (let* ((newobj (beta~head-normalize obj))
		  (clause (cl~create
			   (list (lit~literal-create newobj t))
			   :justification (infer~method-create "INPUT-CLAUSE" :NIL nil  "A INPUT CLAUSE"))))
	     (hocnf~normalize clause env
			      :tautology-elim tautology-elim
			      :remove-equiv-pos  remove-equiv-pos
			      :remove-leibniz-pos remove-leibniz-pos
			      :remove-equiv-neg remove-equiv-neg
			      :remove-leibniz-neg remove-leibniz-neg)))
  (:method (obj env &key (tautology-elim t) (remove-equiv-pos t) (remove-leibniz-pos t) (remove-equiv-neg t) (remove-leibniz-neg t))
	   (hocnf~normalize (hocnf~transform-in-formula
			       (beta~head-normalize obj))
			    env
			    :tautology-elim tautology-elim
			    :remove-equiv-pos  remove-equiv-pos
			    :remove-leibniz-pos remove-leibniz-pos
			    :remove-equiv-neg remove-equiv-neg
			    :remove-leibniz-neg remove-leibniz-neg)))

(defun hocnf=normalize (formula pol variables env position
				&key
				(remove-equiv-pos t)
				(remove-leibniz-pos t)
				(remove-equiv-neg t)
				(remove-leibniz-neg t))
  (declare (edited  "25-JUL-1996")
	   (authors Chris AMEIER)
	   (input   "A formula, the polarity of the formula and some free"
		    "variables (for skolemization to depend on), an environment and a list"
		    "of integers describing  the current position of the formula." 
		    "Furthermore some flags signalling if equivalences and/or equations"
		    "shall be removed")
	   (effect  "create's new skolem constants and enters them in"
		    "the current environment")
	   (value   "A CNF of FORMULA with positions, i.e. a list of lists of pairs of literals"
		    "and positions."))
  (let* ((type-vars (when (term~schema-p formula)
		      (data~schema-domain formula)))
	 (norm-form (beta~head-normalize (if type-vars
					     (data~schema-range formula)
					   formula)))
	 (args (when (term~appl-p norm-form)
		 (data~appl-arguments norm-form))))
    (if pol
	(cond (type-vars ;; (kappa <a1 ... an> A)
	       ;; Kappa gebundene Type-variablen bleiben einfach frei in der Range
	       ;; spaeter beim Kopieren werden sie dann durch andere Typ-variablen ausgetauscht.
	       (hocnf=normalize norm-form pol variables env (append position (list 0)) 
				:remove-equiv-pos  remove-equiv-pos
				:remove-leibniz-pos remove-leibniz-pos
				:remove-equiv-neg remove-equiv-neg
				:remove-leibniz-neg remove-leibniz-neg))	      
	      ((logic~negation-p norm-form) ; not A
	       (hocnf=normalize (first args) nil variables env (append position (list 1)) 
				:remove-equiv-pos  remove-equiv-pos
				:remove-leibniz-pos remove-leibniz-pos
				:remove-equiv-neg remove-equiv-neg
				:remove-leibniz-neg remove-leibniz-neg))
	      ((logic~disjunction-p norm-form) ; (A or B)
	       (hocnf=disj-list (hocnf=normalize (first args) t variables env (append position (list 1))
						 :remove-equiv-pos  remove-equiv-pos
						 :remove-leibniz-pos remove-leibniz-pos
						 :remove-equiv-neg remove-equiv-neg
						 :remove-leibniz-neg remove-leibniz-neg)
				(hocnf=normalize (second args) t variables env (append position (list 2))
						 :remove-equiv-pos  remove-equiv-pos
						 :remove-leibniz-pos remove-leibniz-pos
						 :remove-equiv-neg remove-equiv-neg
						 :remove-leibniz-neg remove-leibniz-neg)))
	      ((logic~conjunction-p norm-form) ; (A and B)
	       (hocnf=conj-list (hocnf=normalize (first args) t variables env (append position (list 1)) 
						 :remove-equiv-pos  remove-equiv-pos
						 :remove-leibniz-pos remove-leibniz-pos
						 :remove-equiv-neg remove-equiv-neg
						 :remove-leibniz-neg remove-leibniz-neg)
				  (hocnf=normalize (second args) t variables env (append position (list 2))
						   :remove-equiv-pos  remove-equiv-pos
						   :remove-leibniz-pos remove-leibniz-pos
						   :remove-equiv-neg remove-equiv-neg
						   :remove-leibniz-neg remove-leibniz-neg)))
	      ((logic~implication-p norm-form) ; (A impl B) 
	       (hocnf=disj-list (hocnf=normalize (first args) nil variables env (append position (list 1))
						 :remove-equiv-pos  remove-equiv-pos
						 :remove-leibniz-pos remove-leibniz-pos
						 :remove-equiv-neg remove-equiv-neg
						 :remove-leibniz-neg remove-leibniz-neg)
				  (hocnf=normalize (second args) t variables env (append position (list 2))
						   :remove-equiv-pos  remove-equiv-pos
						   :remove-leibniz-pos remove-leibniz-pos
						   :remove-equiv-neg remove-equiv-neg
						   :remove-leibniz-neg remove-leibniz-neg)))
	      ((logic~equivalence-p norm-form) ; (A equiv B)
	       (hocnf=conj-list 
		(hocnf=disj-list (hocnf=normalize (first args) nil variables env (append position (list 1 1)) 
						  :remove-equiv-pos  remove-equiv-pos
						  :remove-leibniz-pos remove-leibniz-pos
						  :remove-equiv-neg remove-equiv-neg
						  :remove-leibniz-neg remove-leibniz-neg)
				 (hocnf=normalize (second args) t variables env (append position (list 1 2))
						  :remove-equiv-pos  remove-equiv-pos
						  :remove-leibniz-pos remove-leibniz-pos
						  :remove-equiv-neg remove-equiv-neg
						  :remove-leibniz-neg remove-leibniz-neg))
		(hocnf=disj-list (hocnf=normalize (second args) nil variables env (append position (list 2 1))
						  :remove-equiv-pos  remove-equiv-pos
						  :remove-leibniz-pos remove-leibniz-pos
						  :remove-equiv-neg remove-equiv-neg
						  :remove-leibniz-neg remove-leibniz-neg)
				 (hocnf=normalize (first args) t variables env (append position (list 2 2))
						  :remove-equiv-pos  remove-equiv-pos
						  :remove-leibniz-pos remove-leibniz-pos
						  :remove-equiv-neg remove-equiv-neg
						  :remove-leibniz-neg remove-leibniz-neg)))) 
	      ((logic~existential-quantification-p norm-form) ; (exists x A)
	       (hocnf=normalize	(hocnf~skolemize (data~abstr-scope (first args))
						 (list (logic~quantification-bound-variable norm-form))
						 variables env)
				t variables env (append position (list 1 0))
				:remove-equiv-pos  remove-equiv-pos
				:remove-leibniz-pos remove-leibniz-pos
				:remove-equiv-neg remove-equiv-neg
				:remove-leibniz-neg remove-leibniz-neg))
	      ((logic~universal-quantification-p norm-form) ; (forall x A)
	       (let* ((new-var (data~copy 
				(logic~quantification-bound-variable norm-form)
				:downto '(type+primitive)))
		      (renamed-norm-form (data~replace-free-variables (data~abstr-scope (first args))
								      (list
								       (logic~quantification-bound-variable norm-form))
								      (list
								       new-var))))
		 (hocnf=normalize renamed-norm-form
				  t
				  (append variables (list new-var))
				  env
				  (append position (list 1 0))
				  :remove-equiv-pos  remove-equiv-pos
				  :remove-leibniz-pos remove-leibniz-pos
				  :remove-equiv-neg remove-equiv-neg
				  :remove-leibniz-neg remove-leibniz-neg)))
	      ((and remove-equiv-pos
		    (logic~equality-p norm-form)   ;;; (= P Q)
		    (type~o-p
		     (data~annotation (car (data~appl-arguments norm-form)))))
	       (hocnf=normalize	(term~appl-create (logic~equivalence-constant)
						  (data~appl-arguments norm-form))
				t variables env position
				:remove-equiv-pos  remove-equiv-pos
				:remove-leibniz-pos remove-leibniz-pos
				:remove-equiv-neg remove-equiv-neg
				:remove-leibniz-neg remove-leibniz-neg))
	      ((and remove-leibniz-pos           
		    (logic~equality-p norm-form))    ;;; (= T1 T2)
	       (hocnf=normalize (hocnf=remove=leibniz (first args) (second args))
				t variables env position
				:remove-equiv-pos  remove-equiv-pos
				:remove-leibniz-pos remove-leibniz-pos
				:remove-equiv-neg remove-equiv-neg
				:remove-leibniz-neg remove-leibniz-neg))
	      (t
	       (list (list (list (lit~literal-create norm-form t) position)))))
      (cond (type-vars ;; (not (kappa <a1 ... an> A))
	     ;; Kappa gebundene Type-variablen werden durch neue Type-konstanten ersetzt
	     (hocnf=normalize (hocnf=skolemize-type-variables (term~alpha-copy norm-form nil)
							      type-vars
							      env)
			      pol variables env (append position (list 0))
			      :remove-equiv-pos  remove-equiv-pos
			      :remove-leibniz-pos remove-leibniz-pos
			      :remove-equiv-neg remove-equiv-neg
			      :remove-leibniz-neg remove-leibniz-neg))
	    ((logic~negation-p norm-form) ; not (not A)
	     (hocnf=normalize (first args) t variables env (append position (list 1)) 
			      :remove-equiv-pos  remove-equiv-pos
			      :remove-leibniz-pos remove-leibniz-pos
			      :remove-equiv-neg remove-equiv-neg
			      :remove-leibniz-neg remove-leibniz-neg))
	    ((logic~disjunction-p norm-form) ; not (A or B)
	     (hocnf=conj-list (hocnf=normalize (first args) nil variables env (append position (list 1))
					       :remove-equiv-pos  remove-equiv-pos
					       :remove-leibniz-pos remove-leibniz-pos
					       :remove-equiv-neg remove-equiv-neg
					       :remove-leibniz-neg remove-leibniz-neg)
			      (hocnf=normalize (second args) nil variables env (append position (list 2))
					       :remove-equiv-pos  remove-equiv-pos
					       :remove-leibniz-pos remove-leibniz-pos
					       :remove-equiv-neg remove-equiv-neg
					       :remove-leibniz-neg remove-leibniz-neg)))
	    ((logic~conjunction-p norm-form) ; not (A and B)
	     (hocnf=disj-list (hocnf=normalize (first args) nil variables env (append position (list 1))
					       :remove-equiv-pos  remove-equiv-pos
					       :remove-leibniz-pos remove-leibniz-pos
					       :remove-equiv-neg remove-equiv-neg
					       :remove-leibniz-neg remove-leibniz-neg)
			      (hocnf=normalize (second args) nil variables env (append position (list 2))
					       :remove-equiv-pos  remove-equiv-pos
					       :remove-leibniz-pos remove-leibniz-pos
					       :remove-equiv-neg remove-equiv-neg
					       :remove-leibniz-neg remove-leibniz-neg)))
	    ((logic~implication-p norm-form) ; not (A impl B) 
	     (hocnf=conj-list (hocnf=normalize (first args) t variables env (append position (list 1))
					       :remove-equiv-pos  remove-equiv-pos
					       :remove-leibniz-pos remove-leibniz-pos
					       :remove-equiv-neg remove-equiv-neg
					       :remove-leibniz-neg remove-leibniz-neg)
			      (hocnf=normalize (second args) nil variables env (append position (list 2))
					       :remove-equiv-pos  remove-equiv-pos
					       :remove-leibniz-pos remove-leibniz-pos
					       :remove-equiv-neg remove-equiv-neg
					       :remove-leibniz-neg remove-leibniz-neg)))
	    ((logic~equivalence-p norm-form) ; not (A equiv B)
	     (hocnf=disj-list 
	      (hocnf=conj-list (hocnf=normalize (first args) t variables env (append position (list 1 1)) 
						:remove-equiv-pos  remove-equiv-pos
						:remove-leibniz-pos remove-leibniz-pos
						:remove-equiv-neg remove-equiv-neg
						:remove-leibniz-neg remove-leibniz-neg)
			       (hocnf=normalize (second args) nil variables env (append position (list 1 2))
						:remove-equiv-pos  remove-equiv-pos
						:remove-leibniz-pos remove-leibniz-pos
						:remove-equiv-neg remove-equiv-neg
						:remove-leibniz-neg remove-leibniz-neg))
	      (hocnf=conj-list (hocnf=normalize (second args) t variables env (append position (list 2 1))
						:remove-equiv-pos  remove-equiv-pos
						:remove-leibniz-pos remove-leibniz-pos
						:remove-equiv-neg remove-equiv-neg
						:remove-leibniz-neg remove-leibniz-neg)
			       (hocnf=normalize (first args) nil variables env (append position (list 2 2))
						:remove-equiv-pos  remove-equiv-pos
						:remove-leibniz-pos remove-leibniz-pos
						:remove-equiv-neg remove-equiv-neg
						:remove-leibniz-neg remove-leibniz-neg))))
	    ((logic~existential-quantification-p norm-form) ; not (exists x A)
             (let* ((new-var (keim~copy 
                              (logic~quantification-bound-variable norm-form)))
		    (renamed-norm-form  (data~replace-free-variables (data~abstr-scope (first args))
								     (list
								      (logic~quantification-bound-variable norm-form))
								     (list
								      new-var))))
	       (hocnf=normalize renamed-norm-form
				nil
				(append variables (list new-var))
				env
				(append position (list 1 0))
				:remove-equiv-pos  remove-equiv-pos
				:remove-leibniz-pos remove-leibniz-pos
				:remove-equiv-neg remove-equiv-neg
				:remove-leibniz-neg remove-leibniz-neg)))
	    ((logic~universal-quantification-p norm-form) ; not (forall x A)
	     (hocnf=normalize (hocnf~skolemize (data~abstr-scope (first args))
					       (list (logic~quantification-bound-variable norm-form))
					       variables env)
			      nil variables env (append position (list 1 0))
			     :remove-equiv-pos  remove-equiv-pos
			     :remove-leibniz-pos remove-leibniz-pos
			     :remove-equiv-neg remove-equiv-neg
			     :remove-leibniz-neg remove-leibniz-neg))
	    ((and remove-equiv-neg
		  (logic~equality-p norm-form)
		  (type~o-p
		   (data~annotation (car (data~appl-arguments norm-form)))))
	     (hocnf=normalize (term~appl-create (logic~equivalence-constant)
						(data~appl-arguments norm-form))
			      nil variables env position
			      :remove-equiv-pos  remove-equiv-pos
			      :remove-leibniz-pos remove-leibniz-pos
			      :remove-equiv-neg remove-equiv-neg
			      :remove-leibniz-neg remove-leibniz-neg))
	    ((and remove-leibniz-neg
		  (logic~equality-p norm-form))
	     (hocnf=normalize (hocnf=remove=leibniz (first args) (second args))
			      nil variables env position
			      :remove-equiv-pos  remove-equiv-pos
			      :remove-leibniz-pos remove-leibniz-pos
			      :remove-equiv-neg remove-equiv-neg
			      :remove-leibniz-neg remove-leibniz-neg))
	    (t (list (list (list (lit~literal-create norm-form nil) position))))
	    ))))




(defun hocnf=remove=leibniz (t1 t2 &optional variables)
  (cond ((type~o-p (data~annotation t1))
	 (let ((formula (term~appl-create (logic~implication-constant)
					  (list t1 t2))))
	   (dolist (var variables formula)
	     (setf formula
		   (term~appl-create
		    (logic~universal-quantor :name :forall)  ;; type nicht mehr notwendig (data~annotation var)
		    (list (term~abstr-create (list var) formula)))))))
	((data~ground-p (data~annotation t1))
	 (let* ((type (type~func-create (data~annotation t1) (type~o)))
		(newvar (term~variable-create (gensym) type))
		(term1 (term~appl-create newvar (list t1)))
		(term2 (term~appl-create newvar (list t2))))
	   (hocnf=remove=leibniz term1 term2 (cons newvar variables))))
	((type~complex-p (data~annotation t1))
	 (let* ((type (data~annotation t1))
		(domtypes (data~n-domain (data~annotation type)))
		(newvars (mapcar #'(lambda (tp)
				     (term~variable-create (gensym) tp))
				 domtypes))
		(term1 (term~appl-create t1 newvars))
		(term2 (term~appl-create t2 newvars)))
	   (hocnf=remove=leibniz term1 term2 (append newvars variables))))))
	

(defun hocnf=disj-list (listoflists1 listoflists2)
  (declare (edited  "25-JUL-1996")
	   (authors Chris)
	   (input   "two lists of lists representing clausesets")
	   (effect  "none")
	   (value   "a disjunctive merge of the two clausesets
                     (Factorization included)
                     Example:
                     ((A B) (C D)) ((A F) (G))
                         -->  ((A B F) (A B G) (C D A F) (C D G))"
		    ))
  (apply 'append (mapcar #'(lambda (list-1)
			     (mapcar #'(lambda (list-2)
					 (append list-1 list-2))
				     listoflists2))
			 listoflists1)))

(defun hocnf=conj-list (listoflists1 listoflists2)
  (declare (edited  "25-JUL-1996")
	   (authors Chris)
	   (input   "Two lists of lists representing clausesets.")
	   (effect  "None.")
	   (value   "A conjunctive merge of the two clausesets (append)."))
  (append listoflists1 listoflists2))

(defun hocnf=skolemize-type-variables (term type-variables env)
  (declare (edited  "24-FEB-1999")
	   (authors Ameier)
	   (input   "A term (which is not itself a schema), a list of type-variables and"
		    "an environment.")
	   (effect  "Creates new type-constants for all type-variables and enters them into"
		    "the environment.")
	   (value   "The formula with all occurences of the type-variables replaced by the"
		    "according new type-constants."))
  (let* ((new-type-consts (mapcar #'(lambda (type-var)
				      (let* ((new-symbol (gensym (format nil "tc-~A-" (keim~name type-var)))))
					(type~create-primitive-in-environment new-symbol 'type+constant env)))
				  type-variables)))
    (subst~apply (subst~create type-variables new-type-consts)
		 term)))

(defun hocnf~skolemize (formula binder variables env)
  (declare (edited  "25-JUL-1996")
	   (authors Chris)
	   (input   "A formula.")
	   (effect  "Create's new skolem constants and enters them in"
		    "the environment ENV.")
	   (value   "The formula with all free variables in binder replaced
                     by a new skolem-term (this term depends on the list
                     of allquantification variables in VARIABLES)
                     EXAMPLE:
                     (IMPLIES (P X Y Z) (FORALL [X]. (Q X Y)))
                     (X Y)
                     (U V)
                     ---> (IMPLIES (P (sk1 U V) (sk2 U V) Z)
                                   (FORALL [X]. (Q X (sk2 U V))))
                     "))
  (let ((skolem-terms 
	 (mapcar #'(lambda (x) (hocnf~new-skolem-term-create x variables env))
		 binder)))
    (subst~apply
     (subst~idem (subst~create binder skolem-terms))
     formula)))

(defun hocnf~new-skolem-term-create (var variables env) 
  (declare (edited  "25-JUL-1996")
	   (authors Chris)
	   (input   "a variable and a list of variables and an environment")
	   (effect  "create's new skolem constants and enters them in"
		    "the current environment")
	   (value   "a new skolem term with the same type as var
                     and with the variables from VARIABLES as arguments."))
  (let* ((type (if variables
		   (type~func-create
		    (mapcar #'data~annotation variables)
		    (data~annotation var))
		 (data~annotation var)))
	 (new-constant
	  (sksym~env-enter
	   (gensym (format nil "SK_~A_~A_" (length variables) (keim~name var))) type
	   (length variables) env)))
    (setq hocnf*skolem-constants (cons new-constant hocnf*skolem-constants))
    (if variables 
	(term~appl-create new-constant variables)
      (values new-constant (data~annotation new-constant) (list var (data~annotation var))
	      (list variables (mapcar #'data~annotation variables))))))


(defgeneric hocnf~transform-in-formula (obj)   
  (declare (edited  "02-AUG-1996")
	   (authors Chris)
	   (input   "a literal or a clause or a list of literals or a"
		    "list of clauses")
	   (effect  "none")
	   (value   "the argument transformed in one formula"))
  (:method ((obj cl+clause))
	   (let ((litlist (cl~literals obj)))
	     (if litlist (hocnf~transform-in-formula litlist)
	       (logic~falsity-constant))))
  (:method ((obj lit+literal))
	   (if (lit~positive-p obj) (lit~atom obj)
	     (term~appl-create (logic~negation-constant) (list (lit~atom obj)))))
  (:method ((obj list))
	   (when obj
	     (cond ((cl~clause-p (car obj))
		    (let ((formula (hocnf~transform-in-formula (car obj)))
			  (restlist (cdr obj)))
		      (dolist (cl restlist formula)
			(setq formula (term~appl-create (logic~conjunction-constant)
							(list (hocnf~transform-in-formula cl)
							      formula))))))
		   ((lit~literal-p (car obj))
		    (let ((formula (hocnf~transform-in-formula (car obj)))
			  (restlist (cdr obj)))
		      (dolist (lit restlist formula)
			(setq formula (term~appl-create (logic~disjunction-constant)
							(list (hocnf~transform-in-formula lit)
							      formula))))))))))
