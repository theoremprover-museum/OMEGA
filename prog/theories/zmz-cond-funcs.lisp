;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
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

(meth~deffun theorem-formula (th)
  (declare (edited  "05-MAR-2001")
	   (authors Ameier)
	   (input   "A theorem object.")
	   (effect  "None.")
	   (value   "The formula of the theorem object."))
  (node~formula (first (th~ass-node th))))

(meth~defcond mdetpowerofop-at-posses-p (args cmapp)
	      (declare (edited  "22-FEB-2001")
		       (authors Ameier)
		       (input   "A list of arguments (a formula and a metavariable) and a cmapping <c,mapp>.")
		       (effect  "None.")
		       (value   "If formula contains determined power-of-operation expressions (at the positions"
				"poslist), then <c,mapp'> is returned, where mapp' results from mapp by binding"
				"the input meta-variable to poslist. Otherwise, <c,nil> is returned."
				"A subformula (power-of-operation op n x) is determined if n is a positive integer."))
	      (let* ((formula (first args))
		     (mv (second args))
		     (detposses (methhelp=detpowerofop-posses formula)))
		(if detposses
		    (meth~mapp-extend-mapp cmapp mv detposses)
		  (meth~mapp-new-constraint cmapp nil))))
		  
(meth~new-relational-function 'mdetpowerofop-at-posses-p)

(meth~deffun mrewrite-detpowerofop-at-posses (formula poslist)
  (declare (edited  "22-FEB-2001")
	   (authors Ameier)
	   (input   "A formula and a list of positions. Thereby, each position describes a subformula in the formula"
		    "that is a determined power-of-operation expression. A determined power-of-operation expression"
		    "is a subformula (power-of-operation op n x) where n is a positive integer.")
	   (effect  "None.")
	   (value   "The formula that results from the input formula by replaceing each det. power-of-operation subformula"
		    "(specified by the position list) by the corresponding spelled-out expression, e.g.,"
		    "(power-of-operation + 3 x) is replaced by x + (x + x)."))
  (do* ((rest-pos poslist (rest rest-pos))
	(current-formula formula))
      ((null rest-pos)
       current-formula)
    (setq current-formula (methhelp=rewrite-powerofop-at-position current-formula (first rest-pos)))))

(defun methhelp=rewrite-powerofop-at-position (formula pos)
  (declare (edited  "22-FEB-2001")
	   (authors Ameier)
	   (input   "A formula and a position, such that at the position the formula has a determined"
		    "power-of-operation expression.")
	   (effect  "None.")
	   (value   "The formula that results from rewriting the determined power-of-operation expression"
		    "by its spelled-out version."))
  (let* ((detpower-formula (data~struct-at-position formula pos))
	 (args (data~appl-arguments detpower-formula))
	 (op (first args))
	 (n (second args))
	 (x (third args))
	 (new-expr (methhelp=produce-new-power-expr n op x)))
    (data~replace-at-position formula pos new-expr)))

(defun methhelp=produce-new-power-expr (nin op x)
  (declare (edited  "22-FEB-2001")
	   (authors Ameier)
	   (input   "Three terms: a positive integer n, a binary operation op, and a term x.")
	   (effect  "None.")
	   (value   "A term: x op x op x ... n times."))
  (let ((n (keim~name nin)))
    (do* ((restn n (decf restn))
	  (current-formula x (term~appl-create op (list x current-formula))))
	((= restn 1)
	 current-formula)))  
  )

(defun methhelp=detpowerofop-posses (formula)
  
  (declare (edited  "22-FEB-2001")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "A list of positions in formula. The subterm at such a position is a determined power-of-operation"
		    "expression. A subformula (power-of-operation op n x) is determined if n is a positive integer."))
  (let* ((powerofop-obj (data~schema-range (env~lookup-object 'power-of-operation (pds~environment omega*current-proof-plan))))
	 (posses-of-powerofop-obj (data~positions formula #'(lambda (it)
							      (and (data~appl-p it)
								   (keim~equal (data~appl-function it)
									       powerofop-obj))))))
    (remove-if-not #'(lambda (pos)
		       (let* ((term-at-pos (data~struct-at-position formula pos)))
			 (if (= (length (data~appl-arguments term-at-pos)) 3)
			     (let* ((n (second (data~appl-arguments term-at-pos))))
			       (and (term~number-p n)
				    (integerp (keim~name n))
				    (> (keim~name n) 0)))
			   nil)))
		   posses-of-powerofop-obj)))
			      



(meth~defcond mposint-p (args cmapp)
	      (declare (edited  "22-FEB-2001")
		       (authors Ameier)
		       (input   "A list of arguments (a formula) and a cmapping <c,mapp>.")
		       (effect  "None.")
		       (value   "<C,mapp> if formula is a term+number with a positive integer as name,"
				"<C,nil> otherwise."))
	      (let* ((formula (first args)))
		(meth~mapp-new-constraint cmapp (and (term~number-p formula)
						     (integerp (keim~name formula))
						     (> (keim~name formula) 0)))))
		   	      
(meth~defcond mdetermined-intintervall-p (args cmapp)
	      (declare (edited  "06-MAR-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a formula) and a cmapping <c,mapp>.")
		       (effect  "None.")
		       (value   "<C,mapp> if formula represents an interger interval with determined bounds,"
				"i.e., if formula has the form '(interger-interval n1 n2)' where n1 and n2 are"
				"integers."))
	      (meth~mapp-new-constraint cmapp (methhelp=determined-intintervall-p (first args))))

(defun methhelp=determined-intintervall-p (formula)
  (declare (edited  "21-FEB-2001")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple-values:"
		    "First: T if the formula represents an interger interval with determined bounds,"
		    "       i.e., if formula has the form '(interger-interval n1 n2)' where n1 and n2 are"
		    "       integers, nil otherwise."
		    "Second: The bounds (as numbers!) of the integer-interval, if the first value is t."))
  (let* ((intinter-obj (env~lookup-object 'integer-intervall (pds~environment omega*current-proof-plan))))
    (if (and (data~appl-p formula)
	     (keim~equal (data~appl-function formula) intinter-obj))
	(let* ((fargs (data~appl-arguments formula)))
	  (if (and (= (length fargs) 2)
		   (term~constant-p (first fargs))
		   (term~constant-p (second fargs))
		   (numberp (keim~name (first fargs)))
		   (numberp (keim~name (second fargs))))		
	      (values t
		      (list (keim~name (first fargs)) (keim~name (second fargs))))
	    (values nil nil)))
      (values nil nil))))

(meth~defcond mresclass-set-p (args cmapp)
	      (declare (edited  "06-MAR-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a formula) and a cmapping <c,mapp>.")
		       (effect  "None.")
		       (value   "<C,mapp> if formula represents a resclass-set, <nil,mapp> otherwise."))
	      (meth~mapp-new-constraint cmapp (zmztac=resclass-set-p (first args))))

(meth~defcond mprod-of-resclass-sets-p (args cmapp)
	      (declare (edited  "06-MAR-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a formula) and a cmapping <c,mapp>.")
		       (effect  "None.")
		       (value   "<C,mapp> if formula represents a direct product of resclass-sets, <nil,mapp> otherwise."))
	      (meth~mapp-new-constraint cmapp (ZMZTAC=PROD-OF-RESCLASS-SETS-P (first args))))

(meth~deffun mclass-factor-of (resclass-set)
	     (declare (edited  "06-MAR-2000")
		      (authors Ameier)
		      (input   "A formula representing a set of residue-classes.")
		      (effect  "None.")
		      (value   "The common class-factor of these residue-classes."))  
	     (zmztac=class-factor resclass-set))

(meth~deffun mnum-set-of (set)
	     (declare (edited  "06-MAR-2000")
		      (authors Ameier)
		      (input   "A formula representing a set.")
		      (effect  "None.")
		      (value   "A formula representing the corresponding set of numbers."
			       "I.e., for a residue class set a formula of the corresponding natural numbers"
			       "is created, if the input set is an integer-interval, a formula with the"
			       "corresponding numbers is returned."))
	     (cond ((methhelp=determined-intintervall-p set)
		    (methhelp=number-set-to-intinterval set))
		   (t 
		    (zmztac=number-set-to-resclass-set set))))

(defun methhelp=number-set-to-intinterval (formula)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (bound1 (keim~name (first (data~appl-arguments formula))))
	 (bound2 (keim~name (second (data~appl-arguments formula))))
	 (low-bound (min bound1 bound2))
	 (up-bound (max bound1 bound2))
	 (number-list (do* ((rest-number low-bound (incf rest-number))
			    (number-list (list (post~read-object rest-number env :existing-term))
					 (cons (post~read-object rest-number env :existing-term) number-list)))
			  ((= rest-number up-bound)
			   number-list))))
    ;;(format t "~%NUMBER-LIST: ~A" number-list) 
    (zmztac=produce-number-set number-list)))




(meth~defcond structunitset+op (args cmapp)
	      (declare (edited  "19-MAR-2003")
		       (authors Ameier)
		       (input   "A list of arguments and a cmapping <c,mapp>."
				"First argument is a formula, the second is a position list, and two further"
				"arguments are meta-variables.")
		       (effect  "None.")
		       (value   "The position list are positions of occurences of struct-unit in the formula."
				"The function chooses the first occurence from this list such that the"
				"subformula with struct-unit has the form (struct-unit set op)."
				"If it finds such a subformula, the function returns <c,mapp'> where mapp' results"
				"from mapp by binding the two input meta-variables to set and op."
				"Otherwise, <nil,cmapp>."))


	      (let* ((formula (first args))
		     (goal (second args))
		     (poslist (third args))
		     (mv-structuterm (fourth args))
		     (mv-assump-list (fifth args))
		     (mv-x (sixth args)))

		(multiple-value-bind
		    (structuterm set op)
		    (methhelp=find-replacable-structunit poslist formula)
		  (if structuterm
		      (progn 
			(meth~mapp-extend-mapp cmapp mv-structuterm structuterm)
			(multiple-value-bind
			    (e-support e)
			    (methhelp=find-unit-support (pds~node-supports goal) set op)
			  (if e-support
			      (progn
				(meth~mapp-extend-mapp cmapp mv-assump-list (list e-support))
				(meth~mapp-extend-mapp cmapp mv-x e))
			    (progn
			      (meth~mapp-extend-mapp cmapp mv-assump-list nil)
			      (meth~mapp-extend-mapp cmapp mv-x (list nil))))))
		              ;; OMEGA has problems to accept nil as bounded instantiation -> (list nil) for x!
		    (meth~mapp-new-constraint cmapp nil)))))

(defun methhelp=find-replacable-structunit (poslist formula)
  (do* ((rest-poses poslist (rest rest-poses))
	(set nil)
	(op nil)
	(structuterm nil))
      ((or (null rest-poses)
	   set)
       (if set
	   (values structuterm set op)
	 (values nil nil)))
    (let* ((head-pos (first rest-poses)))
      (multiple-value-bind
	  (new-set new-op new-structuterm)
	  (methhelp=check-structunit-at-position formula head-pos)
	(when (and new-set
		   new-op
		   new-structuterm)
	  (progn (setf set new-set)
		 (setf op new-op)
		 (setf structuterm new-structuterm)
		 ))))))

(meth~new-relational-function 'structunitset+op)

(defun methhelp=find-unit-support (supports set op)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (e-support (first (remove-if-not #'(lambda (support)
			     (let* ((formula (node~formula support)))
			       (if (and (data~appl-p formula)
					(keim~equal (data~appl-function formula)
						    (data~schema-range (env~lookup-object 'unit env)))
					(= (length (data~appl-arguments formula))
					   3))
				   't
				 nil)))
					  supports))))
    (if e-support
	(values e-support
		(third (data~appl-arguments (node~formula e-support))))
      (values nil nil))))
		

(defun methhelp=check-structunit-at-position (formula pos)
  (let* ((up-pos (pos~butlast pos 1))
	 (term-at-pos (data~struct-at-position formula up-pos))
	 (env (pds~environment omega*current-proof-plan)))
    (if (data~appl-p term-at-pos)
	(let* ((func (data~appl-function term-at-pos))
	       (args (data~appl-arguments term-at-pos)))
	  (if (and (or (keim~equal func (data~schema-range (env~lookup-object 'struct-unit env)))
		       (keim~equal func (data~schema-range (env~lookup-object 'group-unit env))))
		   (= (length args) 2))
	      (values (first args)
		      (second args)
		      term-at-pos)
	    (values nil nil nil)))
      (values nil nil nil))))

(meth~deffun create-embedded-unit-term (formula structuterm x)
	     (declare (edited  "19-MAR-2003")
		       (authors Ameier)
		       (input   "A formula, a term of the form '(struct-unit set op)' which occurs at"
				"least one time as subterm in the formula, and X."
				"If there exists a support of the form (unit set op e), then X=e, otherwise"
				"X=nil.")
		       (effect  "None.")
		       (value   "If X=e, then the occurences of '(struct-unit set op)' in the input"
				"formula are replaced by e and this new formula is returned."
				"If X=nil, then a new variable e is created and then the occurences of"
				"'(struct-unit set op)' in the input formula are replaced by this"
				"e. Let's call this new formula femb. Then, the following formula is cretaed"
				"and returned:"
				"(exists-sort (lam e"
				"                  (and (unit set op e)"
				"                       femb))"
				"             set)."))
	     (let* ((set (first (data~appl-arguments structuterm)))
		    (op (second (data~appl-arguments structuterm))))
	       (if (term~p X)  ;; if not given --> X=list with () as only element (see above)
		   (data~replace-struct formula structuterm x)
		 (let* ((e (term~variable-create 'e (first (data~abstr-domain (term~type set)))))
			(femb (data~replace-struct formula structuterm e))
			(env (pds~environment omega*current-proof-plan)))
		   
		   (term~appl-create (env~lookup-object 'exists-sort env)
				     (list (term~abstr-create (list e)
							      (term~appl-create (env~lookup-object 'and env)
										(list (term~appl-create (env~lookup-object 'unit env)
													(list set op e))
										      femb)))
					   set))))))



















(meth~defcond structinverse (args cmapp)
	      (declare (edited  "19-MAR-2003")
		       (authors Ameier)
		       (input   "A list of arguments and a cmapping <c,mapp>."
				"First argument is a formula, the second is a position list, and another"
				"argument that is a meta-variable.")
		       (effect  "None.")
		       (value   "The position list are positions of occurences of group-inverse in the formula."
				"The function chooses the first occurence from this list such that the"
				"subformula with group-inverse has the form (group-inverse set op x)."
				"If it finds such a subformula, the function returns <c,mapp'> where mapp' results"
				"from mapp by binding the meta-variabe to the subterm (group-inverse set op x)."
				"Otherwise, <nil,cmapp>."))
	      (let* ((formula (first args))
		     (poslist (second args))
		     (mv-structiterm (third args)))

		(do* ((rest-poses poslist (rest rest-poses))
		      (set nil)
		      (op nil)
		      (structiterm nil))
		    ((or (null rest-poses)
			 set)
		     (if set
			 (progn
			   (meth~mapp-extend-mapp cmapp mv-structiterm structiterm))
		       (meth~mapp-new-constraint cmapp nil)))
		  (let* ((head-pos (first rest-poses)))
		    (multiple-value-bind
			(new-set new-op new-structiterm)
			(methhelp=check-groupinverse-at-position formula head-pos)
		      (when (and new-set
				 new-op
				 new-structiterm)
			(progn (setf set new-set)
			       (setf op new-op)
			       (setf structiterm new-structiterm)
			       )))))))

(meth~new-relational-function 'structinverse)

(defun methhelp=check-groupinverse-at-position (formula pos)
  (let* ((up-pos (pos~butlast pos 1))
	 (term-at-pos (data~struct-at-position formula up-pos))
	 (env (pds~environment omega*current-proof-plan)))
    (if (data~appl-p term-at-pos)
	(let* ((func (data~appl-function term-at-pos))
	       (args (data~appl-arguments term-at-pos)))
	  (if (and ;;(or (keim~equal func (data~schema-range (env~lookup-object 'struct-unit env)))
		   ;;    (keim~equal func (data~schema-range (env~lookup-object 'group-unit env))))
	           (keim~equal func (data~schema-range (env~lookup-object 'group-inverse env)))
		   (= (length args) 3))
	      (values (first args)
		      (second args)
		      term-at-pos)
	    (values nil nil nil)))
      (values nil nil nil))))

(meth~deffun create-embedded-inverse-term (formula structiterm)
	     (declare (edited  "19-MAR-2003")
		       (authors Ameier)
		       (input   "A formula, and a term of the form '(group-inverse set op x)' which occurs at"
				"least one time as subterm in the formula.")
		       (effect  "None.")
		       (value   "A new variable invx is created and a new formula is computed by replacing each"
				"occurence of '(group-inverse set op x)' in f is replaced by invx. Let's call"
				"this new formula femb. Then the following formula is cretaed and returned as"
				"value:"
				"(exists-sort (lam xinv"
				"                  (and (and (= (op x xinv) (group-unit set op))"
				"                            (= (op xinv x) (group-unit set op))"
                                "                       femb)))"
				"             set)"))
	     (let* ((set (first (data~appl-arguments structiterm)))
		    (op (second (data~appl-arguments structiterm)))
		    (x (third (data~appl-arguments structiterm)))
		    (xinv (term~variable-create 'xi (first (data~abstr-domain (term~type set)))))
		    (femb (data~replace-struct formula structiterm xinv))
		    (env (pds~environment omega*current-proof-plan))
		    (groupu-term (term~appl-create (env~lookup-object 'group-unit env) (list set op)))
		    (inverse-term1 (term~appl-create (env~lookup-object '= env)
						     (list (term~appl-create op (list x xinv))
							   groupu-term)))
		    (inverse-term2 (term~appl-create (env~lookup-object '= env)
						     (list (term~appl-create op (list xinv x))
							   groupu-term)))
		    (and-term (term~appl-create (env~lookup-object 'and env)
						(list (term~appl-create (env~lookup-object 'and env)
									(list inverse-term1 inverse-term2))
						      femb))))

	       (term~appl-create (env~lookup-object 'exists-sort env)
				 (list (term~abstr-create (list xinv)
							  and-term)
				       set))))

















(meth~defcond membed-generalized-function-p (args cmapp)
	      (declare (edited  "19-MAR-2003")
		       (authors Ameier)
		       (input   "A list of arguments and a cmapping <c,mapp>."
				"First argument is a formula, two further arguments are meta-variables.")
		       (effect  "None.")
		       (value   "The function computes all occurences of the generalized functions"
				"plusgen-resclass, minusgen-resclass, and timesgen-resclass in the formula."
				"It replaces a subterm (Xgen-resclass T1 T2) by"
				"(x-resclass (resclass n2 (mod (class-residue T1 n1) n2)) T2), where"
				"n2 is the class-factor of T2 and n1 is the class-factor of T1"
				"(if the function can compute these class-factors)"
				"Moreover, it creates the additional goals that (= (mod n1 n2) 0), that is, that"
				"n2 divides n1, since only then the operation xgen-resclass are well-defined."
				"If there is at least one occurence of xgen-resclass that can be replaced, then"
				"The function returns <C,mapp'> with an extended mapping mapp' where"
				"the first input meta-variable is bound to the changed input formula,"
				"and the second input meta-variable is bound to the set of new goals."))
	      (let* ((formula (first args))
		     (goal-line (second args))
		     (mv-formula (third args))
		     (mv-prems (fourth args)))

		(multiple-value-bind
		    (changed changed-formula new-prems)
		    (methhelp=embed-generalized formula)

		  (if changed
		      (progn
			(meth~mapp-extend-mapp cmapp mv-formula changed-formula)
			(meth~mapp-extend-mapp cmapp mv-prems nil))
		    ;;   (methhelp=new-goals (remove-duplicates new-prems :test #'keim~equal) goal-line)))
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'membed-generalized-function-p)
	      
(defun methhelp=embed-generalized (formula)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (plusgen-obj (env~lookup-object 'plusgen-resclass env))
	 (timesgen-obj (env~lookup-object 'timesgen-resclass env))
	 (minusgen-obj (env~lookup-object 'minusgen-resclass env)))
    
    (if (data~appl-p formula)
	(let* ((func (data~appl-function formula))
	       (args (data~appl-arguments formula)))
	  (cond ((or (keim~equal func plusgen-obj)
		     (keim~equal func timesgen-obj)
		     (keim~equal func minusgen-obj))
		 (multiple-value-bind
		     (changed1 embedded-arg1 prems1)
		     (methhelp=embed-generalized (first args))
		   (multiple-value-bind
		       (changed2 embedded-arg2 prems2)
		       (methhelp=embed-generalized (second args))
		     (let* ((class-factor1 (zmztac=class-factor-of-resclass embedded-arg1))
			    (class-factor2 (zmztac=class-factor-of-resclass embedded-arg2)))
		       (if (and class-factor1
				class-factor2
				(term~number-p class-factor1)
				(term~number-p class-factor2)
				(numberp (keim~name class-factor1))
				(numberp (keim~name class-factor2))
				(= (mod (keim~name class-factor1)
					(keim~name class-factor2))
				   0))
			   (values 't
				   (term~appl-create
				    (cond ((keim~equal func plusgen-obj)
					   (env~lookup-object 'plus-resclass env))
					  ((keim~equal func minusgen-obj)
					   (env~lookup-object 'minus-resclass env))
					  ((keim~equal func timesgen-obj)
					   (env~lookup-object 'times-resclass env)))
				    (list (term~appl-create (env~lookup-object 'resclass env)
					    (list class-factor2
					      (term~appl-create (env~lookup-object 'mod env)
						(list (term~appl-create (env~lookup-object 'class-residue env)
									(list embedded-arg1
									      class-factor1))
						      class-factor2))))
					  embedded-arg2))
				   (append prems1 prems2
					   (list (term~appl-create (env~lookup-object '= env)
						     (list (term~appl-create (env~lookup-object 'mod env)
									     (list class-factor1
										   class-factor2))
							   (post~read-object '0 env :existing-term))))))
			 (values nil formula nil))))))
		(t
		 (do* ((rest-args (reverse args) (rest rest-args))
		       (embedded-args nil)
		       (changed? nil)
		       (new-prems nil))
		     ((null rest-args)
		      (if (null changed?)
			  (values nil formula nil)
			(values 't
				(term~appl-create func embedded-args)
				new-prems)))
		   (let* ((head-arg (first rest-args)))
		     (multiple-value-bind
			 (changed embedded-arg prems)
			 (methhelp=embed-generalized head-arg)
		       (setf embedded-args (cons embedded-arg embedded-args))
		       (when changed
			 (setf changed? 't))
		       (setf new-prems (append prems new-prems))))))))
      (values nil formula nil))))


(meth~defcond mconvertable-residue-class-expressions-p (args cmapp)
	      (declare (edited  "06-MAR-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a formula) and a cmapping <c,mapp>.")
		       (effect  "None.")
		       (value   "<C,mapp> if formula contains a subformula which is a convertable resclass-expression,"
				"<nil,mapp> otherwise."
				"Currently the following expressions are convertable expressions:"
				"1.) A subformula which is an equation on residue classes."
				"2.) A subformula which expresses that a resclass is contained in a set of resclasses."
				"3.) A subformula of the form '(class-residue (op R1 R2) n)' where op is a convertable operator."
				"4.) A subformula of the form '(class-residue (resclass n m) n)'."))
	      
	      (let* ((form (first args))
		     (result (and (null (data~positions form #'logic~existential-quantification-p))
				  (null (data~positions form #'logic~universal-quantification-p))
				  (or
				   ;; checking 1.)
				   (data~positions form #'zmztac=residue-class-equation-p)
				   ;; checking 2.)
				   (data~positions form #'zmztac=resclass-in-resclass-set-p)
				   ;; checking 3.)
				   (zmztac=pushable-class-residue-positions-p form)
				   ;; checking 4.)	
				   (data~positions form #'zmztac=replaceable-class-residue-p)
				   ;; checking 5.)
				   ))))
		
		(if result
		    cmapp
		  (meth~mapp-new-constraint cmapp nil))))

(meth~deffun mconvert-residue-class-expressions (formula)
	     (declare (edited  "06-MAR-2000")
		      (authors Ameier)
		      (input   "A formula containing convertable residue-class expressions"
			       "(see condition mconvertable-residue-class-expressions-p).")
		      (effect  "None.")
		      (value   "A formula where the residue-class expressions are converted as far as possible."))
	     
	     (let* ((formula-after-converting-equations (do* ((rest-positions (data~positions formula #'zmztac=residue-class-equation-p)
									      (rest rest-positions))
							      (current-formula formula))
							    ((null rest-positions)
							     current-formula)
							  (let* ((head-position (first rest-positions)))
							    (setf current-formula 
								  (zmztac=compute-corresponding-class-residue-equation-at-pos
								   current-formula head-position)))))
		    (formula-after-converting-memberships (do* ((rest-positions (data~positions formula-after-converting-equations
												  #'zmztac=resclass-in-resclass-set-p)
										(rest rest-positions))
								(current-formula formula-after-converting-equations))
							      ((null rest-positions)
							       current-formula)
							    (let* ((head-position (first rest-positions)))
							      (setf current-formula
								    (zmztac=compute-class-residue-in-number-set-at-pos 
								     current-formula head-position)))))
		    (formula-after-pushing-class-residues (zmztac=compute-pushed**-line formula-after-converting-memberships))
		    (replaceable-positions (data~positions formula-after-pushing-class-residues #'zmztac=replaceable-class-residue-p))
		    (formula-after-replacing-class-residues (zmztac=compute-replaced-class-residues-term formula-after-pushing-class-residues
													 replaceable-positions)))
	       
	       formula-after-replacing-class-residues))


;; NEXT FUNCTION IS SPECIAL VERSION OF FUNCTION mconvert-residue-class-expression that does not convert
;; directly membership expressions into disjunctions!
(meth~deffun mconvert-residue-class-expressions-pres (formula)
	     (declare (edited  "06-MAR-2000")
		      (authors Ameier)
		      (input   "A formula containing convertable residue-class expressions"
			       "(see condition mconvertable-residue-class-expressions-p).")
		      (effect  "None.")
		      (value   "A formula where the residue-class expressions are converted as far as possible."))
	     
	     (let* ((formula-after-converting-equations (do* ((rest-positions (data~positions formula #'zmztac=residue-class-equation-p)
									      (rest rest-positions))
							      (current-formula formula))
							    ((null rest-positions)
							     current-formula)
							  (let* ((head-position (first rest-positions)))
							    (setf current-formula 
								  (zmztac=compute-corresponding-class-residue-equation-at-pos
								   current-formula head-position)))))
		    (formula-after-converting-memberships (do* ((rest-positions (data~positions formula-after-converting-equations
												  #'zmztac=resclass-in-resclass-set-p)
										(rest rest-positions))
								(current-formula formula-after-converting-equations))
							      ((null rest-positions)
							       current-formula)
							    (let* ((head-position (first rest-positions)))
							      (setf current-formula
								    (methhelp=compute-class-residue-in-number-set-at-pos-pres 
								     current-formula head-position)))))
		    (formula-after-pushing-class-residues (zmztac=compute-pushed**-line formula-after-converting-memberships))
		    (replaceable-positions (data~positions formula-after-pushing-class-residues #'zmztac=replaceable-class-residue-p))
		    (formula-after-replacing-class-residues (zmztac=compute-replaced-class-residues-term formula-after-pushing-class-residues
													 replaceable-positions)))
	       
	       formula-after-replacing-class-residues))

(defun methhelp=compute-class-residue-in-number-set-at-pos-pres (form pos)
  (declare (edited  "02-MAR-2000")
	   (authors Ameier)
	   (input   "A formula of the form '(Resclass-Set resclass)'.")
	   (effect  "None.")
	   (value   "A formula of the form '(Number-Set (class-residue resclass))' where number-set is the"
		    "set of natural numbers corresponding to Resclass-Set."))
  (let* ((formula (data~struct-at-position form pos))
	 (func (data~appl-function formula))
	 (args (data~appl-arguments formula))
	 (resclass-set (if (term~set-p func) func (term~appl-create func (butlast args))))
	 (number-set (zmztac=number-set-to-resclass-set resclass-set))
	 (class-factor (zmztac=class-factor resclass-set))
	 (env (pds~environment omega*current-proof-plan))
	 (new-subformula (term~appl-create (env~lookup-object 'in env)
					   (list (term~appl-create (env~lookup-object :class-residue env)
								   (list (first (last args))
									 class-factor))
						 number-set))))    
    (data~replace-at-position form pos new-subformula)))

(meth~defcond mnummerical-simplify (args cmapp)
  (declare (edited  "08-MAR-2000")
	   (authors Ameier)
	   (input   "A list of args (a term, a term meta-variable, a position, a node, a meta-variable for a list"
		    "of nodes, a meta-variable for a list of lists of positions) and a mapping.")
	   (effect  "The meta-variables in the args are bound.")
	   (value   "<c,mapp'> if the term contains any numerical expressions that were simplified, <nil,mapp> otherwise."
		    "In the case of <c,mapp'> the term meta-variable is bound on the simplified term, the meta-variable"
		    "for a list of nodes is bound on the set of all equations used to simplify the term, and the meta-variable"
		    "for a list of list of positions is bound on the corresponding positions on that the assumptions are applied."
		    "Thereby each position gets as prefix the input position."))
  (let* ((term (first args))
	 (term-mv (second args))
	 (pos (third args))
	 (node (fourth args))
	 (used-supps-mv (fifth args))
	 (positions-mv (sixth args))
	 (supports (pds~node-supports node))
	 (eq-supps (remove-if-not #'(lambda (supp)
				      (let* ((form (node~formula supp)))
					(and (data~appl-p form)
					     (keim~equal (data~appl-function form)
							 (data~schema-range
							  (env~lookup-object '= (pds~environment omega*current-proof-plan))))
					     (= (length (data~appl-arguments form)) 2))))
				  supports)))
    
    (multiple-value-bind
	(simplified-p simplified-term eq-pos-pairs)
	(meth=numerical-simplify term eq-supps pos)

      (if (null simplified-p)

	  (meth~mapp-new-constraint cmapp nil)

	(multiple-value-bind
	    (used-equations applied-positions)
	    (meth=merge-nodes-and-positions eq-pos-pairs)

	  (progn
	    (meth~mapp-extend-mapp cmapp term-mv simplified-term)
	    (meth~mapp-extend-mapp cmapp used-supps-mv used-equations)
	    (meth~mapp-extend-mapp cmapp positions-mv applied-positions)))))))

(meth~new-relational-function 'mnummerical-simplify)

(defun meth=numerical-simplify (term eq-supps pos)
  (declare (edited  "27-MAR-2000" "09-MAR-2000")
	   (authors Sorge Ameier)
	   (input   "A term, usable support nodes with equations, and the current-position of the term (as subterm)"
		    "in another term).")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: A flag (t/nil) signing whether subterms of the term could be simplified."
		    "Second: The simplified term."
		    "Third: A list of pairs of a equation node and a position, respectively."
		    "This function tries to simplify numerical expressions, e.g., 2 + 3 is simplified to 5 etc."
		    "Furthermore, it applies equation of the form 'c = number' if c found as subterm."
		    "If a equation is applied a pair of the applied equation and the position in the term is"
		    "stored in the third value."))
  
  (cond ((data~appl-p term)
	 (let* ((function (data~appl-function term))
		(args (data~appl-arguments term)))
	   
	   (multiple-value-bind
	       (simplified-p simplified-term-list eq-pos-pairs)
	       (meth=numerical-simplify-args args eq-supps pos)
	     
	     (if (and (term~primitive-p function)
		      (not (term~special-p function))
		      (find (keim~name function) natac*function-list :test #'string-equal)
		      (= (length args) 2)
		      (term~number-p (first simplified-term-list))
		      (term~number-p (second simplified-term-list)))
		 
		 (let* ((value1 (keim~name (first simplified-term-list)))
			(value2 (keim~name (second  simplified-term-list))))
		   
		   (values 't
			   (natac~compute function
					   simplified-term-list
					   (pds~environment omega*current-proof-plan))
			   eq-pos-pairs))
	       (if simplified-p
		   (let* ((new-term (term~appl-create function simplified-term-list)))
		     (values 't
			     new-term
			     eq-pos-pairs))
		 (values nil
			 term
			 nil))))))
	((term~constant-p term)
	 (if (term~number-p term)
	     (values nil
		     term
		     nil)
	   (let* ((eq-supp-for-const (find term eq-supps :test #'(lambda (const eq-node)
								   (let* ((form (node~formula eq-node)))
								     (find const (data~appl-arguments form) :test #'keim~equal))))))
	     
	     (if (null eq-supp-for-const)
		 (values nil
			 term
			 nil)
	       (let* ((equation-args (data~appl-arguments (node~formula eq-supp-for-const)))
		      (subst-term (if (keim~equal term (first equation-args))
				      (second equation-args)
				    (first equation-args))))
		 (if (term~number-p subst-term)
		     (values 't
			     subst-term
			     (list (list eq-supp-for-const pos)))
		   (values nil
			   term
			   nil)))))))
	((term~abstr-p term)
	 (multiple-value-bind
	     (simplified-p simplified-range eq-pos-pairs)
	     (meth=numerical-simplify (data~abstr-range term) eq-supps (pos~add-end 0 pos))
	   (values simplified-p
		   (if simplified-p
		       (term~abstr-create (data~abstr-domain term) simplified-range)
		     term)
		   eq-pos-pairs)))
	(t
	 (values nil
		 term
		 nil))))

  
(defun meth=numerical-simplify-args (term-list eq-supps pos)
  (do* ((rest-term-list term-list (rest rest-term-list))
	(current-pos-number 1 (incf current-pos-number))
	(simplified-flag nil)
	(simplified-terms nil)
	(eq-pos-pairs nil))
      ((null rest-term-list)
       (values simplified-flag
	       simplified-terms
	       eq-pos-pairs))
    (let* ((head-term (first rest-term-list))
	   (head-pos (pos~add-end current-pos-number pos)))

      (multiple-value-bind
	  (head-simplified-p head-simplified-term head-eq-pos-pairs)
	  (meth=numerical-simplify head-term eq-supps head-pos)
	
	(when head-simplified-p
	  (setf simplified-flag 't))
	(setf simplified-terms (append simplified-terms (list head-simplified-term)))
	(setf eq-pos-pairs (append eq-pos-pairs head-eq-pos-pairs))))))


(defun meth=merge-nodes-and-positions (eq-pos-pairs)
  (do* ((rest-eq-pos-pairs eq-pos-pairs)
	(back-nodes-list nil)
	(back-poslistlist nil))
      ((null rest-eq-pos-pairs)
       (values back-nodes-list
	       back-poslistlist))
    (let* ((head-pair (first rest-eq-pos-pairs))
	   (head-node (first head-pair))
	   (all-pairs-with-node (remove-if-not #'(lambda (pair)
						   (eq (first pair) head-node))
					       rest-eq-pos-pairs))
	   (all-pairs-without-node (remove-if #'(lambda (pair)
						  (eq (first pair) head-node))
					      rest-eq-pos-pairs))
	   (all-posses (mapcar #'second all-pairs-with-node)))

      (setf rest-eq-pos-pairs all-pairs-without-node)
      (setf back-nodes-list (cons head-node back-nodes-list))
      (setf back-poslistlist (cons all-posses back-poslistlist)))))


(meth~defcond mmodpop-p (args cmapp)
  (declare (edited  "15-MAR-2000")
	   (authors Ameier)
	   (input   "A list of args (two term, two metavariables for terms, a node, and a metavariable for a node.)"
		    "and a mapping.")
	   (effect  "The meta-variebles in the args are bound in the mapping.")
	   (value   "<c,mapp'> if the two terms can be simplified by poping each mod in them, otherwise <nil,mapp>."
		    "In the case of <c,mapp'> the two meta variables for terms are bound to the simplified terms"
		    "respecyively."))
  (let* ((term1 (first args))
	 (term2 (second args))
	 (term1-mv (third args))
	 (term2-mv (fourth args))
	 (node (fifth args))
	 (supports (pds~node-supports node))
	 (assump-mv (sixth args))
	 (mod-obj (env~lookup-object 'mod (pds~environment omega*current-proof-plan))))
    
    (multiple-value-bind
	(simplified1 simp-term1)
	(methhelp=popmods term1 nil)
      (multiple-value-bind
	  (simplified2 simp-term2)
	  (methhelp=popmods term2 nil)
	
	(cond ((and simplified1 simplified2)
	       (meth~mapp-extend-mapp cmapp term1-mv simp-term1)
	       (meth~mapp-extend-mapp cmapp term2-mv simp-term2))
	      ((and ;; simplified1
		    (data~appl-p simp-term1)
		    (keim~equal (data~appl-function simp-term1) mod-obj)
		    (term~constant-p term2))
	       
	       (let* ((modfac-number (keim~name (second (data~appl-arguments simp-term1))))
		      (or-disj-supp (if (not (term~number-p term2))
					(find term2 supports
					      :test #'(lambda (const node)
							(methhelp=or-disj-of-const-and-lesser-numbers-p const
													(node~formula node)
													modfac-number)))
				      nil)))
	     
		 (if (or or-disj-supp
			 (and (term~number-p term2)
			      (< (keim~name term2) modfac-number)
			      (integerp (keim~name term2))
			      (>= (keim~name term2) 0))) 
		     (progn (when or-disj-supp
			      (meth~mapp-extend-mapp cmapp assump-mv or-disj-supp))
			    (meth~mapp-extend-mapp cmapp term1-mv simp-term1)
			    (meth~mapp-extend-mapp cmapp term2-mv (term~appl-create mod-obj
										    (list term2
											  (second (data~appl-arguments simp-term1))))))
		   (meth~mapp-new-constraint cmapp nil))))	
	      ((and ;;simplified2
		    (data~appl-p simp-term2)
		    (keim~equal (data~appl-function simp-term2) mod-obj)
		    (term~constant-p term1))
	       
	       (let* ((modfac-number (keim~name (second (data~appl-arguments simp-term2))))
		      (or-disj-supp (if (not (term~number-p term1))
					(find term1 supports
					      :test #'(lambda (const node)
							(methhelp=or-disj-of-const-and-lesser-numbers-p const
													(node~formula node)
													modfac-number)))
				      nil)))
	     
		 (if (or or-disj-supp
			 (and (term~number-p term1)
			      (< (keim~name term1) modfac-number)
			      (integerp (keim~name term1))
			      (>= (keim~name term1) 0))) 
		     (progn (when or-disj-supp
			      (meth~mapp-extend-mapp cmapp assump-mv or-disj-supp))
			    (meth~mapp-extend-mapp cmapp term1-mv (term~appl-create mod-obj
										    (list term1
											  (second (data~appl-arguments simp-term2)))))
			    (meth~mapp-extend-mapp cmapp term2-mv simp-term2))
		   (meth~mapp-new-constraint cmapp nil))))
	      (t
	       (meth~mapp-new-constraint cmapp nil)))))))

(meth~new-relational-function 'mmodpop-p)

(defun methhelp=or-disj-of-const-and-lesser-numbers-p (const formula number)
  (declare (edited  "15-MAR-2000")
	   (authors Ameier)
	   (input   "A constant, a formula, and a number.")
	   (effect  "None.")
	   (value   "True if the formula has the form:"
		    "(OR (const = n1) ...), such that each ni is a natural number lesser"
		    "than number."))
  (cond ((logic~disjunction-p formula)
	 (and (methhelp=or-disj-of-const-and-lesser-numbers-p const (first (data~appl-arguments formula)) number)
	      (methhelp=or-disj-of-const-and-lesser-numbers-p const (second (data~appl-arguments formula)) number)))
	((and (data~appl-p formula)
	      (keim~equal (data~appl-function formula)
			  (data~schema-range (env~lookup-object '= (pds~environment omega*current-proof-plan)))))
	 (let* ((args (data~appl-arguments formula)))
	   (if (and (find const args :test #'keim~equal)
		    (remove const args :test #'keim~equal))
	       (let* ((other-arg (first (remove const args :test #'keim~equal)))
		      (name-number (keim~name other-arg)))
		 (if (and (term~number-p other-arg)
			  (< name-number number)
			  (integerp name-number)
			  (>= name-number 0))
		     't
		   nil))
	     nil)))
	(t
	 nil)))	      

(defgeneric methhelp=popmods (term internal)
  (declare (edited  "17-MAR-2000")
	   (authors Ameier)
	   (input   "A term and a flag internal, signing whether the term is a subterm of a mod expresssion"
		    "(it is in the range of a mod) if the term is not in the range of a mod, then internal is nil"
		    "otherwise is the mod factor expression.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A flag signing whether the term was simplified." 
		    "Second: A term in which mod expressions of numerical subterms (= subterms consisting of plus,minus,times,div?)"
		    "        are poped to the outmost possible position."
		    "        Conceptually there are applied rewrite rules like (a mod n + b mod n) mod n = (a + b) mod n."))
  (:method ((term term+appl) internal)
	   (cond ((keim~equal (data~appl-function term)
			      (env~lookup-object 'mod (pds~environment omega*current-proof-plan)))
		  ;; Term is modulo term -> following modulos are internal to this modulo factor
		  (let* ((modulo-factor (second (data~appl-arguments term))))
		    (multiple-value-bind
			(simplified simp-term)
			(methhelp=popmods (first (data~appl-arguments term)) modulo-factor)
		      (if (and internal
			       (or (keim~equal internal modulo-factor)
				   (and (term~number-p internal)
					(term~number-p modulo-factor)
					(numberp (keim~name internal))
					(numberp (keim~name modulo-factor))
					(= (mod (keim~name modulo-factor)
						(keim~name internal))
					   0))))
			  ;; forget following modulos if they are wrt. the same modulo-factor
			  ;; or if the outer modulo (=internal) divides the inner one
			  (values 't
				  simp-term)
			(values simplified
				(term~appl-create (data~appl-function term) ;; = mod
						  (list simp-term modulo-factor)))))))
		 (t
		  (if (and internal
			   (find (data~appl-function term)
				 (list (env~lookup-object 'plus (pds~environment omega*current-proof-plan))
				       (env~lookup-object 'minus (pds~environment omega*current-proof-plan))
				       (env~lookup-object 'times (pds~environment omega*current-proof-plan))
				       (env~lookup-object 'div (pds~environment omega*current-proof-plan)))
				 :test #'keim~equal))
		      ;; inside of numerical expressions the external mods are inherited
		      (multiple-value-bind
			  (simplified1 simplified-arg1)
			  (methhelp=popmods (first (data~appl-arguments term)) internal)
			(multiple-value-bind
			    (simplified2 simplified-arg2)
			    (methhelp=popmods (second (data~appl-arguments term)) internal)
			  (values (or simplified1 simplified2)
				  (term~appl-create (data~appl-function term)
						    (list simplified-arg1 simplified-arg2)))))
		    ;; if the term is no numerical expresion external mods are not inherited 
		    (multiple-value-bind
			(simplified-p simplified-args)
			(do* ((rest-args (data~appl-arguments term) (rest rest-args))
			      (back-args nil)
			      (simplified nil))
			    ((null rest-args)
			     (values simplified
				     back-args))
			  (let* ((head-arg (first rest-args)))
			    (multiple-value-bind
				(arg-simpified-p simplified-arg)
				(methhelp=popmods head-arg nil)
			      (setf back-args (append back-args (list simplified-arg)))
			      (when arg-simpified-p
				(setf simplified 't)))))
		      (values simplified-p
			      (term~appl-create (data~appl-function term)
						simplified-args)))))))
  (:method ((term term+term) internal)
	   (values nil
		   term)))
		
(meth~defcond equal-with-maple-p (args cmapp)
  (declare (edited  "15-MAR-2000")
	   (authors Sorge)
	   (input   "A list of two arguments (two terms) and a mapping.")
	   (effect  "Calls Maple.")
	   (value   "<T,cmapp> if the terms form a solvable equation, o/w <NIL,cmapp>"))
  (if (methhelp=mod-function? (first args) (second args) 'or)
      (meth~mapp-new-constraint cmapp nil)
    (let* ((farg (first args))
	   (sarg (second args))
	   (phi (string-downcase (post~string farg)))
	   (psi (string-downcase (post~string sarg)))
	   (equation (concatenate 'string "(= " (string-downcase phi) " " (string-downcase psi) ")"))
	   (primitives (remove-duplicates (append (methhelp=get-primitives farg)
						  (methhelp=get-primitives sarg))))
	   (mv-list (remove-if-not #'meta~p primitives))
	   (result (methhelp=solve-with-maple equation primitives)))
      (if (or (null result) (string-equal result "Error"))
	  (meth~mapp-new-constraint cmapp nil)
	(let* ((eq-list (rcl=dissect-maple-hint result))
	       (mv-strings (mapcar #'(lambda (mv) (find (keim~name mv) eq-list :test #'string-equal :key #'car)) mv-list))
	       (const-list (remove-if-not #'term~constant-p primitives))
	       (const-strings (mapcar #'(lambda (mv) (find (keim~name mv) eq-list :test #'string-equal :key #'car)) const-list)))
	  (if (every #'(lambda (x) (string-equal (first x) (second x))) const-strings)
	      (meth~mapp-new-constraint
	       cmapp
	       (cstr~conjunction (methhelp=make-cstr-bindings mv-list (mapcar #'second mv-strings))))
	    (meth~mapp-new-constraint cmapp nil)))
	))))

(defun methhelp=solve-with-maple (equation primitives)
  (let* ((mv-list (remove-if-not #'meta~p primitives))
	 (mv-string (methhelp=make-primitive-list mv-list)))
    (rcl~call-maple (list "solve" equation mv-string) :syntax 'post2maple)))

#+old(meth~defcond equal-modulo-with-maple-p (args cmapp)
  (declare (edited  "15-MAR-2000")
	   (authors Sorge)
	   (input   "A list of three arguments (two terms and a mod factor) and a mapping.")
	   (effect  "Calls Maple.")
	   (value   "<T,cmapp> if the terms form a solvable equation, o/w <NIL,cmapp>"))
  (let* ((farg (first args))
	 (sarg (second args))
	 (mod-value (third args)))
    (if (or (methhelp=contains-mod-function farg)
	    (methhelp=contains-mod-function sarg)
	    (not (numberp (keim~name mod-value))))
	(meth~mapp-new-constraint cmapp nil)
      (let* ((phi (string-downcase (post~string farg)))
	     (psi (string-downcase (post~string sarg)))
	     (equation (concatenate 'string "(= " (string-downcase phi) " " (string-downcase psi) ")"))
	     (primitives (remove-duplicates (append (methhelp=get-primitives farg)
						    (methhelp=get-primitives sarg))))
	     (mv-list (remove-if-not #'meta~p primitives))
	     (result (methhelp=msolve-with-maple equation primitives mod-value)))
	(if (or (null result) (string-equal result "Error"))
	    (meth~mapp-new-constraint cmapp nil)
	  (let* ((eq-list (rcl=dissect-maple-hint result))
		 (dummy (print eq-list))
		 (mv-strings (mapcar #'(lambda (mv) (find (keim~name mv) eq-list :test #'string-equal :key #'car)) mv-list))
		 (const-list (remove-if-not #'term~constant-p primitives))
		 (const-strings (mapcar #'(lambda (mv) (find (keim~name mv) eq-list :test #'string-equal :key #'car)) const-list)))
	    (if (every #'(lambda (x) (string-equal (first x) (second x))) const-strings)
		(meth~mapp-new-constraint
		 cmapp
		 (cstr~conjunction (methhelp=make-cstr-bindings mv-list (mapcar #'second mv-strings) mod-value)))
	      (meth~mapp-new-constraint cmapp nil))))))))

(meth~defcond equal-modulo-with-maple-p (args cmapp)
  (declare (edited  "15-MAR-2000")
	   (authors Sorge)
	   (input   "A list of three arguments (two terms and a mod factor) and a mapping.")
	   (effect  "Calls Maple.")
	   (value   "<T,cmapp> if the terms form a solvable equation, o/w <NIL,cmapp>"))
  (let* ((farg (first args))
	 (sarg (second args))
	 (mod-value (third args)))
    (if (or (methhelp=contains-mod-function farg)
	    (methhelp=contains-mod-function sarg)
	    (not (numberp (keim~name mod-value))))
	(meth~mapp-new-constraint cmapp nil)
      (let* ((phi (string-downcase (post~string farg)))
	     (psi (string-downcase (post~string sarg)))
	     (equation (concatenate 'string "(= " (string-downcase phi) " " (string-downcase psi) ")"))
	     (primitives (remove-duplicates (append (methhelp=get-primitives farg)
						    (methhelp=get-primitives sarg))))
	     (result (methhelp=msolve-with-maple equation primitives mod-value)))
	(cond ((or (null result) (string-equal result "Error"))
	       (meth~mapp-new-constraint cmapp nil))
	      ((eq result t)
	       (meth~mapp-new-constraint cmapp t))
	      (t (let* ((eq-list (rcl=dissect-maple-hint result))
			(mv-list (remove-if-not #'meta~p primitives))
			(mv-strings (mapcar #'(lambda (mv)
						(second (find (keim~name mv) eq-list :test #'string-equal :key #'car)))
					    mv-list)))
		   (meth~mapp-new-constraint
		    cmapp
		    (cstr~conjunction (methhelp=make-cstr-bindings mv-list mv-strings (keim~name mod-value)))))))))))

(defun maple~apply   (method) (serv~apply   "MAPLE" method :signal-errors nil :timeout 300))

(defun methhelp=msolve-with-maple (equation primitives mod-value)
  (let ((mv-list (remove-if-not #'meta~p primitives))
	(prim-list (remove-if #'meta~p primitives))
	(fres (rcl~call-maple (list "msolve" equation (format nil "~A" mod-value)) :syntax 'post2mapleallsolutions)))
    ;(setq class*msolve-check-line (list equation rcl*check-line mod-value))
    (unless (or (null fres) (string-equal fres "Error"))
      (let ((sols (rcl=split2single-solutions (rcl=dissect-maple-hint fres))))
	(multiple-value-bind (var-sols meta-var-sols)
	    (methhelp=separate-metavariable-solutions sols mv-list)
	  ;;;
	  (when (and
		 (not (and (null var-sols) (null meta-var-sols)))
		 (or
		 (every #'null var-sols)
		    (methhelp=check-solution-array
		     (methhelp=create-solution-array
		      (remove-if #'null var-sols) prim-list meta-var-sols (keim~name mod-value)))))
	    (if mv-list
		(let* ((var-equs (format nil "{~{~A~:* = ~A ,~}" prim-list))
		       (mv-equs (mapcar #'(lambda (x) (format nil "~A = ~A" (car x) (cadr x)))
					meta-var-sols)) ;; here we should again go over the solutions and rewrite them...
		       (equs (format nil "~A ~{~A~^, ~}}" var-equs  mv-equs))
		       (mv-string (methhelp=make-primitive-list mv-list)))
		  (rcl~call-maple (list "solve" equs mv-string) :syntax 'maple2maple))
	      t)))))))
	
(defun methhelp=separate-metavariable-solutions (solutions meta-vars)
  (declare (edited  "08-SEP-2000")
	   (authors Sorge)
	   (input   "A list of solutions and a list of meta-variables.")
	   (effect  "None.")
	   (value   "Two values: The first is a list of solutions where the equations for the meta-variables"
		    "have been removed and the second is a list of those removed meta-variable equations."))
  (flet ((rewrite-variable (x)
			   (etypecase x
			     (keim+name (keim~name x))
			     (string x)
			     (symbol x))))
    (labels ((clean-solution (sol)
			     (when sol
			       (multiple-value-bind (new-sol mvs)
				   (clean-solution (cdr sol))
				 (cond ((find (caar sol) meta-vars :test #'string-equal :key #'rewrite-variable)
					(values new-sol (cons (car sol) mvs)))
				       ((methhelp=tree-find (read-from-string (rcl~maple2post (cadar sol)))
						   (mapcar #'rewrite-variable meta-vars))
					(values (cons (car sol) new-sol) (cons (car sol) mvs)))
				       (t (values (cons (car sol) new-sol) mvs)))))))
      (when solutions
	(multiple-value-bind (new-solutions new-mvs)
	    (methhelp=separate-metavariable-solutions (cdr solutions) meta-vars)
	  (multiple-value-bind (fsol fmv)
	      (clean-solution (car solutions))
	    (values (cons fsol new-solutions)
		    (append fmv new-mvs))))))))

(defun methhelp=tree-find (tree list)
  (declare (edited  "04-OCT-2000")
	   (authors Sorge)
	   (input   "A tree and a list.")
	   (effect  "None.")
	   (value   "True if an element of the tree is also an element of the list."))
  (cond ((null tree) nil)
	((consp tree)
	 (or (methhelp=tree-find (car tree) list)
	     (methhelp=tree-find (cdr tree) list)))
	(t (find tree list :test #'(lambda (x y)
				     (string-equal (format nil "~A" x)
						   (format nil "~A" y)))))))

(defun methhelp=check-solution-array (array)
  (declare (edited  "08-SEP-2000")
	   (authors Sorge)
	   (input   "An array.")
	   (effect  "None.")
	   (value   "T if all elements in the array are T, otherwise NIL."))
  (let ((number (apply #'* (array-dimensions array))))
    (dotimes (n number)
      (unless (eq (row-major-aref array n) t)
	(return-from methhelp=check-solution-array)))
    t))

(defun methhelp=create-solution-array (solutions variables meta-vars mod-value)
  (declare (edited  "08-SEP-2000")
	   (authors Sorge)
	   (input   "A list of solutions, a list of variables, a list of meta-variables, and an integer.")
	   (effect  "None.")
	   (value   "The solution array."))
  (flet ((rewrite-variable (x)
			   (etypecase x
			     (keim+name (keim~name x))
			     (string x)
			     (symbol x))))
    (let* ((array (make-array (make-list (length variables) :initial-element mod-value)))
	   (resvars (sort (mapcar #'rewrite-variable variables) #'string-lessp))
	   ;;;(resmvs (mapcar #'rewrite-variable meta-vars)))
	   )
      (methhelp=enter-solutions
       (methhelp=sort-solutions solutions) array resvars meta-vars mod-value))))

(defun methhelp=enter-solutions (solutions array variables meta-vars mod-value)
  (declare (edited  "08-SEP-2000")
	   (authors Sorge)
	   (input   "A sorted list of solutions, an array, a list of variables,"
		    "a list of metavariables, and an integer.")
	   (effect  "None.")
	   (value   "The array where the solutions are entered in the correct way."))
  (if solutions
    (methhelp=enter-single-solution (car solutions)
				    (methhelp=enter-solutions
				     (cdr solutions) array variables meta-vars mod-value)
				    variables
				    meta-vars
				    mod-value)
    array))

(defun methhelp=enter-single-solution (solution array variables meta-vars mod-value)
  (declare (edited  "08-SEP-2000")
	   (authors Sorge)
	   (input   "A single solutions, that is a list of pairs, an array, a list of varibles,"
		    "a list of meta-variables, and an integer.")
	   (effect  "None.")
	   (value   "The array where the single solution has been entered."))
  (when (methhelp=valid-solution solution variables meta-vars)
    (dolist (x (methhelp=make-integer-list-from-solution solution variables meta-vars mod-value))
      (setf (apply #'aref (cons array x)) t)))
  array)

(defun methhelp=make-integer-list-from-solution (solution variables meta-vars mod-value)
  (declare (edited  "08-SEP-2000")
	   (authors Sorge)
	   (input   "A solution, a list of variables, and an integer.")
	   (effect  "None.")
	   (value   "A list of integer lists, corresponding to the positions in the solution array."))
  (flet (;; make an integer-list of a single solution
	 (make-integers (sol)
			;;(format t "~%0: Sol: ~A" sol)
			(let ((res (cadr sol)))
			  (cond ((integerp (read-from-string res))
				 (list (read-from-string res)))
				((string-equal (car sol) (cadr sol))
				 (rcl=make-element-list mod-value t))
				((find res variables :test #'string-equal)
				 (list res))
				(t (omega~error
				    ";;;METHHELP=MAKE-INTEGER-LIST-FROM-SOLUTIONS: Here should not go anything wrong!"))))))
    (labels (;; recursively make integerlists of the single solutions
	     (make-int-lists (sol)
 			     ;;(format t "~%1: Sol: ~A" sol)
			     (if (cdr sol)
				 (mapcan #'(lambda (new-sol) 
					     (mapcar #'(lambda (new-int) (cons new-int new-sol))
						     (make-integers (car sol))))
					 (make-int-lists (cdr sol)))
			       (mapcar #'list (make-integers (car sol)))))
	     ;; Get a value for a variable via the values of other variables
	     ;; Example: '((x1 2) (x2 x1) (x3 x2)), Value of x3 is 2
	     (get-real-result (var sols)     
			      ;;(format t "~%2: Var: ~A  Sols: ~A" var sols)
			      (if (stringp var)
				  (let ((pos (position var variables :test #'string-equal)))
				    (get-real-result (nth pos sols) sols))
				var))
	     ;; Results where the value of a variable refers to another variable are rewritten to
	     ;; the actual value.
	     (clean-final-result (sol orig-sol)
				 ;;(format t "~%3: Sol: ~A  Orig-Sol: ~A" sol orig-sol)
				 (when sol
				   (cons (get-real-result (car sol) orig-sol)
					 (clean-final-result (cdr sol) orig-sol))))
	     ;; If a variable does not have a value in a given solution it is considered as a degree
	     ;; of liberty and is instantiated with all possible solution in the following function.
	     (complete-solutions (sols &optional (vars variables))
				 ;;(format t "~%4: Sols: ~A  Vars: ~A" sols vars)
				 (when vars
				   (if (string-equal (car vars) (caar sols))
				       (cons (car sols)
					     (complete-solutions (cdr sols) (cdr vars)))
				     (let ((new-var (format nil "~A" (car vars))))
				     (cons (list new-var new-var)
					   (complete-solutions sols (cdr vars)))))))
	     ;; Each variable having a meta-variable as result is considered as a degree of liberty.  The
	     ;; value of all remaining variables with the same meta-variable as result will get the original
	     ;; value (i.e. the values of the variable that had the meta-variable as solution first) as
	     ;; solution.
	     (remove-meta-variables (sols &optional (meta-map nil))
				    ;;(format t "~%5: Sols: ~A  Meta-Map: ~A" sols meta-map)
				 (when sols
				   (let* ((psol (car sols))
					  (var-psol (car psol))
					  (var-reference (assoc psol meta-map :test #'equal)))
				     (cond ((and (find psol meta-vars :test #'equal)
						 var-reference)
					    (cons (list var-psol (cdr var-reference))
						  (remove-meta-variables (cdr sols) meta-map)))
					   ((find psol meta-vars :test #'equal)
					    (cons (list var-psol var-psol)
						  (remove-meta-variables (cdr sols)
									 (acons psol var-psol meta-map))))
					   (t (cons psol (remove-meta-variables (cdr sols) meta-map))))))))
      (mapcar #'(lambda (x) (clean-final-result x x))
	      (make-int-lists (complete-solutions (remove-meta-variables solution)))))))


(defun methhelp=valid-solution (solution variables meta-vars)
  (declare (edited  "08-SEP-2000")
	   (authors Sorge)
	   (input   "A list of pairs and two lists of symbols")
	   (effect  "None.")
	   (value   "T if the list forms a valid solution, i.e. is of the form"
		    "'((\"x\" \"x\") (\"y\" \"1\") (\"z\" \"mv\")) etc., where mv is in META-VARS."
		    "Otherwise NIL."))
  (if solution
      (let* ((sol (car solution))
	     (fsol (car sol))
	     (ssol (cadr sol)))
	(and (or (string-equal fsol ssol)
		 (integerp (read-from-string ssol))
		 (find ssol variables :test #'string-equal)
		 (find sol meta-vars :test #'equal))
	     (methhelp=valid-solution (cdr solution) variables meta-vars)))
    t))
  
  
(defun methhelp=sort-solutions (solutions)
  (declare (edited  "23-AUG-2000")
	   (authors Sorge)
	   (input   "A list of lists of pairs.")
	   (effect  "None.")
	   (value   "A list of lists of pairs sorted alphabetically wrt. to the first element of each pair.")
	   (example "Input: '(((x 1) (y y) (z 3)) ((y y) (z 2) (x 0)))"
		    "Output: '(((x 1) (y y) (z 3)) ((x 0) (y y) (z 2)))"))
  (when solutions
    (cons
     (sort (car solutions) #'string-lessp :key #'car)
     (methhelp=sort-solutions (cdr solutions)))))

(defun methhelp=group-solutions (solutions)
  (declare (edited  "23-AUG-2000")
	   (authors Sorge)
	   (input   "A list of lists of pairs.")
	   (effect  "None.")
	   (value   "A list of lists each containing pairs with the same first element.")
	   (example "Input: '(((x 1) (y y) (z 3)) ((x 0) (y y) (z 2)))"
		    "Output: '(((x 1) (x 0)) ((y y) (y y)) ((z 3) (z 2)))"))
  (if (cdr solutions)
      (methhelp=merge-pairs-in-list (car solutions)
				    (methhelp=sort-solutions (cdr solutions)))
    (mapcar #'list (car solutions))))

(defun methhelp=merge-pairs-in-list (pairs list)
  (declare (edited  "23-AUG-2000")
	   (authors Sorge)
	   (input   "A list of pairs and a list of lists of pairs.")
	   (effect  "None.")
	   (value   "The single pairs are merged into the lists wrt. to their first element.")
	   (example "Input: '((x 1) (y 2)) '(((x 0) (x 3)) ((y 1) (y 0)))"
		    "Output: '(((x 1) (x 0) (x 3)) ((y 2) (y 1) (y 0)))"))
  (if pairs
    (mapcar #'(lambda (pair-list)
		(if (equal (caar pairs) (caar pair-list))
		    (cons (car pairs) pair-list)
		  pair-list))
	    (methhelp=merge-pairs-in-list (cdr pairs) list))
    list))
  
;;; old working version
#+old(defun methhelp=msolve-with-maple (equation primitives mod-value)
  (let* ((mv-list (remove-if-not #'meta~p primitives))
	(mv-string (methhelp=make-primitive-list mv-list))
	(fres (rcl~call-maple (list "msolve" equation (format nil "~A" mod-value)) :syntax 'post2maple)))
    (rcl~call-maple (list "solve" fres mv-string) :syntax 'maple2maple)))

;;(rcl~call-maple (list "solve" fres mv-string) :syntax 'maple2maple))
;;(rcl~call-maple (list "solve" fres mv-string) :syntax 'maple2maple)))

(defun methhelp=make-cstr-bindings (mvs terms &optional (mod-value nil))
  (declare (edited  "18-APR-2000")
	   (authors Sorge)
	   (input   "A list of metavariables and a list of string representing terms."
		    "Optionally an integer.")
	   (effect  "Creates some new constraint-bindings.")
	   (value   "A list of constraint bindings."))
  (flet ((make-term (term)
		    (let ((nterm (read-from-string (rcl~maple2post term)))
			  (env (pds~environment omega*current-proof-plan)))
		      (cond ((and (numberp nterm) (numberp mod-value))
			     (post~read-object (mod nterm mod-value) env :existing-term))
			    ((numberp nterm)
			     (post~read-object nterm env :existing-term))
			    ((numberp mod-value)
			     (post~read-object (list 'mod nterm mod-value) env :existing-term))
			    (t (post~read-object nterm env :existing-term))))))
    (mapcar #'(lambda (mv term)
		(cstr~binding-create (list mv (make-term term))))
	    mvs terms)))

(defgeneric methhelp=contains-mod-function (term)
  (declare (edited  "07-SEP-2000")
	   (authors Sorge)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T if term contains a mod function, otherwise NIL."))
  (:method ((term term+constant))
	   (when (string-equal (keim~name term) 'mod) t))
  (:method ((term term+number))
	   nil)
  (:method ((term term+term))
	   nil)
  (:method ((term term+appl))
	   (let ((func (data~appl-function term))
		 (args (data~appl-arguments term)))
	     (or (methhelp=contains-mod-function func)
		 (some #'methhelp=contains-mod-function args))))
  (:method ((term term+abstr))
	   (methhelp=contains-mod-function (data~abstr-scope term)))
  )

(defun methhelp=mod-function? (term1 term2 &optional (junctor 'and))
  (eval (list junctor
	      (and (term~appl-p term1)
		   (string-equal (keim~name (data~appl-function term1)) :mod))
	      (and (term~appl-p term2)
		   (string-equal (keim~name (data~appl-function term2)) :mod)))))

(defun methhelp=mod-value (term1 term2)
  (when (methhelp=mod-function? term1 term2)
    (let ((modv1 (second (data~appl-arguments term1)))
	  (modv2 (second (data~appl-arguments term2))))
      (and (term~number-p modv1)
	   (term~number-p modv2)
	   (data~equal modv1 modv2)
	   (keim~name modv1)))))

(defun methhelp=make-primitive-list (primitives)
  (if (null primitives)
      "{dummy}"
    (do* ((current-string (format nil "{~A" (keim~name (first primitives))))
	  (rest-primitives (rest primitives) (rest rest-primitives)))
	((null rest-primitives)
	 (string-downcase (format nil "~A}" current-string)))
      (setf current-string (format nil "~A,~A" current-string (keim~name (first rest-primitives)))))))

(defgeneric methhelp=get-primitives (term)
  (:method ((term term+appl))
	   (remove-duplicates (apply #'append (mapcar #'methhelp=get-primitives (data~appl-arguments term)))))
  (:method ((term term+number))
	   nil)
  (:method ((term term+constant))
	   (list term))
  (:method ((term term+variable))
	   (list term))) 

(meth~defcond mexpression-closed-under (args cmapp)
  (declare (edited  "22-MAR-2000")
	   (authors Ameier)
	   (input   "A list of args and a cmapping, the args are:")
	   (effect  "The metavaiable args are bound during the application of the condition.")
	   (value   "A cmapping."))

  (let* ((formula (first args))
	 (goal-node (second args))
	 (formula-mv (third args))
	 (assumps-mv (fourth args))
	 (supports (pds~node-supports goal-node))
	 (env (pds~environment omega*current-proof-plan)))
    
    (multiple-value-bind
	(success1 number-set expr)
	(methhelp=disjunction-of-mod-expr-p formula)
      
      (if (and success1
	       (data~appl-p expr)
	       (keim~equal (data~appl-function expr)
			   (env~lookup-object 'mod env)))
	  
	  (multiple-value-bind
	      (success2 resclass-function replace-pairs)
	      (methhelp=convert-mod-expr (first (data~appl-arguments expr))
					 nil
					 (second (data~appl-arguments expr))
					 1)
	    
	    (if (and success2
		     (<= (length replace-pairs) 2))
		(let* ((depending-consts (mapcar #'first replace-pairs))
		       (according-assumps (mapcar #'(lambda (const)
						      (find const supports
							    :test #'(lambda (co node)
								      (methhelp=or-disj-with-const-and-number-set-p
								       (node~formula node)
								       co
								       number-set))))
						  depending-consts)))
		  (if (null (member nil according-assumps))
		      (let* ((resclass-set (methhelp=convert-number-set number-set (second (data~appl-arguments expr))))
			     (resclass-op (cond ((= (length replace-pairs) 2)
						 (term~abstr-create (mapcar #'second replace-pairs) resclass-function))
						((= (length replace-pairs) 1)
						 (term~abstr-create (list (second (first replace-pairs))
									  (term~variable-create 'x2
												(post~read-object '(o num) env
														  :existing-type)))
								    resclass-function))
						((= (length replace-pairs) 0)
						 (term~abstr-create (list (term~variable-create 'x1
												(post~read-object '(o num) env
														  :existing-type))
									  (term~variable-create 'x2
												(post~read-object '(o num) env
														  :existing-type)))
								    resclass-function))))
			     (closed-under-expr (term~appl-create (env~lookup-object 'closed-under env)
								  (list resclass-set resclass-op))))
			
			(meth~mapp-extend-mapp cmapp formula-mv closed-under-expr)
			(meth~mapp-extend-mapp cmapp assumps-mv according-assumps))
		    (meth~mapp-new-constraint cmapp nil)))
	      (meth~mapp-new-constraint cmapp nil)))
	(meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'mexpression-closed-under)

(defun methhelp=convert-number-set (number-set mod-fac)
  (let* ((numbers (mapcar #'keim~name number-set))
	 (env (pds~environment omega*current-proof-plan)))
    
    (if (do* ((number (- (keim~name mod-fac) 1) (decf number))
	      (flag 't))
	    ((or (null flag)
		 (= number -1))
	     flag)
	  (unless (find number numbers)
	    (setq flag nil)))
	(term~appl-create (env~lookup-object 'resclass-set env)
			  (list mod-fac))
      (let* ((var (term~variable-create 'x (post~read-object '(o num) env :existing-type))))
	(do* ((rest-number-set number-set (rest rest-number-set))
	      (formula nil))
	    ((null rest-number-set)
	     (term~abstr-create (list var) formula))
	  (let* ((head-number (first rest-number-set))
		 (resclass-term (term~appl-create (env~lookup-object '= env)
						  (list var
							(term~appl-create (env~lookup-object 'resclass env)
									  (list mod-fac head-number))))))
	    (if (null formula)
		(setf formula resclass-term)
	      (setf formula (term~appl-create (env~lookup-object 'or env)
					      (list formula resclass-term))))))))))

			       
(defun methhelp=or-disj-with-const-and-number-set-p (formula const number-set)
  (declare (edited  "22-MAR-2000")
	   (authors Ameier)
	   (input   "A formula, a constant, and a set of numbers.")
	   (effect  "None.")
	   (value   "T if formula has the form: (or (= const n1) (= const n2) ..., such that n1,n2, ... are"
		    "in the number set."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (cond ((logic~disjunction-p formula)
	   (every #'(lambda (arg)
		      (methhelp=or-disj-with-const-and-number-set-p arg const number-set))
		  (data~appl-arguments formula)))
	  ((and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object '= env))))
	   (let* ((args (data~appl-arguments formula))
		  (arg1 (first args))
		  (arg2 (second args)))
	     (or (and (keim~equal arg1 const)
		      (find arg2 number-set :test #'keim~equal))
		 (and (keim~equal arg2 const)
		      (find arg1 number-set :test #'keim~equal)))))
	  (t
	   nil))))		    

(defun methhelp=convert-mod-expr (formula pairs mod-fac counter)
  (declare (edited  "22-MAR-2000")
	   (authors Ameier)
	   (input   "A formula, a list of pairs of primitives and variables (in this list the variables and primitives"
		    "are stored such that the variables already replace the primitives), a modulo factor, and a counter.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: t/nil, t if the input formula is an expression that we can convert into a binary function"
		    "       on residue classes, that is the input formula consisting only of the function-symbols plus,minus,times."
		    "Second: the corresponding function expression on residue classes, where corresponding means:"
		    "        plus,minus,times are converted into resclass-plus,minus,times, a number n' is converted into:"
		    "        (resclass n n'), and each other primitive is converted into a variable of type [o num]."
		    "Third: a list with pairs of primitives in the input formula and corresponding variables on the formula"
		    "       of the second output argument."
		    "Fourth: The new counter."))
  (let* ((env (pds~environment omega*current-proof-plan)))

    (cond ((data~appl-p formula)
	   (if (find (data~appl-function formula)
		     (list (env~lookup-object 'plus env)
			   (env~lookup-object 'minus env)
			   (env~lookup-object 'times env)))
	       (let* ((res-op (cond ((keim~equal (data~appl-function formula)
						 (env~lookup-object 'plus env))
				     (env~lookup-object 'plus-resclass env))
				    ((keim~equal (data~appl-function formula)
						 (env~lookup-object 'times env))
				     (env~lookup-object 'times-resclass env))
				    ((keim~equal (data~appl-function formula)
						 (env~lookup-object 'minus env))
				     (env~lookup-object 'minus-resclass env)))))
		 
		 (multiple-value-bind
		     (succ1 result1 pairs1 counter1)
		     (methhelp=convert-mod-expr (first (data~appl-arguments formula)) pairs mod-fac counter)

		   (if succ1
		       (multiple-value-bind
			   (succ2 result2 pairs2 counter2)
			   (methhelp=convert-mod-expr (second (data~appl-arguments formula)) pairs1 mod-fac counter1)

			 (if succ2
			     (values 't
				     (term~appl-create res-op (list result1 result2))
				     pairs2
				     counter2)
			   (values nil nil nil nil)))
		     (values nil nil nil nil))))
	     (values nil nil nil nil)))
	  ((term~number-p formula)
	   (values 't
		   (term~appl-create (env~lookup-object 'resclass env)
				     (list mod-fac formula))
		   pairs
		   counter))
	  ((term~constant-p formula)
	   (let* ((assoc-test (assoc formula pairs :test #'keim~equal)))
	     (if assoc-test
		 ;; constant already replaced!
		 (values 't
			 (second assoc-test)
			 pairs
			 counter)
	       (let* ((new-variable (term~variable-create (make-symbol (format nil "x~A" counter))
							  (post~read-object '(o num) env :existing-type))))
		 (values 't
			 new-variable
			 (append pairs (list (list formula new-variable)))
			 (+ counter 1))))))
	  (t
	   nil))))	      
		    
(defun methhelp=disjunction-of-mod-expr-p (formula)
  (declare (edited  "22-MAR-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: t/nil, t if the formula has the form '(or (exp = n1) (exp = n2) ...)' where exp is"
		    "       an expression with exp = '(mod blabla n)', where blabla consists of the function"
		    "       plus,times,minus."
		    "Second: if first is t, a list of (n1 n2 ...)."
		    "Third: if first is t, exp."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (cond ((logic~disjunction-p formula)
	   (multiple-value-bind
	       (succ1 nset1 exp1)
	       (methhelp=disjunction-of-mod-expr-p (first (data~appl-arguments formula)))
	     (multiple-value-bind
		 (succ2 nset2 exp2)
		 (methhelp=disjunction-of-mod-expr-p (second (data~appl-arguments formula)))
	       
	       (let* ((succ (and succ1 succ2 (keim~equal exp1 exp2))))
		 (values succ
			 (when succ
			   (append nset1 nset2))
			 (when succ
			   exp1))))))
	  ((and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object '= (pds~environment omega*current-proof-plan)))))
	   (let* ((args (data~appl-arguments formula))
		  (arg1 (first (data~appl-arguments formula)))
		  (arg2 (second (data~appl-arguments formula))))
	     (cond ((and (term~number-p arg1)
			 (data~appl-p arg2)
			 (keim~equal (data~appl-function arg2)
				     (env~lookup-object 'mod env)))
		    (values 't
			    (list arg1)
			    arg2))
		   ((and (term~number-p arg2)
			 (data~appl-p arg1)
			 (keim~equal (data~appl-function arg1)
				     (env~lookup-object 'mod env)))
		    (values 't
			    (list arg2)
			    arg1))
		   (t
		    (values nil
			    nil
			    nil)))))
	  (t
	   (values nil
		   nil
		   nil))))) 
	      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying certain tactics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~deffun apply-=subst**-b (formula assumptions positions)
  (declare (edited  "27-MAR-2000")
	   (authors Sorge)
	   (input   "A formula, a list of nodes and a list of position-lists.")
	   (effect  "Creates a new term.")
	   (value   "A formula corresponding to the backward application of =subst**."))
  (batac=equality-subst**-create-f formula
				   (mapcar #'node~formula assumptions)
				   positions))

(meth~deffun expand-num*-positions (term)
  (declare (edited  "27-MAR-2000")
	   (authors Sorge)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "A list of positions where expand-number can be applied."))
  (mapcar #'car (natac=sort-pos-list (natac=function-position-list term))))



(meth~new-relational-function 'myassert)


(meth~defcond myassert (args cmapp)
  (declare (edited  "07-FEB-2000")
	   (authors Ameier)
	   (input   )
	   (effect  )
	   (value   ))
  ;; args: assumption, goal, var
  ;; returns T when ASSUMPTION can be used to assert GOAL and binds
  ;; var to the premises:
  (let* ((assumption (if (data~schema-p (first args))
			 (data~schema-range (term~alpha-copy (first args) (subst~create nil nil)))
		       (first args)))
	 (goal (second args))
	 (meth-var (third args))
	 (goal-sign (not (logic~negation-p goal :theory (prob~theory omega*current-proof-plan))))
	 (goal-func (if goal-sign
			(data~appl-function goal)
		      (data~appl-function (first (data~appl-arguments goal))))))
    (multiple-value-bind
	(sbfn premsn other-concs all-vars ex-vars)
	(sbf~subformula assumption
			T
			#'(lambda (sb) (and (data~appl-p sb)
					    (keim~equal (data~appl-function sb)
							goal-func)))
			goal-sign)
      (if sbfn
	  (let* ((type-subst (if (null (data~schema-p (first args)))
				 (subst~create nil nil)
			       (if goal-sign
				   (methhelp=unify-types sbfn goal)
				 (methhelp=unify-types (first (data~appl-arguments sbfn))
						       (first (data~appl-arguments goal))))))
		 (sbf (subst~apply type-subst sbfn))
		 (prems (mapcar #'(lambda (prem)
				    (subst~apply type-subst prem))
				premsn))				    
		 (mvars (mapcar #'(lambda (alv)
				    (term~generate-term-primitive-with-new-name
				     (keim~name alv) (term~type alv) 'meta+variable
				     (pds~environment omega*current-proof-plan)))
				all-vars))
		 (consts (mapcar #'(lambda (exv)
				     (term~generate-term-primitive-with-new-name
				      (keim~name exv) (term~type exv) 'term+constant
				      (pds~environment omega*current-proof-plan)))
				 ex-vars))
		 (mvars&consts (append mvars consts)))
	    (if mvars&consts
		(multiple-value-bind (new-sbf)
		    (data~replace-free-variables sbf (append all-vars ex-vars) mvars&consts)
		  (multiple-value-bind (cstrs bindingss splitp)
		      (cstr~sat-unify goal new-sbf T)
		    (multiple-value-bind
			(uni-cstr bindings)
			;;(if splitp
			;;   (values
			;;    (cstr~disjunction (mapcar #'(lambda (C Bs)
			;;				   (cstr~conjunction (if (cstr~constraint-p C)
			;;							 (append Bs (list C))
			;;						       Bs)))
			;;			       cstrs bindingss)))
			;;(values cstrs bindingss)
			;; )

			(if splitp
			    (values (first cstrs)
				    (first bindingss))
			  (values cstrs bindingss)
			  )
		      
		      ;;(format t "~%uni-cstr: ~A~%bindings: ~A" uni-cstr bindings)
		      
		      (if uni-cstr			    
			  (let* ((eq (env~lookup-object '= (pds~environment omega*current-proof-plan)))
				 (num-type (env~lookup-object 'num (pds~environment omega*current-proof-plan)))
				 (uni-list (if (cstr~constraint-p uni-cstr)
					       (methhelp=decompose-conjunction-recursive uni-cstr)
					     nil))
				 (uni-list-num (remove-if-not #'(lambda (uni-cstr)
								  (if (keim~equal (term~type (first (cstr~arguments uni-cstr))) num-type)
								      't
								    nil))
							      uni-list))
				 (uni-list-rest (remove-if #'(lambda (uni-cstr)
							       (if (keim~equal (term~type (first (cstr~arguments uni-cstr))) num-type)
								   't
								 nil))
							   uni-list))
				 (bindings-num (remove-if-not #'(lambda (binding-cstr)
								  (if (keim~equal (term~type (first (cstr~arguments binding-cstr))) num-type)
								      't
								    nil))
							      bindings))
				 (bindings-rest (remove-if #'(lambda (binding-cstr)
							       (if (keim~equal (term~type (first (cstr~arguments binding-cstr))) num-type)
								   't
								 nil))
							   bindings))
				 (equations (apply 'append (mapcar #'(lambda (args)
								       (if (keim~equal (term~type (first args)) num-type)
									   (list (term~appl-create eq args))
									 nil))
								   (mapcar #'cstr~arguments
									   (append uni-list-num bindings-num)))))
				 (rest-constraints (append uni-list-rest bindings-rest)))
			    
			    (cond ((or rest-constraints
				       (cstr~constraint-p uni-cstr)
				       (meth=help-bind-mvars-in bindings goal)
				       (meth=help-bind-mvars-in bindings sbf)
				       ;; returns T, iff some meta-variables into SBF (in the case assertion
				       ;; contain meta-variables.) or into GOAL (when GOAL is schematic) are
				     ;; bound by BINDINGS. 
				       )

				 (meth~mapp-new-constraint
				  (meth~mapp-new-extension
				   cmapp (if (meth~mapp-extension cmapp)
					     (mapp~create (cons meth-var (mapp~domain (meth~mapp-extension cmapp)))
							  (cons (mapcar #'(lambda (prem)
									    (multiple-value-bind (new-prem)
										(data~replace-free-variables
										 prem
										 (append all-vars ex-vars) mvars&consts)
									      (beta~normalize new-prem)))
									(append equations prems))
								(mapp~codomain (meth~mapp-extension cmapp))))
					   (mapp~create (list meth-var)
							(list (mapcar #'(lambda (prem)
									  (multiple-value-bind (new-prem)
									      (data~replace-free-variables
									       prem (append all-vars ex-vars) mvars&consts)
									    (beta~normalize new-prem)))
								      (append equations prems))))))
				  (cstr~conjunction (cons (meth~mapp-constraint cmapp)
							  rest-constraints))))
				
				(T
				 ;; The unification of GOAL with NEW-SBF resulted only to bindings of meta-variables
				 ;; in NEW-SBF to terms. In such situation, we apply these bindings to create the
				 ;; premises and save a possible changement for the constraint pool.
				 
				 (let ((all-vars-terms
					(mapcar #'(lambda (mv)
						    (let ((mv-bind (find-if #'(lambda (bind)
										(data~equal mv
											    (cstr~bound-variable bind)))
									    bindings)))
						      (if mv-bind (cstr~binding-term mv-bind) mv)))
						mvars)))
				   
				   (meth~mapp-new-extension
				    cmapp (if (meth~mapp-extension cmapp)
					      (mapp~create (cons meth-var (mapp~domain (meth~mapp-extension cmapp)))
							   (cons (mapcar
								  #'(lambda (prem)
								      (multiple-value-bind (new-prem)
									  (data~replace-free-variables
									   prem
									   (append all-vars ex-vars)
									   (append all-vars-terms consts))
									(beta~normalize new-prem)))
								  (append equations prems))
								 (mapp~codomain (meth~mapp-extension cmapp))))
					    (mapp~create (list meth-var)
							 (list (mapcar
								#'(lambda (prem)
								    (multiple-value-bind (new-prem)
									(data~replace-free-variables
									 prem
									 (append all-vars ex-vars)
									 (append all-vars-terms consts))
								      (beta~normalize new-prem)))
								(append equations prems))))))))))
			(progn
			  (mapcar #'(lambda (item)
				      (env~remove (keim~name item) (pds~environment omega*current-proof-plan)))
				  mvars&consts)
			  (meth~mapp-new-constraint cmapp NIL))))))
	      
	    ;; No all-vars and no ex-vars, unification constraints depend on meta-variables inside the
	    ;; assertion and/or inside the goal. These constraints must be returned.
	    (multiple-value-bind (cstrs bindingss splitp)
		(cstr~sat-unify goal sbf T)
	      (multiple-value-bind
		  (uni-cstr bindings)
		  (if splitp
		      (values
		       (cstr~disjunction (mapcar #'(lambda (C Bs)
						     (cstr~conjunction (if (cstr~constraint-p C)
									   (append Bs (list C))
									 Bs)))
						 cstrs bindingss)))
		    (values cstrs bindingss))
		(if uni-cstr
		    (let* ((eq (env~lookup-object '= (pds~environment omega*current-proof-plan)))
			   (num-type (env~lookup-object 'num (pds~environment omega*current-proof-plan)))
			   (uni-list (if (cstr~constraint-p uni-cstr)
					 (methhelp=decompose-conjunction-recursive uni-cstr)
				       nil))
			   (uni-list-num (remove-if-not #'(lambda (uni-cstr)
							    (if (keim~equal (term~type (first (cstr~arguments uni-cstr))) num-type)
								't
							      nil))
							uni-list))
			   (uni-list-rest (remove-if #'(lambda (uni-cstr)
							 (if (keim~equal (term~type (first (cstr~arguments uni-cstr))) num-type)
							     't
							   nil))
						     uni-list))
			   (bindings-num (remove-if-not #'(lambda (binding-cstr)
							    (if (keim~equal (term~type (first (cstr~arguments binding-cstr))) num-type)
								't
							      nil))
							bindings))
			   (bindings-rest (remove-if #'(lambda (binding-cstr)
							 (if (keim~equal (term~type (first (cstr~arguments binding-cstr))) num-type)
							     't
							   nil))
						     bindings))
			   (equations (apply 'append (mapcar #'(lambda (args)
								 (if (keim~equal (term~type (first args)) num-type)
								     (list (term~appl-create eq args))
								   nil))
							     (mapcar #'cstr~arguments
								     (append uni-list-num bindings-num)))))
			   (rest-constraints (append uni-list-rest bindings-rest)))
		      (if rest-constraints
			  (meth~mapp-new-constraint
			   (meth~mapp-new-extension
			    cmapp (if (meth~mapp-extension cmapp)
				      (mapp~create (cons meth-var (mapp~domain (meth~mapp-extension cmapp)))
						   (cons (append equations prems) (mapp~codomain (meth~mapp-extension cmapp))))
				    (mapp~create (list meth-var) (list prems))))
			   (cstr~conjunction (cons (meth~mapp-constraint cmapp)
						   rest-constraints)))
			(meth~mapp-new-extension
			 cmapp (if (meth~mapp-extension cmapp)
				   (mapp~create (cons meth-var (mapp~domain (meth~mapp-extension cmapp)))
						(cons (append equations prems) (mapp~codomain (meth~mapp-extension cmapp))))
				 (mapp~create (list meth-var) (list prems))))))
		  (meth~mapp-new-constraint cmapp NIL))))))
      (meth~mapp-new-constraint cmapp NIL)))
  ))

(defun methhelp=decompose-conjunction-recursive (cstr)
  (if (cstr~conjunction-p cstr)
      (apply #'append (mapcar #'methhelp=decompose-conjunction-recursive (cstr~arguments cstr)))
    (list cstr)))

(defun methhelp=unify-types (term1 term2)
  (cond ((and (data~appl-p term1)
	      (data~appl-p term2))
	 (let* ((func1 (data~appl-function term1))
		(func2 (data~appl-function term2)))
	   (if (and (term~constant-p func1)
		    (term~constant-p func2)
		    (eq (data~constant-origin func1)
			(data~constant-origin func2)))
	       (let* ((type-subst (type~alpha-match (term~type func1) (term~type func2))))
		 (if type-subst
		     type-subst
		   (subst~create nil nil)))
	     (subst~create nil nil))))		 
	((and (term~constant-p term1)
	      (term~constant-p term2)
	      (eq (data~constant-origin term1)
		  (data~constant-origin term2)))
	 (let* ((type-subst (type~alpha-match (term~type term1) (term~type term2))))
	   (if type-subst
	       type-subst
	     (subst~create nil nil))))
	(t
	 (subst~create nil nil))))

		    
(meth~defcond combindoperation (args cmapp)
  (declare (edited  "18-APR-2000")
	   (authors Ameier)
	   (input   "A list of args (a term with type ((o num) (o num) (o num)) and two meta-variables.")
	   (effect  "None.")
	   (value   "<c,mapp'> if the term has the form <lam x,y op1(x,y) +,*,- op2(x,y)>, <nil,cmapp> otherwise."
		    "In the first case the input meta-variables are bind on lam x',y' op1(x',y') and"
		    "lam x'',y'' op2(x'',y''), respectively."))
  (let* ((op (first args))
	 (op1 (second args))
	 (op2 (third args))
	 (env (pds~environment omega*current-proof-plan)))
    
    (cond ((or (keim~equal op (env~lookup-object 'plus-resclass env))
	       (keim~equal op (env~lookup-object 'times-resclass env))
	       (keim~equal op (env~lookup-object 'minus-resclass env)))
	   (let* ((new-varx (term~variable-create 'x (post~read-object '(o num) env :existing-type)))
		  (new-vary (term~variable-create 'y (post~read-object '(o num) env :existing-type)))
		  (new-varxp (term~variable-create 'xp (post~read-object '(o num) env :existing-type)))
		  (new-varyp (term~variable-create 'yp (post~read-object '(o num) env :existing-type))))

	     (progn
	       (meth~mapp-extend-mapp cmapp op1 (term~abstr-create (list new-varx new-vary) new-varx))
	       (meth~mapp-extend-mapp cmapp op2 (term~abstr-create (list new-varxp new-varyp) new-varyp)))))
	  (t
	   (if (and (data~abstr-p op)
		    (= (length (data~abstr-domain op)) 2))
	       (let* ((vars (data~abstr-domain op))
		      (range (data~abstr-range op)))
		 (if (and (data~appl-p range)
			  (or (keim~equal (data~appl-function range) (env~lookup-object 'plus-resclass env))
			      (keim~equal (data~appl-function range) (env~lookup-object 'minus-resclass env))
			      (keim~equal (data~appl-function range) (env~lookup-object 'times-resclass env))))
		     (let* ((args (data~appl-arguments range))
			    (new-op1 (term~alpha-copy (term~abstr-create vars (first args)) nil))
			    (new-op2 (term~alpha-copy (term~abstr-create vars (second args)) nil)))

		       (progn
			 (meth~mapp-extend-mapp cmapp op1 new-op1)
			 (meth~mapp-extend-mapp cmapp op2 new-op2)))
		   (meth~mapp-new-constraint cmapp nil)))
	     (meth~mapp-new-constraint cmapp nil))))))


(meth~new-relational-function 'combindoperation)  

(meth~defcond intnumber-p (args cmapp)
	      (declare (edited  "18-APR-2000")
		       (authors Ameier)
		       (input   "A list of args (a term with num).")
		       (effect  "None.")
		       (value   "<c,mapp> if the term is a integer number, <nil,cmapp> otherwise."))
	      (meth~mapp-new-constraint cmapp (integerp (keim~name (first args)))))

(meth~defcond theoremsintheory-p (args cmapp)
	      (declare (edited  "20-APR-2000")
		       (authors Ameier)
		       (input   "A list of args (a mv vor new hyp-lines).")
		       (effect  "if the theory of the problem contains some theorems the mv is bound on the"
				"list of new hyps created from these theorems.")
		       (value   "<c,mapp'> if the theory contains some theorems, <nil,cmapp> otherwise."))
	      (let* ((mv (first args))
		     (theory (prob~theory omega*current-proof-plan))
		     (theorems (th~theorems theory)))

		(if theorems
		    (let* ((new-hyps (mapcar #'(lambda (theorem)
						 (let* ((formula (term~alpha-copy (node~formula (prob~conclusion theorem)) nil))
							(label (pds~new-node-name omega*current-proof-plan)))
						   (pdsn~make-hypothesis formula label)))
					     theorems)))
		      (meth~mapp-extend-mapp cmapp mv new-hyps))
		  (meth~mapp-new-constraint cmapp nil))))								 

(meth~new-relational-function 'theoremsintheory-p)

(meth~defcond inrsrc-p (args cmapp)
	      (declare (edited  "20-APR-2000")
		       (authors Ameier)
		       (input   "A liust of args (a term).")
		       (effect  "If the term has the form '(resclass-set MV (resclass n m))', then mv is bound"
				"to n.")
		       (value   "<c,mapp> if the term has the form '(RESCLSet Rescl)' and we can compute, that rescl"
				"is in resclset, <c,nil> otherwise."))
	      (let* ((formula (first args))
		     (env (pds~environment omega*current-proof-plan)))
		(cond ((and (data~appl-p formula)
			    (keim~equal (env~lookup-object 'resclass-set env)
					(data~appl-function formula))
			    (= (length (data~appl-arguments formula)) 2))
		       ;; formula has the form (resclass-set n m)
		       (let* ((class-factor1 (first (data~appl-arguments formula)))
			      (resclass (second (data~appl-arguments formula))))
			 (multiple-value-bind
			     (success class-factor2 residue)
			     (zmz=resclass-formula-p resclass)
			   (if (and (term~number-p class-factor2)
				    (term~number-p residue)
				    (<= 0 (keim~name residue))
				    (< (keim~name residue) (keim~name class-factor2)))
			       ;; the last argument has the form '(resclass n m)' where n and m are numbers
			       ;; and 0<=m<n
			       (cond ((meta~p class-factor1)
				      ;; formula has the form '(resclass-set mv (resclass n m))' where n and m are numbers
				      ;; -> bind mv to n
				      (let* ((new-constraint (cstr~binding-create (list class-factor1 class-factor2))))
					(meth~mapp-new-constraint cmapp new-constraint)))
				     ((keim~equal class-factor1 class-factor2)
				      ;; class-factor1 and class-factor2 are keim~equal
				      ;; -> accept
				      (meth~mapp-new-constraint cmapp 't))
				     (t
				      (meth~mapp-new-constraint cmapp nil)))
			     (meth~mapp-new-constraint cmapp nil)))))
		      (t
		       (meth~mapp-new-constraint cmapp nil)))))
	      
(meth~defcond subsetrcl-p (args cmapp)
	      (declare (edited  "19-APR-2000")
		       (authors Ameier)
		       (input   "A list of args (two resclass sets rs1 and rs2).")
		       (effect  "If rs1 is consistent (all resclasses share a common modulo factor n), and rs2"
				"has the form '(resclass-set m)' where m is a meta-variable then m is bound to n.")
		       (value   "<c,mapp'> if we can compute that rs1 is a subset of rs2, <nil,cmapp> otherwise."))
	      (let* ((rs1 (first args))
		     (rs2 (second args))
		     (env (pds~environment omega*current-proof-plan)))
		;;(format t "RS1: ~A" rs1)
		;;(format t "RS2: ~A" rs2)
		(if (and (zmztac=resclass-set-p rs1)
			 (zmztac=resclass-set-p rs2))
		    ;; -> rs1,rs2 represent sets of resclasses with the same class factor, respectively.
		    (let* ((class-factor-rs1 (zmztac=class-factor rs1))
			   (class-factor-rs2 (zmztac=class-factor rs2)))
		      (cond ((and (data~appl-p rs2)
				  (keim~equal (data~appl-function rs2) (env~lookup-object 'resclass-set env))
				  (= (length (data~appl-arguments rs2)) 1)
				  (meta~p (first (data~appl-arguments rs2))))
			     ;; -> rs2 has the form (resclass-set mv)
			     ;; bind the meta-variable to the class factor of rs1
			     (let* ((new-constraint (cstr~binding-create (list (first (data~appl-arguments rs2)) class-factor-rs1))))
			       (meth~mapp-new-constraint cmapp new-constraint)))
			    ((or (data~positions rs1 #'meta~p)
				 (data~positions rs2 #'meta~p))
			     ;; one of the resclass-set expressions contains further meta-variables
			     ;; -> since we cannot make sure computations -> nil
			     (meth~mapp-new-constraint cmapp nil))
			    (t
			     ;; here other possible cases should be included, escpecially the case where we can compare the sets 
			     ;; of the reside classes
			     (meth~mapp-new-constraint cmapp nil))))
		  (meth~mapp-new-constraint cmapp nil))))

(defun methhelp=pairs-facs (set)
  (declare (edited  "03-AUG-2000")
	   (authors Ameier)
	   (input   "A residue class set or a cartesian product set of residue class sets.")
	   (effect  "None.")
	   (value   "A list of natiral numbers which are the modulo factors of the residue-class-sets."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p set)
	     (keim~equal (data~appl-function set)
			 (data~schema-range (env~lookup-object 'cartesian-product env))))
	(let* ((first-facs (methhelp=pairs-facs (first (data~appl-arguments set))))
	       (second-facs (methhelp=pairs-facs (second (data~appl-arguments set)))))
	  (list first-facs second-facs))
      (zmztac=class-factor set))))

(defun methhelp=number-tupels-to-set (set)
  (declare (edited  "03-AUG-2000")
	   (authors Ameier)
	   (input   "A residue class set or a cartesian-product of residue class sets.")
	   (effect  "None.")
	   (value   "A list of number tupels corresponding to the set, e.g.:"
		    "z2z -> (0 1),"
		    "z2zxz2z -> ((0 0) (0 1) (1 0) (1 1),"
		    "z2zx(z2zxz2z) -> ((0 (0 0)) (0 (0 1)) (0 (1 0)) (0 (1 1)) (1 (0 0)) ..."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p set)
	     (keim~equal (data~appl-function set)
			 (data~schema-range (env~lookup-object 'cartesian-product env))))
	(let* ((tupel1 (methhelp=number-tupels-to-set (first (data~appl-arguments set))))
	       (tupel2 (methhelp=number-tupels-to-set (second (data~appl-arguments set))))
	       (multi-tuple (methhelp=multi-tupel tupel1 tupel2)))
	  multi-tuple)
      (zmztac=residue-class-set-to-number-list set))))

(defun methhelp=multi-tupel (tupel1 tupel2)
  (if (or (null tupel1)
	  (null tupel2))
      nil
    (let* ((head (first tupel1)))
      (append (mapcar #'(lambda (item2)
			  (list head item2))
		      tupel2)
	      (methhelp=multi-tupel (rest tupel1) tupel2)))))

(defun methhelp=number-sets-to-resclass-set (set)
  (let* ((env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p set)
	     (keim~equal (data~appl-function set)
			 (data~schema-range (env~lookup-object 'cartesian-product env))))
	(list (methhelp=number-sets-to-resclass-set (first (data~appl-arguments set)))
	      (methhelp=number-sets-to-resclass-set (second (data~appl-arguments set))))
      (zmztac=number-set-to-resclass-set set))))

(defun methhelp=compute-mvs-in-nsets-expression (mv-tupels number-sets)
  (let* ((parts (mapcar #'(lambda (mv-tupel)
			    (methhelp=mv-tupel-to-number-set-conjunction mv-tupel number-sets))
			mv-tupels))
	 (and-obj (env~lookup-object 'and (pds~environment omega*current-proof-plan))))
    (do* ((rest-parts parts (rest rest-parts))
	  (back nil))
	((null rest-parts)
	 back)
      (let* ((head (first rest-parts)))
	(if (null back)
	    (setf back head)
	  (setf back (term~appl-create and-obj (list head back))))))))

(defun methhelp=mv-tupel-to-number-set-conjunction (mv-tupel number-sets)
   (let* ((and-obj (env~lookup-object 'and (pds~environment omega*current-proof-plan))))
     (if (listp mv-tupel)
	 (term~appl-create and-obj
			   (list (methhelp=mv-tupel-to-number-set-conjunction (first mv-tupel) (first number-sets))
				 (methhelp=mv-tupel-to-number-set-conjunction (second mv-tupel) (second number-sets))))
       (beta~normalize (term~appl-create number-sets (list mv-tupel))))))
		       
(meth~deffun isomorphic-terms-exists-II (set1 set2)
  (declare (edited  "03-AUG-2000")
	   (authors Ameier)
	   (input   )
	   (effect  )
	   (value   ))
	     (let* ((fac1-list (methhelp=pairs-facs set1))
		    (fac2-list (methhelp=pairs-facs set2))
		    (tupels1 (methhelp=number-tupels-to-set set1))
		    (type1 (first (data~abstr-domain (term~type set1))))
		    (type2 (first (data~abstr-domain (term~type set2))))
		    (number-sets2 (methhelp=number-sets-to-resclass-set set2)))
	       
	       (multiple-value-bind
		   (phi new-mv-objs)
		   (methhelp=compute-homomorphism-function-II tupels1 fac1-list fac2-list type1 type2 :new-objects 'meta-variables)

		 (let* ((new-mvs-in-nsets-conjunction (methhelp=compute-mvs-in-nsets-expression new-mv-objs number-sets2)))
		   
		   (list phi new-mvs-in-nsets-conjunction)))))

(meth~deffun isomorphic-terms-FORALL-II (set1 set2)
  (declare (edited  "03-AUG-2000")
	   (authors Ameier)
	   (input   )
	   (effect  )
	   (value   ))
	     (let* ((fac1-list (methhelp=pairs-facs set1))
		    (fac2-list (methhelp=pairs-facs set2))
		    (tupels1 (methhelp=number-tupels-to-set set1))
		    (type1 (first (data~abstr-domain (term~type set1))))
		    (type2 (first (data~abstr-domain (term~type set2))))
		    (number-sets2 (methhelp=number-sets-to-resclass-set set2)))
	       
	       (multiple-value-bind
		   (phi new-mv-objs)
		   (methhelp=compute-homomorphism-function-II tupels1 fac1-list fac2-list type1 type2 :new-objects 'constants)
		 
		 (let* ((new-mvs-in-nsets-conjunction (methhelp=compute-mvs-in-nsets-expression new-mv-objs number-sets2)))
		   
		   (list phi new-mvs-in-nsets-conjunction)))))


(defun methhelp=compute-tupel-object (fac-list &key (new-objects 'meta-variables))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (num-obj (env~lookup-object 'num env)))
    (if (listp fac-list)
	(multiple-value-bind
	    (back-tupel1 new-obj1)
	    (methhelp=compute-tupel-object (first fac-list) :new-objects new-objects)
	  (multiple-value-bind
	      (back-tupel2 new-obj2)
	      (methhelp=compute-tupel-object (second fac-list) :new-objects new-objects)
	    
	    (values (term~appl-create (env~lookup-object 'pair env)
				      (list back-tupel1 back-tupel2))
		    (list new-obj1 new-obj2))))
      (let* ((new-obj (cond ((string-equal new-objects 'meta-variables)
			     (meth=defn-newmetavar 'mv num-obj))
			    ((string-equal new-objects 'variables)
			     (term~generate-term-primitive-with-new-name 'v
									 num-obj
									 'term+variable
									 env))
			    (t
			     (term~generate-term-primitive-with-new-name 'c
									 num-obj
									 'term+constant
									 env)))))
	(values 
	 (term~appl-create (env~lookup-object 'resclass env)
			   (list fac-list
				 new-obj))
	 new-obj)))))

(defun methhelp=compute-new-obj-tupels (tupels fac-list &key (new-objects 'meta-variables))
  (do* ((rest-tupels tupels (rest rest-tupels))
	(back-tupels nil)
	(new-objs nil))
      ((null rest-tupels)
       (values back-tupels
	       new-objs))
    (let* ((head (first rest-tupels)))

      (multiple-value-bind
	  (back-tupel news)
	  (methhelp=compute-tupel-object fac-list :new-objects new-objects)

	(setf back-tupels (append back-tupels (list back-tupel)))
	(setf new-objs (cons news new-objs))))))


(defun methhelp=compute-homomorphism-function-II (tupels fac1-list fac2-list type1 type2 &key (new-objects 'meta-variables))
  (let* ((env (pds~environment omega*current-proof-plan)))

    (multiple-value-bind
	(new-objs news)
	(methhelp=compute-new-obj-tupels tupels fac2-list :new-objects new-objects)

      (let* ((x-var (term~generate-term-primitive-with-new-name 'x
								(data~copy type1 :downto '(type+primitive))
								'term+variable
								env))
	     (that-var (term~generate-term-primitive-with-new-name 'y
								   (data~copy type2 :downto '(type+primitive))
								   'term+variable
								   env))
	     (implies-obj (env~lookup-object 'implies env))
	     (and-obj (env~lookup-object 'and env))
	     (=-obj (env~lookup-object '= env))
	     (resclass-obj (env~lookup-object 'resclass env))
	     (conjunction (do* ((rest-tupels tupels (rest rest-tupels))
				(rest-objects new-objs (rest rest-objects))
				(back-conj nil))
			      ((null rest-tupels)
			       back-conj)
			    (let* ((tupel (first rest-tupels))
				   (object (first rest-objects))
				   (resclass-expr1 (methhelp=resclass-expression tupel fac1-list))
				   (resclass-expr2 object)
				   (implication (term~appl-create implies-obj
								  (list (term~appl-create =-obj (list x-var resclass-expr1))
									(term~appl-create =-obj (list that-var resclass-expr2))))))
			      (if (null back-conj)
				  (setf back-conj implication)
				(setf back-conj (term~appl-create and-obj (list back-conj implication))))))))
	
	(values
	 (term~abstr-create (list x-var)
			    (term~appl-create (env~lookup-object 'that env)
					      (list (term~abstr-create (list that-var)
								       conjunction))))
	 news)))))

(defun methhelp=resclass-expression (tupel fac-list)
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (if (listp tupel)
	(let* ((first-arg (methhelp=resclass-expression (first tupel) (first fac-list)))
	       (second-arg (methhelp=resclass-expression (second tupel) (Second fac-list))))
	  (term~appl-create (env~lookup-object 'pair env)
			    (list first-arg second-arg)))
      (term~appl-create (env~lookup-object 'resclass env)
			(list fac-list tupel)))))

  
(meth~deffun mquantify-sortexistential (formula variables sort)
  (declare (edited  "03-AUG-2000")
	   (authors Ameier)
	   (input   "A formula and a list of variables.")
	   (effect  "None.")
	   (value   "A formula where the variables are existentially quantified."))
  (let* ((exists-sort-quant (env~lookup-object 'exists-sort (pds~environment omega*current-proof-plan))))
    
    (do* ((rest-variables variables (rest rest-variables))
	  (current-formula formula))
	((null rest-variables)
	 current-formula)
      (let* ((head-variable (first rest-variables)))
	(setf current-formula
	      (term~appl-create exists-sort-quant
				(list (term~abstr-create (list head-variable)
							 current-formula)
				      sort)))))))


(meth~deffun isomorphic-terms-exists (set1 set2)
	     (declare (edited  "29-MAY-2000")
		      (authors Ameier)
		      (input   "Two formulas which are resclass-sets.")
		      (effect  "None.")
		      (value   "The input formulas are wrt. a formula (exists-sort phi blable (functions set1 set2))"
			       "As first a instantiation for the function phi is created of the following form:"
			       "(lam (x (o num)) (that (lam (y (o num))"
			       "                           (and (implies (= x (resclass fac1 e1)) (= y (resclass fac2 mv1)))"
			       "         				   .... ))))"
			       "Thereby e1,...,en are the elements of set1, mv1, ..., mvn are new created"
			       "meta-variables, and fac2 and fac1 are the modulo-factor of set2 and set1."
			       "This function returns a list containing two parts:"
			       "1.) A conjunction with"
			       "    (mv1 in NumSetTo(set2)) and  (mv2 in NumSetTo(set2)) ..."
			       "    -> Notice, that these (mvx in NumSetTo(set2)) formulas are expressed as disjunctions!"
			       "2.) The new created instantiation for phi."))
	     (let* ((fac1 (zmztac=class-factor set1))
		    (fac2 (zmztac=class-factor set2))
		    (numbers1 (zmztac=residue-class-set-to-number-list set1))
		    (number-set2 (zmztac=number-set-to-resclass-set set2)))
	       
	       (multiple-value-bind
		   (phi new-meta-vars)
		   (methhelp=compute-homomorphism-function numbers1 fac1 fac2 :new-objects 'meta-variables)
		 
		 (let* ((mv-in-nset2-conjunction (methhelp=compute-object-in-nset-expression new-meta-vars number-set2)))

		   (list mv-in-nset2-conjunction phi))))) 

(meth~deffun isomorphic-terms-forall (set1 set2)
	     (declare (edited  "29-MAY-2000")
		      (authors Ameier)
		      (input   "Two formulas which are resclass-sets.")
		      (effect  "None.")
		      (value   "The input formulas are wrt. a formula (forall-sort phi blable (functions set1 set2))"
			       "As first a instantiation for the function phi is created of the following form:"
			       "(lam (x (o num)) (that (lam (y (o num))"
			       "                           (and (implies (= x (resclass fac1 e1)) (= y (resclass fac2 c1)))"
			       "         				   .... ))))"
			       "Thereby e1,...,en are the elements of set1, c1, ..., cn are new created"
			       "constants, and fac2 and fac1 are the modulo-factors of set2 and set1."
			       "This function returns a list containing two parts:"
			       "1.) A conjunction with"
			       "    (c1 in NumSetTo(set2)) and  (c2 in NumSetTo(set2)) ..."
			       "    -> Notice, that these (cx in NumSetTo(set2)) formulas are expressed as disjunctions!"
			       "2.) The new created instantiation for phi."))
	     (let* ((fac1 (zmztac=class-factor set1))
		    (fac2 (zmztac=class-factor set2))
		    (numbers1 (zmztac=residue-class-set-to-number-list set1))
		    (number-set2 (zmztac=number-set-to-resclass-set set2)))
	       
	       (multiple-value-bind
		   (phi new-constants)
		   (methhelp=compute-homomorphism-function numbers1 fac1 fac2 :new-objects 'constants)
		 
		 (let* ((mv-in-nset2-conjunction (methhelp=compute-object-in-nset-expression new-constants number-set2)))
		   
		   (list mv-in-nset2-conjunction phi))))) 

(defun methhelp=compute-object-in-nset-expression (objects number-set)
  (declare (edited  "24-MAY-2000")
	   (authors Ameier)
	   (input   "A list of objects (either meta-variables or constants) and a number-set expression.")
	   (effect  "None.")
	   (value   "A conjunction of the form:"
		    "(ob1 in number-set) and (ob2 in number-set)..."))		
  (let* ((env (pds~environment omega*current-proof-plan))
	 (and-obj (env~lookup-object 'and env)))

    (do* ((rest-objects objects (rest rest-objects))
	  (back-conj nil))
	((null rest-objects)
	 back-conj)
      (let* ((head-object (first rest-objects))
	     (new-term (beta~normalize (term~appl-create number-set (list head-object)))))
	(if (null back-conj)
	    (setf back-conj new-term)
	  (setf back-conj (term~appl-create and-obj (list back-conj new-term))))))))

(defun methhelp=compute-homomorphism-function (numbers fac1 fac2 &key (new-objects 'meta-variables))
  (declare (edited  "24-MAY-2000")
	   (authors Ameier)
	   (input   "A list of numbers and the class factor of a residue class set, a key-word new-objects"
		    "that signs whether the new created objects (see value slot) should be constants or"
		    "meta-variables.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A function phi:"
		    "       phi = (lam (x (o num)) (that (lam (y (o num))"
		    "                                    (and (implies (= x (resclass fac1 e1)) (= y (resclass fac2 ob1)))"
		    "           	     			   .... ))))"
		    "       Thereby e1,...,en are the elements of numbers and ob1, ..., obn are new created objects."
		    "       The type of the new created objects depends on the value of the kwy-word new-objects:"
		    "       Is the value meta-variables new meta-variables are created, otherwise constants."
		    "Second: The list of the new created objects"))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (new-objs (if (string-equal new-objects 'meta-variables)
		      (mapcar #'(lambda (number)
				  (meth=defn-newmetavar 'mv (env~lookup-object 'num env)))
			      numbers)
		    (mapcar #'(lambda (number)
				(term~generate-term-primitive-with-new-name 'c (env~lookup-object 'num env) 'term+constant env))
			    numbers)))
	 (x-var (term~generate-term-primitive-with-new-name 'x
							    (post~read-object '(o num) env :existing-type)
							    'term+variable
							    env))
	 (that-var (term~generate-term-primitive-with-new-name 'y
							       (post~read-object '(o num) env :existing-type)
							       'term+variable
							       env))
	 (implies-obj (env~lookup-object 'implies env))
	 (and-obj (env~lookup-object 'and env))
	 (=-obj (env~lookup-object '= env))
	 (resclass-obj (env~lookup-object 'resclass env))
	 (conjunction (do* ((rest-numbers numbers (rest rest-numbers))
			    (rest-objects new-objs (rest rest-objects))
			    (back-conj nil))
			  ((null rest-numbers)
			   back-conj)
			(let* ((number (first rest-numbers))
			       (object (first rest-objects))
			       (resclass-expr1 (term~appl-create resclass-obj (list fac1 number)))
			       (resclass-expr2 (term~appl-create resclass-obj (list fac2 object)))
			       (implication (term~appl-create implies-obj
							      (list (term~appl-create =-obj (list x-var resclass-expr1))
								    (term~appl-create =-obj (list that-var resclass-expr2))))))
			  (if (null back-conj)
			      (setf back-conj implication)
			    (setf back-conj (term~appl-create and-obj (list back-conj implication))))))))

    (values
     (term~abstr-create (list x-var)
			(term~appl-create (env~lookup-object 'that env)
					  (list (term~abstr-create (list that-var)
								   conjunction))))
     new-objs)))

     
(meth~defcond mthat-expression-simplify-p (args cmapp)
  (declare (edited  "25-MAY-2000")
	   (authors Ameier)
	   (input   "A list of arguments (a formula phi and a meta-variable mv) and a cmapping.")
	   (effect  "Mayby mv is bound, see value slot.")
	   (value   "If phi contains simplifiable that-expressions mv is bound on the simplified term"
		    "and the cmapping is returned. Otherwise cmapping is set to nil."
		    "A that expresssion is simplifiable (for instance) if it has the form:"
		    "that x. (and (0=0 => x=t) ..., in this case this that expression is replaced by t."))
		    
  (let* ((phi (first args))
	 (mv (second args)))

    (multiple-value-bind
	(success simplified-phi positions)
	(methhelp=simplify-that-expressions phi (pos~empty))
      
      (if success
	  (meth~mapp-extend-mapp cmapp mv simplified-phi)
	(meth~mapp-new-constraint cmapp nil)))))

(defun methhelp=simplify-that-expressions (formula pos)
  (declare (edited  "25-MAY-2000")
	   (authors Ameier)
	   (input   "A formula and a position (signing the position of the formula as a subformula in"
		    "another formula).")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A success-flag (t/nil), t if formula contains that expressions that can be simplified"
		    "       (see function mthat-expression-simplify-p), nil otherwise."
		    "Second: The simplified formula (in case no simplification was possible, the formula itself."
		    "Third: A list of positions in formula whose formulas are simplifed."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    
    (cond ((and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object 'that env))))
	   ;; formula is a that expression!

	   (let ((simplify? (methhelp=simplifiable-that-expression-p formula)))
	     (if simplify?
		 ;; that expression is simplified, resulting in simplify?
		 (values 't simplify? (list pos))
	       (values nil formula nil))))
	  ((data~appl-p formula)
	   ;; formula is a application, but not a that expression

	   (let* ((func (data~appl-function formula))
		  (args (data~appl-arguments formula)))

	     (multiple-value-bind
		 (success simplified-term-list position-list)
		 (methhelp=simplify-that-expressions-in-list (cons func args) pos)
	     
	       (if success
		   (values 't (term~appl-create (first simplified-term-list) (rest simplified-term-list)) position-list)
		 (values nil (term~appl-create (first simplified-term-list) (rest simplified-term-list)) nil)))))
	  ((data~abstr-p formula)
	   ;; formula is an abstraction

	   (let* ((domain (data~abstr-domain formula))
		  (range (data~abstr-range formula)))
	     (multiple-value-bind
		 (success simplified-term position-list)
		 (methhelp=simplify-that-expressions range (pos~add-end 0 pos))

	       (values success
		       (term~abstr-create domain simplified-term)
		       position-list))))
	  ((term~primitive-p formula)
	   (values nil formula nil)))))						

(defun methhelp=simplify-that-expressions-in-list (formula-list pos)
  (do* ((rest-formulas formula-list (rest rest-formulas))
	(counter 0 (+ counter 1))
	(simplified-terms nil)
	(success nil)
	(pos-list nil))
      ((null rest-formulas)
       (values success
	       simplified-terms
	       pos-list))
    (let* ((head-formula (first rest-formulas))
	   (head-pos (pos~add-end counter pos)))

      (multiple-value-bind
	  (succ simpl-formula positions)
	  (methhelp=simplify-that-expressions head-formula head-pos)

	(setf simplified-terms (append simplified-terms (list simpl-formula)))
	(when succ
	  (setf pos-list (append pos-list positions))
	  (setf success 't))))))  

(defun methhelp=simplifiable-that-expression-p (formula)
  (declare (edited  "25-MAY-2000")
	   (authors Ameier)
	   (input   "A formula, which is an application with function that.")
	   (effect  "None.")
	   (value   "If this formula is simplifiable (see function mthat-expression-simplify-p) its simplification,"
		    "nil otherwise."))
  (let* ((abstr (first (data~appl-arguments formula)))
	 (var (first (data~abstr-domain abstr)))
	 (range (data~abstr-range abstr)))

    (do* ((rest-range (list range))
	  (back-term nil))
	((or (null rest-range)
	     back-term)
	 (if back-term
	     back-term
	   nil))
      (let* ((head-range-part (first rest-range)))
	(cond ((logic~conjunction-p head-range-part)
	       (setf rest-range (append (rest rest-range) (data~appl-arguments head-range-part))))
	      ((logic~implication-p head-range-part)
	       (let* ((ante (first (data~appl-arguments head-range-part)))
		      (succ (second (data~appl-arguments head-range-part))))
		 (if (and (logic~equality-p ante)
			  (keim~equal (first (data~appl-arguments ante)) (second (data~appl-arguments ante))))
		     ;; antecendent of implication has the form t=t -> is fulfilled!
		     (if (and (logic~equality-p succ)
			      (or (keim~equal (first (data~appl-arguments succ)) var)
				  (keim~equal (second (data~appl-arguments succ)) var)))
			 ;; succedent of implication has the form var=t' or t'=var
			 ;; -> return t' as the simplification of the that expression
			 (if (keim~equal (first (data~appl-arguments succ)) var)
			     (setf back-term (second (data~appl-arguments succ)))
			   (setf back-term (first (data~appl-arguments succ))))

		       ;; else -> remove this range part
		       (setf rest-range (rest rest-range)))
		   ;; else -> remove this range part
		   (setf rest-range (rest rest-range)))))
	      (t
	       ;; Something went wrong -> setf  rest-range nil -> terminate with nil
	       (setf rest-range nil)))))))
	      
(meth~new-relational-function 'mthat-expression-simplify-p)


(meth~defcond morder-expression-simplify-p (args cmapp)
  (declare (edited  "25-MAY-2000")
	   (authors Ameier)
	   (input   "A list of arguments (a formula phi and a meta-variable mv) and a cmapping.")
	   (effect  "Mayby mv is bound, see value slot.")
	   (value   "If phi contains simplifiable order-expressions (that are expressions where we can determin the"
		    "order of a set) then mv is bound on the simplified term (in which the term (order set) is replaced"
		    "by the value) and the cmapping is returned. Otherwise cmapping is set to nil."))
  
  (let* ((phi (first args))
	 (mv (second args))
	 (env (pds~environment omega*current-proof-plan))
	 (positions (data~positions phi #'(lambda (term)
					    (and (data~appl-p term)
						 (keim~equal (data~schema-range (env~lookup-object 'order env))
							     (data~appl-function term)))))))
    (multiple-value-bind
	(simplified-phi simplified-positions)
	(methhelp=replace-evaluable-order-terms phi positions)
      
      (if simplified-positions
	  (meth~mapp-extend-mapp cmapp mv simplified-phi)
	(meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'morder-expression-simplify-p)

(defun methhelp=replace-evaluable-order-terms (term positions)
  (declare (edited  "08-JUN-2000")
	   (authors Ameier)
	   (input   "A term and a list of positions of order subterms.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A new term in which each evaluatable order term is evaluated."
		    "Second: A list of positions where order-terms are evaluated."))
  (do* ((rest-positions positions (rest rest-positions))
	(current-term term)
	(back-positions nil))
      ((null rest-positions)
       (values current-term
	       back-positions))
    (let* ((head-pos (first rest-positions))
	   (head-order-term (data~struct-at-position current-term head-pos)))

      (multiple-value-bind
	  (evaluatable? result)
	  (methhelp=evaluate-order-term head-order-term)

	(when evaluatable?
	  (setf back-positions (append back-positions (list head-pos)))
	  (setf current-term (data~replace-at-position current-term head-pos result :downto '(term+primitive))))))))

(defun methhelp=evaluate-order-term (order-term)
  (declare (edited  "08-JUN-2000")
	   (authors Ameier)
	   (input   "A order term (application with function order).")
	   (effect  "None.")
	   (value   "Multiple-Vlaue:"
		    "First: Success flag: if we can compute the order of the set t otherwise nil."
		    "Second: The value of the computed order of the set."))
  (let* ((set (first (data~appl-arguments order-term))))
    (cond ((and (zmztac=resclass-set-p set)
		(zmztac=residue-class-set-to-number-list set))
	   ;; Set is a residue class set and we can compute the numbers of the residue-class-set
	   (values 't
		   (term~constant-create (length (zmztac=residue-class-set-to-number-list set))
					 (env~lookup-object 'num (pds~environment omega*current-proof-plan)))))
	  (t
	   (values nil
		   nil)))))

	   
		 
(meth~defcond mcart-product-of-resclasses-p (args cmapp)
	      (declare (edited  "25-MAY-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a term phi) and a cmapping.")
		       (effect  "None.")
		       (value   "If phi is a cartesian-product expressian of residue-class sets, the cmapping is"
				"returned, otherwise cmapping is set to nil."))  
	      (let* ((phi (first args))
		     (env (pds~environment omega*current-proof-plan)))
		(do* ((rest-formulas (list phi))
		      (flag 't))
		    ((or (null rest-formulas)
			 (null flag))
		     (if flag
			 cmapp
		       (meth~mapp-new-constraint cmapp nil)))
		  (let* ((next-formula (first rest-formulas)))
		    (cond ((and (data~appl-p next-formula)
				(keim~equal (data~appl-function next-formula)
					    (data~schema-range (env~lookup-object 'cartesian-product env))))
			   (setf rest-formulas (append (rest rest-formulas) (data~appl-arguments next-formula))))
			  ((zmztac=resclass-set-p next-formula)
			   (setf rest-formulas (rest rest-formulas)))
			  (t
			   (setf flag nil)))))))

(meth~deffun mget-residue-class-sets-from-cartesian-set (cart-product-of-res-classes)
  (declare (edited  "13-JUL-2000")
	   (authors Ameier)
	   (input   "A term, representing a cartesian product of residue classes.")
	   (effect  "None.")
	   (value   "A list of all the residue classes."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (do* ((rest-formulas (list cart-product-of-res-classes))
	  (back-list nil))
	((null rest-formulas)
	 back-list)
      (let* ((next-formula (first rest-formulas)))
	(cond ((and (data~appl-p next-formula)
		    (keim~equal (data~appl-function next-formula)
				(data~schema-range (env~lookup-object 'cartesian-product env))))
	       (setf rest-formulas (append (data~appl-arguments next-formula) (rest rest-formulas))))
	      ((zmztac=resclass-set-p next-formula)
	       (setf rest-formulas (rest rest-formulas))
	       (setf back-list (append back-list (list next-formula)))))))))

(meth~deffun mnum-sets-of (res-class-sets)
  (declare (edited  "13-JUL-2000")
	   (authors Ameier)
	   (input   "A list of residue class sets.")
	   (effect  "None.")
	   (value   "A list with the corresponding num-sets of the residue-classes."))
  (mapcar #'zmztac=number-set-to-resclass-set res-class-sets))

(meth~deffun type-new-consts (type listi)
  (declare (edited  "13-JUL-2000")
	   (authors Ameier)
	   (input   "A type and a list.")
	   (effect  "None.")
	   (value   "A list with new constants (as many as the input list has elements."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (mapcar #'(lambda (item)
		(term~generate-term-primitive-with-new-name 'c type 'term+constant env))
	    listi)))

(meth~deffun type-new-mvs (type listi)
  (declare (edited  "13-JUL-2000")
	   (authors Ameier)
	   (input   "A type and a list.")
	   (effect  "None.")
	   (value   "A list with new meta-variables (as many as the input list has elements."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (mapcar #'(lambda (item)
		(term~generate-term-primitive-with-new-name 'mv type 'meta+variable env))
	    listi)))

(meth~deffun mcreate-pair-term (cartprodresclset new-consts)
  (declare (edited  "13-JUL-2000")
	   (authors Ameier)
	   (input   "A cartesian-product of residue classes expression and a list of new constants.")
	   (effect  "None.")
	   (value   "Computes a pair expression corresponding to the cartesian-product."
		    "For instance, Z_2xZ_3 and constants c1,c2 -> (pair (resclass c1 2) (resclass c2 3))"))
  (methhelp=create-pair-term cartprodresclset new-consts))

(defun methhelp=create-pair-term (cartprodresclset new-consts)
  (let* ((env (pds~environment omega*current-proof-plan)))

    (cond ((and (data~appl-p cartprodresclset)
		(keim~equal (data~appl-function cartprodresclset)
			    (data~schema-range (env~lookup-object 'cartesian-product env))))

	   (multiple-value-bind
	       (pair-term1 rest-new-consts1)
	       (methhelp=create-pair-term (first (data~appl-arguments cartprodresclset)) new-consts)

	     (multiple-value-bind
		 (pair-term2 rest-new-consts2)
		 (methhelp=create-pair-term (second (data~appl-arguments cartprodresclset)) rest-new-consts1)

	       (values (term~appl-create (env~lookup-object 'pair env)
					 (list pair-term1 pair-term2))
		       rest-new-consts2))))
	  ((zmztac=resclass-set-p cartprodresclset)

	   (let* ((class-factor (zmztac=class-factor cartprodresclset)))
	     (values (term~appl-create (env~lookup-object 'resclass env)
				       (list class-factor (first new-consts)))
		     (rest new-consts)))))))


(meth~deffun mcreate-items-in-numsets (new-items num-sets)
  (declare (edited  "13-JUL-2000")
	   (authors Ameier)
	   (input   "A list of constants (of type num) and a list of sets (of type (o num)).")
	   (effect  "None.")
	   (value   "A formula: (and (c1 in set1) ..."
		    "(BETA-NORMALIZED!!)"))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (and-obj (env~lookup-object 'and env)))
    
    (do* ((rest-items new-items (rest rest-items))
	  (rest-sets num-sets (rest rest-sets))
	  (back-formula nil))
	((null rest-items)
	 back-formula)
      (let* ((head-const (first rest-items))
	     (head-set (first rest-sets))
	     (new-subterm (beta~normalize (term~appl-create head-set (list head-const)))))
	(if (null back-formula)
	    (setf back-formula new-subterm)
	  (setf back-formula (term~appl-create and-obj
					       (list back-formula new-subterm))))))))


(meth~deffun rewrite-first-second-pair (interm)
  (declare (edited  "14-JUL-2000")
	   (authors Pollet)
	   (input   )
	   (effect  )
	   (value   ))	   
 (let* ((pds (pds~environment omega*current-proof-plan))
	(pair (env~lookup-object 'pair pds))
	(fir (env~lookup-object 'first-of-pair pds))
	(sec (env~lookup-object 'second-of-pair pds)))
   (labels ((drop-things (term)
			 (cond
			  ((listp term)(mapcar #'drop-things term))
			  ((term~primitive-p term) term)
			  ((term~abstr-p term)(term~abstr-create
					       (data~abstr-domain term)
					       (drop-things (data~abstr-range term))))
			  ((data~schema-equal (data~appl-function term) fir)
			   (let ((new-arg  (drop-things (data~appl-c-argument term))))
			     (or (first-second-case new-arg #'first)
				 (data~appl-create (data~appl-function term)
						   (list new-arg)))))
			  ((data~schema-equal (data~appl-function term) sec)
			   (let ((new-arg  (drop-things (data~appl-c-argument term))))
			     (or (first-second-case new-arg #'second)
				 (data~appl-create (data~appl-function term)
						   (list new-arg)))))
			  (t (term~appl-create (drop-things (data~appl-function term))
					       (drop-things (data~appl-arguments term))))))
	    (first-second-case (term case)
			       (when (and (data~appl-p term)
					  (data~schema-equal (data~appl-function term) pair))
				 (funcall case (data~appl-arguments term)))))
     (drop-things interm))))



(meth~deffun current-mv-binding ()
	     (declare (edited  "06-MAR-2000")
		      (authors Ameier)
		      (input   "Nothing.")
		      (effect  "None.")
		      (value   "I there exists a current constraint store (CS) and this CS contains"
			       "meta-variable bindings, the corresponding substitution."
			       "Otherwise, an empty substitution."))
	     (let* ((cs (pds~constraint-pool omega*current-proof-plan)))
	       
	       (if cs
		   (pds~cstrpool-bindings cs)
		 (subst~create nil nil))))

(meth~defcond simplifiable-positions-p (args cmapp)
	      (declare (edited  "25-MAY-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a term phi) and constant fc, a variable (for a positionlist)"
				"a cmapping.")
		       (effect  "(see value)")
		       (value   "If phi contains expressions of the form '(fc (resclass n NUM))' or"
				"'(fc (pair (resclass n1 NUM1) ...)' then these positions are bound on the"
				"positionlist and cmapp is returned, otheriwse cmapp is set to nil."
				"Notice, NUM, NUM1, ... haveto be numbers! no constants!"))
	      (let* ((phi (first args))
		     (fc (second args))
		     (poslist (third args))
		     (positions (data~positions phi #'(lambda (term)
							(and (data~appl-p term)
							     (keim~equal (data~appl-function term) fc)
							     (methhelp=specified-p (first (data~appl-arguments term))))))))
		(if positions
		    (meth~mapp-extend-mapp cmapp poslist positions)
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'simplifiable-positions-p)


(defun methhelp=specified-p (formula)
  (declare (edited  "04-AUG-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "T if the formula has the form:"
		    "'(resclass n NUM)', or"
		    "'(pair (resclass n1 NUM1) ...)', where NUM,NUM1, ... have to be numbers!, or"
		    "expression that can be simplified to '(resclass n NUM)', or"
		    "expression that can be simplified to '(pair (resclass n1 NUM1) ...)"
		    "where NUM,NUM1, ... have to be numbers!"))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (cond ((and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object 'pair env))))
	   (and (methhelp=specified-p (first (data~appl-arguments formula)))
		(methhelp=specified-p (second (data~appl-arguments formula)))))
	  ((and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (env~lookup-object 'resclass env)))
	   (and (term~number-p (second (data~appl-arguments formula)))
		(term~number-p (first (data~appl-arguments formula)))))
	  (t
	   (multiple-value-bind
	       (success simplified-formula poslist equations)
	       (methhelp=simplify-resclass-expressions formula (pos~empty))
	     (if success
		 (methhelp=specified-p simplified-formula)
	       nil))))))

(meth~deffun simplify-at-positions (formula poslist function)
  (declare (edited  "04-AUG-2000")
	   (authors Ameier)
	   (input   )
	   (effect  )
	   (value   ))
	     (do* ((current-formula formula)
		   (rest-positions poslist (rest rest-positions)))
		 ((null rest-positions)
		  current-formula)
	       (let* ((head-pos (first rest-positions)))
		 (setf current-formula (methhelp=simplify-at-position current-formula head-pos function)))))

(defun methhelp=simplify-at-position (formula pos function)
  (let* ((struct-at-pos (data~struct-at-position formula pos))
	 (arg (first (data~appl-arguments struct-at-pos))))
    (multiple-value-bind
	(success simplified-arg poslist equations)
	(methhelp=simplify-resclass-expressions arg (pos~empty))
      (let* ((new-value (if success
			    (methhelp=evaluate-function function simplified-arg)
			  (methhelp=evaluate-function function arg))))
	(data~replace-at-position formula pos new-value)))))

(defun methhelp=evaluate-function (function arg)
  (let* ((that-expr (data~abstr-range function))
	 (conjs (data~abstr-range (first (data~appl-arguments that-expr))))
	 (all-implications (methhelp=decompose-conjunctions conjs))
	 (corr-imp (find arg all-implications :test #'(lambda (argi implication)
							(keim~equal argi
								    (second (data~appl-arguments 
									     (first (data~appl-arguments implication)))))))))
    (if corr-imp
	(second (data~appl-arguments 
		 (second (data~appl-arguments corr-imp))))
      nil)))	 

(defun methhelp=decompose-conjunctions (conj)
  (do* ((rest-conjs (list conj))
	(back nil))
      ((null rest-conjs)
       back)
    (let* ((head (first rest-conjs)))
      (if (logic~conjunction-p head)
	  (setf rest-conjs (append (data~appl-arguments head) (rest rest-conjs)))
	(progn
	  (setf back (append back (list head)))
	  (setf rest-conjs (rest rest-conjs)))))))





(meth~defcond msimplify-resclass (args cmapp)
	      (declare (edited  "25-MAY-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a term phi, a meta-var mv1 for a second term, a line,"
				"a mv2 for a list of new lines, and a mv3 for a list of positions) and"
				"a cmapping.")
		       (effect  "(see value)")
		       (value   "If phi contains expressions simplifiable expressions of residue classes"
				"(e.g., (plus (resclass 2 0) (resclass 2 1)) -> (resclass 2 1))"
				"then these expressions are simplified and mv1 is bound on the new -simplified- term."
				"Furthermore, mv2 is bound on new open lines containing the equtions used for the"
				"simplification (e.g. , (plus (resclass 2 0) (resclass 2 1)) = (resclass 2 1))"
				"and mv3 is bound on the positions in phi that are simplified."))

	      (let* ((phi (first args))
		     (mv1 (second args))
		     (line (third args))
		     (mv2 (fourth args))
		     (mv3 (fifth args)))

		(multiple-value-bind
		    (success simplified-term positions equations)
		    (methhelp=simplify-resclass-expressions phi (pos~empty))

		  (if success
		      (progn
			(meth~mapp-extend-mapp cmapp mv1 simplified-term)
			(meth~mapp-extend-mapp cmapp mv2 (methhelp=new-goals equations line))
			(meth~mapp-extend-mapp cmapp mv3 positions))
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'msimplify-resclass)

(defun methhelp=new-goals (new-formulas goal-line)
  (declare (edited  "07-AUG-2000")
	   (authors Ameier)
	   (input   "A list of formulas and a line.")
	   (effect  "None.")
	   (value   "A list of new open nodes containing the input formulas respectively."
		    "the hyps of the new nodes are the same as the hyps of the second"
		    "argument."))
  (let* ((hyps (pdsn~hyps goal-line)))
    
    (mapcar #'(lambda (formula)
		(let* ((label (pds~new-node-name omega*current-proof-plan)))
		  (pdsn~open-node-create formula hyps label)))
	    new-formulas)))

(defun methhelp=simplify-resclass-expressions (formula position)
  (declare (edited  "07-AUG-2000")
	   (authors Ameier)
	   (input   "A formula and a position (giving the position of the formula as"
		    "a subformula in another formula.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if there are simplifiable residue class expressions in formula."
		    "Second: The simplified formula."
		    "Third: The positions where simplifications happened"
		    "Fourth: The simplification equations."))
  (let* ((env (pds~environment omega*current-proof-plan)))

    (cond ((and (data~appl-p formula)
		(find (data~appl-function formula) (list (env~lookup-object 'plus-resclass env)
							 (env~lookup-object 'minus-resclass env)
							 (env~lookup-object 'times-resclass env))
		      :test #'keim~equal))
	   ;; -> potential candidat for simplification!
	   (multiple-value-bind
	       (success simplified-term)
	       (methhelp=simplify-p formula)

	     (if success
		 (values 't
			 simplified-term
			 (list position)
			 (list (term~appl-create (env~lookup-object '= env)
						 (list formula simplified-term))))
	       (let* ((args (data~appl-arguments formula)))
		 (multiple-value-bind
		     (success1 simplified1 positions1 equations1)
		     (methhelp=simplify-resclass-expressions (first args) (pos~add-end 1 position))
		   (multiple-value-bind
		       (success2 simplified2 positions2 equations2)
		       (methhelp=simplify-resclass-expressions (second args) (pos~add-end 2 position))
		     (if (or success1 success2)
			 (values 't
				 (term~appl-create (data~appl-function formula)
						   (list simplified1 simplified2))
				 (append positions1 positions2)
				 (append equations1 equations2))
		       (values nil
			       formula
			       nil
			       nil))))))))
	  ((data~appl-p formula)
	   (let* ((function (data~appl-function formula))
		  (args (data~appl-arguments formula)))
	     (do* ((rest-args args (rest rest-args))
		   (counter 1 (+ counter 1))
		   (back-success nil)
		   (back-args nil)
		   (back-positions nil)
		   (back-equations nil))
		 ((null rest-args)
		  (values back-success
			  (if back-success
			      (term~appl-create (data~appl-function formula)
						back-args)
			    formula)
			  back-positions
			  back-equations))
	       (let* ((head-arg (first rest-args))
		      (curr-pos (pos~add-end counter position)))
		 (multiple-value-bind
		     (success simplified positions equations)
		     (methhelp=simplify-resclass-expressions head-arg curr-pos)

		   (when success
		     (setf back-success 't))
		   (setf back-args (append back-args (list simplified)))
		   (setf back-positions (append back-positions positions))
		   (setf back-equations (append back-equations equations))))))))))

(defun methhelp=simplify-p (formula)
  (declare (edited  "07-AUG-2000")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: T if formula is simplifiable, that is if formula has the form:"
		    "       (plus/minus/times-resclass simplifiable1 simplifiable2), where"
		    "       simplifiable1 simplifiable2 are also simplifiable terms."
		    "Second: The simplified value."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p formula)
	     (find (data~appl-function formula) (list (env~lookup-object 'plus-resclass env)
						      (env~lookup-object 'minus-resclass env)
						      (env~lookup-object 'times-resclass env))
		   :test #'keim~equal))
	;; -> formula starts with plus/minus/times-resclass
	(let* ((args (data~appl-arguments formula))
	       (arg1 (first args))
	       (arg2 (second args))
	       (term1 (methhelp=simplify-termination-p arg1))
	       (term2 (methhelp=simplify-termination-p arg2)))
	  (cond ((and term1
		      term2)
		 (values t
			 (methhelp=compute (data~appl-function formula) arg1 arg2)))
		(term1
		 (multiple-value-bind
		     (success2 simplified2)
		     (methhelp=simplify-p arg2)
		   (if success2
		       (values t
			       (methhelp=compute (data~appl-function formula) arg1 simplified2))
		     (values nil nil))))
		(term2
		 (multiple-value-bind
		     (success1 simplified1)
		     (methhelp=simplify-p arg1)
		   (if success1
		       (values t
			       (methhelp=compute (data~appl-function formula) simplified1 arg2))
		     (values nil nil))))
		(t
		 (multiple-value-bind
		     (success1 simplified1)
		     (methhelp=simplify-p arg1)
		   (multiple-value-bind
		       (success2 simplified2)
		       (methhelp=simplify-p arg2)
		     (if (and success1
			      success2)
			 (values t
				 (methhelp=compute (data~appl-function formula) simplified1 simplified2))
		       (values nil nil)))))))
      (values nil nil))))

		     

(defun methhelp=compute (function arg1 arg2)
  (declare (edited  "07-AUG-2000")
	   (authors Ameier)
	   (input   "A resclass fucntion (plus/minus/times-resclass) and two residue-class args.")
	   (effect  "None.")
	   (value   "Evaluated the function with the args."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (plus-resclass (env~lookup-object 'plus-resclass env))
	 (minus-resclass (env~lookup-object 'minus-resclass env))
	 (times-resclass (env~lookup-object 'times-resclass env))
	 (num (env~lookup-object 'num env))
	 (resclass (env~lookup-object 'resclass env))
	 (modfac1 (keim~name (first (data~appl-arguments arg1))))
	 (modfac2 (keim~name (first (data~appl-arguments arg2))))
	 (num1 (keim~name (second (data~appl-arguments arg1))))
	 (num2 (keim~name (second (data~appl-arguments arg2)))))
    
    (when (null (= modfac1 modfac2))
      (omega~error "~%IN Function methhelp=compute two different modulo factors."))
    
    (cond ((and (keim~equal function plus-resclass))
	   (term~appl-create resclass
			     (list (term~constant-create modfac1 num)
				   (term~constant-create (mod (+ num1 num2) modfac1) num))))
	  ((and (keim~equal function minus-resclass))
	   (term~appl-create resclass
			     (list (term~constant-create modfac1 num)
				   (term~constant-create (mod (- num1 num2) modfac1) num))))
	  ((and (keim~equal function times-resclass))
	   (term~appl-create resclass
			     (list (term~constant-create modfac1 num)
				   (term~constant-create(mod (* num1 num2) modfac1) num)))))))	   

(defun methhelp=simplify-termination-p (formula)
  (let* ((env (pds~environment omega*current-proof-plan)))
    (if (and (data~appl-p formula)
	     (keim~equal (data~appl-function formula)
			 (env~lookup-object 'resclass env))
	     (term~number-p (second (data~appl-arguments formula)))
	     (term~number-p (first (data~appl-arguments formula))))
	't
      nil)))

	
(meth~deffun apply-defni* (formula definiendum definiens poslist)
  (declare (edited  "08-AUG-2000")
	   (authors Ameier)
	   (input   "A formula, a definiendum, a definiens, and a list of positions of occurrences of the"
		    "definiendem in the formula.")
	   (effect  "None.")
	   (value   "A new formula created by definition expansion on the positions."))
  (gentac=compute-iterated-defne formula definiendum definiens poslist))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; THE FOLLING ARE THE COND-FUNCS NEEDED FOR THE NOTINJNOTISO STRATEGY
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~deffun compute-hom-lines (set1 op1 op2 h line)
	     (declare (edited  "15-SEP-2000")
		      (authors Ameier)
		      (input   "A formula representing a set of residue-classes, two operations on residue"
			       "class sets, and a constant h.")
		      (effect  "None.")
		      (value   "A list of ND-lines are computed:"
			       "the complete list of homomorpshim equations of the number set of of set1 with op1 into"
			       "op2 (second set is irrelevant at this moment). Thereby h is the homomorphism."))
	     (let* ((class-factor1 (zmztac=class-factor set1))
		    (class-factor-number1 (keim~name class-factor1))
		    (number-list1 (crihelp=number-set-to-resclass-set set1))
		    (operation-on-numbers1 (crihelp=convert-operation op1 class-factor1 nil))
		    (table1 (rcl~multiplication-table number-list1 class-factor-number1
						      :operation operation-on-numbers1))
		    (hyps (pdsn~hyps line)))
	       (apply #'append (mapcar #'(lambda (num1)
					   (mapcar #'(lambda (num2)
						       (methhelp=create-node num1 num2 h op2
									     class-factor1 table1
									     hyps line))
						   number-list1))
				       number-list1))))

(defun methhelp=create-node (num1 num2 h op2 class-factor1 table1 hyps line)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (result (rcl~table-pair2result table1 (list num1 num2)))
	 (num (env~lookup-object 'num env))
	 (resclass (env~lookup-object 'resclass env))
	 (res1 (term~appl-create h
				 (list (term~appl-create resclass
							 (list class-factor1
							       (term~constant-create num1 num))))))
	 (res2 (term~appl-create h
				 (list (term~appl-create resclass
							 (list class-factor1
							       (term~constant-create num2 num))))))
	 (result-obj (term~appl-create h
				       (list (term~appl-create resclass
							       (list class-factor1
								     (term~constant-create result num))))))
	 (equation (term~appl-create (env~lookup-object '= env)
				     (list result-obj
					   (beta~normalize (term~appl-create op2 (list res1 res2))))))
	 (label (pds~new-node-name omega*current-proof-plan))
	 (just (pdsj~closed-just-create	"Hom-Equation" (list line) nil))
	 (node (pdsn~create label hyps equation just)))
    node))

(meth~defcond in-finite-set-p (args cmapp)
	      (declare (edited  "06-MAR-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a formula) and a cmapping <c,mapp>.")
		       (effect  "None.")
		       (value   "<C,mapp> if formula represents an expression 'c in Set' where set is a finite set."))
	      (let* ((formula (first args))
		     (env (pds~environment omega*current-proof-plan)))
		(cond ((and (data~appl-p formula)
			    (keim~equal (data~appl-function formula)
					(data~schema-range (env~lookup-object 'in env)))
			    (= (length (data~appl-arguments formula)) 2) 
			    (or (zmztac=resclass-set-p (second (data~appl-arguments formula)))))
		       ;; formula ha the form (in c set) where we can recognize set as a finite set!
		       cmapp)
		      ((and (data~appl-p formula)
			    (> (length (data~appl-arguments formula)) 1))
		       (let* ((func (data~appl-function formula))
			      (args (data~appl-arguments formula))
			      (reduced-formula (term~appl-create func (butlast args))))
			 (if (or (zmztac=resclass-set-p reduced-formula))
			     ;; formulas has the form (set c) and we can recognize set as a finite set
			     cmapp
			   (meth~mapp-new-constraint cmapp nil))))
		      (t
		       (meth~mapp-new-constraint cmapp nil)))))


		       
(meth~deffun rewrite-finite-set-to-disj (formula)
	     (declare (edited  "06-MAR-2000")
		      (authors Ameier)
		      (input   "A formula representing an expression 'c in Set' where we can"
			       "identify set as a finite set.")
		      (effect  "None.")
		      (value   "A disjunction of the form c = s1 or c = s2 or ..., where s_i are"
			       "the elements of the finite set Set."))
	     (let* ((env (pds~environment omega*current-proof-plan)))
	       
	       (cond ((and (data~appl-p formula)
			   (keim~equal (data~appl-function formula)
				       (data~schema-range (env~lookup-object 'in env)))
			   (= (length (data~appl-arguments formula)) 2))
		      ;; formula has the form (in c set) where we can recognize set as a finite set!
		      (let* ((set (second (data~appl-arguments formula)))
			     (c (first (data~appl-arguments formula))))
			
			(cond ((zmztac=resclass-set-p set)
			       (methhelp=express-residue-class-set-as-disjunction set c))
			      )))
		     ((and (data~appl-p formula)
			   (> (length (data~appl-arguments formula)) 1))
		       (let* ((func (data~appl-function formula))
			      (args (data~appl-arguments formula))
			      (set (term~appl-create func (butlast args)))
			      (c (first (last args))))
			 (cond ((zmztac=resclass-set-p set)
				(methhelp=express-residue-class-set-as-disjunction set c))
			       ))))))

(defun methhelp=express-residue-class-set-as-disjunction (set c)
  (let* ((class-factor (zmztac=class-factor set))
	 (number-set (methhelp=number-tupels-to-set set))
	 (env (pds~environment omega*current-proof-plan))
	 (or-obj (env~lookup-object 'or env))
	 (resclass-obj (env~lookup-object 'resclass env))
	 (=-obj (env~lookup-object '= env)))

    (do* ((rest-numbers number-set (rest rest-numbers))
	  (current-disj nil))
	((null rest-numbers)
	 current-disj)
      (let* ((head-num (first rest-numbers))
	     (equation (term~appl-create =-obj
					 (list c
					       (term~appl-create resclass-obj (list class-factor head-num))))))
	(if (null current-disj)
	    (setf current-disj equation)
	  (setf current-disj (term~appl-create or-obj (list equation current-disj))))))))


(meth~defcond hom-into-resclass (args cmapp)
	      (declare (edited  "06-MAR-2000")
		       (authors Ameier)
		       (input   "A list of arguments (two formulas, a line, and a mv) and a cmapping <c,mapp>.")
		       (effect  "See value.")
		       (value   "<C,mapp> if we find in the hypothesis of the line a line stating that is"
				"a homomorphism from into a resclass with class-facror n, and the two formulas"
				"are composed only of: h(c), plus-resclass, minus-resclass, times-resclass"
				"and expressions (resclass n c). If this is the case then the mv is bound"
				"on the class-factor n."))
	      (let* ((formula1 (first args))
		     (formula2 (second args))
		     (line (third args))
		     (mv-class-factor (fourth args))
		     (mv-homomorphism (fifth args)))
		
		(multiple-value-bind
		    (h class-factor)
		    (methhelp=find-homomorphism (pdsn~hyps line))

		  (if (and h
			   (methhelp=check-term-for-hom-expr formula1 h class-factor)
			   (methhelp=check-term-for-hom-expr formula2 h class-factor))
		      (progn
			(meth~mapp-extend-mapp cmapp mv-class-factor class-factor)
			(meth~mapp-extend-mapp cmapp mv-homomorphism h)
			cmapp)
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'hom-into-resclass)

(defun methhelp=check-term-for-hom-expr (term h class-factor)
  (declare (edited  "06-MAR-2000")
	   (authors Ameier)
	   (input   "A term, a (function) constant, a class-factor.")
	   (effect  "None.")
	   (value   "T if the term consists only of the following expressions: (h c), (resclass class-factor c)"
		    "(where both c's are numbers), plus/minus/times-resclass. Otherwise nil."))
  (if (null (data~appl-p term))
      nil
    (let* ((env (pds~environment omega*current-proof-plan))
	   (func (data~appl-function term))
	   (args (data~appl-arguments term)))
      
      (cond ((or (keim~equal func (env~lookup-object 'plus-resclass env))
		 (keim~equal func (env~lookup-object 'minus-resclass env))
		 (keim~equal func (env~lookup-object 'times-resclass env)))
	     (and (methhelp=check-term-for-hom-expr (first args) h class-factor)
		  (methhelp=check-term-for-hom-expr (second args) h class-factor)))
	    ((keim~equal h func)
	     (let* ((arg (first args)))
	       (if (meta~p arg)
		   nil
		 't)))
	    ((keim~equal func (env~lookup-object 'resclass env))
	     (if (and (= (length args) 2)
		      (keim~equal (first args) class-factor)
		      (term~number-p (second args)))
		 't
	       nil))
	    (t
	     nil)))))
		  
(defun methhelp=find-homomorphism (lines)	
  (declare (edited  "06-MAR-2000")
	   (authors Ameier)
	   (input   "A list of lines.")
	   (effect  "None.")
	   (value   "Searches for a line with formula (homomorphism set1 op1 set2 op2 h), where"
		    "set2 is a residue class set."
		    "For the first found line with such a formula, this functions returns the following"
		    "multiple-value:"
		    "First: h"
		    "Second: the class-factor of set2."
		    "If none such line is found nil is returned."))
  (if (null lines)
      nil
    (let* ((line (first lines))
	   (formula (node~formula line))
	   (env (pds~environment omega*current-proof-plan)))
      (if (and (data~appl-p formula)
	       (keim~equal (data~appl-function formula)
			   (data~schema-range (env~lookup-object 'homomorphism env)))
	       (= (length (data~appl-arguments formula)) 5)
	       (zmztac=resclass-set-p (third (data~appl-arguments formula))))
	  ;; formula has the form (homomorphism set1 op1 set2 op2 h) and set2 is a residue-class
	  (values (fifth (data~appl-arguments formula))
		  (zmztac=class-factor (third (data~appl-arguments formula))))
	(methhelp=find-homomorphism (rest lines))))))


(meth~defcond equal-modulo-maple (args cmapp)
	      (declare (edited  "06-MAR-2000")
		       (authors Ameier)
		       (input   "A list of arguments (two formulas, a class-factor, and the homomorphism function h)"
				"and a cmapping <c,mapp>.")
		       (effect  "See value.")
		       (value   "First the expression about residue-classes are converted corresponding"
				"expressions about numbers. Thereby, each subterm of the form (h c) is replaced"
				"by a new constant c' everywhere in the two formulas. Then maple is called"
				"whether the resulting formulas are equal modulo the class-factor."
				"If this is the case <c,mapp> is retirned, otherwise the conditionis set to nil."))
	      (let* ((formula1 (first args))
		     (formula2 (second args))
		     (class-factor (third args))
		     (h (fourth args)))

		(multiple-value-bind
		    (converted-formula1 converts1)
		    (methhelp=convert-hom-rescl-to-num formula1 h nil)
		  (multiple-value-bind
		      (converted-formula2 converts2)
		      (methhelp=convert-hom-rescl-to-num formula2 h converts1)

		    (let* ((converted-string1 (string-downcase (post~string converted-formula1)))
			   (converted-string2 (string-downcase (post~string converted-formula2)))
			   (equation (concatenate 'string
						  "(= "
						  (string-downcase converted-string1)
						  " "
						  (string-downcase converted-string2) ")"))
			   (primitives (remove-duplicates (append (methhelp=get-primitives converted-formula1)
								  (methhelp=get-primitives converted-formula2))))
			   (mv-list nil)
			   (result (if (null primitives)
				       nil
				     (methhelp=msolve-with-maple equation primitives class-factor))))
		      (if (or (null result) (string-equal result "Error"))
			  (meth~mapp-new-constraint cmapp nil)
			(progn 
			  ;;(format t "~%~%SUCCESS DU HACKFRESS: ~A = ~A MODULO ~A" formula1 formula2 class-factor)
			  (let* ((eq-list (rcl=dissect-maple-hint result))
				 (dummy (print eq-list))
				 (mv-strings (mapcar #'(lambda (mv) (find (keim~name mv) eq-list :test #'string-equal :key #'car)) mv-list))
				 (const-list (remove-if-not #'term~constant-p primitives))
				 (const-strings (mapcar #'(lambda (mv) (find (keim~name mv) eq-list :test #'string-equal :key #'car)) const-list)))
			    (if (every #'(lambda (x) (string-equal (first x) (second x))) const-strings)
				(meth~mapp-new-constraint
				 cmapp
				 (cstr~conjunction (methhelp=make-cstr-bindings mv-list (mapcar #'second mv-strings) class-factor)))
			      (meth~mapp-new-constraint cmapp nil)))))))))
	      )

(defun methhelp=convert-hom-rescl-to-num (term h converts)
  (declare (edited  "06-MAR-2000")
	   (authors Ameier)
	   (input   "A formula, the homomorphism function, and the list of already done converts.")
	   (effect  "None.")
	   (value   "Converts formula into a new formula, thereby:"
		    "(h c) is replaced by a new constant c' with type num"
		    "for each new such created replacement a new pair is added into the assoc-list converts"
		    "(resclass n c) is replaced by c"
		    "plus/minus/times-resclass is replaced by plus/minus/times respectively."
		    "Multiple-Value is returned:"
		    "First: the converted formula"
		    "Second: teh (updated) converts"))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (func (data~appl-function term))
	 (args (data~appl-arguments term)))
    
    (cond ((or (keim~equal func (env~lookup-object 'plus-resclass env))
	       (keim~equal func (env~lookup-object 'minus-resclass env))
	       (keim~equal func (env~lookup-object 'times-resclass env)))
	   (let* ((new-func (cond ((keim~equal func (env~lookup-object 'plus-resclass env))
				   (env~lookup-object 'plus env))
				  ((keim~equal func (env~lookup-object 'times-resclass env))
				   (env~lookup-object 'times env))
				  (t
				   (env~lookup-object 'minus env)))))
	     (multiple-value-bind
		 (conv-arg1 converts1)
		 (methhelp=convert-hom-rescl-to-num (first args) h converts)
	       (multiple-value-bind
		   (conv-arg2 converts2)
		   (methhelp=convert-hom-rescl-to-num (second args) h converts1)
		 
		 (values (term~appl-create new-func (list conv-arg1 conv-arg2))
			 converts2)))))
	  ((keim~equal h func)
	   (let* ((assi (assoc term converts :test #'keim~equal)))
	     (if assi
		 (values (second assi)
			 converts)
	       (let* ((new-constant (term~generate-term-primitive-with-new-name 'c
										(env~lookup-object 'num env)
										'term+constant
										env)))
		 (values new-constant
			 (cons (list term new-constant) converts))))))		  
	  ((keim~equal func (env~lookup-object 'resclass env))
	   (values (second args)
		   converts))
	  (t
	   (omega~error "~% This should not happen in function methhelp=convert-hom-rescl-to-num")))))


(meth~defcond find-partner-in-chain (args cmapp)
	      (declare (edited  "06-MAR-2000")
		       (authors Ameier)
		       (input   "A list of arguments (a goal-node, three meta variables), and a cmapping <c,mapp>.")
		       (effect  "It is checked whether there exists a whole chain of inequations on"
				"expressions of the homomorphism-function h. If this is the case we try"
				"to solve (msolve) with maple an equation between one side of the current"
				"goal and the respective other side of one eqaution on the chain or goal."
				"If this is the case we bind the first meta-variable on the node that contains"
				"the counter part to our current goal and the other two meta-variables on the"
				"two positions of the two equation-parts (position (1) means left side and"
				"position (2) means right side).")
		       (value   "<C,mapp> if the meta-variables are bound, the extended cmapping is returned,"
				"otherwise a nilled cmapping is returned."))
	      (let* ((goal-node (first args))
		     (counter-node-mv (second args))
		     (goal-pos-mv (third args))
		     (counter-node-pos-mv (fourth args))
		     (chain (crihelp=compute-chain goal-node)))
		(if (null chain)
		    (meth~mapp-new-constraint cmapp nil)
		  ;; Notice, this if forbids to check for h(x_i)=h(x_j) in the verry first conclusion of this kind!
		  ;; But since x_i and x_j have to be unequal this makes no sence anyway!
		  (let* ((last-step-pos (first (pdsj~parameters (node~justification (first chain)))))
			 (side (if (= (pos~first last-step-pos) 1)
				   'left
				 'right))
			 (goal-side (if (string-equal side 'left)
					(first (data~appl-arguments (crihelp=update-formula! goal-node)))
				      (second (data~appl-arguments (crihelp=update-formula! goal-node)))))
			 (counter-side (if (string-equal side 'left)
					   (second (data~appl-arguments (crihelp=update-formula! goal-node)))
					 (first (data~appl-arguments (crihelp=update-formula! goal-node)))))
			 ;; we want to minimize the checks, thus we check only for the changed side. That is,
			 ;; if the last step was done on the left side, we check the left side of the current
			 ;; goal equation against the right sides of the chains, and vice versa if the
			 ;; last step was done on the right side ...
			 (chain-sidesa ;;(cons (list goal-node counter-side)
					     (mapcar #'(lambda (chain-node)
							 (let* ((args (data~appl-arguments (crihelp=update-formula! chain-node))))
							   (list chain-node
								 (if (string-equal side 'left)
								     (second args)
								   (first args)))))
						     chain))
			                ;;)
			 (chain-sides (remove-duplicates chain-sidesa
							 :test #'(lambda (it1 it2)
								   (keim~equal (second it1) (second it2)))))
			 ;; to avoid expensive tests with maple further, we remove from the list of possible counter
			 ;; formulas all duplications!
			 )
		    (multiple-value-bind
			(h class-factor)
			(methhelp=find-homomorphism (pdsn~hyps goal-node))
		      
		      (let* ((matching (do* ((rest-sides chain-sides (rest rest-sides))
					     (flag nil))
					   ((or flag
						(null rest-sides))
					    flag)
					 (let* ((head-pair (first rest-sides))
						(head-side (second head-pair))
						(result (crihelp=matching-with-maple goal-side head-side h class-factor)))
					   (when result
					     (setf flag head-pair))))))
			(if matching
			    (progn
			      (meth~mapp-extend-mapp cmapp counter-node-mv (first matching))
			      (meth~mapp-extend-mapp cmapp goal-pos-mv (if (string-equal side 'left)
									   (pos~add-front 1 (pos~empty))
									 (pos~add-front 2 (pos~empty))))
			      (meth~mapp-extend-mapp cmapp counter-node-pos-mv (if (string-equal side 'left)
										   (pos~add-front 2 (pos~empty))
										 (pos~add-front 1 (pos~empty))))
			      )
			  (meth~mapp-new-constraint cmapp nil))))))))

(meth~new-relational-function 'find-partner-in-chain)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Hopefully) obsolete stuff
;;
;; The stuff here is subsumed by other functions or was duplicate
;; code. It is only kept in case something goes horribly wrong
;; somewhere.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| 
(defun methhelp=the-numbers-to-resclass-set (formula)
  (declare (edited  "24-MAY-2000")
	   (authors Ameier)
	   (input   "A formula representing a residue-class set.")
	   (effect  "None.")
	   (value   "A list of numbers, representing the number set to the formula."))
  (let* ((env (pds~environment omega*current-proof-plan)))
    (cond (;; 1.) (resclass-set n)
	   ;;     Formula is an application, starts with resclass-set, and has type o <- (o <- num)
	   
	   (zmz=resclass-set-formula-p formula)
	   (let* ((class-factor (first (data~appl-arguments formula))))
	     (zmztac=count-first-n-nats class-factor)))
	  (;; 2.) 2.1.: (setminus (resclass-set n) (singleton (resclass n m))) or
	   ;;     2.2.: (setminus (resclass-set n) (lam (x (o num)) (or (= x (resclass n m)) .... )))
	   ;;     Formula is an application, starts with setminus, has two arguments, the first argument satisfies the conditions of
	   ;;     the first case.
	   (and (data~appl-p formula)
		(keim~equal (data~appl-function formula)
			    (data~schema-range (env~lookup-object :setminus
								  (pds~environment omega*current-proof-plan))))
		(= (length (data~appl-arguments formula)) 2))
	   (multiple-value-bind
	       (flag1 class-factor1)
	       (zmz=resclass-set-formula-p (first (data~appl-arguments formula)))
	     ;; 2.1. (setminus (resclass-set n) (singleton (resclass n m)))
	     (multiple-value-bind
		 (flag2 class-factor2 residuum)
		 (zmz=resclass-singleton-p (second (data~appl-arguments formula)))
	       (if flag2
		   (remove residuum (zmztac=count-first-n-nats class-factor1) :test #'keim~equal)
		 (multiple-value-bind
		     (flag3 class-factor3 residues)
		     (zmz=or-set-of-resclass-p (second (data~appl-arguments formula)))
		   (if flag3
		       (set-difference (zmztac=count-first-n-nats class-factor1) residues :test #'keim~equal)
		     nil))))))
	  (;; 
	   (zmz=or-set-of-resclass-p formula)
	   
	   (multiple-value-bind
	       (flag class-factor residues)
	       (zmz=or-set-of-resclass-p formula)
	     
	     residues)))))

|#
