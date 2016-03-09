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

(in-package "OMEGA")


       
;;; The predicates an functions of the formal language of method conditions,
;; which is a first order language with quantifications on finite domains.
;;We have
;;  - connectors: MOR, MAND, MXOR ...
;;  - predicates: SUBTERM, ...
;;  - relational functions: ALPHA-MATCHER, BOUND, ...
;;  - functions: TERMTYPE, ...

;;; Functions can occur in the method computations or the mbind conditional.
;;; How to evaluate a condition, where a condition is either atomic (predicate,    
;; or relational function), or compound (with a connector as function symbol):
;;  - When condition is compound, then the evaluation of the arguments is delegated
;;   to the associated function
;;  - When condition is a relational function, then the last argument must be a method
;;   variable, and the first remaining arguments are evaluated before calling the
;;   associated function. When the last argument is bound, then its binding should be
;;   consistent with the result of the function.
;;   For instance, (alpha-matcher t1 t2 sub) is evaluated: to <T old-binding>
;;   when sub is bound in old-binding and sub(t1) == t2, to
;;   <T old-binding U {sub <- matcher(t1 t2)}> when t1 matches t2, and otherwise to <nil old-binding>.
;;   For instance, (bound phi) is evaluated to either <T old-binding> when phi is
;;   bound in old-binding.
;;  - Otherwise, condition is a predicate, then the arguments are evaluated before
;; calling the associated function

;;;
;;; New keywords
;;;

(meth~new-keyword :string)

(defmethod meth~build-with-keyword ((expr string) env (keyword (eql :string)))
  (meth~create-key-expression :string expr))

(defmethod meth~build-with-keyword ((expr symbol) env (keyword (eql :string)))
  (meth~create-key-expression :string (symbol-name expr)))

(meth~new-keyword :symbol)

;;; LC: now defined in method.lisp
;;(defmethod meth~build-with-keyword ((expr symbol) env (keyword (eql :symbol)))
;;  (meth~create-key-expression :symbol expr))
;;
;;(defmethod meth~pds-object ((meth-obj symbol) (mmapp meth+mapping) (indicator (eql :symbol)) &optional bvar-mapp)
;;           (declare (ignore bvar-mapp))
;;           meth-obj)


;;;LC:New
;;;Conventions
;; 1: meth=check-condition(cond,cmapp) returns
;;   - either a cmapp where its constraint can be NIL, T, or a cstr~constraint-p
;;   - or a non-empty list of cmapp-s whose constraints are all different from NIL.

;; 2: When meth=check-condition(cond,cmapp) returns a cmapp1, then it destructively
;;  changes cmapp, i.e., cmapp and cmapp1 are the same. When it returns a list of
;;  cmapps then each element of this list is a copy of cmapp, i.e., a new object with
;;  the same subst and mapp but with other extension and constraint. The first element
;;  in this list is a destructively changed cmapp. Moreover, the elements in this list
;;  should have distincts extensions. 
;; 3: To the destructive update of cmapp-extension: We have to choose between two
;;  possibilities: either copying cmapp-extension within the evaluation of connectors
;;   in two situations: before testing some conditions and when constructing new cmapps
;;  for a list of cmapps as result, or copying cmapp-extensions always when they are
;;  extended. We take the second alternative, because it is simpler to verify.
;; 4: meth~check-condition calls meth=check-condition with an empty extension and T 
;;  as constraint. When meth=check-condition returns a list of cmapp-s, we have to take
;;  into account that the subst, the mapp, and the constraint  of the returned results
;;  of meth~check-condition are not the same objects.
;; 5: When meth=check-condition returns a cmapp with a NIL-constraint, then the
;;  extension of cmapp must equal the original extension.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       I.  Definitions of CONNECTORS and ATOMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(meth~defcond mand (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad)
		       (input   "A list of conditionals (P1...Pn).")
		       (effect  )
		       (value 	"<C, mapp'>, if all conditionals evaluate to <Ci,...>"
				"and C is the final constraint."
				"<NIL, mapp>, otherwise.")
		       )
	      ;;; args = (p1,..,pn), cmapp has the slots: subst, mapp, extension (E)
              ;;; and constraint (C), where only E and C can be changed during the evaluation
              ;;; of conditions: <and(p1,..,pn),[E,C]> returns
              ;;;   <and(p2,..,pn),[E1,C1]> when <p1,[E,C]> = [E1,C1]
              ;;;   [E,NIL] when <p1,[E,C]> = [E,NIL]
              ;;;   Union{1-m: <and(p2,..,pn),[E1i,C1i]>} when <p1,[E,C]> = {[E11,C11],..,[E1m,C1m]}
	      (meth=evaluate-conjuncts args cmapp (meth~mapp-extension cmapp)))

(defun meth=evaluate-conjuncts (conjuncts cmapp old-extension)
  (declare (edited  "22-NOV-1998")
	   (authors Lassaad)
	   (input   "A list of method conditions, a constrained-mapping, and the old extension.")
	   (effect  "None.")
	   (value   "The last successful evaluation of the last element in CONJUNCTS with"
		    "possibly some new constraints, when each element can be successfully evaluated,"
		    "otherwise a negative constrained-mapping with OLD-EXTENSION."))
  (if conjuncts
      (let ((result1 (meth=check-condition (first conjuncts) cmapp)))
	(if (meth~mapping-p result1)
	    (cond ((meth~mapp-constraint result1)
		   (meth=evaluate-conjuncts (rest conjuncts) result1 old-extension))
		  (T ;;; result1 has NIL as constraint
		   (meth~mapp-new-extension result1 old-extension)))
	  ;;;result1 is a list of cmapp-s with positive constraints
	  (let (the-result)
	    (dolist (res result1)
	      (let ((res-res (meth=evaluate-conjuncts (rest conjuncts) res old-extension)))
		(if (meth~mapping-p res-res)
		    (when (meth~mapp-constraint res-res)
		      (setq the-result (append the-result (list res-res))))
		  (setq the-result (append the-result res-res)))))
	    (if the-result (if (rest the-result) the-result (first the-result))
	      ;; (meth~mapp-new-extension (meth~mapp-new-constraint cmapp NIL) old-extension)))))
	      ;; In such situation the constraint if CMAPP must be destructively changed to NIL
	      (meth~mapp-new-extension cmapp old-extension)))))
    cmapp))


(meth~defcond mnot (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input "A conditional")
		       (effect)
		       (value "<T, mapp'>, if the conditional evaluates to <NIL, mapp'>."
			      "<NIL, mapp> otherwise."))
	      ;;; args = (p)
	      ;; <not(p),[E,C]> returns
	      ;;    [E,C] if <p,[E,T]> = [E,NIL]
	      ;;    [E,NIL] if <p,[E,T]> = [E',T]
	      ;;    [E',and(C,not(C'))] if <p,[E,T]> = [E',C'] and C'#T
	      ;;    Union{1-m,Ci'#T: [Ei',and(C,not(Ci'))]} if <p,[E,T]> = {[E1',C1'],..,[Em',Cm']}
	      (let* ((old-extension (meth~mapp-extension cmapp))
		     (old-constraint (meth~mapp-constraint cmapp))
		     (result (meth=check-condition (first args)
						   (if (cstr~constraint-p old-constraint)
						       (meth~mapp-new-constraint cmapp T)
						     cmapp))))
		(if (meth~mapping-p result)
		    (let ((result-cstr (meth~mapp-constraint result)))
		      (cond ((null result-cstr)
			     (meth~mapp-new-constraint result old-constraint))
			    ((cstr~constraint-p result-cstr)
			     (meth~mapp-new-constraint result
						       (cstr~conjunction (list old-constraint (cstr~negate result-cstr)))))
			    (T
			     (meth~mapp-new-constraint (meth~mapp-new-extension result old-extension) NIL))))
		  ;;; result is a list of cmapp-s:
		  (let (the-result)
		    (dolist (res result)
		      (when (cstr~constraint-p (meth~mapp-constraint res))
			;;; res-cstr is either T or a constraint. When res-cstr is T, the associated res is ignored
			;; and when res-cstr is a constraint, the associated res is returned with the negation of res-cstr 
			(setq the-result
			      (append the-result
				      (list (meth~mapp-new-constraint
					     res (cstr~conjunction (list old-constraint
									 (cstr~negate (meth~mapp-constraint res))))))))))
		    (if the-result (if (rest the-result) the-result (first the-result))
		      (meth~mapp-new-constraint (meth~mapp-new-extension cmapp old-extension) NIL))))
		))


(meth~defcond mor (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A list of conditionals (P1...Pn).")
		       (effect  )
		       (value 	"<C, mapp'>, if at least one conditional evaluates to <C,mapp'>"
				"and C is the final constraint."
				"<NIL, mapp>, otherwise.")
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; args = (p1,..,pn) Returns
	      ;;    Union{1-n,Elti is a list or C(Elti)#NIL: <pi,[E,C]>} 
	      ;;    where this set is transformed to [E,NIL] when it is NIL
	      ;;                   and to Elt' when it is {Elt'}
	      ;; Note: We assume that the conditions of a method are so defined that in cases
	      ;; where <or(p1,..,pn),[E,C]> = {[E1,C1],..,[Em,Cm]} the Ek are different.
	      (let ((cmapp-cstr (meth~mapp-constraint cmapp)))
		(meth=evaluate-disjuncts args 
					 (if (cstr~constraint-p cmapp-cstr) 
					     (meth~mapp-new-constraint cmapp T)
					   cmapp)
					 cmapp-cstr
					 (meth~mapp-extension cmapp))))


(defun meth=evaluate-disjuncts (disjuncts cmapp constraint old-extension &optional accumulator)
  (declare (edited  "23-NOV-1998")
	   (authors Lassaad)
	   (input   "A list of method conditions, a method mapping, a constraint (the initial constraint),"
		    "a mapping (the initial extension mapping), and optionally a list of constraint-"
		    "extension mapping pairs.")
	   (effect  "None.")
	   (value   "A (negative/positive) method mapping or a list of method mappings depending on the"
		    "satisfiability of the DISJUNCTS."))
  (if disjuncts
      (let ((result1 (meth=check-condition (first disjuncts) cmapp)))
	(if (meth~mapping-p result1)
	    (cond ((meth~mapp-constraint result1)
		   (if (rest disjuncts)
		       (meth=evaluate-disjuncts (rest disjuncts)
						(meth~mapping-create (meth~mapp-subst cmapp)
								     (meth~mapp-mapp cmapp)
								     old-extension T)
						constraint old-extension
						(append accumulator
							(list (meth~mapp-new-constraint
							       result1
							       (cstr~conjunction (list constraint
										       (meth~mapp-constraint result1)))))))
		     (meth=evaluate-disjuncts NIL cmapp constraint old-extension
					      (append accumulator
						      (list (meth~mapp-new-constraint
							     result1
							     (cstr~conjunction (list constraint
										     (meth~mapp-constraint result1)))))))))
		  (T ;;; disjunct1 is evaluated to NIL
		   (meth=evaluate-disjuncts (rest disjuncts)
					    (meth~mapp-new-constraint cmapp T)
					    constraint old-extension accumulator)))
	  ;;;RESULT1 is a list of cmapp-s
	  (if (rest disjuncts)
	      (meth=evaluate-disjuncts (rest disjuncts)
				       (meth~mapping-create (meth~mapp-subst cmapp)
							    (meth~mapp-mapp cmapp)
							    old-extension T)
				       constraint old-extension
				       (append accumulator
					       (mapcar #'(lambda (mapp)
							   (meth~mapp-new-constraint
							    mapp
							    (cstr~conjunction (list constraint
										    (meth~mapp-constraint mapp)))))
						       result1)))
	    (meth=evaluate-disjuncts NIL cmapp constraint old-extension
				     (append accumulator
					     (mapcar #'(lambda (mapp)
							 (meth~mapp-new-constraint
							  mapp
							  (cstr~conjunction (list constraint
										  (meth~mapp-constraint mapp)))))
						     result1))))))
    (if accumulator (if (rest accumulator) accumulator (first accumulator))
      (meth~mapp-new-constraint cmapp NIL))
    ))


(meth~defcond mxor (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad)
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; args = (p1,..,pn) Returns
	      ;;    [Ei,C] when there is a i such that <pi,[E,T]> = [Ei,T]
	      ;;    [E,NIL] when for all i <pi,[E,T]> = [E,NIL]
	      ;;    Merge{1-m,Eltj is a list or C(Eltj) is a constraint: <pj,[E,T]>} 
	      ;;    where this set is transformed to [E,NIL] when it is NIL
	      ;;                   and to {[E1',C1'],..,[Eq',Cq']}, C1' is the conjunction
	      ;;                   of C and the disjunction of the constraints of Eltj
	      ;;                   with the same Es.
	      (let ((cmapp-cstr (meth~mapp-constraint cmapp)))
		(meth=evaluate-exclusive-disjuncts args 
						   (if (cstr~constraint-p cmapp-cstr) 
						       (meth~mapp-new-constraint cmapp T)
						     cmapp)
						   cmapp-cstr
						   (meth~mapp-extension cmapp))))
                        

(defun meth=evaluate-exclusive-disjuncts (disjuncts cmapp constraint old-extension &optional accumulator)
  (declare (edited  "23-NOV-1998")
	   (authors Lassaad)
	   (input   "a list of method conditions, a method mapping with T as constraint, a constraint (the"
		    "initial constraint), and a mapping (the initial extension mapping), and optionally an"
		    "accumulator for method mappings and for lists of method mappings to be merged.")
	   (effect  "None.")
	   (value   "A (negative/positive) method mapping or a list of method mappings as a result of"
		    "merging ACCUMULATOR."))
  (if disjuncts
      (let ((result1 (meth=check-condition (first disjuncts) cmapp)))
	(if (meth~mapping-p result1)
	    (cond ((null (meth~mapp-constraint result1)) 
		   (meth=evaluate-exclusive-disjuncts (rest disjuncts)
						      (meth~mapp-new-constraint cmapp T)
						      constraint old-extension accumulator)) 
		  ((cstr~constraint-p (meth~mapp-constraint result1))
		   (if (rest disjuncts)
		       (meth=evaluate-exclusive-disjuncts (rest disjuncts)
							  (meth~mapping-create (meth~mapp-subst cmapp)
									       (meth~mapp-mapp cmapp)
									       old-extension T)
							  constraint old-extension
							  (append accumulator (list result1)))
		     (meth=evaluate-exclusive-disjuncts NIL cmapp constraint old-extension
							(append accumulator (list result1)))))
		  (T
		   (meth~mapp-new-constraint result1 constraint)))
	  (if (rest disjuncts)
	      (meth=evaluate-exclusive-disjuncts (rest disjuncts)
						 (meth~mapping-create (meth~mapp-subst cmapp)
								      (meth~mapp-mapp cmapp)
								      old-extension T)
						 constraint old-extension
						 (append accumulator (list result1)))
	    (meth=evaluate-exclusive-disjuncts NIL cmapp constraint old-extension
					       (append accumulator (list result1))))))
    (if accumulator
	(if (rest accumulator)
	    (labels ((collect-ext-results (ext results)
					  ;; Returns the cmapp-s in RESULTS which have an extension that equals
					  ;; EXT, where the elements of RESULTS are either cmapp-s or lists of
					  ;; c-mapps. It returns as second value the elements of RESULTS which
					  ;; have extensions different from EXT.
					  (if results
					      (let ((result1 (first results)))
						(multiple-value-bind (ext-results rest-results)
						    (collect-ext-results ext (rest results))
						  (if (meth~mapping-p result1)
						      (cond ((keim~equal ext (meth~mapp-extension result1))
							     (values (cons result1 ext-results) rest-results))
							    (T
							     (values ext-results (cons result1 rest-results))))
						    ;;;RESULT1 is a list:
						    (let ((ext-mapp (find-if #'(lambda (mapp)
										 (keim~equal ext (meth~mapp-extension mapp)))
									     result1)))
						      (cond (ext-mapp
							     (values (cons ext-mapp ext-results)
								     (cons (if (third result1)
									       (remove ext-mapp result1)
									     (first (remove ext-mapp result1)))
									   rest-results)))
							    (T
							     (values ext-results (cons result1 rest-results))))))))
					    (values NIL NIL)))
		     (collect-same-ext-results (results)
					       ;; Collects the cmapp-s in RESULTS according to the same extensions,
					       ;; where RESULTS is a list of c-mapps and of cmapp lists, where
					       ;; cmapp lists have different extensions:
					       (if (rest results)
						   (let* ((result1 (first results))
							  (cmapp1 (if (meth~mapping-p result1) result1
								    (first result1))))
					;(format t "~%Step1: CMAPP1 is ~A" cmapp1)
						     (multiple-value-bind (ext-results rest-results)
							 (collect-ext-results (meth~mapp-extension cmapp1)
									      (rest results))
					;(format t "~%Step2: EXT-RESULTS is ~A~% and rest-results is ~A"
					;       ext-results rest-results)
						       (if (listp result1)
							   (cons (cons cmapp1 ext-results)
								 (collect-same-ext-results
								  (if (third result1)
								      (cons (rest result1) rest-results)
								    (cons (second result1) rest-results))))
							 (cons (cons cmapp1 ext-results)
							       (when rest-results
								 (collect-same-ext-results rest-results))))))
						 (if (meth~mapping-p (first results))
						     (list results)
						   (mapcar #'list (first results))))))
	      (let ((results  (collect-same-ext-results accumulator)))
					;(format t "~%Step3: RESULTS is ~A" results)
		(if (rest results)
		    (mapcar #'(lambda (res)
				(let ((T-res (find-if-not #'(lambda (mapp)
							      (cstr~constraint-p (meth~mapp-constraint mapp)))
							  res)))
				  (if T-res
				      (meth~mapp-new-constraint T-res constraint)
				    (meth~mapp-new-constraint (first res)
							      (cstr~conjunction
							       (list 
								constraint
								(cstr~disjunction (mapcar #'meth~mapp-constraint res))))))))
			    results)
		  (let* ((sole-res (first results))
			 (T-res (find-if-not #'(lambda (mapp)
						 (cstr~constraint-p (meth~mapp-constraint mapp)))
					     sole-res)))
		    (if T-res
			(meth~mapp-new-constraint T-res constraint)
		      (meth~mapp-new-constraint (first sole-res)
						(cstr~conjunction
						 (list 
						  constraint
						  (cstr~disjunction (mapcar #'meth~mapp-constraint sole-res))))))))))
	  ;;;Only one accumulated result:
	  (let ((sole-res (first accumulator)))
	    (if (meth~mapping-p sole-res)
		(meth~mapp-new-constraint sole-res
					  (cstr~conjunction (list constraint (meth~mapp-constraint sole-res))))
	      ;;; A list of cmapp-s with different extensions:
	      (mapcar #'(lambda (res)
			  (meth~mapp-new-constraint res
						    (cstr~conjunction (list constraint (meth~mapp-constraint res)))))
		      sole-res))))
      ;;;All disjuncts was evaluated to NIL
      (meth~mapp-new-constraint cmapp NIL))
    ))


(meth~defcond mif (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad)
		       (input   "Three conditionals p1 p2 and p3.")
		       (effect  )
		       (value   "(mif p1,p2,p3) avaluates to the same value"
				"as xor(and(p1,p2),and(not(p1),p3))"))
	      ;;; mif(p1,p2,p3) is the same as xor(and(p1,p2),and(not(p1),p3)). It is used
	      ;; for efficiency: args = (p1,p2,p3) Returns
	      ;;   <p2,[E1,C]> when <p1,[E,C]> = [E1,T]
	      ;;   <p3,[E,C]> when <p1,[E,C]> = [E,NIL]
	      ;; o/w either <p1,[E,T]> = [E1,C1]:
	      ;;            TE-Merge(<p2,[E1,T]>,<p3,[E1,T]>,C1,C,E)
	      ;;     or <p1,[E,T]> = {[E11,C11],..,[E1n,C1n]}:
	      ;;            for each i: TE-Merge(<p2,[E1i,T]>,<p3,[E1i,T]>,C1i,C,E)
	      ;; where TE-Merge links then- and else-results together with the same extensions using
	      ;; common if-constraint, and relates the rest then- and else-results to the cond-result
	      ;; by considering the cond-constraint respectively its negation:
	      (let* ((old-ext (meth~mapp-extension cmapp))
		     (old-cstr (meth~mapp-constraint cmapp))
		     (cond-res (meth=check-condition (first args)
						     (if (cstr~constraint-p old-cstr)
							 (meth~mapp-new-constraint cmapp T)
						       cmapp))))
		(cond ((meth~mapping-p cond-res)
		       (let ((cond-cstr (meth~mapp-constraint cond-res)))
			 (cond ((null cond-cstr)
				(meth=check-condition (third args)
						      (meth~mapp-new-constraint cond-res old-cstr)))
			       ((cstr~constraint-p cond-cstr)
				(meth=evaluate-then-else (second args) (third args) cond-res cond-cstr old-cstr old-ext))
			       (T ;;;cond is evaluated to T
				(meth=check-condition (second args)
						      (meth~mapp-new-constraint cond-res old-cstr))))))
		      (T ;;; cond-res is a non-empty list and each of its element has a satisfiable constraint:
		       (let (result)
			 (dolist (cres cond-res)
			   (let ((cond-cstr (meth~mapp-constraint cres)))
			     (if (cstr~constraint-p cond-cstr)
				 ;;;COND-CSTR = [C1,..]
				 (let ((then-else-res (meth=evaluate-then-else (second args) (third args) cres
									       cond-cstr old-cstr old-ext)))
				   (if (meth~mapping-p then-else-res)
				       (when (meth~mapp-constraint then-else-res)
					 (setq result (append result (list then-else-res))))
				     (setq result (append result then-else-res))))
			       ;;;COND-CSTR = [T,..]
			       (let ((then-res (meth=check-condition (second args) 
								     (meth~mapp-new-constraint cres old-cstr))))
				 (if (meth~mapping-p then-res)
				     (when (meth~mapp-constraint then-res)
				       (setq result (append result (list then-res))))
				   (setq result (append result then-res)))))))
			 (if result
			     (if (rest result) result (first result))
			   (meth~mapp-new-constraint (meth~mapp-new-extension cmapp old-ext) NIL)))))
		))


(defun meth=evaluate-then-else (then else ccmapp ccstr old-cstr old-ext)
  (declare (edited  "24-NOV-1998")
	   (authors Lassaad)
	   (input   "Two method conditions (THEN and ELSE of an mif-construct), a method"
		    "mapping resulting from evaluating the condition of this mif-construct,"
		    "the constraint of CCMAPP, and two objects of CCMAPP (OLD-EXT and OLD-CSTR)"
		    "before evaluating the mif-condition.")
	   (effect  "None.")
	   (value   "The result of evaluating THEN and ELSE in the context of CCMAPP taking into"
		    "account CCSTR."))
  ;; mif(p1,p2,p3): CCMAPP = <p1,[Ext,OC]> = [Extp1,C1] , then
  ;;   A: <p2,[Extp1,T]> = [Extp1,NIL]:
  ;;      A-1: <p3,[Extp1,T]> = [Extp1,NIL] return [Ext,NIL]
  ;;      A-2: <p3,[Extp1,T]> = [Extp3,C3]  return [Extp3,and(OC,not(C1),C3)]
  ;;   B: <p2,[Extp1,T]> = [Extp2,C2]:
  ;;      B-1: <p3,[Extp1,T]> = [Extp1,NIL] return [Extp2,and(OC,C1,C2)]
  ;;      B-2: <p3,[Extp1,T]> = [Extp3,C3]:
  ;;           B-2-i:  Extp3 and Extp2 are keim~equal: return [Extp2,and(OC,if(C1,C2,C3))]
  ;;           B-2-ii: o/w: return {[Extp2, and(OC,C1,C2)], [Extp3, and(OC,not(C1),C3)]}
  (let* ((cext (meth~mapp-extension ccmapp))
	 (then-res (meth=check-condition then (meth~mapp-new-constraint ccmapp T)))
	 (else-res (meth=check-condition else (meth~mapping-create (meth~mapp-subst ccmapp)
								   (meth~mapp-mapp ccmapp)
								   cext T))))
    (labels ((merge-then-else (then-cmapps else-cmapps ccstr old-cstr)
			      (if then-cmapps
				  (let* ((tcmapp1 (first then-cmapps))
					 (ecmapp1 (find-if #'(lambda (cmapp) (keim~equal (meth~mapp-extension tcmapp1)
											 (meth~mapp-extension cmapp)))
							   else-cmapps)))
				    (if ecmapp1
					(cons (meth~mapp-new-constraint
					       tcmapp1
					       (cstr~conjunction (list old-cstr
								       (cstr~then-else ccstr
										       (meth~mapp-constraint tcmapp1)
										       (meth~mapp-constraint ecmapp1)))))
					      (merge-then-else (rest then-cmapps) (remove ecmapp1 else-cmapps) ccstr old-cstr))
				      (cons (meth~mapp-new-constraint
					     tcmapp1
					     (cstr~conjunction (list old-cstr ccstr (meth~mapp-constraint tcmapp1))))
					    (merge-then-else (rest then-cmapps) else-cmapps ccstr old-cstr))))
				(mapcar #'(lambda (ecmapp)
					    (meth~mapp-new-constraint
					     ecmapp
					     (cstr~conjunction (list old-cstr (cstr~negate ccstr)
								     (meth~mapp-constraint ecmapp)))))
					else-cmapps))))
      (if (meth~mapping-p then-res)
	  (cond ((null (meth~mapp-constraint then-res)) ;;;A
		 (if (meth~mapping-p else-res)
		     (cond ((meth~mapp-constraint else-res) ;;;A-2
			    (meth~mapp-new-constraint else-res
						      (cstr~conjunction (list old-cstr (cstr~negate ccstr)
									      (meth~mapp-constraint else-res)))))
			   (T;;;A-1
			    (meth~mapp-new-extension else-res old-ext)))
		   ;;; ELSE-RES is a list
		   (mapcar #'(lambda (res) (meth~mapp-new-constraint res
								     (cstr~conjunction (list old-cstr (cstr~negate ccstr)
											     (meth~mapp-constraint res)))))
			   else-res)))
		(T ;;;B
		 (if (meth~mapping-p else-res)
		     (cond ((meth~mapp-constraint else-res) ;;;B-2
			    (merge-then-else (list then-res) (list else-res) ccstr old-cstr))
			   (T;;;B-1
			    (meth~mapp-new-constraint then-res
						      (cstr~conjunction (list old-cstr ccstr
									      (meth~mapp-constraint then-res))))))
		   ;;; ELSE-RES is a list
		   (merge-then-else (list then-res) else-res ccstr old-cstr))))
        ;;; THEN-RES is a list
	(if (meth~mapping-p else-res)
	    (cond ((meth~mapp-constraint else-res)
		   (merge-then-else then-res (list else-res) ccstr old-cstr))
		  (T
		   (mapcar #'(lambda (res) (meth~mapp-new-constraint res
								     (cstr~conjunction (list old-cstr ccstr
											     (meth~mapp-constraint res)))))
			   then-res)))
	  ;;; THEN-RES and ELSE-RES are lists
	  (merge-then-else then-res else-res ccstr old-cstr))))
    ))

(meth~defcond csplit-wrt (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad )
		       (input   )
		       (effect  )
		       (value   ))
	      ;; ARGS: MVAR
	      ;; Splits the constraint of CMAPP wrt. the bindings of MVAR.
	      (let ((mvar (first args)))
		(cond ((and (cstr~constraint-p (meth~mapp-constraint cmapp))
			    (find mvar (cstr~variables (meth~mapp-constraint cmapp))))
		       (multiple-value-bind (mvar-bindings mvar-otherwise)
			   (cstr=variable-paths (meth~mapp-constraint cmapp) mvar)
			 (let ((binding-alters (cstr=same-variable-paths mvar-bindings)))
			   (mapcar #'(lambda (cstr)
				       (meth~mapping-create (meth~mapp-subst cmapp)
							    (meth~mapp-mapp cmapp)
							    (meth~mapp-extension cmapp)
							    cstr))
				   (cond ((and binding-alters mvar-otherwise)
					  (append (mapcar #'cstr~conjunction binding-alters)
						  (list mvar-otherwise)))
					 (binding-alters
					  (mapcar #'cstr~conjunction binding-alters))
					 (mvar-otherwise
					  (list mvar-otherwise))
					 (T
					  (list T)))))))
		      (T
		       cmapp))
		))

(meth~defcond mexists (args cmapp)
	      (declare (edited  "10-DEC-1999")
		       (authors Lassaad)
		       (input   "A meta-variable, a list of values (t1,...,tn) and a condition P(var).")
		       (effect  )
		       (value   "<C,mapp'>, if there exists an i, with C({Var <- ti}) evaluating to <C, mapp'>"
				" and C is not NIL"))
	      ;;; args = var (t1,..,tn) C(var) Returns
	      ;;    Union{1<=i<=n: <C(var),[E U {var <- ti},C]> is positive}
	      (let ((mvar (first args))
		    (alternatives (second args))
		    (constraint (meth~mapp-constraint cmapp))
		    (extension (meth~mapp-extension cmapp))
		    results)
		(if (ssterm~var-p mvar)
		    (dolist (alternative alternatives
				     (if results
					 (if (rest results) results (first results))
				       (meth~mapp-new-constraint (meth~mapp-new-extension cmapp extension) NIL)))
		      (let ((match-subst (ssterm~alpha-match mvar alternative
							     :assocs (logic~quantifiers
								      :theory (prob~theory omega*current-proof-plan))
							     :subst (meth~mapping-copy (meth~mapp-subst cmapp)))))
			(when match-subst
			  (if (rest match-subst)
			      (omega~error ";;;mexists: should be extended.")
			    (let* ((new-extension
				    (if extension
					(meth=union-mappings (subst~domain (first match-subst))
							     (subst~codomain (first match-subst))
							     (meth~mapp-subst cmapp)
							     (meth~mapping-copy extension))
				      (meth=union-mappings (subst~domain (first match-subst))
							   (subst~codomain (first match-subst))
							   (meth~mapp-subst cmapp))))
				   (res (meth=check-condition (third args)
							      (meth~mapping-create (meth~mapp-subst cmapp)
										   (meth~mapp-mapp cmapp)
										   new-extension constraint))))
			      (if (meth~mapping-p res)
				  (when (meth~mapp-constraint res)
				    (push res results))
				(setq results (append results res))))))))
		  (if (find mvar alternatives :test #'keim~equal)
		      (meth=check-condition (third args) cmapp)
		    (meth~mapp-new-constraint cmapp  NIL)))
		))


(meth~defcond mforall (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad)
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; args = var (t1,..,tn) C(var) Returns
	      ;;    cmapp, when for 1<=i<=n: <C(var),[E U {var <- ti},C]> is positive
	      (let ((mvar (first args))
		    (alternatives (second args)))
		(if alternatives
		    (let ((constraint (meth~mapp-constraint cmapp))
			  (extension (meth~mapp-extension cmapp)))
		      (if (ssterm~var-p mvar)
			  (let ((successp T)
				(res-cstrs (list constraint))
				(type-subst (subst~create (remove-if-not #'type~p
									 (append (subst~domain (meth~mapp-subst cmapp))
										 (when extension (mapp~domain extension))))
							  (remove-if-not #'type~p
									 (append (subst~codomain (meth~mapp-subst cmapp))
										 (when extension (mapp~codomain extension)))))))
			    (dolist (alternative alternatives)
			      (let ((match-subst (ssterm~alpha-match mvar alternative
								     :assocs (logic~quantifiers
									      :theory (prob~theory omega*current-proof-plan))
								     :subst (subst~create (subst~domain type-subst)
											  (subst~codomain type-subst)))))
				(if match-subst
				    (if (rest match-subst)
					(omega~error ";;;mforall: should be extended.")
				      (let* ((new-extension
					      (meth=union-mappings (subst~domain (first match-subst))
								   (subst~codomain (first match-subst))
								   (meth~mapp-subst cmapp)
								   (if extension
								       (mapp~create (mapp~domain extension)
										    (mapp~codomain extension))
								     (mapp~create nil nil))))
					;					     (res (meth=check-condition (third args)
					;                                                                        (meth~mapping-create (meth~mapp-subst cmapp)
					;                                                                                             (meth~mapp-mapp cmapp)
					;                                                                                             new-extension constraint))))
					     (res (meth=check-condition (third args)
									(meth~mapping-create (meth~mapp-subst cmapp)
											     (meth~mapp-mapp cmapp)
											     new-extension T))))
					(if (meth~mapping-p res)
					    (cond ((meth~mapp-constraint res)
						   (setq res-cstrs (append res-cstrs (list (meth~mapp-constraint res)))))
						  (T
						   (setq successp nil) (return)))
					  (omega~error ";;;mforall: should be extended for more than one result."))))
				  (progn (setq successp nil) (return)))))
			    (if successp
				(meth~mapp-new-constraint cmapp
							  (cstr~conjunction res-cstrs))
			      (meth~mapp-new-constraint cmapp  NIL)))
			(if (find mvar alternatives :test #'(lambda (x y) (not (keim~equal x y))))
			    (meth~mapp-new-constraint cmapp  NIL)
			  (meth=check-condition (third args) cmapp))))
		  cmapp)))

(defun meth=union-mappings (new-dom new-codom old-subst &optional (res-mapp (mapp~create nil nil)))
  ;;; Returns an extension of RES-MAPP with the bindings in NEW-DOM/NEW-CODOM which are not in OLD-SUBST
  (if new-dom
      (let ((var (first new-dom)))
	(if (or (mapp~get-component var res-mapp)
		(subst~get-component var old-subst))
	    (meth=union-mappings (rest new-dom) (rest new-codom) old-subst res-mapp)
	  (meth=union-mappings (rest new-dom) (rest new-codom) old-subst
			       (mapp~insert-component! var (first new-codom) res-mapp))))
    res-mapp))

(meth~defcond mtrue (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "Nothing.")
		       (effect  )
		       (value   "<T, mapp>."))
	      cmapp)
			 
(meth~defcond mfalse (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "Nothing.")
		       (effect  )
		       (value   "<NIL, mapp>."))
	      (meth~mapp-new-constraint cmapp nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           II.   Some basic predicates 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(meth~defcond mequal (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "Two objects (OBJ1 and OBJ2).")
		       (effect  )
		       (value   "<T, mmapp> when OBJ1 and OBJ2 are keim~equal"
				"<NIL, mmapp>, otherwise."))
	      (meth~mapp-new-constraint cmapp (keim~equal (first args) (second args))))


(meth~defcond mtermsalphaequal (args cmapp)
	      (declare (edited  "19-MAY-2003")
		       (authors AMeier)
		       (input   "Nothing.")
		       (effect  )
		       (value   "<T, mapp>."))
	      (let* ((term1 (first args))
		     (term2 (second args)))
		(if (and (term~p term1)
			 (term~p term2))
		    (meth~mapp-new-constraint cmapp (term~alpha-equal term1 term2))
		  (meth~mapp-new-constraint cmapp NIL))))


(meth~defcond bound-p (args cmapp)
	      (declare (edited  "03-FEB-2000" )
		       (authors Jzimmer )
		       (input   "A Meta-variable VAR.")
		       (effect  )
		       (value   "<T, mmapp> iff VAR is bound to an object other than"
				"<NIL, mmapp>, otherwise."))

	      (if (ssterm~var-p (first args))
		  (meth~mapp-new-constraint cmapp
					    NIL)
		(meth~mapp-new-constraint cmapp
					  T)))

(meth~new-relational-function 'bound-p)

(meth~defcond const-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "T, iff TERM is a constant."))
	      (meth~mapp-new-constraint cmapp
					(term~constant-p (first args))))

(meth~defcond set-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "T, iff TERM is a constant."))
	      (meth~mapp-new-constraint cmapp
					(term~set-p (first args))))

(meth~defcond atom-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "An OBJECT.")
		       (effect  )
		       (value   "<T, mmapp> if OBJECT is an atom."
				"<NIL, mmapp>, otherwise."))
	      (if (logic~atom-p (first args) :theory (prob~theory omega*current-proof-plan)) cmapp
		(meth~mapp-new-constraint cmapp NIL)))

(meth~defcond literal-p (args cmapp)
	      (declare (edited  "6-Jun-2000")
		       (authors Ameier)
		       (input   "An OBJECT.")
		       (effect  )
		       (value   "<T, mmapp> if OBJECT is a literal."
				"<NIL, mmapp>, otherwise."))
	      (if (or (logic~atom-p (first args) :theory (prob~theory omega*current-proof-plan))
		      (and (logic~negation-p (first args) :theory (prob~theory omega*current-proof-plan))
			   (logic~atom-p (first (data~appl-arguments (first args))) :theory (prob~theory omega*current-proof-plan))))
		  cmapp
		(meth~mapp-new-constraint cmapp NIL)))


(meth~defcond meta-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "An OBJECT.")
		       (effect  )
		       (value   "<T, mmapp> when OBJECT is a meta-variable."
				"<NIL, mmapp>, otherwise."))
	      (if (meta~p (first args)) cmapp
		(meth~mapp-new-constraint cmapp NIL)))


(meth~defcond appl-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "T, iff the TERM is an application." ))
	      (if (data~appl-p (first args)) cmapp
		(meth~mapp-new-constraint cmapp NIL)))


(meth~defcond abstr-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "T, iff the TERM is an abstraction." ))
	      (meth~mapp-new-constraint cmapp
					(data~abstr-p (first args))))

(meth~defcond conjunction-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "<T, mmapp> when TERM is a conjunction"
				"<NIL, mmapp>, otherwise."))
	      (meth~mapp-new-constraint cmapp
					(logic~conjunction-p (first args) :theory (prob~theory omega*current-proof-plan))))

(meth~defcond disjunction-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "<T, mmapp> when TERM is a disjunction"
				"<NIL, mmapp>, otherwise."))
	      (meth~mapp-new-constraint cmapp
					(logic~disjunction-p (first args) :theory (prob~theory omega*current-proof-plan))))

(meth~defcond universal-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "<T, mmapp> when TERM is a universal quantification."
				"<NIL, mmapp>, otherwise."))
	      (meth~mapp-new-constraint cmapp
					(logic~universal-quantification-p (first args)
									  :theory (prob~theory omega*current-proof-plan))))

(meth~defcond existential-p (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "<T, mmapp> when TERM is an existential quantification."
				"<NIL, mmapp>, otherwise."))
	      (meth~mapp-new-constraint cmapp
					(logic~existential-quantification-p (first args)
									    :theory (prob~theory omega*current-proof-plan))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         III.  Some basic functions for the construction and
;;                decomposition of terms, lists and positions
;;                       and for term normalization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;---------------------------------------------------
;;        term decomposition
;;---------------------------------------------------
(meth~deffun termtype (term)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A term TERM.")
		      (effect  )
		      (value   "The type of TERM."))
	     (term~type term))


(meth~deffun prlnformula (pds-node)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A pdsn+node NODE.")
		      (effect  )
		      (value   "The formula of NODE."))
	     (node~formula pds-node))

(meth~deffun currprlnformula (node)
	     (declare (edited  "31-MAY-2001")
		      (authors Ameier)
		      (input   "A pdsn+node NODE.")
		      (effect  )
		      (value   "The original formula of NODE."))
	     (let* ((current-cstr-pool (pds~constraint-pool omega*current-proof-plan))
		    (mvar-subst (if current-cstr-pool
				    (pds~cstrpool-bindings current-cstr-pool)
				  nil)))
	       (if (pdsn~schematic-p node)
		   (progn
		     (when (and mvar-subst
				(null (pdsn~up-to-date node)))
		       (setf (pdsn~current-formula node)
			     (subst~apply mvar-subst (node~formula node)))
		       (setf (pdsn~up-to-date node) 't))
		     (pdsn~current-formula node))
		 (node~formula node))))

(meth~deffun prlnhyps (pds-node)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A pdsn+node NODE.")
		      (effect  )
		      (value   "The set of hypothesis of NODE."))
	     (pdsn~hyps pds-node))

(meth~deffun applfunc (formula)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "An application APPL.")
		      (effect  )
		      (value   "The function of APPL."))
	     (data~appl-function formula))

(meth~deffun applargs (formula)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "An application APPL.")
		      (effect  )
		      (value   "The list of arguments of APPL."))
	     (data~appl-arguments formula))

(meth~deffun constants (term)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A term TERM.")
		      (effect  )
		      (value   "All constants that occur in TERM."))
	     (remove-duplicates (remove-if-not #'term~constant-p (data~all-substructs term))))

(meth~deffun metavars (object)
             (declare (edited  "11-AUG-1999")
                      (authors Lassaad Jzimmer)
                      (input   "A term or a node.")
                      (effect  )
                      (value   "All meta-variables that occur in OBJECT."))
             (cond ((or (pdsn~p object) (pdsn~schematic-p object))
                    (let* ((nodes1 (cons object (pdsn~hyps object)))
                           (nodes2 (remove-if-not #'pdsn~schematic-p nodes1))
                           mvars)
                      (dolist (node nodes2 (remove-duplicates mvars))
                        (setq mvars (append mvars
                                            (remove-if-not #'meta~p (data~all-substructs (pds~node-formula node))))))))
                   ((term~p object)
                    (remove-duplicates (remove-if-not #'meta~p (data~all-substructs object))))
                   (T
                    (omega~error ";;;metavars: unallowed argument ~A." object))))

(meth~deffun free-vars (formula)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A term TERM.")
		      (effect  )
		      (value   "A list of all free variables in TERM."))
	     (cstr~free-variables formula))

(meth~deffun first-uvars (formula)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A term TERM.")
		      (effect  )
		      (value   "The list of first universal quantified variables of TERM."))
	     (meth=first-uvars formula))

(defun meth=first-uvars (formula)
  (when (logic~universal-quantification-p formula)
    (cons (logic~quantification-bound-variable formula)
	  (meth=first-uvars (logic~quantification-scope formula)))
    ))


;;---------------------------------------------------
;;         Construction of terms and proof-lines
;;---------------------------------------------------

(meth~deffun abstr-create (vars term)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A function symbol FUNC and a list ARGS of terms .")
		      (effect  )
		      (value   "The application (FUNC ARGS)."))
	     (term~abstr-create vars term))

(meth~deffun appl-create (head arglist)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A function symbol FUNC and a list ARGS of terms .")
		      (effect  )
		      (value   "The application (FUNC ARGS)."))
	     (term~appl-create head arglist))

(meth~deffun just-create (meth-name params prems &optional (status "unexpanded"))
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A method-name, a list of parameters, a list of premises,"
			       "and optional a status.")
		      (effect  )
		      (value   "The corresponding just+justification." ))
	     (pdsj~closed-just-create (infer~find-method meth-name)
				      prems params status))

(meth~deffun emptysubst (args)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "Nothing.")
		      (effect  )
		      (value   "The empty substitution {}."))
	     (subst~create nil nil))

(meth~deffun subst-create (vars terms)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A list (V1...Vn) of variables and a list (T1...Tn) of terms.")
		      (effect  )
		      (value   "The subsitution {V1<-T1,...,Vn<-Tn}." ))
	     (subst~create vars terms))

(meth~deffun otype (cmapp)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "Nothing.")
		      (effect  )
		      (value   "The type o."))
	     (type~o))

(meth~deffun subst-apply (subst term)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A substitution SUBST and a term TERM.")
		      (effect  )
		      (value   "TERM after the application of SUBST."))
	     (let ((new-object (subst~apply subst term :test #'data~equal))
		   )
	       new-object))

(meth~deffun pushneg (formula)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A term TERM.")
		      (effect  )
		      (value   "TERM after the application of pushneg."))
	     (pds~pushneg formula))

(meth~deffun pushneg* (formula)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A term TERM.")
		      (effect  )
		      (value   "TERM after the recursive application of pushneg."))
	     (pds~pushneg-rec formula))

(meth~deffun var-newconst (var)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A variable VAR.")
		      (effect  )
		      (value   "A new constant with the type of VAR."))
	     (term~generate-term-primitive-with-new-name (keim~name var) (term~type var) 'term+constant
							 (pds~environment omega*current-proof-plan)))

(meth~deffun vars-newconsts (vars)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A list VARS of variables.")
		      (effect  )
		      (value   "A list of new constants with the same types as VARS." ))
	     (let ((env (pds~environment omega*current-proof-plan)))
	       (mapcar
		#'(lambda (var)
		    (term~generate-term-primitive-with-new-name (keim~name var) (term~type var)
								'term+constant env))
		vars)))

(meth~deffun consts-newconsts (consts)
	     (declare (edited  "16-JUN-2003" )
		      (authors Vxs  )
		      (input   "A list of constants.")
		      (effect  )
		      (value   "A list of new constants with the same types as CONSTS." ))
	     (let ((env (pds~environment omega*current-proof-plan)))
	       (mapcar
		#'(lambda (const)
		    (if (term~special-p const)
			(term~generate-term-primitive-with-new-name 'c (term~type const)
								    'term+constant env)
		      (term~generate-term-primitive-with-new-name (keim~name const) (term~type const)
								  'term+constant env)))
		consts)))

(meth~deffun var-newvar (var)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A variable VAR.")
		      (effect  )
		      (value   "A new variable with the type of VAR."))
	     ;;; Returns: a new variable with the same type as VAR
	     (term~generate-term-primitive-with-new-name (keim~name var) (term~type var) 'term+variable
							 (pds~environment omega*current-proof-plan)))

(meth~deffun vars-newvars (vars)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A list VARS of variables.")
		      (effect  )
		      (value   "A list of new variables with the same types as VARS." ))
	     (let ((env (pds~environment omega*current-proof-plan)))
	       (mapcar
		#'(lambda (var)
		    (term~generate-term-primitive-with-new-name (keim~name var) (term~type var)
								'term+variable env))
		vars)))

(meth~deffun type-newvar (type)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A type TYPE.")
		      (effect  )
		      (value   "A new variable with type TYPE."))
	     (term~generate-term-primitive-with-new-name 'var type 'term+variable
							 (pds~environment omega*current-proof-plan)))

(meth~deffun types-newvars (types)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A list TYPES of types.")
		      (effect  )
		      (value   "A list of variables of the type TYPES."))
	     (let ((env (pds~environment omega*current-proof-plan)))
	       (mapcar
		#'(lambda (type)
		    (term~generate-term-primitive-with-new-name 'var type
								'term+variable env))
		types)))

(meth~deffun type-newconst (type)
  (declare (edited  "03-NOV-1999")
	   (authors Gebhard)
	   (input   "A type")
	   (effect  "nil")
	   (value   "A new constant with the given type."))
  (term~generate-term-primitive-with-new-name 'const type 'term+constant
					      (pds~environment
					       omega*current-proof-plan)))

  
(meth~deffun NewMetaVarForVar (var)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A variable VAR.")
		      (effect  )
		      (value   "A new meta-variable with the type of VAR and a name"
			       "similar to the name of VAR."))
	     (meth=defn-newmetavar (keim~name var)
				   (term~type var)))

(meth~deffun metavarsforvars (varlist)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A list VARS of variables.")
		      (effect  )
		      (value   "A list of new meta-variables with the types of VARS and names"
			       "similar to the names of VARS."))
	     (mapcar #'(lambda (x) (meth=defn-newmetavar x (term~type x)))
		     varlist))


(meth~deffun newmetavar (name-postfix type)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A name postfix APPX and a type TYPE.")
		      (effect  )
		      (value   "A new meta-variable with type TYPE and name 'M_'APPX."))
	     (let* ((name (if name-postfix
			      (concatenate 'string "M_" (symbol-name name-postfix))
			    "M"))
		    (count 0)
		    (env (pds~environment omega*current-proof-plan))
		    (exists (env~lookup-object name env :test #'string-equal))
		    )
	       (if (null exists)
		   (let* ((sym-name (intern name))
			  (new-var (meta~variable-create sym-name
							 type)))
		     (env~enter sym-name new-var env)
		     (setf plan*meta-vars
			   (union (list new-var)
				  plan*meta-vars))
		     new-var)
		 (loop 
		  (incf count)
		  (let ((new-name (format nil "~A~A" name count)))
		    (unless (env~lookup-object new-name env :test #'string-equal)
		      (let* ((sym-name (intern new-name))
			     (new-var (meta~variable-create sym-name
							    type)))
			(env~enter sym-name new-var env)
			(return-from meth=defn-newmetavar new-var))))))))

(meth~deffun NewConstForVar (var)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A variable VAR.")
		      (effect  )
		      (value   "A new constant with the type of VAR and a name"
			       "that's similar to the name of VAR."))
	     (let ((name (keim~name var))
		   (count 0)
		   (env (pds~environment omega*current-proof-plan))
		   )
	       (if (not (env~lookup-object name env :test #'string-equal))
		   (let* ((new-const (term~constant-create name (term~type var))))
		     (env~enter name new-const env)
		     new-const)
		 (loop 
		  (incf count)
		  (when (> count 20)
		    (return-from meth=defn-newconstforvar var))
		  (let ((new-name (format nil "~A~A" name count)))
		    (unless (env~lookup-object new-name env :test #'string-equal)
		      (let* ((sym-name (intern new-name))
			     (new-const (term~constant-create sym-name
							      (term~type var))))
			(env~enter sym-name new-const env)
			(return-from meth=defn-newconstforvar new-const))))))))

(meth~deffun newprln (hyps formula)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A list HYPS of hypotheses and a formula F." )
		      (effect  )
		      (value   "The proof-line: NewName HYPS |- F."))
	     (let* ((label (pds~new-node-name))
		    (just (pdsj~open-just-create))
		    (new-node (pdsn~create label hyps formula just))
		    )
	       new-node))

(meth~deffun premises-for (node formulas)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A pdsn+node NODE and a list of formulas Fs.")
		      (effect  )
		      (value   "A list of nodes with formulas Fs to be used as premises for NODE."))
	     (mapcar #'(lambda (formula)
			 (pdsn~create (pds~new-node-name omega*current-proof-plan)
				      (pdsn~hyps node) formula (pdsj~open-just-create)))
		     formulas))

;;---------------------------------------------------
;;      Functions for the normalization of terms
;;---------------------------------------------------
(meth~deffun beta-normalize (term)
	     (declare (edited  "11-AUG-1999")
		      (authors Jzimmer)
		      (input   "A term TERM.")
		      (effect  )
		      (value   "The beta-normal-form of TERM."))
	     (beta~normalize term))

(meth~deffun n-normalize (term)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "A term TERM.")
		      (effect  )
		      (value   "The n-normal-form of TERM."))
	     (data~n-normalize term :destructive t))

;;---------------------------------------------------
;;        List handling
;;---------------------------------------------------
(meth~deffun mnil (cmapp)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "Nothing.")
		      (effect  )
		      (value   "The empty list."))
	     nil)

(meth~deffun mcons (atom list)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "An object OBJ and a list LIST.")
		      (effect  )
		      (value   "(cons OBJ LIST)"))
	     (cons atom list))

(meth~deffun mlist (arg1 &rest args)
	     (declare (edited  "12-AUG-1999")
		      (authors Jzimmer)
		      (input   "An object OBJ and a list LIST of objects.")
		      (effect  )
		      (value   "(cons OBJ LIST)."))
	     (cons arg1 args))


(meth~deffun munion (list1 list2)
	     (declare (edited  "12-AUG-1999")
		      (authors Jzimmer)
		      (input   "Two lists (L1 and L2).")
		      (effect  )
		      (value   "The union of the two lists."))	     
	     (union list1 list2))

(meth~deffun mset-difference (set1 set2)
  (declare (edited  "07-MAR-2000")
	   (authors Sorge)
	   (input   "Two sets.")
	   (effect  "None.")
	   (value   "The difference between set1 and set2 with respect to KEIM~~EQUAL. The order is preserved."))
  (labels ((rec-set-difference (set1 set2)
			       (when set1
				 (if (find (car set1) set2 :test #'keim~equal)
				     (rec-set-difference (cdr set1) set2)
				   (cons (car set1) (rec-set-difference (cdr set1) set2))))))
    (rec-set-difference set1 set2)))

(meth~deffun mappend (list1 list2)
	     (declare (edited  "12-AUG-1999")
		      (authors Jzimmer)
		      (input   "Two lists (L1 and L2).")
		      (effect  )
		      (value   "The two lists are append."))
	     (append list1 list2))

(meth~deffun mrest (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The all but the first element of LIST."))
	     (rest list))
(meth~deffun mlast (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The last element of LIST."))
	     (first (last list)))
(meth~deffun mbutlast (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "All but the last element of LIST."))
	     (butlast list))

(meth~deffun mfirst (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The first element of LIST."))
	     (first list))
(meth~deffun msecond (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The second element of LIST."))
	     (second list))
(meth~deffun mthird (list)
	    	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The third element of LIST."))
	     (third list))
(meth~deffun mfourth (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The fourth element of LIST."))
	     (fourth list))
(meth~deffun mfifth (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The fifth element of LIST."))
	     (fifth list))
(meth~deffun msixth (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The sixth element of LIST."))
	     (sixth list))
(meth~deffun mseventh (list)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A list LIST.")
		      (effect  )
		      (value   "The seventh element of LIST."))
	     (seventh list))


;;---------------------------------------------------
;;        Postion handling
;;---------------------------------------------------
(meth~deffun posappend (pos-1 pos-2)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "Two positions (P1 and P2).")
		      (effect  )
		      (value   "P1.P2 ."))
             (pos~concatenate pos-1 pos-2))

(meth~deffun posbutlast (position)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A positions POS.")
		      (effect  )
		      (value   "(butlast POS)."))
	     (pos~butlast position))

(meth~deffun termatpos (term position)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A term TERM and a position POS.")
		      (effect  )
		      (value   "The subterm of TERM at position POS, if it exists."
			       "TERM, otherwise."))
             (let ((poss (data~positions term #'(lambda (substruct) T))))
	       (if (member position poss :test #'keim~equal)
		   (data~struct-at-position term position)
		 term)))

(meth~deffun TermsAtPositions (term positions)
  (declare (edited  "27-MAR-2000")
	   (authors Sorge)
	   (input   "A term and a list of positions.")
	   (effect  "None.")
	   (value   "A list of sub-terms at the given positions if all positions exist."
		    "O/W a list containing term."))
  (let ((poss (data~positions term #'(lambda (substruct) (declare (ignore substruct)) T))))
    (if (every #'(lambda (position) (find position poss :test #'keim~equal)) positions)
	(mapcar #'(lambda (position) (data~struct-at-position term position)) positions)
      (list term))))

(meth~deffun TermRplOccs (term of-term by-term)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A term TERM, a subterm SUB and a term REPL.")
		      (effect  )
		      (value   "TERM after replacing all occurances of SUB by"
			       "REPL."))
	     (data~replace-struct term of-term by-term))

(meth~deffun TermRplAtPos (term-1 pos term-2)
	     (declare (edited  "11-AUG-1999")
		      (authors  Jzimmer)
		      (input   "A term TERM, a position POS and a term REPL.")
		      (effect  )
		      (value   "TERM after replacing the subterm at position POS"
			       "by REPL."))
             (data~replace-at-position term-1 pos term-2))

(meth~deffun emptypos (args)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "Nothing.")
		      (effect  )
		      (value   "The empty position ()."))
	     (pos~empty))

(meth~deffun listposition (number-list)
  (declare (edited  "09-MAR-2000")
	   (authors Ameier)
	   (input   "A list of numbers.")
	   (effect  "None.")
	   (value   "A position with these number-list."))
  (pos~list-position number-list))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         IV.  Some conditionals for matching, unification and 
;;                 the search for subterms, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------
;;    conditionals for matching and unification
;;--------------------------------------------------------------
(meth~defcond alpha-matcher (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "Two terms (T1, T2) and a meta-variable SUB for the alpha-matcher.")
		       (effect  )
		       (value   "<T, mapp'>, if either SUB is bound and SUB(T1) == T2,"
				"or T1 matches T2 and SUB is bound to the alpha-matcher."
				"<NIL, mapp>, otherwise." ))
	      (let* ((mapp (meth~mapp-mapp cmapp))
		     (sub (third args))
		     (pds-subst (or (mapp~get-component sub mapp)
				    (and (meth~mapp-extension cmapp)
					 (mapp~get-component sub (meth~mapp-extension cmapp))))))
		(if pds-subst
		    (if (term~alpha-equal (subst~apply pds-subst (first args))
					  (second args))
			cmapp
		      (meth~mapp-new-constraint cmapp nil))
		  (let ((matcher (term~alpha-match (first args) (second args))))
		    (if matcher
			(meth~mapp-new-extension cmapp
						 (if (meth~mapp-extension cmapp)
						     (mapp~create (cons sub (mapp~domain (meth~mapp-extension cmapp)))
								  (cons matcher (mapp~codomain (meth~mapp-extension cmapp))))
						   (mapp~create (list sub) (list matcher))))
		      (meth~mapp-new-constraint cmapp nil))))
		))


(meth~defcond subterm-matcher (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad )
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; args: term, subterm with eventually free variables, matcher
	      (let ((term (first args))
		    (subterm (second args))
		    (matcher (third args)))
		(let ((term-subterms (remove-duplicates (data~all-substructs term)))
		      (substs))
		  (dolist (term-subterm term-subterms)
		    (let ((subst (term~alpha-match subterm term-subterm)))
		      (when subst
			(push subst substs))))
		  (if substs
		      (if (rest substs)
			  (let ((cmapp-ext (meth~mapp-extension cmapp)))
			    (cons (meth~mapp-new-extension cmapp
							   (if cmapp-ext
							       (mapp~create (cons matcher (mapp~domain cmapp-ext))
									    (cons (first substs) (mapp~codomain cmapp-ext)))
							     (mapp~create (list matcher) 
									  (list (first substs)))))
				  (mapcar #'(lambda (subst)
					      (meth~mapping-create (meth~mapp-subst cmapp)
								   (meth~mapp-mapp cmapp)
								   (if cmapp-ext
								       (mapp~create
									(cons matcher (mapp~domain cmapp-ext))
									(cons subst (mapp~codomain cmapp-ext)))
								     (mapp~create (list matcher) 
										  (list subst)))
								   (meth~mapp-constraint cmapp)))
					  (rest substs))))
			(meth~mapp-new-extension cmapp
						 (if (meth~mapp-extension cmapp)
						     (mapp~create (cons matcher (mapp~domain (meth~mapp-extension cmapp)))
								  (cons (first substs) (mapp~codomain (meth~mapp-extension cmapp))))
						   (mapp~create (list matcher) 
								(list (first substs))))))
		    (meth~mapp-new-constraint cmapp nil)))
		))

(meth~new-relational-function 'subterm-matcher)

(meth~defcond termmgu (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "Two terms (T1, T2) and a meta-variable MGU for a substitution.")
		       (effect  )
		       (value   "<T, mapp'>, if either MGU is bound and unifies T1 and T2,"
				"            or T1 and T2 are unifiable (in this case MGU is bound"
				"            to a most general unifier)."
				"<NIL, mapp>, otherwise."))
	      (let* ((mapp (meth~mapp-mapp cmapp))
		     (term1 (first args))
		     (term2 (second args))
		     (sigma (third args))
		     (pds-sigma (mapp~get-component sigma mapp)))
		(if pds-sigma
		    (if (term~alpha-equal (subst~apply sigma term1)
					  (subst~apply sigma term2))
			cmapp
		      (meth~mapp-new-constraint cmapp nil))
		  (let ((hou-list (uni~unify (list (list term1 term2))
					     :destructive nil
					     :solutions 'one
					     :cost-limit cstr*unification-depth))
			)
		    (if hou-list
			(meth~mapp-extend-mapp cmapp sigma (uni~substitution (first hou-list)))
		      (meth~mapp-new-constraint cmapp nil))))
		))

(meth~defcond alphaunify (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Ameier)
		       (input   "Two terms (T1, T2) and a meta-variable MGU for a substitution.")
		       (effect  )
		       (value   "<T, mapp'>, if either MGU is bound and unifies T1 and T2,"
				"            or T1 and T2 are alpha unifiable (in this case MGU is bound"
				"            to a most general unifier)."
				"<NIL, mapp>, otherwise."))
	      (let* ((mapp (meth~mapp-mapp cmapp))
		     (term1 (first args))
		     (term2 (second args))
		     (sigma (third args))
		     (pds-sigma (mapp~get-component sigma mapp)))
		(if pds-sigma
		    (if (term~alpha-equal (subst~apply sigma term1)
					  (subst~apply sigma term2))
			cmapp
		      (meth~mapp-new-constraint cmapp nil))
		  
		  (let ((unifier (term~alpha-unify term1 term2)))
		    (if unifier
			(progn (meth~mapp-extend-mapp cmapp sigma unifier)
			       (meth~mapp-new-constraint cmapp t))
		      (meth~mapp-new-constraint cmapp nil))))))


(meth~new-relational-function 'alphaunify)
		
(meth~new-relational-function 'match-and-bind-subterm) 

(meth~defcond match-and-bind-subterm (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad )
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; ARGS: term1,term2,substitution,position
 	      ;;; Returns: <T,mmapp>, if term2 matches the subterm at the given position in term1
	      ;;;                     according to the given substitution.
	      ;;;                     Substitution and/or position can be unbound.
	      ;;;          <NIL, mmapp> otherwise
	      (let* ((term1 (first args))
		     (term2 (second args))
		     (subst (third args))
		     (pos (fourth args))
		     (pds-subst (when (subst~p subst) subst))
		     (pds-pos (when (pos~p pos) pos)))
		(when (or (and (not pds-subst) (not (ssterm~var-p subst)))
			  (and (not pds-pos) (not (ssterm~var-p pos))))
		  (omega~error "match-and-bind-subterm: wrong type for either the subst ~A or pos ~A"
			       subst pos))
		(multiple-value-bind (real-subst real-pos)
		    (let ((subpos (data~substruct-positions term1 term2 :test #'uni~syntactic-matcher)))
		      (cond
		       ((and pds-pos (some #'(lambda (x) (keim~equal pds-pos x)) subpos))
			(values (uni~syntactic-matcher term1
						       (data~struct-at-position term2
										pds-pos))
				pds-pos))
		       (subpos
			(values (uni~syntactic-matcher term1
						       (data~struct-at-position term2
										(car  subpos)))
				(car subpos)))))
		  (if (and real-subst real-pos)
			(cond ((and pds-pos pds-subst)
			     (meth~mapp-new-constraint cmapp (and (keim~equal pds-pos real-pos)
								  (keim~equal pds-subst real-subst))))
			    (pds-pos 
			     (if (keim~equal pds-pos real-pos)
				 (meth~mapp-extend-mapp cmapp  subst real-subst)
			       (meth~mapp-new-constraint cmapp nil)))
			    (pds-subst 
			     (if (keim~equal pds-subst real-subst)
				 (meth~mapp-extend-mapp cmapp pos real-pos)
			       (meth~mapp-new-constraint cmapp nil)))
			    (t (meth~mapp-extend-mapp cmapp pos real-pos)
			       (meth~mapp-extend-mapp cmapp subst real-subst)))
		    (meth~mapp-new-constraint cmapp nil)))))

;;--------------------------------------------------------------
;;    conditionals/functions for the search for subterms and subsymbols
;;--------------------------------------------------------------
(meth~defcond var-in (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "<T,mmapp>, if TERM contains a (meta-)variable."
				"<NIL, mmapp>, otherwise."))
	      (let ((subterms (remove-duplicates (data~all-substructs (first args)))))
		(meth~mapp-new-constraint cmapp (some #'term~variable-p subterms))))


(meth~defcond beta-redex-in (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "A term TERM.")
		       (effect  )
		       (value   "<T,mmapp>, if at least one beta-redex occurs in TERM."
				"<NIL, mmapp>, otherwise."))
	      (let ((subterms (remove-duplicates (data~all-substructs (first args)))))
		(meth~mapp-new-constraint cmapp (some #'beta~redex-p subterms))))


(meth~defcond meta-var-in-nodes (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A list NODES of pdsn+nodes.")
		       (effect  )
		       (value   "<T,mmapp>, if at least one of the formulas of the NODES is a meta-variable."
				"<NIL, mmapp>, otherwise."))
	      (meth~mapp-new-constraint cmapp
					(some #'(lambda (node)
						  (meta~p (node~formula node)))
					      (first args))))


(meth~defcond termoccs (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A (sub)term T1, a term T2 and a meta-variable OCCS for the occurances of T1 in T2.")
		       (effect  )
		       (value   "<T,mapp'>, if either OCCS is bound and T1 occurs in T2 at all"
				"           positions in OCCS,"
				"           or T1 occurs in T2 at least once (in this case, OCCS"
				"           is bound to the list of positions of T1 in T2."
				"<NIL, mapp>, otherwise."))
  
	      (let* ((mapp (meth~mapp-mapp cmapp))
		     (subi (first args))
		     (sub (if (term~schema-p subi)
			      (data~schema-range subi)
			    subi))
		     (term (second args))
		     (occs (third args))
		     (pds-occs (mapp~get-component occs mapp)))
		(if pds-occs
		    (if (every #'(lambda (pos)
				   (term~alpha-equal (struct~at-position pos term)
						     sub))
			       pds-occs)
			cmapp
		      (meth~mapp-new-constraint cmapp nil))
		  (if (ssterm~var-p occs)
		      (let ((pds-occs (data~substruct-positions sub term :test #'data~schema-equal))
			    )
			(if pds-occs
			    (progn (meth~mapp-extend-mapp cmapp occs pds-occs)
				   (meth~mapp-new-constraint cmapp T))
			  (meth~mapp-new-constraint cmapp NIL)))
		    (meth~mapp-new-constraint cmapp NIL)))))


(meth~defcond symboloccs (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "A name of a symbol SYM, a term T2 and a meta-variable OCCS for the occurances of SYM in T2.")
		       (effect  )
		       (value   "<T,mapp'>, if either OCCS is bound and SYM occurs in T2 at all"
				"           positions in OCCS,"
				"           or SYM occurs in T2 at least once (in this case, OCCS"
				"           is bound to the list of positions of SYM in T2."
				"<NIL, mapp>, otherwise."))
	      (let* ((mapp (meth~mapp-mapp cmapp))
		     (symbol (first args))
		     (sub (env~lookup-object symbol (pds~environment omega*current-proof-plan)))
		     (term (second args))
		     (occs (third args))
		     (pds-occs (mapp~get-component occs mapp)))
		(if pds-occs
		    (if (every #'(lambda (pos)
				   (term~alpha-equal (struct~at-position pos term)
						     sub))
			       pds-occs)
			cmapp
		      (meth~mapp-new-constraint cmapp nil))
		  (if (ssterm~var-p occs)
		      (let ((pds-occs (data~substruct-positions sub term))
			    )
			;;(omega~message "~& occs: ~S ~S" sub (type-of sub))
			;;(omega~message "~& occs: ~S ~S" occs (type-of occs))
			;;(omega~message "~& pds-occs: ~S" pds-occs)
			(if pds-occs
			    (progn (meth~mapp-extend-mapp cmapp occs pds-occs)
				   (meth~mapp-new-constraint cmapp T))
			  (meth~mapp-new-constraint cmapp NIL)))
		    (meth~mapp-new-constraint cmapp NIL)))))

(meth~new-relational-function 'symboloccs)


(meth~defcond ho-variable (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A variable and a formula.")
		       (effect  )
		       (value   "T, iff the variable occurs in the formula as a head of an application"
				"of as an atom."))
	      (let ((variable (first args))
		    (formula (second args)))
		(labels ((get-ho-variable (var term)
					  (if (data~appl-p term)
					      (cond ((logic~atom-p term :theory (prob~theory omega*current-proof-plan))
						     (term~alpha-equal var (data~appl-function term)))
						    ((logic~negation-p term :theory (prob~theory omega*current-proof-plan))
						     (get-ho-variable var (first (data~appl-arguments term))))
						    ((or (logic~existential-quantification-p
							  term
							  :theory (prob~theory omega*current-proof-plan))
							 (logic~universal-quantification-p
							  term
							  :theory (prob~theory omega*current-proof-plan)))
						     (get-ho-variable var (data~abstr-range (first (data~appl-arguments term)))))
						    (T
						     (or (get-ho-variable var (first (data~appl-arguments term)))
							 (get-ho-variable var (second (data~appl-arguments term))))))
					    (term~alpha-equal var term)))
			 )
		  (if (get-ho-variable variable formula) cmapp
		    (meth~mapp-new-constraint cmapp nil)))
		))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         V.  Some conditionals for using theory definitions
;;                         and assumptions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------
;;           conditionals for definition expansion
;;--------------------------------------------------------------
(meth~defcond th-definition (args cmapp)
	      (declare (edited  "14-NOV-1999")
		       (authors Jzimmer)
		       (input   "A symbol SYM and a meta-variable DEF for its definition.")
		       (effect  )
		       (value   "<T,mmapp>, when either DEF is bound and the associated term is"
				"           keim~equal to the definition of SYM in the problem theory, "
				"           or there is a definition of SYM in the problem theory and"
				"           mmapp is extended on the binding of SYM-DEF to this definition."
				"<NIL, mmapp> otherwise."))
              (let ((prob-theory (prob~theory omega*current-proof-plan))
		    (defined-obj (first args)))
		(if (or (and (term~schema-p defined-obj)
			     (term~constant-p (data~schema-range defined-obj))
			     (env~lookup-object (keim~name (data~schema-range defined-obj)) (th~env prob-theory)))
			 (and (term~constant-p defined-obj)
			      (env~lookup-object (keim~name defined-obj) (th~env prob-theory)))
			 (and (term~special-p defined-obj)))
		    ;;(or (eq prob-theory (th~find-theory 'base))
		    ;;	(not (term~constant-p defined-obj))
		    ;;	(env~lookup-object (keim~name defined-obj) (th~env (th~find-theory 'base)))
		    ;;	)
		    (let ((th-assumption (cond
					  ((term~special-p defined-obj)
					   (repr~find-definition defined-obj prob-theory))
					  ((term~constant-p defined-obj)
					   (th~find-assumption (keim~name defined-obj) prob-theory))
					  (T (th~find-assumption (keim~name (data~schema-range defined-obj)) prob-theory)))))
		      (if (th~definition-p th-assumption)
			  (let* ((definiens (th~ass-formula th-assumption))
				 (meta-def (second args))
				 (sym-def (or (subst~get-component meta-def (meth~mapp-subst cmapp))
					      (and (meth~mapp-extension cmapp)
						   (mapp~get-component meta-def (meth~mapp-extension cmapp))))))
			    (if sym-def
				(if (term~alpha-equal sym-def definiens)
				    cmapp
				  (meth~mapp-new-constraint cmapp nil))
			      (meth~mapp-new-extension cmapp
						       (if (meth~mapp-extension cmapp)
							   (mapp~create (cons meta-def (mapp~domain (meth~mapp-extension cmapp)))
									(cons (data~copy
									       definiens
									       :downto '(type+variable term+constant))
									      (mapp~codomain (meth~mapp-extension cmapp))))
							 (mapp~create (list meta-def)
								      (list (data~copy
									     definiens
									     :downto '(type+variable term+constant))))))))
			(meth~mapp-new-constraint cmapp nil)))
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'th-restricted-definition)

(meth~defcond th-restricted-definition (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   "A restriction a symbol SYM and a meta-variable SYM-DEF for the definition of SYM.")
		       (effect  )
		       (value  "<T,mmapp>, when either SYM-DEF is bound and the associated term is"
			       "           keim~equal to the definition of SYM in the problem theory, "
			       "           or there is a definition of SYM in the problem theory and"
			       "           mmapp is extended on the binding of SYM-DEF to this definition."
			       "           If the definin"
			       "<NIL, mmapp> otherwise"))
  
           ;;;;  here is still some dreaded error, probably due to the data~copy method....
	      (let* ((theory (if (th~p (first args)) (first args)
			       (th~find-theory (first args))))
		     (arg2 (second args))
		     (definiens (cond ((or (stringp arg2)
					   (symbolp arg2)) arg2)
				      ((term~constant-p arg2) (keim~name arg2))))
		     (definiendum (third args)))
		(if (and theory definiens)
		    (let ((definition (th~find-assumption definiens (prob~theory omega*current-proof-plan))))
		      (if (th~definition-p definition)
			  (let ((def-th (th~ass-theory definition))
				(all-th (cons theory (th~imports-recursed theory))))
			    (cond ((find def-th all-th)
				   (meth~mapp-new-constraint cmapp nil))
				  ((ssterm~var-p definiendum)
				   (meth~mapp-new-extension
				    cmapp
				    (if (meth~mapp-extension cmapp)
					(mapp~create
					 (cons definiendum (mapp~domain (meth~mapp-extension cmapp)))
					 (cons (data~copy (th~ass-formula definition)
							  :downto '(type+variable term+constant))
					       (mapp~codomain (meth~mapp-extension cmapp))))
				      (mapp~create (list definiendum)
						   (list (data~copy (th~ass-formula definition)
								    :downto '(type+variable term+constant)))))))
				  (t (if (term~alpha-equal (th~ass-formula definition) definiendum)
					 cmapp
				       (meth~mapp-new-constraint cmapp NIL)))))
			(meth~mapp-new-constraint cmapp nil)))
		  (meth~mapp-new-constraint cmapp nil))))

(meth~defcond getsymbolterm (args tmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "A string and a meta-variable OBJ for the KEIM term. ")
		       (effect  )
		       (value   "<T, mapp'>, if a KEIM symbol with the name SYM exists (in this case,"
				"            OBJ is bound to the KEIM object."
				"<NIL, mapp>, otherwise."))
	      (let* ((name (read-from-string (first args)))
		     (meta (second args))
		     (env (pds~environment omega*current-proof-plan))
		     )
		(if (symbolp name)
		    (let* ((term (post~read-object name env :existing-term))
			   )
		      (if term
			  
			  (meth~mapp-extend-subst tmapp
						  meta
						  term)
			(meth~tm-new-value tmapp nil)))
		  (meth~tm-new-value tmapp nil))))

(meth~new-relational-function 'getsymbolterm)

(meth~deffun th-ass (string)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   "The name ASS of an assumption.")
		      (effect  )
		      (value   "The assumption with name ASS, if it exists in the "
			       "theory of the current problem."))
	     (let ((ass (th~find-assumption string (prob~theory omega*current-proof-plan))))
	       (if ass
		   (pds~add-thy-assertion ass omega*current-proof-plan)
		 (error "Something went wrong while trying to find assumption ~A." string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         VI.  Conditionals for using computer algebra systems 
;;                        and constraint solvers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----------------------------------------------------
;;;       CAS stuff
;;----------------------------------------------------
(meth~deffun compute-with-cas (term)
	     (declare (edited  "11-AUG-1999")
		      (authors Sorge)
		      (input   )
		      (effect  )
		      (value   ))
	     (let ((ca*global-environment (pds~environment omega*current-proof-plan)))
	       (ca~translation term (ca~find-system :mycas))))

(meth~deffun compute-pol-minimum (term)
	     (declare (edited  "11-AUG-1999")
		      (authors Sorge)
		      (input   )
		      (effect  )
		      (value   ))
	     (let ((ca*global-environment (pds~environment omega*current-proof-plan)))
	       (ca~rebuild-object
		(car (ca~minimum (ca~build-object term :realpoly :mycas)))
		:real :mycas)))

(meth~deffun compute-pol-let (term numb)
	     (declare (edited  "11-AUG-1999")
		      (authors Sorge)
		      (input   )
		      (effect  )
		      (value   ))
	     (let ((ca*global-environment (pds~environment omega*current-proof-plan)))
	       (ca~rebuild-object
		(ca~polynomial-let
		 (ca~build-object term :realpoly :mycas)
		 (ca~build-object numb :real :mycas))
		:real :mycas)))

(meth~defcond degree-polynomial (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Sorge Jzimmer)
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; Args:  Polynomial, Degree
 	      ;;; Returns: <T,mmapp>, if Polynomial has the given Degree. Degree can be unbound.
	      ;;;          <NIL, mmapp> otherwise.
	      (let* ((subst (meth~mapp-subst cmapp))
		     (polynomial (first args))
		     (degree (second args))
		     (pds-poly (if (ssterm~var-p polynomial)
				   (meth~mapp-get-component polynomial cmapp :subst)
				 polynomial))
		     (pds-deg (cond ((numberp degree) degree)
				    ((term~number-p degree) (keim~name degree))
				    (t (meth~mapp-get-component degree cmapp :both))))
		     (ca*global-environment (pds~environment omega*current-proof-plan)))
		(if (and pds-poly (cacom~polynomial-p pds-poly))
		    (let ((real-degree (ca~polynomial-degree (ca~build-object pds-poly :realpoly :mycas))))
		      (if pds-deg
			  (meth~mapp-new-constraint cmapp (= real-degree pds-deg))
			(meth~mapp-extend-mapp cmapp degree real-degree)))
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'degree-polynomial)

(meth~defcond mgreater-equal (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Sorge Jzimmer)
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; Args:  Number-1, Number-2 
 	      ;;; Returns: <T,mmapp>, if Number-1 >= Number-2
	      ;;;          <NIL, mmapp> otherwise.
	      (let ((arg1 (if (term~number-p (car args))
			      (keim~name (car args))
			    (car args)))
		    (arg2 (if (term~number-p (cadr args))
			      (keim~name (cadr args))
			    (cadr args))))
		(meth~mapp-new-constraint cmapp (>= arg1 arg2))))

(meth~defcond mgreater (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Sorge Jzimmer)
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; Args:  Number-1, Number-2 
 	      ;;; Returns: <T,mmapp>, if Number-1 > Number-2
	      ;;;          <NIL, mmapp> otherwise.
	      (let ((arg1 (if (term~number-p (car args))
			      (keim~name (car args))
			    (car args)))
		    (arg2 (if (term~number-p (cadr args))
			      (keim~name (cadr args))
			    (cadr args))))
		(meth~mapp-new-constraint cmapp (> arg1 arg2))))


(meth~defcond getcassubst (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Sorge)
		       (input   "Two terms (A and B) and a meta-variable SUBST for a substitution.")
		       (effect  )
		       (value   "<T,mmapp>, when a subterm of B matches a subterm of A under the substitution Subst."
				"<NIL, mmapp>, otherwise."))
	      (let* ((a (first args))
		     (b (second args))
		     (subst (third args))
		     (pds-sub (mapp~get-component subst (meth~mapp-mapp cmapp)))
		     (real-subst (casex~find-substitution a b)))
		(if real-subst
		    (if pds-sub
			(meth~mapp-new-constraint cmapp (keim~equal subst real-subst))
		      (progn
			(meth~mapp-extend-mapp cmapp subst real-subst)
			(meth~mapp-new-constraint cmapp T)))
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'getcassubst)

(meth~defcond casextract (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Sorge)
		       (input   "Two terms (A and B), and unboung meta-variables for the"
				"terms K and L and the substitution SUBST.")
		       (effect  "This conditional uses Maple(tm).")
		       (value   "<T,mapp'>, when B can be devided by A under the substitution SUBST,"
				"i.e. B = K * A + L (in this case, K, L are bound)."
				"<NIL, mapp>, otherwise."))
	      (let ((a (first args))
		    (b (second args))
		    (k (third args))
		    (l (fourth args))
		    (subst (fifth args)))
		(multiple-value-bind (rk rl)
		    (casex~extract a b subst)
		  (if (and rk rl)
		      (progn
			(meth~mapp-extend-mapp cmapp k rk)
			(meth~mapp-extend-mapp cmapp l rl)
			(meth~mapp-new-constraint cmapp T)
			)
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'casextract)

(meth~defcond cas-simplify (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "A term B and a meta-variable unbound C for the resulting term.")
		       (effect  )
		       (value   "<T,mapp'>, B is simplified with a Maple(tm) and the result is"
				"           not alpha-equal to B (in this case,"
				"           C is bound to the resulting term."
				"<NIL,mapp>, otherwise."))
	      (let* ((dummy (casex~initialize-translation))
		     (env (pds~environment omega*current-proof-plan))
		     (b (first args))
		     (c (second args))
		     (b-maple (casex~sub-maple-terms (casex~pre-maple b)))
		     (back (casex~compute-with-maple "simplify" b-maple))
		     (result (if back
				 (post~read-object back env :existing-term)
			       nil))		     
		     )
		(if result
		    (if (term~alpha-equal b result)
			;MP: Why (data~alpha-equal b result nil nil nil) ?
			(meth~mapp-new-constraint cmapp nil)
		      (progn (meth~mapp-extend-mapp cmapp c result)
			     (meth~mapp-new-constraint cmapp t)))
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'cas-simplify)

  
(meth~defcond factorize (args cmapp)
	      (declare (edited  "09-FEB-2001")
		       (authors Jzimmer)
		       (input   "A polynomial.")
		       (effect  )
		       (value   "<T,cmapp'>, if the polynomial could be factorized."
                                "<NIL,cmapp>, otherwise."))
;               (setf opr*halt-service-listener '(:plan :nsp))
;               (oc=read-problem "/home/mrg/jzimmer/xmp/k.post")
;               (load "/home/mrg/jzimmer/prog/kqml/kqml-comm.lisp")
;               (load "/home/mrg/jzimmer/prog/kqml/om-oz.lisp")
              (let* ((env (pds~environment omega*current-proof-plan))
                     (poly (first args))
                     (ca*global-environment env))
                (if poly          ;;(and poly (cacom~polynomial-p poly))
                    (let* ((factorization-var (second args))
                           (openmath (omcont~string poly))
                           ;; send the KQML request for the factorize service
                           (result (kqml~send-and-wait "ask-one" "factorize" "OpenMath"
                                                       openmath))
                           ;; read the result (POST)
                           (result-symbol (read-from-string result))
                           (factors (if (and (listp result-symbol)
                                             (listp (car result-symbol)))
                                        (mapcar #'(lambda (res) 
                                                    (post~read-object res env
                                                                      :existing-term))
                                                (car result-symbol))
                                      nil))
                           (fact1 (first factors))
                           (fact2 (second factors))
                           (product (term~appl-create (env~lookup-object 'times env)
                                                      (list fact1 fact2)))
                           )
                      (meth~mapp-extend-mapp cmapp factorization-var product)
                      (meth~mapp-new-constraint cmapp t))
                  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'factorize)


(meth~defcond rdl-simplify (args cmapp)
	      (declare (edited  "09-FEB-2001")
		       (authors Jzimmer)
		       (input   "An open proof line L1: H |- F .")
		       (effect  )
		       (value   "<T,mapp>, if the clause form of H |- F can be simplified"
				"to true by the RDL system."
				"<NIL,mapp>, otherwise."))
              
              (let* ((env (pds~environment omega*current-proof-plan))
                     (formula (node~formula (first args)))
                     (result-var (second args))
                     (content1 (omcont~string formula))
                     (header (format nil "<OMA><OMS cd=\"reasys\" name=\"simplify-formula\">"))
                     (result-var (format nil "<OMATTR><OMATP>
<OMS cd=\"reasys\" name=\"metatype\"><OMS cd=\"reasys\" name=\"metavar\"></OMATP><OMV name=\"R\"/></OMATTR>"))
                     (tail   (format nil "</OMA>"))
                     (content (format nil "<OMOBJ>~A~A~A~A</OMOBJ>" header content1 result-var tail))
                     (adsf (format t "content: ~A" content))
                     (result-string (kqml~send  "RDLAgent" "ask-one" content))
                     (formula-symbol
                      (read-from-string (format nil "~A" (read-from-string result-string))))
                     ;;(CoSIE=instantiate-hyps (read-from-string formula-string) env))))
                     (result-formula (post~read-object formula-symbol
                                                       env :existing-term))
                     
                     )
		(if formula
		    (if (term~alpha-equal result-formula formula)
			(meth~mapp-new-constraint cmapp nil)
		      (progn (meth~mapp-extend-mapp cmapp result-var result-formula)
			     (meth~mapp-new-constraint cmapp t)))
		  (meth~mapp-new-constraint cmapp nil)))
              )

(meth~new-relational-function 'rdl-simplify)

(meth~defcond lclam-plan-problem (args cmapp)
	      (declare (edited  "09-FEB-2001")
		       (authors Jzimmer)
		       (input   "An open proof line L1: H |- F and a timelimit in seconds.")
		       (effect  )
		       (value   "<T,mapp>, if LClam could find a proof plan for the problem within the timelimit."
				"<NIL,mapp>, otherwise."))
              
              (let* ((env (pds~environment omega*current-proof-plan))
                     (goal (first args))
                     (limit (second args))
                     (adsf (format t "~S " (type-of (read-from-string (format nil "~A" limit)))))
                     (result (lclam~plan-problem goal limit))
                     )
                (format t "state: ~A" (first result))
		(if result 
                    (meth~mapp-new-constraint cmapp t)
		  (meth~mapp-new-constraint cmapp nil)
                  )))

(meth~new-relational-function 'lclam-plan-problem)

(meth~defcond cas-limit (args cmapp)
	      (declare (edited  "27-AUG-1999")
		       (authors "MP, VS, MJ")
		       (input   "Fct for function of limit, var for the variable of the function, limit for the limit that the var goes to and L for the resulting limit.")
		       (effect  )
		       (value   "<t,mapp'>, limit(fct,var,limit)=L is evaluated by Maple in order to find L."
				"           The result is"
				"           not alpha-equal to B (in this case,"
				"           C is bound to the resulting term."
				"<NIL,mapp>, otherwise."))
	      (let* ((dummy (casex~initialize-translation))
		     (env (pds~environment omega*current-proof-plan))
		     (fct (first args))
		     (var (second args))
		     (limit (third args))
		     (L (fourth args))
		     (maple-fct (casex~sub-maple-terms (casex~pre-maple fct)))
		     (back (casex~compute-with-maple "limit"
						     maple-fct
						     (string-capitalize (format nil "(= ~A ~A)" var limit))))
		     (qowrhi (format t "~% hier"))
		     (result )
		     )
		(env~enter (keim~name var) var env)
		(setf result (post~read-object back env :existing-term))
	        (env~remove (keim~name var) env)
		(if result
		    (if (term~alpha-equal L result)
			(meth~mapp-new-constraint cmapp t)
		      (progn (meth~mapp-extend-mapp cmapp L result)
			     (meth~mapp-new-constraint cmapp t)))
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'cas-limit)

(meth~defcond simplify-inequality (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors  Jzimmer)
		       (input   "A term TERM, a list of Eigenvariables EVARS,"
				"a meta-variable BACK for the resulting term, and"
				"a meta-variable SUBGOALS for the new open subgoals.")
		       (effect  "Tries to simplify TERM wrt. the following heuristic:"
				" move absolute values (absval) and Eigenvariables in EVARS"
				" to the left hand side.")
		       (value   "<T,mapp>, If TERM is an (in)equality or a negation of an (in)equality,"
				"          TERM could be simplified and the resulting (in)equality is not"
				"          alpha-equal to TERM (BACK is bound to the simplified formula,"
				"          SUBGOALS is bound to a list of new open proof-lines)."
				"<NIL,mapp>, otherwise."))
	      (let* ((formula (first args))
		     (Eigenvars (when (consp (second args))
				  (remove-duplicates (second args))))
		     (meta-result (third args))
		     (meta-subgoals (fourth args)))
		(multiple-value-bind (new-formula subgoals)
		    (limtac=simplify-inequality formula Eigenvars nil)
		  (if new-formula
		      (if (term~alpha-equal formula new-formula) 
			  ;MP: why (data~alpha-equal formula new-formula nil nil nil) ?
			  (meth~mapp-new-constraint cmapp nil)
			(progn (meth~mapp-extend-mapp cmapp meta-result new-formula)
			       (meth~mapp-extend-mapp cmapp meta-subgoals subgoals)
			       (meth~mapp-new-constraint cmapp t)))
		    (meth~mapp-new-constraint cmapp nil)))))

(meth~new-relational-function 'simplify-inequality)

;;-------------------------------------------------------
;;;      Constraint-solver stuff
;;-------------------------------------------------------
(meth~defcond initialize-cs (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "The name of the constraint-solver and a(n unbound) Meta-var representing"
				"the constraint-state")
		       (effect  )
		       (value   "<T, mmapp>, if the constraint-solver can be initialized correctly"
				"<NIL, mmapp> otherwise."))
	      (let* ((name (intern (first args)))
		     (meta (second args))
		     (new-var (cosint~call-cs name 'initialize))
		     )
		(if new-var
		    (meth~mapp-extend-mapp cmapp meta new-var)
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'initialize-cs)

(meth~defcond test-cs (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "The name of the constraint-solver and proof-line.")
		       (effect  )
		       (value   "<T,mapp>, if the formala is a valid constraint for the given constraint solver"
				"          and it is consistent with the constraint store."
				"<NIL, mapp>, otherwise."))
	      (let* ((name (intern (first args)))
		     (formula (second args))
		     (result (cosint~call-cs name 'testConstraint formula))
		     )
		(meth~mapp-new-constraint cmapp result)))

(meth~deffun tell-cs (name constraint)
	     (declare (edited  "11-AUG-1999")
		      (authors Jzimmer)
		      (input   "The name of the constraint-solver and a proof-line.")
		      (effect  "Destructively changes the constraint store.")
		      (value  "TRUE."
			      "Note: This function should only be used after a successful test with the conditional"
			      "      test-cs."))
	     (let* ((intern-name (intern name))
		    (result (cosint~call-cs intern-name 'tellConstraint constraint))
		    )
	       (if result
		   (logic~truth-constant)
		 (omega~error "The constraint could not be propagated. You should use 'test-cs' first."))))


(meth~defcond test-ass-cs (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "The name of the constraint-solver and proof-line.")
		       (effect  )
		       (value   "<T,mapp>, if the formala is a valid constraint for the given constraint solver"
				"          and it is consistent with the constraint store."
				"<NIL, mapp>, otherwise."))
	      (let* ((name (intern (first args)))
		     (formula (second args))
		     (result (cosint~call-cs name 'testAssumption formula))
		     )
		(meth~mapp-new-constraint cmapp result)))

(meth~deffun tell-ass-cs (name constraint)
	     (declare (edited  "11-AUG-1999")
		      (authors Jzimmer)
		      (input   "The name of the constraint-solver and a proof-line.")
		      (effect  "Destructively changes the constraint store.")
		      (value  "TRUE."
			      "Note: This function should only be used after a successful test with the conditional"
			      "      test-cs."))
	     (let* ((intern-name (intern name))
		    (result (cosint~call-cs intern-name 'tellAssumption constraint))
		    )
	       (if result
		   (logic~truth-constant)
		 (omega~error "The constraint could not be propagated. You should use 'test-cs' first."))))



(meth~defcond ask-cs (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "The name of the constraint-solver and the constraint.")
		       (effect  )
		       (value  "<T,mmapp>, if the constraint is entailed by the constraints in the constraint-store."
			       "<NIL, mmapp>, otherwise."))
	      (let* ((name (intern (first args)))
		     (constraint (second args))
		     (result (cosint~call-cs name 'askConstraint constraint))
		     )
		(meth~mapp-new-constraint cmapp result)))

(meth~defcond Eigenvariable (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "The name of the constraint-solver, a constant C and a meta-variable M_V.")
		       (effect  )
		       (value   "<T,mapp>, if the meta-constraint 'not-occurs(C M_V)' coud be propagated."
				"<NIL,mapp>, otherwise."))
	      (let* ((name (intern (first args)))
		     (const (second args))
		     (meta-var (third args))
		     (result (cosint~call-cs name 'tellMetaConstraint "not-occurs" const meta-var))
		     )
		(meth~mapp-new-constraint cmapp result)))

(meth~defcond no-Eigenvars-in (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "The name of the constraint-solver and a meta-variable M.")
		       (effect  )
		       (value   "<T, mapp>, if the meta-constraint 'not-occurs(any Eigenvariable, M)' coud be propagated."
				"<NIL, mapp>, otherwise."))
	      (let* ((name (intern (first args)))
		     (meta-var (second args))
		     (result (cosint~call-cs name 'tellMetaConstraint "no-Eigenvars" meta-var))
		     )
		(meth~mapp-new-constraint cmapp result)))

(meth~deffun cs-metavar (name)
	     (declare (edited  "11-AUG-1999")
		      (authors Jzimmer)
		      (input   "The name of the constraint solver.")
		      (effect  )
		      (value   "The metavar of the constraint solver that is a placeholder for the"
			       "answer constraint."))
	     (let* ((right-name (intern name))
		    (result (cosint~call-cs right-name 'getMetaVar))
		    )
	       (if result
		   result
		 'failure)))

(meth~deffun cs-eigenvars (name)
	     (declare (edited  "11-AUG-1999")
		      (authors Jzimmer)
		      (input   " The name of the constraint-solver.")
		      (effect  )
		      (value   "The Eigenvariables collected by the constraint solver."))
	     (let* ((right-name (intern name))
		    (result (cosint~call-cs right-name 'getEigenVars))
		    )
	       result))

(meth~deffun subst-to-conjunct (subst)
	     (declare (edited  "11-AUG-1999")
		      (authors Jzimmer)
		      (input   "A substitution of the form {V1<-T1,...,Vn<-Tn}.")
		      (effect  )
		      (value   "The conjunction V1=T1 & ... & Vn=Tn, if n >=1,"
			       "TRUE, otherwise."))
	     (multiple-value-bind (res conj)
		 (meth=subst-to-=conjunct subst)
	       conj))

(defun meth=subst-to-=conjunct (subst)
  (declare (edited  "05-DEC-1997")
	   (authors Jzimmer)
	   (input   "A substitution {V1 --> T1, ... ,Vn --> Tn}.")
	   (effect  )
	   (value   "A term, that is the conjunct of equalities of Variable-Term-pairs."
		    "(and (and ... (and (= V1 T1)) (= V2 T2))... (= Vn Tn)) "))
  
  (let ((conj (meth=make-equalities (subst~domain subst)
				    (subst~codomain subst)
				    (pds~environment omega*current-proof-plan))))
    (values t conj)))

(defun meth=make-equalities (domain codomain env)
  (cond ((null domain)
	 (env~lookup-object 'true env))
	((listp domain)
	 (if (null (rest domain))
	     (term~appl-create (env~lookup-object '= env)
			       (list (first domain)
				     (first codomain)))
	   (term~appl-create (env~lookup-object 'and env)
			     (list (term~appl-create (env~lookup-object '= env)
						     (list (first domain)
							   (first codomain)))
				   (meth=make-equalities (rest domain)
							 (rest codomain)
							 env)))))
	(T
	 (env~lookup-object 'true env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;         VII.  Remaining conditionals, hard to categorize
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------
;;               'mbind' and 'meval'
;;--------------------------------------------------------------
(meth~defcond mbind (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad)
		       (input   "A meta-variable MVAR and a KEIM object OBJ.")
		       (effect  )
		       (value   "<T,mmapp>, if MVAR is bound to an object that is keim~equal to OBJ."
				"<T,mmapp [MVAR <- OBJ]>, if OBJ is not a ssterm~var-p"
				"<NIL, mmapp>, otherwise."))
	      (let ((mvar (first args))
		    (pds-obj (second args)))
		(if (ssterm~var-p pds-obj)
		    (progn
		      (omega~error ";;; ~A in ~A is not yet bound!" pds-obj (cons 'mbind args))
		      (meth~mapp-new-constraint cmapp nil))
		  (if (ssterm~var-p mvar)
		      (meth~mapp-new-extension cmapp
					       (if (meth~mapp-extension cmapp)
						   (mapp~create
						    (cons mvar (mapp~domain (meth~mapp-extension cmapp)))
						    (cons pds-obj (mapp~codomain (meth~mapp-extension cmapp))))
						 (mapp~create (list mvar) (list pds-obj))))
		    (if (term~alpha-equal mvar pds-obj)
			cmapp
		      (meth~mapp-new-constraint cmapp nil))))
		))



(meth~defcond meval (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Jzimmer)
		       (input   "An unbound meta-variable for the result.")
		       (effect  )
		       (value   "The result of the evaluation of the procedural content"
				"of a method."))
	      (let* ((var (first args))
		     (method meth*current-method))
		(multiple-value-bind (applicable-p result)
		    (meth=try-tactic (meth~mapp-mapp cmapp)
				     (meth~mapp-subst cmapp)
				     meth*current-method
				     0)
		  (setf meth*current-method method)
		  (if applicable-p
		      (let* ((new-cm (meth~mapp-extend-subst cmapp var result))
			     )
			(if (meth~mapp-constraint new-cm)
			    new-cm
			  (meth~mapp-new-constraint cmapp nil)))
		    (meth~mapp-new-constraint cmapp nil)))))


(defun meth=try-tactic (mapp subst method &optional (number 0))
  (declare (edited  "26-MAY-1998")
	   (authors Jzimmer)
           (input "A SUBSTITUTION, a METHOD." )
           (effect "Calls the procedural tactic of METHOD using the values for the"
                   "meta-variables in SUBSTITUTION." )
           (value "T, if the tactic can be applied successfully, otherwise nil."))

  (let ((proc-cont (nth number (meth~procedural-content method))))
    (if (eq proc-cont 'schema-interpreter)
	(values nil nil)
      (let* ((symbol-list (mapcar #'keim~name
				  (append (subst~domain mapp)
					  (subst~domain subst))))
	     (value-list (append (subst~codomain mapp)
				 (subst~codomain subst)))
	     (sym-val-list (mapcar #'cons symbol-list value-list))
	     (env (meth~environment method))
	     (new-sym-val-list
	      (mapcar #'(lambda (pair)
			  (let* ((sym (first pair))
				 (term (cdr pair))
				 (new-term
				  (if term
				      term
				    (env~lookup-object sym env))))
			    (cons sym new-term)))
		      sym-val-list))
	     (value-list (mapcar #'cdr new-sym-val-list)))
	(progv symbol-list value-list
	  (eval proc-cont))))))

(meth~deffun inference (string)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad)
		      (input   )
		      (effect  )
		      (value   ))
	     (infer~find-method string))


;;-----------------------------------------------------------
;;     More complex conditionals for the Bledsoe Heuristic
;;                   and related stuff.
;;-------------------------------------------------
;;-------------------------------------------------
;;;; LC: This condition should be reimplemented after implementing critics:

(meth~new-relational-function 'bindable)

(meth~defcond bindable (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; ARGS: meta-var ssterm-var
	      ;; Return the alternative constraint pool for binding META-VAR in the current constraint pool
	      ;; binding SSTERM-VAR to the associated binding of META-VAR, when some alternative bindings of
	      ;; META-VAR are given in the current constraint pool. O/w, returns NIL.
	      (let ((metavar (first args))
		    (cstr-state (pds~constraint-pool omega*current-proof-plan)))
		(multiple-value-bind (binding-paths otherwise)
		    (cstr~variable-paths cstr-state metavar)
		     ;;; returns cstr-state-s:
		  (if binding-paths
		      (let ((last-path (first (last binding-paths))))
			(setf (pds~cstrpool-otherwise last-path) otherwise)
			(mapcar #'(lambda (cstate)
				    (let ((mvar-bind (subst~apply (pds~cstrpool-bindings cstate) metavar)))
				      (meth~mapp-insert-extension
				       (meth~mapping-create
					(meth~mapping-copy (meth~mapp-subst cmapp))
					(meth~mapping-copy (meth~mapp-mapp cmapp))
					(mapp~create (cons (second args)
							   (when (meth~mapp-extension cmapp)
							     (mapp~domain (meth~mapp-extension cmapp))))
						     (cons mvar-bind
							   (when (meth~mapp-extension cmapp)
							     (mapp~codomain (meth~mapp-extension cmapp)))))
					cstate))))
				binding-paths))
		    (meth~mapp-new-constraint cmapp NIL)))
		))


(meth~new-relational-function 'bound-to)

(meth~defcond bound-to (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad)
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; ARGS: meta-var, method variable to be bound to the instance of META-VAR given
	      ;; in the current constraint pool, when there is such a binding; o/w NIL
	      (let ((metavar (first args))
		    (ssvar (second args))
		    (cstr-state (pds~constraint-pool omega*current-proof-plan)))
		(cond ((meta~p metavar);; METAVAR is a meta-variable
		       (cond ((and cstr-state (pds~cstrpool-bindings cstr-state))
			      (let ((cstr-mvar-instance (subst~get-component metavar (pds~cstrpool-bindings cstr-state))))
				(cond ((and cstr-mvar-instance (ssterm~var-p ssvar))
				       (meth~mapp-new-extension cmapp
								(if (meth~mapp-extension cmapp)
								    (mapp~create (cons ssvar
										       (mapp~domain (meth~mapp-extension cmapp)))
										 (cons cstr-mvar-instance
										       (mapp~codomain (meth~mapp-extension cmapp))))
								  (mapp~create (list ssvar) (list cstr-mvar-instance)))))
				      ((and cstr-mvar-instance (term~p ssvar)
					    (term~alpha-equal cstr-mvar-instance ssvar))
				       cmapp)
				      (T
				       (meth~mapp-new-constraint cmapp NIL)))))
			     (T 
			      (meth~mapp-new-constraint cmapp NIL))))
		      (T;; METAVAR is not a meta-variable, then return NIL
		       (meth~mapp-new-constraint cmapp NIL)))
		))

(meth~defcond sub-conjunct (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   )
		       (effect  )
		       (value   ))
	      ;; ARGS: formula1, formula2
	      ;; T, iff FORMULA1 equals FORMULA2 or FORMULA1 is a conjunct in FORMULA2; o/w NIL
	      (let ((formula1 (first args))
		    (formula2 (second args)))
		(labels ((subconjunctp (form conj)
				       (or (term~alpha-equal form conj)
					   (and (logic~conjunction-p conj :theory (prob~theory omega*current-proof-plan))
						(let ((conjuncts (data~appl-arguments conj)))
						  (or (subconjunctp form (first conjuncts))
						      (subconjunctp form (second conjuncts))))))))
		  (if (subconjunctp formula1 formula2) cmapp
		    (meth~mapp-new-constraint cmapp NIL)))))

(meth~defcond boundp (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad )
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; ARGS: meta-var
	      ;; Returns T, when META-VAR is bound in the current constraint pool, o/w NIL
	      (let ((metavar (first args))
		    (cstr-state (pds~constraint-pool omega*current-proof-plan)))
		(cond ((and cstr-state (pds~cstrpool-bindings cstr-state)
			    (subst~get-component metavar (pds~cstrpool-bindings cstr-state)))
		       cmapp)
		      (T
		       (meth~mapp-new-constraint cmapp NIL)))
		))

				    
(meth~defcond bh-instance (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad )
		       (input   )
		       (effect  )
		       (value   ))
	      ;; ARGS: term1 (atom with set variable as head)
	      ;;       term2 literal
	      ;;       ssterm a method variable to be bound
	      ;; Check whether a part instantiation for the set variable in TERM1 can be found to
	      ;; directly close TERM2. This part instantiation is associated to SSTERM. Possibly
	      ;; additional constraints are imposed.
	      (let ((term1 (first args))
		    (term2 (second args))
		    (ssterm (third args)))
		(multiple-value-bind (instance add-cstrs)
		    (meth=bh-instance term1 term2)
		  (if instance
		      (meth~mapp-new-extension
		       (meth~mapp-new-constraint cmapp
						 (cstr~conjunction (cons (meth~mapp-constraint cmapp) add-cstrs)))
		       (if (meth~mapp-extension cmapp)
			   (mapp~create (cons ssterm (mapp~domain (meth~mapp-extension cmapp)))
					(cons instance (mapp~codomain (meth~mapp-extension cmapp))))
			 (mapp~create (list ssterm) (list instance))))
		    (meth~mapp-new-constraint cmapp NIL)))
		))

(meth~new-relational-function 'bh-instance)

(meth~deffun bledsoe-instance (metavar formulas)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad Jzimmer)
		      (input   )
		      (effect  )
		      (value   ))
	     (meth=bledsoe-instance metavar formulas))

(defun meth=bledsoe-instance (metavar formulas &optional term1 codomain terms)
  (declare (edited  "13-APR-1998")
	   (authors Lassaad)
	   (input   "A meta-variable, a list of formulas, and optionally a first"
		    "term, a list of variables, and a list of terms which are used"
		    "to construct term for binding METAVAR.")
	   (effect  "None.")
	   (value   "A term to bind METAVAR."))
  (if formulas
      (let* ((formula (first formulas))
	     (args (data~appl-arguments formula))
	     (matcher))
	(if (logic~implication-p formula :theory (prob~theory omega*current-proof-plan))
	    (let ((matching-res (uni~syntactic-matcher (first args) (second args))))
	      (if matching-res (setq matcher matching-res)
		(return-from meth=bledsoe-instance
		  (omega~error ";;;meth=bledsoe-instance: ~A and ~A cannot be matched!"
			       (first args) (second args)))))
	  (if (logic~disjunction-p formula :theory (prob~theory omega*current-proof-plan))
	      (multiple-value-bind (mterm oterm)
		  (let ((arg1 (first args)))
		    (if (logic~negation-p arg1 :theory (prob~theory omega*current-proof-plan))
			(let ((negated-form (first (data~appl-arguments arg1))))
			  (if (and (term~appl-p negated-form)
				   (eq metavar (data~appl-function negated-form)))
			      (values negated-form (second args))
			    (let ((arg2 (second args)))
			      (if (logic~negation-p arg2 :theory (prob~theory omega*current-proof-plan))
				  (let ((negated-form2 (first (data~appl-arguments arg2))))
				    (if (and (term~appl-p negated-form2)
					     (eq metavar (data~appl-function negated-form2)))
					(values negated-form2 arg1)
				      (return-from meth=bledsoe-instance
					(omega~error ";;;meth=bledsoe-instance: unaccepted formula ~A"
						     formula))))
				(return-from meth=bledsoe-instance
				  (omega~error ";;;meth=bledsoe-instance: unaccepted formula ~A"
					       formula))))))
		      (let ((arg2 (second args)))
			(if (logic~negation-p arg2 :theory (prob~theory omega*current-proof-plan))
			    (let ((negated-form2 (first (data~appl-arguments arg2))))
			      (if (and (term~appl-p negated-form2)
				       (eq metavar (data~appl-function negated-form2)))
				  (values negated-form2 arg1)
				(return-from meth=bledsoe-instance
				  (omega~error ";;;meth=bledsoe-instance: unaccepted formula ~A"
					       formula))))
			  (return-from meth=bledsoe-instance
			    (omega~error ";;;meth=bledsoe-instance: unaccepted formula ~A"
					 formula))))))
		(let ((matching-res (uni~syntactic-matcher mterm oterm)))
		  (if matching-res (setq matcher matching-res)
		    (return-from meth=bledsoe-instance
		      (omega~error ";;;meth=bledsoe-instance: ~A and ~A cannot be matched!"
				   mterm oterm)))))
	    (return-from meth=bledsoe-instance
	      (omega~error ";;;meth=bledsoe-instance: unaccepted formula ~A"
			   formula))))
	(let ((assoc-term (subst~get-component metavar matcher)))
	  (loop
	   (unless (data~free-variables assoc-term)
	     (return))
	   (setq assoc-term (subst~apply matcher assoc-term)))
	  (if term1
	      (if codomain
		  (meth=bledsoe-instance metavar (rest formulas) term1 codomain (cons assoc-term terms))
		(if (term~abstr-p assoc-term)
		    (meth=bledsoe-instance metavar (rest formulas) term1 (data~abstr-domain assoc-term)
					   (cons (data~abstr-range assoc-term) terms))
		  (meth=bledsoe-instance metavar (rest formulas) term1 codomain (cons assoc-term terms))))
	    (if (term~abstr-p assoc-term)
		(meth=bledsoe-instance metavar (rest formulas) (data~abstr-range assoc-term)
				       (data~abstr-domain assoc-term))
	      (meth=bledsoe-instance metavar (rest formulas) assoc-term)))))
    (if term1
	(let ((conj (when terms (first terms))))
	  (when (term~abstr-p conj)
	    (setq conj (data~replace-free-variables (data~abstr-range conj)
						    (data~abstr-domain conj) codomain)))
	  (dolist (aterm (rest terms))
	    (setq conj
		  (term~appl-create
		   (logic~conjunction-constant :name 'and
					       :theory (prob~theory omega*current-proof-plan))
		   (list (if (term~abstr-p aterm)
			     (data~replace-free-variables (data~abstr-range aterm)
							  (data~abstr-domain aterm) codomain)
			   aterm)
			 conj))))
	  (beta~normalize
	   (if conj
	       (if codomain
		   (term~abstr-create codomain (term~appl-create (logic~conjunction-constant
								  :name 'and
								  :theory (prob~theory omega*current-proof-plan))
								 (list term1 conj)))
		 (term~appl-create (logic~conjunction-constant) (list term1 conj)))
	     term1)))
      (omega~error ";;;meth=bledsoe-instance: empty formula list!")
      )))

;;;;LC: NEW conditions just for testing
;;;;
;; CONDs with satisfaction functions
;; for instance subterm-cond. Satisfaction function get
;; first cmapp, then the arguments and returns cmapp
;; 
					;(meth~defcond subterm (args cmapp)
					;              
					;             ;;; args: term1, term2
					;             ;;; Returns: <T, mmapp> when term1 is a subterm of term2
					;              ;;          Otherwise, <NIL, mmapp>
					;              (let ((result (find (first args)
					;                                  (data~all-substructs (second args))
					;                                  :test #'term~alpha-equal)))
					;                (if result cmapp
					;                  (meth~mapp-new-constraint cmapp nil))))

(meth~defcond BHbindable (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad Jzimmer)
		       (input   )
		       (effect  )
		       (value   ))
	      ;;; args: meta-variable, list of formulas
	      ;; adds binding and unify constraints, when MV can be so instantiated
	      ;; via Bledsoe Heuristic that the given formulas are satisfiable
	      (let ((new-constraint (meth=bh-bindable (first args) (second args))))
		(if new-constraint
		    (meth~mapp-new-constraint cmapp
					      (cstr~conjunction (list (meth~mapp-constraint cmapp) new-constraint)))
		  (meth~mapp-new-constraint cmapp NIL))
		))

(defun meth=bh-instance (flex-atom literal)
  (declare (edited  "13-APR-1999")
	   (authors Lassaad)
	   (input   "Two literals: an atom with a meta-variable as a head and a literal.")
	   (effect  "None.")
	   (value   "A binding for the set variable and a list of additional constraints."
		    "e.g., X(i), s(i) --> <lam x. s(x), NIL>"
		    "      X(f(a)), p(Y(f,a)) --> <lam x. p(x), Y(f,a) = f(a)>"))
  (let ((metavar (data~appl-function flex-atom))
	(unify-results (cstr~adapt-hou-result (uni~unify (list (list flex-atom literal))
							 :destructive nil
							 :cost-limit cstr*unification-depth))))
    (when unify-results
      (labels ((subst2bindings-butnotfor (domain codomain mvar)
					 ;; Constructs binding constraints for the variables in DOMAIN which
					 ;; are different from MVAR using the associated term in CODOMAIN
					 (when domain
					   (if (data~equal (first domain) mvar)
					       (subst2bindings-butnotfor (rest domain) (rest codomain) mvar)
					     (cons (cstr~binding-create (list (first domain) (first codomain)))
						   (subst2bindings-butnotfor (rest domain) (rest codomain) mvar)))))
	       )
	(let ((much-p-results (meth==much-projection-results metavar unify-results)))
	  (if (rest much-p-results)
	      (progn 
		(omega~error ";;;meth=bh-instance: must be extended to handle uncomparable unifiers ~A."
			     much-p-results)
		(return-from meth=bh-instance))
	    (let* ((result-hou (first much-p-results))
		   (metavar-binding-term (when (uni~substitution result-hou) 
					   (subst~get-component metavar (uni~substitution result-hou))))
		   (additional-cstrs (append
				      (mapcar #'(lambda (ff) (cstr~simple-create :unify ff
										 (cstr~free-variables ff)))
					      (uni~flex-flex result-hou))
				      (subst2bindings-butnotfor (subst~domain (uni~substitution result-hou))
								(subst~codomain (uni~substitution result-hou))
								metavar))))
	      (values metavar-binding-term additional-cstrs))))))
    ))


(meth~defcond linear (args cmapp)
	      (declare (edited  "11-AUG-1999")
		       (authors Lassaad)
		       (input   )
		       (effect  )
		       (value   ))
	      (multiple-value-bind (cstr other-cstrs)
		  (cstr~sat-linear (first args) T)
		(if cstr
		    (meth~mapp-new-constraint cmapp
					      (cstr~conjunction (cons (meth~mapp-constraint cmapp)
								      (cons cstr other-cstrs))))
		  (meth~mapp-new-constraint cmapp nil))))
  
(meth~deffun bh-conjunct (term1 term2)
	     (declare (edited  "11-AUG-1999")
		      (authors Lassaad)
		      (input   )
		      (effect  )
		      (value   ))
	     ;; s1: io, s2: io --> lam x:i s1(x) and s2(x)
	     ;; s1: io, lam x:i not p(x) --> lam x:i s1(x) and not p(x)
	     (cond ((and (data~abstr-p term1) (data~abstr-p term2))
		    ;; lam x. Phi(x), lam y. p(y) --> lam x. Phi(x) and p(x)
		    (term~abstr-create
		     (data~abstr-domain term1)
		     (term~appl-create (logic~conjunction-constant :name 'and
								   :theory (prob~theory omega*current-proof-plan))
				       (list (data~abstr-range term1)
					     (data~replace-free-variables (data~abstr-range term2)
									  (data~abstr-domain term2)
									  (data~abstr-domain term1))))))
		   ((data~abstr-p term1)
		    ;; lam x. Phi(x), p --> lam x. Phi(x) and p(x)
		    (term~abstr-create
		     (data~abstr-domain term1)
		     (term~appl-create (logic~conjunction-constant :name 'and
								   :theory (prob~theory omega*current-proof-plan))
				       (list (data~abstr-range term1)
					     (term~appl-create term2 (data~abstr-domain term1))))))
		   ((data~abstr-p term2)
		    ;; p, lam x. Phi(x) --> lam x. p(x) and Phi(x) 
		    (term~abstr-create
		     (data~abstr-domain term2)
		     (term~appl-create (logic~conjunction-constant :name 'and
								   :theory (prob~theory omega*current-proof-plan))
				       (list (term~appl-create term1 (data~abstr-domain term2))
					     (data~abstr-range term2)))))
		   ((type~alpha-equal (term~type term1) (term~type term2))
		    (let* ((dom-types (data~abstr-n-domain (term~type term1)))
			   (domain (mapcar #'(lambda (tp) (gb~helpvariable-create 'term+variable tp))
					   dom-types)))
		      (let ((res
			     (term~abstr-create domain
						(term~appl-create (logic~conjunction-constant
								   :name 'and
								   :theory (prob~theory omega*current-proof-plan))
								  (list (term~appl-create term1 domain)
									(term~appl-create term2 domain))))))
			res)))
		   ))

;;; NEW:
(defun meth=bh-bindable (metavar formulas &optional term1 domain terms cstrs)
  (declare (edited  "15-DEC-1998" "13-APR-1998")
	   (authors Lassaad Lassaad)
	   (input   "A meta-variable, a list of formulas, and optionally a first"
		    "term, a list of variables, a list of terms which are used"
		    "to construct term for binding METAVAR, and a list of additional"
		    "constraints. FORMULAS are all implications.")
	   (effect  "None.")
	   (value   "A constraint list containing a binding of METAVAR and possibly"
		    "additional unification constraints."
		    "e.g., (X, <X(i,j) --> Y(i(j)), X(i,j) --> i = j>, nil nil nil nil)"
		    "returns <X<- lam x,y. Z(x,y) and x=y, unify(Y(i(j)),Z(i,j))>"))
  (if formulas
      (let* ((formula (first formulas))
	     (args (data~appl-arguments formula))
	     result-hou)
	(unless (logic~implication-p formula :theory (prob~theory omega*current-proof-plan))
	  (omega~error ";;;meth=bh-bindable: ~A is not an implication." formula)
	  (return-from meth=bh-bindable))
	(let ((precedent (first args))
	      (antecedent (second args)))
	  ;;;; Some help functions:
	  (labels ((subst2bindings-butnotfor (domain codomain mvar)
					     ;;; Constructs binding constraints for the variables in DOMAIN which
					     ;; are different from MVAR using the associated term in CODOMAIN
					     (when domain
					       (if (data~equal (first domain) mvar)
						   (subst2bindings-butnotfor (rest domain) (rest codomain) mvar)
						 (cons (cstr~binding-create (list (first domain) (first codomain)))
						       (subst2bindings-butnotfor (rest domain) (rest codomain) mvar)))))
		   )
	    ;;; Determine first a unifier computed with the most projection rules
	    (cond ((and (logic~negation-p precedent :theory (prob~theory omega*current-proof-plan))
			(logic~negation-p antecedent :theory (prob~theory omega*current-proof-plan)))
		   ;;; ~MV(t1 .. tn) -> ~Phi is not allowed, because such a goal
		   ;; must be transformed into Phi -> MV(t1 .. tn)
		   (omega~error ";;;meth=bh-bindable: ~A having negated precedent and negated antecedent." formula)
		   (return-from meth=bh-bindable))
		  ((logic~negation-p precedent :theory (prob~theory omega*current-proof-plan))
		   (let* ((term1 (first (data~appl-arguments precedent)))
			  (unify-results (cstr~adapt-hou-result
					  (uni~unify (list (list term1
								 (term~appl-create (logic~negation-constant
										    :name 'not
										    :theory (prob~theory omega*current-proof-plan))
										   (rest args))))
						     :destructive nil
						     :cost-limit cstr*unification-depth))))
		     (unless unify-results
		       (omega~error ";;;meth=bh-bindable: ~A and ~A cannot be unified in ~A unification steps."
				    term1 (term~appl-create (logic~negation-constant
							     :name 'not
							     :theory (prob~theory omega*current-proof-plan))
							    (rest args))
				    cstr*unification-depth)
		       (return-from meth=bh-bindable))
		     (let ((much-p-results (meth==much-projection-results metavar unify-results)))
		       (if (rest much-p-results)
			   (progn 
			     (omega~error ";;;meth=bh-bindable: must be extended to handle uncomparable unifiers ~A."
					  much-p-results)
			     (return-from meth=bh-bindable))
			 (setq result-hou (first much-p-results))))))
		  (T
		   (let ((unify-results (cstr~adapt-hou-result
					 (uni~unify (list (list precedent antecedent))
						    :destructive nil
						    :cost-limit cstr*unification-depth))))
		     (unless unify-results
		       (omega~error ";;;meth=bh-bindable: ~A and ~A cannot be unified in ~A unification steps."
				    precedent antecedent cstr*unification-depth)
		       (return-from meth=bh-bindable))
		     (let ((much-p-results (meth==much-projection-results metavar unify-results)))
		       (if (rest much-p-results)
			   (progn 
			     (omega~error ";;;meth=bh-bindable: must be extended to handle uncomparable unifiers ~A."
					  much-p-results)
			     (return-from meth=bh-bindable))
			 (setq result-hou (first much-p-results)))))))
	    ;;; Compute the relevant information from the RESULT-HOU to use them into the recursive call:
	    (let ((metavar-binding-term (when (uni~substitution result-hou) 
					  (subst~get-component metavar (uni~substitution result-hou))))
		  (additional-cstrs (append
				     (mapcar #'(lambda (ff) (cstr~simple-create :unify ff
										(cstr~free-variables ff)))
					     (uni~flex-flex result-hou))
				     (subst2bindings-butnotfor (subst~domain (uni~substitution result-hou))
							       (subst~codomain (uni~substitution result-hou))
							       metavar))))
	      (if metavar-binding-term
		  (cond ((and term1 domain)
		         ;;; A previously considered formula delivered a domain for METAVAR
			 (meth=bh-bindable metavar (rest formulas) term1 domain
					   (cons metavar-binding-term terms) (append cstrs additional-cstrs)))
			((and term1 (term~abstr-p metavar-binding-term))
		         ;;; A previously considered formula didn't deliver a domain for METAVAR and the
			 ;; METAVAR-BINDING-TERM is an abstraction, take its domain.
			 (meth=bh-bindable metavar (rest formulas) term1 (data~abstr-domain metavar-binding-term)
					   (cons (data~abstr-range metavar-binding-term) terms) (append cstrs additional-cstrs)))
			(term1
		         ;;; A previously considered formula didn't deliver a domain for METAVAR and the
			 ;; METAVAR-BINDING-TERM is not an abstraction:
			 (meth=bh-bindable metavar (rest formulas) term1 domain
					   (cons metavar-binding-term terms) (append cstrs additional-cstrs)))
			((and (null term1) (term~abstr-p metavar-binding-term))
		         ;;; No previously considered formula, METAVAR-BINDING-TERM is an abstraction, take its domain:
			 (meth=bh-bindable metavar (rest formulas) (data~abstr-range metavar-binding-term)
					   (data~abstr-domain metavar-binding-term) NIL (append cstrs additional-cstrs)))
			((null term1)
		         ;;; No previously considered formula, METAVAR-BINDING-TERM is not an abstraction:
			 (meth=bh-bindable metavar (rest formulas) metavar-binding-term
					   NIL NIL (append cstrs additional-cstrs))))
		(if additional-cstrs
		    ;;; Only additional constraints
		    (meth=bh-bindable metavar (rest formulas) term1 domain terms (append cstrs additional-cstrs))
		  (progn
		    (omega~error ";;;meth=bh-bindable: illegal argument ~A" formula)
		    (return-from meth=bh-bindable))))))))
    ;;; Compute the binding for METAVAR and the constraints
    (cond ((and term1 terms domain)
	   ;;; A first conjunct (TERM1), other conjunctions (TERMS), and a DOMAIN
	   (let ((conj (first terms)))
	     (if (term~abstr-p conj)
		 (setq conj (data~replace-free-variables (data~abstr-range conj)
							 (data~abstr-domain conj) domain))
	       (setq conj (term~appl-create conj domain)))
	     (dolist (aterm (rest terms))
	       (setq conj
		     (term~appl-create
		      (logic~conjunction-constant :name 'and
						  :theory (prob~theory omega*current-proof-plan))
		      (list (if (term~abstr-p aterm)
				(data~replace-free-variables (data~abstr-range aterm)
							     (data~abstr-domain aterm) domain)
			      (term~appl-create aterm domain))
			    conj))))
	     (cstr~conjunction
	      (cons (cstr~binding-create
		     (list metavar
			   (term~abstr-create domain
					      (term~appl-create (logic~conjunction-constant
								 :name 'and
								 :theory (prob~theory omega*current-proof-plan))
								(list term1 conj)))))
		    cstrs))))
	  ((and term1 terms (null domain))
	   (omega~error ";;;meth=bh-bindable: binding has to be composed from ~A and ~A."
			term1 terms)
	   (return-from meth=bh-bindable))
	  ((and term1 (null terms) domain)
	   (cstr~conjunction
	    (cons (cstr~binding-create
		   (list metavar
			 (term~abstr-create domain term1)))
		  cstrs)))
	  ((and term1 (null terms) (null domain))
	   (cstr~conjunction
	    (cons (cstr~binding-create
		   (list metavar term1))
		  cstrs)))
	  ((and (null term1) cstrs)
	   (cstr~conjunction cstrs))
	  ((and (null term1) (null cstrs))
	   (omega~error ";;;meth=bh-bindable: empty formula list!")))
    ))

(defgeneric meth==less-eq-incomparable (term1 dom1 term2 dom2)
  ;;; Returns a pair:
  ;; - T, iff TERM1 is constructed with less imitations than TERM2,
  ;; e.g., x and f(a,b) and not x and f(a,y)
  ;; - T, iff TERM1 is constructed with same imitations than TERM2,
  ;; e.g., x and y, c and c, and not a and c
  ;; - T, iff TERM1 and TERM2 cannot be compared, e.g., f(a,x) and f(b,y)
  (:method ((mvar1 meta+variable) (dom1 cons) (mvar2 meta+variable) (dom2 cons))
	   ;;; Meta-variables are considered as constants
	   (if (data~equal mvar1 mvar2) (values NIL T) (values NIL NIL T)))
  (:method ((const1 term+constant) (dom1 cons) (const2 term+constant) (dom2 cons))
	   (if (data~equal const1 const2) (values NIL T) (values NIL NIL T)))
  (:method ((hvar1 keim::gb+helpvariable) (dom1 cons) (hvar2 keim::gb+helpvariable) (dom2 cons))
	   (if (find hvar1 dom1)
	       (if (find hvar2 dom2) (values NIL T) (values T))
	     (unless (find hvar2 dom2)
	       (if (data~equal hvar1 hvar2) (values NIL T) (values NIL NIL T)))))
  (:method ((hvar1 keim::gb+helpvariable) (dom1 cons) (term2 term+term) (dom2 cons))
	   (if (find hvar1 dom1) 
	       (if (intersection (data~free-variables term2) dom2) (values NIL NIL T) (values T))
	     (values NIL NIL T)))
  (:method ((term1 term+term) (dom1 cons) (hvar2 keim::gb+helpvariable) (dom2 cons))
	   (if (find hvar2 dom2)
	       (when (intersection (data~free-variables term1) dom1) (values NIL NIL T))
	     (values NIL NIL T)))
  (:method ((appl1 data+appl) (dom1 cons) (appl2 data+appl) (dom2 cons))
	   (if (eq (length (data~appl-arguments appl1)) (length (data~appl-arguments appl2)))
	       (meth==less-eq-incomparable (cons (data~appl-function appl1) (data~appl-arguments appl1)) dom1
					   (cons (data~appl-function appl2) (data~appl-arguments appl2)) dom2)
	     (values NIL NIL T)))
  (:method ((abstr1 data+abstr) (dom1 cons) (abstr2 data+abstr) (dom2 cons))
	   (if (eq (length (data~abstr-domain abstr1)) (length (data~abstr-domain abstr2)))
	       (meth==less-eq-incomparable (data~abstr-range abstr1) (append dom1 (data~abstr-domain abstr1))
					   (data~abstr-range abstr2) (append dom2 (data~abstr-domain abstr2)))
	     (values NIL NIL T)))
  (:method ((terms1 cons) (dom1 cons) (terms2 cons) (dom2 cons))
	   ;;; Return: <T,NIL,NIL>, when for Ti <T,NIL,NIL> and for each Tj in the rest of terms
	   ;; either <T,NIL,NIL> or <NIL,T,NIL>
	   ;; Return: <NIL,T,NIL>, when for each Ti <NIL,T,NIL>
	   ;; Return: <NIL,NIL,T>, when for one Ti <NIL,NIL,T> 
	   ;; O/w return: NIL
	   (multiple-value-bind (less1 eq1 incomp1)
	       (meth==less-eq-incomparable (first terms1) dom1 (first terms2) dom2)
	     (cond (incomp1 (values NIL NIL T))
		   (eq1 (if (rest terms1)
			    (multiple-value-bind (less-res eq-res incomp-res)
				(meth==less-eq-incomparable (rest terms1) dom1 (rest terms2) dom2)
			      (cond (incomp-res (values NIL NIL T))
				    (eq-res (values NIL T))
				    (less-res (values T))))
			  (values NIL T)))
		   (less1 (if (rest terms1)
			      (multiple-value-bind (less-res eq-res incomp-res)
				  (meth==less-eq-incomparable (rest terms1) dom1 (rest terms2) dom2)
				(cond (incomp-res (values NIL NIL T))
				      ((or eq-res less-res) (values T))))
			    (values T))))))
  (:method (obj1 (dom1 cons) obj2 (dom2 cons))
	   (omega~error "meth==less-eq-incomparable not defined for ~A ~A"
			(type-of obj1) (type-of obj2))
	   (error "meth==less-eq-incomparable"))
  )

(defun meth==lesseq&incomparable (term domain terms domains hous)
  ;;; Returns a tuple:
  ;; - An element of HOUS, which is constructed with less or equal imitations than that of TERM:
  ;; - The elements of HOUS, which are considered before finding a lesseq element and which are
  ;; incomparable with that of TERM.
  ;; - The associated ranges for the second value
  ;; - The associated domains for the second value
  (when terms
    (multiple-value-bind (less-p eq-p incomparable-p)
	(meth==less-eq-incomparable (first terms) (first domains) term domain)
      (cond ((or less-p eq-p)
	     (values (first hous)))
	    (incomparable-p
	     (multiple-value-bind (res-leq-hou res-incomp-hous res-incomp-ranges res-incomp-domains)
		 (meth==lesseq&incomparable term domain (rest terms) (rest domains) (rest hous))
	       (if res-leq-hou
		   (values res-leq-hou)
		 (values NIL (cons (first hous) res-incomp-hous)
			 (cons (first terms) res-incomp-ranges)
			 (cons (first domains) res-incomp-domains)))))
	    (T
	     (meth==lesseq&incomparable term domain (rest terms) (rest domains) (rest hous)))))
    ))

(defun meth==much-projection-hous (ranges domains binding-hous)
  ;;; Rejects the HOUS,for which there are other hou in HOUS built by applying
  ;; fewer or equal imitations.
  (if (rest ranges)
      (multiple-value-bind (leq-hou incomp-hous incomp-ranges incomp-domains)
	  (meth==lesseq&incomparable (first ranges) (first domains) (rest ranges) (rest domains) (rest binding-hous))
	(cond (leq-hou
	       (meth==much-projection-hous (rest ranges) (rest domains) (rest binding-hous)))
	      (incomp-hous
	       (cons (first binding-hous)
		     (meth==much-projection-hous incomp-ranges incomp-domains incomp-hous)))
	      ((and (null leq-hou) (null incomp-hous))
	       (list (first binding-hous)))))
    binding-hous
    ))

(defun meth==much-projection-results (metavar hous)
  ;;; Between the HOUs which contains bindings for METAVAR selects those which
  ;; are resulted by applying as much as possible projections. The HOUS which
  ;; dont contain bindings for METAVAR are returned.
  
  (let ((binding-hous
	 (remove-if-not #'(lambda (hou)
			    (and (uni~substitution hou)
				 (subst~get-component metavar (uni~substitution hou))))
			hous)))
    (if (rest binding-hous)
	(let* ((binding-terms
		(mapcar #'(lambda (hou)
			    (subst~get-component metavar (uni~substitution hou)))
			binding-hous))
	       (binding-domains (mapcar #'(lambda (bt)
					    (when (term~abstr-p bt) (data~abstr-domain bt)))
					binding-terms)))
	  (if (some #'null binding-domains)
		;;; Some bindings for METAVAR are not abstractions:
	      (append (remove-if #'null
				 (mapcar #'(lambda (D Hou) (unless D Hou))
					 binding-domains binding-hous))
		      (remove-if #'(lambda (hou) (find hou binding-hous))
				 hous))
	      ;;; Every binding for METAVAR is an abstraction:
	    (append (meth==much-projection-hous (mapcar #'data~abstr-range binding-terms)
						binding-domains binding-hous)
		    (remove-if #'(lambda (hou) (find hou binding-hous))
			       hous))))
      hous))
  )


(meth~new-relational-function 'assert)

(meth~defcond assert (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad)
	 (input   )
	 (effect  )
	 (value   ))
	      ;;; args: assumption, goal, var
;; returns T when ASSUMPTION can be used to assert GOAL and binds
;; var to the premises:
(let* ((assumption (first args))
       (goal (second args))
       (meth-var (third args))
       (goal-sign (not (logic~negation-p goal :theory (prob~theory omega*current-proof-plan))))
       (goal-func (if goal-sign
		      (data~appl-function goal)
		    (data~appl-function (first (data~appl-arguments goal))))))
  (multiple-value-bind (sbf prems other-concs all-vars ex-vars)
      (sbf~subformula assumption T
		      #'(lambda (sb) (and (data~appl-p sb)
					  (keim~equal (data~appl-function sb)
						      goal-func)))
		      goal-sign)
    (if sbf
	(let* ((mvars (mapcar #'(lambda (alv)
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
		  (multiple-value-bind (uni-cstr bindings)
		      (if splitp
			  (values
			   (cstr~disjunction (mapcar #'(lambda (C Bs)
							 (cstr~conjunction (if (cstr~constraint-p C)
									       (append Bs (list C))
									     Bs)))
						     cstrs bindingss)))
			(values cstrs bindingss))
		    (if uni-cstr
			(cond ((or (cstr~constraint-p uni-cstr)
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
								      prems)
							      (mapp~codomain (meth~mapp-extension cmapp))))
					 (mapp~create (list meth-var)
						      (list (mapcar #'(lambda (prem)
									(multiple-value-bind (new-prem)
									    (data~replace-free-variables
									     prem (append all-vars ex-vars) mvars&consts)
									  (beta~normalize new-prem)))
								    prems)))))
				(cstr~conjunction (cons (meth~mapp-constraint cmapp)
							(cons uni-cstr bindings)))))
			      (T
					     ;;; The unification of GOAL with NEW-SBF resulted only to bindings of meta-variables
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
								prems)
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
							      prems))))))))
		      (meth~mapp-new-constraint cmapp NIL)))))
	    ;; No all-vars and no ex-vars, unification constraints depend on meta-variables inside the
	    ;; assertion and/or inside the goal. These constraints must be returned.
	    (multiple-value-bind (cstrs bindingss splitp)
		(cstr~sat-unify goal sbf T)
	      (multiple-value-bind (uni-cstr bindings)
		  (if splitp
		      (values
		       (cstr~disjunction (mapcar #'(lambda (C Bs)
						     (cstr~conjunction (if (cstr~constraint-p C)
									   (append Bs (list C))
									 Bs)))
						 cstrs bindingss)))
		    (values cstrs bindingss))
		(if uni-cstr
		    (meth~mapp-new-constraint
		     (meth~mapp-new-extension
		      cmapp (if (meth~mapp-extension cmapp)
				(mapp~create (cons meth-var (mapp~domain (meth~mapp-extension cmapp)))
					     (cons prems (mapp~codomain (meth~mapp-extension cmapp))))
			      (mapp~create (list meth-var) (list prems))))
		     (cstr~conjunction (cons (meth~mapp-constraint cmapp)
					     (cons uni-cstr bindings))))
		  (meth~mapp-new-constraint cmapp NIL))))))
      (meth~mapp-new-constraint cmapp NIL)))
  ))

(defun meth=help-bind-mvars-in (bindings formula)
  ;; returns T, iff some meta-variables into FORMULA are
  ;; bound by BINDINGS
(let ((formula-mvars (remove-if-not #'meta~p (data~free-variables formula))))
  (when formula-mvars
    (some #'(lambda (fmv)
	      (some #'(lambda (bind)
			(data~equal fmv (cstr~bound-variable bind)))
		    bindings))
	  formula-mvars))
  ))
  

(meth~new-relational-function 'mor-assert)

(meth~defcond mor-assert (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad)
	 (input   )
	 (effect  )
	 (value   ))
	      ;;; args: assumption, goal, var
;; returns T when ASSUMPTION can be used to assert GOAL and binds
;; var to the premises:
;; GOAL-ATOM has a meta-variable as head.
(let* ((assumption (first args))
       (goal (second args))
       (meth-var (third args))
       (goal-sign (not (logic~negation-p goal :theory (prob~theory omega*current-proof-plan))))
       (goal-atom (if goal-sign goal
		    (first (data~appl-arguments goal))))
       (goal-constants (remove-if-not #'term~constant-p
				      (data~all-substructs goal-atom))))
  (multiple-value-bind (sbf prems other-concs all-vars ex-vars)
      (sbf~subformula assumption T
		      #'(lambda (sbf)
			  (and (logic~atom-p sbf :theory (prob~theory omega*current-proof-plan))
			       (term~appl-p sbf)
			       (some #'(lambda (x) (find x goal-constants))
				     (apply #'append
					    (mapcar #'data~all-substructs
						    (data~appl-arguments sbf))))))
		      goal-sign)
    (if sbf
	(let* ((mvars (mapcar #'(lambda (alv)
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
		  (multiple-value-bind (uni-cstr bindings)
		      (if splitp
			  (values
			   (cstr~disjunction (mapcar #'(lambda (C Bs)
							 (cstr~conjunction (if (cstr~constraint-p C)
									       (append Bs (list C))
									     Bs)))
						     cstrs bindingss)))
			(values cstrs bindingss))
		    (if uni-cstr
			(meth~mapp-new-constraint
			 (meth~mapp-new-extension
			  cmapp (if (meth~mapp-extension cmapp)
				    (mapp~create (cons meth-var (mapp~domain (meth~mapp-extension cmapp)))
						 (cons (mapcar
							#'(lambda (prem)
							    (multiple-value-bind (new-prem)
								(data~replace-free-variables
								 prem
								 (append all-vars ex-vars) mvars&consts)
							      (beta~normalize new-prem)))
							prems)
						       (mapp~codomain (meth~mapp-extension cmapp))))
				  (mapp~create (list meth-var)
					       (list (mapcar
						      #'(lambda (prem)
							  (multiple-value-bind (new-prem)
							      (data~replace-free-variables
							       prem (append all-vars ex-vars) mvars&consts)
							    (beta~normalize new-prem)))
						      prems)))))
			 (cstr~conjunction (cons (meth~mapp-constraint cmapp)
						 (cons uni-cstr bindings))))
		      (meth~mapp-new-constraint cmapp NIL)))))
	    (multiple-value-bind (cstrs bindingss splitp)
		(cstr~sat-unify goal sbf T)
	      (multiple-value-bind (uni-cstr bindings)
		  (if splitp
		      (values
		       (cstr~disjunction (mapcar #'(lambda (C Bs)
						     (cstr~conjunction (if (cstr~constraint-p C)
									   (append Bs (list C))
									 Bs)))
						 cstrs bindingss)))
		    (values cstrs bindingss))
		(if uni-cstr
		    (meth~mapp-new-constraint
		     (meth~mapp-new-extension
		      cmapp (if (meth~mapp-extension cmapp)
				(mapp~create (cons meth-var (mapp~domain (meth~mapp-extension cmapp)))
					     (cons prems (mapp~codomain (meth~mapp-extension cmapp))))
			      (mapp~create (list meth-var) (list prems))))
		     (cstr~conjunction (cons (meth~mapp-constraint cmapp)
					     (cons uni-cstr bindings))))
		  (meth~mapp-new-constraint cmapp NIL))))))
      (meth~mapp-new-constraint cmapp NIL)))
  ))

(meth~new-relational-function 'mor-assert4)

(meth~defcond mor-assert4 (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad)
	 (input   )
	 (effect  )
	 (value   ))
	      ;;; args: assumption, goal, var-conc, var-prems 
;; returns T when ASSUMPTION can be used to assert GOAL and binds
;; var-conc to the conclusion and var-prems to the premises:
;; GOAL-ATOM has a meta-variable as head.
(let* ((assumption (first args))
       (goal (second args))
       (meth-var-conc (third args))
       (meth-var-prems (fourth args))
       (goal-sign (not (logic~negation-p goal :theory (prob~theory omega*current-proof-plan))))
       (goal-atom (if goal-sign goal
		    (first (data~appl-arguments goal))))
       (goal-constants (remove-if-not #'term~constant-p
				      (data~all-substructs goal-atom))))
  (multiple-value-bind (sbf prems other-concs all-vars ex-vars)
      (sbf~subformula assumption T
		      #'(lambda (sbf)
			  (and (logic~atom-p sbf :theory (prob~theory omega*current-proof-plan))
			       (term~appl-p sbf)
			       (some #'(lambda (x) (find x goal-constants))
				     (apply #'append
					    (mapcar #'data~all-substructs
						    (data~appl-arguments sbf))))))
		      goal-sign)
    (if sbf
	(let* ((mvars (mapcar #'(lambda (alv)
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
		  (multiple-value-bind (uni-cstr bindings)
		      (if splitp
			  (values
			   (cstr~disjunction (mapcar #'(lambda (C Bs)
							 (cstr~conjunction (if (cstr~constraint-p C)
									       (append Bs (list C))
									     Bs)))
						     cstrs bindingss)))
			(values cstrs bindingss))
		    (if uni-cstr
			(meth~mapp-new-constraint
			 (meth~mapp-new-extension
			  cmapp (if (meth~mapp-extension cmapp)
				    (mapp~create (append (list meth-var-conc meth-var-prems)
							 (mapp~domain (meth~mapp-extension cmapp)))
						 (append
						  (list (multiple-value-bind (new-sbf)
							    (data~replace-free-variables
							     sbf
							     (append all-vars ex-vars) mvars&consts)
							  (beta~normalize new-sbf))
							(mapcar
							 #'(lambda (prem)
							     (multiple-value-bind (new-prem)
								 (data~replace-free-variables
								  prem
								  (append all-vars ex-vars) mvars&consts)
							       (beta~normalize new-prem)))
							 prems))
						  (mapp~codomain (meth~mapp-extension cmapp))))
				  (mapp~create (list meth-var-conc meth-var-prems)
					       (list (multiple-value-bind (new-sbf)
							 (data~replace-free-variables
							  sbf (append all-vars ex-vars) mvars&consts)
						       (beta~normalize new-sbf))
						     (mapcar
						      #'(lambda (prem)
							  (multiple-value-bind (new-prem)
							      (data~replace-free-variables
							       prem (append all-vars ex-vars) mvars&consts)
							    (beta~normalize new-prem)))
						      prems)))))
			 (cstr~conjunction (cons (meth~mapp-constraint cmapp)
						 (cons uni-cstr bindings))))
		      (meth~mapp-new-constraint cmapp NIL)))))
	    (multiple-value-bind (cstrs bindingss splitp)
		(cstr~sat-unify goal sbf T)
	      (multiple-value-bind (uni-cstr bindings)
		  (if splitp
		      (values
		       (cstr~disjunction (mapcar #'(lambda (C Bs)
						     (cstr~conjunction (if (cstr~constraint-p C)
									   (append Bs (list C))
									 Bs)))
						 cstrs bindingss)))
		    (values cstrs bindingss))
		(if uni-cstr
		    (meth~mapp-new-constraint
		     (meth~mapp-new-extension
		      cmapp (if (meth~mapp-extension cmapp)
				(mapp~create (append (list meth-var-conc meth-var-prems)
						     (mapp~domain (meth~mapp-extension cmapp)))
					     (append (list sbf prems)
						     (mapp~codomain (meth~mapp-extension cmapp))))
			      (mapp~create (list meth-var-conc meth-var-prems) (list sbf prems))))
		     (cstr~conjunction (cons (meth~mapp-constraint cmapp)
					     (cons uni-cstr bindings))))
		  (meth~mapp-new-constraint cmapp NIL))))))
      (meth~mapp-new-constraint cmapp NIL)))
  ))
			  

(meth~new-relational-function 'flex-subterms)

(meth~defcond flex-subterms (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
	      ;;; term, meth-var
;; binds meth-var to the flexible subterms of term, if there is some
;; otherwise returns NIL
(let* ((term (first args))
       (meth-var (second args))
       (flex-subterms (remove-if-not #'(lambda (x) (and (term~appl-p x)
							(meta~p (data~appl-function x))))
				     (data~all-substructs term))))
  (if flex-subterms
      (if (meth~mapp-extension cmapp)
	  (meth~mapp-new-extension cmapp
				   (mapp~create (cons meth-var (mapp~domain (meth~mapp-extension cmapp)))
						(cons flex-subterms (mapp~codomain (meth~mapp-extension cmapp)))))
	(meth~mapp-new-extension cmapp (mapp~create (list meth-var) (list flex-subterms))))
    (meth~mapp-new-constraint cmapp NIL))))

(meth~defcond subterm (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(multiple-value-bind (cstr other-cstrs)
    (cstr~sat-subterm (first args) (second args) T)
  (if cstr
      (meth~mapp-new-constraint cmapp
				(cstr~conjunction (cons (meth~mapp-constraint cmapp)
							(cons cstr other-cstrs))))
    (meth~mapp-new-constraint cmapp nil))))

(meth~defcond not-subterm (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(multiple-value-bind (cstr other-cstrs)
    (cstr~sat-subterm (first args) (second args) NIL)
  (if cstr
      (meth~mapp-new-constraint cmapp
				(cstr~conjunction (cons (meth~mapp-constraint cmapp)
							(cons cstr other-cstrs))))
    (meth~mapp-new-constraint cmapp nil))))

(meth~defcond is-head-of (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(multiple-value-bind (cstr other-cstrs)
    (cstr~sat-is-head-of (first args) (second args) T)
  (if cstr
      (meth~mapp-new-constraint cmapp
				(cstr~conjunction (cons (meth~mapp-constraint cmapp)
							(cons cstr other-cstrs))))
    (meth~mapp-new-constraint cmapp nil))))

(meth~defcond not-is-head-of (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(multiple-value-bind (cstr other-cstrs)
    (cstr~sat-is-head-of (first args) (second args) NIL)
  (if cstr
      (meth~mapp-new-constraint cmapp
				(cstr~conjunction (cons (meth~mapp-constraint cmapp)
							(cons cstr other-cstrs))))
    (meth~mapp-new-constraint cmapp nil))))

(meth~defcond not-type= (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(multiple-value-bind (cstr other-cstrs)
    (cstr~sat-type= (first args) (second args) NIL)
  (if cstr
      (meth~mapp-new-constraint cmapp
				(cstr~conjunction (cons (meth~mapp-constraint cmapp)
							(cons cstr other-cstrs))))
    (meth~mapp-new-constraint cmapp NIL))))

(meth~defcond type= (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(multiple-value-bind (cstr other-cstrs)
    (cstr~sat-type= (first args) (second args) T)
  (if cstr
      (meth~mapp-new-constraint cmapp
				(cstr~conjunction (cons (meth~mapp-constraint cmapp)
							(cons cstr other-cstrs))))
    (meth~mapp-new-constraint cmapp NIL))))

(meth~defcond not-unify (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(multiple-value-bind (cstrs bindingss splitp)
    (cstr~sat-unify (first args) (second args) NIL)
  (multiple-value-bind (cstr other-cstrs)
      (if splitp
	  (values
	   (cstr~disjunction (mapcar #'(lambda (C Bs)
					 (cstr~conjunction (if (cstr~constraint-p C)
							       (append Bs (list C))
							     Bs)))
				     cstrs bindingss)))
	(values cstrs bindingss))
    (if cstr
	(meth~mapp-new-constraint cmapp
				  (cstr~conjunction (cons (meth~mapp-constraint cmapp)
							  (cons cstr other-cstrs))))
      (meth~mapp-new-constraint cmapp NIL)))))

(meth~defcond unify (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(multiple-value-bind (cstrs bindingss splitp)
    (cstr~sat-unify (first args) (second args) T)
  (multiple-value-bind (cstr other-cstrs)
      (if splitp
	  (values
	   (cstr~disjunction (mapcar #'(lambda (C Bs)
					 (cstr~conjunction (if (cstr~constraint-p C)
							       (append Bs (list C))
							     Bs)))
				     cstrs bindingss)))
	(values cstrs bindingss))
    (if cstr
	(meth~mapp-new-constraint cmapp
				  (cstr~conjunction (cons (meth~mapp-constraint cmapp)
							  (cons cstr other-cstrs))))
      (meth~mapp-new-constraint cmapp NIL)))))

(meth~defcond ho-match (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
	      ;;; args: var-term, const-term, variables to be considered as constants or NIL:
;; ho-matches VAR-TERM with CONST-TERM:
(let ((var-term (first args))
      (const-term (second args))
      (vs-as-cs (third args)))
  (let ((hou-list (uni~unify (list (list var-term const-term))
			     :class 'uni+match
			     :match-only (if vs-as-cs vs-as-cs
					   (term~free-variables const-term)))))
    (if hou-list
	(let* ((hou (cstr~adapt-hou-result (first hou-list)))
	       (subst (uni~substitution hou))
	       (flex-flex (uni~flex-flex hou)))
	  (meth~mapp-new-constraint
	   cmapp
	   (cstr~conjunction
	    (append (list (meth~mapp-constraint cmapp))
		    (mapcar #'(lambda (ff)
				(cstr~simple-create :unify ff
						    (cstr~free-variables ff)))
			    flex-flex)
		    (mapcar #'(lambda (var term)
				(cstr~binding-create (list var term)))
			    (subst~domain subst) (subst~codomain subst))))))
      (meth~mapp-new-constraint cmapp nil)))))

(meth~defcond cbind (args cmapp)
(declare (edited  "11-AUG-1999")
	 (authors Lassaad )
	 (input   )
	 (effect  )
	 (value   ))
(meth~mapp-new-constraint cmapp
			  (cstr~conjunction
			   (list (meth~mapp-constraint cmapp)
				 (cstr~binding-create args
						      (cons (first args)
							    (cstr~free-variables (second args))))))
			  ))

;;;; Stuff for ore**-method

(meth~defcond nodes-disjunction-p (args cmapp)
  (declare (edited  "07-MAR-2000")
	   (authors Sorge)
	   (input   "A list of nodes.")
	   (effect  "None.")
	   (value   "<T, mmapp> when all nodes are disjunctions"
		    "<NIL, mmapp>, otherwise."))
  (meth~mapp-new-constraint cmapp
			    (every #'(lambda (node)
				       (and (pdsn~p node)
					    (logic~disjunction-p (node~formula node)
								 :theory (prob~theory omega*current-proof-plan))))
				   (first args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing and Applying Tactics in Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond test-tactic (args cmapp)
  (declare (edited  "11-MAY-2000")
	   (authors Vxs)
	   (input   "A list of arguments:"
		    "1. a tactic"
		    "2. an outline"
		    "3. a list of parameters.")
	   (effect  "Can expand CMAPP.")
	   (value   "<T, mapp'>, if the tactic is applicable to the arguments, <NIL, mapp> otherwise."))
  (let* ((inference (first args))
	 (outline (second args))
	 (parameters (third args)))
    (if (meth-help~apply-tactic inference outline parameters :purpose :test)
	cmapp
      (meth~mapp-new-constraint cmapp NIL))))


(meth~deffun apply-tactic (inference outline parameters)
  (declare (edited  "11-MAY-2000")
	   (authors Vxs)
	   (input   "A tactic, an outline and a list of parameters.")
	   (value   "If the tactic is applicable to the arguments the completed outline,"
		    "<NIL, mapp> otherwise."))
  (multiple-value-bind (success conclusions premises hyps wtac)
      (meth-help~apply-tactic inference outline parameters :purpose :apply)
    (cond ((and success wtac) (list conclusions premises hyps))
	  (success conclusions))))
	     

(defun meth-help~apply-tactic (name outline parameters &key ((:purpose purpose) :apply))
  (let* ((outline-pattern (infer~compute-outline-pattern outline)) 
	 (object (infer~outline-pattern2application name outline-pattern))
	 (rule? (rule~find-rule object))
	 (tactic? (tac~find-tactic object))
	 (wtac (infer~find-method name))
	 (wtac? (when (infer~wild-tactic-p wtac)
		  (if object object (infer~outline-function wtac)))))
    (cond (rule? (multiple-value-bind (success new-outline)
		     (rule~apply object outline parameters nil :purpose purpose)
		   (when (and success (not (string-equal purpose :test)))
		     (setf (pdsj~outline-pattern (node~justification (car new-outline))) outline-pattern))
		   (values success new-outline)))
	  (tactic? (multiple-value-bind (success new-outline concl)
		       (tac~apply object outline parameters nil :purpose purpose)
		   (when (and success (not (string-equal purpose :test)))
		     (tac~rewrite-outline-pattern outline-pattern concl))
		   (values success new-outline)))
	  (wtac? (multiple-value-bind (conclusions premises)
		     (wtac~preprocess-outline outline outline-pattern)
		   (multiple-value-bind (success new-conclusions new-premises hypotheses)
		       (wtac~apply wtac object conclusions premises parameters nil
				   :purpose purpose :passkey (infer~passkey wtac))
		     (when (and success (not (string-equal purpose :test)))
		       (let* ((new-pattern (append
					    (make-list (length conclusions) :initial-element "EXISTENT")
					    (make-list (- (length new-conclusions)
							  (length conclusions))
						       :initial-element "NONEXISTENT")
					    (make-list (length premises) :initial-element "EXISTENT")
					    (make-list (- (length new-premises)
							  (length premises))
						       :initial-element "NONEXISTENT"))))
			 (tac~rewrite-outline-pattern new-pattern new-conclusions)))
		     (values success new-conclusions new-premises hypotheses t)))))))


(meth~deffun read-type (expr)
  (declare (edited  "31-MAR-2000")
	   (authors Ameier)
	   (input   "A post expresion.")
	   (effect  "None.")
	   (value   "The post Expression is read as type."))
  (progn
    (setq fatal expr)
    (post~read-object expr (pds~environment omega*current-proof-plan) :existing-type)))
	     
(meth~defcond number-p (args cmapp)
  (declare (edited  "07-MAR-2000")
	   (authors Ameier)
	   (input   "A list of args (a term).")
	   (effect  "None.")
	   (value   "<T, mmapp> if the term is a term\~number-p"
		    "<NIL, mmapp>, otherwise."))
  (let* ((term (first args)))
    (if (term~number-p term)
	cmapp
      (meth~mapp-new-constraint cmapp nil))))


(meth~defcond number-equal-p (args cmapp)
  (declare (edited  "07-MAR-2000")
	   (authors Ameier)
	   (input   "A list of args (two terms which are numbers).")
	   (effect  "None.")
	   (value   "<T, mmapp> if the numbers of the terms are equal."
		    "<NIL, mmapp>, otherwise."))
  (let* ((term1 (first args))
	 (term2 (second args)))
		
    (if (= (keim~name term1) (keim~name term2))
	cmapp
      (meth~mapp-new-constraint cmapp nil))))

(meth~defcond equality-pos-exist (args cmapp)
	       (declare (edited  "15-SEP-2000")
		      (authors Ameier)
		      (input   "")
		      (effect  "")
		      (value   ""))
	      (let* ((term (first args))
		     (equal-term (node~formula (second args)))
		     (position (third args)))
		(meth~mapp-new-constraint cmapp (methhelp=equality-subst-f-p term equal-term position))))

(defun methhelp=equality-subst-f-p (term equal-term position)
  (declare (edited  "15-FEB-2001")
	   (authors Ameier)
	   (input   "A term t, an equation e, a position pos.")
	   (effect  "None.")
	   (value   "T if term contains the position pos, and the term t' at this position equals either the left-hand side"
		    "or the right hand side of the equation."))
  (when (logic~equality-p equal-term)
    (let* ((arg1 (first (data~appl-arguments equal-term)))
	   (arg2 (second (data~appl-arguments equal-term)))
	   (positions-of-arg1 (data~substruct-positions arg1 term :test 'data~equal))
	   (positions-of-arg2 (data~substruct-positions arg2 term :test 'data~equal)))
      (or (find position positions-of-arg1 :test 'keim~equal)
	  (find position positions-of-arg2 :test 'keim~equal)))))


(meth~deffun compute-=subst-prem (term node position)
	     (declare (edited  "15-SEP-2000")
		      (authors Ameier)
		      (input   "")
		      (effect  "")
		      (value   ""))
	     (let* ((equal-term (node~formula node)))
	       (methhelp=equality-subst-create-f term equal-term position)))

(defun methhelp=equality-subst-create-f (term equal-term position)
  (declare (edited  "15-FEB-2001")
	   (authors Ameier)
	   (input   "A term t, an equation e, a position pos.")
	   (effect  "NOne.")
	   (value   "A term t' that equals t except that at the position pos in t an ocurrence of one side"
		    "of e is replaced by the corresponding other side."))
  (let* ((term-at-position (data~struct-at-position term position))
	 (args (data~appl-arguments equal-term)))
    (cond ((data~equal term-at-position (first args))
	   (data~replace-at-position term position (second args)))
	  (t
	   (data~replace-at-position term position (first args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Some condfuncs for Lassaads methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~deffun universal-quant (varlist formula)
	     (declare (edited  "19-APR-2001")
		      (authors Scholl)
		      (input   )
		      (effect  )
		      (value   ))
	     (help~create-forall varlist formula))

(meth~defcond lassert (args cmapp)
	      (declare (edited  "29-MAY-2001")
		       (authors Scholl)
		       (input   )
		       (effect  )
		       (value   ))
	      (let ((ass (first args))
		    (pos (second args))
		    (signs (third args))
		    (subterms (fourth args))
		    (prem (fifth args))
		    (conc (sixth args))
		    (xl (seventh args)))
		(if (not (pos~p pos))
		    (meth~mapp-new-constraint cmapp NIL)
		  (multiple-value-bind (success sf-prems sf-concs mvars c binding vars)
		      (ass~determine-sequent nil (sf~create ass sgn*plus) pos signs subterms)
		    (if success
			(let* ((prems (mapcar 'sf~to-formula sf-prems))
			       (concs (mapcar 'sf~to-formula sf-concs))
			       (substitute-vars (intersection vars (remove-duplicates (apply 'append (mapcar 'term~variables prems)))))
			       (new-binding (help~bind-vars-to-new-metavars substitute-vars (subst~create nil nil))))
			  (meth~mapp-extend-mapp cmapp prem (help~build-conjunction
							     (help~apply-subst-to-all binding (help~apply-subst-to-all new-binding prems))))
			  (meth~mapp-extend-mapp cmapp conc (ass~create-formula-for-conc (set-difference vars (subst~domain new-binding))
											 concs binding new-binding))
			  (meth~mapp-extend-mapp cmapp xl mvars)
			  (when c (meth~mapp-new-constraint cmapp (cstr~conjunction (list (meth~mapp-constraint cmapp) c))))
			  cmapp)
		      (meth~mapp-new-constraint cmapp NIL))))))

(meth~new-relational-function 'lassert)

(meth~defcond literal-subterms (args cmapp)
	      (declare (edited  "29-MAY-2001")
		       (authors Scholl)
		       (input   )
		       (effect  )
		       (value   ))
	      (let ((psi (first args))
		    (subterms (second args)))
		(meth~mapp-extend-mapp cmapp subterms (help~literal-subterms psi))
		cmapp))

(meth~new-relational-function 'literal-subterms)

(meth~defcond assert-with (args cmapp)
	      (declare (edited  "29-MAY-2001")
		       (authors Scholl)
		       (input   )
		       (effect  )
		       (value   ))
	      (let ((ass (first args))
		    (pos (second args))
		    (signs (third args))
		    (subterms (fourth args))
		    (conc (fifth args)))
		(if (not (pos~p pos))
		    (meth~mapp-new-constraint cmapp NIL)
		  (multiple-value-bind (success sf-prems sf-concs mvars c binding vars)
		      (ass~determine-sequent t (sf~create ass sgn*plus) pos signs subterms)
		    (if success
			(let ((prems (mapcar 'sf~to-formula sf-prems)))
			  (meth~mapp-extend-mapp cmapp conc
						 (help~create-forall (set-difference vars (subst~domain binding))
								    (help~build-implication (help~apply-subst-to-all binding prems))))
			  (when c (meth~mapp-new-constraint cmapp (cstr~conjunction (list (meth~mapp-constraint cmapp) c))))
			  cmapp)
		      (meth~mapp-new-constraint cmapp NIL))))))

(meth~new-relational-function 'assert-with)

(meth~defcond match-logical (args cmapp)
	      (declare (edited  "29-MAY-2001")
		       (authors Scholl)
		       (input   )
		       (effect  )
		       (value   ))
	      (let* ((phi (first args))
		     (psi (second args))
		     (new-constr (match~log (sf~create phi sgn*plus) (sf~create psi sgn*plus))))
		(if new-constr
		    (meth~mapp-new-constraint cmapp (cstr~conjunction (list (meth~mapp-constraint cmapp) new-constr)))
		  (meth~mapp-new-constraint cmapp NIL))))

(meth~defcond newconst (args cmapp)
	      (declare (edited  "29-MAY-2001")
		       (authors Scholl)
		       (input   )
		       (effect  )
		       (value   ))
	      (multiple-value-bind (constr inst)
		  (cstr~sat-newconst (first args) T)
		(if (eq t constr)
		    (meth~mapp-extend-subst cmapp (cstr~bound-variable (cstr~inst-to-constr inst))
					    (cstr~binding-term (cstr~inst-to-constr inst)))
		  (meth~mapp-new-constraint cmapp (cstr~conjunction (list (meth~mapp-constraint cmapp) constr))))))


;;;;;;; for the =subst*-method

(meth~defcond equality-poses-exist (args cmapp)
	       (declare (edited  "15-SEP-2000")
		      (authors Ameier)
		      (input   "")
		      (effect  "")
		      (value   ""))
	      (let* ((term (first args))
		     (equations (second args))
		     (positions (third args)))
		(meth~mapp-new-constraint cmapp
					  (every #'(lambda (equation position)
						     (methhelp=equality-subst-f-p term equation position))
						 equations positions))))

(meth~deffun compute-=subst*-prem (term equations positions)
	     (declare (edited  "15-SEP-2000")
		      (authors Ameier)
		      (input   "")
		      (effect  "")
		      (value   ""))
	     (do* ((current-term term)
		   (current-equations equations (rest current-equations))
		   (current-positions positions (rest current-positions)))
		 ((null current-equations)
		  current-term)
	       (setq current-term (methhelp=equality-subst-create-f current-term
								    (first current-equations)
								    (first current-positions)))))

(meth~deffun compute-open-nodes (node formulas)
	     (declare (edited  "15-SEP-2000")
		      (authors Ameier)
		      (input   "")
		      (effect  "")
		      (value   ""))
	     (let* ((hyps (pdsn~hyps node)))
	       (mapcar #'(lambda (formula)
			   (pdsn~open-node-create formula hyps (pds~new-node-name)))
		       formulas)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stuff for calling ATPs in methods (see base theory)

(meth~deffun get-otter-just (node)
  (declare (edited  "15-FEB-2002")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "A justification with method otter, the supports of the node as premises, and"
		    "nil as the only parameter."))
  (pdsj~closed-just-create (infer~find-method 'otter)
			   (pds~node-supports node)
			   (list nil)))

(meth~deffun get-bliksem-just (node)
  (declare (edited  "15-FEB-2002")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "A justification with method bliksem, the supports of the node as premises, and"
		    "nil as the only parameter."))
  (pdsj~closed-just-create (infer~find-method 'bliksem)
			   (pds~node-supports node)
			   (list nil)))

(meth~deffun iterated-expansion-of-def (phi symbol thing-def pos-list)
  (declare (edited  "13-FEB-2002")
	   (authors Ameier)
	   (input   "A formula and a definition.")
	   (effect  "None.")
	   (value   "A formula in which all occurences of the defined concept are expanded."))
  (let* ((definiendum symbol)
	 (definiens (data~copy thing-def :downto '(term+constant
						   type+primitive))))
    (gentac=compute-iterated-defne phi definiendum definiens pos-list)))

	 

(meth~deffun getsupports (node)	     
  (declare (edited  "08-FEB-2002")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "The support nodes of this node."))
  (pds~node-supports node))


(meth~deffun getnillist ()
  (declare (edited  "08-FEB-2002")
	   (authors Ameier)
	   (input   "None.")
	   (effect  "None.")
	   (value   "A list with nil as only element."))
  (list nil))

(meth~defcond otter-proves-node-p (args cmapp)
	      (declare (edited  "08-FEB-2002")
		       (authors Ameier)
		       (input   "A open node, a cmapping.")
		       (effect  "None.")
		       (value   "Otter is called on the node. If Otter proves the goal the cmapping is returned,"
				"oterwise the cmapping is set to nil."))
	      (let* ((node (first args))
		     (result (methhelp=call-otter-on-node node)))
		(if result
		    cmapp
		  (meth~mapp-new-constraint cmapp nil))))

(meth~defcond bliksem-proves-node-p (args cmapp)
	      (declare (edited  "08-FEB-2002")
		       (authors Ameier)
		       (input   "A open node, a cmapping.")
		       (effect  "None.")
		       (value   "Bliksem is called on the node. If Bliksem proves the goal the cmapping is returned,"
				"oterwise the cmapping is set to nil."))
	      (let* ((node (first args))
		     (result (methhelp=call-bliksem-on-node node)))
		(if result
		    cmapp
		  (meth~mapp-new-constraint cmapp nil))))

(meth~deffun get-tramp-just (node atpsym)
  (declare (edited  "15-FEB-2002")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "A justification with method atpsym, the supports of the node as premises, and"
		    "nil as the only parameter."))
  (pdsj~closed-just-create (infer~find-method atpsym)
			   (pds~node-supports node)
			   (list nil)))

(meth~defcond tramp-proves-node-p (args cmapp)
	      (declare (edited  "08-FEB-2002")
		       (authors Ameier)
		       (input   "An open node and a variable, a cmapping.")
		       (effect  "None.")
		       (value   "Tramp is called on the node. If one of the provers connected by tramp proves the goal"
				"the cmapping is returned and extended by binding the variable to the (one) prover that"
				"succeeded to prove the goal, otherwise the cmapping is set to nil."))
	      (let* ((node (first args))
		     (atpvar (second args))
		     (result (methhelp=call-tramp-on-node node 100)))
		(if result
		    (meth~mapp-extend-mapp cmapp atpvar result)
		  (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'tramp-proves-node-p)

(defvar methhelp*otter-problem nil)
(defvar methhelp*bliksem-problem nil)
(defvar methhelp*spass-problem nil)

(defun methhelp=call-tramp-on-node (node ressource)

  (setf methhelp*otter-problem nil)
  (setf methhelp*bliksem-problem nil)
  (setf methhelp*spass-problem nil)
  
  (let* ((time (get-universal-time))
	 (otter-process (proc~create :name "otter-process"
				     :priority 300
				     :function #'methhelp=call-otter-on-node 
				     :args (node ressource)))
#|	 (bliksem-process (proc~create :name "bliksem-process"
				       :priority 300
				       :function #'methhelp=call-bliksem-on-node 
				       :args (node ressource)))
	 (spass-process (proc~create :name "spass-process"
	                             :priority 300
	 			     :function #'methhelp=call-spass-on-node 
	 			     :args (node ressource)))
	 |#
	 )

    (do* ((term-flag nil)
	  )
	((or term-flag
	     (> (- (get-universal-time) time) (+ ressource 60)))
	 nil)
      (when (or (and methhelp*otter-problem
		     (atpprb~complete-p methhelp*otter-problem))
		(and methhelp*spass-problem
		     (atpprb~complete-p methhelp*spass-problem))
		(and methhelp*bliksem-problem
		     (atpprb~complete-p methhelp*bliksem-problem)))	
	(setf term-flag 't)))
    
    (let* ((problems (remove-if #'null (list methhelp*otter-problem methhelp*spass-problem methhelp*bliksem-problem)))
	   (complete-problems (remove-if-not #'atpprb~complete-p problems)))
      
      ;;(format t "~%COMPLETE-PROBLEMS:")
      ;;(mapcar #'(lambda (prob)
      ;;		  (format t "~%      ~A" (atpprb~problem-type prob)))
      ;;	      complete-problems)
      
      (if complete-problems
	  (progn
	    (setf omega*current-resolution-proof (atpprb~problem-part-res-proof (first complete-problems)))
	    (atpprb~problem-type (first complete-problems)))
	nil))))


(defun methhelp=call-otter-on-node (node ressource)
  (let* ((problem-name "OTTERM")
	 (otter-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) (atptop~default-directory)))
	 (in-file (merge-pathnames "otter.in" otter-problem-dir))
	 (out-file (merge-pathnames "otter.out" otter-problem-dir))
	 (otter-problem (otter~generate-otter-problem node
						      (pds~node-supports node)
						      omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof otter-problem))
	 (inter? atptop*interactivity-allowed))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file otter-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring otter-problem-dir)))))
    
    ;; erzeuge otter.in file in otter*in-string, fuege otter*in-string zum otter-problem hinzu (einschlieslich proof-object)
    (otter~add-in-string! otter-problem
			  'auto
			  (append (list (format nil "%%%% MODE FLAGS %%%%~A" #\Newline)
					"set(auto).")
				  (list "set(build_proof_object).")
				  otter*needed-flags)
			  (res~proof-clauses res-proof)
			  nil
			  't)
    
    (setq atptop*interactivity-allowed nil)
    
    ;; call-otter vot Ort -> schreibt otter.out file in den out-string des otter-problems
    (otter=call-otter! otter-problem otter-problem-dir ressource)

    (setq atptop*interactivity-allowed inter?)

    (pds~open-node! node)
    
    ;; parsen des otter-beweises
    (otter~complete-otter-problem! otter-problem :parse-back 't)

    (setf methhelp*otter-problem otter-problem)

    ))

(defun methhelp=call-bliksem-on-node (node ressource)
  (let* ((problem-name "BLIKSEMM")
	 (bliksem-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) (atptop~default-directory)))
	 (in-file (merge-pathnames "bliksem.in" bliksem-problem-dir))
	 (out-file (merge-pathnames "bliksem.out" bliksem-problem-dir))
	 (bliksem-problem (blik~generate-bliksem-problem node
							 (pds~node-supports node)
							 omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof bliksem-problem))
	 (inter? atptop*interactivity-allowed))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file bliksem-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring bliksem-problem-dir)))))
    
    ;; erzeuge bliksem.in file in bliksem*in-string, fuege bliksem*in-string zum bliksem-problem hinzu (einschlieslich proof-object)
    (blik~add-in-string! bliksem-problem
			 (res~proof-clauses res-proof)
			 (format nil "~A~A~A~A~A"
				 "Set( totalproof, 1 )."
				 #\Newline
				 "Set( prologoutput, 1 )."
				 #\Newline
				 "Auto."
				 ))
        
    (setq atptop*interactivity-allowed nil)
    
    ;; call-bliksem vot Ort -> schreibt bliksem.out file in den out-string des bliksem-problems
    (blik=call-bliksem! bliksem-problem bliksem-problem-dir ressource)

    (setq atptop*interactivity-allowed inter?)

    (pds~open-node! node)
    
    ;; parsen des otter-beweises
    (blik~complete-bliksem-problem! bliksem-problem :parse-back 't)

    (setf methhelp*bliksem-problem bliksem-problem)
    
    ))



(defun methhelp=call-spass-on-node (node ressource)
  (let* ((problem-name "SPASSM")
	 (spass-problem-dir (merge-pathnames (format nil "~A/" (string-downcase problem-name)) (atptop~default-directory)))
	 (in-file (merge-pathnames "spass.cnf" spass-problem-dir))
	 (out-file (merge-pathnames "spass.dfg" spass-problem-dir))
	 (spass-problem (spass~generate-spass-problem node
						      (pds~node-supports node)
						      omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof spass-problem))
	 (inter? atptop*interactivity-allowed))
    
    ;; erzeugt directory falls es nicht existiert
    (unless (probe-file spass-problem-dir)
      (sys~call-system (format nil "mkdir ~A" (string-right-trim '(#\/) (namestring spass-problem-dir)))))
    
    ;; erzeuge spass.cnf file in spass*in-string, fuege spass*in-string zum spass-problem hinzu (einschlieslich proof-object)
    (spass~add-in-string! spass-problem 't 0)
    
    (setq atptop*interactivity-allowed nil)
    
    ;; call-spass vot Ort -> schreibt spass.out file in den out-string des spass-problems
    (spass=call-spass! spass-problem spass-problem-dir ressource)

    (setq atptop*interactivity-allowed inter?)

    (pds~open-node! node)
    
    ;; parsen des spass-beweises
    (spass~complete-spass-problem! spass-problem :parse-back 't)

    (setf methhelp*spass-problem spass-problem)

    ))



