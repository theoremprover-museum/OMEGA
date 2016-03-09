;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
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
(in-package :omega)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; in-set-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond elem-in-set-p (args cmapp)  
  (declare (edited  "18-SEP-2002")
	   (authors Pollet)
	   (input   "Takes an element and a set.")
	   (effect  "-")
	   (value   "True, if element is element of the set."))
	     (let ((elem (car args))
		    (set-elems (tyse=elements-of-set (second args) nil)))
	       (if (meta~p elem)
		   (mapcar #'(lambda (set-elem)
			       (meth~mapping-create (meth~mapp-subst cmapp)
						    (meth~mapp-mapp cmapp)
						    (meth~mapp-extension cmapp)
						    (cstr~conjunction (list (meth~mapp-constraint cmapp)
									    (cstr~binding-create (list elem set-elem))))))
			   set-elems)
		 (meth~mapp-new-constraint cmapp
					   (some #'(lambda (el)(data~equal el elem)) set-elems)))))
; does not work with unify			       
;   (meth~mapp-new-constraint cmapp
;	     (cstr~disjunction (mapcar #'(lambda (el) (cstr~binding-create (list elem el))) set-elems)))
								   
(defun tyse=elements-of-set (set &optional (var nil))
  (declare (edited  "23-SEP-2002")
	   (authors Pollet)
	   (input   "A term representing a set and opt. a variable for a set defined by abstraction.")
	   (effect  "-")
	   (value   "A list with elements of the set."))
  (cond
   ((term~set-p set)
    (term~normalform set))
   ((logic~disjunction-p set)
    (append (tyse=elements-of-set (first (data~appl-arguments set)) var)
	    (tyse=elements-of-set (second (data~appl-arguments set)) var)))
   ((logic~equality-p set)
    (destructuring-bind (lhs rhs) (data~appl-arguments set)
      (when (data~equal var lhs) (list rhs))
      (when (data~equal var rhs) (list lhs))))
   ((and (data~abstr-p set) (not var))
    (tyse=elements-of-set (data~abstr-range set)(data~abstr-bound-var set)))
   ((and (data~appl-p set) (not var)
	 (data~schema-equal (data~appl-function set)(env~lookup-object 'union (th~env 'typed-set))))
    	 (append (tyse=elements-of-set (first (data~appl-arguments set)) var)
		 (tyse=elements-of-set (second (data~appl-arguments set)) var)))
   ((and (data~appl-p set) (not var)
	 (data~schema-equal (data~appl-function set)(env~lookup-object 'intersection (th~env 'typed-set))))
    (intersection (tyse=elements-of-set (first (data~appl-arguments set)) var)
		  (tyse=elements-of-set (second (data~appl-arguments set)) var)))
   ((and (data~appl-p set) (not var)
	 (data~schema-equal (data~appl-function set)(env~lookup-object 'singleton (th~env 'typed-set))))
    (data~appl-arguments set))
   (T nil)))

  
(meth~deffun expand-defis-except (term defis)  
	     (declare )
	     (gentac=substitute-defis term defis))

(meth~deffun defis-of (symbol-list)  
	     (declare )
	     (mapcan #'(lambda (sym) (when (th~find-assumption sym (prob~proof-theory omega*current-proof-plan))
				       (list (th~find-assumption sym (prob~proof-theory omega*current-proof-plan)))))
			 symbol-list))

(infer~defmethod "in-set-m"
                 (outline-mappings (((existent) "in-set-m-c"))))

(meth~defmethod in-set-m-c in-set-m
               (in typed-set)
               (rating 10)
               (reasoning :planning :middle-out)

	       (declarations
		(type-variables aaa bbb)
		(sorted-meta-variables
		 (phi o  term)
		 (sym o)
		 (elem aaa  term)
		 (tl list termlist)
		 (set (o aaa)  term)))

	       (application-condition
		(elem-in-set-p elem set))
	       
	       (expansion-computations
		(tl  (defis-of (mlist (:symbol =))))
		(phi (expand-defis-except (:term (in elem set)) tl))
		(sym (:symbol t)))
	       
               (conclusions (- l10))

               (decl-content
		(l05 () phi  ("otter" (sym)()))
                (l10 () (in elem set)  ("defsi" (tl) (l05)))
                ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FORALLI-FINITE-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(meth~defcond finite-situation? (args cmapp)  
  (declare (edited  "18-SEP-2002")
	   (authors Pollet)
	   (input   "A formula and a metavar.")
	   (effect  "-")
	   (value   "If the the terms represents a finite situation T and the metavar is bound a"
		    "list of terms that are the different finite cases."))
	     (let* ((formula (car args))
		    (set (second args))
		    (result (tyse=finite-situation? formula)))
	       (if result
		   (progn (meth~mapp-extend-mapp cmapp set result)
			  (meth~mapp-new-constraint cmapp t))
		      (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'finite-situation?)

(defgeneric tyse=finite-situation? (term)
  (declare (edited  "18-SEP-2002")
	   (authors Pollet)
	   (input   "A formula.")
	   (effect  "-")
	   (value   "If the the terms represents a finite situation a list"
		    "of terms that represent the different finite cases."))
  (:method ((term term+set))  
	    (term~normalform term))
  (:method ((term T))
	    nil)) ;;more cases to come

(meth~deffun make-finite-instances (var finite-list formula)
  (declare (edited  "18-SEP-2002")
	   (authors Pollet))
  (mapcar #'(lambda (fini) (subst~apply (subst~create (list var)(list fini)) formula)) finite-list))

(meth~deffun assemble-conjunction (finite-list)
  (declare (edited  "18-SEP-2002")
	   (authors Pollet))
  (reduce #'(lambda (x y)(term~appl-create (logic~conjunction-constant)(list x y)))
	  finite-list))
  
(infer~defmethod "foralli-finite-sort-m"
		 (outline-mappings (((existent nonexistent) foralli-finite-sort-m-b)))
		 (help "The method for FORALL-sort introduction"))


(meth~defmethod foralli-finite-sort-m-b foralli-finite-sort-m
		(in typed-set)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o term)
		  (phi1 o term)
		  (phiprime o term)
		  (sort1 (o aa) term)
		  (c aa const)
		  (finite-list list termlist)
		  )
		 )

		(application-condition
		 (finite-situation? sort1 finite-list))

		(premises (+ l2))
		(conclusions (- l3))

		(outline-computations
		 (phiprime (make-finite-instances x finite-list phi))
		 (phi1 (assemble-conjunction phiprime))
		 )
		
		(decl-content
		 (l2 ()   phi1                                               ("Open" () ()))
		 (l3 ()   (forall-sort (lam (x aa var) phi) sort1)         ("ForallI-Sort" (c) (l20)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ForallI-sort.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EXISTSI-FINITE-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(meth~deffun mstring (obj)
  (declare )
    (format nil "~A" obj))

(infer~defmethod "existsi-finite-sort-m"
		 (outline-mappings (((existent nonexistent) existsi-finite-sort-m-b)))
		 (help "The method for FORALL-sort introduction"))


(meth~defmethod existsi-finite-sort-m-b existsi-finite-sort-m
		(in typed-set)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o term)
		  (phi1 o term)
		  (phiprime o term)
		  (sort1 (o aa) term)
		  (c aa const)
		  (hack aa string)
		  (finitelist list termlist)
		  (finite aa term)
		  (finite-element list termlist)
		  )
		 )

		
		(application-condition
		 (mand (finite-situation? sort1 finitelist)
		       (mexists finite finitelist (mand
						   (mbind hack (mstring finite)) ;to get arout subsumed mmatching->put into map
						   (mbind finite-element (mlist finite)))))) 

		(premises (+ l2))
		(conclusions (- l3))

		(outline-computations
		 (phi1 (subst-apply (subst-create (mlist x) finite-element) phi))
		 )
		
		(decl-content
		 (l2 ()   phi1                                               ("Open" () ()))
		 (l3 ()   (exists-sort (lam (x aa var) phi) sort1)         ("existsI-Sort" (c) (l2)))
		 )
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ForallI-sort.")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; subset-equal-m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "subset-equal-m"
                 (outline-mappings (((existent nonexistent nonexistent) "subset-equal-m-b"))))

(meth~defmethod subset-equal-m-b subset-equal-m
               (in typed-set)
               (rating 10)
               (reasoning :planning :middle-out)

	       (declarations
		(type-variables aaa)
		(sorted-meta-variables (set1 (o aaa) term) (set2 (o aaa) term)))

	       (application-condition)
	       (outline-computations)
	       
	       (premises (+ l01)(+ l02))
               (conclusions (- l10))
	       
               (decl-content
		(l01 () (subsetp set1 set2)                   ("Open" () ()))
		(l02 () (subsetp set2 set1)                   ("Open" () ()))
                (l10 () (= set1 set2)                         ("subset-equal-m" () (l01 l02)))
                )
	       )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;ExistsI-in-SORT-M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ExistsI-in-Sort-m"
		 (outline-mappings (((existent nonexistent nonexistent) existsi-in-sort-m-b)))
		 (help "The method for Exists-sort introduction"))

(meth~defmethod existsi-in-sort-m-b ExistsI-in-Sort-m
		(in typed-set)
		(rating 10)
		(reasoning :planning :middle-out)

		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o)
		  (pos o poslist)
		  (phi1 o)
		  (sort1 (o aa))
		  (mv aa metavar)
		  )
		 )
		
		(premises (+ l1) (+ l2))
		(conclusions (- l3))
		
		(application-condition
		 (termoccs x phi pos))
		(outline-computations
		 (mv (newmetavar (:symbol :M) aa))
		 (phi1 (subst-apply (subst-create (mlist x) (mlist mv)) phi))
		 )
		
		(decl-content
		 (l1 ()   (in mv sort1)                                    ("Open" () ()))   
		 (l2 ()   phi1                                             ("Open" () ()))
		 (l3 ()   (exists-sort (lam (x aa var) phi) sort1)    ("ExistsI-Sort" (mv pos) (l1 l2)))
		 )

		;;(outline-orderings (before l2 l1))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods has the same effect as the backward application
                                       of the tactic ExistsI-sort"))
		)

(defun tyse=abstr-set2elements (abstr)
  (declare (edited  "10-NOV-2002")
	   (authors Vxs)
	   (input   "A set given as a lambda abstraction.")
	   (effect  "None.")
	   (value   "If the abstraction represents a legal set three values:"
		    "1. The elements of the set."
		    "2. T"
		    "3. The abstracted variable."
		    "Otherwise NIL."))
  (labels ((get-element (element variable)
			(when (logic~equality-p element)
			  (destructuring-bind (left right) (data~appl-arguments element)
			    (cond ((data~equal left variable) (list right))
				  ((data~equal right variable) (list left))
				  (t nil)))))
	   (recurse-set (elements variable)
			(if (logic~disjunction-p elements)
			    (let ((ldisj (recurse-set (first (data~appl-arguments elements)) variable))
				  (rdisj (recurse-set (second (data~appl-arguments elements)) variable)))
			      (when (and ldisj rdisj)
				(append ldisj rdisj)))
			  (get-element elements variable))))
    (when (term~abstr-p abstr)
      (let* ((variable (data~abstr-binder abstr))
	     (set (when (and variable (= (length variable) 1))
		    (recurse-set (data~abstr-range abstr) (car variable)))))
	(when set (values set t (car variable)))))))
	

(defun tyse=simple-set2elements (set)
  (declare (edited  "11-NOV-2002")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "If the term represents a set, the elements of the set and T. O/w NIL."))
  (cond ((term~set-p set)
	 (keim~name set))
	((term~abstr-p set)
	 (tyse=abstr-set2elements set))
	((and (term~constant-p set)
	      (data~schema-equal set (env~lookup-object 'emptyset (th~env 'typed-set))))
	 (values nil t))
	((and (term~appl-p set)
	      (data~schema-equal (data~appl-function set)(env~lookup-object 'singleton (th~env 'typed-set))))
	 (values (data~appl-arguments set) t))
	(t nil)))

(eval-when (compile eval load)
(defvar tyse*set-compositions
  (list (cons (env~lookup-object 'union (th~env 'typed-set)) #'union)
	(cons (env~lookup-object 'intersection (th~env 'typed-set)) #'intersection)
	(cons (env~lookup-object 'setminus (th~env 'typed-set)) #'set-difference)
	(cons (env~lookup-object 'exclunion (th~env 'typed-set))
	      #'(lambda (x y &key test) (union (set-difference x y :test test)
					       (set-difference y x :test test) :test test))))
  "An assoc list relating set composition terms to lisp functions on set. Example: ((union . union) (setminus . set-difference)")

(defvar tyse*set-functions (mapcar #'car tyse*set-compositions)
  "A list of valid set functions.")
)

(defun tyse=composed-set2elements (set &optional (compositions tyse*set-compositions))
  (declare (edited  "11-NOV-2002")
	   (authors Vxs)
	   (input   "A term and an assoc list relating set composition terms to lisp functions on set.")
	   (effect  "None.")
	   (value   "T if the input term is a valid set representation and the set of elements as second value. O/w NIL."))
  (let ((set-op (when (term~appl-p set) (assoc (data~appl-function set) compositions :test #'data~schema-equal))))
      (if set-op
	  (multiple-value-bind (fflag fset)
	      (tyse=composed-set2elements (first (data~appl-arguments set)))
	    (when fflag
		(multiple-value-bind (sflag sset)
		    (tyse=composed-set2elements (second (data~appl-arguments set)))
		  (when sflag
		    (values t (funcall (cdr set-op) fset sset :test #'data~equal))))))
	(let ((elems (tyse=simple-set2elements set)))
	  (when elems (values t elems))))))


(defvar tyse*emptyset 'emptyset)

(defun tyse=element-list2set (list)
  (declare (edited  "12-NOV-2002")
	   (authors Vxs)
	   (input   "A list of terms.")
	   (effect  "Creates a TERM+SET object.")
	   (value   "A set of the terms as a TERM+SET object."))
  (if list
      (post~read-object (mapcar #'(lambda (x)
				    (read-from-string (post~string x)))
				list)
			(pds~environment omega*current-proof-plan)
			:set)
    (post~read-object tyse*emptyset (pds~environment omega*current-proof-plan) :existing-term)))


(defun tyse=set-substruct-positions (term)
  (declare (edited  "14-NOV-2002")
	   (authors Vxs)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "The positions where set structures occur, if there are any."))
  (data~positions term #'tyse=composed-set2elements))

(meth~defcond set-subterm-pos (args cmapp)
  (declare (edited  "14-NOV-2002")
	   (authors Vxs)
	   (input   "Two arguments:"
		    "1 - a term."
		    "2 - a position.")
	   (effect  "Position can be bound.")
	   (value   "True, if the subterm at position is actually a set representation (NOT term+set structure!!!)."
		    "If position was unbound it is bound to the first such position."
		    "NIL is returned in case the term does not contain a set representation."))
  (let* ((position (second args))
	 (pds-position (mapp~get-component position (meth~mapp-mapp cmapp)))
	 (real-positions (tyse=set-substruct-positions (first args))))
    (if real-positions
	(if pds-position
	    (meth~mapp-new-constraint cmapp
				      (find pds-position real-positions :test #'data~equal))
	  (progn (meth~mapp-extend-mapp cmapp position (car real-positions))
		 (meth~mapp-new-constraint cmapp t)))
      (meth~mapp-new-constraint cmapp nil))))

(meth~new-relational-function 'set-subterm-pos)

(meth~defcond set-equal-p (args cmapp)  
  (declare (edited  "13-NOV-2002")
	   (authors Vxs)
	   (input   "Two arguments:"
		    "1 - a set in lambda term representation."
		    "2 - a term+set term.")
	   (effect  "If the second argument is not provided and the first argument is a valid set representation,"
		    "a new term+set object is created and bound to the second argument.")
	   (value   "True, if both terms represent the same set. O/w NIL."))
  (let* ((set1 (first args))
	 (set2 (second args))
	 (pds-set2 (mapp~get-component set2 (meth~mapp-mapp cmapp))))
    (if set1
	(multiple-value-bind (flag1 elems1)
	    (tyse=composed-set2elements set1)
	    (if flag1
		(if pds-set2
		    (meth~mapp-new-constraint cmapp
					      (term~equal-p (tyse=element-list2set elems1) pds-set2))
		  (progn (meth~mapp-extend-mapp cmapp set2 (tyse=element-list2set elems1))
			 (meth~mapp-new-constraint cmapp t)))
	      (meth~mapp-new-constraint nil)))
      (meth~mapp-new-constraint nil))))

(meth~new-relational-function 'set-equal-p)

(meth~deffun convert-set-term (set-term)
  (declare (edited  "14-NOV-2002")
	   (authors Vxs)
	   (input   "A non-TERM+SET representation of a set.")
	   (effect  "Creates a new TERM+SET object.")
	   (value   "A TERM+SET term for this set."))
  (multiple-value-bind (flag elems)
      (tyse=composed-set2elements set-term)
    (when flag (tyse=element-list2set elems))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ReRepresent-Set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defmethod "ReRepresent-Set-m"
		 (outline-mappings (((existent nonexistent) ReRepresent-Set-m-b)))
		 (help "A method to rerepresent sets as uniform set terms."))

(meth~defmethod ReRepresent-Set-m-b ReRepresent-Set-m
		(in typed-set)
		(rating 10)
		(reasoning :planning)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables
		  (phi o) (psi o)
		  (pos o position)
		  (set (o aa) term)))
		
		(premises (+ l1))
		(conclusions (- l2))
		
		(application-condition
		 (set-subterm-pos Phi Pos))
		
		(outline-computations
		 (Psi (termrplatpos Phi Pos (convert-set-term (termatpos Phi Pos)))))
		
		(decl-content
		 (l1 ()   Psi                                              ("Open" () ()))   
		 (l2 ()   Phi                                              ("ReRepresent-Set" (pos) (l1)))
		 )

		;;(outline-orderings (before l2 l1))
		
		(proc-content schema-interpreter)
		(manual (documentation "This methods rewrites subterms representing sets into set structures."))
		)


;(setq t1 (post~read-object '(lam (x num) (or (= x 1) (or (= 2 x) (= x 3)))) (th~env :integer) :existing-term))
;(setq t2 (post~read-object '(lam (x num) (y num) (or (= x 1) (or (= 2 y) (= x 3)))) (th~env :integer) :existing-term))
;(setq t3 (post~read-object '(lam (x num) (or (= x 1) (or (= 2 2) (= x 3)))) (th~env :integer) :existing-term))
;(setq t4 (post~read-object '(lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (th~env :integer) :existing-term))
;(setq t5 (post~read-object '(singleton 5) (th~env :integer) :existing-term))
;(setq t6 (post~read-object '(union (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (lam (x num) (or (= x 1) (or (= 2 x) (= x 3))))) (th~env :integer) :existing-term))
;(setq t7 (post~read-object '(intersection (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (lam (x num) (or (= x 1) (or (= 2 x) (= x 3))))) (th~env :integer) :existing-term))
;(setq t8 (post~read-object '(setminus (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (singleton 5)) (th~env :integer) :existing-term))
;(setq t9 (post~read-object '(setminus (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (singleton 6)) (th~env :integer) :existing-term))
;(setq t10 (post~read-object '(exclunion (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (lam (x num) (or (= x 1) (or (= 2 x) (= x 3))))) (th~env :integer) :existing-term))
;(setq t11 (post~read-object 'emptyset (th~env :integer) :existing-term))
;(setq t12 (post~read-object '(intersection (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) emptyset) (th~env :integer) :existing-term))
;(setq t13 (post~read-object '(union (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) emptyset) (th~env :integer) :existing-term))
;(setq t14 (post~read-object '(union (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (lam (x num) (or (= x 1) (or (= 4 x) (= 5 5))))) (th~env :integer) :existing-term))

;(setq s1 (post~read-object '(= (setminus (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (singleton 6)) (setminus (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (singleton 6))) (th~env :integer) :existing-term))
;(setq s2 (post~read-object '(= (set 1 4 5) (setminus (lam (x num) (or (= x 1) (or (= 4 x) (= x 5)))) (singleton 6))) (th~env :integer) :existing-term))


