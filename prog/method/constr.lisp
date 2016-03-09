;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
(in-package "OMEGA")

(mod~defmod CSTR 
            :uses (data keim omega pds subst)
            :documentation "Data Structures and Functions to deal with constraints"
            :exports (cstr+constraint
		      cstr~variables
		      cstr~arguments
		      cstr+simple
		      cstr~polarity
		      cstr+binding
		      cstr+composition
		      cstr~bound-variable
		      cstr~binding-term
		      cstr~binding-variables
		      cstr~binding-create
		      cstr~simple-create
		      cstr~composition-create
		      cstr~create
                      cstr~constraint-p
                      cstr~simple-p
		      cstr~binding-p
		      cstr~composition-p
		      cstr~free-variables
		      cstr~adapt-hou-result
		      cstr~conjunction-p
		      cstr~conjunction
		      cstr~disjunction-p
		      cstr~disjunction
		      cstr~negate
		      cstr~then-else
		      cstr~merge
		      cstr~satisfiable-p
		      cstr~sat-subterm
		      cstr~sat-is-head-of
		      cstr~sat-linear
		      cstr~sat-type=
		      cstr~sat-unify
		      cstr~variable-paths
		      cstr*unification-depth
		      
                      ))

;;; A constraint system for unification, dis-unification, subterm, type-equality, ...
;;  

(defvar cstr*unification-depth 10)


(eval-when (load compile eval)
(defclass cstr+constraint (keim+name)
  ((vars :initarg :vars
	 :accessor cstr~variables
	 :documentation "The variables of the constraint.")
   (args :initarg :args
	 :accessor cstr~arguments
	 :documentation "The arguments of the constraint."))
  (:documentation "A data structure for representing constraints."))

(defclass cstr+simple (cstr+constraint)
  ((polarity :initarg :polarity
	     :initform T
	     :accessor cstr~polarity
	     :documentation "The polarity (sign) of the constraint."))
  (:documentation "A data structure for representing simple constraints."))

(defclass cstr+binding (cstr+constraint)
  ()
  (:documentation "A data structure for representing binding constraints."))

(defclass cstr+composition (cstr+constraint)
  ()
  (:documentation "A data structure for representing composed constraints."))
)

;;; PDS output
(pp~modify-style pds-post
   (cstr+composition
    (lambda (s cstr)
      (let ((*standard-output* s))
	(pprint-logical-block
	 (nil (list (keim~name cstr)
		    (cstr~arguments cstr)
		    (cstr~variables cstr))
	 :suffix ")" :prefix "(cstr-comp")
	 (loop (pprint-exit-if-list-exhausted)
	       (write-char #\space)
	       (write (pprint-pop))))))))

(pp~modify-style pds-post
   (cstr+constraint
    (lambda (s cstr)
      (let ((*standard-output* s))
	(write "(cstr ")
	(write (keim~name cstr))
	(write "(")
	(mapc #'(lambda (arg) (pds~post-print arg s))  (cstr~arguments cstr))
	(write ")")
	(pprint-logical-block
	 (nil (list (cstr~variables cstr)
		    (if (cstr~simple-p cstr) (cstr~polarity cstr) ""))
	 :suffix ")")
	 (loop (pprint-exit-if-list-exhausted)
	       (write-char #\space)
	       (write (pprint-pop))))))))


;;; Some shortcuts:
(defmethod cstr~bound-variable ((bind cstr+binding))
  (first (cstr~arguments bind)))

(defmethod cstr~binding-term ((bind cstr+binding))
  (second (cstr~arguments bind)))

(defmethod cstr~binding-variables ((bind cstr+binding))
  (remove (cstr~bound-variable bind) (cstr~variables bind)))


;;; Print functions
(defmethod print-object ((cstr cstr+simple) stream)
  (format stream "~A~A(~{~A, ~}~A)"
	  (if (cstr~polarity cstr) '+ '-)
	  (keim~name cstr)
	  (butlast (cstr~arguments cstr))
	  (first (last (cstr~arguments cstr)))))

(defmethod print-object ((cstr cstr+binding) stream)
  (format stream "~A<-~A"
	  (cstr~bound-variable cstr)
	  (cstr~binding-term cstr)))

(defmethod print-object ((cstr cstr+composition) stream)
  (format stream "~A[~{~A, ~}~A]"
	  (keim~name cstr)
	  (butlast (cstr~arguments cstr))
	  (first (last (cstr~arguments cstr)))))


;;; Creation and type functions
(defun cstr~binding-create (args &optional vars)
  (let ((the-vars (if vars vars
		    (cons (first args)
			  (cstr~free-variables (second args))))))
    (make-instance 'cstr+binding
		   :name :bind
		   :vars the-vars
		   :args args)
    ))

(defun cstr~simple-create (name args vars &optional (polarity T))
  (make-instance 'cstr+simple
		 :name name
		 :vars vars
		 :args args
		 :polarity polarity))

(defun cstr~composition-create (name args vars)
  (make-instance 'cstr+composition
		 :name name
		 :vars vars
		 :args args))

(defun cstr~create (name args vars &optional (polarity T))
  (cond ((or (string-equal name :and)
	     (string-equal name :or)
	     (string-equal name :if))
	 (cstr~composition-create name args vars))
	((string-equal name :bind)
	 (if polarity
	     (cstr~binding-create args vars)
	   (cstr~simple-create :unify args vars NIL)))
	(T
	 (cstr~simple-create name args vars polarity))))

(defun cstr~constraint-p (object)
  (typep object 'cstr+constraint))

(defun cstr~simple-p (object)
  (typep object 'cstr+simple))

(defun cstr~binding-p (object)
  (typep object 'cstr+binding))

(defun cstr~composition-p (object)
  (typep object 'cstr+composition))


;;; Help functions
(defgeneric cstr~free-variables (object)
  (declare (edited  "23-MAR-1998")
           (authors Lassaad)
           (input   "An object, which is an argument of a constraint.")
           (effect  "None.")
           (value   "The free variables in OBJECT, i.e. the variables that"
                    "are concerned by the constraint."))
  (:method ((obj-list list))
           (when obj-list
             (union (cstr~free-variables (first obj-list))
                    (cstr~free-variables (rest obj-list)))
             ))
  (:method ((cstr cstr+constraint))
           (cstr~variables cstr))
  (:method ((var term+variable))
           (list var))
  (:method ((term term+term))
           (data~free-variables term))
  (:method ((object T))
           )
  )

(defgeneric cstr=apply-binding (bvar bterm object)
  (declare (edited  "05-FEB-1999")
	   (authors Lassaad)
	   (input   "A meta-variable, a term, and an object.")
	   (effect  "None.")
	   (value   "The object after replacing BVAR with BTERM."))
  (:method (bvar bterm (term term+term))
	   (beta~normalize (data~replace-free-variables term (list bvar) (list bterm))))
  (:method (bvar bterm (cstr cstr+constraint))
	   (if (find bvar (cstr~variables cstr))
	       (cstr~create (keim~name cstr)
			    (mapcar #'(lambda (arg) (cstr=apply-binding bvar bterm arg))
				    (cstr~arguments cstr))
			    (append (data~free-variables bterm) (remove bvar (cstr~variables cstr)))
			    (if (cstr~simple-p cstr) (cstr~polarity cstr) T))
	     cstr))
  )


(defgeneric cstr=all-free-variables (object &optional not-in)
  ;;; 2 values:
  ;; metavars helpvars
  (:method ((obj-list list) &optional not-in)
           (when obj-list
	     (multiple-value-bind (mvars1 hvars1)
		 (cstr=all-free-variables (first obj-list) not-in)
	       (multiple-value-bind (mvars hvars)
		   (cstr=all-free-variables (rest obj-list) not-in)
		 (values (union mvars1 mvars) (union hvars1 hvars))))))
;  (:method ((var hou+nullvariable) &optional not-in)
;           (unless (find var not-in)
;             (values NIL NIL (list var))))
  (:method ((var keim::gb+helpvariable) &optional not-in)
	   (unless (find var not-in)
	     (values NIL (list var))))
  (:method ((var meta+variable) &optional not-in)
	   (unless (find var not-in)
	     (values (list var))))
  (:method ((appl data+appl) &optional not-in)
	   (multiple-value-bind (mvars1 hvars1)
	       (cstr=all-free-variables (data~appl-function appl) not-in)
	     (multiple-value-bind (mvars hvars)
		 (cstr=all-free-variables (data~appl-arguments appl) not-in)
	       (values (union mvars1 mvars) (union hvars1 hvars)))))
  (:method ((abstr data+abstr) &optional not-in)
	   (cstr=all-free-variables (data~abstr-range abstr)
				    (union not-in (data~abstr-domain abstr))))
  (:method ((object T) &optional not-in)
	   (declare (ignore not-in))
           )
  )

(defgeneric cstr~adapt-hou-result (object &optional metavars)
  ;;; Adapts a HOU-solution by:
  ;; - making the substitution idempotent
  ;; - taking the bindings for the elements of METAVARS and rejecting the
  ;; remaining bindings
  ;; - substituting free help variables in the resulted bindings and in the
  ;; flex-flex pairs by meta-variables
  (:method ((nix null) &optional metavars)
	   (declare (ignore metavars))
	   nix)
  (:method ((hous cons) &optional metavars)
	   (if metavars
	       (cons (cstr~adapt-hou-result (first hous) metavars)
		     (cstr~adapt-hou-result (rest hous) metavars))
	     (cons (cstr~adapt-hou-result (first hous))
		   (cstr~adapt-hou-result (rest hous)))))
  (:method ((hou uni+hou) &optional (metavars (remove-if-not #'meta~p (subst~domain (uni~substitution hou)))))
	   (let ((idem-subst (subst~idem (uni~substitution hou)))
		 (flex-flexs (uni~flex-flex hou)))
	     (if flex-flexs
		 ;;; There is free help variables in the IDEM-SUBST bindings of METAVARS 
		 (let ((mapping (mapp~create nil nil))
		       new-flex-flexs)
		   (dolist (ff flex-flexs)
		     (multiple-value-bind (mvars1 hvars1)
			 (cstr=all-free-variables (first ff))
		       (declare (ignore mvars1))
		       (multiple-value-bind (mvars2 hvars2)
			   (cstr=all-free-variables (second ff))
			 (declare (ignore mvars2))
			 (if (or hvars1 hvars2)
			     (progn
			       (dolist (hvar (append hvars1 hvars2))
				 (unless (mapp~get-component hvar mapping)
				   (mapp~insert-component! hvar                                   
							   (term~generate-term-primitive-with-new-name
							    :mvh (term~type hvar) 'meta+variable
							    (pds~environment omega*current-proof-plan))
							   mapping)))
			       (multiple-value-bind (new-f1)
				   (if hvars1
				       (data~replace-free-variables (first ff) (mapp~domain mapping) (mapp~codomain mapping))
				     (first ff))
				 (multiple-value-bind (new-f2)
				     (if hvars2
					 (data~replace-free-variables (second ff) (mapp~domain mapping) (mapp~codomain mapping))
				       (second ff))
				   (setq new-flex-flexs (append new-flex-flexs (list (list new-f1 new-f2)))))))
			   (setq new-flex-flexs (append new-flex-flexs (list ff)))))))
		   ;;; After accumulating the bindings of help free variables in FLEX-FLEX to new meta-variables in MAPPING,
		   ;; now apply this MAPPING to the bindings of METAVARS
		   (let (new-dom new-codom)
		     (dolist (mvar metavars)
		       (let ((assoc-term (subst~get-component mvar idem-subst)))
			 (when assoc-term
			   (push mvar new-dom)
			   (push (multiple-value-bind (new-t)
				     (data~replace-free-variables assoc-term (mapp~domain mapping) (mapp~codomain mapping))
				   (cstr~beta-normalize new-t))
				 new-codom))))
		     (setf (uni~substitution hou) (subst~create new-dom new-codom)
			   (uni~flex-flex hou) new-flex-flexs)))
	       ;;; There is no free help variables in the IDEM-SUBST bindings of METAVARS:
	       (let (new-dom new-codom)
		 (dolist (mvar metavars)
		   (let ((assoc-term (subst~get-component mvar idem-subst)))
		     (when assoc-term 
		       (push mvar new-dom)
		       (push (cstr~beta-normalize assoc-term) new-codom))))
		 (setf (uni~substitution hou) (subst~create new-dom new-codom))))
	     hou))
  (:method ((subst subst+substitution) &optional (metavars (remove-if-not #'meta~p (subst~domain subst))))
	   (let ((idem-subst (subst~idem subst))
		 new-dom new-codom)
	     (dolist (mvar metavars)
	       (let ((assoc-term (subst~get-component mvar idem-subst)))
		 (when assoc-term 
		   (push mvar new-dom)
		   (push (cstr~beta-normalize assoc-term) new-codom))))
	     (subst~create new-dom new-codom)))
  (:method (obj &optional metavars)
	   (declare (ignore metavars))
	   (omega~error ";;;cstr~~freevars2metavars not defined for ~A" (type-of obj)))
  )

(defgeneric cstr~beta-normalize (datum)
  ;;; creates the beta-normalform of datum and eliminates
  ;; the eta-normalforms inside:
  (:method ((term term+variable))
	   (if (bind~binding term)
	       (cstr~beta-normalize (bind~binding term))
	     term))
  (:method ((term term+constant))
           term)
  (:method ((term term+appl))
	   (let* ((hnf-copi (data~alpha-copy term nil)) 
		  (hnf (beta~head-normalize hnf-copi)))    
	     (if (data~appl-p hnf)
		 (term~appl-create (cstr~beta-normalize (data~appl-function hnf))
				   (mapcar #'cstr~beta-normalize (data~appl-arguments hnf)))
	       (cstr~beta-normalize hnf))))
  (:method ((term term+abstr))
	   (let* ((binder (data~abstr-domain term))
		  (scope (data~abstr-range term))
		  (new-scope (cstr~beta-normalize scope)))
	     (if (data~appl-p new-scope)
		 (let* ((args-of-scope (data~appl-arguments new-scope))
			(head-of-scope (data~appl-function new-scope))
			(possible1 (<= (length binder) (length args-of-scope)))
			(vars-counterargs (when possible1
					    (subseq args-of-scope
						    (- (length args-of-scope) (length binder)))))
			(rest-args (when possible1
				     (subseq args-of-scope
					     0 (- (length args-of-scope) (length binder)))))
			(possible2 (when possible1
				     (every #'(lambda (x y) (keim~equal x y))
					    binder
					    vars-counterargs)))
			(possible3 (when possible2
				     (null (intersection
					    binder
					    (apply #'append
						   (mapcar #'data~free-variables
							   (cons head-of-scope rest-args)))
					    :test #'term~alpha-equal)))))
		   (if possible3
		       (if rest-args
			   (term~appl-create head-of-scope rest-args)
			 head-of-scope)
		     (term~abstr-create binder new-scope)))
	       (term~abstr-create binder new-scope))))
  )
	   

(defun cstr~conjunction-p (object)
  (and (cstr~composition-p object) (string-equal (keim~name object) :and)))

(defun cstr~conjunction (list)
  (unless (some #'null list)
    (if list
	(let (conjuncts
	      vars)
	  (dolist (elt list)
	    (cond ((cstr~conjunction-p elt)
		   (setq conjuncts (append conjuncts (cstr~arguments elt))
			 vars (union vars (cstr~variables elt))))
		  ((cstr~constraint-p elt)
		   (setq conjuncts (append conjuncts (list elt))
			 vars (union vars (cstr~variables elt))))))
	  (if conjuncts
	      (if (rest conjuncts) (cstr~composition-create :and conjuncts vars)
		(first conjuncts))
	    T))
      T)))

(defun cstr~disjunction-p (object)
  (and (cstr~composition-p object) (string-equal (keim~name object) :or)))

(defun cstr~disjunction (list)
  (when list
    (let (disjuncts
	  vars)
      (dolist (elt list)
	(when elt
	  (cond ((cstr~disjunction-p elt)
		 (setq disjuncts (append disjuncts (cstr~arguments elt))
		       vars (union vars (cstr~variables elt))))
		((cstr~constraint-p elt)
		 (setq disjuncts (append disjuncts (list elt))
		       vars (union vars (cstr~variables elt)))))))
      (when disjuncts
	(if (rest disjuncts) (cstr~composition-create :or disjuncts vars)
	  (first disjuncts))))))

(defgeneric cstr~negate (cstr)
  (:method ((bind cstr+binding))
	   (cstr~simple-create :unify
			       (cstr~arguments bind)
			       (cstr~variables bind)
			       NIL))
  (:method ((cstr cstr+simple))
	   (cond ((string-equal (keim~name cstr) :some)
		  (cstr~composition-create :and
					   (mapcar #'cstr~negate (cstr~arguments cstr))
					   (cstr~variables cstr)))
		 ((and (not (cstr~polarity cstr))
		       (string-equal (keim~name cstr) :unify)
		       (term~variable-p (first (cstr~arguments cstr))))
		  (cstr~binding-create (cstr~arguments cstr)
				       (cstr~variables cstr)))
		 (T
		  (cstr~simple-create (keim~name cstr)
				      (cstr~arguments cstr)
				      (cstr~variables cstr)
				      (not (cstr~polarity  cstr))))))
  (:method ((cstr cstr+composition))
	   (cond ((string-equal (keim~name cstr) :and)
		  ;;; ~and(c1,c2) --> or(~c1,~c2)
		  (cstr~composition-create :or
					   (mapcar #'cstr~negate (cstr~arguments cstr))
					   (cstr~variables cstr)))
		 ((string-equal (keim~name cstr) :or)
		  ;;; ~or(c1,c2) --> and(~c1,~c2)
		  (cstr~composition-create :and
					   (mapcar #'cstr~negate (cstr~arguments cstr))
					   (cstr~variables cstr)))
		 (T
		  ;;; ~if(c1,c2,c3) = ~or(and(c1,c2),and(~c1,c3))
		  ;; --> and(~and(c1,c2),~and(~c1,c3))
		  ;; --> and(or(~c1,~c2),or(c1,~c3))
		  (cstr~composition-create :and
					   (list (cstr~disjunction (mapcar #'cstr~negate (butlast (cstr~arguments cstr))))
						 (cstr~disjunction (list (first (cstr~arguments cstr))
									 (cstr~negate (third (cstr~arguments cstr))))))
					   (cstr~variables cstr)))))
  (:method (otherwise)
	   (omega~error ";;;cstr~~negate not implemented for ~A" (type-of otherwise)))
  )

(defun cstr~then-else (cond then else)
  (cstr~composition-create :if
			   (list cond then else)
			   (cstr~free-variables (list cond then else))))
		  
	       
(defgeneric cstr~merge (new-cstr cstr-state)
  (declare (edited  "02-DEC-1998")
	   (authors Lassaad)
	   (input   "A new constraint which may not be empty, and a (current)"
		    "constraint pool, which can be NIL.")
	   (effect  "None.")
	   (value   "A constraint pool consisting of the constraint and the"
		    "bindings resulted of merging NEW-CSTR with the constraint"
		    "of CSTR-STATE, when both constraints are compatible, o/w NIL."))
  (:method ((new-cstr cstr+constraint) (nix null))
	   (labels ((cstr&bindings (cstrs)
				   (if cstrs
				       (let ((cstr1 (first cstrs)))
					 (if cstr1
					     (cond ((listp cstr1)
						    (cstr&bindings (append cstr1 (rest cstrs))))
						   ((cstr~binding-p cstr1)
						    (multiple-value-bind (cstr bindings)
							(cstr&bindings (rest cstrs))
						      (values cstr (cons cstr1 bindings))))
						   ((cstr~conjunction-p cstr1)
						    (cstr&bindings (append (cstr~arguments cstr1) (rest cstrs))))
						   (T 
						    (multiple-value-bind (cstr bindings)
							(cstr&bindings (rest cstrs))
						      (values (cstr~conjunction (list cstr1 cstr)) bindings))))
					   (cstr&bindings (rest cstrs))))
				     (values T)))
		    (flat-conjuncts (cstrs)
				    (when cstrs
				      (let ((cstr1 (first cstrs)))
					(if (cstr~conjunction-p cstr1)
					    (append (cstr~arguments cstr1)
						    (flat-conjuncts (rest cstrs)))
					  (cons cstr1 (flat-conjuncts (rest cstrs)))))))
		    )
	     (multiple-value-bind (cstr bindings)
		 (cstr&bindings (list new-cstr))
	       (if (and (cstr~constraint-p cstr) (some #'(lambda (x) (cstr=common-variable-p x cstr)) bindings))
		   (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		       (cstr=merge bindings cstr NIL NIL)
		     (if splitp
			 (multiple-value-bind (the-cstr the-bindings)
			     (cstr=pop-up-bindings (mapcar #'(lambda (S Ms M)
							       (flat-conjuncts
								(if M (cons M (if (cstr~constraint-p S)
										  (cons S Ms) Ms))
								  (if (cstr~constraint-p S)
								      (cons S Ms) Ms))))
							   Scstr Mcstrs Mcstr)
						   NIL NIL NIL)
			   (pds~cstrpool-create (subst~idem (subst~create (mapcar #'cstr~bound-variable the-bindings)
									  (mapcar #'cstr~binding-term the-bindings)))
						(when (cstr~constraint-p the-cstr) the-cstr) nix))
		       (when Scstr
			 (multiple-value-bind (the-cstr the-bindings)
			     (cstr&bindings (list Scstr Mcstr Mcstrs))
			   (pds~cstrpool-create (subst~idem (subst~create (mapcar #'cstr~bound-variable the-bindings)
									  (mapcar #'cstr~binding-term the-bindings)))
						(when (cstr~constraint-p the-cstr) the-cstr) nix)))))
		 (pds~cstrpool-create (subst~idem (subst~create (mapcar #'cstr~bound-variable bindings)
								(mapcar #'cstr~binding-term bindings)))
				      (when (cstr~constraint-p cstr) cstr) nix)))))
  (:method ((new-cstr cstr+constraint) (cstr-state pds+constraint-pool))
	   (labels ((cstr&bindings (cstrs)
				   (if cstrs
				       (let ((cstr1 (first cstrs)))
					 (if cstr1
					     (cond ((listp cstr1)
						    (cstr&bindings (append cstr1 (rest cstrs))))
						   ((cstr~binding-p cstr1)
						    (multiple-value-bind (cstr bindings)
							(cstr&bindings (rest cstrs))
						      (values cstr (cons cstr1 bindings))))
						   ((cstr~conjunction-p cstr1)
						    (cstr&bindings (append (cstr~arguments cstr1) (rest cstrs))))
						   (T 
						    (multiple-value-bind (cstr bindings)
							(cstr&bindings (rest cstrs))
						      (values (cstr~conjunction (list cstr1 cstr)) bindings))))
					   (cstr&bindings (rest cstrs))))
				     (values T)))
		    (flat-conjuncts (cstrs)
				    (when cstrs
				      (let ((cstr1 (first cstrs)))
					(if (cstr~conjunction-p cstr1)
					    (append (cstr~arguments cstr1)
						    (flat-conjuncts (rest cstrs)))
					  (cons cstr1 (flat-conjuncts (rest cstrs)))))))
		    )
	     ;;; We have to merge NEW-CSTR with each other:
	     (multiple-value-bind (cstr bindings)
		 (cstr&bindings (list new-cstr))
	       (let (merge-cstrs merge-splitp the-bindings fail)
		 ;;; Merging the BINDINGS with each other:
		 (when bindings
		   (multiple-value-bind (cstrs splitp)
		       (cstr=merge-list (list (first bindings)) (rest bindings))
		     (if cstrs
			 (setq the-bindings cstrs
			       merge-splitp splitp)
		       (setq fail T))))
		 ;;; Determine MERGE-CSTRs to be merged with the constraint of CSTR-STATE:
		 (cond ((and (not fail) (cstr~constraint-p cstr) the-bindings)
		        ;;; Merging THE-BINDINGS with CSTR:
			(if merge-splitp
			    (dolist (one-bindings the-bindings)
			      (if (cstr=common-variable-p one-bindings cstr)
				  (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
				      (cstr=merge one-bindings cstr NIL NIL)
				    (if splitp
					(setq merge-cstrs
					      (append merge-cstrs
						      (mapcar #'(lambda (S Ms M)
								  (remove-if-not #'cstr~constraint-p
										 (cons S (append Ms (list M)))))
							      Scstr Mcstrs Mcstr)))
				      (when Scstr
					(push (remove-if-not #'cstr~constraint-p
							     (cons Scstr (append Mcstrs (list Mcstr))))
					      merge-cstrs))))
				(push (cons cstr one-bindings) merge-cstrs)))
			  (if (cstr=common-variable-p the-bindings cstr)
			      (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
				  (cstr=merge the-bindings cstr NIL NIL)
				(if splitp
				    (setq merge-cstrs
					  (mapcar #'(lambda (S Ms M)
						      (remove-if-not #'cstr~constraint-p
								     (cons S (append Ms (list M)))))
						  Scstr Mcstrs Mcstr))
				  (when Scstr
				    (push (remove-if-not #'cstr~constraint-p
							 (cons Scstr (append Mcstrs (list Mcstr))))
					  merge-cstrs))))
			    (push (cons cstr the-bindings) merge-cstrs))))
		       ((and (not fail) (cstr~constraint-p cstr) (not the-bindings))
			(push cstr merge-cstrs))
		       ((and (not fail) the-bindings)
			(push the-bindings merge-cstrs))
		       (T
			(setq fail t)))
		 ;;; Preparing MERGE-CSTRS and MERGE-SPLITP
		 (when (and (not fail) merge-cstrs)
		   (if (rest merge-cstrs)
		       (setq merge-splitp T)
		     (setq merge-cstrs (first merge-cstrs)
			   merge-splitp NIL))
		   ;;; Merging MERGE-CSTRS with the constraint of CSTR-STATE
		   (if merge-splitp
		       ;;; NEW-CSTR is splitted to many alternatives:
		       (cond ((pds~cstrpool-constraint cstr-state)
			      ;;; CSTR-STATE had constraints:
			      (let (the-results)
				(dolist (to-merge merge-cstrs)
				  (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
				      (if (cstr=common-variable-p to-merge (pds~cstrpool-constraint cstr-state))
					  (cstr=merge to-merge (pds~cstrpool-constraint cstr-state) NIL NIL)
					(values (if (listp to-merge) (cstr~conjunction to-merge) to-merge)
						NIL (pds~cstrpool-constraint cstr-state)))
				    (if splitp
					(setq the-results
					      (append the-results
						      (mapcar #'(lambda (S Ms M)
								  (if M (cons M (if (cstr~constraint-p S)
										    (cons S Ms) Ms))
								    (if (cstr~constraint-p S) (cons S Ms) Ms)))
							      Scstr Mcstrs Mcstr)))
				      (when Scstr
					(push (if Mcstr (cons Mcstr (if (cstr~constraint-p Scstr)
									(cons Scstr Mcstrs) Mcstrs))
						(if (cstr~constraint-p Scstr) (cons Scstr Mcstrs) Mcstrs))
					      the-results)))))
				(when the-results
				  (if (rest the-results)
				      (multiple-value-bind (cstr bindings)
					  (cstr=pop-up-bindings (mapcar #'flat-conjuncts the-results)
								NIL NIL NIL)
					(pds~cstrpool-create (subst~idem (subst~create (mapcar #'cstr~bound-variable bindings)
										       (mapcar #'cstr~binding-term bindings)))
							     (when (cstr~constraint-p cstr) cstr) cstr-state))
				    (multiple-value-bind (cstr bindings)
					(cstr&bindings (first the-results))
				      (pds~cstrpool-create (subst~idem (subst~create (mapcar #'cstr~bound-variable bindings)
										     (mapcar #'cstr~binding-term bindings)))
							   (when (cstr~constraint-p cstr) cstr) cstr-state))))
				))
			     (T
			      ;;; CSTR-STATE had no constraints:
			      (multiple-value-bind (cstr bindings)
				  (cstr=pop-up-bindings (mapcar #'flat-conjuncts merge-cstrs)
							NIL NIL NIL)
				(pds~cstrpool-create (subst~idem (subst~create (mapcar #'cstr~bound-variable bindings)
									       (mapcar #'cstr~binding-term bindings)))
						     (when (cstr~constraint-p cstr) cstr) cstr-state))))
		     ;;; NEW-CSTR is not splitted to many alternatives:
		     (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
			 (if (and (pds~cstrpool-constraint cstr-state)
				  (cstr=common-variable-p merge-cstrs (pds~cstrpool-constraint cstr-state)))
			     (cstr=merge merge-cstrs (pds~cstrpool-constraint cstr-state) NIL NIL)
			   (Values (if (listp merge-cstrs) (cstr~conjunction merge-cstrs) merge-cstrs)
				   NIL (pds~cstrpool-constraint cstr-state)))
		       (if splitp
			   (multiple-value-bind (cstr bindings)
			       (cstr=pop-up-bindings (mapcar #'(lambda (S Ms M) (flat-conjuncts
										 (if M (cons M (if (cstr~constraint-p S)
												   (cons S Ms) Ms))
										   (if (cstr~constraint-p S) (cons S Ms) Ms))))
							     Scstr Mcstrs Mcstr)
						     NIL NIL NIL)
			     (pds~cstrpool-create (subst~idem (subst~create (mapcar #'cstr~bound-variable bindings)
									    (mapcar #'cstr~binding-term bindings)))
						  (when (cstr~constraint-p cstr) cstr) cstr-state))
			 (when Scstr
			   (multiple-value-bind (cstr bindings)
			       (cstr&bindings (list Scstr Mcstrs Mcstr))
			     (pds~cstrpool-create (subst~idem (subst~create (mapcar #'cstr~bound-variable bindings)
									    (mapcar #'cstr~binding-term bindings)))
						  (when (cstr~constraint-p cstr) cstr) cstr-state))))))
		   )))))
  (:method ((new-cstr cstr+constraint) (old-cstr cstr+constraint))
	   (labels ((flat-conjuncts (cstrs)
				    (when cstrs
				      (let ((cstr1 (first cstrs)))
					(if (cstr~conjunction-p cstr1)
					    (append (cstr~arguments cstr1)
						    (flat-conjuncts (rest cstrs)))
					  (cons cstr1 (flat-conjuncts (rest cstrs)))))))
		    )
	     (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		 (if (cstr=common-variable-p new-cstr old-cstr)
		     (cstr=merge new-cstr old-cstr nil nil)
		   (values old-cstr NIL new-cstr))
	       (if splitp
		   (multiple-value-bind (the-cstr the-bindings)
		       (cstr=pop-up-bindings (mapcar #'(lambda (S Ms M) (flat-conjuncts
									 (if M (cons M (if (cstr~constraint-p S)
											   (cons S Ms) Ms))
									   (if (cstr~constraint-p S) (cons S Ms) Ms))))
						     Scstr Mcstrs Mcstr)
					     NIL NIL NIL)
		     (cstr~conjunction (if (cstr~constraint-p the-cstr) (append the-bindings (list the-cstr))
					 the-bindings)))
		 (when Scstr
		   (cstr~conjunction (append (when Mcstr (list Mcstr)) (cons Scstr Mcstrs))))))))
  (:method ((new-cstr cstr+constraint) (old-cstr T))
	   new-cstr)
  (:method ((new-cstr T) (old-cstr cstr+constraint))
	   old-cstr)
  (:method ((new-cstr T) (old-cstr T))
	   T)
  )

(defgeneric cstr=common-variable-p (cstr1 cstr2)
  ;;; First CSTR should be merged with the second CSTR and the second 
  ;; CSTR should be possibly merged with other CSTRS compatible with
  ;; the first CSTR.
  ;;; Returns T, when the second CSTR is a binding or when both constraints
  ;; have common variables:
  (:method ((bind1 cstr+binding) (bind2 cstr+binding))
	   T)
  (:method ((bind cstr+binding) (cstr cstr+constraint))
	   (find (cstr~bound-variable bind) (cstr~variables cstr)))
  (:method ((cstr cstr+constraint) (bind cstr+binding))
	   T)
  (:method ((cstr1 cstr+constraint) (cstr2 cstr+constraint))
	   (find-if #'(lambda (var) (find var (cstr~variables cstr2)))
		    (cstr~variables cstr1)))
  (:method ((cstr cstr+constraint) (cstrs cons))
	   (let ((cvar (cstr=common-variable-p cstr (first cstrs))))
	     (if cvar cvar
	       (when (rest cstrs)
		     (cstr=common-variable-p cstr (rest cstrs))))))
  (:method ((cstrs cons) (cstr cstr+constraint))
	   (let ((cvar (cstr=common-variable-p (first cstrs) cstr)))
	     (if cvar cvar
	       (when (rest cstrs)
		     (cstr=common-variable-p (rest cstrs) cstr)))))
  (:method ((cstrs1 cons) (cstrs2 cons))
	   (let ((cvar (cstr=common-variable-p (first cstrs1) cstrs2)))
	     (if cvar cvar
	       (when (rest cstrs1)
		     (cstr=common-variable-p (rest cstrs1) cstrs2)))))
  )


(defgeneric cstr=merge (merge-cstr old-cstr sat-cstrs merge-cstrs)
  ;;; Returns:
  ;; <Scstr, Bcstrs, Mcstr, NIL> when the merging of the given constraints is successfull
  ;;                             and delivers the constraints Scstr, Mcstr and the constraint
  ;;                             list Bcstrs
  ;; <Scstrs, Bcstrss, Mcstrs, T> when the merging of the given constraints is successfull
  ;;                              and delivers different alternatives given in the lists Scstrs,
  ;;                              Mcstrs and the list of constraint lists Bcstrss
  ;; NIL when the given constraints cannot be merged (are incompatible).
  ;;; MERGE-CSTR, OLD-CSTR, SAT-CSTRS, and MERGE-CSTRS are themselves satisfiable
  ;; This function checks whether the conjunction of these arguments is satisfiable:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Decomposition of the MERGE-CSTR to simple constraints
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (:method ((merge-cstr cstr+composition) (old-cstr cstr+composition) sat-cstrs merge-cstrs)
	   ;;; MERGE-CSTR and OLD-CSTR must have common variables:
	   (cond ((string-equal (keim~name merge-cstr) :and)
		  (cond ((string-equal (keim~name old-cstr) :and)
			 (cstr=merge (cstr~arguments merge-cstr) (cstr~arguments old-cstr) sat-cstrs merge-cstrs))
			(T ;;; or/if
			 (cstr=merge (cstr~arguments merge-cstr) old-cstr sat-cstrs merge-cstrs))))
		 ((string-equal (keim~name merge-cstr) :or)
		  (cond ((string-equal (keim~name old-cstr) :and)
			 (cstr=merge merge-cstr (cstr~arguments old-cstr) sat-cstrs merge-cstrs))
			(T ;;; or/if
			 (cstr=merge-disjuncts (cstr~arguments merge-cstr) old-cstr sat-cstrs merge-cstrs))))
		 (T ;;;MERGE-CSTR is an if-constraint, i.e., if(c1,c2,c3) where c2 and c3 can be T:
		  (cond ((string-equal (keim~name old-cstr) :and)
			 (cstr=merge merge-cstr (cstr~arguments old-cstr) sat-cstrs merge-cstrs))
			(T ;;; or/if
			 (cstr=merge-if-cstr (cstr~arguments merge-cstr) old-cstr sat-cstrs merge-cstrs))))))
  (:method ((merge-cstr cstr+composition) (old-cstr cstr+constraint) sat-cstrs merge-cstrs)
	   ;;; MERGE-CSTR and OLD-CSTR must have common variables:
	   (cond ((string-equal (keim~name merge-cstr) :and)
		  (cstr=merge (cstr~arguments merge-cstr) old-cstr sat-cstrs merge-cstrs))
		 ((string-equal (keim~name merge-cstr) :or)
		  (cstr=merge-disjuncts (cstr~arguments merge-cstr) old-cstr sat-cstrs merge-cstrs))
		 (T ;;;MERGE-CSTR is an if-constraint, i.e., if(c1,c2,c3) where c2 and c3 can be T:
		  (cstr=merge-if-cstr (cstr~arguments merge-cstr) old-cstr sat-cstrs merge-cstrs))))
  (:method ((merge-cstr cstr+constraint) (old-cstr cstr+composition) sat-cstrs merge-cstrs)
	   ;;; MERGE-CSTR and OLD-CSTR must have common variables:
	   (cond ((string-equal (keim~name old-cstr) :and)
		  (cstr=merge merge-cstr (cstr~arguments old-cstr) sat-cstrs merge-cstrs))
		 ((string-equal (keim~name old-cstr) :or)
		  (cstr=merge-disjuncts merge-cstr (cstr~arguments old-cstr) sat-cstrs merge-cstrs))
		 (T ;;;OLD-CSTR is an if-constraint, i.e., if(c1,c2,c3) where c2 and c3 can be T:
		  (cstr=merge-with-if-cstr merge-cstr (cstr~arguments old-cstr) sat-cstrs merge-cstrs))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Merging the MERGE-CSTRS one by one:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (:method ((merge-cstrs1 cons) (old-cstrs cons) sat-cstrs merge-cstrs2)
	   (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1)
	       ;;; Consider Sat-constraints and Merge-constraints in the recursive calls, because
	       ;; MERGE-CSTRS1 are not yet considered with SAT-CSTRS and MERGE-CSTRS2
	       (cstr=merge (first merge-cstrs1) old-cstrs sat-cstrs merge-cstrs2)
	     (when Scstr1
	       (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 (rest merge-cstrs1) NIL NIL NIL))))
  (:method ((merge-cstrs1 cons) (old-cstr cstr+composition) sat-cstrs merge-cstrs2)
	   (let ((merge-cstr (first merge-cstrs1)))
	     (if (cstr=common-variable-p merge-cstr old-cstr)
		 ;;; Consider Sat-constraints and Merge-constraints in the recursive calls, because
		 ;; MERGE-CSTRS1 are not yet considered with SAT-CSTRS and MERGE-CSTRS2
		 (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1)
		     (cstr=merge merge-cstr old-cstr sat-cstrs merge-cstrs2)
		   (when Scstr1
		     (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 (rest merge-cstrs1) NIL NIL NIL)))
	       (let ((rest-merge-cstrs1 (rest merge-cstrs1)))
		 (if rest-merge-cstrs1
		     (multiple-value-bind (RScstr RMcstrs RMcstr splitp)
			 (cstr=merge rest-merge-cstrs1 old-cstr sat-cstrs merge-cstrs2)
		       (if splitp
			   (values RScstr RMcstrs
				   (mapcar #'(lambda (M) (if M (cstr~conjunction (list merge-cstr M)) merge-cstr))
					   RMcstr) splitp)
			 (when RScstr
			   (values RScstr RMcstrs (if RMcstr (cstr~conjunction (list merge-cstr RMcstr))
						    merge-cstr)))))
		   (if merge-cstrs2
		       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
			   (cstr=merge merge-cstrs2 (append sat-cstrs (list old-cstr)) NIL NIL)
			 (if splitp
			     (let (the-Scstrs
				   the-Mcstrss
				   the-Mcstrs
				   (rest-Scstr Scstr)
				   (rest-Mcstrs Mcstrs))
			       (dolist (M Mcstr)
				 (setq the-Scstrs (append the-Scstrs
							  (list (if M (cstr~conjunction (list (first rest-Scstr) M))
								  (first rest-Scstr))))
				       the-Mcstrss (append the-Mcstrss (list (first rest-Mcstrs))))
				 (push merge-cstr the-Mcstrs)
				 (setq rest-Scstr (rest rest-Scstr)
				       rest-Mcstrs (rest rest-Mcstrs)))
			       (values the-Scstrs the-Mcstrss the-Mcstrs T))
			   (when Scstr
			     (values (if Mcstr (cstr~conjunction (list Scstr Mcstr)) Scstr) Mcstrs merge-cstr))))
		     (values (cstr~conjunction (append sat-cstrs (list old-cstr))) NIL merge-cstr)))))))
  (:method ((merge-cstrs1 cons) (old-cstr cstr+constraint) sat-cstrs merge-cstrs2)
	   (let ((merge-cstr (first merge-cstrs1)))
	     (if (cstr=common-variable-p merge-cstr old-cstr)
		 (cond ((cstr~composition-p merge-cstr)
			;;; Consider Sat-constraints and Merge-constraints in the recursive calls, because
			;; MERGE-CSTR is a composition:
			(multiple-value-bind (Scstr1 Mcstrs1 Mcstr1)
			    (cstr=merge merge-cstr old-cstr sat-cstrs merge-cstrs2)
			  (when Scstr1
			    (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 (rest merge-cstrs1) NIL NIL NIL))))
		       (T ;;; First without sat-cstrs and merge-cstrs
			(multiple-value-bind (Scstr1 Mcstrs1 Mcstr1)
			    (cstr=merge merge-cstr old-cstr NIL NIL)
			  (when Scstr1
			    (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 (rest merge-cstrs1) NIL sat-cstrs merge-cstrs2))))
		       )
	       (let ((rest-merge-cstrs1 (rest merge-cstrs1)))
		 (if rest-merge-cstrs1
		     (multiple-value-bind (RScstr RMcstrs RMcstr splitp)
			 (cstr=merge rest-merge-cstrs1 old-cstr sat-cstrs merge-cstrs2)
		       (if splitp
			   (values RScstr RMcstrs
				   (mapcar #'(lambda (M) (if M (cstr~conjunction (list merge-cstr M)) merge-cstr))
					   RMcstr) splitp)
			 (when RScstr
			   (values RScstr RMcstrs
				   (if RMcstr (cstr~conjunction (list merge-cstr RMcstr))
				     merge-cstr)))))
		   (if merge-cstrs2
		       (multiple-value-bind (Scstr Mcstrs Mcstr)
			   (cstr=merge merge-cstrs2 (append sat-cstrs (list old-cstr)) NIL NIL)
			 (when Scstr
			   (values (if Mcstr (cstr~conjunction (list Scstr Mcstr)) Scstr) Mcstrs merge-cstr)))
		     (values (cstr~conjunction (append sat-cstrs (list old-cstr))) NIL merge-cstr)))))))
  (:method ((merge-cstr cstr+composition) (old-cstrs cons) sat-cstrs merge-cstrs)
	   (if (cstr=common-variable-p merge-cstr (first old-cstrs))
	       ;;; Consider Sat-constraints and Merge-constraints in the recursive call, because
	       ;; MERGE-CSTR is a composition:
	       (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp)
		   (cstr=merge merge-cstr (first old-cstrs) sat-cstrs merge-cstrs)
		 (if (rest old-cstrs)
		     (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 NIL (rest old-cstrs) NIL NIL)
		   (values Scstr1 Mcstrs1 Mcstr1 splitp)))
	     (if (rest old-cstrs)
		 (cstr=merge merge-cstr (rest old-cstrs) (append sat-cstrs (list (first old-cstrs))) merge-cstrs)
	       (if merge-cstrs
		   (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		       (cstr=merge merge-cstrs (append sat-cstrs (list (first old-cstrs))) NIL NIL)
		     (if splitp
			 (let (the-Scstrs
			       (rest-Scstr Scstr))
			   (dolist (M Mcstr)
			     (setq the-Scstrs (append the-Scstrs
						      (list (cstr~conjunction (cons (first rest-Scstr) (when M (list M))))))
				   rest-Scstr (rest rest-Scstr)))
			   (values the-Scstrs Mcstrs (make-sequence 'list (length Scstr) :initial-element merge-cstr) splitp))
		       (when Scstr
			 (values (cstr~conjunction (cons Scstr (when Mcstr (list Mcstr)))) Mcstrs merge-cstr))))
		 (values (if sat-cstrs (cstr~conjunction (append sat-cstrs (list (first old-cstrs))))
			   (first old-cstrs))
			 merge-cstrs merge-cstr)))))
  (:method ((merge-cstr cstr+constraint) (old-cstrs cons) sat-cstrs merge-cstrs)
	   (if (cstr=common-variable-p merge-cstr (first old-cstrs))
	       (cond ((cstr~composition-p (first old-cstrs))
		      ;;; Consider Sat-constraints and Merge-constraints in the recursive call, because
		      ;; OLD-CSTR1 is a composition:
		      (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp)
			  (cstr=merge merge-cstr (first old-cstrs) sat-cstrs merge-cstrs)
			(if (rest old-cstrs)
			    (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 NIL (rest old-cstrs) NIL NIL)
			  (values Scstr1 Mcstrs1 Mcstr1 splitp))))
		     (T ;;;Otherwise
		      (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp)
			  (cstr=merge merge-cstr (first old-cstrs) NIL NIL)
			(if (or (rest old-cstrs) sat-cstrs merge-cstrs)
			    (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 NIL (rest old-cstrs) sat-cstrs merge-cstrs)
			  (values Scstr1 Mcstrs1 Mcstr1 splitp))))
		     )
	     (if (rest old-cstrs)
		 (cstr=merge merge-cstr (rest old-cstrs) (append sat-cstrs (list (first old-cstrs))) merge-cstrs)
	       (if merge-cstrs
		   (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		       (cstr=merge merge-cstrs (append sat-cstrs (list (first old-cstrs))) NIL NIL)
		     (if splitp
			 (let (the-Scstrs
			       (rest-Scstr Scstr))
			   (dolist (M Mcstr)
			     (setq the-Scstrs (append the-Scstrs
						      (list (cstr~conjunction (cons (first rest-Scstr) (when M (list M))))))
				   rest-Scstr (rest rest-Scstr)))
			   (values the-Scstrs Mcstrs (make-sequence 'list (length Scstr) :initial-element merge-cstr) splitp))
		       (values (cstr~conjunction (cons Scstr (when Mcstr (list Mcstr)))) Mcstrs merge-cstr)))
		 (values (if sat-cstrs (cstr~conjunction (append sat-cstrs (list (first old-cstrs))))
			   (first old-cstrs))
			 merge-cstrs merge-cstr)))))
  (:method ((bind1 cstr+binding) (bind2 cstr+binding) (sat-cstrs null) (merge-cstrs null))
	   (cond ((find (cstr~bound-variable bind1) (cstr~binding-variables bind2))
		  ;;; (X1<-t1,X2<-t2[X1])
		  (unless (find (cstr~bound-variable bind2) (cstr~binding-variables bind1))
		    ;;; X2 does not occur in t1
		    (let ((new-bind2 (cstr~binding-create
				      (list 
				       (cstr~bound-variable bind2)
				       (cstr=apply-binding (cstr~bound-variable bind1)
							   (cstr~binding-term bind1)
							   (cstr~binding-term bind2)))
				      (union (remove (cstr~bound-variable bind1)
						     (cstr~variables bind2))
					     (cstr~binding-variables bind1)))))
		      (values T (list new-bind2) bind1))))
		 ((find (cstr~bound-variable bind2) (cstr~binding-variables bind1))
		  ;;; (X1<-t1[X2],X2<-t2) and X1 does not occur in t2
		  (values T (list bind2) (cstr~binding-create
					  (list 
					   (cstr~bound-variable bind1)
					   (cstr=apply-binding (cstr~bound-variable bind2)
							       (cstr~binding-term bind2)
							       (cstr~binding-term bind1)))
					  (union (remove (cstr~bound-variable bind2)
							 (cstr~variables bind1))
						 (cstr~binding-variables bind2)))))
		 ((keim~equal (cstr~bound-variable bind1) (cstr~bound-variable bind2))
		  (multiple-value-bind (uni-cstr bindings splitp)
		      (cstr~sat-unify (cstr~binding-term bind1) (cstr~binding-term bind2) T
				      (union (cstr~binding-variables bind1) (cstr~binding-variables bind2)))
		    (if splitp
			(values (make-sequence 'list (length bindings) :initial-element T)
				(mapcar #'(lambda (Bs uC)
					    (append Bs (when (cstr~constraint-p uC) (list uC))))
					bindings uni-cstr)
				(make-sequence 'list (length bindings) :initial-element bind1)
				splitp)
		      (when uni-cstr
			(values T (append bindings (when (cstr~constraint-p uni-cstr) (list uni-cstr))) bind1)))))
		 (T ;;;BIND1 and BIND2 have no common variables
		  (values T (list bind2) bind1))))
  (:method ((bind cstr+binding) (cstr cstr+simple) (sat-cstrs null) (merge-cstrs null))
	   (multiple-value-bind (new-cstr new-bindings splitp)
	      (cstr~satisfiable-p (keim~name cstr)
				  (mapcar #'(lambda (arg)
					      (cstr=apply-binding (cstr~bound-variable bind)
								  (cstr~binding-term bind)
								  arg))
					  (cstr~arguments cstr))
				  (cstr~polarity cstr))
	     (if splitp
		 (values new-cstr new-bindings (make-sequence 'list (length new-bindings) :initial-element bind) splitp)
	       (values new-cstr new-bindings bind))))
  (:method ((cstr cstr+simple) (bind cstr+binding) (sat-cstrs null) (merge-cstrs null))
	   (if (find (cstr~bound-variable bind) (cstr~variables cstr))
	       (multiple-value-bind (new-cstr new-bindings splitp)
		   (cstr~satisfiable-p (keim~name cstr)
				       (mapcar #'(lambda (arg)
						   (cstr=apply-binding (cstr~bound-variable bind)
								       (cstr~binding-term bind)
								       arg))
					       (cstr~arguments cstr))
				       (cstr~polarity cstr))
		 (when new-cstr
		   (if splitp
		       (values (make-sequence 'list (length new-cstr) :initial-element T)
			       (mapcar #'(lambda (bindings) (cons bind bindings)) new-bindings)
			       (mapcar #'(lambda (cstr) (when (cstr~constraint-p cstr) cstr)) new-cstr)
			       T)
		     (values T (cons bind new-bindings) (when (cstr~constraint-p new-cstr) new-cstr)))))
	     (values T (list bind) cstr)))
  (:method ((merge-cstr cstr+simple) (old-cstr cstr+simple) (sat-cstrs null) (merge-cstrs null))
	   (cond ((and (not (string-equal (keim~name merge-cstr) :some))
		       (string-equal (keim~name merge-cstr) (keim~name old-cstr)))
		  ;;; same name, different to :SOME
		  (cond ((and (subsetp (cstr~variables merge-cstr) (cstr~variables old-cstr))
			      (subsetp (cstr~variables old-cstr) (cstr~variables merge-cstr)))
			 ;;; same variables
			 (labels ((samep (args1 args2)
					 (if args1
					     (and (keim~equal (first args1) (first args2))
						  (samep (rest args1) (rest args2)))
					   T)))
			   (cond ((samep (cstr~arguments merge-cstr) (cstr~arguments old-cstr))
				  ;;; same arguments
				  (when (eq (cstr~polarity merge-cstr) (cstr~polarity old-cstr))
				    (values old-cstr)))
				 (T ;;; different arguments
				  (values old-cstr NIL merge-cstr)))))
			(T ;;; different variables
			 (values old-cstr NIL merge-cstr))))
		 (T ;;; different name
		  (values old-cstr NIL merge-cstr))))
  )


(defgeneric cstr=merge&split (RScstrs RMcstrss RMcstrs rest-merge-cstrs1 rest-cstrs sat-cstrs merge-cstrs)
  ;;; RScstrs, RMcstrss, and RMcstrs result of merging some constraint with other constraints
  ;; rest-merge-cstrs1 rest-cstrs sat-cstrs merge-cstrs: Not yet merged constraints:
  (:method ((RScstrs cons) (RMcstrss cons) (RMcstrs cons) rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
	   (let (res-Scstrs
		 res-Mcstrss
		 res-Mcstrs
		 (the-RMcstrss RMcstrss)
		 (the-RMcstrs RMcstrs))
	     (dolist (RScstr RScstrs)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (cstr=merge&split RScstr (first the-RMcstrss) (first the-RMcstrs)
				     rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
		 (if splitp
		     (setq res-Scstrs (append Scstr res-Scstrs)
			   res-Mcstrss (append Mcstrs res-Mcstrss)
			   res-Mcstrs (append Mcstr res-Mcstrs))
		   (when Scstr
		     (push Scstr res-Scstrs)
		     (push Mcstrs res-Mcstrss)
		     (push Mcstr res-Mcstrs)))
		 (setq the-RMcstrss (rest the-RMcstrss)
		       the-RMcstrs (rest the-RMcstrs))))
	     (if (rest res-Scstrs)
		 (values res-Scstrs res-Mcstrss res-Mcstrs T)
	       (values (first res-Scstrs) (first res-Mcstrss) (first res-Mcstrs)))))
  (:method ((RScstr null) RMcstrs RMcstr  rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
	   ;;; Result of calling cstr=merge was NIL
	   (declare (ignore RMcstrs RMcstr  rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs))
	   nil)
  (:method ((RScstr cstr+constraint) (RMcstrs null) (RMcstr null) rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
	   ;;; Result of calling cstr=merge was <C1 NIL NIL>
	   (cond ((and rest-merge-cstrs1 old-cstrs)
		  (cstr=merge rest-merge-cstrs1 old-cstrs (append sat-cstrs (list RScstr)) merge-cstrs))
		 ((and rest-merge-cstrs1 merge-cstrs)
		  ;;; old-cstrs is NIL
		  (multiple-value-bind (new-merge-cstrs splitp)
		      (cstr=merge-list merge-cstrs rest-merge-cstrs1)
		    (if splitp
			(let (the-Scstrs 
			      the-Mcstrss 
			      the-Mcstrs
			      (Ocstrs (append sat-cstrs (list RScstr))))
			  (dolist (new-merges new-merge-cstrs)
			    (multiple-value-bind (Scstr Mcstrs Mcstr split)
				(cstr=merge new-merges Ocstrs nil nil)
			      (if split
				  (setq the-Scstrs (append the-Scstrs Scstr)
					the-Mcstrss (append the-Mcstrss Mcstrs)
					the-Mcstrs (append the-Mcstrs Mcstr))
				(when Scstr
				  (setq the-Scstrs (append the-Scstrs (list Scstr))
					the-Mcstrss (append the-Mcstrss (list Mcstrs))
					the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
			  (if (rest the-Scstrs)
			      (values the-Scstrs the-Mcstrss the-Mcstrs T)
			    (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs))))
		      (when new-merge-cstrs
			(cstr=merge new-merge-cstrs (append sat-cstrs (list RScstr)) nil nil)))))
		 (rest-merge-cstrs1
		  ;;; old-cstrs is NIL and merge-cstrs is NIL
		  (cstr=merge rest-merge-cstrs1 (append sat-cstrs (list RScstr)) nil nil))
		 (merge-cstrs
		  ;;; rest-merge-cstrs1 is NIL
		  (cstr=merge merge-cstrs (append old-cstrs sat-cstrs (list RScstr)) nil nil))
		 (T 
		  ;;; rest-merge-cstrs1 is NIL and merge-cstrs is NIL
		  (values (cstr~conjunction (append old-cstrs sat-cstrs (list RScstr)))))))
  (:method ((RScstr cstr+constraint) (RMcstrs cons) RMcstr rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
	   ;;; Result of calling cstr=merge was <C1 (B1,..,Bn) C2|NIL>
	   (cond (merge-cstrs
		  (multiple-value-bind (new-merge-cstrs splitp)
		      (cstr=merge-list merge-cstrs RMcstrs)
		    (if splitp
			(let (the-Scstrs the-Mcstrss the-Mcstrs)
			  (dolist (new-merges new-merge-cstrs)
			    (multiple-value-bind (Scstr Mcstrs Mcstr split)
				(cstr=merge&split RScstr NIL RMcstr rest-merge-cstrs1 old-cstrs sat-cstrs new-merges)
			      (if split
				  (setq the-Scstrs (append the-Scstrs Scstr)
					the-Mcstrss (append the-Mcstrss Mcstrs)
					the-Mcstrs (append the-Mcstrs Mcstr))
				(when Scstr
				  (setq the-Scstrs (append the-Scstrs (list Scstr))
					the-Mcstrss (append the-Mcstrss (list Mcstrs))
					the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
			  (when the-Scstrs
			    (if (rest the-Scstrs)
				(values the-Scstrs the-Mcstrss the-Mcstrs T)
			      (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs)))))
		      (when new-merge-cstrs
			(cstr=merge&split RScstr NIL RMcstr rest-merge-cstrs1 
					  old-cstrs sat-cstrs new-merge-cstrs)))))
		 (T
		  ;;; merge-cstrs is NIL
		  (cstr=merge&split RScstr NIL RMcstr rest-merge-cstrs1 old-cstrs sat-cstrs RMcstrs))))
  (:method ((RScstr cstr+constraint) (RMcstrs null) (RMcstr cstr+constraint) rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
	   ;;; Result of calling cstr=merge was <C1 NIL C2>
	   (cond (old-cstrs
		  (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp1)
		      (cstr=merge RMcstr old-cstrs (append sat-cstrs (list RScstr)) merge-cstrs)
		    (if splitp1
			(let (the-Scstrs 
			      the-Mcstrss 
			      the-Mcstrs
			      (aScstr Scstr1)
			      (aMcstr Mcstr1))
			  (dolist (Mcs Mcstrs1)
			    (multiple-value-bind (Scstrs Mcstrss Mcstrs split)
				(cstr=merge&split (first aScstr) Mcs (first aMcstr) rest-merge-cstrs1 NIL NIL NIL)
			      (if split
				  (setq the-Scstrs (append the-Scstrs Scstrs)
					the-Mcstrss (append the-Mcstrss Mcstrss)
					the-Mcstrs (append the-Mcstrs Mcstrs))
				(when Scstrs
				  (setq the-Scstrs (append the-Scstrs (list Scstrs))
					the-Mcstrss (append the-Mcstrss (list Mcstrss))
					the-Mcstrs (append the-Mcstrs (list Mcstrs)))))
			      (setq aScstr (rest aScstr) aMcstr (rest aMcstr))))
			  (when the-Scstrs
			    (if (rest the-Scstrs)
				(values the-Scstrs the-Mcstrss the-Mcstrs T)
			      (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs)))))
		      (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 rest-merge-cstrs1 NIL NIL NIL))))
		 (T 
		  ;;; old-cstrs is NIL 
		  (multiple-value-bind (the-Scstrs the-Mcstrss the-Mcstrs splitp)
		      (cstr=merge&split RScstr NIL NIL rest-merge-cstrs1 NIL sat-cstrs merge-cstrs)
;		    (if splitp
;                        (values the-Scstrs the-Mcstrss
;                                (mapcar #'(lambda (Mcstr) 
;                                            (if Mcstr (cstr~conjunction (list Mcstr RMcstr))
;                                              RMcstr))
;                                        the-Mcstrs) 
;                                splitp)
;                      (values the-Scstrs the-Mcstrss (if the-Mcstrs (cstr~conjunction (list the-Mcstrs RMcstr))
;                                                       RMcstr)))))))
		    (if splitp
			(values (mapcar #'(lambda (Scstr Mcstr)
					    (if Mcstr (cstr~conjunction (list Scstr Mcstr))
					      Scstr))
					the-Scstrs the-Mcstrs)
				the-Mcstrss
				(make-sequence 'list (length the-Mcstrss) :initial-element RMcstr))
		      (values (if the-Mcstrs (cstr~conjunction (list the-Scstrs the-Mcstrs))
				the-Scstrs)
			      the-Mcstrss RMcstr))))))
  (:method ((RScstr T) (RMcstrs null) (RMcstr null) rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
	   ;;; Result of calling cstr=merge was <T NIL NIL>
	   (cond ((and rest-merge-cstrs1 old-cstrs)
		  (cstr=merge rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs))
		 ((and rest-merge-cstrs1 merge-cstrs)
		  ;;; old-cstrs is NIL
		  (multiple-value-bind (new-merge-cstrs splitp)
		      (cstr=merge-list merge-cstrs rest-merge-cstrs1)
		     ;;; TO-DO anpassen von cstr=merge-list ;;; WEITER
		    (cond (sat-cstrs
			   (if splitp
			       (let (the-Scstrs 
				     the-Mcstrss 
				     the-Mcstrs)
				 (dolist (new-merges new-merge-cstrs)
				   (multiple-value-bind (Scstr Mcstrs Mcstr split)
				       (cstr=merge new-merges sat-cstrs nil nil)
				     (if split
					 (setq the-Scstrs (append the-Scstrs Scstr)
					       the-Mcstrss (append the-Mcstrss Mcstrs)
					       the-Mcstrs (append the-Mcstrs Mcstr))
				       (when Scstr
					 (setq the-Scstrs (append the-Scstrs (list Scstr))
					       the-Mcstrss (append the-Mcstrss (list Mcstrs))
					       the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
				 (when the-Scstrs
				   (if (rest the-Scstrs)
				       (values the-Scstrs the-Mcstrss the-Mcstrs T)
				     (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs)))))
			     (when new-merge-cstrs
			       (cstr=merge new-merge-cstrs sat-cstrs nil nil))))
			  (T 
			   (if splitp 
			       (values (mapcar #'cstr~conjunction new-merge-cstrs) NIL NIL T)
			     (values (cstr~conjunction new-merge-cstrs)))))))
		 (rest-merge-cstrs1
		  ;;; old-cstrs is NIL and merge-cstrs is NIL
		  (cond (sat-cstrs
			 (cstr=merge rest-merge-cstrs1 sat-cstrs nil nil))
			(T 
			 (values (cstr~conjunction rest-merge-cstrs1)))))
		 (merge-cstrs
		  ;;; rest-merge-cstrs1 is NIL
		  (cond ((or old-cstrs sat-cstrs)
			 (cstr=merge merge-cstrs (append old-cstrs sat-cstrs) nil nil))
			(T 
			 (values (cstr~conjunction merge-cstrs)))))
		 (T 
		  ;;; rest-merge-cstrs1 is NIL and merge-cstrs is NIL
		  (values (cstr~conjunction (append old-cstrs sat-cstrs))))))
  (:method ((RScstr T) (RMcstrs cons) RMcstr rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
	   ;;; Result of calling cstr=merge was <T (B1,..,Bn) C2|NIL>
	   (cond (merge-cstrs
		  (multiple-value-bind (new-merge-cstrs splitp)
		      (cstr=merge-list merge-cstrs RMcstrs)
		    (if splitp
			(let (the-Scstrs the-Mcstrss the-Mcstrs)
			  (dolist (new-merges new-merge-cstrs)
			    (multiple-value-bind (Scstr Mcstrs Mcstr split)
				(cstr=merge&split RScstr NIL RMcstr rest-merge-cstrs1 old-cstrs sat-cstrs new-merges)
			      (if split
				  (setq the-Scstrs (append the-Scstrs Scstr)
					the-Mcstrss (append the-Mcstrss Mcstrs)
					the-Mcstrs (append the-Mcstrs Mcstr))
				(when Scstr
				  (setq the-Scstrs (append the-Scstrs (list Scstr))
					the-Mcstrss (append the-Mcstrss (list Mcstrs))
					the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
			  (when the-Scstrs
			    (if (rest the-Scstrs)
				(values the-Scstrs the-Mcstrss the-Mcstrs T)
			      (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs)))))
		      (when new-merge-cstrs
			(cstr=merge&split RScstr NIL RMcstr rest-merge-cstrs1 
					  old-cstrs sat-cstrs new-merge-cstrs)))))
		 (T
		  ;;; merge-cstrs is NIL
		  (cstr=merge&split RScstr NIL RMcstr rest-merge-cstrs1 old-cstrs sat-cstrs RMcstrs))))
  (:method ((RScstr T) (RMcstrs null) (RMcstr cstr+constraint) rest-merge-cstrs1 old-cstrs sat-cstrs merge-cstrs)
	   ;;; Result of calling cstr=merge was <T NIL C2>
	   (cond (old-cstrs
		  (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp1)
		      (cstr=merge RMcstr old-cstrs sat-cstrs merge-cstrs)
		    (if splitp1
			(let (the-Scstrs 
			      the-Mcstrss 
			      the-Mcstrs
			      (aScstr Scstr1)
			      (aMcstr Mcstr1))
			  (dolist (Mcs Mcstrs1)
			    (multiple-value-bind (Scstrs Mcstrss Mcstrs split)
				(cstr=merge&split (first aScstr) Mcs (first aMcstr) rest-merge-cstrs1 NIL NIL NIL)
			      (if split
				  (setq the-Scstrs (append the-Scstrs Scstrs)
					the-Mcstrss (append the-Mcstrss Mcstrss)
					the-Mcstrs (append the-Mcstrs Mcstrs))
				(when Scstrs
				  (setq the-Scstrs (append the-Scstrs (list Scstrs))
					the-Mcstrss (append the-Mcstrss (list Mcstrss))
					the-Mcstrs (append the-Mcstrs (list Mcstrs)))))
			      (setq aScstr (rest aScstr) aMcstr (rest aMcstr))))
			  (when the-Scstrs
			    (if (rest the-Scstrs)
				(values the-Scstrs the-Mcstrss the-Mcstrs T)
			      (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs)))))
		      (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 rest-merge-cstrs1 NIL NIL NIL))))
		 (T 
		  ;;; old-cstrs is NIL 
		  (multiple-value-bind (the-Scstrs the-Mcstrss the-Mcstrs splitp)
		      (cstr=merge&split RScstr NIL NIL rest-merge-cstrs1 NIL sat-cstrs merge-cstrs)
		    (if splitp
			(values the-Scstrs the-Mcstrss
				(mapcar #'(lambda (Mcstr) 
					    (if Mcstr (cstr~conjunction (list Mcstr RMcstr))
					      RMcstr))
					the-Mcstrs) 
				splitp)
		      (values the-Scstrs the-Mcstrss (if the-Mcstrs (cstr~conjunction (list the-Mcstrs RMcstr))
						       RMcstr)))))))
  )


  
  


(defun cstr=merge-if-cstr (x y z u)
  (omega~error ";;;cstr=merge-if-cstr is not yet implemented."))

(defun cstr=merge-with-if-cstr (merge-cstr cond-then-else sat-cstrs merge-cstrs)
  ;;; provisorisch
;  (if (cstr=common-variable-p merge-cstr (first cond-then-else))
;      (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1)
;          (cstr=merge merge-cstr (first cond-then-else) NIL NIL)
;        (cond (Scstr1
  (cstr=merge-disjuncts merge-cstr (list (remove-if-not #'cstr~constraint-p (butlast cond-then-else))
					 (append (list (cstr~negate (first cond-then-else)))
						 (when (cstr~constraint-p (third cond-then-else))
						   (last cond-then-else))))
			sat-cstrs merge-cstrs))
  
  

(defgeneric cstr=merge-list (list1 list2)
  ;;; LIST1 is consistent
  ;; merges LIST2 into LIST1
  (:method ((nix1 null) (nix2 null))
	   )
  (:method ((nix null) (cstrs cons))
	   (cstr=merge-list (list (first cstrs)) (rest cstrs)))
  (:method ((nix null) (cstr cstr+constraint))
	   (list cstr))
  (:method ((list1 cons) (nix null))
	   list1)
  (:method ((cstrs1 cons) (cstrs2 cons))
	   (if (cstr=common-variable-p (first cstrs2) cstrs1)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (cstr=merge (first cstrs2) cstrs1 NIL NIL)
		 (if splitp
		     (let (res-lists
			   (the-Mcstrs Mcstrs)
			   (the-Mcstr Mcstr))
		       (dolist (S Scstr)
			 (multiple-value-bind (result split)
			     (cstr=merge-list (remove-if-not #'cstr~constraint-p
							     (append (first the-Mcstrs) (list (first Mcstr) S)))
					      (rest cstrs2))
			   (if split
			       (setq res-lists (append res-lists result))
			     (when result
			       (setq res-lists (append res-lists (list result)))))
			   (setq the-Mcstrs (rest the-Mcstrs)
				 the-Mcstr (rest the-Mcstr))))
		       (when res-lists
			 (if (rest res-lists) (values res-lists T) (values (first res-lists)))))
		   (when Scstr
		     (cstr=merge-list (remove-if-not #'cstr~constraint-p (append Mcstrs (list Mcstr Scstr)))
				      (rest cstrs2)))))
	     (cstr=merge-list (append cstrs1 (list (first cstrs2))) (rest cstrs2))))
  (:method ((cstrs1 cons) (cstr cstr+constraint))
	   (if (cstr=common-variable-p cstr cstrs1)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (cstr=merge cstr cstrs1 NIL NIL)
		 (if splitp
		     (values (mapcar #'(lambda (S Ms M)
					 (remove-if-not #'cstr~constraint-p (append Ms (list M S))))
				     Scstr Mcstrs Mcstr)
			     T)
		   (when Scstr
		     (values (remove-if-not #'cstr~constraint-p (append Mcstrs (list Mcstr Scstr)))))))
	     (values (append cstrs1 (list cstr)))))
  )


;(defun cstr~pop-up-bindings (rest-paths done-paths wrt-bind pop-up)
;  ;;; The elements of REST-PATHS and of DONE-PATHS are lists of the form ((B0 .. Bn) C1 .. Cm)
;  ;; where n may be 0. WRT-BIND is a binding and POP-UP is a key stating whether it is possible 
;  ;; to pop-up WRT-BIND.
;  ;;; This function returns a constraint and a list of bindings:
;                                        ;(:method ((nix1 null) done-paths 
;  (labels ((remove-wrt-binding (paths)
;                               (mapcar #'(lambda (path) 
;                                           (if (rest (first path)) (append (rest (first path))
;                                                                           (rest path))
;                                             (rest path)))
;                                       paths))
;           (samevar-binding-p (bind cstr)
;                              ;;; BIND is a binding X <- t and CSTR is a constraint
;                              ;; This function returns CSTR, when this is a binding of
;                              ;; the form X <- t', i.e., of the same variable X, and otherwise
;                              ;; it returns NIL
;                              (when (and (cstr~binding-p cstr)
;                                         (keim~equal (cstr~bound-variable bind) (cstr~bound-variable cstr)))
;                                cstr))
;           (same-bindings-p (bind1 bind2)
;                            ;;; BIND1 is a binding X <- t and BIND2 is a binding of
;                            ;; the form X <- t', i.e., of the same variable X. This  function
;                            ;; returns T, when t1 keim~equal t2, and otherwise it returns NIL
;                            (and (subsetp (cstr~binding-variables bind1) (cstr~binding-variables bind2))
;                                 (subsetp (cstr~binding-variables bind2) (cstr~binding-variables bind1))
;                                 (term~alpha-equal (cstr~binding-term bind1) (cstr~binding-term bind2))))
;           (find-binding (bind path)
;                         ;;; BIND is a binding of the form X<-t, and PATH is a list of the form
;                         ;; ((B0 .. Bn) C1 .. Cm) where n might be 0
;                         ;;; Returns:
;                         ;; - Ci, when there is a Ci of the form X<-t, o/w NIL
;                         ;; - Cj, when there is a Cj of the form X<-t', o/w NIL.
;                         ;; Ci and Cj may not be both different from NIL.
;                         ;; - ((Ck B0 .. Bn) C1 .. Ck-1 Ck+1 .. Cm), when Ck is returned either
;                         ;; as first return or as second return, otherwise PATH
;                         (let ((samevar-bind (find-if #'(lambda (c) (samevar-binding-p bind c)) path)))
;                           (if samevar-bind
;                               (let ((rest-path (remove samevar-bind path)))
;                                 (if (same-bindings-p bind samevar-bind)
;                                     (values NIL bind
;                                             (if (listp (first rest-path))
;                                                 (cons (cons bind (first rest-path)) (rest rest-path))
;                                               (cons (list bind) rest-path)))
;                                   (values samevar-bind NIL
;                                           (if (listp (first rest-path))
;                                               (cons (cons samevar-bind (first rest-path)) (rest rest-path))
;                                             (cons (list samevar-bind) rest-path)))))
;                             (values NIL NIL path))))
;           (same-binding-p (bind1 bind2)
;                           (and (keim~equal (cstr~bound-variable bind1) (cstr~bound-variable bind2))
;                                (subsetp (cstr~binding-variables bind1) (cstr~binding-variables bind2))
;                                (subsetp (cstr~binding-variables bind2) (cstr~binding-variables bind1))
;                                (term~alpha-equal (cstr~binding-term bind1) (cstr~binding-term bind2))))
;           (covered-by1 (bindings paths)
;                        ;;; BINDINGS is of the form (B0 .. Bn) and PATHS is a list of paths having the form 
;                        ;; ((B0' .. Bl') C1' .. Ck'), where l' is different from 0
;                        ;;; Returns the first element in PATHS, for which (B0 .. Bn) is a sublist of (B1' .. Bl'):
;                        (find-if #'(lambda (path)
;                                     (let ((path-bindings (first path)))
;                                       (every #'(lambda (b1)
;                                                  (some #'(lambda (b2) (same-binding-p b1 b2))
;                                                        (rest path-bindings)))
;                                              bindings)))
;                                 paths))
;           (covered-by2 (binding bindings paths)
;                        ;;; BINDINGS is of the form (B0 .. Bn) and PATHS is a list of paths having the form 
;                        ;; ((B0' .. Bl') C1' .. Ck')
;                        ;;; Returns <((BINDING B0' .. Bl') C1' .. Cj-1' Cj+1' .. Ck') remaining PATHS> , 
;                        ;; iff BINDING corresponds to a Cj' and (B0 .. Bn) is a sublist of (B0' .. Bl') 
;                        ;; for some element of PATHS
;                        (let (apath apath-bind)
;                          (dolist (path paths)
;                            (let ((path-bindings (when (consp (first path)) (first path))))
;                              (when (every #'(lambda (b1)
;                                               (some #'(lambda (b2) (same-binding-p b1 b2))
;                                                     path-bindings))
;                                           bindings)
;                                (let ((bind-cstr (find-if #'(lambda (c) (samevar-binding-p binding c))
;                                                          path)))
;                                  (when (and bind-cstr (same-bindings-p binding bind-cstr))
;                                    (setq apath path apath-bind bind-cstr)
;                                    (return))))))
;                          (if apath
;                              (let ((rest-apath (remove apath-bind apath)))
;                                (cond ((and (consp (first rest-apath)) (rest rest-apath))
;                                       ;;; REST-APATH is of the form ((B1 .. Bn) C1 .. Cm)
;                                       (values (cons (cons binding (first rest-apath)) (rest rest-apath))
;                                               (remove apath paths)))
;                                      ((and (consp (first rest-apath)) (null (rest rest-apath)))
;                                       ;;; REST-APATH is of the form ((B1 .. Bn))
;                                       (if (> (length (first rest-apath)) (length bindings))
;                                           (values (list (cons binding (first rest-apath)))
;                                                   (remove apath paths))
;                                         (covered-by2 binding bindings (remove apath paths))))
;                                      (rest-apath
;                                       ;;; REST-APATH is of the form (C1 .. Cm)
;                                       (values (cons (list binding) rest-apath)
;                                               (remove apath paths)))
;                                      ((null rest-apath)
;                                       (covered-by2 binding bindings (remove apath paths)))))
;                            (values NIL paths))))
;           (covered-by3 (bindings paths)
;                        ;;; BINDINGS is of the form (B0 .. Bn) and PATHS is a list of paths having the form 
;                        ;; ((B0' .. Bl') C1' .. Ck')
;                        ;;; Returns T, iff (B0 .. Bn) is a sublist of (B0' .. Bl') for some element of PATHS
;                        (find-if #'(lambda (path)
;                                     (let ((path-bindings (when (consp (first path)) (first path))))
;                                       (every #'(lambda (b1)
;                                                  (some #'(lambda (b2) (same-binding-p b1 b2))
;                                                        path-bindings))
;                                              bindings)))
;                                 paths))
;           (build-conjunctions (paths)
;                               (mapcar #'(lambda (path)
;                                           (cstr~conjunction (if (listp (first path)) 
;                                                                 (append (first path) (rest path))
;                                                               path)))
;                                       paths))
;           )
;    (cond
;     ((and wrt-bind pop-up (null rest-paths))
;         ;;; All paths are worked off and WRT-BIND can be popped up
;      (multiple-value-bind (recursive-cstr recursive-bindings)
;          (cstr~pop-up-bindings (remove-wrt-binding done-paths) NIL NIL NIL)
;        (values recursive-cstr (cons wrt-bind recursive-bindings))))
;     ((and wrt-bind pop-up rest-paths)
;         ;;; We have to work the paths in REST-PATHS wrt. WRT-BIND
;      (multiple-value-bind (same-bind same-var-bind result-path)
;          (find-binding wrt-bind (first rest-paths))
;        (cond ((and same-bind (rest result-path))
;               (cstr~pop-up-bindings (rest rest-paths) 
;                                     (append done-paths (list result-path))
;                                     wrt-bind pop-up))
;              ((and same-bind (null (rest result-path)))
;                  ;;; RESULT-PATH is ((WRT-BIND B0 .. Bn))
;               ;; Since POP-UP and SAME-BIND hold,  RESULT-PATH is covered by some 
;               ;; path ((B0' .. Bl') C1' .. Ck') in DONE-PATHS, iff (B0 .. Bn) is a
;               ;; sublist of (B1' .. Bl').
;               (if (covered-by1 (rest (first result-path)) done-paths)
;                   (cstr~pop-up-bindings (rest rest-paths) done-paths wrt-bind pop-up)
;                    ;;; RESULT-PATH is ((WRT-BIND B0 .. Bn))
;                 ;; RESULT-PATH is covered by some path ((B0' .. Bl') C1' .. Ck') in the rest 
;                 ;; of REST-PATHS, iff WRT-BIND corresponds to one Ci' and (B0 .. Bn) is a 
;                 ;; sublist of (B0' .. Bl').
;                 (multiple-value-bind (covering-path the-rest-paths)
;                     (covered-by2 wrt-bind (rest (first result-path)) (rest rest-paths))
;                   (if covering-path
;                       (cstr~pop-up-bindings the-rest-paths 
;                                             (append done-paths (list covering-path))
;                                             wrt-bind pop-up)
;                        ;;; No path in DONE-PATHS and in THE-REST-PATHS covers RESULT-PATH:
;                     ;; Since POP-UP and SAME-BIND hold, we have to check whether WRT-BIND
;                     ;; can be popped up in THE-REST-PATHS.
;                     (multiple-value-bind (Rcstr Rbindings)
;                         (if the-rest-paths
;                             (cstr~pop-up-bindings the-rest-paths NIL wrt-bind pop-up)
;                           (values T (list wrt-bind)))
;                       (cond ((find wrt-bind Rbindings)
;                              (values (cstr~disjunction 
;                                       (append (build-conjunctions (remove-wrt-binding done-paths))
;                                               (list (cstr~conjunction (rest (first result-path)))
;                                                     (cstr~conjunction (cons Rcstr 
;                                                                             (remove wrt-bind Rbindings))))))
;                                      (list wrt-bind)))
;                             (T
;                              (values 
;                               (cstr~disjunction
;                                (append (build-conjunctions (append done-paths result-path))
;                                        (list (cstr~conjunction (cons Rcstr Rbindings)))))))))))))
;              ((and same-var-bind (rest result-path))
;               (cstr~pop-up-bindings (rest rest-paths) 
;                                     (append done-paths (list result-path))
;                                     wrt-bind NIL))
;              ((and same-var-bind (null (rest result-path)))
;                  ;;; RESULT-PATH is ((WRT-BIND B0 .. Bn))
;               ;; Since POP-UP and SAME-VAR-BIND hold,  RESULT-PATH cannot be covered by some 
;               ;; path ((B0' .. Bl') C1' .. Ck') in DONE-PATHS. Only paths in the rest of 
;               ;; REST-PATHS can cover RESULT-PATH: 
;               (multiple-value-bind (covering-path the-rest-paths)
;                   (covered-by2 same-var-bind (rest (first result-path)) (rest rest-paths))
;                 (if covering-path
;                     (cstr~pop-up-bindings the-rest-paths 
;                                           (append done-paths (list covering-path))
;                                           wrt-bind NIL)
;                   (values (cstr~disjunction
;                            (build-conjunctions (append done-paths result-path the-rest-paths)))))))
;              ((and (null same-bind) (null same-var-bind))
;               (cstr~pop-up-bindings (rest rest-paths) 
;                                     (append done-paths (list result-path)) 
;                                     wrt-bind NIL)))))
;     ((and wrt-bind (null pop-up) (null rest-paths))
;      (cstr~pop-up-bindings done-paths NIL NIL NIL))
;     ((and wrt-bind (null pop-up) rest-paths)
;         ;;; We have to work the paths in REST-PATHS wrt. WRT-BIND
;      (multiple-value-bind (same-bind same-var-bind result-path)
;          (find-binding wrt-bind (first rest-paths))
;        (cond ((rest result-path)
;               (cstr~pop-up-bindings (rest rest-paths) 
;                                     (append done-paths (list result-path))
;                                     wrt-bind pop-up))
;              ((and (or same-bind same-var-bind) (null (rest result-path)))
;                  ;;; RESULT-PATH of the form ((B0 .. Bn)) is covered by some 
;               ;; path ((B0' .. Bl') C1' .. Ck') in DONE-PATHS, iff (B0 .. Bn) is a
;               ;; sublist of (B0' .. Bl').
;               (if (covered-by3 (first result-path) done-paths)
;                   (cstr~pop-up-bindings (rest rest-paths) done-paths wrt-bind pop-up)
;                    ;;; RESULT-PATH is ((WRT-BIND' B0 .. Bn))
;                 ;; RESULT-PATH is covered by some path ((B0' .. Bl') C1' .. Ck') in the rest 
;                 ;; of REST-PATHS, iff WRT-BIND' corresponds to one Ci' and (B0 .. Bn) is a 
;                 ;; sublist of (B0' .. Bl').
;                 (multiple-value-bind (covering-path the-rest-paths)
;                     (covered-by2 (or same-bind same-var-bind) (rest (first result-path)) (rest rest-paths))
;                   (if covering-path
;                       (cstr~pop-up-bindings the-rest-paths 
;                                             (append done-paths (list covering-path))
;                                             wrt-bind pop-up)
;                     (values (cstr~disjunction
;                              (build-conjunctions (append done-paths result-path the-rest-paths))))))))
;              ((and (null same-bind) (null same-var-bind))
;               (cstr~pop-up-bindings (rest rest-paths) 
;                                     (append done-paths (list result-path)) 
;                                     wrt-bind pop-up)))))
;     ((and (null wrt-bind) (null pop-up) (null rest-paths))
;      (values (cstr~disjunction (build-conjunctions done-paths))))
;     ((and (null wrt-bind) (null pop-up) rest-paths)
;         ;;; We have to determine some binding to be considered in the recursive call
;      (let* ((path1 (first rest-paths))
;             (new-bind (find-if #'cstr~binding-p path1)))
;        (cond ((null new-bind)
;               (cstr~pop-up-bindings (rest rest-paths) 
;                                     (append done-paths (list path1))
;                                     wrt-bind pop-up))
;              (new-bind
;               (let ((result-path (cons (if (listp (first path1))
;                                            (cons new-bind (first path1))
;                                          (list new-bind))
;                                        (remove new-bind path1))))
;                 (cond ((rest result-path) 
;                        (cstr~pop-up-bindings (rest rest-paths) 
;                                              (append done-paths (list result-path)) 
;                                              new-bind (not done-paths)))
;                       ((null (rest result-path))
;                           ;;; RESULT-PATH is ((NEW-BIND B0 .. Bn))
;                        ;; RESULT-PATH is covered by some path ((B0' .. Bl') C1' .. Ck') in the rest 
;                        ;; of REST-PATHS, iff NEW-BIND corresponds to one Ci' and (B0 .. Bn) is a 
;                        ;; sublist of (B0' .. Bl').
;                        (multiple-value-bind (covering-path the-rest-paths)
;                            (covered-by2 new-bind (rest (first result-path)) (rest rest-paths))
;                          (if covering-path
;                              (cstr~pop-up-bindings the-rest-paths 
;                                                    (append done-paths (list covering-path))
;                                                    new-bind (not done-paths))
;                            (values (cstr~disjunction
;                                     (build-conjunctions (append done-paths result-path the-rest-paths)))))))
;                       ))))))
;     )))

(defun cstr=help-remove-wrt-binding (paths)
  (mapcar #'(lambda (path) 
	      (if (rest (first path)) (cons (rest (first path))
					    (rest path))
		(rest path)))
	  paths))

(defun cstr=help-samevar-binding-p (bind cstr)
  ;;; BIND is a binding X <- t and CSTR is a constraint
  ;; This function returns CSTR, when this is a binding of
  ;; the form X <- t', i.e., of the same variable X, and otherwise
  ;; it returns NIL
  (when (and (cstr~binding-p cstr)
	     (keim~equal (cstr~bound-variable bind) (cstr~bound-variable cstr)))
    cstr))

(defun cstr=help-same-bindings-p (bind1 bind2)
  ;;; BIND1 is a binding X <- t and BIND2 is a binding of
  ;; the form X <- t', i.e., of the same variable X. This  function
  ;; returns T, when t1 keim~equal t2, and otherwise it returns NIL
  (and (subsetp (cstr~binding-variables bind1) (cstr~binding-variables bind2))
       (subsetp (cstr~binding-variables bind2) (cstr~binding-variables bind1))
       (term~alpha-equal (cstr~binding-term bind1) (cstr~binding-term bind2))
       ))

(defun cstr=help-find-binding (bind path)
  ;;; BIND is a binding of the form X<-t, and PATH is a list of the form
  ;; ((B0 .. Bn) C1 .. Cm) where n might be 0
  ;;; Returns:
  ;; - Ci, when there is a Ci of the form X<-t, o/w NIL
  ;; - Cj, when there is a Cj of the form X<-t', o/w NIL.
  ;; Ci and Cj may not be both different from NIL.
  ;; - ((Ck B0 .. Bn) C1 .. Ck-1 Ck+1 .. Cm), when Ck is returned either
  ;; as first return or as second return, otherwise PATH
  (let ((samevar-bind (find-if #'(lambda (c) (cstr=help-samevar-binding-p bind c)) path)))
    (if samevar-bind
	(let ((rest-path (remove samevar-bind path)))
	  (if (cstr=help-same-bindings-p bind samevar-bind)
	      (values bind NIL
		      (if (listp (first rest-path))
			  (cons (cons bind (first rest-path)) (rest rest-path))
			(cons (list bind) rest-path)))
	    (values NIL samevar-bind
		    (if (listp (first rest-path))
			(cons (cons samevar-bind (first rest-path)) (rest rest-path))
		      (cons (list samevar-bind) rest-path)))))
      (values NIL NIL path))
    ))

(defun cstr=help-same-binding-p (bind1 bind2)
  (and (keim~equal (cstr~bound-variable bind1) (cstr~bound-variable bind2))
       (subsetp (cstr~binding-variables bind1) (cstr~binding-variables bind2))
       (subsetp (cstr~binding-variables bind2) (cstr~binding-variables bind1))
       (term~alpha-equal (cstr~binding-term bind1) (cstr~binding-term bind2))
       ))

(defun cstr=help-covered-by1 (bindings paths)
  ;;; BINDINGS is of the form (B0 .. Bn) and PATHS is a list of paths having the form 
  ;; ((B0' .. Bl') C1' .. Ck'), where l' is different from 0
  ;;; Returns the first element in PATHS, for which (B0 .. Bn) is a sublist of (B1' .. Bl'):
  (find-if #'(lambda (path)
	       (let ((path-bindings (first path)))
		 (every #'(lambda (b1)
			    (some #'(lambda (b2) (cstr=help-same-binding-p b1 b2))
				  (rest path-bindings)))
			bindings)))
	   paths))

(defun cstr=help-covered-by2 (binding bindings paths)
  ;;; BINDINGS is of the form (B0 .. Bn) and PATHS is a list of paths having the form 
  ;; ((B0' .. Bl') C1' .. Ck')
  ;;; Returns <((BINDING B0' .. Bl') C1' .. Cj-1' Cj+1' .. Ck') remaining PATHS> , 
  ;; iff BINDING corresponds to a Cj' and (B0 .. Bn) is a sublist of (B0' .. Bl') 
  ;; for some element of PATHS
  (let (apath apath-bind)
    (dolist (path paths)
      (let ((path-bindings (when (consp (first path)) (first path))))
	(when (every #'(lambda (b1)
			 (some #'(lambda (b2) (cstr=help-same-binding-p b1 b2))
			       path-bindings))
		     bindings)
	  (let ((bind-cstr (find-if #'(lambda (c) (cstr=help-samevar-binding-p binding c))
				    path)))
	    (when (and bind-cstr (cstr=help-same-bindings-p binding bind-cstr))
	      (setq apath path apath-bind bind-cstr)
	      (return))))))
    (if apath
	(let ((rest-apath (remove apath-bind apath)))
	  (cond ((and (consp (first rest-apath)) (rest rest-apath))
		 ;;; REST-APATH is of the form ((B1 .. Bn) C1 .. Cm)
		 (values (cons (cons binding (first rest-apath)) (rest rest-apath))
			 (remove apath paths)))
		((and (consp (first rest-apath)) (null (rest rest-apath)))
		 ;;; REST-APATH is of the form ((B1 .. Bn))
		 (if (> (length (first rest-apath)) (length bindings))
		     (values (list (cons binding (first rest-apath)))
			     (remove apath paths))
		   (cstr=help-covered-by2 binding bindings (remove apath paths))))
		(rest-apath
		 ;;; REST-APATH is of the form (C1 .. Cm)
		 (values (cons (list binding) rest-apath)
			 (remove apath paths)))
		((null rest-apath)
		 (cstr=help-covered-by2 binding bindings (remove apath paths)))))
      (values NIL paths))
    ))

(defun cstr=help-covered-by3 (bindings paths)
  ;;; BINDINGS is of the form (B0 .. Bn) and PATHS is a list of paths having the form 
  ;; ((B0' .. Bl') C1' .. Ck')
  ;;; Returns T, iff (B0 .. Bn) is a sublist of (B0' .. Bl') for some element of PATHS
  (find-if #'(lambda (path)
	       (let ((path-bindings (when (consp (first path)) (first path))))
		 (every #'(lambda (b1)
			    (some #'(lambda (b2) (cstr=help-same-binding-p b1 b2))
				  path-bindings))
			bindings)))
	   paths))

(defun cstr=help-build-conjunctions (paths)
  (mapcar #'(lambda (path)
	      (cstr~conjunction (if (listp (first path)) 
				    (append (first path) (rest path))
				  path)))
	  paths))
	  

(defgeneric cstr=pop-up-bindings (rest-paths done-paths wrt-bind pop-up)
  ;;; The elements of REST-PATHS and of DONE-PATHS are lists of the form ((B0 .. Bn) C1 .. Cm)
  ;; where n may be 0. WRT-BIND is a binding and POP-UP is a key stating whether it is possible 
  ;; to pop-up WRT-BIND.
  ;;; This function returns a constraint and a list of bindings:
  (:method ((rest-paths null) (done-paths cons) (wrt-bind cstr+binding) (pop-up T))
	   ;;; All paths are worked off and WRT-BIND can be popped up
	   ;(format t "~%ARGS:~% ~A~% ~A~% ~A~% ~A" rest-paths done-paths wrt-bind pop-up)
	   (multiple-value-bind (recursive-cstr recursive-bindings)
	       (cstr=pop-up-bindings (cstr=help-remove-wrt-binding done-paths) NIL NIL NIL)
	     (values recursive-cstr (cons wrt-bind recursive-bindings))))
  (:method ((rest-paths cons) done-paths (wrt-bind cstr+binding) (pop-up T))
	   ;(format t "~%ARGS:~% ~A~% ~A~% ~A~% ~A" rest-paths done-paths wrt-bind pop-up)
	   ;;; We have to work the paths in REST-PATHS wrt. WRT-BIND
	   (multiple-value-bind (same-bind same-var-bind result-path)
	       (cstr=help-find-binding wrt-bind (first rest-paths))
	     (cond ((and same-bind (rest result-path))
		    (cstr=pop-up-bindings (rest rest-paths) 
					  (append done-paths (list result-path))
					  wrt-bind pop-up))
		   ((and same-bind (null (rest result-path)))
		    ;;; RESULT-PATH is ((WRT-BIND B0 .. Bn))
		    ;; Since POP-UP and SAME-BIND hold,  RESULT-PATH is covered by some 
		    ;; path ((B0' .. Bl') C1' .. Ck') in DONE-PATHS, iff (B0 .. Bn) is a
		    ;; sublist of (B1' .. Bl').
		    (if (cstr=help-covered-by1 (rest (first result-path)) done-paths)
			(cstr=pop-up-bindings (rest rest-paths) done-paths wrt-bind pop-up)
		      ;;; RESULT-PATH is ((WRT-BIND B0 .. Bn))
		      ;; RESULT-PATH is covered by some path ((B0' .. Bl') C1' .. Ck') in the rest 
		      ;; of REST-PATHS, iff WRT-BIND corresponds to one Ci' and (B0 .. Bn) is a 
		      ;; sublist of (B0' .. Bl').
		      (multiple-value-bind (covering-path the-rest-paths)
			  (cstr=help-covered-by2 wrt-bind (rest (first result-path)) (rest rest-paths))
			(if covering-path
			    (cstr=pop-up-bindings the-rest-paths 
						  (append done-paths (list covering-path))
						  wrt-bind pop-up)
			  ;;; No path in DONE-PATHS and in THE-REST-PATHS covers RESULT-PATH:
			  ;; Since POP-UP and SAME-BIND hold, we have to check whether WRT-BIND
			  ;; can be popped up in THE-REST-PATHS.
			  (multiple-value-bind (Rcstr Rbindings)
			      (if the-rest-paths
				  (cstr=pop-up-bindings the-rest-paths NIL wrt-bind pop-up)
				(values T (list wrt-bind)))
			    (cond ((find wrt-bind Rbindings)
				   (values (cstr~disjunction 
					    (append (cstr=help-build-conjunctions (cstr=help-remove-wrt-binding done-paths))
						    (list (cstr~conjunction (rest (first result-path)))
							  (cstr~conjunction (cons Rcstr 
										  (remove wrt-bind Rbindings))))))
					   (list wrt-bind)))
				  (T
				   (values 
				    (cstr~disjunction
				     (append (cstr=help-build-conjunctions (append done-paths result-path))
					     (list (cstr~conjunction (cons Rcstr Rbindings)))))))))))))
		   ((and same-var-bind (rest result-path))
		    (cstr=pop-up-bindings (rest rest-paths) 
					  (append done-paths (list result-path))
					  wrt-bind NIL))
		   ((and same-var-bind (null (rest result-path)))
		    ;;; RESULT-PATH is ((WRT-BIND B0 .. Bn))
		    ;; Since POP-UP and SAME-VAR-BIND hold,  RESULT-PATH cannot be covered by some 
		    ;; path ((B0' .. Bl') C1' .. Ck') in DONE-PATHS. Only paths in the rest of 
		    ;; REST-PATHS can cover RESULT-PATH: 
		    (multiple-value-bind (covering-path the-rest-paths)
			(cstr=help-covered-by2 same-var-bind (rest (first result-path)) (rest rest-paths))
		      (if covering-path
			  (cstr=pop-up-bindings the-rest-paths 
						(append done-paths (list covering-path))
						wrt-bind NIL)
			(values (cstr~disjunction
				 (cstr=help-build-conjunctions (append done-paths result-path the-rest-paths)))))))
		   ((and (null same-bind) (null same-var-bind))
		    (cstr=pop-up-bindings (rest rest-paths) 
					  (append done-paths (list result-path)) 
					  wrt-bind NIL)))))
  (:method ((rest-paths null) (done-paths cons) (wrt-bind cstr+binding) (pop-up null))
	   ;(format t "~%ARGS:~% ~A~% ~A~% ~A~% ~A" rest-paths done-paths wrt-bind pop-up)
	   (cstr=pop-up-bindings done-paths NIL NIL NIL))
  (:method ((rest-paths cons) done-paths (wrt-bind cstr+binding) (pop-up null))
	   ;(format t "~%ARGS:~% ~A~% ~A~% ~A~% ~A" rest-paths done-paths wrt-bind pop-up)
	   ;;; We have to work the paths in REST-PATHS wrt. WRT-BIND
	   (multiple-value-bind (same-bind same-var-bind result-path)
	       (cstr=help-find-binding wrt-bind (first rest-paths))
	     (cond ((rest result-path)
		    (cstr=pop-up-bindings (rest rest-paths) 
					  (append done-paths (list result-path))
					  wrt-bind pop-up))
		   ((and (or same-bind same-var-bind) (null (rest result-path)))
		    ;;; RESULT-PATH of the form ((B0 .. Bn)) is covered by some 
		    ;; path ((B0' .. Bl') C1' .. Ck') in DONE-PATHS, iff (B0 .. Bn) is a
		    ;; sublist of (B0' .. Bl').
		    (if (cstr=help-covered-by3 (first result-path) done-paths)
			(cstr=pop-up-bindings (rest rest-paths) done-paths wrt-bind pop-up)
		      ;;; RESULT-PATH is ((WRT-BIND' B0 .. Bn))
		      ;; RESULT-PATH is covered by some path ((B0' .. Bl') C1' .. Ck') in the rest 
		      ;; of REST-PATHS, iff WRT-BIND' corresponds to one Ci' and (B0 .. Bn) is a 
		      ;; sublist of (B0' .. Bl').
		      (multiple-value-bind (covering-path the-rest-paths)
			  (cstr=help-covered-by2 (or same-bind same-var-bind) (rest (first result-path)) (rest rest-paths))
			(if covering-path
			    (cstr=pop-up-bindings the-rest-paths 
						  (append done-paths (list covering-path))
						  wrt-bind pop-up)
			  (values (cstr~disjunction
				   (cstr=help-build-conjunctions (append done-paths result-path the-rest-paths))))))))
		   ((and (null same-bind) (null same-var-bind))
		    (cstr=pop-up-bindings (rest rest-paths) 
					  (append done-paths (list result-path)) 
					  wrt-bind pop-up)))))
  (:method ((rest-paths null) (done-paths cons) (wrt-bind null) (pop-up null))
	   ;(format t "~%ARGS:~% ~A~% ~A~% ~A~% ~A" rest-paths done-paths wrt-bind pop-up)
	   (values (cstr~disjunction (cstr=help-build-conjunctions done-paths))))
  (:method ((rest-paths cons) done-paths (wrt-bind null) (pop-up null))
	   ;;; We have to determine some binding to be considered in the recursive call
	   ;(format t "~%ARGS:~% ~A~% ~A~% ~A~% ~A" rest-paths done-paths wrt-bind pop-up)
	   (let* ((path1 (first rest-paths))
		  (new-bind (find-if #'cstr~binding-p path1)))
	     (cond ((null new-bind)
		    (cstr=pop-up-bindings (rest rest-paths) 
					  (append done-paths (list path1))
					  wrt-bind pop-up))
		   (new-bind
		    (let ((result-path (if (listp (first path1))
					   (cons (cons new-bind (first path1))
						 (remove new-bind (rest path1)))
					 (cons (list new-bind) (remove new-bind path1)))))
		      (cond ((rest result-path) 
			     (cstr=pop-up-bindings (rest rest-paths) 
						   (append done-paths (list result-path)) 
						   new-bind (not done-paths)))
			    ((null (rest result-path))
			     ;;; RESULT-PATH is ((NEW-BIND B0 .. Bn))
			     ;; RESULT-PATH is covered by some path ((B0' .. Bl') C1' .. Ck') in the rest 
			     ;; of REST-PATHS, iff NEW-BIND corresponds to one Ci' and (B0 .. Bn) is a 
			     ;; sublist of (B0' .. Bl').
			     (multiple-value-bind (covering-path the-rest-paths)
				 (cstr=help-covered-by2 new-bind (rest (first result-path)) (rest rest-paths))
			       (if covering-path
				   (cstr=pop-up-bindings the-rest-paths 
							 (append done-paths (list covering-path))
							 new-bind (not done-paths))
				 (cond (done-paths
					(values (cstr~disjunction
						 (cstr=help-build-conjunctions (append done-paths result-path the-rest-paths)))))
				       (T
					;;; No path in DONE-PATHS and in THE-REST-PATHS covers RESULT-PATH:
					;; Since POP-UP and SAME-BIND were together NIL, we have to check whether NEW-BIND
					;; can be popped up in THE-REST-PATHS.
					(multiple-value-bind (Rcstr Rbindings)
					    (if the-rest-paths
						(cstr=pop-up-bindings the-rest-paths NIL new-bind T)
					      (values T (list new-bind)))
					  (cond ((find new-bind Rbindings)
						 (values
						  (cstr~disjunction 
						   (list (cstr~conjunction (rest (first result-path)))
							 (cstr~conjunction (cons Rcstr 
										 (remove new-bind Rbindings)))))
						  (list new-bind)))
						(T
						 (values 
						  (cstr~disjunction
						   (append (cstr=help-build-conjunctions result-path)
							   (list (cstr~conjunction (cons Rcstr Rbindings))))))))))))))
			    ))))))
  )



(defun cstr=disjunction-pop-bindings (bind&cstrs binding binding-cstrs &optional disjuncts)
  ;;;A list of cons pairs: binding.cstr, a binding and a list of cstrs 
  ;; groups the cstrs for the same binding in disjunction and compose this with
  ;; the binding as disjunction:
  ;; When there is only one common binding, returns: <disjunction binding>
  (if bind&cstrs
      (let ((bind&cstr (find-if #'(lambda (bc) (keim~equal (cstr~binding-term (car bc))
							   (cstr~binding-term binding)))
				bind&cstrs)))
	(if bind&cstr
	    (cstr=disjunction-pop-bindings (remove bind&cstr bind&cstrs) binding
					   (append binding-cstrs (list (cdr bind&cstr))) disjuncts)
	  (cstr=disjunction-pop-bindings (rest bind&cstrs) (car (first bind&cstrs))
					 (list (cdr (first bind&cstrs)))
					 (cons (cstr~conjunction (list binding (cstr~disjunction binding-cstrs)))
					       disjuncts))))
    (if disjuncts
	(values (cstr~disjunction
		 (cons (cstr~conjunction (list binding (cstr~disjunction binding-cstrs)))
		       disjuncts)))
      (values (cstr~disjunction binding-cstrs) binding))
    ))


(defgeneric cstr=merge-disjuncts (merge-cstr old-cstr sat-cstrs merge-cstrs)
  (:method ((disjuncts cons) (old-cstr cstr+composition) sat-cstrs merge-cstrs)
	   (let (the-Scstrs the-Mcstrss the-Mcstrs)
	     (dolist (disjunct disjuncts)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (if (cstr=common-variable-p disjunct old-cstr)
		       (cstr=merge disjunct old-cstr sat-cstrs merge-cstrs)
		     (if merge-cstrs
			 (cstr=merge merge-cstrs (cons disjunct (append sat-cstrs (list old-cstr))) NIL NIL)
		       (values (cstr~conjunction (cons disjunct (append sat-cstrs (list old-cstr)))))))
		 (when (and (listp Scstr) (some #'(lambda (x) (eq x T)) Scstr))
		   (setq the-Scstrs NIL the-Mcstrss NIL the-Mcstrs NIL)
		   (mapc #'(lambda (S Ms M) (when (eq S T)
					      (setq the-Scstrs (append the-Scstrs (list S))
						    the-Mcstrss (append the-Mcstrss (list Ms))
						    the-Mcstrs (append the-Mcstrs (list M)))))
			 Scstr Mcstrs Mcstr)
		   (return))
		 (when (eq Scstr T)
		   (setq the-Scstrs (list Scstr)
			 the-Mcstrss (list Mcstrs)
			 the-Mcstrs (list Mcstr))
		   (return))
		 (if splitp
		     (setq the-Scstrs (append the-Scstrs Scstr)
			   the-Mcstrss (append the-Mcstrss Mcstrs)
			   the-Mcstrs (append the-Mcstrs Mcstr))
		   (when Scstr
		     (setq the-Scstrs (append the-Scstrs (list Scstr))
			   the-Mcstrss (append the-Mcstrss (list Mcstrs))
			   the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
	     (when the-Scstrs
	       (if (rest the-Scstrs)
		   (values the-Scstrs the-Mcstrss the-Mcstrs T)
		 (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs))))))
  (:method ((disjuncts cons) (old-cstr cstr+binding) sat-cstrs merge-cstrs)
	   (let (the-Scstrs the-Mcstrss the-Mcstrs)
	     (dolist (disjunct disjuncts)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (cond ((cstr~composition-p disjunct)
			  (cstr=merge disjunct old-cstr sat-cstrs merge-cstrs))
			 (T
			  (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp1)
			      (cstr=merge disjunct old-cstr NIL NIL)
			    (when Scstr1
			      (multiple-value-bind (RScstr RMcstrs RMcstr Rsplitp)
				  (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 NIL NIL sat-cstrs merge-cstrs)
				(when RScstr
				  (if (or (and (cstr~binding-p disjunct)
					       (or (and (null splitp1) (null Mcstrs1))
						   (and splitp1 (every #'null Mcstrs1))))
					  (and (cstr~simple-p disjunct)
					       (or (and (null splitp1) (null Mcstr1) (null (rest Mcstrs1)))
						   (and splitp1 (every #'null Mcstr1)
							(every #'(lambda (Ms) (null (rest Ms))) Mcstrs1)))))
				      (progn
					(if Rsplitp
					    (setq the-Scstrs RScstr
						  the-Mcstrss RMcstrs
						  the-Mcstrs RMcstr)
					  (setq the-Scstrs (list RScstr)
						the-Mcstrss (list RMcstrs)
						the-Mcstrs (list RMcstr)))
					(return (values)))
				    (values RScstr RMcstrs RMcstr Rsplitp))))))))
		 (when (cstr~composition-p disjunct)
		   (when (and (listp Scstr) (some #'(lambda (x) (eq x T)) Scstr))
		     (setq the-Scstrs NIL the-Mcstrss NIL the-Mcstrs NIL)
		     (mapc #'(lambda (S Ms M) (when (eq S T)
						(setq the-Scstrs (append the-Scstrs (list S))
						      the-Mcstrss (append the-Mcstrss (list Ms))
						      the-Mcstrs (append the-Mcstrs (list M)))))
			   Scstr Mcstrs Mcstr)
		     (return))
		   (when (eq Scstr T)
		     (setq the-Scstrs (list Scstr)
			   the-Mcstrss (list Mcstrs)
			   the-Mcstrs (list Mcstr))
		     (return)))
		 (if splitp
		     (setq the-Scstrs (append the-Scstrs Scstr)
			   the-Mcstrss (append the-Mcstrss Mcstrs)
			   the-Mcstrs (append the-Mcstrs Mcstr))
		   (when Scstr
		     (setq the-Scstrs (append the-Scstrs (list Scstr))
			   the-Mcstrss (append the-Mcstrss (list Mcstrs))
			   the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
	     (when the-Scstrs
	       (if (rest the-Scstrs)
		   (values the-Scstrs the-Mcstrss the-Mcstrs T)
		 (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs))))))
  (:method ((disjuncts cons) (old-cstr cstr+simple) sat-cstrs merge-cstrs)
	   (let (the-Scstrs the-Mcstrss the-Mcstrs)
	     (dolist (disjunct disjuncts)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (if (cstr=common-variable-p disjunct old-cstr)
		       (cond ((cstr~composition-p disjunct)
			      (cstr=merge disjunct old-cstr sat-cstrs merge-cstrs))
			     (T
			      (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp1)
				  (cstr=merge disjunct old-cstr NIL NIL)
				(when Scstr1
				  (multiple-value-bind (RScstr RMcstrs RMcstr Rsplitp)
				      (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 NIL NIL sat-cstrs merge-cstrs)
				    (when RScstr
				      (if (or (and (cstr~binding-p disjunct)
						   (or (and (null splitp1) (eq Scstr1 T) (null Mcstrs1))
						       (and splitp1 (every #'(lambda (x) (eq x T)) Scstr1)
							    (every #'null Mcstrs1))))
					      (and (cstr~simple-p disjunct)
						   (or (and (null splitp1) (null Mcstr1))
						       (and splitp1 (every #'null Mcstr1)))))
					  (progn
					    (if Rsplitp
						(setq the-Scstrs RScstr
						      the-Mcstrss RMcstrs
						      the-Mcstrs RMcstr)
					      (setq the-Scstrs (list RScstr)
						    the-Mcstrss (list RMcstrs)
						    the-Mcstrs (list RMcstr)))
					    (return (values)))
					(values RScstr RMcstrs RMcstr Rsplitp))))))))
		     (if merge-cstrs
			 (cstr=merge merge-cstrs (cons disjunct (append sat-cstrs (list old-cstr))) NIL NIL)
		       (values (cstr~conjunction (append sat-cstrs (list old-cstr))) NIL disjunct)))
		 (when (cstr~composition-p disjunct)
		   (when (and (listp Scstr) (some #'(lambda (x) (eq x T)) Scstr))
		     (setq the-Scstrs NIL the-Mcstrss NIL the-Mcstrs NIL)
		     (mapc #'(lambda (S Ms M) (when (eq S T)
						(setq the-Scstrs (append the-Scstrs (list S))
						      the-Mcstrss (append the-Mcstrss (list Ms))
						      the-Mcstrs (append the-Mcstrs (list M)))))
			   Scstr Mcstrs Mcstr)
		     (return))
		   (when (eq Scstr T)
		     (setq the-Scstrs (list Scstr)
			   the-Mcstrss (list Mcstrs)
			   the-Mcstrs (list Mcstr))
		     (return)))
		 (if splitp
		     (setq the-Scstrs (append the-Scstrs Scstr)
			   the-Mcstrss (append the-Mcstrss Mcstrs)
			   the-Mcstrs (append the-Mcstrs Mcstr))
		   (when Scstr
		     (setq the-Scstrs (append the-Scstrs (list Scstr))
			   the-Mcstrss (append the-Mcstrss (list Mcstrs))
			   the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
	     (when the-Scstrs
	       (if (rest the-Scstrs)
		   (values the-Scstrs the-Mcstrss the-Mcstrs T)
		 (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs))))))
  (:method ((merge-cstr cstr+composition) (disjuncts cons) sat-cstrs merge-cstrs)
	   (let (the-Scstrs the-Mcstrss the-Mcstrs)
	     (dolist (disjunct disjuncts)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (if (cstr=common-variable-p merge-cstr disjunct)
		       (cstr=merge merge-cstr disjunct sat-cstrs merge-cstrs)
		     (if merge-cstrs
			 (cstr=merge merge-cstrs (cons merge-cstr (append sat-cstrs (list disjunct))) NIL NIL)
		       (values (cstr~conjunction (cons merge-cstr (append sat-cstrs (list disjunct)))))))
		 (when (and (listp Scstr) (some #'(lambda (x) (eq x T)) Scstr))
		   (setq the-Scstrs NIL the-Mcstrss NIL the-Mcstrs NIL)
		   (mapc #'(lambda (S Ms M) (when (eq S T)
					      (setq the-Scstrs (append the-Scstrs (list S))
						    the-Mcstrss (append the-Mcstrss (list Ms))
						    the-Mcstrs (append the-Mcstrs (list M)))))
			 Scstr Mcstrs Mcstr)
		   (return))
		 (when (eq Scstr T)
		   (setq the-Scstrs (list Scstr)
			 the-Mcstrss (list Mcstrs)
			 the-Mcstrs (list Mcstr))
		   (return))
		 (if splitp
		     (setq the-Scstrs (append the-Scstrs Scstr)
			   the-Mcstrss (append the-Mcstrss Mcstrs)
			   the-Mcstrs (append the-Mcstrs Mcstr))
		   (when Scstr
		     (setq the-Scstrs (append the-Scstrs (list Scstr))
			   the-Mcstrss (append the-Mcstrss (list Mcstrs))
			   the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
	     (when the-Scstrs
	       (if (rest the-Scstrs)
		   (values the-Scstrs the-Mcstrss the-Mcstrs T)
		 (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs))))))
  (:method ((merge-cstr cstr+binding) (disjuncts cons) sat-cstrs merge-cstrs)
	   (let (the-Scstrs the-Mcstrss the-Mcstrs)
	     (dolist (disjunct disjuncts)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (if (cstr=common-variable-p merge-cstr disjunct)
		       (cond ((cstr~composition-p disjunct)
			      (cstr=merge merge-cstr disjunct sat-cstrs merge-cstrs))
			     (T
			      (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp1)
				  (cstr=merge merge-cstr disjunct NIL NIL)
				(when Scstr1
				  (multiple-value-bind (RScstr RMcstrs RMcstr Rsplitp)
				      (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 NIL NIL sat-cstrs merge-cstrs)
				    (when RScstr
				      (if (or (and (cstr~binding-p disjunct)
						   (or (and (null splitp1) (null Mcstrs1))
						       (and splitp1 (every #'null Mcstrs1))))
					      (and (cstr~simple-p disjunct)
						   (or (and (null splitp1) (eq Scstr1 T) (null Mcstrs1))
						       (and splitp1 (every #'(lambda (x) (eq x T)) Scstr1)
							    (every #'null Mcstrs1)))))
					  (progn
					    (if Rsplitp
						(setq the-Scstrs RScstr
						      the-Mcstrss RMcstrs
						      the-Mcstrs RMcstr)
					      (setq the-Scstrs (list RScstr)
						    the-Mcstrss (list RMcstrs)
						    the-Mcstrs (list RMcstr)))
					    (return (values)))
					(values RScstr RMcstrs RMcstr Rsplitp))))))))
		     (if merge-cstrs
			 (multiple-value-bind (merge-Scstr merge-Mcstrs merge-Mcstr merge-splitp)
			     (cstr=merge merge-cstrs (append sat-cstrs (list disjunct)) NIL NIL)
			   (if merge-splitp
			       (values (mapcar #'(lambda (Sc Mc)
						   (if Mc (if (cstr~constraint-p Sc)
							      (cstr~conjunction (list Mc Sc))
							    Mc)
						     Sc))
					       merge-Scstr merge-Mcstr)
				       merge-Mcstrs
				       (make-sequence 'list (length merge-Scstr) :initial-element merge-cstr)
				       merge-splitp)
			     (when merge-Scstr
			       (values (if merge-Mcstr (if (cstr~constraint-p merge-Scstr)
							   (cstr~conjunction (list merge-Mcstr merge-Scstr))
							 merge-Mcstr)
					 merge-Scstr)
				       merge-Mcstrs
				       merge-cstr))))
		       (values (cstr~conjunction (append sat-cstrs (list disjunct))) NIL merge-cstr)))
		 (when (cstr~composition-p disjunct)
		   (when (and (listp Scstr) (some #'(lambda (x) (eq x T)) Scstr))
		     (setq the-Scstrs NIL the-Mcstrss NIL the-Mcstrs NIL)
		     (mapc #'(lambda (S Ms M) (when (eq S T)
						(setq the-Scstrs (append the-Scstrs (list S))
						      the-Mcstrss (append the-Mcstrss (list Ms))
						      the-Mcstrs (append the-Mcstrs (list M)))))
			   Scstr Mcstrs Mcstr)
		     (return))
		   (when (eq Scstr T)
		     (setq the-Scstrs (list Scstr)
			   the-Mcstrss (list Mcstrs)
			   the-Mcstrs (list Mcstr))
		     (return)))
		 (if splitp
		     (setq the-Scstrs (append the-Scstrs Scstr)
			   the-Mcstrss (append the-Mcstrss Mcstrs)
			   the-Mcstrs (append the-Mcstrs Mcstr))
		   (when Scstr
		     (setq the-Scstrs (append the-Scstrs (list Scstr))
			   the-Mcstrss (append the-Mcstrss (list Mcstrs))
			   the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
	     (when the-Scstrs
	       (if (rest the-Scstrs)
		   (values the-Scstrs the-Mcstrss the-Mcstrs T)
		 (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs))))))
  (:method ((merge-cstr cstr+simple) (disjuncts cons) sat-cstrs merge-cstrs)
	   (let (the-Scstrs the-Mcstrss the-Mcstrs)
	     (dolist (disjunct disjuncts)
	       (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
		   (if (cstr=common-variable-p merge-cstr disjunct)
		       (cond ((cstr~composition-p disjunct)
			      (cstr=merge merge-cstr disjunct sat-cstrs merge-cstrs))
			     (T
			      (multiple-value-bind (Scstr1 Mcstrs1 Mcstr1 splitp1)
				  (cstr=merge merge-cstr disjunct NIL NIL)
				(when Scstr1
				  (multiple-value-bind (RScstr RMcstrs RMcstr Rsplitp)
				      (cstr=merge&split Scstr1 Mcstrs1 Mcstr1 NIL NIL sat-cstrs merge-cstrs)
				    (when RScstr
				      (if (or (and (cstr~binding-p disjunct)
						   (or (and (null splitp1) (null Mcstr1) (null (rest Mcstrs1)))
						       (and splitp1 (every #'null Mcstr1)
							    (every #'(lambda (Ms) (null (rest Ms))) Mcstrs1))))
					      (and (cstr~simple-p disjunct)
						   (or (and (null splitp1) (null Mcstr1))
						       (and splitp1 (every #'null Mcstr1)))))
					  (progn
					    (if Rsplitp
						(setq the-Scstrs RScstr
						      the-Mcstrss RMcstrs
						      the-Mcstrs RMcstr)
					      (setq the-Scstrs (list RScstr)
						    the-Mcstrss (list RMcstrs)
						    the-Mcstrs (list RMcstr)))
					    (return (values)))
					(values RScstr RMcstrs RMcstr Rsplitp))))))))
		     (if merge-cstrs
			 (cstr=merge merge-cstrs (cons merge-cstr (append sat-cstrs (list disjunct))) NIL NIL)
		       (values (cstr~conjunction (cons merge-cstr (append sat-cstrs (list disjunct)))))))
		 (when (cstr~composition-p disjunct)
		   (when (and (listp Scstr) (some #'(lambda (x) (eq x T)) Scstr))
		     (setq the-Scstrs NIL the-Mcstrss NIL the-Mcstrs NIL)
		     (mapc #'(lambda (S Ms M) (when (eq S T)
						(setq the-Scstrs (append the-Scstrs (list S))
						      the-Mcstrss (append the-Mcstrss (list Ms))
						      the-Mcstrs (append the-Mcstrs (list M)))))
			   Scstr Mcstrs Mcstr)
		     (return))
		   (when (eq Scstr T)
		     (setq the-Scstrs (list Scstr)
			   the-Mcstrss (list Mcstrs)
			   the-Mcstrs (list Mcstr))
		     (return)))
		 (if splitp
		     (setq the-Scstrs (append the-Scstrs Scstr)
			   the-Mcstrss (append the-Mcstrss Mcstrs)
			   the-Mcstrs (append the-Mcstrs Mcstr))
		   (when Scstr
		     (setq the-Scstrs (append the-Scstrs (list Scstr))
			   the-Mcstrss (append the-Mcstrss (list Mcstrs))
			   the-Mcstrs (append the-Mcstrs (list Mcstr)))))))
	     (when the-Scstrs
	       (if (rest the-Scstrs)
		   (values the-Scstrs the-Mcstrss the-Mcstrs T)
		 (values (first the-Scstrs) (first the-Mcstrss) (first the-Mcstrs))))))
  )
  

(defun cstr~satisfiable-p (cstr-name cstr-args polarity)
  ;;; Calls the satisfiability function of the constraint CSTR-NAME with
  ;; the arguments CSTR-ARGS and the POLARITY:
  (let ((func-name (read-from-string (concatenate 'string "cstr~sat-"
						  (format nil "~A" cstr-name)))))
    (apply (symbol-function func-name) (append cstr-args (list polarity)))))


  
								  

;;; Satisfiability functions:

;;; SOME
(defun cstr~sat-some (cstr1 &rest rest-args)
  (declare (edited  "05-FEB-1999")
	   (authors Lassaad)
	   (input   "A constraint and a list containing other constraints and a"
		    "boolean as last element which corresponds to the polarity"
		    "of the constraint  SOME.")
	   (effect  "None.")
	   (value   "Two values: a (possibly empty, i.e, T) constraint,"
		    "and a (possibly empty) list of (binding) constraints,"
		    "when the satisfiability is possible, o/w, NIL."))
  (let ((cstrs (cons cstr1 (butlast rest-args)))
	result-cstrs successp)
    (dolist (cstr cstrs)
      (cond ((cstr~composition-p cstr)
	     (omega~error ";;;cstr~~sat-some must be extended for compositions."))
	    ((cstr~binding-p cstr)
	     (push cstr result-cstrs))
	    ((cstr~simple-p cstr)
	     (multiple-value-bind (new-cstr new-bindings splitp)
		 (cstr~satisfiable-p (keim~name cstr) (cstr~arguments cstr) (cstr~polarity cstr))
	       (when new-cstr
		 (if splitp
		     (let* ((disjuncts (mapcar #'(lambda (C Bs) 
						   (if (cstr~constraint-p C)
						       (cstr~conjunction (cons C Bs))
						     (when Bs (cstr~conjunction Bs))))
					       new-cstr new-bindings))
			    (positive-disjuncts (remove-if #'null disjuncts)))
		       (if positive-disjuncts
			   (push (cstr~disjunction positive-disjuncts) result-cstrs)
			 ;;;CSTR is successfull without additional constraints
			 (return (setq successp t))))
		   (if (or (cstr~constraint-p new-cstr) new-bindings)
		       (push (cstr~conjunction (if (cstr~constraint-p new-cstr)
						   (cons new-cstr new-bindings)
						 new-bindings))
			     result-cstrs)
		     ;;;CSTR is successfull without additional constraints
		     (return (setq successp t)))))))))
    (cond (successp (values t))
	  ((rest result-cstrs)
	   (values (cstr~simple-create 'some
				       result-cstrs
				       (remove-duplicates (apply #'append (mapcar #'cstr~variables result-cstrs))))))
	  ((first result-cstrs)
	   (let ((result-cstr (first result-cstrs)))
	     (cond ((cstr~conjunction-p result-cstr)
		    (let ((result-bindings (remove-if-not #'cstr~binding-p (cstr~arguments result-cstr))))
		      (if result-bindings
			  (values (cstr~conjunction (remove-if #'cstr~binding-p (cstr~arguments result-cstr)))
				  result-bindings)
			(values result-cstr))))
		   ((cstr~binding-p result-cstr)
		    (values T (list result-cstr)))
		   (T (values result-cstr))))))
    ))

;;; SUBTERM
(defgeneric cstr~sat-subterm (term1 term2 polarity)
  (declare (edited  "26-NOV-1998")
	   (authors Lassaad)
	   (input   "Two terms and a boolean POLARITY to choose between"
		    "asking for either satisfiability or unsatisfiability"
		    "of the constraint.")
	   (effect  "None.")
	   (value   "Two values: a (possibly empty, i.e, T) constraint,"
		    "and a (possibly empty) list of (binding) constraints,"
		    "when the (un-)satisfiability is possible, o/w, NIL."))
  
  (:method ((const1 term+constant) (const2 term+constant) polarity)
	   (if polarity
	       (values (data~equal const1 const2))
	     (values (not (data~equal const1 const2)))))
  (:method ((term term+term) (domain-var keim::gb+helpvariable) polarity)
	   (values (not polarity)))
  (:method ((mvar1 meta+variable) (mvar2 meta+variable) polarity)
	   (if (keim~equal mvar1 mvar2)
	       (values polarity)
	     (values (cstr~simple-create :subterm
					 (list mvar1 mvar2)
					 (list mvar1 mvar2)
					 polarity))))
  (:method ((const term+constant) (mvar meta+variable) polarity)
	   (values (cstr~simple-create :subterm
                                       (list const mvar)
				       (list mvar)
                                       polarity)))
  (:method ((term term+term) (mvar meta+variable) polarity)
           (values (cstr~simple-create :subterm
                                       (list term mvar)
                                       (union (cstr~free-variables term) (list mvar))
                                       polarity)))
  (:method ((mvar meta+variable) (term term+term) polarity)
           (values (cstr~simple-create :subterm
                                       (list mvar term)
                                       (union (list mvar) (cstr~free-variables term))
                                       polarity)))
  (:method ((const term+constant) (appl term+appl) polarity)
	   (let ((func (data~appl-function appl)))
	     (if (meta~p func)
		 (values (cstr~simple-create :subterm
					     (list const appl)
					     (cons func (cstr~free-variables (data~appl-arguments appl)))
					     polarity))
	       (if (term~abstr-p func)
		   (cstr~sat-subterm const (beta~normalize appl) polarity)
		 (multiple-value-bind (fcstr)
		     (cstr~sat-subterm const func T)
		   (cond ((cstr~constraint-p fcstr)
			  (values (cstr~simple-create :subterm
						      (list const appl)
						      (union (cstr~free-variables func)
							     (cstr~free-variables (data~appl-arguments appl)))
						      polarity)))
			 (fcstr ;;; CONST is a subterm of FUNC:
			  (values polarity))
			 (T ;;; CONST is not a subterm of FUNC:
			  (cstr~sat-subterm const (data~appl-arguments appl) polarity))))))))
  (:method ((const term+constant) (abstr term+abstr) polarity)
	   (cstr~sat-subterm const (data~abstr-range abstr) polarity))
  (:method ((const term+constant) (var term+variable) polarity)
	   ;;; VAR arises in the domain of a lambda-abstraction
	   (values (not polarity)))
  (:method ((term T) (args cons) polarity)
	   (let (cstrs subtermp)
	     (dolist (arg args)
	       (multiple-value-bind (argcstr)
		   (cstr~sat-subterm term arg T)
		 (if (cstr~constraint-p argcstr)
		     (push argcstr cstrs)
		   (when argcstr
		     (setq subtermp T) (return)))))
	     (if subtermp
		 (values polarity)
	       (if cstrs
		   (cond (polarity
			  (values (if (rest cstrs)
				      (cstr~simple-create :some cstrs
							  (remove-duplicates
							   (apply #'append (mapcar #'cstr~variables cstrs))))
				    (first cstrs))))
			 (T
			  (values (cstr~conjunction (mapcar #'cstr~negate cstrs)))))
		 (values (not polarity))))))
  
;  (:method ((const term+constant) (abstr term+abstr))
;           (term~subterm-p const (data~abstr-range abstr)))
;  (:method ((mvar1 meta+variable) (mvar2 meta+variable))
;           (or (data~equal mvar1 mvar2)
;               (constr~create 'subterm (list mvar1 mvar2))))
;  (:method ((mvar meta+variable) (const term+constant))
;           (constr~create 'subterm (list mvar1 const)))
;  (:method ((mvar meta+variable) (appl term+appl))
;           (let ((func (data~appl-function appl)))
;             (if (meta~p func)
;                 (constr~create 'subterm (list const appl))
;               (let ((terms (cons func (data~appl-arguments appl)))
;                     (constrs))
;                 (do* ((rest-terms terms (rest rest-terms))
;                       (result (term~subterm-p mvar (first rest-terms))
;                               (term~subterm-p mvar (first rest-terms))))
;                     ((or (null rest-terms)
;                          (and result (not (constr~p result))))
;                      ;;; REST-TERMS is empty, or RESULT is T 
;                      (if (null rest-terms)
;                          (if constrs
;                              (cond ((rest constrs)
;                                     (constr~create 'or constrs))
;                                    (t (first constrs))))
;                        (or (setq constrs NIL)
;                            result)))
;                   (if (constr~p result)
;                       (setq constrs (append constrs (list result)))))))))
  (:method ((term1 T) (term2 T) polarity)
	   (declare (ignore polarity))
	   (omega~error "cstr~~sat-subterm not yet implemented for ~A, ~A"
			(type-of term1) (type-of term2)))
  )

;;; IS-HEAD-OF
(defgeneric cstr~sat-is-head-of (term1 term2 polarity)
  (declare (edited  "08-APR-1999")
	   (authors Lassaad)
	   (input   "Two terms and a boolean POLARITY to choose between"
		    "asking for either satisfiability or unsatisfiability"
		    "of the constraint.")
	   (effect  "None.")
	   (value   "Two values: a (possibly empty, i.e, T) constraint,"
		    "and a (possibly empty) list of (binding) constraints,"
		    "when the (un-)satisfiability is possible, o/w, NIL."))
  (:method ((const term+constant) (mvar meta+variable) polarity)
           (values (cstr~simple-create :is-head-of
                                       (list const mvar)
                                       (list mvar)
                                       polarity)))
  (:method ((const term+constant) (appl term+appl) polarity)
           (let ((func (data~appl-function appl)))
	     (cond ((meta~p func)
		    (values (cstr~simple-create :is-head-of
						(list const appl)
						(cons func (cstr~free-variables (data~appl-arguments appl)))
						polarity)))
		   ((data~abstr-p func)
		    (cstr~sat-subterm const (beta~normalize appl) polarity))
		   ((data~appl-p func)
		    (multiple-value-bind (fcstr)
			(cstr~sat-is-head-of const func T)
		      (cond ((cstr~constraint-p fcstr)
			     (values (cstr~simple-create :is-head-of
							 (list const appl)
							 (union (cstr~free-variables func)
								(cstr~free-variables (data~appl-arguments appl)))
							 polarity)))
                         (fcstr ;;; CONST is head of FUNC:
                          (values polarity))
                         (T ;;; CONST is not head of FUNC:
			  (values (not polarity))))))
		   ((term~constant-p func)
		    (if polarity 
			(values (data~equal const func))
		      (values (not (data~equal const func)))))
		   (T
		    (values (not polarity))))))
  (:method ((const term+constant) (abstr term+abstr) polarity)
	   (cstr~sat-is-head-of const (data~abstr-range abstr) polarity))
  (:method ((const term+constant) (abstr term+constant) polarity)
	   (values (not polarity)))
  (:method ((term1 T) (term2 T) polarity)
	   (declare (ignore polarity))
	   (omega~error "cstr~~sat-is-head-of not yet implemented for ~A, ~A"
			(type-of term1) (type-of term2))
	   (error "IS-HEAD-OF"))
  )

;;; LINEAR
(defgeneric cstr~sat-linear (term polarity)
  (declare (edited  "08-APR-1999")
	   (authors Lassaad)
	   (input   "A term and a boolean POLARITY to choose between"
		    "asking for either satisfiability or unsatisfiability"
		    "of the constraint.")
	   (effect  "None.")
	   (value   "Two values: a (possibly empty, i.e, T) constraint,"
		    "and a (possibly empty) list of (binding) constraints,"
		    "when the (un-)satisfiability is possible, o/w, NIL."))
  (:method ((const term+constant) polarity)
	   (values polarity))
  (:method ((mvar meta+variable) polarity)
	   (values (cstr~simple-create :linear
				       (list mvar)
				       (list mvar)
				       polarity)))
  (:method ((appl term+appl) polarity)
	   (let ((func (data~appl-function appl)))
	     (cond ((meta~p func)
		    (values (cstr~simple-create :linear
						(list appl)
						(cons func (cstr~free-variables (data~appl-arguments appl)))
						polarity)))
		   ((data~abstr-p func)
		    (cstr~sat-linear (beta~normalize appl) polarity))
		   ((data~appl-p func)
		    (cstr~sat-linear (data~n-normalize appl) polarity))
		   ((term~constant-p func)
		    (values polarity)))))
  (:method ((abstr term+abstr) polarity)
	   (multiple-value-bind (bvars-possibly-in-flex-subterms bvars-not-occur)
	       (cstr=help-linear-wrt (data~abstr-range abstr)
				     (data~abstr-domain abstr)
				     (data~abstr-domain abstr))
	     (cond ((null bvars-possibly-in-flex-subterms)
		    ;; Each domain variable occurs at least once in range and not in a flexible subterm
		    (values polarity))
		   ((null bvars-not-occur)
		    ;; Each domain variable occurs in range
		    (values (cstr~simple-create :linear
						(list abstr)
						(cstr~free-variables (data~abstr-range abstr)))))
		   (T
		    ;; Some domain variables dont occur in range
		    (values (not polarity))))))
  (:method (term polarity)
	   (declare (ignore polarity))
	   (omega~error "cstr~~sat-linear not yet implemented for ~A"
			(type-of term))
	   (error "LINEAR"))
  )

(defgeneric cstr=help-linear-wrt (object bvars1 bvars2)
  (declare (edited  "04-MAY-1999")
	   (authors Lassaad)
	   (input   "A term or a term list (corresponds to (subterms of) the range of"
		    "some abstraction), and two variable lists (correspond to (a subset"
		    "of) the domain of this abstraction.")
	   (effect  "None.")
	   (value   "A Pair: - The elements of BVARS1 which dont occur once in OBJECT"
		    "not nested in a flexible subterm."
		    "        - The elements of BVARS2 which dont occur in OBJECT."))
  (:method (object (nix null) bvars2)
	   (declare (ignore object bvars2))
	   (values NIL NIL))
  (:method ((terms cons) bvars1 bvars2)
	   (multiple-value-bind (rest-bvars1 rest-bvars2)
	       (cstr=help-linear-wrt (first terms) bvars1 bvars2)
	     (cstr=help-linear-wrt (rest terms) rest-bvars1 rest-bvars2)))
  (:method ((nix null) bvars1 bvars2)
	   (values bvars1 bvars2))
  (:method ((appl term+appl) (bvars1 cons) (bvars2 cons))
	   (let ((func (data~appl-function appl)))
	     (cond ((meta~p func)
		    (values bvars1
			    (set-difference bvars2 (data~free-variables appl))))
		   ((term~constant-p func)
		    (cstr=help-linear-wrt (data~appl-arguments appl) bvars1 bvars2))
		   ((term~variable-p func)
		    (cstr=help-linear-wrt (data~appl-arguments appl)
					  (remove func bvars1) (remove func bvars2)))
		   (T
		    (omega~error ";;;cstr=help-linear-wrt must be extended for ~A ~A ~A"
				 appl bvars1 bvars2)
		    (error "cstr=help-linear-wrt")))))
  (:method ((appl term+appl) (bvars1 cons) (nix null))
	   (let ((func (data~appl-function appl)))
	     (cond ((meta~p func)
		    (values bvars1 NIL))
		   ((term~constant-p func)
		    (cstr=help-linear-wrt (data~appl-arguments appl) bvars1 NIL))
		   ((term~variable-p func)
		    (cstr=help-linear-wrt (data~appl-arguments appl)
					  (remove func bvars1) NIL))
		   (T
		    (omega~error ";;;cstr=help-linear-wrt must be extended for ~A ~A ~A"
				 appl bvars1 nix)
		    (error "cstr=help-linear-wrt")))))
  (:method ((abstr term+abstr) bvars1 bvars2)
	   (cstr=help-linear-wrt (data~abstr-range abstr) bvars1 bvars2))
  (:method ((const term+constant) bvars1 bvars2)
	   (values bvars1 bvars2))
  (:method ((var term+variable) bvars1 bvars2)
	   (values (remove var bvars1) (remove var bvars2)))
  (:method (object bvars1 bvars2)
	   (omega~error "cstr=help-linear-wrt not yet implemented for ~A ~A ~A"
			(type-of object) (type-of bvars1) (type-of bvars2))
	   (error "cstr=help-linear-wrt"))
  )

		    
;;; TYPE=
(defgeneric cstr~sat-type= (type1 type2 polarity)
  (declare (edited  "26-NOV-1998")
	   (authors Lassaad)
	   (input   "Two types and a boolean POLARITY to choose between"
		    "asking for either satisfiability or unsatisfiability"
		    "of the constraint.")
	   (effect  "None.")
	   (value   "The (un)satisfiability value of the constraint."))
  (:method ((type1 type+constant) (type2 type+constant) polarity)
	   (if polarity
	       (values (keim~equal type1 type2))
	     (values (not (keim~equal type1 type2)))))
  (:method ((type1 T) (type2 T) polarity)
	   (omega~error "cstr~~sat-type= not yet implemented for ~A, ~A"
			(type-of type1) (type-of type2)))
  )



;;; UNIFY
(defgeneric cstr~sat-unify (term1 term2 polarity &optional metavars)
  ;;; METAVARS are the relevant variables for them and only for them bindings should be delivered, this is
  ;; needed to get the help variable away:
  ;;; Returns:
  ;; <Const,Bindings,NIL> when there is only one unification solution
  ;; <(Const1,...,Constn),(Bindings1,...,Bindingsn),T> when there is many unification solutions
  ;; NIL, when there is no solution:
  (declare (edited  "26-NOV-1998")
	   (authors Lassaad)
	   (input   "Two terms and a boolean POLARITY to choose between"
		    "asking for either satisfiability or unsatisfiability"
		    "of the constraint.")
	   (effect  "None.")
	   (value   "The (un)satisfiability value of the constraint."))
  (:method ((terms1 cons) (terms2 cons) polarity &optional metavars)
	   (if (= (length terms1) (length terms2))
	       (let ((hou-list (uni~unify (mapcar #'(lambda (t1 t2) (list t1 t2)) terms1 terms2)
					  :destructive nil
					  :solutions 'one
					  :cost-limit cstr*unification-depth)))
		 (if hou-list
		     (cond (polarity
			    (let ((hous (if metavars
					    (cstr~adapt-hou-result hou-list metavars)
					  (cstr~adapt-hou-result hou-list))))
			      (if (rest hous)
				  (let (cstrs bindings)
				    (dolist (hou (reverse hous))
				      (if (uni~flex-flex hou)
					  (push (cstr~conjunction (mapcar #'(lambda (ff)
									      (cstr~simple-create :unify ff
												  (cstr~free-variables ff)))
									  (uni~flex-flex hou))) cstrs)
					(push T cstrs))
				      (push (mapcar #'(lambda (var term)
							(cstr~binding-create (list var term)))
						    (subst~domain (uni~substitution hou))
						    (subst~codomain (uni~substitution hou))) bindings))
				    (values cstrs bindings T))
				(let ((hou (first hous)))
				  (values (if (uni~flex-flex hou)
					      (cstr~conjunction
					       (mapcar #'(lambda (ff) (cstr~simple-create :unify ff
											  (cstr~free-variables ff)))
						       (uni~flex-flex hou)))
					    T)
					  (mapcar #'(lambda (var term)
						      (cstr~binding-create (list var term)))
						  (subst~domain (uni~substitution hou))
						  (subst~codomain (uni~substitution hou))))))))
			   (T ;;;
			    (let ((hous (if metavars
					    (cstr~adapt-hou-result hou-list metavars)
					  (cstr~adapt-hou-result hou-list))))
			      (if (rest hous)
				  (let (conjuncts)
				    (dolist (hou (reverse hous))
				      (let ((negative-cstrs
					     (append (mapcar #'(lambda (ff)
								 (cstr~simple-create :unify ff
										     (cstr~free-variables ff)
										     NIL))
							     (uni~flex-flex hou))
						     (mapcar #'(lambda (x y)
								 (cstr~simple-create
								  :unify (list x y)
								  (cstr~free-variables (list x y)) NIL))
							     (subst~domain (uni~substitution hou))
							     (subst~codomain (uni~substitution hou))))))
					(if (rest negative-cstrs)
					    (push (cstr~simple-create :some negative-cstrs
								      (remove-duplicates
								       (apply #'append (mapcar #'cstr~variables negative-cstrs))))
						  conjuncts)
					  (when (first negative-cstrs)
					    (push (first negative-cstrs) conjuncts)))))
				    (values (cstr~conjunction conjuncts)))
				(let* ((hou (first hous))
				       (negative-cstrs
					(append (mapcar #'(lambda (ff)
							    (cstr~simple-create :unify ff
										(cstr~free-variables ff)
										NIL))
							(uni~flex-flex hou))
						(mapcar #'(lambda (x y)
							    (cstr~simple-create
							     :unify (list x y)
							     (cstr~free-variables (list x y)) NIL))
							(subst~domain (uni~substitution hou))
							(subst~codomain (uni~substitution hou))))))
				  (values
				   (if (rest negative-cstrs)
				       (cstr~simple-create :some negative-cstrs
							   (remove-duplicates
							    (apply #'append (mapcar #'cstr~variables negative-cstrs))))
				     (first negative-cstrs))))))))
		   (values (not polarity))))
	     (values (not polarity))))
  (:method ((var1 meta+variable) (term2 term+term) polarity &optional metavars)
	   (declare (ignore metavars))
	   (if (cstr=hou-occur-p var1 term2)
	       (if polarity nil (values T))
	     (if polarity
		 (values T (list (cstr~binding-create (list var1 term2)
						      (cons var1 (cstr~free-variables term2)))))
	       (values (cstr~simple-create :unify
					   (list var1 term2)
					   (cons var1 (cstr~free-variables term2))
					   polarity)))))
  (:method ((term1 term+term) (var2 meta+variable) polarity &optional metavars)
	   (declare (ignore metavars))
	   (if (cstr=hou-occur-p var2 term1)
	       (if polarity nil (values T))
	     (if polarity
		 (values T (list (cstr~binding-create (list var2 term1))))
	       (values (cstr~simple-create :unify
					   (list var2 term1)
					   (cons var2 (cstr~free-variables term1))
					   polarity)))))
  (:method ((var1 term+variable) (term2 term+term) polarity &optional metavars)
	   (declare (ignore metavars))
	   (if (cstr=hou-occur-p var1 term2)
	       (if polarity nil (values T))
	     (if polarity
		 (values T (list (cstr~binding-create (list var1 term2)
						      (cons var1 (cstr~free-variables term2)))))
	       (values (cstr~simple-create :unify
					   (list var1 term2)
					   (cons var1 (cstr~free-variables term2))
					   polarity)))))
  (:method ((term1 term+term) (var2 term+variable) polarity &optional metavars)
	   (declare (ignore metavars))
	   (if (cstr=hou-occur-p var2 term1)
	       (if polarity nil (values T))
	     (if polarity
		 (values T (list (cstr~binding-create (list var2 term1))))
	       (values (cstr~simple-create :unify
					   (list var2 term1)
					   (cons var2 (cstr~free-variables term1))
					   polarity)))))
  (:method ((term1 term+term) (term2 term+term) polarity &optional metavars)
	   (let ((hou-list (uni~unify (list (list term1 term2))
				      :destructive nil
				      :solutions 'one
				      :cost-limit cstr*unification-depth)))
	     (if hou-list
		 (cond (polarity
			(let ((hous (if metavars
					(cstr~adapt-hou-result hou-list metavars)
				      (cstr~adapt-hou-result hou-list))))
			  (if (rest hous)
			      (let (cstrs bindings)
				(dolist (hou (reverse hous))
				  (if (uni~flex-flex hou)
				      (push (cstr~conjunction (mapcar #'(lambda (ff) (cstr~simple-create :unify ff
											    (cstr~free-variables ff)))
								      (uni~flex-flex hou))) cstrs)
				    (push T cstrs))
				  (push (mapcar #'(lambda (var term)
						    (cstr~binding-create (list var term)))
						(subst~domain (uni~substitution hou))
						(subst~codomain (uni~substitution hou))) bindings))
				(values cstrs bindings T))
			    (let ((hou (first hous)))
			      (values (if (uni~flex-flex hou)
					  (cstr~conjunction
					   (mapcar #'(lambda (ff) (cstr~simple-create :unify ff
										      (cstr~free-variables ff)))
						   (uni~flex-flex hou)))
					T)
				      (mapcar #'(lambda (var term)
						  (cstr~binding-create (list var term)))
					      (subst~domain (uni~substitution hou))
					      (subst~codomain (uni~substitution hou))))))))
		       (T ;;;
			(let ((hous (if metavars
					(cstr~adapt-hou-result hou-list metavars)
				      (cstr~adapt-hou-result hou-list))))
			  (if (rest hous)
			      (let (conjuncts)
				(dolist (hou (reverse hous))
				  (let ((negative-cstrs
					 (append (mapcar #'(lambda (ff)
							     (cstr~simple-create :unify ff
										 (cstr~free-variables ff)
										 NIL))
							 (uni~flex-flex hou))
						 (mapcar #'(lambda (x y)
							     (cstr~simple-create
							      :unify (list x y)
							      (cstr~free-variables (list x y)) NIL))
							 (subst~domain (uni~substitution hou))
							 (subst~codomain (uni~substitution hou))))))
				    (if (rest negative-cstrs)
					(push (cstr~simple-create :some negative-cstrs
								  (remove-duplicates
								   (apply #'append (mapcar #'cstr~variables negative-cstrs))))
					      conjuncts)
				      (when (first negative-cstrs)
					(push (first negative-cstrs) conjuncts)))))
				(values (cstr~conjunction conjuncts)))
			    (let* ((hou (first hous))
				   (negative-cstrs
				    (append (mapcar #'(lambda (ff)
							(cstr~simple-create :unify ff
									    (cstr~free-variables ff)
									    NIL))
						    (uni~flex-flex hou))
					    (mapcar #'(lambda (x y)
							(cstr~simple-create
							 :unify (list x y)
							 (cstr~free-variables (list x y)) NIL))
						    (subst~domain (uni~substitution hou))
						    (subst~codomain (uni~substitution hou))))))
			      (values
			       (if (rest negative-cstrs)
				   (cstr~simple-create :some negative-cstrs
						       (remove-duplicates
							(apply #'append (mapcar #'cstr~variables negative-cstrs))))
				 (first negative-cstrs))))))))
	       (values (not polarity)))))
  (:method ((term1 T) (term2 T) polarity &optional metavars)
	   (declare (ignore polarity metavars))
	   (omega~error "cstr~~sat-unify not yet implemented for ~A, ~A"
			(type-of term1) (type-of term2)))
  )


(defgeneric cstr=hou-occur-p (variable term)
  (declare (edited  "27-NOV-1998")
	   (authors Lassaad)
	   (input   "A variable, and a term which is beta-normalized.")
	   (effect  "None.")
	   (value   "T, iff VARIABLE occurs in TERM and not as argument"
		    "of a flexible head."))
  (:method (variable (term term+constant))
           (declare (ignore variable))
	   nil)
  (:method (variable (term term+variable))
	   (keim~equal variable term))
  (:method (variable (term term+appl))
	   (and (not (data~variable-p  (data~appl-function term)))
		(or (cstr=hou-occur-p variable (data~appl-function term))
		    (dolist (subterm (data~appl-arguments term) nil)
		      (when (cstr=hou-occur-p variable subterm)
			(return T))))))
  (:method (variable (term term+abstr))
           (if (find variable (data~abstr-domain term) :test #'keim~equal)
	       nil
             (cstr=hou-occur-p variable (data~abstr-range term)))))
      

(defun cstr~variable-paths (cstr-state metavar)
  (multiple-value-bind (binding-paths otherwise)
      (when (and (pds~constraint-pool-p cstr-state)
		 (pds~cstrpool-constraint cstr-state)
		 (find metavar (cstr~variables (pds~cstrpool-constraint cstr-state))))
	(multiple-value-bind (metavar-paths other-path)
	    ;;; METAVAR-PATHS should be a list of the form ((metavar <- t1 C1) ... (metavar <- tn Cn))
	    (cstr=variable-paths (pds~cstrpool-constraint cstr-state) metavar)
	  (values (cstr=same-variable-paths metavar-paths) other-path)))
    (when binding-paths
      (labels ((cstr&bindings (cstrs)
			      (if cstrs
				  (let ((cstr1 (first cstrs)))
				    (if cstr1
					(cond ((listp cstr1)
					       (cstr&bindings (append cstr1 (rest cstrs))))
					      ((cstr~binding-p cstr1)
					       (multiple-value-bind (cstr bindings)
						   (cstr&bindings (rest cstrs))
						 (values cstr (cons cstr1 bindings))))
					      ((cstr~conjunction-p cstr1)
					       (cstr&bindings (append (cstr~arguments cstr1) (rest cstrs))))
					      (T 
					       (multiple-value-bind (cstr bindings)
						   (cstr&bindings (rest cstrs))
						 (values (cstr~conjunction (list cstr1 cstr)) bindings))))
				      (cstr&bindings (rest cstrs))))
				(values T))))
	(values (mapcar #'(lambda (Bpath)
			    (multiple-value-bind (cstr bindings)
				(cstr&bindings Bpath)
			      (pds~cstrpool-create (subst~create (mapcar #'cstr~bound-variable bindings)
								 (mapcar #'cstr~binding-term bindings))
						   (when (cstr~constraint-p cstr) cstr) cstr-state)))
			binding-paths)
		(multiple-value-bind (cstr bindings)
		    (if (cstr~constraint-p otherwise) (cstr&bindings (list otherwise))
		      (cstr&bindings otherwise))
		  (pds~cstrpool-create (subst~create (mapcar #'cstr~bound-variable bindings)
						     (mapcar #'cstr~binding-term bindings))
				       (when (cstr~constraint-p cstr) cstr) cstr-state)))))
    ))

(defun cstr=same-variable-paths (var-paths)
  ;;; VAR-PATHS is a list of constraint lists of the form (X<- t, C), for instance
  ;; ((X<- t1, C1) (X<- t2, C2) (X<- t1, C3)).
  ;; This function builds disjunction for the Cis associated to the same bindings.
  ;; For instance, the result of the above list must be ((X<- t1, or(C1 C3)), (X<-t2, C2))
  (when var-paths
    (let* ((var-path1 (first var-paths))
	   (var-bind1 (first var-path1))
	   (bind1-paths (remove-if-not #'(lambda (path)
					   (cstr=help-same-bindings-p var-bind1 (first path)))
				       (rest var-paths)))
	   (the-var-path1 (find-if #'(lambda (path) (null (rest path))) (cons var-path1 bind1-paths))))
      (cond (the-var-path1
	     ;;; One path with no additional constraints
	     (cons (append the-var-path1 (list T))
		   (cstr=same-variable-paths (set-difference (rest var-paths) bind1-paths))))
	    (T
	     (cons (list var-bind1
			 (cstr~disjunction (mapcar #'(lambda (path) (cstr~conjunction (rest path)))
						   (cons var-path1 bind1-paths))))
		   (cstr=same-variable-paths (set-difference (rest var-paths) bind1-paths))))))
    ))

(defun cstr=help-cstr&bindings (cstrs)
  (if cstrs
      (let ((cstr1 (first cstrs)))
	(if cstr1
	    (cond ((listp cstr1)
		   (cstr=help-cstr&bindings (append cstr1 (rest cstrs))))
		  ((cstr~binding-p cstr1)
		   (multiple-value-bind (cstr bindings)
		       (cstr=help-cstr&bindings (rest cstrs))
		     (values cstr (cons cstr1 bindings))))
		  ((cstr~conjunction-p cstr1)
		   (cstr=help-cstr&bindings (append (cstr~arguments cstr1) (rest cstrs))))
		  (T 
		   (multiple-value-bind (cstr bindings)
		       (cstr=help-cstr&bindings (rest cstrs))
		     (values (cstr~conjunction (list cstr1 cstr)) bindings))))
	  (cstr=help-cstr&bindings (rest cstrs))))
    (values T)
    ))
	   
(defgeneric cstr=variable-paths (cstr var)
  ;;; Returns a two value: a list of constraint lists, where every list corresponds to a
  ;; propagation path of CSTR wrt. to a binding of VAR, and a constraint which
  ;; corresponds to the propagation path of CSTR where VAR is not bound:
  (:method ((cstr cstr+composition) var)
	   (cond ((string-equal (keim~name cstr) :and)
		  (let ((conjunct-number (- 1))
			bind-cstrs other-cstr)
		    (dolist (conjunct (cstr~arguments cstr))
		      (setq conjunct-number (+ conjunct-number 1))
		      (when (find var (cstr~variables conjunct))
			(multiple-value-bind (Bcstrs Ocstr)
			    (cstr=variable-paths conjunct var)
			  (when Bcstrs
			    (return (setq bind-cstrs Bcstrs
					  other-cstr Ocstr))))))
		    (if bind-cstrs
			(let ((rest-conjuncts (remove (nth conjunct-number (cstr~arguments cstr))
						      (cstr~arguments cstr)))
			      binding-paths other-path)
			  (dolist (bind-cstr bind-cstrs)
			    (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
				(cstr=merge (first bind-cstr) (append (rest bind-cstr) rest-conjuncts) NIL NIL)
			      (if splitp
				  (let ((rest-Scstr Scstr)
					(rest-Mcstrs Mcstrs))
				    (dolist (Mc Mcstr)
				      (multiple-value-bind (var-binding rest-cstr)
					  (if (cstr~binding-p Mc)
					      (if (data~equal (cstr~bound-variable Mc) var)
						  (values Mc (cstr~conjunction (cons (first rest-Scstr)
										     (first rest-Mcstrs))))
						(progn 
						  (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as a third value but it returned ~A instead." var Mc)
						  (return-from cstr=variable-paths)))
					    (multiple-value-bind (cstr bindings)
						(cstr=help-cstr&bindings (list Mc))
					      (let ((var-bind (find-if #'(lambda (bind)
									   (data~equal (cstr~bound-variable bind) var))
								       bindings)))
						(if var-bind
						    (values var-bind
							    (cstr~conjunction (append (remove var-bind bindings)
										      (cons cstr (cons (first rest-Scstr)
												       (first rest-Mcstrs))))))
						  (progn 
						    (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as an argument of the third value but it returned ~A instead." var Mc)
						    (return-from cstr=variable-paths))))))
					(setq binding-paths 
					      (append binding-paths (list (list var-binding rest-cstr)))))
				      (setq rest-Scstr (rest rest-Scstr)
					    rest-Mcstrs (rest rest-Mcstrs))))
				(when Scstr
				  (multiple-value-bind (var-binding rest-cstr)
				      (if (cstr~binding-p Mcstr)
					  (if (data~equal (cstr~bound-variable Mcstr) var)
					      (values Mcstr (cstr~conjunction (cons Scstr Mcstrs)))
					    (progn 
					      (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as a third value but it returned ~A instead." var Mcstr)
					      (return-from cstr=variable-paths)))
					(multiple-value-bind (cstr bindings)
					    (cstr=help-cstr&bindings (list Mcstr))
					  (let ((var-bind (find-if #'(lambda (bind)
								       (data~equal (cstr~bound-variable bind) var))
								   bindings)))
					    (if var-bind
						(values var-bind (cstr~conjunction (append (remove var-bind bindings)
											   (cons cstr (cons Scstr Mcstrs)))))
					      (progn 
						(omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as an argument of the third value but it returned ~A instead." var Mcstr)
						(return-from cstr=variable-paths))))))
				    (setq binding-paths 
					  (append binding-paths (list (list var-binding rest-cstr)))))))))
			  (when other-cstr
			    (multiple-value-bind (Scstr Mcstrs Mcstr splitp)
				(cstr=merge other-cstr rest-conjuncts NIL NIL)
			      (if splitp
				  (let ((rest-Scstr Scstr)
					(rest-Mcstrs Mcstrs)
					other-paths)
				    (dolist (Mc Mcstr)
				      (let ((all-other-cstr (cstr~conjunction (if Mc (cons Mc
											   (cons (first rest-Scstr)
												 (first rest-Mcstrs)))
										(cons (first rest-Scstr)
										      (first rest-Mcstrs))))))
					(when (cstr~constraint-p all-other-cstr)
					  (multiple-value-bind (Bcstrs Ocstr)
					      (if (find var (cstr~variables other-cstr))
						  (cstr=variable-paths all-other-cstr var)
						(values NIL all-other-cstr))
					    (setq binding-paths (append binding-paths Bcstrs)
						  other-paths (append other-paths (list Ocstr))))))
				      (setq rest-Scstr (rest rest-Scstr)
					    rest-Mcstrs (rest rest-Mcstrs)))
				    (setq other-path (cstr~disjunction (remove-if #'null other-paths))))
				(when Scstr
				  (let ((all-other-cstr (cstr~conjunction (if Mcstr (cons Mcstr (cons Scstr Mcstrs))
									    (cons Scstr Mcstrs)))))
				    (when (cstr~constraint-p all-other-cstr)
				      (multiple-value-bind (Bcstrs Ocstr)
					  (if (find var (cstr~variables other-cstr))
					      (cstr=variable-paths all-other-cstr var)
					    (values NIL all-other-cstr))
					(setq binding-paths (append binding-paths Bcstrs)
					      other-path Ocstr))))))))
			  (values binding-paths other-path))
		      (values NIL cstr))))
		 ((string-equal (keim~name cstr) :or)
		  (let (bind-cstrs other-cstrs)
		    (dolist (disjunct (cstr~arguments cstr))
		      (if (find var (cstr~variables disjunct))
			  (multiple-value-bind (Bcstrs Ocstr)
			      (cstr=variable-paths disjunct var)
			    (when Bcstrs
			      (setq bind-cstrs (append bind-cstrs Bcstrs)))
			    (when Ocstr
			      (setq other-cstrs (append other-cstrs (list Ocstr)))))
			(setq other-cstrs (append other-cstrs (list disjunct)))))
		    (values bind-cstrs (when other-cstrs (cstr~disjunction other-cstrs)))))
		 ((string-equal (keim~name cstr) :if)
		  (let ((cond (first (cstr~arguments cstr)))
			(then (second (cstr~arguments cstr)))
			(else (third (cstr~arguments cstr))))
		    (if (find var (cstr~variables cond))
			(let (binding-paths other-paths)
			  ;;; FIRST cond,then:
			  (multiple-value-bind (PBcstrs POcstr)
			      (cstr=variable-paths cond var)
			    (if PBcstrs
				(progn 
				  (if (cstr~constraint-p then)
				      (dolist (PBcstr PBcstrs)
					(multiple-value-bind (Scstr Mcstrs Mcstr)
					    (if (cstr=common-variable-p PBcstr then)
						(cstr=merge PBcstr then NIL NIL)
					      (values then NIL (cstr~conjunction PBcstr)))
					  (when Scstr
					    (multiple-value-bind (var-binding rest-cstr)
						(if (cstr~binding-p Mcstr)
						    (if (data~equal (cstr~bound-variable Mcstr) var)
							(values Mcstr (cstr~conjunction (cons Scstr Mcstrs)))
						      (progn 
							(omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as a third value but it returned ~A instead." var Mcstr)
							(return-from cstr=variable-paths)))
						  (multiple-value-bind (cstr bindings)
						      (cstr=help-cstr&bindings (list Mcstr))
						    (let ((var-bind (find-if #'(lambda (bind)
										 (data~equal (cstr~bound-variable bind) var))
									     bindings)))
						      (if var-bind
							  (values var-bind
								  (cstr~conjunction (append (remove var-bind bindings)
											    (cons cstr (cons Scstr Mcstrs)))))
							(progn 
							  (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as an argument of the third value but it returned ~A instead." var Mcstr)
							  (return-from cstr=variable-paths))))))
					      (setq binding-paths
						    (append binding-paths (list (list var-binding rest-cstr))))))))
				    (append binding-paths PBcstrs))
				  (when POcstr
				    (if (cstr~constraint-p then)
					(multiple-value-bind (Scstr Mcstrs Mcstr)
					    (if (cstr=common-variable-p POcstr then)
						(cstr=merge POcstr then NIL NIL)
					      (values then NIL POcstr))
					  (when Scstr
					    (let ((other-cstr (cstr~conjunction (if Mcstr (cons Mcstr (cons Scstr Mcstrs))
										  (cons Scstr Mcstrs)))))
					      (when (cstr~constraint-p other-cstr)
						(multiple-value-bind (Bcstrs Ocstr)
						    (if (find var (cstr~variables other-cstr))
							(cstr=variable-paths other-cstr var)
						      (values NIL other-cstr))
						  (setq binding-paths (append binding-paths Bcstrs)
							other-paths (append other-paths (list Ocstr))))))))
				      (setq other-paths (append other-paths (list POcstr))))))
			      (when POcstr
				(if (cstr~constraint-p then)
				    (if (find var (cstr~variables then))
					(multiple-value-bind (TBcstrs TOcstr)
					    (cstr=variable-paths then var)
					  (when TBcstrs
					    (dolist (TBcstr TBcstrs)
					      (multiple-value-bind (Scstr Mcstrs Mcstr)
						  (if (cstr=common-variable-p TBcstr POcstr)
						      (cstr=merge TBcstr POcstr NIL NIL)
						    (values POcstr NIL (cstr~conjunction TBcstr)))
						(when Scstr
						  (multiple-value-bind (var-binding rest-cstr)
						      (if (cstr~binding-p Mcstr)
							  (if (data~equal (cstr~bound-variable Mcstr) var)
							      (values Mcstr (cstr~conjunction (cons Scstr Mcstrs)))
							    (progn 
							      (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as a third value but it returned ~A instead." var Mcstr)
							      (return-from cstr=variable-paths)))
							(multiple-value-bind (cstr bindings)
							    (cstr=help-cstr&bindings (list Mcstr))
							  (let ((var-bind (find-if #'(lambda (bind)
										       (data~equal (cstr~bound-variable bind) var))
										   bindings)))
							    (if var-bind
								(values var-bind
									(cstr~conjunction (append (remove var-bind bindings)
												  (cons cstr (cons Scstr Mcstrs)))))
							      (progn 
								(omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as an argument of the third value but it returned ~A instead." var Mcstr)
								(return-from cstr=variable-paths))))))
						    (setq binding-paths
							  (append binding-paths (list (list var-binding rest-cstr)))))))))
					  (when TOcstr
					    (multiple-value-bind (Scstr Mcstrs Mcstr)
						(if (cstr=common-variable-p TOcstr POcstr)
						    (cstr=merge TOcstr POcstr NIL NIL)
						  (values POcstr NIL TOcstr))
					      (when Scstr
						(let ((other-cstr (cstr~conjunction (if Mcstr (cons Mcstr (cons Scstr Mcstrs))
										      (cons Scstr Mcstrs)))))
						  (when (cstr~constraint-p other-cstr)
						    (multiple-value-bind (Bcstrs Ocstr)
							(if (find var (cstr~variables other-cstr))
							    (cstr=variable-paths other-cstr var)
							  (values NIL other-cstr))
						      (setq binding-paths (append binding-paths Bcstrs)
							    other-paths (append other-paths (list Ocstr))))))))))
				      (setq other-paths
					    (append other-paths
						    (list (cstr~conjunction (list POcstr then))))))
				  ;;;No binding path for VAR in the propagation of COND and THEN is T:
				  (setq other-paths
					(append other-paths (list POcstr)))))
			      ))
			  ;;; THEN not(cond), else:
			  (multiple-value-bind (NBcstrs NOcstr)
			      (cstr=variable-paths (cstr~negate cond) var)
			    (if NBcstrs
				(progn 
				  (if (cstr~constraint-p else)
				      (dolist (NBcstr NBcstrs)
					(multiple-value-bind (Scstr Mcstrs Mcstr)
					    (if (cstr=common-variable-p NBcstr else)
						(cstr=merge NBcstr else NIL NIL)
					      (values else NIL (cstr~conjunction NBcstr)))
					  (when Scstr
					    (multiple-value-bind (var-binding rest-cstr)
						(if (cstr~binding-p Mcstr)
						    (if (data~equal (cstr~bound-variable Mcstr) var)
							(values Mcstr (cstr~conjunction (cons Scstr Mcstrs)))
						      (progn 
							(omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as a third value but it returned ~A instead." var Mcstr)
							(return-from cstr=variable-paths)))
						  (multiple-value-bind (cstr bindings)
						      (cstr=help-cstr&bindings (list Mcstr))
						    (let ((var-bind (find-if #'(lambda (bind)
										 (data~equal (cstr~bound-variable bind) var))
									     bindings)))
						      (if var-bind
							  (values var-bind
								  (cstr~conjunction (append (remove var-bind bindings)
											    (cons cstr (cons Scstr Mcstrs)))))
							(progn 
							  (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as an argument of the third value but it returned ~A instead." var Mcstr)
							  (return-from cstr=variable-paths))))))
					      (setq binding-paths
						    (append binding-paths (list (list var-binding rest-cstr))))))))
				    (append binding-paths NBcstrs))
				  (when NOcstr
				    (if (cstr~constraint-p else)
					(multiple-value-bind (Scstr Mcstrs Mcstr)
					    (if (cstr=common-variable-p NOcstr else)
						(cstr=merge NOcstr else NIL NIL)
					      (values else NIL NOcstr))
					  (when Scstr
					    (let ((other-cstr (cstr~conjunction (if Mcstr (cons Mcstr (cons Scstr Mcstrs))
										  (cons Scstr Mcstrs)))))
					      (when (cstr~constraint-p other-cstr)
						(multiple-value-bind (Bcstrs Ocstr)
						    (if (find var (cstr~variables other-cstr))
							(cstr=variable-paths other-cstr var)
						      (values NIL other-cstr))
						  (setq binding-paths (append binding-paths Bcstrs)
							other-paths (append other-paths (list Ocstr))))))))
				      (setq other-paths (append other-paths (list NOcstr))))))
			      (when NOcstr
				(if (cstr~constraint-p else)
				    (if (find var (cstr~variables else))
					(multiple-value-bind (EBcstrs EOcstr)
					    (cstr=variable-paths else var)
					  (when EBcstrs
					    (dolist (EBcstr EBcstrs)
					      (multiple-value-bind (Scstr Mcstrs Mcstr)
						  (if (cstr=common-variable-p EBcstr NOcstr)
						      (cstr=merge EBcstr NOcstr NIL NIL)
						    (values NOcstr NIL (cstr~conjunction EBcstr)))
						(when Scstr
						  (multiple-value-bind (var-binding rest-cstr)
						      (if (cstr~binding-p Mcstr)
							  (if (data~equal (cstr~bound-variable Mcstr) var)
							      (values Mcstr (cstr~conjunction (cons Scstr Mcstrs)))
							    (progn 
							      (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as a third value but it returned ~A instead." var Mcstr)
							      (return-from cstr=variable-paths)))
							(multiple-value-bind (cstr bindings)
							    (cstr=help-cstr&bindings (list Mcstr))
							  (let ((var-bind (find-if #'(lambda (bind)
										       (data~equal (cstr~bound-variable bind) var))
										   bindings)))
							    (if var-bind
								(values var-bind
									(cstr~conjunction (append (remove var-bind bindings)
												  (cons cstr (cons Scstr Mcstrs)))))
							      (progn 
								(omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as an argument of the third value but it returned ~A instead." var Mcstr)
								(return-from cstr=variable-paths))))))
						    (setq binding-paths (list (list var-binding rest-cstr))))))))
					  (when EOcstr
					    (multiple-value-bind (Scstr Mcstrs Mcstr)
						(if (cstr=common-variable-p EOcstr NOcstr)
						    (cstr=merge EOcstr NOcstr NIL NIL)
						  (values NOcstr NIL EOcstr))
					      (when Scstr
						(let ((other-cstr (cstr~conjunction (if Mcstr (cons Mcstr (cons Scstr Mcstrs))
										      (cons Scstr Mcstrs)))))
						  (when (cstr~constraint-p other-cstr)
						    (multiple-value-bind (Bcstrs Ocstr)
							(if (find var (cstr~variables other-cstr))
							    (cstr=variable-paths other-cstr var)
							  (values NIL other-cstr))
						      (setq binding-paths (append binding-paths Bcstrs)
							    other-paths (append other-paths (list Ocstr))))))))))
				      (setq other-paths
					    (append other-paths
						    (list (cstr~conjunction (list NOcstr else))))))
				  ;;;No binding path for VAR in the propagation of COND and THEN is T:
				  (setq other-paths
					(append other-paths (list NOcstr)))))
			      ))
			  ;;; Return the accumulated results:
			  (values binding-paths (when other-paths (cstr~disjunction other-paths))))
		      ;;; VAR does not occur in COND:
		      (let (binding-paths other-paths)
			;;; FIRST then path:
			(if (cstr~constraint-p then)
			    (if (find var (cstr~variables then))
				(multiple-value-bind (TBcstrs TOcstr)
				    (cstr=variable-paths then var)
				  (if TBcstrs
				      (progn 
					(dolist (TBcstr TBcstrs)
					  (multiple-value-bind (Scstr Mcstrs Mcstr)
					      (if (cstr=common-variable-p TBcstr cond)
						  (cstr=merge TBcstr cond NIL NIL)
						(values cond NIL (cstr~conjunction TBcstr)))
					    (when Scstr
					      (multiple-value-bind (var-binding rest-cstr)
						  (if (cstr~binding-p Mcstr)
						      (if (data~equal (cstr~bound-variable Mcstr) var)
							  (values Mcstr (cstr~conjunction (cons Scstr Mcstrs)))
							(progn 
							  (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as a third value but it returned ~A instead." var Mcstr)
							  (return-from cstr=variable-paths)))
						    (multiple-value-bind (cstr bindings)
							(cstr=help-cstr&bindings (list Mcstr))
						      (let ((var-bind (find-if #'(lambda (bind)
										   (data~equal (cstr~bound-variable bind) var))
									       bindings)))
							(if var-bind
							    (values var-bind
								    (cstr~conjunction (append (remove var-bind bindings)
											      (cons cstr (cons Scstr Mcstrs)))))
							  (progn 
							    (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as an argument of the third value but it returned ~A instead." var Mcstr)
							    (return-from cstr=variable-paths))))))
						(setq binding-paths
						      (append binding-paths (list (list var-binding rest-cstr))))))))
					(when TOcstr
					  (multiple-value-bind (Scstr Mcstrs Mcstr)
					      (if (cstr=common-variable-p TOcstr cond)
						  (cstr=merge TOcstr cond NIL NIL)
						(values cond NIL TOcstr))
					    (when Scstr
					      (let ((other-cstr (cstr~conjunction (if Mcstr (cons Mcstr (cons Scstr Mcstrs))
										    (cons Scstr Mcstrs)))))
						(when (cstr~constraint-p other-cstr)
						  (multiple-value-bind (Bcstrs Ocstr)
						      (if (find var (cstr~variables other-cstr))
							  (cstr=variable-paths other-cstr var)
							(values NIL other-cstr))
						    (setq binding-paths (append binding-paths Bcstrs)
							  other-paths (append other-paths (list Ocstr))))))))))
				    (setq other-paths
					  (append other-paths
						  (list (cstr~conjunction (list cond TOcstr)))))))
			      (setq other-paths
				    (append other-paths
					    (list (cstr~conjunction (list cond then))))))
			  (setq other-paths
				(append other-paths (list cond))))
			;;; THEN else path:
			(if (cstr~constraint-p else)
			    (if (find var (cstr~variables else))
				(multiple-value-bind (EBcstrs EOcstr)
				    (cstr=variable-paths else var)
				  (if EBcstrs
				      (let ((neg-cond (cstr~negate cond)))
					(dolist (EBcstr EBcstrs)
					  (multiple-value-bind (Scstr Mcstrs Mcstr)
					      (if (cstr=common-variable-p EBcstr neg-cond)
						  (cstr=merge EBcstr neg-cond NIL NIL)
						(values neg-cond NIL (cstr~conjunction EBcstr)))
					    (when Scstr
					      (multiple-value-bind (var-binding rest-cstr)
						  (if (cstr~binding-p Mcstr)
						      (if (data~equal (cstr~bound-variable Mcstr) var)
							  (values Mcstr (cstr~conjunction (cons Scstr Mcstrs)))
							(progn 
							  (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as a third value but it returned ~A instead." var Mcstr)
							  (return-from cstr=variable-paths)))
						    (multiple-value-bind (cstr bindings)
							(cstr=help-cstr&bindings (list Mcstr))
						      (let ((var-bind (find-if #'(lambda (bind)
										   (data~equal (cstr~bound-variable bind) var))
									       bindings)))
							(if var-bind
							    (values var-bind
								    (cstr~conjunction (append (remove var-bind bindings)
											      (cons cstr (cons Scstr Mcstrs)))))
							  (progn 
							    (omega~error ";;;cstr=variable-paths: cstr=merge should return a binding for ~A as an argument of the third value but it returned ~A instead." var Mcstr)
							    (return-from cstr=variable-paths))))))
						(setq binding-paths
						      (append binding-paths (list (list var-binding rest-cstr))))))))
					(when EOcstr
					  (multiple-value-bind (Scstr Mcstrs Mcstr)
					      (if (cstr=common-variable-p EOcstr neg-cond)
						  (cstr=merge EOcstr neg-cond NIL NIL)
						(values neg-cond NIL EOcstr))
					    (when Scstr
					      (let ((other-cstr (cstr~conjunction (if Mcstr (cons Mcstr (cons Scstr Mcstrs))
										    (cons Scstr Mcstrs)))))
						(when (cstr~constraint-p other-cstr)
						  (multiple-value-bind (Bcstrs Ocstr)
						      (if (find var (cstr~variables other-cstr))
							  (cstr=variable-paths other-cstr var)
							(values NIL other-cstr))
						    (setq binding-paths (append binding-paths Bcstrs)
							  other-paths (append other-paths (list Ocstr))))))))))
				    (setq other-paths
					  (append other-paths
						  (list (cstr~conjunction (list (cstr~negate cond) EOcstr)))))))
			      (setq other-paths
				    (append other-paths
					    (list (cstr~conjunction (list (cstr~negate cond) else))))))
			  (setq other-paths
				(append other-paths (list (cstr~negate cond)))))
			;;; Return the results:
			(values binding-paths (when other-paths (cstr~disjunction other-paths))))
		      )))
		 (T ;;;
		  (omega~error ";;;cstr=variable-paths: Unknown composition ~A" cstr))))
  (:method ((cstr cstr+binding) var)
	   (if (keim~equal (cstr~bound-variable cstr) var)
	       (values (list (list cstr)))
	     (values NIL cstr)))
  (:method ((cstr cstr+simple) var)
	   (declare (ignore var))
	   (values NIL cstr))
  )
			  












