

(in-package "KEIM")

(mod~defmod sort :uses (type term)
            :documentation "Fundamental data structures."
            :exports (
		      sort+sort
                      sort+primitive
		      sort+constant
		      sort+polyconst
		      sort+complex
		      sort+abstr
		      sort+intersection

		      sort+environment
		      sort+termdecl

		      sort~env-create
		      sort~env-enter
		      sort~env-sorts
		      sort~env-termdecls

		      sort~lookup-sort
		      sort~sort-of-pred
		      sort~insert-term-decl

		      sort~top-term-decls
		      sort~all-term-decls
		      sort~all-imit-term-decls
		      
		      sort~domain-sort
                      sort~range-sort
		      sort~subsorts
		      sort~type-of-sort
		      sort~sort-length
		      sort~type-length
		      sort~c-domain
		      sort~c-range
		      sort~j-c-domain
		      sort~j-c-range
		      sort~np-domain
		      sort~n-range-rel
		      sort~sort-p
		      sort~functional-p
		      )
	    )

#{ \section{sorts}\label{mod:sort}

This module provides sorts which refines the underlying type system.
Like terms or types in \keim the  different categories for  the sort system
consists of constant sorts, variable sorts and abstraction sorts.
It is possible to define constant sorts which refines a functional type.

#}

(defvar sort*constructor 'fun-sort
  "Predicate in the Omega theory for the construction of non-primitive function sorts." )
(defvar sort*tdquantor 'forall-sort
  "Quantor in the Omega theory for term declarations.")
(defvar sort*tdkappa 'all-types
  "Quantor in the Omega theory for polymorphic types.")
(defvar sort*sortpred 'sort-defined
  "Predicate for welldefined sorts in the Omega theory.")

;;;
;;; Classes
;;;

(eval-when (load compile eval)

  (defclass sort+sort (data+object data+struct)
    ()
    (:documentation "The class of all sorts in KEIM."))

  (defclass sort+primitive (sort+sort data+primitive)
    ()
    (:documentation "The class of primitive sorts."))
  
  (defclass sort+complex (sort+sort data+complex)
    ()
    (:documentation "The class of complex sorts."))
  
  (defclass sort+abstr (sort+complex data+abstr)
    ()
    (:documentation "The class of functional sorts."))

  (defclass sort+constant (sort+primitive data+constant)
    ((domain :initarg :domain-sort
	     :initform nil
	     :accessor sort~domain-sort)
     (range  :initarg :range-sort
	     :initform nil
	     :accessor sort~range-sort))
    (:documentation "The class of constant sorts."))

  (defclass sort+polyconst (sort+constant)
    ((instances  :initarg :instances
		 :initform nil
		 :accessor sort=instances))
    (:documentation "The class of constant sorts."))

  (defclass sort+intersection (data+object)
    ((members :initarg :members
	      :initform nil
	      :accessor sort~intersection-members))
    (:documentation "The class of intersection sorts."))
  
  (defclass sort+environment (help+help)
    ((parents :initarg :parents
	      :initform nil
	      :accessor sort=env-parents)
     (env     :initarg :env
	      :initform nil
	      :accessor sort=env-env)
     (sorts   :initarg :sorts
	      :initform (make-hash-table)
	      :accessor sort=env-sorts)
     (termdecls   :initarg :termsdecls
		  :initform (make-hash-table)
		  :accessor sort=env-termdecls)
;     (subdecls    :initarg :subdecls
;                  :initform nil
;                  :accessor sort=env-subdecls))
     )
    (:documentation "The environment for sorts."))

;  (defclass sort+subdecl (data+top)
;    ((subsort   :initarg :subsort
;                :initform nil
;                :accessor sort~sd-subsort)
;     (supersort :initarg :supersort
;                :initform nil
;                :accessor sort~sd-supersort)
;     (theorem   :initarg :theorem
;                :initform nil
;                :accessor sort~sd-theorem))
;    (:documentation "The class of subsort decls."))

  (defclass sort+termdecl (data+top)
    ((term      :initarg :term
	        :initform nil
	        :accessor sort~td-term)
     (sort      :initarg :sort
	        :initform nil
	        :accessor sort~td-sort)
     (theorem   :initarg :theorem
		:initform nil
		:accessor sort~td-theorem))
    (:documentation "The class of term decls.")))


;;;
;;; Functions for sorted environments
;;; 

(defun sort~env-create (&key (name nil) (unsortedenv nil) (parents nil))
  (make-instance 'sort+environment
		 :name name
		 :env unsortedenv
		 :parents parents))

(defgeneric sort~env-enter (obj env &key termdecl)
  (declare (edited  "15-MAR-2000")
	   (authors Pollet)
	   (input   "An object (sort-constant, termdecl, or a cons with key termdecl)"
		    "and an sorted environment." )
	   (effect  "The object will be stored into the sorted environment.")
	   (value   "undefined."))
  (:method ((obj sort+constant) (env sort+environment) &key (termdecl nil))
	   (declare (ignore termdecl))
	   (setf (gethash (sort=real-name obj) (sort=env-sorts env)) obj))
  (:method ((obj sort+termdecl) (env sort+environment) &key (termdecl nil))
	   (declare (ignore termdecl))
	   (let* ((tdhash (sort=env-termdecls env))
                  (sort (sort~td-sort obj))
		  (term (sort~td-term obj))
		  (typedsort (sort=create-sort-with-type sort (term~type term)))
		  (head (data~top (data~n-range term)))
		  (key (if (member head (data~n-domain term) :test #'data~equal) nil head)))
	     (setf (sort~td-sort obj) typedsort)
	     (labels ((insert-td (so)  
				 (when (sort~functional-p so)
				   (insert-td (data~n-range so)))
				 (let* ((keylist (gethash so tdhash))
					(term-decls (cons obj (second (assoc key keylist)))))
				   (setf (gethash (data~constant-origin so) tdhash)
					 (acons key (list term-decls) (remove key keylist
								       :key #'car :count 1))))))
	       (insert-td (data~n-range sort)))))
  (:method ((obj cons) (env sort+environment) &key termdecl)
	   (cond
	     (termdecl
	      (sort~env-enter (make-instance 'sort+termdecl
					     :term (first obj)
					     :sort (second obj)
					     :theorem (third obj)) env)))))

(defun sort~env-sorts (env &key (recursive t))
  (let ((sorts))
    (maphash #'(lambda (label obj) (push obj sorts)) (sort=env-sorts env))
    (if recursive
	(remove-duplicates (append sorts (mapcan #'sort~env-sorts (sort=env-parents env))))
      sorts)))

(defun sort~env-termdecls (env &key (recursive t))
  (let ((tds))
    (maphash #'(lambda (label obj) (push (apply #'append
				    (mapcar #'second obj)) tds))
	     (sort=env-termdecls env))
    (let ((tdss (apply #'append tds)))
    (if recursive
	(remove-duplicates (append tdss (mapcan #'sort~env-termdecls (sort=env-parents env))))
      tdss))))

(defgeneric sort=real-name (name)
  (:method ((name data+constant))
	   (sort=real-name (keim~name name)))
  (:method ((name symbol))
	   (read-from-string (string name))))

(defgeneric sort=create-sort-with-type (sort type)
  (declare (edited  "20-FEB-2000")
	   (authors Pollet)
	   (input   "A SORT and a TYPE.")
	   (effect  "none")
	   (value   "Creates a (functional) with TYPE, when SORT contains"
                    "polymorphic sorts the typevariables will be instantiated"
                    "with respect to TYPE."
                    "Error, when a sort with TYPE cannot be created."))
  (:method ((sort sort+abstr)(type type+func))
	  (data~abstr-create
	   (sort=create-sort-with-type
		   (data~abstr-c-domain sort)
		   (data~abstr-c-domain type))
	   (sort=create-sort-with-type (data~abstr-c-range sort)
				       (data~abstr-c-range type))))
  (:method ((sort sort+constant)(type type+type))
	   (if (data~equal (data~annotation sort) type)
	       sort
	     (error ";;Typeclash with sort: ~A type: ~A" sort type)))
  (:method ((sort sort+polyconst)(type type+type))
	   (let ((origin (data~constant-origin sort)))
	     (if (type~alpha-match (data~annotation origin) type)
		 (let ((new-sort-const (if (type~variables type)
					   (make-instance 'sort+polyconst :name (keim~name sort))
					 (make-instance 'sort+constant :name  (keim~name sort)))))

		   (setf (data~annotation new-sort-const) type)
		   (setf (data~constant-origin new-sort-const) origin)
		   new-sort-const)
	       (error ";;Typeclash with sort: ~A type: ~A" sort type)))))

(defgeneric sort~lookup-sort (post-sort env)
  (declare (edited  "15-FEB-2000" "24-JUL-1995 17:47")
	   (authors Pollet GKLEIN)
	   (input   "A POST representation of an existing SORT and an environment ENV.")
	   (effect  "None.")
	   (value   "The sort is looked up in the environment and a sort is returned."
		    "NIL if the sort is not in the environment."))
  (:method ((post-sort list) (env sort+environment))
	   (cond ((= (length post-sort) 1)
		  (sort~lookup-sort (car post-sort) env))
		 ((and (>= (length post-sort) 3) (symbolp (first post-sort)) (string= (first post-sort) "IS"))
		  (sort~intersection-create (mapcar #'(lambda (x) (sort~lookup-sort x env)) (cdr post-sort) env)))
		 (t (if (consp post-sort)
			(data~abstr-create (mapcar #'(lambda (x) (sort~lookup-sort x env)) (reverse (cdr post-sort)))
					   (sort~lookup-sort (car post-sort) env))
		      (error "complex sort ~S should be a cons" post-sort)))))
  (:method ((post-sort symbol) (env cons))
	   (or (gethash (sort=real-name post-sort) (sort=env-sorts (car env)))
	       (sort~lookup-sort post-sort (cdr env))
	       (sort~lookup-sort post-sort (car env))))
  (:method ((post-sort symbol) (env null))
	   nil)
  (:method ((post-sort T) (env T))
	   nil)
  (:method ((post-sort symbol) (env sort+environment))
	   (let ((obj (or (gethash (sort=real-name post-sort) (sort=env-sorts env))
			  (sort~lookup-sort post-sort (sort=env-parents env))))) obj)))
  
(defgeneric sort~sort-of-pred (pred env)
  (declare (edited  "15-MAR-2000")
	   (authors Pollet)
	   (input   "A term and a sorted environment.")
	   (effect  "none")
	   (value   "If there is a sort, that corresponds to the term,"
		    "then the (functional) sort will be returned, else nil."
		    "Note that terms that represent functional sort are"
		    "constructed via a constructor-term (sort*constructor)."))
  (:method ((pred term+appl) (env sort+environment))
	   (let ((functor (env~lookup-object sort*constructor (sort=env-env env))))
	     (when (and functor (data~schema-equal functor (data~appl-function pred)))
	       (data~abstr-create (sort~sort-of-pred (first (data~appl-arguments pred)) env)
				  (sort~sort-of-pred (second (data~appl-arguments pred)) env)))))
  (:method ((pred term+schema) (env sort+environment))
	   (sort~sort-of-pred (data~schema-range pred) env))
  (:method ((pred data+struct) (env sort+environment))
	   nil)
  (:method ((pred term+constant) (env sort+environment))
	   (let ((sort (sort~lookup-sort (keim~name pred) env)))
	     (when sort 
	       (sort=create-sort-with-type sort
					   (data~abstr-c-domain (term~type pred)))))))

;;;
;;;
;;;

(defmethod data~abstr-create :around (domain (range sort+sort)
					     &key kappa destructive mode
					     outer-kappas type-var-subst)
  (change-class (call-next-method) 'sort+abstr))

;;;
;;;
;;;

(defun sort=type-with-new-vars (type)
  (let ((vars (type~variables type)))
    (if vars
	(subst~apply  (subst~create vars
				    (mapcar #'(lambda (var)
						(type~variable-create (etypecase (keim~name var)
									(symbol (gentemp(format nil "~A" (keim~name var))))
									(string (gentemp (keim~name var))))))
					    vars))
		      type)
      type)))

(defgeneric sort~type-of-sort (sort)
  (:method ((sort sort+polyconst))
	   (if (eq (data~constant-origin sort) sort)
	       (sort=type-with-new-vars (data~annotation sort))
	     (data~annotation sort)))
  (:method ((sort sort+constant))
	   (data~annotation sort))
  (:method ((sort data+abstr))
	   (if (data~annotation sort) (data~annotation sort)
	     (let* ((binder (data~abstr-n-domain sort))
		    (binder-types (mapcar #'sort~type-of-sort binder))
		    (range-type (sort~type-of-sort (data~abstr-n-range sort))))
	       (data~abstr-create binder-types range-type)))))

;;; Note: The length of a functional base sort is 0 in contrast to the length of
;;;       the underlying type.

(defgeneric sort~sort-length (sort)
  (:method ((sort sort+constant))
	   0)
  (:method ((sort data+abstr))
	   (length (data~abstr-n-domain sort))))

(defun sort~type-length (sort)
  (let ((styp (sort~type-of-sort sort)))
    (if (data~abstr-p styp)
	(length (data~abstr-n-domain (sort~type-of-sort sort)))
      0)))

;;; Term declarations of the form (TERM SORT) are attached to the n-range sort of
;;; SORT which is a constant sort.
;;; For example, let SORT be the sort (A1,..,An) -> A.
;;; Then the term declaration (TERM SORT) will be attached to the constant sort A.


(defun sort=term-decls (sort env)
  (remove-duplicates (append (gethash (data~constant-origin sort) (sort=env-termdecls env))
			     (mapcan #'(lambda (nextenv)
					 (sort=term-decls sort nextenv))
				     (sort=env-parents env)))))

(defun sort~top-term-decls (sort top env)
  (declare (edited  "07-FEB-1996 09:45")
	   (authors GKLEIN)
	   (input   "A constant sort SORT and a constant term TOP-SYMBOL.")
	   (effect  "None.")
	   (value   "A list of all term declarations which have SORT as their n-range sort"
		    "and which have TOP-SYMBOL as their top symbol."
		    "If TOP-SYMBOL is NIL then all term declarations which are projections."
		    "will be returned."))
  (mapcan #'(lambda (td) (sort=type-compatible sort td))
	 (mapcar  #'(lambda (tds) (when (or (and (null (car tds))(null top))
					    (and (car tds) top (data~equal (car tds) top)))
			     (second tds))) (sort=term-decls sort env))))

(defun sort~all-term-decls (sort env)
  (declare (edited  "07-FEB-1996 10:18")
	   (authors GKLEIN)
	   (input   "A constant sort SORT.")
	   (effect  "None.")
	   (value   "A list of all term declarations which have SORT as their n-range sort."))
  (mapcan #'(lambda (td) (sort=type-compatible sort td))
	 (mapcar #'(lambda (term-decl) (second term-decl))
		 (sort=term-decls sort env))))

(defun sort~all-imit-term-decls (sort env)
  (declare (edited  "15-MAR-1996 11:17")
	   (authors GKLEIN)
	   (input   "A constant sort SORT.")
	   (effect  "None.")
	   (value   "A list of all term declarations which have SORT as their n-range sort,"
		    "and which are not projection terms."))
  (mapcan #'(lambda (td) (sort=type-compatible sort td))
	 (mapcar #'(lambda (term-decl)
		     (when (car term-decl) ; imitation term declarations have a non NIL key
		       (second term-decl)))
		 (sort=term-decls sort env))))

(defun sort=type-compatible (sort tds)
  (mapcan #'(lambda (td) (let* ((tdsort (sort~td-sort td))
				(tdtype (sort~type-of-sort tdsort))
				(subst  (type~alpha-match (sort~type-of-sort (data~n-range tdsort))
							  (sort~type-of-sort sort))))
			   (cond
			    ((null subst) nil)
			    ((or (null (type~variables tdtype)) (subst~empty-p subst))
			     (list td))
			    (subst
			     (list (make-instance 'sort+termdecl
						  :term (Data~replace-free-variables
							 (data~copy (sort~td-term td) :downto '(type+type))
							 (subst~domain subst)
							 (subst~codomain subst))
						  :sort (sort=create-sort-with-type
							 tdsort
							 (subst~apply subst tdtype))
						  :theorem (sort~td-theorem td)))))))
	  tds))

#{
In order to obtain the domain and range sort information of a functional constant sort,
there exists the following functions which enable the access to that information.
#}

(defgeneric sort~c-domain (sort)
  (declare (edited  "02-FEB-1996 10:32")
	   (authors GKLEIN Pollet)
	   (input   "A functional sort.")
	   (effect  "None.")
	   (value   "The c-domain sort of sort."))
  (:method ((sort sort+constant))
	       (error "sort is a constant"))
  (:method ((sort data+abstr))
	   (data~abstr-c-domain sort)))

(defgeneric sort~c-range (sort)
  (declare (edited  "02-FEB-1996 10:35")
	   (authors GKLEIN Pollet)
	   (input   "A functional sort.")
	   (effect  "None.")
	   (value   "The c-range sort of sort."))
  (:method ((sort sort+constant))
	       (error "sort is a constant"))
  (:method ((sort data+abstr))
	   (data~abstr-c-range sort)))

(defgeneric sort~j-c-range (j sort)
  (declare (edited  "02-FEB-1996 10:39")
	   (authors GKLEIN Pollet)
	   (input   "A nonnegative number J and a sort SORT.")
	   (effect  "None.")
	   (value   "The J-th c-range sort of SORT."))
  (:method ((j (eql 0)) sort)
	   sort)
  (:method ((j number) (sort sort+constant))
	   (error "sort is a constant"))
  (:method ((j number) (sort data+abstr))
	   (let* ((n-domain (data~abstr-n-domain sort))
		  (l (length n-domain)))
	     (cond ((< j l)
		    (data~abstr-create (subseq n-domain j) (data~abstr-n-range sort)))
		   ((= j l)
		    (data~abstr-n-range sort))
		   (t
		    (sort~j-c-range (- j l) (data~abstr-n-range sort)))))))

(defgeneric sort~j-c-domain (j sort)
  (declare (edited  "02-FEB-1996 10:41")
	   (authors GKLEIN Pollet)
	   (input   "A nonnegative number J and a sort SORT.")
	   (effect  "None.")
	   (value   "The J-th c-domain sort of SORT."))
  (:method ((j (eql 0)) sort)
	   sort)
  (:method ((j number) (sort sort+constant))
	   (error "sort is a constant"))
  (:method ((j number) (sort data+abstr))
	   (let* ((n-domain (data~abstr-n-domain sort))
		  (l (length n-domain)))
	     (if (<= j l)
		 (elt n-domain (- j 1))
	       (sort~j-c-domain (- j l) (data~abstr-n-range sort))))))

(defgeneric sort~np-domain (sort m)
  (declare (edited  "02-FEB-1996 10:54")
	   (authors GKLEIN)
	   (input   "A sort SORT and a nonnegative number M.")
	   (effect  "None.")
	   (value   "The list of the M first domain sorts of SORT."
		    "Functional base sorts have been broken when needed."))
  (:method (sort (m (eql 0)))
	   (declare (ignore sort))
	   nil)
  (:method (sort (m number))
	   (let* ((dom (sort~c-domain sort))
		  (rest (sort~np-domain (sort~c-range sort) (- m 1))))
	     (cons dom rest))))

;(defun sort~complete-broken-n-domain (sort)
;  (declare (edited  "23-FEB-1996 16:23")
;           (authors GKLEIN)
;           (input   "A sort SORT.")
;           (effect  "None.")
;           (value   "The complete broken n-domain of the sort SORT."))
;  (sort~np-domain sort (sort~type-length sort)))
;
;(defun sort~complete-broken-n-range (sort)
;  (declare (edited  "26-FEB-1996 11:06")
;           (authors GKLEIN)
;           (input   "A sort SORT.")
;           (effect  "None.")
;           (value   "The m-th n-range sort of SORT, if m is the length of the type of SORT."
;                    "Functional n-range base sorts have been broken whenever needed."))
;  (sort~j-c-range (sort~type-length sort) sort))

#{
Let us consider the two sorts A and B where A is a constant sort and B is arbitrary.
Now we want to know if there exists some n-range sort of B which is eq to the sort A.
Furthermore we are interested at the index m such that \({\frak r}^m(B)=A\).
The function sort~n-range-rel does this work.
Instead of the test predicate eq some other test predicate is possible like sort~subsort-p for instance,
in order to get a m such that \({\frak r}^m(B)\subseteq A\).
#}

;(defgeneric sort~n-range-rel (sortA sortB &key test)
;  (declare (edited  "13-NOV-1995 15:49")
;           (authors GKLEIN)
;           (input   "A constant sort sortA, an arbitrary sort sortB and a test predicate TEST (default is eq).")
;           (effect  "None.")
;           (value   #{If there exists a number m with \(({\frak r}^m(sortB),sortA)\in TEST\) #}
;                    "then sort~n-range-rel returns m, otherwise nil."))
;  (:method ((sortA sort+constant) (sortB data+abstr) &key (test #'eq))
;           (let* ((n-range (data~abstr-n-range sortB))
;                  (k (length (data~abstr-n-domain sortB)))
;                  (m (sort~n-range-rel sortA n-range :test test)))
;             (if m
;                 (+ k m)
;               nil)))
;  (:method ((sortA sort+constant) (sortB sort+constant) &key (test #'eq))
;           (if (funcall test sortB sortA)
;               0
;             (if (not (sort~functional-p sortB))
;                 nil
;               (let ((m (sort~n-range-rel sortA (sort~c-range sortB) :test test)))
;                 (if m
;                     (+ m 1)
;                   nil)))))
;  (:method ((sortA sort+sort) (sortB sort+sort) &key (test #'eq))
;           (declare (ignore test))
;           nil))

(defgeneric sort~n-range-rel (sortA sortB &key test)
  (:method ((sortA sort+constant) (sortB data+abstr) &key (test #'eq))
           (let* ((n-range (data~abstr-n-range sortB))
                  (k (length (data~abstr-n-domain sortB)))
                  (m (sort~n-range-rel sortA n-range :test test)))
             (if m
                 (+ k m)
               nil)))
  (:method ((sortA sort+constant) (sortB sort+constant) &key (test #'eq))
           (if (funcall test sortB sortA)
               0
             nil))
  (:method ((sortA sort+sort) (sortB sort+sort) &key (test #'eq))
           (declare (ignore test))
           nil))

;;;
;;;
;;;

(Defun sort~n-range-type-rel (sortA sortB &key (test #'eq))
   (declare (ignore test))
  (let* ((typeA (sort~type-of-sort sortA))
         (typeB (sort~type-of-sort sortB))
         (ndomA (data~n-domain typeA))
         (ndomB (data~n-domain typeB)))
    (cond ((> (length ndomA) (length ndomB))
           nil)
          (t
           (let* ((m (- (length ndomB) (length ndomA)))
                  (ndomB-rest (subseq ndomB m)))
             (if (and (every #'data~equal ndomA ndomB-rest)
                      (data~equal (data~n-range typeA) (data~n-range typeB)))
                 m
               nil))))))
    
;;;
;;; Predicates on sorts
;;;

(defun sort~sort-p (thing)
  (typep thing 'sort+sort))

(defgeneric sort~functional-p (sort)
  (:method ((sort sort+constant))
	     nil)
  (:method ((sort data+abstr))
	   t))

;;;
;;; Subsorts
;;;


;;;
;;; Intersection-sorts
;;;

#{
Intersection sorts are sets of sorts.
#}

;;;
;;; The members of a intersection sort should be minimal by the subsort relation sort~subsort-p.
;;;

(defgeneric sort~intersection-create (thing env)
  (declare (edited  "18-MAR-1996 11:37")
	   (authors GKLEIN)
	   (input   "A list of sorts.")
	   (effect  "None.")
	   (value   "An intersection sort if the cardinality of the member sorts minmized by sort~subsort-p"
		    "is greater than 2. If the set of member sorts consists of exactly one element then"
		    "this sort is returned (and not an instance of sort+intersection !)."))
  (:method ((thing list) (env sort+environment)) 
	   (let* ((all-sorts (apply #'append (mapcar #'(lambda (sorti)
							 (if (sort~intersection-p sorti)
							     (sort~intersection-members sorti)
							   (list sorti)))
						     thing)))
		  (rem-sorts (remove-duplicates (remove-if #'(lambda (sort)
							       (some #'(lambda (sortj)
									 (and (not (data~equal sort sortj))
									      (sort~subsort-p sortj sort env)))
								     all-sorts))
							   all-sorts)
						:test #'data~equal)))
	     (if (> (length rem-sorts) 1)
		 (make-instance 'sort+intersection
				:members rem-sorts)
	       (first rem-sorts))))
  (:method (thing env)
           (error "~A is not a list of sorts." thing)))

;;;
;;; predicates on intersection
;;;

(defun sort~intersection-p (thing)
  (typep thing 'sort+intersection))


;;;
;;; type and length of intersection sorts 
;;;

(defmethod sort~type-of-sort ((sort sort+intersection))
  (sort~type-of-sort (car (sort~intersection-members sort))))

(defmethod sort~sort-length ((sort sort+intersection))
  (apply #'min (mapcar #'sort~sort-length (sort~intersection-members sort))))

;;;
;;;
;;;

(defgeneric sort~intersection-nf (sort)
  (:method ((sort sort+intersection))
	   (let* ((member-sorts (sort~intersection-members sort))
		  (m (sort~sort-length sort))
		  (domsortlists (mapcar #'(lambda (sorti) (subseq (data~n-domain sorti) 0 m)) member-sorts))
		  (d (sort=max-sort-eq domsortlists)))
	     (cond ((= d 0)
		    sort)
		   (t
		    (let* ((new-dom (subseq (data~n-domain (first member-sorts)) 0 d))
			   (new-members (mapcar #'(lambda (sorti)
						    (let ((new-dom (subseq (data~n-domain sorti) d))
							  (new-ran (data~n-range sorti)))
						      (if new-dom
							  (data~abstr-create new-dom new-ran)
							new-ran)))
						member-sorts))
			   (new-ran (sort~intersection-create new-members)))
		      (if new-dom
			  (data~abstr-create new-dom new-ran)
			new-ran))))))
  (:method ((sort sort+sort))
	   sort))
	   
(defun sort=max-sort-eq (sortlist)
  (cond ((null sortlist)
	 0)
	((null (car sortlist))
	 0)
	(t
	 (let ((first-elems (mapcar #'first sortlist))
	       (rest-list (mapcar #'cdr sortlist)))
	   (if (every #'(lambda (sort) (data~equal (first first-elems) sort)) (cdr first-elems))
	       (+ 1 (sort=max-sort-eq rest-list))
	     0)))))

;;;
;;;
;;;

(defmethod data~n-range ((sort sort+intersection))
  sort)

(defmethod data~n-domain ((sort sort+intersection))
  (let* ((slen (sort~sort-length sort))
	 (sorts (sort~intersection-members sort))
	 (sorts-doms (mapcar #'(lambda (sortm) (subseq (data~n-domain sortm) 0 slen)) sorts))
	 (l (sort=max-sort-eq sorts-doms)))
    (subseq (first sorts-doms) 0 l)))

;;;
;;; POST input
;;; 

(defmethod post~read-object (constant  (env sort+environment) 
			     (indicator (eql :sort-constant)))
  (let ((sort (if (and (string= (first constant) sort*sortpred)
		       (> (length constant) 1))
		   (second constant) (first constant))))
    (sort=read-constant sort env)
    (sort~lookup-sort sort env)))

(defun sort=read-constant (sort-symbol env)
  (declare (edited  "19-JUL-1995 17:22")
	   (authors GKLEIN)
	   (input "a sort constant declaration (sort-symbol type-symbol) and an environment")
	   (effect "Declares the sort-symbol as a sort-constant with the type type-symbol.")
	   (value "undefined"))
  (let  ((sort (sort~lookup-sort sort-symbol env))
	 (unsortedenv (sort=env-env env)))
    (cond ((and sort (not (data~constant-p sort)))
	   (error "Can't declare ~S as a sort constant, because it already exists in the environment as a ~A. "
		  sort-symbol (class-name (class-of sort))))
	  (sort nil)
	  ((not (symbolp sort-symbol))
	   (error "Can't declare ~S as a sort constant, because it is not a symbol." sort-symbol))
	  (t
	   (let* ((type (sort=read-sort-typecheck sort-symbol (term~type (env~lookup-object sort-symbol unsortedenv))))
		  (new-sort-const (if (type~variables type)
				      (data~constant-create 'sort+polyconst :name sort-symbol)
				    (data~constant-create 'sort+constant :name sort-symbol))))
	     (setf (data~annotation new-sort-const) (sort=type-with-new-vars type))
	     (setf (data~constant-origin new-sort-const) new-sort-const)
	     (sort~env-enter new-sort-const env))))))

(defun sort=read-sort-typecheck (name predtype &optional sorttype)
    (cond ((data~schema-p predtype)
	   (sort=read-sort-typecheck name (data~schema-range predtype) sorttype))
          ((not (type~func-p predtype))
	   (error ";;;sort ~A: type ~A is not a function type"
		  name predtype))
	  ((not (eq (data~abstr-range predtype) (type~o)))
	   (error ";;;sort ~A: range of type ~A is not of kind o"
		  name predtype))
	  ((not (= (length (data~abstr-domain predtype)) 1))
	       (error ";;;sort ~A: domain of type ~A is unary"
		      name predtype))
	  ((and sorttype
		(not (type~alpha-match sorttype (car (data~abstr-domain predtype)))))
	   (error ";;;sort ~A: type ~A of the predicate incompatible to type ~A"
		  name predtype sorttype))
	  (T (car (data~abstr-domain predtype)))))

(defmethod post~read-object (term-decl (env sort+environment) (indicator (eql :termdecl)))
  (labels ((th2tdsort (term)
		      (cond ((and (atom (first term))(string= (first term) sort*tdquantor))
			     (append (last term) (th2tdsort (third (second term)))))
			    ((and (atom (first term))(string= (first term) sort*tdkappa))
			     (th2tdsort (car (last term))))
			    (t (butlast term))))
	   (th2tdterm (term)
		      (cond ((and (atom (first term))(string= (first term) sort*tdquantor))
			     (list (caadr term) (cadadr term) (th2tdterm (third (second term)))))
			    ((and (atom (first term))(string= (first term) sort*tdkappa))
			     (append (butlast term) (list (th2tdterm (car (last term))))))
			    (t (second term))))
	   (func2abstr (sort)
		       (cond ((and (consp sort)(atom (first sort))(string= (first sort) sort*constructor))
			      (func2abstr (reverse (rest sort))))
			     ((consp sort)
			      (mapcar #'func2abstr sort))
			      (t sort))))
    ;(format t "~%term ~A"  (th2tdterm (first term-decl)))
    ;(format t "~%sort ~A" (reverse (th2tdsort (first term-decl))))
    (sys~handler-case
     (let* ((post-term (th2tdterm (car term-decl)))
	    (post-sort (func2abstr (reverse (th2tdsort (car term-decl)))))
	    (term (post~read-object post-term (sort=env-env env) :existing-term)) 
	    (realterm  (if (data~schema-p term) (data~schema-range term) term))
	    (sort (sort~lookup-sort post-sort env))
	    (theo (second term-decl)))    
       (if (and term sort)
	   (let* ((sorttype (sort~type-of-sort sort))
		  (termtype (term~type realterm)))
	     (if (type~alpha-match sorttype termtype)
		 (sort~env-enter (list realterm sort theo) env :termdecl t)
	       (error "Types of sort ~A and term ~A are incompatible" post-sort post-term)))
	 (error "Could not read POST representation ~A of term or sort." term-decl)))
     (error (cond) (error ";;;Termdecl formula cannot be transformed: ~A" cond)))))

;;;
;;; POST output
;;; 

(defmethod post~print ((sort sort+primitive) stream)
  (format stream "~A" (keim~name sort)))

(defmethod post~print ((type sort+complex) stream)
  (format stream "(")
  (post~print (data~abstr-range type) stream)
  (format stream " ")
  (post~print (reverse (data~abstr-domain type)) stream)
  (format stream ")"))

;;;
;;; print-object;;;

(defmethod print-object ((primitive sort+constant) stream)
  (case print*appearance
    ((verbose) (format stream "~:[unnamed (!) Constant~;~:*Constant ~S~]" (keim~name primitive)))
    ((discriminate) (format stream "~(~S~)" (keim~name primitive)))
    (t (format stream "~S" (keim~name primitive)))))

(defmethod print-object ((primitive sort+termdecl) stream)
  (case print*appearance
    ;((verbose) (format stream "~:[unnamed (!) Constant~;~:*Constant ~S~]" (keim~name primitive)))
    ;((discriminate) (format stream "~(~S~)" (keim~name primitive)))
    (t (format stream "~S::~S" (sort~td-term primitive)(sort~td-sort primitive)))))

;(defmethod print-object ((primitive sort+subdecl) stream)
;  (case print*appearance
;    ;((verbose) (format stream "~:[unnamed (!) Constant~;~:*Constant ~S~]" (keim~name primitive)))
;    ;((discriminate) (format stream "~(~S~)" (keim~name primitive)))
;    (t (format stream "~S<~S" (sort~sd-subsort primitive)(sort~sd-supersort primitive)))))

(defmethod print-object ((abstr sort+abstr) stream)
  (declare (special *print-level* *print-length*))
  (case print*appearance
    (t (format stream "(~{ ~S ~}) -> ~A"
               (data~abstr-domain abstr)
               (data~abstr-range abstr)))))

(defmethod print-object ((thing sort+intersection) stream)
  (format stream "<~{ ~S ~}> " (sort~intersection-members thing)))

