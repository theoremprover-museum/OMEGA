;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1998 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@ags.uni-sb.de                                    ;;
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

  

(in-package  "KEIM")

(mod~defmod TYPE 
            :uses (beta bind data env keim post)
            :documentation "Classes and functions for types."
            :exports (
		      ;; Classes

		      type+appl
		      type+complex
		      type+constant
		      type+func
		      type+primitive
		      type+schema
		      type+type
		      type+variable
		      type+def
		      type+constructor
		      
		      ;; Creating Access 
		      type~def-p
		      type~create-type-def
		      type~constructor-p
		      type~constructor-create
		      type~constructor-arity	      
		      type~abstract
		      type~appl-create
		      type~appl-p
		      type~apply
		      type~bound-variables
		      type~complex-p
		      type~constant-create
		      type~constant-p
		      
		      type~create-primitive-in-environment
		      type~free-variables
		      type~func-create
		      type~func-p
		      type~function-create
		      type~not-free-occuring
		      type~not-schema
		      type~not-schema-p
		      type~order
		      type~p
		      type~predicate-create
		      type~primitive-p
		      type~schema-create
		      type~schema-p
		      type~symbols
		      type~type-variables
		      type~variable-create
		      type~variable-p
		      type~variables
		      
		      ;; Equalities
		      
		      type~alpha-equal
		      type~alpha-match
		      type~alpha-unify
		      
		      ;; Others
		      
		      type~i
		      type~i-p
		      type~o
		      type~o-p
		      type~list-p
		      type~list
		      type~num
		      type~num-p
		      type~cyc
		      type~tuple
		      type~multi-set-p
		      type~multi-set

		      type~generate-type-primitive-with-new-name
		      type~generate-new-name
		      type~expand-type-def
		      
		      ;; Constants
		      
		      type*i
		      type*o
		      type*num
		      type*list
		      type*cyc
		      type*tuple
		      ))

;; KEIM-3-REPLACEMENTS:
;; type~env-lookup                 -> use post~read-object
;; type~read-existing-type         -> use post~read-object
;; type~enter-type-constant        -> use tyep~create-primitive-in-environment
;; type~enter-type-variable        -> use tyep~create-primitive-in-environment
;; type~read-new-type-constants    -> use post~read-object
;; type~read-new-type-constant     -> use post~read-object 
;; type~read-new-type-variables    -> use post~read-object
;; type~read-new-type-variable     -> use post~read-object

;; =========================================================================
;; The Most General Type-Class
;; =========================================================================

(eval-when (load compile eval) 
  (defclass type+type (data+object data+struct)
    ()
    (:documentation "The class of all types in KEIM.")))

(defmethod print-object ((datum type+type) stream)
  (format stream "|Some type|"))

;; =========================================================================
;;                               Primitive Types
;; =========================================================================

(eval-when (load compile eval) 
  (defclass type+primitive (type+type data+primitive)
    ()
    (:documentation "The class of primitive (non complex) types.")))
  
(eval-when (load compile eval) 
  (defclass type+constant (type+primitive data+constant)
    ()
    (:documentation "The class of type-constants.")))
  
(eval-when (load compile eval) 
  (defclass type+variable (type+primitive data+variable)
    ()
    (:documentation "The class of type-variables.")))

(eval-when (load compile eval)
  (defclass type+constructor (type+constant)
    ((arity :initform nil
	    :initarg :arity
	    :accessor type~constructor-arity))
    (:documentation "The class of type-constructors.")))

(eval-when (load compile eval)
  (defclass type+def (type+constructor) 
    ((expansion :initform nil
		:initarg :expansion
		:accessor type~def-expansion))))

;;
;; We are not entirely sure whether the realization of type-definitions as type-constructors is really
;; the most suitable way.
;; Indeed they are used in a similar way: Both have arities (type-def's arities result from the number of
;; polymorphic type-variables they contain, e.g., in (set (all-types aa (o aa))) set has arity 1).
;; Hence, we assume (to formulate it very carefully) that they behave similarly technically, such that
;; we do not have to make major changes in all kinds of matchings, applications and so on.
;; However, they are also different with respect to the fact that type-defs are expanded ...
;;
;; AMeier, AFiedler
;;

;; =========================================================================
;;                               Complex  Types
;; ==========================================================================

(eval-when (load compile eval) 
  (defclass type+complex (type+type data+complex)
    ()
    (:documentation "The class of complex types.")))

(eval-when (load compile eval) 
  (defclass type+appl (type+complex data+appl)
    ()
    (:documentation "The class of applications of types.")))
  
(eval-when (load compile eval) 
  (defclass type+func (type+complex data+abstr) 
    ()
    (:documentation "The class of function types.")))

(eval-when (load compile eval) 
  (defclass type+schema (type+complex data+schema) 
    ()
    (:documentation "The class of function types.")))

;; =========================================================================
;;                               Create Funktionen 
;; =========================================================================

;; ----------------------        P R I M I T I V E

(defun type~variable-create (symbol)
  (data~variable-create 'type+variable :name symbol))

(defun type~constant-create (symbol)
  (let ((const (data~constant-create 'type+constant :name symbol)))
    (setf (data~constant-origin const) const)
    const))

(defun type~constructor-create (symbol arity)
  (let ((const (data~constant-create 'type+constructor :name symbol)))
    (setf (data~constant-origin const) const)
    (setf (type~constructor-arity const) arity)
    const))

(defun type~create-type-def (name type)
  (make-instance 'type+def
		 :name name
		 :arity (if (type~schema-p type)
			    (length (data~schema-domain type))
			  0)
		 :expansion type))

;; ---------------------- C O M P L E X E 

;; --------------------    Abstraktionen
(defun type~func-create (domain range &key destructive (mode :conserve))
  (data~abstr-create domain range :destructive destructive :mode mode))

(defmethod data~abstr-create :around (domain (range type+type) &key
					     destructive
					     (mode :n-normalize))
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "")
	   (effect  "")
	   (value   ""))
  (declare (ignore destructive mode domain))
  (change-class (call-next-method) 'type+func))

;; -------------------- Schemata
(defun type~schema-create (type &key domain (rename nil))
  (declare (edited  "02-NOV-1998")
           (authors Gebhard)
           (input   "A list of type vars and a type")
           (effect  "None")
           (value   "The (new) type, as kappa abstracted schema. Renmae will build a
renamed copy of type."))
  (let* ((copy (if rename
		   (data~copy type :downto (list 'data+constant))
		 type))      
	 (dom  (if domain
		   domain
		 (data~all-variables copy))))
    (if (and (every 'type~variable-p domain)
	     (type~not-schema type))
        (data~schema-create copy dom)
      (error "Kappas have to be t-vars and type has to be a none-schema."))))

(defmethod data~schema-create :around ((datum type+type) kappa &key
					     destructive
					     (mode :n-normalize))
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "See data~~schema-create")
	   (effect  "See data~~schema-create")
	   (value   "See data~~schema-craete"))
  (declare (ignore destructive mode kappa))
  (change-class (call-next-method) 'type+schema))

;; -------------------- Applikationen
(defun type~appl-create (function arguments &key
				            destructive
				            (mode :n-normalize))
  (data~appl-create function arguments :destructive destructive :mode mode))

(defmethod data~appl-create :around ((function type+type) arguments &key
				     destructive
				     mode)
  (declare (edited  "22-APR-1995")
	   (authors Kohlhase)
	   (input   "")
	   (effect  "")
	   (value   ""))
  (declare (ignore destructive mode arguments))
  (change-class (call-next-method) 'type+appl))

;; =========================================================================
;;                            P R I N T I N G 
;; =========================================================================

;; -------------------------  P R I M I T I V E

(defmethod print-object ((primitive type+primitive) stream)
  (declare (edited  "19-JAN-1995")
           (authors SORGE Fehrer))
  (case print*appearance
    ((verbose)
     (format stream
	     "~:[unnamed (!) primitive Typ~;~:*primitive type called ~S~]"
	     (keim~name primitive)))
    (t (format stream "~(~A~)" (keim~name primitive)))))

(defmethod print-object ((primitive type+variable) stream)
  (declare (edited  "19-JAN-1995")
           (authors SORGE Fehrer))
  (case print*appearance
    ((verbose)
     (format stream "~:[unnamed (!) Typevariable~;~:*Typevariable ~S~]"
	     (keim~name primitive)))
    ((or discriminate short&discriminate)
     (format stream "~:@(~A~)" (keim~name primitive)))
    (t
     (format stream "~(~A~)" (keim~name primitive)))))

(defmethod print-object ((primitive type+constant) stream)
  (declare (edited  "24-MAR-1998 16:28")
           (authors SORGE Fehrer))
 (case print*appearance
    ((verbose)
     (format stream "~:[unnamed (!) Typeconstant~;~:*Typeconstant ~S~]"
	     (keim~name primitive)))
    (t
     (format stream "~(~A~)" (keim~name primitive)))))


;; ----------------------------- C O M P L E X

;; --------------------------     Abstraktion

(defmethod print-object ((abstr type+func) stream)  
  (declare (edited  "17-NOV-1994")
           (authors SORGE Fehrer RICHTS))
  (case print*appearance
    ((or short&discriminate short) 
     (format stream "[~A" (data~abstr-range abstr))
     (map nil #'(lambda (x) (format stream " ~A" x))
	  (reverse (cdr (data~abstr-domain abstr))))
     (format stream " ~A]" (car (data~abstr-domain abstr))))
    (t (format stream "[~A" (car (data~abstr-domain abstr)))
       (map nil #'(lambda (x) (format stream " ~A" x))
	    (cdr (data~abstr-domain abstr)))
       (format stream "->~A]" (data~abstr-range abstr)))))

;; -------------- Applikation

(defmethod print-object ((appl type+appl) stream)    ;; Applikations 
  (declare (edited  "17-NOV-1994")
           (authors Fehrer RICHTS)
           (input   )
           (effect  )
           (value   )
           (special *print-length*))
  (case print*appearance
    (t (cond    ((or (null *print-length*) 
                     (> *print-length* (length (data~appl-arguments appl))))
                 (format stream "(~A~{ ~A~})" (data~appl-function appl) 
                         (data~appl-arguments appl)))
                (t (format stream "(~A~{ ~A~} ...)"
                           (data~appl-function appl)
                           (subseq (data~appl-arguments appl) 0
				   (1- *print-length*))))))))


(defmethod print-object ((scheme type+schema) stream)  
  (declare (edited  "17-NOV-1994")
           (authors SORGE Fehrer RICHTS))

  (format stream "<~A" (car (data~schema-domain scheme)))

  (map nil #'(lambda (x) (format stream " ~A" x))
       (cdr (data~schema-domain scheme)))
  (format stream ">_~A" (data~schema-range scheme)))


;; =========================================================================
;;                           Specials i and o
;; =========================================================================

(defvar type*i (type~constant-create 'i))
(defvar type*o (type~constant-create 'o))
(defvar type*num (type~constant-create 'num))
(defvar type*list (type~constant-create 'list))
(defvar type*cyc (type~constant-create 'cyc))
(defvar type*tuple (type~constructor-create 'tuple 2))
(defvar type*multi-set (type~constant-create 'multi-set))

;; This type is a dummy type for cartesian product.
;; It is needed for defining methods.

(defun type~i () type*i)
(defun type~o () type*o)
(defun type~num () type*num)
(defun type~list () type*list)
(defun type~cyc () type*cyc)
(defun type~tuple () type*tuple)
(defun type~multi-set () type*multi-set)


(defun type~i-p (type)
  (declare (edited  "23-AUG-1991 12:24")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is the primitive type i."))
  (eq type*i type))

(defun type~o-p (type)
  (declare (edited  "27-JAN-1992 12:24")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is the primitive type o."))
  (eq type*o type))

(defun type~list-p (type)
  (declare (edited  "09-APR-1998")
	   (authors Lassaad)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is the primitive type list."))
  (eq (type~list) type))

(defun type~num-p (type)
  (declare (edited  "09-APR-1998")
	   (authors Lassaad)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is the primitive type list."))
  (eq (type~num) type))

(defun type~multi-set-p (type)
  (declare (edited  "14-JUL-2004" "09-APR-1998")
	   (authors Vxs Lassaad)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is the primitive type multi-set."))
  (eq (type~multi-set) type))

;; =========================================================================
;;                             Testing
;; =========================================================================

(defun type~p (typ)
  (typep typ 'type+type))

(defun type~primitive-p (typ)
  (typep typ 'type+primitive))

(defun type~variable-p (typ)
  (typep typ 'type+variable))

(defun type~constant-p (typ)
  (typep typ 'type+constant))

(defun type~complex-p (typ)
  (typep typ 'type+complex))

(defun type~func-p (typ)
  (typep typ 'type+func))

(defun type~appl-p (typ)
  (typep typ 'type+appl))

(defun type~schema-p (typ)
  (typep typ 'type+schema))

(defun type~constructor-p (typ)
  (typep typ 'type+constructor))

(defun type~def-p (obj)
  (typep obj 'type+def))

(defun type~not-schema (typ)
  (declare (edited  "02-NOV-1998")
	   (authors Gebhard)
	   (input   "A type")
	   (effect  "None")
	   (value   "T iff typ is not a type+schema."))
  (not (typep typ 'type+schema)))

(defun type~not-schema-p (typ)
  (declare (edited  "02-NOV-1998")
	   (authors Gebhard)
	   (input   "A type")
	   (effect  "None")
	   (value   "T iff typ is a type but not a type+schema."))
  (and (typep typ 'type+type)
       (not (typep typ 'type+schema))))

;; =========================================================================
;;                         ApplyING Types
;; =========================================================================

(data~defgeneric type~not-free-occuring (typevar (datum) &optional boundvars)
  (declare (edited  "21-JAN-1998")
	   (authors Gebhard)
	   (input   "A type-var and a datum")
	   (effect  "None")
	   (value   "True if the variable doesn't occur kappa-free in datum"))
  (:method (typevar (datum type+func) &optional boundvars)
	   (type~not-free-occuring typevar
				   (cons (data~abstr-range datum)
					 (data~abstr-domain datum))
				   boundvars))
  (:method (typevar (datum type+appl) &optional boundvars)
	   (type~not-free-occuring typevar
				   (cons (data~appl-function datum)
					 (data~appl-arguments datum))
				   boundvars))
  (:method (typevar (datum type+constant) &optional boundvars)
	   (declare (ignore typevar boundvars))
	   t)
  (:method (typevar (datum type+variable) &optional boundvars)
	   (or (find typevar boundvars :test #'data~equal)
	       (not (eq typevar datum))))
  (:method (typevar (datum list) &optional boundvars)
	   (every #'(lambda (x) (type~not-free-occuring typevar x boundvars))
		  datum))
  (:method (typevar (datum type+schema) &optional boundvars)
	   (let ((new-bound (append (data~schema-domain datum)
				    boundvars)))
	     (type~not-free-occuring typevar
				     (data~schema-range datum)
				     new-bound))))

;; Eine Der beiden naechsten Funktionen sollte ersatzlos gestrichen werden koennen AM
(data~defgeneric type~variables ((type))
   (declare (edited  "05-DEC-1997")
	    (authors Gebhard)
	    (input   "A Type. not a schema")
	    (effect  "None")
	    (value   "The list of all variables occuring in the type"))
   (:method ((type type+variable)) (list type))
   (:method ((type type+constant)) nil)
   (:method ((type type+func))
	    (remove-duplicates
	     (append (mapcan 'type~variables (data~abstr-domain type))
                        (type~variables (data~abstr-range type)))))
   (:method ((type type+appl))
	    (remove-duplicates
	     (apply 'append (mapcar 'type~variables
				    (cons (data~appl-function type)
					  (data~appl-arguments type))))))
   (:method ((type type+schema))
	    (type~variables (data~schema-range type)))
   (:method ((tylist list))
	    (remove-duplicates
	     (mapcan 'type~variables tylist))))


(defgeneric type~type-variables (type)
  (declare (edited  "10-DEC-1997")
	   (authors Ameier)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "A list of all type-variables in the type."))
  (:method ((type type+constant))
	   nil)
  (:method ((type type+variable))
	   (list type))
  (:method ((type type+func))
	   (remove-duplicates
	    (append (type~type-variables (data=abstr-range type))
		    (apply 'append (mapcar #'type~type-variables (data=abstr-domain type))))))
  (:method ((type type+appl))
	   (remove-duplicates
	    (append (type~type-variables (data~appl-function type))
		    (apply 'append (mapcar #'type~type-variables (data~appl-arguments type))))))
  (:method ((type type+schema))
	   (remove-duplicates
	    (append (data~schema-domain type)
		    (type~type-variables (data~schema-range type)))))
  (:method ((types list))
	   (remove-duplicates
	    (apply #'append (mapcar #'type~type-variables types))))
  (:method (type)
	   (error "KEIM: type~~type-variables::~A is not of type TYPE+PRIMITIVE or TYPE+FUNC" type)))


(defun type=possible-arguments-number (type-abstr)
  (let* ((direct-length (length (data~abstr-domain type-abstr))))
    (if (data~abstr-p (data~abstr-range type-abstr))
	(+ direct-length (type=possible-arguments-number (data~abstr-range type-abstr)))
      direct-length)))

;; Comment by AMEIER:
;; Added the if (> (length type3) (length (data~abstr-domain (if (data~schema-p type1)
;;							     (data~abstr-range type1)
;;							   type1))))
;; since otherwise things such as (type~apply (o (o o o) (o o o)) ((o o o) (o o o) (o o o))) were accepted!
(defun type~apply (type1 type2 &key substitution)
  (if (or (data~schema-p type1)
	  (if (listp type2)
	      (not (some 'data~schema-p type2))
	    (not (data~schema-p type2))))
      (let* ((type3  (if (listp type2)
			 type2
		       (list type2))))
	(if (> (length type3) (type=possible-arguments-number (if (data~schema-p type1)
								  (data~abstr-range type1)
								type1)))
	    (error "To many args in type-apply.")
	  (let* ((bindables (type~type-variables (cons type1 type3)))
		 (result (multiple-value-bind
			     (res-data res-subst)
			     (bind~with-bindings
			      ((let ((normalized
				      (beta~head-normalize-with-args-and-bindings
				       nil
				       type1
				       type3
				       bindables)))
				 (bind~insert-bindings! normalized :local T)))
			      :insert nil)
			   (list res-data res-subst)))
		 (substed-result (subst~apply (second result) (first result))))
	    (if substitution
		(list substed-result (second result))
	      substed-result))))
    (error "Please do not apply type-schemata.")))

;; =========================================================================
;;                       Sonstiges
;; =========================================================================

(defgeneric type~expand-type-def (type)
  (declare (edited  "20-FEB-2001")
	   (authors Ameier)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "A type resulting from the input type by the expansion of all type-defs contained in the input type."))
  (:method ((type type+def))
	   (type~def-expansion type))
  (:method ((type type+appl))
	   (let* ((func (data~appl-function type))
		  (args (data~appl-arguments type))
		  (ex-func (type~expand-type-def func))
		  (ex-args (mapcar #'type~expand-type-def args)))
	     (if (type~schema-p ex-func)
		 (let* ((scope (data~schema-range ex-func))
			(vars (data~schema-domain ex-func))
			(subst (subst~create vars ex-args)))
		   (subst~apply subst scope))
	       (type~appl-create ex-func ex-args))))
  (:method ((type type+func))
	   (let* ((domain (data~abstr-domain type))
		  (range (data~abstr-range type)))
	     (type~func-create (mapcar #'type~expand-type-def domain)
			       (type~expand-type-def range))))
  (:method ((type type+schema))
	   (type~schema-create (type~expand-type-def (data~schema-range type))
			       :domain (data~schema-domain type)))
  (:method ((type type+type))
	   type))

(defun type~function-create (arity)
  (declare (edited  "08-AUG-1991 11:13")
	   (authors RICHTS)
	   (input   "A number n.")
	   (effect  "None.")
	   (value   "The structure representing the type (i1,...,in) -> i.")
	   (example "3 --> (i,i,i) -> i"))
  (if (zerop arity)
      type*i
    (type~func-create (make-list arity
				 :initial-element type*i)
		      type*i)))


(defun type~predicate-create (arity)
  (declare (edited  "01-APR-1998" "08-AUG-1991 11:13")
	   (authors Gebhard RICHTS)
	   (input   "A number n.")
	   (effect  "None.")
	   (value   "The structure representing the type (i1,...,in) -> o.")
	   (example "3 --> (i,i,i) -> o"))
  (if (zerop arity)
      type*o
    (type~func-create (make-list arity
				 :initial-element type*i)
		      type*o)))

(defgeneric type~order (type)
  (declare (edited  "20-MAY-1996 09:23")
	   (authors GKLEIN)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "The order of the type of the datum."))
  (:method ((type type+primitive))
	   0)
  (:method ((type type+func))
	   (1+ (max (type~order (data~abstr-c-domain type))
		    (type~order (data~abstr-c-range type)))))
  (:method ((type type+appl))
	   (1+ (max (type~order (data~appl-function type))
		    (max (car (mapcar 'type~order (data~appl-arguments type)))))))
  (:method ((type type+schema))
	   (type~order (data~schema-range type)))
  (:method (type)
	   (error "~A is not of type type+primitive or type+func" type)))

(defgeneric type~symbols (type)
  (declare (edited  "20-MAY-1996 09:23")
	   (authors GKLEIN)
	   (input   "A datum.")
	   (effect  "None.")
	   (value   "A list of the basic type(constant)-symbols of the type of"
		    "the datum."))
  (:method ((type type+constant))
	   (list type))
  (:method ((type type+variable))
	   nil)
  (:method ((type type+func))
	   (union (type~symbols (data~abstr-c-domain type))    ;;;
		  (type~symbols (data~abstr-c-range type))))   ;;;
  (:method ((type type+appl))
	   (union (type~symbols (data~appl-function type))
		  (mapcar 'type~symbols (data~appl-arguments type))))
  (:method ((type type+schema))
	   (type~symbols (data~schema-range type)))
  (:method (type)
	   (error "KEIM: type~~symbols::~A is not of subtype type+type" type)))



(defmethod data~replacement-check-p ((struct1 type+type) (struct2 type+type))
  (declare (edited  "29-JAN-1998")
	   (authors Ameier)
	   (input   "Two types.")
	   (effect  "None.")
	   (value   "Always T."))
  't)

;; =========================================================================
;;                      Post Reader + PRINTER
;; =========================================================================

;; POST-READER FUER TYPE-VAR + TYPE-CONST + TYPE-constructor

(defmethod post~read-object (typevars (env env+environment)
				      (indicator (eql :type-variables)))
  (mapcar #'(lambda (typevar)
	      (post~read-object typevar env :type-variable))
	  typevars))
  
(defmethod post~read-object (typevar (env env+environment) 
				     (indicator (eql :type-variable)))
  (type~create-primitive-in-environment typevar 'type+variable env))

(defmethod post~read-object (typevars (env env+environment)
				      (indicator
				       (eql :type-variables-multiple)))
  (mapcar #'(lambda (typevar)
	      (post~read-object typevar env :type-variable-multiple))
	  typevars))

(defmethod post~read-object (typevar (env env+environment) 
				     (indicator (eql :type-variable-multiple)))
  (type~create-primitive-in-environment typevar 'type+variable env :allow-multi 't))

(defmethod post~read-object (typeconsts (env env+environment)
					(indicator
					 (eql :type-constants-multiple)))
  (mapcar #'(lambda (typeconst)
	      (post~read-object typeconst env :type-constant-multiple))
	  typeconsts))

(defmethod post~read-object (typeconst (env env+environment) 
				       (indicator (eql :type-constant-multiple)))
  (type~create-primitive-in-environment typeconst 'type+constant env :allow-multi 't))

(defmethod post~read-object (typeconsts (env env+environment) 
					(indicator (eql :type-constants)))
  (mapcar #'(lambda (typeconst) 
	      (post~read-object typeconst env :type-constant))
	  typeconsts))
  
(defmethod post~read-object (typeconst (env env+environment) 
				       (indicator (eql :type-constant)))
  (type~create-primitive-in-environment typeconst 'type+constant env))

(defmethod post~read-object (typeconst (env env+environment) 
				       (indicator (eql :type-constructor)))
  (type~create-primitive-in-environment (first typeconst)
					'type+constructor
					env
					:arity (second typeconst)))

(defmethod post~read-object (typeconsts (env env+environment) 
					(indicator (eql :type-constructors)))
  (mapcar #'(lambda (typeconst) 
	      (post~read-object typeconst env :type-constructor))
	  typeconsts))

(defmethod post~read-object (typedef (env env+environment) 
				     (indicator (eql :type-def)))
  (let* ((name (first typedef))
	 (type (second typedef))
	 (typeinterpreted (post~read-object type env :existing-type))
	 (type-def (type~create-type-def name typeinterpreted))
	 (env-check (env~lookup-object name env))
	 (class-thing (class-name (class-of env-check))))
    
    (cond ((null (symbolp name))
	   (error "~A should be a symbol." name))
	  (env-check
	   (error "Can't declare ~S as a type-def with type ~A, because it already exists in the environment as a ~A. "
		  name typeinterpreted class-thing))
	  (t
	   (env~enter name type-def env)))))

(defmethod post~read-object (typedefs (env env+environment) 
				      (indicator (eql :type-defs)))
  (mapcar #'(lambda (typedef)
	      (post~read-object typedef env :type-def))
	  typedefs))

(defun type~create-primitive-in-environment (symbol class env
						    &key
						    (allow-multi nil)
						    (arity nil))
  (declare (edited  "15-NOV-1996")
	   (authors Ameier)
	   (input   "A symbol, a type primitive class (type+variable or"
		    "type+constant) and an environment."
		    "As keyword allow-multi, wether it is allowed to insert"
		    "the type again.")
	   (effect  "If there is already an object in the environment with the"
		    "key symbol, but an other class, error."
		    "If there is already an object in the environment with the"
		    "same symbol and the same class, this object is returned."
		    "Otherwise a type-object of the class is created, added to"
		    "the environment and returned.")
	   (value   "Look at effect."))
 (when (not (or (equal class 'type+constant)
		(equal class 'type+variable)
		(equal class 'type+constructor)))
   (error "~A is not a type+prim class" class))
 (let* ((thing (env~lookup-object symbol env))
	(class-thing (class-name (class-of thing))))
   (cond (allow-multi
	  (let ((type-object (if (equal class 'type+variable)
				 (type~variable-create symbol)
			       (type~constant-create symbol))))
	    (env~enter symbol type-object env)
	    type-object))
	 ((and thing
	       (not (equal class-thing class)))
	  (error "Can't declare ~S as a type-object of class ~A, because it already exists in the environment as a ~A. "
		 symbol class class-thing))
	 ((and thing
	       (equal class-thing class))
	  thing)
	 ((and (string= (string symbol) "O") (equal class 'type+constant))
	  (env~enter symbol (type~o) env)
	  (type~o))
	 ((and (string= (string symbol) "I") (equal class 'type+constant))
	  (env~enter symbol (type~i) env)
	  (type~i))
	 ((and (string= (string symbol) "NUM") (equal class 'type+constant))
	  (env~enter symbol (type~num) env)
	  (type~list))
	 ((and (string= (string symbol) "LIST") (equal class 'type+constant))
	  (env~enter symbol (type~list) env)
	  (type~list))
	 ((and (string= (string symbol) "CYC") (equal class 'type+constant))
	  (env~enter symbol (type~cyc) env)
	  (type~cyc))
	 ((and (string= (string symbol) "TUPLE") (equal class 'type+constructor))
	  (env~enter symbol (type~tuple) env)
	  (type~tuple))
	 (t
	  (let ((type-object (cond ((equal class 'type+variable)
				    (type~variable-create symbol))
				   ((equal class 'type+constant)
				    (type~constant-create symbol))
				   ((equal class 'type+constructor)
				    (type~constructor-create symbol arity)))))
	    (env~enter symbol type-object env)
	    type-object)))))

;; POST READER FUER EXISTING-TYPE:

(defmethod post~read-object (type (env env+environment) 
				  (indicator (eql :existing-type)))
  (cond ((and (listp type)
	      (null (listp (first type)))
	      (string= (string (first type)) "ALL-TYPES"))
	 (let* ((syn-type-variables (rest (butlast type)))
		(rest-type (first (last type)))
		(type-vars (post~read-object syn-type-variables env :type-variables-multiple))
		(new-type (post~read-object rest-type env :existing-type)))
	   
	   ;; rauswerfen der type-variables aus dem environment
	   (mapcar #'(lambda (var)
		       (env~remove var env))
		   syn-type-variables)
	   
	   ;; SCHEMA erzeugen
	   (type~schema-create new-type :domain type-vars)
	   
	   ))
	((and (listp type)
	      (> (length type) 1))
	 (let* ((sub-types (mapcar #'(lambda (sub-type)
				       (post~read-object sub-type env :existing-type))
				   type))
		(first-subtype (first sub-types))
		(rest-subtypes (rest sub-types)))

	   (if (type~constructor-p first-subtype)
	       (if (= (type~constructor-arity first-subtype) (length rest-subtypes))
		   (type~appl-create first-subtype (rest sub-types))
		 (error "~%Tried to apply type-constructor ~A on types ~A, but ~A has arity ~A"
			first-subtype
			rest-subtypes
			first-subtype
			(type~constructor-arity first-subtype)))
	     (type~func-create (reverse (rest sub-types)) (first sub-types)))))
	((and (listp type)
	      (= (length type) 1))
	 (type=env-lookup-symbol (first type) env))
	(t
	 (type=env-lookup-symbol type env))))

(defmethod post~read-object (type (env env+environment) 
				  (indicator (eql :existing-type-arity-check)))
  (let* ((ex-type (post~read-object type env :existing-type)))
    (if (type~check-arities-rec-p ex-type nil)
	ex-type
      (error "~%Type ~A contains type-constructors which are applied not accordingly to their arity (to say it clear: the arities are wrong!)" ex-type))))

;;
;; Why is the indicator :existing-type-arity-check necessary?
;; Why is the type~check-arities-rec-p necessary?
;;
;; The problem is the following:
;; When reading a type containing type-constructors, the arities of the type-constructors cannot be checked properly on the fly!
;; Let us assume that C is a type-constructor with 1 argument.
;; (C a b) would produce an error since the creation of an data+appl with 2 arguments would fail because C is declared with one argument!
;; But simply C or even (C) would produce no error, since the then C would be accepted just as a basic type and it would never
;; be checked if C is a type-constructor! Hence, it would also never be checked whether the arity is corect!
;; Such a check for the correct arities of used type-constructors can only be done when - besides the type-constructor itself -
;; also the context of its usage is known. This can be done only after the type is created.
;;
;; AMeier
;;

(defgeneric type~check-arities-rec-p (type above-obj)
  (:method ((type type+primitive) above-obj)
	   (cond ((not (type~constructor-p type))
		  t)
		 ((= (type~constructor-arity type) 0)
		  t)
		 ((and (data~appl-p above-obj)
			  (keim~equal type (data~appl-function above-obj))
			  (= (type~constructor-arity type) (length (data~appl-arguments above-obj))))
		  t)
		 (t
		  nil)))
  (:method ((type type+schema) above-obj)
	   (type~check-arities-rec-p (data~schema-range type) type))
  (:method ((type type+func) above-obj)
	   (every #'(lambda (typi)
		      (type~check-arities-rec-p typi type))
		  (cons (data~abstr-range type) (data~abstr-domain type))))
  (:method ((type type+appl) above-obj)
	   (every #'(lambda (typi)
		      (type~check-arities-rec-p typi type))
		  (cons (data~appl-function type) (data~appl-arguments type)))))
  


(defun type=env-lookup-symbol (type env)
  (declare (edited  "14-NOV-1997")
	   (authors Ameier)
	   (input  "an object and an environment")
	   (effect "None.")
	   (value  "Looks up the object in environment, returns it if it is associated with a type, otherwise error"))
  (let ((obj (env~lookup-object type env)))
    (cond ((not obj) 
	   (error "~A is not an existing type." type))
	  ((not (type~p obj))
	   (error "~A is declared in the environment as a ~A, not as a type."
		  type (class-of obj)))
	  (t obj))))

(defmethod post~print ((type type+primitive) stream)
  (format stream "~A" (keim~name type)))

(defmethod post~print ((type type+appl) stream)
  (format stream "(")
  (post~print (data~appl-function type) stream)
  (format stream " ")
  (post~print (data~appl-arguments type) stream)
  (format stream ")"))

(defmethod post~print ((type type+func) stream)
  (format stream "(")
  (post~print (data~abstr-range type) stream)
  (format stream " ")
  (post~print (reverse (data~abstr-domain type)) stream)
  (format stream ")"))

(defmethod post~print ((type type+schema) stream)
  (let* ((domain (data~schema-domain type))
	 (range (data~schema-range type)))
    (format stream "(all-types")
    (mapcar #'(lambda (type-var)
		(format stream " ~A " (keim~name type-var)))
	    domain)
    (post~print range stream)
    (format stream ")")))



;;
;; =========================================================================
;;                       ALPHA EQUAL-P  FUER TYPEN
;; =========================================================================

(defmethod data~alpha-equal ((datum1 type+type) (datum2 type+type)
			     bound1 bound2 bindables)
	   (type=alpha-equal datum1 datum2 bound1 bound2 bindables))


(defmethod data~alpha-copy ((datum type+type) renaming)
  (data~copy datum :downto '(type+primitive)))


(data~defgeneric type~alpha-equal ((type1) (type2) &optional bindables)
  (declare (edited  "22-JAN-1998")
	   (authors Gebhard)
	   (input   "Two types")
	   (effect  "None")
	   (value   "A subst realising the mapping for a-equality, or nil"))
  (:method ((type1 type+type) (type2 type+type) &optional bindables)
	   (multiple-value-bind (success subst)
	       (bind~with-bindings
		((type=alpha-equal type1 type2 nil nil bindables)))
	     (when success
	       (subst~idem subst))))
  (:method ((type1 t) (type2 t) &optional bindables)
	   (declare (ignore bindables))
	   (error "Please only apply type~~alpha-equal on types.")))


(data~defgeneric type=alpha-equal ((type1) (type2) bit1 bit2 bindables)
  (declare (edited  "22-JAN-1998")
	   (authors Gebhard)
	   (input   "Two types, two lists of bound variables and two lists of"
		    "restproblems. For unifying one argument for the"
		    "substitutable variables.")
	   (effect  "none")
	   (value   "T if the two types are unifyable with respect to"
		    "alpha equality and the bindables variabels."))
  (:method ((type1 type+appl) (type2 type+appl) bit1 bit2 bindables)
	   (let ((lev-res  (when (= (length (data~appl-arguments type1))
				    (length (data~appl-arguments type2)))
			     (type=alpha-equal
			      (cons (data~appl-function type1)
				    (data~appl-arguments type1))
			      (cons (data~appl-function type2)
				    (data~appl-arguments type2))
			     bit1 bit2 bindables))))
	     lev-res))
  (:method ((type1 type+func) (type2 type+func) bit1 bit2 bindables)
	   (let* ((con2 (< (length (data~abstr-domain type1))
			  (length (data~abstr-domain type2))))
		 (con3 (> (length (data~abstr-domain type1))
			  (length (data~abstr-domain type2))))
		 (newran1 (if con3
			      (type~func-create
			       (last (data~abstr-domain type1)
				     (- (length (data~abstr-domain type1))
					(length (data~abstr-domain type2))))
			       (data~abstr-range type1))
			    (data~abstr-range type1)))
		 (newdom1 (if con3 (subseq (data~abstr-domain type1)
					   0
					   (length (data~abstr-domain type2)))
			    (data~abstr-domain type1)))
		 (newran2 (if con2
			      (type~func-create
			       (last (data~abstr-domain type2)
				     (- (length (data~abstr-domain type2))
					(length (data~abstr-domain type1))))
			       (data~abstr-range type2))
			    (data~abstr-range type2)))
		 (newdom2  (if con2 (subseq (data~abstr-domain type2)
					    0
					    (length (data~abstr-domain type1)))
			     (data~abstr-domain type2))))
	     (type=alpha-equal (cons newran1 newdom1)
			       (cons newran2 newdom2)
			       bit1 bit2
			       bindables)))
  (:method ((type1 type+constant) (type2 type+constant) bit1 bit2 bindables)
	   (declare (ignore bit1 bit2 bindables))
	   (data~equal type1 type2))
  (:method ((type1 type+variable) (type2 type+variable) bit1 bit2 bindables)
	   (cond ((data~equal type1 type2)
		  t)
		 ((bind~binding type1)
		  (type=alpha-equal (bind~binding type1)
				    type2 bit1 bit2
				    bindables))
		 ((bind~binding type2)
		  (type=alpha-equal type1
				    (bind~binding type2)
				    bit1 bit2 bindables))
		 ((find type1 bindables)
		  (bind~bind type1 type2)
		  t)
		 ((find type2 bindables)
		  (bind~bind type2 type1)
		  t)
		 ((find type1 bit1)
		  (when (find type2 bit2)
		    (progn
		      (delete type1 bit1)
		      (delete type2 bit2)
		      (bind~bind type2 type1)
		      t)))
		 (t
		  nil)))
  (:method ((type1 type+variable) (type2 type+type) bit1 bit2 bindables)
	   (cond ((bind~binding type1)
		  (type=alpha-equal (bind~binding type1)
				    type2
				    bit1 bit2 bindables))
		 ((and (find type1 bindables)
		       (type~not-free-occuring type1 type2))
		  (bind~bind type1 type2) t)
		 (t
		  nil)))
  (:method ((type1 type+type) (type2 type+variable) bit1 bit2 bindables)
	   (cond ((bind~binding type2)
		   (type=alpha-equal type1
				     (bind~binding type2)
				     bit1 bit2 bindables))
		  ((and (find type2 bindables)
			(type~not-free-occuring type2 type1))
		   (bind~bind type2 type1) t)
		  (t nil)))
  (:method ((type1 data+struct) (type2 data+struct) bit1 bit2 bindables)
	   (declare (ignore bit1 bit2 bindables))
	   (error ";;ERROR: Type=alpha-equal: Type... funcs are only applicable on type+... classes!"))
  (:method ((types1 list) (types2 list) bit1 bit2 bindables)
	   (every #'(lambda (x y) (type=alpha-equal x y bit1 bit2 bindables))
		  types1 types2))
  (:method ((type1 type+schema) (type2 type+schema) bit1 bit2 bindables)

	   ;; Bindungen muessen direkt ausgefuehrt werden, da sonst bei <aa>_o und <bb>_o {} rauskommt !!
	   ;; AMEIER 
	   
	   ;;(type=alpha-equal (data~schema-range type1)
	   ;;		     (data~schema-range type2)
	   ;;		     (append (data~schema-domain type1) bit1)
	   ;;		     (append (data~schema-domain type2) bit2)
	   ;;		     bindables))

	   (let* ((kappas1 (data~schema-domain type1))
		  (kappas2 (data~schema-domain type2)))

	     (if (null (= (length kappas1) (length kappas2)))
		 nil

	       (progn
		 
		 (mapcar #'(lambda (kappa1 kappa2)
			     (bind~bind kappa2 kappa1))
			 kappas1 kappas2)
		 
		 (type=alpha-equal (data~schema-range type1)
				   (data~schema-range type2)
				   bit1
				   bit2
				   bindables)))))
  
  (:method ((type1 type+type) (type2 type+type) bit1 bit2 bindables)
	   (declare (ignore bit1 bit2  bindables))
	   nil))

(data~defgeneric type~alpha-match ((type1) (type2))
  (declare (edited  "22-JAN-1998")
	   (authors Gebhard)
	   (input   "Two types.")
	   (effect  "None on the types.")
	   (value   "A substition realising a matching (mod alpha-eq)"
		    "if possible, else nil"))
  (:method ((type1 type+type) (type2 type+type))
	   (let ((bindables (type~free-variables type1)))
	     (multiple-value-bind (success subst)
		 (bind~with-bindings
		  ((type=alpha-equal type1 type2 nil nil bindables)))
	       (when success
		 (subst~idem subst))))))

(data~defgeneric type~alpha-unify ((type1) (type2))
  (declare (edited  "22-JAN-1998")
	   (authors Gebhard)
	   (input   "Two types")
	   (effect  "None")
	   (value   "A subst if the types are unifyable, or nil"))
  (:method ((type1 type+type) (type2 type+type))
	   (let ((bindables (append (type~free-variables type1)
				    (type~free-variables type2))))
	     (multiple-value-bind
		 (success subst)
		 (bind~with-bindings
		  ((type=alpha-equal type1 type2 nil nil bindables)))
	     (if success
		 (subst~idem subst)
	       nil)))))

(data~defgeneric type~bound-variables ((type))
  (declare (edited  "03-NOV-1998")
	   (authors Gebhard)
	   (input   "A type")
	   (effect  "None")
	   (value   "The kappa-bound variables of the type."))
  (:method ((type type+schema))
	   (data~schema-domain type))
  (:method ((type type+type))
	   nil))
     
(defun type~abstract (type add-domain)
  (declare (edited  "09-MAR-1998")
	   (authors Gebhard)
	   (input   "A type (t1,...,tn)->t0 and a list (s1...sm).")
	   (effect  "None.")
	   (value   "The type (s1,...,sm,t1,...,tn) -> t0.")
	   (example "(i,i) -> o   ((i) -> o i) --> ((i)->o,i,i,i) -> o"))
  (if (and (listp add-domain)
	   (every 'type~p add-domain))
      (let ((newtype (if (data~abstr-p type)
			 (data~copy type
				    :explode nil
				    :preserve :all-classes
				    :downto (list 'type+primitive))
		       (type~func-create nil
					 (data~copy type
						    :explode nil
						    :preserve :all-classes
						    :downto
						     (list 'type+primitive)))))
	    (old-dom (if (data~abstr-p type)
			 (data~abstr-domain type)
		       nil)))
	(setf (data~abstr-domain newtype)
	      (append (data~copy add-domain
				 :explode nil
				 :preserve :all-classes
				 :downto (list 'type+primitive))
		      old-dom))
	(data~n-normalize newtype))
    (error "type~~abstract: The second argument has to be a typelist.~%")))



(defmethod env~post-print (key (var type+variable) stream)
  (declare (ignore key))
  (format stream "~&(type-variables (~A)~%" (keim~name var))
  (values))

(defmethod env~post-print (key (con type+constant) stream)
  (declare (ignore key))
  (format stream "~&(type-constants (~A))~%" (keim~name con))
  (values))




;; =========================================================================
;; Typespezifische Methoden aus dem Beta Modul (dessen generischen Fktnen
;;                   meist welche die von hop ererbt wurden)
;; =========================================================================

(defmethod beta~contains-redex-p ((datum type+func))
	   (some #'beta~contains-redex-p
		 (cons (data~abstr-range datum) (data~abstr-domain datum))))

(defmethod beta=contract ((term type+appl) &key destructive)
	   (let* ((ab        (data~appl-function term))
		  (ab-dom    (data~abstr-domain ab))
		  (arg       (first (data~appl-arguments term)))
		  (new-scope (if (data~abstr-p ab)
				 (data~replace-free-variables
				  (data~abstr-range ab)
				  (list (first ab-dom))
				  (list arg)
				  :destructive destructive)
			       ab))
		  (new-ab    (if (cdr ab-dom)
				 (type~func-create (cdr ab-dom) new-scope)
			       new-scope)))
	     (if (cdr (data~appl-arguments term))
		     (type~appl-create new-ab (cdr (data~appl-arguments term)))
	       new-ab)))

;; =======================================================================
;;   Funktionen aus Data, auf die unterschiedlichen Bindungsverhaeltnisse
;;   bei Typen angepasst
;; =======================================================================

;; ===================   data=replace-fv

(defmethod data=replace-fv ((datum type+constant)
			    bound domain codomain destructive downto test)
  (declare (ignore bound destructive test downto codomain domain))
  datum)

(defmethod data=replace-fv ((datum type+variable)
			    bound domain codomain destructive downto test)  
  (let ((pos (position datum domain :test test)))
    (cond ((or (find datum bound :test test) (null pos))
	   (if (or destructive
		   (eq downto data*all-classes-keyword)
		   (some #'(lambda (x) (typep datum x)) downto))
	       datum
	     (data~copy datum)))
	  (pos
	   (nth pos codomain))   
	  (t
	   (error "data~~replace-fv: This message shopuldn't appear!")))))

(defmethod data=replace-fv ((datum type+appl)
			    bound domain codomain destructive downto test)
  (if (or destructive
	  (eq downto data*all-classes-keyword)
	  (some #'(lambda (x) (typep datum x)) downto))
      (progn
	(setf (data~appl-function datum)
	      (data=replace-fv (data~appl-function datum)
			       bound domain codomain 't downto test))
	(setf (data~appl-arguments datum)
	      (data=replace-fv (data~appl-arguments datum)
			       bound domain codomain 't downto test))
	datum)
    (let* ((new-fun (data=replace-fv
		     (data~appl-function datum)
		     bound domain codomain destructive downto test))
	   (new-args (data=replace-fv
		      (data~appl-arguments datum)
		      bound domain codomain  destructive downto test)))
      (make-instance (class-of datum)
		     :function new-fun
		     :arguments new-args 
		     :status (data=status datum)
		     :annotation nil))))

(defmethod data=replace-fv ((datum type+func)
			    bound domain codomain destructive downto test)
  (if (or destructive
	  (eq downto data*all-classes-keyword)
	  (some #'(lambda (x) (typep datum x)) downto))
      (progn
	(setf (data~abstr-range datum)
	      (data=replace-fv (data~abstr-range datum)
			       bound domain codomain 't downto test))
	(setf (data~abstr-domain datum)
	      (data=replace-fv (data~abstr-domain datum)
			       bound domain codomain 't downto test))
	datum)
    (let* ((new-dom (data=replace-fv
		     (data~abstr-domain datum)
		     bound domain codomain destructive downto test))
	   (new-ran (data=replace-fv
		     (data~abstr-range datum)
		     bound domain codomain destructive downto test)))
      (make-instance (class-of datum)
		     :range new-ran 
		     :domain new-dom
		     :status (data=status datum)
		     :annotation nil))))


(defmethod data=replace-fv ((datum type+schema) bound domain codomain destructive downto test)

  (if (or destructive (eq downto data*all-classes-keyword) (some #'(lambda (x) (typep datum x)) downto))
      (progn
	(setf (data~schema-range datum)
	      (data=replace-fv (data~schema-range datum) (remove-duplicates (append bound (data~schema-domain datum)))
			       domain codomain 't downto test))
	datum)
    (let* ((new-ran (data=replace-fv (data~schema-range datum) (remove-duplicates (append bound (data~schema-domain datum)))
				     domain codomain destructive downto test)))
      
      (make-instance (class-of datum)
		     :datum new-ran
		     :domain (data~schema-domain datum)
		     :status (data=status datum)
		     :annotation (data~copy (data~annotation datum) :downto (list 'data+variable))))))

;; ======================= subst~idem-rec

(defmethod subst~idem-rec ((appl type+appl) subst)
  (let ((new-func (subst~idem-rec (data~appl-function appl) subst)))
    (when new-func
      (let ((new-args (mapcar #'(lambda (arg)
				  (subst~idem-rec arg subst))
			      (data~appl-arguments appl))))
	(unless (some #'null new-args)
	  (type~appl-create new-func new-args))))))

(defmethod subst~idem-rec ((abstr type+func) subst)
  (let* ((binder (data~abstr-binder abstr))
	 (new-abstr)
	 (new-binder (mapcar #'(lambda (x) (subst~idem-rec x subst)) binder))
	 (new-range (subst~idem-rec (data~abstr-range abstr) subst)))
    (when new-range
      (unless (some #'null new-binder)
	(setf new-abstr (type~func-create new-binder new-range))))
    new-abstr))


(defmethod subst~idem-rec ((scheme type+schema) subst)
  (let* ((binder (data~schema-domain scheme))
	 (old-labels (mapcar #'data~label binder))
	 (new-scheme))
    (unwind-protect
	(progn
	  (mapc #'(lambda (x) (setf (data~label x) x)) binder)
	  (let ((new-datum (subst~idem-rec (data~schema-range scheme) subst)))
	    (when new-datum
		(setf new-scheme (type~schema-create new-datum :domain binder)))))  
      (mapc #'(lambda (x y) (setf (data~label x) y)) binder old-labels))
    new-scheme))

;; ============================  data~free-variables

(defmethod data~free-variables ((datum type+type))
  (type~free-variables datum))

(defun type~free-variables (type)
  (declare (edited  "07-JAN-1999")
	   (authors Gebhard)
	   (input   "a type")
	   (effect  "None")
	   (value   "The list of all typevariables, that are not kappa-bound in type."))
  ;(setminus (type~variables type) (type~bound-variables type)))
  ;;; LC: Ich nehme an, hier wird SET-DIFFERENCE gemeint.
  (set-difference (type~type-variables type) (type~bound-variables type)))

;; ============================ data~substructs
;; Method zu data~substructs und data~all-substructs
;; da bei Typen die Teilterme in der Domaine einer
;; Abstraktion nicht von der Range erfasst werden.
(defmethod data~substructs ((data type+func))
  (cons (data~abstr-range data) (data~abstr-domain data)))

(defmethod data~all-substructs ((data type+func))
  (append (list data (data~abstr-range data))
	  (data~all-substructs (data~abstr-domain data))))

(setf (symbol-function 'type~new-var-name)
      (data~name-generator 0 :prefix 'tv))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun type~generate-type-primitive-with-new-name (symbol class env)
  (declare (edited  "20-MAR-1998")
	   (authors Ameier)
	   (input   "A symbol, a class and an environment.")
	   (effect  "A new element of class CLASS is created and added into "
		    "the environment (CClass has to be a subclass of"
		    "type+primitive). If there is not already an element with"
		    "name SYMBOL in the environment the name of the new"
		    "element will be SYMBOL, otherwise it will be SYMBOLn"
		    "where n is the first natural number, that SYMBOLn is not"
		    "in the environment.")
	   (value   "The new created object."))
  (when (null (subtypep class 'type+primitive))
    (error "~A should be a subclass of type+primitive."))
  (let* ((name (type~generate-new-name symbol env)))
    (type~create-primitive-in-environment name  class env)))

(defun type~generate-new-name (symbol env)
  (declare (edited  "20-MAR-1998")
	   (authors Ameier)
	   (input   "A symbol and an environment.")
	   (effect  "None.")
	   (value   "A symbol, if there is no element in the environment with"
		    "name SYMBOL, SYMBOL itself, otherwise a SYMBOLN, where n"
		    "is the first natural number, that SYMBOLN is not in the"
		    "environment."))
  (let ((n 1)
	(new-symbol))
    (loop
     (setq new-symbol (intern (format nil "~A~A" symbol n)
			      (find-package :keim)))
     (when (null (env~lookup-object new-symbol env))
       (return))
     (incf n))
    new-symbol))



