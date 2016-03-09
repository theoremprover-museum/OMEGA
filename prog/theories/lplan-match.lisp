; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
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

(defclass match+result ()
  ((sigma             :initarg :sigma
		      :initform (subst~create nil nil)
		      :accessor match~res-sigma)
   (sigma-1           :initarg :sigma-1
		      :initform (subst~create nil nil)
		      :accessor match~res-sigma-1)
   (sigmat            :initarg :sigmat
		      :initform (subst~create nil nil)
		      :accessor match~res-sigmat)
   (sigmau            :initarg :sigmau
		      :initform (subst~create nil nil)
		      :accessor match~res-sigmau)
   (ct                :initarg :ct
		      :initform t
		      :accessor match~res-ct)
   (cmy               :initarg :cmy
		      :initform nil
		      :accessor match~res-cmy))
  (:documentation "The result of the logical matching."))

(defmethod print-object ((object match+result) stream)
  (format stream "~%sigma ~A~%sigma-1 ~A~%sigmat ~A~%sigmau ~A~%ct ~A~%cmy ~A"
	  (match~res-sigma object)
	  (match~res-sigma-1 object)
	  (match~res-sigmat object)
	  (match~res-sigmau object)
	  (match~res-ct object)
	  (match~res-cmy object)))

(defun match~res-p (object)
  (typep object 'match+result))

(defun match~res-create-empty ()
  (make-instance 'match+result))

(defgeneric match~res-to-constr (res)
  (:method ((res list))
	   (cstr~disjunction (mapcar 'match~res-to-constr res)))
  (:method ((res match+result))
	   (cstr~conjunction
	    (cstr~inst-simple-compose (cstr~inst-apply (cstr~inst-from-subst (match~res-sigma-1 res))
						       (cstr~inst-from-subst (match~res-sigma res)))
				      (cstr~inst-from-subst (match~res-sigmat res))))))

(defmethod match~res-change-sigma ((result match+result) sigma sigma-1)
  (make-instance 'match+result
		 :sigma (subst~compose-substitution-nso (match~res-sigma result) sigma)
		 :sigma-1 (subst~compose-substitution-nso (match~res-sigma-1 result) sigma-1)
		 :sigmat (match~res-sigmat result)
		 :sigmau (match~res-sigmau result)
		 :ct (match~res-ct result)
		 :cmy nil))

(defmethod match~res-append-cmy ((result match+result) cmy)
  (make-instance 'match+result
		 :sigma (match~res-sigma result)
		 :sigma-1 (match~res-sigma-1 result)
		 :sigmat (match~res-sigmat result)
		 :sigmau (match~res-sigmau result)
		 :ct (match~res-ct result)
		 :cmy (append (match~res-cmy result) cmy)))

(defmethod match~res-change-sigmat ((result match+result) sigmat ct)
  (make-instance 'match+result
		 :sigma (match~res-sigma result)
		 :sigma-1 (match~res-sigma-1 result)
		 :sigmat (subst~compose-substitution-nso (match~res-sigmat result) sigmat)
		 :sigmau (match~res-sigmau result)
		 :ct ct
		 :cmy (match~res-cmy result)))

(defmethod match~res-add-sigmau ((result match+result) (x term+term) (y term+term))
  (make-instance 'match+result
		 :sigma (match~res-sigma result)
		 :sigma-1 (match~res-sigma-1 result)
		 :sigmat (match~res-sigmat result)
		 :sigmau (subst~insert-component x y (match~res-sigmau result))
		 :ct (match~res-ct result)
		 :cmy (match~res-cmy result)))

(defmethod match~res-add-sigmau (result (x list) (y list))
  (when result
    (if x
	(match~res-add-sigmau (match~res-add-sigmau result (first x) (first y)) (rest x) (rest y))
      result)))
  
(defclass match+result-with-formulas (match+result)
  ((formulas          :initarg :formulas
		      :initform nil
		      :accessor match~res-formulas)
   (var               :initarg :var
		      :initform nil
		      :accessor match~res-var))
  (:documentation "The result of the logical matching."))

(defmethod print-object ((object match+result-with-formulas) stream)
  (format stream "~%formulas ~A~%var ~A~%sigma ~A~%sigma-1 ~A~%sigmat ~A~%sigmau ~A~%ct ~A~%cmy ~A"
	  (match~res-formulas object)
	  (match~res-var object)
	  (match~res-sigma object)
	  (match~res-sigma-1 object)
	  (match~res-sigmat object)
	  (match~res-sigmau object)
	  (match~res-ct object)
	  (match~res-cmy object)))  

(defmethod match~res-create-with-formulas ((result match+result) formulas)
  (when result
    (make-instance 'match+result-with-formulas
		   :sigma (match~res-sigma result)
		   :sigma-1 (match~res-sigma-1 result)
		   :sigmat (match~res-sigmat result)
		   :sigmau (match~res-sigmau result)
		   :ct (match~res-ct result)
		   :cmy (match~res-cmy result)
		   :formulas formulas)))
		   ;;(if (listp formulas) (if (= 1 (length formulas)) (first formulas) formulas) formulas))))

(defmethod match~res-create-with-formula-and-var ((result match+result) formula var)
  (when result
    (make-instance 'match+result-with-formulas
		   :sigma (match~res-sigma result)
		   :sigma-1 (match~res-sigma-1 result)
		   :sigmat (match~res-sigmat result)
		   :sigmau (match~res-sigmau result)
		   :ct (match~res-ct result)
		   :cmy (match~res-cmy result)
		   :var var
		   :formulas formula)))

(defmethod match~res-without-formulas ((result match+result-with-formulas))
  (make-instance 'match+result
		 :sigma (match~res-sigma result)
		 :sigma-1 (match~res-sigma-1 result)
		 :sigmat (match~res-sigmat result)
		 :sigmau (match~res-sigmau result)
		 :ct (match~res-ct result)
		 :cmy (match~res-cmy result)))

(defun match~cmy-create (term schema)
  (list term schema))

(defun match~cmy-term (cmy)
  (first cmy))

(defun match~cmy-schema (cmy)
  (second cmy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Type Matching}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric match~type (y x result)
  (:method ((y list) (x list) result)
	   (if y
	       (match~type (rest y) (rest x) (match~type (first y) (first x) result))
	     result))
  (:method ((y term+term) (x term+term) result)
	   (let ((my (term~type y))
		 (tau (term~type x)))
	     (multiple-value-bind (ct-1 sigmat-1)
		 (match=type (keim::subst~apply-nso (match~res-sigmat result) my) tau)
	       (when sigmat-1
		 (multiple-value-bind (ct-2 inst)
		     (cstr~simplify (match~res-ct result) (cstr~inst-from-subst sigmat-1) ct-1 :type-match t)
		   (match~res-change-sigmat result (cstr~inst-to-subst inst) ct-2)))))))
  
(defun match=type (my tau)
  (let* ((my-list (match=type-linearize my))
	 (n (length my-list))
	 (tau-list (match=type-linearize tau))
	 (m (length tau-list)))
    (when (<=  n m)
      (let ((f (position-if 'cstr~typevar-p my-list))
	    (l (position-if 'cstr~typevar-p my-list :from-end t)))
	(if (not f)
	    ;; my-list contains no type variables
	    (and (= (length my-list) (length tau-list)) (every 'keim~equal my-list tau-list))
	  ;; my-list contains type variables
	  (when (and (every 'keim~equal (subseq my-list 0 f) (subseq tau-list 0 f))
		     (every 'keim~equal (subseq my-list (+ l 1) n) (subseq tau-list (+ (+ (- m n) l) 1) m)))
	    (match=type1 (subseq my-list f (+ l 1)) (subseq tau-list f (+ (+ (- m n) l) 1)))))))))

(defun cstr~sat-type=1 (type1 type2 polarity)
  (declare (ignore polarity))
  (let ((result (match~type type1 type2 (match~res-create-empty))))
    (values (match~res-ct result) (cstr~inst-from-subst (match~res-sigmat result)) nil)))

(defun match=type1 (my-list tau-list)
  (let ((n (length my-list))
	(m (length tau-list)))
    (cond ((= n 1) (values t (meth~subst-create my-list (list (match=type-from-linearized tau-list)))))
	  ((= n m)
	   (let* ((type-pairs (mapcar 'list my-list tau-list))
		  (const-pairs (remove-if #'(lambda (p) (cstr~typevar-p (first p))) type-pairs))
		  (var-pairs (remove-if-not #'(lambda (p) (cstr~typevar-p (first p))) type-pairs)))
	     (when (every #'(lambda (p) (keim~equal (first p) (second p))) const-pairs)
	       (values t (meth~subst-create (mapcar 'first var-pairs) (mapcar 'second var-pairs))))))
	  ((< n m)
	   (let ((type-consts (remove-if 'cstr~typevar-p my-list)))
	     (when (match=same-order-p type-consts tau-list)
	       (values (cstr~create :type=1 my-list tau-list) (subst~create nil nil)))))
	  (t (omega~error ";;; match=type1: Illegal case.")))))

(defun match=same-order-p (type-consts tau-list)
  (if type-consts
      (let ((f (position (first type-consts) tau-list :test 'keim~equal)))
	(when f
	  (match=same-order-p (rest type-consts) (subseq tau-list (+ f 1) (length tau-list)))))
    t))

(defun match=type-from-linearized (list)
  (let ((domain (reverse (rest (reverse list))))
	(range (first (last list))))
    (if domain
	(type~func-create domain range)
      range)))

(defgeneric match=type-linearize (type)
  (:method ((type type+constant))
	   (list type))
  (:method ((type type+variable))
	   (list type))
  (:method ((type type+func))
	   (append (type~symbols (data~abstr-c-domain type))
		   (type~symbols (data~abstr-c-range type))))
  (:method ((type type+appl))
	   (append (type~symbols (data~appl-function type))
		   (mapcar 'type~symbols (data~appl-arguments type))))
  (:method ((type type+schema))
	   (type~symbols (data~schema-range type)))
  (:method (type)
	   (error "KEIM: type~~symbols::~A is not of subtype type+type" type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Logical Matching}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod match~log ((sf-phi sf+sf) (sf-psi sf+sf))
  (match~res-to-constr (match=log sf-phi sf-psi (match~res-create-empty))))

(defmethod match=log ((sf-phi sf+sf) (sf-psi sf+sf) result)
  (let ((sf-phi-list (sf~beta-subformulas sf-phi))
	(sf-psi-list (sf~beta-subformulas sf-psi)))
    (when (<= (length sf-psi-list) (length sf-phi-list))
      (match=cf sf-phi-list sf-psi-list result))))

;; construct result list
(defmethod match=phi-list ((sf-phi-list list) sf-psi result func func-transform)
  (apply 'append (mapcar #'(lambda (sf-phi)
			     (mapcar #'(lambda (res)
					 (match~res-create-with-formulas res (remove sf-phi sf-phi-list)))
				     (funcall func (funcall func-transform sf-phi) sf-psi result)))
			 sf-phi-list)))

;; process result list
(defmethod match=cf-all-results (result-list sf-psi)
  (apply 'append (mapcar #'(lambda (res) (match=cf (match~res-formulas res)
						   sf-psi (match~res-without-formulas res)))
			 result-list)))

(defmethod match=gamma-delta-cf-all-results (result-list sf-phi vars func-cf)
  (if (> (length vars) 1)
      (apply 'append (mapcar #'(lambda (res) (funcall func-cf sf-phi (match~res-formulas res)
							     (remove (match~res-var res) vars)
							     (match~res-without-formulas res)))
			     result-list))
    (apply 'append (mapcar #'(lambda (res) (match=cf sf-phi (match~res-formulas res) (match~res-without-formulas res)))
			   result-list))))

(defmethod match=cf ((sf-phi sf+sf) (sf-psi-list list) result)
  (match=cf (sf~beta-subformulas sf-phi) sf-psi-list result))

(defmethod match=cf ((sf-phi-list list) (sf-psi-list list) result)
  (let ((m (length sf-phi-list))
	(n (length sf-psi-list)))
    (cond ((and (= n 1) (> m 1))
	   (match=lcf sf-phi-list (first sf-psi-list) result))
	  ((and (= n 1) (= m 1))
	   (let ((sf-phi (first sf-phi-list))
		 (sf-psi (first sf-psi-list)))
	     (match=cf sf-phi sf-psi result)))
	  (t
	   (let* ((sf-rigid (find-if #'(lambda (sf) (help~rigid-literal-p (sf~formula sf))) sf-psi-list))
		  (sf-composed (find-if #'(lambda (sf) (help~composed-p (sf~formula sf))) sf-psi-list))
		  (sf-psi-rest (if sf-rigid (remove sf-rigid sf-psi-list)
				 (if sf-composed (remove sf-composed sf-psi-list)
				   (rest sf-psi-list))))
		  (result-list
		   (cond (sf-rigid (match=rl sf-phi-list sf-rigid result))
			 (sf-composed (match=cf sf-phi-list sf-composed result))
			 (t (match=fl sf-phi-list (first sf-psi-list) result)))))
	     (match=cf-all-results result-list sf-psi-rest))))))

(defmethod match=lcf ((sf-phi-list list) (sf-psi sf+sf) result)
  (apply 'append (mapcar #'(lambda (sf-phi) (match=cf sf-phi sf-psi result)) sf-phi-list)))

(defmethod match=rl ((sf-phi-list list) (sf-psi sf+sf) result)
  (match=phi-list sf-phi-list sf-psi result 'match=rl 'sf~id))

(defmethod match=rl ((sf-phi sf+sf) (sf-psi sf+sf) result)
  (when (and (eq (sf~sign sf-phi) (sf~sign sf-psi))
	     (keim~equal (match=term-head (sf~formula sf-phi)) (match=term-head (sf~formula sf-psi)))
	     (help~literal-p (sf~formula sf-phi)))
    (if (sf~equality-p sf-phi)
	;; take symmetry of = into consideration
	(match=cmy (list (match~res-append-cmy result (list (match~cmy-create (sf~formula (sf~equality-1 sf-phi))
									      (sf~formula (sf~equality-1 sf-psi)))
							    (match~cmy-create (sf~formula (sf~equality-2 sf-phi))
									      (sf~formula (sf~equality-2 sf-psi)))))
			 (match~res-append-cmy result (list (match~cmy-create (sf~formula (sf~equality-1 sf-phi))
									      (sf~formula (sf~equality-2 sf-psi)))
							    (match~cmy-create (sf~formula (sf~equality-2 sf-phi))
									      (sf~formula (sf~equality-1 sf-psi)))))))
      ;; no symmetry
      (match=cmy (list (match~res-append-cmy result (list (match~cmy-create (sf~to-formula sf-phi) (sf~to-formula sf-psi)))))))))

(defmethod match=fl ((sf-phi-list list) (sf-psi sf+sf) result)
  (match=phi-list sf-phi-list sf-psi result 'match=fl 'sf~id))

(defmethod match=fl ((sf-phi sf+sf) (sf-psi sf+sf) result)
  (when (help~literal-p (sf~formula sf-phi))
    (match=cmy (list (match~res-append-cmy result (list (match~cmy-create (sf~to-formula sf-phi) (sf~to-formula sf-psi))))))))

(defmethod match=cf ((sf-phi-list list) (sf-psi sf+sf) result)
  (match=phi-list sf-phi-list sf-psi result 'match=cf 'sf~id))

(defmethod match=cf ((sf-phi sf+sf) (sf-psi sf+sf) result)
  (cond ((help~rigid-literal-p (sf~formula sf-psi))
	 (match=rl sf-phi sf-psi result))
	((help~composed-p (sf~formula sf-psi))
	 (match=cf-h sf-phi sf-psi result))
	(t
	 (match=fl sf-phi sf-psi result))))

(defmethod match=cf-h ((sf-phi sf+sf) (sf-psi sf+sf) result)
  (cond ((sf~delta-p sf-psi)
	 (match=delta-cf sf-phi (sf~delta-1-0 sf-psi) (sf~gamma-delta-vars sf-psi) result))
	;; -----------------------------------------------------------------------------------------------------------
	((sf~gamma-p sf-psi)
	 (match=gamma-cf sf-phi (sf~gamma-1-0 sf-psi) (sf~gamma-delta-vars sf-psi) result))
	;; -----------------------------------------------------------------------------------------------------------
	((sf~alpha-p sf-psi)
	 (let ((result-list (match=alpha-cf sf-phi (sf~negate (sf~alpha-1 sf-psi)) result)))
	   ;;(result-list-dual (match=alpha-cf sf-phi (sf~negate (sf~alpha-2 sf-psi)) result)))
	   (match=cf-all-results result-list (sf~alpha-2 sf-psi))))
	;;(match=cf-all-results result-list-dual (sf~alpha-1 sf-psi)))))
	((sf~beta-p sf-psi)
	 (match=log sf-phi sf-psi result))
	(t (omega~error ";;; match=cf: psi is neither beta, delta, gamma nor alpha formula: ~A" sf-psi))))

(defmethod match=gamma-delta-cf ((sf-phi sf+sf) (sf-psi sf+sf) vars result func-p func-1-0 func-s func-cf)
  (when (funcall func-p sf-phi)
    (let ((sort (sf~gamma-delta-sort sf-phi))
	  (phi-vars (sf~gamma-delta-vars sf-phi)))
      (if sort
	  ;; sorted
	  (let ((result-list (funcall func-s sort (first phi-vars) sf-psi vars result)))
	    (match=gamma-delta-cf-all-results result-list (funcall func-1-0 sf-phi) vars func-cf))
	;; not sorted
	(when (= (length phi-vars) (length vars))
	  (let* ((associations (match~permutations phi-vars))
		 (new-result (remove-if 'null (mapcar #'(lambda (asso)
							  (let ((result-with-type (match~type vars asso result)))
							    (if result-with-type
								(match~res-create-with-formulas
								 (match~res-add-sigmau (match=cmy result-with-type) vars asso)
								 (funcall func-1-0 sf-phi))
							      nil)))
						      associations))))
	    (match=cf-all-results new-result sf-psi)))))))

(defmethod match=gamma-delta-s (sort var (sf-psi sf+sf) vars result func-p func-subformulas func-transform)
  (when (funcall func-p sf-psi)
    (let ((subformulas (funcall func-subformulas sf-psi)))
      (apply 'append (mapcar #'(lambda (sub)
				 (match=gamma-delta-s-sub sort var (funcall func-transform sub) sub subformulas vars result))
			     subformulas)))))

(defmethod match=gamma-delta-s-sub (sort var (sub-trans sf+sf) (sub sf+sf) (subformulas list) vars result)
  (let ((lit (sf~to-formula sub-trans)))
    (when (and (help~literal-p lit) (= 1 (length (help~literal-subterms lit))))
      (let ((x (first (help~literal-subterms lit))))
	(when (find x vars)
	  (let ((new-result (match~type x var result)))
	    (when new-result
	      (let ((new-result-1 (match=cmy
				   (match~res-add-sigmau
				    (match~res-append-cmy
				     (match=cmy new-result)
				     (list (match~cmy-create (term~appl-create sort (list var)) lit)))
				    x var))))
		(when new-result-1
		  (list (match~res-create-with-formula-and-var new-result-1 (remove sub subformulas) x)))))))))))

(defmethod match=delta-s (sort var (sf-psi sf+sf) vars result)
  (match=gamma-delta-s sort var sf-psi vars result 'sf~alpha-p 'sf~alpha-subformulas 'sf~negate))

(defmethod match=delta-cf ((sf-phi sf+sf) (sf-psi sf+sf) vars result)
  (match=gamma-delta-cf sf-phi sf-psi vars result 'sf~delta-p 'sf~delta-1-0 'match=delta-s 'match=delta-cf))

(defmethod match=gamma-s (sort var (sf-psi sf+sf) vars result)
  (match=gamma-delta-s sort var sf-psi vars result 'sf~beta-p 'sf~beta-subformulas 'sf~id))

(defmethod match=gamma-cf ((sf-phi sf+sf) (sf-psi sf+sf) vars result)
  (match=gamma-delta-cf sf-phi sf-psi vars result 'sf~gamma-p 'sf~gamma-1-0 'match=gamma-s 'match=gamma-cf))

(defmethod match=alpha-cf ((sf-phi sf+sf) (sf-psi sf+sf) result)
  (cond ((sf~alpha-p sf-phi)
	 (match=phi-list (list (sf~alpha-1 sf-phi) (sf~alpha-2 sf-phi)) sf-psi result 'match=equiv 'sf~negate))
	((and (sf~alpha-equiv-p sf-phi) (sf~plus-p sf-phi))
	 (append (match=phi-list (list (sf~alpha-equiv-1 sf-phi sgn*plus) (sf~alpha-equiv-2 sf-phi sgn*plus))
				 sf-psi result 'match=equiv 'sf~id)
		 (match=phi-list (list (sf~alpha-equiv-1 sf-phi sgn*minus) (sf~alpha-equiv-2 sf-phi sgn*minus))
				 sf-psi result 'match=equiv 'sf~id)))
	((and (sf~alpha-equiv-p sf-phi) (sf~minus-p sf-phi))
	 (match=phi-list (list (list (sf~alpha-equiv-1 sf-phi sgn*plus) (sf~alpha-equiv-2 sf-phi sgn*minus))
			       (list (sf~alpha-equiv-1 sf-phi sgn*minus) (sf~alpha-equiv-2 sf-phi sgn*plus)))
			 sf-psi result 'match=equiv 'sf~negate))
	(t nil)))

(defmethod match=equiv ((sf-phi-list list) (sf-psi sf+sf) result)
  (let ((sf-phi-list2 (apply 'append (mapcar 'sf~beta-subformulas sf-phi-list)))
	(sf-psi-list (sf~beta-subformulas sf-psi)))
    (when (= (length sf-psi-list) (length sf-phi-list2))
      (match=cf sf-phi-list2 sf-psi-list result))))

(defmethod match=equiv ((sf-phi sf+sf) (sf-psi sf+sf) result)
  (let ((sf-phi-list (sf~beta-subformulas sf-phi))
	(sf-psi-list (sf~beta-subformulas sf-psi)))
    (when (= (length sf-psi-list) (length sf-phi-list))
      (match=cf sf-phi-list sf-psi-list result))))

(defgeneric match=cmy (result)
  (:method ((result-list list))
	   (remove-if 'null (mapcar 'match=cmy result-list)))
  (:method ((result match+result))
	   (if (cstr~constraint-p (match~res-ct result))
	       ;; there are type constraints
	       ;; no matching
	       result
	     ;; no type constraints
	     ;; matching
	     (if (match~res-cmy result)
		 (multiple-value-bind (sigma sigma-1)
		     (match~syn (match~res-cmy result)
				(match~res-sigmat result) (match~res-sigmau result))
		   (when sigma
		     (match~res-change-sigma result sigma sigma-1)))
	       result))))

(defun match~permutations (vars)
  (if vars
      (apply 'append (mapcar #'(lambda (var) (mapcar #'(lambda (perm) (cons var perm)) (match~permutations (remove var vars)))) vars))
    (list nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#{
\subsection{Syntactical Matching}
#}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match~syn (cmy sigmat sigmau)
  (let ((terms (mapcar 'match~cmy-term cmy))
	(schemas (mapcar 'match~cmy-schema cmy)))
    (multiple-value-bind (schemas-1 sigma-1)
	(match=transform schemas sigmat sigmau (subst~create nil nil))
      (let ((orig-metavars (remove-if-not 'meta~p (apply 'append (mapcar 'term~variables schemas-1)))))
	(values (match=syn terms schemas-1 orig-metavars) sigma-1)))))

(defun match=transform (schemas sigmat sigmau sigma-1)
  (if schemas
      (let* ((schema-first (keim::subst~apply-nso sigma-1 (keim::subst~apply-nso sigmau (first schemas))))
	     (metavars (set-difference (remove-if-not 'meta~p (data~free-variables schema-first)) (subst~domain sigma-1)))
	     (metavars-with-typevars (remove-if-not #'(lambda (mvar) (cstr~typevars (term~type mvar))) metavars))
	     (new-metavars (mapcar #'(lambda (metavar)
				       (cstr~create-new-metavar metavar (cstr=inst-apply (term~type metavar)
											 (cstr~inst-from-subst sigmat))))
				   metavars-with-typevars)))
	(multiple-value-bind (schemas-rest sigma-1-rest)
	    (match=transform (rest schemas) sigmat sigmau (match=subst-insert-components metavars-with-typevars new-metavars sigma-1))
	  (values (cons (data~replace-free-variables-ntc schema-first metavars-with-typevars new-metavars) schemas-rest)
		  sigma-1-rest)))
    (values nil sigma-1)))

(defun match~create-new-var (term)
  (let* ((new-name (term~generate-new-name 'x (pds~environment omega*current-proof-plan)))
	 (new-var (term~variable-create new-name (term~type term))))
    (env~enter new-name new-var (pds~environment omega*current-proof-plan))
    new-var))

(defun match~create-help-metavar (type)
  (let* ((new-name (term~generate-new-name 'h (pds~environment omega*current-proof-plan)))
	 (new-metavar (meta~variable-create new-name type)))
    (env~enter new-name new-metavar (pds~environment omega*current-proof-plan))
    new-metavar))

(defun match=try-set (set try-func)
  (if set
      (let ((success (funcall try-func (first set))))
	(if success
	    success
	  (match=try-set (rest set) try-func)))
    nil))

(defun match=mapp-insert-components (varlist termlist mapping)
  (if varlist
      (mapp~insert-component (first varlist) (first termlist) (match=mapp-insert-components (rest varlist) (rest termlist) mapping))
    mapping))

(defun match=subst-insert-components (varlist termlist subst)
  (if varlist
      (subst~insert-component (first varlist) (first termlist) (match=subst-insert-components (rest varlist) (rest termlist) subst))
    subst))

(defun match=mapp-switch (mapp)
  (mapp~create (mapp~codomain mapp) (mapp~domain mapp)))

(defun match=mapp-equal (term1 term2 mapping)
  (if (find term1 (mapp~domain mapping))
      (keim~equal (mapp~get-component term1 mapping) term2)
    (keim~equal term1 term2)))

(defun match=term-head (term)
  (cond ((term~appl-p term) (data~appl-function term))
	((term~abstr-p term) (match=term-head (data~abstr-range term)))
	(t term)))

(defun match=term-arguments (term)
  (cond ((term~appl-p term) (data~appl-arguments term))
	((term~abstr-p term) (match=term-arguments (data~abstr-range term)))
	(t nil)))

(defun match=term-domain (term)
  (if (term~abstr-p term)
      (data~abstr-domain term)
    nil))

(defun match=term-range (term)
  (if (term~abstr-p term)
      (data~abstr-range term)
    term))

(defun match~create-abstr (vars term)
  (if vars
      (term~abstr-create vars term)
    term))
      
(defun match~create-appl (head args)
  (if args
      (term~appl-create head args)
    head))
      
(defun match=type-match-p (arg term)
  (when (keim~equal (term~type (match=term-head arg)) (term~type (match=term-head term)))
    (let* ((arg-args (match=term-arguments arg))
	   (term-args (match=term-arguments term))
	   (arg-length (list-length arg-args))
	   (term-length (list-length term-args)))
      (when (and (<= arg-length term-length)
		 (every 'keim~equal (mapcar 'term~type arg-args) (mapcar 'term~type (subseq term-args 0 arg-length))))
	(let ((rest-types (mapcar 'term~type (subseq term-args arg-length))))
	  (values t rest-types))))))

(defun match=arg-match-p (arg term)
  (let ((term-head (match=term-head term))
	(arg-head (match=term-head arg)))
    (if (and (not (meta~p arg-head)) (not (meta~p term-head)))
	(when (keim~equal arg-head term-head)
	  (match=type-match-p arg term))
      (match=type-match-p arg term))))

(defun match=projection-term (arg term args)
  (multiple-value-bind (success rest-types)
      (match=arg-match-p arg term)
    (when success
      (let* ((args-types (mapcar 'term~type args))
	     (lambda-domain-types (mapcar 'term~type (match=term-domain arg)))
	     (lambda-metavar-types (mapcar #'(lambda (type) (type~func-create args-types type)) lambda-domain-types))
	     (help-metavar-types (mapcar #'(lambda (rest-type) (type~func-create args-types rest-type)) rest-types))
	     (help-metavars (mapcar 'match~create-help-metavar (append lambda-metavar-types help-metavar-types)))
	     (lambda-vars (mapcar 'match~create-new-var args))
	     (arg-var (nth (position arg args) lambda-vars))
	     (projection-term (beta~normalize
			       (match~create-appl arg (mapcar #'(lambda (help-metavar) (term~appl-create help-metavar args))
							    help-metavars))))
	     (lambda-term (term~abstr-create
			   lambda-vars
			   (match~create-appl arg-var (mapcar #'(lambda (help-metavar) (term~appl-create help-metavar lambda-vars))
							    help-metavars)))))
	(values t projection-term lambda-term help-metavars)))))

(defun match=check-linearity (lambda-term)
  (let ((lambda-vars (match=term-domain lambda-term))
	(term (match=term-range lambda-term)))
    (not (set-difference lambda-vars (term~variables term)))))

(defun match=create-new-metavar (metavar type)
  (let ((new-name (term~generate-new-name (keim~name metavar) (pds~environment omega*current-proof-plan))))
    (meta~variable-create new-name type)))

(defun match=syn (term schema orig-metavars)
  (cond ((and (listp term) (listp schema))
	 (if (= (list-length term) (list-length schema))
	     (if term
		 (let ((first-subst (match=syn (first term) (first schema) orig-metavars)))
		   (when first-subst
		     (let ((rest-subst (match=syn (help~apply-subst-to-all first-subst (rest term))
						      (help~apply-subst-to-all first-subst (rest schema)) orig-metavars)))
		       (when rest-subst
			 (subst~disjoint-compose-substitution-nso first-subst rest-subst)))))
	       (subst~create nil nil))
	   (omega~error "The term lists ~A and ~A have different length." term schema)))
	;; -----------------------------------------------------------------------------------------------------------
	((term~abstr-p schema)
	 (let ((lambda-vars (data~abstr-domain schema)))
	   (match=syn (term~appl-create term lambda-vars) (data~abstr-range schema) orig-metavars)))
	;; -----------------------------------------------------------------------------------------------------------
	((term~abstr-p term)
	 (let ((lambda-vars (data~abstr-domain term)))
	   (match=syn (data~abstr-range term) (term~appl-create schema lambda-vars) orig-metavars)))
	;; -----------------------------------------------------------------------------------------------------------
	(t (let ((term-head (match=term-head term))
		 (schema-head (match=term-head schema))
		 (term-args (match=term-arguments term))
		 (schema-args (match=term-arguments schema)))
	     ;; -----------------------------------------------------------------------------------------------------------
	     (cond ((keim~equal term-head schema-head)
		    (match=syn term-args schema-args orig-metavars))
		   ;; -----------------------------------------------------------------------------------------------------------
		   ((meta~p schema-head)
		    (let ((ti (find-if #'(lambda (arg) (and (null (match=term-arguments arg)) (match=arg-match-p arg term))) schema-args)))
		      (if ti
			  (match=try-argument ti term schema orig-metavars)
			(cond ((or (term~constant-p term-head) (meta~p term-head)) (match=apply-proj-or-im term schema orig-metavars))
			      (t (match=apply-projection term schema orig-metavars))))))
		   ;; -----------------------------------------------------------------------------------------------------------
		   (t nil))))))

(defun match=apply-proj-or-im (term proj-term orig-metavars)
  (let ((subst (match=apply-projection term proj-term orig-metavars)))
    (if subst
	subst
      (match=apply-imitation term proj-term orig-metavars))))

(defun match=apply-projection (term proj-term orig-metavars)
  (let ((metavar (match=term-head proj-term)))
    (if (meta~p metavar)
	(let ((args (match=term-arguments proj-term)))
	  (match=try-set args #'(lambda (arg) (match=try-argument arg term proj-term orig-metavars))))
      (omega~error "The head of the right side is not a meta variable: ~A" metavar))))

(defun match=try-argument (arg term proj-term orig-metavars)
  (let ((args (match=term-arguments proj-term))
	(metavar (match=term-head proj-term)))
    (multiple-value-bind (success projection-term lambda-term)
	(match=projection-term arg term args)
      (when success
	(let ((subst (match=syn term projection-term orig-metavars)))
	  (when subst
	    (let* ((subst-lambda-term (beta~normalize (keim::subst~apply-nso subst lambda-term)))
		   (orig-metavar-subst (subst~insert-component metavar subst-lambda-term
							       (subst~restrict-substitution subst orig-metavars))))
	      (if (find metavar orig-metavars)
		  (when (match=check-linearity subst-lambda-term)
		    orig-metavar-subst)
		orig-metavar-subst))))))))

(defun match=apply-imitation (term proj-term orig-metavars)
  (let ((metavar (match=term-head proj-term)))
    (if (meta~p metavar)
	(let* ((args (match=term-arguments proj-term))
	       (term-head (match=term-head term))
	       (term-args (match=term-arguments term))
	       (lambda-vars (mapcar 'match~create-new-var args))
	       (lambda-types (mapcar 'term~type lambda-vars))
	       (help-metavar-types (mapcar #'(lambda (term-arg) (type~func-create lambda-types (term~type term-arg))) term-args))
	       (help-metavars (mapcar 'match~create-help-metavar help-metavar-types))
	       (help-metavar-appls (mapcar #'(lambda (help-metavar) (match~create-appl help-metavar lambda-vars)) help-metavars))
	       (help-metavar-beta-appls (mapcar #'(lambda (help-metavar) (match~create-appl help-metavar args)) help-metavars))
	       (lambda-term (match~create-abstr lambda-vars (match~create-appl term-head help-metavar-appls))))
	  (let ((subst (match=syn term-args help-metavar-beta-appls orig-metavars)))
	    (when subst
	      (let* ((subst-lambda-term (beta~normalize (keim::subst~apply-nso subst lambda-term)))
		     (orig-metavar-subst (subst~insert-component metavar subst-lambda-term
								 (subst~restrict-substitution subst orig-metavars))))
		(if (find metavar orig-metavars)
		    (when (match=check-linearity subst-lambda-term)
		      orig-metavar-subst)
		  orig-metavar-subst)))))
      (omega~error "The head is not a meta variable: ~A" metavar))))
