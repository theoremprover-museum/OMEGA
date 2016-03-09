;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
(in-package :omega)

; interface function    (tu~unify 
; format of equations:  (lhs rhs prems econtext)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;Module for Theory Unification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; global Variables
 
(setf tu*store nil)
(setf tu*store-use nil)
(setf tu*trace nil) 

;;; Objekt-Klassen
 
(defclass tu+equation ()
  ((lhs
    :initarg :lhs
    :initform nil
    :accessor tu=lhs)
   (rhs
    :initarg :rhs
    :initform nil
    :accessor tu=rhs)
   (prems
    :initarg :prems
    :initform nil
    :accessor tu~prems)
   (context
    :initarg :context
    :initform nil
    :accessor tu~context)))
 
(defclass tu+direq (tu+equation) ())
 
(defclass tu+undireq (tu+equation) ())
 
(defun tu=make-direq (lhs rhs prems context)
  (multiple-value-bind (renamed-lhs renamed-rhs renamed-prems)
      (tu=rename-free-variables lhs rhs prems)
    (make-instance 'tu+direq
		   :lhs renamed-lhs
		   :rhs renamed-rhs
		   :prems renamed-prems
		   :context context)))
 
(defun tu=make-undireq (lhs rhs prems context)
  (multiple-value-bind (renamed-lhs renamed-rhs renamed-prems)
      (tu=rename-free-variables lhs rhs prems)
    (make-instance 'tu+undireq
		   :lhs renamed-lhs
		   :rhs renamed-rhs
		   :prems renamed-prems
		   :context context)))
 
 (defclass tu+variation ()
  ((term
    :initarg :term
    :initform nil
    :accessor tu=term)
   (prems
    :initarg :prems
    :initform nil
    :accessor tu~prems)
   (context
    :initarg :context
    :initform nil
    :accessor tu~context)))
 
(defun tu=make-variation (term prems context)
  (make-instance 'tu+variation
   :term term
   :prems prems
   :context context))
 
(defclass tu+subst (subst+substitution)
  ((prems
    :initarg :prems
    :initform nil
    :accessor tu~prems)
   (context
    :initarg :context
    :initform nil
    :accessor tu~context)))
 
(defun tu=make-tu+subst (domain codomain prems context)
  (make-instance 'tu+subst
   :domain domain
   :codomain codomain
   :prems prems
   :context context))
 
;;; print functions
 
(defmethod print-object ((variation tu+variation) stream)
  (format stream "[")
  (post~print (tu=term variation) stream)
  (format stream ":")
  (mapc #'(lambda (prm) (post~print prm stream)) (tu~prems variation))
  (format stream ":")
  (mapc #'(lambda (cont) (post~print cont stream)) (tu~context variation))
  (format stream "]"))
 
(defmethod print-object ((equation tu+equation) stream)
  (format stream "[")
  (post~print (tu=lhs equation) stream)
  (format stream "==")
  (post~print (tu=rhs equation) stream)
  (format stream ":")
  (mapc #'(lambda (prm) (post~print prm stream)) (tu~prems equation))
  (format stream ":")
  (mapc #'(lambda (cont) (post~print cont stream)) (tu~context equation))
  (format stream "]"))
 

(defmethod print-object ((substitution tu+subst) stream)
  (format stream "[")
  (post~print (subst~domain substitution) stream)
  (format stream "->")
  (post~print (subst~codomain substitution) stream)
  (format stream ":")
  (mapc #'(lambda (prm) (post~print prm stream)) (tu~prems substitution))
  (format stream ":")
  (mapc #'(lambda (cont) (post~print cont stream)) (tu~context substitution))
  (format stream "]"))
 
;;; top-level functions for unification
 
(defun tu~unify (term1 term2 equs &optional (counter 5))
  (let ((subst? (term~alpha-unify term1 term2)))
    (if subst?  (tu=make-tu+subst (subst~domain subst?) (subst~codomain subst?) nil nil)
      (let ((classified (tu=classify-equations equs))
	    (t1 (tu=make-variation term1 nil nil))
	    (t2 (tu=make-variation term2 nil nil)))
	(tu=var+unify (list t1) (list t2) nil nil classified counter)))))


#|
(defun tu=var+unify (t1 t2 t1old t2old equs &optional (counter 5))
  (if (> counter 0)
      (let* ((counter (- counter 1))
      (variations (tu=remove-duplicates (mapcan #'(lambda (terms1)
           (tu=variations-srec terms1 equs)) t1)))
     
      (t1new (tu=set-difference  variations t1)))
 (print 't1new)
 (print t1new)
 (print `t1old)
 (print t1old)
 (print 'variations)
 (print variations)
 (if t1new
     (progn
       ;; nÃ¤chste Zeile?
       ;; (tu=var+unify t1new t2 (append t1 t1old) t2old equs counter)
       (mapcan #'(lambda (terms1new)
     (mapcar #'(lambda (terms2)
          (tu=unify-help terms1new terms2))
      t2))
        t1new)
       ;; switch vars
       (tu=var+unify t2 t1new t2old (append t1 t1old) equs counter))
  
   (progn (format t "~%depth ~A~%" counter)
   (print '(keine neuen variations)) ))
 (progn (print'(laeuft zu lange))
        t1))))     
|#
 
 
 
(defun tu=var+unify (t1 t2  t1old t2old equs &optional (counter 5))
  (tu=trace "new ~A ~A old ~A ~A counter ~A" t1 t2 t1old t2old counter)
  (if (> counter 0)
      (if (or (not (null t1))(not (null t2)))
	     (let ((t1new (tu=var+unify-help t1 t1old (append t2 t2old) equs)))
	       (if (subst~p  t1new)
		   t1new
		 (let ((t2new (tu=var+unify-help t2 t2old (append t1new t1 t1old) equs)))
		   (if (subst~p  t2new)
		       t2new
		     (tu=var+unify t1new t2new (append t1 t1old) (append t2 t2old) equs (1- counter))))))
	(progn
	  (tu=trace "~%depth ~A~%" counter)
	  (tu=trace "keine neuen variations")))
    (progn (tu=trace "laeuft zu lange")
	   t1)))
 
 
 
(defun tu=var+unify-help (var varold  varothers equs &optional collected)
  (if (null var) collected
    (let* ((varnew (tu=variations-srec (car var) equs))
    (varnewfiltered (tu=set-difference  varnew (append var varold collected)))
    (subst (tu=unify-help varnewfiltered varothers)))
      (if (subst~p subst) subst
 (tu=var+unify-help (rest var) (cons (car var) varold) varothers equs (append varnewfiltered collected))))))
 
 
;;; help functions
 

(defun tu=unify-help (vars1 vars2)
  (if (not (null vars1))
      (let* ((subst (tu=unify-help-help (car vars1) vars2)))
 (if subst
     subst
   (tu=unify-help (rest vars1) vars2)))))
 
(defun tu=unify-help-help (var1 vars2)
  (if (not (null vars2))
      (let ((subst (term~alpha-unify (tu=term var1) (tu=term (car vars2)))))
 (if (not (null subst))
     (tu=convert-subst+substitution-to-tu+subst subst (tu=get-prems var1 (car vars2) subst)(remove-duplicates (append (tu~context var1) (tu~context (car vars2)))))
   (tu=unify-help-help var1 (rest vars2))))))
 
 

(defun tu=get-prems (v1 v2 subst)
  (remove-duplicates (append
		      (mapcar #'(lambda (ps)
				  (subst~apply subst ps))
			      (tu~prems v1))
		      (mapcar #'(lambda (ps)
				  (subst~apply subst ps))
			      (tu~prems v2))) :test #'term~alpha-equal))
 
 
 

(defun tu=equal-varis? (variation1 variation2)
  (and (term~alpha-equal (tu=term variation1) (tu=term variation2))
       (mapcar #'(lambda (var1 var2)
     (term~alpha-equal var1 var2)) (tu~prems variation1) (tu~prems variation2))))
 
 
 
(defun tu=equal-subst? (sub1 sub2)
  (and (= (length (subst~domain sub1))
   (length (subst~domain sub2)))
       (every #'(lambda (var1 value1) (let ((value2 (subst~get-component var1 sub2)))
     (and value2 (term~alpha-equal value2 value1))))
       (subst~domain sub1)(subst~codomain sub1))))
 
(defun tu=equal-tu+subst? (sub1 sub2)
  (and (= (length (subst~domain sub1))
   (length (subst~domain sub2)))
       (every #'(lambda (var1 value1) (let ((value2 (subst~get-component var1 sub2)))
     (and value2 (term~alpha-equal value2 value1))))
       (subst~domain sub1)(subst~codomain sub1))))
 

(defun tu=convert-subst+substitution-to-tu+subst (subst &optional prems context)
  (when (not (null subst))
    (tu=make-tu+subst (subst~domain subst) (subst~codomain subst) prems context)))
 
(defgeneric tu=appl-elements (term)
  (:method ((term term+appl))
    (cons (data~appl-function term)
   (data~appl-arguments term)))
  (:method ((term term+primitive))
    (list term)))
   
(defun tu=substs-merge (substs)
  (if (<= (length substs) 1)
      (car substs)
    (tu=substs-merge
     (cons (mapcan #'(lambda (sub1)
         (mapcar #'(lambda (sub2)
       (tu=subs-merge sub1 sub2))
          (second substs)))
     (first substs))
    (rest (rest substs))))))
 
(defgeneric tu=subs-merge (sub1 sub2)
  (:method ((sub1 subst+substitution)(sub2 subst+substitution))
    (subst~compose-substitution sub1 sub2))
  (:method ((sub1 t)(sub2 t))
    nil))
 

;;; apply rewrites version with single equation application
;; stimmt das??? sollen hier alle kreiert werden/counter weg!!!!
 
(defun tu=variations-sall (variation equs &optional counter)
  (setf tu*store nil)
  (sys::gc)
  (let* ((start (get-internal-real-time))
 (result (tu=var+unify (list variation) (list variation) nil nil equs ))
  (time (- (get-internal-real-time) start)))
    (format T "Time: ~A  ~%Variations: ~A" time (length result))
    result))
 
#|
(defun tu=variations-shelp (variations equs collected &optional counter)
  (if (> counter 0)
      (let* ((counter (- counter 1))
      (variations (tu=remove-duplicates (mapcan #'(lambda (vari)
          (tu=variations-srec vari equs)) variations)))
     
      (new (tu=set-difference  variations collected)))
 (print 'new)
 (print new)
 (print `collected)
 (print collected)
 (print 'variations)
 (print variations)
 
 (if new
     (tu=variations-shelp new equs (append new collected) counter)
;;   (tu=variations-shelp new equs (tu=remove-duplicates (append new collected)) counter)
   (progn (format t "~%depth ~A~%" counter)
   collected)))
    (progn (print'(laeuft zu lange))
    collected)))
|#
 
(defun tu=variations-srec (variation equs)
  (if (not (typep (tu=term variation) 'term+appl))
      (tu=variations variation equs)
   
    (let ((stored? (if tu*store-use (assoc variation tu*store :test #'tu=equal-varis?) nil))) ;only use store when variable is set.
      (if stored? (rest stored?)
 (let* ((elems  (tu=make-variation (cons (data~appl-function (tu=term variation))
      (data~appl-arguments (tu=term variation)))
       (tu~prems variation)(tu~context variation)))
        ;;
        (sub-variations (tu=combine-variations
    (mapcar #'(lambda (elem)
         (tu=variations-srec (tu=make-variation elem (tu~prems variation) (tu~context variation)) equs))
     (tu=term elems))))
        ;;
        (top-variations (tu=variations variation equs))
        ;;
        (all-variations (append sub-variations top-variations)))
   (tu=trace "tu=variations-srec")
   (when tu*store-use (push (cons variation all-variations) tu*store))
   all-variations)))))
 
;;; utility functions
 
(defun tu=variations (variation equs)
  (let* ((alldirectedeqs (append equs
				 (mapcan #'(lambda (x)
					     (tu=new-undireqs x)) equs)))
	 (dummy (tu=trace "~A" alldirectedeqs))
	 (variations
	  (remove nil
		  (mapcar #'(lambda (es)
			      (tu=get-variations variation es))
			  alldirectedeqs))))
    (tu=remove-duplicates (cons variation variations))))
 
(defun tu=new-undireqs (equ)
  (when (typep equ 'tu+undireq)
    (list (tu=make-undireq (tu=rhs equ)(tu=lhs equ) (tu~prems equ) (tu~context equ)))))
 
(defun tu=get-variations  (variation equation)
  (let ((subst (term~alpha-match (tu=lhs equation) (tu=term variation))))
    (when subst
      (tu=make-variation
       (subst~apply  subst (tu=rhs equation))
       (remove-duplicates (append (tu~prems variation) (mapcar #'(lambda (ps)
								   (subst~apply subst ps))
							       (tu~prems equation))) :test #'term~alpha-equal)
       (remove-duplicates (append (tu~context equation) (tu~context variation)))))))
 
 
 
 
(defun tu=combine-variations (variations)
  (if (<= (length variations) 1)
      (car variations)
    (let* ((combine-first2 (mapcan #'(lambda (sub1)
          (mapcar #'(lambda (sub2)
        (tu=combine-variation sub1 sub2))
           (second variations)))
      (first variations))))
      (tu=combine-variations
       (cons combine-first2
      (rest (rest variations)))))))
 

(defun tu=subsumed-var (var variations)
  (some #'(lambda (varii)
     (term~alpha-equal (tu=term var)(tu=term varii)))
     ;(subsetp (tu~prems varii) (tu~prems var) :test #'term~alpha-equal)))
      variations))  
 
 
 

(defun tu=remove-duplicates (variations)
    (tu=remove-duplicates-help  (first variations) (rest variations) nil))
 

(defun tu=remove-duplicates-help (variation variations collectedvars)
  (if variation
     (tu=remove-duplicates-help (first variations)
           (rest variations)
           (if (or (tu=subsumed-var variation variations)
            (tu=subsumed-var variation collectedvars))
        collectedvars
      (cons variation collectedvars)))
    collectedvars))
 
 
 
 
 
(defun tu=set-difference (vars1 vars2)
  (set-difference vars1 vars2 :test #'(lambda (x y) (term~alpha-equal (tu=term x)(tu=term y)))))
 
 
(defun tu=combine-variation (fct arg)
  (tu=make-variation  (term~appl-create (tu=term fct) (list (tu=term arg)))
        (remove-duplicates (append (tu~prems fct) (tu~prems arg)) :test #'term~alpha-equal)
        (remove-duplicates (append (tu~context fct) (tu~context arg)))))
 
;;  (term~appl-create fct (list arg)))
 
;;; heuristic classification of equations
 
(defun tu=classify-equations (equations)
  (let ((newequs (remove-if #'(lambda (e)(or (tu=classify-both-free? e)
					     (tu=classify-id? e)))
			    equations))
	equs)
    (mapc #'(lambda (es) (let ((esclass (tu=classify-directed? es)))
      (push esclass equs)))
   newequs)
    (tu=trace "~%equs: ~A" equs)
    equs))

(defun tu=classify-id? (equation)
  (term~alpha-equal (first equation)(second equation)))

(defun tu=classify-both-free? (equation)
  (let ((free1 (term~free-variables (first equation)))
	(free2 (term~free-variables (second equation))))
  (and (set-difference free1 free2 :test #'term~alpha-equal)
       (set-difference free2 free1 :test #'term~alpha-equal))))
 
(defun tu=classify-directed? (equation)
  ;;falls eine richtung gibt dann (lhs->rhs)
  ;;sonst: nil
  (let ((lhs (first equation))
	(rhs (second equation))
	(prems (third equation))
	(econtext (fourth equation)))
    (tu=trace "seiten ~A ~A ~%prems ~A ~%context ~A" lhs rhs prems econtext)
 (cond ((set-difference
	 (term~free-variables lhs)
	 (term~free-variables rhs) :test #'term~alpha-equal)
        (tu=make-direq lhs rhs prems econtext))
       ((set-difference
	 (term~free-variables rhs)
	 (term~free-variables lhs) :test #'term~alpha-equal)
        (tu=make-direq rhs lhs prems econtext))
       ((some #'(lambda (arg) (term~alpha-equal lhs arg))(data~all-substructs rhs))
        (tu=make-direq rhs lhs prems econtext))
       ((some #'(lambda (arg) (term~alpha-equal rhs arg))(data~all-substructs lhs))
        (tu=make-direq lhs rhs prems econtext))
       (t  (tu=make-undireq lhs rhs prems econtext)))))
 
 
 
 
   
;;;   Tracen
(defun trace-all ()
  (trace tu=make-variation)
  (trace tu=make-direq)
  (trace tu=make-undireq)
  (trace tu~unify)
  (trace tu=var+unify)
  (trace tu=variations)
  (trace tu=variations-sall)
;;  (trace tu=variations-shelp)
  (trace tu=var+unify-help)
  (trace tu=unify-help)
  (trace tu=get-prems)
  (trace tu=equal-varis?)
  (trace tu=equal-subst?)
  (trace tu=equal-tu+subst?)
  (trace tu=convert-subst+substitution-to-tu+subst)
  (trace tu=appl-elements)
  (trace tu=substs-merge)
  (trace tu=subs-merge)
  (trace tu=variations-sall)
  (trace tu=variations-srec)
  (trace tu=variations)
  (trace tu=new-undireqs)
  (trace tu=get-variations)
  (trace tu=combine-variations)
  (trace tu=subsumed-var)
  (trace tu=remove-duplicates)
  (trace tu=remove-duplicates-help)
  (trace tu=set-difference)
 
  (trace tu=variations-srec)
  (trace tu=get-variations)
  (trace tu=new-undireqs)
  (trace tu=combine-variations)
  (trace tu=combine-variation)
  (trace tu=thm-convert)
  (trace tu=classify-equations)
  (trace tu=classify-both-free?)
  (trace tu=classify-directed?)
  )
 
(defun untrace-all ()
  (untrace tu=make-variation)
  (untrace tu=make-direq)
  (untrace tu=make-undireq)
  (untrace tu~unify)
  (untrace tu=var+unify)
  (untrace tu=variations)
  (untrace tu=variations-sall)
;;  (untrace tu=variations-shelp)
  (untrace tu=var+unify-help)
  (untrace tu=unify-help)
  (untrace tu=get-prems)
  (untrace tu=equal-varis?)
  (untrace tu=equal-subst?)
  (untrace tu=equal-tu+subst?)
  (untrace tu=convert-subst+substitution-to-tu+subst)
  (untrace tu=appl-elements)
  (untrace tu=substs-merge)
  (untrace tu=subs-merge)
  (untrace tu=variations-sall)
  (untrace tu=variations-srec)
  (untrace tu=variations)
  (untrace tu=new-undireqs)
  (untrace tu=get-variations)
  (untrace tu=combine-variations)
  (untrace tu=subsumed-var)
  (untrace tu=remove-duplicates)
  (untrace tu=remove-duplicates-help)
  (untrace tu=set-difference)
  (untrace tu=variations-srec)
  (untrace tu=get-variations)
  (untrace tu=new-undireqs)
  (untrace tu=combine-variations)
  (untrace tu=combine-variation)
  (untrace tu=thm-convert)
  (untrace tu=classify-equations)
  (untrace tu=classify-both-free?)
  (untrace tu=classify-directed?)
  )
 
 
 
;;;  Thoereme
 
;; lange version
(defgeneric tu=thm-convert (obj)
  (:method ((obj cons))
    (let ((converted (tu=thm-convert (car obj))))
      (if converted (cons converted (tu=thm-convert (cdr obj)))
        (tu=thm-convert (cdr obj)))))
  (:method ((obj null))
    nil))
 

(defmethod tu=thm-convert ((node node+node))
  (declare (edited  "05-AUG-2005")
    (authors Steffi)
    (input   "A node")
    (effect  )
    (value   "A list with 4 elements: lhs rhs prems context or NIL"))
  (let* ((context (list  node))
  (formula2list (tu=thm-convert-formula (node~formula node))))
    (when formula2list (reverse (cons context  (reverse formula2list))))
    ))
 
(defmethod tu=thm-convert ((thm PROB+PROBLEM))
  (declare (edited  "05-AUG-2005")
    (authors Steffi)
    (input   "A theorem")
    (effect  )
    (value   "A list with 4 elements: lhs rhs prems context or NIL"))
  (let* ((context (list  thm))
  (formula2list (tu=thm-convert-formula (node~formula (prob~conclusion thm)))))
    (when formula2list (reverse (cons context  (reverse formula2list))))
    ))
 
 
(defun tu=thm-convert-formula (formula)
  (declare (edited  "05-AUG-2005")
    (authors Steffi)
    (input   "A formula")
    (effect  )
    (value   "A list with 3 elements: lhs rhs prems"))
  (let* ((scope (tu=thm-scope formula))
	 (premsp t)
	 (prems (tu=thm-prems scope))
	 (lhs (tu=thm-lhs scope))
	 (rhs (tu=thm-rhs scope)))
    (tu=trace "scope ~A prems ~A lhs ~A rhs ~A"
	      scope
	      prems
	      lhs
	      rhs)
    (cond ((or (null lhs) (null rhs))
    nil)
   (t
    (list lhs rhs prems)))))
 
(defun tu=thm-scope (conclusion)
 (if (not (logic~universal-quantification-p conclusion)) ; :theory (prob~theory thm)))
     (progn
       (tu=trace  "conclusion ~A"       conclusion))
   (progn (tu=trace "universal func ~A arg ~A ~A ~A"
		    (data~appl-function conclusion)
		    (first (data~appl-arguments conclusion))
		    (data~abstr-domain (first (data~appl-arguments conclusion)))
		    (data~abstr-range (first (data~appl-arguments conclusion))))
   (let* ((var (logic~quantification-bound-variable conclusion))
	  (scope (logic~quantification-scope conclusion))
	  (newvar (term~variable-create (gentemp (keim~name var))
					(term~type var))))
    
     (tu=trace "subst ~A ~A"
	       (subst~create  (list var) (list newvar))
	       scope)
    
     ;; subst bauen var -> newvar und anwenden auf scope
   (tu=thm-scope   (subst~apply (subst~create  (list var) (list newvar)) scope))))))
 

 
(defun tu=thm-prems (scope)
  (cond ((or (logic~equality-p scope :theory (prob~theory thm))
	     (logic~equivalence-p scope :theory (prob~theory thm)))
	 (tu=trace "equation")
	 nil)
	((logic~implication-p scope :theory (prob~theory thm))
	 (tu=trace "implication ~A ~A"
		   scope
		   (first (data~appl-arguments  scope)))
	 (tu=thm-prems-rec (first (data~appl-arguments  scope))))
	(t
	 nil)))
 
 
 
(defun tu=thm-prems-rec (prems &optional collected-prems preliminary-prem)
  ;; input: formel
  ;; output: liste mit nicht-Konjunktionen
  (cond ((logic~conjunction-p prems :theory (prob~theory thm))
  (tu=trace "konjunction ~A ~A"
	    (data~appl-function prems)
	    (data~appl-arguments prems))
;;  (mapcar #'(lambda (args)
;;       (remove nil (list args collected-prems)))
;;        (data~appl-arguments prems))
  (tu=trace "coll-prems ~A"   collected-prems)
  (mapcan #'(lambda (arg) (tu=thm-prems-rec arg)) (data~appl-arguments prems)))
 (t
  (list prems))))
 
 
(defun tu=thm-lhs (scope)
  (cond ((or (logic~equality-p scope :theory (prob~theory thm))
      (logic~equivalence-p scope :theory (prob~theory thm)))
  (tu=trace "equation")
  (first (data~appl-arguments  scope)))
 ((and (logic~implication-p scope :theory (prob~theory thm))
       (or (logic~equality-p    (second (data~appl-arguments scope)) :theory (prob~theory thm))
    (logic~equivalence-p   (second (data~appl-arguments  scope)) :theory (prob~theory thm))))
  (tu=trace "implication-lhs")
  (first (data~appl-arguments (second (data~appl-arguments  scope)))))
 (t
  nil)))
 
 
 
(defun tu=thm-rhs (scope)
  (cond ((or (logic~equality-p scope :theory (prob~theory thm))
      (logic~equivalence-p scope :theory (prob~theory thm)))
  (tu=trace  "equation")
  (second (data~appl-arguments  scope)))
 ((and (logic~implication-p scope :theory (prob~theory thm))
       (or (logic~equality-p    (second (data~appl-arguments scope)) :theory (prob~theory thm))
    (logic~equivalence-p   (second (data~appl-arguments  scope)) :theory (prob~theory thm))))
  (tu=trace "implication-rhs")
  (second (data~appl-arguments (second (data~appl-arguments  scope)))))    
 (t
  nil)))
 

 
;;;;;;;;;;;;;;;;;;;;

(defun tu=rename-free-variables (lhs rhs prems)
  (let* ((freevars (append (term~free-variables lhs)
			   (term~free-variables rhs)))
	 (newvars (mapcar #'(lambda (var) (tu=variable-create (gentemp (keim~name var)) (term~type var)))
			  freevars)))
    (values
     (data~replace-free-variables lhs  freevars newvars)
     (data~replace-free-variables rhs  freevars newvars)
     (mapcar #'(lambda (prem) (data~replace-free-variables prem freevars newvars)) prems))))

(defun tu=variable-create (name type)
  (term~variable-create name type))


(defun tu=trace (string &rest args)
  (when tu*trace
    (let ((newstring (concatenate 'string "~%-----------------------~% " string)))
      (apply #'format (cons T (cons newstring args))))))
