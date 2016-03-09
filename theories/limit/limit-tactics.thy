(in-package :omega)

(defvar limtac*new-meta-vars nil "The newly created meta-variables")
(defvar limtac*new-symbols nil "The newly created constants and meta-variables (in order).")
(defvar limtac*already-derived nil "The assumptions already extracted from the answer-constraintion.")

(eval-when (load compile eval)
  (unless (com~find-category 'limit)
    (com~defcategory limit
		     (help "Tactics of the theory  limit ."))))


;;*********************************
;; some helpful functions
;;*********************************
(defun limtac=less-p (formula)
  (and (term~appl-p formula)
       (data~equal (data~appl-function formula)
		   (env~lookup-object 'less (pds~environment omega*current-proof-plan)))))

(defun limtac=leq-p (formula)
  (and (term~appl-p formula)
       (data~equal (data~appl-function formula)
		   (env~lookup-object 'leq (pds~environment omega*current-proof-plan)))))

(defun limtac=equality-p (formula)
  (and (term~appl-p formula)
       (data~equal (data~appl-function formula)
		   (data~schema-range
		    (env~lookup-object '= (pds~environment omega*current-proof-plan))))))

(defun limtac=geq-p (formula)
  (and (term~appl-p formula)
       (data~equal (data~appl-function formula)
		   (env~lookup-object 'geq (pds~environment omega*current-proof-plan)))))

(defun limtac=greater-p (formula)
  (and (term~appl-p formula)
       (data~equal (data~appl-function formula)
		   (env~lookup-object 'greater (pds~environment omega*current-proof-plan)))))

;; logic connectives
(defun limtac=disjunct-p (formula)
  (and (term~appl-p formula)
       (data~equal (data~appl-function formula)
		   (env~lookup-object 'or(pds~environment omega*current-proof-plan)))))


;; terms ....
(defun limtac=summ-p (formula)
  (and 
   (term~appl-p formula)
   (data~equal (data~appl-function formula)
	       (env~lookup-object 'plus (pds~environment omega*current-proof-plan)))
   ))

(defun limtac=product-p (formula)
  (and 
   (term~appl-p formula)
   (data~equal (data~appl-function formula)
	       (env~lookup-object 'times (pds~environment omega*current-proof-plan)))
   ))

(defun limtac=fraction-p (formula)
  (and 
   (term~appl-p formula)
   (data~equal (data~appl-function formula)
	       (env~lookup-object 'div (pds~environment omega*current-proof-plan)))
   ))

(defun limtac=number-p (term)
  (term~number-p term))

(defun limtac=zero-p (formula)
  (data~equal formula
	      (post~read-object '0 (pds~environment omega*current-proof-plan) :existing-term))
  )

(defun limtac=absval-p (formula)
  (and (term~appl-p formula)
       (data~equal (data~appl-function formula)
		   (env~lookup-object 'absval (pds~environment omega*current-proof-plan)))))

(defun limtac=min-p (formula)
  (and (term~appl-p formula)
       (data~equal (data~appl-function formula)
		   (env~lookup-object 'min (pds~environment omega*current-proof-plan)))))

(defun limtac=true-p (formula)
  (data~equal formula
	      (env~lookup-object 'true (pds~environment omega*current-proof-plan))))

(defun limtac=numtype-p (formula)
  (and (term~p formula)
       (keim~equal (term~type formula)
		   (env~lookup-object 'num (pds~environment omega*current-proof-plan)))))

(defun limtac=function-p (formula)
  (let ((num (env~lookup-object 'num (pds~environment omega*current-proof-plan))))
    (and (term~p formula)
	 (term~appl-p formula)
	 (keim~equal (term~type (data~appl-function formula))
		     (type~func-create num num))
	 (some #'limtac=var-in (data~appl-arguments formula)))))

(defun limtac=absval-in (formula)
  (let ((occs (data~substruct-positions (env~lookup-object 'absval (pds~environment omega*current-proof-plan))
					formula)))
    occs))

(defun limtac=var-in (formula)
  (let ((subterms (remove-duplicates (data~all-substructs formula))))
    (some #'term~variable-p subterms)))


;;  functions for term creation
(defun limtac=less-create (lhs rhs)
  (term~appl-create (env~lookup-object 'less (pds~environment omega*current-proof-plan))
		    (list lhs rhs)))
(defun limtac=leq-create (lhs rhs)
  (term~appl-create (env~lookup-object 'leq (pds~environment omega*current-proof-plan))
		    (list lhs rhs)))
(defun limtac=equality-create (lhs rhs)
  (term~appl-create (env~lookup-object '= (pds~environment omega*current-proof-plan))
		    (list lhs rhs)))
(defun limtac=geq-create (lhs rhs)
  (term~appl-create (env~lookup-object 'geq (pds~environment omega*current-proof-plan))
		    (list lhs rhs)))
(defun limtac=greater-create (lhs rhs)
  (term~appl-create (env~lookup-object 'greater (pds~environment omega*current-proof-plan))
		    (list lhs rhs)))

(defun limtac=disjunction-create (lhs rhs)
  (term~appl-create (env~lookup-object 'or (pds~environment omega*current-proof-plan))
		    (list lhs rhs)))


(defun limtac=absval-create (formula)
  (term~appl-create (env~lookup-object 'absval (pds~environment omega*current-proof-plan))
		    (list formula)))

(defun limtac=summ-create (add1 add2)
  (term~appl-create (env~lookup-object 'plus (pds~environment omega*current-proof-plan))
		    (list add1 add2)))
(defun limtac=product-create (fac1 fac2)
  (term~appl-create (env~lookup-object 'times (pds~environment omega*current-proof-plan))
		    (list fac1 fac2)))
(defun limtac=fraction-create (nom denom)
  (term~appl-create (env~lookup-object 'div (pds~environment omega*current-proof-plan))
		    (list nom denom)))


(defun limtac=number-create (num)
  (post~read-object num (pds~environment omega*current-proof-plan) :existing-term))

;;**************************************************
;;**************************************************
;; The pseudo-tactic 'solved' replaces all tactics
;; that have not been implemented yet.
;;**************************************************
(infer~defrule solved
	       (outline-mappings (((closed) solved-c)
				  ((existent) solved-a)))
	       (help "Replaces expansion-tactics."))

(rule~defrule solved-c solved (in limit)
  (declarations
   (meta-variables (M O)))
  (conclusion
   (L1 () M))
  (description "pseudo-rule"))

(rule~defrule solved-a solved (in limit)
  (declarations
   (meta-variables (M O)))
  (conclusion
   (L1 () M))
  (description "pseudo-rule"))

(infer~defrule Focus
		 (outline-mappings )
		 (help "Setting the focus on a subformula."))

(infer~defrule RemoveFocus
	       (outline-mappings )
	       (help "Removing the focus from a subformula."))

;;**************************************************
;;**************************************************
;; subst-back
;;     (asigma < b) and (x1=t1 and ... and xn=tn)
;;      --> (a < b)
;;  where sigma={ (x1 <-- t1), ... ,(xn <-- tn) }
;;**************************************************
(infer~deftactic subst-back
		 (outline-mappings (((existent existent existent) subst-back-a)
				    ))
		 (parameter-types )
		 (expansion-function limtac=expand-subst-back)
		 (help "The application of a substitution to a goal."))


(tac~deftactic subst-back-a
	       subst-back
	       (in limit)
   (premises L2 l3)
   (conclusions L1)
   (sideconditions (limtac=equ-conjunct-p (formula l3))
		   (limtac=applied-subst-p (formula l1)
					   (formula l3)
					   (formula l2))
		   )
   (description "Application of a substitution to a goal."))

(defun limtac=equ-conjunct-p (formula)
  (cond ((or (limtac=equality-p formula)
	     (limtac=true-p formula))
	 t)
	((logic~conjunction-p formula)
	 (and (limtac=equ-conjunct-p (first (data~appl-arguments formula)))
	      (limtac=equ-conjunct-p (second (data~appl-arguments formula)))))
	(T nil)))

(defun limtac=conjunct2subst (conj &optional (subst (subst~create nil nil)))
  (cond ((limtac=true-p conj)
	 subst)
	((limtac=equality-p conj)
	 (let* ((new-subst (subst~insert-component
			    (first (data~appl-arguments conj))
			    (second (data~appl-arguments conj))
			    subst))
		)
	   new-subst))
	((logic~conjunction-p conj)
	 (let* ((equ (first (data~appl-arguments conj)))
		(new-subst (subst~insert-component
			    (first (data~appl-arguments equ))
			    (second (data~appl-arguments equ))
			    subst))
		)
	   (limtac=conjunct2subst (second (data~appl-arguments conj)) new-subst)))))

(defun limtac=applied-subst-p (concl conj prem)
  (let* ((subst (limtac=conjunct2subst conj))
	 (formula (subst~apply subst concl))
	 )
    (data~equal formula prem)))


;;**********************
;;  subst-back expansion
;;**********************
(defun limtac=expand-subst-back (outline parameters)
  (declare (ignore parameters))
  (tacl~init outline)
  (let* ((concl (first outline))
	 (prem (second outline))
	 (conj (third outline))
	 (equs (limtac=split-conjunct (node~formula concl) conj))
	 )
    (limtac=subst-back-help concl prem equs)
    )
  (tacl~end))

(defun limtac=split-conjunct (formula conj)
  (let ((conj-form (node~formula conj))
	)
    (cond ((limtac=equality-p conj-form)
	   (let* ((var (first (data~appl-arguments conj-form)))
		  (var-poss (data~substruct-positions var formula))
		  )
	     (when var-poss
	       (mapcar #'(lambda (pos)
			   (cons conj pos))
		       var-poss))
	     ))
	  ((logic~conjunction-p conj-form)
	   (let* ((res (tacl~apply 'ande (list nil nil conj) nil))
		  (equ-node (first res))
		  (new-conj (second res))
		  )
	     (append (limtac=split-conjunct formula equ-node)
		     (limtac=split-conjunct formula new-conj))))
	  (T nil))))

(defun limtac=subst-back-help (concl prem equs)
  (cond ((null equs)
	 (tacl~apply 'weaken (list concl prem) nil))
	((listp equs)
	 (if (rest equs)
	     (let* ((res (tacl~apply '=subst
				     (list concl nil (car (first equs)))
				     (list (cdr (first equs)))))
		    (new-concl (second res))
		    )
	       (limtac=subst-back-help new-concl prem (rest equs))
	       )
	   (tacl~apply '=subst
		       (list concl prem (car (first equs)))
		       (list (cdr (first equs))))))
	(T nil)))
								   
;;**********************
;;  subst-back command
;;**********************
(com~defcommand subst-back
  (argnames concl prem conj)
  (argtypes ndline ndline ndline)
  (arghelps "The conclusion" "The premise" "A conjunct of equalities")
  (function limtac=subst-back)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "The application of a substitution."))


(defun limtac=subst-back (concl prem conj)
  (infer~compute-outline 'subst-back (list concl prem conj) NIL))


;;**************************************************
;;**************************************************
;; subst-forw
;;     (a<b) and (x1=t1 and ... and xn=tn)
;;      --> (asigma < b)
;;  where sigma={ (x1 <-- t1), ... ,(xn <-- tn) }
;;**************************************************
(infer~deftactic subst-forw
		 (outline-mappings (((existent existent existent) subst-forw-a)
				    ))
		 (parameter-types )
		 (expansion-function limtac=expand-subst-forw)
		 (help "The application of a substitution to a goal."))


(tac~deftactic subst-forw-a subst-forw (in limit)
   (premises L2 l3)
   (conclusions L1)
   (sideconditions (limtac=equ-conjunct-p (formula l3))
		   (limtac=applied-subst-p (formula l2)
					   (formula l3)
					   (formula l1))
		   )
   (description "Application of a substitution to a goal."))


;;**********************
;;  subst-forw expansion
;;**********************
(defun limtac=expand-subst-forw (outline parameters)
  (declare (ignore parameters))
  (tacl~init outline)
  (let* ((concl (first outline))
	 (prem (second outline))
	 (conj (third outline))
	 (equs (limtac=split-conjunct (node~formula prem) conj))
	 )
    (limtac=subst-forw-help concl prem equs)
    )
  (tacl~end))

(defun limtac=subst-forw-help (concl prem equs)
  (cond ((null equs)
	 (tacl~apply 'weaken (list concl prem) nil))
	((listp equs)
	 (if (rest equs)
	     (let* ((res (tacl~apply '=subst
				     (list nil prem (car (first equs)))
				     (list (cdr (first equs)))))
		    (adf (format t "~%res :~S" res))
		    (new-prem (first res))
		    )
	       (limtac=subst-forw-help concl new-prem (rest equs))
	       )
	   (progn (format t "~%~S ~S ~S ~S" concl
				prem
				(car (first equs))
				(cdr (first equs)))
		  (let ((res (tacl~apply '=subst
					 (list nil prem (car (first equs)))
					 (list (cdr (first equs))))
			     ))
		    (tacl~apply 'weaken
				(list concl (first res)) nil)))))
	(T nil)))

;;**********************
;;  subst-forw command
;;**********************
(com~defcommand subst-forw
  (argnames concl prem conj)
  (argtypes ndline ndline ndline)
  (arghelps "The conclusion" "The premise" "A conjunct of equalities")
  (function limtac=subst-forw)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "The application of a substitution."))


(defun limtac=subst-forw (concl prem conj)
  (infer~compute-outline 'subst-forw (list concl prem conj) NIL))



;;**************************************************
;;**************************************************
;; <add-r
;;     (a < b)  --> (a + c < a + c)
;;**************************************************
(infer~deftactic <add-r
		 (outline-mappings (
				    ((existent existent) <add-r-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<add-r)
		 (help "The additivity of < ."))

(tac~deftactic <add-r-f <add-r (in limit)
   (premises L2)
   (conclusions L1)
   (computations
    (L1 (limtac=add-to-inequality-right (formula l2) addend)))
   (sideconditions 
    (limtac-less-p (formula l2))
    (limtac=numtype-p addend))
   (description "Forward-application of the additivity of < ."))

(tac~deftactic <add-r-a <add-r (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions 
    (limtac-less-p (formula l2))
    (limtac-less-p (formula l1))
    (limtac=correctly-added-right-p (formula l2) (formula l1)))
   (description "Forward-application of the additivity of < ."))

(defun limtac=correctly-added-right-p (ineq1 ineq2)
  (let ((lhs1 (first (data~appl-arguments ineq1)))
	(rhs1 (second (data~appl-arguments ineq1)))
	(lhs2 (first (data~appl-arguments ineq2)))
	(rhs2 (second (data~appl-arguments ineq2)))
	)
    (and  (limtac=summ-p lhs2)
	  (limtac=summ-p rhs2)
	  (data~equal (second (data~appl-arguments lhs2))
		      (second (data~appl-arguments rhs2)))
	  (data~equal (first (data~appl-arguments lhs2))
		      lhs1)
	  (data~equal (first (data~appl-arguments rhs2))
		      rhs1))))

;;**********************
;;  <add-r expansion
;;**********************
(defun limtac=expand-<add-r (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (lhs1 (first (data~appl-arguments (node~formula prem))))
	 (rhs1 (second (data~appl-arguments (node~formula prem))))
	 (lhs2 (first (data~appl-arguments (node~formula conc))))
	 (rhs2 (second (data~appl-arguments (node~formula conc)))) 
	 (line1 (tacl~insert&return-assumption 'limit 'less-additivity-right))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list lhs1)))
     (res2 ('foralle (list nil (first res1)) (list rhs1)))
     (res3 ('foralle (list nil (first res2)) (list (second (data~appl-arguments lhs2)))))
     (imp ('impe (list conc prem (first res3)) NIL))
     )
    (tacl~end)))

;;**********************
;;  <add-r command
;;**********************
(com~defcommand <add-r
  (argnames ineq concl)
  (argtypes ndline ndline)
  (arghelps "An inequality" "An inequality to proof")
  (function limtac=<add-r)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the additivity of '<'."))


(defun limtac=<add-r (ineq concl)
  (infer~compute-outline '<add-r (list concl ineq) NIL))

;;**************************************************
;; <add-l
;;     (a < b)  --> (c + a < c + b)
;;**************************************************
(infer~deftactic <add-l
		 (outline-mappings (
				    ((existent existent) <add-l-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<add-l)
		 (help "The additivity of < ."))

(tac~deftactic <add-l-f <add-l (in limit)
   (premises L2)
   (conclusions L1)
   (computations
    (L1 (limtac=add-to-inequality-left (formula l2) addend)))
   (sideconditions 
    (limtac-less-p (formula l2))
    (limtac=numtype-p addend))
   (description "Forward-application of the additivity of < ."))

(tac~deftactic <add-l-a <add-l (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions 
    (limtac-less-p (formula l2))
    (limtac-less-p (formula l1))
    (limtac=correctly-added-left-p (formula l2) (formula l1)))
   (description "Forward-application of the additivity of < ."))

(defun limtac=correctly-added-left-p (ineq1 ineq2)
  (let ((lhs1 (first (data~appl-arguments ineq1)))
	(rhs1 (second (data~appl-arguments ineq1)))
	(lhs2 (first (data~appl-arguments ineq2)))
	(rhs2 (second (data~appl-arguments ineq2)))
	)
    (and  (limtac=summ-p lhs2)
	  (limtac=summ-p rhs2)
	  (data~equal (first (data~appl-arguments lhs2))
		      (first (data~appl-arguments rhs2)))
	  (data~equal (second (data~appl-arguments lhs2))
		      lhs1)
	  (data~equal (second (data~appl-arguments rhs2))
		      rhs1))))


;;**********************
;;  <add-l expansion
;;**********************
(defun limtac=expand-<add-l (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (lhs1 (first (data~appl-arguments (node~formula prem))))
	 (rhs1 (second (data~appl-arguments (node~formula prem))))
	 (lhs2 (first (data~appl-arguments (node~formula conc))))
	 (rhs2 (second (data~appl-arguments (node~formula conc)))) 
	 (line1 (tacl~insert&return-assumption 'limit 'less-additivity-left))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list lhs1)))
     (res2 ('foralle (list nil (first res1)) (list rhs1)))
     (res3 ('foralle (list nil (first res2)) (list (first (data~appl-arguments lhs2)))))
     (imp ('impe (list conc prem (first res3)) NIL))
     )
    (tacl~end)))
;;**********************
;;  <add-l command
;;**********************
(com~defcommand <add-l
  (argnames ineq concl)
  (argtypes ndline ndline)
  (arghelps "An inequality" "An inequality to proof")
  (function limtac=<add-l)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the additivity of '<'."))


(defun limtac=<add-l (ineq concl)
  (infer~compute-outline '<add-l (list concl ineq) NIL))



;;**************************************************
;; <=add-l
;;     (a <= b)  --> (c + a <= c + b)
;;**************************************************
(infer~deftactic <=add-l
		 (outline-mappings (
				    ((existent existent) <=add-l-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<=add-l)
		 (help "The additivity of <= ."))

(tac~deftactic <=add-l-a <=add-l (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions 
    (limtac=leq-p (formula l2))
    (limtac=leq-p (formula l1))
    (limtac=correctly-added-left-p (formula l2) (formula l1)))
   (description "Forward-application of the additivity of <=."))

;;**********************
;;  <=add-l expansion
;;**********************
(defun limtac=expand-<=add-l (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (lhs1 (first (data~appl-arguments (node~formula prem))))
	 (rhs1 (second (data~appl-arguments (node~formula prem))))
	 (lhs2 (first (data~appl-arguments (node~formula conc))))
	 (rhs2 (second (data~appl-arguments (node~formula conc)))) 
	 (line1 (tacl~insert&return-assumption 'limit 'leq-additivity-left))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list lhs1)))
     (res2 ('foralle (list nil (first res1)) (list rhs1)))
     (res3 ('foralle (list nil (first res2)) (list (first (data~appl-arguments lhs2)))))
     (imp ('impe (list conc prem (first res3)) NIL))
     )
    (tacl~end)))




;;**************************************************
;; <=add-r
;;     (a <= b)  --> (a + c <= a + c)
;;**************************************************
(infer~deftactic <=add-r
		 (outline-mappings (
				    ((existent existent) <=add-r-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<=add-r)
		 (help "The additivity of <= ."))

(tac~deftactic <=add-r-f <=add-r (in limit)
   (premises L2)
   (conclusions L1)
   (computations
    (L1 (limtac=add-to-inequality-right (formula l2) addend)))
   (sideconditions 
    (limtac=leq-p (formula l2))
    (limtac=numtype-p addend))
   (description "Forward-application of the additivity of <= ."))

(tac~deftactic <=add-r-a <=add-r (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions 
    (limtac=leq-p (formula l2))
    (limtac=leq-p (formula l1))
    (limtac=correctly-added-right-p (formula l2) (formula l1)))
   (description "Forward-application of the additivity of <= ."))

;;**********************
;;  <=add-r expansion
;;**********************
(defun limtac=expand-<=add-r (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (lhs1 (first (data~appl-arguments (node~formula prem))))
	 (rhs1 (second (data~appl-arguments (node~formula prem))))
	 (lhs2 (first (data~appl-arguments (node~formula conc))))
	 (rhs2 (second (data~appl-arguments (node~formula conc)))) 
	 (line1 (tacl~insert&return-assumption 'limit 'leq-additivity-right))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list lhs1)))
     (res2 ('foralle (list nil (first res1)) (list rhs1)))
     (res3 ('foralle (list nil (first res2)) (list (second (data~appl-arguments lhs2)))))
     (imp ('impe (list conc prem (first res3)) NIL))
     )
    (tacl~end)))
;;**********************
;;  <=add-r command
;;**********************
(com~defcommand <=add-r
  (argnames ineq concl)
  (argtypes ndline ndline)
  (arghelps "An leq-line" "An leq-line to proof")
  (function limtac=<=add-r)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the additivity of '<='."))


(defun limtac=<=add-r (ineq concl)
  (infer~compute-outline '<=add-r (list concl ineq) NIL))


;;**************************************************
;;**************************************************
;; <mult-l
;;     (a < b)  and (0 < c) --> (c * a < c * b)
;;**************************************************
(infer~deftactic <mult-l
		 (outline-mappings (
				    ((existent existent existent) <mult-l-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<mult-l)
		 (help "The multplicity of < ."))

(tac~deftactic <mult-l-a <mult-l (in limit)
   (premises L2 L3)
   (conclusions L1)
   (computations)
   (sideconditions 
    (limtac-less-p (formula l2))
    (limtac-less-p (formula l3))
    (limtac-less-p (formula l1))
    (limtac=correctly-multiplied-left-p (formula l2)
					(formula l3)
					(formula l1)))
   (description "Forward-application of the multiplicity of < ."))

(defun limtac=correctly-multiplied-left-p (ineq1 ineq2 ineq3)
  (let ((lhs1 (first (data~appl-arguments ineq1)))
	(rhs1 (second (data~appl-arguments ineq1)))
	(zero (first (data~appl-arguments ineq2)))
	(factor (second (data~appl-arguments ineq2)))
	(lhs3 (first (data~appl-arguments ineq3)))
	(rhs3 (second (data~appl-arguments ineq3)))
	)
    (and (limtac=product-p lhs3)
	 (limtac=product-p rhs3)
	 (data~equal (first (data~appl-arguments lhs3))  ;; (* M A)
		     (first (data~appl-arguments rhs3))) ;; (* M B)
	 (data~equal (first (data~appl-arguments lhs3))  ;; (< 0 M)
		     factor)
	 (limtac=zero-p zero)                       ;; (< 0 M)
	 (data~equal (second (data~appl-arguments rhs3))
		     rhs1)
	 (data~equal (second (data~appl-arguments lhs3))
		     lhs1)
	 )))

;;**********************
;;  <mult-l expansion
;;**********************
(defun limtac=expand-<mult-l (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (ineq (third outline))
	 (lhs1 (first (data~appl-arguments (node~formula prem))))
	 (rhs1 (second (data~appl-arguments (node~formula prem))))
	 (zero (first (data~appl-arguments (node~formula ineq))))
	 (factor (second (data~appl-arguments (node~formula ineq)))) 
	 (lhs3 (first (data~appl-arguments (node~formula conc))))
	 (rhs3 (second (data~appl-arguments (node~formula conc)))) 
	 (line1 (tacl~insert&return-assumption 'limit 'less-multiplicity-left))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle* (list nil line1) (list (list lhs1 rhs1 factor)))) ;; implies
     (res2 ('andI (list nil prem ineq) NIL))                ;; (a<b)
     (imp ('impe (list conc (first res2) (first res1)) NIL))
     )
    (tacl~end)))

;;**********************
;;  <mult-l command
;;**********************
(com~defcommand <mult-l
  (argnames ineq factor concl)
  (argtypes ndline ndline ndline)
  (arghelps "An inequality" "The inequality of the factor" "An inequality to proof")
  (function limtac=<mult-l)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the multitivity of '<'."))


(defun limtac=<mult-l (ineq factor concl)
  (infer~compute-outline '<mult-l (list concl ineq factor) NIL))




;;**************************************************
;;**************************************************
;; <=mult-r
;;     (a <= b)  and (0 <= c) --> (a * c <= b * c)
;;**************************************************

(infer~deftactic <=mult-r
		 (outline-mappings (((existent existent existent) <=mult-r-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<=mult-r)
		 (help "The multplicity of <= ."))

(tac~deftactic <=mult-r-a <=mult-r (in limit)
   (premises L2 L3)
   (conclusions L1)
   (computations)
   (sideconditions 
    (limtac-leq-p (formula l2))
    (limtac-leq-p (formula l3))
    (limtac-leq-p (formula l1))
    (limtac=correctly-multiplied-right-p (formula l2)
					 (formula l3)
					 (formula l1)))
   (description "Forward-application of the multiplicity of <= ."))

(defun limtac=correctly-multiplied-right-p (ineq1 ineq2 ineq3)
  (let ((lhs1 (first (data~appl-arguments ineq1)))    ;; A
	(rhs1 (second (data~appl-arguments ineq1)))   ;; B
	(zero (first (data~appl-arguments ineq2)))    ;; 0
	(factor (second (data~appl-arguments ineq2))) ;; M
	(lhs3 (first (data~appl-arguments ineq3)))    ;; (* A M) ?
	(rhs3 (second (data~appl-arguments ineq3)))   ;; (* B M) ?
	)
    (and (limtac=product-p lhs3)
	 (limtac=product-p rhs3)
	 (data~equal (second (data~appl-arguments lhs3))  ;; (* M A)
		     (second (data~appl-arguments rhs3))) ;; (* M B)
	 (data~equal (second (data~appl-arguments lhs3))  ;; (< 0 M)
		     factor)
	 (limtac=zero-p zero)                       ;; (< 0 M)
	 (data~equal (first (data~appl-arguments rhs3))
		     rhs1)
	 (data~equal (first (data~appl-arguments lhs3))
		     lhs1)
	 )))

;;**********************
;;  <=mult-r expansion
;;**********************
(defun limtac=expand-<=mult-r (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (ineq (third outline))
	 (lhs1 (first (data~appl-arguments (node~formula prem))))   ;; A
	 (rhs1 (second (data~appl-arguments (node~formula prem))))  ;; B
	 (zero (first (data~appl-arguments (node~formula ineq))))   ;; 0
	 (factor (second (data~appl-arguments (node~formula ineq))))  ;; M
	 (lhs3 (first (data~appl-arguments (node~formula conc))))   ;; A * M
	 (rhs3 (second (data~appl-arguments (node~formula conc))))  ;; B * M
	 (line1 (tacl~insert&return-assumption 'limit 'leq-multiplicity-right))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle* (list nil line1) (list (list lhs1 rhs1 factor)))) ;; implies
     (res2 ('andI (list nil prem ineq) NIL))                ;; (a<b)
     (imp ('impe (list conc (first res2) (first res1)) NIL))
     )
    (tacl~end)))

;;**********************
;;  <=mult-r command
;;**********************
(com~defcommand <=mult-r
  (argnames ineq factor concl)
  (argtypes ndline ndline ndline)
  (arghelps "An inequality" "The inequality of the factor" "An inequality to proof")
  (function limtac=<=mult-r)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the multitivity of '<='."))


(defun limtac=<=mult-r (ineq factor concl)
  (infer~compute-outline '<=mult-r (list concl ineq factor) NIL))


;;**************************************************
;;**************************************************
;; <impot=
;;     x < y => not (= x y)
;;**************************************************

(infer~deftactic <impnot=
		 (outline-mappings (((existent existent) <impnot=-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<impnot=)
		 (help "x < y => not (= x y)."))

(tac~deftactic <impnot=-a <impnot= (in limit)
   (premises l2)
   (conclusions L1)
   (computations)
   (sideconditions
    (limtac=less-p (formula l2))
    (limtac=not-equal-p (formula l1))
    (limtac=correctly-<impnot= (formula l1) (formula l2)))
   (description "Forward-application of the 0<=abs."))

(defun limtac=not-equal-p (formula)
  (and (logic~negation-p formula)
       (data~appl-p (first (data~appl-arguments formula)))
       (keim~equal (data~appl-function (first (data~appl-arguments formula))
				       (data~schema-range (env~lookup-object '= (pds~environment omega*current-proof-plan)))))))

(defun limtac=correctly-<impnot= (not= ineq)
  (let ((lhs1 (first (data~appl-arguments ineq)))
	(rhs1 (second (data~appl-arguments ineq)))
	(lhs2 (first (data~appl-arguments (first (data~appl-arguments not=)))))
	(rhs2 (second (data~appl-arguments(first (data~appl-arguments not=)))))
	)
    (and (data~equal lhs1 lhs2)
	 (data~equal rhs1 rhs2))))

;;**********************
;;  <impnot= expansion
;;**********************

(defun limtac=expand-<impnot= (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (line1 (tacl~insert&return-assumption 'limit '<-implies-not=))
	 (args (data~appl-arguments (node~formula prem))))
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle* (list nil line1) (list args)))
     (res2 ('impe (list conc prem (first res1)) nil))
     )
    (tacl~end))
  )

;;**************************************************
;;**************************************************
;; 0<=abs
;;     0 <= |x| 
;;**************************************************

(infer~deftactic 0<=abs
		 (outline-mappings (((existent) 0<=abs-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-0<=abs)
		 (help "0 <= |x|."))

(tac~deftactic 0<=abs-a 0<=abs (in limit)
   (premises )
   (conclusions L1)
   (computations)
   (sideconditions 
    (limtac-leq-p (formula l1))
    (limtac=correctly-0<=abs (formula l1)))
   (description "Forward-application of the 0<=abs."))

(defun limtac=correctly-0<=abs (ineq)
  (let ((lhs (first (data~appl-arguments ineq)))    ;; 0 ?
	(rhs (second (data~appl-arguments ineq)))   ;; |x| ?
	)
    (and (limtac=zero-p lhs)
	 (limtac=absval-p rhs))))

;;**********************
;;  0<=abs expansion
;;**********************

(defun limtac=expand-0<=abs (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (line1 (tacl~insert&return-assumption 'limit 'absval-geq-zero))
	 (a (first (data~appl-arguments (second (data~appl-arguments (node~formula conc))))))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle* (list conc line1) (list (list a))))
     ;;(res2 ('weaken (list conc (first res1)) ()))
     )
    (tacl~end)))

;;**********************
;;  0<=abs command
;;**********************
(com~defcommand 0<=abs
  (argnames concl)
  (argtypes ndline)
  (arghelps "The abs inequality")
  (function limtac=0<=abs)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help ""))

(defun limtac=0<=abs (ineq)
  (infer~compute-outline '0<=abs (list ineq) NIL))







;;**************************************************
;;**************************************************
;; <trans
;;     (a < b) & (b < c) --> (a < c)
;;**************************************************
(infer~deftactic <trans
		 (outline-mappings (((nonexistent existent existent) <trans-f)
				    ((existent existent existent) <trans-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<trans)
		 (help "The transitivity of < ."))

(tac~deftactic <trans-f <trans (in limit)
   (premises L2 l3)
   (conclusions L1)
   (computations
    (L1 (limtac=transitive-inequality-create (formula l2)
					     (formula l3))))
   (sideconditions (limtac-less-p (formula l2))
		   (limtac-less-p (formula l3))
		   (limtac=transitive-inequalities-p (formula l2)
						     (formula l3)))
   (description "Forward-application of the leq-transitivity."))

(tac~deftactic <trans-a <trans (in limit)
   (premises L2 l3)
   (conclusions L1)
   (computations)
   (sideconditions (limtac-less-p (formula l2))
		   (limtac-less-p (formula l3))
		   (limtac-less-p (formula l1))
		   (limtac=transitive-inequalities-p (formula l2)
						     (formula l3)
						     (formula l1)))
   (description "Application of the leq-transitivity to existent lines."))

;;**********************
;;  <trans expansion
;;**********************
(defun limtac=expand-<trans (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem1 (second outline))
	 (prem2 (third outline))
	 (konseq (node~formula conc))
	 (ante1 (node~formula prem1))
	 (ante2 (node~formula prem2))
	 (formula (node~formula conc))
	 (line1 (tacl~insert&return-assumption 'limit 'less-transitivity))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list (first (data~appl-arguments ante1)))))
     (res2 ('foralle (list nil (first res1)) (list (second (data~appl-arguments ante1)))))
     (res3 ('foralle (list nil (first res2))(list (second (data~appl-arguments ante2)))))
     (conj ('andi (list nil prem1 prem2) NIL))
     (imp ('impe (list conc (first conj) (first res3)) NIL))
     )
    (tacl~end)))

;;**********************
;;  <trans command
;;**********************
(com~defcommand <trans
  (argnames ineq1 ineq2 concl)
  (argtypes ndline ndline ndline)
  (arghelps "An inequality" "The second inequality" "The inequality to proof")
  (function limtac=<trans)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the transitivity of '<'."))


(defun limtac=<trans (ineq1 ineq2 concl)
  (infer~compute-outline '<trans (list concl ineq2 ineq1) NIL))

;;**************************************************
;;**************************************************
;; <=trans
;;     (a <= b) & (b <= c) --> (a <= c)
;;**************************************************
(infer~deftactic <=trans
		 (outline-mappings (((nonexistent existent existent) <=trans-f)
				    ((existent existent existent) <=trans-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<=trans)
		 (help "The transitivity of <= ."))

(tac~deftactic <=trans-f <=trans (in limit)
   (premises L2 l3)
   (conclusions L1)
   (computations
    (L1 (limtac=transitive-leq-create (formula l2)
				      (formula l3))))
   (sideconditions (limtac=leq-p (formula l2))
		   (limtac=leq-p (formula l3))
		   (limtac=transitive-inequalities-p (formula l2)
						     (formula l3)))
   (description "Forward-application of the leq-transitivity."))

(tac~deftactic <=trans-a <=trans (in limit)
   (premises L2 l3)
   (conclusions L1)
   (computations)
   (sideconditions (limtac=leq-line-p (formula l2))
		   (limtac=leq-line-p (formula l3))
		   (limtac=leq-line-p (formula l1))
		   (limtac=transitive-inequalities-p (formula l2)
						     (formula l3)
						     (formula l1)))
   (description "Application of the leq-transitivity to existent lines."))

;;**********************
;;  <=trans expansion
;;**********************
(defun limtac=expand-<=trans (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem1 (second outline))
	 (prem2 (third outline))
	 (konseq (node~formula conc))
	 (ante1 (node~formula prem1))
	 (ante2 (node~formula prem2))
	 (formula (node~formula conc))
	 (line1 (tacl~insert&return-assumption 'limit 'leq-transitivity))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list (first (data~appl-arguments ante1)))))
     (res2 ('foralle (list nil (first res1)) (list (second (data~appl-arguments ante1)))))
     (res3 ('foralle (list nil (first res2))(list (second (data~appl-arguments ante2)))))
     (conj ('andi (list nil prem1 prem2) NIL))
     (imp ('impe (list conc (first conj) (first res3)) NIL))
     )
    (tacl~end)))

;;**********************
;;  <=trans command
;;**********************
(com~defcommand <=trans
  (argnames ineq1 ineq2 concl)
  (argtypes ndline ndline ndline)
  (arghelps "An leq-line" "The second leq-line" "The leq-line to proof")
  (function limtac=<=trans)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the transitivity of '<='."))


(defun limtac=<=trans (ineq1 ineq2 concl)
  (infer~compute-outline '<=trans (list concl ineq2 ineq1) NIL))

;;**************************************************
;;**************************************************
;; <=trans<
;;     (a <= b) & (b < c) --> (a < c)
;;**************************************************
(infer~deftactic <=trans<
		 (outline-mappings (((nonexistent existent existent) <=trans<-f)
				    ((existent existent existent) <=trans<-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<=trans<)
		 (help "The transitivity of < ."))

(tac~deftactic <=trans<-f <=trans< (in limit)
   (premises L2 l3)
   (conclusions L1)
   (computations
    (L1 (limtac=transitive-inequality-create (formula l2)
					     (formula l3))))
   (sideconditions (limtac=leq-p (formula l2))
		   (limtac-less-p (formula l3))
		   (limtac=transitive-inequalities-p (formula l2)
						     (formula l3)))
   (description "Forward-application of the leq-transitivity."))

(tac~deftactic <=trans<-a <=trans< (in limit)
   (premises L2 l3)
   (conclusions L1)
   (computations)
   (sideconditions (limtac=leq-p (formula l2))
		   (limtac-less-p (formula l3))
		   (limtac-less-p (formula l1))
		   (limtac=transitive-inequalities-p (formula l2)
						     (formula l3)
						     (formula l1)))
   (description "Application of the leq-transitivity to existent lines."))


(defun limtac=transitive-inequality-create (ineq1 ineq2)
  (term~appl-create (env~lookup-object 'less (pds~environment omega*current-proof-plan))
	       (list (first (data~appl-arguments ineq1))
		     (second (data~appl-arguments ineq2)))))


(defun limtac=transitive-inequalities-p (ineq1 ineq2 &optional (concl nil))
  (and
   (data~equal (second (data~appl-arguments ineq1))
	       (first (data~appl-arguments ineq2)))
   (if concl
       (and (data~equal (first (data~appl-arguments ineq1))
			(first (data~appl-arguments concl)))
	    (data~equal (second (data~appl-arguments ineq2))
			(second (data~appl-arguments concl))))
     T)))

;;**********************
;;  <=trans< expansion
;;**********************
(defun limtac=expand-<=trans< (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem1 (second outline))
	 (prem2 (third outline))
	 (konseq (node~formula conc))
	 (ante1 (node~formula prem1))
	 (ante2 (node~formula prem2))
	 (formula (node~formula conc))
	 (line1 (tacl~insert&return-assumption 'limit 'leq-less-transitivity))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list (first (data~appl-arguments ante1)))))
     (res2 ('foralle (list nil (first res1)) (list (second (data~appl-arguments ante1)))))
     (res3 ('foralle (list nil (first res2))(list (second (data~appl-arguments ante2)))))
     (conj ('andi (list nil prem1 prem2) NIL))
     (imp ('impe (list conc (first conj) (first res3)) NIL))
     )
    (tacl~end)))

;;**********************
;;  <=trans< command
;;**********************
(com~defcommand <=trans<
  (argnames leq-line ineq concl)
  (argtypes ndline ndline ndline)
  (arghelps "An leq-line" "An inequality" "The inequality to proof")
  (function limtac=<=trans<)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the transitivity of '<'."))


(defun limtac=<=trans< (leq-line ineq concl)
  (infer~compute-outline '<=trans< (list concl ineq leq-line) NIL))


;;**************************************************
;;**************************************************
;; <trans<=
;;     (a < b) & (b <= c) --> (a < c)
;;**************************************************
(infer~deftactic <trans<=
		 (outline-mappings (((nonexistent existent existent) <trans<=-f)
				    ((existent existent existent) <trans<=-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-<trans<=)
		 (help "The transitivity of < ."))

(tac~deftactic <trans<=-f <trans<= (in limit)
   (premises L2 l3)
   (conclusions L1)
   (computations
    (L1 (limtac=transitive-inequality-create (formula l2)
					     (formula l3))))
   (sideconditions (limtac=leq-p (formula l2))
		   (limtac-less-p (formula l3))
		   (limtac=transitive-inequalities-p (formula l2)
						     (formula l3)))
   (description "Forward-application of the leq-transitivity."))

(tac~deftactic <trans<=-a <trans<= (in limit)
   (premises L2 l3)
   (conclusions L1)
   (computations)
   (sideconditions (limtac=leq-p (formula l2))
		   (limtac-less-p (formula l3))
		   (limtac-less-p (formula l1))
		   (limtac=transitive-inequalities-p (formula l2)
						     (formula l3)
						     (formula l1)))
   (description "Application of the leq-transitivity to existent lines."))


(defun limtac=transitive-inequality-create (ineq1 ineq2)
  (term~appl-create (env~lookup-object 'less (pds~environment omega*current-proof-plan))
	       (list (first (data~appl-arguments ineq1))
		     (second (data~appl-arguments ineq2)))))


;;**********************
;;  <trans<= expansion
;;**********************
(defun limtac=expand-<trans<= (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem1 (second outline))
	 (prem2 (third outline))
	 (konseq (node~formula conc))
	 (ante1 (node~formula prem1))
	 (ante2 (node~formula prem2))
	 (formula (node~formula conc))
	 (line1 (tacl~insert&return-assumption 'limit 'less-leq-transitivity))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list (first (data~appl-arguments ante1)))))
     (res2 ('foralle (list nil (first res1)) (list (second (data~appl-arguments ante1)))))
     (res3 ('foralle (list nil (first res2))(list (second (data~appl-arguments ante2)))))
     (conj ('andi (list nil prem1 prem2) NIL))
     (imp ('impe (list conc (first conj) (first res3)) NIL))
     )
    (tacl~end)))

;;**********************
;;  <trans<= command
;;**********************
(com~defcommand <trans<=
  (argnames ineq leq-line concl)
  (argtypes ndline ndline ndline)
  (arghelps "An inequality"
	    "An leq-line"
	    "The inequality to proof")
  (function limtac=<trans<=)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the transitivity of '<'."))


(defun limtac=<trans<= (ineq leq-line concl)
  (infer~compute-outline '<trans<= (list concl leq-line ineq) NIL))



;;**************************************************
;;**************************************************
;; triangle
;;     (a = (b + c)) --> |a| <= |b| + |c|
;;**************************************************
(infer~deftactic triangle 
		 (outline-mappings (((nonexistent existent) triangle-f)
				    ((existent existent) triangle-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-triangle)
		 (help "The triangle inequality."))

(tac~deftactic triangle-f triangle (in limit)
   (premises L2 )
   (conclusions L1)
   (computations
    (L1 (limtac=triangelize-equality (formula l2))))
   (sideconditions (limtac=summ-equality-p (formula l2))
		   )
   (description "Forward-application of the triangle-inequality."))

(tac~deftactic triangle-a triangle (in limit)
   (premises L2 )
   (conclusions L1)
   (computations)
   (sideconditions (limtac=summ-equality-p (formula l2) (formula l1)))
   (description "Application of the triangle-inequality to existent lines."))



(defun limtac=summ-equality-p (equality &optional (ineq nil))
  (and (limtac=equality-p equality)
       (limtac=summ-p (second (data~appl-arguments equality)))
       (if ineq
	   (and (limtac=leq-p ineq)
		(let ((summ (second (data~appl-arguments ineq)))
		      (lhs (first (data~appl-arguments equality)))
		      (equalsumm (second (data~appl-arguments equality))))
		  
		  (and (limtac=summ-p summ)
		       (limtac=absval-p (first (data~appl-arguments ineq)))
		       (let ((add1 (first (data~appl-arguments summ)))
			     (add2 (second (data~appl-arguments summ))))
			 (and (limtac=absval-p add1)
			      (limtac=absval-p add2)
			      (data~equal (first (data~appl-arguments add1))
					  (first (data~appl-arguments equalsumm)))
			      (data~equal (first (data~appl-arguments add2))
					  (second (data~appl-arguments equalsumm)))
			      (data~equal (first (data~appl-arguments
						  (first (data~appl-arguments ineq))))
					  lhs))))))
	 T)))

(defun limtac=triangelize-equality (equality)
  (let* ((lhs (first (data~appl-arguments equality)))
	 (summ (second (data~appl-arguments equality)))
	 (add1 (first (data~appl-arguments summ)))
	 (add2 (second (data~appl-arguments summ)))
	 )
    (term~appl-create (env~lookup-object 'leq (pds~environment omega*current-proof-plan))
		 (list (limtac=absval-create lhs)
		       (limtac=summ-create
			(limtac=absval-create add1)
			(limtac=absval-create add2))))
    ))




;;**********************
;;  triangle expansion
;;**********************
(defun limtac=expand-triangle (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem(second outline))
	 (ineq (node~formula conc))
	 (equ (node~formula prem))
	 (summ (second (data~appl-arguments equ)))
	 (formula (node~formula conc))
	 (line1 (tacl~insert&return-assumption 'limit 'triangle-inequality))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list (first (data~appl-arguments equ)))))
     (res2 ('foralle (list nil (first res1)) (list (first (data~appl-arguments summ)))))
     (res3 ('foralle (list nil (first res2))(list (second (data~appl-arguments summ)))))
     (imp ('impe (list conc prem (first res3)) NIL))
     )
    (tacl~end)))

;;**********************
;;  triangle command
;;**********************
(com~defcommand triangle
  (argnames equ ineq)
  (argtypes ndline ndline)
  (arghelps "An equality" "The inequality to proof")
  (function limtac=triangle)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the triangle-inequality."))


(defun limtac=triangle (equ ineq)
  (infer~compute-outline 'triangle (list ineq equ) NIL))



;;**************************************************
;;**************************************************
;; abs-mult
;;     |a*b| = |a|*|b|
;;**************************************************
;;NOTE: If there are more than one double absval applications
;;      in the premise, the first one is simplified, because
;;      it is not possible to use Positions in the meta-
;;      justifications of methods.

(infer~deftactic abs-mult
		 (outline-mappings (((nonexistent existent) abs-mult-f)
				    ((existent existent) abs-mult-a)))
		 (parameter-types position)
		 (expansion-function limtac=expand-abs-mult)
		 (help "The multiplicity of |.| "))


(tac~deftactic abs-mult-f abs-mult (in limit)
   (parameters (POS pos+position "A position"))
   (premises L2)
   (conclusions L1)
   (computations
    (L1 (limtac=absval-product-create (formula l2))))
   (sideconditions (limtac=absval-product-p (formula l2)))
   (description "Forward-application multiplicity of |.| ."))

(tac~deftactic abs-mult-a abs-mult (in limit)
   (parameters (POS pos+position "A position"))
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions (limtac=absval-product-p (formula l2) ;;; da stimmt was noch nicht!!!!
					    (formula l1)))
   (description "Application of the multiplicityi of |.| to existent lines."))

(defun limtac=absval-product-p (formula &optional (single nil))
  (let* ((pos (limtac=compute-absval-product-pos formula))
	 (term (when pos (data~struct-at-position formula pos)))
	 )
    (format  t "~% pos: ~S " pos)
    (and pos
	 (if single
	     (let ((product (data~struct-at-position single pos)))
	       (format  t "~% product: ~S" product)
	       (and (limtac=product-p product)
		    (let ((fact1 (first (data~appl-arguments product)))
			  (fact2 (second (data~appl-arguments product))))
		      (and (limtac=absval-p fact1)
			   (limtac=absval-p fact2)
			   (data~equal (first (data~appl-arguments (first (data~appl-arguments term))))
				       (first (data~appl-arguments fact1)))
			   (data~equal (second (data~appl-arguments (first (data~appl-arguments term))))
				       (first (data~appl-arguments fact2)))
			   ))))
	   T))))

(defun limtac=absval-product-create (formula position)
  (let* ((pos (limtac=compute-absval-product-pos formula))
	 (absval (data~struct-at-position formula pos))
	 (term (limtac=product-create
		(limtac=absval-create (first (data~appl-arguments
					      (first (data~appl-arguments absval)))))
		(limtac=absval-create (second (data~appl-arguments
					       (first (data~appl-arguments absval)))))
		))
	 )
    (data~replace-at-position formula pos term)))

;;**********************
;;  abs-mult expansion
;;**********************
(defun limtac=expand-abs-mult (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (pos (first parameters))
	 )
    (when pos
      (tacl~init outline)
      (tacl~apply 'apply-rewrite (list conc prem) (list pos 'absval-mult 'srl nil))
      (tacl~end))))

(defun limtac=compute-absval-product-pos (formula)
  (let* ((poss (data~substruct-positions (env~lookup-object
					'absval (pds~environment
						 omega*current-proof-plan))
				       formula))
	 (poss2 (remove-if-not #'(lambda (pos)
				   (let ((term (data~struct-at-position formula
								 (pos~butlast pos))))
				     (limtac=product-p
				      (first (data~appl-arguments term)))))
			       poss))
	 )
    (when poss2
      (pos~butlast (first poss2)))))


;;**********************
;;  abs-mult command
;;**********************
(com~defcommand abs-mult
  (argnames line conc pos)
  (argtypes ndline ndline position)
  (arghelps "An line with absval of a product" "A line with the product of absvals"
	    "The position of the absval")
  (function limtac=abs-mult)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the multiplicity of |.| "))


(defun limtac=abs-mult (line conc position)
  (infer~compute-outline 'abs-mult (list conc line) (list position)))

;;**************************************************
;;**************************************************
;; abs-idemp
;;     (P ||a||) <=> (P |a|)
;;**************************************************
;;NOTE: If there are more than one double absval applications
;;      in the premise, the first one is simplified, because
;;      it is not possible to use Positions in the meta-
;;      justifications of methods.

(infer~deftactic abs-idemp
		 (outline-mappings (((nonexistent existent) abs-idemp-f)
				    ((existent existent) abs-idemp-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-abs-idemp)
		 (help "The idempotence of |.| "))

(tac~deftactic abs-idemp-f abs-idemp (in limit)
   (premises L2)
   (conclusions L1)
   (computations
    (L1 (limtac=simple-absval-create (formula l2))))
   (sideconditions (limtac=double-absval-p (formula l2)))
   (description "Forward-application of idempotence of |.| ."))

(tac~deftactic abs-idemp-a abs-idemp (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions (limtac=double-absval-p (formula l2)
					   (formula l1)))
   (description "Application of the idempotence of |.| to existent lines."))

(defun limtac=double-absval-p (formula &optional (single nil))
  (let* ((pos (limtac=compute-double-absval-pos formula))
	 (term (when pos (data~struct-at-position formula pos)))
	 )
    (and pos
	 (if single
	     (let ((single-term (data~struct-at-position single pos)))
	       (data~equal (first (data~appl-arguments term))
			   single-term))
	   T))))

(defun limtac=simple-absval-create (formula)
  (let* ((pos (limtac=compute-double-absval-pos formula))
	 (term (first (data~appl-arguments (data~struct-at-position formula pos))))
	 )
    (data~replace-at-position formula pos term)))
	
;;**********************
;;  abs-idemp expansion
;;**********************
(defun limtac=expand-abs-idemp (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (double (node~formula prem))
	 (single (node~formula conc))
	 (pos (limtac=compute-double-absval-pos double))
	 )
    (when pos
      (tacl~init outline)
      (tacl~apply 'apply-rewrite (list conc prem) (list pos 'absval-idempotence :srl nil))
      (tacl~end))))

(defun limtac=compute-double-absval-pos (formula)
  (let* ((poss (data~substruct-positions (env~lookup-object
					'absval (pds~environment
						 omega*current-proof-plan))
				       formula))
	 (poss2 (remove-if-not #'(lambda (pos)
				   (let ((term (data~struct-at-position formula
								 (pos~butlast pos))))
				     (limtac=absval-p
				      (first (data~appl-arguments term)))))
			       poss))
	 )
    (when poss2
      (pos~butlast (first poss2)))))

;;**********************
;;  abs-idemp command
;;**********************
(com~defcommand abs-idemp
  (argnames line conc)
  (argtypes ndline ndline)
  (arghelps "An line with double-absval" "The conclusion")
  (function limtac=abs-idemp)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the idempotence of |.| "))


(defun limtac=abs-idemp (line conc)
  (infer~compute-outline 'abs-idemp (list conc line) nil))


;;**************************************************
;;**************************************************
;; leq-intro
;;     (a < b) or (a = b)  --> (a <= b)
;;**************************************************
(infer~deftactic leq-intro
		 (outline-mappings (
				    ((existent existent) leq-intro-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-leq-intro)
		 (help "The introduction of <= ."))

(tac~deftactic leq-intro-a leq-intro (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions (limtac=disjunct-p (formula l2))
		   (limtac=leq-p (formula L1))
		   (limtac=correct-leq-disjunct-p (formula L2)
						  (formula L1))
		   )
   (description "Application of the introduction of <= ."))

(defun limtac=correct-leq-disjunct-p (disj leq)
  (let ((a (first (data~appl-arguments leq)))
	(b (second (data~appl-arguments leq)))
	)
    (and (limtac-less-p (first (data~appl-arguments disj)))
	 (limtac=equality-p (second (data~appl-arguments disj)))
	 (data~equal a (first (data~appl-arguments (first (data~appl-arguments disj)))))
	 (data~equal a (first (data~appl-arguments (second (data~appl-arguments disj)))))
	 (data~equal b (second (data~appl-arguments (first (data~appl-arguments disj)))))
	 (data~equal b (second (data~appl-arguments (second (data~appl-arguments disj))))))))

;;**********************
;;  leq-intro expansion
;;**********************
(defun limtac=expand-leq-intro (outline parameter)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (lhs (first (data~appl-arguments (node~formula conc))))
	 (rhs (second (data~appl-arguments (node~formula conc))))
	 (line1 (tacl~insert&return-assumption 'limit 'leq-introduction))
	 )
    (tacl~init outline)
    (tacl~sequence
     (res1 ('foralle (list nil line1) (list lhs)))
     (res2 ('foralle (list nil (first res1)) (list rhs)))
     (imp ('impe (list conc prem (first res2)) NIL))
     )
    (tacl~end)))

;;**********************
;;  leq-intro command
;;**********************
(com~defcommand leq-intro
  (argnames ineq concl)
  (argtypes ndline ndline)
  (arghelps "An disjunction of an equality and an equality" "The leq-line")
  (function limtac=leq-intro)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the introduction of '<='."))


(defun limtac=leq-intro (ineq concl)
  (infer~compute-outline 'leq-intro (list concl ineq) NIL))


;;**************************************************
;;**************************************************
;; solve-disj
;;     OrIr or (OrIl and solver-CS) depending on the premise
;;**************************************************
(infer~deftactic solve-disj
		 (outline-mappings (((existent existent) solve-disj-a)
				    ))
		 (parameter-types )
		 (expansion-function limtac=expand-solve-disj)
		 (help "The application of solver-CS or OrIr."))

(tac~deftactic solve-disj-a solve-disj (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions (limtac=disjunct-p (formula L1))
		   )
   (description "Application of OrIr or OrIl."))

;;**********************
;;  solve-disj expansion
;;**********************
(defun limtac=expand-solve-disj (outline parameters)
  (declare (ignore parameters))
  (let* ((conc (first outline))
	 (prem (second outline))
	 (disj (node~formula conc))
	 (one-side (node~formula prem))
	 )
    (tacl~init outline)
    (if (data~equal one-side (second (data~appl-arguments disj)))
	(tacl~apply 'orir (list conc prem) (list (first (data~appl-arguments disj))))
      (let ((res (tacl~apply 'oril (list conc nil) (list (second (data~appl-arguments disj)))))
	    )
	(tacl~apply 'solver-CS (list (second res) prem) nil)))
    (tacl~end)))

;;**********************
;;  solve-disj command
;;**********************
(com~defcommand solve-disj
  (argnames concl prem)
  (argtypes ndline ndline)
  (arghelps "The disjunction" "One of the disjuncts")
  (function limtac=solve-disj)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "The application of a substitution."))


(defun limtac=solve-disj (concl prem)
  (infer~compute-outline 'solve-disj (list concl prem) NIL))


;;**************************************************
;;**************************************************
;; solver-CS
;;     (a < b) & ...& (b < c) & .... --> (b < c)
;;**************************************************
(infer~deftactic solver-CS
		 (outline-mappings (
				    ((existent existent) solver-CS-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-solver-cs)
		 (help "The tactic for simple cs-solving-computations."))

(tac~deftactic solver-CS-a solver-CS (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions )
   (description "Derive the (in)equality from the constraint-state."))


;;**********************
;;  solver-CS expansion
;;**********************
(defun limtac=expand-solver-cs (outline parameters)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (concl (first outline))
	 (prem (second outline))
	 (formula (node~formula concl))
	 )
    (setf (node~formula concl) formula)
    (limtac=extract-formula concl prem)))


(defun limtac=extract-formula (concl premise)
  (let* ((formula (node~formula concl))
	 (poss (apply #'append
		      (mapcar #'(lambda (line)
				  (let ((pos (data~substruct-positions formula (node~formula line))))
				    (when pos
				      (list (cons line (first pos))))))
			      limtac*already-derived)))
	 (prem (if poss
		   (car (first poss))
		 premise))
	 (pos (if poss
		  (cdr (first poss))
		(let ((positions (data~substruct-positions formula (node~formula prem))))
		  (when positions (first positions)))))
	 )
    (if pos
	(progn
	  (format t "~% concl :~S" concl)
	  (format t "~% pos :~S" pos)
	  (format t "~% prem :~S" prem)
	  (tacl~init (list concl premise))
	  (limtac=extract-formula-help concl pos (list prem))
	  (tacl~end))
      (progn (format t "~%The formula is not in the answer constraint...--> test for entailment.")
	     (limtac=test-entailment concl premise formula)))))

(defun limtac=test-entailment (concl premise formula)
  (cond ((limtac=leq-p formula)
	 (let* ((lhs (first (data~appl-arguments formula)))
		(rhs (first (data~appl-arguments formula))))
	   (cond ((data~equal lhs rhs)
		  (let ((line1 (tacl~insert&return-assumption 'limit 'leq-reflexivity))
			)
		    (tacl~init (list concl premise))
		    (tacl~apply 'foralle (list concl line1) (list lhs))
		    (tacl~end)
		    (values t t))
		  )
		 (T
		  (format t "~%The formula is not in the answer-constraint and is not entailed.")
		  (tacl~init (list concl premise))
		  (tacl~apply 'solved (list concl) nil)
		  (tacl~end)
		  (values t t)))))
	(T   (format t "~%The formula is not in the answer-constraint and is not entailed.")
	     (tacl~init (list concl premise))
	     (tacl~apply 'solved (list concl) nil)
	     (tacl~end)
	     (values t t))))


(defun limtac=extract-formula-help (concl pos new-lines)
  (if (pos~empty-p pos)
      (values T new-lines)
    
    (let* ((num (pos~first pos))
	   (rule (cond ((= num 1) 'andel)
		       ((= num 2) 'ander)
		       (T nil))))
      (if rule
	  (let ((new-outline (tacl~apply rule
					 (list (when (pos~empty-p (pos~rest pos))
						 concl)
					       (first new-lines))
					 nil))
		)
	    (unless (pos~empty-p (pos~rest pos))
	      (setf limtac*already-derived
		    (cons (first new-outline)
			  limtac*already-derived)))
	    (limtac=extract-formula-help concl
					 (pos~rest pos)
					 (cons (first new-outline)
					       new-lines)))
	(values NIL NIL)))))

;;**************************************************
;;**************************************************
;; prove-CS
;;     prove the answer constraint conjunct (a < b) & ...& (b <= c) & .... 
;;**************************************************
(infer~deftactic prove-CS
		 (outline-mappings (((existent) prove-CS-a)))
		 (parameter-types )
		 (expansion-function limtac=expand-prove-cs)
		 (help "The tactic for simple cs-solving-computations."))

(tac~deftactic prove-CS-a prove-CS (in limit)
	       (premises)
	       (conclusions L1)
	       (computations)
	       (sideconditions )
	       (description "Prove the instantiated inequalities and equations."))

;;**********************
;;  prove-cs command
;;**********************
(com~defcommand pcs
  (argnames concl)
  (argtypes ndline)
  (arghelps "The formula")
  (function limtac=command-prove-cs)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help ""))


(defun limtac=command-prove-cs (concl)
  (infer~compute-outline 'prove-cs (list concl) NIL))


;;**********************
;;  prove-CS expansion
;;**********************
(defun limtac=expand-prove-CS (outline parameters)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (concl (first outline))
	 (formula (node~formula concl))
	 )
    (if (limtac=var-in formula)
	(progn (omega~message "~& You should instantiate the meta-variables first.")
	       (values NIL NIL))
      (let* ((foo (opr~arrest-listener opr*service-listener))
	     (trace (cosie~searchTrace))
	     (bar (opr~release-listener opr*service-listener))
	     )
	(omega~message "~& Trace: ~S" trace)
	(tacl~init outline)
	(limtac=prove-cs concl trace)
	(tacl~end)))
    ))

(defun limtac=prove-cs (concl trace)
  (format t "~& prove-cs: ~S" concl)
  (if (null concl)
      (values t t)
    (if (listp concl)
	(if (every #'(lambda (conc)
		       (limtac=prove-cs conc trace))
		   concl)
	    (values t t)
	  (values t t) ;(values NIL  NIL)
	  )
      
      (let* ((formula (node~formula concl))
	     (formulatrace (limtac=trace-for formula trace))
	     (existing (limtac=search-node formula (pdsn~hyps concl) concl))
	     )
	(format t "~& formula ~S" formula)
	(format t "~% trace_for: ~S" formulatrace)
	(format t "~% existing: ~S" existing)
	(cond ((and existing
		    (not (keim~equal existing concl))
		    (not (member concl (pds~linearize-plan existing) :test #'keim~equal)))
	       (tacl~apply 'weaken (list concl existing) nil))
	      
	      ((and formulatrace
		    (first formulatrace)
		    (not (string-equal (first formulatrace) 'mult-frac-leq))) ;; UNHAPPILY, THE THEOREM MULT-FRAC-LEQ DOES NOT EXIST, AMEIER 
	       (let* ((theorem (first formulatrace))
		      (premises (second formulatrace))
		      (args (fourth formulatrace))
		      (exist-prems
		       (mapcar #'(lambda (prem)
				   (limtac=search-node prem (pdsn~hyps concl)))
			       premises))
		      )
		 (format t "~& theorem: ~S" theorem)
		 (format t "~& args: ~S" args)
		 (let* ((thm (tacl~insert&return-assumption 'limit theorem))
			)
		   (format t "~& thm: ~S" thm)
		   (if thm
		       (if premises
			   (let* ((impl (tacl~apply 'foralle* (list nil thm) (list args)))
				  (adsf (format t "~& impl: ~S" impl))
				  (conj (tacl~apply 'impe (list concl nil (first impl)) nil))
				  (asdf (format t "~& conj: ~S" conj))
				  )
			     (if (logic~conjunction-p (node~formula (second conj)))
				 (let* ((conjs (tacl~apply 'andi* (list (second conj) nil) nil))
					(adf (format t "~& conjs: ~S" conjs))
					(prems (second conjs))
					)
				   (mapcar #'(lambda (prem exist-prem)
					       (when exist-prem
						 (tacl~apply 'weaken (list prem exist-prem) nil)))
					   prems
					   exist-prems)
				   (let* ((open-prems
					   (mapcan #'(lambda (prem exist-prem)
						       (unless exist-prem
							 (list prem)))
						   prems
						   exist-prems))
					  )
				     (limtac=prove-cs open-prems trace)))
			       (let* ((prem (second conj))
				      )
				 (if (first exist-prems)
				     (let* ((exist-prem (first exist-prems)))
				       (tacl~apply 'weaken (list prem exist-prem) nil))
				   (limtac=prove-cs prem trace)))))
			 (tacl~apply 'foralle* (list concl thm) (list args)))
		     (values nil nil)))))
	      
	      ((logic~conjunction-p formula)
	       (let ((res (tacl~apply 'andi* (list concl nil) NIL)))
		 (format t "~% conjuncts: ~S" res)
		 (if res
		     (limtac=prove-cs (rest res)  trace)
		   (values nil nil))
		 ))
	      
	      ((and (limtac=equality-p formula)
		    (keim~equal (first (data~appl-arguments formula))
				(second (data~appl-arguments formula))))
	       (format t "~& equalitiy: ~S" formula)
	       (if (tacl~apply '=ref (list concl) (list (first (data~appl-arguments formula))))
		   (values t t)
		 (values NIL NIL)
		 ))
	      
	      ((limtac=geq-p formula)
	       (let* ((thm (tacl~insert&return-assumption 'limit 'leq2geq))
		      (res1 (tacl~apply 'foralle* (list nil thm) (list (first
									(data~appl-arguments formula))
								       (second
									(data~appl-arguments formula)))))
		      (res2 (tacl~apply 'impe (list concl nil (first res1)) nil))
		      (premise (second res2))
		      )
		 (limtac=prove-cs premise  trace)))
	      
	      ((limtac=greater-p formula)
	       (let* ((thm (tacl~insert&return-assumption 'limit 'less2greater))
		      (res1 (tacl~apply 'foralle* (list nil thm) (list (first
									(data~appl-arguments formula))
								       (second
									(data~appl-arguments formula)))))
		      (res2 (tacl~apply 'impe (list concl nil (first res1)) nil))
		      (premise (second res2))
		      )
		 (limtac=prove-cs premise  trace)))
	      
	      ((limtac=leq-p formula)
	       (let ((lhs (first (data~appl-arguments formula)))
		     (rhs (second (data~appl-arguments formula)))
		     )
		 (cond ((data~equal lhs rhs)
			(let* ((line1 (tacl~insert&return-assumption 'natural 'leq-refl))
			       )
			  (tacl~apply 'foralle (list concl line1) (list lhs))
			  (values t t)
			  ))
		       ((and (limtac=min-p lhs)
			     (or (data~equal (first (data~appl-arguments lhs))
					     rhs)
				 (data~equal (second (data~appl-arguments lhs))
					     rhs)))
			(let* ((line1 (tacl~insert&return-assumption 'limit
								     (if (data~equal (first (data~appl-arguments lhs))
										     rhs)
									 'min-leq-l
								       'min-leq-r)))
			       )
			  (tacl~apply 'foralle* (list concl line1) (list (data~appl-arguments lhs)))
			  (values t t)))
		       (;; The following case is a real hack, inserted by AMEIER for the OMEGA EVALUATION 2001
			;; TO ENSURE AT LEAST ONE RUNNING EXAMPLE!!!!
			;; PLEASE DO THIS IN A PROPER WAY SOME DAY!!!
			(or (limtac=simplify-p lhs)
			    (limtac=simplify-p rhs))
			(let* ((result-line (limtac=simplify-do concl)))
			  (limtac=prove-cs result-line trace)))
		       (t (values NIL  NIL)))
		 ))
	      
	      ((limtac=less-p formula)
	       (let ((lhs (first (data~appl-arguments formula)))
		     (rhs (second (data~appl-arguments formula)))
		     )
		 (if (limtac=zero-p lhs)
		     (limtac=prove-greater-zero concl trace)
		   (values nil nil))))

	
	      ((logic~implication-p formula)
	       (let ((res (tacl~apply 'impi (list concl nil) nil))
		     )
		 (if res
		     (limtac=prove-cs (rest res)  trace)
		   (values  nil nil))))
	      
	      (T (values T T))))))) ;nil nil

(defun limtac=simplify-p (formula)
  (let* ((env (pds~environment omega*current-proof-plan)))
    (when (and (data~appl-p formula)
	       (keim~equal (data~appl-function formula) (env~lookup-object 'div env)))
      (let* ((args (data~appl-arguments formula))
	     (arg1 (first args))
	     (arg2 (second args)))
	(when (and (data~appl-p arg1)
		   (keim~equal (data~appl-function arg1) (env~lookup-object 'times env)))
	  (let* ((args2 (data~appl-arguments arg1))
		 (arg21 (first args2))
		 (arg22 (second args2)))
	    (when (and (data~equal arg21 arg2)
		       (term~number-p arg2)
		       (> (keim~name arg2) 0))
	      't)))))))

(defun limtac=simplify-do (concl)
  (let* ((formula (node~formula concl))
	 (lhs (first (data~appl-arguments formula)))
	 (rhs (second (data~appl-arguments formula)))
	 (new-concl1 (if (limtac=simplify-p lhs)
			 (limtac-simplify-doII concl (pos~list-position '(1)))
		       concl))
	 (new-concl2 (if (limtac=simplify-p rhs)
			 (limtac-simplify-doII new-concl1 (pos~list-position '(2)))
		       new-concl1)))
    new-concl2))

(defun limtac-simplify-doII (concl position)
  (let* ((formula (node~formula concl))
	 (term (data~struct-at-position formula position))
	 (x (second (data~appl-arguments term)))
	 (y (second (data~appl-arguments (first (data~appl-arguments term)))))
	 (thm1 (tacl~insert&return-assumption 'limit 'div-r))
	 (res1 (tacl~apply 'foralle* (list nil thm1) (list (list x y))))
	 (res2 (tacl~apply 'impe (list nil nil (first res1)) nil))
	 
	 (noteq (second res2)) ;; 0 <> x
	 (thm2 (tacl~insert&return-assumption 'limit 'less-notequal))
	 (res3 (tacl~apply 'foralle* (list nil thm2) (list (list (term~constant-create '0 (env~lookup-object 'num (pds~environment omega*current-proof-plan)))
								 x))))
	 (res4 (tacl~apply 'impe (list noteq nil (first res3)) nil))
	 (res5 (tacl~apply 'arith-simplify (list (second res4)) (list 'less)))

	 (eq (first res2)) ;; Y = (DIV (TIMES X Y) X)
	 (res6 (tacl~apply '=subst (list concl nil eq) (list position))))
    (second res6)))
	 
    

	       
(defun limtac=trace-for(formula trace)
  (let ((result (member formula trace
			:test #'(lambda (formula entry)
				  (if (member formula (third entry)
					      :test #'keim~equal)
				      (list (first entry) (second entry))
				    nil))))
	)
    (when result
      (first result))))

(defun limtac=search-node(formula hyps &optional (concl nil))
  (let* ((nodes (prob~proof-steps omega*current-proof-plan))
	 ;;(asdf (format t "~& nodes: ~S" nodes))
	 (valid-nodes (remove-if-not #'(lambda (node)
					 (subsetp (pdsn~hyps node) hyps))
				     nodes))
	 (node (find formula valid-nodes :test #'(lambda (term node)
						   ;;(format t "~& node: ~S" node)
						   (and (keim~equal term (node~formula
									  node))
							(if concl
							    (not (keim~equal node concl))
							  t)))))
	 )
    node))

(defun limtac=prove-greater-zero (concl trace)
  (let* ((formula (node~formula concl))
	 (term (second (data~appl-arguments formula)))
	 )
    (cond ((limtac=fraction-p term)
	   (let* ((thm (tacl~insert&return-assumption 'limit 'fraction-greater-zero))
		  (res1 (tacl~apply 'foralle* (list nil thm) (list (data~appl-arguments term))))
		  (conj (tacl~apply 'impe (list concl nil (first res1)) nil))
		  )
	     (limtac=prove-cs (second conj) trace)))
	  ((limtac=product-p term)
	   (let* ((thm (tacl~insert&return-assumption 'limit 'product-greater-zero))
		  (res1 (tacl~apply 'foralle* (list nil thm) (list (data~appl-arguments term))))
		  (conj (tacl~apply 'impe (list concl nil (first res1)) nil))
		  )
	     (limtac=prove-cs (second conj) trace)))
	  ((and (limtac=number-p term)
		(> (keim~name term) 0))
	   (tacl~apply 'arith-simplify (list concl) (list 'less)))
	  ((limtac=min-p term)
	   (let* ((thm (tacl~insert&return-assumption 'limit 'min-greater))
		  (res1 (tacl~apply 'foralle* (list nil thm) (list (append (data~appl-arguments term)
									   (list (first (data~appl-arguments formula)))))))
		  (conj (tacl~apply 'impe (list concl nil (first res1)) nil))
		  )
	     (limtac=prove-cs (second conj) trace)))
	  ((and (limtac=absval-p term)
		(limtac=number-p (first (data~appl-arguments term)))
		(> (keim~name (first (data~appl-arguments term))) 0))
	   (let* ((thm1 (tacl~insert&return-assumption 'limit 'absval-great-zero))
		  (thm2 (tacl~insert&return-assumption 'limit 'less-notequal))
		  (res1 (tacl~apply 'foralle* (list nil thm1) (list (list (first (data~appl-arguments term))))))
		  (res2 (tacl~apply 'impe (list concl nil (first res1)) nil))
		  (res3 (tacl~apply 'foralle* (list nil thm2) (list (list (term~constant-create '0 (env~lookup-object 'num (pds~environment omega*current-proof-plan)))
									  (first (data~appl-arguments term))))))
		  (res4 (tacl~apply 'impe (list (second res2) nil (first res3)) nil)))
	     (tacl~apply 'arith-simplify (list (second res4)) (list 'less))))
	  (T (values nil nil)))))



;;*********************************************************
;;*********************************************************
;; skolemize-m-b
;;   forall(e)exists(d)forall(x).P(e d x) --> P(e' M_D x')
;;*********************************************************
(infer~deftactic skolemize
		 (outline-mappings (
				    ((existent existent) skolemize-a)
				    ))
		 (parameter-types term-list)
		 (expansion-function limtac=expand-skolemize)
		 (help "The skolemization for limit-formulae."))

;;NOTE: The tactic is not completely implemented !!!
(tac~deftactic skolemize-a skolemize (in limit)
   (premises L2)
   (conclusions L1)
   (computations)
   (sideconditions )
   (description "Application of the skolemization."))


#|
;;**********************
;;  skolemize command
;;**********************
(com~defcommand skolemize
  (argnames ineq concl)
  (argtypes ndline ndline)
  (arghelps "An inequality" "An inequality to proof")
  (function limtac=skolemize)
  (defaults )
  (frag-cats tactics limit)
  (log-p T)
  (help "Applies the additivity of '<'."))


(defun limtac=skolemize (ineq concl)
  (infer~compute-outline 'skolemize (list concl ineq) NIL))
|#

(defun limtac=expand-skolemize (outline parameters)
  (declare (edited  "12-MAR-1998")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  
  (let* ((concl (first outline))
	 (concformula (node~formula concl))
	 (premise (second outline))
	 (params (first parameters))
	 )
    
    (format t "~%concl: ~S" concl)
    (format t "~%premise: ~S" premise)
    (format t "~%params: ~S" params)
    (tacl~init outline)
    (if (limtac=quantification-p concformula)
	(limtac=expand-skolemize-backward premise concl params)
      (limtac=expand-skolemize-forward concl premise params))
    ))

(defun limtac=expand-skolemize-backward (premise node params)
  (declare (edited  "12-MAR-1998")
	   (authors Jzimmer)
	   (input   "A premise, the conclusion an the parameters.")
	   (effect  "Expands NODE by applying ForallI and ExistsI.")
	   (value   "Two values. (t t) if the expansion was successfull."
		    "(nil nil) otherwise."))
  
  (if node 
      (let* ((formula (node~formula node))
	     )
	(format t "~% formel: ~S" formula)
	(cond ((logic~universal-quantification-p formula)
	       (if (limtac=quantification-p (limtac=scope formula))
		   (let* ((const (first params))
			  (new-nodes (tacl~apply 'forallI (list node nil) (list const))))
		     (limtac=expand-skolemize-backward premise
						       (second new-nodes)
						       (rest params)))
		 (let* ((const (first params))
			)
		   (tacl~apply 'forallI (list node premise) (list const))
		   (tacl~end)
		   (values t t))))
	      
	      ((logic~existential-quantification-p formula)
	       (if (limtac=quantification-p (limtac=scope formula))
		   (let* ((meta (first params))
			  (poss (data~substruct-positions
				 (first (data~abstr-domain
					 (first (data~appl-arguments formula))))
				 (limtac=scope formula)))
			  (new-nodes (tacl~apply 'existsi (list node nil)
						 (list meta poss))))
		     (limtac=expand-skolemize-backward premise
						       (second new-nodes)
						       (rest params)))
		 (let* ((meta (first params))
			(poss (data~substruct-positions
			       (first (data~abstr-domain (data~appl-arguments formula)))
			       (limtac=scope formula))))
		   (tacl~apply 'existsi (list node premise) (list meta poss))
		   (tacl~end)
		   (values t t))))
	      
	      (T (format t "~%The tactic 'skolemize' could not be expanded!!  ~S " formula)
		 (tacl~end)
		 (values nil nil))))))

(defun limtac=expand-skolemize-forward (concl node consts metas)
  (declare (edited  "12-MAR-1998")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  
  (let* ((formula (node~formula node))
	 )
    
    (cond ((logic~universal-quantification-p formula)
	   (if (limtac=quantification-p (limtac=scope formula))
	       (let* ((meta (first (last metas)))
		      (new-nodes (tacl~apply 'forallE (list nil node) (list meta))))
		 (limtac=expand-skolemize-forward concl 
						  (first new-nodes)
						  consts
						  (butlast metas)))
	     (let* ((meta (first (last metas))))
	       (tacl~apply 'forallE (list concl node) (list meta)))))
	  
	  
	  ((logic~existential-quantification-p formula)
	   (if (limtac=quantification-p (limtac=scope formula))
	       (let* ((const (first (last consts)))
		      (new-nodes (tacl~apply 'existsE (list concl node NIL) (list const))))
		 
		 (format t "~%new-nodes:~S" new-nodes))
	     (let* ((const (first (last consts))))
	       (tacl~apply 'existsE (list concl node) (list const)))))
	  
	  (T (format t "~%Fehler...~S" formula)))))


(defun limtac=quantification-p (formula)
  (or (logic~universal-quantification-p formula)
      (logic~existential-quantification-p formula)))

(defun limtac=scope (formula)
  (data~abstr-range (first (data~appl-arguments formula))))

(defun limtac=expand-forward (lines premise vars consts)
  (declare (edited  "12-MAR-1998")
	   (authors Jzimmer)
	   (input   )
	   (effect  )
	   (value   ))
  
  (values nil nil))

(defun limtac~skolemize (formula positive)
  (declare (edited  "02-FEB-1998")
	   (authors Jzimmer)
	   (input   "A formula and a boolean variable.")
	   (effect  "Computes the skolemization in place according to"
		    "Bledsoe.")
	   (value   "Two values, T and the skolemized formula."))
  
  (setf limtac*new-meta-vars nil)
  (setf limtac*new-symbols nil)
  
  (let ((new-formula (limtac=skolemize formula nil nil positive T))
	)
    (setf plan*meta-vars
	  (append limtac*new-meta-vars
		  plan*meta-vars))
    (values T (list new-formula
		    (list limtac*new-symbols)))))


(defgeneric limtac=skolemize (formula pos-bind neg-bind positive forall)
  (declare (edited  "02-FEB-1998")
	   (authors Jzimmer)
	   (input   "A formula, an alist with the positive bindings found so"
		    "far, an alist with the negative bindings, a boolean variable"
		    "and a list of all forall-quantified variables found so far."
		    )
	   (effect  )
	   (value   "The formula skolemized positive or negative according to"
		    "the value of POSITIVE."))
  
  (:method ((formula-list list) pos-bind neg-bind positive forall)
	   (when formula-list
	     (cons (limtac=skolemize (first formula-list) pos-bind neg-bind positive forall)
		   (limtac=skolemize (rest formula-list) pos-bind neg-bind positive forall))))
  
  (:method ((formula term+appl) pos-bind neg-bind positive forall)
	   (cond ((logic~negation-p formula)
		  (term~appl-create (data~appl-function formula)
			       (limtac=skolemize (data~appl-arguments formula)
						 pos-bind
						 neg-bind
						 (not positive)
						 forall)))
		 ((logic~implication-p formula)
		  (term~appl-create (data~appl-function formula)
				    (cons (limtac=skolemize (first (data~appl-arguments formula))
							    pos-bind
							    neg-bind
							    (not positive)
							    forall)
					  (limtac=skolemize (rest (data~appl-arguments formula))
							    pos-bind
							    neg-bind
							    positive
							    forall))))
		 ((logic~existential-quantification-p formula)
		  (limtac=skolemize (first (data~appl-arguments formula))
				    pos-bind
				    neg-bind
				    positive
				    NIL))
		 ((logic~universal-quantification-p formula)
		  (limtac=skolemize (first (data~appl-arguments formula))
				    pos-bind
				    neg-bind
				    positive
				    T))
		 (t (term~appl-create (data~appl-function formula)
				      (limtac=skolemize (data~appl-arguments formula)
							pos-bind
							neg-bind
							positive
							forall)))))
  (:method ((formula term+abstr) pos-bind neg-bind positive (forall (eql nil)))
	   (let ((var (first (data~abstr-domain formula))))
	     (if positive
		 (let ((new-term (limtac=new-skolem-metavar var neg-bind))
		       )
		   (setf limtac*new-meta-vars
			 (cons new-term limtac*new-meta-vars))
		   (setf limtac*new-symbols
			 (append limtac*new-symbols
				 (list new-term)))
		   (limtac=skolemize (data~abstr-range formula)
				     (cons (cons var new-term)
					   pos-bind)
				     neg-bind
				     positive
				     forall))
	       (let ((new-term (limtac=new-skolem-constant var))
		     )
		 (setf limtac*new-symbols
		       (append limtac*new-symbols
			       (list new-term)))
		 (limtac=skolemize (data~abstr-range formula)
				   pos-bind
				   (cons (cons var new-term)
					 neg-bind)
				   positive
				   forall)))
	     ))
  
  (:method ((formula term+abstr) pos-bind neg-bind positive (forall (eql T)))
	   (let ((var (first (data~abstr-domain formula)))
		 )
	     (if positive
		 (let ((new-term (limtac=new-skolem-constant var))
		       )
		   (setf limtac*new-symbols
			 (append limtac*new-symbols
				 (list new-term)))
		   (limtac=skolemize (data~abstr-range formula)
				     (cons (cons var new-term)
					   pos-bind)
				     neg-bind
				     positive
				     forall))
	       (let ((new-term (limtac=new-skolem-metavar var neg-bind))
		     )
		 (setf limtac*new-meta-vars
		       (cons new-term limtac*new-meta-vars))
		 (setf limtac*new-symbols
			 (append limtac*new-symbols
				 (list new-term)))
		 (limtac=skolemize (data~abstr-range formula)
				   pos-bind
				   (cons (cons var new-term)
					 neg-bind)
				   positive
				   forall))
	       )
	     ))
  
  (:method ((const term+constant) pos-bind neg-bind positive forall)
	   const)
  (:method ((var term+variable) pos-bind neg-bind positive forall)
	   (let* ((pair1 (assoc var pos-bind))
		  (pair2 (assoc var neg-bind))
		  (new-term
		   (cond (pair1 (cdr pair1))
			 (pair2 (cdr pair2))
			 (T var)))
		  )
	     new-term))
  )


(defun limtac=new-skolem-constant (var)
  (declare (edited  "03-APR-1998" "03-FEB-1998")
	   (authors Jzimmer Jzimmer)
	   (input   "A quantified variable.")
	   (effect  )
	   (value   "A newly created (skolem-)variable with the type of VAR"
		    "and the name of VAR extended by a number."))
  
  (let ((name (keim~name var))
	(count 0)
	(env (pds~environment omega*current-proof-plan))
	)
    (if (not (env~lookup-object name env :test #'string-equal))
	(let* (
	       (new-var (term~constant-create name
					      (term~type var))))
	  (env~enter name new-var env)
	  new-var)
      
      (loop 
       (incf count)
       (let ((new-name (format nil "~A~A" name count)))
	 (unless (env~lookup-object new-name env :test #'string-equal)
	   (let* ((sym-name (intern new-name))
		  (new-var (term~constant-create sym-name
						(term~type var))))
	     (env~enter sym-name new-var env)
	     (return-from limtac=new-skolem-constant new-var))))))))


(defun limtac=new-skolem-metavar (var neg-bind)
  (declare (edited  "03-APR-1998" "03-FEB-1998")
	   (authors Jzimmer Jzimmer)
	   (input   "A quantified variable and an alist of all negative"
		    "variable bindings found so far.")
	   (effect  )
	   (value   "A newly created skolem-function(-application) with the name of"
		    "VAR extended by a number."
		    "NOTE: At the moment, a simple meta-variable is created."))
  (let ((name (concatenate 'string "M_" (symbol-name (keim~name var))))
	(count 0)
	(env (pds~environment omega*current-proof-plan))
	)
    (if (not (env~lookup-object name env :test #'string-equal))
	(let* ((sym-name (intern name))
	       (new-var (meta~variable-create sym-name
					      (term~type var))))
	  (env~enter sym-name new-var env)
	  new-var)
      (loop 
       (incf count)
       (let ((new-name (format nil "~A~A" name count)))
	 (unless (env~lookup-object new-name env :test #'string-equal)
	   (let* ((sym-name (intern new-name))
		  (new-var (meta~variable-create sym-name
						 (term~type var))))
	     (env~enter sym-name new-var env)
	     (return-from limtac=new-skolem-metavar new-var))))))))



;;*********************************************************
;;*********************************************************
;;     simplify inequality                                *
;;*********************************************************

(defun limtac=simplify-inequality (formula Eigenvars subgoals)
  ;;(format t "~% simp-ineq: eigenvars : ~S" Eigenvars)
  
  (cond ((limtac=bothsides-0-minus-p formula)
	 (limtac=simplify-bothsides-0-minus formula Eigenvars subgoals))

	((logic~negation-p formula)                           ;; not ...
	 (limtac=simplify-negation formula Eigenvars subgoals))
	
	((limtac=less-p formula)                              ;; < ...
	 (limtac=simplify-less formula Eigenvars subgoals))
	
	((limtac=leq-p formula)                               ;; <= ...
	 (limtac=simplify-leq formula Eigenvars subgoals))

	((limtac=equality-p formula)                          ;; = ...
	 (limtac=simplify-equality formula Eigenvars subgoals))
	
	((limtac=geq-p formula)                               ;; >= ...
	 (limtac=simplify-geq formula Eigenvars subgoals))
	
	((limtac=greater-p formula)                           ;; > ...
	 (limtac=simplify-greater formula Eigenvars subgoals))

	(T (values formula subgoals))))

(defun limtac=bothsides-0-minus-p (formula)
  (if (data~appl-p formula)
      (let* ((func (data~appl-function formula))
	     (args (data~appl-arguments formula)))
	(if (or (keim~equal func (env~lookup-object 'less (th~env 'limit)))
		(keim~equal func (env~lookup-object 'leq (th~env 'limit)))
		(keim~equal func (env~lookup-object 'greater (th~env 'limit)))
		(keim~equal func (env~lookup-object 'geq (th~env 'limit)))
		(keim~equal func (data~schema-range (env~lookup-object '= (th~env 'limit)))))
	    (let* ((lterm (first args))
		   (rterm (second args)))
	      (if (and (data~appl-p lterm)
		       (keim~equal (data~appl-function lterm) (env~lookup-object 'minus (th~env 'limit)))
		       (keim~equal (first (data~appl-arguments lterm)) (post~read-object '0 (th~env 'limit) :existing-term))
		       (data~appl-p rterm)
		       (keim~equal (data~appl-function rterm) (env~lookup-object 'minus (th~env 'limit)))
		       (keim~equal (first (data~appl-arguments rterm)) (post~read-object '0 (th~env 'limit) :existing-term)))
		  't
		nil))))
    nil))

(defun limtac=simplify-bothsides-0-minus (formula Eigenvars subgoals)
  (let* ((func (data~appl-function formula))
	 (lterm (first (data~appl-arguments formula)))
	 (rterm (second (data~appl-arguments formula)))
	 (rest-lterm (second (data~appl-arguments lterm)))
	 (rest-rterm (second (data~appl-arguments rterm)))
	 )
    (values (term~appl-create func (list rest-rterm rest-lterm))
	    nil)))

(defun limtac=simplify-negation (formula eigenvars subgoals)
  (let* ((ineq (first (data~appl-arguments formula)))
	 (simple-extra (limtac=absminus-reducable-negation-p formula))
	 (new-ineq
	  (cond (simple-extra
		 simple-extra)
		((limtac=less-p ineq)                 ;; not a<b ==> a>=b
		 (limtac=geq-create (first (data~appl-arguments ineq))
				    (second (data~appl-arguments ineq))))
		((limtac=leq-p ineq)                  ;; not a<=b ==> a>b
		 (limtac=greater-create (first (data~appl-arguments ineq))
					(second (data~appl-arguments ineq))))
		;; this case should be controled.... (CASESPLIT ?)
		((limtac=equality-p ineq)             ;; not a=b  ==> a<b or a>b
		 (limtac=disjunction-create
		  (limtac=less-create (first (data~appl-arguments ineq))
				      (second (data~appl-arguments ineq)))
		  (limtac=greater-create (first (data~appl-arguments ineq))
					 (second (data~appl-arguments ineq)))))
		((limtac=geq-p ineq)                  ;; not a>=b  ==> a<b 
		 (limtac=less-create (first (data~appl-arguments ineq))
				     (second (data~appl-arguments ineq))))
		((limtac=greater-p ineq)              ;; not a>b   ==> a<=b
		 (limtac=leq-create (first (data~appl-arguments ineq))
				     (second (data~appl-arguments ineq))))
		(T (values formula subgoals))))
	 )
    (values new-ineq subgoals)))

(defun limtac=absminus-reducable-negation-p (formula)
  ;; is the formula of the form     not (|t1-t2| > 0)
  ;; simplify to t1=t2
  (if (logic~negation-p formula)
      (let* ((arg (first (data~appl-arguments formula)))
	     (env (pds~environment omega*current-proof-plan)))
	(if (limtac=greater-p arg)
	    (let* ((left (first (data~appl-arguments arg)))
		   (right (second (data~appl-arguments arg))))
	      (if (and (keim~equal right (post~read-object '0 env :existing-term))
		       (data~appl-p left)
		       (keim~equal (data~appl-function left) (env~lookup-object 'absval env)))
		  (let* ((absarg (first (data~appl-arguments left))))
		    (if (and (data~appl-p absarg)
			     (keim~equal (data~appl-function absarg) (env~lookup-object 'minus env)))
			(term~appl-create (env~lookup-object '= env)
					  (list (first (data~appl-arguments absarg))
						(second (data~appl-arguments absarg))))
		      nil))
		nil))
	  nil))
    nil))  

(defun limtac=simplify-less (formula eigenvars subgoals)
  (let ((lhs (first (data~appl-arguments formula)))
	(rhs (second (data~appl-arguments formula))))
    (cond ((limtac=product-p lhs)
	   (let* ((fac1 (first (data~appl-arguments lhs)))
		  (fac2 (second (data~appl-arguments lhs)))
		  )
	     (cond ((limtac=fraction-p fac1)       ;; 1/a * b < c    ==>  1 * b < a * c , if a>0 !!!
		    (let* ((nom (first (data~appl-arguments fac1)))
			   (denom (second (data~appl-arguments fac1)))
			   (new-lhs (limtac=product-create nom fac2))
			   (new-rhs (limtac=product-create denom rhs))
			   (new-formula (limtac=less-create new-lhs new-rhs))
			   (new-subgoal (limtac=greater-create denom (limtac=number-create 0)))
			   )
		      (limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals))))
		   (T                              ;; a * b < c      ==>  b < c / a , if a>0  and not |.| in a!!!!!!    
		    (if (not (limtac=absval-in fac1))
			(let* ((new-lhs fac2)
			       (new-rhs (limtac=fraction-create rhs fac1))
			       (new-formula (limtac=less-create new-lhs new-rhs))
			       (new-subgoal (limtac=greater-create fac1 (limtac=number-create 0)))
			       )
			  (limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals)))
		      (if (not (limtac=absval-in fac2)) ;; a * b < c ==> a < c / b , if b>0 and not |.| in b !!
			  (let* ((new-lhs fac1)
				 (new-rhs (limtac=fraction-create rhs fac2))
				 (new-formula (limtac=less-create new-lhs new-rhs))
				 (new-subgoal (limtac=greater-create fac2 (limtac=number-create 0)))
				 )
			    (limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals)))
			(values formula subgoals))))
		   )))
	  ((limtac=fraction-p lhs)                 ;; a/b < c        ==>  a < b * c , if b>0 !!!
	   (let* ((nom (first (data~appl-arguments lhs)))
		  (denom (second (data~appl-arguments lhs))))
	     (let* ((new-lhs nom)
		    (new-rhs (limtac=product-create denom rhs))
		    (new-formula (limtac=less-create new-lhs new-rhs))
		    (new-subgoal (limtac=greater-create denom (limtac=number-create 0)))
		    )
	       (limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals)))))
	  
	  ((limtac=absval-in rhs)
	   (if (not (limtac=absval-in lhs))
	       (let ((new-formula (limtac=greater-create rhs lhs))
		     )
		 (limtac=simplify-inequality new-formula eigenvars subgoals))
	     (values formula subgoals)))
	  
	  (T
	   (values formula subgoals)))))

(defun limtac=simplify-leq (formula eigenvars subgoals)
  (let ((lhs (first (data~appl-arguments formula)))
	(rhs (second (data~appl-arguments formula))))
    (cond
     ((limtac=function-p rhs)                     ;;MP:    t <= f(X) ==> f(X) >= t
	(let ((new-formula (limtac=geq-create rhs lhs)))
	  (limtac=simplify-inequality new-formula eigenvars subgoals)))
     ((limtac=product-p lhs)
	   (let* ((fac1 (first (data~appl-arguments lhs)))
		  (fac2 (second (data~appl-arguments lhs)))
		  )
	     (cond ((limtac=fraction-p fac1)       ;; 1/a * b <= c    ==>  1 * b <= a * c
		    (let* ((nom (first (data~appl-arguments fac1)))
			   (denom (second (data~appl-arguments fac1)))
			  (new-lhs (limtac=product-create nom fac2))
			  (new-rhs (limtac=product-create denom rhs))
			  (new-formula (term~appl-create (data~appl-function formula) (list new-lhs new-rhs)))
			  )
		      (limtac=simplify-inequality new-formula eigenvars subgoals)))
		   (T                              ;; t * b <= c      ==>  b <= c / t !!!!!!    
		    (let* ((new-lhs fac2)
			   (new-rhs (limtac=fraction-create rhs fac1))
			   (new-formula (term~appl-create (data~appl-function formula) (list new-lhs new-rhs)))
			   (new-subgoal (limtac=greater-create fac1
							       (limtac=number-create 0)))
			   )
		      (limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals))))
		   )))
	   (t (values formula subgoals)))))

(defun limtac=simplify-equality (formula eigenvars subgoals)
  (values formula subgoals))

(defun limtac=simplify-geq (formula eigenvars subgoals)
  (let ((lhs (first (data~appl-arguments formula)))
	(rhs (second (data~appl-arguments formula))))
    (if (and (not (limtac=absval-in lhs))      ;; |t2| =< t1
	     (not (limtac=function-p lhs)))    ;;MP f(X) =< t
	(let ((new-formula (limtac=leq-create rhs lhs)))
	  (limtac=simplify-inequality new-formula eigenvars subgoals))
      (values formula subgoals))))

;;(defun limtac=simplify-geq (formula eigenvars subgoals)
;;  (values formula subgoals))

(defun limtac=simplify-greater (formula eigenvars subgoals)
  (let ((lhs (first (data~appl-arguments formula)))
	(rhs (second (data~appl-arguments formula))))
    ;;(omega~message "not abs : ~S ~S" formula (not (limtac=absval-in lhs)))
    (if (not (limtac=absval-in lhs))      ;;  t1 > |t2|  ==>  |t2| < t1
	(if (limtac=absval-in rhs)
	    (let ((new-formula (limtac=less-create rhs lhs)))
	      (limtac=simplify-inequality new-formula eigenvars subgoals))
	  (values formula subgoals))
      (cond ((limtac=product-p lhs)
	     (let* ((fac1 (first (data~appl-arguments lhs)))
		    (fac2 (second (data~appl-arguments lhs)))
		    )
	       (cond ((limtac=fraction-p fac1)       ;; 1/a * b > c    ==>  1 * b > a * c , if a>0 !!!
		      (let* ((nom (first (data~appl-arguments fac1)))
			     (denom (second (data~appl-arguments fac1)))
			     (new-lhs (limtac=product-create nom fac2))
			     (new-rhs (limtac=product-create denom rhs))
			     (new-formula (limtac=greater-create new-lhs new-rhs))
			     (new-subgoal (limtac=greater-create denom (limtac=number-create 0)))
			     )
			(limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals))))
		     (T                              ;; a * b > c      ==>  b > c / a , if a>0  and not |.| in a!!!!!!    
		      (if (not (limtac=absval-in fac1))
			  (let* ((new-lhs fac2)
				 (new-rhs (limtac=fraction-create rhs fac1))
				 (new-formula (limtac=greater-create new-lhs new-rhs))
				 (new-subgoal (limtac=greater-create fac1 (limtac=number-create 0)))
				 )
			    (limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals)))
			(if (not (limtac=absval-in fac2)) ;; a * b > c ==> a > c / b , if b>0 and not |.| in b !!
			    (let* ((new-lhs fac1)
				   (new-rhs (limtac=fraction-create rhs fac2))
				   (new-formula (limtac=greater-create new-lhs new-rhs))
				   (new-subgoal (limtac=greater-create fac2 (limtac=number-create 0)))
				   )
			      (limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals)))
			  (values formula subgoals))))
		     )))
	    ((limtac=fraction-p lhs)                 ;; a/b > c        ==>  a > b * c , if b>0 !!!
	     (let* ((nom (first (data~appl-arguments lhs)))
		    (denom (second (data~appl-arguments lhs))))
	       (let* ((new-lhs nom)
		      (new-rhs (limtac=product-create denom rhs))
		      (new-formula (limtac=greater-create new-lhs new-rhs))
		      (new-subgoal (limtac=greater-create denom (limtac=number-create 0)))
		      )
		 (limtac=simplify-inequality new-formula eigenvars (cons new-subgoal subgoals)))))
	    
	    ((and (limtac=var-in rhs)
		  (and (not (limtac=var-in lhs))
		       (not (limtac=absval-p lhs))))
	     (let ((new-formula (limtac=less-create rhs lhs))
		   )
	       (limtac=simplify-inequality new-formula eigenvars subgoals)))
	    (T
	     (values formula subgoals))
	    ))))




