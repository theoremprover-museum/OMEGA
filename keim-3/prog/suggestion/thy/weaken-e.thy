;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric nic=str-pos-subf-p (obj1 obj2 &optional bindables extract-info)
  (declare (edited  "28-12-99")
	   (authors Chris)
	   (input   "Two objects, where obj1 is a node or a formula and obj2 a"
		    "node, formula or a list nodes or formulas. (Furthermore, Optionally a list of"
		    "bindable variables and an extraction information.")
	   (effect  "None.")
	   (value   "A list containing list of:"
		    "precise extraction information (list of command names (+ additional info)"
		    "and a node"))
  (:method ((obj1 node+node) obj2 &optional bindables extract-info)
	   (nic=str-pos-subf-p (node~formula obj1) obj2 bindables extract-info))
  (:method (obj1 (obj2 list) &optional bindables extract-info)
	   (mapcan #'(lambda (x) (nic=str-pos-subf-p obj1 x bindables extract-info))
		   obj2))
  (:method (obj1 (obj2 node+node) &optional bindables extract-info)
	   (nic=str-pos-subf-p obj1 (node~formula obj2) bindables (cons (keim~name obj2) extract-info)))
  (:method ((obj1 term+term) (obj2 term+term) &optional bindables extract-info)
	   (let ((res-weaken (let ((subst (term~alpha-unify obj2 obj1)))
			       (when subst
				 ;(not (set-difference (subst~domain subst) bindables
				 ;      :test #'data~equal)))
				 (list (cons (list :WEAKEN subst) extract-info)))))
		 (res-conj (when (logic~conjunction-p obj2) ; A <= (A and B); A <= (B and
							    ; A)
			     (append
			      (nic=str-pos-subf-p obj1
						  (first (data~appl-arguments obj2))
						  bindables
						  (cons :NIC-AND-E-l extract-info))
			      (nic=str-pos-subf-p obj1
						  (second (data~appl-arguments obj2))
						  bindables
						  (cons :NIC-AND-E-r extract-info)))))
		 (res-disj (when (logic~disjunction-p obj2) ; A <= (A or B); A <= (B or A)
			     ;; extend this part by introducing all possible formulas C
			     (append 
			      (nic=str-pos-subf-p obj1
						  (first (data~appl-arguments obj2))
						  bindables
						  (cons :NIC-OR-E-l extract-info))
			      (nic=str-pos-subf-p obj1
						  (second (data~appl-arguments obj2))
						  bindables
						  (cons :NIC-OR-E-r extract-info)))))
		 (res-imp (when (logic~implication-p obj2) ; A <= (B implies A)
			    ;; extend this by introducing all possible formulas A
			    (append 
			     (nic=str-pos-subf-p obj1
						 (second (data~appl-arguments obj2))
						 bindables
						 (cons :NIC-IMP-E extract-info)))))
		 (res-all (when (logic~universal-quantification-p obj2) ; A(t) <= ALL x . A(x) for every
					                                ; term t
			    (let ((bound-var (logic~quantification-bound-variable obj2)))
			      (nic=str-pos-subf-p obj1
						  (logic~quantification-scope obj2)
						  (cons bound-var bindables)
						  (cons (list :NIC-FORALL-E bound-var)
							(cons (LIST :DECLARE `(variables
									       (,bound-var
										,(data~annotation bound-var)
										)))
							      extract-info))))))
		 (res-ex (when (logic~existential-quantification-p obj2) ; A(t) <= EX x . A(x) for
				                                         ;  every term t

			   ;; How to handle skolemization
			   (multiple-value-bind (new-obj2 skolemterms)
			       (hocnf~skolemize (logic~quantification-scope obj2)
						(list (logic~quantification-bound-variable obj2))
						(foci~free-vars (foci~active-pc))
						(pds~environment (agplan~current-proof-plan)))
			     (nic=str-pos-subf-p obj1 new-obj2 bindables
						 (cons (list :NIC-EXISTS-E-e (car skolemterms)) extract-info))))))
	     (append res-weaken res-conj res-disj res-imp res-all res-ex))))


(defun batac=formula-list-subsetp (list1 list2)
  (declare (edited  "02-FEB-1999 16:39")
	   (authors SORGE)
	   (input   "Two lists of formulas.")
	   (value   "T if both lists contain the same formulas, if list1 is"
		    "a proper subset of list2 the elements of list2 not occuring in"
		    "list1, and NIL if an element of list1 does not occur in list2"
		    "(all in a data~~equal sense)."))
  (cond ((and (null list1) (null list2)) t)
	((and (null list1) list2) list2)
	((and list1 (null list2)) nil)
	(t
	 (let* ((formula1 (car list1))
		(formula2 (find formula1 list2 :test #'data~equal)))
	   (when formula2
	     (batac=formula-list-subsetp (cdr list1) (remove formula2 list2 :count 1)))))))



(defgeneric agent=mismatch-positions (term1 term2 &optional (pos (pos~empty)))
      (:method ((term1 term+primitive) (term2 term+term) &optional (pos (pos~empty)))
	       (when (not (term~alpha-equal term1 term2))
		 (list pos)))
      (:method ((term1 term+term) (term2 term+primitive) &optional (pos (pos~empty)))
	       (when (not (term~alpha-equal term1 term2))
		 (list pos)))
      (:method ((term1 term+appl) (term2 term+appl) &optional (pos (pos~empty)))
	       (when (not (term~alpha-equal term1 term2))
		 (if (term~alpha-equal (data~appl-function term1)
				       (data~appl-function term2))
		     (let* ((num 0)
			    (result 
			     (mapcan #'(lambda (x y)
					 (agent=mismatch-positions x y (pos~add-end (incf num) pos)))
				     (data~appl-arguments term1)
				     (data~appl-arguments term2))))
		       (if (= (length result) 1) result (list pos))) 
		   (list pos))))
      (:method ((term1 term+abstr) (term2 term+abstr) &optional (pos (pos~empty)))
	       (when (not (term~alpha-equal term1 term2))
		 (agent=mismatch-positions (data~abstr-range term1)
					   (data~abstr-range term2)
					   (pos~add-end 0 pos)))))

(defgeneric nic=repeated-line-p (obj)
  (declare (edited  "29-12-99")
           (authors Chris)
           (input   "A proof line or a formula.")
           (effect  "None.")
           (value   "T, iff line is a repeated proof line or formula in proof search."))
  (:method ((obj node+node))
	   (some #'(lambda (x)
		      (and (data~equal (node~formula x) (node~formula obj))
			   ; (subsetp (pdsn~hyps obj) (pdsn~hyps x))
			   (or (not (nic~elimination-p x)) (nic~elimination-p obj))))
		  (let ((descendants (foci~descendants (foci~active-pc))))
		    (when descendants (rest descendants))))) ;;; the first descendant may
							    ;;; be identical to the
							    ;;; current goal, see or-e
  (:method ((obj term+term))
	   (some #'(lambda (x) (data~equal (node~formula x) obj))
		 (let ((descendants (foci~descendants (foci~active-pc))))
		   (when descendants (rest descendants))))))

(defgeneric nic=str-pos-subforms (obj1)
  (declare (edited  "28-12-99")
	   (authors Chris)
	   (input   "An objects, which is a list, node or a formula")
	   (effect  "None.")
	   (value   "A list containing of strictly positive subformulas of obj1"))
  (:method ((obj1 list))
	   (remove-duplicates (mapcan #'nic=str-pos-subforms obj1) :test #'keim~equal))
  (:method ((obj1 node+node))
	   (nic=str-pos-subforms (node~formula obj1)))
  (:method ((obj1 term+term))
	   (cond ((logic~atom-p obj1) (list obj1))
		 ((logic~negation-p obj1) (list obj1))
		 ((logic~conjunction-p obj1) ; A <= (A and B); A <= (B and
					; A)
		  (cons obj1
			(append
			 (nic=str-pos-subforms (first (data~appl-arguments obj1)))
			 (nic=str-pos-subforms (second (data~appl-arguments obj1))))))
		 ((logic~disjunction-p obj1) ; A <= (A or B); A <= (B or A)
		  ;; extend this part by introducing all possible formulas C
		  (cons obj1
			(append 
			 (nic=str-pos-subforms (first (data~appl-arguments obj1)))
			 (nic=str-pos-subforms (second (data~appl-arguments obj1))))))
		 ((logic~implication-p obj1) ; A <= (B implies A)
		  ;; extend this by introducing all possible formulas A
		  (cons obj1
			(nic=str-pos-subforms (second (data~appl-arguments obj1)))))
		 ((logic~universal-quantification-p obj1) ; A(t) <= ALL x . A(x) for every
					; term t
		  (cons obj1
			(nic=str-pos-subforms (logic~quantification-scope obj1))))
		 ((logic~existential-quantification-p obj1) ; A(t) <= EX x . A(x) for
					;  every term t
		  (cons obj1
			(nic=str-pos-subforms (hocnf~skolemize (logic~quantification-scope obj1)
							       (list (logic~quantification-bound-variable obj1))
							       (foci~free-vars (foci~active-pc))
							       (pds~environment
								(agplan~current-proof-plan)))))))))

(defun similar-p (f1 f2)
	   (and (data~appl-p f1)
		(data~appl-p f2)
		(data~equal (data~appl-function f1)
			    (data~appl-function f2))
		(equal (length (data~appl-arguments f1))
		       (length (data~appl-arguments f2)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; weaken-thy*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~defwild-tactic weaken-thy*
		 (outline-mappings (((existent list) weaken-thy*-a)
				    ((existent nonexistent) weaken-thy*-b)
				    ))
		 (passkey :node)
		 (parameter-types position-list term term-list)
		 (expansion-function batac=expand-weaken-e)
		 (help "Replacement property of equaelity."))

(defun weaken-thy*-b (concs prems parameters)
  (let* ((term (cadr parameters))
	(equations (caddr parameters))
	(positions (car parameters))
	(label-counter 0)
	(dummy-hyps (mapcar #'(lambda (x)
				(prog1
				  (pdsn~make-hypothesis x (format nil "mumpitz~A"
								label-counter))
				  (incf label-counter))) equations)))
    (when (and term
	       (batac=weaken-e-check-positions-p (node~formula (car concs)) positions)
	       (nic=str-pos-subf-p term (pds~node-supports (car concs)))
	       (not (data~free-variables term))
	       (not (some #'data~free-variables equations))
	       (batac=weaken-e-check-equal-formulas-p (node~formula (car concs)) term positions)
	       (batac=weaken-e-check-equalities-by-WM-2 (node~formula (car concs)) term dummy-hyps))
      (values nil equations))))

(defun weaken-thy*-a (concs prems parameters)
  (let* ((term (cadr parameters))
	 (premise-eqs (cdr prems))
	 (param-equations (caddr parameters))
	 (positions (car parameters))
	 (label-counter 0)
	 (dummy-hyps (mapcar #'(lambda (x)
				(progn
				  (pdsn~make-hypothesis x (format nil "mumpitz~A"
								  label-counter))
				  (incf label-counter))) param-equations))
	 (premise-term (car prems)))
    (when (and (data~equal term (node~formula premise-term))
	       (similar-p (node~formula premise-term) (node~formula (car concs)))
	       (not (data~free-variables term))
	       (not (some #'data~free-variables param-equations))
	       (not (some #'(lambda (x)
			      (data~free-variables (node~formula x))) premise-eqs))
	       (batac=weaken-e-check-equal-formulas-p (node~formula (car concs)) term positions)
	       (batac=weaken-e-check-equalities-by-WM-2 (node~formula (car concs)) term
							(append dummy-hyps premise-eqs)))
      (if param-equations
	(values nil param-equations) T))))
	       
      
	       
(com~defcommand weaken-thy*
		(argnames line1 line2 line3 positions term equations)
		(argtypes ndline ndline ndline-list position-list term term-list)
		(arghelps "The Conclusion" "The premisse" "A list of equations"
			  "A list of positions"
			  "A strictly positive subformula of the hypotheses." "A list of equations")
		(function batac==weaken-thy*)
		(frag-cats tactics base)
		(defaults nil nil nil nil nil nil)
		(level 1)
		(log-p T)
		(help "Equality-Substitution in several sub-terms."))


;; ---------------------------------
;; some functions used by the tactic
;; ---------------------------------

(defun batac==weaken-thy* (F1 F2 EQS positions term equations)
  (infer~compute-outline 'weaken-thy* (list F1 (remove-if #'null (cons F2 EQS))) (list positions term equations)))
	       

(defun batac=weaken-e-check-positions-p (formula positions)
  (let* ((all-positions (data~positions formula #'(lambda (x) T))))
    (subsetp positions all-positions :test #'keim~equal)))

(defun batac=weaken-e-check-equal-formulas-p (formula1 formula2 positions)
  (let* ((terms-at-pos-1 (mapcar #'(lambda (pos) (data~struct-at-position formula1 pos))
				 positions))
	 (terms-at-pos-2 (mapcar #'(lambda (pos) (data~struct-at-position formula2 pos))
				 positions)))
    (if (every #'(lambda (pair)
		   (keim~equal (term~type (first pair)) (term~type (second pair))))
	       (mapcar #'list terms-at-pos-1 terms-at-pos-2))
	(let* ((formula1*
		(do* ((rest-positions positions (rest rest-positions))
		      (rest-terms-2 terms-at-pos-2 (rest rest-terms-2))
		      (current-formula formula1))
		    ((null rest-positions)
		     current-formula)
		  (let* ((head-pos (first rest-positions))
			 (head-term-2 (first rest-terms-2)))
		    (setf current-formula (data~replace-at-position current-formula
								    head-pos head-term-2))))))
	  (if (term~alpha-equal formula1* formula2) T NIL))
      NIL)))


(defun batac=weaken-e-check-equalities-by-WM-2 (formula1 formula2 equations)
  (let* ((env (pds~environment omega*current-proof-plan))
	 (=obj (env~lookup-object '= env))
	 (formula (term~appl-create =obj (list formula1 formula2)))
	 (just (pdsj~open-just-create))
	 (conc (node~create 'dummy formula just)))
    (batac=call-otter conc equations)))


(defun batac=weaken-e-check-equalities-by-WM (formula1 formula2 positions equations)
  (let* ((terms-at-pos-1 (mapcar #'(lambda (pos) (data~struct-at-position formula1 pos))
				 positions))
	 (terms-at-pos-2 (mapcar #'(lambda (pos) (data~struct-at-position formula2 pos))
				 positions))
	 (env (pds~environment omega*current-proof-plan))
	 (=obj (env~lookup-object '= env))
	 (conclusions (mapcar #'(lambda (term1 term2)
				  (let ((formula (term~appl-create =obj (list term1
									      term2)))
					(just (pdsj~open-just-create)))
				    (node~create 'dummy formula just)))
			      terms-at-pos-1 terms-at-pos-2)))
    (every #'(lambda (conc)
	       (batac=call-wm conc equations)
	       ;;(batac=call-otter conc equations)
	       )
	   conclusions)))


(defun weaken-e-compute-l2 (term) term)


(defun batac=call-wm (conc equations)
  (let* ((wald-problem-dir "~/") 
	 (in-file (merge-pathnames "waldmeister.in" wald-problem-dir))
	 (out-file (merge-pathnames "waldmeister.out" wald-problem-dir))
	 (wald-problem (wald~generate-wald-problem conc
						   equations
						   omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof wald-problem)))
    
    ;; erzeuge waldmeister.in file in wald*in-string, fuege wald*in-string zum otter-problem hinzu (einschlieslich proof-object)
    (wald~add-in-string! wald-problem
			 (res~proof-clauses res-proof)
			 "")
    
    ;; call-wald vor Ort -> schreibt wald.out file in den out-string des otter-problems
    (wald=call-wald! wald-problem wald-problem-dir 10)
    
    ;; parsen des wald-beweises
    (wald~complete-wald-problem! wald-problem :parse-back nil)))

(defun batac=call-otter (conc equations)
  (let* ((otter-problem-dir "~/") 
	 (in-file (merge-pathnames "otter.in" otter-problem-dir))
	 (out-file (merge-pathnames "otter.out" otter-problem-dir))
	 (otter-problem (otter~generate-otter-problem conc
						      equations
						      omega*current-proof-plan))
	 (res-proof (atpprb~problem-part-res-proof otter-problem)))
    
    ;; erzeuge otter.in file in otter*in-string, fuege otter*in-string zum otter-problem hinzu (einschlieslich proof-object)
    (otter~add-in-string! otter-problem
			  'auto
			  (append (list (format nil "%%%% MODE FLAGS %%%%~A" #\Newline)
					"set(auto)."
					"clear(print_proofs).")
				  (list "set(build_proof_object).")
				  (list "clear(print_given).")
				  otter*needed-flags)
			  (res~proof-clauses res-proof)
			  nil
			  't
			  :docu nil)
    
    ;; call-otter vor Ort -> schreibt wald.out file in den out-string des otter-problems
    (otter=call-otter! otter-problem otter-problem-dir 10)
    
    ;; parsen des otter-beweises
    (otter~complete-otter-problem! otter-problem :parse-back nil)))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The weaken-thy* agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(agent~defmatrix weaken-thy*
		 (agents (c-predicate (for line1)
				      (uses )
				      (definition (data~appl-p line1)))
			 (function (for equations)
				   (uses line1)
				   (exclude line2 line3)
				   (definition (fun1 (:node line1))))
			 (function (for term)
				   (uses line1 equations)
				   (multiple term)
				   (exclude line2 line3)
				   (definition (fun2 (:node line1) equations)))
			 (function (for positions)
				   (uses line1 term equations)
				   (exclude line2 line3)
				   (definition (delete (pos~empty)
						       (agent=mismatch-positions line1
										 term
						       :test #'keim~equal))))
			 ;; hier gibt es noch einen Fehler.
			 ;; Wenn keine passenden Knoten vorhanden wird LINE3 mit NIL
			 ;; instanziert. Man muesste eigentlich einen predicate agenten
			 ;; nehmen der fehlschlaegt, aber wie soll der Liste von Knoten
			 ;; zurueckgeben ?
			 ;; Kann man an dieser Stelle eigentlich einen Funktionsagenten
			 ;; verwenden ?
			 ;;
			 (function (for line3)
				   (uses line1)
				   (exclude equations)
				   (definition (fun4 (:node line1))))
			 (function (for equations)
				   (uses line1 line3)
				   (exclude equations)
				   (definition (fun3 (:node line1) (:param line3))))
			 (s-predicate (for line2 term positions)
				      (uses line1 equations line3)
				      (definition (pred0 line2 line1 (:param line3)
							 (:param equations)))))
		 (predicates
		  (fun1 (node)
			(remove-if #'(lambda (x)
				       (or 
				       (data~free-variables x)
				       (not (logic~equality-p x))))
				   (nic=str-pos-subforms (pds~node-supports node))))
		  (fun2 (node equations)
			(let ((formula (node~formula node)))
			  (remove-if #'null
				     (mapcar #'(lambda (subf)
						 (when (and (not (data~free-variables subf))
								 (similar-p formula subf))
						   ;; pos berechnen
						   (let ((dif-positions
							  (delete (pos~empty)
								  (agent=mismatch-positions
								   formula subf)
								  :test #'keim~equal)))
						     (when (and
							    (batac=weaken-e-check-equal-formulas-p formula subf dif-positions)    
							    (batac=weaken-e-check-equalities-by-WM
							     formula subf dif-positions 
							     (list equations)))))))
						 (nic=str-pos-subforms (pds~node-supports
									node))))))
		  (fun3 (conc-node line3)
			(let* ((possible-equations (remove-if-not #'(lambda (f)
								      (and
								       (not (data~free-variables f))
								       (logic~equality-p f)))
								  (nic=str-pos-subforms
								   (pds~node-supports
								    conc-node))))
			       (equations (batac=formula-list-subsetp
					   (mapcar #'(lambda (x) (node~formula x)) line3)
					   possible-equations)))
			  (if (equal equations 'T) NIL equations)))
		  (fun4 (node)
			(remove-if-not #'(lambda (x) (logic~equality-p (node~formula x)))
						     (pds~node-supports node)))
		  (pred0 (l2 conc-line l3 eqs)
			 (progn
			   (print 'huhu)
			   (print l2)
			   (print conc-line)
			   (print l3)
			   (print eqs)
			   (print 'endegelaende)
			   (let* ((eqs-nodes (mapcar #'(lambda (x)
							(progn
							  (pdsn~make-hypothesis x (format nil "mumpitz~A"
											  label-counter))
							  (incf label-counter))) eqs))
				  (all-eqs (append l3 eqs-nodes)))
			     (when (and (similar-p l2 conc-line)
					(not (data~free-variables l2)))
			       (let ((dif-positions
				      (delete (pos~empty)
					      (agent=mismatch-positions
					       l2 conc-line)
					      :test #'keim~equal)))
				 (when (and (batac=weaken-e-check-equal-formulas-p l2 conc-line
										   dif-positions)
					    (or (print 'huhu) T)
					    (batac=weaken-e-check-equalities-by-WM
					     l2 conc-line dif-positions 
					     all-eqs))
				   (list l2 dif-positions)))))))))
				 
			     
				    
				    
				    
		  
		
		 
		  
			 
				      
