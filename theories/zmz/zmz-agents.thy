(in-package :omega)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORALLI-SORT-RESCLASS AGENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand ForallI-Sort-Resclass-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line with a universal quantification of sort residue class.")
		(function ilo~com-ForallI-Sort-Resclass-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method ForallI-Sort-Resclass-m-b"))
		

(defun ilo~com-ForallI-Sort-Resclass-m-b (goal)
  (pplan~execute-method 'ForallI-Sort-Resclass-m-b goal nil nil))

(agent~defmatrix ForallI-Sort-Resclass-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 goal))))
		 (predicates
		  (pred1 (g)
			 (and (data~appl-p g)
			      (keim~equal (data~appl-function g)
					  (data~schema-range (env~lookup-object 'forall-sort
										(pds~environment omega*current-proof-plan))))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ExistsI-SORT-RESCLASS AGENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand ExistsI-Sort-Resclass-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line with an existential quantification of sort residue class.")
		(function ilo~com-ExistsI-Sort-Resclass-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method ExistsI-Sort-Resclass-m-b"))
		

(defun ilo~com-ExistsI-Sort-Resclass-m-b (goal)
  (pplan~execute-method 'ExistsI-Sort-Resclass-m-b goal nil nil))

(agent~defmatrix ExistsI-Sort-Resclass-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 goal))))
		 (predicates
		  (pred1 (g)
			 (and (data~appl-p g)
			      (keim~equal (data~appl-function g)
					  (data~schema-range (env~lookup-object 'exists-sort
										(pds~environment omega*current-proof-plan))))
			      (= (length (data~appl-arguments g)) 2)
			      (multiple-value-bind (boole forget1 forget2)
				  (zmztac=resclass-set-p (second (data~appl-arguments g)))
				boole)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert-Resclass-To-Num-m-b Agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand Convert-Resclass-To-Num-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line containing a expression on residue classes that can be converted into a corresponing expression on natural numbers.")
		(function ilo~com-Convert-Resclass-To-Num-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method Convert-Resclass-To-Num-m-b"))

(defun ilo~com-Convert-Resclass-To-Num-m-b (goal)
  (pplan~execute-method 'Convert-Resclass-To-Num-m-b goal nil nil))

(agent~defmatrix Convert-Resclass-To-Num-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 goal))))
		 (predicates
		  (pred1 (g)
			 (and (null (data~positions g #'logic~existential-quantification-p))
			      (null (data~positions g #'logic~universal-quantification-p))
			      (or
			       ;; checking 1.)
			       (data~positions g #'zmztac=residue-class-equation-p)
			       ;; checking 2.)
			       (data~positions g #'zmztac=resclass-in-resclass-set-p)
			       ;; checking 3.)
			       (zmztac=pushable-class-residue-positions-p g)
			       ;; checking 4.)	
			       (data~positions g #'zmztac=replaceable-class-residue-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Or-E**-m-b Agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand Or-E**-m-b 
		(argnames goal or-supps)
		(argtypes ndline ndline-list)
		(arghelps "An open line with some disjunctions as supports."
			  "The disjunction supports.") 
		(function ilo~com-Or-E**-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method Or-E**-m-b"))

(defun ilo~com-Or-E**-m-b (goal or-supps)
  (pplan~execute-method 'Or-E**-m-b goal nil (list or-supps)))

(agent~defmatrix Or-E**-m-b 
		 (agents
		  (c-predicate (for goal or-supps)
			       (uses )
			       (level 1)
			       (definition (pred1 (:node goal))))
		  (function (for or-supps)
			    (uses goal)
			    (level 1)
			    (definition (pred0 (:node goal)))))
		 (predicates
		  (pred0 (g) (remove-if-not #'(lambda (supp)
						(logic~disjunction-p (node~formula supp)))
					    (pds~node-supports g)))
		  (pred1 (g)
			 (let* ((supps (pred0 g)))
			   (when supps (list supps))))))
			 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplify-Numerical-Expr-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand Simplify-Numerical-Expr-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line with some numerically simplifiable expressions.")
		(function ilo~com-Simplify-Numerical-Expr-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method Simplify-Numerical-Expr-m-b"))

(defun ilo~com-Simplify-Numerical-Expr-m-b (goal)
  (pplan~execute-method 'Simplify-Numerical-Expr-m-b goal nil nil))

(agent~defmatrix Simplify-Numerical-Expr-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 (:node goal)))))
		 (predicates
		  (pred1 (g)
			 (let* ((supps (pds~node-supports g))
				(eq-supps (remove-if-not #'(lambda (supp)
							     (let* ((form (node~formula supp)))
							       (and (data~appl-p form)
								    (keim~equal (data~appl-function form)
										(data~schema-range
										 (env~lookup-object '= (pds~environment omega*current-proof-plan))))
								    (= (length (data~appl-arguments form)) 2))))
							 supps)))
			   (prog1 (meth=numerical-simplify (node~formula g) eq-supps (pos~empty)))
			   ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  OrIL-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand OrIL-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line with a disjunction.")
		(function ilo~com-OrIL-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method OrIL-m-b."))

(defun ilo~com-OrIL-m-b (goal)
  (pplan~execute-method 'OrIL-m-b goal nil nil))

(agent~defmatrix OrIL-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 goal))))
		 (predicates
		  (pred1 (g)
			 (logic~disjunction-p goal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Orir-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand Orir-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line with a disjunction.")
		(function ilo~com-Orir-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method Orir-m-b."))

(defun ilo~com-Orir-m-b (goal)
  (pplan~execute-method 'Orir-m-b goal nil nil))

(agent~defmatrix OrIR-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 goal))))
		 (predicates
		  (pred1 (g)
			 (logic~disjunction-p goal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Reflex-m-b -> there is another one in this file? MP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(com~defcommand Reflex-m-b
;                (argnames goal)
;                (argtypes ndline)
;                (arghelps "An open line with an equation.")
;                (function ilo~com-reflex-m-b)
;                (defaults )
;                (frag-cats tactics zmz)
;                (log-p t)
;                (help "Applies the method Reflex-m-b."))
;
;(defun ilo~com-reflex-m-b (goal)
;  (pplan~execute-method 'Reflex-m-b goal nil nil))
;
;(agent~defmatrix Reflex-m-b
;                 (agents
;                  (c-predicate (for goal)
;                               (uses )
;                               (level 1)
;                               (definition (pred1 goal))))
;                 (predicates
;                  (pred1 (g)
;                         (and (data~appl-p g)
;                              (keim~equal (data~appl-function g)
;                                          (data~schema-range
;                                           (env~lookup-object '= (pds~environment omega*current-proof-plan))))
;                              (= (length (data~appl-arguments g)) 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  solve-modulo-equation-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand solve-modulo-equation-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line with an equation.")
		(function ilo~com-solve-modulo-equation-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method solve-modulo-equation-m-b."))

(defun ilo~com-solve-modulo-equation-m-b (goal)
  (pplan~execute-method 'solve-modulo-equation-m-b goal nil nil))

(defun ilo~com-solve-modulo-equation-m-b-pred1 (g)
  (and (data~appl-p g)
       (keim~equal (data~appl-function g)
		   (data~schema-range
		    (env~lookup-object '= (pds~environment omega*current-proof-plan))))
       (let* ((args (data~appl-arguments g))
	      (lhs (first args))
	      (rhs (second args))
	      (mod (env~lookup-object 'mod (pds~environment omega*current-proof-plan))))
	 (and (data~appl-p lhs)
	      (data~appl-p rhs)
	      (data~equal (data~appl-function lhs) mod)
	      (data~equal (data~appl-function rhs) mod)))))

(agent~defmatrix solve-modulo-equation-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (ilo~com-solve-modulo-equation-m-b-pred1 goal)))))
		 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   popmod-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand popmod-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line with terms containing modulo operation.")
		(function ilo~com-popmod-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method popmod-m-b."))

(defun ilo~com-popmod-m-b (goal)
  (pplan~execute-method 'popmod-m-b goal nil nil))

(agent~defmatrix popmod-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (pred1 goal))))
		 (predicates
		  (pred1 (g)
			 (data~substruct-positions
			  (env~lookup-object 'mod (pds~environment omega*current-proof-plan))
			  g))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   weaken-m-a  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(com~defcommand weaken-m-a
;                (argnames goal assump)
;                (argtypes ndline ndline)
;                (arghelps "An open line." "A line with the term.")
;                (function ilo~com-weaken-m-a)
;                (defaults )
;                (frag-cats tactics zmz)
;                (log-p t)
;                (help "Applies the method weaken-m-a."))
;
;(defun ilo~com-weaken-m-a (goal assump)
;  (pplan~execute-method 'weaken-m-a goal (list assump) nil))
;
;(agent~defmatrix weaken-m-a
;   (agents (c-predicate (for goal)
;                        (uses assump)
;                        (level 1)
;                        (definition (pred1 assump goal)))
;           (c-predicate (for goal)
;                        (exclude assump)
;                        (level 1)
;                        (definition (pred2 goal)))
;           (s-predicate (for assump)
;                        (uses goal)
;                        (level 1)
;                        (definition  (pred1 assump goal)))
;           (s-predicate (for assump)
;                        (exclude goal)
;                        (level 1)
;                        (definition (pred3 assump))))
;   (predicates
;    (pred1 (a1 a2)
;           (term~alpha-equal a1 a2))
;    (pred2 (line1)
;           (find-if #'(lambda (x)
;                         (pred1 line1 (node~formula x)))
;                    (pds~node-supports (:node line1))))
;    (pred3 (line2)
;           t)))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   opclosedunder-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand opclosedunder-m-b
               (argnames goal)
               (argtypes ndline)
               (arghelps "An open line containing a disjunction of modulo expressions.")
               (function ilo~com-opclosedunder-m-b)
               (defaults )
               (frag-cats tactics zmz)
               (log-p t)
               (help "Applies the method opclosedunder-m-b."))

(defun ilo~com-opclosedunder-m-b (goal)
 (pplan~execute-method 'opclosedunder-m-b goal nil nil))

(agent~defmatrix opclosedunder-m-b
  (agents (c-predicate (for goal)
                       (level 1)
                       (definition (pred1 goal))))
  (predicates
   (pred1 (a1)
          (logic~disjunction-p a1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   reflex-m-b -> now in group-agents MP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(com~defcommand reflex-m-b
;  (argnames goal)
;  (argtypes ndline)
;  (arghelps "An open line with an equation.")
;  (function ilo=com-reflex-m-b)
;  (defaults )
;  (frag-cats tactics zmz)
;  (log-p T)
;  (help "Applies the method reflex-m-b"))
;
;(defun ilo=com-reflex-m-b  (goal)
;  (pplan~execute-method 'reflex-m-b goal nil nil))
;
;(agent~defmatrix reflex-m-b
;   (agents (c-predicate (for goal)
;                        (uses )
;                        (level 1)
;                        (definition (pred1 goal))))
;   (predicates
;    (pred1 (a1)
;           (and
;            (logic~equality-p a1)
;            (term~alpha-unify (car (data~appl-arguments a1))
;                              (cadr (data~appl-arguments a1)))))))
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   solve-equation-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand solve-equation-m-b
		(argnames goal)
		(argtypes ndline)
		(arghelps "An open line with an equation.")
		(function ilo~com-solve-equation-m-b)
		(defaults )
		(frag-cats tactics zmz)
		(log-p t)
		(help "Applies the method solve-equation-m-b."))

(defun ilo~com-solve-equation-m-b (goal)
  (pplan~execute-method 'solve-equation-m-b goal nil nil))

(defun ilo~com-solve-equation-m-b-pred1 (g)
  (and (data~appl-p g)
       (keim~equal (data~appl-function g)
		   (data~schema-range
		    (env~lookup-object '= (pds~environment omega*current-proof-plan))))
       (let ((mod (env~lookup-object 'mod (pds~environment omega*current-proof-plan))))
	 (null (data~substruct-positions mod g)))))

(agent~defmatrix solve-equation-m-b
		 (agents
		  (c-predicate (for goal)
			       (uses )
			       (level 1)
			       (definition (ilo~com-solve-equation-m-b-pred1 goal)))))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   notreflex-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand notreflex-m-b
  (argnames goal)
  (argtypes ndline)
  (arghelps "An open line with an inequality.")
  (function ilo=com-notreflex-m-b)
  (defaults )
  (frag-cats tactics zmz)
  (log-p T)
  (help "Applies the method notreflex-m-b"))

(defun ilo=com-notreflex-m-b  (goal)
  (pplan~execute-method 'notreflex-m-b goal nil nil))

(agent~defmatrix notreflex-m-b
   (agents (c-predicate (for goal)
                        (uses )
                        (level 1)
                        (definition (pred1 goal))))
   (predicates
    (pred1 (a1)
	   (and
	    (logic~negation-p a1)
            (logic~equality-p (car (data~appl-arguments a1)))))))


