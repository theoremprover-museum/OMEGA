;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Command & Agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ilo=homomorphism? (form)
  (when (and (data~appl-p form)
	     (data~schema-equal (data~appl-function form)
				(env~lookup-object :homomorphism
						   (pds~environment omega*current-proof-plan))))
    (data~appl-arguments form)))

(defun ilo=group? (form)
  (when (and (data~appl-p form)
	     (data~schema-equal (data~appl-function form)
				(env~lookup-object :group
						   (pds~environment omega*current-proof-plan))))
    (data~appl-arguments form)))

(defun ilo=assoc ()
   (data~abstr-n-range  (data~schema-range
			 (post~read-object
			  '(all-types aaa (lam (x aaa)(y aaa)(z aaa)(op (aaa aaa aaa))
					       (op (op x y) z)))
			  (pds~environment omega*current-proof-plan) :existing-term))))


(defun ilo=equalities-equal? (equ1 equ2)
  (when (and (logic~equality-p equ1)
	     (logic~equality-p equ2))
    (or (term~alpha-equal equ1 equ2)
	(let ((args1 (data~appl-arguments equ1))
	      (args2 (data~appl-arguments equ2)))
	  (and (term~alpha-unify (first args1) (second args2))
	       (term~alpha-unify (second args1)(first args2)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand homomorphism-m-b
  (argnames goal homo)
  (argtypes ndline ndline)
  (arghelps "An open line containing an equation with the operation of the codomain as substerm"
	    "A line with the homomorphism")
  (function ilo=com-homomorphism-m-b )
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method homomorphism-m-b"))
  
(defun ilo=com-homomorphism-m-b  (goal homo)
  (pplan~execute-method 'homomorphism-m-b goal (list homo) nil))

(agent~defmatrix homomorphism-m-b 
   (agents (c-predicate (for goal)
                        (uses homo)
			(level 1)
                        (definition (pred1 goal homo)))
           (s-predicate (for homo)
                        (uses)
			(level 1)
                        (definition (ilo=homomorphism? homo))))
   (predicates
    (pred1 (goal homo)
	   (when (logic~equality-p goal)
	     (let ((op-ran (fourth (ilo=homomorphism? homo))))
	       (when op-ran (data~substruct-positions op-ran goal)))))))
	     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(com~defcommand defni-m
;  (argnames goal)
;  (argtypes ndline)
;  (arghelps "An open line")
;  (function ilo=com-defni-m )
;  (defaults )
;  (frag-cats tactics group)
;  (log-p T)
;  (help "Applies the method defni-m"))
;  
;(defun ilo=com-defni-m  (goal)
;  (pplan~execute-method 'defni-m goal nil nil))
;
;(agent~defmatrix defni-m 
;   (agents (c-predicate (for goal)
;                        (uses)
;                        (level 1)
;                        (definition (pred1 goal))))
;   (predicates
;    (pred1 (a1)
;           (when (data~appl-p a1)
;             (let ((defi (th~find-assumption (keim~name (data~appl-function a1)) (prob~theory omega*current-proof-plan))))
;               (and defi
;                    (not (eq (th~ass-theory defi) (th~find-theory 'base)))
;                    (not (eq (th~ass-theory defi) (th~find-theory 'post)))))))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand foralli-sort-m-b
  (argnames goal)
  (argtypes ndline)
  (arghelps "An open line")
  (function ilo=com-foralli-sort-m-b)
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method foralli-sort-m-b"))
  
(defun ilo=com-foralli-sort-m-b  (goal)
  (pplan~execute-method 'foralli-sort-m-b goal nil nil))

(agent~defmatrix foralli-sort-m-b
   (agents (c-predicate (for goal)
                        (uses )
			(level 1)
                        (definition (pred1 goal))))
   (predicates
    (pred1 (a1)
	    (and (data~appl-p a1)
		 (data~schema-equal (data~appl-function a1)
				    (env~lookup-object :forall-sort
						       (pds~environment omega*current-proof-plan)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand existsi-sort-m-b
  (argnames goal)
  (argtypes ndline)
  (arghelps "An open line")
  (function ilo=com-existsi-sort-m-b)
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method existsi-sort-m-b"))
  
(defun ilo=com-existsi-sort-m-b  (goal)
  (pplan~execute-method 'existsi-sort-m-b goal nil nil))

(agent~defmatrix existsi-sort-m-b
   (agents (c-predicate (for goal)
                        (uses)
			(level 1)
                        (definition (pred1 goal))))
   (predicates
    (pred1 (a1)
	   (and (data~appl-p a1)
		(data~schema-equal (data~appl-function a1)
				   (env~lookup-object :exists-sort
					       (pds~environment omega*current-proof-plan)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand group-closed-m-b
  (argnames goal group)
  (argtypes ndline ndline)
  (arghelps "An open line" "An assumption with group")
  (function ilo=com-group-closed-m-b)
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method group-closed-m-b"))
  
(defun ilo=com-group-closed-m-b  (goal group)
  (pplan~execute-method 'group-closed-m-b goal (list group) nil))

(defun ilo=com-group-closed-m-b-pred  (group goalline)
  (let* ((tupel (ilo=group? group))
	 (grp   (first tupel))
	 (op    (second tupel))
	 (goal (pdsn~current-formula goalline)))
    (and  (data~appl-p goal)
	  (data~equal grp (data~appl-function goal))
	  (data~appl-p (first (data~appl-arguments goal)))
	  (data~equal op (data~appl-function (first (data~appl-arguments goal)))))))

(agent~defmatrix group-closed-m-b
   (agents (c-predicate (for goal)
                        (uses group)
			(level 1)
                        (definition (ilo=com-group-closed-m-b-pred group (:node goal))))
	   (s-predicate (for group)
                        (uses )
			(level 1)
                        (definition (pred1 group))))
   (predicates
    (pred1 (a1)
	   (ilo=group? a1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand group-neut-m-b
  (argnames goal group)
  (argtypes ndline ndline)
  (arghelps "An open line" "An assumption with group")
  (function ilo=com-group-neut-m-b)
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method group-neut-m-b"))
  
(defun ilo=com-group-neut-m-b  (goal group)
  (pplan~execute-method 'group-neut-m-b goal (list group) nil))


(agent~defmatrix group-neut-m-b
   (agents (c-predicate (for goal)
                        (uses group)
			(level 1)
                        (definition (pred1 group goal)))
	   (s-predicate (for group)
                        (uses )
			(level 1)
                        (definition (ilo=group? group))))
   (predicates
    (pred1 (group goal)
	   	   (when (logic~equality-p goal)
	     (let* ((tupel (ilo=group? group))
		    (grp (first tupel))
		    (op (second tupel))
		    (env (pds~environment omega*current-proof-plan))
		    (var (term~generate-term-primitive-with-new-name
			  'agent-var
			  (data~abstr-c-domain (term~type grp))
			  'term+variable   env))
		    (var2 (term~generate-term-primitive-with-new-name
			  'agent-var
			  (data~abstr-c-domain (term~type grp))
			  'term+variable   env))
		    (rhs1  (term~appl-create
			    op
			    (list var var2)))
		    (rhs2  (term~appl-create
			    op
			    (list var2 var))))
		    (or (ilo=equalities-equal? (term~appl-create (logic~equality-constant)
								 (list var rhs1))
					       goal)
			(ilo=equalities-equal? (term~appl-create (logic~equality-constant)
								 (list var rhs2))
					       goal)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand group-inv-m-b
  (argnames goal group)
  (argtypes ndline ndline)
  (arghelps "An open line" "An assumption with group")
  (function ilo=com-group-inv-m-b)
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method group-inv-m-b"))
  
(defun ilo=com-group-inv-m-b  (goal group)
  (pplan~execute-method 'group-inv-m-b goal (list group) nil))

(defun ilo=com-group-inv-m-b-pred  (group goal)
	   (when (logic~equality-p goal)
	     (let* ((tupel (ilo=group? group))
		    (grp (first tupel))
		    (op (second tupel))
		    (env (pds~environment omega*current-proof-plan))
		    (var1 (term~generate-term-primitive-with-new-name
			  'agent-var
			  (data~abstr-c-domain (term~type grp))
			  'term+variable   env))
		    (var2 (term~generate-term-primitive-with-new-name
			  'agent-var
			  (data~abstr-c-domain (term~type grp))
			  'term+variable   env))
		    (var3 (term~generate-term-primitive-with-new-name
			  'agent-var
			  (data~abstr-c-domain (term~type grp))
			  'term+variable   env)))
	       (ilo=equalities-equal? (term~appl-create (logic~equality-constant)
							(list var1 (term~appl-create
								    op
								    (list var2 var3))))
					       goal))))


(agent~defmatrix group-inv-m-b
   (agents (c-predicate (for goal)
                        (uses group)
			(level 1)
                        (definition (ilo=com-group-inv-m-b-pred group goal)))
	   (s-predicate (for group)
                        (uses )
			(level 1)
                        (definition (ilo=group? group)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(com~defcommand andi-m-b
;  (argnames goal)
;  (argtypes ndline)
;  (arghelps "An open line")
;  (function ilo=com-andi-m-b)
;  (defaults )
;  (frag-cats tactics group)
;  (log-p T)
;  (help "Applies the method andi-m-b"))
;
;(defun ilo=com-andi-m-b  (goal)
;  (pplan~execute-method 'andi-m-b goal nil nil))
;
;(agent~defmatrix andi-m-b
;   (agents (c-predicate (for goal)
;                        (uses )
;                        (level 1)
;                        (definition (pred1 goal))))
;   (predicates
;    (pred1 (a1)
;            (logic~conjunction-p a1))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand equal-func-m-b
  (argnames goal)
  (argtypes ndline)
  (arghelps "An open line with an equation")
  (function ilo=com-equal-func-m-b)
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method equal-func-m-b"))

(defun ilo=com-equal-func-m-b  (goal)
  (pplan~execute-method 'equal-func-m-b goal nil nil))

(agent~defmatrix equal-func-m-b
   (agents (c-predicate (for goal)
                        (uses )
			(level 1)
                        (definition (pred1 goal))))
   (predicates
    (pred1 (a1)
	   (when (logic~equality-p a1)
	     (let* ((args (data~appl-arguments a1))
		    (lhs (first args))
		    (rhs (second args)))
	       (and (data~appl-p lhs)
		    (data~appl-p rhs)
		    (data~equal (data~appl-function lhs)(data~appl-function rhs))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand unpack-s-b
  (argnames goal)
  (argtypes ndline)
  (arghelps "An open line containing quantifier, connector or definition")
  (function ilo=com-unpack-s-b )
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method unpack-s-b"))
  
(defun ilo=com-unpack-s-b  (goal)
  (pplan~execute-method 'unpack-s-b goal nil nil))

(agent~defmatrix unpack-s-b
   (agents (c-predicate (for goal)
                        (uses )
			(level 1)
                        (definition (pred1  goal))))
   (predicates
    (pred1 (a1)
	   (and (data~appl-p a1)
		(or (data~schema-equal (data~appl-function a1)
				       (env~lookup-object :forall-sort
							  (pds~environment omega*current-proof-plan)))
		    (data~schema-equal (data~appl-function a1)
				       (env~lookup-object :exists-sort
							  (pds~environment omega*current-proof-plan)))
		    (logic~connective-p goal))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(com~defcommand element-of-domain-m-b2
;  (argnames goal image)
;  (argtypes ndline ndline)
;  (arghelps "An open line stating that some 'x' is element of the domain of 'f'"
;            "A line that states that 'y' is element of the image of 'f'")
;  (function ilo=com-element-of-domain-m-b2 )
;  (defaults )
;  (frag-cats tactics group)
;  (log-p T)
;  (help "Applies the method element-of-domain-m-b2"))
;  
;(defun ilo=com-element-of-domain-m-b2  (goal image)
;  (pplan~execute-method 'element-of-domain-m-b2 goal (list image) nil))
;
;(agent~defmatrix element-of-domain-m-b2
; (agents
;  (c-predicate (for goal)
;               (uses  image)
;               (level 1)
;               (definition (pred1 goal image)))
;  (s-predicate (for image)
;               (uses)
;               (level 1)
;               (definition (pred2 image))))
; (predicates
;    (pred1 (goal image)
;           (and (data~appl-p goal)
;                (data~equal (data~appl-function goal)
;                            (second (data~appl-arguments image))))
;           )
;    (pred2 (image)
;           (and
;            (data~appl-p image)
;            (data~schema-equal (env~lookup-object :image-of-domain (pds~environment omega*current-proof-plan))
;                               (data~appl-function image))
;            (= (length (data~appl-arguments image)) 3)))))
;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(com~defcommand element-of-domain-m-b1
;  (argnames goal image)
;  (argtypes ndline ndline)
;  (arghelps "An open line of the form 'f(x)=y'" "A line that states that 'y' is element of the image of 'f'")
;  (function ilo=com-element-of-domain-m-b1 )
;  (defaults )
;  (frag-cats tactics group)
;  (log-p T)
;  (help "Applies the method element-of-domain-m-b1"))
;  
;(defun ilo=com-element-of-domain-m-b1  (goal image)
;  (pplan~execute-method 'element-of-domain-m-b1 goal (list image) nil))
;
;(agent~defmatrix element-of-domain-m-b1
;   (agents (c-predicate (for goal)
;                        (uses )
;                        (level 1)
;                        (definition (pred0 goal)))
;           (c-predicate (for goal)
;                        (uses  image)
;                        (level 1)
;                        (definition (pred1 goal image)))
;           (s-predicate (for image)
;                        (uses)
;                        (level 1)
;                        (definition (pred2 image)))
;           (s-predicate (for image)
;                        (uses  goal)
;                        (level 1)
;                        (definition (pred3 goal image))))
;   (predicates
;    (pred (goal image)
;         (or (and (data~appl-p (first (data~appl-arguments goal)))
;                  (data~equal (data~appl-function (first (data~appl-arguments goal)))
;                              (first (data~appl-arguments image)))
;                  (data~equal (data~appl-function (second (data~appl-arguments goal)))
;                              (third (data~appl-arguments image))))
;             (and (data~appl-p (second (data~appl-arguments goal)))
;                  (data~equal (data~appl-function (second (data~appl-arguments goal)))
;                              (first (data~appl-arguments image)))
;                  (data~equal (data~appl-function (first (data~appl-arguments goal)))
;                              (third (data~appl-arguments image))))))
;    (pred0 (goal)
;           (and (logic~equality-p goal)
;                (or (and (data~appl-p (first (data~appl-arguments goal)))
;                         (data~primitive-p (second (data~appl-arguments goal))))
;                    (and (data~appl-p (second (data~appl-arguments goal)))
;                         (data~primitive-p (first (data~appl-arguments goal)))))))
;    (pred1 (goal image)
;           (and (logic~equality-p goal)
;                (pred goal image)))
;    (pred2 (image)
;           (and
;            (data~appl-p image)
;            (data~schema-equal (env~lookup-object :image-of-domain (pds~environment omega*current-proof-plan))
;                               (data~appl-function image))
;            (= (length (data~appl-arguments image)) 3)))
;    (pred3 (goal image)
;           (and
;            (data~appl-p image)
;            (data~schema-equal (env~lookup-object :image-of-domain (pds~environment omega*current-proof-plan))
;                               (data~appl-function image))
;            (= (length (data~appl-arguments image)) 3)
;            (pred goal image)))))
;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(com~defcommand element-of-domain-m-f
;  (argnames image pre)
;  (argtypes ndline ndline)
;  (arghelps  "An open line that states that 'y' is element of the image of 'f'" "A line of the form 'f(x)=y'")
;  (function ilo=com-element-of-domain-m-f )
;  (defaults )
;  (frag-cats tactics group)
;  (log-p T)
;  (help "Applies the method element-of-domain-m-f"))
;
;(defun ilo=com-element-of-domain-m-f  (image pre)
;  (pplan~execute-method 'element-of-domain-m-f image (list pre) nil))
;
;(agent~defmatrix element-of-domain-m-f
;   (agents (c-predicate (for image)
;                        (uses )
;                        (level 1)
;                        (definition (pred0 image)))
;           (c-predicate (for image)
;                        (uses  pre)
;                        (level 1)
;                        (definition (pred1 image pre)))
;           (s-predicate (for pre)
;                        (uses)
;                        (level 1)
;                        (definition (pred2 pre)))
;           (s-predicate (for pre)
;                        (uses  image)
;                        (level 1)
;                        (definition (pred3 image pre))))
;   (predicates
;    (pred (goal image)
;         (or (and (data~appl-p (first (data~appl-arguments goal)))
;                  (data~equal (data~appl-function (first (data~appl-arguments goal)))
;                              (first (data~appl-arguments image)))
;                  (data~equal (data~appl-function (second (data~appl-arguments goal)))
;                              (third (data~appl-arguments image))))
;             (and (data~appl-p (second (data~appl-arguments goal)))
;                  (data~equal (data~appl-function (second (data~appl-arguments goal)))
;                              (first (data~appl-arguments image)))
;                  (data~equal (data~appl-function (first (data~appl-arguments goal)))
;                              (third (data~appl-arguments image))))))
;    (pred0 (goal)
;           (and (logic~equality-p goal)
;                (or (and (data~appl-p (first (data~appl-arguments goal)))
;                         (data~primitive-p (second (data~appl-arguments goal))))
;                    (and (data~appl-p (second (data~appl-arguments goal)))
;                         (data~primitive-p (first (data~appl-arguments goal)))))))
;    (pred1 (goal image)
;           (and (logic~equality-p goal)
;                (pred goal image)))
;    (pred2 (image)
;           (and
;            (data~appl-p image)
;            (data~schema-equal (env~lookup-object :image-of-domain (pds~environment omega*current-proof-plan))
;                               (data~appl-function image))
;            (= (length (data~appl-arguments image)) 3)))
;    (pred3 (goal image)
;           (and
;            (data~appl-p image)
;            (data~schema-equal (env~lookup-object :image-of-domain (pds~environment omega*current-proof-plan))
;                               (data~appl-function image))
;            (= (length (data~appl-arguments image)) 3)
;            (pred goal image)))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand weaken-m-c
  (argnames goal closed)
  (argtypes ndline ndline)
  (arghelps "An open line." "A closed that equals the open line.")
  (function ilo=com-weaken-m-c)
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method weaken-m-c"))

(defun ilo=com-weaken-m-c  (goal closed)
  (pplan~execute-method 'weaken-m-c goal (list closed) nil))

(agent~defmatrix weaken-m-c
	(agents	  (c-predicate (for goal)
			       (level 1)
			       (definition #'(lambda (X) true)))
		  (s-predicate (for closed)
			       (uses goal)
			       (level 1)
			       (definition (ilo=equalities-equal? goal closed)))
		  (s-predicate (for closed)
			       (uses goal)
			       (level 1)
			       (definition (term~alpha-unify goal closed)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand group-assoc-m-b
 (argnames goal group)
 (argtypes ndline ndline)
 (arghelps "An open line" "An assumption with group")
 (function ilo=com-group-assoc-m-b)
 (defaults )
 (frag-cats tactics group)
 (log-p T)
 (help "Applies the method group-assoc-m-b"))
 
(defun ilo=com-group-assoc-m-b  (goal group)
  (pplan~execute-method 'group-assoc-m-b goal (list group) nil))

(agent~defmatrix group-assoc-m-b
  (agents (c-predicate (for goal)
                       (uses)
                       (level 1)
                       (definition (pred1 goal)))
          (s-predicate (for group)
                       (uses goal)
                       (level 1)
                       (definition (pred2 group goal))))
  (predicates
   (pred1 (goal)
	  (when (logic~equality-p goal)
	    (let ((rhs  (ilo=assoc)))
	      (or (term~alpha-match rhs (first (data~appl-arguments goal)))
		  (term~alpha-match rhs (second (data~appl-arguments goal)))))))
   (pred2 (group goal)
	  (let ((grp-op (second (ilo=group? group))))
	    (when grp-op
	       (or (data~equal (data~appl-function (first (data~appl-arguments goal)))
			       grp-op)
		   (data~equal (data~appl-function (second (data~appl-arguments goal)))
			       grp-op)))))))

   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand subset-image-range-m-b
 (argnames goal homo)
 (argtypes ndline ndline)
 (arghelps "An open line 'R(c)'" "A homomorphism with codomain 'R'")
 (function ilo=com-subset-image-range-m-b)
 (defaults )
 (frag-cats tactics group)
 (log-p T)
 (help "Applies the method subset-image-range-m-b"))
 
(defun ilo=com-subset-image-range-m-b  (goal homo)
  (pplan~execute-method 'subset-image-range-m-b goal (list homo) nil))

(agent~defmatrix subset-image-range-m-b
  (agents (c-predicate (for goal)
                       (uses homo)
                       (level 1)
                       (definition (pred1 goal homo)))
          (s-predicate (for homo)
                       (uses)
                       (level 1)
                       (definition (ilo=homomorphism? homo))))
  (predicates
   (pred1 (goal homo)
	  (when (data~appl-p goal)
	    (let ((grp (fourth (ilo=homomorphism? homo))))
	      ;(omega~trace "~%~% grp ~A func ~A bool ~A ~%~%" grp (data~appl-function goal)
		;	   (data~equal grp (data~appl-function goal)))
	      (when grp (data~equal grp (data~appl-function goal))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand neutral-in-group-m-c
 (argnames goal group)
 (argtypes ndline ndline)
 (arghelps "An open line" "An assumption with group")
 (function ilo=com-neutral-in-group-m-c)
 (defaults )
 (frag-cats tactics group)
 (log-p T)
 (help "Applies the method neutral-in-group-m-c"))
 
(defun ilo=com-neutral-in-group-m-c  (goal group)
  (pplan~execute-method 'neutral-in-group-m-c goal (list group) nil))

(agent~defmatrix neutral-in-group-m-c
  (agents (c-predicate (for goal)
                       (uses)
                       (level 1)
                       (definition (pred0 goal)))
	  (c-predicate (for goal)
                       (uses group)
                       (level 1)
                       (definition (pred1 goal group)))
          (s-predicate (for group)
                       (uses goal)
                       (level 1)
                       (definition (pred2 goal group))))
  (predicates
   (pred0 (goal)
	  (when (data~appl-p goal)
	    (let ((goal-arg (first (data~appl-arguments  goal))))
	      (and (data~appl-p goal-arg)
		   (data~schema-equal (data~appl-function goal-arg)
				      (env~lookup-object 'group-unit
							 (pds~environment omega*current-proof-plan)))))))
   (pred1 (goal group)
	    (let ((args (ilo=group? group)))
	      (when (and (data~appl-p goal)
			 (data~equal (data~appl-function goal)
				     (first args)))
		     (let ((goal-arg (first (data~appl-arguments  goal))))
		       (and (data~appl-p goal-arg)
			    (data~schema-equal (data~appl-function goal-arg)
					 (env~lookup-object 'group-unit
							    (pds~environment omega*current-proof-plan)))
			    (every #'data~equal args (data~appl-arguments goal-arg)))))))
   (pred2 (goal group)
	  (let ((args (ilo=group? group)))
	    (when (and  args 
			(data~appl-p goal)
			(data~equal (data~appl-function goal)
				    (first args)))
	      (let ((goal-arg (first (data~appl-arguments  goal))))
		(and (data~appl-p goal-arg)
		     (data~schema-equal (data~appl-function goal-arg)
					(env~lookup-object 'group-unit
							   (pds~environment omega*current-proof-plan)))
		     (every #'data~equal args (data~appl-arguments goal-arg)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand inverse-in-group-m-b
 (argnames goal group)
 (argtypes ndline ndline)
 (arghelps "An open line" "An assumption with group")
 (function ilo=com-inverse-in-group-m-b)
 (defaults )
 (frag-cats tactics group)
 (log-p T)
 (help "Applies the method inverse-in-group-m-b"))
 
(defun ilo=com-inverse-in-group-m-b  (goal group)
  (pplan~execute-method 'inverse-in-group-m-b goal (list group) nil))

(agent~defmatrix inverse-in-group-m-b
  (agents (c-predicate (for goal)
                       (uses)
                       (level 1)
                       (definition (pred0 goal)))
	  (c-predicate (for goal)
                       (uses group)
                       (level 1)
                       (definition (pred1 goal group)))
          (s-predicate (for group)
                       (uses goal)
                       (level 1)
                       (definition (pred2 goal group))))
  (predicates
   (pred0 (goal)
	  (when (data~appl-p goal)
	    (let ((goal-arg (first (data~appl-arguments  goal))))
	      (and (data~appl-p goal-arg)
		   (data~schema-equal (data~appl-function goal-arg)
				      (env~lookup-object 'group-inverse
							 (pds~environment omega*current-proof-plan)))))))
   (pred1 (goal group)
	    (let ((args (ilo=group? group)))
	      (when (and (data~appl-p goal)
			 (data~equal (data~appl-function goal)
				     (first args)))
		     (let ((goal-arg (first (data~appl-arguments  goal))))
		       (and (data~appl-p goal-arg)
			    (data~schema-equal (data~appl-function goal-arg)
					       (env~lookup-object 'group-inverse
								  (pds~environment omega*current-proof-plan)))
			    (every #'data~equal args (butlast (data~appl-arguments goal-arg))))))))
   (pred2 (goal group)
	  (let ((args (ilo=group? group)))
	    (when (and  args 
			(data~appl-p goal)
			(data~equal (data~appl-function goal)
				    (first args)))
	      (let ((goal-arg (first (data~appl-arguments  goal))))
		(and (data~appl-p goal-arg)
		     (data~schema-equal (data~appl-function goal-arg)
					(env~lookup-object 'group-inverse
							   (pds~environment omega*current-proof-plan)))
		     (every #'data~equal args (butlast (data~appl-arguments goal-arg))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~defcommand apply-assertion-m-b
  (argnames goal assertion)
  (argtypes ndline ndline)
  (arghelps "An open line that follows from the assumption" "The assumption to be applied")
  (function ilo=com-apply-assertion-m-b )
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method apply-assertion-m-b"))
  
(defun ilo=com-apply-assertion-m-b  (goal assertion)
  (pplan~execute-method 'apply-assertion-m-b goal (list assertion) nil))

(agent~defmatrix apply-assertion-m-b 
   (agents (c-predicate (for goal)
                        (uses assertion)
			(level 1)
                        (definition (pred1 goal assertion)))
           (s-predicate (for assertion)
                        (uses)
			(level 1)
                        (definition (logic~logical-p assertion))))
   (predicates
    (pred1 (goal assertion)
	   (and assertion (logic~logical-p goal)))))	     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFN-EXPAND AGENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand DefnExp-m-b
		(argnames goal const)
		(argtypes ndline term)
		(arghelps "An open line with a definition to expand"
			  "The defined constant to expand")
		(function ilo~com-DefnExp-m-b)
		(defaults )
		(frag-cats tactics group)
		(log-p t)
		(help "Applies the method DefnExp-m-b"))


(defun ilo~com-defnexp-m-b (goal term)
  (pplan~execute-method 'defnexp-m-b goal nil (list (nic=defined-const (node~formula goal)))))

(defun nic=defined-const (goal)
  (cond ((logic~negation-p goal)
	 (if (data~appl-p (first (data~appl-arguments goal)))
	     (data~appl-function (first (data~appl-arguments goal)))
	   (first (data~appl-arguments goal))))
	((data~appl-p goal)
	 (data~appl-function goal))
	(t
	 goal)))


(defun ilo~com-defnexp-m-b-pred1 (obj)
  (notany #'(lambda (thy) (th~find-assumption (keim~name obj) thy))
	  '(generic base post)))  
  
(agent~defmatrix DefnExp-m-b
		  (agents
		   (c-predicate (for goal)
				(uses )
				(level 1)
				(definition (pred1 goal)))
		   (function (for const)
			     (uses goal)
			     (level 1)
			     (definition (func1 goal))))
		  (predicates
		   (pred1 (g)
			  (let* ((const (nic=defined-const g))
				 (th-assumption (th~find-assumption (keim~name const) (prob~theory omega*current-proof-plan))))
			    (if (and (th~definition-p th-assumption)
				     (ilo~com-defnexp-m-b-pred1 const))
				't
			      nil)))
		   (func1 (g)
			  (nic=defined-const g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   reflex-m-b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand reflex-m-b
  (argnames goal)
  (argtypes ndline)
  (arghelps "An open line with an equation.")
  (function ilo=com-reflex-m-b)
  (defaults )
  (frag-cats tactics group)
  (log-p T)
  (help "Applies the method reflex-m-b"))

(defun ilo=com-reflex-m-b  (goal)
  (pplan~execute-method 'reflex-m-b goal nil nil))

(agent~defmatrix reflex-m-b
   (agents (c-predicate (for goal)
                        (uses )
                        (level 1)
                        (definition (pred1 goal))))
   (predicates
    (pred1 (a1)
	   (and
            (logic~equality-p a1)
	    (term~alpha-unify (car (data~appl-arguments a1))
			      (cadr (data~appl-arguments a1)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Customization
;;;;

(th~defagentdefault group
		    (nic~use-nic-agents)
		    (oc=stop-use-resources)
		    (auto~set-auto-default-interval 10)
		    (nic~use-nic-heuristics)
		    (nic~add-command-agents '(
					      counterexample-by-satchmo
					      solved-by-fo-atp
					      solved-by-leo-pl-atp
					      solved-or-result-by-LEO))
		    )



