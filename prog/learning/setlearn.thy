(in-package :omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;condfuncs

(meth~deffun agplandefinition (formula)
	     (declare)
	     (let ((defi (agplan~contained-definition formula)))
	       (when defi
		 (th~definition-constant defi))))


(meth~defcond agent-pl-atp (args mmapp)
	      (declare)
	      (let* ((node (first args))
		     (formula (second args))
		     (result (when (and (agplan~pl-like-formula-p formula)
					(not (agplan~contains-simplifiable-terms formula)))
			       (agplan~tackle-by-pl-atp node
							(keim::agent=unique-parameter-agent-name
							 :solved-by-pl-atp
							 (list :node :key-to-proof)
							 nil)
							auto*default-interval))))
		(meth~mapp-new-constraint mmapp result)))

(meth~defcond agent-counterexample-by-satchmo (args mmapp)
	      (declare)
	      (let* ((node (first args))
		     (formula (second args))
		     (result (when (and (agplan~pl-like-formula-p formula)
					(not (agplan~contains-simplifiable-terms formula))
					(not (logic~falsity-constant-p formula))
					(if (pds~node-supports node)
					    (mapcan #'(lambda (x) (logic~fo-formula-p (node~formula x)))
						    (pds~node-supports  node))
					  t))
			       (bloodypollet~find-counterexample-by-satchmo node))))
		(meth~mapp-new-constraint mmapp result)))

(defun bloodypollet~find-counterexample-by-satchmo (line)
  (multiple-value-bind (res out-string)
      (satch~call-satchmo line ; the proof line
			  OMEGA*CURRENT-PROOF-PLAN
			  (atptop~get-default-directory) ; the directory 
			  1000			; ressource
			  )
    (setf agplan*current-counterexample out-string)
    (let ((new-pds (when (not res)  ;;; not yet realised)
	       (format nil "~%Satchmo has found a countermodel for line ~A: ~%~A~%" line out-string))))
      (when new-pds 
	(agplan~store-new-pds line :satchmo (gensym) new-pds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; methods


(infer~defmethod nic-forall-i-m (outline-mappings (((existent nonexistent) nic-forall-i-m-b))))

(meth~defmethod nic-forall-i-m-b nic-forall-i-m
		(in typed-set)
                (reasoning :planning)
		(rating 1)
		(declarations
		 (type-variables aa)
		 (sorted-meta-variables (l2 o prln) (phi o)
					(c aa const)))
                 (premises (+ l2))

                 (application-condition
                  (mand (mbind c (var-newconst x))
			(test-tactic (:symbol nic-forall-i)
				     (mlist l1 (mnil)) (mlist c))))

		 (conclusions (- l1))

		 (outline-computations
                  (dummy (apply-tactic (:symbol nic-forall-i)
				       (mlist l1 (mnil)) (mlist c)))
                  (l2 (msecond dummy)))

		 (decl-content (l1 () (forall (lam (x aa var) phi))))
                 (proc-content apply-tactic))




(infer~defmethod set-ext-contract-m (outline-mappings (((existent nonexistent) set-ext-contract-m-b))))

(meth~defmethod set-ext-contract-m-b set-ext-contract-m
		(in typed-set)
                (reasoning :planning)
		(rating 1)
		(declarations
		 (sorted-meta-variables (l2 o prln) (phi o)))
		
                 (premises (+ l2))

                 (application-condition
			(test-tactic (:symbol set-ext-contract)
				     (mlist l1 (mnil)) (mnil)))

		 (conclusions (- l1))

		 (outline-computations
                  (dummy (apply-tactic (:symbol set-ext-contract)
				       (mlist l1 (mnil)) (mnil)))
                  (l2 (msecond dummy)))

		 (decl-content (l1 () phi))
                 (proc-content schema-interpreter))

(infer~defmethod pl-atp-m (outline-mappings (((existent) pl-atp-m-b))))

(meth~defmethod pl-atp-m-b pl-atp-m
		(in typed-set)
                (reasoning :planning)
		(rating 1)
		(declarations
		 (sorted-meta-variables (phi o)))
		
                 (application-condition
		  (agent-pl-atp l1 phi))

		 (conclusions (- l1))

		 (decl-content (l1 () phi))
                 (proc-content apply-tactic))


(infer~defmethod  counterexample-by-satchmo-m (outline-mappings (((existent)  counterexample-by-satchmo-m-b))))

(meth~defmethod  counterexample-by-satchmo-m-b  counterexample-by-satchmo-m
		(in typed-set)
                (reasoning :planning)
		(rating 1)
		(declarations
		 (sorted-meta-variables (phi o)))
		
                 (application-condition
		  (agent-counterexample-by-satchmo l1 phi))

		 (conclusions (- l1))

		 (decl-content (l1 () phi))
                 (proc-content apply-tactic))


(infer~defmethod defni*-m (outline-mappings (((existent nonexistent) defni*-m-b))))


(meth~defmethod defni*-m-b defni*-m
		(in typed-set)
		(rating 1)
		(reasoning :planning :middle-out)
		(declarations
		 (type-variables ccc aaa bbb)
		 (sorted-meta-variables
		  (phi o) (phi-prime o) (phi-prime-prime o)
		  (pairop ccc)
		  (poslist o positionlist)
		  ;;(new-prem o prln)
		  )
		 )
		
		(premises (+ l0))
		(conclusions (- l100))	
		
		(application-condition
		 (mand (mbind pairop (agplandefinition phi))
		       (termoccs pairop phi poslist)
		       (th-definition (termatpos phi (mfirst poslist)) pairopdef)
		       ;;(test-tactic (:symbol defni*)
		       ;;			 (mlist l100 (mnil))
		       ;;	 		 (mlist pairop pairopdef poslist))
		       ))
		
		(outline-computations
		 ;;(new-prem (msecond (apply-tactic (:symbol defni*)
		 ;;				  (mlist l100 (mnil))
		 ;;				  (mlist pairop pairopdef poslist)))))
		 (phi-prime (apply-defni* phi pairop pairopdef poslist)))
		
		(decl-content
		 (l0   () phi-prime          ("open" () ()))
		 (l100 () phi                ("defni*" (pairop pairopdef poslist) (l0)))
		 )

		(proc-content schema-interpreter)
		;; (proc-content apply-tactic))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; learn

(lea~produce-method '( (exp  nic-forall-i-m-b  3)  set-ext-contract-m-b   nic-forall-i-m-b  (star-max  defni*-m-b)
		       (disj  counterexample-by-satchmo-m-b  pl-atp-m-b ) )
		    :method 'set-agents-m-b :inference 'set-agents-m :theory 'typed-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; strat
(strat~define-strategy-ks
 (name learnedset)
 (refinement-algorithm PPlanner)
 (condition trylearnedset?)
 (methods (
	   set-agents-m-b
	   nic-forall-i-m-b 
	   set-ext-contract-m-b 
	   pl-atp-m-b 
	   counterexample-by-satchmo-m-b 
	   defni*-m-b 

	   truei-m-b
	   ))
 (normalization-methods )
 (restriction-methods (	   	   truei-m-b
		       ))
 (control-rules  ())
 (loop-detection 5)
 (randomization-rules nil)
 (selection waterfall)
 (termination-check no-further-goal-p)
 (print "Set examples by learned agents behaviour"))

(strat~define-strategy-ks
 (name onlyset)
 (refinement-algorithm PPlanner)
 (condition trylearnedset?)
 (methods (
	   ;set-agents-m-b
	   nic-forall-i-m-b 
	   set-ext-contract-m-b 
	   pl-atp-m-b 
	   counterexample-by-satchmo-m-b 
	   defni*-m-b 

	   truei-m-b
	   ))
 (normalization-methods )
 (restriction-methods (	   	   truei-m-b
		       ))
 (control-rules  ())
 (loop-detection 5)
 (randomization-rules nil)
 (selection waterfall)
 (termination-check no-further-goal-p)
 (print "Set examples by learned agents behaviour"))

(defun trylearnedset? (forget) T) ;try the strategy for every proof situation
(defun no-further-goal-p () nil) ;never stop, MULTI stops when there are no open goals
