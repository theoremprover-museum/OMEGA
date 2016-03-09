(in-package :omega)

(eval-when (compile)
           (error "This file should not be compiled."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   SOLVED BY PL ATP  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand solved-by-pl-atp
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "Node to prove with PL-ATP" "A string referring to the automatically found proof.")
  (frag-cats extern)
  (function agplan=solved-by-pl-atp)
  (log-p T) 
  (help ""))


(defun agplan=solved-by-pl-atp (line key-to-proof)
  ;(agplan~show-pds key-to-proof)
  (keim~put line 'atp-problems
	     (cons (agplan~lookup-pds key-to-proof) (keim~get line 'atp-problems)))
  (let ((supports (pds~node-supports line)))
    (infer~compute-outline (infer~find-method 'pl-atp) (cons line supports)
			   (list nil)))
  (agplan~show-orig-pds))

(agent~defagent solved-by-pl-atp c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (agplan~pl-like-formula-p node)
			      (list (agplan~tackle-by-pl-atp (:node node))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   SOLVED BY FO ATP  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand solved-by-fo-atp
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "Node to prove with PL-ATP" "A string referring to the automatically found proof.")
  (frag-cats extern)
  (function agplan=solved-by-fo-atp)
  (log-p T) 
  (help ""))

(defun agplan=solved-by-fo-atp (line key-to-proof)
   ;(agplan~show-pds key-to-proof)
   (keim~put line 'atp-problems
	     (cons (agplan~lookup-pds key-to-proof) (keim~get line 'atp-problems)))
   (let ((supports (pds~node-supports line)))
     (infer~compute-outline (infer~find-method 'otter) (cons line supports)
			    (list nil)))
   (agplan~show-orig-pds)
  )

(agent~defagent solved-by-fo-atp c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (progn
			 	; (sleep 5)
	              		(list (agplan~tackle-by-fo-atp (:node node))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   SOLVED BY LEO     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand solved-by-LEO
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "Node to prove with LEO" "A string referring to the automatically found proof.")
  (frag-cats extern)
  (function agplan=solved-by-leo)
  (log-p T) 
  (help ""))

(defun agplan=solved-by-leo (line key-to-proof)
  (keim~put line 'atp-problems
	    (cons (agplan~lookup-pds key-to-proof) (keim~get line
							     'atp-problems)))
  (let ((supports (pds~node-supports line)))
    (infer~compute-outline (infer~find-method 'leo) (cons line supports)
			   nil))
  (agplan~show-orig-pds))



;(defun buf=fun (node)
;  (let* ((supports (pds~node-supports node))
;         (impl-term (when supports (node~formula (car supports))))
;         (rest-supports (rest supports))
;         (conjunction (dolist (support rest-supports
;                                       impl-term)
;                        (setf impl-term 
;                              (term~appl-create
;                               (logic~conjunction-constant)
;                               (list (node~formula support)
;                                     impl-term)))))
;         (leo-problem-node
;          (if conjunction
;              (pdsn~open-node-create
;               (term~appl-create
;                (logic~implication-constant)
;                (list conjunction (node~formula node)))
;               nil (pds~new-node-name))
;            (pdsn~open-node-create (node~formula node) nil
;                                   (pds~new-node-name))))
;         (res (agplan~tackle-by-leo
;               leo-problem-node 10)))
;    (when res (list res))))


(agent~defagent solved-by-leo c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (not (logic~fo-formula-p node))
			      (buf=fun (:node node)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Partially Solved by LEO  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(infer~deftactic partial-result-by-LEO
;                 (outline-mappings (((existent nonexistent) partial-result-by-LEO-b)
;                                    ))
;                 (parameter-types term)
;                 (expansion-function batac=expand-partial-result-by-LEO)
;                 (help "Call LEO and get partial result (FO clauses) back in negated form."))
;
;(tac~deftactic partial-result-by-LEO-b partial-result-by-LEO (in base)
;   (parameters (Term term+term "The term returned by LEO."))
;   (premises P)
;   (conclusions C)
;   (computations (P (special=compute-LEO-term Term)))
;   (sideconditions (special=sidecond-partial-result-by-LEO C Term))
;   (description "Backward application of partial-result-by-LEO."))
;
;(defun special=compute-LEO-term (term)
;  term)
;
;(defun special=sidecond-partial-result-by-LEO (conc term)
;  t)
;
;
;(com~defcommand partial-result-by-LEO
;  (argnames Conclusion Premise term)
;  (argtypes ndline ndline term)
;  (arghelps "Node to transform with LEO" "Line with FO clauses optained from LEO" "Term representing the (negated) FO clauses optained from LEO")
;  (frag-cats extern)
;  (function agplan=partial-result-by-leo)
;  (log-p T) 
;  (help ""))

;(defun agplan=partial-result-by-LEO (conc prem term)
;  (infer~compute-outline 'partial-result-by-LEO (list conc prem) (list term)))


;(agent~defagent solved-by-leo c-predicate
;                (for node key-to-proof)
;                (uses )
;                (level 1)
;                (definition (when (not (logic~fo-formula-p node))
;                              (buf=fun (:node node)))))


;(defun buf=fun (node)
;  (let* ((supports (pds~node-supports node))
;         (impl-term (when supports (node~formula (car supports))))
;         (rest-supports (rest supports))
;         (conjunction (dolist (support rest-supports
;                                       impl-term)
;                        (setf impl-term 
;                              (term~appl-create
;                               (logic~conjunction-constant)
;                               (list (node~formula support)
;                                     impl-term)))))
;         (leo-problem-node
;          (if conjunction
;              (pdsn~open-node-create
;               (term~appl-create
;                (logic~implication-constant)
;                (list conjunction (node~formula node)))
;               nil (pds~new-node-name))
;            (pdsn~open-node-create (node~formula node) nil
;                                   (pds~new-node-name))))
;         (multiple-value-bind  **** hier bin ich *** (agplan~tackle-by-leo
;               leo-problem-node 10)))
;    (when res (list res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   SOLVED BY TPS     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(com~defcommand solved-by-TPS
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "Node to prove with TPS" "A string referring to the automatically found proof.")
  (frag-cats extern)
  (function agplan=solved-by-tps)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-pl-atp-node)))
  (log-p T) 
  (help ""))

(defun agplan=solved-by-tps (line key-to-proof)
  (agplan~show-pds key-to-proof))


(agent~defagent solved-by-tps c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (not (logic~fo-formula-p node))
			      (list (agplan~tackle-by-tps (:node node))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Counterexample with Satchmo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand counterexample-by-satchmo
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "Node to analyze by satchmo" "A string referring to the countermodel.")
  (frag-cats extern)
  (function agplan=counterexample-by-satchmo)
  (log-p T) 
  (help ""))

(defun agplan=counterexample-by-satchmo (line key-to-proof)
  (agplan~show-pds key-to-proof))


(agent~defagent counterexample-by-satchmo c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (logic~fo-formula-p node)
			      (list (agplan~find-counterexample-by-satchmo (:node node))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MP modulo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic impe-modulo
		 (outline-mappings (((nonexistent existent existent) impe-modulo-f)
                                    ((existent existent existent) impe-modulo-a)
                                    ((existent existent nonexistent) impe-modulo-l)
                                    ))
		 (parameter-types string string)
		 (expansion-function batac=expand-impe-modulo)
		 (help "Modus Ponens Modulo."))


(tac~deftactic impe-modulo-f impe-modulo (in base)
   (parameters (sys string "The system to be applied.") (key-to-proof string "The key to the proof"))
   (premises L1 L2)
   (conclusions L3)
   (computations (L3 (batac=impe-modulo-compute-f (formula L2))))
   (sideconditions )
   (description "Forward application of modus ponens modulo."))


(tac~deftactic impe-modulo-a impe-modulo (in base)
   (parameters (sys string "The system to be applied.") (key-to-proof string "The key to the proof"))
   (premises L1 L2)
   (conclusions L3)
   (sideconditions (batac=impe-modulo-a-p (formula L1) (formula L3)))
   (description "Closing modus ponens modulo."))

(defun batac=impe-modulo-compute-f (imp)
  (when (logic~implication-p imp)
    (second (data~appl-arguments imp))))

(defun batac=impe-modulo-a-p (aimpb b)
  (and (logic~implication-p aimpb)
       (data~equal (second (data~appl-arguments aimpb))
		   b)))

(tac~deftactic impe-modulo-l impe-modulo (in base)
   (parameters (sys string "The system to be applied.") (key-to-proof string "The key to the proof"))
   (premises L1 L2)
   (conclusions L3)
   (computations (L2 (batac=impe-modulo-l-create (formula L1))))
   (sideconditions (batac=impe-modulo-a-p (formula L1) (formula L3)))
   (description "Sideward application (left) of modus ponens modulo."))


(defun batac=impe-modulo-l-create (aimpb)
  (car (data~appl-arguments aimpb)))

(defun batac=expand-impe-modulo (outline parameters)
  (let ((precond1 (second outline))  ;;  (A=>B) 
	(precond2 (third outline))   ;;  nB
	(conc (first outline)))     ;;   nA
    (tacl~init outline)
    (omega~message "To do, chris")
    (tacl~end)))


(com~defcommand impe-modulo
  (argnames aimpb a b sys key-to-proof)
  (argtypes ndline ndline ndline string string)
  (arghelps "A line with implication" "A line which implies the antecedent"
            "A line with the succedent (conclusion)." "The system to be employed for the subproof."
	    "The key to the proof")
  (function batac=impe-modulo)
  (frag-cats tactics base)
  (defaults )
  (log-p T)
  (help "Modus Ponens Modulo."))


(defun batac=impe-modulo (aimpb a b sys key-to-proof)
  ; (agplan~show-pds key-to-proof)
  (keim~put b 'atp-problems
	    (cons (agplan~lookup-pds key-to-proof) (keim~get b 'atp-problems)))
  (infer~compute-outline 'impe-modulo (list b aimpb a) (list sys key-to-proof))
  (agplan~show-orig-pds))


(agent~defmatrix impe-modulo 
  (agents  (c-predicate (for b aimpb)
                        (uses )
			(exclude a)
			(level 1)
                        (definition (let ((res (find-if #'(lambda (x)
							    (and (logic~implication-p (node~formula x))
								 (term~alpha-equal (second (data~appl-arguments (node~formula x)))
										   b)))
							(pds~node-supports (:node b)))))
				      (when res (list res)))))
	   (s-predicate (for a sys key-to-proof)
                        (uses aimpb)
			(level 1)
                        (definition
			  (when (not (string-equal (keim~name (:node a)) (keim~name (:node aimpb))))
			    (let ((res (agplan~tackle-by-leo
					(pdsn~open-node-create
					 (term~appl-create (logic~implication-constant)
							   (list a (car (data~appl-arguments aimpb))))
					 nil
					 (pds~new-node-name)))))
			      (when res (list "LEO" res))))))))




