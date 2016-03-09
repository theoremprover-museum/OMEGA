;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solved by FO ATP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (agplan~apply-advertised-pds key-to-proof))


(agent~defagent solved-by-fo-atp c-ext-pred
                (for node key-to-proof)
                (uses )
                (level 1)
                (definition (when (not (agplan=contains-simplifiable-terms node))
			      (list (agplan~tackle-by-fo-atp (:node node)
							     (keim::agent=unique-parameter-agent-name
							      :solved-by-fo-atp
							      (list :node :key-to-proof)
							      nil)
							     auto*default-interval)))))

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

(defun agplan=solved-by-pl-atp (node key-to-proof)
  (agplan~apply-advertised-pds key-to-proof))


(agent~defagent solved-by-pl-atp c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (and (agplan~pl-like-formula-p node)
				       (not (agplan=contains-simplifiable-terms node)))
			      (list (agplan~tackle-by-pl-atp (:node node)
                                                           (keim::agent=unique-parameter-agent-name
                                                            :solved-by-pl-atp
                                                            (list :node :key-to-proof)
                                                            nil)
                                                            auto*default-interval)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solved by LEO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand solved-or-result-by-LEO
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "Node to prove with LEO" "A string referring to the automatically found proof.")
  (frag-cats extern)
  (function agplan=solved-by-leo)
  (log-p T) 
  (help ""))

(defun agplan=solved-by-LEO (line key-to-proof)
  (agplan~apply-advertised-pds key-to-proof))


(agent~defagent solved-or-result-by-leo c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (and (not (logic~fo-formula-p node))
				       (not (agplan=contains-simplifiable-terms node))
				       (not (agplan=leo-partial-result-subnode-p node)))
		              (omega~message "~%Agent \"result-by-leo\" is calling LEO with time-resource ~A"  (floor (/ auto*default-interval 10)))
			      (list (agplan~tackle-by-leo (:node node)
							  'ext-input-recursive
							  (floor (/ auto*default-interval 10)))))))

	
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solved by LEO as PL ATP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand solved-by-LEO-PL-ATP
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "Node to prove with LEO" "A string referring to the automatically found proof.")
  (frag-cats extern)
  (function agplan=solved-by-leo)
  (log-p T) 
  (help ""))

(agent~defagent SOLVED-BY-LEO-PL-ATP c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (and (agplan~pl-like-formula-p node)
				       (not (agplan=contains-simplifiable-terms node))
				       (not (agplan=leo-partial-result-subnode-p node)))
		              (omega~message "~%Agent \"LEO-PL-ATP\" is calling LEO with time-resource ~A"  auto*default-interval)
			      (let ((res (agplan~tackle-by-leo-pl (:node node)
								  'ext-input-recursive
								   auto*default-interval)
					 (when res (list res))))))))

	
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Counterexample with Satchmo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic counterexample-by-satchmo
		 (outline-mappings (((existent) counterexample-by-satchmo-a)))
                 (expansion-function batac=expand-counterexample-by-satchmo
		 (help "Counterexample by Satchmo.")))

(tac~deftactic counterexample-by-satchmo-a counterexample-by-satchmo (in base)
   (premises )
   (conclusions L1)
   (computations )
   (sideconditions )  ; (agplan=counterexample-with-satchmo (formula L1)))
   (description "Apply counterexample by Satchmo."))


(defun agplan=counterexample-with-satchmo (node)
  (agplan~find-counterexample-by-satchmo node))

 
(defun batac=expand-counterexample-by-satchmo (outline parameters)
  (omega~message "Expansion function has still to be implemented; should
produce the concrete counterexample and open the node agaIN ... Chris"))

(com~defcommand counterexample-by-satchmo
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "Node to analyze by satchmo" "A string referring to the countermodel.")
  (frag-cats extern)
  (function agplan=counterexample-by-satchmo)
  (log-p T) 
  (help ""))

(defun agplan=counterexample-by-satchmo (line key-to-proof)
  (infer~compute-outline 'counterexample-by-satchmo (list line) nil)
  (agplan~show-pds key-to-proof))

(agent~defagent counterexample-by-satchmo c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (and (agplan~pl-like-formula-p node)
				       (not (agplan=contains-simplifiable-terms node))
				       (not (logic~falsity-constant-p node))
				       (if (pds~node-supports (:node node))
					   (mapcan #'(lambda (x) (logic~fo-formula-p (node~formula x)))
						   (pds~node-supports (:node node)))
					   t))
			      (list (agplan~find-counterexample-by-satchmo
				     (:node node))))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simplify with CAS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic simplify-with-CAS
		 (outline-mappings (((nonexistent existent) simplify-with-CAS-f)
				    ((existent nonexistent) simplify-with-CAS-b)
				   ))
                 (expansion-function batac=simplify-with-CAS-expand)
		 (help "Simplification."))

(defun batac=simplify-with-CAS (line1 line2)
  (infer~compute-outline 'simplify-with-CAS (list line1 line2) nil))

(defun batac=simplify-with-CAS-expand (outline parameters)
  (omega~message "Expansion function has still to be implemented ... Chris"))

(com~defcommand simplify-with-CAS
  (argnames conclusion premise)
  (argtypes ndline ndline)
  (arghelps "The contracted conclusion line" "The expanded premise line")
  (function batac=simplify-with-CAS)
  (frag-cats tactics base introduction nic-special-tactic)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Simplify with CAS."))

(tac~deftactic simplify-with-CAS-b simplify-with-CAS (in base)
   (premises L1)
   (conclusions L2)
   (computations (L1 (batac=simplify-with-CAS-b (formula L2))))
   (sideconditions (agplan=contains-simplifiable-terms (formula L2)))
   (description "Simplify with CAS in forward direction."))

(defun batac=simplify-with-CAS-b (formula)
   (agplan~simplify-with-cas formula))
					      
(tac~deftactic simplify-with-CAS-f simplify-with-CAS (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=simplify-with-CAS-b (formula L1))))
   (sideconditions (agplan=contains-simplifiable-terms (formula L1)))
   (description "Simplify with CAS  in backward direction."))


(com~defcommand simplify-goal-with-CAS
  (argnames node key-to-proof)
  (argtypes ndline string)
  (arghelps "goal to simplify" "A string referring to the simplified pds.")
  (frag-cats extern)
  (function agplan=simplified-with-CAS)
  (log-p T) 
  (help ""))

(defun agplan=simplified-with-CAS (line key-to-proof)
  (agplan~apply-advertised-pds key-to-proof))

(defun agplan=contains-simplifiable-terms (node &optional (stringlist (list 'plus 'power
									    'times 'minus
									    'gcd 'lcm)))
  (data~positions node
		  #'(lambda (x) (and
				 (term~primitive-p x)
				 (find x stringlist
				       :test #'(lambda (x y)
						 (string-equal
						  (agplan~name x) y)))))))


(agent~defagent SIMPLIFY-GOAL-WITH-CAS c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (agplan=contains-simplifiable-terms node)
		              (omega~message "~%Agent \"SIMPLIFY-WITH-CAS\" is making calls to MAPLE in order to simplify the goal")
			      (let ((res (agplan~simplify-goal-with-CAS (:node node))))
				(when res (list res))))))



		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Heuristics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar auto*default-interval 5
  "Time in second for a single step of the mechanism.")


;;; kann nach Andrew demo weg 



(com~defcommand auto-default-interval
  (argnames seconds)
  (argtypes integer)
  (arghelps "Set the default execution interval for the O-ANTS theorem prover.")
  (frag-cats extern)
  (function set-auto-default-interval)
  (defaults (auto*default-interval))
  (log-p T) 
  (help ""))

(defun set-auto-default-interval (num)
  (setq auto*default-interval num))		







;;;;;;;;;;;  For Mateja and the Learning stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





