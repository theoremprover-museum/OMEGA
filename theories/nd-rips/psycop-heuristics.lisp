(defun psycop-heuristics=proof-finished-p ()
  (null (pds~open-nodes omega*current-proof-plan)))

(defun psycop-heuristics~heuristics ()
  (if (not (psycop-heuristics=proof-finished-p))
      (progn (apply-com-to-candidates "RIPS-MATCHING" (find-candidates-rips-matching))
	     (apply-forward-rules)
	     (apply-com-to-candidates "RIPS-MATCHING" (find-candidates-rips-matching))
	     (if (not (psycop-heuristics=proof-finished-p))
	       (if (attempt-one-backward-rule-application (ordered-subgoals))
		   (psycop-heuristics~heuristics)
		 NIL)))
    NIL))
   
(defun psycop-heuristics~find-supports (pred &optional (pds pds*current-proof-plan))
  (let ((supp-nodes (pds~support-nodes pds)))
    (append (mapcan #'(lambda (node) (and (eval (list pred (node~formula node))) (list node)))
		 supp-nodes)
	(mapcan #'(lambda (node) (and (eval (list pred (node~formula node))) (list node)))
		 (set-difference (prob~proof-steps pds) supp-nodes)))
    ))

;;;;;;;;;;;;;;;; FORWARD APPLICATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; old 

(defun psycop-heuristics=find-candidates-rips-modus-ponens ()
  (let ((implications (psycop-heuristics~find-supports
		       (lambda (x) (logic~implication-p  x)))))
    (mapcan (lambda (x)
	      (let* ((antecedents
		      (psycop-heuristics~find-supports
		       (lambda (y)
			 (equal y (first
				   (data~appl-arguments
				    (node~formula x))))))))
		(mapcar (lambda (y) (list x y NIL))
			antecedents))
	      )
	    implications)
    )
  )

;;;; new ---> this one is correct, whereas the one above is not.

(defun psycop-heuristics=find-candidates-rips-modus-ponens ()
  (let ((implications (psycop-heuristics~find-supports
		       (lambda (x) (logic~implication-p  x)))))
    (mapcan (lambda (x)
	      (let* ((domain-nodes (nd-rips=domain x))
		     (antecedents (remove-if (lambda (y) (not (equal (node~formula y) (first
				   (data~appl-arguments
				    (node~formula x)))))) domain-nodes)))
		(mapcar (lambda (y) (list x y NIL))
			antecedents))
	      )
	    implications)
    )
  )

(defun psycop-heuristics=find-candidates-rips-modus-ponens ()
  (let ((implications (psycop-heuristics~find-supports
		       (lambda (x) (logic~implication-p  x)))))
    (mapcan (lambda (x)
	      (let* ((antecedents
		      (psycop-heuristics~find-supports
		       (lambda (y)
			 (equal y (first (data~appl-arguments (node~formula x))))))))
		(mapcar (lambda (y) (list x y NIL))
			antecedents))
	      )
	    implications)
    )
  )


;;; TODO: are the above functions really different?

 (defun psycop-heuristics=find-candidates-rips-fwd-ande ()
  (let ((conjunctions (psycop-heuristics~find-supports
		       (lambda (x) (logic~conjunction-p  x)))))
    (mapcar (lambda (x) (list x NIL NIL)) conjunctions)
    )
  )

(defun psycop-heuristics=doubleneg-p (F2)
    (and (logic~negation-p F2)
	 (let ((F21 (first (data~appl-arguments F2))))
	   (and (logic~negation-p F21)))))

(defun psycop-heuristics=find-candidates-rips-fwd-notnote ()
  (let ((notnots (psycop-heuristics~find-supports (lambda (x) (psycop-heuristics=doubleneg-p  x)))))
    (mapcar (lambda (x) (list x NIL)) notnots)
    )
  )


;;; there is potential to combine syll-1 and syll-2
(defun psycop-heuristics=find-candidates-rips-fwd-disj-syll-1 ()
  (let ((disjunctions (psycop-heuristics~find-supports (lambda (x) (logic~disjunction-p  x)))))
    (mapcan (lambda (x)
	      (let* ((antecedents (psycop-heuristics~find-supports
				   (lambda (y)
				     (and (logic~negation-p y)
					  (equal (first (data~appl-arguments y))
						 (first (data~appl-arguments (node~formula x)))))))))
		(mapcar (lambda (y) (list x y NIL)) antecedents))
	      )
	    disjunctions)
  )
  )

(defun psycop-heuristics=find-candidates-rips-fwd-disj-syll-2 ()
  (let ((disjunctions (psycop-heuristics~find-supports (lambda (x) (logic~disjunction-p  x)))))
    (mapcan (lambda (x)
	      (let* ((antecedents (psycop-heuristics~find-supports
				   (lambda (y)
				     (and (logic~negation-p y)
					  (equal (first (data~appl-arguments y))
						 (second (data~appl-arguments (node~formula x)))))))))
		(mapcar (lambda (y) (list x y NIL)) antecedents))
	      )
	    disjunctions)
  )
  )


(defun psycop-heuristics=find-candidates-rips-fwd-disj-modus-ponens ()
  (let ((disjunctions (psycop-heuristics~find-supports
		       (lambda (x)
			 (and (logic~implication-p  x)
			      (logic~disjunction-p (first (data~appl-arguments x))))))))
    (mapcan (lambda (x)
	      (let* ((antecedents
		      (psycop-heuristics~find-supports
		       (lambda (y)
			 (or (equal y (first (data~appl-arguments
					      (first (data~appl-arguments (node~formula x))))))
			     (equal y (second (data~appl-arguments
					       (first (data~appl-arguments (node~formula x)))))))))))
		(mapcar (lambda (y) (list x y NIL)) antecedents))
	      )
	    disjunctions)


    ))

(defun psycop-heuristics=find-candidates-rips-fwd-conj-modus-ponens ()
  (let ((conjunctions (psycop-heuristics~find-supports
		       (lambda (x)
			 (and (logic~implication-p  x)
			      (logic~conjunction-p
			       (first
				(data~appl-arguments x))))))))
    (mapcan (lambda (x)
	      (let* ((antecedents1
		      (psycop-heuristics~find-supports
		       (lambda (y)
			 (equal y (first
				   (data~appl-arguments
				    (first (data~appl-arguments
					    (node~formula x))))))))))
		(mapcan (lambda (z)
			  (let ((antecedents2
				 (psycop-heuristics~find-supports
				  (lambda (y)
				    (equal y (second (data~appl-arguments
						      (first
						       (data~appl-arguments
							(node~formula x))))))))))
			    (mapcar (lambda (y) (list x z y NIL)) antecedents2)))
			
		antecedents1
		)))
	    conjunctions)


    ))


   
(defun psycop-heuristics=find-candidates-rips-fwd-disj-syll ()
  (append (psycop-heuristics=find-candidates-rips-fwd-disj-syll-1)
	  (psycop-heuristics=find-candidates-rips-fwd-disj-syll-2)))

(defun psycop-heuristics=find-candidates-rips-fwd-demorgan1 ()
  (let ((demorgan
	 (psycop-heuristics~find-supports
	  (lambda (x) (and (logic~negation-p  x)
			   (logic~conjunction-p (first (data~appl-arguments x))))))))
    (mapcar (lambda (x) (list x NIL)) demorgan)
    )
  )


(defun find-candidates-rips-fwd-demorgan2 ()
  (let ((demorgan
	 (psycop-heuristics~find-supports
	  (lambda (x) (and (logic~negation-p  x)
			   (logic~disjunction-p (first (data~appl-arguments x))))))))
    (mapcar (lambda (x) (list x NIL)) demorgan)
    )
  )

(defun find-candidates-rips-fwd-conjunctive-syllogism ()
  (let ((term1 (psycop-heuristics~find-supports
		(lambda (x) (and (logic~negation-p  x)
				 (logic~conjunction-p (first (data~appl-arguments x))))))))
    (mapcan (lambda (x)
	      (let* ((term2
		      (psycop-heuristics~find-supports
		       (lambda (y)
			 (or (equal y (first (data~appl-arguments
					      (first (data~appl-arguments (node~formula x))))))
			     (equal y (second (data~appl-arguments
					       (first (data~appl-arguments (node~formula x)))))))))))
		(mapcar (lambda (y) (list x y NIL)) term2))
	      )
	    term1)


    ))

(defun find-candidates-rips-fwd-dilemma ()
  (let ((disjunctions (psycop-heuristics~find-supports
		       (lambda (x) (logic~disjunction-p x))))) 
    (mapcan (lambda (x)
	      (let* ((terms1
		      (psycop-heuristics~find-supports
		       (lambda (y) (and (logic~implication-p y)
					(equal (first (data~appl-arguments
						       (node~formula x)))
					       (first (data~appl-arguments y))))))))
		(mapcan (lambda (y)
			  (let ((terms2 (psycop-heuristics~find-supports
				   (lambda (z)
				     (and
				      (logic~implication-p z)
				      (equal (second (data~appl-arguments
						      (node~formula x)))
					     (first (data~appl-arguments z)))
				      (equal (second (data~appl-arguments z))
					     (second (data~appl-arguments (node~formula y))))
				      )))))
			    (mapcar (lambda (z) (list x y z NIL)) terms2)))
						      
		terms1
		)))
	    disjunctions)


    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MATCHING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-candidates-rips-matching ()
  (let* ((opennodes (pds~open-nodes omega*current-proof-plan))
	 (supportnodes (pds~support-nodes omega*current-proof-plan)))
    (mapcan (lambda (x)
	      (remove-if (lambda (x) (null x))
			 (mapcar
			  (lambda (y)
			    (if (nd-rips=isomorphic-p (node~formula x)
						      (node~formula y))
				(list x y)))
			  supportnodes)))
	    opennodes)
  ))


;;;;;;;;;;;;;; FORWARD APPLICATION MECHANISM ;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-com-to-candidates (command-name candidates-list)
  (mapcar (lambda (x)
	    (comint~apply-command2arguments
	     (bb~find-command command-name) x))
	  candidates-list))



(defun apply-forward-rules ()
  ;;;; RIPS-MODUS-PONENS
  (mapcar (lambda (x)
	    (comint~apply-command2arguments
	     (bb~find-command "RIPS-MODUS-PONENS") x))
	  (psycop-heuristics=find-candidates-rips-modus-ponens))
  ;;;; RIPS-FWD-ANDE
   (apply-com-to-candidates "RIPS-ANDE"
 			   (psycop-heuristics=find-candidates-rips-fwd-ande))
  ;;;; RIPS-NOTNOTE
   (apply-com-to-candidates "RIPS-NOTNOTE"
	 		   (psycop-heuristics=find-candidates-rips-fwd-notnote))
  ;;;; RIPS-DISJ-SYLL
  ; (apply-com-to-candidates "RIPS-DISJ-SYLL"
  ;			   (psycop-heuristics=find-candidates-rips-fwd-disj-syll))
  ;;;; RIPS-DISJ-MODUS-PONENS
  ; (apply-com-to-candidates "RIPS-DISJ-MODUS-PONENS"
;			   (psycop-heuristics=find-candidates-rips-fwd-disj-modus-ponens))
  ;;;; RIPS-CONJ-MODUS-PONENS
   (apply-com-to-candidates "RIPS-CONJ-MODUS-PONENS"
 			   (psycop-heuristics=find-candidates-rips-fwd-conj-modus-ponens))
  ;;;; RIPS-DEMORGAN-1
    (apply-com-to-candidates "RIPS-DEMORGAN-1"
	    		    (psycop-heuristics=find-candidates-rips-fwd-demorgan1))
   ;;;; RIPS-DEMORGAN-1
    (apply-com-to-candidates "RIPS-DEMORGAN-2"
	 		    (find-candidates-rips-fwd-demorgan2))
    ;;;; RIPS-DISJ-SYLL
  ;; (apply-com-to-candidates "RIPS-CONJ-SYLL" (find-candidates-rips-fwd-conjunctive-syllogism)) ;; has left and right -- TODO
   ;;;; RIPS-DEMORGAN-1
    (apply-com-to-candidates "RIPS-DILEMMA"
	 		    (find-candidates-rips-fwd-dilemma))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BACKWARD APPLICATION MECHANISM ;;;;;;;;;;;;

(defun ordered-subgoals ()
  (remove-if (lambda (x) (not  (pdsn~open-node-p x))) (order-nodes-with-goaldepth)))

(defun order-nodes-with-goaldepth ()
  (let ((root (prob~proof-root omega*current-proof-plan)))
    (deepening-step (list root) (list root))))


(defun deepening-step (nodelist resultlist)
  (let ((expansion-list (mapcan (lambda (x) (pdsn~just-premises x)) nodelist)))
    (if (null expansion-list) resultlist
      (deepening-step expansion-list (append expansion-list resultlist)))))

; (defun apply-backward-until-new-subgoal ()
;   (let* ((ordered-subgoals (ordered-subgoals)))
;      ***




(defun attempt-one-backward-rule-application (ordered-subgoals)
  (if (null ordered-subgoals) NIL
    (if (equal (try-specific-subgoal (car ordered-subgoals)) 'F)
	(attempt-one-backward-rule-application (cdr ordered-subgoals))
      'T)))

(defun try-specific-subgoal (subgoal)
  (let ((success 'F))
    (if (some (lambda (x) (not (null x)))
	      (apply-com-to-candidates
	       "RIPS-BCKW-ANDI"
	       (find-candidates-rips-bckw-andi subgoal)))
	(setf success 'T))
    success
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BACKWARD APPLICATION CANDIDATES

;;RIPS-BCKW-ANDI

(defun find-candidates-rips-bckw-andi (subgoal)
  (if (logic~conjunction-p (node~formula subgoal))
      (list (list subgoal))
    NIL))

;;;;;;;;; here begins experimentation




