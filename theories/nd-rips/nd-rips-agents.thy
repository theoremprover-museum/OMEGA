;;; \omega-ants agents for theory nd-rips





(in-package :omega)

(eval-when (compile)
           (error "This file should not be compiled."))



;;; STUFF

(defun 1st-subterm (a1)
  (first (data~appl-arguments  a1)))
(defun 2nd-subterm (a1)
  (second (data~appl-arguments  a1)))


; CONJUNCTIVE MODUS PONENS
;
; IF P AND Q THEN R
; P 
; Q 
; ------------------
; R
;
; (a) If a sentence of the form  IF P AND Q THEN R holds in some domain D
; (b) and P holds in D
; (c) and Q holds in D
; (d) and R does not yet hold in D (does it suffice to look into the supports?)
; (e) then add R to D
;
;

; tested for: first 3 arguments: OK
;
;

(agent~defmatrix rips-conj-modus-ponens ;;; no c-predicates since only used in forward direction
   (agents (s-predicate (for implication)
                        (uses )
                        (definition (pred-conj-impl implication)))
	   (s-predicate (for antecedent1)
                        (uses implication)
                        (definition (pred-match1 implication antecedent1)))
	   (s-predicate (for antecedent2)
			(uses implication antecedent1)
			(definition (pred-match2 implication antecedent1 antecedent2))))
   (predicates
    (pred-conj-impl (a1)
		    (and (logic~implication-p  a1)
		    	 (logic~conjunction-p (first (data~appl-arguments  a1)))))
		    (Pred-match1 (a1 a2)
		 (or (term~alpha-equal (first
					(data~appl-arguments
					 (first (data~appl-arguments a1)))) a2)
		     (term~alpha-equal (second
					(data~appl-arguments
					 (first (data~appl-arguments a1)))) a2 ) ))
    (pred-match2 (a1 a2 a3)
		 (let* ((conj (first (data~appl-arguments a1)))
			(a (first (data~appl-arguments conj)))
			(b (second (data~appl-arguments conj))))
		   (or (and (term~alpha-equal a2 a)
			     (term~alpha-equal a3 b))
		       (and (term~alpha-equal a2 b)
			     (term~alpha-equal a3 a)))))))






; MODUS PONENS - FORWARD IF ELIMINATION
;
; IF P THEN Q
; P 
; ------------------
; Q
;
; (a) If a sentence of the form IF P THEN Q holds in some domain D,
; (b) and P holds in D,
; (c) and Q does not yet hold in D,
; (d) then add Q to D.

; MODUS PONENS - BACKWARD IF ELIMINATION
;
; IF P THEN Q
; P 
; ------------------
; Q
;
; (a) Set D to domain of current goal
; (b) Set Q to current goal
; (c) If the sentence of the form IF P THEN Q holds in some domain D,
; (d) and D does not yet contain the subgoal P,
; (e) then add P to the list of subgoals
;


; tested for first 2 arguments: ok
;
;

(agent~defmatrix rips-modus-ponens 
    (agents (s-predicate (for implication)
                         (uses )
                         (definition (pred-conj-impl implication))) ;; corresponds to (a) (forward)
            (s-predicate (for antecedent)
                         (uses implication)
                         (definition (pred-match1 implication antecedent)))
    	    (c-predicate (for succedent)
			 (uses implication)
                         (definition (pred-match2 implication succedent)))
	    (c-predicate (for succedent)                                              
                         (uses implication antecedent)                                
                         (definition (pred-match3 implication antecedent succedent))) 
	    

	    )
    (predicates
     (pred-conj-impl (a1)     
 		    (logic~implication-p a1))
     (pred-match1 (a1 a2)
 		 (term~alpha-equal (first (data~appl-arguments  a1)) a2))
     (pred-match2 (a1 a2)
 		 (term~alpha-equal (second (data~appl-arguments  a1)) a2))
     (pred-match3 (a1 a2 a3)
 		 (and (pred-match1 a1 a2)
 		      (pred-match2 a1 a3)))
    
    ))


; FORWARD DILEMMA
;
; P v Q
; IF P THEN R
; IF Q THEN R
; ------------------
; R
;
; (a) If a sentence of the form P v Q holds in some domain D,
; (b) and IF P THEN R holds in D,
; (c) and IF Q THEN R holds in D,
; (d) and R does not yet hold in D,
; (e) then add R to D.
;

;
; tested with first 3 arguments: ok
;


(Agent~defmatrix rips-dilemma 
   (agents (s-predicate (for disjunction)
                        (uses )
                        (definition (pred-disj disjunction)))
	    (s-predicate (for implication1)
                        (uses disjunction)
                         (definition (pred-match1 disjunction implication1)))
            (s-predicate (for implication2)
                         (uses disjunction implication1)
                         (definition (pred-match2 disjunction implication1 implication2)))
 	   (c-predicate (for succedent)
                         (uses disjunction implication1 implication2)
                         (definition (pred-match3 disjunction implication1 implication2 succedent)))
	   )
   (predicates
    (pred-disj (a1)     
		    (logic~disjunction-p a1))
    (pred-match1 (a1 a2)
		 (and (logic~implication-p a2)
		      (or (term~alpha-equal (1st-subterm a1) (1st-subterm a2))
			  (term~alpha-equal (2nd-subterm a1) (1st-subterm a2)))))
    (pred-match2 (a1 a2 a3) 
		 (and (logic~implication-p a3)
		      (term~alpha-equal (2nd-subterm a2) (2nd-subterm a3))             
		      (or (and  (term~alpha-equal (1st-subterm a1) (1st-subterm a2))
				(term~alpha-equal (2nd-subterm a1) (1st-subterm a3)))
			  (and  (term~alpha-equal (2nd-subterm a1) (1st-subterm a2))
			        (term~alpha-equal (1st-subterm a1) (1st-subterm a3))))))
    (pred-match3 (a1 a2 a3 a4)
		 (and  (term~alpha-equal (2nd-subterm a2) a4)
		       (term~alpha-equal (2nd-subterm a3) a4)))
    ))



; FORWARD DISJUNCTIVE-MODUS-PONENS
;
; (P v Q) => R
;  P
; ------------------
;  R 
;
; (a) If a sentence of the form (P v Q) => R holds in some domain D,
; (b) and P or Q also holds in D,
; (c) and R does not yet hold in D,
; (d) then add R to D.

; BACKWARD DISJUNCTIVE-MODUS-PONENS
;
; (P v Q) => R
;  P
; ------------------
;  R 
;
; (a) Set D to domain of current goal.
; (b) Set R to current goal
; (c) It the sentence (P v Q) => R holds in D,
; (d) and D does not yet contain the subgoal P,
; (e) then add P to the list of subgoals
; (f) If the subgoal in (e) fails,
; (g) and D does not yet contain the subgoal Q,
; (h) then add Q to the list of subgals.
;

; tested with: implication only (1st s-predicate)
;              implication and antecedent (2nd s-predicate) 
;              all three agents
; 

(Agent~defmatrix rips-disj-modus-ponens
   (agents (s-predicate (for implication)
                        (uses )
                        (definition (pred-impl-disj implication))) ;;; forward & backward direction
	    (s-predicate (for antecedent)
                        (uses implication)
                        (definition (pred-match1 implication antecedent))) ;forward direction
 	   (c-predicate (for succedent) 
                         (uses implication)
                         (definition (pred-match3 implication succedent))) ;backward & forward direction
	   )
   (predicates
    (pred-impl-disj (a1)     
		    (and (logic~implication-p a1)
			 (logic~disjunction-p (1st-subterm a1))))
    (pred-match1 (a1 a2)
		      (or (term~alpha-equal (1st-subterm (1st-subterm a1)) a2)
			  (term~alpha-equal (2nd-subterm (1st-subterm a1)) a2)))
    (pred-match3 (a1 a2)
		 (term~alpha-equal (2nd-subterm a1) a2))
    ))



; FORWARD DISJUNCTIVE SYLLOGISM
;
;  P OR Q
;  NOT Q
; ------------------
;  P 
;
; (a) If a sentence of the form P OR Q holds in some domain D,
; (b) then if NOT P holds in D and Q does not yet hold in D,
; (c) then add Q to D
; (d) else if NOT Q holdds in D and P does not yet hold in D,
; (e) then add P to D.
;
; tested in forward direction

; BACKWARD DISJUNCTIVE SYLLOGISM
;
;  P OR Q
;  NOT Q
; ------------------
;  P 
;
; (a) Set D to domain of current goal
; (b) Set Q to current goal
; (c) If a sentence of the form P OR Q or Q OR P holds in D,
; (d) and NOT P is a subformula of a sentence that holds in D,
; (e) and D does not yet contain the subgoal NOT P,              ;;;;;;; !!!!!!
; (f) then add NOT P to the list of subgoals.


(Agent~defmatrix rips-disj-syll
   (agents (s-predicate (for disjunction)
                        (uses )
                        (definition (pred-disj disjunction))) 
	    (s-predicate (for antecedent)
                        (uses disjunction)
			(exclude succedent) ;;; does not seem to work?
                        (definition (pred-match1 disjunction antecedent)))
 	     (c-predicate (for succedent) 
                         (uses disjunction)
			 (exclude antecedent)  ;;;does not seem to work???
                         (definition (pred-match3 disjunction succedent))) 
	   )
   (predicates
    (pred-disj (a1)     
	       (logic~disjunction-p a1))
    (pred-match1 (a1 a2)
		 (and (logic~negation-p a2)
		      (or (term~alpha-equal (1st-subterm a1) (1st-subterm a2))
			  (term~alpha-equal (2nd-subterm a1) (1st-subterm a2)))))
    (pred-match3 (a1 a2)
  		 (or (term~alpha-equal (1st-subterm a1) a2)
		     (term~alpha-equal (2nd-subterm a1) a2)))
    ))





; RIPS-DEMORGAN-1
;
; NOT (P AND Q)
; ------------------
; (NOT P) OR (NOT Q)
;
; (a) Set D to domain of current goal
; (b) If current goal is of the form (NOT P) OR (NOT Q),
; (c) and NOT (P AND Q) is a subformula of a sentence that holds in D,
; (d) then add NOT (P AND Q) to the list of subgoals.
;
; tested for all 2 arguments: ok.
;
;

(agent~defmatrix rips-demorgan-1
   (agents (s-predicate (for antecedent)
                        (uses )
                        (definition (pred-match1 antecedent)))
	   (s-predicate (for antecedent)
                        (uses succedent)
                        (definition (pred-match1a antecedent succedent)))
           (c-predicate (for succedent)
                        (uses antecedent)
                        (definition (pred-match1a antecedent succedent)))
	   (c-predicate (for succedent)
                        (uses )
                        (definition (pred-match2 succedent)))
	   )
   (predicates
    (pred-match1 (a1)
		 (and (logic~negation-p a1)
		      (logic~conjunction-p (1st-subterm a1))))
    (pred-match2 (a1)
		 (and (logic~disjunction-p a1)
		      (logic~negation-p (1st-subterm a1))
		      (logic~negation-p (2nd-subterm a1))))
    (pred-match1a (a1 a2)	  
		 (and (pred-match1 a1)   ;; this is a bit wasteful.
		      (pred-match2 a2)   ;; 
		      (term~alpha-equal (1st-subterm (1st-subterm a1)) (1st-subterm (1st-subterm a2)))
		      (term~alpha-equal (2nd-subterm (1st-subterm a1)) (1st-subterm (2nd-subterm a2)))))
    ))

; RIPS-DEMORGAN-2
;
; NOT (P OR Q)
; ------------------
; (NOT P) AND (NOT Q)
;
; (a) Set D to domain of current goal
; (b) If current goal is of the form (NOT P) AND (NOT Q),
; (c) and NOT (P OR Q) is a subformula of a sentence that holds in D,
; (d) then add NOT (P OR Q) to the list of subgoals.
;
;
; not tested yet.
;

(agent~defmatrix rips-demorgan-2
   (agents (s-predicate (for antecedent)
                        (uses )
                        (definition (pred-match1 antecedent)))
	   (s-predicate (for antecedent)
                        (uses succedent)
                        (definition (pred-match1a antecedent succedent)))
           (c-predicate (for succedent)
                        (uses antecedent)
                        (definition (pred-match1a antecedent succedent)))
	   (c-predicate (for succedent)
                        (uses )
                        (definition (pred-match2 succedent)))
	   )
   (predicates
    (pred-match1 (a1)
		 (and (logic~negation-p a1)
		      (logic~disjunction-p (1st-subterm a1))))
    (pred-match2 (a1)
		 (and (logic~conjunction-p a1)
		      (logic~negation-p (1st-subterm a1))
		      (logic~negation-p (2nd-subterm a1))))
    (pred-match1a (a1 a2)	  
		 (and (pred-match1 a1)   ;; this is a bit wasteful.
		      (pred-match2 a2)   ;; 
		      (term~alpha-equal (1st-subterm (1st-subterm a1)) (1st-subterm (1st-subterm a2)))
		      (term~alpha-equal (2nd-subterm (1st-subterm a1)) (1st-subterm (2nd-subterm a2)))))
    ))


; RIPS-ANDE (FORWARD)
;
; P AND Q
; ------------------
; P
;
; P AND Q
;
; ------------------
; Q
;
; (a) If a sentence of the form P AND Q holds in some domain D,
; (b) then if P does not yet hold in D,
; (c) then add P to D,
; (d) and if Q does not yet hold in D,
; (e) then add Q to D
;
;
; tested in forward direction.
;
;
; RIPS-ANDE (BACKWARD)
;
; BIG TODO: This doesn't yet correspond to Rips, even not the tactic.
;
; P AND Q
; ------------------
; P
;
; Q AND P
;
; ------------------
; P
;
; (a) Set D to domain of current goal,
; (b) Set P to current goal
; (c) If the sentence P AND Q is a subformula of a sentence that holds in D,
; (d) and D does not yet contain the subgoal P AND Q, 
; (e) then add P AND Q to the list of subgoals.
; (f) If the subgoal in (e) fails,
; (g) and the sentence Q AND P is a subformula of a sentence that holds in D,
; (h) and D does not yet contain the subgoal Q AND P,
; (i) then add Q AND P to the list of subgoals.
;

(agent~defmatrix rips-ande
   (agents (s-predicate (for conjunction)
                        (uses )
			(exclude conjunct1 conjunct2)
                        (definition (pred-conj conjunction)))
	  
	   )
   (predicates
    (pred-conj (a1)
		 (logic~conjunction-p a1))
		   
    ))


;
; RIPS-NOTNOTE (FORWARD)
;
; NOT NOT P
; ------------------
; P 
;
; (a) If a sentence of the form NOT NOT P holds in some domain D,
; (b) and P does not yet hold in D,
; (c) then add P to D.

;
; RIPS-NOTNOTE (BACKWARD)
;
; NOT NOT P
; ------------------
; P 
;
; (a) Set D to domain of current goal
; (b) Set P to current goal.
; (c) If the sentence NOT NOT P is a subformula of a sentence that holds in D,
; (d) and D does not yet contain the subgoal NOT NOT P,
; (e) then add NOT NOT P to the list of subgoals.
;
;

(agent~defmatrix rips-notnote
   (agents (s-predicate (for negnegterm)
                        (uses )
			(exclude term)
                        (definition (pred-negneg negnegterm)))
	   (c-predicate (for term)
                        (uses )
			(exclude negnegterm)
                        (definition (pred-term term)))
	  
	   )
   (predicates
    (pred-negneg (a1)
		 (and (logic~negation-p a1)
		      (logic~negation-p (1st-subterm a1))
		      ))
    (pred-term (a1)
		 T)
		   
    ))

; RIPS-IMPI (BACKWARD)
;
; [P]
; .
; .
; .
; Q
; ------------------
; P => Q 
;
; (a) Set D to domain of current goal
; (b) If the current goal is of the form  P => Q,
; (c) and neither D nor ist superdomains nor its immediate subdomains contain suppostitions P and subgoal Q,
; (d) and IR P THEN Q is a subformula of the premises or conclusion,  ;;; TODO !
; (e) then set up a subdomain of D, D', with supposition P.
; (f) add the subgoal of proving Q in D' to the list of subgoals.
;
; tested

(agent~defmatrix rips-impi
   (agents
    (c-predicate (for succedent)
		 (uses )
		 (definition (pred-impl succedent)))
	  
	   )
   (predicates
    (pred-impl (a1)
		 (logic~implication-p a1))
		   
    ))

; RIPS-ANDI
;
; P
; Q
; ------------------
; P AND Q
;
; (a) Set D to domain of current goal
; (b) If current goal is of the form P AND Q,
; (c) and D does not yet contain the subgoal P, 
; (d) then add the subgoal of proving P in D to the list of subgoals.
; (e) If the subgoal in (d) succeeds,
; (f) and D does not yet contain subgoal Q,
; (g) then add the subgoal of proving Q in D to the list of subgoals.
;
;
; tested with all three arguments



(agent~defmatrix rips-andi
   (agents (s-predicate (for conjunct1) ; corresponds to (d)
                        (uses conjunction)
                        (definition (pred-match1 conjunction conjunct1)))
	   (s-predicate (for conjunct2) ; corresponds to (g)
                        (uses conjunction)
                        (definition (pred-match2 conjunction conjunct2)))
           (c-predicate (for conjunction)
                        (uses )
                        (definition (logic~conjunction-p conjunction)))  ; corresponds to (e)
	   
	   )
   (predicates
    (pred-match1 (a1 a2)
		 (term~alpha-equal (1st-subterm a1) a2))
		; (and (term~alpha-equal (1st-subterm a1) a2)
		;      (notany #'(lambda (x) (term~alpha-equal a2
		;				       (node~formula x)))
		;		       (pds~node-supports (:node a1)))))
		;      ;(not (some #'(lambda (hyp-node)
		;		     (term~alpha-equal (node~formula hyp-node)
			;			       a2 ))
				; (pds~node-supports (:node a1))))
		     ; ))
    (pred-match2 (a1 a2)
		 (term~alpha-equal (2nd-subterm a1) a2))
    
    ))		  
    

;;; NOTE - TODO!

;;; NOTI - TODO!



					
;
; RIPS-ORE
; 
; P v Q          
; [P] ---> R      
; [Q] ---> R      
; ----------------
;  R         
;
; (a) Set D to domain of current goal
; (b) Set R to current goal.
; (c) If a sentence of the form P OR Q holds in D,
; (d) and both P and Q are subformulas or negations of subformulas of the premises or conclusion,
; (e) and R is a subformula or negation of a subformula of the premises or conclusion,
; (f) and neither D nor its superdomains  nor its immediate subdomains contain supposition P and subgoal R,
; (g) and neither D nor its superdomains  nor its immediate subdomains contain supposition P and subgoal R,
; (h) then set up a subdomain of D, D', with supposition Q and subgoal R,
; (i) and add the subgoal of proving R in D' to the list of subgoals.
; (j) If the subgoal in (i) succeeds,
; (k) the set up another subdomain of D, D'', with supposition Q,
; (l) and add the subgoal of proving R in D'' to the list of subgoals.
;
; tested for two arguments
;

(agent~defmatrix rips-ore
   (agents (s-predicate (for disjunction) ;corresponds to (c)
                        (uses )
                        (definition (logic~disjunction-p disjunction)))
           (c-predicate (for conclusion)
                        (uses )
                        (definition (pred-conj conjunction)))  
	   
	   )
   (predicates
    (pred-conj (a1)
	       T)
    
    ))


;;;;;;;;;;;;;; backward if elimination ;;; TODO

(agent~defmatrix rips-bckw-if-elim
   (agents (s-predicate (for implication) ;corresponds to (c)
                        (uses )
                        (definition (logic~implication-p implication)))
           (c-predicate (for succedent)
                        (uses implication)
                        (definition (pred-bckw implication succedent)))  
	   
	   )
   (predicates
    (pred-bckw (implication succedent)
	       (format t "pred-bckw")
	       (nd-rips=backward-IF-elim-condition succedent implication))
    
    ))


(agent~defmatrix rips-bckw-if-intro
   (agents (c-predicate (for implication) 
                        (uses )
                        (definition (logic~implication-p implication)))
	   )
   (predicates
    )
    )


(agent~defmatrix rips-bckw-andi
   (agents (c-predicate (for conjunction) 
                        (uses )
                        (definition (logic~conjunction-p conjunction)))
	   )
   (predicates
    )
    )


(agent~defmatrix rips-matching
   (agents
           (s-predicate (for assertion) 
                        (uses )
                        (definition 'T))
           (c-predicate (for subgoal) 
                        (uses assertion)
                        (definition (nd-rips=isomorphic-p conjunction assertion)))
	   )
   (predicates
    )
    )



   (th~defagentdefault nd-rips
  	      (csm~set-considered-commands '(rips-conj-modus-ponens rips-modus-ponens rips-dilemma rips-demorgan-1 rips-demorgan-2 rips-andi rips-disj-modus-ponens rips-disj-syll rips-ande rips-notnote rips-impi rips-ore weaken rips-bckw-if-elim  rips-bckw-if-intro rips-bckw-andi rips-matching defn-expand))
  		    )


  (th~defagentdefault relation
   	      (csm~set-considered-commands '(rips-conj-modus-ponens rips-modus-ponens rips-dilemma rips-demorgan-1 rips-demorgan-2 rips-andi rips-disj-modus-ponens rips-disj-syll rips-ande rips-notnote rips-impi rips-ore weaken rips-bckw-if-elim))
  	 	    )



;; (defmethod bb~command-suggestion-leq-p
;;   ((sugg1 bb+command-suggestion) (sugg2 bb+command-suggestion) (indicator (eql :i-e-f)))
;;   (let ((com1 (mapcar #'keim~name (com~categories (bb~entry-command sugg1))))
;; 	(com2 (mapcar #'keim~name (com~categories (bb~entry-command sugg2)))))
;;     (flet ((in (cat list)
;; 	       (find cat list :test #'string-equal)))
;;       (or (in :elimination com2)
;; 	  (and (in :introduction com2) (not (in :elimination com1)))
;; 	  (and (in :false com2) (not (or (in :elimination com1)
;; 					 (in :introduction com1))))
;; 	  ))))


;; (defun pds~not-free-in-nodes-or-hyps-p (term &rest lines)
;;   (declare (edited  "23-SEP-1996" "09-FEB-1993 12:46")
;; 	   (authors Lassaad nesmith)
;; 	   (input   "A TERM and  as &rest argument a list of nodes.")
;; 	   (effect  "None.")
;; 	   (value   "T if TERM is not free in (occurs un-lambda-bound in)
;; LINES' formulas nor free in any of their hypotheses, otherwise nil."))
;;   (let ((hyps (delete-duplicates
;; 	       (mapcan #'copy-list (mapcar #'pdsn~hyps lines)))))
;;     (dolist (line 
;; 	     (remove-duplicates (append lines hyps)) 
;; 	     t)
;;       (let ((unbound-symbols
;; 	     (data~all-unbound-symbols (node~formula line))))
;; 	(if (find term unbound-symbols :test #'data~equal-p)
;; 	    (return-from pds~not-free-in-nodes-or-hyps-p nil))))))



;;; TODOS: rips backward ande 
