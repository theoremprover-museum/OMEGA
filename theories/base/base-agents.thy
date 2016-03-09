;;; Problem 1: Multiple Rueckgabe von Werten; z.B. func0 eql line berechnet neue alle positionen
;;;            an denen eql in line anwendbar ist; z.Zt. wird aber nur 1. gefundene
;;;            Position gebunden
;;; Problem 2: Ausfiltern von nicht konsistenten Vorschlaegen z.B. bei =trans <eql1 eql2 eql3>

(in-package :omega)


(eval-when (compile)
           (error "This file should not be compiled."))







(agent~defmatrix andi
  ; (information :pl1)
   (agents (c-predicate (for Conjunction)
                        (uses )
                        (definition (logic~conjunction-p Conjunction)))
           (c-predicate (for Conjunction)
                        (uses LConj)
                        (definition (pred1 Conjunction LConj)))
           (c-predicate (for Conjunction)
                        (uses RConj)
                        (definition (pred2 Conjunction RConj)))
           (c-predicate (for Conjunction)
                        (uses LConj RConj)
                        (definition (and (pred1 Conjunction LConj)
                                         (pred2 Conjunction RConj))))
           (s-predicate (for RConj)
                        (uses Conjunction)
                        (definition (pred2 Conjunction RConj)))
	   (s-predicate (for LConj)
                        (uses Conjunction)
                        (definition (pred1 Conjunction LConj))
                        (help "Predicate for the left conjunct of a given conjunction.")))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1) 
                (data~equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (second (data~appl-arguments a1)))))))

;; MP: This is my version of wild-agents. c-pred-list/s-pred-list will instantiate the argument
;; with all open/support lines. The predicate-function is supposed to work as a filter on the
;; nodes, basically it is more a function than a predicate. 
;; (Until now it is only possible to give nodes to the predicate-function.)

(defun baag=split-conjuncts (conj conj-list)
  (and (logic~conjunction-p conj)
       (let ((conjuncts (batac=split-on-and conj)))
	 (remove-if-not #'(lambda (x) (member (node~formula x) conjuncts :test #'data~equal))
			conj-list))))

(agent~defmatrix andi* 
   (agents (c-predicate (for Conjunction)
	        	(uses)
    			(definition (logic~conjunction-p Conjunction)))
           (s-pred-list (for Conjunct-List)
                        (uses Conjunction)
                        (definition (baag=split-conjuncts conjunction (:node Conjunct-list))))))

(agent~defmatrix ande* 
   (agents (c-pred-list (for Conjunct-List)
			(uses Conjunction)
                        (definition (baag=split-conjuncts Conjunction (:node Conjunct-List))))
           (s-predicate (for Conjunction)
                        (uses)
                        (definition (logic~conjunction-p Conjunction)))))

#|
(agent~defmatrix andi* ;; under construction
   (agents (c-predicate (for Conjunction)
	        	(uses)
    			(definition (logic~conjunction-p Conjunction)))
           (c-predicate (for Conjunction)
                        (uses Conjunct-List)
                        (definition (pred1 Conjunction Conjunct-List)))
           (s-pred-list (for Conjunct-List)
                        (uses Conjunction)
                        (definition (pred1 Conjunction Conjunct-List)))
   )
   (predicates
    (pred1 (conj conj-list)
           (and (logic~conjunction-p conj)
                (batac=formula-list-subsetp conj-list (batac=split-on-and conj))))
   )
)
			     
                             

(agent~defmatrix ande* ;; under construction
   (agents (c-pred-list (for Conjunct-List)
			(uses Conjunction)
                        (definition (pred1 Conjunction Conjunct-List)))
           (s-predicate (for Conjunction)
                        (uses)
                        (definition (logic~conjunction-p Conjunction)))
           (s-predicate (for Conjunction)
                        (uses Conjunct-List)
                        (definition (pred1 Conjunction Conjunct-List)))
   )
   (predicates
     (pred1 (conj conj-list)
	    (and (logic~conjunction-p conj)
		 (batac=formula-list-subsetp (batac=split-on-and conj) conj-list))) 
   )
)
|#

(agent~defmatrix weaken 
   (agents (c-predicate (for lowerline)
                        (uses upperline)
                        (definition (pred1 upperline lowerline)))
           (c-predicate (for lowerline)
			(exclude upperline)
                        (definition (pred2 lowerline)))
           (s-predicate (for upperline)
                        (uses lowerline)
                        (definition  (pred1 upperline lowerline)))
	   (s-predicate (for upperline)
			(exclude lowerline)
                        (definition (pred3 upperline))))
   (predicates
    (pred1 (a1 a2)
	   (term~alpha-equal a1 a2))
    (pred2 (line1)
	   (find-if #'(lambda (x) (pred1 line1 (node~formula x)))
		    (pds~node-supports (:node line1))))
    (pred3 (line2)
	   (find-if #'(lambda (x) (pred1 (node~formula x) line2))
		    (list (agplan~current-open-node))))))


(agent~defmatrix orir 
   (agents (c-predicate (for disjunction)
                        (uses )
                        (definition (logic~disjunction-p disjunction)))
           (c-predicate (for disjunction)
                        (uses rdisj)
                        (definition (pred1 disjunction rdisj)))
           (c-predicate (for disjunction)
                        (uses rdisj par)
                        (definition (pred2 disjunction rdisj (:param par))))
           (s-predicate (for rdisj)
                        (uses disjunction)
                        (definition (pred1 disjunction rdisj)))
	   (function    (for par)
                        (uses disjunction)
                        (definition (first (data~appl-arguments disjunction)))))
   (predicates
    (pred1 (dis rdis) (and (logic~disjunction-p dis)
			   (data~equal (second (data~appl-arguments dis)) rdis)))
    (pred2 (dis rdis par) (and (logic~disjunction-p disjunction)
			       (data~equal (second (data~appl-arguments dis)) rdis)
			       (data~equal (first (data~appl-arguments dis)) par)))))


(agent~defmatrix oril 
   (agents (c-predicate (for disjunction)
                        (uses )			
                        (definition (logic~disjunction-p disjunction)))
           (c-predicate (for disjunction)
                        (uses ldisj)
                        (definition (pred1 disjunction ldisj)))
           (c-predicate (for disjunction)
                        (uses ldisj par)
                        (definition (pred2 disjunction ldisj (:param par))))
           (s-predicate (for ldisj)
                        (uses disjunction)
                        (definition (pred1 disjunction ldisj)))
	   (function    (for par)
                        (uses disjunction)
                        (definition (second (data~appl-arguments disjunction)))))
   (predicates
    (pred1 (dis ldis) (and (logic~disjunction-p dis)
			   (data~equal (first (data~appl-arguments dis)) ldis)))
    (pred2 (dis ldis par) (and (logic~disjunction-p ldis)
			       (data~equal (first (data~appl-arguments dis)) ldis)
			       (data~equal (second (data~appl-arguments dis)) par)))))


(agent~defmatrix ore 
   (agents (s-predicate (for disjunction)
                        (uses )
                        (definition (logic~disjunction-p disjunction)))
	   (c-predicate (for goal)
           		(uses )
           		(definition (pds~find-node-support (:node goal) #'logic~disjunction-p))))
)


(agent~defmatrix andel 
   (agents (s-predicate (for conjunction)
                        (uses )
                        (definition (logic~conjunction-p conjunction)))
           (s-predicate (for conjunction)
                        (uses lconj)
                        (definition (pred1 conjunction lconj)))
	   (c-predicate (for lconj)
                        (uses conjunction)
                        (definition (pred1 conjunction lconj))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))))


(agent~defmatrix ander 	
   (agents (s-predicate (for conjunction)
                        (uses )
			(exclude rconj)
                        (definition (logic~conjunction-p conjunction)))
           (s-predicate (for conjunction)
                        (uses rconj)
                        (definition (pred1 conjunction rconj)))
	   (c-predicate (for rconj)
                        (uses conjunction)
                        (definition (pred1 conjunction rconj))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (second (data~appl-arguments a1)))))))




(agent~defmatrix impe 	
   (agents (s-predicate (for implication)
                        (uses )
                        (definition (logic~implication-p implication)))
           (s-predicate (for implication)
                        (uses antecedent)
                        (definition (pred1 implication antecedent)))
           (s-predicate (for implication)
                        (uses succedent)
                        (definition (pred2 implication succedent)))
           (s-predicate (for implication)
                        (uses antecedent succedent)
                        (definition (and (pred1 implication antecedent)
                                         (pred2 implication succedent))))
           (c-predicate (for succedent)
                        (uses implication)
                        (definition (pred2 implication succedent)))
	   (s-predicate (for antecedent)
                        (uses implication)
                        (definition (pred1 implication antecedent))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~implication-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
	   (and (logic~implication-p a1)
                (data~equal a2 (second (data~appl-arguments a1)))))))


(agent~defmatrix noti 	
   (agents (s-predicate (for falsity)
                        (uses )
                        (definition (logic~falsity-constant-p falsity)))
           (s-predicate (for falsity)   
                        (uses negation)
                        (definition (pds~find-node-support (:node falsity)
							   #'(lambda (x) (data~equal x (first (data~appl-arguments negation))))))) 
	   (c-predicate (for negation)
                        (uses )
                        (definition (logic~negation-p negation)))))



(agent~defmatrix note 	
   (agents (s-predicate (for negation)
                        (uses )
			(exclude line)
                        (definition (logic~negation-p negation)))
	   (s-predicate (for negation)
                        (uses line)
                        (definition (pred1 negation line)))
	   (c-predicate (for falsehood)
			(uses )
			(definition (logic~falsity-constant-p falsehood)))
	   (s-predicate (for line)
                        (uses negation)
                        (definition (pred1 negation line))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~negation-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))))


(agent~defmatrix falsee 	
   (agents (s-predicate (for falsehood)
			(uses )
			(definition (logic~falsity-constant-p falsehood)))))


(defun o=matching-term (term1 term2)
  (multiple-value-bind (ok subst)
      (term~alpha-match term1 term2)
    (when (and ok (= 1 (length (subst~domain subst))))
      (car (subst~codomain subst)))))


(agent~defmatrix existsi 	
   (agents (c-predicate (for exists-line)
                        (uses )
                        (definition (logic~existential-quantification-p exists-line)))
           (c-predicate (for exists-line)
                        (uses prem)
                        (definition (pred1 exists-line prem)))
	   (c-predicate (for exists-line)
                        (uses prem witness)
                        (definition (pred2 exists-line prem (:param witness))))
	   (c-predicate (for exists-line)
                        (uses prem witness positionlist)
                        (definition (pred3 exists-line prem (:param witness) (:param positionlist))))
	   (s-predicate (for prem witness)
                        (uses exists-line)
                        (definition (pred1 exists-line prem)))
	   (s-predicate (for prem)                     
                        (uses exists-line witness)
                        (definition (pred2 exists-line prem (:param witness))))
	   (s-predicate (for prem)
                        (uses exists-line witness positionlist)
                        (definition (pred3 exists-line prem (:param witness) (:param positionlist))))
	   (function (for witness)
		     (uses exists-line)
		     (definition (func2 exists-line)))   ;; beliebige neue Konstante berechnen
	   (function (for witness)
		     (uses exists-line prem)
		     (definition (func3 exists-line prem)))   ;; exists-var liefert positionen, dort
						              ;; steht term
	   (function (for positionlist)
		     (uses exists-line)
		     (definition (func4 exists-line)))        ;; exists-var liefert positionen
	   (function (for positionlist)
		     (uses prem witness)
		     (definition (func5 prem (:param witness))))  ;; suche positionen von witness in prem
   )
   (predicates
    (pred1 (a1 a2)
	   (and
	    (logic~existential-quantification-p a1)
	    (let ((matcher (term~alpha-match (logic~quantification-scope a1) a2)))
	      (when matcher (list (car (subst~codomain matcher)))))))
    (pred2 (a1 a2 a3)
	   (and (logic~existential-quantification-p a1)
		(data~equal (subst~apply (subst~create (list (logic~quantification-bound-variable
							      a1))
						       (list a3))
					 (logic~quantification-scope a1))
			    a2)))
    (pred3 (a1 a2 a3 a4)
	   (and
	    (pred2 a1 a2 a3)
	    (every #'(lambda (pos)
		       (data~equal (data~struct-at-position a2 pos) a3))
		   a4)))
    (func2 (a1)
	   (term~generate-term-primitive-with-new-name 'a (term~type (logic~quantification-bound-variable
								a1))
						 'term+constant
						 (pds~environment (agplan~current-proof-plan))))
    (func3 (a1 a2)
	   (when (logic~existential-quantification-p a1)
	     (agplan~matching-term (logic~quantification-scope a1) a2)))
    (func4 (a1)
	   (data~positions (logic~quantification-scope a1)
			   #'(lambda (x) (data~equal x (logic~quantification-bound-variable
							a1)))))
    (func5 (a1 a2)
	   (data~positions a1 #'(lambda (x) (data~equal x a2))))))
	   

		      

(agent~defmatrix existse 	
   (agents (s-predicate (for exline)
                        (uses )
                        (definition (logic~existential-quantification-p exline))) 
           (s-predicate (for exline)
                        (uses subgoal)
                        (definition (pred1 exline subgoal))) 
	   (s-predicate (for exline)
                        (uses conc)
                        (definition (pred1 exline conc))) 
	   (c-predicate (for conc)
                        (uses )
                        (definition (pred2 conc))) 
	   (c-predicate (for conc)
                        (uses exline)
                        (definition (pred1 exline conc))) 
	   (c-predicate (for conc)
                         (uses subgoal)
                        (definition  (pred3 subgoal conc))) 
           (s-predicate (for subgoal)
                        (uses conc)
                        (definition (pred3 subgoal conc))) 
           (s-predicate (for subgoal)
                        (uses exline)
                        (definition (pred1 exline subgoal))) 
	   (function (for parameter)
		     (uses exline)
		     (definition (func0 exline))) 
	   (function (for parameter)
		     (uses exline conc)
		     (definition (func1 exline conc)))  ;;suche matchbarer exlinescope in
						 ;;supports schlage ergebnis matching vor
	   
	   (function (for parameter)
		     (uses exline subgoal)
		     (definition (func1 exline subgoal))) ;;suche matchbarer exlinescope in
						 ;;supports schlage ergebnis matching vor
	   
   )
   (predicates
    (pred1 (exline subgoal)
	   (and
	    (logic~existential-quantification-p exline)
	    (pds~find-node-support (:node subgoal) #'(lambda (x)
						       (term~alpha-match
							(logic~quantification-scope exline)
							x)))))
    (pred2 (conc)
	   (pds~find-node-support  (:node conc) #'(lambda (x)
						    (logic~existential-quantification-p x))))

    (pred3 (subgoal conc)
	   (and (not (equal (:node conc) (:node subgoal)))
                (data~equal conc subgoal)))

    (func0 (exline) (when (logic~existential-quantification-p exline)
		      (term~generate-term-primitive-with-new-name 
		       'a (term~type (logic~quantification-bound-variable exline))
		       'term+constant (pds~environment (agplan~current-proof-plan)))))
    (func1 (exline goal)
	   (when (logic~existential-quantification-p exline)
	     (agplan~matching-term
	      (logic~quantification-scope exline)
	      (pds~node-formula (pds~find-node-support (:node goal) #'(lambda (x)
								      (and (term~p x)
						                           (agplan~matching-term (logic~quantification-scope exline) x))
								      ))))
	   )
    )  ;schwachsinn muss spaeter weg   
   )
)   


(agent~defmatrix foralle 
   (agents (s-predicate (for univ-line)
                        (uses )
                        (definition (logic~universal-quantification-p univ-line)))
           (s-predicate (for univ-line)
                        (uses line)
                        (definition (pred1 univ-line line)))
           (s-predicate (for univ-line)
                        (uses line term)
                        (definition (pred2 univ-line line (:param term))))
           (c-predicate (for line)
                        (uses univ-line)
                        (definition (pred1 univ-line line)))
	   (c-predicate (for line)
                        (uses univ-line term)		
                        (definition (pred2 univ-line line (:param term))))
	   (function    (for term)
                        (uses line)	
                        (definition (func1 line))) ;; erste Konstante suchen
	   (function    (for term)
                        (uses univ-line)	
                        (definition (func2 univ-line))) ;; neue Konstante vorschlagen
	   (function    (for term)
                        (uses univ-line line)	
                        (definition (func3 univ-line line)))) ;; exakte Konstante berechnen
   (predicates
    (pred1 (univ-line line)
	   (and
	    (logic~universal-quantification-p univ-line)
	    (term~alpha-match (logic~quantification-scope univ-line) line)))
    
    (pred2 (univ-line line term)
	   (and
	    (logic~universal-quantification-p univ-line)
	    (agplan~matching-term (logic~quantification-scope univ-line) line)
	    (data~equal (agplan~matching-term (logic~quantification-scope univ-line) line)
			term)))
    (func1 (line)
	   (data~struct-at-position
	    line
	    (car (data~positions line #'term~constant-p))))
    (func2 (univ-line)
	   (and
	    (logic~universal-quantification-p univ-line)
	    (term~generate-term-primitive-with-new-name 'a (term~type (logic~quantification-bound-variable
								 univ-line))
						  'term+constant
						  (pds~environment (agplan~current-proof-plan)))))
    (func3 (univ-line line)
	   (when (pred1 univ-line line)
	     (agplan~matching-term (logic~quantification-scope univ-line) line)))))


(agent~defmatrix lambda
   (agents (c-predicate (for line1)
                        (uses )
			(level 1)		
                        (definition (data~positions line1 #'data~abstr-p)))
           (c-predicate (for line1)
                        (uses line2)
			(level 1)		
                        (definition (lam~equal-p line1 line2)))
	   (s-predicate (for line2)
                        (uses )
			(level 1)		
                        (definition (data~positions line2 #'data~abstr-p)))
           (s-predicate (for line2)
                        (uses line1)
			(level 1)		
                        (definition (lam~equal-p line1 line2)))))


(agent~defmatrix exte
   (agents (c-predicate (for lowerline)
                        (uses )
			(level 1)
                        (definition (and (logic~equality-p lowerline)
					 (pred1 lowerline))))
           (c-predicate (for lowerline)
                        (uses upperline)
			(level 2)
                        (definition (agplan~ext-check upperline lowerline)))
	   (s-predicate (for upperline)
                        (uses )
			(level 1)
                        (definition (logic~universal-quantification-p upperline)))
           (s-predicate (for upperline)
                        (uses lowerline)
			(level 2)
                        (definition (agplan~ext-check upperline lowerline))))
   (predicates
    (pred1 (line)
	   (type~func-p (term~type (car (data~appl-arguments line)))))))
				       
	   

(agent~defmatrix exti
   (agents (c-predicate (for lowerline)
                        (uses )
			(level 1)
                        (definition (logic~existential-quantification-p lowerline)))
           (c-predicate (for lowerline)
                        (uses upperline)
			(level 2)
                        (definition (agplan~ext-check upperline lowerline)))
	   (s-predicate (for upperline)
                        (uses )
			(level 1)
                        (definition (and (logic~equality-p upperline)
					 (type~complex-p (data~annotation (car
									   (data~appl-arguments upperline)))))))
           (s-predicate (for upperline)
                        (uses lowerline)
			(level 2)
                        (definition (agplan~ext-check upperline lowerline)))))



(agent~defmatrix exti-neg
   (agents (c-predicate (for lowerline)
                        (uses )
			(level 1)
                        (definition (pred1 lowerline)))
           (c-predicate (for lowerline)
                        (uses upperline)
			(level 2)
                        (definition (and (pred1 lowerline) (pred2 upperline)
					 (agplan~ext-check (car (data~appl-arguments lowerline))
							   (car (data~appl-arguments
								 upperline))))))
	   (s-predicate (for upperline)
                        (uses )
			(level 1)
                        (definition (pred2 upperline)))
           (s-predicate (for upperline)
                        (uses lowerline)
			(level 2)
                        (definition (and (pred1 lowerline) (pred2 upperline)
					 (agplan~ext-check (logic~quantification-scope
							    lowerline)
							   (car (data~appl-arguments
								 upperline)))))))
   (predicates
    (pred1 (lowerline)
	   (and (logic~negation-p lowerline)
		(logic~universal-quantification-p
		 (car (data~appl-arguments lowerline)))))
    (pred2 (upperline)
	   (and (logic~negation-p upperline)
		(logic~equality-p
		 (car (data~appl-arguments upperline)))))))



(agent~defmatrix ande 	
   (agents (s-predicate (for Conjunction)
                        (uses )
                        (definition (logic~conjunction-p Conjunction)))
           (s-predicate (for Conjunction)
                        (uses LConj)
                        (definition (pred1 Conjunction LConj)))
           (s-predicate (for Conjunction)
                        (uses RConj)
                        (definition (pred2 Conjunction RConj)))
           (s-predicate (for Conjunction)
                        (uses LConj RConj)
                        (definition (and (pred1 Conjunction LConj)
                                         (pred2 Conjunction RConj))))
           (c-predicate (for RConj)
                        (uses Conjunction)
                        (definition (pred2 Conjunction RConj)))
	   (c-predicate (for LConj)
                        (uses Conjunction)
                        (definition (pred1 Conjunction LConj))
                        (help "Predicate for the left conjunct of a given conjunction.")))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (second (data~appl-arguments a1)))))))


(agent~defmatrix notnote 	
   (agents (s-predicate (for neg)
                        (uses )
                        (definition (batac=double-negation-p neg)))
           (s-predicate (for neg)
                        (uses pos)
                        (definition (data~equal (batac=introduce-double-negation pos) neg)))
           (c-predicate (for pos)
                        (uses neg)
                        (definition (data~equal (batac=introduce-double-negation pos) neg)))))


(agent~defmatrix notnoti 	
   (agents (c-predicate (for neg)
                        (uses )
                        (definition (batac=double-negation-p neg)))
           (c-predicate (for neg)
                        (uses pos)
                        (definition (data~equal (batac=introduce-double-negation pos) neg)))
           (s-predicate (for pos)
                        (uses neg)
                        (definition (data~equal (batac=introduce-double-negation pos) neg)))))



(agent~defmatrix pushneg 	
   (agents (s-predicate (for neg)
                        (uses )
                        (definition (pred1 neg)))
           (s-predicate (for neg)
                        (uses pos)
                        (definition (and (pred1 neg)             
					 (data~equal (:param pos) (pds~pushneg neg)))))
           (c-predicate (for pos)
                        (uses neg)
                        (definition (and (pred1 neg)
					 (data~equal (:param pos) (pds~pushneg neg))))))
   (predicates
    (pred1 (neg) (and (logic~negation-p neg)
		      (not (logic~atom-p (car (data~appl-arguments neg))))))))
	   


(agent~defmatrix pullneg 	
   (agents (c-predicate (for neg)
                        (uses )
                        (definition (pred1 neg)))
           (c-predicate (for neg)
                        (uses pos)
                        (definition (and (pred1 neg)
					 (data~equal (:param pos) (pds~pushneg neg)))))
           (s-predicate (for pos)
                        (uses neg)
                        (definition (and (pred1 neg)
					 (data~equal (:param pos) (pds~pushneg neg))))))
   (predicates
    (pred1 (neg) (and (logic~negation-p neg)
		      (not (logic~atom-p (car (data~appl-arguments neg))))))))


(agent~defmatrix ormp ; 	
   (agents (s-predicate (for disj)
                        (uses )
                        (definition (pred1 disj)))
           (s-predicate (for disj)
                        (uses conc)
			(level 1) ;
                        (definition (pred2 disj conc)))
	   (s-predicate (for disj)
                        (uses conc neg)
			(level 1) 
                        (definition (pred3 disj conc neg)))
	   (s-predicate (for neg)
                        (uses )
                        (definition (pred4 neg)))
           (s-predicate (for neg)
                        (uses disj)
			(level 1)
                        (definition (pred5 disj neg)))
	   (s-predicate (for neg)
                        (uses disj conc)
			(level 1)
                        (definition (pred3 disj conc neg)))
	   (c-predicate (for conc)
                        (uses disj)
			(level 1)
                        (definition (pred2 disj conc)))
           (c-predicate (for conc)
                        (uses disj neg)
			(level 1)
                        (definition (pred3 disj conc neg))))
   (predicates
    (pred1 (disj) (logic~disjunction-p disj))
    (pred2 (disj conc) (and (pred1 disj)
			    (data~equal (cadr (data~appl-arguments disj))
					conc)))
    (pred3 (disj conc neg) (and (pred2 disj conc)
				(pred4 neg)
				(data~equal (car (data~appl-arguments disj))
					    (car (data~appl-arguments neg)))))
    (pred4 (neg) (logic~negation-p neg))
    (pred5 (disj neg) (and (pred1 disj)
			   (pred4 neg)
			   (data~equal (car (data~appl-arguments disj))
				       (car (data~appl-arguments neg)))))))    
    
		       

(agent~defmatrix equivel 	
   (agents (s-predicate (for equiv)
                        (uses )
                        (definition (pred1 equiv)))
           (s-predicate (for equiv)
                        (uses limp)
                        (definition (pred2 equiv limp)))
           (c-predicate (for limp)
                        (uses )
                        (definition (pred3 limp)))
	   (c-predicate (for limp)
                        (uses equiv)
                        (definition (pred2 equiv limp))))
   (predicates
    (pred1 (equiv) (logic~equivalence-p equiv))
    (pred2 (equiv limp) (and (pred1 equiv)
			     (pred3 limp)
			     (data~equal (batac=equivel-f-create equiv) limp)))
    (pred3 (limp) (logic~implication-p limp))))
    


(agent~defmatrix equiver 	
   (agents (s-predicate (for equiv)
                        (uses )
                        (definition (pred1 equiv)))
           (s-predicate (for equiv)
                        (uses rimp)
                        (definition (pred2 equiv rimp)))
           (c-predicate (for rimp)
                        (uses )
                        (definition (pred3 rimp)))
	   (c-predicate (for rimp)
                        (uses equiv)
                        (definition (pred2 equiv rimp))))
   (predicates
    (pred1 (equiv) (logic~equivalence-p equiv))
    (pred2 (equiv rimp) (and (pred1 equiv)
			     (pred3 rimp)
			     (data~equal (batac=equiver-f-create equiv) rimp)))
    (pred3 (rimp) (logic~implication-p rimp))))



(agent~defmatrix equive 	
   (agents (s-predicate (for equiv)
                        (uses )
                        (definition (pred1 equiv)))
           (s-predicate (for equiv)
                        (uses rimp)
                        (definition (pred2 equiv rimp)))
	   (s-predicate (for equiv)
                        (uses limp)
                        (definition (pred4 equiv limp)))
	   (s-predicate (for equiv)
                        (uses limp rimp)
                        (definition (and (pred2 equiv rimp) (pred4 equiv limp))))
           (c-predicate (for rimp)
                        (uses )
                        (definition (pred3 rimp)))
	   (c-predicate (for rimp)
                        (uses equiv)
                        (definition (pred2 equiv rimp)))
	   (c-predicate (for rimp)
                        (uses limp)
                        (definition (pred5 limp rimp)))
	   (c-predicate (for rimp)
                        (uses equiv limp)
                        (definition (and (pred2 equiv rimp) (pred4 equiv limp))))
	   (c-predicate (for limp)
                        (uses )
                        (definition (pred3 limp)))
	   (c-predicate (for limp)
                        (uses equiv)
                        (definition (pred4 equiv limp)))
	   (c-predicate (for limp)
                        (uses rimp)
                        (definition (pred5 limp rimp)))
	   (c-predicate (for limp)
                        (uses equiv rimp)
                        (definition (and (pred2 equiv rimp) (pred4 equiv limp)))))
	   
   (predicates
    (pred1 (equiv) (logic~equivalence-p equiv))
    (pred2 (equiv rimp) (and (pred1 equiv)
			     (pred3 rimp)
			     (data~equal (batac=equiver-f-create equiv) rimp)))
    (pred3 (imp) (logic~implication-p imp))
    (pred4 (equiv limp) (and (pred1 equiv)
			     (pred3 limp)
			     (data~equal (batac=equivel-f-create equiv) limp)))
    (pred5 (limp rimp) (batac=reversed-implications-p limp rimp))))



(agent~defmatrix equivi 	
   (agents (c-predicate (for equiv)
                        (uses )
                        (definition (pred1 equiv)))
           (c-predicate (for equiv)
                        (uses rimp)
                        (definition (pred2 equiv rimp)))
	   (c-predicate (for equiv)
                        (uses limp)
                        (definition (pred4 equiv limp)))
	   (c-predicate (for equiv)
                        (uses limp rimp)
                        (definition (and (pred2 equiv rimp) (pred4 equiv limp))))
           (s-predicate (for rimp)
                        (uses )
                        (definition (pred3 rimp)))
	   (s-predicate (for rimp)
                        (uses equiv)
                        (definition (pred2 equiv rimp)))
	   (s-predicate (for rimp)
                        (uses limp)
                        (definition (pred5 limp rimp)))
	   (s-predicate (for rimp)
                        (uses equiv limp)
                        (definition (and (pred2  equiv rimp) (pred4 equiv limp))))
	   (s-predicate (for limp)
                        (uses )
                        (definition (pred3 limp)))
	   (s-predicate (for limp)
                        (uses equiv)
                        (definition (pred4 equiv limp)))
	   (s-predicate (for limp)
                        (uses rimp)
                        (definition (pred5 limp rimp)))
	   (s-predicate (for limp)
                        (uses equiv rimp)
                        (definition (and (pred2 equiv rimp) (pred4 equiv limp)))))
	   
   (predicates
    (pred1 (equiv) (logic~equivalence-p equiv))
    (pred2 (equiv rimp) (and (pred1 equiv)
			     (pred3 rimp)
			     (data~equal (batac=equiver-f-create equiv) rimp)))
    (pred3 (imp) (logic~implication-p imp))
    (pred4 (equiv limp) (and (pred1 equiv)
			     (pred3 limp)
			     (data~equal (batac=equivel-f-create equiv) limp)))
    (pred5 (limp rimp) (batac=reversed-implications-p limp rimp))))




(defun batac=compute-foralle-instance (formula term-list)
  (let ((new-term formula))
    (dolist (x term-list)
      (when (logic~universal-quantification-p new-term)
	(setq new-term (beta~contract
			(term~appl-create (car (data~appl-arguments new-term))
					  (list x))))))
    new-term))

(defun batac=compute-foralle-termlist (all-formula formula &optional reversed-varlist)
  (multiple-value-bind (ok subst)
      (term~alpha-match all-formula formula)
    (if ok (mapcar #'(lambda (x) (subst-apply subst x))
		   (reverse reversed-varlist))
      (when (logic~universal-quantification-p all-formula)
	(batac=compute-foralle-termlist
	 (logic~quantification-scope all-formula)
	 formula
	 (cons (logic~quantification-bound-variable all-formula) reversed-varlist))))))


(agent~defmatrix foralle* 
   (agents (s-predicate (for univline)
                        (uses )
                        (definition (pred0 univline)))
           (s-predicate (for univline)
                        (uses elimline)
			(level 20)
                        (definition (pred1 univline elimline)))
           (s-predicate (for univline)
                        (uses elimline terms)
			(level 1)
                        (definition (pred2 univline elimline (:param terms))))
           (c-predicate (for elimline)
                        (uses univline)
			(level 20)
                        (definition (pred1 univline elimline)))
	   (c-predicate (for elimline)
                        (uses univline terms)
			(level 20)
                        (definition (pred2 univline elimline (:param terms))))
	   (function    (for terms)
                        (uses elimline)
			(level 3)
                        (definition (func1 elimline))) ;; alle Konstanten suchen
	   (function    (for terms)
                        (uses univline)
			(level 20)
                        (definition (func2 univline))) ;; neue Konstanten vorschlagen
	   (function    (for terms)
                        (uses univline elimline)
			(level 3)
                        (definition (reverse (batac=compute-foralle-termlist univline elimline))))) ;; exakte Terme berechnen
   (predicates
    (pred0 (univline) (logic~universal-quantification-p univline))
    (pred1 (univline elimline) 
	   (and (pred0 univline)
		(find-if #'(lambda (x) (term~alpha-match x elimline))
			 (mapcar #'(lambda (pos) (data~struct-at-position univline pos))
				 (data~positions univline
						 #'(lambda (x) (type~o-p (term~type x))))
			 )
		)
	   )
    ) ;; nach allen quant.Var. suchen 
    (pred2 (univline elimline terms)
	   (and (pred0 univline)
		(data~equal (batac=compute-foralle-instance univline terms) elimline)))

    (func1 (line)
	   (mapcar #'(lambda (pos)
		       (data~struct-at-position elimline pos))
		   (data~positions elimline #'term~constant-p)))
    (func2 (univline)
	   (when (pred0 univline)
	     (mapcar
	      #'(lambda (var)
		  (term~generate-term-primitive-with-new-name 'a (term~type var)
						  'term+constant
						  (pds~environment (agplan~current-proof-plan))))
	      (orules=strip-quantors2 univline))))))



(agent~defmatrix foralli* 
   (agents (c-predicate (for univline)
                        (uses )
                        (definition (pred0 univline)))
           (c-predicate (for univline)
                        (uses line)
			(level 20)
                        (definition (pred1 univline line)))
           (c-predicate (for univline)
                        (uses line newconsts)
			(level 1)
                        (definition (pred2 univline line (:param newconsts))))
           (s-predicate (for line)
                        (uses univline)
			(level 20)
                        (definition (pred1 univline line)))
	   (s-predicate (for line)
                        (uses univline newconsts)
			(level 1)
                        (definition (pred2 univline line (:param newconsts))))
	   (function    (for newconsts)
                        (uses line)
			(level 3)
                        (definition (func1 line))) ;; alle Konstanten suchen
	   (function    (for newconsts)
                        (uses univline)
			(level 3)
                        (definition (func2 univline))) ;; neue Konstanten vorschlagen
	   (function    (for newconsts)
                        (uses univline line)
			(level 3)
                        (definition (batac=compute-foralle-termlist univline line)))) ;; exakte Konstante berechnen
   (predicates
    (pred0 (univline) (logic~universal-quantification-p univline))
    (pred1 (univline line)
   	   (and (pred0 univline)
		(find-if #'(lambda (x) (term~alpha-match x line))
			 (mapcar #'(lambda (pos) (data~struct-at-position univline pos))
     			 (data~positions univline #'(lambda (x)
       			                                    (type~o-p (term~type x)))))))) 

    (pred2 (univline line newconsts)
	   (and (pred0 univline)
		(data~equal (batac=compute-foralle-instance univline newconsts) line)))

    (func1 (line)
	   (mapcar #'(lambda (pos)
		       (data~struct-at-position line pos))
		   (data~positions line #'term~constant-p)))
    (func2 (univline)
	   (when (pred0 univline)
	     (mapcar
	      #'(lambda (var)
		  (term~generate-term-primitive-with-new-name 'a (term~type var)
						  'term+constant
						  (pds~environment (agplan~current-proof-plan))))
	      (orules=strip-quantors2 univline))))))


(agent~defmatrix =ref 	
   (agents (c-predicate (for equality-line term)
                        (uses )
                        (definition
			  (and 
			   (logic~equality-p equality-line)
			   (data~equal (car (data~appl-arguments equality-line))
				       (cadr (data~appl-arguments equality-line)))
			   (list (car (data~appl-arguments equality-line))))))
           (c-predicate (for equality-line)
                        (uses term)
                        (definition (and (logic~equality-p equality-line)
					 (data~equal (car (data~appl-arguments
							   equality-line))
						     term))))
           (function    (for term)
                        (uses equality-line)
                        (definition (when (logic~equality-p equality-line)
				      (when (data~equal (car (data~appl-arguments
							      equality-line))
							(cadr (data~appl-arguments
							      equality-line)))
					(car (data~appl-arguments
							      equality-line))))))))




(agent~defmatrix =sym 	
   (agents (c-predicate (for equality-line1)
                        (uses )
			(level 1)
                        (definition (pred1 equality-line1)))
	   (c-predicate (for equality-line1)
                        (uses equality-line2)
			(level 1)
                        (definition (pred2 equality-line1 equality-line2)))
	   (s-predicate (for equality-line2)
                        (uses equality-line1)
			(level 1)
                        (definition (pred2 equality-line1 equality-line2))))
   (predicates
    (pred1 (eql) (logic~equality-p eql))
    (pred2 (eql1 eql2) (and (logic~equality-p eql1)
			    (logic~equality-p eql2)
			    (data~equal (batac=equality-sym-create eql1) eql2)))))



(agent~defmatrix =trans 	
   (agents (c-predicate (for equality-line1) ;; there exists an equation support line,
					     ;; which has one side in common with equality-line1
                        (uses )
			(exclude equality-line2 equality-line3)
			(level 1)
                        (definition (and (pred1 equality-line1)
					 (find-if #'(lambda (x) (pred2 equality-line1
								       (node~formula x)))
						  (pds~node-supports (:node equality-line1))))))
	   (c-predicate (for equality-line1)   ;; equality-line1 and equality-line2 have
					       ;; one side in common
                        (uses equality-line2)
			(exclude equality-line3)
			(level 1)
                        (definition (pred2 equality-line1 equality-line2)))
	   (c-predicate (for equality-line1)    ;; the three equality line fulfil the
						;; specification of =trans
                        (uses equality-line2  equality-line3)
			(level 1)
                        (definition (pred3 equality-line1 equality-line2  equality-line3)))
	   (s-predicate (for equality-line2)   ;; equality-line1 and equality-line2 have
					       ;; one side in common
                        (uses equality-line1)
			(exclude equality-line3)
			(level 1)
                        (definition (pred2 equality-line1 equality-line2)))
           (s-predicate (for equality-line2)   ;; the three equality line fulfil the
					       ;; specification of =trans
                        (uses equality-line1 equality-line3)
			(level 1)
                        (definition (pred3 equality-line1 equality-line2  equality-line3)))
	   (s-predicate (for equality-line3)  ;; equality-line1 and equality-line3 have
					      ;; one side in common
                        (uses equality-line1)
			(exclude equality-line2)
			(level 1)
                        (definition (pred2 equality-line1 equality-line3)))
           (s-predicate (for equality-line3)  ;; the three equality line fulfil the
					      ;; specification of =trans
                        (uses equality-line1 equality-line2)
			(level 1)
                        (definition (pred3 equality-line1 equality-line2
					   equality-line3))))
   (predicates
    (pred1 (eql) (logic~equality-p eql))
    (pred2 (eql1 eql2) (and (logic~equality-p eql1)
			    (logic~equality-p eql2)
			    (not (data~equal eql1 eql2))
			    (or
			     (data~equal (first (data~appl-arguments eql1))
					 (first (data~appl-arguments eql2)))
			     (data~equal (second (data~appl-arguments eql1))
					 (first (data~appl-arguments eql2)))	 
			     (data~equal (first (data~appl-arguments eql1))
					 (second (data~appl-arguments eql2)))
			     (data~equal (second (data~appl-arguments eql1))
					 (second (data~appl-arguments eql2))))))
    (pred3 (eql1 eql2 eql3) (and (logic~equality-p eql1)
				 (logic~equality-p eql2)
				 (logic~equality-p eql3)
				 (data~equal (batac=equality-trans-create eql2 eql3) eql1)))))



;;;;;;  weg hier 

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
      
      
    

    
    


(agent~defmatrix =subst         
   (agents (s-predicate (for equality-line)
                        (uses )
			(exclude line1 line2 position)
                        (level 1)
                        (definition (pred1 equality-line)))
           (s-predicate (for equality-line position)
                        (uses line1)
			(exclude line2)
			;(multiple position)    ;!!!!! hier Veraenderung vorgenommen
                        (level 3)
                        (definition (pred2 equality-line line1)))
           (s-predicate (for equality-line)
                        (uses line2)
			;(multiple position)  ;!!!!! hier Veraenderung vorgenommen
			(exclude line1)
                        (level 3)
                        (definition (pred2 equality-line line2)))
           (s-predicate (for equality-line)
                        (uses line1 line2)
			(exclude position)
                        (level 10)
                        (definition (pred3 equality-line line1 line2)))
           (s-predicate (for equality-line)
                        (uses line1 line2 position)
                        (level 10)
                        (definition (pred4 equality-line line1 line2 (:param position))))
           (s-predicate (for equality-line)
                        (uses line1 position)
			(exclude line2)
                        (level 3)
                        (definition (pred5 equality-line line1 (:param position))))
           (s-predicate (for equality-line)
                        (uses line2 position)
			(exclude line1)
                        (level 3)
                        (definition (pred5 equality-line line2 (:param position))))
           (c-predicate (for line1)
                        (uses )
			(exclude equality-line line2 position)
                        (level 20)
                        (definition (pred8 (:node line1))))
	   (c-predicate (for line1 line2)
                        (uses )
			(exclude equality-line position)
                        (level 20)
                        (definition (pred8 (:node line1))))
	                ;; Comment: pred8 returns in case of success a list containing
	                ;; the extra node to be bound to line2
           (c-predicate (for line1 position) 
			(uses equality-line)
			;(multiple position)    ;!!!!! hier Veraenderung vorgenommen
			(exclude line2)
                        (level 3)
                        (definition (pred2 equality-line line1)))
           (c-predicate (for line1)
                        (uses line2)
			(exclude equality-line position)
                        (level 20)
                        (definition (pred7 line1 line2)))
           (c-predicate (for line1)
                        (uses position)
			(exclude line2 equality-line)
                        (level 2)
                        (definition (pred0 line1 (:param position))))
           (c-predicate (for line1)  
                        (uses equality-line line2)
			(exclude position)
                        (level 10)
                        (definition (pred3 equality-line line1 line2)))
           (c-predicate (for line1)
                        (uses equality-line position)
			(exclude line2)
                        (level 3)
                        (definition (pred5 equality-line line1 (:param position))))
           (c-predicate (for line1)
                        (uses line2 position)
			(exclude equality-line)
                        (level 10)
                        (definition (pred6 line1 line2 (:param position))))
           (c-predicate (for line1) 
                        (uses equality-line line2 position)
                        (level 10)
                        (definition (pred4 equality-line line1 line2 (:param position))))
           
           (s-predicate (for line2)
                        (uses )
			(exclude equality-line line1 position)
                        (level 20)
                        (definition (pred9 line2)))
           (s-predicate (for line2 position)  ; !!!!! hier Veraenderung vorgenommen
			;(multiple position)
                        (uses equality-line) ;warnung
			(exclude line1)
                        (level 3)
                        (definition (pred2 equality-line line2)))
           (s-predicate (for line2)
                        (uses line1)
			(exclude equality-line position)
                        (level 20)
                        (definition (pred7 line1 line2)))
           (s-predicate (for line2)
                        (uses position)
			(exclude line1 equality-line)
                        (level 2)
                        (definition (pred0 line2 (:param position))))
           (s-predicate (for line2)  ;warnung
                        (uses equality-line line1)
			(exclude position)
                        (level 10)
                        (definition (pred3 equality-line line1 line2)))
           (s-predicate (for line2)  ;warnung
                        (uses equality-line position)
			(exclude line1)
                        (level 3)
                        (definition (pred5 equality-line line2 (:param position))))
           (s-predicate (for line2)
                        (uses line1 position)
			(exclude equality-line)
                        (level 10)
                        (definition (pred6 line1 line2 (:param position))))
           (s-predicate (for line2)
                        (uses equality-line line1 position)
                        (level 10)
                        (definition (pred4 equality-line line1 line2 (:param position))))

           (function    (for position)
                        (uses equality-line line1)
			;(multiple position)   ;; !!!!! hier Veraenderung vorgenommen
			(exclude line2)
                        (level 3)
                        (definition (func0 equality-line line1)))
           (function    (for position)
                        (uses equality-line line2)
			;(multiple position)
			(exclude line1)
                        (level 3)
                        (definition (func0 equality-line line2)))
           (function    (for position)
                        (uses line1 line2)
			(exclude equality-line)
                        (level 20)
                        (definition (pred7 line1 line2)))
           (function    (for position)
                        (uses equality-line line1 line2)
                        (level 10)
                        (definition (car (pred3 equality-line line1
							     line2)))))
   (predicates
    (pred0 (line position) ;; is position indeed a position in line
           (find position (data~positions line #'term~p) :test #'keim~equal))
    
    (pred1 (eql) ;; is eql and equation
           (logic~equality-p eql))

    (pred2 (eql line)  ;; is eql an equation which is applicable to line at a non top
		       ;; position
	   (when (and (not (keim~equal eql line2)) (pred1 eql))
	     (let ((pos-list (func0 eql line)))
	       (when pos-list (list (car pos-list))))))

    (pred3 (equality-line line1 line2)
                      ;; is equality-line an equation which is applicable to line1
		      ;; at a non top position such that line2 is obtained
	   (and (pred1 equality-line)
		(not (keim~equal equality-line line2))
		;; !! testing formulas for equivalence. It might be better to test the corresponding
		;; nodes for equivalence.
     		(let ((positions (delete (pos~empty)
					 (agent=mismatch-positions line1 line2)
					 :test #'keim~equal)))
		  (when (= (length positions) 1)
		    (let ((subterm1 (data~struct-at-position line1 (car positions)))
			  (subterm2 (data~struct-at-position line2 (car positions)))
			  (eql1 (first (data~appl-arguments equality-line)))
			  (eql2 (second (data~appl-arguments equality-line))))
		      (when (or (and (data~equal subterm1 eql1) (data~equal subterm2 eql2))
				(and (data~equal subterm1 eql2) (data~equal subterm2 eql1)))
			positions))))))
    
    (pred4 (equality-line line1 line2 position)
	   ;; is equality-line an equation which is applicable to line1
	   ;; at position such that line2 is obtained
	   (and (pred0 line1 position)
		(pred0 line2
		       position)
		(pred1 equality-line)
		(let ((subterm1 (data~struct-at-position line1 position))
		     (subterm2 (data~struct-at-position line2 position))
		     (eql1 (first (data~appl-arguments equality-line)))
		     (eql2 (second (data~appl-arguments equality-line))))
		  (or (and (data~equal subterm1 eql1) (data~equal subterm2 eql2))
		      (and (data~equal subterm1 eql2) (data~equal subterm2 eql1))))
		(not (keim~equal equality-line line2))))
    ;; !! testing formulas for equivalence. It might be better to test the corresponding
    ;; nodes for equivalence.

    (pred5 (equality-line line1 position)
	   ;; is equality-line an equation which is applicable to line1
	   ;; at position
	   (and (pred0 line1 position)
		(not (keim~equal equality-line line1))
		;; !! testing formulas for equivalence. It might be better to test the corresponding
		;; nodes for equivalnce.
		(pred1 equality-line)
		(or (data~equal (first (data~appl-arguments equality-line))
				(data~struct-at-position line1 position))
		    (data~equal (second (data~appl-arguments equality-line))
				(data~struct-at-position line1 position)))))

    (pred6 (line1 line2 position)
      ;; do line1 and line2 only differ at subterm-position position
	   (let ((positions (delete (pos~empty)
				    (agent=mismatch-positions line1 line2)
				    :test #'keim~equal)))
	     (when (= (length positions) 1)
	       (keim~equal (car positions) position))))
	       
    (pred7 (line1 line2) ;; do line1 and line2 only differ at a single subterm-position 
	   (find-if #'(lambda (x) (pred6 line1 line2 x))
		    (delete (pos~empty) (data~positions line1 #'term~p))))

    (pred8 (line1) ;; is there a support line of line1 which differs
		   ;; from line1 only wrt to a single subterm position
	           ;; Take care: Pred8 returns a list with additional nodes to be bount
	           ;; in case of success
	   (let ((node (find-if #'(lambda (x) (= (length (delete (pos~empty)
								 (agent=mismatch-positions
								  (node~formula line1) (node~formula x))
								 :test #'keim~equal))
						 1))
				(pds~node-supports line1))))
	     (when node (progn (setq test* node) (list node)))))

    (pred9 (line2) ;; is there open line which differs from line2 only wrt to a
		   ;; single subterm position
	   (find-if #'(lambda (x) (= (length (delete (pos~empty)
						     (agent=mismatch-positions
						      line2 (node~formula x))
						     :test #'keim~equal))
				     1))
		    (list (agplan~current-open-node))))

    (func0 (eql line)  ;;; returns a non top position in line where one hand side of eql is applicable
	   (when (pred1 eql)
	     (delete (pos~empty)
		     (data~positions line #'(lambda (x)
					      (or (data~equal (first (data~appl-arguments eql)) x)
						  (data~equal (second
							       (data~appl-arguments eql))
							      x))))
		     :test #'keim~equal)))))
    


(agent~defmatrix neg=sym 	
   (agents (c-predicate (for inequality-line1)
                        (uses )
			(level 1)
                        (definition (pred1 inequality-line1)))
           (s-predicate (for inequality-line2)
                        (uses )
			(level 1)
                        (definition (pred1 inequality-line2)))
	   (c-predicate (for inequality-line1)
                        (uses inequality-line2)
			(level 1)
                        (definition (pred2 inequality-line1 inequality-line2)))
	   (s-predicate (for inequality-line2)
                        (uses inequality-line1)
			(level 1)
                        (definition (pred2 inequality-line1 inequality-line2))))
   (predicates
    (pred1 (neql) (and (logic~negation-p neql)
		       (logic~equality-p (car (data~appl-arguments neql)))))
    (pred2 (neql1 neql2) (and (pred1 neql1)
			      (pred1 neql2)
			      (data~equal (batac=inequality-sym-create-f neql1) neql2)))))


(agent~defmatrix imp2or 	
   (agents (s-predicate (for implication-line)
                        (uses )
			(level 1)
                        (definition (pred1 implication-line)))
	   (s-predicate (for implication-line)
                        (uses or-line)
			(level 1)
                        (definition (batac=imp2or-a-p implication-line or-line)))
	   (c-predicate (for or-line)
                        (uses )
			(level 1)
                        (definition (pred2 or-line)))
	   (c-predicate (for or-line)
                        (uses implication-line)
			(level 1)
                        (definition (batac=imp2or-a-p implication-line or-line))))
   (predicates
    (pred1 (impl) (logic~implication-p impl))
    (pred2 (disj) (logic~disjunction-p disj))))


(agent~defmatrix modtoll 
   (agents (s-predicate (for aimpb)
                        (uses )
			(level 1)
                        (definition (logic~implication-p aimpb)))
           (s-predicate (for aimpb)
                        (uses negb)
			(level 1)
                        (definition (pred2 aimpb negb)))
           (s-predicate (for aimpb)
                        (uses nega)
			(level 1)
                        (definition (pred1 aimpb nega)))
           (s-predicate (for aimpb)
                        (uses negb nega)
			(level 1)
                        (definition (and (pred2 aimpb negb)
                                         (pred1 aimpb nega))))
           (c-predicate (for nega)
                        (uses aimpb)
			(level 1)
                        (definition (pred1 aimpb nega)))
	   (s-predicate (for negb)
                        (uses aimpb)
			(level 1)
                        (definition (pred2 aimpb negb))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~implication-p a1)
                (data~equal a2 (logic~negate (first (data~appl-arguments a1))))))
    (pred2 (a1 a2)
	   (and (logic~implication-p a1)
                (data~equal a2 (logic~negate (second (data~appl-arguments a1))))))))


(agent~defmatrix beta-normalize 
   (agents (c-predicate (for line1)
                        (uses line2)
			(level 3)
                        (definition (data~equal (beta~normalize line2) line1)))
	   (c-predicate (for line1)
                        (uses )
			(exclude line2)
			(level 3)
                        (definition (not (beta~normform-p line1))))
           (s-predicate (for line2)
                        (uses )
			(exclude line1)
			(level 3)
                        (definition (not (beta~normform-p line2))))
           (s-predicate (for line2)
                        (uses line1)
			(level 3)
                        (definition (data~equal (beta~normalize line2) line1)))))


(agent~defmatrix beta-expand 
   (agents (c-predicate (for line1)
                        (uses )
			(level 3)
                        (definition (not (beta~normform-p line1))))
           (c-predicate (for line1)
                        (uses line2)
			(level 3)
                        (definition (pred1 line1 line2)))
	   (c-predicate (for line1)
                        (uses line2 positions)
			(level 3)
                        (definition (pred2 line1 line2 (:param positions))))
           (s-predicate (for line2)
                        (uses line1)
			(level 3)
                        (definition (pred1 line1 line2)))
	   (s-predicate (for line2)
                        (uses line1 positions)
			(level 3)
                        (definition (pred2 line1 line2 (:param positions))))
;           (function (for positions)
;                        (uses line1 line2)
;                        (definition (func line1 line2)))
	   )
   (predicates 
    (pred1 (line1 line2) (lam~equal-p line1 line2))
    (pred2 (line1 line2 position) (data~equal (beta~expand line2 positions) line1)) 
    ))
   
	   

(agent~defmatrix contrapos 
   (agents (c-predicate (for conc)
                        (uses )
			(level 1)
                        (definition (logic~implication-p conc)))
           (c-predicate (for conc)
                        (uses line2)
			(level 1)
                        (definition (pred1 conc line2)))
	   (s-predicate (for line2)
                        (uses )
			(level 1)
                        (definition (logic~implication-p line2)))
           (s-predicate (for line2)
                        (uses conc)
			(level 1)
                        (definition (pred1 conc line2))))
   (predicates
    (pred1 (conc line2) (and (logic~implication-p conc)
			     (data~equal (batac=contrapos-f line2) conc)))))
    


(agent~defmatrix or2imp 	
   (agents (c-predicate (for implication-line)
                        (uses )
			(level 1)
                        (definition (pred1 implication-line)))
	   (c-predicate (for implication-line)
                        (uses or-line)
			(level 1)
                        (definition (batac=imp2or-a-p implication-line or-line)))
	   (s-predicate (for or-line)
                        (uses )
			(level 1)
                        (definition (pred2 or-line)))
	   (s-predicate (for or-line)
                        (uses implication-line)
			(level 1)
                        (definition (batac=imp2or-a-p implication-line or-line))))
   (predicates
    (pred1 (impl) (logic~implication-p impl))
    (pred2 (disj) (logic~disjunction-p disj))))


(agent~defmatrix equivsubst ;; line2 suggestion incorrect	
   (agents (s-predicate (for equivalence-line)
                        (uses )
                        (definition (pred1 equivalence-line)))
	   (s-predicate (for equivalence-line)
			(uses line1)
                        (definition (pred2 equivalence-line line1)))
	   (s-predicate (for equivalence-line)
                        (uses line2)
                        (definition (pred2 equivalence-line line2)))
	   (s-predicate (for equivalence-line)
                        (uses line1 line2)
                        (definition (pred3 equivalence-line line1 line2))) 
	   (s-predicate (for equivalence-line)
                        (uses line1 line2 position)
                        (definition (pred4 equivalence-line line1 line2 (:param position))))
	   (s-predicate (for equivalence-line)
                        (uses line1 position)
                        (definition (pred5 equivalence-line line1 (:param position))))
	   (s-predicate (for equivalence-line)
                        (uses line2 position)
                        (definition (pred5 equivalence-line line2 (:param position))))

	   (c-predicate (for line1)
                        (uses equivalence-line)
                        (definition (pred2 equivalence-line line1)))
	   (c-predicate (for line1)
                        (uses line2)
                        (definition (pred7 line1 line2)))
	   (c-predicate (for line1)
                        (uses position)
                        (definition (pred0 line1 (:param position))))
	   (c-predicate (for line1)
                        (uses equivalence-line line2)
                        (definition (pred3 equivalence-line line1 line2)))
	   (c-predicate (for line1)
                        (uses equivalence-line position)
                        (definition (pred5 equivalence-line line1 (:param position))))
	   (c-predicate (for line1)
                        (uses line2 position)
                        (definition (pred6 line1 line2 position)))
	   (c-predicate (for line1)
                        (uses equivalence-line line2 position)
                        (definition (pred4 equivalence-line line1 line2 (:param position))))

	   (s-predicate (for line2)
                        (uses equivalence-line)
                        (definition (pred2 equivalence-line line2)))
	   (s-predicate (for line2)
                        (uses line1)
                        (definition (pred7 line1 line2)))
	   (s-predicate (for line2)
                        (uses position)
                        (definition (pred0 line2 (:param position))))
	   (s-predicate (for line2)
                        (uses equivalence-line line1)
                        (definition (pred3 equivalence-line line1 line2)))
	   (s-predicate (for line2)
                        (uses equivalence-line position)
                        (definition (pred5 equivalence-line line2 (:param position))))
	   (s-predicate (for line2)
                        (uses line1 position)
                        (definition (pred6 line1 line2 (:param position))))
	   (s-predicate (for line2)
                        (uses equivalence-line line1 position)
                        (definition (pred4 equivalence-line line1 line2 (:param position))))

	   (function    (for position)
			(uses equivalence-line line1)
			(definition (car (func0 equivalence-line line1))))
	   (function    (for position)
			(uses equivalence-line line2)
			(definition (car (func0 equivalence-line line2))))
	   (function    (for position)
			(uses line1 line2)
			(definition (car (pred7 line1 line2))))
	   (function    (for position)
			(uses equivalence-line line1 line2)
			(level 10)
			(definition (pred3 equivalence-line line1 line2))))
   (predicates
    (pred0 (line position) (find position (data~positions line #'term~p) :test #'keim~equal))
    (pred1 (eql) (logic~equivalence-p eql)) ; fixed
    (pred2 (eql line)  (and (pred1 eql) (func0 eql line)))
    (pred3 (equivalence-line line1 line2)
	   (when (pred1 equivalence-line)
	     (find-if #'(lambda (x)
			  (term~alpha-equal
			  (batac=equivsubst-create-f line2 equivalence-line x) 
			   line1)) 
		        (func0 equivalence-line line1))))
    (pred4 (equivalence-line line1 line2 position)
	   (and (pred1 equivalence-line)
	        (pred5 equivalence-line line1 position)
		(term~alpha-equal (batac=equivsubst-create-f line2 equivalence-line position)
				  line1)))
    (pred5 (equivalence-line line1 position)
	   (find position (func0 equivalence-line line1) :test #'keim~equal))
    (pred6 (line1 line2 position)
	   (and (pred0 line1 position)
		(pred0 line2 position)
		(pred4 
		 (batac=equivsubst-create-l line1 line2 position)
		 line1 line2 position)))
    (pred7 (line1 line2)
	   (some #'(lambda (x) (pred6 line1 line2 x))
		 (union (data~positions line1 #'term~p) (data~positions line2 #'term~p))))

    (func0 (eql line)
	   (when (pred1 eql)
	     (data~positions line #'(lambda (x)
				      (or (data~equal (first (data~appl-arguments eql)) x)
					  (data~equal (second (data~appl-arguments eql)) x))))))
    )
)


(agent~defmatrix =2equiv 
   (agents (s-predicate (for equality)
                        (uses )
			(level 3)
                        (definition (func1 equality)))
	   (s-predicate (for equality)
                        (uses equivalence)
			(level 3)
                        (definition (func3 equality equivalence)))
	   (s-predicate (for equality)
                        (uses position)
			(level 3)
                        (definition (find (:param position) (func1 equality))))
	   (s-predicate (for equality)
                        (uses equivalence position)
			(level 3)
                        (definition (and (pred3 equality equivalence)
					 (batac==2equiv-applicable-p equality equivalence
								     (:param position)))))
	   (c-predicate (for equivalence)
                        (uses )
			(level 3)
                        (definition (func2 equivalence)))
	   (c-predicate (for equivalence)
                        (uses equality)
			(level 3)
                        (definition (func3 equality equivalence)))
	   (c-predicate (for equivalence)
                        (uses position)
			(level 3)
                        (definition (find (:param position) (func2 equivalence))))
	   (c-predicate (for equivalence)
                        (uses equality position)
			(level 3)
                        (definition (and (pred3 equality equivalence)
					 (batac==2equiv-applicable-p equality equivalence
								     (:param position)))))
	   (function    (for position)
                        (uses equality)
			(level 3)
                        (definition (car (func1 equality))))
	   (function    (for position)
                        (uses equivalence)
			(level 3)
                        (definition (car (func2 equivalence))))
	   (function    (for position)
                        (uses equality equivalence)
			(level 3)
                        (definition (func3 equality equivalence)))

	   )
   (predicates
    (pred1 (equiv) (logic~equivalence-p equiv))
    (pred2 (eq) (logic~equality-p eq))
    (pred3 (eq equiv) (and (pred1 equiv) (pred2 eq)))
    (func1 (eq) (when (pred2 eq) (data~positions eq #'(lambda (x) (logic~equality-p x)))))
    (func2 (equiv) (when (pred1 equiv) (data~positions equiv #'(lambda (x) (logic~equivalence-p x)))))
    (func3 (equality equiv) (when (pred3 equality equiv)
			      (find-if #'(lambda (pos)
					   (batac==2equiv-applicable-p equality equiv pos))
				       (func2 equiv))))
   ))


(agent~defmatrix equiv2= 
   (agents (c-predicate (for equality)
                        (uses )
			(level 3)
                        (definition (func1 equality)))
	   (c-predicate (for equality)
                        (uses equivalence)
			(level 3)
                        (definition (func3 equality equivalence)))
	   (c-predicate (for equality)
                        (uses pos)
			(level 3)
                        (definition (find (:param pos) (func1 equality) :test #'keim~equal)))
	   (c-predicate (for equality)
                        (uses equivalence pos)
			(level 3)
                        (definition (and (pred3 equality equivalence)
					 (batac==2equiv-applicable-p equality equivalence
								     (:param pos)))))
	   (s-predicate (for equivalence)
                        (uses )
			(level 3)
                        (definition (func2 equivalence)))
	   (s-predicate (for equivalence)
                        (uses equality)
			(level 3)
                        (definition (func3 equality equivalence)))
	   (s-predicate (for equivalence)
                        (uses pos)
			(level 3)
                        (definition (find (:param pos) (func2 equivalence) :test #'keim~equal)))
	   (s-predicate (for equivalence)
                        (uses equality pos)
			(level 3)
                        (definition (and (pred3 equality equivalence)
					 (batac==2equiv-applicable-p equality equivalence
								     (:param pos)))))
	   (function    (for pos)
                        (uses equality)
			(level 3)
                        (definition (car (func1 equality))))
	   (function    (for pos)
                        (uses equivalence)
			(level 3)
                        (definition (car (func2 equivalence))))
	   (function    (for pos)
                        (uses equality equivalence)
			(level 3)
                        (definition (func3 equality equivalence)))

	   )
   (predicates
        (pred1 (equiv) (logic~equivalence-p equiv))
    (pred2 (eq) (logic~equality-p eq))
    (pred3 (eq equiv) (and (pred1 equiv) (pred2 eq)))
    (func1 (eq) (when (pred2 eq) (data~positions eq #'(lambda (x) (logic~equality-p x)))))
    (func2 (equiv) (when (pred1 equiv) (data~positions equiv #'(lambda (x) (logic~equivalence-p x)))))
    (func3 (equality equiv) (when (pred3 equality equiv)
			      (find-if #'(lambda (pos)
					   (batac==2equiv-applicable-p equality equiv pos))
				       (func2 equiv))))
   ))


(agent~defmatrix idemor 	
   (agents (c-predicate (for disjunction)
                        (uses )
			(level 1)
                        (definition (func disjunction)))
	   (c-predicate (for disjunction)
                        (uses merge)
			(level 1)
                        (definition (and (func disjunction)
					 (data~equal merge (func disjunction)))))
	   (s-predicate (for merge)
                        (uses disjunction)
			(level 1)
                        (definition (and (func disjunction)
					 (data~equal merge (func disjunction))))))
   (predicates
    (func (disj) (when (and (logic~disjunction-p disj)
			    (data~equal (car (data~appl-arguments disj))
					(cadr (data~appl-arguments disj))))
		   (car (data~appl-arguments disj))))))


(agent~defmatrix simplify 	   ;;; zweimal das gleiche
   (agents (c-predicate (for concl)
                        (uses line2)
			(level 10)
                        (definition (batac=simplified-p concl line2)))
	   (s-predicate (for concl)
                        (uses line2)
			(level 10)
                        (definition (batac=simplified-p concl line2)))))


(agent~defmatrix simplify-goal 	;;; zweimal das gleiche
   (agents (c-predicate (for concl)
                        (uses line2)
			(level 10)
                        (definition (batac=simplified-p line2 concl)))
	   
	   (s-predicate (for concl)
                        (uses line2)
			(level 10)
                        (definition (batac=simplified-p line2 concl)))))


(agent~defmatrix impi 
   (agents (c-predicate (for implication)
                        (uses )
			(definition (logic~implication-p implication))))
) 


(agent~defmatrix kappai
   (agents (c-predicate (for kappa-line)
                        (uses )
			(definition (data~schema-p kappa-line)))
	   (function    (for type-list)
			(uses kappa-line)
			(definition (let ((vars (data~schema-domain kappa-line))
					  (env (pds~environment pds*current-proof-plan)))
						    (mapcar #'(lambda (var)
								(type~generate-type-primitive-with-new-name 'tc 'type+constant env))
							    vars))))))

(agent~defmatrix kappae
		 (agents (s-predicate (for kappa-line)
				      (uses )
				      (definition (data~schema-p kappa-line)))
			 (function    (for type-list)
				      (uses kappa-line)
				      (definition (let ((vars (data~schema-domain
							       kappa-line))
							(env (pds~environment pds*current-proof-plan)))
						    (mapcar #'(lambda (var)
								(type~generate-type-primitive-with-new-name 'tc 'type+constant env))
							    vars))))))

			  




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(agent~defclassifier ho
;                     (function #'(lambda (x) (not (logic~fo-formula-p (node~formula x)))))
;                     (help "Is the goal higher order?"))
;
;(agent~defclassifier fo
;                     (function #'(lambda (x) (logic~fo-formula-p (node~formula x))))
;                     (help "Is the goal first order?"))
;
;(agent~defclassifier theory
;                     (function #'(lambda (x) (prob~theory omega*current-proof-plan)))
;                     (information t)
;                     (help "Theory of the problem."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agents for external reasoners
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
                (definition (when (and (not (agplan~contains-simplifiable-terms node))
				       (not (agplan~contains-higher-concepts node
									     (list :struct :group :loop :semigroup :quasigroup :monoid :magma)
									     :exists-unique :atmost-one :that)))
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
				       (not (agplan~contains-simplifiable-terms node)))
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
				       (not (agplan~contains-simplifiable-terms node))
				       (not (agplan~leo-partial-result-subnode-p node))
				       (not (leo~clause-fo-like-p node)))
		              (omega~message "~%Agent \"result-by-leo\" is calling LEO with time-resource ~A"  auto*default-interval)
			      (list (agplan~tackle-by-leo (:node node)
							  'ext-input-recursive
							  (floor auto*default-interval))))))

	
		

;;; I need a tactic for (partial) leo-derivations

(infer~deftactic leo-derivation
		 (outline-mappings (((nonexistent existent) leo-derivation-f)))
                 (expansion-function leo=expand-leo-derivation))


(tac~deftactic leo-derivation-f leo-derivation (in base)
               (premises L2)
               (conclusions L1)
	       (computations (L1 (leo=compute-leo-derivation-f)))
               (sideconditions (leo=leo-derivation-f-p (formula L2)))
               (description "Forward application of leo-derivation-f tactic."))


(defun leo=leo-derivation-f-p (formula)
  t ;;; what to choose here ? node that leo-derivation cannot be called
    ;;; interactively, since there is no command for it. It is used within
    ;;; leo~insert-partial-result only, and the side-conditions should
    ;;; be checked there in a more global context (see when-part).
  )

(defun leo=compute-leo-derivation-f ()
  ;;; just choose formula suggested by LEO in LEO*G-PARTIAL-RESULT
  (leo~transform-in-formula LEO*G-PARTIAL-RESULT))

	       



(defun leo=expand-leo-derivation (outline paramters)
  (declare (ignore (outline paramters)))
  (omega~message "I am sorry, the expansion of `leo-derivation' is not possible yet."))






;;;; here we are; this function has to be modified apropriately in agplan.lisp




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
				       (not (agplan~contains-simplifiable-terms node))
				       (not (agplan~leo-partial-result-subnode-p node)))
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
				       (not (agplan~contains-simplifiable-terms node))
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
   (sideconditions (agplan~contains-simplifiable-terms (formula L2)))
   (description "Simplify with CAS in forward direction."))

(defun batac=simplify-with-CAS-b (formula)
   (agplan~simplify-with-cas formula))
					      
(tac~deftactic simplify-with-CAS-f simplify-with-CAS (in base)
   (premises L1)
   (conclusions L2)
   (computations (L2 (batac=simplify-with-CAS-b (formula L1))))
   (sideconditions (agplan~contains-simplifiable-terms (formula L1)))
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

(agent~defagent SIMPLIFY-GOAL-WITH-CAS c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (agplan~contains-simplifiable-terms node)
		              (omega~message "~%Agent \"SIMPLIFY-WITH-CAS\" is making calls to MAPLE in order to simplify the goal")
			      (let ((res (agplan~simplify-goal-with-CAS (:node node))))
				(when res (list res))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   SOLVED BY TPS     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (agplan~apply-advertised-pds key-to-proof))



(agent~defagent solved-by-tps c-predicate
		(for node key-to-proof)
		(uses )
		(level 1)
		(definition (when (and (not (logic~fo-formula-p node))
				       (not (term~schema-p node))) 
			      (list (agplan~tackle-by-tps (:node node))))))


;;;;;;; island-tactic and lemma

(agent~defmatrix island-tactic
   (agents (c-predicate
	    (for conc)
	    (uses )
	    (level 1)
	    (definition t))))

(agent~defmatrix lemma
   (agents (c-predicate
	    (for node)
	    (uses )
	    (level 1)
	    (definition t))))

;;;;;;;;;;;;;;;;;; matching agent

(com~defcommand apply-mmatching
		(argnames strategy   method   goal   params      supports    mmatching)
		(argtypes symbol     symbol   ndline symbol-list ndline-list anything)
		(arghelps "strategy" "method" "goal" "params"    "supports"  "mmatching")
		(function oants~apply-mmatching)
		(defaults )
		(frag-cats tactics base)
		(log-p t)
		(help "Applies a mmatching"))


(defgeneric oants~apply-mmatching (strategy method goal param supports matching)
  (:method (strategy (method meth+method) (goal node+node) param supports  (matching number))
	   (declare (ignore strategy))
	   (let* ((pds omega*current-proof-plan)
		  (agenda (pds~agenda pds))
		  (task (plan~find-task-for-goal-in-agenda goal agenda))
		  (mmatching (nth matching (oants=real-mmatchings method goal supports param)))
		  (open (pds~open-nodes pds)))
	     (when mmatching 
	       (omega~message "OANTS: Selected matching: ~A, Applying now this matching." mmatching)
	       (setf pplan*roc-state-description (roc~create-pplanner-state-description nil nil nil nil nil nil nil nil nil));;hack
	       (multiple-value-bind
		   (new-agenda new-opens new-supps)
		   (pplan=apply-plan-step! task mmatching pds agenda :kind 'standard)
		 (foci~update-pcs (set-difference open (pds~open-nodes pds)) new-opens)
		 ))))
    (:method (strategy (method symbol) goal param supports matching)
	   (when method (oants~apply-mmatching strategy (meth~find-method method) goal param supports matching))))


(defgeneric oants=mmatchings (method goal supports params)
  (:method ((method meth+method) (goal node+node) (supports list) (params list))
	      (let ((matchs (oants=real-mmatchings method goal supports params)))
		(when matchs
		  (do* ((i (1- (length matchs)) (1- i))
			(return (list i) (cons i return)))
		      ((zerop i) return)))))
(:method ((method symbol) goal supports params)
	   (when method (oants=mmatchings (meth~find-method method) goal supports params)))
  (:method (method (goal symbol) supports params)
	   (when goal (oants=mmatchings method (pds~label2node goal) supports params))))

(defun oants=real-mmatchings (method goal supports params)
	   (let* ((pds omega*current-proof-plan)
		  (agenda (pds~agenda pds))
		  (task (plan~find-task-for-goal-in-agenda goal agenda))
		  (parameter-types (infer~parameter-types (meth~inference method)))
		  (new-params (mapcar #'(lambda (x y) (arg~read-type x y)) parameter-types params)))
	     (setf (keim::pdsc~broken-matchings (pdsj~control (node~justification (agenda~task-node task))))  nil)
	     (let* ((goal-matchings (pplan=matching-goals task method :kind 'standard)))
	       (when goal-matchings
		 (pplan=handling-support-tupel pds agenda task method goal-matchings (list supports new-params)
					       :kind 'standard
					       :mmatchings 'all)))))

(defun oants=strategies () sod*current-strategies)

(defgeneric oants=methods (strat)
  (:method ((strat symbol))
	   (oants=methods (strat~find-strategy-ks strat)))
  (:method ((strat strat+strategy-ks))
	   (gethash 'methods (strat~strategy-ks-hash-table strat))))

(defun oants=goal ()
  (foci~focus-line (foci~active-pc)))

(defun oants=supports (goal supps)
  (pds~node-supports goal))

(defgeneric oants=params (method strat goal)
  (:method ((method meth+method)(strat strat+strategy-ks)(goal node+node))
	   (when (meth~parameters method)
	     (let* ((meth  (keim~name method))
		    (pds   omega*current-proof-plan)
		    (crs   (remove-if-not #'(lambda (cr)  ;take a cr
					      (some #'(lambda (to-do) ; is in the to-do part ...
							(omega~trace "to-do ~A" to-do)
							(some #'(lambda (single) ; of every command that has a list 
								  (omega~trace "single ~A" single)
								  (and (consp single)  ; not only a name but a list
								       (string-equal meth (first single))  ; for the method
								       (not (null (third single)))))  ; that has a parameter-inst (not complete)
							      (when  (listp (second to-do)) (second to-do))))
							(cri~to-do-part cr)))
					  (mapcar #'(lambda (crname)(gethash crname cri*control-rules-hashtable))
						  (gethash 'control-rules (strat~strategy-ks-hash-table strat))))))
	       (reverse (remove-duplicates
			 (mapcan #'(lambda (cr)
				     (omega~trace "processing ~A of ~A" cr crs)
				     (mapcar #'third
					     (remove-if #'(lambda (answer)
							    (or (atom answer)
								(not (eq (car answer) meth))))
							(cri~call (list meth)
								  :kind 'methods
								  :task (plan~find-task-for-goal-in-agenda goal (pds~agenda pds))
								  :task-node goal 
								  :task-formula (node~formula goal)
								  :agenda (pds~agenda pds)
								  :crules (list (keim~name cr))
								  :pds pds))))
				 crs))))))
  (:method ((method symbol) strat goal)
	   (when method (oants=params (meth~find-method method) strat goal)))
  (:method (method (strat symbol) goal)
	   (when strat (oants=params  method (strat~find-strategy-ks strat) goal)))
  (:method (method strat (goal symbol))
	   (when goal (oants=params  method strat (pds~label2node goal)))))
	   
(agent~defmatrix apply-mmatching
		 (agents 
		  (function    (for strategy)
			       (multiple strategy)
			       (definition (oants=strategies)))
		  (function    (for method)
			       (uses strategy)
			       (multiple method)
			       (definition (oants=methods (:param strategy))))
		  (c-predicate (for goal)
			       (definition T))
		  (function    (for params)
			       (uses method strategy goal)
			       (multiple params)
			       (definition (oants=params (:param method) (:param strategy) (:node goal))))
		  (s-pred-list (for supports)
			       (uses goal)
			       (definition (oants=supports (:node goal)(:node supports))))
		  (function    (for mmatching)
			       (uses method goal params)
			       (multiple mmatching)
			       (definition (oants=mmatchings (:param method) (:node goal) nil (:param params))))
		  (function    (for mmatching)
			       (uses method goal supports params)
			       (multiple mmatching)
			       (definition (oants=mmatchings (:param method) (:node goal) (:param supports) (:param params))))))



;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solved by LEO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand LEO+Bliksem
  (argnames node clauses fo-proof)
  (argtypes ndline anything boolean)
  (arghelps "Node to prove with LEO and Bliksem" "A list of FO-like Leo clauses." "A flag for the FO prover.")
  (frag-cats extern)
  (function agplan=leo+bliksem)
  (log-p T) 
  (help ""))

(defun agplan=LEO+Bliksem (line clauses fo-proof)
  (declare (ignore clauses))
  (when fo-proof
    (infer~compute-outline 'leo+bliksem line fo-proof)))


(agent~defmatrix Leo+Bliksem
		 (agents 
		  (c-predicate (for node)
			       (uses )
;;			       (definition (oc=call-leo-on-node  (:node node) 'ext-input-recursive)))
 			       (definition (agplan~tackle-by-leo (:node node)
 								 'ext-input-recursive
 								 (floor auto*default-interval))))
		  (function    (for clauses)
			       (uses )
			       (definition
				 (when leo*g-fo-clauses-changed leo*g-fo-clauses)))
		  (function    (for fo-proof)
			       (uses clauses)
			       (definition (agplan=leo-call-bliksem clauses)))))


(defun agplan=leo-call-bliksem (clauses)
  (leo~call-bliksem :clauses clauses :resource leo*f-fo-atp-resource))
		

;;; I need a tactic for (partial) leo-derivations

(infer~deftactic leo+bliksem
		 (outline-mappings (((nonexistent) leo+bliksem-a)))
		 (parameter-types boolean)
                 (expansion-function leo=expand-leo-derivation))


(tac~deftactic leo+bliksem-a leo+bliksem (in base)
               (conclusions L1)
	       (parameters (fo-proof boolean "A flag."))
               (sideconditions (leo=leo+bliksem-a-p fo-proof))
               (description "Forward application of leo+bliksem-f tactic."))


(defun leo=leo+bliksem-a-p (fo-proof)
  fo-proof ;;; does the proof exist?
  )



(defun leo=expand-leo+bliksem (outline paramters)
  (declare (ignore (outline paramters)))
  (omega~message "I am sorry, the expansion of `leo+bliksem' is not yet possible."))






(agent~defmatrix defsi 	
   (agents (c-predicate (for concl)
                        (uses )
			(level 20)
                        (definition (agplan~contained-definition concl)))
	   (c-predicate (for concl)
                        (uses line2)
			(level 20)
                        (definition (gentac=substituted-definitions-p
				     line2 concl (func1 line2))))
	   (c-predicate (for concl)
                        (uses line2 defs)
			(level 20)
                        (definition  (gentac=substituted-definitions-p
				      line2 concl (:param defs))))
	   #+weg(s-predicate (for line2)
                        (uses )
			(level 20)
                        (definition (not (agplan~contained-definition line2))))
	   (s-predicate (for line2)
                        (uses concl)
			(level 20)
                        (definition (gentac=substituted-definitions-p
				     line2 concl (func1 line2))))
	   (s-predicate (for line2)
                        (uses concl defs)
			(level 20)
                        (definition (gentac=substituted-definitions-p
				     concl line2 (:param defs))))
	   #+weg(function  (for defs)
			(uses)
			(level 20)
			(definition (list (repr~find-definition '= (prob~theory omega*current-proof-plan)))))
	   (function    (for defs)
			(uses line2)
			(level 20)
			(definition (cons (repr~find-definition '= (prob~theory omega*current-proof-plan)) (func1 line2))))
	   (function    (for defs)
			(uses concl line2)
			(level 20)
			(definition (if (gentac=substituted-definitions-p
					   line2 concl (func1 line2))
					(cons (repr~find-definition '= (prob~theory omega*current-proof-plan)) (func1 line2))
				      (list (repr~find-definition '= (prob~theory omega*current-proof-plan)))))))
   (predicates
    (func0 (line) (data~positions
		    line
		    #'(lambda (x)
			(and (term~constant-p x)
			     (th~find-assumption (keim~name x) (prob~theory
								(agplan~current-proof-plan)))))))
    (func1 (line) (mapcar #'(lambda (x)
			      (and (term~constant-p x)
				   (th~find-assumption (keim~name x) (prob~theory
								      (agplan~current-proof-plan)))))
			  (func0 line)))))
