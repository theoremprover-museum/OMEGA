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



(agent~defmatrix weaken 
   (agents (c-predicate (for lowerline)
                        (uses upperline)
			(level 1)
                        (definition (pred1 upperline lowerline)))
           (c-predicate (for lowerline)
			(exclude upperline)
			(level 1)
                        (definition (pred2 lowerline)))
           (s-predicate (for upperline)
                        (uses lowerline)
			(level 1)
                        (definition  (pred1 upperline lowerline)))
	   (s-predicate (for upperline)
			(exclude lowerline)
			(level 1)
                        (definition (pred3 upperline))))
   (predicates
    (pred1 (a1 a2)
	   (term~alpha-equal a1 a2))
    (pred2 (line1)
	   (find-if #'(lambda (x)
			 (pred1 line1 (node~formula x)))
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
			(level 1)
                        (definition (logic~disjunction-p disjunction)))
	   (c-predicate (for goal)
			(uses )
			(level 3)
			(definition (pds~find-node-support (:node goal) #'logic~disjunction-p)))))  ;;; wie geht das hier


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
			(level 5)
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
    (func1 (a1)
	   (data~struct-at-position
	    a1
	    (car (data~positions a1 #'term~constant-p))))
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
                        (definition (pred3 exline conc)))
	   (c-predicate (for conc)
                        (uses subgoal)
                        (definition  (and (not (equal (:node conc) (:node subgoal)))
					  (data~equal conc subgoal))))
           (s-predicate (for subgoal)
                        (uses conc)
                        (definition (and (not (equal (:node conc) (:node subgoal)))
                                         (data~equal conc subgoal))))
           (s-predicate (for subgoal)
                        (uses exline)
                        (definition (pred3 exline subgoal)))
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
						    (logic-existential-quantification-p x))))
    (pred3 (exline conc)
	   (and
	    (logic~existential-quantification-p exline)
	    (pds~find-node-support  (:node conc) #'(lambda (x)
						     (term~alpha-match
						      (logic~quantification-scope exline)
						      x)))))
    (func0 (exline) (when (logic~existential-quantification-p exline)
		      (term~generate-term-primitive-with-new-name 
		       'a (term~type (logic~quantification-bound-variable exline))
		       'term+constant (pds~environment (agplan~current-proof-plan)))))
    (func1 (exline goal)
	   (when
	       (logic~existential-quantification-p exline)
	     (agplan~matching-term
	      (logic~quantification-scope exline)
	      (or (car 
		   (pds~find-node-support (:node goal) #'(lambda (x)
							   (and (term~p x)
								(agplan~matching-term  
								 (logic~quantification-scope exline)
								 x)))))
		  (logic~negation-constant)))))))  ;schwachsinn muss spaeter weg





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



#+weg(agent~defmatrix defn-expand
   (agents (s-predicate (for line)
                        (uses )
			(level 3)		
                        (definition (agplan~contained-definition line)))
           (s-predicate (for line)
                        (uses definition)
			(level 3)
                        (definition (pred1 line (:param definition))))
	   (s-predicate (for line)
                        (uses definition position)
			(level 5)
                        (definition (pred2 line (:param definition) (:param position))))
	   (function (for definition)
		     (uses line)
		     (level 3)
		     (definition (agplan~contained-definition line)))
           (function (for definition)
		     (uses line position)
		     (level 30)
		     (definition (func1 line (:param position))))
	   (function (for position)
		     (uses line)
		     (level 3)
		     (definition (car (data~substruct-positions (agplan~contained-definition
							    line)
							   line))))
	   (function (for position)
		     (uses line definition)
		     (level 3)
		     (definition (car (pred1 line (:param definition))))))
   (predicates
    (pred1 (line definition)
	    (data~positions line #'(lambda (x)
				     (and (term~constant-p x)
					  (keim~equal (th~definition-constant definition)
						      x)))))
    (pred2 (line definition position)
	   (find position (pred1 line definition)))
    (func1 (line position)
	   (when (find position (data~positions line #'term-constant-p))
	     (find-if
	      #'(lambda (x) (keim~equal (data~struct-at-position line position)
					(th~definition-constant x)))
	      (th~definitions-recursed (prob~theory (agplan~current-proof-plan))))))))
    
    
#+weg(agent~defmatrix defn-contract
   (agents (c-predicate (for line)
                        (uses )
			(level 3)
                        (definition (agplan~contained-definition line)))
           (c-predicate (for line)
                        (uses definition)
			(level 3)
                        (definition (pred1 line (:param definition))))
	   (c-predicate (for line)
                        (uses definition pos)
			(level 5)
                        (definition (pred2 line (:param definition) (:param pos))))
	   (function (for definition)
		     (uses line)
		     (level 3)
		     (definition (agplan~contained-definition line)))
	   (function (for definition pos)
		     (uses line)
		     (level 3)
		     (definition (agplan~get-definition&pos line)))
           (function (for definition)
		     (uses line pos)
		     (level 30)
		     (definition (func1 line (:param pos))))
	   (function (for pos)
		     (uses line)
		     (level 3)
		     (definition (car (data~substruct-positions (agplan~contained-definition
								 line)
								line))))
	   (function (for pos)
		     (uses line definition)
		     (level 3)
		     (definition (car (pred1 line (:param definition))))))
   (predicates
    (pred1 (line definition)
	    (data~positions line #'(lambda (x)
				     (and (term~constant-p x)
					  (keim~equal (th~definition-constant definition)
						      x)))))
    (pred2 (line definition position)
	   (find position (pred1 line definition)))
    (func1 (line position)
	   (when (find position (data~positions line #'term-constant-p))
	     (find-if
	      #'(lambda (x) (keim~equal (data~struct-at-position line position)
					(th~definition-constant x)))
	      (th~definitions-recursed (prob~theory (agplan~current-proof-plan))))))))





(agent~defmatrix exte
   (agents (c-predicate (for lowerline)
                        (uses )
			(level 1)
                        (definition (logic~equality-p lowerline)))
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
                        (definition (agplan~ext-check upperline lowerline)))))
				       
	   

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


(agent~defmatrix ormp 	
   (agents (s-predicate (for disj)
                        (uses )
			(level 1)
                        (definition (pred1 disj)))
           (s-predicate (for disj)
                        (uses conc)
			(level 1)
                        (definition (pred2 disj conc)))
	   (s-predicate (for disj)
                        (uses conc neg)
			(level 1)
                        (definition (pred3 disj conc neg)))
	   (s-predicate (for neg)
                        (uses )
			(level 1)
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
	 (cons (logic~quantification-bound-variable all-formual) reversed-varlist))))))


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
                        (definition (batac=compute-foralle-termlist univline elimline)))) ;; exakte Terme berechnen
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
			(level 1)
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
		(find-if #'(lambda (x)
			     (term~alpha-match x line))
			 (mapcar #'(lambda (pos)
				     (data~struct-at-position univline pos))
				 (data~positions univline
						 #'(lambda (x)
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


(agent~defmatrix equivsubst 	
   (agents (s-predicate (for equality-line)
                        (uses )
			(level 1)
                        (definition (pred1 equality-line)))
	   (s-predicate (for equality-line)
                        (uses line1)
			(level 3)
                        (definition (pred2 equality-line line1)))
	   (s-predicate (for equality-line)
                        (uses line2)
			(level 3)
                        (definition (pred2 equality-line line2)))
	   (s-predicate (for equality-line)
                        (uses line1 line2)
			(level 10)
                        (definition (pred3 equality-line line1 line2)))
	   (s-predicate (for equality-line)
                        (uses line1 line2 position)
			(level 10)
                        (definition (pred4 equality-line line1 line2 (:param position))))
	   (s-predicate (for equality-line)
                        (uses line1 position)
			(level 3)
                        (definition (pred5 equality-line line1 (:param position))))
	   (s-predicate (for equality-line)
                        (uses line2 position)
			(level 3)
                        (definition (pred5 equality-line line2 (:param position))))

	   (c-predicate (for line1)
                        (uses equality-line)
			(level 3)
                        (definition (pred2 equality-line line1)))
	   (c-predicate (for line1)
                        (uses line2)
			(level 20)
                        (definition (pred7 line1 line2)))
	   (c-predicate (for line1)
                        (uses position)
			(level 2)
                        (definition (pred0 line1 (:param position))))
	   (c-predicate (for line1)
                        (uses equality-line line2)
			(level 10)
                        (definition (pred3 equality-line line1 line2)))
	   (c-predicate (for line1)
                        (uses equality-line position)
			(level 3)
                        (definition (pred5 equality-line line1 (:param position))))
	   (c-predicate (for line1)
                        (uses line2 position)
			(level 10)
                        (definition (pred6 line1 line2 position)))
	   (c-predicate (for line1)
			(level 10)
                        (uses equality-line line2 position)
                        (definition (pred4 equality-line line1 line2 (:param position))))

	   (s-predicate (for line2)
                        (uses equality-line)
			(level 3)
                        (definition (pred2 equality-line line2)))
	   (s-predicate (for line2)
                        (uses line1)
			(level 20)
                        (definition (pred7 line1 line2)))
	   (s-predicate (for line2)
                        (uses position)
			(level 2)
                        (definition (pred0 line2 (:param position))))
	   (s-predicate (for line2)
                        (uses equality-line line1)
			(level 10)
                        (definition (pred3 equality-line line1 line2)))
	   (s-predicate (for line2)
                        (uses equality-line position)
			(level 3)
                        (definition (pred5 equality-line line2 (:param position))))
	   (s-predicate (for line2)
                        (uses line1 position)
			(level 10)
                        (definition (pred6 line1 line2 (:param position))))
	   (s-predicate (for line2)
                        (uses equality-line line1 position)
			(level 10)
                        (definition (pred4 equality-line line1 line2 (:param position))))

	   (function    (for position)
			(uses equality-line line1)
			(level 3)
			(definition (func0 equality-line line1)))
	   (function    (for position)
			(uses equality-line line2)
			(level 3)
			(definition (func0 equality-line line2)))
	   (function    (for position)
			(uses line1 line2)
			(level 20)
			(definition (car (pred7 line1 line2))))
	   (function    (for position)
			(uses equality-line line1 line2)
			(level 10)
			(definition (pred3 equality-line line1 line2))))
   
   (predicates
    (pred0 (line position) (find position (data~positions line #'term~p) :test #'keim~equal))
    (pred1 (eql) (logic~implication-p eql))
    (pred2 (eql line)  (and (pred1 eql) (func0 eql line)))
    (pred3 (equality-line line1 line2)
	   (when (pred1 equality-line)
	     (find-if #'(lambda (x)
			  (term~alpha-equal
			   (batac=equivsubst-create-f line2 equality-line x) 
			   x))
		      (func0 equality-line line1))))
    (pred4 (equality-line line1 line2 position)
	   (and (pred1 equality-line)
	        (find position (func0 equality-line line1) :test #'keim~equal)
		(term~alpha-equal (batac=equivsubst-create-f line2 equality-line position)
				  line1)))
    (pred5 (equality-line line1 position)
	   (find position (func0 equality-line line1) :test #'keim~equal))
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
					  (data~equal (second (data~appl-arguments eql)) x))))))))


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


#+weg(agent~defmatrix defse 	
   (agents (c-predicate (for concl)
                        (uses )
			(level 20)
                        (definition (not (agplan~contained-definition concl))))
	   (c-predicate (for concl)
                        (uses line2)
			(level 20)
                        (definition (gentac=substituted-definitions-p
				     concl line2 (func1 concl))))
	   (c-predicate (for concl)
                        (uses line2 defs)
			(level 20)
                        (definition  (gentac=substituted-definitions-p
				     concl line2 (:param defs))))
	   (s-predicate (for line2)
                        (uses )
			(level 20)
                        (definition (agplan~contained-definition line2)))
	   (s-predicate (for line2)
                        (uses concl)
			(level 20)
                        (definition (gentac=substituted-definitions-p
				     concl line2 (func1 concl))))
	   (s-predicate (for line2)
                        (uses concl defs)
			(level 20)
                        (definition (gentac=substituted-definitions-p
				     concl line2 (:param defs))))
	   (function    (for defs)
			(uses)
			(level 20)
			(definition (list '=)))
	   (function    (for defs)
			(uses concl)
			(level 20)
			(definition (cons '= (func1 concl))))
	   (function    (for defs)
			(uses concl line2)
			(level 20)
			(definition (if  (gentac=substituted-definitions-p
					  concl line2 (func1 concl))
					(cons '= (func1 concl))
				      (list '=)))))
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



#+weg(agent~defmatrix defsi 	
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
	   (s-predicate (for line2)
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
	   (function    (for defs)
			(uses)
			(level 20)
			(definition (list '=)))
	   (function    (for defs)
			(uses line2)
			(level 20)
			(definition (cons '= (func1 line2))))
	   (function    (for defs)
			(uses concl line2)
			(level 20)
			(definition (if (gentac=substituted-definitions-p
					   line2 concl (func1 line2))
					(cons '= (func1 line2))
				      (list '=)))))
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


#+weg(agent~defmatrix defn-expand* 
   (agents (s-predicate (for line)
                        (uses )
			(level 3)
                        (definition (agplan~contained-definition line)))
           (s-predicate (for line)
                        (uses definition)
			(level 10)
                        (definition (pred1 line (:param definition))))
	   (s-predicate (for line)
			(level 5)
                        (uses definition position-list)
                        (definition (pred2 line (:param definition) (:param position-list))))
	   (function (for definition)
		     (uses line)
		     (level 3)
		     (definition (agplan~contained-definition line)))
           (function (for definition)
		     (uses line position-list)
		     (level 20)
		     (definition (func1 line (:param position-list))))
	   (function (for position-list)
		     (uses line)
		     (level 3)
		     (definition (data~substruct-positions (agplan~contained-definition
							    line)
							   line)))
	   (function (for position-list)
		     (uses line definition)
		     (level 10)
		     (definition (pred1 line (:param definition)))))
   (predicates
    (pred1 (line definition)
	    (data~positions line #'(lambda (x)
				     (and (term~constant-p x)
					  (keim~equal (th~definition-constant definition)
						      x)))))
    (pred2 (line definition position-list)
	   (subsetp position-list (pred1 line definition)))
    (func1 (line position-list)
	   (when (every #'(lambda (x) (find x (data~positions line #'term-constant-p) :test #'keim~equal))
			position-list)
	     (find-if
	      #'(lambda (x) (keim~equal (data~struct-at-position line (car position-list))
						 (th~definition-constant x)))
	      (th~definitions-recursed (prob~theory (agplan~current-proof-plan))))))))
    
   

#+weg(agent~defmatrix defn-contract* 
   (agents (c-predicate (for line)
                        (uses )
			(level 3)
                        (definition (not (agplan~contained-definition line))))
           (c-predicate (for line)
                        (uses definition)
			(level 10)
                        (definition (pred1 line (:param definition))))
	   (c-predicate (for line)
                        (uses definition position-list)
			(level 5)
                        (definition (pred2 line (:param definition) (:param position-list))))
	   (function (for definition)
		     (uses line)
		     (level 3)
		     (definition (agplan~contained-definition line)))
           (function (for definition)
		     (uses line position-list)
		     (level 20)
		     (definition (func1 line (:param position-list))))
	   (function (for position-list)
		     (uses line)
		     (level 3)
		     (definition (data~substruct-positions (agplan~contained-definition
							    line)
							   line)))
	   (function (for position-list)
		     (uses line definition)
		     (level 20)
		     (definition (pred1 line (:param definition)))))
   (predicates
    (pred1 (line definition)
	    (data~positions line #'(lambda (x)
				     (and (term~constant-p x)
					  (keim~equal (th~definition-constant definition)
						      x)))))
    (pred2 (line definition position-list)
	   (subsetp position-list (pred1 line definition)))
    (func1 (line position-list)
	   (when (every #'(lambda (x) (find x (data~positions line #'term-constant-p) :test #'keim~equal))
			position-list)
	     (find-if
	      #'(lambda (x) (keim~equal (data~struct-at-position line (car position-list))
						 (th~definition-constant x)))
	      (th~definitions-recursed (prob~theory (agplan~current-proof-plan))))))))



(agent~defmatrix impi 
   (agents (c-predicate (for implication)
                        (uses )
			(definition (logic~implication-p implication))))
) 




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

